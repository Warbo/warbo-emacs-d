;; -*- lexical-binding: t; -*-
(require 'ert)
(require 'eglot)
(require 'haskell-mode)
(require 'flymake)

;;; ============================================================================
;;; Test Philosophy
;;; ============================================================================
;;
;; These tests verify that our Emacs config provides working Haskell development
;; features. Per README.md guidelines:
;;
;; - Tests check FUNCTIONAL outcomes (hover shows type info, diagnostics appear)
;; - Tests do NOT check implementation details (eglot--managed-mode value)
;; - Tests do NOT mock; they use real HLS
;;
;; Batch mode limitation: In interactive Emacs, opening a .hs file triggers
;; haskell-mode-hook which runs eglot-ensure, and post-command-hook fires
;; automatically. In batch mode, we must explicitly trigger these hooks.
;; This is acceptable because we're still testing the config's hooks work.

;;; ============================================================================
;;; Test Helpers
;;; ============================================================================

(defvar warbo-haskell-test-timeout 20
  "Timeout in seconds for waiting on HLS operations.")

(defun warbo-haskell-test-poll (predicate timeout message)
  "Poll PREDICATE until true or TIMEOUT seconds elapse.
MESSAGE describes what we're waiting for. Returns predicate result or nil."
  (let ((start (float-time))
        (result nil))
    (while (and (not (setq result (funcall predicate)))
                (< (- (float-time) start) timeout))
      (accept-process-output nil 0.1))
    result))

(defun warbo-haskell-test-setup-project (dir)
  "Create a minimal Haskell/Nix project in DIR for testing."
  ;; Git repo (required for project detection)
  (let ((default-directory dir))
    (call-process "git" nil nil nil "init"))
  ;; Cabal file (helps HLS understand the project)
  (with-temp-file (expand-file-name "test.cabal" dir)
    (insert "cabal-version: 2.4\n"
            "name: test\n"
            "version: 0.1.0.0\n"
            "executable test\n"
            "  main-is: Main.hs\n"
            "  build-depends: base\n"
            "  default-language: Haskell2010\n"
            "  ghc-options: -Wall\n"))
  ;; Nix shell with HLS and hasktags
  (with-temp-file (expand-file-name "shell.nix" dir)
    (insert "{ pkgs ? import <nixpkgs> {} }:\n"
            "pkgs.mkShell {\n"
            "  buildInputs = [\n"
            "    pkgs.haskell-language-server\n"
            "    pkgs.ormolu\n"
            "    pkgs.ghc\n"
            "    pkgs.cabal-install\n"
            "    pkgs.haskellPackages.hasktags\n"
            "  ];\n"
            "}\n"))
  ;; Direnv integration
  (with-temp-file (expand-file-name ".envrc" dir)
    (insert "use nix\n"))
  (let ((default-directory dir))
    (call-process "direnv" nil nil nil "allow")))

(defun warbo-haskell-test-wait-for-tags ()
  "Wait for TAGS file to appear."
  (let* ((root (vc-root-dir))
         (tags-file (when root (expand-file-name "TAGS" root))))
    (when tags-file
      (warbo-haskell-test-poll
       (lambda () (file-exists-p tags-file))
       5
       "TAGS file generation"))))

(defun warbo-haskell-test-start-hls ()
  "Start HLS for the current buffer, working around batch mode limitations.
This simulates what happens interactively when opening a Haskell file:
1. direnv updates environment (via prog-mode-hook in config)
2. eglot-ensure is called (via haskell-mode-hook in config)
3. post-command-hook fires (triggers actual eglot connection)
Returns non-nil if HLS connected successfully."
  ;; Disable direnv auto-switching to prevent it from unloading the environment
  ;; when post-command-hook runs (direnv-mode hooks into post-command-hook and
  ;; may see a different directory context in batch mode)
  (let ((direnv-mode-was-on (and (boundp 'direnv-mode) direnv-mode)))
    (when direnv-mode-was-on
      (direnv-mode -1))
    (unwind-protect
        (progn
          ;; Step 1: Load direnv environment for this buffer's directory
          (when (fboundp 'direnv-update-environment)
            (direnv-update-environment default-directory))
          ;; Step 2: The config's haskell-mode-hook includes eglot-ensure
          ;; In batch mode, eglot-ensure defers to post-command-hook
          ;; Step 3: Fire post-command-hook to trigger the deferred connection
          (run-hooks 'post-command-hook)
          ;; Wait for connection
          (warbo-haskell-test-poll
           (lambda () (eglot-current-server))
           warbo-haskell-test-timeout
           "HLS to connect"))
      ;; Restore direnv-mode if it was on
      (when direnv-mode-was-on
        (direnv-mode 1)))))

(defun warbo-haskell-test-wait-for-indexing ()
  "Wait for HLS to index the current file (needed for hover/jump/etc)."
  (warbo-haskell-test-poll
   (lambda ()
     (when-let ((server (eglot-current-server)))
       (let ((buf (jsonrpc-events-buffer server)))
         (when buf
           (with-current-buffer buf
             ;; HLS sends diagnostics after processing
             (or (save-excursion
                   (goto-char (point-min))
                   (re-search-forward "publishDiagnostics" nil t))
                 ;; Or shows progress
                 (save-excursion
                   (goto-char (point-min))
                   (re-search-forward "\\$/progress" nil t))))))))
   warbo-haskell-test-timeout
   "HLS indexing"))

(defun warbo-haskell-test-hover-at-point ()
  "Request hover info at point. Returns content string or nil."
  (when-let ((server (eglot-current-server)))
    (condition-case nil
        (let* ((params (eglot--TextDocumentPositionParams))
               (response (jsonrpc-request server :textDocument/hover params
                                          :timeout 5)))
          (when response
            (let ((contents (plist-get response :contents)))
              (cond
               ((stringp contents) contents)
               ((plist-get contents :value) (plist-get contents :value))
               (t (format "%S" contents))))))
      (error nil))))

(defun warbo-haskell-test-definition-at-point ()
  "Request definition location at point. Returns location plist or nil."
  (when-let ((server (eglot-current-server)))
    (condition-case nil
        (let* ((params (eglot--TextDocumentPositionParams))
               (response (jsonrpc-request server :textDocument/definition params
                                          :timeout 5)))
          (cond
           ((null response) nil)
           ((vectorp response) (aref response 0))
           ((plist-get response :uri) response)
           ((listp response) (car response))
           (t response)))
      (error nil))))

(defmacro with-haskell-test-file (content &rest body)
  "Create temp Haskell project with CONTENT in Main.hs, run BODY.
HLS is started and ready before BODY runs. Tests in BODY should
check functional outcomes (hover works, diagnostics appear), not
implementation details (eglot--managed-mode)."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "haskell-test-" t))
          (file (expand-file-name "Main.hs" dir)))
     (unwind-protect
         (progn
           (warbo-haskell-test-setup-project dir)
           (with-temp-file file (insert ,content))
           (with-current-buffer (find-file-noselect file)
             (warbo-haskell-test-wait-for-tags)
             (unless (warbo-haskell-test-start-hls)
               (ert-fail "HLS failed to start - check haskell-language-server-wrapper is available"))
             (warbo-haskell-test-wait-for-indexing)
             ,@body))
       (when-let ((buf (find-buffer-visiting file)))
         (with-current-buffer buf
           (when (eglot-current-server)
             (ignore-errors (eglot-shutdown (eglot-current-server)))))
         (kill-buffer buf))
       (delete-directory dir t))))

;;; ============================================================================
;;; Configuration Tests - verify our config is set up correctly
;;; ============================================================================

(ert-deftest warbo-test-haskell-eglot-command-is-executable ()
  "HLS executable is configured correctly for eglot.
This verifies the config sets up the right command, not that HLS is globally
installed (it may only be available via direnv in project directories)."
  ;; Check the configured command - the actual executable availability is
  ;; tested implicitly by the functional tests (diagnostics, hover, etc.)
  (let ((server-cmd (alist-get 'haskell-mode eglot-server-programs)))
    (should server-cmd)
    ;; The command should reference haskell-language-server-wrapper
    (should (cl-some (lambda (part)
                       (and (stringp part)
                            (string-match-p "haskell-language-server" part)))
                     (if (listp server-cmd) server-cmd (list server-cmd))))))

(ert-deftest warbo-test-haskell-hook-registration ()
  "Our config registers eglot-ensure in haskell-mode-hook."
  (should (memq 'eglot-ensure haskell-mode-hook)))

(ert-deftest warbo-test-haskell-eglot-server-programs ()
  "Eglot knows how to start HLS for haskell-mode."
  (should (alist-get 'haskell-mode eglot-server-programs)))

(ert-deftest warbo-test-haskell-project-root-detection ()
  "Emacs detects project root in a git repo with cabal file."
  (let ((dir (make-temp-file "haskell-project-" t)))
    (unwind-protect
        (progn
          (let ((default-directory dir))
            (call-process "git" nil nil nil "init"))
          (with-temp-file (expand-file-name "test.cabal" dir)
            (insert "name: test"))
          (with-temp-buffer
            (setq default-directory dir)
            (setq buffer-file-name (expand-file-name "Main.hs" dir))
            (let ((proj (project-current)))
              (should proj)
              (should (string-prefix-p (file-name-as-directory dir)
                                       (expand-file-name (project-root proj)))))))
      (delete-directory dir t))))

;;; ============================================================================
;;; Functional Tests - verify LSP features work with our config
;;; ============================================================================

(ert-deftest warbo-test-haskell-diagnostics-show-type-errors ()
  "Opening a Haskell file with type errors shows diagnostics."
  (with-haskell-test-file
   "main :: IO ()\nmain = putStrLn 42"  ; Type error: 42 isn't a String

   ;; Functional test: flymake shows the error to the user
   (flymake-start)
   (let ((diags (warbo-haskell-test-poll
                 #'flymake-diagnostics
                 warbo-haskell-test-timeout
                 "diagnostics")))
     (should diags)
     (should (cl-some (lambda (d)
                        (string-match-p "Num\\|Couldn't match\\|type"
                                        (flymake-diagnostic-text d)))
                      diags)))))

(ert-deftest warbo-test-haskell-hover-shows-type-info ()
  "Hovering over a standard function shows its type."
  (with-haskell-test-file
   "main = putStrLn \"hello\""

   ;; Position on putStrLn
   (goto-char (point-min))
   (search-forward "putStrLn")
   (backward-char 1)
   ;; Functional test: hover shows useful type information
   (let ((hover (warbo-haskell-test-poll
                 (lambda ()
                   (let ((h (warbo-haskell-test-hover-at-point)))
                     (and h (string-match-p "String\\|IO\\|putStrLn" h) h)))
                 10
                 "hover info")))
     (should hover)
     (should (stringp hover)))))

(ert-deftest warbo-test-haskell-jump-to-definition ()
  "M-. can jump to a local definition."
  (with-haskell-test-file
   "myFunc :: Int\nmyFunc = 10\n\nmain = print myFunc"

   ;; Position on myFunc usage in main
   (goto-char (point-max))
   (search-backward "myFunc")
   ;; Functional test: HLS knows where myFunc is defined
   (let ((def (warbo-haskell-test-poll
               #'warbo-haskell-test-definition-at-point
               10
               "definition location")))
     (should def)
     ;; Verify it points to line 1 or 2 (the definition, not the usage on line 4)
     (let* ((range (plist-get def :range))
            (start (plist-get range :start))
            (line (plist-get start :line)))  ; 0-indexed
       (should (< line 2))))))

(ert-deftest warbo-test-haskell-formatting ()
  "Formatting command cleans up whitespace."
  (with-haskell-test-file
   "foo=   5"

   (call-interactively 'eglot-format-buffer)
   ;; Functional test: buffer content is properly formatted
   (let ((formatted (warbo-haskell-test-poll
                     (lambda () (string-match-p "foo = 5" (buffer-string)))
                     5
                     "formatting")))
     (should formatted))))

(ert-deftest warbo-test-haskell-missing-signature-warning ()
  "HLS warns about missing type signatures."
  (with-haskell-test-file
   "f x = x"  ; Missing signature

   (flymake-start)
   (let ((diags (warbo-haskell-test-poll
                 #'flymake-diagnostics
                 warbo-haskell-test-timeout
                 "diagnostics")))
     ;; HLS should produce at least a warning
     (should (> (length diags) 0)))))

;;; ============================================================================
;;; Stubs for future tests
;;; ============================================================================

(ert-deftest warbo-test-haskell-multi-package-project ()
  "Test cross-package navigation in multi-package cabal project."
  (ert-skip "TODO"))

(ert-deftest warbo-test-haskell-stack-project ()
  "Test HLS with Stack-based project."
  (ert-skip "TODO"))

(ert-deftest warbo-test-haskell-import-completion ()
  "Test completion suggests imported functions."
  (ert-skip "TODO"))

(ert-deftest warbo-test-haskell-refactoring-rename ()
  "Test renaming across occurrences."
  (ert-skip "TODO"))

(ert-deftest warbo-test-haskell-repl-integration ()
  "Test loading modules into GHCi."
  (ert-skip "TODO"))

(ert-deftest warbo-test-haskell-documentation-lookup ()
  "Test accessing Haddock documentation."
  (ert-skip "TODO"))

(ert-deftest warbo-test-haskell-type-at-point ()
  "Test displaying inferred types."
  (ert-skip "TODO"))

(ert-deftest warbo-test-haskell-error-location-precision ()
  "Test diagnostics point to correct positions."
  (ert-skip "TODO"))

(ert-deftest warbo-test-haskell-external-dependencies ()
  "Test projects with Hackage dependencies."
  (ert-skip "TODO"))

(ert-deftest warbo-test-haskell-cross-module-references ()
  "Test navigation between local modules."
  (ert-skip "TODO"))

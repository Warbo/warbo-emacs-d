;;; haskell-tests --- Test Haskell editing -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; Batch mode limitation: In interactive Emacs, opening a .hs file triggers
;;; haskell-mode-hook which runs eglot-ensure, and post-command-hook fires
;;; automatically.  In batch mode, we must explicitly trigger these hooks.
;;; This is acceptable because we're still testing the config's hooks work.
;;;
;;; Code:
(require 'ert)
(require 'eglot)
(require 'haskell-mode)
(require 'flymake)

;; Helpers

(defvar warbo-haskell-test-timeout 60
  "Timeout in seconds for waiting on HLS operations.")

(defun warbo-haskell-test-poll (predicate timeout _message)
  "Poll PREDICATE until true or TIMEOUT seconds elapse.
MESSAGE describes what we're waiting for.  Returns predicate result or nil."
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
  ;; Nix shell with HLS and other tools
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
  "Generate TAGS file synchronously for the current project, then visit it.
The async `warbo-haskell-tags' hook may not run in batch mode (e.g. if
`vc-root-dir' returns nil before an initial commit).  We generate TAGS
ourselves and call `visit-tags-table' so that the etags xref backend
can find it without prompting \"Visit tags table\"."
  ;; FIXME: Our Emacs config should be ensuring files like this are being
  ;; created. Otherwise we're not testing our Emacs config!
  (let* ((root (or (vc-root-dir) default-directory))
         (tags-file (expand-file-name "TAGS" root)))
    ;; Generate TAGS synchronously if not already present
    (unless (file-exists-p tags-file)
      (let ((default-directory root))
        (call-process "hasktags" nil nil nil "--etags" ".")))
    (unless (file-exists-p tags-file)
      (ert-fail (format "TAGS file was not created in %s" root)))
    ;; Clear existing tags tables to avoid "Keep current list of tags tables
    ;; also?" prompt when switching between test projects
    (setq tags-file-name nil)
    (setq tags-table-list nil)
    (visit-tags-table tags-file)))

(defun warbo-haskell-test-start-hls ()
  "Start HLS for the current buffer, working around batch mode limitations.
This simulates what happens interactively when opening a Haskell file:
1. direnv updates environment
2. `eglot-ensure' is called
3. `post-command-hook' fires (triggers actual eglot connection)
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
  "Wait for HLS to be ready.
Returns non-nil when ready, nil on timeout.
For files with errors, waits for diagnostics.
For error-free files, waits for hover to work."
  (or
   ;; First try: wait for diagnostics (works for files with errors)
   (warbo-haskell-test-poll
    #'flymake-diagnostics
    5  ; Short timeout - if no errors, won't get diagnostics
    "diagnostics")
   ;; Second try: test if hover works (works for all files once indexed)
   (warbo-haskell-test-poll
    (lambda ()
      (let ((result nil))
        (eglot-hover-eldoc-function
         (lambda (doc &rest _) (setq result doc)))
        (accept-process-output nil 1.0)
        result))
    warbo-haskell-test-timeout
    "hover response")))

(defun warbo-haskell-test-get-documentation-buffer ()
  "Get documentation at point.
Returns buffer contents as string, or nil if not yet available."
  (when (eglot-current-server)
    ;; In batch mode, post-command-hook doesn't fire after cursor movement,
    ;; but eldoc relies on it to trigger documentation fetching.
    (run-hooks 'post-command-hook)
    ;; In batch mode, the idle timer that triggers eldoc doesn't fire,
    ;; so we call the command directly.
    (eldoc)
    ;; Wait for async LSP response AND let timers run (for eldoc's callback)
    (accept-process-output nil 0.5)
    (sleep-for 0.1)
    ;; Try to get the eldoc buffer.  When called non-interactively,
    ;; eldoc-doc-buffer returns the buffer directly.
    (condition-case nil
        (when-let ((buf (eldoc-doc-buffer)))
          (with-current-buffer buf
            (let ((content (buffer-substring-no-properties (point-min) (point-max))))
              (when (> (length content) 0)
                content))))
      (user-error nil))))

(defun warbo-haskell-test-jump-to-definition ()
  "Jump to definition using xref (M-.).
Returns non-nil if we jumped to a different location.
In batch mode, multiple xref results (e.g. type signature + definition from
etags, plus eglot's result) would trigger an interactive prompt that hangs.
We use `xref-show-definitions-completing-read' with a `completing-read'
override that auto-selects the first candidate."
  (when (eglot-current-server)
    (let ((start-pos (point))
          (start-file buffer-file-name)
          (xref-show-definitions-function #'xref-show-definitions-completing-read)
          (xref-show-xrefs-function #'xref-show-definitions-completing-read)
          ;; Auto-select first candidate in batch mode
          (completing-read-function
           (lambda (_prompt collection &rest _args)
             (car (all-completions "" collection)))))
      (condition-case nil
          (progn
            ;; FIXME: Press M-. like a user would
            (xref-find-definitions (xref-backend-identifier-at-point (xref-find-backend)))
            (accept-process-output nil 0.5)
            (or (not (equal buffer-file-name start-file))
                (not (= (point) start-pos))))
        (error nil)))))

(defmacro with-haskell-test-file (content &rest body)
  "Create temp Haskell project with CONTENT in Main.hs, run BODY.
HLS is started and ready before BODY runs."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "haskell-test-" t))
          (file (expand-file-name "Main.hs" dir)))
     (unwind-protect
         (progn
           (warbo-haskell-test-setup-project dir)
           (with-temp-file file (insert ,content))
           ;; Use find-file (not find-file-noselect) because eglot-ensure
           ;; in haskell-mode-hook needs the buffer to be selected
           (find-file file)
           (warbo-haskell-test-wait-for-tags)
           (unless (warbo-haskell-test-start-hls)
             (ert-fail "HLS failed to start - check haskell-language-server-wrapper is available"))
           (unless (warbo-haskell-test-wait-for-indexing)
             (ert-fail (format "HLS not ready within %ds timeout. Server: %s, flymake-diagnostics: %s"
                               warbo-haskell-test-timeout
                               (eglot-current-server)
                               (flymake-diagnostics))))
           ,@body)
       (when-let ((buf (find-buffer-visiting file)))
         (with-current-buffer buf
           (when (eglot-current-server)
             (ignore-errors (eglot-shutdown (eglot-current-server)))
             ;; Wait for the server process to actually exit, so stale state
             ;; doesn't leak into subsequent tests
             (warbo-haskell-test-poll
              (lambda () (not (eglot-current-server)))
              10
              "eglot shutdown"))
           ;; Prevent kill-buffer prompting about unsaved changes (e.g.
           ;; after completion-at-point modifies the buffer)
           (set-buffer-modified-p nil))
         (kill-buffer buf))
       (delete-directory dir t))))

;; Configuration tests

(ert-deftest warbo-test-haskell-eglot-command-is-executable ()
  "HLS executable is configured correctly for eglot."
  ;; Check the configured command - the actual executable availability is
  ;; tested implicitly by the functional tests (diagnostics, hover, etc.)
  (let ((server-cmd (alist-get 'haskell-mode eglot-server-programs)))
    (should server-cmd)
    (should (cl-some (lambda (part)
                       (and (stringp part)
                            (string-match-p "haskell-language-server" part)))
                     (if (listp server-cmd) server-cmd (list server-cmd))))))


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

;; LSP tests

(ert-deftest warbo-test-haskell-diagnostics-show-type-errors ()
  "Opening a Haskell file with type errors shows diagnostics."
  (with-haskell-test-file
   "main :: IO ()\nmain = putStrLn 42"  ; Type error: 42 isn't a String

   (let ((diags (flymake-diagnostics)))
     (should diags)
     (should (cl-some (lambda (d)
                        (string-match-p "Num\\|Couldn't match\\|type"
                                        (flymake-diagnostic-text d)))
                      diags)))))

(ert-deftest warbo-test-haskell-eldoc-shows-type-info ()
  "Documentation buffer shows type info for standard functions."
  (with-haskell-test-file
   "main = putStrLn \"hello\""

   ;; Position on putStrLn
   (goto-char (point-min))
   (search-forward "putStrLn")
   (backward-char 1)
   (let ((doc (warbo-haskell-test-poll
               (lambda ()
                 (let ((d (warbo-haskell-test-get-documentation-buffer)))
                   (and d (string-match-p "String\\|IO\\|putStrLn" d) d)))
               10
               "documentation")))
     (should doc)
     (should (stringp doc)))))

(ert-deftest warbo-test-haskell-jump-to-definition ()
  "Keyboard shortcut can jump to a local definition."
  (with-haskell-test-file
   "myFunc :: Int\nmyFunc = 10\n\nmain = print myFunc"

   ;; Position on myFunc usage in main
   (goto-char (point-max))
   (search-backward "myFunc")
   (let ((usage-line (line-number-at-pos)))
     (let ((jumped (warbo-haskell-test-poll
                    #'warbo-haskell-test-jump-to-definition
                    10
                    "jump to definition")))
       (should jumped)
       ;; Should be on line 1 or 2 (the definition, not the usage on line 4)
       (let ((def-line (line-number-at-pos)))
         (should (< def-line usage-line))
         (should (<= def-line 2)))))))

(ert-deftest warbo-test-haskell-formatting ()
  "Formatting command cleans up whitespace."
  (with-haskell-test-file
   "foo=   5"

   (call-interactively 'eglot-format-buffer)
   (let ((formatted (warbo-haskell-test-poll
                     (lambda () (string-match-p "foo = 5" (buffer-string)))
                     5
                     "formatting")))
     (should formatted))))

(ert-deftest warbo-test-haskell-missing-signature-warning ()
  "HLS warns about missing type signatures."
  (with-haskell-test-file
   ;; Valid Main.hs with a helper function that lacks a type signature
   "module Main where\n\nhelper x = x  -- Missing type signature\n\nmain :: IO ()\nmain = print (helper 42)"

   (let ((diags (warbo-haskell-test-poll
                 #'flymake-diagnostics
                 warbo-haskell-test-timeout
                 "diagnostics")))
     (unless (> (length diags) 0)
       (ert-fail (format "No diagnostics for missing signature. flymake-mode: %s, eglot: %s"
                         flymake-mode
                         (eglot-current-server))))
     ;; Verify we got a missing signature diagnostic specifically
     ;; HLS reports: "Top-level binding with no type signature: ..."
     (let ((has-sig-warning
            (cl-some (lambda (d)
                       (string-match-p "\\(no\\|missing\\|lacks?\\).*type signature"
                                       (downcase (flymake-diagnostic-text d))))
                     diags)))
       (unless has-sig-warning
         (ert-fail (format "Expected missing signature warning but got diagnostics: %s"
                           (mapcar #'flymake-diagnostic-text diags))))
       (should has-sig-warning)))))

(ert-deftest warbo-test-haskell-multi-package-project ()
  "Test cross-package navigation in multi-package cabal project.
This verifies HLS can navigate to definitions in sibling packages."
  (let* ((dir (make-temp-file "haskell-multipackage-" t))
         (pkg1-dir (expand-file-name "pkg1" dir))
         (pkg2-dir (expand-file-name "pkg2" dir))
         (file1 (expand-file-name "Lib.hs" pkg1-dir))
         (file2 (expand-file-name "Main.hs" pkg2-dir)))
    (unwind-protect
        (progn
          ;; TODO: We could maybe extend the common test setup function to
          ;; accept extra definitions, etc. rather than having so much copypasta

          ;; Create project structure
          (make-directory pkg1-dir t)
          (make-directory pkg2-dir t)
          (let ((default-directory dir))
            (call-process "git" nil nil nil "init"))

          ;; Root cabal.project linking both packages
          (with-temp-file (expand-file-name "cabal.project" dir)
            (insert "packages: pkg1/*.cabal\n          pkg2/*.cabal\n"))

          ;; Package 1: library with exported function
          (with-temp-file (expand-file-name "pkg1.cabal" pkg1-dir)
            (insert "cabal-version: 2.4\n"
                    "name: pkg1\n"
                    "version: 0.1.0.0\n"
                    "library\n"
                    "  exposed-modules: Lib\n"
                    "  build-depends: base\n"
                    "  default-language: Haskell2010\n"))
          (with-temp-file file1
            (insert "module Lib (sharedFunc) where\n\n"
                    "sharedFunc :: String -> String\n"
                    "sharedFunc s = \"shared: \" ++ s\n"))

          ;; Package 2: executable using pkg1
          (with-temp-file (expand-file-name "pkg2.cabal" pkg2-dir)
            (insert "cabal-version: 2.4\n"
                    "name: pkg2\n"
                    "version: 0.1.0.0\n"
                    "executable pkg2\n"
                    "  main-is: Main.hs\n"
                    "  build-depends: base, pkg1\n"
                    "  default-language: Haskell2010\n"))
          (with-temp-file file2
            (insert "module Main where\n"
                    "import Lib\n\n"
                    "main :: IO ()\n"
                    "main = putStrLn (sharedFunc \"test\")\n"))

          ;; Nix shell with HLS and other tools
          (with-temp-file (expand-file-name "shell.nix" dir)
            (insert "{ pkgs ? import <nixpkgs> {} }:\n"
                    "pkgs.mkShell {\n"
                    "  buildInputs = [\n"
                    "    pkgs.haskell-language-server\n"
                    "    pkgs.ghc\n"
                    "    pkgs.cabal-install\n"
                    "    pkgs.haskellPackages.hasktags\n"
                    "  ];\n"
                    "}\n"))

          (with-temp-file (expand-file-name ".envrc" dir)
            (insert "use nix\n"))
          (let ((default-directory dir))
            (call-process "direnv" nil nil nil "allow"))

          ;; Test navigation from pkg2 to pkg1
          (with-current-buffer (find-file-noselect file2)
            (warbo-haskell-test-wait-for-tags)
            (unless (warbo-haskell-test-start-hls)
              (ert-fail "HLS failed to start in multi-package project"))
            (warbo-haskell-test-wait-for-indexing)

            ;; Navigate to sharedFunc definition
            (goto-char (point-min))
            (search-forward "sharedFunc")
            (backward-char 1)

            (let ((jumped (warbo-haskell-test-poll
                           #'warbo-haskell-test-jump-to-definition
                           warbo-haskell-test-timeout
                           "cross-package definition")))
              (should jumped)
              ;; Should have jumped to pkg1/Lib.hs
              (should (string-match-p "pkg1.*Lib\\.hs" buffer-file-name)))))

      ;; Cleanup
      (dolist (file (list file1 file2))
        (when-let ((buf (find-buffer-visiting file)))
          (with-current-buffer buf
            (when (eglot-current-server)
              (ignore-errors (eglot-shutdown (eglot-current-server)))
              (warbo-haskell-test-poll
               (lambda () (not (eglot-current-server)))
               10
               "eglot shutdown")))
          (kill-buffer buf)))
      (delete-directory dir t))))

(ert-deftest warbo-test-haskell-stack-project ()
  "Test HLS with Stack-based project.
Verifies stack.yaml projects work with HLS.

SKIPPED: Nixpkgs 25.11 stack has a bug where `stack exec` hits a <<loop>>
error (Haskell infinite loop detection) after constructing the SourceMap.
This causes HLS to fail when it runs `stack exec ghc -- --numeric-version`
to determine the GHC version.  When this fails, HLS prompts the user to
choose an option, which HANGS the test suite in batch mode.

https://github.com/NixOS/nixpkgs/issues/467614

Revisit this when the next stable Nixpkgs comes out."
  :tags '(:skip)
  (skip-unless nil)
  (let* ((dir (make-temp-file "haskell-stack-" t))
         (file (expand-file-name "Main.hs" dir)))
    (unwind-protect
        (progn
          (let ((default-directory dir))
            (call-process "git" nil nil nil "init"))

          ;; TODO: Maybe extend the shared test data setup function to accept
          ;; extra parameters, so we can avoid much of this copypasta.
          ;; Stack configuration
          (with-temp-file (expand-file-name "stack.yaml" dir)
            ;; Use ghc-9.10.3 to match the GHC version from pkgs.ghc in shell.nix
            ;; Disable Stack's nix integration since we provide GHC via shell.nix
            ;; compiler-check: match-minor helps avoid Stack's strict version matching
            (insert "resolver: ghc-9.10.3\n"
                    "system-ghc: true\n"
                    "install-ghc: false\n"
                    "skip-ghc-check: true\n"
                    "compiler-check: match-minor\n"
                    "notify-if-nix-on-path: false\n"
                    "nix:\n"
                    "  enable: false\n"
                    "packages:\n  - .\n"))

          (with-temp-file (expand-file-name "package.yaml" dir)
            (insert "name: stack-test\n"
                    "version: 0.1.0.0\n"
                    "dependencies:\n  - base\n"
                    "executables:\n"
                    "  stack-test:\n"
                    "    main: Main.hs\n"))

          (with-temp-file (expand-file-name "shell.nix" dir)
            (insert "{ pkgs ? import <nixpkgs> {} }:\n"
                    "pkgs.mkShell {\n"
                    "  buildInputs = [\n"
                    "    pkgs.haskell-language-server\n"
                    "    pkgs.stack\n"
                    "    pkgs.ghc\n"
                    "    pkgs.haskellPackages.hasktags\n"
                    "  ];\n"
                    "}\n"))

          (with-temp-file (expand-file-name ".envrc" dir)
            (insert "use nix\n"))
          (let ((default-directory dir))
            (call-process "direnv" nil nil nil "allow"))

          (with-temp-file file
            (insert "main :: IO ()\nmain = putStrLn \"stack project\"\n"))

          (with-current-buffer (find-file-noselect file)
            (warbo-haskell-test-wait-for-tags)
            (unless (warbo-haskell-test-start-hls)
              (ert-fail "HLS failed to start in stack project"))
            (warbo-haskell-test-wait-for-indexing)

            ;; Documentation should work in stack project
            (goto-char (point-min))
            (search-forward "putStrLn")
            (backward-char 1)

            (let ((doc (warbo-haskell-test-poll
                        (lambda ()
                          (let ((d (warbo-haskell-test-get-documentation-buffer)))
                            (and d (string-match-p "String\\|IO" d) d)))
                        warbo-haskell-test-timeout
                        "documentation in stack project")))
              (should doc))))

      (when-let ((buf (find-buffer-visiting file)))
        (with-current-buffer buf
          (when (eglot-current-server)
            (ignore-errors (eglot-shutdown (eglot-current-server)))
            (warbo-haskell-test-poll
             (lambda () (not (eglot-current-server)))
             10
             "eglot shutdown")))
        (kill-buffer buf))
      (delete-directory dir t))))

(ert-deftest warbo-test-haskell-import-completion ()
  "Test completion suggests imported functions.
Verifies `completion-at-point' provides relevant suggestions."
  (with-haskell-test-file
   "import Data.List\n\nmain = print (interc)"

   ;; Position at incomplete "interc"
   (goto-char (point-max))
   (backward-char 1)

   ;; Invoke completion like a user would
   (completion-at-point)

   ;; Buffer should now contain "intercalate"
   (let ((content (buffer-string)))
     (unless (string-match-p "intercalate" content)
       (ert-fail (format "Expected buffer to contain 'intercalate' after completion, but got: %S"
                         content)))
     (should (string-match-p "intercalate" content)))))

(ert-deftest warbo-test-haskell-refactoring-rename ()
  "Test renaming across occurrences.
Verifies eglot-rename updates all references to a symbol."
  (with-haskell-test-file
   "foo :: Int\nfoo = 42\n\nmain = print foo"

   ;; Position on first occurrence of 'foo'
   (goto-char (point-min))
   (search-forward "foo")
   (backward-char 1)

   (when (eglot-current-server)
     (condition-case err
         (progn
           ;; Provide newname via cl-letf
           (cl-letf (((symbol-function 'read-string)
                      (lambda (&rest _) "bar")))
             (eglot-rename "bar"))
           (accept-process-output nil 0.5)

           ;; All occurrences should be renamed
           (let ((content (buffer-string)))
             (should (string-match-p "bar :: Int" content))
             (should (string-match-p "bar = 42" content))
             (should (string-match-p "print bar" content))
             (should-not (string-match-p "foo" content))))
       (error
        (ert-fail (format "Rename failed: %S" err)))))))

(ert-deftest warbo-test-haskell-repl-integration ()
  "Test loading modules into GHCi.
Verifies haskell-mode can load current file and evaluate expressions.
Note: Uses internal API for evaluation since batch mode cannot handle
the interactive 'Hit space to flush' prompts that block on user input."
  (with-haskell-test-file
   ;; Use module Main (matching filename Main.hs) and omit the type signature
   ;; so -Wall produces a missing-signature diagnostic, which lets
   ;; wait-for-indexing detect that HLS is ready.
   "module Main where\n\ntestFunc = \"works\"\n\nmain = putStrLn testFunc\n"

   (require 'haskell-interactive-mode)
   (require 'haskell-process)

   (haskell-process-load-file)

   (let ((proc-ready
          (warbo-haskell-test-poll
           (lambda ()
             (and (haskell-session-maybe)
                  (haskell-process-process (haskell-process))))
           10
           "GHCi process")))
     (unless proc-ready
       (ert-fail "GHCi process failed to start"))

     (let* ((result-output nil)
            (process (haskell-process)))
       (haskell-process-queue-command
        process
        (make-haskell-command
         :state nil
         :go (lambda (_) (haskell-process-send-string (haskell-process) "1 + 2"))
         :complete (lambda (_ response) (setq result-output response))))

       (let ((got-result
              (warbo-haskell-test-poll
               (lambda () result-output)
               10
               "REPL evaluation")))
         (unless got-result
           (ert-fail "REPL evaluation returned no result"))
         (should (string-match-p "3" got-result)))))))

(ert-deftest warbo-test-haskell-documentation-lookup ()
  "Test accessing documentation via eldoc-doc-buffer.
Verifies eldoc documentation buffer shows info for standard library functions."
  (with-haskell-test-file
   "main = putStrLn \"hello\""

   ;; Position on putStrLn
   (goto-char (point-min))
   (search-forward "putStrLn")
   (backward-char 1)

   ;; eldoc-doc-buffer should show documentation
   (let ((doc (warbo-haskell-test-poll
               (lambda ()
                 (let ((d (warbo-haskell-test-get-documentation-buffer)))
                   ;; Documentation should mention String, IO, or describe the function
                   (and d
                        (or (string-match-p "String.*IO\\|IO.*String" d)
                            (string-match-p "putStrLn" d))
                        d)))
               warbo-haskell-test-timeout
               "documentation")))
     (unless doc
       (let ((eldoc-buf (ignore-errors (eldoc-doc-buffer))))
         (ert-fail (format "No documentation found. Debug info:
  eldoc-mode: %s
  eldoc-documentation-functions: %s
  eldoc--doc-buffer: %s
  eldoc-doc-buffer returns: %s
  eldoc buffer content: %S
  eglot server: %s
  point: %s
  buffer around point: %S"
                           (bound-and-true-p eldoc-mode)
                           (bound-and-true-p eldoc-documentation-functions)
                           (bound-and-true-p eldoc--doc-buffer)
                           eldoc-buf
                           (when (buffer-live-p eldoc-buf)
                             (with-current-buffer eldoc-buf
                               (buffer-substring-no-properties (point-min) (point-max))))
                           (eglot-current-server)
                           (point)
                           (buffer-substring-no-properties
                            (max (point-min) (- (point) 20))
                            (min (point-max) (+ (point) 20)))))))
     (should (stringp doc))
     ;; Should contain signature or description
     (should (or (string-match-p "String" doc)
                 (string-match-p "IO" doc))))))

(ert-deftest warbo-test-haskell-type-at-point ()
  "Test displaying inferred types via eldoc-doc-buffer.
Verifies documentation buffer shows types for unannotated local definitions."
  (with-haskell-test-file
   "myValue = 42\n\nmain = print myValue"

   ;; Position on myValue (no type signature, should infer)
   (goto-char (point-min))
   (search-forward "myValue")
   (backward-char 1)

   ;; eldoc-doc-buffer should show inferred type
   (let* ((any-doc (warbo-haskell-test-poll
                    #'warbo-haskell-test-get-documentation-buffer
                    warbo-haskell-test-timeout
                    "any documentation"))
          (doc (and any-doc (string-match-p "Num\\|Integer\\|Int" any-doc) any-doc)))
     (unless doc
       (ert-fail (format "No type info found. Debug:
  got documentation: %S
  expected pattern: Num, Integer, or Int
  eldoc-mode: %s
  eglot: %s"
                         any-doc
                         (bound-and-true-p eldoc-mode)
                         (eglot-current-server))))
     (should (string-match-p "Num\\|Integer\\|Int" doc)))))

(ert-deftest warbo-test-haskell-error-location-precision ()
  "Test diagnostics point to correct positions.
Verifies diagnostics highlight the exact location of errors."
  (with-haskell-test-file
   "foo :: String\nfoo = 42\n\nmain = print foo"

   (let ((diags (warbo-haskell-test-poll
                 #'flymake-diagnostics
                 warbo-haskell-test-timeout
                 "diagnostics")))
     (unless diags
       (ert-fail (format "No diagnostics appeared. flymake-mode: %s, eglot: %s"
                         flymake-mode
                         (eglot-current-server))))

     ;; Error should be on line 2 (foo = 42)
     (let ((error-diag (cl-find-if
                        (lambda (d)
                          (string-match-p "Couldn't match\\|type"
                                          (flymake-diagnostic-text d)))
                        diags)))
       (should error-diag)

       ;; Check the diagnostic points to line 2
       (let* ((region (flymake-diagnostic-beg error-diag))
              (line (line-number-at-pos region)))
         (should (= line 2)))))))

(ert-deftest warbo-test-haskell-external-dependencies ()
  "Test projects with Hackage dependencies.
Verifies HLS can provide info about imported library functions."
  (let* ((dir (make-temp-file "haskell-deps-" t))
         (file (expand-file-name "Main.hs" dir)))
    (unwind-protect
        ;; TODO: Again, extending the test data setup function would avoid lots
        ;; of this copypasta.
        (progn
          (let ((default-directory dir))
            (call-process "git" nil nil nil "init"))

          ;; Project with text dependency
          (with-temp-file (expand-file-name "test.cabal" dir)
            (insert "cabal-version: 2.4\n"
                    "name: test\n"
                    "version: 0.1.0.0\n"
                    "executable test\n"
                    "  main-is: Main.hs\n"
                    "  build-depends: base, text\n"
                    "  default-language: Haskell2010\n"))

          (with-temp-file (expand-file-name "shell.nix" dir)
            (insert "{ pkgs ? import <nixpkgs> {} }:\n"
                    "pkgs.mkShell {\n"
                    "  buildInputs = [\n"
                    "    pkgs.haskell-language-server\n"
                    "    pkgs.ghc\n"
                    "    pkgs.cabal-install\n"
                    "    pkgs.haskellPackages.hasktags\n"
                    "    (pkgs.haskellPackages.ghcWithPackages (ps: [ ps.text ]))\n"
                    "  ];\n"
                    "}\n"))

          (with-temp-file (expand-file-name ".envrc" dir)
            (insert "use nix\n"))
          (let ((default-directory dir))
            (call-process "direnv" nil nil nil "allow"))

          (with-temp-file file
            (insert "import Data.Text (pack)\n\n"
                    "main = print (pack \"test\")\n"))

          (with-current-buffer (find-file-noselect file)
            (warbo-haskell-test-wait-for-tags)
            (unless (warbo-haskell-test-start-hls)
              (ert-fail "HLS failed to start with external deps"))
            (warbo-haskell-test-wait-for-indexing)

            ;; Documentation should work on external library function
            (goto-char (point-min))
            (search-forward "pack")
            (backward-char 1)

            (let ((doc (warbo-haskell-test-poll
                        (lambda ()
                          (let ((d (warbo-haskell-test-get-documentation-buffer)))
                            (and d (string-match-p "String\\|Text\\|pack" d) d)))
                        warbo-haskell-test-timeout
                        "documentation for external function")))
              (should doc))))

      (when-let ((buf (find-buffer-visiting file)))
        (with-current-buffer buf
          (when (eglot-current-server)
            (ignore-errors (eglot-shutdown (eglot-current-server)))
            (warbo-haskell-test-poll
             (lambda () (not (eglot-current-server)))
             10
             "eglot shutdown")))
        (kill-buffer buf))
      (delete-directory dir t))))

(ert-deftest warbo-test-haskell-cross-module-references ()
  "Test navigation between local modules.
Verifies jump-to-definition works across local module boundaries."
  (let* ((dir (make-temp-file "haskell-modules-" t))
         (file1 (expand-file-name "Utils.hs" dir))
         (file2 (expand-file-name "Main.hs" dir)))
    (unwind-protect
        ;; TODO: Again, test data setup should be extended to allow tests like
        ;; this to specify the extra parts they want without having to copypaste
        ;; everything else that's common.
        (progn
          (let ((default-directory dir))
            (call-process "git" nil nil nil "init"))

          (with-temp-file (expand-file-name "test.cabal" dir)
            (insert "cabal-version: 2.4\n"
                    "name: test\n"
                    "version: 0.1.0.0\n"
                    "executable test\n"
                    "  main-is: Main.hs\n"
                    "  other-modules: Utils\n"
                    "  build-depends: base\n"
                    "  default-language: Haskell2010\n"))

          (with-temp-file (expand-file-name "shell.nix" dir)
            (insert "{ pkgs ? import <nixpkgs> {} }:\n"
                    "pkgs.mkShell {\n"
                    "  buildInputs = [\n"
                    "    pkgs.haskell-language-server\n"
                    "    pkgs.ghc\n"
                    "    pkgs.cabal-install\n"
                    "    pkgs.haskellPackages.hasktags\n"
                    "  ];\n"
                    "}\n"))

          (with-temp-file (expand-file-name ".envrc" dir)
            (insert "use nix\n"))
          (let ((default-directory dir))
            (call-process "direnv" nil nil nil "allow"))

          ;; Utils module with helper function
          (with-temp-file file1
            (insert "module Utils (helper) where\n\n"
                    "helper :: String -> String\n"
                    "helper x = \"Helper: \" ++ x\n"))

          ;; Main module importing Utils
          (with-temp-file file2
            (insert "module Main where\n"
                    "import Utils\n\n"
                    "main :: IO ()\n"
                    "main = putStrLn (helper \"test\")\n"))

          ;; Test navigation from Main to Utils
          (with-current-buffer (find-file-noselect file2)
            (warbo-haskell-test-wait-for-tags)
            (unless (warbo-haskell-test-start-hls)
              (ert-fail "HLS failed to start in multi-module project"))
            (warbo-haskell-test-wait-for-indexing)

            ;; Position on 'helper' in Main
            (goto-char (point-max))
            (search-backward "helper")

            ;; Jump to definition in Utils
            (let ((jumped (warbo-haskell-test-poll
                           #'warbo-haskell-test-jump-to-definition
                           warbo-haskell-test-timeout
                           "cross-module definition")))
              (should jumped)
              ;; Should have jumped to Utils.hs
              (should (string-match-p "Utils\\.hs" buffer-file-name)))))

      ;; Cleanup
      (dolist (file (list file1 file2))
        (when-let ((buf (find-buffer-visiting file)))
          (with-current-buffer buf
            (when (eglot-current-server)
              (ignore-errors (eglot-shutdown (eglot-current-server)))
              (warbo-haskell-test-poll
               (lambda () (not (eglot-current-server)))
               10
               "eglot shutdown")))
          (kill-buffer buf)))
      (delete-directory dir t))))

;;; haskell-tests.el ends here

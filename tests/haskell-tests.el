;; -*- lexical-binding: t; -*-
(require 'ert)
(require 'eglot)
(require 'haskell-mode)
(require 'flymake)

(defun warbo-haskell-test-setup-nix-project (dir)
  "Set up DIR as a Nix-based Haskell project with HLS and direnv.
Creates shell.nix, .envrc, and .dir-locals.el for eglot configuration."
  (with-temp-file (expand-file-name "shell.nix" dir)
    (insert "{ pkgs ? import <nixpkgs> {} }:\n"
            "pkgs.mkShell {\n"
            "  buildInputs = [\n"
            "    pkgs.haskell-language-server\n"
            "    pkgs.ormolu\n"
            "    pkgs.ghc\n"
            "    pkgs.cabal-install\n"
            "  ];\n"
            "}\n"))
  (with-temp-file (expand-file-name ".envrc" dir)
    (insert "use nix\n"))
  (with-temp-file (expand-file-name ".dir-locals.el" dir)
    (insert "((haskell-mode . ((eglot-server-programs . ((haskell-mode . (\"haskell-language-server-wrapper\" \"--lsp\")))))))\n"))
  ;; NOTE: Added minimal .cabal file. HLS now connects but still not producing
  ;; diagnostics/hover/definitions. Likely needs GHC to compile the project first,
  ;; or HLS may be waiting for explicit file changes/saves to trigger analysis.
  (with-temp-file (expand-file-name "test.cabal" dir)
    (insert "cabal-version: 2.4\n"
            "name: test\n"
            "version: 0.1.0.0\n"
            "executable test\n"
            "  main-is: Main.hs\n"
            "  build-depends: base\n"
            "  default-language: Haskell2010\n"))
  (let ((default-directory dir))
    (call-process "direnv" nil nil nil "allow")))

(defmacro with-haskell-eglot-test (content &rest body)
  "Create a temporary Haskell file with CONTENT, open it and run BODY."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "test-hls-" t))
          (file (expand-file-name "Main.hs" dir)))
     (unwind-protect
         (progn
           (warbo-haskell-test-setup-nix-project dir)
           (let ((default-directory dir))
             (call-process "git" nil nil nil "init"))
           (with-current-buffer (find-file-noselect file)
             (insert ,content)
             (save-buffer)
             (haskell-mode)
             (call-interactively 'eglot)
             (with-timeout (15 (ert-fail "Timed out waiting for Eglot to manage buffer"))
               (while (not (bound-and-true-p eglot--managed-mode))
                 (accept-process-output nil 0.3)))
             (sleep-for 2)
             ,@body))
       (when-let ((buf (find-buffer-visiting file)))
         (kill-buffer buf))
       (delete-directory dir t))))

(ert-deftest warbo-test-haskell-eglot-command-is-executable ()
  "Test that HLS executable exists in PATH."
  (should (executable-find "haskell-language-server-wrapper")))




(ert-deftest warbo-test-haskell-mode-enables-eglot ()
  "Test that opening a Haskell file in a Git project enables Eglot."
  (let* ((dir (make-temp-file "test-git-project-" t))
         (file (expand-file-name "Main.hs" dir)))
    (unwind-protect
        (progn
          (let ((default-directory dir))
            (call-process "git" nil nil nil "init"))
          (with-current-buffer (find-file-noselect file)
            (haskell-mode)
            (call-interactively 'eglot)
            (with-timeout (10 (ert-fail "Eglot did not enable within 10s"))
              (while (not (bound-and-true-p eglot--managed-mode))
                (accept-process-output nil 0.2)))
            (should (bound-and-true-p eglot--managed-mode))))
      (when-let ((buf (get-file-buffer file)))
        (kill-buffer buf))
      (delete-directory dir t))))

(ert-deftest warbo-test-haskell-project-root-detection ()
  "Test that Emacs can identify a Haskell project root in a git dir."
  (let ((dir (make-temp-file "test-hs-project-" t)))
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
              (should (string-prefix-p
                       (file-name-as-directory dir)
                       (expand-file-name (project-root proj)))))))
      (delete-directory dir t))))

(ert-deftest warbo-test-haskell-eglot-diagnostics ()
  "Test that Flymake shows type errors after C-c ! l."
  (with-haskell-eglot-test
   "main :: IO ()\nmain = putStrLn 42"

   (flymake-start)
   (with-timeout (15 (ert-fail "Timed out waiting for diagnostics"))
     (while (null (flymake-diagnostics))
       (accept-process-output nil 0.5)))

   (let ((diags (flymake-diagnostics)))
     (should (> (length diags) 0))
     (should (string-match-p "Num\\|Couldn't match\\|type" (flymake-diagnostic-text (car diags)))))))

(ert-deftest warbo-test-haskell-eglot-formatting ()
  "Test that formatting command fixes sloppy whitespace."
  (with-haskell-eglot-test
   "foo=   5"

   (call-interactively 'eglot-format-buffer)
   (sleep-for 0.5)
   (should (string-match-p "foo = 5" (buffer-string)))))

(ert-deftest warbo-test-haskell-eglot-docs ()
  "Test that eldoc shows type information for standard functions."
  (with-haskell-eglot-test
   "main = putStrLn \"hello\""

   (goto-char (point-min))
   (search-forward "putStrLn")
   (backward-char 1)
   (let ((doc-found nil)
         (attempts 0))
     (with-timeout (5 (ert-fail (format "Timed out waiting for eldoc after %d attempts. Last message: %S"
                                        attempts (current-message))))
       (while (not doc-found)
         (setq attempts (1+ attempts))
         (eldoc-print-current-symbol-info)
         (accept-process-output nil 0.3)
         (when-let ((msg (current-message)))
           (when (string-match-p "String.*IO\\|putStrLn" msg)
             (setq doc-found msg)))))
     (should (stringp doc-found))
     (should (string-match-p "String.*IO\\|putStrLn" doc-found)))))

(ert-deftest warbo-test-haskell-check-eldoc-buffer ()
  "Diagnostic: check what eldoc actually produces."
  ;; TODO: Eldoc returns nil even after 5s and 10 attempts. HLS connects successfully
  ;; (formatting works) but hover requests likely returning empty/null responses.
  ;; Possible causes:
  ;; - HLS hasn't indexed the file (needs GHC compilation or explicit save trigger)
  ;; - eldoc-print-current-symbol-info doesn't block; need to inspect *EGLOT events*
  ;; - May need to wait for HLS "initialized" or "indexing complete" notification
  (with-haskell-eglot-test
   "main = putStrLn \"hello\""

   (goto-char (point-min))
   (search-forward "putStrLn")
   (backward-char 1)
   (dotimes (_ 10)
     (eldoc-print-current-symbol-info)
     (accept-process-output nil 0.5))
   (let ((msg (current-message)))
     (message "Eldoc message after 5s: %S" msg)
     (should msg))))

(ert-deftest warbo-test-haskell-eglot-find-definition ()
  "Test jumping to a definition within the file using M-."
  ;; TODO: xref-find-definitions not jumping (pos 41->41 on same line). Causes:
  ;; - HLS not returning textDocument/definition response (likely null/empty)
  ;; - xref tries etags backend (we stub it to quit), then gives up
  ;; - HLS needs to index/compile the project before providing definitions
  ;; - Check *EGLOT events* for textDocument/definition request/response
  ;; - May need to wait for HLS background compilation to complete
  (with-haskell-eglot-test
   "myFunc :: Int\nmyFunc = 10\n\nmain = print myFunc"

   (goto-char (point-max))
   (backward-word 1)
   (let ((start-pos (point))
         (start-line (line-number-at-pos)))
     (cl-letf (((symbol-function 'read-file-name)
                (lambda (&rest _) (signal 'quit nil))))
       (condition-case err
           (call-interactively 'xref-find-definitions)
         (quit nil)
         (error (ert-fail (format "xref-find-definitions error: %S" err)))))
     (let ((end-pos (point))
           (end-line (line-number-at-pos)))
       (message "Jump: line %d->%d, pos %d->%d" start-line end-line start-pos end-pos)
       (should (not (= end-pos start-pos)))
       (should (looking-at "myFunc"))))))

(ert-deftest warbo-test-haskell-check-xref-backends ()
  "Diagnostic: check which xref backends are active."
  (with-haskell-eglot-test
   "myFunc :: Int\nmyFunc = 10\n\nmain = print myFunc"

   (goto-char (point-max))
   (backward-word 1)
   (let ((backends (if (boundp 'xref-backend-functions)
                       xref-backend-functions
                     (list (xref-find-backend)))))
     (message "Active xref backends: %S" backends)
     (should backends))))

(ert-deftest warbo-test-haskell-eglot-code-action ()
  "Test applying a code action (adding a missing signature) via menu."
  (with-haskell-eglot-test
   "f x = x"

   (flymake-start)
   (with-timeout (15 (ert-fail (format "Waiting for missing-sig warning. Diagnostics: %S"
                                       (flymake-diagnostics))))
     (while (null (flymake-diagnostics))
       (accept-process-output nil 0.5)))
   (goto-char (point-min))
   (let ((initial-content (buffer-string)))
     (call-interactively 'eglot-code-action-quickfix)
     (sleep-for 1)
     (should (not (string= initial-content (buffer-string))))
     (goto-char (point-min))
     (should (looking-at "f :: ")))))

(ert-deftest warbo-test-haskell-check-hls-produces-warnings ()
  "Diagnostic: verify HLS produces missing-signature warnings at all."
  ;; TODO: HLS not producing diagnostics even with .cabal file. Possible causes:
  ;; - Missing-signature warnings disabled by default in HLS
  ;; - Need hie.yaml with explicit `-Wmissing-signatures` GHC flag
  ;; - HLS may require successful GHC compilation before reporting warnings
  ;; - Check *EGLOT events* buffer for publishDiagnostics messages (likely empty)
  (with-haskell-eglot-test
   "f x = x"

   (flymake-start)
   (sleep-for 5)
   (let ((diags (flymake-diagnostics)))
     (message "Diagnostics after 5s: %S" diags)
     (when diags
       (message "First diagnostic text: %s" (flymake-diagnostic-text (car diags))))
     (should (> (length diags) 0)))))

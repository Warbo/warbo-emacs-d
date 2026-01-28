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
   (let ((doc-found nil))
     (with-timeout (5 (ert-fail "Timed out waiting for eldoc"))
       (while (not doc-found)
         (eldoc-print-current-symbol-info)
         (accept-process-output nil 0.3)
         (when-let ((msg (current-message)))
           (when (string-match-p "String.*IO\\|putStrLn" msg)
             (setq doc-found msg)))))
     (should (stringp doc-found))
     (should (string-match-p "String.*IO\\|putStrLn" doc-found)))))

(ert-deftest warbo-test-haskell-eglot-find-definition ()
  "Test jumping to a definition within the file using M-."
  (with-haskell-eglot-test
   "myFunc :: Int\nmyFunc = 10\n\nmain = print myFunc"

   (goto-char (point-max))
   (backward-word 1)
   (let ((start-pos (point)))
     (cl-letf (((symbol-function 'read-file-name)
                (lambda (&rest _) (signal 'quit nil))))
       (condition-case nil
           (call-interactively 'xref-find-definitions)
         (quit nil)))
     (should (not (= (point) start-pos)))
     (should (looking-at "myFunc")))))

(ert-deftest warbo-test-haskell-eglot-code-action ()
  "Test applying a code action (adding a missing signature) via menu."
  (with-haskell-eglot-test
   "f x = x"

   (flymake-start)
   (with-timeout (15 (ert-fail "Waiting for missing-sig warning"))
     (while (null (flymake-diagnostics))
       (accept-process-output nil 0.5)))
   (goto-char (point-min))
   (let ((initial-content (buffer-string)))
     (call-interactively 'eglot-code-action-quickfix)
     (sleep-for 1)
     (should (not (string= initial-content (buffer-string))))
     (goto-char (point-min))
     (should (looking-at "f :: ")))))

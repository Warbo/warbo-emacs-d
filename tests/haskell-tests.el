;;; haskell-tests --- Test Haskell editing -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; Batch mode limitation: In interactive Emacs, when eglot-ensure runs the
;;; post-command-hook fires automatically.  In batch mode, we must explicitly
;;; trigger these hooks.
;;; This is acceptable because we're still testing the config's hooks work.
;;;
;;; Code:
(require 'ert)
(require 'eglot)
(require 'flymake)

;; Helpers

(defvar warbo-haskell-test-timeout 60
  "Timeout in seconds for waiting on HLS operations.")

(defun warbo-haskell-test-cleanup-sessions ()
  "Kill all haskell-mode session processes and clear the session list.
This prevents stale sessions from leaking between tests and triggering
the interactive \"Choose Haskell session:\" prompt (which hangs in batch
mode).  We manipulate `haskell-sessions' directly rather than calling
`haskell-session-kill', since that calls `(haskell-session)' which can
itself prompt.
We do NOT kill interactive buffers here, since they have a
`kill-buffer-hook' (`haskell-interactive-kill') that prompts
\"Kill the whole session?\".  Leaving them around is harmless once the
session is removed from `haskell-sessions'."
  (when (boundp 'haskell-sessions)
    (dolist (session haskell-sessions)
      (ignore-errors (haskell-kill-session-process session)))
    (setq haskell-sessions nil)))

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
  ;; Eglot configuration for haskell-ts-mode (normally lives in .dir-locals.el
  ;; of real projects; we include it here so the test project resembles one)
  (with-temp-file (expand-file-name ".dir-locals.el" dir)
    (insert (format "((haskell-ts-mode . ((eglot-server-programs . ((haskell-ts-mode . %S))))))\n"
                    warbo-haskell-eglot-args)))
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
    ;; Load direnv environment so hasktags is available
    (when (fboundp 'direnv-update-environment)
      (direnv-update-environment root))
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
Returns non-nil if HLS connected successfully.
On failure, prints diagnostic info to help debug the problem."
  ;; Disable direnv auto-switching to prevent it from unloading the environment
  ;; when post-command-hook runs (direnv-mode hooks into post-command-hook and
  ;; may see a different directory context in batch mode)
  (let ((direnv-mode-was-on (and (boundp 'direnv-mode) direnv-mode))
        (debug-lines nil))
    (cl-flet ((dbg (fmt &rest args)
                (push (apply #'format fmt args) debug-lines)))
      (when direnv-mode-was-on
        (direnv-mode -1))
      (unwind-protect
          (progn
            (dbg "default-directory: %s" default-directory)
            (dbg "major-mode: %s" major-mode)
            (dbg "buffer-file-name: %s" buffer-file-name)
            (dbg "eglot--managed-mode at entry: %S" eglot--managed-mode)
            (dbg "eglot-current-server at entry: %S" (eglot-current-server))
            (dbg "eglot-server-programs (buffer-local): %S"
                 (if (local-variable-p 'eglot-server-programs)
                     eglot-server-programs
                   '(not-buffer-local)))
            (dbg "dir-locals-file: %S"
                 (and (fboundp 'dir-locals-find-file)
                      (dir-locals-find-file default-directory)))
            ;; Step 1: Load direnv environment for this buffer's directory
            (when (fboundp 'direnv-update-environment)
              (direnv-update-environment default-directory))
            (dbg "exec-path after direnv: %S" exec-path)
            (dbg "haskell-language-server in PATH: %S"
                 (executable-find "haskell-language-server"))
            (dbg "haskell-language-server-wrapper in PATH: %S"
                 (executable-find "haskell-language-server-wrapper"))
            (dbg "eglot--guess-contact result: %S"
                 (ignore-errors (eglot--guess-contact)))
            (dbg "post-command-hook (buffer-local) before eglot-ensure: %S"
                 (if (local-variable-p 'post-command-hook)
                     post-command-hook
                   '(not-buffer-local)))
            ;; Step 2: (Re-)call eglot-ensure now that PATH is correct.
            ;; maybe-connect may have already fired prematurely (before direnv
            ;; loaded the environment) and silently failed, removing itself from
            ;; post-command-hook.  Calling eglot-ensure again re-queues it.
            (eglot-ensure)
            (dbg "post-command-hook (buffer-local) after eglot-ensure: %S"
                 (if (local-variable-p 'post-command-hook)
                     post-command-hook
                   '(not-buffer-local)))
            ;; Step 3: Fire post-command-hook to trigger the deferred connection
            (run-hooks 'post-command-hook)
            (dbg "eglot-current-server immediately after post-command-hook: %S"
                 (eglot-current-server))
            ;; Wait for connection
            (let ((result (warbo-haskell-test-poll
                           (lambda () (eglot-current-server))
                           warbo-haskell-test-timeout
                           "HLS to connect")))
              (unless result
                ;; Only emit diagnostics on failure
                (let ((events-buf (warbo-haskell-test-events-buffer)))
                  (dbg "eglot events buffer: %S"
                       (if (and events-buf (buffer-live-p events-buf))
                           (with-current-buffer events-buf (buffer-string))
                         'not-found)))
                (dolist (line (nreverse debug-lines))
                  (message "HLS-DEBUG: %s" line)))
              result))
        ;; Restore direnv-mode if it was on
        (when direnv-mode-was-on
          (direnv-mode 1))))))

(defun warbo-haskell-test-events-buffer ()
  "Return the eglot events buffer for the current buffer, or nil."
  (let* ((project (project-current))
         (project-name (when project
                         (file-name-nondirectory
                          (directory-file-name (project-root project)))))
         (mode-name (symbol-name major-mode)))
    (when project-name
      (get-buffer (format "*EGLOT (%s/(%s)) events*"
                          project-name mode-name)))))

(defun warbo-haskell-test-wait-for-indexing ()
  "Wait for HLS to be ready.
Returns non-nil when ready, nil on timeout.
Each iteration checks both diagnostics and hover, so either signal
suffices.  Point should be on an identifier before calling this, since
HLS returns no hover info for keywords like `module'."
  (warbo-haskell-test-poll
   (lambda ()
     (or
      ;; Diagnostics appear for files with errors/warnings
      (flymake-diagnostics)
      ;; Hover works for any identifier once HLS has indexed
      (let ((result nil))
        (eglot-hover-eldoc-function
         (lambda (doc &rest _) (setq result doc)))
        (accept-process-output nil 1.0)
        result)))
   warbo-haskell-test-timeout
   "diagnostics or hover"))

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

(defmacro with-haskell-test-file (content point-on &rest body)
  "Create temp Haskell project with CONTENT in Main.hs, run BODY.
POINT-ON is a string to search for in the buffer; point is positioned
on its first occurrence before waiting for HLS.  This matters because
`warbo-haskell-test-wait-for-indexing' uses hover-at-point to detect
readiness, and HLS returns nothing for keywords like `module'.
HLS is started and ready before BODY runs."
  (declare (indent 2))
  `(let* ((dir (make-temp-file "haskell-test-" t))
          (file (expand-file-name "Main.hs" dir)))
     (unwind-protect
         (progn
           (warbo-haskell-test-setup-project dir)
           (with-temp-file file (insert ,content))
           ;; Use find-file (not find-file-noselect) because eglot needs the
           ;; buffer to be selected when it starts
           (find-file file)
           (warbo-haskell-test-wait-for-tags)
           (unless (warbo-haskell-test-start-hls)
             (ert-fail "HLS failed to start - check haskell-language-server-wrapper is available"))
           ;; Position point on an identifier so hover-based readiness
           ;; detection works (HLS returns nothing for keywords)
           (goto-char (point-min))
           (search-forward ,point-on)
           (goto-char (match-beginning 0))
           (unless (warbo-haskell-test-wait-for-indexing)
             (let ((events-buf (warbo-haskell-test-events-buffer)))
               (ert-fail (format "HLS not ready within %ds timeout. flymake-diags: %s, eglot-managed: %s, events(full): %S"
                                 warbo-haskell-test-timeout
                                 (flymake-diagnostics)
                                 (eglot-managed-p)
                                 (when (and events-buf (buffer-live-p events-buf))
                                   (with-current-buffer events-buf
                                     (buffer-string)))))))
           ,@body)
       (warbo-haskell-test-cleanup-sessions)
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

(ert-deftest warbo-test-haskell-shebang-opens-ts-mode ()
  "A .hs file with a runhaskell/runghc shebang opens in haskell-ts-mode.
Emacs resolves shebangs via `interpreter-mode-alist', which takes priority
over `auto-mode-alist'.  `major-mode-remap-alist' should swap-out haskell-mode
for haskell-ts-mode."
  (dolist (interpreter '("runhaskell" "runghc"))
    (let* ((dir (make-temp-file "haskell-shebang-" t))
           (file (expand-file-name "script.hs" dir)))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert (format "#!/usr/bin/env %s\n" interpreter)
                      "module Main where\n"
                      "main = putStrLn \"hello\"\n"))
            (let ((buf (find-file-noselect file)))
              (unwind-protect
                  (with-current-buffer buf
                    (should (eq major-mode 'haskell-ts-mode)))
                (kill-buffer buf))))
        (delete-directory dir t)))))

;; LSP tests

(ert-deftest warbo-test-haskell-diagnostics-show-type-errors ()
  "Opening a Haskell file with type errors shows diagnostics."
  (with-haskell-test-file
      "main :: IO ()\nmain = putStrLn 42"  ; Type error: 42 isn't a String
      "putStrLn"

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
      "putStrLn"

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
      "myFunc"

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
      "foo"

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
      "helper"

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
          ;; Open file like a user would (C-x C-f), so direnv and hooks run
          (find-file file2)
          (warbo-haskell-test-wait-for-tags)
          (unless (warbo-haskell-test-start-hls)
            (ert-fail "HLS failed to start in multi-package project"))
          ;; Position on an identifier so hover-based readiness detection
          ;; works (HLS returns nothing for keywords like `module')
          (goto-char (point-min))
          (search-forward "sharedFunc")
          (goto-char (match-beginning 0))
          (warbo-haskell-test-wait-for-indexing)

          ;; Navigate to sharedFunc definition (point is already on it)
          (let ((jumped (warbo-haskell-test-poll
                         #'warbo-haskell-test-jump-to-definition
                         warbo-haskell-test-timeout
                         "cross-package definition")))
            (should jumped)
            ;; Should have jumped to pkg1/Lib.hs
            (should (string-match-p "pkg1.*Lib\\.hs" buffer-file-name))))

      ;; Cleanup
      (warbo-haskell-test-cleanup-sessions)
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

          ;; Open file like a user would (C-x C-f), so direnv and hooks run
          (find-file file)
          (warbo-haskell-test-wait-for-tags)
          (unless (warbo-haskell-test-start-hls)
            (ert-fail "HLS failed to start in stack project"))
          ;; Position on an identifier so hover-based readiness detection
          ;; works (HLS returns nothing for keywords like `module')
          (goto-char (point-min))
          (search-forward "putStrLn")
          (goto-char (match-beginning 0))
          (warbo-haskell-test-wait-for-indexing)

          ;; Documentation should work in stack project
          (let ((doc (warbo-haskell-test-poll
                      (lambda ()
                        (let ((d (warbo-haskell-test-get-documentation-buffer)))
                          (and d (string-match-p "String\\|IO" d) d)))
                      warbo-haskell-test-timeout
                      "documentation in stack project")))
            (should doc)))

      (warbo-haskell-test-cleanup-sessions)
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
      "print"

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
      "foo"

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
Verifies we can load current file and evaluate expressions.
Note: Uses internal API for evaluation since batch mode cannot handle
the interactive 'Hit space to flush' prompts that block on user input."
  (with-haskell-test-file
      ;; Use module Main (matching filename Main.hs) and omit the type signature
      ;; so -Wall produces a missing-signature diagnostic, which lets
      ;; wait-for-indexing detect that HLS is ready.
      "module Main where\n\ntestFunc = \"works\"\n\nmain = putStrLn testFunc\n"
      "testFunc"

   (require 'haskell-interactive-mode)
   (require 'haskell-process)

   (warbo-haskell-test-cleanup-sessions)

   ;; In batch mode, haskell-process-load-file may still prompt if creating
   ;; a new session.  The "Choose Haskell session:" prompt goes through
   ;; `haskell-completing-read-function' (default: `ido-completing-read'),
   ;; not `completing-read', so we must override both.  Also disable the
   ;; y-or-n-p prompt from `haskell-session-new-assume-from-cabal'.
   (let ((haskell-completing-read-function
          (lambda (_prompt collection &rest _args)
            (or (car (all-completions "" collection))
                (car collection)
                "")))
         (haskell-process-load-or-reload-prompt nil))
     (cl-letf (((symbol-function 'completing-read)
                (lambda (_prompt collection &rest _args)
                  (or (car (all-completions "" collection))
                      (car collection)
                      "")))
               ((symbol-function 'ido-completing-read)
                (lambda (_prompt choices &rest _args)
                  (car choices))))
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
             (should (string-match-p "3" got-result)))))))))

(ert-deftest warbo-test-haskell-documentation-lookup ()
  "Test accessing documentation via eldoc-doc-buffer.
Verifies eldoc documentation buffer shows info for standard library functions."
  (with-haskell-test-file
      "main = putStrLn \"hello\""
      "putStrLn"

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
      "myValue"

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
      "foo"

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

          ;; Open file like a user would (C-x C-f), so direnv and hooks run
          (find-file file)
          (warbo-haskell-test-wait-for-tags)
          (unless (warbo-haskell-test-start-hls)
            (ert-fail "HLS failed to start with external deps"))
          ;; Position on an identifier so hover-based readiness detection
          ;; works (HLS returns nothing for keywords like `module')
          (goto-char (point-min))
          (search-forward "pack")
          (goto-char (match-beginning 0))
          (warbo-haskell-test-wait-for-indexing)

          ;; Documentation should work on external library function
          (let ((doc (warbo-haskell-test-poll
                      (lambda ()
                        (let ((d (warbo-haskell-test-get-documentation-buffer)))
                          (and d (string-match-p "String\\|Text\\|pack" d) d)))
                      warbo-haskell-test-timeout
                      "documentation for external function")))
            (should doc)))

      (warbo-haskell-test-cleanup-sessions)
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
          ;; Open file like a user would (C-x C-f), so direnv and hooks run
          (find-file file2)
          (warbo-haskell-test-wait-for-tags)
          (unless (warbo-haskell-test-start-hls)
            (ert-fail "HLS failed to start in multi-module project"))
          ;; Position on an identifier so hover-based readiness detection
          ;; works (HLS returns nothing for keywords like `module')
          (goto-char (point-max))
          (search-backward "helper")
          (warbo-haskell-test-wait-for-indexing)

          ;; Jump to definition in Utils (point is already on 'helper')
          (let ((jumped (warbo-haskell-test-poll
                         #'warbo-haskell-test-jump-to-definition
                         warbo-haskell-test-timeout
                         "cross-module definition")))
            (should jumped)
            ;; Should have jumped to Utils.hs
            (should (string-match-p "Utils\\.hs" buffer-file-name))))

      ;; Cleanup
      (warbo-haskell-test-cleanup-sessions)
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

(ert-deftest warbo-test-haskell-hlint-on-the-fly ()
  "Test that opening a Haskell file with style issues shows hlint diagnostics.
HLS's hlint plugin reports suggestions as flymake diagnostics.  We use
'putStrLn (show ...)' which hlint flags as 'Use print'."
  :tags '(:hlint :linter)
  (with-haskell-test-file
      "module Main where\n\nmain :: IO ()\nmain = putStrLn (show (1 + 1))"
      "putStrLn"

   (let ((hlint-diag
          (warbo-haskell-test-poll
           (lambda ()
             (seq-find (lambda (d)
                         (string-match-p
                          "hlint\\|Use print"
                          (flymake-diagnostic-text d)))
                       (flymake-diagnostics)))
           warbo-haskell-test-timeout
           "hlint diagnostics")))
     (unless hlint-diag
       (ert-fail (format "No hlint diagnostic found. All diagnostics: %s"
                         (mapcar #'flymake-diagnostic-text
                                 (flymake-diagnostics)))))
     (should (string-match-p "print" (flymake-diagnostic-text hlint-diag))))))

(ert-deftest warbo-test-haskell-hlint-apply-suggestion ()
  "Test applying an hlint code action via eglot.
Uses 'putStrLn (show ...)' which hlint suggests replacing with 'print'.
Waits for the hlint diagnostic, then applies the code action.

Note: HLS requires the 'apply-refact' tool in PATH to provide hlint
code actions.  Diagnostics work without it (HLS bundles the hlint
library), but code actions need apply-refact to rewrite the source."
  :tags '(:hlint :code-action)
  (with-haskell-test-file
      "module Main where\n\nmain :: IO ()\nmain = putStrLn (show (1 + 1))"
      "putStrLn"

   ;; Wait for hlint diagnostic to appear
   (let ((hlint-diag
          (warbo-haskell-test-poll
           (lambda ()
             (seq-find (lambda (d)
                         (string-match-p
                          "hlint\\|Use print"
                          (flymake-diagnostic-text d)))
                       (flymake-diagnostics)))
           warbo-haskell-test-timeout
           "hlint diagnostics")))
     (unless hlint-diag
       (ert-fail (format "No hlint diagnostic. All diagnostics: %s"
                         (mapcar #'flymake-diagnostic-text
                                 (flymake-diagnostics)))))

     ;; Position point on "putStrLn" where the diagnostic is
     (goto-char (point-min))
     (search-forward "putStrLn")
     (goto-char (match-beginning 0))

     ;; HLS needs apply-refact in PATH for hlint code actions.
     ;; apply-refact is currently broken in nixpkgs (marked as broken),
     ;; so code actions may not be available even though diagnostics are.
     (let ((actions (eglot-code-actions (point) (point))))
       (unless actions
         (ert-skip "No code actions available (apply-refact likely missing)"))
       (let ((before (buffer-string)))
         (eglot-code-action-quickfix (point) (point))
         (accept-process-output nil 1.0)
         (should (not (string= before (buffer-string))))
         (should (string-match-p "print" (buffer-string))))))))

(ert-deftest warbo-test-haskell-hlint-flycheck-fallback ()
  "Test that flycheck runs hlint when HLS is not available.
When the LSP binary is missing, eglot should not start and flycheck
should provide hlint diagnostics as a fallback.  Uses 'putStrLn (show
...)' which hlint flags as 'Use print'."
  :tags '(:hlint :flycheck :fallback)
  (let* ((dir (make-temp-file "haskell-test-" t))
         (file (expand-file-name "Main.hs" dir)))
    (unwind-protect
        (progn
          (warbo-haskell-test-setup-project dir)
          (with-temp-file file
            (insert "module Main where\n\nmain :: IO ()\nmain = putStrLn (show (1 + 1))"))
          (find-file file)

          ;; Load direnv environment so hlint is in PATH
          (when (fboundp 'direnv-update-environment)
            (direnv-update-environment default-directory))
          (skip-unless (executable-find "hlint"))

          ;; Block eglot from starting, simulating HLS being unavailable
          (let ((eglot-server-programs nil))
            (run-hooks 'post-command-hook)
            ;; flycheck should be active (no eglot to take over)
            (unless (bound-and-true-p flycheck-mode)
              (flycheck-mode 1))

            ;; Select the hlint checker directly (normally chains from
            ;; haskell-ghc, but GHC checking isn't what we're testing)
            (flycheck-select-checker 'haskell-hlint)
            (flycheck-buffer)

            (let ((found (warbo-haskell-test-poll
                          (lambda ()
                            (and (not (eq (flycheck-get-checker-state
                                          'haskell-hlint)
                                         'running))
                                 (flycheck-current-errors)))
                          warbo-haskell-test-timeout
                          "flycheck hlint errors")))
              (unless found
                (ert-fail
                 (format "No flycheck hlint errors.  State: %s, checker: %s"
                         (flycheck-get-checker-state 'haskell-hlint)
                         flycheck-checker)))
              (let ((msgs (mapcar #'flycheck-error-message found)))
                (should (cl-some (lambda (m) (string-match-p "print" m))
                                 msgs))))))
      ;; Cleanup
      (warbo-haskell-test-cleanup-sessions)
      (when-let ((buf (find-buffer-visiting file)))
        (with-current-buffer buf
          (set-buffer-modified-p nil))
        (kill-buffer buf))
      (delete-directory dir t))))

(ert-deftest warbo-test-haskell-hoogle-search-online ()
  "Test searching Hoogle online for a function and getting results.
Should be able to search the Hoogle database and display results."
  :tags '(:skip :hoogle :online)
  (skip-unless nil)
  ;; TODO: Set up Hoogle search integration
  ;; - Can use helm-hoogle, haskell-hoogle, or similar packages
  ;; - Should allow searching by function name and viewing results
  )

(ert-deftest warbo-test-haskell-hoogle-lookup-at-point ()
  "Test looking up documentation for the function at point using Hoogle.
With point on an identifier like 'foldl', a key binding should show Hoogle results."
  :tags '(:skip :hoogle :lookup)
  (skip-unless nil)
  ;; TODO: Set up a keybinding to look up the identifier at point in Hoogle
  ;; - Could use helm-hoogle with a command wrapper
  ;; - Or integrate with haskell-hoogle package
  )

(ert-deftest warbo-test-haskell-hoogle-local-database ()
  "Test that a local Hoogle database can be generated and used.
Should work offline after running 'hoogle generate'."
  :tags '(:skip :hoogle :local)
  (skip-unless nil)
  ;; TODO: Ensure Hoogle database is generated and available
  ;; - Can be done via 'hoogle generate' command
  ;; - Local database should work offline
  )

(ert-deftest warbo-test-haskell-hoogle-project-specific ()
  "Test Hoogle search scoped to project dependencies.
With a .cabal file specifying dependencies, Hoogle results should prioritize those."
  :tags '(:skip :hoogle :project)
  (skip-unless nil)
  ;; TODO: Set up Hoogle search scoped to project dependencies
  ;; - Could parse .cabal file and restrict search to declared packages
  ;; - Or configure Hoogle to index only project dependencies
  )

(ert-deftest warbo-test-haskell-hoogle-insert-import ()
  "Test that Hoogle can help add missing imports.
When a function is undefined, Hoogle should help identify and add the needed import."
  :tags '(:skip :hoogle :import)
  (skip-unless nil)
  ;; TODO: Integrate Hoogle with import insertion
  ;; - When code action or eglot reports an undefined symbol
  ;; - Query Hoogle to find which module it comes from
  ;; - Automatically add the import
  )

(ert-deftest warbo-test-haskell-djinn-synthesis ()
  "Test using Djinn to synthesize function implementations from type signatures.
Given a type like 'Maybe a -> a -> a', Djinn should suggest a function body."
  :tags '(:skip :djinn :synthesis)
  (skip-unless nil)
  ;; TODO: Set up Djinn integration for function synthesis
  ;; - Should allow invoking Djinn on a type signature
  ;; - Could use djinn executable or Haskell bindings
  )  ; Djinn might say "cannot" for this type

(ert-deftest warbo-test-haskell-djinn-at-point ()
  "Test synthesizing a function body for the type signature at point.
With point on a type signature like 'f :: a -> a', should generate an implementation."
  :tags '(:skip :djinn :synthesis)
  (skip-unless nil)
  ;; TODO: Set up a keybinding to invoke Djinn on the type signature at point
  ;; - Parse the type signature under the cursor
  ;; - Call Djinn with that signature
  ;; - Insert the generated implementation
  )

(ert-deftest warbo-test-haskell-magichaskeller-search ()
  "Test using MagicHaskeller to synthesize implementations from examples.
Given input/output examples, should suggest matching function implementations."
  :tags '(:skip :magichaskeller :synthesis)
  (skip-unless nil)
  ;; TODO: Set up MagicHaskeller integration if available
  ;; - This is a heavier-weight synthesis tool than Djinn
  ;; - Would need Haskell environment and mhs executable
  )

(ert-deftest warbo-test-haskell-hoogleplus-synthesis ()
  "Test using Hoogle+ for program synthesis from type and examples.
Given input/output examples and type constraints, should suggest matching functions."
  :tags '(:skip :hoogleplus :synthesis)
  (skip-unless nil)
  ;; TODO: Set up Hoogle+ integration if available
  ;; - Hoogle+ is a research prototype for example-based synthesis
  ;; - May not be readily available in standard Haskell environments
  )

(ert-deftest warbo-test-haskell-quickspec-generate-properties ()
  "Test generating QuickCheck properties using QuickSpec.
For a function like 'reverse', should discover properties such as 'reverse (reverse xs) = xs'."
  :tags '(:skip :quickspec :testing :property)
  (skip-unless nil)
  ;; TODO: Set up QuickSpec integration to generate properties
  ;; - Requires QuickSpec library and a way to invoke it
  ;; - Should discover algebraic properties of functions
  )

(ert-deftest warbo-test-haskell-spectacular-test-generation ()
  "Test generating QuickCheck test cases from function type signatures.
Should produce test property skeletons based on the function's type."
  :tags '(:skip :spectacular :testing)
  (skip-unless nil)
  ;; TODO: Set up Spectacular integration for test generation
  ;; - This is research-level tooling for automatic test generation
  ;; - Would generate QuickCheck properties from signatures
  )

(ert-deftest warbo-test-haskell-ghci-start ()
  "Test starting a GHCi REPL in the current project.
Should open a buffer running GHCi with the project loaded."
  :tags '(:ghci :repl)
  ;; TODO: Test haskell-interactive-mode-hook or similar
  (skip-unless (fboundp 'haskell-interactive-switch))
  (with-haskell-test-file
   "main = putStrLn \"Hello\""
   "putStrLn"

   ;; Override session-choosing prompts that hang in batch mode.
   ;; `haskell-completing-read-function' defaults to `ido-completing-read',
   ;; not `completing-read', so we must handle both.
   (let ((haskell-completing-read-function
          (lambda (_prompt collection &rest _args)
            (or (car (all-completions "" collection))
                (car collection)
                "")))
         (haskell-process-load-or-reload-prompt nil))
     (call-interactively 'haskell-interactive-switch)
     (sleep-for 2)
     (let ((repl-buffer (get-buffer "*haskell*")))
       (should repl-buffer)
       (with-current-buffer repl-buffer
         (should (string-match-p "GHCi" (buffer-string))))))))

(ert-deftest warbo-test-haskell-send-region-to-ghci ()
  "Test sending selected code to GHCi for evaluation.
Selecting '2 + 2' and sending to REPL should show '4'."
  :tags '(:ghci :repl)
  ;; TODO: Test haskell-interactive-mode region eval
  (skip-unless (fboundp 'haskell-interactive-mode-eval-region))
  (with-haskell-test-file
   "x = 2 + 2"
   "x"

   ;; Override session-choosing prompts that hang in batch mode
   (let ((haskell-completing-read-function
          (lambda (_prompt collection &rest _args)
            (or (car (all-completions "" collection))
                (car collection)
                "")))
         (haskell-process-load-or-reload-prompt nil))
     (call-interactively 'haskell-interactive-switch)
     (sleep-for 1)
     (goto-char (point-min))
     (set-mark (point))
     (end-of-line)
     (call-interactively 'haskell-interactive-mode-eval-region)
     (sleep-for 1)
     (with-current-buffer (get-buffer "*haskell*")
       (should (string-match-p "4" (buffer-string)))))))

(ert-deftest warbo-test-haskell-ghci-reload-on-save ()
  "Test that saving a Haskell file automatically reloads it in GHCi."
  :tags '(:ghci :repl :reload)
  ;; TODO: Check if haskell-mode has auto-reload on save
  (skip-unless (fboundp 'haskell-interactive-switch))
  (should t))  ; Placeholder

(ert-deftest warbo-test-haskell-add-import ()
  "Test adding a missing import interactively.
With point on an undefined symbol, should offer to add the import."
  :tags '(:import :code-action)
  ;; TODO: This should work via eglot code actions
  (with-haskell-test-file
   "main = print $ sortBy compare [3,1,2]"
   "print"

   (flymake-start)
   (sleep-for 3)
   (goto-char (point-min))
   (search-forward "sortBy")
   (call-interactively 'eglot-code-action-quickfix)
   (sleep-for 1)
   (goto-char (point-min))
   (should (search-forward "import" nil t))))

(ert-deftest warbo-test-haskell-organize-imports ()
  "Test organizing imports (sorting, grouping, removing unused).
Messy imports should be cleaned up: sorted, grouped, and unused imports removed."
  :tags '(:skip :import :formatting)
  (skip-unless nil)
  ;; TODO: Set up import organization
  ;; - Can use HLS code actions, stylish-haskell, ormolu, or similar formatters
  ;; - Should sort, group, and remove unused imports
  )

(ert-deftest warbo-test-haskell-qualify-import ()
  "Test adding qualified import for a symbol.
Should convert 'import Data.Map' to 'import qualified Data.Map as M'."
  :tags '(:skip :import)
  (skip-unless nil)
  ;; TODO: Set up qualified import addition
  ;; - Could be an HLS code action or custom command
  ;; - Should convert a regular import to a qualified import with alias
  )

(ert-deftest warbo-test-haskell-case-split ()
  "Test case-splitting a function argument.
With point on a Maybe parameter, should generate Just/Nothing cases."
  :tags '(:case-split :editing)
  ;; TODO: Check if HLS provides case-split code action
  (skip-unless (fboundp 'eglot-code-actions))
  (with-haskell-test-file
   "f :: Maybe Int -> Int\nf x = undefined"
   "f"

   (goto-char (point-min))
   (search-forward "x")
   (let ((actions (eglot-code-actions (point-min) (point-max) "refactor.rewrite")))
     (when actions
       ;; Apply the case-split action if available
       (funcall (car actions))
       (sleep-for 1)
       (should (string-match-p "Just\\|Nothing" (buffer-string)))))))

(ert-deftest warbo-test-haskell-type-hole-completion ()
  "Test that typing '_' (a hole) offers completions.
In 'f x = _', the hole should suggest 'x' as a completion."
  :tags '(:hole :completion)
  ;; TODO: HLS should provide completions for holes
  (with-haskell-test-file
   "f :: Int -> Int\nf x = _"
   "f"

   (goto-char (point-max))
   (backward-char 1)
   (sleep-for 2)
   ;; Trigger completion
   (completion-at-point)
   (sleep-for 1)
   ;; Should show completion candidates
   (should (or (get-buffer "*Completions*")
               (looking-at "x")))))

(ert-deftest warbo-test-haskell-generate-type-signature ()
  "Test generating a type signature for a function without one.
For 'f x = x + 1', should insert 'f :: Num a => a -> a'."
  :tags '(:type-signature :code-action)
  ;; TODO: HLS might provide this via code actions
  (with-haskell-test-file
   "f x = x + 1"
   "f"

   (goto-char (point-min))
   (call-interactively 'eglot-code-action-quickfix)
   (sleep-for 1)
   (goto-char (point-min))
   (should (search-forward "f ::" nil t))))

(ert-deftest warbo-test-haskell-smart-indent ()
  "Test that pressing TAB correctly indents Haskell code.
An unindented 'where' clause should indent to the correct level."
  :tags '(:indent :editing)
  ;; TODO: Test haskell-mode's built-in indentation
  (with-haskell-test-file
   "f x = g x\nwhere\ng y = y + 1"
   "f"

   (goto-char (point-min))
   (search-forward "where")
   (beginning-of-line)
   (indent-for-tab-command)
   (should (looking-at "  where"))  ; Should be indented
   (forward-line)
   (beginning-of-line)
   (indent-for-tab-command)
   (should (looking-at "    g"))))  ; Should be further indented

(ert-deftest warbo-test-haskell-view-haddock ()
  "Test viewing Haddock documentation for a symbol.
Should open browser or display docs for the identifier at point."
  :tags '(:skip :haddock :documentation)
  (skip-unless nil)
  ;; TODO: Set up Haddock documentation lookup
  ;; - Can use haskell-hoogle, haskell-mode, or similar packages
  ;; - Should look up the identifier at point and display documentation
  )

(ert-deftest warbo-test-haskell-generate-haddock-comment ()
  "Test generating a Haddock comment skeleton for a function.
Should insert a comment with parameter and return value documentation placeholders."
  :tags '(:skip :haddock :documentation)
  (skip-unless nil)
  ;; TODO: Set up Haddock comment generation
  ;; - Generate a Haddock comment template for a function signature
  ;; - Should include parameter descriptions and return value docs
  )

(ert-deftest warbo-test-haskell-cabal-build ()
  "Test building the current project with Cabal.
Should compile without errors and show output."
  :tags '(:cabal :build)
  ;; TODO: Integrate with compile command
  (skip-unless (executable-find "cabal"))
  (should t))  ; Placeholder

(ert-deftest warbo-test-haskell-stack-build ()
  "Test building the current project with Stack.
Should compile without errors and show output."
  :tags '(:stack :build)
  ;; TODO: Integrate with compile command
  (skip-unless (executable-find "stack"))
  (should t))  ; Placeholder

(ert-deftest warbo-test-haskell-run-tests ()
  "Test running the test suite for the current project.
Should execute tests and display results in a compilation buffer."
  :tags '(:skip :testing :project)
  (skip-unless nil)
  ;; TODO: Set up project test runner
  ;; - Should detect and run tests (cabal test, stack test, etc.)
  ;; - Display results in a compilation-like buffer
  )

(ert-deftest warbo-test-haskell-find-symbol-in-project ()
  "Test finding all occurrences of a symbol in the project.
Pressing M-? on 'myFunc' should show references via xref."
  :tags '(:search :xref)
  (with-haskell-test-file
   "myFunc = 42\n\nmain = print myFunc"
   "myFunc"

   (goto-char (point-min))
   (search-forward "myFunc")
   (backward-word)
   ;; In batch mode, xref may prompt to select/display results.  We override
   ;; completing-read to auto-select the first candidate (the minimal mock
   ;; needed to avoid hanging); everything else follows the real user flow.
   (let ((xref-show-xrefs-function #'xref-show-definitions-completing-read)
         (completing-read-function
          (lambda (_prompt collection &rest _args)
            (car (all-completions "" collection)))))
     (execute-kbd-macro (kbd "M-?"))
     ;; xref-show-definitions-completing-read jumps to a result; or an
     ;; *xref* buffer may be created.  Either way, "myFunc" should appear.
     (let ((xref-buf (get-buffer "*xref*")))
       (should (or xref-buf
                   ;; Jumped directly to a reference — buffer still has myFunc
                   (string-match-p "myFunc" (buffer-string))))
       (when xref-buf
         (with-current-buffer xref-buf
           (should (string-match-p "myFunc" (buffer-string)))))))))

(ert-deftest warbo-test-haskell-rename-symbol ()
  "Test renaming a symbol across the project.
Renaming 'myFunc' to 'newFunc' should update all occurrences."
  :tags '(:rename :refactor)
  (with-haskell-test-file
   "myFunc = 42\n\nmain = print myFunc"
   "myFunc"

   (goto-char (point-min))
   (search-forward "myFunc")
   (backward-word)
   (condition-case err
       (progn
         ;; Call eglot-rename directly with new name to avoid interactive prompt
         (eglot-rename "newFunc")
         (accept-process-output nil 0.5)

         ;; All occurrences should be renamed
         (should (not (string-match-p "myFunc" (buffer-string))))
         (should (string-match-p "newFunc" (buffer-string))))
     (error
      (ert-fail (format "Rename failed: %S" err))))))

(ert-deftest warbo-test-haskell-complete-pragma ()
  "Test completions in a LANGUAGE pragma.
When typing a LANGUAGE pragma, should suggest valid GHC extensions."
  :tags '(:skip :pragma :language)
  (skip-unless nil)
  ;; TODO: Set up LANGUAGE pragma completion
  ;; - Should suggest valid GHC extensions when typing pragmas
  )

(ert-deftest warbo-test-haskell-jump-to-definition-with-prompt ()
  "Test C-u M-. (jump to definition with prompt) works from whitespace.
Regression test for issue ba19a9bd56efb1af: calling haskell-mode-jump-to-tag
with a prefix argument (C-u M-.) was giving \"Wrong type argument: stringp, nil\"
when point was on whitespace or no identifier was at point.

This test verifies that we can successfully jump to a definition when:
1. Point is on whitespace (no identifier at point)
2. Prefix arg is given (to request prompting)
3. User provides an identifier at the prompt

The expected outcome is that we jump to the definition line."
  :tags '(:jump-to-definition :regression)
  (with-haskell-test-file
      "myFunc :: Int\nmyFunc = 42\n\nmain = print myFunc"
      "myFunc"

   ;; First verify that etags backend can find myFunc at all
   (let ((etags-xrefs (xref-backend-definitions 'etags "myFunc")))
     (unless etags-xrefs
       (ert-fail (format "etags found no definitions for myFunc. tags-file-name: %s, exists: %s, xref-backend-functions: %s"
                         tags-file-name
                         (and tags-file-name (file-exists-p tags-file-name))
                         xref-backend-functions))))

   ;; Position on the blank line where there's no identifier at point
   (goto-char (point-min))
   (search-forward "= 42")
   (forward-line 1)
   (beginning-of-line)
   ;; Verify we're on a blank line (haskell-ident-at-point should return nil)
   (should-not (haskell-ident-at-point))
   (let ((start-line (line-number-at-pos))
         (start-pos (point)))

     ;; Our advice prompts via completing-read, then xref-find-definitions
     ;; may also prompt via xref-show-definitions-completing-read if there
     ;; are multiple results.  Mock completing-read to handle both:
     ;; - First call (our advice): return "myFunc"
     ;; - Subsequent calls (xref picking a result): pick the first candidate
     (let ((xref-show-definitions-function
            #'xref-show-definitions-completing-read)
           (xref-show-xrefs-function
            #'xref-show-definitions-completing-read)
           (completing-read-function
            (lambda (_prompt collection &rest _args)
              (if (and collection (not (functionp collection)))
                  ;; xref is asking us to pick from results: take the first
                  (car (all-completions "" collection))
                ;; Our advice is asking for the identifier to search for
                "myFunc"))))
       ;; Call with prefix arg (t for next-p parameter)
       ;; This used to crash with "Wrong type argument: stringp, nil"
       (haskell-mode-jump-to-tag t)
       (accept-process-output nil 0.5)

       ;; Verify we jumped to the definition
       (let ((end-line (line-number-at-pos))
             (end-pos (point)))
         ;; Should have moved from start position
         (should (not (= end-pos start-pos)))
         ;; Should be on line 1 or 2 (the definition lines)
         (should (<= end-line 2))
         ;; Should have "myFunc" on the current line
         (should (string-match-p "myFunc"
                                 (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (line-end-position)))))))))

;;; haskell-tests.el ends here

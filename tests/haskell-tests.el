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

;;; New test stubs for multi-package and realistic scenarios

(ert-deftest warbo-test-haskell-multi-package-project ()
  "Test cross-package jump-to-definition in a multi-package cabal project."
  ;; TODO: Create a project with 2+ packages (e.g., lib and exe)
  ;; - Setup cabal.project file listing multiple .cabal files
  ;; - Package A exports a function, Package B imports and uses it
  ;; - Test jumping from B's usage to A's definition
  ;; - Verify HLS indexes all packages (may need to build first with cabal build)
  ;; - Check that diagnostics work across package boundaries
  ;; - Test that changes in A trigger re-check in B
  (ert-skip "TODO: Implement multi-package project test"))

(ert-deftest warbo-test-haskell-stack-project ()
  "Test Haskell Language Server with a Stack-based project."
  ;; TODO: Create stack.yaml with resolver and package list
  ;; - Test that HLS uses Stack's GHC and package set
  ;; - Verify stack.yaml takes precedence over .cabal in project root
  ;; - Test with stack-based dependencies (not in cabal's package list)
  ;; - Check that 'stack build' integration works
  ;; - May need to run 'stack setup' first in the test environment
  (ert-skip "TODO: Implement Stack project test"))

(ert-deftest warbo-test-haskell-import-completion ()
  "Test that completion suggests imported modules and functions."
  ;; TODO: Create file importing Data.List, test completion after typing "sor"
  ;; - Should suggest 'sort', 'sortBy', 'sortOn' from Data.List
  ;; - Test unqualified vs qualified imports (Data.List vs import qualified)
  ;; - Test that completion works for both module names (import Data.<TAB>)
  ;;   and function names (sor<TAB> after import)
  ;; - Use company-mode or completion-at-point to trigger completion
  ;; - May need to wait for HLS to finish indexing before completion works
  (ert-skip "TODO: Implement import completion test"))

(ert-deftest warbo-test-haskell-refactoring-rename ()
  "Test renaming a function across multiple occurrences."
  ;; TODO: Define function 'oldName' used in multiple places
  ;; - Position cursor on definition, invoke eglot-rename
  ;; - Verify all occurrences renamed to 'newName'
  ;; - Test with references across multiple functions/scopes
  ;; - Should preserve syntax and not break code
  ;; - Check that rename works for local variables, parameters, and top-level defs
  (ert-skip "TODO: Implement refactoring rename test"))

(ert-deftest warbo-test-haskell-repl-integration ()
  "Test loading Haskell modules into GHCi REPL."
  ;; TODO: Open a .hs file, start inferior-haskell-mode or similar
  ;; - Use C-c C-l or similar to load current buffer into GHCi
  ;; - Verify module loads without errors
  ;; - Test evaluating expressions in REPL that use module functions
  ;; - Check that REPL shows results correctly
  ;; - May need haskell-mode's interactive mode or dante/intero integration
  ;; - Consider whether this is config-specific (not all setups use REPL)
  (ert-skip "TODO: Implement REPL integration test"))

(ert-deftest warbo-test-haskell-documentation-lookup ()
  "Test accessing Haddock documentation for library functions."
  ;; TODO: Position cursor on 'map' or other Prelude function
  ;; - Invoke eglot-code-action or similar to show documentation
  ;; - Verify documentation buffer/popup contains type signature
  ;; - Check for description text from Haddock
  ;; - Test with both Prelude (built-in) and external package functions
  ;; - May use eldoc, hover, or dedicated doc command
  ;; - Note: Currently eldoc tests are failing, may need HLS initialization fix
  (ert-skip "TODO: Implement documentation lookup test"))

(ert-deftest warbo-test-haskell-type-at-point ()
  "Test displaying inferred types for expressions."
  ;; TODO: Write expression like 'let x = map (+1) [1,2,3]'
  ;; - Position cursor on 'x', request type info
  ;; - Should show 'x :: [Integer]' or similar
  ;; - Test on sub-expressions: 'map' should show '(a -> b) -> [a] -> [b]'
  ;; - Test on partial applications and complex expressions
  ;; - Use eglot's hover or dedicated type-at-point command
  ;; - Related to eldoc tests which are currently failing
  (ert-skip "TODO: Implement type-at-point test"))

(ert-deftest warbo-test-haskell-error-location-precision ()
  "Test that diagnostics point to correct line and column."
  ;; TODO: Create file with error at specific position (e.g., line 5 col 10)
  ;; - Introduce type error like 'x = "string" + 123'
  ;; - Verify flymake-diagnostics returns error at exact position
  ;; - Check that error underline/highlight covers correct span
  ;; - Test with multiple errors to ensure all are located correctly
  ;; - Verify column offsets match (zero-indexed vs one-indexed)
  (ert-skip "TODO: Implement error location precision test"))

(ert-deftest warbo-test-haskell-external-dependencies ()
  "Test projects with external dependencies from Hackage/Stackage."
  ;; TODO: Create .cabal with dependency like 'text' or 'aeson'
  ;; - Import from external package: 'import Data.Text'
  ;; - Test that HLS can resolve imports (may need 'cabal build' first)
  ;; - Verify jump-to-definition works for external package functions
  ;; - Check completion suggests functions from external packages
  ;; - Test that type errors involving external types are reported correctly
  ;; - May be slow if cabal needs to download/build dependencies
  ;; - Consider using common packages available in Nix environment
  (ert-skip "TODO: Implement external dependencies test"))

(ert-deftest warbo-test-haskell-cross-module-references ()
  "Test importing from local modules and navigating between them."
  ;; TODO: Create two files: Lib.hs (exports functions) and Main.hs (imports Lib)
  ;; - Main.hs: 'import Lib' and use functions from Lib
  ;; - Test jump-to-definition from Main to Lib
  ;; - Test that changes in Lib trigger diagnostics in Main
  ;; - Verify completion in Main suggests Lib's exported functions
  ;; - Test with qualified imports: 'import qualified Lib as L'
  ;; - Check that refactoring/rename works across module boundaries
  ;; - Ensure HLS tracks both files and updates appropriately
  (ert-skip "TODO: Implement cross-module references test"))

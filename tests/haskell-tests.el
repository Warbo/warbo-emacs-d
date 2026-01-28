;; -*- lexical-binding: t; -*-
(require 'ert)
(require 'eglot)
(require 'haskell-mode)
(require 'flymake)

(defmacro with-haskell-eglot-test (content &rest body)
  "Create a temporary Haskell file with CONTENT, open it and run BODY."
  (declare (indent 1))
  `(let ((file (make-temp-file "test-hls-" nil ".hs"))
         (buffer (current-buffer)))
     (unwind-protect
         (progn
           (with-current-buffer (find-file file)
             (should (equal major-mode 'haskell-mode))
             (insert ,content)
             (save-buffer)
             ;;(haskell-mode)
             (eglot-ensure)
             (with-timeout (10 (ert-fail "Timed out waiting for Eglot to manage buffer"))
               (while (not (eglot-managed-p))
                 (accept-process-output nil 0.2)))
             (sleep-for 1.5) ;; Let HLS analyse the file
             ,@body))
       (let ((buf (find-buffer-visiting file)))
         (when buf (kill-buffer buf)))
       (delete-file file))))

(ert-deftest warbo-test-haskell-eglot-command-is-executable ()
  "Test that the command Eglot resolves for Haskell actually exists."
  (with-temp-buffer
    (setq buffer-file-name "test.hs")
    (haskell-mode)

    ;; Ask Eglot what it intends to run (returns (MODE PROJ CLASS CONTACT))
    (let ((guessed (eglot--guess-contact)))
      (should (not (null guessed)))

      ;; Extract the executable from the contact info
      ;; contact-info is usually ("program" "arg1" ...) or (host port)
      (let* ((contact-info (nth 3 guessed))
             (program (if (listp contact-info)
                          (car contact-info)
                        contact-info)))
        (should (stringp program))
        (should (executable-find program))))))

(ert-deftest test-diagnose-ensure-failure ()
  "Diagnose why eglot-ensure exits early.
Checks:
1. Is 'haskell-mode' ignored via eglot-stay-out-of?
2. Does manually calling eglot--guess-contact work?
3. Does eglot-ensure trigger an error in *Messages*?"
  (let* ((dir (make-temp-file "test-diag-" t))
         (file (expand-file-name "Main.hs" dir))
         (msg-start (with-current-buffer (messages-buffer) (point-max))))
    (unwind-protect
        (progn
          ;; Setup Git project
          (let ((default-directory dir))
            (call-process "git" nil nil nil "init"))

          (with-current-buffer (find-file file)
            ;; 1. CHECK IGNORES
            (when (and (boundp 'eglot-stay-out-of)
                       (member 'haskell-mode eglot-stay-out-of))
              (ert-fail "FAIL: haskell-mode is listed in 'eglot-stay-out-of'."))

            ;; 2. CHECK GUESSING MANUALLY
            ;; This tells us if your config is valid, independent of the hook logic.
            (let ((contact (ignore-errors (eglot--guess-contact))))
              (unless contact
                (ert-fail (format "FAIL: Manual eglot--guess-contact returned nil. \nConfig: %S"
                                  (assoc 'haskell-mode eglot-server-programs)))))

            ;; 3. TRIGGER ENSURE & CHECK MESSAGES
            ;; We call it manually to catch the error directly if possible
            (condition-case err
                (eglot-ensure)
              (error (ert-fail (format "FAIL: eglot-ensure crashed explicitly: %S" err))))

            ;; Allow async startup
            (sleep-for 0.5)

            ;; Check if it worked
            (unless (eglot-managed-p)
              ;; IF NOT: Dump *Messages* to see if the hook swallowed an error
              (let ((logs (with-current-buffer (messages-buffer)
                            (buffer-substring-no-properties msg-start (point-max)))))
                (ert-fail (format "FAIL: eglot-ensure ran but did not start server.\n\n--- *MESSAGES* ---\n%s"
                                  (if (string-empty-p logs) "[No logs produced]" logs)))))))
      (delete-directory dir t))))

(ert-deftest test-force-connection ()
  "Bypass eglot-ensure and force a connection to see if it explodes.
This determines if the issue is 'Starting' vs 'Deciding to start'."
  (let* ((dir (make-temp-file "test-force-" t))
         (file (expand-file-name "Main.hs" dir)))
    (unwind-protect
        (progn
          ;; 1. Setup Project
          (let ((default-directory dir))
            (call-process "git" nil nil nil "init"))

          (with-current-buffer (find-file file)
            ;; 2. Get contact info (we know this works from previous tests)
            (let ((contact (eglot--guess-contact)))
              (unless contact
                (ert-fail "Setup error: eglot--guess-contact is nil"))

              ;; 3. FORCE CONNECTION
              ;; We call the internal function eglot--connect directly.
              ;; This takes (PROJECT CLASS CONTACT LANGUAGE-IDS).
              ;; eglot--guess-contact returns (MANAGED-MODE PROJECT CLASS CONTACT)
              ;; Note: API for eglot--guess-contact changed slightly in versions,
              ;; but usually returns list.

              (condition-case err
                  ;; Emacs 29/30 signature for eglot--connect is (PROJECT CLASS CONTACT LANGUAGES)
                  (eglot--connect (nth 1 contact) ;; Project
                                  (nth 2 contact) ;; Class
                                  (nth 3 contact) ;; Contact info
                                  (list (nth 0 contact))) ;; Language ID
                (error
                 (ert-fail (format "FORCE CONNECT CRASHED: %S" err))))

            ;; 4. Wait for async startup
            (sleep-for 1)

            ;; 5. Check status
            (if (eglot-managed-p)
                (should (eglot-managed-p)) ;; Pass
              ;; If we are here, connect ran but didn't result in a managed buffer
              (ert-fail (format "Force connect ran, but buffer is not managed.\nEvents: %S"
                                (when (eglot-current-server)
                                  (with-current-buffer (eglot-events-buffer (eglot-current-server))
                                    (buffer-string)))))))))
      ;; Cleanup
      (delete-directory dir t))))

(ert-deftest test-debug-eglot-internals ()
  "Diagnose the silent failure of eglot-ensure and inspect eglot--connect."
  (let* ((dir (make-temp-file "test-env-" t))
         (file (expand-file-name "Main.hs" dir))
         (debug-log '()))

    (cl-flet ((log (fmt &rest args)
                   (push (apply #'format fmt args) debug-log)))

      (unwind-protect
          (progn
            ;; 1. Inspect eglot--connect signature
            (let ((arity (func-arity 'eglot--connect)))
              (log "DEBUG: eglot--connect arity: %S" arity))

            ;; 2. Monkey-patch eglot-ensure to trace its logic
            (advice-add 'eglot-ensure :override
                        (lambda ()
                          (log "TRACE: eglot-ensure called")
                          (cond
                           ((eglot-managed-p)
                            (log "TRACE: Quit because already managed"))
                           ((and (boundp 'eglot-stay-out-of)
                                 (member major-mode eglot-stay-out-of))
                            (log "TRACE: Quit because mode in eglot-stay-out-of"))
                           ((not buffer-file-name)
                            (log "TRACE: Quit because no buffer-file-name"))
                           (t
                            (let ((contact (eglot--guess-contact)))
                              (if (not contact)
                                  (log "TRACE: Quit because eglot--guess-contact returned nil")
                                (log "TRACE: Contact found: %S. Attempting connect..." contact)
                                ;; Try to call the original internal connector
                                (condition-case err
                                    (eglot--connect (nth 1 contact)
                                                    (nth 2 contact)
                                                    (nth 3 contact)
                                                    (list (nth 0 contact)))
                                  (error
                                   (log "TRACE: Crash during connect: %S" err)))))))))

            ;; 3. Run the setup
            (let ((default-directory dir))
              (call-process "git" nil nil nil "init"))

            (with-current-buffer (find-file file)
              ;; Trigger the hook (which calls our patched ensure)
              (haskell-mode)
              ;; Wait a tick
              (sleep-for 0.5))

            ;; 4. Fail to dump logs
            (ert-fail (mapconcat #'identity (nreverse debug-log) "\n")))

        ;; Cleanup
        (advice-remove 'eglot-ensure (advice-function-member 'eglot-ensure 'override))
        (delete-directory dir t)))))

(ert-deftest warbo-test-haskell-mode-enables-eglot ()
  "Test that opening a Haskell file in a Git project enables Eglot."
  (let* ((dir (make-temp-file "test-git-project-" t))
         (file (expand-file-name "Main.hs" dir))
         (msg-start (with-current-buffer (messages-buffer) (point-max))))
    (unwind-protect
        (progn
          (let ((default-directory dir))
            (call-process "git" nil nil nil "init"))
          (with-current-buffer (find-file file)
            (with-timeout
                (5 (let ((debug-msg
                          (format
                           "%s\n"
                           `((buffer-name ,(buffer-name))
                             (buffer-file-name ,buffer-file-name)
                             (major-mode ,major-mode)
                             (project-current ,(ignore-errors (project-current)))
                             (haskell-mode-hook ,haskell-mode-hook)
                             (eglot-server-program ,(assoc 'haskell-mode eglot-server-programs))
                             (eglot-managed-p ,(eglot-managed-p))
                             (eglot-current-server
                              ,(if (fboundp 'eglot-current-server) (eglot-current-server) "n/a"))
                             (eglot-events-buffer
                              ,(if (fboundp 'eglot-events-buffer)
                                   (ignore-errors (buffer-name (eglot-events-buffer (eglot-current-server))))
                                 "n/a"))
                             (messages
                              ,(with-current-buffer (messages-buffer)
                                 (buffer-substring-no-properties msg-start (point-max))))))))
                     (ert-fail (concat "Eglot did not enable within 5s." debug-msg))))
              (while (not (eglot-managed-p))
                (sleep-for 0.1)))
            (should (eglot-managed-p))))
      (let ((buf (get-file-buffer file)))
        (when buf (kill-buffer buf)))
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
  "Test that HLS detects a type error."
  (with-haskell-eglot-test
   "main :: IO ()\nmain = putStrLn 42" ;; Error: Num is not String

   (with-timeout (5 (ert-fail "Timed out waiting for diagnostics"))
     (while (null (flymake-diagnostics))
       (accept-process-output nil 0.5)))

   (let ((diags (flymake-diagnostics)))
     (should (> (length diags) 0))
     (should (string-match-p "No instance for" (flymake-diagnostic-text (car diags)))))))

(ert-deftest warbo-test-haskell-eglot-formatting ()
  "Test that Eglot formats a buffer with sloppy whitespace."
  (with-haskell-eglot-test
   "foo=   5"

   (eglot-format-buffer)
   (sleep-for 0.5)
   (should (string-match-p "foo = 5" (buffer-string)))))

(ert-deftest warbo-test-haskell-eglot-docs ()
  "Test that we can get type information for standard functions."
  (with-haskell-eglot-test
   "main = putStrLn \"hello\""

   (goto-char (point-min))
   (search-forward "putStrLn")
   (backward-char 1)
   (let ((doc-string (eglot-help-at-point)))
     (should (stringp doc-string))
     (should (string-match-p "String -> IO ()" doc-string)))))

(ert-deftest warbo-test-haskell-eglot-find-definition ()
  "Test jumping to a definition within the file."
  (with-haskell-eglot-test
   "myFunc :: Int\nmyFunc = 10\n\nmain = print myFunc"

   (goto-char (point-max))
   (backward-word 1)
   (xref-find-definitions)
   (should (looking-at "myFunc"))))

(ert-deftest warbo-test-haskell-eglot-code-action ()
  "Test applying a code action (adding a missing signature)."
  (with-haskell-eglot-test
   "f x = x"

   (with-timeout (5 (ert-fail "Waiting for missing-sig warning"))
     (while (null (flymake-diagnostics))
       (accept-process-output nil 0.5)))
   (goto-char (point-min))
   (let ((actions (eglot-code-actions (point-min) (point-max) "quickfix")))
     (should (> (length actions) 0))
     (eglot-execute (car actions))
     (sleep-for 0.5)
     (goto-char (point-min))
     (should (looking-at "f :: ")))))

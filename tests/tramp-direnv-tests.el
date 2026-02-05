;;; Tests for TRAMP and direnv interaction
;;;
;;; Regression test for issue bc0929953fb2b107:
;;; direnv should not complain when opening remote files via TRAMP
;;;
;;; These tests use a custom "dummy" TRAMP method that accesses local files
;;; without requiring any authentication, making them suitable for automated
;;; testing.

(require 'ert)
(require 'tramp)
(require 'direnv)

;;; ----------------------------------------------------------------------------
;;; Dummy TRAMP method setup
;;; ----------------------------------------------------------------------------

(defvar tramp-direnv-test-method "dummy"
  "TRAMP method to use for testing.
Uses a custom 'dummy' method that provides local loopback access
without requiring any authentication.")

(defun tramp-direnv-test-setup-dummy-method ()
  "Set up a dummy TRAMP method for testing.
This method accesses local files through TRAMP without requiring
authentication, making it suitable for automated testing."
  (unless (assoc "dummy" tramp-methods)
    (add-to-list 'tramp-methods
                 '("dummy"
                   (tramp-login-program "sh")
                   (tramp-login-args (("-i")))
                   (tramp-remote-shell "/bin/sh")
                   (tramp-remote-shell-login ("-l"))
                   (tramp-remote-shell-args ("-c"))
                   (tramp-connection-timeout 10))))
  ;; Also need to register it with tramp-default-host-alist so that
  ;; empty host works (e.g., /dummy::/path)
  (unless (assoc "dummy" tramp-default-host-alist)
    (add-to-list 'tramp-default-host-alist '("dummy" nil "localhost"))))

(defun tramp-direnv-test-cleanup-dummy-connections ()
  "Clean up any TRAMP connections made during testing."
  (when (fboundp 'tramp-cleanup-all-connections)
    (tramp-cleanup-all-connections)))

;;; ----------------------------------------------------------------------------
;;; Test utilities
;;; ----------------------------------------------------------------------------

(defvar tramp-direnv-test-messages nil
  "List to capture messages during tests.")

(defun tramp-direnv-test-capture-message (orig-fun &rest args)
  "Capture messages to `tramp-direnv-test-messages' instead of displaying them."
  (let ((msg (apply #'format-message args)))
    (push msg tramp-direnv-test-messages)
    msg))

(defun tramp-direnv-test-capture-user-error (orig-fun &rest args)
  "Capture user-error calls instead of signaling them."
  (let ((msg (apply #'format-message args)))
    (push (cons 'user-error msg) tramp-direnv-test-messages)
    (signal 'user-error (list msg))))

(defmacro tramp-direnv-test-with-captured-output (&rest body)
  "Execute BODY while capturing all messages and errors."
  `(let ((tramp-direnv-test-messages nil))
     (unwind-protect
         (progn
           (advice-add 'message :around #'tramp-direnv-test-capture-message)
           (advice-add 'user-error :around #'tramp-direnv-test-capture-user-error)
           (condition-case err
               (progn ,@body)
             (user-error
              ;; user-error already captured, just continue
              nil)
             (error
              ;; Capture unexpected errors
              (push (cons 'error (error-message-string err)) tramp-direnv-test-messages))))
       (advice-remove 'message #'tramp-direnv-test-capture-message)
       (advice-remove 'user-error #'tramp-direnv-test-capture-user-error))
     tramp-direnv-test-messages))

(defun tramp-direnv-test-has-direnv-error-p (messages)
  "Check if MESSAGES contains the direnv remote files error."
  (seq-some (lambda (msg)
              (let ((text (if (consp msg) (cdr msg) msg)))
                (string-match-p "Cannot use direnv for remote files" text)))
            messages))

;;; ----------------------------------------------------------------------------
;;; Tests
;;; ----------------------------------------------------------------------------

(ert-deftest warbo-tramp-direnv-opening-remote-file-should-not-error ()
  "Opening a file via TRAMP should not cause direnv to complain.

Regression test for issue bc0929953fb2b107.

This test uses a dummy TRAMP method that provides local loopback
access without requiring authentication."
  :tags '(tramp direnv regression)

  ;; Set up the dummy TRAMP method
  (tramp-direnv-test-setup-dummy-method)

  (let* ((test-dir (make-temp-file "tramp-direnv-test-" t))
         (envrc-file (expand-file-name ".envrc" test-dir))
         (test-file (expand-file-name "test.txt" test-dir))
         (tramp-test-file (format "/%s::%s" tramp-direnv-test-method test-file))
         (test-buffer nil)
         ;; Reduce TRAMP verbosity for cleaner test output
         (tramp-verbose 0))

    (unwind-protect
        (progn
          ;; Set up test directory with .envrc
          (with-temp-file envrc-file
            (insert "export TEST_VAR=hello\n"))

          ;; Create a test file
          (with-temp-file test-file
            (insert "This is a test file for TRAMP+direnv testing.\n"))

          ;; Try to open the file via TRAMP and capture any messages/errors
          (let ((messages (tramp-direnv-test-with-captured-output
                           (setq test-buffer (find-file-noselect tramp-test-file)))))

            ;; The test passes if we didn't get the direnv error
            (when (tramp-direnv-test-has-direnv-error-p messages)
              (let ((all-msgs (mapconcat (lambda (m)
                                           (if (consp m)
                                               (format "%s: %s" (car m) (cdr m))
                                             m))
                                         (reverse messages)
                                         "\n")))
                (ert-fail (format "Direnv complained about remote files.\n\nCaptured messages:\n%s\n\nBuffer: %s\nFile: %s\nTRAMP file: %s"
                                  all-msgs
                                  test-buffer
                                  test-file
                                  tramp-test-file))))))

      ;; Clean up
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer))
      (when (file-exists-p test-dir)
        (delete-directory test-dir t))
      (tramp-direnv-test-cleanup-dummy-connections))))

(ert-deftest warbo-tramp-direnv-switching-to-remote-buffer-should-not-error ()
  "Switching to a buffer visiting a TRAMP file should not cause direnv to complain.

Regression test for issue bc0929953fb2b107.

This tests the scenario where direnv-mode is active and we switch to
a buffer that's visiting a file via TRAMP. The direnv hooks should
handle this gracefully without errors."
  :tags '(tramp direnv regression)

  ;; Set up the dummy TRAMP method
  (tramp-direnv-test-setup-dummy-method)

  (let* ((test-dir (make-temp-file "tramp-direnv-test-" t))
         (envrc-file (expand-file-name ".envrc" test-dir))
         (test-file (expand-file-name "test.py" test-dir))  ; Use .py to trigger prog-mode
         (tramp-test-file (format "/%s::%s" tramp-direnv-test-method test-file))
         (test-buffer nil)
         (other-buffer nil)
         ;; Reduce TRAMP verbosity for cleaner test output
         (tramp-verbose 0))

    (unwind-protect
        (progn
          ;; Set up test directory with .envrc
          (with-temp-file envrc-file
            (insert "export TEST_VAR=hello\n"))

          ;; Create a test Python file (to trigger prog-mode and direnv hooks)
          (with-temp-file test-file
            (insert "# Test Python file\nprint('hello')\n"))

          ;; Create another local buffer to switch from
          (setq other-buffer (generate-new-buffer "*tramp-test-temp*"))
          (switch-to-buffer other-buffer)

          ;; Open the TRAMP file and switch to it
          (let ((messages (tramp-direnv-test-with-captured-output
                           (setq test-buffer (find-file-noselect tramp-test-file))
                           (switch-to-buffer test-buffer)
                           ;; Simulate some user interaction that might trigger direnv
                           (goto-char (point-min)))))

            ;; The test passes if we didn't get the direnv error
            (when (tramp-direnv-test-has-direnv-error-p messages)
              (let ((all-msgs (mapconcat (lambda (m)
                                           (if (consp m)
                                               (format "%s: %s" (car m) (cdr m))
                                             m))
                                         (reverse messages)
                                         "\n")))
                (ert-fail (format "Direnv complained when switching to remote buffer.\n\nCaptured messages:\n%s\n\nBuffer: %s\nMode: %s\nFile: %s\nTRAMP file: %s"
                                  all-msgs
                                  test-buffer
                                  (with-current-buffer test-buffer major-mode)
                                  test-file
                                  tramp-test-file))))))

      ;; Clean up
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer))
      (when (buffer-live-p other-buffer)
        (kill-buffer other-buffer))
      (when (file-exists-p test-dir)
        (delete-directory test-dir t))
      (tramp-direnv-test-cleanup-dummy-connections))))

(provide 'tramp-direnv-tests)

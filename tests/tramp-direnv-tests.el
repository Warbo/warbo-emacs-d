;;; Tests for TRAMP and direnv interaction
;;;
;;; Regression test for issue bc0929953fb2b107:
;;; direnv should not complain when opening remote files via TRAMP
;;;
;;; NOTE: These tests require a TRAMP method that works without interactive
;;; authentication. Options include:
;;; - /sudo:: if sudo is configured for passwordless access (NOPASSWD in sudoers)
;;; - SSH to localhost with key-based authentication configured
;;; - A custom TRAMP method configured for local testing
;;;
;;; If running these tests interactively requires entering a password,
;;; you may want to set up passwordless sudo for testing or skip these tests.

(require 'ert)
(require 'tramp)
(require 'direnv)

(defvar tramp-direnv-test-method "sudo"
  "TRAMP method to use for testing.
Should be a method that provides local loopback access without
requiring interactive authentication. Common options:
  - \"sudo\" (if passwordless sudo is configured)
  - \"su\" (if passwordless su is configured)
  - A custom method configured specifically for testing")

(defun tramp-direnv-test-can-access-without-password-p ()
  "Check if we can access files via TRAMP without a password prompt.
Returns t if passwordless TRAMP access works, nil otherwise."
  (condition-case nil
      (let* ((test-file "/tmp/.tramp-test-canary")
             (tramp-path (format "/%s::%s" tramp-direnv-test-method test-file))
             ;; Suppress all TRAMP messages and prompts
             (tramp-verbose 0)
             (process-connection-type nil))
        ;; Try to check if file exists via TRAMP with a timeout
        (with-timeout (2 nil)  ; 2 second timeout
          (file-exists-p tramp-path)))
    (error nil)))

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
         (advice-add 'message :around #'tramp-direnv-test-capture-message)
       (advice-add 'user-error :around #'tramp-direnv-test-capture-user-error)
       (condition-case err
           (progn ,@body)
         (error
          ;; Capture the error
          (push (cons 'error (error-message-string err)) tramp-direnv-test-messages)
          ;; Re-signal if it's not a user-error we already captured
          (unless (eq (car err) 'user-error)
            (signal (car err) (cdr err))))))
     (advice-remove 'message #'tramp-direnv-test-capture-message)
     (advice-remove 'user-error #'tramp-direnv-test-capture-user-error)
     tramp-direnv-test-messages))

(defun tramp-direnv-test-has-direnv-error-p (messages)
  "Check if MESSAGES contains the direnv remote files error."
  (seq-some (lambda (msg)
              (let ((text (if (consp msg) (cdr msg) msg)))
                (string-match-p "Cannot use direnv for remote files" text)))
            messages))

(ert-deftest tramp-direnv-opening-remote-file-should-not-error ()
  "Opening a file via TRAMP should not cause direnv to complain.

Regression test for issue bc0929953fb2b107.

This test uses a local TRAMP method that acts as a loopback.
It will be skipped if passwordless TRAMP access is not available."
  :tags '(tramp direnv regression)

  (skip-unless (tramp-direnv-test-can-access-without-password-p))

  (let* ((test-dir (make-temp-file "tramp-direnv-test-" t))
         (envrc-file (expand-file-name ".envrc" test-dir))
         (test-file (expand-file-name "test.txt" test-dir))
         (tramp-test-file (format "/%s::%s" tramp-direnv-test-method test-file))
         (test-buffer nil))

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
        (delete-directory test-dir t)))))

(ert-deftest tramp-direnv-switching-to-remote-buffer-should-not-error ()
  "Switching to a buffer visiting a TRAMP file should not cause direnv to complain.

Regression test for issue bc0929953fb2b107.

This tests the scenario where direnv-mode is active and we switch to
a buffer that's visiting a file via TRAMP. The direnv hooks should
handle this gracefully without errors.
It will be skipped if passwordless TRAMP access is not available."
  :tags '(tramp direnv regression)

  (skip-unless (tramp-direnv-test-can-access-without-password-p))

  (let* ((test-dir (make-temp-file "tramp-direnv-test-" t))
         (envrc-file (expand-file-name ".envrc" test-dir))
         (test-file (expand-file-name "test.py" test-dir))  ; Use .py to trigger prog-mode
         (tramp-test-file (format "/%s::%s" tramp-direnv-test-method test-file))
         (test-buffer nil)
         (other-buffer nil))

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
        (delete-directory test-dir t)))))

(provide 'tramp-direnv-tests)

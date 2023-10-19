(defun warbo-wait-for-comint (comint-buffer)
  (with-temp-buffer
    (let ((temp-buffer (current-buffer)))
      (with-current-buffer comint-buffer
        ;; Run another command in comint-buffer, but send its output to
        ;; temp-buffer. That way we can eaily wait for it.
        (comint-redirect-send-command-to-process
         "true" ;; Command to run
         temp-buffer
         (get-buffer-process comint-buffer)
         ;; Don't echo and don't show temp-buffer
         nil t)

        ;; Busy-loop until the redirected command has finished
        (while (not comint-redirect-completed)
          (sleep-for 0.1))))))

;; Only bother checking shell directories if we're on my laptop ;)
(when (equal (getenv "HOME") "/home/chris")
  (ert-deftest warbo-shells-exist ()
    "Shell directories exist"
    (dolist (pair startup-shells)
      (should (file-accessible-directory-p (cadr pair)))))

  (ert-deftest warbo-shells-open ()
    "Startup shells are open"
    (dolist (pair startup-shells)
      (should (member (car pair) (mapcar 'buffer-name (buffer-list)))))))

(ert-deftest warbo-shell-unique ()
  "Shell names don't overlap"
  (with-temp-buffer
    (rename-buffer "*test-buffer-1*")
    (should (equal (free-name-num "test-buffer") "*test-buffer-2*"))))

(ert-deftest warbo-shell-query-working-directory ()
  "Explicitly update the working directory"
  (save-excursion
    (let ((shell-buf (bash)))
      (with-current-buffer shell-buf
        (insert "ls > /dev/null && cd \"$(echo /)\"")
        (comint-send-input)
        (warbo-wait-for-comint shell-buf)

        (should (equal "/" (dirs)))))))

(ert-deftest warbo-shell-tracks-working-directory ()
  "Moving around the filesystem should get tracked by shell-mode"
  (save-excursion
    (let ((shell-buf (bash)))
      (with-current-buffer shell-buf
        ;; Change directory to /, but written in a way that's hard for Emacs to
        ;; guess what will happen (the argument to 'cd' is computed, rather than
        ;; being written literally; and 'cd' will only get called if the initial
        ;; 'ls' call succeeds)
        (comint-send-string
         (get-buffer-process shell-buf)
         "ls > /dev/null && cd \"$(echo /)\"\n")
        (warbo-wait-for-comint shell-buf)

        ;; Check that we indeed changed directory to /
        (should (equal "/" default-directory))))))

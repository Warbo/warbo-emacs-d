(defun warbo-wait-for-comint (comint-buffer)
  "Wait for the prompt in COMINT-BUFFER, then run a command to sync."
  (with-current-buffer comint-buffer
    ;; Wait for the initial prompt to appear.
    (let ((process (get-buffer-process comint-buffer))
          (timeout-start (float-time)))
      (while (and (not (save-excursion
                         (goto-char (point-max))
                         (re-search-backward comint-prompt-regexp nil t)))
                  (< (- (float-time) timeout-start) 4.0)) ; 4s timeout
        (accept-process-output process 0.1)))
    (unless (save-excursion
              (goto-char (point-max))
              (re-search-backward comint-prompt-regexp nil t))
      (error "Prompt not found in %s" comint-buffer)))

  ;; Run a sync command and wait for it to finish.
  (with-temp-buffer
    (let ((temp-buffer (current-buffer)))
      (with-current-buffer comint-buffer
        (comint-redirect-send-command-to-process
         "true" ;; Command to run
         temp-buffer
         (get-buffer-process comint-buffer)
         nil t) ;; Don't echo and don't show temp-buffer

        ;; Busy-loop until the redirected command has finished
        (while (not comint-redirect-completed)
          (sleep-for 0.1))))))

(defmacro in-shell-buffer (&rest body)
  "Run BODY in a new shell buffer, with setup and teardown."
  `(let ((shell-buf (bash)))
     (unwind-protect
          (with-current-buffer shell-buf
            (warbo-wait-for-comint shell-buf)
            ,@body)
       (let ((kill-buffer-query-functions nil))
         (kill-buffer shell-buf)))))

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
  "Shell names don't overlap."
  (with-temp-buffer
    (rename-buffer "*test-buffer-1*")
    (should (equal (free-name-num "test-buffer") "*test-buffer-2*"))))

;; FIXME: These seem to do an infinite loop
;; (ert-deftest warbo-shell-query-working-directory ()
;;   "Explicitly update the working directory"
;;   (save-excursion
;;     (let ((shell-buf (bash)))
;;       (with-current-buffer shell-buf
;;         (insert "ls > /dev/null && cd \"$(echo /)\"")
;;         (comint-send-input)
;;         (warbo-wait-for-comint shell-buf)

;;         (should (equal "/" (dirs)))))))

;; (ert-deftest warbo-shell-tracks-working-directory ()
;;   "Moving around the filesystem should get tracked by shell-mode"
;;   (save-excursion
;;     (let ((shell-buf (bash)))
;;       (with-current-buffer shell-buf
;;         ;; Change directory to /, but written in a way that's hard for Emacs to
;;         ;; guess what will happen (the argument to 'cd' is computed, rather than
;;         ;; being written literally; and 'cd' will only get called if the initial
;;         ;; 'ls' call succeeds)
;;         (comint-send-string
;;          (get-buffer-process shell-buf)
;;          "ls > /dev/null && cd \"$(echo /)\"\n")
;;         (warbo-wait-for-comint shell-buf)

;;         ;; Check that we indeed changed directory to /
;;         (should (equal "/" default-directory))))))

(defmacro should-line (expected)
  "Assert that the current line's content equals EXPECTED."
  `(should (string-equal
            ,expected
            (buffer-substring-no-properties
             (line-beginning-position)
             (line-end-position)))))

(ert-deftest warbo-shell-up-down-move-point ()
  "Up and down cursor keys should move point, not cycle history."
  (in-shell-buffer
   (goto-char (point-min))
   (insert "line 1\nline 2\n")
   (goto-char (point-max)) ; Move to prompt
   (re-search-backward comint-prompt-regexp nil t) ; Move to beginning of prompt

   (let* ((prompt-line-number (line-number-at-pos))
          (prompt-line-content
           (buffer-substring-no-properties
            (line-beginning-position)
            (line-end-position))))

     (execute-kbd-macro (kbd "<up>"))
     (should (= (1- prompt-line-number) (line-number-at-pos)))
     (should (string-equal
              "line 2"
              (string-trim (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position)))))

     (execute-kbd-macro (kbd "<down>"))
     (should (= prompt-line-number (line-number-at-pos)))

     ;; Verify prompt line is unchanged (no history cycling)
     (should-line prompt-line-content))))

(ert-deftest warbo-shell-C-up-down-cycle-history ()
  "C-up and C-down should cycle history when on the command line."
  (in-shell-buffer
   (insert "echo command1")
   (comint-send-input)
   (warbo-wait-for-comint (current-buffer))
   (insert "echo command2")
   (comint-send-input)
   (warbo-wait-for-comint (current-buffer))

   (execute-kbd-macro (kbd "C-<up>"))
   (should-line "echo command2")

   (execute-kbd-macro (kbd "C-<up>"))
   (should-line "echo command1")

   (execute-kbd-macro (kbd "C-<down>"))
   (should-line "echo command2")))

(ert-deftest warbo-shell-C-a-at-prompt ()
  "C-a at the prompt should move to the beginning of the command."
  (in-shell-buffer
   (insert "some text")
   (execute-kbd-macro (kbd "C-a"))
   (should (= (point) (save-excursion (comint-bol) (point))))))

(ert-deftest warbo-shell-C-a-elsewhere ()
  "C-a elsewhere should move to the first non-whitespace character."
  (in-shell-buffer
   (insert "  some text")
   (goto-char (point-max))
   (execute-kbd-macro (kbd "C-a"))
   (should (= (point) (+ (line-beginning-position) 2)))))

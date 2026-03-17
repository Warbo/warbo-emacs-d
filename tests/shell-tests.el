(defun warbo-wait-for-comint (comint-buffer)
  "Wait for the prompt in COMINT-BUFFER, then run a command to sync."
  (with-current-buffer comint-buffer
    ;; Wait for the initial prompt to appear.
    (let ((process (get-buffer-process comint-buffer))
          (timeout-start (float-time))
          (prompt-found nil))
      ;; Give the process initial time to start producing output
      (dbg "warbo-wait-for-comint: Starting, buffer=%s" comint-buffer)
      (accept-process-output process 1)
      (dbg "warbo-wait-for-comint: After initial accept-process-output, buffer content length=%d" (length (buffer-string)))
      (dbg "warbo-wait-for-comint: Buffer content: %s" (buffer-string))
      (while (and (not prompt-found)
                  (< (- (float-time) timeout-start) 4.0)) ; 4s timeout
        (save-excursion
          (goto-char (point-max))
          ;; Search backwards from the end for the prompt
          ;; Only accept it if it's at the very end of the buffer
          (when (re-search-backward comint-prompt-regexp nil t)
            (goto-char (match-end 0))
            (when (= (point) (point-max))
              (setq prompt-found t))))
        (unless prompt-found
          (accept-process-output process 0.1)))
      (unless prompt-found
        (error "Prompt not found in %s" comint-buffer)))))

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
(ert-deftest comint-prompt-regexp-is-defined ()
  "Check that comint-prompt-regexp is defined and has a value."
  (should (boundp 'comint-prompt-regexp))
  (should comint-prompt-regexp)
  (dbg "comint-prompt-regexp value: %s" comint-prompt-regexp))

(ert-deftest shell-buffer-contains-text ()
  "Check that a shell buffer actually contains text."
  (let ((shell-buf (bash)))
    (unwind-protect
        (with-current-buffer shell-buf
          ;; Give the shell process time to initialize
          (accept-process-output (get-buffer-process shell-buf) 1)
          (let ((buffer-content (buffer-string)))
            (should (> (length buffer-content) 0))
            (dbg "Shell buffer content length: %d" (length buffer-content))
            (dbg "Shell buffer content: %s" buffer-content)))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer shell-buf)))))

(ert-deftest prompt-regexp-matches-in-shell-buffer ()
  "Check if comint-prompt-regexp actually matches anything in a shell buffer."
  (let ((shell-buf (bash)))
    (unwind-protect
        (with-current-buffer shell-buf
          (accept-process-output (get-buffer-process shell-buf) 1)
          (let ((buffer-content (buffer-string)))
            (if (string-match comint-prompt-regexp buffer-content)
                (dbg "Prompt regexp MATCHED in buffer")
              (dbg "Prompt regexp DID NOT MATCH in buffer"))
            (should (string-match comint-prompt-regexp buffer-content))))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer shell-buf)))))

(ert-deftest find-prompt-with-re-search-forward ()
  "Check if we can find the prompt using re-search-forward."
  (let ((shell-buf (bash)))
    (unwind-protect
        (with-current-buffer shell-buf
          (accept-process-output (get-buffer-process shell-buf) 1)
          (goto-char (point-min))
          (let ((found (re-search-forward comint-prompt-regexp nil t)))
            (if found
                (dbg "re-search-forward FOUND prompt at position %d" found)
              (dbg "re-search-forward DID NOT FIND prompt"))
            (should found)))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer shell-buf)))))

(ert-deftest find-prompt-with-re-search-backward ()
  "Check if we can find the prompt using re-search-backward from end."
  (let ((shell-buf (bash)))
    (unwind-protect
        (with-current-buffer shell-buf
          (accept-process-output (get-buffer-process shell-buf) 1)
          (goto-char (point-max))
          (let ((found (re-search-backward comint-prompt-regexp nil t)))
            (if found
                (dbg "re-search-backward FOUND prompt at position %d" found)
              (dbg "re-search-backward DID NOT FIND prompt"))
            (should found)))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer shell-buf)))))

(ert-deftest diagnose-beginning-of-line ()
  "Diagnose what beginning-of-line actually does in a shell buffer."
  (let ((shell-buf (bash)))
    (unwind-protect
        (with-current-buffer shell-buf
          (accept-process-output (get-buffer-process shell-buf) 1)
          (let ((buffer-content (buffer-string)))
            (dbg "Full buffer content: %s" buffer-content)
            (dbg "Buffer length: %d" (length buffer-content))
            (goto-char (point-max))
            (dbg "point-max: %d" (point-max))
            (beginning-of-line)
            (dbg "After beginning-of-line, point: %d" (point))
            (dbg "Content from beginning-of-line to point-max: %s"
                     (buffer-substring (point) (point-max)))
            (goto-char (point-max))
            (dbg "Searching backward for newline from point-max...")
            (if (re-search-backward "\n" nil t)
                (dbg "Found newline at position %d" (point))
              (dbg "No newline found, buffer starts at position 1"))))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer shell-buf)))))

(ert-deftest warbo-wait-for-comint-detects-prompt ()
  "The `warbo-wait-for-comint' function should detect when the prompt appears."
  (in-shell-buffer
   (insert "echo 'test output'")
   (comint-send-input)
   ;; This should not raise an error if the prompt is detected
   (warbo-wait-for-comint (current-buffer))
   ;; If we get here, the prompt was found
   (should t)))

(ert-deftest warbo-wait-for-comint-timeout ()
  "The `warbo-wait-for-comint' function should timeout if prompt never appears."
  (let ((shell-buf (bash)))
    (unwind-protect
        (with-current-buffer shell-buf
          (warbo-wait-for-comint shell-buf)
          ;; Send a command that will hang (sleep for longer than timeout)
          (insert "sleep 10")
          (comint-send-input)
          ;; Delete the prompt that was just sent, simulating output without a prompt
          (goto-char (point-max))
          (let ((inhibit-read-only t))
            (when (re-search-backward comint-prompt-regexp nil t)
              (delete-region (match-beginning 0) (point-max))))
          ;; This should raise an error due to timeout
          (should-error (warbo-wait-for-comint shell-buf)))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer shell-buf)))))

(ert-deftest warbo-shell-query-working-directory ()
  "Explicitly update the working directory."
  (in-shell-buffer
   (insert "ls > /dev/null && cd \"$(echo /)\"")
   (comint-send-input)
   (warbo-wait-for-comint (current-buffer))
   (dirs)
   (dbg "/ ")
   (should (equal "/" default-directory))))

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
   (dbg "History item: 1")
   (should-line "echo command2")

   (execute-kbd-macro (kbd "C-<up>"))
   (dbg "History item: 2")
   (should-line "echo command1")

   (execute-kbd-macro (kbd "C-<down>"))
   (dbg "History item: 1")
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

(ert-deftest warbo-startup-shells-dont-steal-current-buffer ()
  "The shells opened by open-startup-shells should be in the background."
  (sleep-for 2)
  (when (get-buffer "home")
    ;; Suppress the "there is currently a running process" prompt
    (let ((kill-buffer-query-functions nil))
      (kill-buffer (get-buffer "home"))))
  (should-not (get-buffer "home"))
  (let ((old-buf (current-buffer)))
    (open-startup-shells)
    (sleep-for 2)
    (let ((home (get-buffer "home")))
      (should home)
      (should-not (eq (current-buffer) home))
      (should (eq (current-buffer) old-buf))
      (with-current-buffer home
        (should (eq major-mode 'shell-mode))))))

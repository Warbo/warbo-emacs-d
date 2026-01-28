;;; mistty-tests.el --- Tests for mistty terminal -*- lexical-binding: t -*-

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; `http://www.gnu.org/licenses/'.

;;; Commentary:
;;
;; ERT tests for mistty.el.
;;

;;; Code:

(require 'ert)
(require 'mistty) ; The file under test

(defmacro in-mistty-buffer (&rest body)
  "Run BODY in a new mistty buffer, with setup and teardown."
  `(let ((mistty-buf (mistty-cycle-or-create
                      (lambda (b) nil) ; Always create a new buffer
                      (lambda (other-window) (mistty-create nil other-window))
                      nil)))
     (unwind-protect
         (with-current-buffer mistty-buf
           (let* ((process nil)
                  (timeout-start (float-time)))
             ;; Wait for the process to be created and live
             (while (and (or (null process) (not (process-live-p process)))
                         (< (- (float-time) timeout-start) 4.0)) ; Reverted timeout to 4s
               (setq process (buffer-local-value 'mistty-proc mistty-buf)) ; Use buffer-local-value
               (sleep-for 0.1)) ; Small delay to avoid busy-waiting

             (unless (and process (process-live-p process))
               (error "Mistty process did not start within timeout"))

             ;; Now wait for the prompt, accepting process output
             (setq timeout-start (float-time)) ; Reset timeout for prompt
             (while (and (not (save-excursion
                                (goto-char (point-max))
                                (re-search-backward "^[^ ]*[$#] $" nil t)))
                         (< (- (float-time) timeout-start) 4.0)) ; Reverted timeout to 4s
               (accept-process-output process 0.1) ; Crucial for prompt to appear
               (sleep-for 0.1)))
           ,@body)
       (let ((kill-buffer-query-functions nil))
         (kill-buffer mistty-buf)))))

(defmacro should-line (expected)
  "Assert that the current line's content equals EXPECTED."
  `(should (string-equal
            ,expected
            (buffer-substring-no-properties
             (line-beginning-position)
             (line-end-position)))))

(ert-deftest mistty-up-down-move-point ()
  "Up and down cursor keys should move point, not cycle history."
  (in-mistty-buffer
   (goto-char (point-min))
   (insert "line 1\nline 2\n")
   (goto-char (point-max)) ; Move to prompt
   (re-search-backward "^[^ ]*[$#] $" nil t) ; Move to beginning of prompt

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

(defmacro warbo-wait-for (condition &optional timeout)
  "Wait until CONDITION is non-nil, or TIMEOUT seconds have passed.
The waiting process will repeatedly accept process output and sit for a short duration."
  (let ((timeout-val (or timeout 4.0)))
    `(let ((start-time (float-time)))
       (while (and (not ,condition)
                   (< (- (float-time) start-time) ,timeout-val))
         (accept-process-output nil 0.1)
         (sit-for 0.01))
       (unless ,condition
         (ert-fail (format "Timeout waiting for condition: %s" ',condition))))))

;; TODO: Timeout waiting for "command2" to appear after executing commands
(ert-deftest mistty-C-up-down-cycle-history ()
  "C-up and C-down should cycle history when on the command line."
  (in-mistty-buffer
   (execute-kbd-macro (kbd "e c h o \s-c o m m a n d 1 RET"))
   (warbo-wait-for (re-search-backward "command1" nil t))
   (execute-kbd-macro (kbd "e c h o \s-c o m m a n d 2 RET"))
   (warbo-wait-for (re-search-backward "command2" nil t))

   (execute-kbd-macro (kbd "C-<up>"))
   (warbo-wait-for (string-equal "echo command2" (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
   (should-line "echo command2")

   (execute-kbd-macro (kbd "C-<up>"))
   (warbo-wait-for (string-equal "echo command1" (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
   (should-line "echo command1")

   (execute-kbd-macro (kbd "C-<down>"))
   (warbo-wait-for (string-equal "echo command2" (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
   (should-line "echo command2")))

;; TODO: Timeout waiting for point to reach original-location after C-a
(ert-deftest mistty-C-a-at-prompt ()
  "C-a at the prompt should move to the beginning of the command."
  (in-mistty-buffer
   (let ((original-location (point)))
     (insert "some text")
     (execute-kbd-macro (kbd "C-a"))
     (warbo-wait-for (= (point) original-location))
     (should (= (point) original-location)))))

;; TODO: Timeout waiting for "  hello world" to appear in buffer after printf
(ert-deftest mistty-C-a-elsewhere ()
  "C-a elsewhere should move to the first non-whitespace character."
  (in-mistty-buffer
   ;; Run printf with leading spaces
   (execute-kbd-macro
    (kbd "p r i n t f \s-\" \s-\s-h e l l o \s-w o r l d \n \" RET"))
   (warbo-wait-for (re-search-forward "  hello world" nil t))

   ;; Go to the end of a line with leading spaces
   (goto-char (line-end-position))

   (execute-kbd-macro (kbd "C-a"))
   (warbo-wait-for (= (point) (+ (line-beginning-position) 2)))
   (should (= (point) (+ (line-beginning-position) 2)))))

(ert-deftest mistty-test-emulate-terminal-passthrough ()
  "Test that `mistty--emulate-terminal' passes through regular text."
  (in-mistty-buffer
   (let ((input-string "hello world\n"))
     (mistty-send-string input-string)
     (sleep-for 0.5)
     (accept-process-output)
     (should (string-match-p (regexp-quote input-string) (buffer-string))))))

;; TODO: Control chars (SOH/STX) not appearing in buffer - terminal processes them
(ert-deftest mistty-test-emulate-terminal-control-chars ()
  "Test how `mistty--emulate-terminal' handles SOH and STX control characters."
  (in-mistty-buffer
   (let ((input-string (concat "prefix" (string ?\001 ?\002) "suffix\n")))
     (mistty-send-string input-string)
     (sleep-for 0.5)
     (accept-process-output)
     (should (string-match-p (regexp-quote input-string) (buffer-string))))))

(ert-deftest mistty-test-emulate-terminal-other-control-chars ()
  "Test that `mistty--emulate-terminal' passes through other control characters."
  (in-mistty-buffer
   (let ((input-string (concat "line1\r" "line2\e[K\n")))
     (mistty-send-string input-string)
     (sleep-for 0.5)
     (accept-process-output)
     (should (string-match-p "line2\n" (buffer-string))))))

;; Integration tests for <f2> mistty functionality

(require 'warbo-shells)

(defvar warbo-f2-test-buffers nil
  "Track buffers created during <f2> tests for cleanup.")

(defun warbo-f2-wait-for-mistty (buf &optional timeout)
  "Wait for BUF to have a live mistty process, up to TIMEOUT seconds."
  (let ((timeout-val (or timeout 4.0))
        (start-time (float-time)))
    (while (and (< (- (float-time) start-time) timeout-val)
                (not (and (buffer-live-p buf)
                          (buffer-local-value 'mistty-proc buf)
                          (process-live-p (buffer-local-value 'mistty-proc buf)))))
      (accept-process-output nil 0.1)
      (sit-for 0.05))))

(defun warbo-f2-kill-matching-buffers (pattern)
  "Kill all buffers whose names match PATTERN."
  (dolist (buf (buffer-list))
    (when (string-match-p pattern (buffer-name buf))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buf)))))

(defmacro with-f2-test-env (dir &rest body)
  "Run BODY in directory DIR with real mistty, cleaning up created buffers."
  (declare (indent 1))
  `(let ((warbo-f2-test-buffers nil)
         (default-directory ,dir)
         (original-buf (current-buffer))
         (ctx (let ((default-directory ,dir)) (warbo-mistty-context))))
     ;; Kill any pre-existing buffers that would collide
     (warbo-f2-kill-matching-buffers
      (regexp-quote (format "%s.mistty" (cdr ctx))))
     (unwind-protect
         (progn ,@body)
       ;; Clean up any mistty buffers we created
       (dolist (buf warbo-f2-test-buffers)
         (when (buffer-live-p buf)
           (let ((kill-buffer-query-functions nil))
             (kill-buffer buf))))
       (when (buffer-live-p original-buf)
         (switch-to-buffer original-buf)))))

(defun warbo-f2-press ()
  "Press <f2> and track the resulting buffer."
  (let ((buf-before (current-buffer)))
    (call-interactively #'warbo-mistty-switch-or-create)
    (unless (eq (current-buffer) buf-before)
      (push (current-buffer) warbo-f2-test-buffers))
    (warbo-f2-wait-for-mistty (current-buffer))
    (current-buffer)))

(ert-deftest warbo-f2-creates-buffer-in-non-git-dir ()
  "Pressing <f2> in a non-git directory creates a named mistty buffer."
  (with-f2-test-env "/tmp/"
    (warbo-f2-press)
    (should (derived-mode-p 'mistty-mode))
    (should (string-match-p "^tmp\\.mistty$" (buffer-name)))))

(ert-deftest warbo-f2-switches-to-existing-buffer ()
  "Pressing <f2> switches to existing mistty buffer for same context."
  (with-f2-test-env "/tmp/"
    (let ((first-buf (warbo-f2-press)))
      ;; Switch away
      (switch-to-buffer "*scratch*")
      ;; Press <f2> again (use call-interactively, not execute-kbd-macro)
      (call-interactively #'warbo-mistty-switch-or-create)
      ;; Should switch back to existing buffer, not create new one
      (should (eq (current-buffer) first-buf)))))

(ert-deftest warbo-f2-creates-numbered-buffer-when-in-mistty ()
  "Pressing <f2> while in a mistty buffer creates another with incremented number."
  (with-f2-test-env "/tmp/"
    (let ((first-buf (warbo-f2-press)))
      ;; Press <f2> again while still in mistty buffer
      (let ((second-buf (warbo-f2-press)))
        ;; Should be in a different buffer
        (should-not (eq second-buf first-buf))
        ;; New buffer should have a number suffix
        (should (string-match-p "^tmp\\.mistty<[0-9]+>$" (buffer-name)))))))

(ert-deftest warbo-f2-uses-git-repo-name ()
  "Pressing <f2> in a git repo uses the repo name, not the subdirectory."
  (with-f2-test-env (expand-file-name "~/.emacs.d/personal/")
    (warbo-f2-press)
    ;; Should use the git repo name (.emacs.d), not "personal"
    (should (string-match-p "emacs.*\\.mistty" (buffer-name)))))

(ert-deftest warbo-f2-creates-correctly-named-buffer ()
  "Calling warbo-mistty-switch-or-create should create a properly named buffer."
  (let* ((dir-a (make-temp-file "mistty-test-a" t))
         (dir-b (make-temp-file "mistty-test-b" t))
         (dir-a-name (file-name-nondirectory (directory-file-name dir-a)))
         (dir-b-name (file-name-nondirectory (directory-file-name dir-b)))
         buf-a buf-a-name)
    (unwind-protect
        (progn
          ;; Call function in directory A
          (let ((default-directory dir-a))
            (call-interactively #'warbo-mistty-switch-or-create)
            (warbo-f2-wait-for-mistty (current-buffer))
            (setq buf-a (current-buffer))
            (setq buf-a-name (buffer-name buf-a))
            ;; Buffer should be named after the directory, not "*mistty*"
            (should (string-match-p (regexp-quote dir-a-name) buf-a-name))
            (should-not (string-equal "*mistty*" buf-a-name)))

          ;; Switch to a scratch buffer in directory B
          (let ((default-directory dir-b))
            (switch-to-buffer (get-buffer-create "*test-scratch*"))
            ;; Call function again
            (call-interactively #'warbo-mistty-switch-or-create)
            (warbo-f2-wait-for-mistty (current-buffer))
            ;; Should be in a different buffer
            (should-not (eq (current-buffer) buf-a))
            ;; New buffer should be named after dir-b
            (should (string-match-p (regexp-quote dir-b-name) (buffer-name)))
            ;; Original buffer should still exist with its original name
            (should (buffer-live-p buf-a))
            (should (string-equal buf-a-name (buffer-name buf-a)))))
      ;; Cleanup
      (let ((kill-buffer-query-functions nil))
        (when (buffer-live-p buf-a) (kill-buffer buf-a))
        (when (get-buffer "*test-scratch*") (kill-buffer "*test-scratch*"))
        (dolist (buf (buffer-list))
          (when (string-match-p "\\.mistty" (buffer-name buf))
            (kill-buffer buf))))
      (delete-directory dir-a t)
      (delete-directory dir-b t))))

(provide 'mistty-tests)

;;; mistty-tests.el ends here

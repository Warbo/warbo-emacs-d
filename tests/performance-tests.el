(defun make-performance-shell (given-name)
  (let ((name (concat "*test-performance-shell-" given-name "*")))
    (rename-buffer name)
    (refresh-terminal)
    (should (equal name (buffer-name)))
    (should (equal major-mode 'shell-mode))
    (goto-char (point-max))
    name))

(defun send-performance-commands (cmds)
  ;; Grab the start time
  (let* ((name  (buffer-name))
         (proc  (get-buffer-process name))
         (start (float-time)))

    ;; Run each command
    (dolist (cmd cmds)
      (comint-send-string name cmd)
      (comint-send-input))

    ;; Wait for the result
    (accept-process-output proc)
    (redraw-display)
    (while (< (- (float-time) start) 3)
      (sleep-for 1))))

(ert-deftest warbo-performance-longlines ()
  "Navigating a buffer with long lines can hang Emacs at 100% CPU"
  (let ((name "*test-performance-shell*"))
    (with-temp-buffer
      (rename-buffer name)
      (refresh-terminal)
      (should (equal name (buffer-name)))
      (should (equal major-mode 'shell-mode))

      ;; Write a command which will print a long line, with many parentheses to be
      ;; matched, etc.
      (goto-char (point-max))
      (comint-send-string name
                          "for N in $(seq 1 10000); do printf '{[('; done; ")
      (comint-send-string name
                          "for N in $(seq 1 10000); do printf ')]}'; done; echo")

      ;; Grab the start time
      (let ((proc  (get-buffer-process name))
            (start (float-time)))

        ;; Run the command
        (comint-send-input)

        ;; Wait for the result
        (accept-process-output proc)

        ;; Force redraw (maybe useless in headless mode?)
        (redraw-display)

        (while (< (- (float-time) start) 3)
          (sleep-for 1))

        ;; Make sure we don't have any long lines
        (goto-char (point-min))
        (while (< (point) (point-max))
          ;; Lines should be split at 1000 characters, but we give some leeway for
          ;; ANSI control characters and things. If our line-splitting's broken,
          ;; we'll end up an order of magnitude too big, so this margin is fine.
          (should (< (- (line-end-position) (line-beginning-position)) 1500))
          (forward-line))))))

(ert-deftest warbo-performance-existing-newlines ()
  "Don't split up output if it's already made of short lines"
  (let ((name "*test-performance-shell2*"))
    (with-temp-buffer
      (rename-buffer name)
      (refresh-terminal)
      (should (equal name (buffer-name)))
      (should (equal major-mode 'shell-mode))

      ;; Write a command which will print lots of output, made of short lines
      (goto-char (point-max))
      (comint-send-string name
                          "for N in $(seq 1 10000); do printf 'AB\n'; done; ")
      (comint-send-string name
                          "for N in $(seq 1 10000); do printf 'AB\n'; done; echo")

      ;; Grab the start time
      (let ((proc  (get-buffer-process name))
            (start (float-time))
            (count 0))

        ;; Run the command
        (comint-send-input)

        ;; Wait for the result
        (accept-process-output proc)

        ;; Force redraw (maybe useless in headless mode?)
        (redraw-display)

        (while (< (- (float-time) start) 3)
          (sleep-for 1))

        ;; Make sure all printed lines are 'AB'
        (goto-char (point-min))
        (while (< (point) (point-max))
          ;; Lines should be split at 1000 characters, but we give some leeway for
          ;; ANSI control characters and things. If our line-splitting's broken,
          ;; we'll end up an order of magnitude too big, so this margin is fine.
          (should-not (equal (char-after) ?B))
          (when (equal (char-after) ?A)
            (setq count (+ 1 count))
            (should (string-equal (buffer-substring (line-beginning-position)
                                                    (line-end-position))
                                  "AB")))
          (forward-line))
        (should (> count 1000))))))

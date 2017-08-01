(ert-deftest warbo-performance-longlines ()
  "Navigating a buffer with long lines can hang Emacs at 100% CPU"
  (with-temp-buffer
    (rename-buffer "*test-performance-shell*")
    (refresh-terminal)
    (should (equal "*test-performance-shell*" (buffer-name)))
    (should (equal major-mode 'shell-mode))

    ;; Write a command which will print a long line, with many parentheses to be
    ;; matched, etc.
    (goto-char (point-max))
    (comint-send-string "*test-performance-shell*"
                        "for N in $(seq 1 10000); do printf '{[('; done; ")
    (comint-send-string "*test-performance-shell*"
                        "for N in $(seq 1 10000); do printf ')]}'; done; echo")

    ;; Grab the start time
    (let ((proc  (get-buffer-process "*test-performance-shell*"))
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
        (forward-line)))))

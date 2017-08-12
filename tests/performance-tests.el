(defun sleep-ensure (time)
  "Keep calling (sleep-for 1) until TIME seconds have elapsed."
  (let ((start (float-time)))
    (while (< (- (float-time) time) start)
      (sleep-for 1))))

(defun buffer-raw ()
  "Unformatted buffer string."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun show-context (&rest args)
  "Take ARGS and format them all for output."
  `(args  ,args))

(defun make-performance-shell (given-name)
  "Create a shell buffer with the GIVEN-NAME."
  (let ((name (concat "*test-performance-shell-" given-name "*")))
    (rename-buffer name)
    (refresh-terminal)
    (should (equal name (buffer-name)))
    (should (equal major-mode 'shell-mode))
    (sleep-ensure 3)
    (goto-char (point-max))
    name))

(defun send-performance-commands (cmds &optional given-time)
  "Send each string in CMDS to the current buffer, with a delay of GIVEN-TIME."
  ;; Grab the start time
  (let* ((name  (buffer-name))
         (proc  (get-buffer-process name))
         (start nil)
         (time  (or given-time 3)))
    (dolist (cmd cmds)
      ;; Run each command
      (setq start (float-time))
      (comint-send-string name cmd)
      (comint-send-input)

      ;; Wait for the result
      (accept-process-output proc)
      (redraw-display)
      (sleep-ensure time))))

(ert-deftest warbo-performance-split-lines-at ()
  (should (equal (split-lines-at 3 "hello world")   "hel\nlo \nwor\nld"))
  (should (equal (split-lines-at 3 "\nhello world") "\nhello\n wo\nrld"))
  (should (equal (split-lines-at 3 "ab\nc\ndef\ng") "ab\nc\ndef\ng")))

(defun short-line (limit line &rest args)
  (< (length line) limit))
(put 'short-line 'ert-explainer 'show-context)

(ert-deftest warbo-performance-longlines ()
  "Navigating a buffer with long lines can hang Emacs at 100% CPU"
  (with-temp-buffer
    ;; Write a command which will print a long line, with many parentheses to
    ;; be matched, etc.
    (make-performance-shell "longlines")
    (send-performance-commands
     (list "for N in $(seq 1 10000); do printf '{[('; done;"
           "for N in $(seq 1 10000); do printf ')]}'; done; echo")
     2)

      ;; Make sure we don't have any long lines
    (goto-char (point-min))
    (while (< (point) (point-max))
      ;; Newlines should be split every 1000 characters, except whenever those
      ;; 1000-character chunks already contain newlines. In the worst case, we
      ;; get a chunk starting with a newline, followed by 999 characters, which
      ;; is appended straight on to the following 1000 character chunk, giving a
      ;; line length of 1999. We bump this a bit to  give some leeway for ANSI
      ;; control characters and things. If our line-splitting's broken, we'll
      ;; end up an order of magnitude too big, so this margin is fine.
      (should (short-line 2500 (buffer-substring-no-properties
                                (line-beginning-position)
                                (line-end-position))))
      (forward-line))))

(ert-deftest warbo-performance-existing-newlines ()
  "Don't split up output if it's already made of short lines"
  (with-temp-buffer
    ;; Write a command which will print lots of output, made of short lines
    (make-performance-shell "existing-newlines")
    (send-performance-commands
     (list "for N in $(seq 1 10000); do printf 'AB\n'; done; echo"))

    ;; Make sure all printed lines are 'AB'
    (let ((count 0))
      (goto-char (point-min))
      (while (< (point) (point-max))
        ;; Lines should either be prompts or "AB"; if a line begins with "B"
        ;; then some "AB" line must have been split inappropriately.
        (should-not (equal (char-after) ?B))

        ;; Any line beginning with "A" should be "AB" (i.e. there are no
        ;; inappropriately-split lines consisting of "A" and there are no
        ;; missing newlines like "ABABABAB...")
        (when (equal (char-after) ?A)
          ;; We count how many lines begin with "A" since this could be
          ;; vacuously true
          (setq count (+ 1 count))
          (should (string-equal (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position))
                                "AB")))
        (forward-line))

      ;; Ensure we actually saw some output lines (i.e. our test isn't vacuous)
      (should (> count 1000)))))

(defun nonempty-line (start end str)
  "Whether the line in STR between START and END is nonempty"
  (not (equal start end)))
(put 'nonempty-line 'ert-explainer 'show-context)

(defun is-prompt (buf-str line)
  ;; Reverse the given line
  (let ((enil (reverse line)))
    ;; Strip off leading spaces
    (while (and (not (equal "" enil))
                (equal (string (elt enil 0)) " "))
      (setq enil (seq-drop enil 1)))

    ;; The result should start with a $
    (and (not (equal "" enil))
         (equal (elt enil 0) ?$))))
(put 'nonempty-line 'ert-explainer 'show-context)

(ert-deftest warbo-performance-trailing-newlines ()
  "Don't add trailing newline if output doesn't have one"
  (with-temp-buffer
    ;; Press enter a bunch of times, without writing any actual commands
    (make-performance-shell "trailing-newlines")
    (send-performance-commands (list "" "" "" "" "" "" "") 1)

    ;; We should have a bunch of prompts, rather than empty lines
    (let ((count 0))
      (goto-char (point-min))
      (while (< (point) (point-max))
        (setq count (+ 1 count))

        ;; Lines shouldn't be empty
        (should (nonempty-line (line-beginning-position) (line-end-position)
                               (buffer-raw)))

        ;; Each line should be a shell prompt
        (should (is-prompt (buffer-raw)
                           (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position))))
        (forward-line))

      ;; Ensure we saw some prompts (i.e. our test isn't vacuous)
      (should (> count 2)))))

;; Shells can easily cause Emacs to hang with large outputs; make sure lines are
;; split at regular intervals to minimise this

(defun split-lines-at (n existing str)
  "Limit lines to N chars in EXISTING+STR, by adding newlines to STR.

Given a potentially long string STR, this will return an augmented version
with newline characters inserted such that no line contains more than N
characters.  This is useful for transforming shell command output before
Emacs tries to display it, since commands may give out huge lines which makes
Emacs churn.

EXISTING is whatever will appear before STR, i.e if it ends in a long line we
will split STR earlier, to prevent their concatenation being too long."
  (let* ((prior     (last (split-string existing (regexp-quote "\n"))))
         (preceding (if prior (car prior) ""))
         (result    nil)
         (countdown (max 0 (- n (length preceding)))))
    (concat (reverse (dolist (char (string-to-list str) result)
                       (when (< countdown 1)
                         (unless (equal char ?\n)
                           (setq result (cons ?\n result))
                           (setq countdown n)))
                       (setq countdown (- countdown 1))
                       (setq result (cons char result))
                       (when (equal char ?\n)
                         (setq countdown n)))))))

(ert-deftest warbo-split-lines-at ()
  (should (equal (split-lines-at 1 ""   "hello")         "h\ne\nl\nl\no"))
  (should (equal (split-lines-at 3 ""   "hello world")   "hel\nlo \nwor\nld"))
  (should (equal (split-lines-at 3 "fo" "hello world")   "h\nell\no w\norl\nd"))
  (should (equal (split-lines-at 3 ""   "\nhello world") "\nhel\nlo \nwor\nld"))
  (should (equal (split-lines-at 3 ""   "ab\nc\ndef\ng") "ab\nc\ndef\ng")))

(defvar shell-split-length 500
  "The length of chunks when splitting long shell lines.")

(add-hook 'shell-mode-hook
          (lambda ()
            (add-hook 'comint-preoutput-filter-functions
                      (lambda (string)
                        (let ((existing (buffer-substring-no-properties
                                         (point-max)

                                         ;; point starts at 1
                                         (max 1 (- (point-max)
                                                   shell-split-length)))))
                          (split-lines-at shell-split-length existing string)))
                      nil
                      t)))

(defun short-line (limit line &rest args)
  (< (length line) limit))

(defun show-context (&rest args)
  "Take ARGS and format them all for output."
  `(args ,args))

(put 'short-line 'ert-explainer 'show-context)

(defun sleep-ensure (time)
  "Keep calling (sleep-for 1) until TIME seconds have elapsed."
  (let ((start (float-time)))
    (while (< (- (float-time) time) start)
      (sleep-for 1))))

(defun buffer-raw ()
  "Unformatted buffer string."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun make-shell-for-splitting (given-name)
  "Create a shell buffer with the GIVEN-NAME."
  (let ((name (concat "*test-shell-for-splitting-" given-name "*")))
    (rename-buffer name)
    (refresh-terminal)
    (should (equal name (buffer-name)))
    (should (equal major-mode 'shell-mode))
    (sleep-ensure 3)
    (goto-char (point-max))
    name))

(defun send-shell-splitting-commands (cmds &optional given-time)
  "Send each string in CMDS to the current buffer, with a delay of GIVEN-TIME."
  ;; Grab the start time
  (let* ((name (buffer-name))
         (proc (get-buffer-process name)))
    (dolist (cmd cmds)
      ;; Run each command
      (comint-send-string name cmd)
      (comint-send-input)

      ;; Wait for the result
      (accept-process-output proc)
      (redraw-display)
      (sleep-ensure (or given-time 3)))))

(defun try-splitting-long-lines (size)
  "Generate text in a shell buffer, with lines split at length SIZE."
  (let ((shell-split-length size)
        (result             nil)
        (temp-name          nil))
    (with-temp-buffer
      ;; Write a command which will print a long line, with many parentheses to
      ;; be matched, etc.
      (setq temp-name (make-shell-for-splitting (format "longlines-%d" size)))
      (send-shell-splitting-commands
       (list (format "for N in $(seq 1 %d); do printf '{[('; done;" (* 10 size))
             (format "for N in $(seq 1 %d); do printf ')]}'; done;" (* 10 size))
             "echo")
       2)

      ;; Grab the output and split into lines
      (setq result (split-string (substring-no-properties (buffer-string))
                                 (regexp-quote "\n"))))

    ;; Kill temp buffer if still around; don't ask for confirmation about
    ;; whether to kill the subprocess, just kill it.
    (when (get-buffer temp-name)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer temp-name)))
    result))

(require 'cl)
(ert-deftest warbo-shell-splitter-runs ()
  "Try using bogus splitter and make sure it ran"
  ;; flycheck complains about flet, but cl-flet doesn't seem to work!
  (flet ((split-lines-at (size existing string)
                         "x"))
    (let* ((lines    (try-splitting-long-lines 100))
           (combined (string-join lines))
           (chars    (mapcar 'char-to-string (string-to-list combined)))
           (unique   (delete-dups chars)))
      (should (equal unique '("x"))))))

(ert-deftest warbo-shell-split-longlines ()
  "Navigating a buffer with long lines can hang Emacs at 100% CPU"
  ;; Make sure we don't have any long lines
  (dolist (size (list 10 100 1000))
    ;; Lines should be split roughly every SIZE characters (accounting for
    ;; prompts, ANSI escape characters, existing newlines, etc.).
    (dolist (line (try-splitting-long-lines size))
      (should (short-line (* 2 size) line)))))

(ert-deftest warbo-shell-split-utilise-existing-newlines ()
  "Don't split up output if it's already made of short lines"
  (with-temp-buffer
    ;; Write a command which will print lots of output, made of short lines
    (make-shell-for-splitting "existing-newlines")
    (send-shell-splitting-commands
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

(ert-deftest warbo-shell-split-preserves-trailing-newlines ()
  "Don't add trailing newline if output doesn't have one"
  (with-temp-buffer
    ;; Press enter a bunch of times, without writing any actual commands
    (make-shell-for-splitting "trailing-newlines")
    (send-shell-splitting-commands (list "" "" "" "" "" "" "") 1)

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

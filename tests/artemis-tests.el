(ert-deftest warbo-artemis-defined ()
  "Have Artemis functionality"
  (should (fboundp 'artemis-mode))
  (should (fboundp 'switch-to-artemis)))

(ert-deftest warbo-artemis-switch ()
  "Can switch on artemis-mode"
  ;; Load some file
  (let* ((path "~/.emacs.d/tests/artemis-tests.el")
         (buf  (find-file path)))
    ;; Switch away to ensure we're not just altering the current buffer
    (with-current-buffer (get-buffer "*scratch*")
      (switch-to-artemis path))
    (with-current-buffer buf
      (should (equal major-mode 'artemis-mode))
      (kill-buffer))))

(ert-deftest warbo-artemis-fill-paragraph-no-indentation ()
  "The `fill-paragraph' command should not indent lines in `artemis-mode'."
  (with-temp-buffer
    (artemis-mode)
    (setq fill-column 80)

    ;; Insert a long line that will need to be wrapped
    (insert "When writing a long artemis issue or comment, pressing M-q to spread it out over multiple lines based on current fill-column should not make the subsequent lines appear with a tab for indentation like this.")

    ;; Fill the paragraph
    (fill-paragraph)

    ;; Check that no line starts with whitespace (tabs or spaces)
    (goto-char (point-min))
    (while (not (eobp))
      (should-not (looking-at "^[ \t]"))
      (forward-line 1))))

(ert-deftest warbo-artemis-fill-paragraph-respects-fill-column ()
  "The `fill-paragraph' command should respect `fill-column' in `artemis-mode'."
  (with-temp-buffer
    (artemis-mode)
    (setq fill-column 80)

    ;; Insert a long line
    (insert "This is a very long line that contains many words and should be wrapped when we call fill-paragraph because it exceeds the fill-column which is set to eighty characters.")

    ;; Fill the paragraph
    (fill-paragraph)

    ;; Check that all lines are within fill-column
    (goto-char (point-min))
    (while (not (eobp))
      (end-of-line)
      (should (<= (current-column) fill-column))
      (forward-line 1))))

(ert-deftest warbo-artemis-fill-paragraph-multiple-wraps ()
  "The `fill-paragraph' command should not indent text with multiple wraps."
  (with-temp-buffer
    (artemis-mode)
    (setq fill-column 40)  ; Use a shorter fill-column to force multiple wraps

    ;; Insert a very long line
    (insert "When writing a long artemis issue or comment, pressing M-q to spread it out over multiple lines based on current fill-column, which I always set to 80, makes the subsequent lines appear with a tab for indentation, like this. Note that it should not even be indented at all, let alone with a tab!")

    ;; Fill the paragraph
    (fill-paragraph)

    ;; Count lines and verify no indentation
    (let ((line-count 0))
      (goto-char (point-min))
      (while (not (eobp))
        (setq line-count (1+ line-count))
        ;; No line should start with whitespace
        (should-not (looking-at "^[ \t]"))
        (forward-line 1))
      ;; Should have wrapped into multiple lines
      (should (> line-count 1)))))

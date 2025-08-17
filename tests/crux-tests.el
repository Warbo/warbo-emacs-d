;;; crux-tests.el --- Tests for crux -*- lexical-binding: t; -*-

(require 'crux)

;;; Test `crux-with-region-or-buffer'

(defun crux-buffer-test-function (&optional start end)
  "A test function that returns the start and end positions."
  (interactive)
  (list start end))

(ert-deftest crux-with-region-or-buffer-non-interactive-no-args-no-region ()
  "Test `crux-with-region-or-buffer` non-interactively with no arguments or region."
  (crux-with-region-or-buffer crux-buffer-test-function)
  (unwind-protect
      (with-temp-buffer
        (insert "line 1\nline 2\nline 3")
        (should (equal (list 1 21) (crux-buffer-test-function))))
    (advice-remove 'crux-buffer-test-function 'with-region-or-buffer)))

(ert-deftest crux-with-region-or-buffer-non-interactive-no-args-with-region ()
  "Test `crux-with-region-or-buffer` non-interactively with no arguments but a region."
  (crux-with-region-or-buffer crux-buffer-test-function)
  (unwind-protect
      (with-temp-buffer
        (insert "line 1\nline 2\nline 3")
        (set-mark 8)
        (goto-char 14)
        (should (equal (list 8 14) (crux-buffer-test-function))))
    (advice-remove 'crux-buffer-test-function 'with-region-or-buffer)))

(ert-deftest crux-with-region-or-buffer-non-interactive-with-args ()
  "Test `crux-with-region-or-buffer` non-interactively with arguments."
  (crux-with-region-or-buffer crux-buffer-test-function)
  (unwind-protect
      (with-temp-buffer
        (insert "line 1\nline 2\nline 3")
        (should (equal (list 2 5) (crux-buffer-test-function 2 5))))
    (advice-remove 'crux-buffer-test-function 'with-region-or-buffer)))

(ert-deftest crux-with-region-or-buffer-interactive-with-region ()
  "Test `crux-with-region-or-buffer` interactively with a region."
  (crux-with-region-or-buffer crux-buffer-test-function)
  (unwind-protect
      (with-temp-buffer
        (insert "line 1\nline 2\nline 3")
        (goto-char 8)
        (set-mark 14)
        (let ((last-command this-command))
          (should (equal (list 8 14) (call-interactively 'crux-buffer-test-function)))))
    (advice-remove 'crux-buffer-test-function 'with-region-or-buffer)))

(ert-deftest crux-with-region-or-buffer-interactive-no-region ()
  "Test `crux-with-region-or-buffer` interactively with no region."
  (crux-with-region-or-buffer crux-buffer-test-function)
  (unwind-protect
      (with-temp-buffer
        (insert "line 1\nline 2\nline 3")
        (let ((last-command this-command))
          (should (equal (list 1 21) (call-interactively 'crux-buffer-test-function)))))
    (advice-remove 'crux-buffer-test-function 'with-region-or-buffer)))

;;; Test `crux-with-region-or-line'

(defun crux-line-test-function (&optional start end)
  "A test function that returns the start and end positions."
  (interactive)
  (list start end))

(ert-deftest crux-with-region-or-line-non-interactive-no-args ()
  "Test `crux-with-region-or-line` non-interactively with no arguments."
  (crux-with-region-or-line crux-line-test-function)
  (unwind-protect
      (with-temp-buffer
        (insert "line 1\nline 2\nline 3")
        (goto-char 10)
        (should (equal (list 8 14) (crux-line-test-function))))
    (advice-remove 'crux-line-test-function 'with-region-or-line)))

(ert-deftest crux-with-region-or-line-non-interactive-with-args ()
  "Test `crux-with-region-or-line` non-interactively with arguments."
  (crux-with-region-or-line crux-line-test-function)
  (unwind-protect
      (with-temp-buffer
        (insert "line 1\nline 2\nline 3")
        (should (equal (list 2 5) (crux-line-test-function 2 5))))
    (advice-remove 'crux-line-test-function 'with-region-or-line)))

(ert-deftest crux-with-region-or-line-interactive-with-region ()
  "Test `crux-with-region-or-line` interactively with a region."
  (crux-with-region-or-line crux-line-test-function)
  (unwind-protect
      (with-temp-buffer
        (insert "line 1\nline 2\nline 3")
        (goto-char 2)
        (set-mark 5)
        (let ((last-command this-command))
          (should (equal (list 2 5) (call-interactively 'crux-line-test-function)))))
    (advice-remove 'crux-line-test-function 'with-region-or-line)))

(ert-deftest crux-with-region-or-line-interactive-no-region ()
  "Test `crux-with-region-or-line` interactively with no region."
  (crux-with-region-or-line crux-line-test-function)
  (unwind-protect
      (with-temp-buffer
        (insert "line 1\nline 2\nline 3")
        (goto-char 10)
        (let ((last-command this-command))
          (should (equal (list 8 14) (call-interactively 'crux-line-test-function)))))
    (advice-remove 'crux-line-test-function 'with-region-or-line)))

;;; Test `crux-with-region-or-sexp-or-line'

(defun crux-sexp-test-function (&optional start end)
  "A test function that returns the start and end positions."
  (interactive)
  (list start end))

(ert-deftest crux-with-region-or-sexp-or-line-non-interactive-sexp ()
  "Test `crux-with-region-or-sexp-or-line` non-interactively on a sexp."
  (crux-with-region-or-sexp-or-line crux-sexp-test-function)
  (unwind-protect
      (with-temp-buffer
        (insert "(foo bar)\nbaz")
        (goto-char 2)
        (should (equal (list 1 9) (crux-sexp-test-function))))
    (advice-remove 'crux-sexp-test-function 'with-region-or-sexp-or-line)))

(ert-deftest crux-with-region-or-sexp-or-line-non-interactive-string ()
  "Test `crux-with-region-or-sexp-or-line` non-interactively on a string."
  (crux-with-region-or-sexp-or-line crux-sexp-test-function)
  (unwind-protect
      (with-temp-buffer
        (insert "\"foo bar\"\nbaz")
        (goto-char 2)
        (should (equal (list 1 9) (crux-sexp-test-function))))
    (advice-remove 'crux-sexp-test-function 'with-region-or-sexp-or-line)))

(ert-deftest crux-with-region-or-sexp-or-line-non-interactive-line ()
  "Test `crux-with-region-or-sexp-or-line` non-interactively on a line."
  (crux-with-region-or-sexp-or-line crux-sexp-test-function)
  (unwind-protect
      (with-temp-buffer
        (insert "(foo bar)\nbaz")
        (goto-char 10)
        (should (equal (list 10 13) (crux-sexp-test-function))))
    (advice-remove 'crux-sexp-test-function 'with-region-or-sexp-or-line)))

(ert-deftest crux-with-region-or-sexp-or-line-interactive-sexp ()
  "Test `crux-with-region-or-sexp-or-line` interactively on a sexp."
  (crux-with-region-or-sexp-or-line crux-sexp-test-function)
  (unwind-protect
      (with-temp-buffer
        (insert "(foo bar)\nbaz")
        (goto-char 2)
        (let ((last-command this-command))
          (should (equal (list 1 9) (call-interactively 'crux-sexp-test-function)))))
    (advice-remove 'crux-sexp-test-function 'with-region-or-sexp-or-line)))

;;; Test `crux-with-region-or-point-to-eol'

(defun crux-eol-test-function (&optional start end)
  "A test function that returns the start and end positions."
  (interactive)
  (list start end))

(ert-deftest crux-with-region-or-point-to-eol-non-interactive-no-args ()
  "Test `crux-with-region-or-point-to-eol` non-interactively with no arguments."
  (crux-with-region-or-point-to-eol crux-eol-test-function)
  (unwind-protect
      (with-temp-buffer
        (insert "line 1\nline 2\nline 3")
        (goto-char 10)
        (should (equal (list 10 14) (crux-eol-test-function))))
    (advice-remove 'crux-eol-test-function 'with-region-or-point-to-eol)))

(ert-deftest crux-with-region-or-point-to-eol-non-interactive-with-args ()
  "Test `crux-with-region-or-point-to-eol` non-interactively with arguments."
  (crux-with-region-or-point-to-eol crux-eol-test-function)
  (unwind-protect
      (with-temp-buffer
        (insert "line 1\nline 2\nline 3")
        (should (equal (list 2 5) (crux-eol-test-function 2 5))))
    (advice-remove 'crux-eol-test-function 'with-region-or-point-to-eol)))

(ert-deftest crux-with-region-or-point-to-eol-interactive-with-region ()
  "Test `crux-with-region-or-point-to-eol` interactively with a region."
  (crux-with-region-or-point-to-eol crux-eol-test-function)
  (unwind-protect
      (with-temp-buffer
        (insert "line 1\nline 2\nline 3")
        (goto-char 2)
        (set-mark 5)
        (let ((last-command this-command))
          (should (equal (list 2 5) (call-interactively 'crux-eol-test-function)))))
    (advice-remove 'crux-eol-test-function 'with-region-or-point-to-eol)))

(ert-deftest crux-with-region-or-point-to-eol-interactive-no-region ()
  "Test `crux-with-region-or-point-to-eol` interactively with no region."
  (crux-with-region-or-point-to-eol crux-eol-test-function)
  (unwind-protect
      (with-temp-buffer
        (insert "line 1\nline 2\nline 3")
        (goto-char 10)
        (let ((last-command this-command))
          (should (equal (list 10 14) (call-interactively 'crux-eol-test-function)))))
    (advice-remove 'crux-eol-test-function 'with-region-or-point-to-eol)))
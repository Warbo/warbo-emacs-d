;;; ffap-goto-line.el --- Add line:column support to find-file-at-point -*- lexical-binding: t; -*-

;; Author: Chris Warburton <chriswarbo@gmail.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: files, convenience
;; URL: http://chriswarbo.net/git/warbo-emacs-d

;;; Commentary:

;; This package extends `find-file-at-point' to support line and column
;; annotations in file paths, such as "file.txt:12:34" (line 12, column 34)
;; or "file.txt:12" (line 12).
;;
;; This is particularly useful when working with compiler error messages,
;; grep output, or other tools that output file paths with line numbers.
;;
;; Usage:
;;   (require 'ffap-goto-line)
;;   (ffap-goto-line-mode 1)  ; Enable globally
;;
;; Or with use-package:
;;   (use-package ffap-goto-line
;;     :load-path "."  ; if in same directory as init.el
;;     :config
;;     (ffap-goto-line-mode 1))

;;; Code:

(require 'ffap)

;;;###autoload
(define-minor-mode ffap-goto-line-mode
  "Enable line:column support in `find-file-at-point'.

When enabled, `find-file-at-point' will look for line and column numbers
after file names, separated by colons.  For example:
  - file.txt:12:34 will open file.txt and go to line 12, column 34
  - file.txt:12 will open file.txt and go to line 12

Toggling this minor mode lets us toggle the behaviour, without having to
faff around with things like «unadvising»"
  :global t
  :init-value nil
  :group 'ffap
  (if ffap-goto-line-mode
      (ffap-goto-line--enable)
    (ffap-goto-line--disable)))

(defun ffap-goto-line--advice (orig &rest args)
  "Advice for `find-file-at-point'.

ORIG is the original function, ARGS are its arguments.

If `ffap-goto-line-mode' is enabled, we'll scan ahead in the buffer to see if
there's a line number and column, separated by colons, e.g. if we've called
`ffap' when the point is on `/home/foo/bar.txt:12:34':

 - We search forward and find the `:12:34'
 - We parse this to get line 12 and column 34
 - We call the original `ffap', which guesses filename `/home/foo/bar.txt'
 - Once opened, we go to line 12 column 34

If only one colon-separated number is found, we assume it's the line number."
  (let* ((have-col (and ffap-goto-line-mode
                        (looking-at ".*:[0-9]+:[0-9]+")))
         (col      (and ffap-goto-line-mode
                        (looking-at ".*:[0-9]+:\\([0-9]+\\)")
                        (string-to-number (match-string 1))))
         (line     (if have-col
                       (and (looking-at ".*:\\([0-9]+\\):[0-9]+")
                            (string-to-number (match-string 1)))
                     (and ffap-goto-line-mode
                          (looking-at ".*:\\([0-9]+\\)")
                          (string-to-number (match-string 1))))))
    (apply orig args)
    (when line
      (goto-char (point-min))
      (forward-line (1- line)))
    (when col  (move-to-column col))))

(defun ffap-goto-line--enable ()
  "Enable ffap-goto-line functionality."
  (advice-add 'find-file-at-point :around #'ffap-goto-line--advice))

(defun ffap-goto-line--disable ()
  "Disable ffap-goto-line functionality."
  (advice-remove 'find-file-at-point #'ffap-goto-line--advice))

;;;###autoload
(defun ffap-goto-line-setup-keybinding ()
  "Set up the recommended keybinding for ffap-goto-line.

This replaces the default \[find-file] binding with `find-file-at-point', which
will fall back to regular `find-file' behavior when no file is found at point."
  (global-set-key (kbd "C-x C-f") 'find-file-at-point))

(provide 'ffap-goto-line)

;;; ffap-goto-line.el ends here

;;; warbo-rolling-shell --- Custom shell-mode with rolling contents -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)

(defvar-local rolling-shell-at-end nil
  "Whether point was at end of buffer before output.")
(defvar-local rolling-shell-old-line nil
  "Line number before output.")

(defvar rolling-shell-max-lines 10000
  "Maximum number of lines to keep in the rolling shell buffer.")

(defun rolling-shell-truncate-buffer ()
  "Truncate the rolling shell buffer to `rolling-shell-max-lines`.
Returns the number of lines truncated."
  (let ((max-lines rolling-shell-max-lines)
        (inhibit-read-only t))
    (save-excursion
      (goto-char (point-max))
      (forward-line (- max-lines))
      (beginning-of-line)
      (let ((lines-to-delete (count-lines (point-min) (point))))
        (delete-region (point-min) (point))
        lines-to-delete))))

(defun rolling-shell-preoutput-filter (text)
  "Return TEXT unchanged, but also update the old-point."
  (prog1 text
    (let ((old-point (point)))
      (setq rolling-shell-at-end (= old-point (point-max)))
      (setq rolling-shell-old-line (line-number-at-pos)))))

(defun rolling-shell-output-filter (_text)
  "Filter to insert output into rolling shell.  _TEXT is ignored."
  ;; Insert the new text
  (goto-char (point-max))

  ;; Truncate if necessary and get number of truncated lines
  (let ((truncated-lines (rolling-shell-truncate-buffer)))
    ;; Adjust point based on where it was
    (if rolling-shell-at-end
        (goto-char (point-max))       ; Keep point at end if it was there
      (goto-char (point-min))
      (forward-line (- rolling-shell-old-line truncated-lines 1))))

  ;; Ensure point is visible
  (when (and (< (point) (window-start))
             (not (eq (current-buffer) (window-buffer))))
    (set-window-point (get-buffer-window (current-buffer)) (point)))

  ;; Return nil to indicate we've handled the output
  nil)

(require 'warbo-shells)

(defun command-in-rolling-buffer (buf-dir-cmd &optional max-lines)
  "Pass BUF-DIR-CMD to command-in-buffer, but rolling (truncated to MAX-LINES)."
  (with-current-buffer (command-in-buffer buf-dir-cmd)
    (setq-local rolling-shell-max-lines (or max-lines 10000))
    (setq-local comint-buffer-maximum-size rolling-shell-max-lines)
    (rolling-shell-preoutput-filter "")  ;; Initialise local variables
    (add-hook 'comint-preoutput-filter-functions
              'rolling-shell-preoutput-filter
              nil
              t)
    (add-hook 'comint-output-filter-functions
              'rolling-shell-output-filter
              nil
              t)

    (buffer-disable-undo)))

(provide 'warbo-rolling-shell)
;;; warbo-rolling-shell.el ends here

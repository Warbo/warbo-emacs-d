;;; warbo-rolling-shell --- Custom shell-mode with rolling contents
;;; Commentary:
;;; Code:

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

(defun rolling-shell-output-filter (text)
  "Filter to insert TEXT into rolling shell."
  (let* ((old-point (point))
         (at-end (= old-point (point-max)))
         (old-line (line-number-at-pos))
         (inhibit-read-only t))
    ;; Insert the new text
    (goto-char (point-max))
    (insert text)

    ;; Truncate if necessary and get number of truncated lines
    (let ((truncated-lines (rolling-shell-truncate-buffer)))
      ;; Adjust point based on where it was
      (if at-end
          (goto-char (point-max))  ; Keep point at end if it was there
        (goto-char (point-min))
        (forward-line (- old-line truncated-lines 1))))

    ;; Ensure point is visible
    (when (and (< (point) (window-start))
               (not (eq (current-buffer) (window-buffer))))
      (set-window-point (get-buffer-window (current-buffer)) (point))))

  ;; Return nil to indicate we've handled the output
  nil)

(require 'warbo-shells)

(defun command-in-rolling-buffer (buf-dir-cmd &optional max-lines)
  "Pass BUF-DIR-CMD to command-in-buffer, but rolling (truncated to MAX-LINES)."
  (with-current-buffer (command-in-buffer buf-dir-cmd)
    (setq-local rolling-shell-max-lines (or max-lines 10000))
    (setq-local comint-output-filter-functions '(rolling-shell-output-filter))
    (setq-local comint-buffer-maximum-size rolling-shell-max-lines)
    (buffer-disable-undo)
    (add-hook 'shell-mode-hook 'rolling-shell-setup nil t)))

(provide 'warbo-rolling-shell)
;;; warbo-rolling-shell.el ends here
(require 'use-package)

;; Artemis uses maildirs for issue tracking. Use message-mode for editing these,
;; but augment it a little (e.g. save and close, rather than send).
(defun artemis-save ()
  "Save and close the buffer; used for Artemis messages instead of 'send'."
  (interactive)
  (save-some-buffers)
  (kill-buffer))

(defvar artemis-mode-hook nil)
(defvar artemis-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-c") 'artemis-save)
    map)
  "Keymap for Artemis major mode.")

(define-derived-mode artemis-mode message-mode "Artemis"
  "Major mode for editing Artemis issues (which are maildir under the hood)."
  (set-syntax-table artemis-mode-syntax-table))

(provide 'artemis-mode)

(defun switch-to-artemis (filename)
  "Look for a buffer with the given FILENAME and switch it to artemis-mode."
  (let ((buf (find-buffer-visiting filename)))
    (when buf
      (with-current-buffer buf
        (artemis-mode)))))

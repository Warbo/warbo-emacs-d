;;; artemis-mode --- Emacs mode for the Artemis issue tracker

;;; Commentary:

;; Provides a major mode for Artemis issues, based on message-mode

(require 'use-package)

;;; Code:

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

(defun switch-to-artemis (filename)
  "Look for a buffer with the given FILENAME and switch it to artemis-mode."
  (let ((buf (find-buffer-visiting filename)))
    (when buf
      (with-current-buffer buf
        (artemis-mode)))))

(defun buffer-contains-substring (s)
  "Helper function: does string S appear in the current buffer?
From https://stackoverflow.com/a/3034272/884682"
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (search-forward s nil t))))

(defun is-artemis-buffer-p (&optional buffer)
  "Is the given BUFFER (default: 'current-buffer') an artemis issue?"
  (with-current-buffer (or buffer (current-buffer))
    (let ((f (buffer-file-name)))
      (and
       (string-prefix-p "/tmp/tmp" f)
       (string-suffix-p ".txt"     f)
       (or (buffer-contains-substring "Subject: brief description")
           (buffer-contains-substring "Detailed description."))))))

(defun server-visit-artemis-issue-hook ()
  "Invokes 'artemis-mode' if the current buffer is an artemis issue."
  (when (is-artemis-buffer-p)
    (artemis-mode)))

;; This hook fires when emacsclient visits a file, checking if the visited file
;; is an artemis issue, and if so invoking artemis-mode. We use this hook
;; because:
;;  - We cannot rely on filenames to spot issues, since they're all '.txt'
;;  - We cannot call artemis-mode when invoking emacsclient, since opening files
;;    and evaluating lisp are mutually exclusive options
;;  - We cannot have emacsclient evaluate lisp to open the file and invoke
;;    artemis-mode, since it will exit immediately and hence not work as EDITOR
(add-hook 'server-visit-hook 'server-visit-artemis-issue-hook)

(provide 'artemis-mode)
;;; artemis-mode.el ends here

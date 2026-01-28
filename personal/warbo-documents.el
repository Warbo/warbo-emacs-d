;;; warbo-documents --- Helpers for reading PDFs, etc. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package doc-view
  :functions (doc-view-fit-width-to-window)
  :custom
  (doc-view-continuous t)
  :config
  ;; Useful for manual entry of PDF titles into BibTeX
  (defun take-name ()
    "Find the next localfile key which doesn't have a title."
    (re-search-forward "^@misc{zzzzz.*,[\n][\t]localfile = \"[^\"]*\"[\n]")
    (forward-line -1)
    (beginning-of-line)
    (re-search-forward "localfile = ")
    ;; Copy the contents
    (forward-char)  ;; Past "
    (set-mark (point))
    (end-of-line)
    (backward-char) ;; Past "
    ;; Open the file in a temporary doc-view buffer
    (let ((selection (buffer-substring-no-properties (mark) (point)))
          (title     ""))
      (with-temp-buffer
        (insert-file-contents selection)
        (doc-view-mode)
        (switch-to-buffer (current-buffer))
        (sit-for 2)
        (doc-view-fit-width-to-window)
        ;; Query for the title
        (setq title (read-from-minibuffer "Title: ")))
      ;; Insert a title key into the BibTeX
      (end-of-line)
      (insert ",\n\ttitle = \"")
      (insert title)
      (insert "\""))))

(provide 'warbo-documents)
;;; warbo-documents.el ends here

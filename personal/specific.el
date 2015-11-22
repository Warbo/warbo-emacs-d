;; Make doc-view continuous
(setq doc-view-continuous t)

;; Proof General
(defun init-pg ()
  (ignore-errors (load-file "~/.nix-profile/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el")))

;; Useful for manual entry of PDF titles into BibTeX
(defun take-name ()
  ;; Find the next localfile key which doesn't have a title
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
    (insert "\"")))

;; Don't run Flymake over TRAMP
(if (boundp 'flymake-allowed-file-name-masks)
    (setq flymake-allowed-file-name-masks
          (cons '("^/ssh:" (lambda () nil))
                flymake-allowed-file-name-masks)))

;; Make doc-view continuous
(setq doc-view-continuous t)

;; Browse with eww, fallback to conkeror with "&"
(setq browse-url-browser-function 'eww-browse-url) ; use eww as default browser
(setq browse-url-generic-program (executable-find "conkeror")
      shr-external-browser 'browse-url-generic)

;; Haskell programming
(require 'use-package)
(use-package dante
             :ensure t
             :commands 'dante-mode
             :init
             (add-hook 'haskell-mode-hook 'dante-mode)
             (add-hook 'haskell-mode-hook 'flycheck-mode))

(add-hook 'haskell-mode-hook 'dante-mode)
(add-hook 'haskell-mode-hook 'flycheck-mode)
;(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(eval-after-load "haskell-mode"
  '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))

(eval-after-load "haskell-cabal"
  '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))

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

;; Try to prevent "undo-tree-mapc: Wrong type argument: listp, \.\.\."
;; From http://defindit.com/readme_files/tom_emacs.html
(when (file-exists-p ".emacs.desktop")
  (setq desktop-path '("."))
  (desktop-save-mode 1))

;; Try to prevent TRAMP slowdowns, as per
;; http://emacs.stackexchange.com/a/17579/5391
(setq projectile-mode-line "Projectile")

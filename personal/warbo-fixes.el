;;; warbo-fixes --- Prevent known breakages -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Try to prevent "undo-tree-mapc: Wrong type argument: listp, \.\.\."
;; From http://defindit.com/readme_files/tom_emacs.html
(use-package desktop
  :if (file-exists-p ".emacs.desktop")
  :custom
  (desktop-path '("."))
  :config
  (desktop-save-mode 1))

;; Prevent Nix '.drv' files from opening as LaTeX
(add-to-list 'auto-mode-alist '("\\.drv\\'" . fundamental-mode))

(provide 'warbo-fixes)
;;; warbo-fixes.el ends here

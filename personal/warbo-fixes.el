;; Try to prevent "undo-tree-mapc: Wrong type argument: listp, \.\.\."
;; From http://defindit.com/readme_files/tom_emacs.html
(when (file-exists-p ".emacs.desktop")
  (setq desktop-path '("."))
  (desktop-save-mode 1))

;; Prevent Nix '.drv' filesf rom opening as LaTeX
(add-to-list 'auto-mode-alist '("\\.drv\\'" . fundamental-mode))

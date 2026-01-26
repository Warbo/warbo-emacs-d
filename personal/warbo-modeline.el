;;; warbo-modeline.el --- Warbo's Emacs config: Modeline settings -*- lexical-binding: t; -*-
;;; Commentary:
;;; Configuration for Emacs modeline display.
;;; Code:
;; TODO: Fix "Cannot load diminish" error

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(use-package diminish
  :config
  (mapcar 'diminish '(editorconfig-mode
                      whitespace-cleanup-mode
                      flyspell-mode
                      company-mode
                      projectile-mode
                      eldoc-mode
                      visual-line-mode
                      smartparens-mode
                      flycheck-mode
                      lsp-lens-mode
                      beacon-mode
                      pretty-sha-path-mode
                      volatile-highlights-mode
                      undo-tree-mode
                      anzu-mode)))

(provide 'warbo-modeline)
;;; warbo-modeline.el ends here

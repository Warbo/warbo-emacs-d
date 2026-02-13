;;; warbo --- Set up treesitter modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package treesit
  :defines (treesit-font-lock-level)
  :config
  (setq treesit-font-lock-level 4))

;; auto-install missing grammars
(use-package treesit-auto
  :ensure t
  :disabled
  :after tree-sitter
  :functions (global-treesit-auto-mode)
  :config
  (global-treesit-auto-mode))

;; TODO: See what the keybindings are, if any; and remap if desired
(use-package treesit-fold
  :ensure t
  ;; FIXME: Use quelpa?
  :vc (:url "https://github.com/emacs-tree-sitter/treesit-fold")
  :functions (treesit-fold-mode)
  :config
  (treesit-fold-mode))

(provide 'warbo-treesitter)
;;; warbo-treesitter.el ends here

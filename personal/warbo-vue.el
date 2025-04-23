(use-package web-mode
  :ensure t
  :config
  (setq web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-sql-indent-offset 2
        web-mode-script-padding 0       ; start script in col 0
        web-mode-enable-current-column-highlight t
        )
  (define-derived-mode vue-mode web-mode "GO.Vue"
    "A major mode derived from web-mode, for editing .vue files with LSP support.")
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
  )

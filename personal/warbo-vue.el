;;; warbo-vue.el --- A major mode for Vue.js files -*- lexical-binding: t -*-

;;; Commentary:

;; This package provides `vue-mode', a major mode derived from `web-mode' for
;; editing Vue.js files.

;;; Code:

(use-package web-mode
  :ensure t
  :mode (("\\.html" . web-mode)
         ;; Haskell shakespeare templates
         ("\\.hamlet" . web-mode)
         ("\\.lucius" . web-mode)
         ("\\.julius" . web-mode))
  :config
  (setq web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-sql-indent-offset 2
        web-mode-script-padding 0  ;; start script in col 0
        web-mode-enable-current-column-highlight t
        )
  (define-derived-mode vue-mode web-mode "GO.Vue"
    "A major mode derived from web-mode, for editing .vue files with LSP support.")
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
  )

(provide 'warbo-vue)
;;; warbo-vue.el ends here

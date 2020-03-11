(use-package scala-mode
  :ensure t
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package sbt-mode
  :ensure t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package lsp-mode
  :ensure t
  ;; Optional - enable lsp-mode automatically in scala files
  :hook  (scala-mode . lsp)
  (lsp-mode . lsp-lens-mode)
  :config (setq lsp-prefer-flymake nil))

;; Enable nice rendering of documentation on hover
(use-package lsp-ui
  :ensure t)

;; lsp-mode supports snippets, but in order for them to work you need to use
;; yasnippet. If you don't want to use snippets set lsp-enable-snippet to nil in
;; your lsp-mode settings to avoid odd behavior with snippets and indentation
(use-package yasnippet
  :ensure t)

;; Add company-lsp backend for metals
(use-package company-lsp
  :ensure t)

;; Posframe is a pop-up tool that must be manually installed for dap-mode
(use-package posframe
  :ensure t)

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package dap-mode
  :ensure t
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

;; Use the Tree View Protocol for viewing the project structure and triggering
;; compilation
(use-package lsp-treemacs
  :ensure t
  :config
  (lsp-metals-treeview-enable t)
  (setq lsp-metals-treeview-show-when-views-received t))

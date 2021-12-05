;;; package --- summary

;;; Commentary:

;;; Code:

(use-package direnv
  :ensure t
  :config
  (add-to-list 'direnv-non-file-modes 'shell-mode)
  (direnv-mode))

(use-package cue
  :ensure t
  :quelpa (cue-mode :fetcher github
                    :repo    "russell/cue-mode")
  :mode (("\\.cue\\'"  . cue-mode)))

(use-package js2-mode
  :ensure t)

(use-package js2-refactor
  :ensure t)

(use-package xref-js2
  :ensure t)

(use-package json-mode
  :ensure t)

;; Unset some conflicting keybindings before binding them to magit
(global-unset-key (kbd "s-m"))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("s-m m" . magit-status)
         ("s-m l" . magit-log)
         ("s-m f" . magit-log-buffer-file)
         ("s-m b" . magit-blame))
  :init
  (setq magit-diff-paint-whitespace t)
  (setq magit-diff-highlight-trailing t)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package magit-popup
  :ensure t)

(use-package make-mode
  ;; makefile-mode is built-in, so doesn't need downloading
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\Makefile\\'" . makefile-mode))
    (add-to-list 'auto-mode-alist '("\\.mk\\'"      . makefile-mode))

    (font-lock-add-keywords
     'makefile-mode
     '(("define" . font-lock-keyword-face)
       ("endef"  . font-lock-keyword-face)
       ("ifeq"   . font-lock-keyword-face)
       ("ifneq"  . font-lock-keyword-face)
       ("ifdef"  . font-lock-keyword-face)
       ("ifndef" . font-lock-keyword-face)
       ("else"   . font-lock-keyword-face)
       ("endif"  . font-lock-keyword-face)))

    (add-hook 'makefile-mode-hook 'linum-mode)
    (add-hook 'makefile-mode-hook
              (lambda ()
                (setq whitespace-style '(face tab-mark trailing))
                      indent-tabs-mode t))))

(use-package nix-mode
  :ensure t)

(use-package smartparens
  :ensure t)

;; Use the modeline to show the definition of the function at point
(use-package which-func
  :config
  (progn
    ;; TODO: Would we rather append major modes to this list as and when they
    ;; benefit from which-func; rather than having it always enabled?
    ;; TODO: Check elisp, haskell, scala, JS, python, etc.
    ;(setq which-func-modes nil)
    (which-function-mode 1)))

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package lsp-mode
  :quelpa (lsp-mode :fetcher github
                    :repo    "emacs-lsp/lsp-mode")
  :ensure t
  ;:defer  t
  ;; Optional - enable lsp-mode automatically in scala files
  :hook  (scala-mode . lsp)
  (lsp-mode . lsp-lens-mode)
  :config (progn
            (setq lsp-prefer-flymake nil)
            ;; Avoids the following error:
            ;; Error running timer ‘lsp--on-idle’:
            ;; (error "The connected server(s) does not support method
            ;; textDocument/documentHighlight.
            (setq lsp-enable-links nil)
            (setq lsp-restart 'auto-restart)
            ;; Better performance than 4k default
            (setq read-process-output-max (* 1024 1024))
            ;; Useful for debugging, but ver slow otherwise
            (setq lsp-log-io nil)))

;; Enable nice rendering of documentation on hover
(use-package lsp-ui
  :ensure t
  :defer  t)

;; lsp-mode supports snippets, but in order for them to work you need to use
;; yasnippet. If you don't want to use snippets set lsp-enable-snippet to nil in
;; your lsp-mode settings to avoid odd behavior with snippets and indentation
(use-package yasnippet
  :ensure t)

;; Add company-lsp backend for metals
(use-package company-lsp
  :ensure t
  :defer  t)

;; Posframe is a pop-up tool that must be manually installed for dap-mode
(use-package posframe
  :ensure t)

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package lsp-java
  :ensure t
  :defer  t)

(use-package dap-mode
  :ensure t
  :defer  t
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

;; Use the Tree View Protocol for viewing the project structure and triggering
;; compilation
(use-package lsp-treemacs
  :ensure t
  :defer  t
  :config
  (lsp-metals-treeview-enable t)
  (setq lsp-metals-treeview-show-when-views-received t))

(use-package typescript-mode
  :ensure t
  :mode (("\\.ts\\'"  . typescript-mode)
         ("\\.tsx\\'" . typescript-mode)))

;; We can hook into prog-mode to affect any programming-related buffer
(add-hook 'prog-mode-hook
          (lambda ()
            (when (executable-find ispell-program-name)
              (flyspell-prog-mode))
            (smartparens-mode +1)
            (set (make-local-variable 'comment-auto-fill-only-comments) t)

            ;; Highlight a bunch of well known comment annotations.
            (font-lock-add-keywords
             nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
                    1 font-lock-warning-face t)))))

;; smart curly braces
(sp-pair "{" nil :post-handlers
         '(((lambda (&rest _ignored)
              (crux-smart-open-line-above)) "RET")))

;; Enable on-the-fly syntax checking
(if (fboundp 'global-flycheck-mode)
    (global-flycheck-mode +1)
  (add-hook 'prog-mode-hook 'flycheck-mode))

(mac-only
 (setq flycheck-global-modes '(not c-mode)))

(provide 'programming)
;;; programming.el ends here

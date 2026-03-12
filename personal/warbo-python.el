;;; warbo-python --- Programming functionality specific to Python -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package python
  :mode ("\\.py\\'" . python-ts-mode)
  :custom
  (python-shell-interpreter "python3")
  (python-shell-buffer-name "Python3")
  :config
  ;;(add-hook 'python-mode-hook 'blacken-mode)
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  )

;; (use-package pyvenv
;;   :ensure t
;;   :config
;;   (setq pyvenv-exec-shell "zsh"))

;; (use-package blacken
;;   :ensure t)

(use-package elpy
  :disabled
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq elpy-get-info-from-shell t
        python-shell-interpreter "/run/current-system/sw/bin/python3")
  (custom-set-variables
   '(elpy-rpc-python-command "/run/current-system/sw/bin/python3")
   '(elpy-test-discover-runner-command
     '("/run/current-system/sw/bin/python3" "-m" "unittest")))
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "/run/current-system/sw/bin/python3"))

(use-package cython-mode
  :ensure t
  :mode ("\\.pxd\\'" "\\.pyd\\'" "\\.pyi\\'" "\\.pyx\\'"))

(provide 'warbo-python)
;;; warbo-python.el ends here

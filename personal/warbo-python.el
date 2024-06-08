(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :config
  ;(add-hook 'python-mode-hook 'blacken-mode)
  (custom-set-variables '(python-shell-interpreter "python3")
                        '(python-shell-buffer-name "Python3")))

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

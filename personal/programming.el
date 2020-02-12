;;; package --- summary

;;; Commentary:

;;; Code:

(use-package json-mode
  :ensure t)

;; Unset some conflicting keybindings first
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

(use-package nix-mode
  :ensure t)

(use-package scala-mode
  :ensure t)

(use-package smartparens
  :ensure t)

(provide 'programming)
;;; programming.el ends here

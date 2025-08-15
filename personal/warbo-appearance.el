;;; warbo-appearance.el --- Warbo's Emacs config: Appearance settings -*- lexical-binding: t; -*-
;;; Commentary:
;;; Configuration for Emacs UI appearance and visual feedback.
;;; Code:

(menu-bar-mode -1)
(blink-cursor-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(fset 'yes-or-no-p 'y-or-n-p)

(setq frame-title-format
      '("" invocation-name " - " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))

(use-package beacon
  :ensure t
  :config
  (beacon-mode +1))

(provide 'warbo-appearance)
;;; warbo-appearance.el ends here

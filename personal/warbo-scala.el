;;; warbo-scala --- Programming functionality specific to Scala -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package scala-ts-mode
  :ensure t
  :mode "\\.s\\(cala\\|bt\\)$"
  :bind (:map scala-mode-map
              ("C-c C-c" . sbt-run-previous-command))
  :config
  (add-hook 'scala-mode-hook 'scala-format-on-save-mode))

(provide 'warbo-scala)
;;; warbo-scala.el ends here

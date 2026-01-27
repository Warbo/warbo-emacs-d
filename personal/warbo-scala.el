;;; warbo-scala --- Programming functionality specific to Scala -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package scala-mode
  :ensure t
  :mode "\\.s\\(cala\\|bt\\)$"
  :bind (:map scala-mode-map
              ("C-c C-c" . sbt-run-previous-command)))

(use-package yasnippet
  :ensure t)

;; Posframe is a pop-up tool that must be manually installed for dap-mode
(use-package posframe
  :ensure t)

(provide 'warbo-scala)
;;; warbo-scala.el ends here

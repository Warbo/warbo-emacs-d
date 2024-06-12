;;; warbo-lisp --- Functionality specific to Lisp/Scheme/S-expressions
;;; Commentary:
;;; Code:

;; Make parentheses dimmer when editing LISP
;; (defface paren-face
;;   '((((class color) (background dark))
;;      (:foreground "grey30"))
;;     (((class color) (background light))
;;      (:foreground "grey30")))
;;   "Face used to dim parentheses.")

;; (mapcar (lambda (mode)
;;           (add-hook mode
;;                     (lambda ()
;;                       (font-lock-add-keywords nil
;;                                               '(("(\\|)" . 'paren-face))))))
;;         '(emacs-lisp-mode-hook scheme-mode-hook racket-mode-hook))

;; Force paredit mode in Lisp buffers
(use-package paredit
  :ensure t
  :init
  (dolist (hook '(emacs-lisp-mode-hook
                     clojure-mode-hook
                        lisp-mode-hook
                      racket-mode-hook
                      scheme-mode-hook))
    (add-hook hook 'paredit-mode))
  :diminish paredit-mode)

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t)

;; Unit testing for ELisp
(defer (lambda ()
         (require 'ert)
         (defun ert-silently ()
           (interactive)
           (ert t))
         (define-key emacs-lisp-mode-map       (kbd "C-x r") 'ert-silently)
         (define-key lisp-interaction-mode-map (kbd "C-x r") 'ert-silently)))

(provide 'warbo-lisp)
;;; warbo-lisp.el ends here

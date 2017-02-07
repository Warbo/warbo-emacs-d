;; Make parentheses dimmer when editing LISP
(defface paren-face
  '((((class color) (background dark))
     (:foreground "grey30"))
    (((class color) (background light))
     (:foreground "grey30")))
  "Face used to dim parentheses.")

(mapcar (lambda (mode)
          (add-hook mode
                    (lambda ()
                      (font-lock-add-keywords nil
                                              '(("(\\|)" . 'paren-face))))))
        '(emacs-lisp-mode-hook scheme-mode-hook racket-mode-hook))

;; Unit testing for ELisp
(defer (lambda ()
         (require 'ert)
         (defun ert-silently ()
           (interactive)
           (ert t))
         (define-key emacs-lisp-mode-map       (kbd "C-x r") 'ert-silently)
         (define-key lisp-interaction-mode-map (kbd "C-x r") 'ert-silently)))

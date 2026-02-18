;;; warbo-lisp --- Functionality specific to Lisp/Scheme/S-expressions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(declare-function defer "warbo-preload-generic")

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
;; Scroll key handling is now done by warbo-scroll-keys.el
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
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t)

(use-package racket-mode
  :ensure t)

(defun function-source (name)
  "Return a string of source code for the function with the given NAME."
  (let ((file-pos (find-function-noselect name)))
    (with-current-buffer (car file-pos)
      (goto-char (cdr file-pos))
      (let ((start (point)))
        (forward-sexp)
        (buffer-substring-no-properties start (point))))))

(defun reload-function-definition (name)
  "Re-evaluats the definition of function NAME, e.g. to disable ERT."
  (let ((file-pos (find-function-noselect name)))
    (with-current-buffer (car file-pos)
      (goto-char (cdr file-pos))
      (eval-defun nil))))

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

;;; warbo-web --- Setup for Web browsing in Emacs
;;; Commentary:
;;; Code:

;; From https://github.com/GriffinSchneider/emacs-config eww-customizations.el
(require 'eww)

(defvar gcs-shr-width 110)

(define-advice shr-insert-document (force-shr-width :around (orig-fun &rest args))
  "From https://github.com/GriffinSchneider/emacs-config eww-customizations.el"
  (let ((shr-width (min (1- (window-width)) gcs-shr-width)))
    (apply orig-fun args)))

(defun eww-increase-width ()
  (interactive)
  (make-local-variable 'gcs-shr-width)
  (setq gcs-shr-width  (+ 10 gcs-shr-width))
  (eww-reload))
(define-key eww-mode-map (read-kbd-macro "+") 'eww-increase-width)

(defun eww-decrease-width ()
  (interactive)
  (make-local-variable 'gcs-shr-width)
  (setq gcs-shr-width  (- gcs-shr-width 10))
  (eww-reload))
(define-key eww-mode-map (read-kbd-macro "-") 'eww-decrease-width)

;; Browse with Firefox, since eww seems to barf on news fallback to Firefox
;; with "&"
;;(setq browse-url-browser-function 'eww-browse-url)
(setq browse-url-browser-function 'browse-url-firefox)
(setq browse-url-generic-program (executable-find "firefox")
      shr-external-browser 'browse-url-generic)

;; Set the default browser to Debian's default.
;; W3M and Firefox are good choices.
;(setq browse-url-browser-function 'w3m-browse-url)
;(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; (setq browse-url-browser-function 'browse-url-generic
;;       browse-url-generic-program "firefox")

;;(use-package w3m
;;  :ensure t
;;  :config
;;  (progn
;;    (setq w3m-default-display-inline-images t
;;          w3m-search-default-engine         "duckduckgo"
;;          w3m-use-cookies           t)))

(provide 'warbo-web)
;;; warbo-web.el ends here

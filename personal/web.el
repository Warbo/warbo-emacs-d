;; From https://github.com/GriffinSchneider/emacs-config eww-customizations.el
(require 'eww)

(defvar gcs-shr-width 110)

(defadvice shr-insert-document (around force-shr-width activate)
  (let ((shr-width (min (1- (window-width)) gcs-shr-width)))
    ad-do-it))

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

(setq w3m-search-default-engine "duckduckgo")

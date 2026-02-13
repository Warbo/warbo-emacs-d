;;; warbo-web --- Setup for Web tech -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Web development

(use-package html-ts-mode
  :mode (("\\.html" . html-ts-mode)
         ;; Haskell shakespeare templates
         ("\\.hamlet" . html-ts-mode)
         ("\\.lucius" . html-ts-mode)
         ("\\.julius" . html-ts-mode))
  :config
  (add-to-list 'major-mode-remap-alist '(html-mode . html-ts-mode))
  (add-to-list 'major-mode-remap-alist '(web-mode . html-ts-mode))
  ;; FIXME: Change these to html-ts-mode equivalents:
  ;; HTML Indent   web-mode-markup-indent-offset             html-ts-mode-indent-offset (or tab-width)
  ;; CSS Indent    web-mode-css-indent-offset                css-ts-mode-indent-offset
  ;; JS/TS Indent  web-mode-code-indent-offset               js-ts-mode-indent-offset
  ;; Padding       web-mode-script-padding                   Not supported (indentation is structural)
  ;; Highlight     web-mode-enable-current-column-highlight  Use external package (e.g., column-highlight-mode)
  ;; (setq web-mode-code-indent-offset 2
  ;;       web-mode-css-indent-offset 2
  ;;       web-mode-markup-indent-offset 2
  ;;       web-mode-sql-indent-offset 2
  ;;       web-mode-script-padding 0 ;; start script in col 0
  ;;       web-mode-enable-current-column-highlight t
  ;;       )
  )

(use-package css-mode
  :mode (("\\.css\\'" . css-ts-mode))
  :config
  (add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode)))

;; TODO: Probably not needed anymore
(use-package coffee-mode
  :ensure t
  :mode "\\.coffee\\'")

;; TODO: Probably not needed anymore
(use-package elm-mode
  :ensure t
  :mode "\\.elm\\'")

(use-package vue-ts-mode
  :quelpa (vue-ts-mode :fetcher github
                       :repo "8uff3r/vue-ts-mode")
  :mode ("\\.vue\\'")
  :config
  (defvar warbo-vue-eglot-args
    '("vue-language-server" "--stdio"
      :initializationOptions
      (:typescript (:tsdk "node_modules/typescript/lib")
       :vue (:hybridMode :json-false)
             :languageFeatures (:completion (:defaultTagNameCase "both"
                                             :defaultAttrNameCase "kebabCase"
                                             :getDocumentNameCasesRequest nil
                                             :getDocumentSelectionRequest nil)
                                :diagnostics (:getDocumentVersionRequest nil))
             :documentFeatures (:documentFormatting
                                (:defaultPrintWidth 100
                                 :getDocumentPrintWidthRequest nil)
                                :documentSymbol t
                                :documentColor t)))
    "Eglot server program entry for vue-ts-mode.
Use in .dir-locals.el like:
  (eglot-server-programs . ((vue-ts-mode . ,warbo-vue-eglot-args)))")
  )

;; Web browsing

;; From https://github.com/GriffinSchneider/emacs-config eww-customizations.el
(require 'eww)

(defvar gcs-shr-width 110)

(define-advice shr-insert-document
    (:around (orig-fun &rest args) force-shr-width)
  "From https://github.com/GriffinSchneider/emacs-config eww-customizations.el."
  (let ((shr-width (min (1- (window-width)) gcs-shr-width)))
    (apply orig-fun args)))

;; TODO: Bindings should go in a use-package block
(defun eww-increase-width ()
  "Increase width then reload page."
  (interactive)
  (make-local-variable 'gcs-shr-width)
  (setq gcs-shr-width  (+ 10 gcs-shr-width))
  (eww-reload))
(define-key eww-mode-map (read-kbd-macro "+") 'eww-increase-width)

(defun eww-decrease-width ()
  "Reduce width then reload page."
  (interactive)
  (make-local-variable 'gcs-shr-width)
  (setq gcs-shr-width  (- gcs-shr-width 10))
  (eww-reload))
(define-key eww-mode-map (read-kbd-macro "-") 'eww-decrease-width)

;; Browse with Firefox, since eww seems to barf on news fallback to Firefox
;; with "&"
;;(setq browse-url-browser-function 'eww-browse-url)
(setq browse-url-browser-function 'browse-url-firefox)
(setq browse-url-secondary-browser-function 'browse-url-firefox)
(setq browse-url-generic-program (executable-find "firefox"))

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

;;; warbo-early-init --- Set up Emacs packaging, etc. before init.el
;;; Commentary:
;;; Code:
(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives
             '("gnu"       . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("melpa"     . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org"       . "http://orgmode.org/elpa/"))
;; Certificate expired?
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))


(provide 'early-init)
;;; early-init.el ends here

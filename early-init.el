;;; warbo-early-init --- Set up Emacs packaging, etc. before init.el
;;; Commentary:
;;; Code:
(unless (or (getenv "EMACS_UNDER_TEST")
            (and (boundp 'server-clients) server-clients))
  (server-start))

(require 'package)
(setq package-enable-at-startup nil)

;(add-to-list 'package-archives
;             '("gnu"       . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
            '("melpa"     . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(message "Loading personal/preload/*.el files")
(let ((preload-dir (expand-file-name "personal/preload"
                                     (file-name-directory load-file-name))))
  (when (file-exists-p preload-dir)
    (message "Loading personal configuration files in %s..." preload-dir)
    (mapc 'load (directory-files preload-dir 't "^[^#].*el$"))))

(provide 'early-init)
;;; early-init.el ends here

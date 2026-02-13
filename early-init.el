;;; warbo-early-init --- Set up Emacs packaging, etc. before init.el
;;; Commentary:
;;; Code:
(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives
            '("melpa"     . "https://melpa.org/packages/"))

(message "Loading personal/preload/*.el files")
(let ((preload-dir (expand-file-name "personal/preload" user-emacs-directory)))
  (when (file-exists-p preload-dir)
    (add-to-list 'load-path preload-dir)
    (message "Loading personal configuration files in %s..." preload-dir)
    (mapc 'load (directory-files preload-dir 't "^[^#].*el$"))))

(provide 'early-init)
;;; early-init.el ends here

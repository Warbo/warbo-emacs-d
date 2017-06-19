(defun defer (f)
  (run-with-idle-timer 2 nil f))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Turn off pesky scrollbars
(scroll-bar-mode -1)
(menu-bar-mode -1)

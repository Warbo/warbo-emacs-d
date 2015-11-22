(defun defer (f)
  (run-with-idle-timer 2 nil f))

;; Turn off pesky scrollbars
(scroll-bar-mode -1)
(menu-bar-mode -1)

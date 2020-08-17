;; Settings that only make sense on macOS
(mac-only

 (setq ns-function-modifier 'hyper)

 (defun prelude-swap-meta-and-super ()
   "Swap the mapping of Meta and Super. Very useful for people using their Mac
    with a Windows external keyboard from time to time."
   (interactive)
   (if (eq mac-command-modifier 'super)
       (progn
         (setq mac-command-modifier 'meta)
         (setq mac-option-modifier 'super)
         (message "Command is now bound to META and Option is bound to SUPER."))
     (progn
       (setq mac-command-modifier 'super)
       (setq mac-option-modifier 'meta)
       (message "Command is now bound to SUPER and Option is bound to META."))))

 ;; Fix Option-3 to give # on UK Mac keyboard. Emacs sees Option as Alt (AKA
 ;; Meta AKA M) which Emacs will intercept and complain that it isn't a command
 ;; Taken from https://stackoverflow.com/a/45648401
 (define-key key-translation-map (kbd "M-3") (kbd "#"))

 ;; Use the global menu bar on macOS, since it auto-hides
 (menu-bar-mode +1)

 (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t   ))
 (add-to-list 'default-frame-alist '(ns-appearance           . dark))

 ;; Enable emoji, and stop the UI from freezing when trying to display them.
 (if (fboundp 'set-fontset-font)
     (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

  ;(define-key prelude-mode-map (kbd "C-c w") 'prelude-swap-meta-and-super)
  ;(define-key prelude-mode-map (kbd "s-/") 'hippie-expand)
 )

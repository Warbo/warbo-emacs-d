;; Settings that only make sense on macOS
(mac-only

 ;; Set our modifier keys. Note that our Karabiner config turns the left Ctrl
 ;; key into a Command key; this is useful in most programs but not Emacs, so
 ;; we switch it back here (I don't think we ever need a Command modifier in
 ;; Emacs, so this should be safe).

 (setq ns-function-modifier 'hyper  )  ;; Fn key awkwardly placed in bottom left
 (setq mac-command-modifier 'control)  ;; Turn Ctrl key back into Control
 (setq mac-option-modifier  'meta   )  ;; Left Option/Alt should always be Meta

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

  ;(define-key prelude-mode-map (kbd "s-/") 'hippie-expand)
 )

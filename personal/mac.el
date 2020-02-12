;; Settings that only make sense on macOS
(mac-only

 ;; On OS X Emacs doesn't use the shell PATH if it's not started from
 ;; the shell. Let's fix that:
 ;; FIXME: Don't use prelude
 (prelude-require-packages '(exec-path-from-shell))

 (require 'exec-path-from-shell)
 (exec-path-from-shell-initialize)

 ;; exec-path-from-shell will overwrite PATH and exec-path, so we need to add
 ;; Nix paths on to the front again
 (add-nix-to-path)
 
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

 ;; Use the global menu bar on macOS, since it auto-hides
 (menu-bar-mode +1)

 ;; Enable emoji, and stop the UI from freezing when trying to display them.
 (if (fboundp 'set-fontset-font)
     (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))


  ;(define-key prelude-mode-map (kbd "C-c w") 'prelude-swap-meta-and-super)
  ;(define-key prelude-mode-map (kbd "s-/") 'hippie-expand)
 )

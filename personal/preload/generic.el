(defun defer (f)
  "Defer calling the function F until Emacs has finished initialising."
  (run-with-idle-timer 2 nil f))

(defconst machine-id (cond
                      ((file-directory-p "/Users/chris") 'mac)
                      ((file-directory-p "/home/chris")  'thinkpad)
                      (t                                 'unknown)))

;; See which per-machine options we should enable
(defmacro mac-only (&rest body)
  `(when (equal machine-id 'mac)
      ,@body))

(defmacro thinkpad-only (&rest body)
  `(when (equal machine-id 'thinkpad)
     ,@body))

;; Set PATH, so external commands will work (extra important if we're not on NixOS)
(require 'subr-x)
(thinkpad-only
 (setenv "PATH"
         (string-join `("/run/current-system/sw/bin"
                        ,(getenv "PATH")
                        "/home/chris/.nix-profile/bin"
                        "/home/chris/System/Programs/bin")
                      ":"))
 (setq exec-path (append '("/run/current-system/sw/bin")
                         exec-path
                         '("/home/chris/.nix-profile/bin"
                           "/home/chris/System/Programs/bin"))))
(mac-only
   ;; The prelude-osx.el file will be run when we're on macOS, and that runs
   ;; exec-path-from-shell to inherit env vars from the shell Emacs has been
   ;; launched from (if any). This overrides our PATH fix, so we need to do it
   ;; again after that's been run.
  (defun add-nix-to-path ()
    (message "Setting macOS PATH to use Nix")
    (let ((pre-env (getenv "PATH"))
          (pre-var exec-path))
      (setenv "PATH"
              (string-join `("/run/current-system/sw/bin"
                             ,(getenv "PATH"))
                           ":"))
      (setq exec-path (append '("/run/current-system/sw/bin")
                              exec-path))
      (when (or (equal (getenv "PATH") pre-env)
                (equal exec-path       pre-var))
        (message "WARNING: Failed to update PATH"))))
  (add-nix-to-path)
  (defadvice exec-path-from-shell-initialize (after mac-path-fix)
    (add-nix-to-path)))

;; Turn off UI clutter
(scroll-bar-mode -1)
(menu-bar-mode -1)

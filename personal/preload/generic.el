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

;; Set PATH and 'exec-path', so external commands will work.
;; TODO: Check if this is still needed, or if there's a cleaner approach
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
 ;; Nix does some environment fiddling in its default bashrc. The following flag
 ;; gets set once it's configured, under the assumption that child processes
 ;; will inherit the fixed env vars. Since Emacs on macOS seems to screw these
 ;; up, we unset the flag, which causes our shells to re-do the fiddling.
 ;; NOTE: If you get Apple popups asking you to install developer tools for git,
 ;; gcc, etc. then this variable is the culprit!
 (setenv "__NIX_DARWIN_SET_ENVIRONMENT_DONE" "")

 (let ((extra '("/Users/chris/.nix-profile/bin"
                "/run/current-system/sw/bin"
                "/nix/var/nix/profiles/default/bin"
                "/usr/local/bin"
                "/usr/bin"
                "/usr/sbin"
                "/bin"
                "/sbin")))
   (setenv "PATH" (string-join (append extra (list (getenv "PATH"))) ":"))
   (setq exec-path (append extra exec-path))))

;; Set up other env vars early, so they're inherited by shells
;; Set a reasonable value for COLUMNS, e.g. for shell buffers
(setenv "COLUMNS" "80")

;; Turn off UI clutter
(scroll-bar-mode -1)
(menu-bar-mode -1)

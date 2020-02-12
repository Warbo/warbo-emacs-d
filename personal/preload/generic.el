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

(require 'subr-x)
(defun add-nix-to-path ()
  "Set PATH and 'exec-path', so external commands will work.
This is extra important if we're not on NixOS.  Note that this needs to be run
again if something else overwrites the PATH and 'exec-path' (e.g. this can
happen on macOS)."
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
   (setenv "PATH"
           (string-join `("/run/current-system/sw/bin"
                          ,(getenv "PATH"))
                        ":"))
   (setq exec-path (append '("/run/current-system/sw/bin")
                           exec-path))))

(add-nix-to-path)

;; Turn off UI clutter
(scroll-bar-mode -1)
(menu-bar-mode -1)

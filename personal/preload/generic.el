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

;; Nix does some environment fiddling in its default bashrc. The following flag
;; gets set once it's configured, under the assumption that child processes will
;; inherit the fixed env vars. Since Emacs on macOS seems to screw these up, we
;; unset the flag, which causes our shells to re-do the fiddling.
(mac-only
 (setenv "__NIX_DARWIN_SET_ENVIRONMENT_DONE" ""))

(mac-only
 ;; Hackily hard-coded, but seems to work for now
 ;; TODO: Would be nicer to string-join these from a list
 (setenv "NIX_PATH" "darwin-config=/Users/chris/.nixpkgs/darwin-configuration.nix:/nix/var/nix/profiles/per-user/root/channels:/Users/chris/.nix-defexpr/channels"))

;; Turn off UI clutter
(scroll-bar-mode -1)
(menu-bar-mode -1)

(defun defer (f)
  "Defer calling the function F until Emacs has finished initialising."
  (run-with-idle-timer 2 nil f))

(defconst machine-id (cond
                      ((file-directory-p "/Users"     ) 'mac)
                      ((file-directory-p "/home/chris") 'thinkpad)
                      (t                                'unknown)))

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

 (let ((extra `(,(concat (getenv "HOME") "/.nix-profile/bin")
                "/run/current-system/sw/bin"
                "/nix/var/nix/profiles/default/bin"
                "/usr/local/bin"
                "/usr/bin"
                "/usr/sbin"
                "/bin"
                "/sbin")))
   (setenv "PATH" (string-join (append extra (list (getenv "PATH"))) ":"))
   (setq exec-path (append extra exec-path)))

 (setq explicit-shell-file-name (executable-find "wrappedShell"))

 (unless (getenv "SSL_CERT_FILE")
   (require 'seq)
   (seq-do (lambda (path) (setenv "SSL_CERT_FILE" path))
           (seq-take
            (seq-filter
             'file-exists-p
             '("/run/current-system/etc/ssl/certs/ca-certificates.crt"
               "/run/current-system/etc/ssl/certs/ca-bundle.crt"
               "/nix/var/nix/profiles/default/etc/ssl/certs/ca-certificates.crt"
               "/nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt"
               "~/.nix-profile/etc/ssl//certs/ca-certificates.crt"
               "~/.nix-profile/etc/ssl//certs/ca-bundle.crt"))
            1))))

;; Set up other env vars early, so they're inherited by shells
;; Set a reasonable value for COLUMNS, e.g. for shell buffers
(setenv "COLUMNS" "80")

;; Turn off UI clutter
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; I actually like tab-bar-mode, since:
;;  - Each tab can contain multiple windows, just like a frame
;;  - The tabs show their current buffer name
;; My old workflow was to create a frame for each task/project I was currently
;; working on, e.g. if I'm waiting for some command to finish in ProjectA, I can
;; leave its frame open, and work on ProjectB in another frame (on another
;; desktop) in the mean time.
;; The downside of using frames is they're hard to distinguish visually; so it
;; might require a few desktop switches to find the one I want. This is less of
;; an issue with tabs, since they show the buffer names :)
(tab-bar-mode 1)

(provide 'generic)
;;; generic.el ends here

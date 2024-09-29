(defun defer (f)
  "Defer calling the function F until Emacs has finished initialising."
  (run-with-idle-timer 2 nil f))

(defconst machine-id
  (cond
   ((equal (system-name) "nixos-amd64")
    'nixos-amd64)
   ((and (file-directory-p "/home/chris")
         (not (file-exists-p "/run/current-system/sw/bin/pw-top")))
    'thinkpad)
   ((file-directory-p "/home/manjaro") 'manjaro)
   ((and (file-directory-p "/mnt/c/Users")
         (file-directory-p "/home/nixos"))
    'wsl)
   ((and (file-directory-p "/mnt/c/Users")
         (file-directory-p "/home/chrisw"))
    'wsl-ubuntu)
   (t                                  'unknown)))

;; See which per-machine options we should enable
(defmacro thinkpad-only (&rest body)
  "Only evaluate BODY iff on thinkpad."
  `(when (equal machine-id 'thinkpad)
     ,@body))

(defmacro manjaro-only (&rest body)
  "Only evaluate BODY iff on manjaro."
  `(when (equal machine-id 'manjaro)
     ,@body))

(defmacro wsl-only (&rest body)
  "Only evaluate BODY iff on wsl."
  `(when (equal machine-id 'wsl)
     ,@body))

(defmacro wsl-ubuntu-only (&rest body)
  "Only evaluate BODY iff on wsl-ubuntu."
  `(when (equal machine-id 'wsl-ubuntu)
     ,@body))

;; Set PATH and 'exec-path', so external commands will work.
(use-package exec-path-from-shell
  :commands exec-path-from-shell-initialize
  :custom
  (exec-path-from-shell-arguments '("-l"))
  :config
  (exec-path-from-shell-initialize))

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

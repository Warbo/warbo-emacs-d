;;; warbo-tramp --- Useful config for editing remote files -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'tramp)

;; Declare variable from projectile to silence byte-compiler
(defvar projectile-mode-line)

;; Don't go looking for version-control metadata on remotes (we can still call
;; magit whenever we like)
;; From https://www.gnu.org/software/tramp/#Frequently-Asked-Questions-1
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; Try to prevent TRAMP slowdowns, as per
;; http://emacs.stackexchange.com/a/17579/5391
(setq projectile-mode-line "Projectile")

;; From https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
(setq remote-file-name-inhibit-locks t
      ;; Move things using scp (rather than copying back and forth)
      tramp-use-scp-direct-remote-copying t
      ;; Avoid little roundtrips
      remote-file-name-inhibit-auto-save-visited t
      ;; Files under this size will be piped through ssh; avoiding the overhead
      ;; of spinning up a separate connection
      tramp-copy-size-limit (* 1024 1024) ;; 1MB

      ;; Ssh!
      tramp-verbose 2)

;; Use direct async processes (should make things faster)
;; Again, from https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))
(connection-local-set-profiles
 '(:application tramp :protocol "scp")
 'remote-direct-async-process)

;; Try multiplexing each host's SSH sessions through one connection. This should
;; avoid trying to open multiple connections to the same host, which can cause
;; "Forbidden reentrant call of Tramp" errors (e.g. when auto-complete fires off
;; requests in the background, and they collide with other Tramp processes)
(setq tramp-ssh-controlmaster-options
      (string-join
       '(;; Automatically start a re-usable, master connection
         "-o ControlMaster=auto"
         ;; Socket for the master connection. We want this path to "collide" for
         ;; every connection attempt to that host (to re-use existing connection
         ;; if present), whilst avoiding collisions with other hosts. Note these
         ;; paths can't be too long, or SSH rejects them.
         "-o ControlPath=~/.ssh/tramp-%%r@%%h:%%p"
         ;; Keep the connection open for 10 mins after last use.
         "-o ControlPersist=600")
       " "))

;; Make sure this is set in the same way as a normal shell (in case Emacs was
;; started as a SystemD unit in a different environment)
(let ((sock (string-trim (shell-command-to-string "bash -c 'printf \"%s\" \"$SSH_AUTH_SOCK\"'"))))
  (when (and sock (not (string-empty-p sock)))
    (setenv "SSH_AUTH_SOCK" sock)))

;; Add all the many non-FHS PATH entries we might want
(dolist (user '("chris" "chrisw" "jo" "manjaro" "user" "nixos"))
  (dolist (dir '(".nix-profile/bin" "bin" "System/Programs"))
    (add-to-list 'tramp-remote-path (concat "/home/" user "/" dir))))

;; Provides TRAMP remotes like /nspawn:myuser@mycontainer:/ including
;; auto-complete. If you hit 'Interactive authentication required', try hopping
;; from /sudo like '/sudo:root@localhost|nspawn:chrisw@nixos-basic:/'
(tramp-enable-method "nspawn")

(provide 'warbo-tramp)
;;; warbo-tramp.el ends here

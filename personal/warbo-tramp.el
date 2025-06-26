;;; warbo-tramp --- Useful config for editing remote files
;;; Commentary:
;;; Code:

(require 'tramp)

;; Don't run Flymake over TRAMP
(if (boundp 'flymake-allowed-file-name-masks)
    (setq flymake-allowed-file-name-masks
          (cons '("^/ssh:" (lambda () nil))
                flymake-allowed-file-name-masks)))

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
      ;; Avoid little rountrips
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

;; Make sure this is set in the same way as a normal shell (in case Emacs was
;; started as a SystemD unit in a different environment)
(setenv "SSH_AUTH_SOCK" (shell-command-to-string "bash -c 'printf $SSH_AUTH_SOCK'"))

;; Add all the many non-FHS PATH entries we might want
(dolist (user '("chris" "chrisw" "jo" "manjaro" "user" "nixos"))
  (dolist (dir '(".nix-profile/bin" "bin" "System/Programs"))
    (add-to-list 'tramp-remote-path (concat "/home/" user "/" dir))))

;; Provides TRAMP remotes like /nspawn:myuser@mycontainer:/ including
;; auto-complete. If you hit 'Interactive authentication required', try hopping
;; from /sudo like '/sudo:root@localhost|nspawn:chrisw@nixos-basic:/'
(with-eval-after-load 'tramp (tramp-enable-method "nspawn"))

(provide 'warbo-tramp)
;;; warbo-tramp.el ends here

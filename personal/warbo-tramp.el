;;; warbo-tramp --- Useful config for editing remote files
;;; Commentary:
;;; Code:

(require 'tramp)

;; Don't run Flymake over TRAMP
(if (boundp 'flymake-allowed-file-name-masks)
    (setq flymake-allowed-file-name-masks
          (cons '("^/ssh:" (lambda () nil))
                flymake-allowed-file-name-masks)))

;; Try to prevent TRAMP slowdowns, as per
;; http://emacs.stackexchange.com/a/17579/5391
(setq projectile-mode-line "Projectile")

;; Make sure this is set in the same way as a normal shell (in case Emacs was
;; started as a SystemD unit in a different environment)
(setenv "SSH_AUTH_SOCK" (shell-command-to-string "bash -c 'printf $SSH_AUTH_SOCK'"))

;; Add all the many non-FHS PATH entries we might want
(dolist (user '("chris" "jo" "manjaro" "user" "nixos"))
  (dolist (dir '(".nix-profile/bin" "bin" "System/Programs"))
    (add-to-list 'tramp-remote-path (concat "/home/" user "/" dir))))

(provide 'warbo-tramp)
;;; warbo-tramp.el ends here

;; Don't run Flymake over TRAMP
(if (boundp 'flymake-allowed-file-name-masks)
    (setq flymake-allowed-file-name-masks
          (cons '("^/ssh:" (lambda () nil))
                flymake-allowed-file-name-masks)))

;; Try to prevent TRAMP slowdowns, as per
;; http://emacs.stackexchange.com/a/17579/5391
(setq projectile-mode-line "Projectile")

;; TODO: Take from ~/.bashrc, e.g. by running a bash script and parsing it out
(setenv "SSH_AUTH_SOCK" "/run/user/1000/gcr/ssh")

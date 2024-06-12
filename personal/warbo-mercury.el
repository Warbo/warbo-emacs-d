;;; warbo-mercury --- Support for the Mercury logic programming language
;;; Commentary:
;;; Code:

(defun compile-mercury ()
  (interactive)
  (let* ((file-name  (file-name-nondirectory (buffer-file-name)))
         (is-mercury (equal "m" (file-name-extension file-name)))
         (contracted (file-name-sans-extension file-name)))
    (compile (cond
              ((file-exists-p "default.nix") "nix-build")
              ((file-exists-p "Makefile"     "make -k"  ))
              (is-mercury     (concat "nix-shell -p mercury --run 'mmc --make "
                                      contracted
                                      "'"))
              (t (error "Not a Mercury '.m' file"))))))

(use-package metal-mercury-mode
  :quelpa (metal-mercury-mode :fetcher github
                              :repo    "ahungry/metal-mercury-mode")
  :mode "\\.m\\'"
  :defer t  ;; Don't access `metal-mercury-mode-map' until the mode's loaded

  :config
  (setq metal-mercury-mode-compile-function
        (lambda (module-name)
          (cl-concatenate 'string
                          "nix-shell -p mercury "
                          "--run 'mmc --make " module-name "'")))
  (bind-key "<f9>" 'compile-mercury metal-mercury-mode-map))

(provide 'warbo-mercury)
;;; warbo-mercury.el ends here

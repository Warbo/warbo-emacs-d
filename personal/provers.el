;; Isabelle files
;; (use-package isar-mode
;;   :quelpa (isar-mode :fetcher github :repo "agomezl/simp-isar-mode")
;;   :mode "\\.thy\\'")

;; Proof General
(defun init-pg ()
  "Load ProofGeneral from Nix location."
  (ignore-errors (load-file "~/.nix-profile/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el")))

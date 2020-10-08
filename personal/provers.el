;; Isabelle files
;; (use-package isar-mode
;;   :quelpa (isar-mode :fetcher github :repo "agomezl/simp-isar-mode")
;;   :mode "\\.thy\\'")

;; Proof General
(defun init-pg ()
  "Load ProofGeneral from Nix location."
  (ignore-errors (load-file "~/.nix-profile/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el")))

(use-package proof-general
  :ensure t
  :mode ("\\.v\\'" . coq-mode)
  :custom-face
  ;(proof-locked-face ((t (:background "#add8e6"))))
  )

(use-package company-coq
  :ensure t
  :hook
  (coq-mode . company-coq-mode)
  :init
  ;(setq company-coq-disabled-features '(hello prettify-symbols))
  )

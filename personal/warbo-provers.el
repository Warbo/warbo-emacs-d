;;; warbo-provers --- Helpers for (interactive) theorem provers
;;; Commentary:
;;; Code:

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

(use-package tla-mode
  :ensure t
  :quelpa (tla-mode
           :fetcher github
           :repo "ratish-punnoose/tla-mode"
           :commit "be82e6287f5c219ab7a918813fa273ffa7b220ec"
           :shallow nil)
  :mode "\\.tla\\'"
  :hook (tla-mode . prettify-symbols-mode)
  )

(provide 'warbo-provers)
;;; warbo-provers.el ends here

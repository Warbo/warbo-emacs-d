;;; warbo-provers --- Helpers for (interactive) theorem provers -*- lexical-binding: t; -*-
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
  :mode
  ("\\.v\\'"  . coq-mode)
  ("\\.mv\\'" . coq-mode)
  :custom-face
  ;(proof-locked-face ((t (:background "#add8e6"))))
  )

(use-package tla-mode
  :quelpa (tla-mode
           :fetcher github
           :repo "shonfeder/tla-mode"
           :branch "fix-single-line-comment"
           :commit "be82e6287f5c219ab7a918813fa273ffa7b220ec")
  :mode "\\.tla\\'"
  :hook (tla-mode . prettify-symbols-mode)
  )

(provide 'warbo-provers)
;;; warbo-provers.el ends here

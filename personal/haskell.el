;; Haskell programming
(require 'use-package)

(defun haskell-indent ()
  "Send Haskell buffers through indentation program."
  ;; Check that we've been "manually saved" (not auto-saved) and that we're a
  ;; Haskell file (see https://emacs.stackexchange.com/a/14476/5391 )
  (when (and (memq this-command '(save-buffer save-some-buffers))
             (eq major-mode 'haskell-tng-mode))
    (shell-command-to-string (format "brittany %s" buffer-file-name))
    (revert-buffer nil t t)))

(add-hook 'after-save-hook 'haskell-indent)

(eval-after-load "haskell-cabal"
  '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-tng-compile))

;; haskell-tng is a minimalist alternative to intero, dante, HIE, etc. (since I
;; could never get those to work anyway)
(thinkpad-only
  (use-package popup) ;; dependency of haskell-tng-hsinspect
  )

;; Dependencies
(use-package company   :ensure t)
(use-package yasnippet :ensure t)

(use-package haskell-tng-mode
  :ensure nil
  :load-path "/run/current-system/sw/share/emacs/site-lisp/haskell-tng.el"
  :mode ((rx ".hs" eos) . haskell-tng-mode)

  :config
  (require 'haskell-tng-hsinspect)
  (require 'haskell-tng-extra)
  (require 'haskell-tng-extra-abbrev)
  (require 'haskell-tng-extra-company)
  (require 'haskell-tng-extra-projectile)
  (require 'haskell-tng-extra-smartparens)
  (require 'haskell-tng-extra-yasnippet)

  :bind
  (:map
   haskell-tng-mode-map
   ("RET"     . haskell-tng-newline)
   ("C-c c"   . haskell-tng-compile)
   ("C-c C-c" . haskell-tng-compile)
   ("C-c e"   . next-error)))

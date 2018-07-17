;; Haskell programming
(require 'use-package)
;; (use-package dante
;;   :ensure t
;;   :commands 'dante-mode
;;   :init
;;   (add-hook 'haskell-mode-hook 'dante-mode)
;;   (add-hook 'haskell-mode-hook 'flycheck-mode))

;; (add-hook 'haskell-mode-hook 'dante-mode)
(add-hook 'haskell-mode-hook 'flycheck-mode)
;(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(custom-set-variables '(haskell-stylish-on-save nil))

(defun haskell-indent ()
  "Send Haskell buffers through indentation program."
  ;; Check that we've been "manually saved" (not auto-saved) and that we're a
  ;; Haskell file (see https://emacs.stackexchange.com/a/14476/5391 )
  (when (and (memq this-command '(save-buffer save-some-buffers))
             (eq major-mode 'haskell-mode))
    (shell-command-to-string (format "brittany %s" buffer-file-name))
    (revert-buffer nil t t)))

(add-hook 'after-save-hook 'haskell-indent)

(eval-after-load "haskell-mode"
  '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))

(eval-after-load "haskell-cabal"
  '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))

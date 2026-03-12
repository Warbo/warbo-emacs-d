;;; warbo-spellcheck.el --- Warbo's Emacs config: Spell checking settings -*- lexical-binding: t; -*-
;;; Commentary:
;;; Configuration for Emacs spell checking using Flyspell.
;;; Code:

(use-package flyspell
  :config
  ;; flyspell-mode does spell-checking on the fly as you type
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra")
        ;; Plain word list for ispell-completion-at-point (provided by scowl)
        ispell-alternate-dictionary "/run/current-system/sw/share/dict/wbritish.txt")

  (defun prelude-enable-flyspell ()
    "Enable command `flyspell-mode' if `ispell-program-name' is in PATH."
    (when  (executable-find ispell-program-name)
      (flyspell-mode +1)))

  (add-hook 'text-mode-hook 'prelude-enable-flyspell))

(add-hook 'prog-mode-hook
          (lambda ()
            (when (executable-find ispell-program-name)
              (flyspell-prog-mode))))

(add-hook 'LaTeX-mode-hook 'flyspell-mode)  ;; Relies on aspell

(provide 'warbo-spellcheck)
;;; warbo-spellcheck.el ends here

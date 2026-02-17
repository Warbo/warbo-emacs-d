;;; warbo-rust --- Setup for working with rust projects -*- lexical-binding: t; -*-

;;; Commentary:
;;; Inspired by https://robert.kra.hn/posts/rust-emacs-setup/

;;; Code:
;; TODO: Replace obsolete yas/fallback-behavior with yas-fallback-behavior
;; TODO: Replace obsolete yas/expand with yas-expand
;; TODO: Replace obsolete yas/minor-mode with yas-minor-mode
(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("C-c C-c l" . flycheck-list-errors))
  :defines (rustic-major-mode)
  :config

  (setq rustic-format-on-save t)
  (setq rustic-major-mode 'rust-ts-mode)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  "Save without prompting the user, if buffer is visiting a file.
Ensures \\[rustic-cargo-run] works without having to confirm.  Once
https://github.com/brotzeit/rustic/issues/253 has been resolved this should no
longer be necessary."
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(provide 'warbo-rust)
;;; warbo-rust.el ends here

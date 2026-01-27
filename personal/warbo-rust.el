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
  :config

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  "Save without prompting the user, if buffer is visiting a file.
Ensures \\[rustic-cargo-run] works without having to confirm.  Once
https://github.com/brotzeit/rustic/issues/253 has been resolved this should no
longer be necessary."
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(defun yasnippet-or-completion ()
  "Try to expand a yasnippet snippet, otherwise invoke completion."
  (interactive)
  (or (do-yas-expand)
      (completion-at-point)))

(defun check-expansion ()
  "Return t if point is at a location where completion is likely.
This is the case if point is at the end of a symbol, or after a `.', or
after `::'."
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  "Try to expand a yasnippet snippet, returning nil on failure."
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  "Indent the current line, or complete the current symbol.
If the minibuffer is active, then completion is performed.  Otherwise,
if yasnippet is active and a snippet can be expanded, that is done.
Otherwise, if at a point where completion is likely, completion is
invoked.  Otherwise, the current line is indented."
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (completion-at-point)
          (indent-for-tab-command)))))

(provide 'warbo-rust)
;;; warbo-rust.el ends here

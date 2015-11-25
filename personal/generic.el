;; Set some repositories for the package manager
(setq package-archives '(("gnu"       . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa"     . "http://melpa.milkbox.net/packages/")
                         ))

;; Resize windows with Shift-Control-Arrow-Cursor
(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)

;; When TRAMP connections die, auto-save can hang
(setq auto-save-default t)

;; Turn off Prelude's auto-save-when-switching-buffer
(ad-unadvise 'switch-to-buffer)
(ad-unadvise 'other-window)
(ad-unadvise 'windmove-up)
(ad-unadvise 'windmove-down)
(ad-unadvise 'windmove-left)
(ad-unadvise 'windmove-right)

;; Start emacs server, so emacsclient works
(defer 'server-start)

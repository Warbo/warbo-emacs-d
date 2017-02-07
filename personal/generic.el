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

;; Disable expensive modes when long lines are found
(when (require 'so-long nil :noerror)
  (so-long-enable))

;; Disable smartparens mode, as it's really slow
(with-eval-after-load 'smartparens
  (show-smartparens-global-mode -1))

;; Allow commands to use Nix
(setenv "NIX_REMOTE" "daemon")

;; Set PATH
(setenv "PATH"
        (concat (getenv "PATH")
                ":/run/current-system/sw/bin:/home/chris/.nix-profile/bin"))
(setq exec-path (append exec-path '("/run/current-system/sw/bin"
                                    "/home/chris/.nix-profile/bin")))

;; Set a reasonable value for COLUMNS, e.g. for shell buffers
(setenv "COLUMNS" "80")

;; Enable fill-column-indicator when editing files
(setq-default fill-column 80)

;; Show '...' in place of long Nix hashes
(pretty-sha-path-global-mode)

;; Allow invoked programs to use pulseaudio
(setenv "PULSE_SERVER" "/run/user/1000/pulse/native")

;; Wrap the display of long lines, without altering the text itself
(global-visual-line-mode)

;; Start emacs server, so emacsclient works
(defer 'server-start)

;; Force font. This does nothing in terminal mode, so we poll until there's a
;; graphical display, set the font, then cancel the polling

(defvar desired-font "Liberation Mono" "The font the use in graphical mode")

;; We store the timer
(when (boundp 'force-font-timer)
  (cancel-timer force-font-timer)
  (makunbound 'force-font-timer))

(setq force-font-timer
      (run-with-timer 5 5 (lambda ()
                            (when (display-graphic-p)
                              (if (member desired-font (font-family-list))
                                  (set-face-attribute 'default nil
                                                      :font desired-font)
                                (message "Font %S wasn't found" desired-font))

                              ;; While we're here, enable fci-mode globally too
                              (require 'fill-column-indicator)
                              (define-globalized-minor-mode
                                my-global-fci-mode
                                fci-mode
                                (lambda ()
                                  (when (buffer-file-name)
                                    (turn-on-fci-mode))))
                              (my-global-fci-mode 1)

                              (cancel-timer force-font-timer)))))

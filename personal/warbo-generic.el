;;; warbo-generic --- General Emacs settings, useful in all modes -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs configuration, and generally-useful packages

;;; Code:

;; Resize windows with Shift-Control-Arrow-Cursor
(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)

;; Scrolling one line at a time, without having to move the cursor to top/bottom
(global-unset-key (kbd "M-<up>"))
(global-unset-key (kbd "M-<down>"))
(global-set-key (kbd "M-<up>")   'scroll-up-line)
(global-set-key (kbd "M-<down>") 'scroll-down-line)

;; Easily toggle truncation; helps on narrow phone screens
(global-set-key (kbd "C-c C-t") 'toggle-truncate-lines)

(use-package ace-window
  :ensure t
  :bind (("s-w" . ace-window)))

(use-package anzu
  :ensure t
  :functions (global-anzu-mode)
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package avy
  :ensure t
  :bind (("C-c j" . avy-goto-word-or-subword-1)
         ("s-." . avy-goto-word-or-subword-1))
  :custom
  (avy-background t)
  (avy-style 'at-full))

(use-package beacon
  :ensure t)

(use-package browse-kill-ring
  :ensure t
  :functions (browse-kill-ring-default-keybindings)
  :bind (("s-y" . browse-kill-ring))
  :config
  (browse-kill-ring-default-keybindings))

(use-package crux
  ;; TODO: 2025-08-19 This is our fork which avoids deprecation warnings
  ;; TODO: ace-window.el has Case warnings for 'visible, 'global, 'frame
  ;; TODO: browse-kill-ring.el uses obsolete defadvice (upstream package issue)
  :quelpa (crux :fetcher github
                :repo "Warbo/crux"
                :commit "f21b2974df1218c782dbed321b8cb38e325d1a8f"
                :upgrade t)
  :bind (("C-^" . crux-top-join-line)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ("C-c r" . crux-rename-buffer-and-file))
  :functions (indent-region@with-region-or-buffer
              untabify@with-region-or-buffer
              kill-region@with-region-or-line)
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-line kill-region))

(use-package dired
  :custom
  (dired-dwim-target t))

(use-package diff-hl
  :ensure t
  :functions (global-diff-hl-mode)
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

(use-package discover-my-major
  :ensure t
  :bind (:map help-map ("C-m" . discover-my-major)))

(use-package easy-kill
  :ensure t
  :bind ([remap kill-ring-save] . easy-kill)
        ([remap mark-sexp] . easy-mark))

(use-package epl
  :ensure t)

(use-package ag
  :ensure t)

(use-package deadgrep
  :ensure t
  :functions (deadgrep)
  :bind
  (("<f5>" . #'deadgrep)))

(use-package embark
  :ensure t
  :functions (embark-prefix-help-command)
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)
   )
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package fill-column-indicator
  :ensure t
  :functions (fci-mode turn-on-fci-mode))

(use-package git-timemachine
  :ensure t)

(use-package god-mode
  :ensure t)

(use-package grizzl
  :ensure t)

(use-package guru-mode
  :ensure t)

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)))

(use-package imenu-anywhere
  :ensure t)

(use-package marginalia
  :ensure t
  :functions (marginalia-mode)
  :init
  (marginalia-mode))

(use-package move-text
  :ensure t)

(use-package operate-on-number
  :ensure t)

(use-package ov
  :ensure t
  :functions (ov-set ov-reset)
  :config
  (defun prelude-todo-ov-evaporate (ov after _beg _end &optional _length)
    "Helper for `prelude-annotate-todo'.
OV is the overlay, AFTER indicates post-change.  _BEG, _END, _LENGTH ignored."
    (let ((inhibit-modification-hooks t))
      (if after (ov-reset ov))))

  (defun prelude-annotate-todo ()
    "Put fringe marker on TODO: lines in the curent buffer."
    (interactive)
    (ov-set (format "[[:space:]]*%s+[[:space:]]*TODO:" comment-start)
            'before-string
            (propertize (format "A")
                        'display '(left-fringe right-triangle))
            'modification-hooks '(prelude-todo-ov-evaporate))))

(use-package popup
  :ensure t)

(use-package pretty-sha-path
  :ensure t
  :functions (pretty-sha-path-global-mode)
  :config (pretty-sha-path-global-mode))

(use-package projectile
  :ensure t
  :custom
  ;; Try to prevent TRAMP slowdowns, as per
  ;; http://emacs.stackexchange.com/a/17579/5391
  (projectile-mode-line "Projectile")

  :defines (prelude-savefile-dir)
  :functions (projectile-mode)
  :config
  ;; projectile is a project management mode
  (unless noninteractive
    (setq projectile-cache-file (expand-file-name  "projectile.cache" prelude-savefile-dir))
    (projectile-mode t)))

(use-package smartrep
  ;; TODO: smartrep.el uses obsolete destructuring-bind and loop
  ;; TODO: ag.el should use -zip-pair instead of -zip
  :ensure t
  :functions (smartrep-define-key)
  :config
  (smartrep-define-key global-map "C-c ."
    '(("+" . apply-operation-to-number-at-point)
      ("-" . apply-operation-to-number-at-point)
      ("*" . apply-operation-to-number-at-point)
      ("/" . apply-operation-to-number-at-point)
      ("\\" . apply-operation-to-number-at-point)
      ("^" . apply-operation-to-number-at-point)
      ("<" . apply-operation-to-number-at-point)
      (">" . apply-operation-to-number-at-point)
      ("#" . apply-operation-to-number-at-point)
      ("%" . apply-operation-to-number-at-point)
      ("'" . operate-on-number-at-point))))

(use-package undo-tree
  :ensure t
  :functions (global-undo-tree-mode)
  :config
  (progn
    (setq undo-tree-visualizer-timestamps t
          undo-tree-history-directory-alist (quote (("" . "~/.emacs.d/.appdata/.undo-tree-history")))
          undo-tree-auto-save-history nil  ;; Freezes Emacs on big XML files
          undo-tree-visualizer-lazy-drawing 1000)
    (global-undo-tree-mode)
    ))

(use-package volatile-highlights
  :ensure t
  :functions (volatile-highlights-mode)
  :config
  (volatile-highlights-mode t))

(use-package which-key
  :ensure t)

(use-package zenburn-theme
  :ensure t
  :config
  (setq zenburn-override-colors-alist
        '(("zenburn-bg-2"  . "#000000")
          ("zenburn-bg-1"  . "#080808")
          ("zenburn-bg-05" . "#0D0D0D")
          ("zenburn-bg"    . "#101010")
          ("zenburn-bg+05" . "#181818")
          ("zenburn-bg+1"  . "#212121")
          ("zenburn-bg+2"  . "#313131")
          ("zenburn-bg+3"  . "#414141")))
  (load-theme 'zenburn t))

(use-package zop-to-char
  :ensure t
  :bind (("M-z" . zop-up-to-char)
         ("M-Z" . zop-to-char)))

(use-package zygospore
  :ensure t
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)))

;; When TRAMP connections die, auto-save can hang
(setq auto-save-default t)

;; Disable expensive modes when long lines are found
(use-package so-long
  :ensure t
  :config
  (global-so-long-mode)

  ;; Bi-directional text can slow down Emacs's processing
  (setq-default bidi-display-reordering nil)

  ;; Disable which-key, as it's slow and swallows keypresses
  (which-key-mode -1))

(use-package nhexl-mode
  :ensure t)

(use-package unicode-fonts
  :ensure t
  :functions (unicode-fonts-setup)
  :config
  (unicode-fonts-setup))

(use-package smartparens
  :ensure t
  :hook ((prog-mode . smartparens-mode)
         (smartparens-mode . warbo-smartparens-scroll-keys))
  :functions (smartparens-global-mode
              crux-smart-open-line-above
              prelude-wrap-with
              sp-wrap-with-pair
              sp-pair
              sp-use-paredit-bindings)
  :custom
  (sp-base-key-bindings 'paredit)
  (sp-autoskip-closing-pair 'always)
  (sp-hybrid-kill-entire-symbol nil)
  :config
  (defun prelude-wrap-with (s)
    "Create a wrapper function for smartparens using S."
    (lambda (&optional _arg)
       (interactive "P")
       (sp-wrap-with-pair s)))

  (defun warbo-smartparens-scroll-keys ()
    "Ensure M-<up>/M-<down> scroll in smartparens buffers."
    (local-set-key (kbd "M-<up>")   #'scroll-up-line)
    (local-set-key (kbd "M-<down>") #'scroll-down-line))

  (smartparens-global-mode 1)
  (sp-use-paredit-bindings)
  ;; sp-use-paredit-bindings binds M-? to sp-convolute-sexp, which conflicts
  ;; with xref-find-references (the standard M-? binding).  Unbind it.
  (define-key smartparens-mode-map (kbd "M-?") nil)

  (define-key prog-mode-map (kbd "M-(") (prelude-wrap-with "("))
  ;; FIXME: pick terminal friendly binding
  ;; (define-key prog-mode-map (kbd "M-[") (prelude-wrap-with "["))
  (define-key prog-mode-map (kbd "M-\"") (prelude-wrap-with "\""))

  ;; smart curly braces
  (with-eval-after-load 'crux
    (sp-pair "{" nil :post-handlers
             `((,(lambda (&rest _ignored) (crux-smart-open-line-above))
                "RET")))))

;; Hovering tooltips are annoying
;(setq tooltip-use-echo-area t)
;(tooltip-mode nil)

;; Honour .editorconfig file settings
(use-package editorconfig
  :ensure t
  :custom
  (editorconfig-exclude-regexps '(".*/recentf$"
                                  ".*\\.zip$"))
  :config
  (add-hook 'prog-mode-hook 'editorconfig-mode))

(use-package consult
  :ensure t
  :bind* (("C-c r"     . consult-recent-file))
  :bind (("C-c i"     . consult-imenu)
         ("C-c f"     . consult-fd)
         ("C-c b"     . consult-project-buffer)
         ("C-x b"     . consult-buffer)
         ("C-c B"     . consult-bookmark)
         ;("C-c h"     . consult-ripgrep)
         ("C-c y"     . consult-yank-pop)
         ("C-c C-h a" . describe-symbol)
         )
  :custom
  (consult-narrow-key (kbd ";"))
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-project-root-function #'deadgrep--project-root) ;; ensure ripgrep works
  (consult-preview-key '(:debounce 0.25 any))
  )

(use-package consult-eglot
  :disabled
  :ensure t
  :config
  (defun pt/consult-eglot ()
    (interactive)
    (let ((completion-styles '(emacs22)))
      (call-interactively #'consult-eglot-symbols)))
  :bind (:map eglot-mode-map ("s-t" . #'pt/consult-eglot)))

(use-package embark-consult
  :ensure t
  :after (embark consult))

(use-package embark-vc
  :ensure t
  :after embark)

;; See https://www.masteringemacs.org/article/whats-new-in-emacs-28-1
(setq completions-detailed t)

;; Highlight dodgy whitespace (tabs, trailing, otherwise-empty lines, etc.) in
;; programs, config files, etc.
(use-package whitespace
  :ensure t
  :demand t
  :diminish whitespace-mode
  :config
  (progn
    (setq whitespace-style '(face
                             tabs
                             empty
                             trailing
                             lines-tail
                             space-before-tab
                             space-after-tab)
          whitespace-line-column 80)

    (add-hook 'conf-mode-hook 'whitespace-mode)
    (add-hook 'prog-mode-hook 'whitespace-mode)
    (add-hook 'text-mode-hook 'whitespace-mode)

    (whitespace-mode +1)))

;; Strip trailing whitespace, etc. when saving files, but only if the file was
;; "clean" when it was opened. This way, editing files which already contain
;; dodgy whitespace won't cause all of that to be stripped (which would pollute
;; diffs and git commits, for example)
(use-package whitespace-cleanup-mode
  :ensure t
  :functions (global-whitespace-cleanup-mode)
  :config
  ;; Avoid cleanup in vue-mode, since it can reindent everything weirdly. This
  ;; is probably due to it using mmm-mode to handle mixtures of HTML, JS, etc.
  ;(cl-pushnew 'vue-mode whitespace-cleanup-mode-ignore-modes)
  ;(cl-pushnew 'vue-html-mode whitespace-cleanup-mode-ignore-modes)
  ;; Otherwise, enable everywhere else
  (global-whitespace-cleanup-mode))

;; Used by pi-coding-agent
(use-package phscroll
  :quelpa (phscroll :fetcher github
                    :repo "misohena/phscroll"))

(require 'cl-lib)
(cl-macrolet
    ((use-package-here (name &rest args)
       `(progn ;;(message "load-file-name: %S" load-file-name)
               ;;(message "fnd: %S" (file-name-directory load-file-name))
               (use-package ,name
                 :load-path ,(file-name-directory (macroexp-file-name))
                 ,@args))))
  (use-package-here ffap-goto-line
    :functions (ffap-goto-line-mode)
    :bind ("C-x C-f" . find-file-at-point)
    :config
    (ffap-goto-line-mode 1)))

;; We don't want C-a to go all of the way back; drop us on the actual code
;; (Taken from https://stackoverflow.com/a/7250027/884682 )
(defun smart-line-beginning ()
  "Move point to the beginning of text on the current line.
If point is already at the beginning of text, move it to the beginning of line."
  (interactive)
  (let ((pt (point)))
    (back-to-indentation)
    (when (eq pt (point))
      (beginning-of-line))))
(global-set-key (kbd "C-a") 'smart-line-beginning)

;; Home and End should stick to the current line
(global-set-key (kbd "<home>") 'smart-line-beginning)
(global-set-key (kbd "<end>" ) 'end-of-line)

(global-set-key (kbd "C-x t TAB") 'tab-switcher)

;; Turn URLs into buttons
(global-goto-address-mode 1)

;; https://www.blogbyben.com/2022/05/gotcha-emacs-on-mac-os-too-many-files.html
(defun file-notify-rm-all-watches ()
  "Remove all existing file notification watches from Emacs."
  (interactive)
  (maphash
   (lambda (key _value)
     (file-notify-rm-watch key))
   file-notify-descriptors))

;; Allow commands to use Nix
(when (file-exists-p "/nix/var/nix/daemon-socket/socket")
 (setenv "NIX_REMOTE" "daemon"))

;; Enable fill-column-indicator when editing files
(setq-default fill-column 80)

;; Wrap the display of long lines, without altering the text itself
(global-visual-line-mode)

;; Highlight matching parentheses globally
(show-paren-mode 1)

;; Enable fill-column-indicator globally when the buffer is visiting a file
(define-globalized-minor-mode
  my-global-fci-mode
  fci-mode
  (lambda ()
    (when (and (buffer-file-name)
               ;; fci can have problems when Emacs daemon has GUI and CLI
               ;; clients, so stick to only showing it in GUIs for now
               (display-graphic-p))
      (turn-on-fci-mode)))
  :group 'fill-column-indicator)
(my-global-fci-mode 1)

(require 'dash)
(defun font-strings-match (x y)
  "Check if the strings X and Y (in XLFD format) match, allowing wildcards."
  (-all? 'identity
         (cl-mapcar (lambda (part-x part-y)
                      (or (member "*" (list part-x part-y))
                          (equal part-x part-y)))
                    (split-string x "-")
                    (split-string y "-"))))

(defun set-desired-font ()
  "Choose and set a font, depending on which machine we're on."
  (let ((f (cond
            ((equal machine-id 'wsl) "fixed")

            ;; wls-ubuntu is awkward, since the WSL X server can crash and lose
            ;; access to bitmap fonts other than 'fixed'...
            ((equal machine-id 'wsl-ubuntu)
             (let* ((cmd (executable-find "font-exists"))
                    (want "-jmk-neep-medium-r-semicondensed--11-*-*-*-*-*-*-*"))
               (if (and (boundp 'desired-font)
                        (equal want desired-font)
                        ;; See if there's an existing frame whose font matches
                        ;; `want' (taking wildcards into account). If so, there
                        ;; is no need to choose a font, we can just use `want'.
                        ;; This is needed when `emacsclient' gets invoked from
                        ;; within Emacs (e.g. when writing a commit message in
                        ;; Magit): we don't want an xfontsel window to pop up
                        ;; and take focus away from Emacs!
                        (-any?
                         ;; Does any existing frame have a font matching `want'?
                         (lambda (frame)
                           (let ((frame-font (frame-parameter frame 'font)))
                             ;; Do any matches for `want' also match frame-font?
                             (-any? (lambda (font)
                                      (font-strings-match frame-font font))
                                    (x-list-fonts want nil frame))))
                         (frame-list)))
                   ;; We can use `want', no need to run any commands
                   want
                 ;; We need to pick a font. We'd like `want', but it may not be
                 ;; available (e.g. if the WSL X server crashed), so run the
                 ;; font-exists cmd to check. Note that pops up an xfontsel
                 ;; window momentarily, which may change the focused window!
                 (with-temp-buffer
                   (if (and cmd (equal 0 (call-process cmd nil t nil want)))
                       want
                     (if cmd
                         (message "%s" (buffer-string))
                       (message "'font-exists' command not in PATH"))
                     (message "Font '%s' not found (%s), falling back to %s"
                              want
                              "see *Messages* for details"
                              "fixed")
                     "fixed")))))

            ;; This seems to depend on whether our monitor is connected...
            ((equal machine-id 'manjaro)
             "EnvyCodeR Nerd Font Mono-11")

            ((equal machine-id 'framework)
             "DroidSansM Nerd Font Mono-11")

            ((font-utils-exists-p "Droid Sans Mono-8")
             "Droid Sans Mono-8")

            ((font-utils-exists-p "DroidSansM Nerd Font Mono-8")
             "DroidSansM Nerd Font Mono-8")

            ((font-utils-exists-p "EnvyCodeR Nerd Font Mono-8")
             "EnvyCodeR Nerd Font Mono-8"))))

    ;; Declare desired-font. Will not replace a value that's already defined.
    (defvar desired-font
      f
      "The font to use in graphical mode.")
    (cond
     ;; Return early if desired-font matches f, or is otherwise non-nil
     (desired-font nil)
     ((equal desired-font f) nil)
     ;; Otherwise set the value as necessary
     (t (setq desired-font f)))
    ;; Apply now, if possible
    (when desired-font
      (add-to-list 'default-frame-alist `(font . ,desired-font))
      (set-frame-font desired-font))))

;; Set hook, so it will run when emacsclients open new frames
(add-hook 'server-after-make-frame-hook 'set-desired-font)

;; Run now if we're already in a graphical frame (e.g. not started as a daemon)
(when (display-graphic-p)
  (set-desired-font))

(provide 'warbo-generic)
;;; warbo-generic.el ends here

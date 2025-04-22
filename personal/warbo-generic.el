;;; warbo-generic --- General Emacs settings, useful in all modes

;;; Commentary:

;; Emacs configuration, and generally-useful packages

;;; Code:

;; Resize windows with Shift-Control-Arrow-Cursor
(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)

;; Easily toggle truncation; helps on narrow phone screens
(global-set-key (kbd "C-c C-t") 'toggle-truncate-lines)

(use-package ag
  :ensure t)

(use-package fill-column-indicator
  :ensure t)

(use-package flycheck
  :ensure t)

(use-package git-timemachine
  :ensure t)

(use-package popup
  :ensure t)

(use-package pretty-sha-path
  :ensure t
  :config (pretty-sha-path-global-mode))

(use-package smart-mode-line
  :disabled
  :ensure t
  :init
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme nil)
  (add-hook 'after-init-hook #'sml/setup))

(use-package undo-tree
  :ensure t
  :config
  (progn
    (setq undo-tree-visualizer-timestamps t
          undo-tree-history-directory-alist (quote (("" . "~/.emacs.d/.appdata/.undo-tree-history")))
          undo-tree-auto-save-history nil  ;; Freezes Emacs on big XML files
          undo-tree-visualizer-lazy-drawing 1000)
    (global-undo-tree-mode)))

(use-package zenburn-theme
  :ensure t)

(use-package zygospore
  :ensure t
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)))

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
  :config
  (unicode-fonts-setup))

(use-package smartparens
  :ensure t
  :config
  ;; Disable smartparens mode, as it's really slow
  (show-smartparens-global-mode -1))

;; Hovering tooltips are annoying
;(setq tooltip-use-echo-area t)
;(tooltip-mode nil)

;; Honour .editorconfig file settings
(use-package editorconfig
  :ensure t
  :config
  (setq editorconfig-exclude-regexps
        '(".*/recentf$"
          ".*\.zip$"))
  (add-hook 'prog-mode-hook 'editorconfig-mode)
  )

(use-package company
  :disabled
  :ensure t
  :config
  (setq company-idle-delay              nil
        company-tooltip-flip-when-above t
        company-minimum-prefix-length   1
        company-show-quick-access       t
        company-tooltip-limit           20
        company-dabrev-downcase         nil)
  :bind  ("TAB" . 'company-indent-or-complete-common))

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
          whitespace-indentation 'whitespace-trailing
          whitespace-line-column 80)

    (add-hook 'conf-mode-hook 'whitespace-mode)
    (add-hook 'prog-mode-hook 'whitespace-mode)
    (add-hook 'text-mode-hook 'whitespace-mode)

    (whitespace-mode +1)))

;; Strip trailing whitespace, etc. when saving files, but only if the file was
;; "clean" when it was opened. This way, editing files which already contain
;; dodgy whitespace won't cause all of that to be stripped (which would pollute
;; diffs and git commits, for example)
(require 'cl-lib)
(use-package whitespace-cleanup-mode
  :ensure t
  :config
  ;; Avoid cleanup in vue-mode, since it can reindent everything weirdly. This
  ;; is probably due to it using mmm-mode to handle mixtures of HTML, JS, etc.
  (cl-pushnew 'vue-mode whitespace-cleanup-mode-ignore-modes)
  (cl-pushnew 'vue-html-mode whitespace-cleanup-mode-ignore-modes)
  ;; Otherwise, enable everywhere else
  (global-whitespace-cleanup-mode))

;; Flycheck all the things
(use-package flycheck
  :ensure t
  :hook ((after-init . global-flycheck-mode))
  :config
  (add-to-list 'flycheck-checkers 'nix)

  ;; Flycheck builds a command to run, e.g. ("myCompiler" "check" "MyFile.ext"),
  ;; but that assumes the executable (e.g. "myCompiler") is in PATH. We'd rather
  ;; encapsulate such things as dependencies of a project's Nix derivation,
  ;; which are then available if we run "nix-shell".

  ;; Running nix-shell over and over can be slow, so we use functions from
  ;; nix-sandbox which cache the environment (e.g. the PATH), which makes
  ;; looking up the relevant executable much faster.

  ;; The default behaviour of nix-sandbox isn't ideal, for two reasons: firstly
  ;; we can spend a while waiting for a nix-shell to get built synchronously,
  ;; just for some throwaway command like a syntax checker. Secondly, if we
  ;; cancel the synchronous call (e.g. with C-g) then the sandbox will probably
  ;; need to be rebuilt from scratch next time.

  ;; To avoid this we augment nix-sandbox in the following way:
  ;;  - Run nix-shell processes asynchronously, to avoid having to wait.
  ;;  - Error-out immediately if the sandbox is still being built.

  ;; This way, we can carry on working (without checkers) while builds are going
  ;; on in the background, and the checkers will start working once the builds
  ;; finish.

  ;; (defvar nix-sandbox-builders-map
  ;;   (make-hash-table :test 'equal :size 100)
  ;;   "Stores the builder processes")

  ;; (defun nix-sandbox-build-asynchronously (sandbox)
  ;;   "Launch a process to build a nix-shell in SANDBOX.
  ;;    The build command is copypasta from nix-create-sandbox-rc."
  ;;   (or (gethash sandbox nix-sandbox-builders-map)
  ;;       (let* ((name (concat "nix-sandbox-builder-" sandbox))
  ;;              (proc (puthash
  ;;                     sandbox
  ;;                     (start-process
  ;;                      name name
  ;;                      "nostderr" "nix-shell" "--run"
  ;;                      "declare +x shellHook; declare -x; declare -xf")
  ;;                     nix-sandbox-builders-map)))
  ;;         (message "Starting nix-shell builder for dir %s\n" sandbox)

  ;;         ;; Prevents 'Process foo finished' messages polluting output buffer
  ;;         (set-process-sentinel proc #'ignore)
  ;;         proc)))

  ;; (defun nix-create-sandbox-rc-async (sandbox)
  ;;   "Replacement for nix-create-sandbox-rc.
  ;;    Looks up or creates a nix-shell process in the SANDBOX dir. If the process
  ;;    is running (e.g. if it's newly created, or takes a while) then an interrupt
  ;;    is triggered, as if the user cancelled this operation. If it's no longer
  ;;    running, we dump its output to a temp file and return it."
  ;;   (let ((proc (nix-sandbox-build-asynchronously sandbox)))
  ;;     ;; When we're not finished yet, pretend we cancelled with C-g
  ;;     (when (process-live-p proc)
  ;;       (keyboard-quit))

  ;;     ;; Process is finished, remove it from cache.
  ;;     (message "Found finished builder for nix-shell dir %s\n" sandbox)
  ;;     (remhash sandbox nix-sandbox-builders-map)

  ;;     ;; Write output to a file
  ;;     (let ((filename (make-temp-file "nix-sandbox-rc-")))
  ;;       (with-current-buffer (process-buffer proc)
  ;;         (write-file filename)
  ;;         (kill-buffer))

  ;;       ;; Return filename, to use as our 'rc' file
  ;;       filename)))

  ;;         ;; Override nix-create-sandbox-rc. Using advice is inefficient, but
  ;;         ;; will work even if nix-sandbox hasn't been loaded yet.

  ;; (advice-add 'nix-create-sandbox-rc
  ;;             :override #'nix-create-sandbox-rc-async)

  ;; ;; Tell flycheck to look up commands in Nix sandboxes, if we're in one
  ;; (setq flycheck-command-wrapper-function
  ;;       (lambda (command)
  ;;         (let ((sandbox (thinkpad-only (nix-current-sandbox))))
  ;;           (if sandbox
  ;;               (apply 'nix-shell-command sandbox command)
  ;;             command)))

        ;; flycheck-executable-find
        ;; (lambda (command)
        ;;   (let ((sandbox (thinkpad-only (nix-current-sandbox))))
        ;;     (if sandbox
        ;;         (nix-executable-find sandbox command)
        ;;       (executable-find command))))
        )

;; Look for line and column numbers when using find-file-at-point

(define-minor-mode ffap-goto-line-mode
  "Tells `find-file-at-point' to look for line and column numbers.

  Toggling this minor mode lets us toggle the behaviour, without having to faff
  around with things like 'unadvising'."
  :global t :init-value t)

(defun ffap-goto-line-advice (orig &rest args)
  "Advice for `find-file-at-point' (it's bound to ORIG, with args in ARGS).

   If `ffap-goto-line-mode' is non-nil, we'll scan ahead in the buffer to see if
   there's a line number and column, separated by colons, e.g. if we've called
   `ffap' when the point is on `/home/foo/bar.txt:12:34':

    - We search forward and find the `:12:34'
    - We parse this to get line 12 and column 34
    - We call the original `ffap', which guesses filename `/home/foo/bar.txt'
    - Once opened, we go to line 12 column 34

   If only one colon-separated number is found, we assume it's the line number."
  (let* ((have-col (and ffap-goto-line-mode
                        (looking-at ".*:[0-9]+:[0-9]+")))
         (col      (and ffap-goto-line-mode
                        (looking-at ".*:[0-9]+:\\([0-9]+\\)")
                        (string-to-number (match-string 1))))
         (line     (if have-col
                       (and (looking-at ".*:\\([0-9]+\\):[0-9]+")
                            (string-to-number (match-string 1)))
                     (and ffap-goto-line-mode
                          (looking-at ".*:\\([0-9]+\\)")
                          (string-to-number (match-string 1))))))
    (apply orig args)
    (and line (goto-line line))
    (and col  (move-to-column col))))

(advice-add 'find-file-at-point :around #'ffap-goto-line-advice)

;; Also use find-file-at-point for C-x C-f instead of find-file (ffap will
;; emulate find-file if it can't guess a filename)
(require 'bind-key)
(bind-key* "C-x C-f" 'find-file-at-point)

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
(bind-key* "C-a" 'smart-line-beginning)

;; Home and End should stick to the current line
(global-set-key (kbd "<home>") 'smart-line-beginning)
(global-set-key (kbd "<end>" ) 'end-of-line)

(global-set-key (kbd "C-x t TAB") 'tab-switcher)

;; Turn URLs into buttons
(global-goto-address-mode 1)

(defun xah-copy-file-path (&optional @dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path.
If `universal-argument' is called first, copy only the dir path.

If in dired, copy the file/dir cursor is on, or marked files.

If a buffer is not file and not dired, copy value of `default-directory'
 (which is usually the “current” dir when that buffer was created)

URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
Version 2017-09-01"
  (interactive "P")
  (let (($fpath
         (if (string-equal major-mode 'dired-mode)
             (progn
               (let (($result (mapconcat 'identity
                                         (dired-get-marked-files) "\n")))
                 (if (equal (length $result) 0)
                     (progn default-directory )
                   (progn $result))))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if @dir-path-only-p
         (progn
           (message "Directory path copied: 「%s」" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "File path copied: 「%s」" $fpath)
         $fpath )))))
(global-set-key (kbd "C-M-w" ) 'xah-copy-file-path)

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
(thinkpad-only
 (setenv "NIX_PATH"
         (replace-regexp-in-string
          (rx (* (any " \t\n")) eos)
          ""
          (shell-command-to-string
           "/run/current-system/sw/bin/bash -l -c 'echo \"$NIX_PATH\"'"))))

;; Enable fill-column-indicator when editing files
(setq-default fill-column 80)

;; Allow invoked programs to use pulseaudio
(thinkpad-only
  (setenv "PULSE_SERVER" "/var/run/pulse/native"))

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
      (turn-on-fci-mode))))
(my-global-fci-mode 1)

(require 'dash)
(require 'cl-lib)
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
             (let* ((font-exists-env "FONT_EXISTS_CMD")
                    (cmd (getenv font-exists-env))
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
                 ;; FONT_EXISTS_CMD to check. Note that pops up an xfontsel
                 ;; window momentarily, which may change the focused window!
                 (with-temp-buffer
                   (if (and cmd (equal 0 (call-process cmd nil t nil want)))
                       want
                     (if cmd
                         (message "%s" (buffer-string))
                       (message "Env var %s not set by Home Manager"
                                font-exists-env))
                     (message "Font '%s' not found (%s), falling back to %s"
                              want
                              "see *Messages* for details"
                              "fixed")
                     "fixed")))))

            ;; This seems to depend on whether our monitor is connected...
            ((equal machine-id 'manjaro)
             "EnvyCodeR Nerd Font Mono-11")

            ((font-utils-exists-p "EnvyCodeR Nerd Font Mono-8")
             "EnvyCodeR Nerd Font Mono-8")

            ((font-utils-exists-p "Droid Sans Mono-9")
             "Droid Sans Mono-9"))))

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

(wsl-ubuntu-only
 (run-at-time
  5
  nil
  (lambda ()
    (message "Auto-spawning emacsclient -c")
    (start-process
     "auto-emacsclient"
     nil
     ;; Run a graphical terminal so emacsclient knows it's to run graphically
     "urxvt"
     ;; Execute emacsclient via nohup, so it will detach from the terminal. This
     ;; (a) allows the terminal window to close immediately, and (b) prevents
     ;; the emacsclient process getting killed when the terminal closes. We make
     ;; the window "undecorated", to avoid the titlebar clutter; this prevents
     ;; us dragging the window around, but we can get it in position using some
     ;; Super+arrow-key combos. Undecorated is preferable to fullscreen, since
     ;; it doesn't steal focus from programs running on another monitor.
     "-e" "nohup" "emacsclient" "-c" "-F" "((undecorated . t))"))))

(provide 'warbo-generic)
;;; warbo-generic.el ends here

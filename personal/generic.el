;; Resize windows with Shift-Control-Arrow-Cursor
(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)

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
  (which-key-mode -1)
  )

(use-package smartparens
  :ensure t
  :config
  ;; Disable smartparens mode, as it's really slow
  (show-smartparens-global-mode -1))

;; Hovering tooltips are annoying
;(setq tooltip-use-echo-area t)
;(tooltip-mode nil)

(use-package company
  :ensure t
  :config
  (setq company-idle-delay              nil
        company-tooltip-flip-when-above t
        company-minimum-prefix-length   1
        company-show-numbers            t
        company-tooltip-limit           20
        company-dabrev-downcase         nil))

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
(use-package whitespace-cleanup-mode
  :ensure t
  :config
  (progn
    (global-whitespace-cleanup-mode)))

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

  (defvar nix-sandbox-builders-map
    (make-hash-table :test 'equal :size 100)
    "Stores the builder processes")

  (defun nix-sandbox-build-asynchronously (sandbox)
    "Launch a process to build a nix-shell in SANDBOX.
     The build command is copypasta from nix-create-sandbox-rc."
    (or (gethash sandbox nix-sandbox-builders-map)
        (let* ((name (concat "nix-sandbox-builder-" sandbox))
               (proc (puthash
                      sandbox
                      (start-process
                       name name
                       "nostderr" "nix-shell" "--run"
                       "declare +x shellHook; declare -x; declare -xf")
                      nix-sandbox-builders-map)))
          (message "Starting nix-shell builder for dir %s\n" sandbox)

          ;; Prevents 'Process foo finished' messages polluting output buffer
          (set-process-sentinel proc #'ignore)
          proc)))

  (defun nix-create-sandbox-rc-async (sandbox)
    "Replacement for nix-create-sandbox-rc.
     Looks up or creates a nix-shell process in the SANDBOX dir. If the process
     is running (e.g. if it's newly created, or takes a while) then an interrupt
     is triggered, as if the user cancelled this operation. If it's no longer
     running, we dump its output to a temp file and return it."
    (let ((proc (nix-sandbox-build-asynchronously sandbox)))
      ;; When we're not finished yet, pretend we cancelled with C-g
      (when (process-live-p proc)
        (keyboard-quit))

      ;; Process is finished, remove it from cache.
      (message "Found finished builder for nix-shell dir %s\n" sandbox)
      (remhash sandbox nix-sandbox-builders-map)

      ;; Write output to a file
      (let ((filename (make-temp-file "nix-sandbox-rc-")))
        (with-current-buffer (process-buffer proc)
          (write-file filename)
          (kill-buffer))

        ;; Return filename, to use as our 'rc' file
        filename)))

          ;; Override nix-create-sandbox-rc. Using advice is inefficient, but
          ;; will work even if nix-sandbox hasn't been loaded yet.

  (advice-add 'nix-create-sandbox-rc
              :override #'nix-create-sandbox-rc-async)

  ;; Tell flycheck to look up commands in Nix sandboxes, if we're in one
  (setq flycheck-command-wrapper-function
        (lambda (command)
          (let ((sandbox (thinkpad-only (nix-current-sandbox))))
            (if sandbox
                (apply 'nix-shell-command sandbox command)
              command)))

        flycheck-executable-find
        (lambda (command)
          (let ((sandbox (thinkpad-only (nix-current-sandbox))))
            (if sandbox
                (nix-executable-find sandbox command)
              (executable-find command))))))

(use-package dwim-compile
  :ensure t
  :config
  ;; Compile using nix-build if there's a default.nix file
  (add-to-list 'dwim-c/build-tool-alist
               '(nix "\\`default\\.nix\\'" "nix-build")))

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
(bind-key* "C-a"     'smart-line-beginning)

;; Home and End should stick to the current line
(global-set-key (kbd "<home>") 'smart-line-beginning)
(global-set-key (kbd "<end>" ) 'end-of-line)

;; Allow commands to use Nix
(setenv "NIX_REMOTE" "daemon")
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

;; Start emacs server, so emacsclient works. We can only run one emacs server at
;; a time, so skip this if this emacs instance is just for running tests.
(unless (or (getenv "EMACS_UNDER_TEST")
            ;; Also skip if we're already running a server (e.g. if we're
            ;; reloading our config, and don't want to close existing frames)
            (and (boundp 'server-clients) server-clients))
  (defer 'server-start))

(thinkpad-only
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
                                                       :font desired-font
                                                       :height 80)
                                 (message "Font %S wasn't found" desired-font))

                               (cancel-timer force-font-timer))))))

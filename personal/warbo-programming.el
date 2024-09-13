;;; warbo-programming --- Generic programming-related stuff
;;; Commentary:
;;; Code:

;; Define some reformatters, used by various modes below

(use-package reformatter
  :ensure t
  :config
  (reformatter-define cue-format
    :program "cue"
    :args '("fmt" "-"))

  (reformatter-define nix-format
    :program "nixfmt"
    :args '("-w" "80"))

  (reformatter-define scala-format
    :program "scalafmt"
    :args '("--config-str" "version = \"3.4.3\", runner.dialect = \"scala212\""
            "--stdin"
            "--stdout"))

  (reformatter-define sh-format
    :program "shfmt")

  (reformatter-define xmllint-format
    :program "xmllint"
    :args '("--format" "-"))

  (reformatter-define yamlfix-format
    :program "yamlfix"
    :args '("-")))

;; These modes are built-in, so we don't need use-package to run add-hook
(add-hook 'sh-mode-hook 'sh-format-on-save-mode)

;; Put as much as possible in use-package expressions; roughly alphabetically

(use-package cue-mode
  :ensure t
  :after reformatter
  :quelpa (cue-mode :fetcher github
                    :repo    "russell/cue-mode")
  :mode (("\\.cue\\'"  . cue-mode))
  :config
  (add-hook 'cue-mode-hook 'cue-format-on-save-mode))

;(use-package direnv
;  :ensure t
;  :init
;  (add-hook 'prog-mode-hook #'direnv-update-environment)
;  :config
;  (add-to-list 'direnv-non-file-modes 'shell-mode)
;  (direnv-mode))

(use-package haskell-mode
  :ensure t)

(defvar ghcid-height 65536 "How many lines to truncate a ghcid buffer to.")
(defun ghcid ()
  "Run ghcid in a shell-mode buffer"
  (interactive)
  (when (get-buffer "ghcid")
    (let ((kill-buffer-query-functions nil)) ;; Don't ask
      (kill-buffer "ghcid")))
  (let ((shell-mode-hook nil))  ;; Avoid warbo-shell-hook's colour mangling
    (let ((buf (command-in-buffer (list "ghcid" (vc-root-dir) ". GHCID"))))
      (with-current-buffer buf
        (compilation-minor-mode 1) ;; Nice warning/error highlighting
        (setq-local comint-buffer-maximum-size ghcid-height)
        (add-hook 'comint-output-filter-functions
                  'comint-truncate-buffer
                  nil
                  :local)))))

;; SEE https://github.com/ndmitchell/ghcid/blob/master/plugins/emacs/ghcid.el
;; Compilation mode does some caching for markers in files, but it gets confused
;; because ghcid reloads the files in the same process. Here we parse the
;; 'Reloading...' message from ghcid and flush the cache for the mentioned
;; files. This approach is very similar to the 'omake' hacks included in
;; compilation mode.
(add-to-list
 'compilation-error-regexp-alist-alist
 '(ghcid-reloading
   "Reloading\\.\\.\\.\\(\\(\n  .+\\)*\\)" 1 nil nil nil nil
   (0 (progn (compilation-forget-errors) nil))
   ))
(add-to-list 'compilation-error-regexp-alist 'ghcid-reloading)


(use-package js2-mode
  :ensure t)

;; (use-package js2-refactor
;;   :ensure t)

(use-package xref-js2
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package jq-mode
  :ensure t
  :mode ("\\.jq" . jq-mode))

;; Unset some conflicting keybindings before binding them to magit
(global-unset-key (kbd "s-m"))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("s-m m" . magit-status)
         ("s-m l" . magit-log)
         ("s-m f" . magit-log-buffer-file)
         ("s-m b" . magit-blame))
  :init
  (setq magit-diff-paint-whitespace t)
  (setq magit-diff-highlight-trailing t)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package magit-popup
  :ensure t)

(use-package make-mode
  ;; makefile-mode is built-in, so doesn't need downloading
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\Makefile\\'" . makefile-mode))
    (add-to-list 'auto-mode-alist '("\\.mk\\'"      . makefile-mode))

    (font-lock-add-keywords
     'makefile-mode
     '(("define" . font-lock-keyword-face)
       ("endef"  . font-lock-keyword-face)
       ("ifeq"   . font-lock-keyword-face)
       ("ifneq"  . font-lock-keyword-face)
       ("ifdef"  . font-lock-keyword-face)
       ("ifndef" . font-lock-keyword-face)
       ("else"   . font-lock-keyword-face)
       ("endif"  . font-lock-keyword-face)))

    (add-hook 'makefile-mode-hook 'linum-mode)
    (add-hook 'makefile-mode-hook
              (lambda ()
                (setq whitespace-style '(face tab-mark trailing))
                      indent-tabs-mode t))))

(use-package nix-mode
  :ensure t
  :after reformatter
  :config
  (add-hook 'nix-mode-hook 'nix-format-on-save-mode))

(use-package scala-mode
  :ensure t
  :config
  (add-hook 'scala-mode-hook 'scala-format-on-save-mode))

(use-package smartparens
  :ensure t)

(use-package yaml-mode
  :ensure t
  :config
  (add-hook 'yaml-mode-hook 'yamlfix-format-on-save-mode))

;; Use the modeline to show the definition of the function at point
(use-package which-func
  :config
  (progn
    ;; TODO: Would we rather append major modes to this list as and when they
    ;; benefit from which-func; rather than having it always enabled?
    ;; TODO: Check elisp, haskell, scala, JS, python, etc.
    ;(setq which-func-modes nil)
    (which-function-mode 1)))

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package dash
  :defer t)

(use-package f
  :commands f-join  ;; add autoload for f-join
  :defer t)

(use-package s
  :defer t)

(use-package diminish
  :config
  (mapcar 'diminish '(editorconfig-mode
                      whitespace-cleanup-mode
                      flyspell-mode
                      company-mode
                      projectile-mode
                      eldoc-mode
                      visual-line-mode
                      smartparens-mode
                      prelude-mode
                      flycheck-mode
                      lsp-lens-mode
                      beacon-mode
                      pretty-sha-path-mode)))

(use-package eglot
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'eglot-ensure)
  (setq eglot-connect-timeout 300)  ;; Big projects might take a while!
  :custom
  (eglot-autoshutdown t)  ;; shutdown language server after closing last file
  (eglot-confirm-server-initiated-edits nil)  ;; allow edits without confirmation
  )

(use-package company
  :ensure t
  :hook (prog-mode . company-mode))

(use-package yasnippet
  :ensure t)

;; Posframe is a pop-up tool that must be manually installed for dap-mode
(use-package posframe
  :ensure t)

;; Use the Debug Adapter Protocol for running tests and debugging
;; (use-package dap-mode
;;   ;; Includes dap-python
;;   :disabled
;;   :ensure t
;;   :defer  t
;;   :hook
;;   (lsp-mode . dap-mode)
;;   (lsp-mode . dap-ui-mode)
;;   :config
;;   (require 'dap-ui))

(use-package typescript-mode
  :ensure t
  :mode (("\\.ts\\'"  . typescript-mode)
         ("\\.tsx\\'" . typescript-mode)))

(use-package nxml-mode
  :no-require t
  :mode
  ("\\.xml$" . nxml-mode)
  ("\\.pom$" . nxml-mode)
  :commands (pretty-print-xml-region)
  :init
  ;; Mapping xml to nxml
  (fset 'xml-mode 'nxml-mode)

  :config
  (setq nxml-child-indent 2
        ;;nxml-auto-insert-xml-declaration-flag t
        nxml-slash-auto-complete-flag t
        nxml-bind-meta-tab-to-complete-flag t))

(use-package vue-mode
  :ensure t
  :mode (("\\.vue$" . vue-mode)))

(define-derived-mode nix-derivation-mode prog-mode "nix-derivation-mode"
  "Custom major mode, which runs Nix .drv files through 'nix show-derivation'.
   The result is JSON, so we derive from json-mode."
  (setq major-mode 'nix-derivation-mode)
  (setq mode-name "DRV")

  ;; Hide all of the Nix store hashes
  (pretty-sha-path-mode 1)

  ;; /nix/store files are read-only, so Emacs makes their buffers read-only too
  (read-only-mode -1)

  ;; Clear the buffer and replace with 'nix show-derivation' run on the file
  (erase-buffer)
  (shell-command
   (concat "nix derivation show "
           "--extra-experimental-features nix-command "
           (buffer-file-name)
           "^*")
   (buffer-name))

  ;; Don't prompt to save our changed content (we can't, since it's read-only!)
  (set-buffer-modified-p nil)

  ;; Avoid any further modifications (since they can't be saved)
  (read-only-mode 1)

  (run-hooks 'nix-derivation-mode-hook))
(add-to-list 'auto-mode-alist '("/nix/store/.*\\.drv" . nix-derivation-mode))

;; We can hook into prog-mode to affect any programming-related buffer
(add-hook 'prog-mode-hook
          (lambda ()
            (when (executable-find ispell-program-name)
              (flyspell-prog-mode))
            (smartparens-mode +1)
            (set (make-local-variable 'comment-auto-fill-only-comments) t)

            ;; Highlight a bunch of well known comment annotations.
            (font-lock-add-keywords
             nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
                    1 font-lock-warning-face t)))))

;; smart curly braces
(sp-pair "{" nil :post-handlers
         '(((lambda (&rest _ignored)
              (crux-smart-open-line-above)) "RET")))

;; Enable on-the-fly syntax checking
(if (fboundp 'global-flycheck-mode)
    (global-flycheck-mode +1)
  (add-hook 'prog-mode-hook 'flycheck-mode))

(mac-only
 (setq flycheck-global-modes '(not c-mode)))

;; Bind a key to look for 'test.sh' and run it
(defun warbo-find-and-run-tests-sentinel (process signal)
  "A process sentinel suitable for 'set-process-sentinel'.
The returned sentinel will send a notification when the attached (asynchronous)
PROCESS gets an exit SIGNAL.
Inspired by https://emacs.stackexchange.com/a/42174/5391"
  (when (memq (process-status process) '(exit signal))
    (let* ((buf    (process-buffer process))
           (dir    (with-current-buffer buf default-directory))
           (prefix (car (last (s-split "/" dir t))))
           (msg    (concat "'Tests complete for " prefix "'")))
      (shell-command
       (concat "(command -v notify-send && notify-send " msg ") || echo " msg)))
    (shell-command-sentinel process signal)))

(defun warbo-find-and-run-tests ()
  "Look for a test runner in the current dir (or parents) and run it."
  (interactive)
  (let ((dir (s-chomp (shell-command-to-string
                       "git rev-parse --show-toplevel"))))
    (when (and (s-starts-with-p "/" dir)
               (not (s-starts-with-p "fatal:" dir)))
      (let ((cmd (cond
                  ;; TODO: Check for more things here, e.g. the existence of a
                  ;; .cabal file containing a test suite, the existence of a
                  ;; Python project with tests, etc.
                  ((file-exists-p (concat dir "/test.sh")) "./test.sh")
                  ((file-exists-p (concat dir "/tests.sh")) "./tests.sh")
                  (t nil))))
        (when cmd
          (let* ((prefix (car (last (s-split "/" dir t))))
                 (output-buffer (generate-new-buffer
                                 (concat "*" prefix " test*")))
                 (proc (with-current-buffer output-buffer
                         (cd dir)
                         (async-shell-command cmd output-buffer)
                         (get-buffer-process output-buffer))))
            (if (process-live-p proc)
                (set-process-sentinel  proc warbo-find-and-run-tests-sentinel)
              (message "Tests finished immediately"))))))))
(keymap-global-set "<f5>" 'warbo-find-and-run-tests)

(provide 'warbo-programming)
;;; warbo-programming.el ends here

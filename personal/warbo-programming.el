;;; package --- summary

;;; Commentary:

;;; Code:

;; Define some reformatters, used by various modes below

(use-package reformatter
  :ensure t)

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
  :args '("-"))

;; These modes are built-in, so we don't need use-package to run add-hook
(add-hook 'sh-mode-hook 'sh-format-on-save-mode)

;; Put as much as possible in use-package expressions; roughly alphabetically

(use-package cue-mode
  :ensure t  :after reformatter
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

;(use-package lsp-mode
;  :quelpa (lsp-mode :fetcher github
;                    :repo    "emacs-lsp/lsp-mode")
;  :after (dash dap-mode direnv)  ; lsp-mode uses functions defined by dash
;  ;:disabled
;  :ensure t
;  ;:defer  t
;  ;; Optional - enable lsp-mode automatically in scala files
;  :hook  ((scala-mode       ; metals?
;           python-mode      ; spyder IDE python-lsp-server?
;           haskell-mode     ; haskell-language-server
;           js-mode          ; ts-ls (tsserver wrapper)
;           typescript-mode  ; ts-ls (tsserver wrapper)
;           java-mode        ; eclipse-jdtls
;           web-mode         ; ts-ls/HTML/CSS
;           ) . lsp-deferred)
;  (lsp-mode . lsp-lens-mode)
;  :commands lsp
;  :config
;  (advice-add 'lsp :before #'direnv-update-environment)
;  (setq lsp-auto-guess-root t)
;  (setq lsp-enable-symbol-highlighting nil)
;  (setq lsp-enable-on-type-formatting nil)
;  (setq lsp-signature-auto-activate t)
;  (setq lsp-signature-render-documentation t)
;  (setq lsp-eldoc-hook nil)
;  (setq lsp-eldoc-enable-hover t)
;  (setq lsp-modeline-code-actions-enable nil)
;  (setq lsp-modeline-diagnostics-enable nil)
;  (setq lsp-headerline-breadcrumb-enable nil)
;  (setq lsp-semantic-tokens-enable nil)
;  ;(setq lsp-enable-folding nil)
;  (setq lsp-enable-imenu nil)
;  (setq lsp-enable-snippet nil)
;  (setq lsp-idle-delay 0.5)
;  (setq lsp-prefer-flymake nil)
;  (setq lsp-restart 'auto-restart)
;
;  ;; Avoids the following error:
;  ;; Error running timer ‘lsp--on-idle’:
;  ;; (error "The connected server(s) does not support method
;  ;; textDocument/documentHighlight.
;  (setq lsp-enable-links nil)
;
;  ;; Better performance than 4k default
;  (setq read-process-output-max (* 1024 1024))
;
;  ;; Useful for debugging, but very slow otherwise
;  (setq lsp-log-io nil))

;; Enable nice rendering of documentation on hover
;(use-package lsp-ui
;  :ensure t
;  :defer  t
;  :commands lsp-ui-mode
;  :config
;  (setq lsp-ui-doc-enable t)
;  (setq lsp-ui-doc-show-with-cursor nil)
;  (setq lsp-ui-doc-header t)
;  (setq lsp-ui-doc-include-signature t)
;  (setq lsp-ui-doc-border (face-foreground 'default))
;  (setq lsp-ui-sideline-show-diagnostics nil)
;  (setq lsp-ui-sideline-show-code-actions nil)
;  (setq lsp-ui-sideline-delay 0.05)
;  (setq lsp-ui-flycheck t))

;; lsp-mode supports snippets, but in order for them to work you need to use
;; yasnippet. If you don't want to use snippets set lsp-enable-snippet to nil in
;; your lsp-mode settings to avoid odd behavior with snippets and indentation
(use-package yasnippet
  :ensure t)

;; Add company-lsp backend for metals
;(use-package company-lsp
;  :ensure t
;  :defer  t)

;; Posframe is a pop-up tool that must be manually installed for dap-mode
(use-package posframe
  :ensure t)

;; Use the Debug Adapter Protocol for running tests and debugging
;(use-package lsp-java
;  ;; Includes dap-java
;  :ensure t
;  :defer  t)

;(use-package dap-mode
;  ;; Includes dap-python
;  :disabled
;  :ensure t
;  :defer  t
;  :hook
;  (lsp-mode . dap-mode)
;  (lsp-mode . dap-ui-mode)
;  :config
;  (require 'dap-ui))

;; Use the Tree View Protocol for viewing the project structure and triggering
;; compilation
;; (use-package lsp-treemacs
;;   :disabled
;;   :ensure t
;;   :defer  t
;;   :config
;;   (setq lsp-metals-treeview-show-when-views-received t))

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
        ;nxml-auto-insert-xml-declaration-flag t
        nxml-slash-auto-complete-flag t
        nxml-bind-meta-tab-to-complete-flag t))

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

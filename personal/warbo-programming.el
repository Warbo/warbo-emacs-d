;;; warbo-programming --- Generic programming-related stuff -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Declare functions defined in other files
(declare-function command-in-rolling-buffer "warbo-rolling-shell")

;; Define some reformatters, used by various modes below

(use-package reformatter
  :ensure t
  :config
  (reformatter-define cue-format
    :program "cue"
    :args '("fmt" "-")
    :group 'reformatter)

  (reformatter-define nix-format
    :program "nixfmt"
    :args '("-w" "80")
    :group 'reformatter)

  (reformatter-define scala-format
    :program "scalafmt"
    :args '("--config-str" "version = \"3.4.3\", runner.dialect = \"scala212\""
            "--stdin"
            "--stdout")
    :group 'reformatter)

  (reformatter-define sh-format
    :program "shfmt"
    :group 'reformatter)

  (reformatter-define xmllint-format
    :program "xmllint"
    :args '("--format" "-")
    :group 'reformatter)

  (reformatter-define yamlfix-format
    :program "yamlfix"
    :args '("-")
    :group 'reformatter))

;; These modes are built-in, so we don't need use-package to run add-hook
(add-hook 'sh-mode-hook 'sh-format-on-save-mode)

;; Put as much as possible in use-package expressions; roughly alphabetically

(use-package rainbow-identifiers
  :ensure t
  :hook (prog-mode . rainbow-identifiers-mode))

(use-package auctex
  :ensure t
  :mode "\\.latex\\'")

(use-package cask-mode
  :ensure t
  :mode "Cask")

(use-package clojure-mode
  :ensure t
  :mode "\\.clj\\'")

(use-package cmake-mode
  :ensure t
  :mode ("\\.cmake\\'" "CMakeLists\\.txt\\'"))

(use-package coffee-mode
  :ensure t
  :mode "\\.coffee\\'")

(use-package css-mode
  :ensure t
  :mode "\\.css\\'")

(use-package csv-mode
  :ensure t
  :mode "\\.csv\\'")

(use-package cue-mode
  :ensure t
  :after reformatter
  :quelpa (cue-mode :fetcher github
                    :repo    "russell/cue-mode")
  :mode (("\\.cue\\'"  . cue-mode))
  :config
  (add-hook 'cue-mode-hook 'cue-format-on-save-mode))

(use-package cython-mode
  :ensure t
  :mode ("\\.pyd\\'" "\\.pyi\\'" "\\.pyx\\'"))

(use-package d-mode
  :ensure t
  :mode "\\.d\\'")

(use-package dart-mode
  :ensure t
  :mode "\\.dart\\'")

(use-package direnv
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'direnv-update-environment)
  :config
  (add-to-list 'direnv-non-file-modes 'shell-mode)
  (direnv-mode))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

(use-package elixir-mode
  :ensure t
  :mode ("\\.ex\\'" "\\.exs\\'" "\\.elixir\\'"))

(use-package elm-mode
  :ensure t
  :mode "\\.elm\\'")

(use-package erlang
  :ensure t
  :mode "\\.erl\\'")

(use-package feature-mode
  :ensure t
  :mode "\\.feature\\'")

(use-package go-mode
  :ensure t
  :mode "\\.go\\'")

(use-package groovy-mode
  :ensure t
  :mode "\\.groovy\\'")

(use-package haml-mode
  :ensure t
  :mode "\\.haml\\'")

(use-package xref-union
  :ensure t
  :hook (haskell-mode . xref-union-mode)
  :config
  (setq tags-revert-without-query 1)

  (defun case-sensitive-xref-find-definitions-advice (orig-fun &rest args)
    "Sets `tags-case-fold-search` to `t' for name at point, but not prompts."
    (if current-prefix-arg
        (apply orig-fun args)
      (let ((tags-case-fold-search t))
        (apply orig-fun args))))

  ;; Advise both the normal and “other-window” variants if you like:
  (advice-add 'xref-find-definitions
              :around #'case-sensitive-xref-find-definitions-advice)
  (advice-add 'xref-find-definitions-other-window
              :around #'case-sensitive-xref-find-definitions-advice))

(defun warbo-haskell-tags ()
  "Run command to generate TAGS file in root directory of current repo."
  (let ((default-directory (vc-root-dir)))
    (when default-directory
      (start-process "hasktags" nil "hasktags" "--etags" "."))))

(defun warbo-haskell-setup ()
  "Custom hook to setup `haskell-mode'."
  (add-hook 'after-save-hook 'warbo-haskell-tags nil t))

(use-package haskell-mode
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'warbo-haskell-setup))

(defun replace-region-string (s &optional beg end)
  "Replace the region between BEG and END (default: region, or point-min/max)
with the string S. Unlike `replace-region-contents' this maintains text
 properties like colouring."
  (let ((beg2 (or beg (and (use-region-p) (region-beginning)) (point-min)))
        (end2 (or end (and (use-region-p) (region-end))       (point-max))))
    (delete-region beg2 end2)
    (goto-char beg2)
    (insert s)))

(defun pretty-simple-region (start end)
  "Send region (between START and END) through `pretty-simple' command."
  (interactive "r")
  (if (use-region-p)
      (let* ((old (buffer-substring-no-properties start end))
             (new (with-temp-buffer
                    (insert old)
                    (when (zerop (call-process-region (point-min)
                                                      (point-max)
                                                      "pretty-simple"
                                                      t
                                                      t
                                                      nil))
                      ;; pretty-simple gives us ANSI colour codes, which
                      ;; are especially useful for punctuation. We could
                      ;; apply those using ansi-color-apply, but I prefer
                      ;; the look of xterm-color-filter.
                      (replace-region-string
                       (xterm-color-filter (buffer-string))
                       (point-min)
                       (point-max))

                      ;; Enable a simple prog-mode like json-mode, which will
                      ;; give us rainbow-delimiters and rainbow-identifiers
                      (json-mode)
                      (font-lock-ensure)

                      (buffer-string)))))
        (when new
          (replace-region-string new start end)))
    (message "No region selected")))

(use-package warbo-rolling-shell
  :config
  (defvar ghcid-height 65536 "How many lines to truncate a ghcid buffer to.")
  (defun ghcid ()
    "Run ghcid in a shell-mode buffer"
    (interactive)
    (when (get-buffer "ghcid")
      (let ((kill-buffer-query-functions nil)) ;; Don't ask
        (kill-buffer "ghcid")))
    (let ((shell-mode-hook nil))  ;; Avoid warbo-shell-hook's colour mangling
      (let ((buf (command-in-rolling-buffer
                  (list "ghcid" default-directory ". ~/GHCID")
                  ghcid-height)))
        (with-current-buffer buf
          ;; Nice warning/error highlighting
          (compilation-minor-mode 1))))))

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
  :ensure t
  :mode (("\\.js" . js2-mode)
         ("\\.julius" . web-mode)))

;; (use-package js2-refactor
;;   :ensure t)

(use-package xref-js2
  :ensure t)

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(use-package jq-mode
  :ensure t
  :mode ("\\.jq" . jq-mode))

(use-package kotlin-mode
  :ensure t
  :mode "\\.kt\\'")

(use-package kivy-mode
  :ensure t
  :mode "\\.kv\\'")

(use-package less-css-mode
  :ensure t
  :mode "\\.less\\'")

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'")

;; Unset some conflicting keybindings before binding them to magit
(global-unset-key (kbd "s-m"))

;; From https://github.com/magit/magit/discussions/4748#discussioncomment-3589929
(defun my/magit-log-reflog (&optional args files)
  "Show log for objects mentioned in reflog, passing along ARGS and FILES."
  (interactive (magit-log-arguments))
  (magit-log-setup-buffer (list "--reflog") args files))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("s-m m" . magit-status)
         ("s-m l" . magit-log)
         ("s-m f" . magit-log-buffer-file)
         ("s-m b" . magit-blame)
         ("C-x M-g" . magit-dispatch-popup) ;; Moved from prelude-global-keybindings.el
         )
  :init
  (setq magit-diff-paint-whitespace t)
  (setq magit-diff-highlight-trailing t)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

  ;; From https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
  (setq magit-tramp-pipe-stty-settings 'pty)

  (transient-append-suffix 'magit-log "o"
    '("R" "reflog objects" my/magit-log-reflog)))

(use-package magit-tbdiff
  :ensure t)

(use-package magit-delta
  :disabled
  :ensure t
  :hook (magit-mode . magit-delta-mode))

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

(use-package markdown-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)))

(defun warbo-remove-bel (text)
  "Remove BEL characters (^G) from TEXT."
  (replace-regexp-in-string "\C-g" "" text))

(use-package nix-mode
  :ensure t
  :after reformatter
  :config
  (add-hook 'nix-mode-hook 'nix-format-on-save-mode)
  ;; nix-mode can run nixfmt, but it can't specify commandline args like "-w 80"
  ;; so we prefer reformatter.
  (setq nix-nixfmt-bin nil)

  (define-advice nix-repl-completion-at-point (:after () warbo-filter-bel)
    "`nix-repl-completion-at-point' can leave ^G (BEL chars) in the buffer."
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "C-g" nil t)
        (replace-match "")))))

(use-package php-mode
  :ensure t
  :mode "\\.php\\'")

(use-package pkgbuild-mode
  :ensure t
  :mode "PKGBUILD\\'")

(use-package protobuf-mode
  :ensure t
  :mode "\\.proto\\'")

(use-package puppet-mode
  :ensure t
  :mode "\\.pp\\'")

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")

(use-package scala-mode
  :ensure t
  :mode "\\.scala\\'"
  :config
  (add-hook 'scala-mode-hook 'scala-format-on-save-mode))

(use-package sass-mode
  :ensure t
  :mode "\\.sass\\'")

(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'")

(use-package slim-mode
  :ensure t
  :mode "\\.slim\\'")

(use-package sws-mode
  :ensure t)

(use-package stylus-mode
  :ensure t
  :after sws-mode
  :mode "\\.styl\\'")

(use-package swift-mode
  :ensure t
  :mode "\\.swift\\'")

(use-package textile-mode
  :ensure t
  :mode "\\.textile\\'")

(use-package tuareg
  :ensure t
  :mode "\\.ml\\'")

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" "\\.yaml\\'")
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

(use-package flyover
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flyover-mode)
  (setq flyover-levels '(error warning))
  (setq flyover-use-theme-colors t)
  (setq flyover-show-at-eol nil)
  (setq flyover-virtual-line-type 'curved-arrow))

(use-package dash
  :ensure t)

(use-package f
  :ensure t)

(use-package s
  :ensure t)

(use-package eglot
  :ensure t
  :commands eglot-ensure eglot
  :hook ((vue-mode . eglot-ensure)
         (c-mode-common . eglot-ensure)
         (c-ts-base-mode . eglot-ensure)
         (haskell-mode . eglot-ensure)
         (js-base-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (typescript-ts-base-mode . eglot-ensure))
  :config
  (setq eglot-connect-timeout 300)  ;; Big projects might take a while!
  ;; From https://gluer.org/blog/improving-eglot-performance/
  (define-advice jsonrpc--log-event (:override (&rest _))
    "Silence jsonrpc logging to improve performance.")
  ;; From https://www.reddit.com/r/emacs/comments/1b25904/is_there_anything_i_can_do_to_make_eglots/
  ;(setf (plist-get eglot-events-buffer-config :size) 0)
  ;(eldoc-echo-area-use-multiline-p nil)
  (setf (alist-get 'typescript-mode eglot-server-programs)
        '("typescript-language-server" "--stdio"))
  (let ((projects (expand-file-name "~/src")))
    (when (file-directory-p projects)
      (eval-when-compile
        (require 'dash)
        (require 's))
      (require 'dash)
      (require 's)
      ;; TODO: Use `eglot-workspace-configuration` instead, so we can have per-project settings
      (setf (alist-get 'haskell-mode eglot-server-programs)
            `("haskell-language-server-9.12.2" "lsp"
              :initializationOptions
              (:haskell ( :formattingProvider "fourmolu"
                          :checkProject nil
                          :sessionLoading "multipleComponents"))))
      (let* ((root (car (-filter (lambda (entry)
                                   (and (not (s-starts-with? "yesod" entry))
                                        (not (s-starts-with? "." entry))))
                                 (directory-files "~/src"))))
             (tsdk (file-name-concat
                    projects root "webpack" "node_modules" "typescript" "lib")))
        (setf (alist-get 'vue-mode eglot-server-programs)
              `("vue-language-server" "--stdio"
                :initializationOptions
                (:typescript (:tsdk ,tsdk)
                             :vue (:hybridMode :json-false)
                             :languageFeatures (:completion
                                                (:defaultTagNameCase "both"
                                                                     :defaultAttrNameCase "kebabCase"
                                                                     :getDocumentNameCasesRequest nil
                                                                     :getDocumentSelectionRequest nil)
                                                :diagnostics
                                                (:getDocumentVersionRequest nil))
                             :documentFeatures (:documentFormatting
                                                (:defaultPrintWidth 100
                                                                    :getDocumentPrintWidthRequest nil)
                                                :documentSymbol t
                                                :documentColor t)))))))
  :custom
  (eglot-autoshutdown t)  ;; shutdown language server after closing last file
  (eglot-confirm-server-initiated-edits nil)  ;; allow edits without confirmation
  )

;; (use-package eglot-booster
;;   :ensure t
;;   :after eglot
;;   :config
;;   (eglot-booster-mode))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (keymap-set vertico-map "TAB" #'minibuffer-complete)
  :config
  ;; When typing an exact buffer name, it should be the default selection.
  ;; Consult adds U+200000 as invisible suffix for multi-category support.
  ;; Buffer completion sets display-sort-function to `identity`, so we must
  ;; use vertico-sort-override-function to take precedence.
  (defun warbo-vertico-sort-prefer-exact (candidates)
    "Sort CANDIDATES with exact match first, then by history/length/alpha."
    (let* ((sorted (vertico-sort-history-length-alpha candidates))
           (input (minibuffer-contents-no-properties)))
      (if (and input (not (string-empty-p input)))
          (let ((exact (seq-find (lambda (c)
                                   ;; Consult adds U+200000 as invisible suffix
                                   ;; for multi-category support. Strip it.
                                   (let ((visible (string-trim-right
                                                   (substring-no-properties c)
                                                   (string ?\x200000))))
                                     (string= input visible)))
                                 sorted)))
            (if exact
                (cons exact (delete exact sorted))
              sorted))
        sorted)))
  (setq vertico-sort-override-function #'warbo-vertico-sort-prefer-exact))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(basic partial-completion emacs22))
  (completion-category-defaults nil)
  ;; Prefer exact matches for buffer names, then fall back to orderless
  (completion-category-overrides '((buffer (styles basic orderless)))))

(use-package corfu
  :ensure t

  :custom
  ;; Pop up immediately
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0)

  (corfu-quit-no-match 'separator)

  ;; Select the prompt by default. That way, TAB will expand an unambiguous
  ;; prefix (the default is 'valid, which can auto-select the first suggestion,
  ;; in which case TAB will insert that, rather than expanding)
  (corfu-preselect 'prompt)

  :config
  ;; "Cycling" causes TAB to go through the list of suggestions; but I prefer it
  ;; to expand an unambiguous prefix.
  (setq completion-cycle-threshold nil)
  (setq tab-always-indent 'complete)
  (setq tab-first-completion 'word)
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package kind-icon
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode)

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
    (let ((yas-fallback-behavior 'return-nil))
      (yas-expand)))

  (defun tab-indent-or-complete ()
    "Indent the current line, or complete the current symbol.
If the minibuffer is active, then completion is performed.  Otherwise,
if yasnippet is active and a snippet can be expanded, that is done.
Otherwise, if at a point where completion is likely, completion is
invoked.  Otherwise, the current line is indented."
    (interactive)
    (if (minibufferp)
        (minibuffer-complete)
      (if (or (not yas-minor-mode)
              (null (do-yas-expand)))
          (if (check-expansion)
              (completion-at-point)
            (indent-for-tab-command))))))

;; Posframe is a pop-up tool that must be manually installed for dap-mode
(use-package posframe
  :ensure t)

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
  :custom
  (nxml-child-indent 2)
  (nxml-slash-auto-complete-flag t)
  (nxml-bind-meta-tab-to-complete-flag t))

(define-derived-mode nix-derivation-mode prog-mode "nix-derivation-mode"
  "Custom major mode, which runs Nix .drv files through `nix show-derivation'.
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
   (concat "nix derivation show --pretty "
           "--extra-experimental-features nix-command "
           (buffer-file-name)
           "^*")
   (buffer-name))

  ;; Don't prompt to save our changed content (we can't, since it's read-only!)
  (set-buffer-modified-p nil)

  ;; Avoid any further modifications (since they can't be saved)
  (read-only-mode 1)

  (run-hooks 'nix-derivation-mode-hook))
;; TODO: Why isn't nix-derivation-mode starting automatically?
;; TEST IT!
(add-to-list 'auto-mode-alist '("/nix/store/.*\\.drv" . nix-derivation-mode))

;; We can hook into prog-mode to affect any programming-related buffer
(add-hook 'prog-mode-hook
          (lambda ()
            (set (make-local-variable 'comment-auto-fill-only-comments) t)

            ;; Highlight a bunch of well known comment annotations.
            (font-lock-add-keywords
             nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
                    1 font-lock-warning-face t)))))

;; Enable on-the-fly syntax checking
(if (fboundp 'global-flycheck-mode)
    (global-flycheck-mode +1)
  (add-hook 'prog-mode-hook 'flycheck-mode))

(defun warbo-find-and-run-tests-sentinel (process signal)
  "A process sentinel suitable for `set-process-sentinel'.
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

(defvar warbo-run-buffer-tests-function nil
  "A function to be run by `warbo-find-and-run-tests'.
If non-nil, this function will be called with no arguments to run
tests for the current buffer. It is intended to be set via
.dir-locals.el or similar.")
(make-variable-buffer-local 'warbo-run-buffer-tests-function)
(put 'warbo-run-buffer-tests-function
     'safe-local-variable
     (lambda (v) (member v '(warbo-run-tests))))

(defun warbo-find-and-run-tests (arg)
  "Run `warbo-run-buffer-tests-function' if it's non-nil. Otherwise, look for a
test runner in the current git repo and run it if found. Allows prefix arg to be
passed along."
  (interactive "P")
  (if warbo-run-buffer-tests-function
      (funcall warbo-run-buffer-tests-function arg)
    (let ((dir (magit-toplevel)))
      (when dir
        (let ((cmd (cond
                    ((file-exists-p (concat dir "/test.sh"))
                     "./test.sh")
                    ((file-exists-p (concat dir "/tests.sh"))
                     "./tests.sh")
                    ((file-exists-p (concat dir "/test-runner.sh"))
                     "./test-runner.sh")
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
                  (set-process-sentinel proc #'warbo-find-and-run-tests-sentinel)
                (message "Tests finished immediately")))))))))
(keymap-global-set "<f6>" 'warbo-find-and-run-tests)

(provide 'warbo-programming)
;;; warbo-programming.el ends here

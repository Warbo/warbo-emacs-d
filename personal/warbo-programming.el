;;; warbo-programming --- Generic programming-related stuff -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Declare functions defined in other files
(declare-function command-in-rolling-buffer "warbo-rolling-shell")
(declare-function pretty-sha-path-mode "pretty-sha-path")
(declare-function warbo-vertico-sort-prefer-exact "warbo-programming")
(declare-function eglot-ensure@check-binary-exists "warbo-programming" t t)
(declare-function nix-repl-completion-at-point@warbo-filter-bel
                  "warbo-programming" t t)
(declare-function xterm-color-filter "xterm-color")
(declare-function case-sensitive-xref-find-definitions-advice
                  "warbo-programming")
(declare-function corfu-popupinfo-mode "corfu-popupinfo")
(declare-function check-expansion "warbo-programming")
(declare-function reformatter--do-region "reformatter")
(declare-function reformatter--make-temp-file "reformatter")
(declare-function magit-toplevel "magit-git")

;; Define some reformatters, used by various modes below. Annoyingly, the
;; reformatter-define macro creates minor modes, which declare buffer-local
;; variables; and that causes a warning if they're not at the top-level!

(use-package reformatter
  :ensure t)

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
  :group 'reformatter)

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
  :functions (warbo-direnv-update-environment)
  :init
  (defun warbo-direnv-update-environment ()
    "Update direnv environment, but only for local files.
Avoids errors when visiting remote files via TRAMP."
    (unless (file-remote-p default-directory)
      (direnv-update-environment)))
  (add-hook 'prog-mode-hook #'warbo-direnv-update-environment)
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

(defvar warbo-vue-eglot-args
  '("vue-language-server" "--stdio"
    :initializationOptions
    (:typescript (:tsdk "node_modules/typescript/lib")
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
                        :documentColor t)))
  "Eglot server program entry for vue-mode.
Use in .dir-locals.el like:
  (eglot-server-programs . ((vue-mode . ,warbo-vue-eglot-args)))")

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
  :defines (whitespace-style)
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

(use-package flymake
  ;; Flymake itself is less powerful than flycheck but eglot can feed it via LSP
  :hook ((eglot-managed-mode . flymake-mode))
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics))
  :config
  (setq flymake-no-changes-timeout 0.5)
  ;; Don't run Flymake over TRAMP
  ;; TODO: Is there a nicer way to upsert this value?
  (if (boundp 'flymake-allowed-file-name-masks)
      (setq flymake-allowed-file-name-masks
            (cons '("^/ssh:" (lambda () nil))
                  flymake-allowed-file-name-masks))))

(use-package flycheck
  ;; Non-eglot/LSP modes should use flycheck as it's more powerful than flymake.
  ;; For eglot-managed buffers, flycheck gets disabled in favour of flymake (see
  ;; warbo-flycheck-disable-for-eglot on eglot-managed-mode-hook below).
  :ensure t
  :init
  (defun warbo-maybe-enable-flycheck ()
    "Enable flycheck only if eglot is not managing this buffer."
    (unless (bound-and-true-p eglot--managed-mode)
      (flycheck-mode 1)))

  (defun warbo-flycheck-disable-for-eglot ()
    "Disable flycheck when eglot starts managing a buffer.
Eglot feeds diagnostics to flymake, so flycheck would be redundant.
If eglot disconnects, flycheck gets re-enabled via `prog-mode-hook'
next time the buffer's mode is set."
    (when (bound-and-true-p flycheck-mode)
      (flycheck-mode -1)))
  :hook ((prog-mode . warbo-maybe-enable-flycheck)
         (eglot-managed-mode . warbo-flycheck-disable-for-eglot))
  :config
  (add-to-list 'flycheck-checkers 'nix))

(use-package flyover
  :ensure t
  :custom
  (flyover-levels '(error warning))
  (flyover-checkers '(flycheck flymake))
  (flyover-hide-during-completion nil)
  (flyover-use-theme-colors t)
  (flyover-show-at-eol t)
  (flyover-virtual-line-type nil)
  (flyover-background-lightness 45)
  :hook ((flycheck-mode . flyover-mode)
         (flymake-mode . flyover-mode)))

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
         (js-base-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (typescript-ts-base-mode . eglot-ensure)
         (haskell-mode . eglot-ensure))
  :custom
  (eglot-connect-timeout 300)  ;; Big projects might take a while!
  :functions (warbo-eglot-check-binary-exists)
  :config
  (defun warbo-eglot-check-binary-exists ()
    "Check if the LSP binary for the current mode exists in PATH.
Returns the binary name if found, nil otherwise."
    (let* ((mode-server (or
                         ;; Check for exact match first
                         (assoc major-mode eglot-server-programs)
                         ;; Then check if any entry matches (for derived modes)
                         (cl-find-if (lambda (entry)
                                       (let ((modes (car entry)))
                                         (or (eq modes major-mode)
                                             (and (listp modes)
                                                  (memq major-mode modes)))))
                                     eglot-server-programs)))
           (contact (cdr mode-server)))
      (when contact
        (let* ((program (cond
                         ((stringp contact) contact)
                         ((and (consp contact) (stringp (car contact)))
                          (car contact))
                         (t nil))))
          (and program (executable-find program) program)))))

  ;; Add advice to prevent eglot from starting when binary doesn't exist
  ;; This catches calls from .dir-locals.el and other sources
  (define-advice eglot-ensure (:around (orig-fun &rest args) check-binary-exists)
    "Only start eglot if the LSP binary exists in PATH.
This prevents eglot from failing when the binary isn't available."
    (if (warbo-eglot-check-binary-exists)
        (apply orig-fun args)
      (let* ((mode-server (or
                           (assoc major-mode eglot-server-programs)
                           (cl-find-if (lambda (entry)
                                         (let ((modes (car entry)))
                                           (or (eq modes major-mode)
                                               (and (listp modes)
                                                    (memq major-mode modes)))))
                                       eglot-server-programs)))
             (contact (cdr mode-server))
             (program (when contact
                        (cond
                         ((stringp contact) contact)
                         ((and (consp contact) (stringp (car contact)))
                          (car contact))
                         (t nil)))))
        (when program
          (message "eglot: LSP binary '%s' not found in PATH - not starting"
                   program)))))

  ;; From https://gluer.org/blog/improving-eglot-performance/
  (define-advice jsonrpc--log-event (:override (&rest _))
    "Silence jsonrpc logging to improve performance.")
  ;; From https://www.reddit.com/r/emacs/comments/1b25904/is_there_anything_i_can_do_to_make_eglots/
  ;(setf (plist-get eglot-events-buffer-config :size) 0)
  ;(eldoc-echo-area-use-multiline-p nil)
  (setf (alist-get 'typescript-mode eglot-server-programs)
        '("typescript-language-server" "--stdio"))
  :custom
  (eglot-autoshutdown t)  ;; shutdown language server after closing last file
  (eglot-confirm-server-initiated-edits nil))  ;; allow edits without confirmation

;; (use-package eglot-booster
;;   :ensure t
;;   :after eglot
;;   :config
;;   (eglot-booster-mode))

(use-package vertico
  :ensure t
  :functions (vertico-sort-history-length-alpha vertico--metadata-get)
  :init
  (vertico-mode)
  (keymap-set vertico-map "TAB" #'minibuffer-complete)
  :config
  ;; When typing an exact buffer name, it should be the default selection.
  ;; Consult adds U+200000 as invisible suffix for multi-category support.
  ;; Buffer completion sets display-sort-function to `identity`, so we must
  ;; use vertico-sort-override-function to take precedence.
  (defun warbo-vertico-sort-prefer-exact (candidates)
    "Sort CANDIDATES with exact match first, preserving MRU when appropriate.
When the completion source provides its own sorting (display-sort-function is
identity), we preserve that order and only move exact matches to front. This
respects MRU ordering from sources like consult. For other completions, we sort
by history/length/alpha."
    (let* ((display-sort-fn (vertico--metadata-get 'display-sort-function))
           ;; If source provides its own sorting (identity), preserve it
           (sorted (if (eq display-sort-fn #'identity)
                       candidates
                     (vertico-sort-history-length-alpha candidates)))
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
  :functions (yas-expand yas-reload-all)
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode)

  (defun yasnippet-or-completion ()
    "Try to expand a yasnippet snippet, otherwise invoke completion."
    (interactive)
    (or (yas-expand)
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
              (null (yas-expand)))
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

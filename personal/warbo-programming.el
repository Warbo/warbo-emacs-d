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

(use-package rainbow-identifiers
  :ensure t
  :hook (prog-mode . rainbow-identifiers-mode))

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

(use-package xref-union
  :ensure t
  :hook haskell-mode
  :config
  (setq tags-revert-without-query 1))

(defun warbo-haskell-tags ()
  "Runs command to generate TAGS file in root directory of current repo"
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

                      ;; This is commonly used for the output of Haskell
                      ;; Show instances, which looks enough like Haskell
                      ;; code for haskell-mode syntax highlighting to work.
                      ;;(haskell-font-lock-defaults-create)
                      ;;(font-lock-ensure)
                      ;;(haskell-mode)
                      ;;(haskell-font-lock-fontify-block 'shell-mode (point-min) (point-max))

                      ;; Enable a simple prog-mode like json-mode, which will
                      ;; give us rainbow-delimiters and rainbow-identifiers
                      (json-mode)
                      (font-lock-fontify-buffer)

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

;; From https://github.com/magit/magit/discussions/4748#discussioncomment-3589929
(defun my/magit-log-reflog (&optional args files)
  "Show log for objects mentioned in reflog."
  (interactive (magit-log-arguments))
  (magit-log-setup-buffer (list "--reflog") args files))

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

  (defun warbo-filter-bel ()
    "`nix-repl-completion-at-point' can leave ^G (BEL chars) in the buffer."
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "\C-g" nil t)
        (replace-match ""))))

  (advice-add 'nix-repl-completion-at-point :after 'warbo-filter-bel))

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
  :commands eglot-ensure eglot
  :hook ((vue-mode . eglot-ensure)
         (c-mode-common . eglot-ensure)
         (c-ts-base-mode . eglot-ensure)
         (js-base-mode . eglot-ensure)
         (typescript-ts-base-mode . eglot-ensure))
  :config
  (add-hook 'haskell-mode-hook 'eglot-ensure)
  (setq eglot-connect-timeout 300)  ;; Big projects might take a while!
  ;; From https://gluer.org/blog/improving-eglot-performance/
  (advice-add 'jsonrpc--log-event :override #'ignore)
  ;; From https://www.reddit.com/r/emacs/comments/1b25904/is_there_anything_i_can_do_to_make_eglots/
  ;(setf (plist-get eglot-events-buffer-config :size) 0)
  ;(eldoc-echo-area-use-multiline-p nil)
  (let ((projects (expand-file-name "~/src")))
    (when (file-directory-p projects)
      (eval-when-compile
        (require 'dash)
        (require 's))
      (require 'dash)
      (require 's)
      (let* ((root (car (-filter (lambda (entry)
                                   (and (not (s-starts-with? "yesod" entry))
                                        (not (s-starts-with? "." entry))))
                                 (directory-files "~/src"))))
             (tsdk (file-name-concat
                    projects root "webpack" "node_modules" "typescript" "lib")))
        (add-to-list 'eglot-server-programs
                     `(vue-mode . ("vue-language-server" "--stdio"
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
                                                                   :documentColor t))
                                   ))))))
  :custom
  (eglot-autoshutdown t)  ;; shutdown language server after closing last file
  (eglot-confirm-server-initiated-edits nil)  ;; allow edits without confirmation
  )

;; (use-package eglot-booster
;;   :ensure t
;;   :after eglot
;;   :config
;;   (eglot-booster-mode))

(use-package company
  :disabled
  :ensure t
  :hook (prog-mode . company-mode))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (keymap-set vertico-map "TAB" #'minibuffer-complete))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(basic partial-completion emacs22))
  (completion-category-defaults nil)
  ;(completion-category-overrides '((file (styles literal-prefix))))
  )

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

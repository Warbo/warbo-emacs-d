;;; warbo-haskell --- Haskell-specific configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(declare-function command-in-rolling-buffer "warbo-rolling-shell")
(declare-function haskell-cabal--find-tags-dir "haskell-cabal")
(declare-function haskell-ident-at-point "haskell-mode")
(declare-function haskell-string-drop-qualifier "haskell-string")
(declare-function haskell-string-trim "haskell-string")
(declare-function warbo-haskell-jump-to-tag-fix "warbo-haskell")

(defvar warbo-haskell-eglot-args
  '("haskell-language-server-wrapper" "--lsp"
    :initializationOptions
    (:haskell (:formattingProvider "fourmolu"
               :checkProject :json-false
               :sessionLoading "multipleComponents")))
  "Eglot server program entry for haskell-ts-mode.
Use in .dir-locals.el like:
    (eglot-server-programs . ((haskell-ts-mode . ,warbo-haskell-eglot-args)))")
;; eglot-server-programs is set per-project via .dir-locals.el
(put 'eglot-server-programs 'safe-local-variable #'listp)

(defun haskell-trace ()
  "Insert `Trace.trace` at point and add import if required."
  (interactive)
  (let ((orig-pos (point-marker))
        (import-line "import qualified Debug.Trace as Trace"))
    ;; Ensure the marker advances if we insert text exactly at its position
    ;; (e.g., if we're at the absolute beginning of the file)
    (set-marker-insertion-type orig-pos t)

    (save-excursion
      (goto-char (point-min))
      ;; Check if import already exists
      (unless (re-search-forward
               "^import[ \t]+qualified[ \t]+Debug\\.Trace[ \t]+as[ \t]+Trace"
               nil
               t)
        (goto-char (point-min))
        (cond
         ;; Case 1: Existing imports, insert before first
         ((re-search-forward "^import " nil t)
          (beginning-of-line)
          (insert import-line "\n"))

         ;; Case 2: No imports, with module declaration; insert after `where`
         ((re-search-forward "^module[ \t\n]" nil t)
          (if (re-search-forward "\\<where\\>" nil t)
              (progn
                (forward-line 1)
                (insert "\n" import-line "\n"))
            ;; Fallback if `where` isn't cleanly found
            (goto-char (point-min))
            (insert import-line "\n\n")))

         ;; Case 3: No imports or module declaration. Insert at the top, after
         ;; {-# ... #-} pragmas (if any).
         (t
          (goto-char (point-min))
          (while (re-search-forward "^{-#\\(?:.\\|\n\\)*?#-}[ \t]*\n*" nil t))
          (insert import-line "\n\n")))))

    ;; Now insert the `trace` call
    (goto-char orig-pos)
    (insert "Trace.trace")

    ;; Clean up marker
    (set-marker orig-pos nil)))

(defun warbo-haskell-tags ()
  "Run command to generate TAGS file in root directory of current repo.
Only runs if hasktags is available in PATH."
  (when (executable-find "hasktags")
    (let ((default-directory (vc-root-dir)))
      (when default-directory
        (start-process "hasktags" nil "hasktags" "--etags" ".")))))

(defun warbo-haskell-setup ()
  "Custom hook to setup `haskell-ts-mode'."
  (when (executable-find "hasktags")
    ;; Generate TAGS if they don't exist yet
    (let* ((root (vc-root-dir))
           (tags-file (when root (expand-file-name "TAGS" root))))
      (when (and tags-file (not (file-exists-p tags-file)))
        (warbo-haskell-tags)))
    ;; Keep TAGS updated when we save
    (add-hook 'after-save-hook 'warbo-haskell-tags nil t)))

(use-package haskell-ts-mode
  :ensure t
  :after xref-union
  :functions (tags-completion-table)
  :mode ("\\.hs\\'" "\\.lhs\\'" "\\.hsc\\'")
  :bind (:map haskell-ts-mode-map
              ("C-c d" . haskell-hoogle))
  :init
  ;; Ensures shebangs like '#!/usr/bin/env runhaskell' use haskell-ts-mode
  ;; TODO: Add a simple test to tests/haskell-tests.el that checks whether a
  ;; (dummy) file with that shebang will get opened in haskell-ts-mode.
  (add-to-list 'major-mode-remap-alist '(haskell-mode . haskell-ts-mode))
  :hook ((haskell-ts-mode . eglot-ensure)
         (haskell-ts-mode . xref-union-mode)
         (haskell-ts-mode . warbo-haskell-setup))
  :config

  ;; Fix for issue ba19a9bd56efb1af: haskell-mode-jump-to-tag with prefix arg
  ;; (C-u M-.) was giving "Wrong type argument: stringp, nil" when there's no
  ;; identifier at point.  The bug is that the original function bails out
  ;; entirely when haskell-ident-at-point returns nil, even when the user
  ;; asked to be prompted (via prefix arg).  The fix: when prompting was
  ;; requested or there's no identifier, prompt for one ourselves (with
  ;; TAGS completion) and pass the string to xref-find-definitions.
  ;; xref-union ensures etags can find it even if eglot can't (eglot's
  ;; backend ignores the identifier string and looks at cursor position).
  (defun warbo-haskell-jump-to-tag-fix (orig-fun &optional next-p)
    "Fix for haskell-mode-jump-to-tag to handle nil identifier.
If NEXT-P is non-nil or there's no identifier at point, prompt
for an identifier then jump to its definition.  Otherwise call
ORIG-FUN."
    (let* ((raw-ident (haskell-ident-at-point))
           (ident (when raw-ident
                    (haskell-string-drop-qualifier raw-ident)))
           (ident-valid (and ident
                             (not (string= ""
                                           (haskell-string-trim ident))))))
      (if (and ident-valid (not next-p))
          ;; Normal case: valid identifier, no prompting requested
          (funcall orig-fun next-p)
        ;; No valid identifier or prompting requested: prompt the user
        ;; for an identifier, then search for it in the TAGS file.
        (let* ((tags-file-dir (haskell-cabal--find-tags-dir))
               (tags-file-name (when tags-file-dir
                                 (concat tags-file-dir "TAGS")))
               (tags-revert-without-query t)
               (prompted-ident
                (completing-read
                 (if ident-valid
                     (format "Find definition (default %s): " ident)
                   "Find definition: ")
                 (when (and tags-file-name (file-exists-p tags-file-name))
                   (tags-completion-table))
                 nil nil nil nil
                 (when ident-valid ident))))
          (when (and prompted-ident
                     (not (string= "" prompted-ident)))
            (xref-find-definitions prompted-ident))))))

  (advice-add 'haskell-mode-jump-to-tag :around #'warbo-haskell-jump-to-tag-fix))

;; When HLS isn't available, flycheck provides hlint diagnostics as a fallback.
;; flycheck-haskell configures flycheck's Haskell checkers with the correct
;; cabal settings, GHC options, language extensions, and source directories.
(use-package flycheck-haskell
  :vc (:url "https://github.com/Warbo/flycheck-haskell.git" :rev :newest)
  :init
  ;; Warn if the installed flycheck-haskell lacks Cabal >= 3.14 support.
  ;; This happens when the package was installed from the original upstream
  ;; rather than the Warbo fork; use-package :vc won't reinstall automatically.
  (with-temp-buffer
    (ignore-errors
      (insert-file-contents (locate-file "get-cabal-configuration.hs"
                                         (list (file-name-directory
                                                (locate-library "flycheck-haskell"))))))
    (unless (search-forward "Cabal314OrLater" nil t)
      (warn (concat "flycheck-haskell lacks Cabal >= 3.14 support.\n"
                    "Reinstall from the fixed fork with:\n"
                    "  (package-vc-install"
                    " \"https://github.com/Warbo/flycheck-haskell.git\""
                    " nil nil 'flycheck-haskell)"))))
  :hook (haskell-ts-mode . flycheck-haskell-setup))

;; ghcid integration: run ghcid in a shell buffer with nice error highlighting
(use-package warbo-rolling-shell
  :config
  (defvar ghcid-height 65536 "How many lines to truncate a ghcid buffer to.")
  (defun ghcid ()
    "Run ghcid in a shell-mode buffer"
    (interactive)
    (when (get-buffer "ghcid")
      (let ((kill-buffer-query-functions nil)) ;; Don't ask
        (kill-buffer "ghcid")))
    (dlet ((shell-mode-hook nil))  ;; Avoid warbo-shell-hook's colour mangling
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

(provide 'warbo-haskell)
;;; warbo-haskell.el ends here

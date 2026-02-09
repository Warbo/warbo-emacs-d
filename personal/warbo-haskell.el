;;; warbo-haskell --- Haskell-specific configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Declare functions defined in other files
(declare-function command-in-rolling-buffer "warbo-rolling-shell")

(defvar warbo-haskell-eglot-args
  '("haskell-language-server" "lsp"
    :initializationOptions
    (:haskell (:formattingProvider "fourmolu"
               :checkProject :json-false
               :sessionLoading "multipleComponents")))
  "Eglot server program entry for haskell-mode.
Use in .dir-locals.el: (eglot-server-programs . ((haskell-mode . ,warbo-haskell-eglot-args)))")

(defun warbo-haskell-tags ()
  "Run command to generate TAGS file in root directory of current repo.
Only runs if hasktags is available in PATH."
  (when (executable-find "hasktags")
    (let ((default-directory (vc-root-dir)))
      (when default-directory
        (start-process "hasktags" nil "hasktags" "--etags" ".")))))

(defun warbo-haskell-setup ()
  "Custom hook to setup `haskell-mode'."
  (when (executable-find "hasktags")
    ;; Generate TAGS if they don't exist yet
    (let* ((root (vc-root-dir))
           (tags-file (when root (expand-file-name "TAGS" root))))
      (when (and tags-file (not (file-exists-p tags-file)))
        (warbo-haskell-tags)))
    ;; Keep TAGS updated when we save
    (add-hook 'after-save-hook 'warbo-haskell-tags nil t)))

(use-package haskell-mode
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'warbo-haskell-setup))

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

(provide 'warbo-haskell)
;;; warbo-haskell.el ends here

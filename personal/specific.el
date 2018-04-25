;;; warbo-emacs-d --- A collection of specific toggles, etc.

;;; Commentary:

;;; Code:

;; Make doc-view continuous
(setq doc-view-continuous t)

;; Browse with conkeror, since eww seems to barf on news fallback to conkeror
;; with "&"
;;(setq browse-url-browser-function 'eww-browse-url)
(setq browse-url-browser-function 'browse-url-conkeror)
(setq browse-url-generic-program (executable-find "conkeror")
      shr-external-browser 'browse-url-generic)

;; Haskell programming
(require 'use-package)
;; (use-package dante
;;   :ensure t
;;   :commands 'dante-mode
;;   :init
;;   (add-hook 'haskell-mode-hook 'dante-mode)
;;   (add-hook 'haskell-mode-hook 'flycheck-mode))

;; (add-hook 'haskell-mode-hook 'dante-mode)
(add-hook 'haskell-mode-hook 'flycheck-mode)
;(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(custom-set-variables '(haskell-stylish-on-save nil))

(defun haskell-indent ()
  "Send Haskell buffers through indentation program."
  ;; Check that we've been "manually saved" (not auto-saved) and that we're a
  ;; Haskell file (see https://emacs.stackexchange.com/a/14476/5391 )
  (when (and (memq this-command '(save-buffer save-some-buffers))
             (eq major-mode 'haskell-mode))
    (shell-command-to-string (format "brittany %s" buffer-file-name))
    (revert-buffer nil t t)))

(add-hook 'after-save-hook 'haskell-indent)

(eval-after-load "haskell-mode"
  '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))

(eval-after-load "haskell-cabal"
  '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))

;; Isabelle files
(use-package isar-mode
  :quelpa (isar-mode :fetcher github :repo "agomezl/simp-isar-mode")
  :mode "\\.thy\\'")

;; Proof General
(defun init-pg ()
  "Load ProofGeneral from Nix location."
  (ignore-errors (load-file "~/.nix-profile/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el")))

;; Useful for manual entry of PDF titles into BibTeX
(defun take-name ()
  "Find the next localfile key which doesn't have a title."
  (re-search-forward "^@misc{zzzzz.*,[\n][\t]localfile = \"[^\"]*\"[\n]")
  (forward-line -1)
  (beginning-of-line)
  (re-search-forward "localfile = ")
  ;; Copy the contents
  (forward-char)  ;; Past "
  (set-mark (point))
  (end-of-line)
  (backward-char) ;; Past "
  ;; Open the file in a temporary doc-view buffer
  (let ((selection (buffer-substring-no-properties (mark) (point)))
        (title     ""))
    (with-temp-buffer
      (insert-file-contents selection)
      (doc-view-mode)
      (switch-to-buffer (current-buffer))
      (sit-for 2)
      (doc-view-fit-width-to-window)
      ;; Query for the title
      (setq title (read-from-minibuffer "Title: ")))
    ;; Insert a title key into the BibTeX
    (end-of-line)
    (insert ",\n\ttitle = \"")
    (insert title)
    (insert "\"")))

;; Don't run Flymake over TRAMP
(if (boundp 'flymake-allowed-file-name-masks)
    (setq flymake-allowed-file-name-masks
          (cons '("^/ssh:" (lambda () nil))
                flymake-allowed-file-name-masks)))

;; Try to prevent "undo-tree-mapc: Wrong type argument: listp, \.\.\."
;; From http://defindit.com/readme_files/tom_emacs.html
(when (file-exists-p ".emacs.desktop")
  (setq desktop-path '("."))
  (desktop-save-mode 1))

;; Try to prevent TRAMP slowdowns, as per
;; http://emacs.stackexchange.com/a/17579/5391
(setq projectile-mode-line "Projectile")

;; Flycheck all the things
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-to-list 'flycheck-checkers 'nix)

;; Artemis uses maildirs for issue tracking. Use message-mode for editing these,
;; but augment it a little (e.g. save and close, rather than send).
(defun artemis-save ()
  "Save and close the buffer; used for Artemis messages instead of 'send'."
  (interactive)
  (save-some-buffers)
  (kill-buffer))

(defvar artemis-mode-hook nil)
(defvar artemis-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-c") 'artemis-save)
    map)
  "Keymap for Artemis major mode.")

(define-derived-mode artemis-mode message-mode "Artemis"
  "Major mode for editing Artemis issues (which are maildir under the hood)."
  (set-syntax-table artemis-mode-syntax-table))

(provide 'artemis-mode)

(defun switch-to-artemis (filename)
  "Look for a buffer with the given FILENAME and switch it to artemis-mode."
  (let ((buf (find-buffer-visiting filename)))
    (when buf
      (with-current-buffer buf
        (artemis-mode)))))

;; Prevent Nix '.drv' filesf rom opening as LaTeX
(add-to-list 'auto-mode-alist '("\\.drv\\'" . fundamental-mode))

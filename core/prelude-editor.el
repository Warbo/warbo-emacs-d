;;; prelude-editor.el --- Emacs Prelude: enhanced core editing experience.
;;
;; Copyright © 2011-2016 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Refinements of the core editing experience in Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; Helper macros (kept outside use-package for now)
(defmacro with-region-or-buffer (func)
  "When called with no active region, call FUNC on current buffer."
  `(advice-add ',func :before
               (lambda ()
                 (interactive
                  (if mark-active
                      (list (region-beginning) (region-end))
                    (list (point-min) (point-max)))))))

;; Group 1: Basic Editor Behavior and Tweaks
(use-package emacs
  :bind (("C-x \\" . align-regexp)
         ("C-+" . text-scale-increase)
         ("C--" . text-scale-decrease)
         ("C-x O" . (lambda () (interactive) (other-window -1)))
         ("C-x M-m" . shell)
         ("C-<backspace>" . (lambda () (interactive) (kill-line 0) (indent-according-to-mode)))
         ("M-/" . hippie-expand)
         ("C-x C-b" . ibuffer)
         ("<f12>" . menu-bar-mode)
         :map help-command
         ("A" . apropos)
         ("C-f" . find-function)
         ("C-k" . find-function-on-key)
         ("C-v" . find-variable)
         ("C-l" . find-library)
         ("C-i" . info-display-manual)
         :map isearch-mode-map
         ("C-o" . isearch-occur)
         ("C-x p" . proced)
         ("C-x C-m" . smex)
         )
  :config
  ;; Death to the tabs!  However, tabs historically indent to the next
  ;; 8-character offset; specifying anything else will cause *mass*
  ;; confusion, as it will change the appearance of every existing file.
  ;; In some cases (python), even worse -- it will change the semantics
  ;; (meaning) of the program.
  ;;
  ;; Emacs modes typically provide a standard means to change the
  ;; indentation width -- eg. c-basic-offset: use that to adjust your
  ;; personal indentation width, while maintaining the style (and
  ;; meaning) of any files you load.
  (setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
  (setq-default tab-width 8)            ;; but maintain correct appearance

  ;; Newline at end of file
  (setq require-final-newline t)

  ;; delete the selection with a keypress
  (delete-selection-mode t)

  ;; disable annoying blink-matching-paren
  (setq blink-matching-paren nil)

  ;; hippie expand is dabbrev expand on steroids
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                           try-expand-dabbrev-all-buffers
                                           try-expand-dabbrev-from-kill
                                           try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-all-abbrevs
                                           try-expand-list
                                           try-expand-line
                                           try-complete-lisp-symbol-partially
                                           try-complete-lisp-symbol))

  ;; enable narrowing commands
  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil)
  (put 'narrow-to-defun 'disabled nil)

  ;; enabled change region case commands
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)

  ;; enable erase-buffer command
  (put 'erase-buffer 'disabled nil)

  ;; highlight the current line
  (global-hl-line-mode +1)

  ;; Advice for basic commands
  (with-region-or-buffer indent-region)
  (with-region-or-buffer untabify)

  ;; automatically indenting yanked text if in programming-modes
  (defun yank-advised-indent-function (beg end)
    "Do indentation, as long as the region isn't too large."
    (if (<= (- end beg) prelude-yank-indent-threshold)
        (indent-region beg end nil)))

  (advice-add 'yank :after 'yank-indent
    (lambda (&optional arg)
      "If current mode is one of `prelude-yank-indent-modes',
indent yanked text (with prefix arg don't indent)."
      (if (and (not arg)
               (not (member major-mode prelude-indent-sensitive-modes))
               (or (derived-mode-p 'prog-mode)
                   (member major-mode prelude-yank-indent-modes)))
          (let ((transient-mark-mode nil))
            (yank-advised-indent-function (region-beginning) (region-end))))))

  (advice-add 'yank-pop :after 'yank-pop-indent
    (lambda (&optional arg)
      "If current mode is one of `prelude-yank-indent-modes',
indent yanked text (with prefix arg don't indent)."
      (if (and (not arg)
               (not (member major-mode prelude-indent-sensitive-modes))
               (or (derived-mode-p 'prog-mode)
                   (member major-mode prelude-yank-indent-modes)))
          (let ((transient-mark-mode nil))
            (yank-advised-indent-function (region-beginning) (region-end))))))

  ;; abbrev config
  (add-hook 'text-mode-hook 'abbrev-mode)

  ;; make a shell script executable automatically on save
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p)

  ;; .zsh file is shell script too
  (add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

  ;; Colorize output of Compilation Mode, see
  ;; http://stackoverflow.com/a/3072831/355252
  (defun prelude-colorize-compilation-buffer ()
    "Colorize a compilation mode buffer."
    (interactive)
    ;; we don't want to mess with child modes such as grep-mode, ack, ag, etc
    (when (eq major-mode 'compilation-mode)
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region (point-min) (point-max)))))
  (add-hook 'compilation-filter-hook #'prelude-colorize-compilation-buffer)

  ;; server-visit-files advice
  (advice-add 'server-visit-files :filter-args
              (lambda (args)
                "Filter arguments for `server-visit-files` to handle filename:linenumber."
                (let ((files (car args))
                      (proc (cadr args))
                      (nowait (caddr args)))
                  (list
                   (mapcar (lambda (fn)
                             (let ((name (car fn)))
                               (if (string-match "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$" name)
                                   (cons
                                    (match-string 1 name)
                                    (cons (string-to-number (match-string 2 name))
                                          (string-to-number (or (match-string 3 name) ""))))
                                 fn))) files)
                   proc
                   nowait))))
  )

;; Group 2: File/Buffer Management
(use-package files
  :config
  ;; store all backup and autosave files in the tmp dir
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))

  ;; revert buffers automatically when underlying files are changed externally
  (global-auto-revert-mode t)

  ;; meaningful names for buffers with the same name
  (require 'uniquify) ; uniquify is built-in, but require is good practice
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

  ;; saveplace remembers your location in a file when saving files
  ;; save-place is built-in
  (setq save-place-file (expand-file-name "saveplace" prelude-savefile-dir))
  ;; activate it for all buffers
  (if (< emacs-major-version 25)
      (progn (require 'saveplace)
             (setq-default save-place t))
    (save-place-mode 1))

  ;; savehist keeps track of some history
  ;; savehist is built-in
  (require 'savehist)
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" prelude-savefile-dir))
  (savehist-mode +1)

  ;; save recent files
  ;; recentf is built-in
  (require 'recentf)
  (setq recentf-save-file (expand-file-name "recentf" prelude-savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)

  (defun prelude-recentf-exclude-p (file)
    "A predicate to decide whether to exclude FILE from recentf."
    (let ((file-dir (file-truename (file-name-directory file))))
      (-any-p (lambda (dir)
                (string-prefix-p dir file-dir))
              (mapcar 'file-truename (list prelude-savefile-dir package-user-dir)))))

  (add-to-list 'recentf-exclude 'prelude-recentf-exclude-p)

  (recentf-mode +1)

  ;; bookmarks
  ;; bookmark is built-in
  (require 'bookmark)
  (setq bookmark-default-file (expand-file-name "bookmarks" prelude-savefile-dir)
        bookmark-save-flag 1)

  ;; clean up obsolete buffers automatically
  ;; midnight is built-in
  (require 'midnight)

  ;; semanticdb
  (setq semanticdb-default-save-directory
        (expand-file-name "semanticdb" prelude-savefile-dir)))

(use-package windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings))

(use-package rect
  :config
  ;; note - this should be after volatile-highlights is required
  ;; add the ability to cut the current line, without marking it
  ;; rect is built-in
  nil) ; require is enough, no specific config here

(use-package tramp
  :config
  ;; tramp, for sudo access
  ;; keep in mind known issues with zsh - see emacs wiki
  (setq tramp-default-method "ssh"))

(use-package imenu
  :config
  (set-default 'imenu-auto-rescan t))

(use-package flyspell
  :config
  ;; flyspell-mode does spell-checking on the fly as you type
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))

  (defun prelude-enable-flyspell ()
    "Enable command `flyspell-mode' if `prelude-flyspell' is not nil."
    (when (and prelude-flyspell (executable-find ispell-program-name))
      (flyspell-mode +1)))

  (add-hook 'text-mode-hook 'prelude-enable-flyspell))

;; bookmarks
(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks" prelude-savefile-dir)
      bookmark-save-flag 1)

(use-package dired
  :config
  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)

  ;; enable some really cool extensions like C-x C-j(dired-jump)
  (require 'dired-x)) ; dired-x is built-in

(use-package ediff
  :config
  ;; ediff - don't start another frame
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package re-builder
  :config
  ;; saner regex syntax
  (setq reb-re-syntax 'string))

(use-package eshell
  :bind (("C-x m" . eshell)
         ("C-x M" . (lambda () (interactive) (eshell t))))
  :config
  (require 'eshell) ; eshell is built-in
  (setq eshell-directory-name (expand-file-name "eshell" prelude-savefile-dir)))

(use-package compile
  :config
  ;; Compilation from Emacs
  (setq compilation-ask-about-save nil  ;; Save before compiling
        compilation-always-kill t       ;; Kill old procs before starting new
        compilation-scroll-output 'first-error ;; Autoscroll to first error
        ))

(use-package ansi-color
  :config
  ;; ansi-color is built-in
  nil) ; require is enough, hook is in emacs use-package

(use-package winner-mode
  :config
  ;; enable winner-mode to manage window configurations
  (winner-mode +1))

(provide 'prelude-editor)

;;; prelude-editor.el ends here

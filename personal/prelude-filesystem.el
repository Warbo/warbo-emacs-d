;;; prelude-filesystem.el --- Filesystem settings. -*- lexical-binding: t; -*-
;;
;; This file contains file and buffer history management functionality,
;; extracted from Emacs Prelude.
;;
;; Copyright © 2011-2016 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configuration for managing files, paths to store temp files, etc.

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

(use-package files
  :config
  ;; revert buffers automatically when underlying files are changed externally
  (global-auto-revert-mode t)

  ;; meaningful names for buffers with the same name
  (require 'uniquify) ; uniquify is built-in, but require is good practice
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\*") ; don't muck with special buffers

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

(provide 'prelude-filesystem)
;;; prelude-filesystem.el ends here

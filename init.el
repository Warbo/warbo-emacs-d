;;; init.el --- Prelude's configuration entry point.
;;
;; Copyright (c) 2011-2016 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://batsov.com/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file simply sets up the default load path and requires
;; the various modules defined within Emacs Prelude.

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

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Use "package.el" to get "use-package", which simplifies packaging
(unless (package-installed-p 'use-package)
  ;;(package-refresh-contents)
  (package-install 'use-package))

;; Get quelpa-use-package, which allows 'packages' to come from arbitrary places
(unless (package-installed-p 'quelpa-use-package)
  ;;(package-refresh-contents)
  (package-install 'quelpa-use-package))

(use-package quelpa-use-package
  :init (setq quelpa-update-melpa-p nil))

(use-package quelpa
  :ensure t)

;; Set PATH and 'exec-path', so external commands will work.
(use-package exec-path-from-shell
  :commands exec-path-from-shell-initialize
  :custom
  (exec-path-from-shell-arguments '("-l"))
  :config
  (exec-path-from-shell-initialize))

(defvar prelude-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Prelude distribution.")
(defvar prelude-personal-dir (expand-file-name "personal" prelude-dir)
  "This directory is for your personal configuration.

Users of Emacs Prelude are encouraged to keep their personal configuration
changes in this directory.  All Emacs Lisp files there are loaded automatically
by Prelude.")
(defvar prelude-savefile-dir (expand-file-name "savefile" prelude-dir)
  "This folder stores all the automatically generated save/history-files.")

(unless (file-exists-p prelude-savefile-dir)
  (make-directory prelude-savefile-dir))

;; add Prelude's directories to Emacs's `load-path'
(add-to-list 'load-path (expand-file-name "core" prelude-dir))

(require 'prelude-packages)
(require 'prelude-custom)  ;; Needs to be loaded before core, editor and ui

(unless noninteractive
  (require 'prelude-ui))

(require 'prelude-core)
(require 'prelude-mode)
(require 'prelude-editor)
(require 'prelude-global-keybindings)

;; Config changes made through the customize UI will be stored in custom.el
(add-to-list 'load-path prelude-personal-dir)
(setq custom-file (expand-file-name "custom.el" prelude-personal-dir))
(load custom-file)
(load (expand-file-name "flycheck-custom.el" prelude-personal-dir))

;; Most of our custom functionality lives in personal/warbo.el
(use-package warbo)

(message "Finished init.el")

;;; init.el ends here

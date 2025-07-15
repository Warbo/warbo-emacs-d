;;; init.el --- Entry point for Warbo's Emacs config.
;;
;; Copyright (c) 2011-2016 Bozhidar Batsov
;; Copyright (c) 2016-     Chris Warburton
;;
;; Author: Chris Warburton
;; URL: http://chriswarbo.net/git/warbo-emacs-d
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file was originally part of Emacs Prelude by Bozhidar Batsov, and has
;; since been heavily modified (to remove options/indirections I don't need, and
;; due to functionality being available in other packages or from Emacs updates)
;;
;; Over time I hope to remove all the remnants of Emacs Prelude, so everything
;; is defined as use-package blocks.

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

;; Use "package.el" to get "use-package", which simplifies packaging
(package-initialize)
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

(defvar prelude-dir user-emacs-directory
  "The root dir of the Emacs Prelude distribution.")
(defvar prelude-savefile-dir (expand-file-name "savefile" prelude-dir)
  "This folder stores all the automatically generated save/history-files.")

(unless (file-exists-p prelude-savefile-dir)
  (make-directory prelude-savefile-dir))

;; add Prelude's directories to Emacs's `load-path'
(add-to-list 'load-path (expand-file-name "core" prelude-dir))

;; Use cl-macrolet to avoid repeating ourselves, and to ensure :load-path gets a
;; literal value (other expressions, like function calls, won't work).
(cl-macrolet
    ((prelude-package (name &rest args)
       `(use-package ,name
          :demand t
          :load-path
          ,(expand-file-name "core" user-emacs-directory)
          ,@args)))

  (prelude-package prelude-custom) ;; Needs to be loaded before core, editor and ui
  (prelude-package prelude-ui
                   :unless noninteractive)
  (prelude-package prelude-core)
  (prelude-package prelude-editor))

(let ((personal-dir
       (expand-file-name "personal" user-emacs-directory)))
  ;; Config changes made through the customize UI will be stored in custom.el
  (add-to-list 'load-path personal-dir)
  (setq custom-file (expand-file-name "custom.el" personal-dir))
  (load custom-file)
  (load (expand-file-name "flycheck-custom.el" personal-dir)))

;; Most of our custom functionality lives in personal/warbo.el
(cl-macrolet
    ((in-personal (name)
       `(use-package ,name
         :load-path
         ,(expand-file-name "personal" user-emacs-directory))))
  (in-personal warbo))

(message "Finished init.el")

;;; init.el ends here

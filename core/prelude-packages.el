;;; prelude-packages.el --- Emacs Prelude: default package selection.
;;
;; Copyright © 2011-2016 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Takes care of the automatic installation of all the packages required by
;; Emacs Prelude.

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
(require 'package)

(use-package ace-window
  :ensure t)

(use-package avy
  :ensure t)

(use-package anzu
  :ensure t)

(use-package beacon
  :ensure t)

(use-package browse-kill-ring
  :ensure t)

(use-package crux
  :ensure t)

(use-package discover-my-major
  :ensure t)

(use-package diff-hl
  :ensure t)

(use-package easy-kill
  :ensure t)

(use-package epl
  :ensure t)

(use-package expand-region
  :ensure t)

(use-package gist
  :ensure t)

(use-package god-mode
  :ensure t)

(use-package grizzl
  :ensure t)

(use-package guru-mode
  :ensure t)

(use-package imenu-anywhere
  :ensure t)

(use-package ov
  :ensure t)

(use-package projectile
  :ensure t)

(use-package move-text
  :ensure t)

(use-package operate-on-number
  :ensure t)

(use-package smartrep
  :ensure t)

(use-package volatile-highlights
  :ensure t)

(use-package which-key
  :ensure t)

(use-package zop-to-char
  :ensure t)

(use-package puppet-mode
  :ensure t
  :mode "\\.pp\\'")

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")

(provide 'prelude-packages)
;; Local Variables:
;; End:

;;; prelude-packages.el ends here

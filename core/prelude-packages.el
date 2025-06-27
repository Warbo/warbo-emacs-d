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
(require 'seq)

;; TODO: Switch all this to use-package!

(defvar prelude-packages
  '(ace-window
    avy
    anzu
    beacon
    browse-kill-ring
    crux
    dash
    discover-my-major
    diff-hl
    diminish
    easy-kill
    epl
    expand-region
    flycheck
    gist
    git-timemachine
    god-mode
    grizzl
    guru-mode
    imenu-anywhere
    ov
    projectile
    move-text
    operate-on-number
    ;;smart-mode-line
    smartparens
    smartrep
    undo-tree
    volatile-highlights
    which-key
    zenburn-theme
    zop-to-char)
  "A list of packages to ensure are installed at launch.")

(defun prelude-packages-installed-p ()
  "Check if all packages in `prelude-packages' are installed."
  (seq-every-p #'package-installed-p prelude-packages))

(defun prelude-require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package prelude-packages)
    (add-to-list 'prelude-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun prelude-require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'prelude-require-package packages))

(defun prelude-install-packages ()
  "Install all packages listed in `prelude-packages'."
  (unless (prelude-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs Prelude is now refreshing its package database...")
    ;(package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (prelude-require-packages prelude-packages)))

;; run package installation
(prelude-install-packages)

(defun prelude-list-foreign-packages ()
  "Browse third-party packages not bundled with Prelude.

Behaves similarly to `package-list-packages', but shows only the packages that
are installed and are not in `prelude-packages'.  Useful for
removing unwanted packages."
  (interactive)
  (package-show-package-list
   (seq-difference package-activated-list prelude-packages)))


;; Auto-install packages using use-package :mode
(use-package clojure-mode :mode "\\.clj\\'")
(use-package cmake-mode :mode ("\\.cmake\\'" "CMakeLists\\.txt\\'"))
(use-package coffee-mode :mode "\\.coffee\\'")
(use-package css-mode :mode "\\.css\\'")
(use-package csv-mode :mode "\\.csv\\'")
(use-package cask-mode :mode "Cask")
(use-package d-mode :mode "\\.d\\'")
(use-package dart-mode :mode "\\.dart\\'")
(use-package elm-mode :mode "\\.elm\\'")
(use-package elixir-mode :mode ("\\.ex\\'" "\\.exs\\'" "\\.elixir\\'"))
(use-package erlang :mode "\\.erl\\'")
(use-package feature-mode :mode "\\.feature\\'")
(use-package go-mode :mode "\\.go\\'")
(use-package groovy-mode :mode "\\.groovy\\'")
(use-package haml-mode :mode "\\.haml\\'")
(use-package haskell-mode :mode "\\.hs\\'")
(use-package json-mode :mode "\\.json\\'")
(use-package kotlin-mode :mode "\\.kt\\'")
(use-package kivy-mode :mode "\\.kv\\'")
(use-package auctex :mode "\\.latex\\'")
(use-package less-css-mode :mode "\\.less\\'")
(use-package lua-mode :mode "\\.lua\\'")
;; markdown-mode handled separately to use gfm-mode
(use-package markdown-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)))
(use-package tuareg :mode "\\.ml\\'")
(use-package puppet-mode :mode "\\.pp\\'")
(use-package php-mode :mode "\\.php\\'")
(use-package protobuf-mode :mode "\\.proto\\'")
(use-package cython-mode :mode ("\\.pyd\\'" "\\.pyi\\'" "\\.pyx\\'"))
(use-package pkgbuild-mode :mode "PKGBUILD\\'")
(use-package rust-mode :mode "\\.rs\\'")
(use-package sass-mode :mode "\\.sass\\'")
(use-package scala-mode :mode "\\.scala\\'")
(use-package scss-mode :mode "\\.scss\\'")
(use-package slim-mode :mode "\\.slim\\'")
(use-package stylus-mode :mode "\\.styl\\'")
(use-package swift-mode :mode "\\.swift\\'")
(use-package textile-mode :mode "\\.textile\\'")
(use-package thrift :mode "\\.thrift\\'")
(use-package yaml-mode :mode ("\\.yml\\'" "\\.yaml\\'"))
(use-package dockerfile-mode :mode "Dockerfile\\'")

(provide 'prelude-packages)
;; Local Variables:
;; End:

;;; prelude-packages.el ends here

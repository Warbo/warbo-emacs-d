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

(use-package dash
  :ensure t)

(use-package discover-my-major
  :ensure t)

(use-package diff-hl
  :ensure t)

(use-package diminish
  :ensure t)

(use-package easy-kill
  :ensure t)

(use-package epl
  :ensure t)

(use-package expand-region
  :ensure t)

(use-package flycheck
  :ensure t)

(use-package gist
  :ensure t)

(use-package git-timemachine
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

(use-package smartparens
  :ensure t)

(use-package smartrep
  :ensure t)

(use-package undo-tree
  :ensure t)

(use-package volatile-highlights
  :ensure t)

(use-package which-key
  :ensure t)

(use-package zenburn-theme
  :ensure t)

(use-package zop-to-char
  :ensure t)

(use-package clojure-mode
  :ensure t
  :mode "\\.clj\\'")

(use-package cmake-mode
  :ensure t
  :mode ("\\.cmake\\'" "CMakeLists\\.txt\\'"))

(use-package coffee-mode
  :ensure t
  :mode "\\.coffee\\'")

(use-package css-mode
  :ensure t
  :mode "\\.css\\'")

(use-package csv-mode
  :ensure t
  :mode "\\.csv\\'")

(use-package cask-mode
  :ensure t
  :mode "Cask")

(use-package d-mode
  :ensure t
  :mode "\\.d\\'")

(use-package dart-mode
  :ensure t
  :mode "\\.dart\\'")

(use-package elm-mode
  :ensure t
  :mode "\\.elm\\'")

(use-package elixir-mode
  :ensure t
  :mode ("\\.ex\\'" "\\.exs\\'" "\\.elixir\\'"))

(use-package erlang
  :ensure t
  :mode "\\.erl\\'")

(use-package feature-mode
  :ensure t
  :mode "\\.feature\\'")

(use-package go-mode
  :ensure t
  :mode "\\.go\\'")

(use-package groovy-mode
  :ensure t
  :mode "\\.groovy\\'")

(use-package haml-mode
  :ensure t
  :mode "\\.haml\\'")

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'")

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(use-package kotlin-mode
  :ensure t
  :mode "\\.kt\\'")

(use-package kivy-mode
  :ensure t
  :mode "\\.kv\\'")

(use-package auctex
  :ensure t
  :mode "\\.latex\\'")

(use-package less-css-mode
  :ensure t
  :mode "\\.less\\'")

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'")

(use-package markdown-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)))

(use-package tuareg
  :ensure t
  :mode "\\.ml\\'")

(use-package puppet-mode
  :ensure t
  :mode "\\.pp\\'")

(use-package php-mode
  :ensure t
  :mode "\\.php\\'")

(use-package protobuf-mode
  :ensure t
  :mode "\\.proto\\'")

(use-package cython-mode
  :ensure t
  :mode ("\\.pyd\\'" "\\.pyi\\'" "\\.pyx\\'"))

(use-package pkgbuild-mode
  :ensure t
  :mode "PKGBUILD\\'")

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")

(use-package sass-mode
  :ensure t
  :mode "\\.sass\\'")

(use-package scala-mode
  :ensure t
  :mode "\\.scala\\'")

(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'")

(use-package slim-mode
  :ensure t
  :mode "\\.slim\\'")

(use-package stylus-mode
  :ensure t
  :mode "\\.styl\\'")

(use-package swift-mode
  :ensure t
  :mode "\\.swift\\'")

(use-package textile-mode
  :ensure t
  :mode "\\.textile\\'")

(use-package thrift
  :ensure t
  :mode "\\.thrift\\'")

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" "\\.yaml\\'"))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

(provide 'prelude-packages)
;; Local Variables:
;; End:

;;; prelude-packages.el ends here

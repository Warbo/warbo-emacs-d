(defun defer (f)
  "Defer calling the function F until Emacs has finished initialising."
  (run-with-idle-timer 2 nil f))

;; See which per-machine options we should enable
(defmacro mac-only (&rest body)
  `(when (file-directory-p "/Users/chris")
      ,@body))

(defmacro thinkpad-only (&rest body)
  `(when (file-directory-p "/home/chris")
     ,@body))

;; Set up packaging, first with Emacs's built-in "package.el" functionality
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Next we use "package.el" to get "use-package", which simplifies packaging
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Get quelpa-use-package, which allows 'packages' to come from arbitrary places
(unless (package-installed-p 'quelpa-use-package)
  (package-refresh-contents)
  (package-install 'quelpa-use-package))

(use-package quelpa-use-package
  :init (setq quelpa-update-melpa-p nil))

;; Turn off UI clutter
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; Prevent later problems with magit
(use-package magit
  :ensure t)

;; (use-package ace-jump-mode)

;; (use-package ace-window)

;; (use-package ag)

;; (use-package ahg)

;; (use-package anzu)

;; (use-package arduino-mode)

;; (use-package async)

;; (use-package auctex)

;; (use-package avy)

;; (use-package beacon)

;; (use-package bind-key)

;; (use-package browse-kill-ring)

;; (use-package caml)

;; (use-package cmake-mode)

;; (use-package company)

;; (use-package counsel)

;; (use-package crux)

;; (use-package csv-mode)

;; (use-package dante)

;; (use-package dash)

;; (use-package dash-functional)

;; (use-package diff-hl)

;; (use-package diminish)

;; (use-package discover-my-major)

;; (use-package dumb-jump)

;; (use-package easy-kill)

;; (use-package ebib)

;; (use-package elisp-slime-nav)

;; (use-package epl)

;; (use-package ess)

;; (use-package expand-region)

;; (use-package f)

;; (use-package faceup)

;; (use-package fill-column-indicator)

;; (use-package flx)

;; (use-package flx-ido)

;; (use-package flycheck)

;; (use-package flycheck-haskell)

;; (use-package flymake-easy)

;; (use-package flymake-haskell-multi)

;; (use-package fold-dwim)

;; (use-package fsm)

;; (use-package geiser)

;; (use-package gh)

;; (use-package gist)

;; (use-package git-commit)

;; (use-package git-timemachine)

;; (use-package gitconfig-mode)

;; (use-package gitignore-mode)

;; (use-package gnuplot)

;; (use-package god-mode)

;; (use-package grizzl)

;; (use-package guru-mode)

;; (use-package haskell-mode)

;; (use-package helm)

;; (use-package helm-core)

;; (use-package helm-projectile)

;; (use-package highlight)

;; (use-package ht)

;; (use-package htmlize)

;; (use-package hydra)

;; (use-package identica-mode)

;; (use-package idris-mode)

;; (use-package iedit)

;; (use-package imenu-anywhere)

;; (use-package intero)

;; (use-package ivy)

;; (use-package jabber)

;; (use-package json-mode)

;; (use-package json-reformat)

;; (use-package json-snatcher)

;; (use-package julia-mode)

;; (use-package key-chord)

;; (use-package let-alist)

;; (use-package lispy)

;; (use-package load-relative)

;; (use-package loc-changes)

;; (use-package logito)

;; (use-package lsp-haskell)

;; (use-package lsp-mode)

;; (use-package macrostep)

;; (use-package magit)

;; (use-package magit-popup)

;; (use-package makey)

;; (use-package markdown-mode)

;; (use-package marshal)

;; (use-package maude-mode)

;; (use-package memoize)

;; (use-package memory-usage)

;; (use-package monky)

;; (use-package move-text)

;; (use-package multiple-cursors)

;; (use-package nix-buffer)

;; (use-package nix-mode)

;; (use-package nix-sandbox)

;; (use-package nixos-options)

;; (use-package noflet)

;; (use-package o-blog)

;; (use-package operate-on-number)

;; (use-package org)

;; (use-package ov)

;; (use-package package-build)

;; (use-package paredit)

;; (use-package parsebib)

;; (use-package pcache)

;; (use-package php-mode)

;; (use-package pkg-info)

;; (use-package popup)

;; (use-package pretty-sha-path)

;; (use-package projectile)

;; (use-package prop-menu)

;; (use-package quelpa)

;; (use-package quelpa-use-package)

;; (use-package racket-mode)

;; (use-package rainbow-delimiters)

;; (use-package rainbow-mode)

;; (use-package realgud)

;; (use-package rich-minority)

;; (use-package ruby-tools)

;; (use-package s)

;; (use-package scala-mode)

;; (use-package seq)

;; (use-package shadchen)

;; (use-package slime)

;; (use-package smart-mode-line)

;; (use-package smartparens)

;; (use-package smartrep)

;; (use-package smex)

;; (use-package solarized-theme)

;; (use-package swiper)

;; (use-package test-simple)

;; (use-package tuareg)

;; (use-package undo-tree)

;; (use-package use-package)

;; (use-package volatile-highlights)

;; (use-package w3m)

;; (use-package wc-mode)

;; (use-package which-key)

;; (use-package with-editor)

;; (use-package ws-butler)

;; (use-package xterm-color)

;; (use-package yaml-mode)

;; (use-package yasnippet)

;; (use-package zenburn-theme)

;; (use-package zop-to-char)

;; (use-package zoutline)

;; Installed but apparently not on elpa/melpa
;; "ack-and-a-half"
;; "csv-nav"
;; "ido-completing-read+"
;; "ido-ubiquitous"
;; "image+"
;; "isar-mode"
;; "metal-mercury-mode"
;; "prelude-emacs-lisp"
;; "prelude-haskell"
;; "prelude-js"
;; "prelude-lisp"
;; "prelude-programming"
;; "scala-mode2"
;; "simp-isar-mode"
;; "so-long"
;; "sunrise-commander"
;; "sunrise-x-buttons"
;; "sunrise-x-loop"
;; "sunrise-x-modeline"

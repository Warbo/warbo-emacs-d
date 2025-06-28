;;; prelude-mode.el --- Emacs Prelude: minor mode
;;
;; Copyright © 2011-2016 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A minor mode defining a local keymap, plus a menu.

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
(require 'easymenu)

 (defvar prelude-mode-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "C-c r") 'crux-rename-buffer-and-file)
     map)
   "Keymap for Prelude mode.")

;; define minor mode
(define-minor-mode prelude-mode
  "Minor mode to consolidate Emacs Prelude extensions.

\\{prelude-mode-map}"
  :lighter " Pre"
  ;:keymap prelude-mode-map
)
(provide 'prelude-mode)
;;; prelude-mode.el ends here

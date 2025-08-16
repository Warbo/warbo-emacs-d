;;; prelude-custom.el --- Emacs Prelude: Prelude's customizable variables.
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

;; TODO: This is part of my config, so I can just set the values I want
;; directly; rather than making them customisable. Delete those which aren't
;; used, and push the others closer to their use-sites, to see if we can just
;; define them inline somewhere.

;; customize
(defgroup prelude nil
  "Emacs Prelude configuration."
  :prefix "prelude-"
  :group 'convenience)

(provide 'prelude-custom)

;;; prelude-custom.el ends here

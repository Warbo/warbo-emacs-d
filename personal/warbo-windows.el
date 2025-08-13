;;; warbo-windows --- Window layouts -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; Stored window configs
;;;
;;; Code:

(defun windows-tall ()
  "Splits Emacs frame to work well on a tall monitor."
  (interactive)
  ;; ┌─┐
  ;; │ │
  ;; └─┘
  (delete-other-windows)

  ;; ┌─┐
  ;; │ │
  ;; ├─┤
  ;; │ │
  ;; └─┘
  (split-window-below)

  ;; ┌─┬─┐
  ;; │ │ │
  ;; ├─┴─┤
  ;; │   │
  ;; └───┘
  (ignore-errors (windmove-up))
  (split-window-right)

  ;; ┌─┬─┐
  ;; │ │ │
  ;; ├─┤ │
  ;; │ │ │
  ;; ├─┴─┤
  ;; │   │
  ;; └───┘
  (ignore-errors (windmove-up))
  (ignore-errors (windmove-left))
  (split-window-below)

  ;; ┌─┬─┐
  ;; │ │ │
  ;; ├─┼─┤
  ;; │ │ │
  ;; ├─┴─┤
  ;; │   │
  ;; └───┘
  (ignore-errors (windmove-up))
  (ignore-errors (windmove-up))
  (ignore-errors (windmove-right))
  (ignore-errors (windmove-right))
  (ignore-errors (windmove-up))
  (ignore-errors (windmove-up))
  (ignore-errors (windmove-right))
  (ignore-errors (windmove-right))
  (split-window-below)

  (balance-windows))

(defun windows-wide ()
  "Splits Emacs frame to work well on a wide monitor."
  (interactive)
  ;; ┌─┐
  ;; │ │
  ;; └─┘
  (delete-other-windows)

  ;; ┌─┬─┐
  ;; │ │ │
  ;; └─┴─┘
  (split-window-right)

  ;; ┌─┬─┬─┐
  ;; │ │ │ │
  ;; └─┴─┴─┘
  (ignore-errors (windmove-right))
  (split-window-right)

  ;; ┌─┬─┬─┐
  ;; │ │ │ │
  ;; ├─┤ │ │
  ;; │ │ │ │
  ;; └─┴─┴─┘
  (ignore-errors (windmove-left))
  (ignore-errors (windmove-left))
  (split-window-below)

  ;; ┌─┬─┬─┐
  ;; │ │ │ │
  ;; ├─┼─┤ │
  ;; │ │ │ │
  ;; └─┴─┴─┘
  (ignore-errors (windmove-left))
  (ignore-errors (windmove-left))
  (ignore-errors (windmove-right))
  (split-window-below)

  ;; ┌─┬─┬─┐
  ;; │ │ │ │
  ;; ├─┼─┼─┤
  ;; │ │ │ │
  ;; └─┴─┴─┘
  (ignore-errors (windmove-right))
  (ignore-errors (windmove-right))
  (split-window-below)

  (balance-windows))

;; TODO: Keybinding which picks one of these, based on frame size

(provide 'warbo-windows)
;;; warbo-windows ends here

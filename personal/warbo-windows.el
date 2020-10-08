;;; warbo-windows -- Window layouts
;;;
;;; Commentary:
;;; Stored window configs
;;;
;;; Code:

;; TODO: Set sizes
(defun windows-tall ()
  (interactive)
  ;; +-+
  ;; | |
  ;; +-+
  (delete-other-windows)

  ;; +-+
  ;; | |
  ;; +-+
  ;; | |
  ;; +-+
  (split-window-below)

  ;; +-+-+
  ;; | | |
  ;; +-+-+
  ;; |   |
  ;; +---+
  (ignore-errors (windmove-up))
  (split-window-right)

  ;; +-+-+
  ;; | | |
  ;; +-+ |
  ;; | | |
  ;; +-+-+
  ;; |   |
  ;; +---+
  (ignore-errors (windmove-up))
  (ignore-errors (windmove-left))
  (split-window-below)

  ;; +-+-+
  ;; | | |
  ;; +-+-+
  ;; | | |
  ;; +-+-+
  ;; |   |
  ;; +---+
  (ignore-errors (windmove-up))
  (ignore-errors (windmove-up))
  (ignore-errors (windmove-right))
  (ignore-errors (windmove-right))
  (ignore-errors (windmove-up))
  (ignore-errors (windmove-up))
  (ignore-errors (windmove-right))
  (ignore-errors (windmove-right))
  (split-window-below)
  )

;; TODO: Balance the sizes
(defun windows-wide ()
  (interactive)
  ;; +-+
  ;; | |
  ;; +-+
  (delete-other-windows)

  ;; +-+-+
  ;; | | |
  ;; +-+-+
  (split-window-right)

  ;; +-+-+-+
  ;; | | | |
  ;; +-+-+-+
  (ignore-errors (windmove-right))
  (split-window-right)

  ;; +-+-+-+
  ;; | | | |
  ;; +-+ | |
  ;; | | | |
  ;; +-+-+-+
  (ignore-errors (windmove-left))
  (ignore-errors (windmove-left))
  (split-window-below)

  ;; +-+-+-+
  ;; | | | |
  ;; +-+-+ |
  ;; | | | |
  ;; +-+-+-+
  (ignore-errors (windmove-left))
  (ignore-errors (windmove-left))
  (ignore-errors (windmove-right))
  (split-window-below)

  ;; +-+-+-+
  ;; | | | |
  ;; +-+-+-+
  ;; | | | |
  ;; +-+-+-+
  (ignore-errors (windmove-right))
  (ignore-errors (windmove-right))
  (split-window-below)
  )

;; TODO: Keybinding which picks one of these, based on frame size

(provide 'warbo-windows)
;;; warbo-windows ends here

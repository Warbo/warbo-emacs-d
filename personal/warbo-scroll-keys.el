;;; warbo-scroll-keys.el --- Robust M-<up>/M-<down> scroll keybindings -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides a minor mode that ensures M-<up> and M-<down> are always bound to
;; scroll-up-line and scroll-down-line, even in modes like paredit and
;; smartparens that try to override these keys.
;;
;; The approach uses a minor mode with a keymap that has higher priority than
;; most other minor modes, ensuring our bindings take precedence.

;;; Code:

(defvar warbo-scroll-keys-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-<up>") #'scroll-up-line)
    (define-key map (kbd "M-<down>") #'scroll-down-line)
    map)
  "Keymap for `warbo-scroll-keys-mode'.")

;;;###autoload
(define-minor-mode warbo-scroll-keys-mode
  "Minor mode to ensure M-<up> and M-<down> scroll by one line.

This mode provides robust keybindings for M-<up> and M-<down> that
work even in modes like paredit and smartparens which try to override
these keys for their own purposes."
  :lighter " Scroll"
  :keymap warbo-scroll-keys-mode-map
  :global nil)

;;;###autoload
(define-globalized-minor-mode global-warbo-scroll-keys-mode
  warbo-scroll-keys-mode
  warbo-scroll-keys-mode-enable
  :group 'editing-basics)

(defun warbo-scroll-keys-mode-enable ()
  "Enable `warbo-scroll-keys-mode' in current buffer."
  (unless (minibufferp)
    (warbo-scroll-keys-mode 1)))

;; Also bind globally as a fallback
(global-set-key (kbd "M-<up>") #'scroll-up-line)
(global-set-key (kbd "M-<down>") #'scroll-down-line)

;; Add hooks for modes that are known to override these keys
(defun warbo-scroll-keys-setup-hooks ()
  "Set up hooks to ensure scroll keys work in all modes."

  ;; For paredit
  (with-eval-after-load 'paredit
    (define-key paredit-mode-map (kbd "M-<up>") nil)
    (define-key paredit-mode-map (kbd "M-<down>") nil))

  ;; For smartparens
  (with-eval-after-load 'smartparens
    (define-key smartparens-mode-map (kbd "M-<up>") nil)
    (define-key smartparens-mode-map (kbd "M-<down>") nil))

  ;; For org-mode, which might bind these
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "M-<up>") #'scroll-up-line)
    (define-key org-mode-map (kbd "M-<down>") #'scroll-down-line)))

;; Run the setup
(warbo-scroll-keys-setup-hooks)

;; Enable globally by default
;;;###autoload
(global-warbo-scroll-keys-mode 1)

(provide 'warbo-scroll-keys)
;;; warbo-scroll-keys.el ends here

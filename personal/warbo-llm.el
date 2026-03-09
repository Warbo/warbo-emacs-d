;;; warbo-llm --- Large Language Model support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Declare external pi-coding-agent functions to suppress byte-compile warnings
(declare-function pi-coding-agent "pi-coding-agent")
(declare-function pi-coding-agent-abort "pi-coding-agent")
(declare-function pi-coding-agent-send "pi-coding-agent")

;; Declare our own functions for forward references (all defined below in :init)
(declare-function warbo-guess-pi-buffer "warbo-llm")
(declare-function warbo-pi-look-at-point "warbo-llm")
(declare-function warbo-show-pi-chat "warbo-llm")
(declare-function warbo-pi-send-message "warbo-llm")

(defun warbo-ignore-pi-whitespace ()
  "Disable `whitespace-mode' in pi chat buffers."
  (whitespace-mode -1))

(use-package pi-coding-agent
  :ensure t
  :hook (pi-coding-agent-chat-mode . warbo-ignore-pi-whitespace)
  ;; Bind warbo-pi globally; pi-coding-agent mode maps will override this.
  :bind (("C-c C-p" . warbo-pi)
         ("C-c C-P" . warbo-pi-message)
         :map pi-coding-agent-chat-mode-map
         ("C-c C-k" . pi-coding-agent-abort))
  :init
  (defalias 'pi 'pi-coding-agent)

  (defun warbo-guess-pi-buffer (type)
    "Find or guess the pi-coding-agent buffer name for TYPE (:chat or :input).
First tries to find an existing buffer matching the current directory.
Falls back to generating a name the same way pi-coding-agent does."
    (let* ((session-dir (expand-file-name
                         (or (when-let ((proj (project-current)))
                               (project-root proj))
                             default-directory)))
           (pattern (format "^\\*pi-coding-agent-%s:" type)))
      ;; First, try to find an existing buffer with matching default-directory
      (or (cl-find-if
           (lambda (buf)
             (and (string-match-p pattern (buffer-name buf))
                  (with-current-buffer buf
                    (equal (expand-file-name default-directory)
                           session-dir))))
           (buffer-list))
          ;; Fall back to generating the name the same way pi does
          (get-buffer
           (format "*pi-coding-agent-%s:%s*"
                   type
                   (abbreviate-file-name session-dir))))))

  (defun warbo-show-pi-chat ()
    "Show pi chat buffer for current repo/dir, maintaining selected window.

If there's an existing pi chat buffer for the current repo/dir then make it
visible. If not, starts a pi session and makes its chat window visible. Makes
sure the pi chat window has enough space, but otherwise keeps the current buffer
and window selections as similar as possible."
    (interactive)
    (let* ((current-window (selected-window))
           (pi-buffer (or (warbo-guess-pi-buffer "chat")
                          (save-window-excursion
                            (pi-coding-agent)
                            (warbo-guess-pi-buffer "chat")))))
      (if pi-buffer
          (unless (get-buffer-window pi-buffer 'visible)
            (display-buffer pi-buffer
                            '(display-buffer-in-side-window
                              (side . bottom)
                              (slot . 0)
                              (window-height . 15)))
            ;; Ensure current buffer remains selected
            (select-window current-window))
        (error "Error: Could not find or create pi chat session"))))

  (defun warbo-pi-send-message (message)
    "Sends MESSAGE to the pi coding agent."
    (interactive "sMessage: ")
    (let* ((input-buffer (warbo-guess-pi-buffer "input")))
      (if input-buffer
          (with-current-buffer input-buffer
            (goto-char (point-max))
            (insert message)
            (pi-coding-agent-send))
        (error "No pi coding agent input buffer found"))))

  (defun warbo-pi-message ()
    "If pi chat isn't visible, show/start it. Otherwise send a message to pi."
    (interactive)
    (let ((chat-buffer (warbo-guess-pi-buffer "chat")))
      (if (and chat-buffer (get-buffer-window chat-buffer 'visible))
          (call-interactively 'warbo-pi-send-message)
        (warbo-show-pi-chat)))
    )

  (defun warbo-pi-look-at-point ()
    "Ask pi coding agent to take a look at current point."
    (interactive)
    (warbo-pi-send-message
     (format
      "Action the request/issue/problem at point in buffer '%s'"
      (buffer-name (current-buffer)))))

  (defun warbo-pi ()
    "If pi chat isn't visible, show/start it. Otherwise have pi look at point."
    (interactive)
    (let ((chat-buffer (warbo-guess-pi-buffer "chat")))
      (if (and chat-buffer (get-buffer-window chat-buffer 'visible))
          (warbo-pi-look-at-point)
        (warbo-show-pi-chat))))
  )

(provide 'warbo-llm)
;;; warbo-llm.el ends here

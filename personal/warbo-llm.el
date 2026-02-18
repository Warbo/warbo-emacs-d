;;; warbo-llm --- Large Language Model support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun warbo-ignore-pi-whitespace ()
  "Disable `whitespace-mode' in pi chat buffers."
  (whitespace-mode -1))

(use-package pi-coding-agent
  :ensure t
  :hook (pi-coding-agent-chat-mode . warbo-ignore-pi-whitespace)
  ;; Bind warbo-pi globally; pi-coding-agent mode maps will override this.
  :bind (("C-c C-p" . warbo-pi)
         :map pi-coding-agent-chat-mode-map
         ("C-c C-k" . pi-coding-agent-abort))
  :init
  (defalias 'pi 'pi-coding-agent)

  (defun warbo-guess-pi-buffer (type)
    (let* ((current-dir (or (vc-root-dir)
                            default-directory)))
      (format "*pi-coding-agent-%s:%s*"
              type
              (file-name-as-directory current-dir))))

  (defun warbo-show-pi-chat ()
    "Show pi chat buffer for current repo/dir, maintaining selected window.

If there's an existing pi chat buffer for the current repo/dir then make it
visible. If not, starts a pi session and makes its chat window visible. Makes
sure the pi chat window has enough space, but otherwise keeps the current buffer
and window selections as similar as possible."
    (interactive)
    (let* ((buffer-name (warbo-guess-pi-buffer "chat"))
           (current-window (selected-window))
           (pi-buffer (or (get-buffer buffer-name)
                          (save-window-excursion
                            (pi-coding-agent)
                            (get-buffer buffer-name)))))
      (if pi-buffer
          (unless (get-buffer-window pi-buffer 'visible)
            (display-buffer pi-buffer
                            '(display-buffer-in-side-window
                              (side . bottom)
                              (slot . 0)
                              (window-height . 15)))
            ;; Ensure current buffer remains selected
            (select-window current-window))
        (error "Error: Did not create pi session for '%s'" buffer-name))))

  (defun warbo-pi-look-at-point ()
    "Sends a message to the pi coding agent to take a look at current point."
    (interactive)
    (let* ((input-buffer-name (warbo-guess-pi-buffer "input"))
           (input-buffer (get-buffer input-buffer-name))
           (message (format "%s in buffer '%s' and %s"
                            "Please read around the current point"
                            (buffer-name (current-buffer))
                            "action the request/issue described there.")))
      (if input-buffer
          (with-current-buffer input-buffer
            (goto-char (point-max))
            (insert message)
            (pi-coding-agent-send))
        (error "No pi coding agent input buffer found for '%s'"
               input-buffer-name))))

  (defun warbo-pi ()
    "If pi chat isn't visible, show/start it. Otherwise have pi look at point."
    (interactive)
    (let ((chat-buffer (get-buffer (warbo-guess-pi-buffer "chat"))))
      (if (and chat-buffer (get-buffer-window chat-buffer 'visible))
          (warbo-pi-look-at-point)
        (warbo-show-pi-chat))))
  )

(provide 'warbo-llm)
;;; warbo-llm.el ends here

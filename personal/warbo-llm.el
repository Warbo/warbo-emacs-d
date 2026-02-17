;;; warbo-llm --- Large Language Model support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun warbo-ignore-pi-whitespace ()
  "Disable `whitespace-mode' in pi chat buffers."
  (whitespace-mode -1))

(use-package pi-coding-agent
  :ensure t
  :hook (pi-coding-agent-chat-mode . warbo-ignore-pi-whitespace)
  :init (defalias 'pi 'pi-coding-agent)
  :config
  (defun warbo-show-pi-chat ()
    "Show pi chat buffer for current repo/dir, maintaining selected window.

If there's an existing pi chat buffer for the current repo/dir then make it
visible. If not, starts a pi session and makes its chat window visible. Makes
sure the pi chat window has enough space, but otherwise keeps the current buffer
and window selections as similar as possible."
    (interactive)
    (let* ((current-dir (or (vc-root-dir)
                            default-directory))
           (buffer-name (format "*pi-coding-agent-chat:%s*"
                                (file-name-as-directory current-dir)))
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

  ;; Bind warbo-show-pi-chat globally; pi-coding-agent mode maps will override
  :bind ("C-c C-p" . warbo-show-pi-chat)
  )

(provide 'warbo-llm)
;;; warbo-llm.el ends here

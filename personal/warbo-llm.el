;;; warbo-llm --- Large Language Model support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun warbo-ignore-pi-whitespace ()
  "Disable `whitespace-mode' in pi chat buffers."
  (whitespace-mode -1))

(use-package pi-coding-agent
  :ensure t
  :hook (pi-coding-agent-chat-mode . warbo-ignore-pi-whitespace)
  :init (defalias 'pi 'pi-coding-agent))

(provide 'warbo-llm)
;;; warbo-llm.el ends here

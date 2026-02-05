;;; warbo-llm --- Large Language Model support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package vterm
  :ensure)

(use-package aidermacs
  :ensure
  :custom
  (aidermacs-show-diff-after-change t)
  (aidermacs-watch-files t)
  (aidermacs-backend 'vterm) ;; Required for watch-files
  (aidermacs-default-chat-mode 'architect)
  (aidermacs-default-model "anthopic/claude-opus-4-5")
  (aidermacs-weak-model "anthopic/claude-haiku-4-5")
  :config
  (global-set-key (kbd "C-c a") 'aidermacs-transient-menu))

(defun warbo-ignore-pi-whitespace ()
  "Disable `whitespace-mode' in pi chat buffers."
  (whitespace-mode -1))

(use-package pi-coding-agent
  :ensure t
  :hook (pi-coding-agent-chat-mode . warbo-ignore-pi-whitespace)
  :init (defalias 'pi 'pi-coding-agent))

(provide 'warbo-llm)
;;; warbo-llm.el ends here

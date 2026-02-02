;;; warbo-llm --- Large Language Model support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package aider
  :ensure
  :disabled
  :config
  (setq aider-program "aider")
  (setq aider-args
        (pcase machine-id
          ('wsl-ubuntu
           `("--model" ,(concat "ollama_chat/" warbo-local-llm)
             "--set-env" "OLLAMA_API_BASE=http://127.0.0.1:11434"
             ))
          (_
           '("--model" "anthropic/claude-sonnet-4-5"
             "--weak-model" "anthropic/claude-haiku-4-5"))))
  (global-set-key (kbd "C-c a") 'aider-transient-menu))

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

(provide 'warbo-llm)
;;; warbo-llm.el ends here

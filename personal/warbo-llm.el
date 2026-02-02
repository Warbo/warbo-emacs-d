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
  :config
  (global-set-key (kbd "C-c a") 'aidermacs-transient-menu)
  (setq aidermacs-show-diff-after-change t)
  (setq aidermacs-watch-files t)
  (setq aidermacs-backend 'vterm) ;; Required for watch-files
  (setq aidermacs-project-read-only-files '("CONVENTIONS.md" "GEMINI.md")))

(provide 'warbo-llm)
;;; warbo-llm.el ends here

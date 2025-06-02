;;; warbo-llm --- Large Language Model support
;;; Commentary:
;;; Code:

(use-package llm
  :ensure t)

(use-package ellama
  :ensure t
  :config
  (setopt ellama-language "English")
  (require 'llm-ollama)
  (let ((model (if (equal machine-id 'wsl-ubuntu) "qwen2.5-coder:14b" "qwen2.5-coder:1.5b")))
    (setopt ellama-provider
            (make-llm-ollama
             :chat-model model
             :embedding-model model))))

(use-package aider
  :ensure t
  :config
  (setq aider-args
        '("--model" "gemini/gemini-2.5-flash-preview-04-17"
          "--no-auto-lint"))

  ;; (setenv "GEMINI_API_KEY" <your-gemini-api-key>)
  ;; Optional: Set a key binding for the transient menu
  (global-set-key (kbd "C-c a") 'aider-transient-menu))

(provide 'warbo-llm)
;;; warbo-llm.el ends here

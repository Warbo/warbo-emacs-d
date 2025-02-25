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

(provide 'warbo-llm)
;;; warbo-llm.el ends here

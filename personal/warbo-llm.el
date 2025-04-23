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
  (let* ((base "qwen2.5-coder")
         (size (if (equal machine-id 'wsl-ubuntu) "14b" "1.5b"))
         (model (string-join (list base size) ":")))
    (setopt ellama-provider
            (make-llm-ollama
             :chat-model model
             :embedding-model model)))

(provide 'warbo-llm)
;;; warbo-llm.el ends here

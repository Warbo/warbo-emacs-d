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

  ;; Hint that we want in-depth analysis, not hand-holding
  (setopt ellama-user-nick "SeniorDev")

  ;; Useful to see what it's doing
  (setopt ellama-always-show-chain-steps t)

  ;; Alter prompts to try and reduce annoying behaviours
  (setopt ellama-code-review-prompt-template
          "You are a senior programmer. You need to review the given code. Identify the most important issues, if any; don't get hung up on minor quibbles with style/opinion. The code was written by a senior dev, who should know what they're doing; keep this in mind when looking for issues (might there be a good reason for what's been done?). Be concise, and to the point; further discussion can be had later."))

(provide 'warbo-llm)
;;; warbo-llm.el ends here

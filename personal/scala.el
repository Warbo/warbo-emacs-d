(use-package scala-mode
  :ensure t
  :mode "\\.s\\(cala\\|bt\\)$"
  :bind (("C-c C-c" . sbt-run-previous-command)))

(use-package sbt-mode
  :ensure t
  :pin melpa-stable
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  (setq sbt:default-command "test")
  (setq sbt:program-options
        '("-Dsbt.color=always"
          "-Dsbt.log.noformat=false"
          ;; See https://github.com/hvesalai/emacs-sbt-mode/issues/139
          "-Djline.terminal=auto"
          ;; See https://github.com/hvesalai/emacs-sbt-mode/issues/152
          "-Dsbt.supershell=false"
          )))

(use-package lsp-metals
  :quelpa (lsp-metals :fetcher github
                      :repo    "emacs-lsp/lsp-metals")
  :ensure t
  ;:defer  t
  )

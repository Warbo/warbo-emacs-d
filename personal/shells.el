;; Interpret and use ansi color codes in shell output windows. We use
;; https://github.com/atomontage/xterm-color rather than Emacs's built-in ansi
;; handling, e.g. (ansi-color-for-comint-mode-on), since that's SLOW
(use-package xterm-color
  :ensure t
  :custom
  (comint-output-filter-functions
   (remove 'ansi-color-process-output comint-output-filter-functions)
   "Remove built-in handling of ANSI colour codes")
  (comint-preoutput-filter-functions
   (cons 'xterm-color-filter
         (remove 'xterm-color-filter comint-preoutput-filter-functions))
   "Ensure xterm-color's preoutput handler is in place"))

(use-package shx
  :ensure t
  :custom
  (shx-leader "#" "Use '#ssh', '#view', etc. since : conflicts with REPLs")
  :config
  ;; One of the nice things about shell-mode is that we can edit the buffer
  ;; contents, e.g. to retroactively format previous output, or to build up new
  ;; commands in preparation for copy/pasting them to the prompt. Annoyingly,
  ;; shx-mode takes over the SPC key, for some magic which I don't actually
  ;; use, but nevertheless prevents inserting a space at the point. Hence we
  ;; remove this binding here, to allow typing spaces like normal shell-mode.
  (define-key shx-mode-map (kbd "SPC") nil)

  (define-advice shx-cmd-ssh
      (:around (f host) ssh-default-to-bash)
    "Assume remote hosts will be using /bin/bash, rather than wrappedShell"
    ;; TODO: Would be even better if we check whether the remote command exists,
    ;; before falling back to a default (similar to shx--shell-command, but we
    ;; need to use 'host' rather than 'default-directory'
    (let ((explicit-shell-file-name
           (cond ((string= "" host) explicit-shell-file-name)
                 (t "/bin/bash"))))
     (apply f (list host)))))

(defun extract-directory-from-prompt (s)
  "Like `comint-osc-process-output' but acts on the given string S."
  (let ((dir default-directory))
    (when
        (save-excursion
          (with-temp-buffer
            (insert s)
            (ansi-osc-apply-on-region (point-min) (point-max))
            (unless (equal dir default-directory)
              (setq dir default-directory)
              t)))
      (ignore-errors (cd-absolute dir))))
  s)

(defun warbo-shell-mode-hook ()
  "Sets up a shell-mode buffer nicely."
  ;; shx-mode is nice, but don't let it mess with cwd
  (unless shx-mode (shx-mode 1))
  (advice-remove #'find-file-at-point #'shx--with-shx-cwd)
  (advice-remove #'ffap-at-mouse #'shx--with-shx-cwd)

  ;; Use xterm-color to turn ANSI escape codes into Emacs text properties
  ;; (setq comint-preoutput-filter-functions
  ;;       (cons 'xterm-color-filter
  ;;             (remove 'xterm-color-filter
  ;;                     comint-preoutput-filter-functions)))

  ;; Look for an OSC7 escape sequence to keep track of the current directory.
  ;; This is invisible, machine readable and mostly standard. We must do this
  ;; before xterm-color-filter is applied, since that will strip out the ANSI.
  (setq comint-preoutput-filter-functions
        (cons 'extract-directory-from-prompt
              (remove 'extract-directory-from-prompt
                      comint-preoutput-filter-functions)))

  ;; Don't try to keep track of the shell's current directory by looking for
  ;; 'cd' commands: it's very limited and fragile (use OSC7 instead)
  (shell-dirtrack-mode -1)

  ;; Don't try to keep track of the shell's current directory by parsing the
  ;; prompt with a regular expression: it's very limited and fragile (use OSC7)
  (dirtrack-mode -1)

  ;; Disable font-locking to improve performance. We don't need
  ;; it since we're using xterm-color
  (font-lock-mode -1)
  ;; Prevent font-locking from being re-enabled in this buffer
  (make-local-variable 'font-lock-function)
  (setq font-lock-function (lambda (_) nil))

  ;; Wrap at edge of the screen, not at last whitespace
  (visual-line-mode -1)

  ;;(company-mode 1)

  ;; Avoid overriding prompt colours
  ;; https://stackoverflow.com/a/50776528/884682
  (face-remap-set-base 'comint-highlight-prompt :inherit nil))

(use-package shell
  :hook
  (shell-mode . warbo-shell-mode-hook)
  ;; :bind
  ;; (:map shell-mode-map
  ;;       ;; Use Company's drop-down completions, rather than a separate window
  ;;       ("TAB" . 'company-manual-begin))
  )

(require 'esh-mode)
(use-package eshell
  :commands eshell
  :custom
  (eshell-cmpl-cycle-completions nil "Stop auto-complete at first ambiguity")
  ;; Use ansi-term for these commands
  (eshell-visual-commands
        (append '("mutt"
                  "vim"
                  "screen"
                  "lftp"
                  "ipython"
                  "telnet"
                  "ssh"
                  "mysql")
                (and  (boundp 'eshell-visual-commands)
                      eshell-visual-commands)))
  :bind
  (:map eshell-mode-map
        ;; Swap cursor keys and C-p/C-n in EShell.
        ;; C-up/C-down still does history like Shell mode
        ("C-p"    . eshell-previous-matching-input-from-input)
        ("C-n"    . eshell-next-matching-input-from-input)
        ("<up>"   . previous-line)
        ("<down>" . next-line)))

;; Uses Bash's real tab-completion, rather than an ELisp approximation
;; (use-package bash-completion
;;   :ensure t
;;   :config
;;   (bash-completion-setup))

(defun make-numbered-name (prefix n)
  "For string PREFIX and number N, combine into '*PREFIX-N*'."
  (concat "*" prefix "-" (number-to-string n) "*"))

(defun free-name-num (prefix)
  "For string PREFIX, return an unused buffer name of the form '*PREFIX-1*'."
  (let* ((n     1)
         (name  (make-numbered-name prefix n))
         (taken (mapcar 'buffer-name (buffer-list))))
    (while (member name taken)
      (setq n    (1+ n))
      (setq name (make-numbered-name prefix n)))
    name))

;; Auto-increment shell names. Get a new EShell with "M-x sh", get a new Shell
;; with "M-x bash"
(defun sh ()
  "Start a new EShell."
  (interactive)
  (let ((buf (free-name-num "eshell")))
    (command-execute 'eshell)
    (rename-buffer buf)
    buf))

(defun bash ()
  "Start a shell-mode shell."
  (interactive)
  (let ((buf (free-name-num "shell")))
    (shell buf)
    ;; (with-current-buffer buf
    ;;   ;; Stops shell-mode echoing our input, since the shell already does
    ;;   ;; NOTE: Commented-out, since only seems to be needed when wrapping
    ;;   ;; bash in fold (via expect)!
    ;;   (setq comint-process-echoes t))
    buf))

(defun bash-unwrapped ()
  "Start a shell-mode shell, forcing 'bash' as the shell."
  (interactive)
  (let ((explicit-shell-file-name "bash"))
    (bash)))

;; "Refresh" an SSH shell after a connection dies
(defun refresh-terminal ()
  "Start a new shell, like the current."
  (interactive)
  (let ((buf-name (buffer-name)))
    (progn (command-execute 'bash)
           (kill-buffer   buf-name)
           (rename-buffer buf-name))))

(defun refresh-terminal-unbuffered ()
  "Start a new shell, like the current.  Avoids buffering the bash shell."
  (interactive)
  (let ((buf-name                 (buffer-name))
        (explicit-shell-file-name "bash"))
    (progn (command-execute 'bash)
           (kill-buffer   buf-name)
           (rename-buffer buf-name))))

(defun eshell/emacs (file)
  "Replace Emacs command in eshell, so FILE is opened in this instance."
  (find-file file))

(defun eshell-in (dir)
  "Launch a new eshell in directory DIR."
  (let ((buffer (sh)))
    (with-current-buffer buffer
      (eshell/cd dir)
      (eshell-send-input))
    buffer))

(defun eshell-named-in (namedir)
  "Launch eshell with buffer name and working directory taken from NAMEDIR."
  (let* ((name   (car namedir))
         (dir    (eval (cadr namedir))))
    (unless (get-buffer name)
      (with-current-buffer (eshell-in dir)
        (rename-buffer name)))
    name))

(defun shell-in (dir)
  "Launch a new shell in directory DIR."
  (let ((default-directory (if (equal "/" (substring dir -1))
                               dir
                             (concat dir "/"))))
    (bash)))

(defun shell-named-in (namedir)
  "Launch shell with buffer name and working directory taken from NAMEDIR."
  (let* ((name (car namedir))
         (dir  (eval (cadr namedir))))
    (unless (get-buffer name)
      (with-current-buffer (shell-in dir)
        (rename-buffer name)))
    (get-buffer name)))

(defun shell-from-buf (buf)
  "Switch to buffer BUF then open a shell.  Useful for piggybacking on TRAMP."
  (with-current-buffer buf
    (bash)))

(defun shell-with-name-from-buf (namebuf)
  "Open a shell in a buffer and rename, with buffer and name given by NAMEBUF."
  (let* ((name (car namebuf))
         (buf  (eval (cadr namebuf))))
    (unless (get-buffer name)
      (with-current-buffer (shell-from-buf buf)
        (rename-buffer name)))
    name))

(defconst startup-shells
  '(("home" "~")))

(thinkpad-only
 (defconst startup-shells
   '(("antiunification"      "~/Programming/Haskell/EquationalAntiUnification")
     ("benchmark-paper"      "~/Writing/TEBenchmarkPaper")
     ("blog"                 "~/blog")
     ("bucketing"            "~/Programming/bucketing-algorithms")
     ("bucketing-paper"      "~/Writing/Bucketing")
     ("deleteme"             "~/DELETEME")
     ("dotfiles"             "~/.dotfiles")
     ("ghc-base-asts"        "~/Programming/ghc-base-asts")
     ("haskell-te"           "~/Programming/haskell-te")
     ("home"                 "~")
     ("isaplanner-tip"       "~/Programming/Isabelle/IsaPlannerTip")
     ("laminar"              "~/Programming/Laminar")
     ("ML4HSFE"              "~/Programming/Haskell/ML4HSFE")
     ("music-scripts"        "~/Programming/music-scripts")
     ("nix-config"           "~/Programming/Nix/nix-config")
     ("nix-helpers"          "~/Programming/Nix/nix-helpers")
     ("nixpkgs"              "~/System/Programs/nixpkgs-2")
     ("quickspec-profiling"  "~/Programming/NotMine/quickspec2/quickspec")
     ("quickspeccer"         "~/Programming/Haskell/QuickSpeccer")
     ("te-benchmark"         "~/Programming/TheoryExplorationBenchmark")
     ("tests"                "~/System/Tests")
     ("thesis-shell"         "~/Writing/PhDThesis")
     ("warbo-packages"       "~/Programming/Nix/warbo-packages")
     ("warbo-utilities"      "~/warbo-utilities")
     ("writing"              "~/Writing"))
   "Useful buffers to open at startup."))
(manjaro-only
 (defconst startup-shells
   '(("blog"            "~/repos/chriswarbo-net")
     ("deleteme"        "~/DeleteMe")
     ("dotfiles"        "~/repos/warbo-dotfiles")
     ("home"            "~")
     ("nix-config"      "~/repos/nix-config")
     ("nix-helpers"     "~/repos/nix-helpers")
     ("repos"           "~/repos")
     ("warbo-packages"  "~/repos/warbo-packages")
     ("warbo-utilities" "~/repos/warbo-utilities")
     )
   "Useful buffers to open at startup"))

;; Avoid complaints from 'less' about terminal not being fully functional
(setq process-environment (cons "PAGER=cat" process-environment))

(defun open-startup-shells ()
  "Open a new shell for each entry in startup-shells."
  (interactive)
  (mapc 'shell-named-in startup-shells))

;; TODO: Start each buffer as empty, but with a local function that starts the
;; shell if switched-to (window-buffer-change-functions could do this)

(open-startup-shells)

(defun command-in-buffer (buf-dir-cmd)
  "Poor man's comint: BUF-DIR-CMD lists what to run where (e.g. a REPL)."
  (let* ((name (nth 0 buf-dir-cmd))
         (dir  (nth 1 buf-dir-cmd))
         (cmd  (nth 2 buf-dir-cmd))
         (buf  (get-buffer name)))
    (unless buf
      (with-current-buffer (shell-named-in (list name dir))
        (goto-char (point-max))
        (insert cmd)
        (comint-send-input)))
    (get-buffer name)))

(defconst startup-programs
  '()
  "Shell commands to run in particular buffers at startup.")

(mapc 'command-in-buffer startup-programs)

;; From http://stackoverflow.com/a/27908343/884682
(defun eshell/clear ()
  "Clear terminal."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-hook 'eshell-mode-hook
          '(lambda()
             (local-set-key (kbd "C-l") 'eshell/clear)))

(defadvice shell-command
    (after shell-in-new-buffer (command &optional output-buffer error-buffer))
  "From https://stackoverflow.com/a/6895517/884682 ."
  (when (get-buffer "*Async Shell Command*")
    (with-current-buffer "*Async Shell Command*"
      (rename-uniquely))))
(ad-activate 'shell-command)

;; From http://www.enigmacurry.com/2008/12/26/emacs-ansi-term-tricks/
(defer (lambda ()
         (require 'term)
         (defun visit-ansi-term ()
           "If the current buffer is:
     1) a running ansi-term named *ansi-term*, rename it.
     2) a stopped ansi-term, kill it and create a new one.
     3) a non ansi-term, go to an already running ansi-term
        or start a new one while killing a defunt one"
           (interactive)
           (let ((is-term (string= "term-mode" major-mode))
                 (is-running (term-check-proc (buffer-name)))
                 (term-cmd "bash")
                 (anon-term (get-buffer "*ansi-term*")))
             (if is-term
                 (if is-running
                     (if (string= "*ansi-term*" (buffer-name))
                         (call-interactively 'rename-buffer)
                       (if anon-term
                           (switch-to-buffer "*ansi-term*")
                         (ansi-term term-cmd)))
                   (kill-buffer (buffer-name))
                   (ansi-term term-cmd))
               (if anon-term
                   (if (term-check-proc "*ansi-term*")
                       (switch-to-buffer "*ansi-term*")
                     (kill-buffer "*ansi-term*")
                     (ansi-term term-cmd))
                 (ansi-term term-cmd)))))
         (global-set-key (kbd "<f2>") 'visit-ansi-term)))

(custom-set-variables
 '(comint-scroll-to-bottom-on-input nil)  ; allow inserting anywhere
 '(comint-scroll-to-bottom-on-output nil) ; allow browsing while output arrives
 '(comint-scroll-show-maximum-output t)   ; scroll to show max possible output
 '(comint-completion-autolist t)          ; show completion list when ambiguous
 '(comint-input-ignoredups t)             ; no duplicates in command history
 '(comint-completion-addsuffix t)         ; insert space/slash after file completion
 )

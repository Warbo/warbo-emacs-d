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
  (shx-leader "#" "Use '#ssh', '#view', etc. since : conflicts with REPLs"))

(use-package shell
  :hook
  (shell-mode . (lambda ()
                  (unless shx-mode (shx-mode 1))

                  ;; Wrap at edge of the screen, not at last whitespace
                  (visual-line-mode -1)

                  ;; Avoid overriding prompt colours
                  ;; https://stackoverflow.com/a/50776528/884682
                  (face-remap-set-base 'comint-highlight-prompt :inherit nil))))

(defvar eshell-mode-map)
(defvar eshell-visual-commands)
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
                eshell-visual-commands))
  :bind
  (:map eshell-mode-map
        ;; Swap cursor keys and C-p/C-n in EShell.
        ;; C-up/C-down still does history like Shell mode
        ("C-p"    . eshell-previous-matching-input-from-input)
        ("C-n"    . eshell-next-matching-input-from-input)
        ("<up>"   . previous-line)
        ("<down>" . next-line)))

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
    (with-current-buffer buf
      ;; Stops shell-mode echoing our input, since the shell already does
      (setq comint-process-echoes t))
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
(mac-only
 (defconst startup-shells
   '(("home"     "~")
     ("deleteme" "~/DeleteMe")
     ("repos"    "~/repos"))
   "Useful buffers to open at startup"))

(defun open-startup-shells ()
  (interactive)
  (mapc 'shell-named-in startup-shells)
  ;; Open each entry in ~/repos in a new shell on Mac. We do this here rather than
  ;; in mac.el to make sure shell-named-in is available.
  (mac-only
   (mapc (lambda (d)
           (unless (s-prefix? "." d)
             (shell-named-in `(,d ,(format "%s/repos/%s" (getenv "HOME") d)))))
         (directory-files "~/repos"))))

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

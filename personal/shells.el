;; Swap cursor keys and C-p/C-n in EShell.
;; C-up/C-down still does history like Shell mode
(defun m-eshell-hook ()
  ;; define control p, control n and the up/down arrow
  (define-key eshell-mode-map [(control p)]
    'eshell-previous-matching-input-from-input)
  (define-key eshell-mode-map [(control n)]
    'eshell-next-matching-input-from-input)

  (define-key eshell-mode-map [up] 'previous-line)
  (define-key eshell-mode-map [down] 'next-line))

(add-hook 'eshell-mode-hook 'm-eshell-hook)

;; Shells should wrap at the edge of the screen, not at the last whitespace
(add-hook 'shell-mode-hook
          (lambda ()
            (visual-line-mode -1)))

;; Shells can easily cause Emacs to hang with large outputs; make sure lines are
;; split at regular intervals to minimise this

(defun split-lines-at (n str)
  (let ((result     (seq-take str n))
        (remaining  (seq-drop str n)))
    (while (> (length remaining) 0)
      (setq result    (concat result "\n" (seq-take remaining n)))
      (setq remaining (seq-drop remaining n)))
    result))

(add-hook 'shell-mode-hook
          (lambda ()
            (add-hook 'comint-preoutput-filter-functions
                      (lambda (string)
                        (split-lines-at 1000 string))
                      nil
                      t)))

;; Auto-complete should stop at the first ambiguity
(setq eshell-cmpl-cycle-completions nil)

;; Use ansi-term for these commands
(add-hook
 'eshell-first-time-mode-hook
 (lambda ()
   (setq eshell-visual-commands
         (append '("mutt"
                   "vim"
                   "screen"
                   "lftp"
                   "ipython"
                   "telnet"
                   "ssh"
                   "mysql")
                 eshell-visual-commands))))

(defun make-numbered-name (prefix n)
  "Make strings of the form '*PREFIX-N*'"
  (concat "*" prefix "-" (number-to-string n) "*"))

(defun free-name-num (prefix)
  "Return an unused buffer name, of the form '*PREFIX-1*'"
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
  "Start a new EShell"
  (interactive)
  (let ((buf (free-name-num "eshell")))
    (command-execute 'eshell)
    (rename-buffer buf)
    buf))

(defun bash ()
  "Start a bash shell"
  (interactive)
  (let ((explicit-shell-file-name "bash")
        (buf                      (free-name-num "shell")))
    (shell buf)
    buf))

;; "Refresh" an SSH shell after a connection dies
(defun refresh-terminal ()
  "Start a new shell, like the current"
  (interactive)
  (let ((buf-name (buffer-name)))
    (progn (command-execute 'bash)
           (kill-buffer   buf-name)
           (rename-buffer buf-name))))

(defun eshell/emacs (file)
  "Running 'emacs' in eshell should open a new buffer"
  (find-file file))

(defun eshell-in (dir)
  "Launch a new eshell in the given directory"
  (let ((buffer (sh)))
    (with-current-buffer buffer
      (eshell/cd dir)
      (eshell-send-input))
    buffer))

(defun eshell-named-in (namedir)
  "Launch a new eshell with the given buffer name in the given directory"
  (let* ((name   (car namedir))
         (dir    (eval (cadr namedir))))
    (unless (get-buffer name)
      (with-current-buffer (eshell-in dir)
        (rename-buffer name)))
    name))

(defun shell-in (dir)
  "Launch a new shell in the given directory"
  (let ((default-directory (if (equal "/" (substring dir -1))
                               dir
                             (concat dir "/"))))
    (bash)))

(defun shell-named-in (namedir)
  "Launch a new shell with the given buffer name in the given directory"
  (let* ((name (car namedir))
         (dir  (eval (cadr namedir))))
    (unless (get-buffer name)
      (with-current-buffer (shell-in dir)
        (rename-buffer name)))
    (get-buffer name)))

(defun shell-from-buf (buf)
  "Switch to the given buffer then open a shell.
   Useful for piggybacking on TRAMP."
  (with-current-buffer buf
    (bash)))

(defun shell-with-name-from-buf (namebuf)
  "Switch to a given buffer, open a shell and rename it."
  (let* ((name (car namebuf))
         (buf  (eval (cadr namebuf))))
    (unless (get-buffer name)
      (with-current-buffer (shell-from-buf buf)
        (rename-buffer name)))
    name))

(defconst startup-shells
  '((".nixpkgs"             "~/.nixpkgs")
    ("benchmark-paper"      "~/Writing/benchmark2016")
    ("blog"                 "~/blog")
    ("deleteme"             "~/DELETEME")
    ("dotfiles"             "~/.dotfiles")
    ("haskell-te"           "~/Programming/haskell-te")
    ("home"                 "~")
    ("isaplanner-tip"       "~/Programming/Isabelle/IsaPlannerTip")
    ("nixpkgs"              "~/System/Programs/nixpkgs-2")
    ("te-benchmark"         "~/Programming/TheoryExplorationBenchmark")
    ("tests"                "~/System/Tests")
    ("utilities"            "~/warbo-utilities")
    ("writing"              "~/Writing"))
  "Useful buffers to open at startup")

(mapcar 'shell-named-in startup-shells)

(defun command-in-buffer (buf-dir-cmd)
  "Poor man's comint. Start a shell in a dir, and run a command (e.g. a REPL)"
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
  "Shell commands to run in particular buffers at startup")

(mapcar 'command-in-buffer startup-programs)

;; From http://stackoverflow.com/a/27908343/884682
(defun eshell/clear ()
  "Clear terminal"
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

; interpret and use ansi color codes in shell output windows
(ansi-color-for-comint-mode-on)

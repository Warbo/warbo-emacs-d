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
    name))

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
  '(("writing"       "~/Writing")
    ("blog"          "~/blog")
    ("ml4hs"         "~/Programming/ML4HS")
    ("hs2ast"        "~/Programming/Haskell/HS2AST")
    ("mlspec"        "~/Programming/Haskell/MLSpec")
    ("tree-features" "~/Programming/Haskell/TreeFeatures")
    ("haskell-te"    "~/System/Packages/haskell-te")
    ("astplugin"     "~/Programming/Haskell/AstPlugin")
    ("quickspec"     "~/Programming/Haskell/quickspec")
    ("qs-measure"    "~/Programming/Haskell/QuickSpecMeasure")
    ("utilities"     "~/warbo-utilities")
    ("deleteme"      "~/DELETEME")
    ("matrices"      "~/Programming/Haskell/Matrices")
    ("tests"         "~/System/Tests")
    ("nixpkgs"       "~/Programming/nixpkgs")
    ("repos"         "~/Programming/repos")
    (".nixpkgs"      "~/.nixpkgs")
    ("documents"     "~/Documents")
    ("home"          "~"))
  "Useful buffers to open at startup")

(mapcar 'shell-named-in startup-shells)

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

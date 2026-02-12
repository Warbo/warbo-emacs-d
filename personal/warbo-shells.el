;;; warbo-shells --- Customisations for shell-mode, eshell and ansi-term -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(declare-function eshell/cd "em-dirs")
(declare-function shx-cmd-ssh "shx")
(declare-function shx-cmd-ssh@ssh-default-to-bash "warbo-shells")

;; Interpret and use ansi color codes in shell output windows. We use
;; https://github.com/atomontage/xterm-color rather than Emacs's built-in ansi
;; handling, e.g. (ansi-color-for-comint-mode-on), since that's SLOW
(use-package xterm-color
  :ensure t
  :config
  ;; Switch comint's output filters from ansi-color to xterm-color.
  ;; Ensure xterm-color's preoutput handler is in place and remove 't'
  (setq comint-preoutput-filter-functions
        (cons 'xterm-color-filter
              (remove 'xterm-color-filter comint-preoutput-filter-functions)))
  ;; Remove built-in handling of ANSI colour codes and remove 't'
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))

  ;; 2025-09-16: Remove any occurrences of 't', which seem to be appearing
  ;; sporadically for some unknown reason. Would be nice to fix the cause,
  ;; rather than (attempting to fix) the symptom.
  (setq comint-preoutput-filter-functions
        (remove t comint-preoutput-filter-functions))
  (setq comint-output-filter-functions
        (remove t comint-output-filter-functions))
  (setq comint-input-filter-functions
        (remove t comint-input-filter-functions))

  ;; Synchronize standard ansi-color faces with xterm-color's palette and bold
  ;; setting. This ensures e.g. MisTTY looks the same as shell-mode.
  (let ((ansi-color-names
         '("black" "red" "green" "yellow" "blue" "magenta" "cyan" "white"))
        (bright-weight (if xterm-color-use-bold-for-bright 'bold 'normal)))
    (dotimes (i (length ansi-color-names))
      (let* ((name (nth i ansi-color-names))
             (std-face (intern (concat "ansi-color-" name)))
             (bright-face (intern (concat "ansi-color-bright-" name)))
             (std-color (aref xterm-color-names i))
             (bright-color (aref xterm-color-names-bright i)))

        ;; Set standard color face attributes
        (set-face-attribute std-face nil
                            :foreground std-color
                            :weight 'normal) ; Standard colors are never bold

        ;; Set bright color face attributes
        (set-face-attribute bright-face nil
                            :foreground bright-color
                            :weight bright-weight)))))

(defvar possible-shell-binaries
  '("/run/current-system/sw/bin/bash"
    "~/.nix-profile/bin/bash"
    "/usr/bin/bash"
    "/bin/bash"
    "/bin/sh")
  "A list of paths where we might find a shell binary, in order of preference.")

(defun warbo-mistty-context ()
  "Return (HOST . REPO-OR-DIR) for current buffer's context.
HOST is nil for local, REPO-OR-DIR is git repo name or directory name."
  (let* ((remote (file-remote-p default-directory))
         (host (when remote
                 (tramp-file-name-host (tramp-dissect-file-name default-directory))))
         (local-dir (expand-file-name
                     (or (file-remote-p default-directory 'localname)
                         default-directory)))
         (git-toplevel
          (ignore-errors
            (let ((default-directory local-dir))
              (with-temp-buffer
                (when (zerop (call-process "git" nil t nil
                                           "rev-parse" "--show-toplevel"))
                  (string-trim (buffer-string)))))))
         (repo-or-dir (if (and git-toplevel (not (string-empty-p git-toplevel)))
                          (file-name-nondirectory git-toplevel)
                        (file-name-nondirectory (directory-file-name local-dir)))))
    (cons host repo-or-dir)))

(defun warbo-mistty-buffer-name (host repo-or-dir &optional num)
  "Construct mistty buffer name from HOST, REPO-OR-DIR, and optional NUM.
Format: repo@host.mistty<N> or repo.mistty<N> for local."
  (let ((base (if host
                  (format "%s@%s.mistty" repo-or-dir host)
                (format "%s.mistty" repo-or-dir))))
    (if num
        (format "%s<%d>" base num)
      base)))

(defun warbo-mistty-find-buffers (host repo-or-dir)
  "Find all mistty buffers matching HOST and REPO-OR-DIR."
  (let ((pattern (regexp-quote (if host
                                   (format "%s@%s.mistty" repo-or-dir host)
                                 (format "%s.mistty" repo-or-dir)))))
    (cl-remove-if-not
     (lambda (buf)
       (and (string-match-p pattern (buffer-name buf))
            (with-current-buffer buf (derived-mode-p 'mistty-mode))))
     (buffer-list))))

(defun warbo-mistty-next-number (host repo-or-dir)
  "Find next available number for mistty buffer with HOST and REPO-OR-DIR."
  (let* ((bufs (warbo-mistty-find-buffers host repo-or-dir))
         (nums (mapcar (lambda (buf)
                         (let ((name (buffer-name buf)))
                           (if (string-match "<\\([0-9]+\\)>$" name)
                               (string-to-number (match-string 1 name))
                             1)))
                       bufs)))
    (if nums (1+ (apply #'max nums)) 2)))

(defun warbo-mistty-switch-or-create ()
  "Switch to or create a mistty buffer based on current context.
If in a mistty buffer, create another with same context but new number."
  (interactive)
  (let* ((ctx (warbo-mistty-context))
         (host (car ctx))
         (repo-or-dir (cdr ctx))
         (existing (warbo-mistty-find-buffers host repo-or-dir))
         (in-matching-mistty (and (derived-mode-p 'mistty-mode)
                                  (member (current-buffer) existing))))
    (cond
     ;; Already in a matching mistty buffer: create new one with next number
     (in-matching-mistty
      (let* ((num (warbo-mistty-next-number host repo-or-dir))
             (name (warbo-mistty-buffer-name host repo-or-dir num)))
        (mistty-create nil nil)
        (rename-buffer name t)))
     ;; Multiple existing buffers: choose interactively
     ((> (length existing) 1)
      (let ((chosen (completing-read "Switch to mistty: "
                                     (mapcar #'buffer-name existing)
                                     nil t)))
        (switch-to-buffer chosen)))
     ;; One existing buffer: switch to it
     ((= (length existing) 1)
      (switch-to-buffer (car existing)))
     ;; No existing buffers: create one
     (t
      (let ((name (warbo-mistty-buffer-name host repo-or-dir nil)))
        (mistty-create nil nil)
        (rename-buffer name t))))))

(global-set-key (kbd "<f2>") #'warbo-mistty-switch-or-create)

(use-package mistty
  :ensure t
  :init
  (defun warbo-mistty-no-whitespace-mode ()
    (whitespace-mode -1))
  :bind ((:map mistty-prompt-map
               ("C-<up>"   . mistty-send-C-p)
               ("C-<down>" . mistty-send-C-n)
               ("C-a"      . mistty-beginning-of-line))
         (:map mistty-mode-map
               ("C-a"      . smart-line-beginning)))
  :hook ((mistty . warbo-mistty-no-whitespace-mode)))

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

  (require 'cl-lib)
  (define-advice shx-cmd-ssh
      (:around (f host) ssh-default-to-bash)
    "Look for a Bash on the HOST we're connecting to (to avoid prompting)."
    (let ((host-prefix (if (string= "" host) host (concat "/ssh:" host ":"))))
      (let ((found (cl-some (lambda (path)
                              (and (file-exists-p (concat host-prefix path))
                                   path))
                            ;; Check if any of these exist, when host-prefix is
                            ;; prepended. If so, the first one is returned.
                            possible-shell-binaries)))
        (if found
            (let ((explicit-shell-file-name found))
              (apply f (list host)))
          (apply f (list host)))))))

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

(require 'tramp)
(defvar warbo-shell-want-lowercase-system-name nil
  "Setting this to t will cause (system-name) to lowercase its result.")

(define-advice system-name
    (:around (orig-fun &rest args) system-name-maybe-lowercased)
  "Maybe lowercases result of function `system-name' (ORIG-FUN with ARGS)."
  (if warbo-shell-want-lowercase-system-name
      (downcase (apply orig-fun args))
    (apply orig-fun args)))

(define-advice ansi-osc-directory-tracker
    (:around (orig-fun &rest args) track-osc-over-tramp)
  "Advise `ansi-osc-directory-tracker' (ORIG-FUN, w/ ARGS) to work over TRAMP."
  ;; The second element of ARGS is the text extracted from the OSC7 PS1 prompt.
  ;; It should have the form "file://host/path", e.g. "file://nixos/home/chris".
  ;; Split up such URLs, the same as `ansi-osc-directory-tracker' does, and act
  ;; according to the host part...
  (let* ((osc-string (cadr args))
         (parsed-uri (url-generic-parse-url osc-string))
         (host (url-host parsed-uri))
         (path (url-filename parsed-uri))
         (warbo-shell-want-lowercase-system-name t))
    (if (or (null host)
            (string= (downcase host) (system-name)))
        ;; If this is a local directory, call the original function
        (apply orig-fun args)
      ;; If our buffer is already using TRAMP, we'll update the path part of the
      ;; `default-directory', but assume the host/method/etc. remain unchanged.
      ;; This is a pretty safe bet, since Emacs will reliably update the latter
      ;; as needed; whilst the hostname in the PS1 prompt cannot be relied upon
      ;; (e.g. we may be using its IP instead; or it may be a missing a `.local'
      ;; domain; or we may be using multi-hop, or `sudo', etc.).
      (when (file-remote-p default-directory)
        (let ((vec (tramp-dissect-file-name default-directory)))
          (setf (tramp-file-name-localname vec) path)
          (setq default-directory (tramp-make-tramp-file-name vec)))))))

(require 'shx)
(defun warbo-shell-mode-hook ()
  "Set up a shell-mode buffer nicely."
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

  ;; Avoid overriding prompt colours
  ;; https://stackoverflow.com/a/50776528/884682
  (face-remap-set-base 'comint-highlight-prompt :inherit nil))

(use-package shell
  :hook
  (shell-mode . warbo-shell-mode-hook))

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
    buf))

(defun refresh-terminal ()
  "Restart this shell's process, even if remote."
  (interactive)
  (let ((host-prefix (or (file-remote-p (or (buffer-file-name)
                                            default-directory))
                         "")))
    (let ((found (cl-some (lambda (path)
                            (and (file-exists-p (concat host-prefix path))
                                 path))
                          ;; Check if any of these exist, when host-prefix is
                          ;; prepended. If so, the first one is returned.
                          possible-shell-binaries)))
      (if found
          (let ((explicit-shell-file-name found))
            (shx--restart-shell))
        (shx--restart-shell)))))

(defun find-start-of-env-vars (output)
  "Drop lines from start of OUTPUT which don't contain an '=' character."
  (let* ((has-trailing-newline (and (> (length output) 0)
                                    (char-equal (aref output (1- (length output))) ?\n)))
         (lines (split-string output "\n"))
         (idx 0)
         (n (length lines)))
    (while (and (< idx n)
                (not (string-match-p "=" (nth idx lines))))
      (setq idx (1+ idx)))
    (if (>= idx n)
        ""
      (let ((out (mapconcat #'identity (nthcdr idx lines) "\n")))
        (if has-trailing-newline (concat out "\n") out)))))

(defun refresh-emacs-env-vars-from-shell ()
  "Run a shell and copy its env vars into the Emacs environment."
  (interactive)
  (message "Refreshing environment variables...")
  ;; Use 'env -i HOME=$HOME' to make a fresh environment, with only HOME set.
  ;; Everything else will be built up from scratch, via the login shell.
  (let* ((shell-command (concat "env -i HOME=" (getenv "HOME") " "
                                (or ;;(executable-find "bash")
                                    (getenv "SHELL")
                                    "/bin/sh")
                                " -l -c 'printenv -0'"))
         (output (find-start-of-env-vars
                  (shell-command-to-string shell-command)))
         ;; Split by null byte to handle values with newlines safely
         (env-lines (split-string output "\0" t)))
    (dolist (line env-lines)
      (when (string-match "^\\([^=]+\\)=\\(.*\\)" line)
        (let ((var (match-string 1 line))
              (val (match-string 2 line)))
          ;; Skip strict Emacs-specific vars to avoid confusing Emacs
          (unless (member var '("_" "PWD" "SHLVL" "TERM"))
            (setenv var val)))))
    ;; Crucial: Sync exec-path with the new PATH env var
    (setq exec-path (split-string (getenv "PATH") path-separator)))
  (message "Environment variables refreshed."))

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

(defconst warbo-shells-sources
  (if (file-directory-p "~/src")
      (cl-remove-if (lambda (d) (or (s-starts-with-p "." d) (s-starts-with-p "y" d)))
                    (directory-files "~/src")))
  "Source repos in ~/src.")

(require 's)
(defconst startup-shells
  (pcase machine-id
    ('thinkpad
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
       ("writing"              "~/Writing")))

    ('manjaro
     '(("blog"            "~/repos/chriswarbo-net")
       ("deleteme"        "~/DeleteMe")
       ("dotfiles"        "~/repos/warbo-dotfiles")
       ("home"            "~")
       ("nix-config"      "~/repos/nix-config")
       ("nix-helpers"     "~/repos/nix-helpers")
       ("repos"           "~/repos")
       ("warbo-packages"  "~/repos/warbo-packages")
       ("warbo-utilities" "~/repos/warbo-utilities")))

    ('nixos-amd64
     '(("blog" "~/repos/chriswarbo-net")
      ("nix-config" "~/repos/nix-config")
      ("nix-helpers" "~/repos/nix-helpers")
      ("warbo-packages" "~/repos/warbo-packages")
      ("warbo-utilities" "~/repos/warbo-utilities")
      ("home" "~")))

    ('framework
     '(("blog" "~/Code/chriswarbo-net")
       ("home" "~")
       ("nix-config" "~/Code/nix-config")
       ("nix-helpers" "~/Code/nix-helpers")
       ("warbo-packages" "~/Code/warbo-packages")
       ("warbo-utilities" "~/Code/warbo-utilities")))

    ('wsl
     '(("deleteme" "~/deleteme")
       ("home" "~")
       ("nix-config" "~/nix-config")
       ("nix-helpers" "~/nix-helpers")
       ("warbo-dotfiles" "~/warbo-dotfiles")
       ("emacs-d" "~/.emacs.d")
       ("warbo-packages" "~/warbo-packages")))

    ('wsl-ubuntu
     `(("home" "~")
       ("emacs-d" "~/.emacs.d")
       ("nix-config" "~/nix-config")
       ("notes" "~/notes")
       ,@(mapcar (lambda (d) `(,d ,(concat "~/src/" d))) warbo-shells-sources)))

    (_ '(("home" "~"))))
  "Useful buffers to open at startup.")

;; Avoid complaints from 'less' about terminal not being fully functional
(setq process-environment (cons "PAGER=cat" process-environment))

;; Show CWD when prompting for a command, with M-! or M-&
(setq shell-command-prompt-show-cwd t)

(defun open-startup-shells ()
  "Open a new shell for each entry in startup-shells."
  (interactive)
  (mapc 'shell-named-in startup-shells))
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

;; TODO: Set bindings via use-package
(add-hook 'eshell-mode-hook
          #'(lambda()
              (local-set-key (kbd "C-l") 'eshell/clear)))

(define-advice shell-command
    (:after (_command &optional _output-buffer _error-buffer))
  "From https://stackoverflow.com/a/6895517/884682 ."
  (when (get-buffer "*Async Shell Command*")
    (with-current-buffer "*Async Shell Command*"
      (rename-uniquely))))

(custom-set-variables
 '(comint-scroll-to-bottom-on-input nil)  ; allow inserting anywhere
 '(comint-scroll-to-bottom-on-output nil) ; allow browsing while output arrives
 '(comint-scroll-show-maximum-output t)   ; scroll to show max possible output
 '(comint-completion-autolist t)          ; show completion list when ambiguous
 '(comint-input-ignoredups t)             ; no duplicates in command history
 '(comint-completion-addsuffix t)         ; insert space/slash after path
 )

(provide 'warbo-shells)
;;; warbo-shells.el ends here

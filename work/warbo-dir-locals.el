;;; warbo-dir-locals -- Apply project-specific settings
;;;
;;; Commentary:
;;; Acts like having a .dir-locals.el file in each project repo, but doesn't
;;; clutter up the actual git repos (since not everyone uses Emacs).  Also lets
;;; us get away with personal, system-specific settings.
;;;
;;; Code:

(defun run-hooks-when-finished (hook process signal)
  "A process sentinel suitable for 'set-process-sentinel'.
The returned sentinel will run the given HOOK when the attached (asynchronous)
PROCESS gets an exit SIGNAL.
Inspired by https://emacs.stackexchange.com/a/42174/5391"
  (when (memq (process-status process) '(exit signal))
    (run-hooks hook)
    (shell-command-sentinel process signal)))

(defun run-test-script-then-hook (prefix cmd sentinel)
  "Run CMD asynchronously, then call HOOK when it's finished.
The process buffer name will contain PREFIX,"
  (let* ((output-buffer (generate-new-buffer (concat "*" prefix " test*")))
         (proc (progn
                 (async-shell-command cmd output-buffer)
                 (get-buffer-process output-buffer))))
    (if (process-live-p proc)
        (set-process-sentinel  proc sentinel)
      (message "Tests finished immediately"))))


(defun github-backup-notify ()
  "Alert when GitHub Backup test script finishes."
  (shell-command "say 'github backup tests finished'"))

(defvar github-backup-after-test-hook nil
  "Hook called after github-backup-test.")

(add-hook 'github-backup-after-test-hook #'github-backup-notify)

(defun github-backup-sentinel (process signal)
  "PROCESS sentinel for test script exit SIGNAL."
  (run-hooks-when-finished 'github-backup-after-test-hook process signal))

(defun github-backup-test ()
  "Run test script for GitHub Backup.
Must be called when cwd is somewhere in GitHub Backup repo."
  (interactive)
  (run-test-script-then-hook
   "github-backup"
   (let* ((dir "$(git rev-parse --show-toplevel)")
          (cd  (concat "pushd " dir " > /dev/null"))
          (tst "./test.sh"))
     (concat cd " && " tst))
   #'github-backup-sentinel))

(makunbound  'github-backup-mode-map)
(fmakunbound 'github-backup-mode-map)
(define-minor-mode github-backup-mode
  "Minor mode for Zipabout's GitHub Backups.
In particular, defines a keymap for shortcuts like running tests"
  nil
  :global nil
  :group 'zipabout
  :keymap (list (cons (kbd "C-c c") 'github-backup-test)))


(defun image-service-notify ()
  "Alert when Image Service test script finishes."
  (shell-command "say 'image service tests finished'"))

(defvar image-service-after-test-hook nil
  "Hook called after image-service-test.")

(add-hook 'image-service-after-test-hook #'image-service-notify)

(defun image-service-sentinel (process signal)
  "PROCESS sentinel for test script exit SIGNAL."
  (run-hooks-when-finished 'image-service-after-test-hook process signal))

(defun image-service-test ()
  "Run test script for Image Service.
Must be called when current working directory is somewhere in Image Service."
  (interactive)
  (run-test-script-then-hook
   "image-service"
   (let* ((dir "$(git rev-parse --show-toplevel)")
          (cd  (concat "pushd " dir " > /dev/null"))
          (tst "./test.sh"))
     (concat cd " && " tst))
   #'image-service-sentinel))

(makunbound  'image-service-mode-map)
(fmakunbound 'image-service-mode-map)
(define-minor-mode image-service-mode
  "Minor mode for Zipabout's Image Service.
In particular, defines a keymap for shortcuts like running tests"
  nil
  :global nil
  :group 'zipabout
  :keymap (list (cons (kbd "C-c c") 'image-service-test)))



(defun nix-helpers-finished-notify ()
  "Alert when nix-helpers test script has finished."
  (shell-command "say 'nix helpers tests finished'"))

(defvar nix-helpers-after-test-hook nil
  "Hook called after nix-helpers-test.")

(add-hook 'nix-helpers-after-test-hook #'nix-helpers-finished-notify)

(defun nix-helpers-sentinel (process signal)
  "PROCESS sentinel for test script exit SIGNAL."
  (run-hooks-when-finished 'nix-helpers-after-test-hook process signal))

(defun nix-helpers-test ()
  "Run test script for Zipabout's nix-helpers.
Must be called when current working directory is somewhere in nix-helpers repo."
  (interactive)
  (run-test-script-then-hook
   "nix-helpers"
   (let* ((dir "$(git rev-parse --show-toplevel)")
          (cd  (concat "pushd " dir " > /dev/null"))
          (tst "./test.sh"))
     (concat cd " && " tst))
   #'nix-helpers-sentinel))

(makunbound  'nix-helpers-mode-map)
(fmakunbound 'nix-helpers-mode-map)
(define-minor-mode nix-helpers-mode
  "Minor mode for files in nix-helpers. In particular, we create a keymap for
common shortcuts, like running tests."
  nil
  :global nil
  :group 'zipabout
  :keymap (list (cons (kbd "C-c c") 'nix-helpers-test)))

(defun zoning-finished-notify ()
  "Alert when zoning test script has finished."
  (shell-command "say 'nix helpers tests finished'"))

(defvar zoning-after-test-hook nil
  "Hook called after zoning-test.")

(add-hook 'zoning-after-test-hook #'zoning-finished-notify)

(defun zoning-sentinel (process signal)
  "PROCESS sentinel for test script exit SIGNAL."
  (run-hooks-when-finished 'zoning-after-test-hook process signal))

(defun zoning-test ()
  "Run test script for Zipabout's zoning.
Must be called when current working directory is somewhere in zoning repo."
  (interactive)
  (run-test-script-then-hook
   "zoning"
   (let* ((dir "$(git rev-parse --show-toplevel)")
          (cd  (concat "pushd " dir " > /dev/null"))
          (tst "nix-build"))
     (concat cd " && " tst))
   #'zoning-sentinel))

(ignore-errors
  (makunbound  'zoning-mode-map)
  (fmakunbound 'zoning-mode-map))
(define-minor-mode zoning-mode
  "Minor mode for files in zoning. In particular, we create a keymap for
common shortcuts, like running tests."
  nil
  :global nil
  :group 'zipabout
  :keymap (list (cons (kbd "C-c c") 'zoning-test)))


(dir-locals-set-class-variables
 'github-backup
 '((nil . ((eval . ((lambda () (github-backup-mode t))))))))

(dir-locals-set-class-variables
 'image-service
 '((nil . ((eval . ((lambda () (image-service-mode t))))))))

(dir-locals-set-class-variables
 'nix-helpers
 '((nil . ((eval . ((lambda () (nix-helpers-mode t))))))))

(dir-locals-set-class-variables
 'zoning
 '((nil . ((eval . ((lambda () (zoning-mode t))))))))

;; Apply the above classes to their relevant project directories
(mapc (lambda (pair)
        (dir-locals-set-directory-class
         (concat (getenv "HOME") "/repos/" (car pair))
         (cdr pair)))
      '(("github-backup"   . github-backup)
        ("image-service"   . image-service)
        ("nix-helpers"     . nix-helpers)
        ("platform-zoning" . zoning)))

(provide 'warbo-dir-locals)
;;; warbo-dir-locals.el ends here

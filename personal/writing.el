;; Don't clobber windmove bindings (this must run before ORG loads)
;; "(add-hook 'org-shiftup-final-hook 'windmove-up)", etc. don't seem to work
;; Default disputed keys remap so that windowmove commands aren't overridden

(setq org-disputed-keys '(([(shift up)] . [(meta p)])
                          ([(shift down)] . [(meta n)])
                          ([(shift left)] . [(meta -)])
                          ([(shift right)] . [(meta +)])
                          ([(meta return)] . [(control meta return)])
                          ([(control shift right)] . [(meta shift +)])
                          ([(control shift left)] . [(meta shift -)])))
(setq org-replace-disputed-keys t)

;; active Babel languages
(org-babel-do-load-languages 'org-babel-load-languages '((haskell    . t)
                                                         (sh         . t)
                                                         (gnuplot    . t)
                                                         (dot        . t)))

(setq org-confirm-babel-evaluate nil)

(setq org-src-fontify-natively t)

;; f5 to save-and-export in Org mode
(defun org-export-as-pdf ()
  (interactive)
  (save-buffer)
  (org-latex-export-to-pdf))

(defun org-export-and-preview ()
  (interactive)
  (let* ((pdf (replace-regexp-in-string "\.org$" ".pdf" (buffer-name)))
         (buf (get-buffer pdf)))
    (when buf (with-current-buffer buf (auto-revert-mode 1)))
    (org-export-as-pdf)
    (unless buf (find-file pdf))))

(add-hook
 'org-mode-hook
 (lambda ()
   (define-key org-mode-map
     (kbd "<f5>") 'org-export-and-preview)))

;; Use AUCTeX
(add-to-list 'auto-mode-alist '("\\.tex$" . LaTeX-mode))

(setq TeX-auto-save t)
(setq TeX-parse-self t)

;; Visual line wrapping in document modes
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)

(add-hook
 'org-mode-hook
 (lambda ()
   (whitespace-mode 0)
   (setq-local whitespace-style (remove-if (lambda (x)
                                             (member x (list 'lines-tail
                                                             'lines)))
                                           whitespace-style))
   (whitespace-mode 1)))

(add-hook 'markdown-mode-hook 'turn-on-visual-line-mode)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)

;; Spell checking. This relies on aspell.
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

(add-hook 'LaTeX-mode-hook (lambda () (whitespace-mode 0)))

(defun compile-with-make ()
  "Run COMPILE, without prompting for a command"
  (interactive)
  (let ((compilation-read-command nil))
    (compile (cond
              ((file-exists-p "render.sh")   "render.sh")
              ((file-exists-p "Makefile")    "make -k ")
              ((file-exists-p "default.nix") "nix-build")
              (t (error "Couldn't find render.sh or Makefile"))))))

(add-hook
 'LaTeX-mode-hook (lambda ()
                    (define-key (current-local-map)
                      (kbd "<f9>") 'compile-with-make)))

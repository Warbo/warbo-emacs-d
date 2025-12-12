(defmacro with-temp-buffer-selected (&rest body)
  "Create a temp buffer, selected in the current window, then run BODY."
  `(with-temp-buffer
    (let ((test-buffer (current-buffer))
          (test-window (selected-window)))
      (set-window-buffer test-window test-buffer)
      (with-selected-window test-window
        (with-current-buffer test-buffer
          ,@body)))))

(ert-deftest warbo-test-delete-selection-mode-enabled ()
  "Test that `delete-selection-mode' is enabled globally."
  (should delete-selection-mode))

(ert-deftest warbo-test-transient-mark-mode-enabled ()
  "Test that `transient-mark-mode' is enabled globally."
  (should transient-mark-mode))

(ert-deftest warbo-test-yank-indent ()
  "Test that yanking in a programming mode indents the yanked text."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun my-func ()\n)")
    (kill-new "(message \"hello\")")
    (goto-char (1- (point-max)))
    (yank)
    (should (string-match-p "\n  (message \"hello\"))" (buffer-string)))))

(ert-deftest warbo-test-yank-pop-after-yank-indents ()
  "Test that `yank-pop` after a `yank` indents the replaced text."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun my-func ()\n)")
    (kill-new "(message \"the real thing\")")
    (kill-new "\"a decoy\"")
    (goto-char (1- (point-max)))
    (yank) ;; Yank "a decoy"
    (let ((last-command 'yank))
      (yank-pop)) ;; Replace with "the real thing"
    (should (string-match-p "\n  (message \"the real thing\"))" (buffer-string)))))

(ert-deftest warbo-test-yank-pop-interactive-indents ()
  "Test that `yank-pop` called interactively indents the selected text."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun my-func ()\n)")
    (goto-char (1- (point-max)))
    (cl-letf (((symbol-function 'completing-read)
                       (lambda (prompt choices &rest args)
                         (should (string-equal "Yank from kill-ring: " prompt))
                         "(message \"selected item\")")))
              (let ((current-prefix-arg 1)
                    ;; Ensure yank-pop takes the interactive branch
                    (last-command nil))
                (yank-pop)))
    (should (string-match-p "\n  (message \"selected item\"))" (buffer-string)))))

(ert-deftest warbo-test-typing-replaces-region ()
  "Test that typing replaces the contents of the region."
  (with-temp-buffer-selected
   (python-mode)
   (insert "foo = 'hello world'")
   (goto-char 13)
   (set-mark-command nil)
   (goto-char 8)
   (execute-kbd-macro (kbd "g o o d b y e"))
   (should (string-equal (buffer-string) "foo = 'goodbye world'"))))

(ert-deftest warbo-test-typing-deletes-region ()
  "Test that typing with an active region deletes it first."
  (with-temp-buffer-selected
   (python-mode)
   (insert "foo = 'hello world'")
   (goto-char 13)
   (set-mark-command nil)
   (goto-char 8)
   (dbg "Before typing X:")
   (dbg "  region-active-p: %s" (region-active-p))
   (dbg "  use-region-p: %s" (use-region-p))
   (dbg "  mark-active: %s" mark-active)
   (dbg "  transient-mark-mode: %s" transient-mark-mode)
   (dbg "  delete-selection-mode: %s" delete-selection-mode)
   (dbg "  region-beginning: %s" (region-beginning))
   (dbg "  region-end: %s" (region-end))
   (dbg "  buffer-string: %s" (buffer-string))
   (dbg "  current-buffer: %s" (current-buffer))
   (should (region-active-p))
   (should (eq (current-buffer) test-buffer))
   (execute-kbd-macro (kbd "X"))
   (dbg "After typing X:")
   (dbg "  buffer-string: %s" (buffer-string))
   (dbg "  current-buffer: %s" (current-buffer))
   (should (eq (current-buffer) test-buffer))
   (should (string-equal (buffer-string) "foo = 'X world'"))))

(ert-deftest warbo-test-yank-without-region ()
  "Test that yanking without an active region inserts text."
  (with-temp-buffer-selected
   (python-mode)
   (insert "foo = 'hello world'")
   (kill-new "goodbye")
   (goto-char 8)
   (execute-kbd-macro (kbd "C-y"))
   (should (string-match-p "goodbye" (buffer-string)))))

(ert-deftest warbo-test-yank-replaces-region ()
  "Test that yanking replaces the contents of the region."
  (with-temp-buffer-selected
   (python-mode)
   (insert "foo = 'hello world'")
   (kill-new "goodbye")
   (goto-char 13)
   (set-mark-command nil)
   (goto-char 8)
   (execute-kbd-macro (kbd "C-y"))
   (should (string-equal (buffer-string) "foo = 'goodbye world'"))))

(ert-deftest warbo-test-expand-and-contract-brackets ()
  "Test that C-<left> and C-<right> expand/contract bracketssexps."
  (with-temp-buffer-selected
   (python-mode)
   (let ((original-code "foo(w, bar(x, y), z)")
         (contracted-code "foo(w, bar(x), y, z)")
         (expanded-code "foo(w, bar(x, y, z))"))
     ;; Test contraction
     (insert original-code)
     (goto-char (1+ (string-search "x" original-code)))
     (let ((initial-point (point)))
       (execute-kbd-macro (kbd "C-<left>"))
       (should (string-equal (buffer-string) contracted-code))
       (should (= initial-point (point))))

     ;; Test expansion
     (delete-region (point-min) (point-max))
     (insert original-code)
     (goto-char (1+ (string-search "x" original-code)))
     (let ((initial-point (point)))
       (execute-kbd-macro (kbd "C-<right>"))
       (should (string-equal (buffer-string) expanded-code))
       (should (= initial-point (point)))))))

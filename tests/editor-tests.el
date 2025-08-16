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

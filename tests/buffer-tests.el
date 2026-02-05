;; CONFIRMED FINDINGS:
;; - vertico-sort-override-function takes precedence over display-sort-function
;; - Buffer completion (via consult) sets display-sort-function to `identity`
;; - Our sort function IS called on every keystroke
;; - Candidates have text properties AND invisible suffix characters
;; - The invisible suffix is U+200000 (decimal 2097152), in Supplementary
;;   Private Use Area-B. It appears as U+FFFD in terminal output.
;; - substring-no-properties removes properties but NOT the invisible char
;; - Use string-trim-right with (string ?\x200000) to strip it

(ert-deftest warbo-test-buffer-vertico-sort-function-set ()
  "Test that the vertico sort override function is set to prefer exact matches."
  (should (eq vertico-sort-override-function #'warbo-vertico-sort-prefer-exact)))

(ert-deftest warbo-test-buffer-sort-function-logic ()
  "Test that warbo-vertico-sort-prefer-exact moves exact matches to front."
  (cl-letf (((symbol-function 'minibuffer-contents-no-properties)
             (lambda () "foo")))
    (let ((result (warbo-vertico-sort-prefer-exact '("foo-bar" "foo" "foo-baz"))))
      (should (equal (car result) "foo")))))

(ert-deftest warbo-test-buffer-sort-function-called ()
  "Test that vertico actually calls our sort function during completion."
  (let ((buf (generate-new-buffer "warbo-test-sort-check"))
        (sort-calls nil))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (switch-to-buffer (get-buffer-create "*scratch*"))
          (advice-add 'vertico-sort-history-length-alpha :before
                      (lambda (candidates)
                        (push (list 'inner-sort (length candidates)) sort-calls))
                      '((name . test-spy-inner)))
          (advice-add 'warbo-vertico-sort-prefer-exact :before
                      (lambda (candidates)
                        (push (list 'outer-sort
                                    (minibuffer-contents-no-properties)
                                    (length candidates))
                              sort-calls))
                      '((name . test-spy-outer)))
          (unwind-protect
              (progn
                (execute-kbd-macro (kbd "C-x b warbo-test-sort RET"))
                (should sort-calls))
            (advice-remove 'warbo-vertico-sort-prefer-exact 'test-spy-outer)
            (advice-remove 'vertico-sort-history-length-alpha 'test-spy-inner)))
      (when (get-buffer buf) (kill-buffer buf)))))

(ert-deftest warbo-test-buffer-inspect-invisible-suffix ()
  "Inspect the actual character codes of the invisible suffix consult adds."
  (let ((buf (generate-new-buffer "warbo-test-suffix-check"))
        (suffix-info nil))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (switch-to-buffer (get-buffer-create "*scratch*"))
          (advice-add 'warbo-vertico-sort-prefer-exact :before
                      (lambda (candidates)
                        (dolist (c candidates)
                          (when (string-match-p "warbo-test-suffix" c)
                            ;; Find where invisible property starts
                            (let* ((len (length c))
                                   (visible-end nil))
                              (dotimes (i len)
                                (when (and (not visible-end)
                                           (get-text-property i 'invisible c))
                                  (setq visible-end i)))
                              (when visible-end
                                (let* ((visible-part (substring c 0 visible-end))
                                       (invisible-part (substring c visible-end))
                                       (invisible-chars (mapcar (lambda (ch)
                                                                  (list ch (format "#x%x" ch)))
                                                                (string-to-list invisible-part))))
                                  (push (list 'visible visible-part
                                              'visible-len visible-end
                                              'total-len len
                                              'invisible-chars invisible-chars)
                                        suffix-info)))))))
                      '((name . test-spy)))
          (unwind-protect
            (execute-kbd-macro (kbd "C-x b warbo-test-suffix RET"))
            (advice-remove 'warbo-vertico-sort-prefer-exact 'test-spy)))
      (kill-buffer buf))))

(ert-deftest warbo-test-buffer-see-candidates-at-exact-match ()
  "See what candidates exist when exact buffer name is typed.
CONFIRMED: When input is 'warbo-test-buffer-4', candidates include:
  #(\"warbo-test-buffer-4\\x00\" 0 19 (face consult-buffer ...) 19 20 (invisible t ...))
The exact match IS present but has an invisible suffix character."
  (let ((buf-exact (generate-new-buffer "warbo-test-buffer-4"))
        (buf-prefix-a (generate-new-buffer "warbo-test-buffer-4.nix"))
        (buf-prefix-b (generate-new-buffer "warbo-test-buffer-4-manager.nix"))
        (sort-calls nil))
    (unwind-protect
        (progn
          (switch-to-buffer buf-exact)
          (switch-to-buffer buf-prefix-a)
          (switch-to-buffer buf-prefix-b)
          (switch-to-buffer (get-buffer-create "*scratch*"))
          (advice-add 'warbo-vertico-sort-prefer-exact :before
                      (lambda (candidates)
                        (push (list (minibuffer-contents-no-properties)
                                    candidates)
                              sort-calls))
                      '((name . test-spy)))
          (unwind-protect
            (execute-kbd-macro (kbd "C-x b warbo-test-buffer-4 RET"))
            (advice-remove 'warbo-vertico-sort-prefer-exact 'test-spy)))
      (kill-buffer buf-exact)
      (kill-buffer buf-prefix-a)
      (kill-buffer buf-prefix-b))))

(ert-deftest warbo-test-buffer-check-display-sort-function ()
  "Check if buffer completion provides its own display-sort-function."
  (let ((metadata-seen nil))
    (advice-add 'vertico--sort-function :before
                (lambda ()
                  (push (list 'sort-fn-check
                              (vertico--metadata-get 'display-sort-function)
                              vertico-sort-override-function
                              vertico-sort-function)
                        metadata-seen))
                '((name . test-spy)))
    (unwind-protect
      (execute-kbd-macro (kbd "C-x b test RET"))
      (advice-remove 'vertico--sort-function 'test-spy))))

(ert-deftest warbo-test-buffer-vertico-sort-function-called ()
  "Check if vertico--sort-function is called at all."
  (let ((called nil))
    (advice-add 'vertico--sort-function :before
                (lambda () (setq called t))
                '((name . test-spy)))
    (unwind-protect
        (progn
          (execute-kbd-macro (kbd "C-x b test RET"))
          (should called))
      (advice-remove 'vertico--sort-function 'test-spy))))

(ert-deftest warbo-test-buffer-vertico-candidates-populated ()
  "Test that vertico--candidates gets populated during completion."
  (let ((buf (generate-new-buffer "warbo-test-vertico-check")))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (switch-to-buffer (get-buffer-create "*scratch*"))
          (let ((candidates-seen nil))
            (advice-add 'vertico--update :after
                        (lambda (&rest _)
                          (when (bound-and-true-p vertico--candidates)
                            (setq candidates-seen vertico--candidates)))
                        '((name . test-spy)))
            (unwind-protect
                (progn
                  (execute-kbd-macro (kbd "C-x b warbo-test-vertico RET"))
                  (should candidates-seen))
              (advice-remove 'vertico--update 'test-spy))))
      (kill-buffer buf))))

(ert-deftest warbo-test-buffer-exact-match-selected ()
  "Test that typing an exact buffer name selects that buffer by default."
  (let ((buf-exact (generate-new-buffer "warbo-test-buffer-1"))
        (buf-prefix-a (generate-new-buffer "warbo-test-buffer-1.nix<wsl-ubuntu>"))
        (buf-prefix-b (generate-new-buffer "warbo-test-buffer-1-manager.nix")))
    (unwind-protect
        (progn
          ;; Switch to buffers in order to establish recency (most recent last)
          (switch-to-buffer buf-exact)
          (switch-to-buffer buf-prefix-a)
          (switch-to-buffer buf-prefix-b)

          ;; Switch away so we can test switching back
          (switch-to-buffer (get-buffer-create "*scratch*"))

          ;; Simulate user typing "C-x b warbo-test-buffer-1 RET"
          (execute-kbd-macro (kbd "C-x b warbo-test-buffer-1 RET"))

          ;; Check that we switched to the exact match, not a prefix match
          (should (equal (buffer-name (current-buffer)) "warbo-test-buffer-1")))
      ;; Cleanup
      (kill-buffer buf-exact)
      (kill-buffer buf-prefix-a)
      (kill-buffer buf-prefix-b))))

(ert-deftest warbo-test-buffer-completion-prefers-exact-match ()
  "Test that buffer completion prefers exact matches over prefix matches."
  (let ((buf-exact (generate-new-buffer "warbo-test-buffer-2"))
        (buf-prefix-a (generate-new-buffer "warbo-test-buffer-2-file.el"))
        (buf-prefix-b (generate-new-buffer "warbo-test-buffer-2ing.txt")))
    (unwind-protect
        (progn
          ;; Make warbo-test-buffer-2-file.el the most recently used
          (switch-to-buffer buf-exact)
          (switch-to-buffer buf-prefix-b)
          (switch-to-buffer buf-prefix-a)

          ;; Switch away so we can test switching back
          (switch-to-buffer (get-buffer-create "*scratch*"))

          ;; Simulate user typing "C-x b warbo-test-buffer-2 RET"
          (execute-kbd-macro (kbd "C-x b warbo-test-buffer-2 RET"))

          ;; When we type "warbo-test-buffer-2" exactly, we should get the exact match
          ;; not "warbo-test-buffer-2-file.el" even though it's more recent
          (should (equal (buffer-name (current-buffer)) "warbo-test-buffer-2")))
      ;; Cleanup
      (kill-buffer buf-exact)
      (kill-buffer buf-prefix-a)
      (kill-buffer buf-prefix-b))))

(ert-deftest warbo-test-buffer-completion-without-exact-match ()
  "Test that buffer completion works normally when there's no exact match."
  (let ((buf-prefix-a (generate-new-buffer "warbo-test-buffer-3.nix"))
        (buf-prefix-b (generate-new-buffer "warbo-test-buffer-3-manager.nix")))
    (unwind-protect
        (progn
          ;; Make warbo-test-buffer-3-manager.nix the most recently used
          (switch-to-buffer buf-prefix-a)
          (switch-to-buffer buf-prefix-b)

          ;; Switch away so we can test switching back
          (switch-to-buffer (get-buffer-create "*scratch*"))

          ;; Simulate user typing "C-x b warbo-test-buffer-3 RET"
          (execute-kbd-macro (kbd "C-x b warbo-test-buffer-3 RET"))

          ;; When we type "warbo-test-buffer-3" but there's no exact match,
          ;; it should select one of the prefix matches
          (should (member (buffer-name (current-buffer))
                        '("warbo-test-buffer-3-manager.nix" "warbo-test-buffer-3.nix"))))
      ;; Cleanup
      (kill-buffer buf-prefix-a)
      (kill-buffer buf-prefix-b))))

;; Tests for issue 94e28d5e88cf6b0d: MRU sorting when no exact match

(ert-deftest warbo-test-buffer-mru-without-exact-match ()
  "Test that buffer completion defaults to MRU when there's no exact match.
When typing a prefix that matches multiple buffers but doesn't match any
buffer exactly, pressing RET should select the most-recently-used buffer."
  (let ((buf-a (generate-new-buffer "warbo-mru-test-alpha.txt"))
        (buf-b (generate-new-buffer "warbo-mru-test-beta.txt"))
        (buf-c (generate-new-buffer "warbo-mru-test-gamma.txt")))
    (unwind-protect
        (progn
          ;; Establish MRU order: buf-a (oldest), buf-b, buf-c (most recent)
          (switch-to-buffer buf-a)
          (switch-to-buffer buf-b)
          (switch-to-buffer buf-c)

          ;; Switch away so we can test switching back
          (switch-to-buffer (get-buffer-create "*scratch*"))

          ;; Type prefix "warbo-mru-test" which matches all three but none exactly
          (execute-kbd-macro (kbd "C-x b warbo-mru-test RET"))

          ;; Should switch to buf-c (the most recently used)
          (should (equal (buffer-name (current-buffer)) "warbo-mru-test-gamma.txt")))
      ;; Cleanup
      (kill-buffer buf-a)
      (kill-buffer buf-b)
      (kill-buffer buf-c))))

(ert-deftest warbo-test-buffer-mru-ordering-preserved ()
  "Test that MRU ordering is preserved across multiple switches.
This tests that the MRU list stays correct even after switching buffers
multiple times in different orders."
  (let ((buf-1 (generate-new-buffer "warbo-mru-order-1.el"))
        (buf-2 (generate-new-buffer "warbo-mru-order-2.el"))
        (buf-3 (generate-new-buffer "warbo-mru-order-3.el"))
        (scratch (get-buffer-create "*scratch*")))
    (unwind-protect
        (progn
          ;; First access pattern
          (switch-to-buffer buf-1)
          (switch-to-buffer buf-2)
          (switch-to-buffer buf-3)
          
          ;; Switch away and back - should get buf-3 (most recent)
          (switch-to-buffer scratch)
          (execute-kbd-macro (kbd "C-x b warbo-mru-order RET"))
          (should (equal (buffer-name (current-buffer)) "warbo-mru-order-3.el"))

          ;; Now switch to buf-1 to make it most recent
          (switch-to-buffer buf-1)
          
          ;; Switch away and back - should now get buf-1
          (switch-to-buffer scratch)
          (execute-kbd-macro (kbd "C-x b warbo-mru-order RET"))
          (should (equal (buffer-name (current-buffer)) "warbo-mru-order-1.el")))
      ;; Cleanup
      (kill-buffer buf-1)
      (kill-buffer buf-2)
      (kill-buffer buf-3))))

(ert-deftest warbo-test-buffer-previous-buffer-shortcut ()
  "Test that empty buffer switch defaults to the previously open buffer.
Pressing C-x b RET (with no input) should switch to the buffer that was
most recently visited before the current one."
  (let ((buf-first (generate-new-buffer "warbo-previous-test-first"))
        (buf-second (generate-new-buffer "warbo-previous-test-second")))
    (unwind-protect
        (progn
          ;; Start at first buffer
          (switch-to-buffer buf-first)
          
          ;; Switch to second buffer
          (switch-to-buffer buf-second)
          
          ;; Now C-x b RET should take us back to first buffer
          (execute-kbd-macro (kbd "C-x b RET"))
          (should (equal (buffer-name (current-buffer)) "warbo-previous-test-first"))
          
          ;; And doing it again should take us back to second buffer
          (execute-kbd-macro (kbd "C-x b RET"))
          (should (equal (buffer-name (current-buffer)) "warbo-previous-test-second")))
      ;; Cleanup
      (kill-buffer buf-first)
      (kill-buffer buf-second))))

(ert-deftest warbo-test-buffer-mru-with-mixed-names ()
  "Test MRU sorting with buffers that have varying prefix overlap.
This ensures that the sorting works correctly even when buffer names
have different levels of similarity to the typed prefix."
  (let ((buf-exact-prefix (generate-new-buffer "proj-foo"))
        (buf-longer-name (generate-new-buffer "proj-foobar.txt"))
        (buf-different-suffix (generate-new-buffer "proj-foo-manager.el")))
    (unwind-protect
        (progn
          ;; Access in order: exact-prefix (oldest), longer-name, different-suffix (newest)
          (switch-to-buffer buf-exact-prefix)
          (switch-to-buffer buf-longer-name)
          (switch-to-buffer buf-different-suffix)

          ;; Switch away
          (switch-to-buffer (get-buffer-create "*scratch*"))

          ;; Type "proj-fo" which matches all three
          (execute-kbd-macro (kbd "C-x b proj-fo RET"))

          ;; Should get the most recently used: buf-different-suffix
          (should (equal (buffer-name (current-buffer)) "proj-foo-manager.el")))
      ;; Cleanup
      (kill-buffer buf-exact-prefix)
      (kill-buffer buf-longer-name)
      (kill-buffer buf-different-suffix))))

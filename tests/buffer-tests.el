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
                (message "Sort calls: %S" sort-calls)
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
              (progn
                (execute-kbd-macro (kbd "C-x b warbo-test-suffix RET"))
                (message "Suffix info: %S" suffix-info))
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
              (progn
                (execute-kbd-macro (kbd "C-x b warbo-test-buffer-4 RET"))
                (message "Candidates at each step: %S" sort-calls))
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
        (progn
          (execute-kbd-macro (kbd "C-x b test RET"))
          (message "Metadata seen: %S" metadata-seen))
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
          (message "vertico--sort-function called: %S" called)
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

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

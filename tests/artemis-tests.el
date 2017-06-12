(ert-deftest warbo-artemis-defined ()
  "Have Artemis functionality"
  (should (fboundp 'artemis-mode))
  (should (fboundp 'switch-to-artemis)))

(ert-deftest warbo-artemis-switch ()
  "Can switch on artemis-mode"
  ;; Load some file
  (let* ((path "~/.emacs.d/tests/artemis-tests.el")
         (buf  (find-file path)))
    ;; Switch away to ensure we're not just altering the current buffer
    (with-current-buffer (get-buffer "*scratch*")
      (switch-to-artemis path))
    (with-current-buffer buf
      (should (equal major-mode 'artemis-mode))
      (kill-buffer))))

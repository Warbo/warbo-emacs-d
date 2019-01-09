;; Only bother checking shell directories if we're on my laptop ;)
(when (equal (getenv "HOME") "/home/chris")
  (ert-deftest warbo-shells-exist ()
    "Shell directories exist"
    (dolist (pair startup-shells)
      (should (file-accessible-directory-p (cadr pair)))))

  (ert-deftest warbo-shells-open ()
    "Startup shells are open"
    (dolist (pair startup-shells)
      (should (member (car pair) (mapcar 'buffer-name (buffer-list)))))))

(ert-deftest warbo-shell-unique ()
  "Shell names don't overlap"
  (with-temp-buffer
    (rename-buffer "*test-buffer-1*")
    (should (equal (free-name-num "test-buffer") "*test-buffer-2*"))))

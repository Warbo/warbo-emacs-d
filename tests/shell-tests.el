(ert-deftest warbo-shells-exist ()
  "Shell directories exist"
  (dolist (pair startup-shells)
    (should (file-accessible-directory-p (cadr pair)))))

(ert-deftest warbo-shells-open ()
  "Startup shells are open"
  (dolist (pair startup-shells)
    (should (member (car pair) (mapcar 'buffer-name (buffer-list))))))

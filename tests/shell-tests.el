(ert-deftest warbo-shells-exist ()
  "Shell directories exist"
  (dolist (pair startup-shells)
    (should (file-accessible-directory-p (cadr pair)))))

(ert 'warbo-shells-exist)

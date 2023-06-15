(ert-deftest warbo-python-mode ()
  "Python buffers should get python-mode"
  (with-temp-buffer
    (rename-buffer "warbo-python-mode.py")
    (normal-mode)
    (should (equal major-mode 'python-mode))))

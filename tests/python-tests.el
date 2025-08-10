(ert-deftest warbo-python-mode ()
  "Python buffers should get python-mode"
  (with-temp-buffer
    (setq buffer-file-name "warbo-python-mode.py")
    (set-auto-mode)
    (should (equal major-mode 'python-mode))))

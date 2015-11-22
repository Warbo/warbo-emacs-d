;; Load init
(load "~/.emacs.d/init.el")

;; Load all *-tests.el files
(let ((load-test `(lambda (f) (load  (concat "~/.emacs.d/tests/" f)))))
  (mapcar load-test (directory-files "." nil ".*-tests\.el")))

;; Run all tests
(ert-run-tests-batch-and-exit)

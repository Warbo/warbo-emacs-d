(message "Loading init.el")
(load "~/.emacs.d/init.el")

(message "Loading *-tests.el files")
(let ((load-test `(lambda (f) (load  (concat "~/.emacs.d/tests/" f)))))
  (mapcar load-test (directory-files "~/.emacs.d/tests" nil ".*-tests\.el")))

(message "Running tests")
(ert-run-tests-batch-and-exit (getenv "EMACS_TEST_REGEX"))

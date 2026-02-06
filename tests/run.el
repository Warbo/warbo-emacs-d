(require 'files)
;; Prevent async native compilation during tests.  The async subprocess
;; doesn't have macros from personal/preload/ (e.g. `thinkpad-only'),
;; producing broken .eln files whose load-time sentinel errors crash batch
;; mode before ERT can report results.
(advice-add 'native-compile-async :override #'ignore)
(let ((default-directory user-emacs-directory))
  (load-file "early-init.el")
  (load-file "init.el"))
(let ((default-directory (expand-file-name "tests" user-emacs-directory)))
  (mapcar 'load-file (directory-files default-directory nil ".*-tests\.el")))
(ert-run-tests-batch-and-exit (getenv "EMACS_TEST_REGEX"))

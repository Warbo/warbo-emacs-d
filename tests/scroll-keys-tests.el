;;; scroll-keys-tests.el --- Tests for M-<up> and M-<down> scroll keybindings -*- lexical-binding: t; -*-
;;;
;;; Tests to ensure M-<up> and M-<down> are bound to scroll-up-line and
;;; scroll-down-line in various modes that might override these bindings.

(require 'ert)

;; Ensure paredit and smartparens are loaded for testing
;; These will be loaded via init.el when running the full test suite
(require 'paredit)
(require 'smartparens)

(defun test-scroll-key-binding (mode-name mode-fn up-expected down-expected)
  "Test that scroll keys work in MODE-NAME after enabling MODE-FN.
UP-EXPECTED and DOWN-EXPECTED are the expected command symbols."
  (with-temp-buffer
    ;; Set up a temporary buffer in the specified mode
    (funcall mode-fn)

    ;; Check what M-<up> is bound to
    (let ((up-binding (key-binding (kbd "M-<up>")))
          (down-binding (key-binding (kbd "M-<down>"))))

      (list :mode mode-name
            :up-binding up-binding
            :up-matches (eq up-binding up-expected)
            :down-binding down-binding
            :down-matches (eq down-binding down-expected)))))

(ert-deftest warbo-scroll-keys-in-emacs-lisp-mode ()
  "Test that scroll keys work in emacs-lisp-mode."
  (let ((result (test-scroll-key-binding
                 'emacs-lisp-mode
                 #'emacs-lisp-mode
                 'scroll-up-line
                 'scroll-down-line)))
    (should (plist-get result :up-matches))
    (should (plist-get result :down-matches))))

(ert-deftest warbo-scroll-keys-in-lisp-interaction-mode ()
  "Test that scroll keys work in lisp-interaction-mode."
  (let ((result (test-scroll-key-binding
                 'lisp-interaction-mode
                 #'lisp-interaction-mode
                 'scroll-up-line
                 'scroll-down-line)))
    (should (plist-get result :up-matches))
    (should (plist-get result :down-matches))))

(ert-deftest warbo-scroll-keys-in-paredit-mode ()
  "Test that scroll keys work in paredit-mode."
  (with-temp-buffer
    (emacs-lisp-mode)
    (paredit-mode 1)
    (let ((up-binding (key-binding (kbd "M-<up>")))
          (down-binding (key-binding (kbd "M-<down>"))))
      (should (eq up-binding 'scroll-up-line))
      (should (eq down-binding 'scroll-down-line)))))

(ert-deftest warbo-scroll-keys-in-smartparens-mode ()
  "Test that scroll keys work in smartparens-mode."
  (with-temp-buffer
    (emacs-lisp-mode)
    (smartparens-mode 1)
    (let ((up-binding (key-binding (kbd "M-<up>")))
          (down-binding (key-binding (kbd "M-<down>"))))
      (should (eq up-binding 'scroll-up-line))
      (should (eq down-binding 'scroll-down-line)))))

(ert-deftest warbo-scroll-keys-in-prog-mode ()
  "Test that scroll keys work in prog-mode derivatives."
  (let ((result (test-scroll-key-binding
                 'python-mode
                 #'python-mode
                 'scroll-up-line
                 'scroll-down-line)))
    (should (plist-get result :up-matches))
    (should (plist-get result :down-matches))))

(ert-deftest warbo-scroll-keys-in-text-mode ()
  "Test that scroll keys work in text-mode."
  (let ((result (test-scroll-key-binding
                 'text-mode
                 #'text-mode
                 'scroll-up-line
                 'scroll-down-line)))
    (should (plist-get result :up-matches))
    (should (plist-get result :down-matches))))

(defun run-scroll-key-tests ()
  "Run all scroll key tests and report results."
  (interactive)
  (ert-run-tests-interactively "test-scroll-keys"))

(defun check-scroll-keys-in-current-buffer ()
  "Check what M-<up> and M-<down> are bound to in the current buffer."
  (interactive)
  (let ((up-binding (key-binding (kbd "M-<up>")))
        (down-binding (key-binding (kbd "M-<down>"))))
    (message "Current mode: %s" major-mode)
    (message "Active minor modes: %s"
             (seq-filter #'identity
                        (mapcar (lambda (mode)
                                 (when (and (boundp mode) (symbol-value mode))
                                   mode))
                               minor-mode-list)))
    (message "M-<up> is bound to: %s (expected: scroll-up-line)" up-binding)
    (message "M-<down> is bound to: %s (expected: scroll-down-line)" down-binding)
    (list :mode major-mode
          :up-binding up-binding
          :down-binding down-binding
          :correct (and (eq up-binding 'scroll-up-line)
                       (eq down-binding 'scroll-down-line)))))

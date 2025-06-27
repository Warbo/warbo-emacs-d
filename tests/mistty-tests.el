;;; mistty-tests.el --- Tests for mistty terminal -*- lexical-binding: t -*-

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; `http://www.gnu.org/licenses/'.

;;; Commentary:
;;
;; ERT tests for mistty-term.el.
;;

;;; Code:

(require 'ert)
(require 'term) ; For term-mode and term-emulate-terminal
(require 'mistty-term) ; The file under test

(defvar mistty-tests--emulate-terminal-args nil
  "Variable to store arguments passed to advised `term-emulate-terminal'.")

(defun mistty-tests--advice-term-emulate-terminal (orig-fun proc str)
  "Advice to capture arguments passed to `term-emulate-terminal'."
  (setq mistty-tests--emulate-terminal-args (list proc str))
  ;; Call the original function to allow normal processing if needed,
  ;; though for these tests we primarily care about the input.
  (funcall orig-fun proc str))

(ert-deftest mistty-test-emulate-terminal-passthrough ()
  "Test that `mistty--emulate-terminal' passes through regular text."
  (let ((test-buffer (generate-new-buffer "*mistty-test*"))
        (test-proc (make-process :name "test-proc" :buffer nil :command '("sleep" "100"))))
    (with-current-buffer test-buffer
      (term-mode)
      (set-process-buffer test-proc test-buffer)
      (unwind-protect
          (progn
            (advice-add 'term-emulate-terminal :around #'mistty-tests--advice-term-emulate-terminal)
            (setq mistty-tests--emulate-terminal-args nil) ; Reset capture variable

            (let ((input-string "hello world\n"))
              (mistty--emulate-terminal test-proc input-string)
              (should (equal mistty-tests--emulate-terminal-args (list test-proc input-string))))

            (setq mistty-tests--emulate-terminal-args nil)
            (let ((input-string "another line\r"))
              (mistty--emulate-terminal test-proc input-string)
              (should (equal mistty-tests--emulate-terminal-args (list test-proc input-string)))))
        ;; Cleanup
        (advice-remove 'term-emulate-terminal #'mistty-tests--advice-term-emulate-terminal)
        (kill-process test-proc)
        (kill-buffer test-buffer)))))

(ert-deftest mistty-test-emulate-terminal-control-chars ()
  "Test how `mistty--emulate-terminal' handles SOH and STX control characters."
  (let ((test-buffer (generate-new-buffer "*mistty-test*"))
        (test-proc (make-process :name "test-proc" :buffer nil :command '("sleep" "100"))))
    (with-current-buffer test-buffer
      (term-mode)
      (set-process-buffer test-proc test-buffer)
      (unwind-protect
          (progn
            (advice-add 'term-emulate-terminal :around #'mistty-tests--advice-term-emulate-terminal)
            (setq mistty-tests--emulate-terminal-args nil) ; Reset capture variable

            ;; String with SOH (^A, \001) and STX (^B, \002)
            (let ((input-string (concat "prefix" (string ?\001 ?\002) "suffix\n")))
              (mistty--emulate-terminal test-proc input-string)
              ;; Assert that the original string *including* the control chars is passed
              ;; to term-emulate-terminal, as per the current code.
              (should (equal mistty-tests--emulate-terminal-args (list test-proc input-string))))

            (setq mistty-tests--emulate-terminal-args nil)
            ;; String with only control chars
            (let ((input-string (string ?\001 ?\002)))
              (mistty--emulate-terminal test-proc input-string)
              (should (equal mistty-tests--emulate-terminal-args (list test-proc input-string)))))
        ;; Cleanup
        (advice-remove 'term-emulate-terminal #'mistty-tests--advice-term-emulate-terminal)
        (kill-process test-proc)
        (kill-buffer test-buffer)))))

(ert-deftest mistty-test-emulate-terminal-other-control-chars ()
  "Test that `mistty--emulate-terminal' passes through other control characters."
  (let ((test-buffer (generate-new-buffer "*mistty-test*"))
        (test-proc (make-process :name "test-proc" :buffer nil :command '("sleep" "100"))))
    (with-current-buffer test-buffer
      (term-mode)
      (set-process-buffer test-proc test-buffer)
      (unwind-protect
          (progn
            (advice-add 'term-emulate-terminal :around #'mistty-tests--advice-term-emulate-terminal)
            (setq mistty-tests--emulate-terminal-args nil) ; Reset capture variable

            ;; String with ESC (\e, \033) and CR (\r, \015)
            (let ((input-string (concat "line1\r" "line2\e[K\n")))
              (mistty--emulate-terminal test-proc input-string)
              (should (equal mistty-tests--emulate-terminal-args (list test-proc input-string)))))
        ;; Cleanup
        (advice-remove 'term-emulate-terminal #'mistty-tests--advice-term-emulate-terminal)
        (kill-process test-proc)
        (kill-buffer test-buffer)))))

(provide 'mistty-tests)

;;; mistty-tests.el ends here

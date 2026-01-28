;;; web-tests --- Test stubs for warbo-web.el
;;; Commentary:
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'eww) ;; Ensure eww is loaded for its functions and variables
(require 'warbo-web) ;; Ensure warbo-web.el is loaded for its definitions

;; --- Test Stubs ---

(ert-deftest warbo-web-eww-width-increase-visual-test ()
  "Manual test: Verify `eww-increase-width` visually affects EWW rendering width."
  ;; High-level steps for manual verification:
  ;; 1. Open an EWW buffer with a long HTML page (e.g., `eww "https://www.gnu.org/software/emacs/manual/html_node/emacs.html"`).
  ;; 2. Note the current rendering width and text wrapping.
  ;; 3. Press `+` (eww-increase-width).
  ;; 4. Observe that the EWW buffer's content re-renders with a wider effective width,
  ;;    and text wrapping adjusts accordingly.
  ;; 5. Verify that `gcs-shr-width` variable has increased by 10.
  (should t) ;; Placeholder: This test requires manual execution and observation.
  )

(ert-deftest warbo-web-eww-width-decrease-visual-test ()
  "Manual test: Verify `eww-decrease-width` visually affects EWW rendering width."
  ;; High-level steps for manual verification:
  ;; 1. Open an EWW buffer with a long HTML page.
  ;; 2. Note the current rendering width and text wrapping.
  ;; 3. Press `-` (eww-decrease-width).
  ;; 4. Observe that the EWW buffer's content re-renders with a narrower effective width,
  ;;    and text wrapping adjusts accordingly.
  ;; 5. Verify that `gcs-shr-width` variable has decreased by 10.
  (should t) ;; Placeholder: This test requires manual execution and observation.
  )

;; TODO: captured-url is nil - browse-url-firefox not called or eww-data binding issue
(ert-deftest warbo-web-external-browser-dispatch-via-keypress ()
  "Test that pressing '&' in EWW mode dispatches to `browse-url-firefox` with the correct URL."
  (let (captured-url
        (test-url "http://example.com/test-page")
        (original-browse-url-firefox (symbol-function 'browse-url-firefox))) ;; Store original function
    (unwind-protect
        (progn
          ;; Set our mock function for browse-url-firefox using fset
          (fset 'browse-url-firefox
                (lambda (url &rest args)
                  (setq captured-url url)
                  nil))

          (cl-letf ((eww-data (list :url test-url)) ;; Correctly bind eww-data as a variable
                    ;; Ensure browse-url-secondary-browser-function is set as warbo-web.el would
                    (browse-url-secondary-browser-function #'browse-url-firefox))
            (with-temp-buffer
              (eww-mode) ;; Activate eww-mode

              ;; Simulate pressing '&' in eww-mode
              (execute-kbd-macro (kbd "&"))

              ;; Assert that browse-url-firefox was called with the correct URL
              (should (equal captured-url test-url)))))
      ;; Restore original function definition
      (fset 'browse-url-firefox original-browse-url-firefox))))

(provide 'web-tests)
;;; web-tests.el ends here

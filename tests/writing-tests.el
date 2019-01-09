(defmacro with-test-file (ext &rest body)
  "Create a temporary file with extension EXT, open it in a temporary buffer,
   evaluate BODY, then delete the file."
  `(let ((name nil))
     (unwind-protect
         (progn
           ;; Generate a unque name ending with .EXT
           (setq name (make-temp-file "test" nil (concat "." ,ext)))

           ;; Touch the file
           (with-temp-file name)

           ;; Re-open using find-file, to be more realistic
           (save-excursion
             (find-file name)
             ,@body))
       (when name (delete-file name)))))

(ert-deftest warbo-tex-comfortable ()
  "Make sure the desired modes, etc. are activated for TeX files."
  (with-test-file "tex"
        (set-auto-mode)
        (should (equal major-mode 'latex-mode))
        (should     visual-line-mode)
        (should     flyspell-mode)
        (should-not whitespace-mode)

        (save-excursion
          (describe-bindings)
          (with-current-buffer "*Help*"
            (goto-char 0)
            (search-forward "<f9>")
            (search-forward "c")
            (backward-char)
            (let ((binding (buffer-substring-no-properties
                            (point)
                            (+ (point) (length "compile-with-make")))))
              (should (equal binding "compile-with-make")))))))

(ert-deftest warbo-markdown-comfortable ()
  "Make sure the desired modes, etc. are activated for Markdown files."
  (with-test-file "md"
       (set-auto-mode)
       (should     visual-line-mode)
       (should     flyspell-mode)
       (should-not whitespace-mode)))

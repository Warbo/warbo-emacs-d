;;; bytecode.el --- Sensible byte-compilation for personal config -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file sets up byte-compilation for the files in the `personal`
;; directory. It compiles them into a version-specific cache directory to
;; avoid conflicts when switching between different Emacs versions.

;;; Code:

;; Always load newest byte code
(setq load-prefer-newer t)

(defun warbo-bytecode-compile-directory (source-dir target-cache-dir)
  "Compile Emacs Lisp files from SOURCE-DIR into TARGET-CACHE-DIR."
  (add-to-list 'load-path target-cache-dir)
  (make-directory target-cache-dir t)

  ;; Temporarily override the function that decides where to put .elc files,
  ;; then recompile the personal directory.
  (let ((byte-compile-dest-file-function
         (lambda (file)
           (let* ((rel (file-relative-name file source-dir))
                  (dest (expand-file-name rel target-cache-dir)))
             (make-directory (file-name-directory dest) t)
             dest))))
    (message "Byte-compiling files from %s into %s..." source-dir target-cache-dir)
    (byte-recompile-directory source-dir 0)))

(provide 'bytecode)

;;; bytecode.el ends here

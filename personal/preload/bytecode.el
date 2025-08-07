;;; bytecode.el --- Sensible byte-compilation for personal config -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file sets up byte-compilation for the files in the `personal`
;; directory. It compiles them into a version-specific cache directory to
;; avoid conflicts when switching between different Emacs versions.

;;; Code:

;; Always load newest byte code
(setq load-prefer-newer t)

;; Compile personal files into a version-specific cache directory.
;; This is the standard way to avoid "magic number mismatch" errors
;; when switching between different Emacs versions.
(let* ((personal-dir (expand-file-name "personal" user-emacs-directory))
       (cache-dir (expand-file-name (format "personal-cache/%s/" emacs-version)
                                    user-emacs-directory)))
  ;; Add the version-specific cache to the load-path.
  (add-to-list 'load-path cache-dir)
  (make-directory cache-dir t)

  ;; Temporarily override the function that decides where to put .elc files,
  ;; then recompile the personal directory.
  (let ((byte-compile-dest-file-function
         (lambda (file)
           (let* ((rel (file-relative-name file personal-dir))
                  (dest (expand-file-name rel cache-dir)))
             (make-directory (file-name-directory dest) t)
             dest))))
    (message "Byte-compiling personal files into %s..." cache-dir)
    (byte-recompile-directory personal-dir 0)))

(provide 'bytecode)

;;; bytecode.el ends here

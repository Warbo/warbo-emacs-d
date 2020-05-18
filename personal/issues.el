;;; issues --- Emacs UI for the Artemis issue tracker

;;; Commentary:

;; Provide a 'list-issues' command for interacting with Artemis within Emacs

(require 'seq)

;;; Code:

(defvar issue-artemis-command-output nil
  "The output of 'artemis list -a'.

   We use dynamic scope to look this up, as a form of dependency injection.
   This lets us test functions without invoking commands.")

;; These macros can be used for dependency injection: they set the variable
;; 'issue-artemis-command-output' to either a string or the result of an
;; 'artemis list' command.

(defmacro issue-with-artemis-strings (strs &rest body)
  "Use the list of strings STRS as artemis command output and run BODY."
  `(let ((issue-artemis-command-output ,strs))
     ,@body))

(defun issue-artemis-list ()
  "Get the output of an actual 'artemis list' command."
  (split-string (shell-command-to-string "artemis list -a -o latest") "\n"))

;; Parse artemis command output into useful datastructures

(defun issue-parse-line (line)
  "Given a LINE from 'artemis list', return its components."
  ;; Regexen
  (let* ((as-group   (lambda (r) (concat "\\(" r "\\)")))
         (r-id       (funcall as-group "[0-9a-f]\\{16\\}"))
         (r-comments (concat "( *" (funcall as-group "[0-9]+") ")"))
         (r-status   (concat "\\[" (funcall as-group  "[^]]*") "\\]"))
         (r-desc     (funcall as-group ".*"))
         (r-line     (concat "^" r-id " " r-comments " "
                             r-status ": " r-desc "$")))

    ;; Components of the line
    (string-match r-line line)
    (let* ((id            (match-string-no-properties 1 line))
           (comments      (match-string-no-properties 2 line))
           (status        (match-string-no-properties 3 line))
           (description   (match-string-no-properties 4 line))
           (comment-count (string-to-number comments)))
      `(id            ,id
        comment-count ,comment-count
        status        ,status
        description   ,description))))

(defun issue-artemis-lines ()
  "Parse all of the lines from an 'artemis list' command."
  (mapcar 'issue-parse-line
          (seq-filter (lambda (line) (not (string= "" line)))
                      (issue-artemis-list))))

(defun issue-list-ids ()
  "Return a list of all artemis issue IDs."
  (mapcar (lambda (line) (plist-get line 'id))
          (issue-artemis-lines)))

(defun issue-get-line (id)
  "Get the artemis line containing the given ID.

   This is low-level, since it is missing information like the time of the last
   update.  Its advantage is only requiring 'artemis list' output."
  (car (seq-filter (lambda (line) (equal (plist-get line 'id) id) )
                   (issue-artemis-lines))))

(defun issue-comment-count (id)
  "Return the number of comments on the issue with the given ID."
  (plist-get (issue-get-line id) 'comment-count))

(defun issue-get-comment (id index)
  "Return the comment of issue ID with INDEX."
  (shell-command-to-string
   (concat "artemis show " id " " (number-to-string index))))

(defun issue-comments (id)
  "Return a list of all comments on the issue with the given ID."
  (cdr (issue-chain id)))

(defun issue-chain-raw (id)
  "Return a list of the issue with the given ID and each of its comments."
  (let ((range (number-sequence 0 (issue-comment-count id)))
        (cmd   (lambda (index) (issue-get-comment id index))))
    (mapcar cmd range)))

(defun issue-parse-comment (str)
  "Pull details out of raw issue/comment text STR."
  (let* ((lines (split-string str "\n"))
         (get   (lambda (header)
                  (let ((found (car (seq-filter
                                     (lambda (line)
                                       (string-prefix-p header line))
                                     lines))))
                    (if found
                        (substring found (length header))
                      nil))))
         (date (parse-time-string (funcall get "Date: "))))
    `(date        ,date
      date-string ,(concat (number-to-string (nth 5 date)) "-"
                           (format "%0.2d"   (nth 4 date)) "-"
                           (format "%0.2d"   (nth 3 date))))))

(defun issue-chain (issue)
  "Return a list of details parsed from ISSUE and its comments."
  (mapcar 'issue-parse-comment (issue-chain-raw issue)))

(defun issue-details (id)
  "Return all details of the given issue ID."
  (let* ((all-fields   (issue-chain id))
         (date-strings (mapcar (lambda (entry)
                                 (plist-get entry 'date-string))
                               all-fields))
         (latest  (car (sort date-strings
                             (lambda (x y) (string-greaterp x y))))))
    (append (issue-get-line id) `(updated ,latest))))

(defun issue-all-details ()
  "Return all details of all artemis issues."
  (mapcar 'issue-details (issue-list-ids)))

(defun issues-compare-numeric (x y)
  "Compare strings X and Y containing numbers."
  (<= (string-to-number x) (string-to-number y)))

(define-derived-mode issues-mode tabulated-list-mode "issues-mode"
  "Major mode issues-mode for interacting with Artemis issues"
  (setq tabulated-list-format [("Updated"     10 t                      )
                               ("Status"      8  t                      )
                               ("ID"          16 t                      )
                               ("Comments"    8  'issues-compare-numeric)
                               ("Description" 0  nil                    )])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Updated" t))
  (tabulated-list-init-header))

(defun issues-root-directory ()
  "Return the artemis .issues directory for the current git repo.

   This assumes that we're in a git repo, since it runs git commands."
  (pcase (split-string (shell-command-to-string "git rev-parse --show-toplevel")
                       "\n")
    (`(,root . ,_) (concat root "/.issues"))))

(defun issues-issue-directory (issue)
  "Get the .issues directory for the given ISSUE."
  (concat (issues-root-directory) "/" issue "/new"))

(defun issues-issue-files (issue)
  "Return (filename contents) pairs for files associated with ISSUE."
  (let* ((attrs    (directory-files-and-attributes
                    (issues-issue-directory issue)
                    t))
         (is-file  (lambda (x) (pcase x (`(,_ nil . ,_) t) (_ nil))))
         (get      (lambda (x) (pcase x
                                 (`(,f . ,_) (list f (issues-read-file f)))))))
    (mapcar get (seq-filter is-file attrs))))

(defun issues-read-file (f)
  "Read the contents of file F as a string."
  (with-temp-buffer
    (insert-file-contents f)
    (buffer-substring-no-properties
     (point-min)
     (point-max))))

(defun issues-read-message-id (data)
  "Read the Message-Id header from the given issue file DATA."
  (let* ((header      "Message-Id: ")
         (header-line (lambda (line) (string-prefix-p header line))))
    (cadr (split-string
           (car (split-string
                 (substring
                  (car (seq-filter header-line
                                   (split-string data "\n")))
                  (length header))
                 "@"))
           "-"))))

(defun issues-file-map-entry (entry)
  "Given ENTRY (filename contents), return a (message-id filename) pair."
  (list (issues-read-message-id (cadr entry) )
        (car entry)))

(defun issues-file-map (issue)
  "Return a map of ISSUE files: "
  (mapcar 'issues-file-map-entry (issues-issue-files issue)))

(defun issues-show-issue ()
  "Get the issue under point and open its associated file."
  (interactive)
  (pcase (tabulated-list-get-entry)
    (`[,_ ,_ ,id ,_ ,_] (let* ((map   (issues-file-map id))
                               (first (cadr (assoc "0" map))))
                          (find-file first)
                          (message-mode)
                          (read-only-mode)))))

(defvar issues-mode-map (make-sparse-keymap) "Keymap for `issues-mode'.")
(define-key issues-mode-map (kbd "RET") 'issues-show-issue)

(defun list-issues ()
  "Entry point for artemis UI."
  (interactive)
  (pop-to-buffer "*issues*" nil)
  (issues-mode)
  (use-local-map issues-mode-map)
  (setq tabulated-list-entries
        (mapcar
         (lambda (details)
           (let ((id (plist-get details 'id)))
             `(,id [,(plist-get details 'updated)
                    ,(plist-get details 'status)
                    ,id
                    ,(number-to-string (plist-get details 'comment-count))
                    ,(plist-get details 'description)])))
         (issue-all-details)))
  (tabulated-list-print t))

(provide 'issues)
;;; issues.el ends here

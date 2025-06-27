;;; warbo-issues --- Emacs UI for the Artemis issue tracker

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
           (comments      (match-string-no-properties 3 line)) ; Corrected index for the number
           (status        (match-string-no-properties 4 line)) ; Corrected index for status
           (description   (match-string-no-properties 5 line)) ; Corrected index for description
           (comment-count (string-to-number comments)))
      `(id            ,id
        index         0
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
        (cmd   (lambda (index) (list index (issue-get-comment id index)))))
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
         (date  (parse-time-string (funcall get "Date: ")))
         (msgid (cadr (split-string (funcall get "Message-Id: ")
                                    "-"))))
    `(date        ,date
      date-string ,(concat (number-to-string (nth 5 date)) "-"
                           (format "%0.2d"   (nth 4 date)) "-"
                           (format "%0.2d"   (nth 3 date)))
      message-id  ,msgid)))

(defun issue-chain (issue)
  "Return a list of details parsed from ISSUE and its comments."
  (mapcar (lambda (details)
            (list (car details) (issue-parse-comment (cadr details))))
          (issue-chain-raw issue)))

(defun issue-last-updated (id)
  "Look through ID and its comments and return the latest date."
  (let* ((all-fields (issue-chain id))
         (timestamps (mapcar (lambda (entry)
                               (plist-get (cadr entry) 'date))
                             all-fields)))
    (car (sort timestamps
               (lambda (x y) (string-greaterp (issues-timestamp-to-iso x)
                                              (issues-timestamp-to-iso y)))))))

(defun issue-all-details ()
  "Return all details of all artemis issues, including comments."
  (apply
   'append
   (mapcar
    (lambda (id)
      (let* ((updated     (issue-last-updated id))
             (issue-line  (issue-get-line     id))
             (raw-details (car   (issue-chain id)))
             (issue-details
              (issues-append-sort-key
               (append issue-line
                       `(issue ,id
                         index 0
                         updated ,updated
                         date  ,(plist-get (cadr raw-details) 'date-string)))))
             (comments
              (mapcar
               (lambda (comment)
                 (let ((index   (car  comment))
                       (details (cadr comment)))
                   (issues-append-sort-key
                    `(id            ,(plist-get       details 'message-id   )
                      date          ,(plist-get       details 'date-string  )
                      status        ,(plist-get issue-details 'status       )
                      issue         ,(plist-get issue-details 'id           )
                      updated       ,(plist-get issue-details 'updated      )
                      comment-count ,(plist-get issue-details 'comment-count)
                      index         ,index
                      description   ""))))
               (issue-comments id))))
        (cons issue-details comments)))
    (issue-list-ids))))

(defun issues-compare-numeric (x y)
  "Compare strings X and Y containing numbers."
  (<= (string-to-number x) (string-to-number y)))

(defun issues-make-sort-key (details)
  "Given DETAILS of an item, return a sorting key.
When sorted lexicographically, these keys should put issues in order of
last-updated-time, whilst keeping comments in index order beneath their issue."
  (let* ((index   (plist-get details 'index        ))
         (total   (plist-get details 'comment-count))
         (updated (plist-get details 'updated      ))
         (order   (- total index)))
    (format "%s%0.2d" (issues-timestamp-to-iso updated) order)))

(defun issues-append-sort-key (details)
  "Send DETAILS into 'issues-make-sort-key' and append the result."
  (append details (list 'sort-key (issues-make-sort-key details))))

(defun issues-timestamp-to-iso (timestamp)
  "Convert the given TIMESTAMP, as produced by 'parse-time-string', to ISO8601.
Any timezone information is ignored; we assume the timestamp is UTC."
  (apply 'format
         (cons
          "%0.4d-%0.2d-%0.2dT%0.2d:%0.2d:%0.2dZ"
          ;; Timestamp format is (SEC MIN HOUR DAY MON YEAR DOW DST TZ)
          ;;                      0   1   2    3   4   5    6   7   8
          (mapcar (lambda (n) (nth n timestamp)) '(5 4 3 2 1 0)))))

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
    (cadr (split-string (substring
                         (car (seq-filter header-line
                                          (split-string data "\n")))
                         (length header))
                        "-"))))

(defun issues-file-map-entry (entry)
  "Given ENTRY (filename contents), return a (message-id filename) pair."
  (list (issues-read-message-id (cadr entry) )
        (car entry)))

(defun issues-file-map (issue)
  "Return an alist of files relating to ISSUE: (message-id filename) pairs."
  (mapcar 'issues-file-map-entry (issues-issue-files issue)))

(defun issues-current-issue ()
  "Find an artemis issue ID from the current context (open issue or list)."
  (pcase major-mode
    ('issues-mode (pcase (tabulated-list-get-entry)
                    (`[,_ ,_ ,_ ,issue ,id ,index ,_]
                     (if (equal index "0")
                         (list issue index)
                         (list issue id)))))

    ('issues-read-mode
     (let ((issue (car (split-string
                        (nth 1 (split-string default-directory
                                             (regexp-quote "/.issues/")))
                        "/")))
           (details (issues-parse-comment (current-buffer))))
       (list issue (plist-get details 'message-id))))))

(defun issues-add-comment ()
  "Run 'artemis add XXX', taking the issue ID from the current context."
  (interactive)
  (let ((issue (issues-current-issue)))
    (call-process "artemis" nil 0 nil "add" issue)))

(defun issues-close ()
  "Close the issue under point."
  (interactive)
  (let ((issue (car (issues-current-issue))))
    (call-process "artemis" nil 0 nil "close" issue)))

;;; Modes

(defvar issues-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET"    ) 'issues-show-issue )
    (define-key map (kbd "C-c C-c") 'issues-add-comment)
    (define-key map (kbd "C-c C-k") 'issues-close)
    map)
  "Keymap for `issues-mode'.")

(define-derived-mode issues-mode tabulated-list-mode "issues-mode"
  "Major mode issues-mode for interacting with Artemis issues"
  (setq tabulated-list-format [("Sort"        4  t                      )
                               ("Date"        10 t                      )
                               ("Status"      8  t                      )
                               ("Issue"       16 t                      )
                               ("ID"          3  t                      )
                               ("Index"       5  'issues-compare-numeric)
                               ("Description" 0  nil                    )])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Sort" t))
  (tabulated-list-init-header))

(defvar issues-read-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "q"      ) 'kill-current-buffer)
    (define-key map (kbd "C-c C-c") 'issues-add-comment)
    (define-key map (kbd "C-c C-k") 'issues-close)
    map)
  "Keymap for issues-read-mode.")

(define-derived-mode issues-read-mode message-mode "Issues-Read"
  "Major mode for reading Artemis issues (which are maildir under the hood)."
  ;;(set-syntax-table artemis-mode-syntax-table)\
  (read-only-mode))

;;; Commands

(defun issues-show-issue ()
  "Get the issue under point and open its associated file."
  (interactive)
  (let* ((issue-part (issues-current-issue))
         (issue      (car  issue-part))
         (part       (cadr issue-part))
         (map        (issues-file-map issue))
         (first      (cadr (assoc part map))))
    (find-file first)
    (issues-read-mode)))

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
             `(,id [,(plist-get details 'sort-key)
                    ,(plist-get details 'date    )
                    ,(plist-get details 'status  )
                    ,(plist-get details 'issue   )
                    ,id
                    ,(number-to-string (plist-get details 'index))
                    ,(plist-get details 'description)])))
         (issue-all-details)))
  (tabulated-list-print t))

(provide 'warbo-issues)
;;; warbo-issues.el ends here

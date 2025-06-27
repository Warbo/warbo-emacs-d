;; Test our pure functions

(ert-deftest warbo-issues-can-parse-line ()
  (let* ((id            "ead7eaa500b8d729")
         (comment-count 1)
         (status        "resolved")
         (description   "I am a description")
         (line          (concat id
                                " (" (number-to-string comment-count) ")"
                                " [" status "]"
                                ": " description))
         (parsed        (issue-parse-line line)))
    (dolist (want `((id            . ,id)
                    (comment-count . ,comment-count)
                    (status        . ,status)
                    (description   . ,description)))
      (should (equal (plist-get parsed (car want)) (cdr want))))))

(ert-deftest warbo-issues-can-parse-comment ()
  (let* ((example "======================================================================
Subject: Loop through repos
State: resolved
----------------------------------------------------------------------
Comment: 1
From: Chris Warburton
Date: Fri, 14 Feb 2020 22:26:59 +0000
Subject: Re: Loop through repos
Message-Id: <de5a1245372439c0-c3aa3beb1e69f7c5-artemis@Chriss-MacBook-Pro.local>
References: <de5a1245372439c0-0-artemis@Chriss-MacBook-Pro.local>
In-Reply-To: <de5a1245372439c0-0-artemis@Chriss-MacBook-Pro.local>

Done

----------------------------------------------------------------------
")
         (parsed (issue-parse-comment example))
         (date   (plist-get parsed 'date))
         (y-m-d  (mapcar (lambda (n) (nth n date)) '(5 4 3))))
    (should (equal y-m-d '(2020 2 14)))))

(ert-deftest warbo-issues-format-timestamp ()
  (should (equal "2020-06-07T01:02:03Z"
                 (issues-timestamp-to-iso '(3 2 1 7 6 2020 nil nil nil)))))

(ert-deftest warbo-issues-sort-key ()
  (should (equal (issues-make-sort-key
                  '(updated       (12 23 4 5 6 2020 nil nil nil)
                    date          "2020-08-09"
                    comment-count 3
                    index         1))
                 "2020-06-05T04:23:12Z02")))

;; Test functions which call shell commands by shadowing those shell invocations
;; with dynamic bindings

(defun unlines (&rest lines)
  "Join LINES with newlines."
  (s-join "\n" lines))

(defvar warbo-issues-examples
  `(lines ((id "0000000000000001" comment-count 3 status "new"      description "Desc1")
           (id "0000000000000002" comment-count 2 status "resolved" description "Desc2")
           (id "0000000000000003" comment-count 0 status "new"      description "Desc3"))

    files ("0000000000000001" (,(unlines "Date: 2020-01-01 00:00:00 +0000"
                            "Message-Id: <0000000000000001-0-artemis@example.com>"
                            "Subject: issue1"
                            "First post 1")
                  ,(unlines "Date: 2020-01-02 00:00:00 +0000"
                            "Message-Id: <0000000000000001-1-artemis@example.com>"
                            "Subject: issue1"
                            "Comment 1-1")
                  ,(unlines "Date: 2020-01-03 00:00:00 +0000"
                            "Message-Id: <0000000000000001-2-artemis@example.com>"
                            "Subject: issue1"
                            "Comment 1-2")
                  ,(unlines "Date: 2020-01-04 00:00:00 +0000"
                            "Message-Id: <0000000000000001-3-artemis@example.com>"
                            "Subject: issue1"
                            "Comment 1-3"))
           "0000000000000002" (,(unlines "Date: 2020-02-01 00:00:00 +0000"
                            "Message-Id: <0000000000000002-0-artemis@example.com>"
                            "Subject: issue2"
                            "First post 2")
                  ,(unlines "Date: 2020-02-02 00:00:00 +0000"
                            "Message-Id: <0000000000000002-1-artemis@example.com>"
                            "Subject: issue2"
                            "Comment 2-1")
                  ,(unlines "Date: 2020-02-03 00:00:00 +0000"
                            "Message-Id: <0000000000000002-2-artemis@example.com>"
                            "Subject: issue2"
                            "Comment 2-2"))
           "0000000000000003" (,(unlines "Date: 2020-03-01 00:00:00 +0000"
                            "Message-Id: <0000000000000003-0-artemis@example.com>"
                            "Subject: issue3"
                            "First post 3")))))

(defun warbo-issues-mock-shell-command-to-string (examples command)
  "Mock `shell-command-to-string' using EXAMPLES data."
  (cond
   ((string= command "artemis list -a -o latest")
    (s-join "\n" (mapcar (lambda (line-plist)
                           ;; Format string adjusted to match real output spacing and empty status
                           (format "%s (%3s) [%s]: %s"
                                   (plist-get line-plist 'id)
                                   (plist-get line-plist 'comment-count)
                                   (plist-get line-plist 'status)
                                   (plist-get line-plist 'description)))
                         (plist-get examples 'lines))))
   ((string-prefix-p "artemis show " command)
    (let* ((parts (split-string command " "))
           (id    (nth 2 parts))
           (index (string-to-number (nth 3 parts)))
           (comments-list (lax-plist-get (plist-get examples 'files) id)))
      (nth index comments-list))) ; Get the index-th comment string
   ((string= command "git rev-parse --show-toplevel")
    "/mock/repo")
   (t
    (error "Unexpected shell command: %s" command))))

(defun warbo-issues-mock-directory-files-and-attributes (examples dir &rest args)
  "Mock `directory-files-and-attributes' using EXAMPLES data."
  (let* ((parts (split-string dir "/"))
         (issue-id (nth (- (length parts) 2) parts)) ; Assumes dir is like /mock/repo/.issues/ID/new
         (files (lax-plist-get (plist-get examples 'files) issue-id)))
    (mapcar (lambda (index)
              (list (concat dir "/" (number-to-string index)) nil)) ; Return dummy attributes
            (range 0 (length files)))))

(defun warbo-issues-mock-insert-file-contents (examples file &rest args)
  "Mock `insert-file-contents' using EXAMPLES data."
  (let* ((parts (split-string file "/"))
         (issue-id (nth (- (length parts) 2) parts)) ; Assumes file is like /mock/repo/.issues/ID/new/INDEX
         (index (string-to-number (file-name-nondirectory file)))
         (content (nth index (lax-plist-get (plist-get examples 'files) issue-id))))
    (insert content)))

(defmacro inject-examples (examples &rest body)
  "Replace effectful procedures by returning EXAMPLES, when running BODY."
  `(cl-letf (((symbol-function 'shell-command-to-string)
              (lambda (command &rest args)
                (warbo-issues-mock-shell-command-to-string ,examples command)))
             ((symbol-function 'directory-files-and-attributes)
              (lambda (dir &rest args)
                (apply 'warbo-issues-mock-directory-files-and-attributes ,examples dir args)))
             ((symbol-function 'insert-file-contents)
              (lambda (file &rest args)
                (apply 'warbo-issues-mock-insert-file-contents ,examples file args))))
     ,@body))

(defmacro with-examples (&rest body)
  "Run BODY with some example issues."
  `(inject-examples warbo-issues-examples
     ,@body))

(ert-deftest warbo-issues-can-list-ids ()
  (with-examples
   (should (equal (issue-list-ids) '("0000000000000001" "0000000000000002" "0000000000000003")))))

(ert-deftest warbo-issues-can-lookup-details ()
  (with-examples
   (should (equal (issue-get-line "0000000000000002")
                  '(id            "0000000000000002"
                    comment-count 2
                    status        "resolved"
                    description   "Desc2")))))

(ert-deftest warbo-issues-can-get-comment ()
  (with-examples
   (should (equal (issue-get-comment "0000000000000001" 2)
                  (nth 2 (lax-plist-get (plist-get warbo-issues-examples 'files)
                                        "0000000000000001"))))))

(ert-deftest warbo-issues-can-lookup-comments ()
  (with-examples
   (let ((get-date-string (lambda (entry) (plist-get entry 'date-string))))
     (dolist
         (id-want '(("0000000000000001" . ("2020-01-02" "2020-01-03" "2020-01-04"))
                    ("0000000000000002" . ("2020-02-02" "2020-02-03"))
                    ("0000000000000003" . nil)))
       (should (equal (mapcar get-date-string
                              (mapcar 'cadr
                                      (issue-comments (car id-want))))
                      (cdr id-want)))))))

(ert-deftest warbo-issues-includes-comments-in-list ()
  (with-examples
   (let ((details (issue-all-details)))
     ;; TODO: Calculate desired length from warbo-issues-examples
     (should (equal (length details) (+ 4 3 1))))))

(ert-deftest warbo-issues-shows-fields-when-listing ()
  (with-examples
   (dolist (line (issue-all-details))
     (should (plist-get line 'id         ))
     (should (plist-get line 'index      ))
     (should (plist-get line 'status     ))
     (should (plist-get line 'date       ))
     (should (plist-get line 'sort-key   ))
     (should (plist-get line 'description)))))

;; Test the top-level invocation

(ert-deftest warbo-issues-defined ()
  "Have issues commands"
  (should (fboundp 'list-issues)))

(ert-deftest warbo-issues-can-list-issues ()
  "Can invoke list-issues"
  (with-examples
   (list-issues)))

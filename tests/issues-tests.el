;; Test our pure functions

(ert-deftest warbo-issues-can-parse-line ()
  (let* ((id            "ead7eaa500b8d729")
         (comment-count 1)
         (status        "resolved")
         (description   "I am a description")
         (line          (concat id
                                " (  " (number-to-string comment-count) ")"
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
  `(lines ((id "id1" comment-count 3 status "new"      description "Desc1")
           (id "id2" comment-count 2 status "resolved" description "Desc2")
           (id "id3" comment-count 0 status "new"      description "Desc3"))

    files ("id1" (,(unlines "Date: 2020-01-01 00:00:00 +0000"
                            "Message-Id: <id1-0-artemis@example.com>"
                            "Subject: issue1"
                            "First post 1")
                  ,(unlines "Date: 2020-01-02 00:00:00 +0000"
                            "Message-Id: <id1-1-artemis@example.com>"
                            "Subject: issue1"
                            "Comment 1-1")
                  ,(unlines "Date: 2020-01-03 00:00:00 +0000"
                            "Message-Id: <id1-2-artemis@example.com>"
                            "Subject: issue1"
                            "Comment 1-2")
                  ,(unlines "Date: 2020-01-04 00:00:00 +0000"
                            "Message-Id: <id1-3-artemis@example.com>"
                            "Subject: issue1"
                            "Comment 1-3"))
           "id2" (,(unlines "Date: 2020-02-01 00:00:00 +0000"
                            "Message-Id: <id2-0-artemis@example.com>"
                            "Subject: issue2"
                            "First post 2")
                  ,(unlines "Date: 2020-02-02 00:00:00 +0000"
                            "Message-Id: <id2-1-artemis@example.com>"
                            "Subject: issue2"
                            "Comment 2-1")
                  ,(unlines "Date: 2020-02-03 00:00:00 +0000"
                            "Message-Id: <id2-2-artemis@example.com>"
                            "Subject: issue2"
                            "Comment 2-2"))
           "id3" (,(unlines "Date: 2020-03-01 00:00:00 +0000"
                            "Message-Id: <id3-0-artemis@example.com>"
                            "Subject: issue3"
                            "First post 3")))))

(defmacro inject-examples (examples &rest body)
  "Replace effectful procedures by returning EXAMPLES, when running BODY."
  `(cl-flet ((issue-artemis-lines ()
               (plist-get ,examples 'lines))
             (issue-get-comment   (id index)
               (nth index (lax-plist-get
                           (plist-get ,examples 'files)
                           id)))
          (issues-issue-files  (issue)
            (let ((files (lax-plist-get
                          (plist-get examples 'files)
                          issue)))
              (mapcar (lambda (index)
                        (list (concat
                               issue "/"
                               (number-to-string index))
                              (nth index files)))
                      (range 0 (length files))))))
     ,@body))

(defmacro with-examples (&rest body)
  "Run BODY with some example issues."
  `(inject-examples warbo-issues-examples
    (progn ,@body)))

(ert-deftest warbo-issues-can-list-ids ()
  (with-examples
   (should (equal (issue-list-ids) '("id1" "id2" "id3")))))

(ert-deftest warbo-issues-can-lookup-details ()
  (with-examples
   (should (equal (issue-get-line "id2")
                  '(id            "id2"
                    comment-count 2
                    status        "resolved"
                    description   "Desc2")))))

(ert-deftest warbo-issues-can-get-comment ()
  (with-examples
   (should (equal (issue-get-comment "id1" 2)
                  (nth 2 (lax-plist-get (plist-get warbo-issues-examples 'files)
                                        "id1"))))))

(ert-deftest warbo-issues-can-lookup-comments ()
  (with-examples
   (let ((get-date-string (lambda (entry) (plist-get entry 'date-string))))
     (dolist
         (id-want '(("id1" . ("2020-01-02" "2020-01-03" "2020-01-04"))
                    ("id2" . ("2020-02-02" "2020-02-03"))
                    ("id3" . nil)))
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

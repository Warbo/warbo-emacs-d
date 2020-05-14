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

;; Test functions which call shell commands by shadowing those shell invocations
;; with dynamic bindings

(defmacro with-issue-lines (entries &rest body)
  "Override 'artemis list' parsing to return ENTRIES when running BODY."
  `(flet ((issue-artemis-lines () ,entries))
     ,@body))

(ert-deftest warbo-issues-lines-macro ()
  (with-issue-lines
   '(a b c)
   (should (equal (issue-artemis-lines) '(a b c)))))

(ert-deftest warbo-issues-can-list-ids ()
  (with-issue-lines
   '((id "id1") (id "id2"))
   (should (equal (issue-list-ids) '("id1" "id2")))))

(ert-deftest warbo-issues-can-lookup-details ()
  (with-issue-lines
   '((id "id1" foo "foo") (id "id2" foo "bar"))
   (should (equal (issue-get-line "id2")
                  '(id "id2" foo "bar")))))

(defmacro with-issue-comments (comments &rest body)
  "Take comments from the mapping COMMENTS when running BODY."
  `(flet ((issue-get-comment (id index)
                             (nth index
                                  (lax-plist-get ,comments id))))
     ,@body))

(defvar warbo-issues-examples
  '("id1" ("Date: 2020-01-01\nSubject: issue1\nFirst post 1"
           "Date: 2020-01-02\nSubject: issue1\nComment 1-1"
           "Date: 2020-01-03\nSubject: issue1\nComment 1-2"
           "Date: 2020-01-04\nSubject: issue1\nComment 1-3")
    "id2" ("Date: 2020-02-01\nSubject: issue2\nFirst post 2"
           "Date: 2020-02-02\nSubject: issue2\nComment 2-1"
           "Date: 2020-02-03\nSubject: issue2\nComment 2-2")
    "id3" ("Date: 2020-03-01\nSubject: issue3\nFirst post 3")))

(defmacro with-examples (&rest body)
  "Run BODY with some example issues."
  `(with-issue-comments
    warbo-issues-examples
    (with-issue-lines
     '((id "id1" comment-count 3)
       (id "id2" comment-count 2)
       (id "id3" comment-count 0))
     (progn ,@body))))

(ert-deftest warbo-issues-comments-macro ()
  (with-examples
   (should (equal (issue-get-comment "id1" 2)
                  (nth 2 (lax-plist-get warbo-issues-examples "id1"))))))

(ert-deftest warbo-issues-can-lookup-comments ()
  (with-examples
   (let ((get-date-string (lambda (entry) (plist-get entry 'date-string))))
     (dolist
         (id-want '(("id1" . ("2020-01-02" "2020-01-03" "2020-01-04"))
                    ("id2" . ("2020-02-02" "2020-02-03"))
                    ("id3" . nil)))
       (should (equal (mapcar get-date-string (issue-comments (car id-want)))
                      (cdr id-want)))))))

;; Test the top-level invocation

(ert-deftest warbo-issues-defined ()
  "Have issues commands"
  (should (fboundp 'list-issues)))

(ert-deftest warbo-issues-can-list-issues ()
  "Can invoke list-issues"
  (with-issue-lines
   '((id "id1" comment-count 0 status "stat" description "Desc"))
   (list-issues)))


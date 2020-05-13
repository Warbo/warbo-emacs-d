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
                             (nth (1- index)
                                  (lax-plist-get ,comments id))))
     ,@body))

(ert-deftest warbo-issues-comments-macro ()
  (with-issue-comments
   '("id1" ("comment11" "comment12" "comment13")
     "id2" ("comment21" "comment22"))
   (should (equal (issue-get-comment "id1" 2) "comment12"))))

(ert-deftest warbo-issues-can-lookup-comments ()
  (with-issue-lines
   '((id "id1" comment-count 3)
     (id "id2" comment-count 2)
     (id "id3" comment-count 0))
   (with-issue-comments
    '("id1" ("comment11" "comment12" "comment13")
      "id2" ("comment21" "comment22"))
    (should (equal (issue-comments "id2")
                   '("comment21" "comment22")))
    (should (equal (issue-comments "id1")
                   '("comment11" "comment12" "comment13")))
    (should (equal (issue-comments "id3")
                   nil)))))

;; Test the top-level invocation

(ert-deftest warbo-issues-defined ()
  "Have issues commands"
  (should (fboundp 'list-issues)))

(ert-deftest warbo-issues-can-list-issues ()
  "Can invoke list-issues"
  (with-issue-lines
   '((id "id1" comment-count 0 status "stat" description "Desc"))
   (list-issues)))


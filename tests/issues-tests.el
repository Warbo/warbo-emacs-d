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

(ert-deftest warbo-issues-parse-line-zero-comments ()
  "Parsing a line with zero comments should work."
  (let* ((line "abcdef0123456789 (  0) [new]: Something to do")
         (parsed (issue-parse-line line)))
    (should (equal (plist-get parsed 'id) "abcdef0123456789"))
    (should (equal (plist-get parsed 'comment-count) 0))
    (should (equal (plist-get parsed 'status) "new"))
    (should (equal (plist-get parsed 'description) "Something to do"))))

(ert-deftest warbo-issues-parse-line-high-comment-count ()
  "Parsing a line with a high comment count should work."
  (let* ((line "abcdef0123456789 (123) [new]: Many comments")
         (parsed (issue-parse-line line)))
    (should (equal (plist-get parsed 'comment-count) 123))))

(ert-deftest warbo-issues-parse-line-empty-status ()
  "Parsing a line with an empty status field should return empty string."
  (let* ((line "abcdef0123456789 (  0) []: No status")
         (parsed (issue-parse-line line)))
    (should (equal (plist-get parsed 'status) ""))))

(ert-deftest warbo-issues-parse-line-returns-nil-for-garbage ()
  "Parsing a line that doesn't match should return nil."
  (should (null (issue-parse-line "this is not a valid line")))
  (should (null (issue-parse-line ""))))

(ert-deftest warbo-issues-parse-line-sets-index-to-zero ()
  "Parsed lines should always have index 0 (they represent the issue itself)."
  (let* ((line "abcdef0123456789 (  3) [new]: Test")
         (parsed (issue-parse-line line)))
    (should (equal (plist-get parsed 'index) 0))))

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

(ert-deftest warbo-issues-parse-comment-extracts-message-id ()
  "Parsing a comment should extract the middle part of the Message-Id."
  (let* ((example "Date: Mon, 01 Jun 2020 12:00:00 +0000
Message-Id: <abcdef0123456789-c3aa3beb1e69f7c5-artemis@host>
Subject: Test

Body text")
         (parsed (issue-parse-comment example)))
    (should (equal (plist-get parsed 'message-id) "c3aa3beb1e69f7c5"))))

(ert-deftest warbo-issues-parse-comment-date-string-format ()
  "The date-string field should be in YYYY-MM-DD format."
  (let* ((example "Date: Mon, 01 Jun 2020 12:00:00 +0000
Message-Id: <abcdef0123456789-0-artemis@host>
Subject: Test

Body")
         (parsed (issue-parse-comment example)))
    (should (equal (plist-get parsed 'date-string) "2020-06-01"))))

(ert-deftest warbo-issues-parse-comment-date-string-pads-months ()
  "Months and days should be zero-padded in date-string."
  (let* ((example "Date: Thu, 02 Jan 2020 03:04:05 +0000
Message-Id: <abcdef0123456789-0-artemis@host>
Subject: Test

Body")
         (parsed (issue-parse-comment example)))
    (should (equal (plist-get parsed 'date-string) "2020-01-02"))))

(ert-deftest warbo-issues-format-timestamp ()
  (should (equal "2020-06-07T01:02:03Z"
                 (issues-timestamp-to-iso '(3 2 1 7 6 2020 nil nil nil)))))

(ert-deftest warbo-issues-format-timestamp-pads-zeros ()
  "Timestamps should zero-pad all numeric components."
  (should (equal "0001-01-01T00:00:00Z"
                 (issues-timestamp-to-iso '(0 0 0 1 1 1 nil nil nil)))))

(ert-deftest warbo-issues-sort-key ()
  (should (equal (issues-make-sort-key
                  '(updated       (12 23 4 5 6 2020 nil nil nil)
                    date          "2020-08-09"
                    comment-count 3
                    index         1))
                 "2020-06-05T04:23:12Z02")))

(ert-deftest warbo-issues-sort-key-issue-itself ()
  "For index 0 (the issue itself), the trailing order should be the comment count."
  (should (equal (issues-make-sort-key
                  '(updated       (0 0 0 1 1 2020 nil nil nil)
                    comment-count 5
                    index         0))
                 "2020-01-01T00:00:00Z05")))

(ert-deftest warbo-issues-sort-key-last-comment ()
  "For the last comment, the trailing order should be 00."
  (should (equal (issues-make-sort-key
                  '(updated       (0 0 0 1 1 2020 nil nil nil)
                    comment-count 5
                    index         5))
                 "2020-01-01T00:00:00Z00")))

(ert-deftest warbo-issues-sort-key-ordering ()
  "Sort keys should order issues by update time descending, with comments in
index order beneath their issue."
  (let* ((newer-issue (issues-make-sort-key
                       '(updated (0 0 0 2 1 2020 nil nil nil)
                         comment-count 2 index 0)))
         (newer-c1    (issues-make-sort-key
                       '(updated (0 0 0 2 1 2020 nil nil nil)
                         comment-count 2 index 1)))
         (newer-c2    (issues-make-sort-key
                       '(updated (0 0 0 2 1 2020 nil nil nil)
                         comment-count 2 index 2)))
         (older-issue (issues-make-sort-key
                       '(updated (0 0 0 1 1 2020 nil nil nil)
                         comment-count 1 index 0)))
         (older-c1    (issues-make-sort-key
                       '(updated (0 0 0 1 1 2020 nil nil nil)
                         comment-count 1 index 1)))
         ;; Sort descending (reverse lexicographic) like the UI does
         (sorted (sort (list older-c1 newer-c2 older-issue newer-issue newer-c1)
                       'string-greaterp)))
    ;; Newer issue and its comments should come first
    (should (equal sorted (list newer-issue newer-c1 newer-c2
                                older-issue older-c1)))))

(ert-deftest warbo-issues-compare-numeric ()
  "issues-compare-numeric should compare stringified numbers numerically."
  (should (issues-compare-numeric "1" "2"))
  (should (issues-compare-numeric "2" "2"))
  (should-not (issues-compare-numeric "3" "2"))
  (should (issues-compare-numeric "0" "10")))

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
         ;; dir is like /mock/repo/.issues/ID/new => ID is second-to-last
         (issue-id (nth (- (length parts) 2) parts))
         (files (lax-plist-get (plist-get examples 'files) issue-id)))
    (mapcar (lambda (index)
              (list (concat dir "/" (number-to-string index)) nil)) ; Return dummy attributes
            (number-sequence 0 (1- (length files))))))

(defun warbo-issues-mock-insert-file-contents (examples file &rest args)
  "Mock `insert-file-contents' using EXAMPLES data."
  (let* ((parts (split-string file "/"))
         ;; file is like /mock/repo/.issues/ID/new/INDEX => ID is third-to-last
         (issue-id (nth (- (length parts) 3) parts))
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

;; Tests using mock data

(ert-deftest warbo-issues-can-list-ids ()
  (with-examples
   (should (equal (issue-list-ids) '("0000000000000001" "0000000000000002" "0000000000000003")))))

(ert-deftest warbo-issues-can-lookup-details ()
  (with-examples
   (should (equal (issue-get-line "0000000000000002")
                  '(id            "0000000000000002"
                    index         0
                    comment-count 2
                    status        "resolved"
                    description   "Desc2")))))

(ert-deftest warbo-issues-get-line-returns-nil-for-missing-id ()
  "Looking up an ID that doesn't exist should return nil."
  (with-examples
   (should (null (issue-get-line "9999999999999999")))))

(ert-deftest warbo-issues-can-get-comment ()
  (with-examples
   (should (equal (issue-get-comment "0000000000000001" 2)
                  (nth 2 (lax-plist-get (plist-get warbo-issues-examples 'files)
                                        "0000000000000001"))))))

(ert-deftest warbo-issues-get-comment-first-post ()
  "Getting comment index 0 should return the first post (the issue body)."
  (with-examples
   (should (string-match-p "First post 1"
                           (issue-get-comment "0000000000000001" 0)))))

(ert-deftest warbo-issues-comment-count ()
  "issue-comment-count should return the number of comments for an issue."
  (with-examples
   (should (equal (issue-comment-count "0000000000000001") 3))
   (should (equal (issue-comment-count "0000000000000002") 2))
   (should (equal (issue-comment-count "0000000000000003") 0))))

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

(ert-deftest warbo-issues-comments-exclude-first-post ()
  "issue-comments should return only comments, not the original issue post."
  (with-examples
   ;; Issue 1 has 3 comments (indices 1, 2, 3); index 0 is the issue itself
   (let ((comments (issue-comments "0000000000000001")))
     (should (equal (length comments) 3))
     ;; The first comment should have index 1, not 0
     (should (equal (car (car comments)) 1)))))

(ert-deftest warbo-issues-chain-includes-all-entries ()
  "issue-chain should return the issue post plus all comments."
  (with-examples
   (should (equal (length (issue-chain "0000000000000001")) 4))
   (should (equal (length (issue-chain "0000000000000002")) 3))
   (should (equal (length (issue-chain "0000000000000003")) 1))))

(ert-deftest warbo-issues-chain-raw-indices ()
  "issue-chain-raw should return entries with sequential indices starting at 0."
  (with-examples
   (let ((raw (issue-chain-raw "0000000000000001")))
     (should (equal (mapcar 'car raw) '(0 1 2 3))))))

(ert-deftest warbo-issues-chain-parses-dates ()
  "issue-chain should parse dates from each entry."
  (with-examples
   (let* ((chain (issue-chain "0000000000000001"))
          (dates (mapcar (lambda (entry)
                           (plist-get (cadr entry) 'date-string))
                         chain)))
     (should (equal dates '("2020-01-01" "2020-01-02" "2020-01-03" "2020-01-04"))))))

(ert-deftest warbo-issues-last-updated ()
  "issue-last-updated should return the most recent date across all comments."
  (with-examples
   ;; Issue 1's last comment is dated 2020-01-04
   (let ((updated (issue-last-updated "0000000000000001")))
     (should (equal (nth 5 updated) 2020))
     (should (equal (nth 4 updated) 1))
     (should (equal (nth 3 updated) 4)))
   ;; Issue 3 has no comments, so last updated is its own date (2020-03-01)
   (let ((updated (issue-last-updated "0000000000000003")))
     (should (equal (nth 5 updated) 2020))
     (should (equal (nth 4 updated) 3))
     (should (equal (nth 3 updated) 1)))))

(ert-deftest warbo-issues-includes-comments-in-list ()
  (with-examples
   (let ((details (issue-all-details)))
     ;; Issue 1: 1 issue + 3 comments = 4
     ;; Issue 2: 1 issue + 2 comments = 3
     ;; Issue 3: 1 issue + 0 comments = 1
     ;; Total = 8
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

(ert-deftest warbo-issues-all-details-issue-entries-have-correct-status ()
  "Each entry in issue-all-details should carry the status of its parent issue."
  (with-examples
   (let ((details (issue-all-details)))
     ;; All entries for issue 1 should be "new"
     (dolist (entry (seq-filter (lambda (d) (equal (plist-get d 'issue) "0000000000000001"))
                                details))
       (should (equal (plist-get entry 'status) "new")))
     ;; All entries for issue 2 should be "resolved"
     (dolist (entry (seq-filter (lambda (d) (equal (plist-get d 'issue) "0000000000000002"))
                                details))
       (should (equal (plist-get entry 'status) "resolved"))))))

(ert-deftest warbo-issues-all-details-comments-have-empty-description ()
  "Comments in issue-all-details should have empty description."
  (with-examples
   (let* ((details (issue-all-details))
          (comments (seq-filter (lambda (d) (> (plist-get d 'index) 0)) details)))
     (dolist (c comments)
       (should (equal (plist-get c 'description) ""))))))

(ert-deftest warbo-issues-all-details-issues-have-description ()
  "Issue entries (index 0) in issue-all-details should have a description."
  (with-examples
   (let* ((details (issue-all-details))
          (issues (seq-filter (lambda (d) (equal (plist-get d 'index) 0)) details)))
     (dolist (i issues)
       (should (not (equal (plist-get i 'description) "")))))))

(ert-deftest warbo-issues-all-details-sorted-by-sort-key ()
  "When sorted by sort-key descending, issues with newer updates come first,
and comments appear in index order within their issue."
  (with-examples
   (let* ((details (issue-all-details))
          (sorted  (sort (copy-sequence details)
                         (lambda (a b)
                           (string-greaterp (plist-get a 'sort-key)
                                            (plist-get b 'sort-key))))))
     ;; Check that issue 3 (updated 2020-03-01) comes before issue 2 (2020-02-03)
     ;; which comes before issue 1 (2020-01-04)
     (let ((issue-order (seq-uniq (mapcar (lambda (d) (plist-get d 'issue)) sorted))))
       (should (equal issue-order '("0000000000000003" "0000000000000002" "0000000000000001"))))
     ;; Within each issue group, indices should be in order 0, 1, 2, ...
     (dolist (issue-id '("0000000000000001" "0000000000000002" "0000000000000003"))
       (let ((indices (mapcar (lambda (d) (plist-get d 'index))
                              (seq-filter (lambda (d)
                                            (equal (plist-get d 'issue) issue-id))
                                          sorted))))
         (should (equal indices (number-sequence 0 (1- (length indices))))))))))

(ert-deftest warbo-issues-all-details-comment-count-propagated ()
  "All entries for an issue should carry the correct comment-count."
  (with-examples
   (let ((details (issue-all-details)))
     (dolist (entry (seq-filter (lambda (d) (equal (plist-get d 'issue) "0000000000000001"))
                                details))
       (should (equal (plist-get entry 'comment-count) 3)))
     (dolist (entry (seq-filter (lambda (d) (equal (plist-get d 'issue) "0000000000000003"))
                                details))
       (should (equal (plist-get entry 'comment-count) 0))))))

(ert-deftest warbo-issues-root-directory ()
  "issues-root-directory should append /.issues to the git root."
  (with-examples
   (should (equal (issues-root-directory) "/mock/repo/.issues"))))

(ert-deftest warbo-issues-issue-directory ()
  "issues-issue-directory should return the /new subdirectory for an issue."
  (with-examples
   (should (equal (issues-issue-directory "0000000000000001")
                  "/mock/repo/.issues/0000000000000001/new"))))

(ert-deftest warbo-issues-read-message-id ()
  "issues-read-message-id should extract the middle segment of a Message-Id."
  (let ((data "Date: 2020-01-01 00:00:00 +0000\nMessage-Id: <abc123-deadbeef-artemis@host>\nSubject: Test\n\nBody"))
    (should (equal (issues-read-message-id data) "deadbeef"))))

(ert-deftest warbo-issues-read-message-id-first-post ()
  "For the first post (index 0), the middle segment is '0'."
  (let ((data "Date: 2020-01-01 00:00:00 +0000\nMessage-Id: <abc123-0-artemis@host>\nSubject: Test\n\nBody"))
    (should (equal (issues-read-message-id data) "0"))))

(ert-deftest warbo-issues-file-map ()
  "issues-file-map should return (message-id filename) pairs for an issue."
  (with-examples
   (let ((fmap (issues-file-map "0000000000000001")))
     ;; Should have 4 entries (issue + 3 comments)
     (should (equal (length fmap) 4))
     ;; Each entry should be a list of (message-id filename)
     (dolist (entry fmap)
       (should (= (length entry) 2))
       (should (stringp (car entry)))
       (should (stringp (cadr entry)))))))

(ert-deftest warbo-issues-append-sort-key ()
  "issues-append-sort-key should add a sort-key to the plist."
  (let* ((details '(updated       (0 0 0 1 1 2020 nil nil nil)
                    comment-count 2
                    index         0))
         (result (issues-append-sort-key details)))
    (should (plist-get result 'sort-key))
    ;; Original fields should still be present
    (should (equal (plist-get result 'comment-count) 2))
    (should (equal (plist-get result 'index) 0))))

;; Test that issue-all-details makes the minimum number of subprocess calls

(ert-deftest warbo-issues-all-details-minimal-subprocess-calls ()
  "issue-all-details should call artemis list once and artemis show once per
comment/issue-post, not redundantly."
  (let ((call-counts (list (cons "artemis-list" 0)
                           (cons "artemis-show" 0))))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (command &rest args)
                 (cond
                  ((string= command "artemis list -a -o latest")
                   (cl-incf (cdr (assoc "artemis-list" call-counts)))
                   (warbo-issues-mock-shell-command-to-string
                    warbo-issues-examples command))
                  ((string-prefix-p "artemis show " command)
                   (cl-incf (cdr (assoc "artemis-show" call-counts)))
                   (warbo-issues-mock-shell-command-to-string
                    warbo-issues-examples command))
                  (t (warbo-issues-mock-shell-command-to-string
                      warbo-issues-examples command))))))
      (issue-all-details)
      ;; Should call "artemis list" exactly once
      (should (equal (cdr (assoc "artemis-list" call-counts)) 1))
      ;; Should call "artemis show" once per issue+comment:
      ;; Issue 1: 4 (index 0,1,2,3), Issue 2: 3 (0,1,2), Issue 3: 1 (0) = 8
      (should (equal (cdr (assoc "artemis-show" call-counts)) 8)))))

;; Test the top-level invocation

(ert-deftest warbo-issues-defined ()
  "Have issues commands"
  (should (fboundp 'list-issues)))

(ert-deftest warbo-issues-modes-defined ()
  "The issues modes should be defined."
  (should (fboundp 'issues-mode))
  (should (fboundp 'issues-read-mode)))

(ert-deftest warbo-issues-can-list-issues ()
  "Can invoke list-issues"
  (with-examples
   (list-issues)))

(ert-deftest warbo-issues-list-creates-buffer ()
  "list-issues should create a *issues* buffer with tabulated-list content."
  (with-examples
   (list-issues)
   (let ((buf (get-buffer "*issues*")))
     (should buf)
     (with-current-buffer buf
       (should (eq major-mode 'issues-mode))
       ;; Should have entries in the tabulated list
       (should (> (length tabulated-list-entries) 0))
       ;; Should have 8 entries total (4+3+1)
       (should (equal (length tabulated-list-entries) 8)))
     (kill-buffer buf))))

(ert-deftest warbo-issues-list-entries-have-seven-columns ()
  "Each tabulated-list entry should have 7 columns matching the format spec."
  (with-examples
   (list-issues)
   (let ((buf (get-buffer "*issues*")))
     (with-current-buffer buf
       (dolist (entry tabulated-list-entries)
         (let ((row (cadr entry)))
           (should (vectorp row))
           (should (equal (length row) 7)))))
     (kill-buffer buf))))

(ert-deftest warbo-issues-list-keybindings-work ()
  "The issues-mode keymap should have the expected bindings."
  (with-examples
   (list-issues)
   (let ((buf (get-buffer "*issues*")))
     (with-current-buffer buf
       (should (eq (lookup-key issues-mode-map (kbd "RET"))     'issues-show-issue))
       (should (eq (lookup-key issues-mode-map (kbd "C-c C-n")) 'issues-add-issue))
       (should (eq (lookup-key issues-mode-map (kbd "C-c C-c")) 'issues-add-comment))
       (should (eq (lookup-key issues-mode-map (kbd "C-c C-k")) 'issues-close)))
     (kill-buffer buf))))

(ert-deftest warbo-issues-add-issue-defined ()
  "issues-add-issue should be an interactive command."
  (should (fboundp 'issues-add-issue))
  (should (commandp 'issues-add-issue)))

(ert-deftest warbo-issues-list-shows-expected-headers ()
  "The issues list should display the correct column headers."
  (with-examples
   (list-issues)
   (let ((buf (get-buffer "*issues*")))
     (with-current-buffer buf
       ;; Check that tabulated-list-format contains the expected headers
       ;; tabulated-list-format is a vector where each element is a list (NAME WIDTH SORT)
       (let ((headers (mapcar (lambda (i) (car (elt tabulated-list-format i)))
                              (number-sequence 0 (1- (length tabulated-list-format))))))
         (should (equal headers '("Sort" "Date" "Status" "Issue" "ID" "Index" "Description")))))
     (kill-buffer buf))))

(ert-deftest warbo-issues-list-all-columns-configured ()
  "All expected columns should be present with correct configuration."
  (with-examples
   (list-issues)
   (let ((buf (get-buffer "*issues*")))
     (with-current-buffer buf
       ;; Verify we have exactly 7 columns
       (should (= (length tabulated-list-format) 7))

       ;; Verify each column's configuration
       (let ((sort-col   (elt tabulated-list-format 0))
             (date-col   (elt tabulated-list-format 1))
             (status-col (elt tabulated-list-format 2))
             (issue-col  (elt tabulated-list-format 3))
             (id-col     (elt tabulated-list-format 4))
             (index-col  (elt tabulated-list-format 5))
             (desc-col   (elt tabulated-list-format 6)))

         ;; Sort column: sortable with default comparison
         (should (equal (car sort-col) "Sort"))
         (should (eq (nth 2 sort-col) t))

         ;; Date column: sortable with default comparison
         (should (equal (car date-col) "Date"))
         (should (eq (nth 2 date-col) t))

         ;; Status column: sortable with default comparison
         (should (equal (car status-col) "Status"))
         (should (eq (nth 2 status-col) t))

         ;; Issue column: sortable with default comparison
         (should (equal (car issue-col) "Issue"))
         (should (eq (nth 2 issue-col) t))

         ;; ID column: sortable with default comparison
         (should (equal (car id-col) "ID"))
         (should (eq (nth 2 id-col) t))

         ;; Index column: sortable with numeric comparison
         (should (equal (car index-col) "Index"))
         ;; The sort predicate should be the issues-compare-numeric symbol
         (should (eq (nth 2 index-col) 'issues-compare-numeric))

         ;; Description column: not sortable
         (should (equal (car desc-col) "Description"))
         (should (eq (nth 2 desc-col) nil))))
     (kill-buffer buf))))

(ert-deftest warbo-issues-list-column-sorting-by-date ()
  "Date column should be sortable (has sort predicate set to t)."
  (with-examples
   (list-issues)
   (let ((buf (get-buffer "*issues*")))
     (with-current-buffer buf
       ;; Check that Date column (index 1) has a sort predicate
       (let ((date-col-spec (elt tabulated-list-format 1)))
         (should (equal (car date-col-spec) "Date"))
         ;; Third element is sort predicate; t means use default string comparison
         (should (eq (nth 2 date-col-spec) t))))
     (kill-buffer buf))))

(ert-deftest warbo-issues-list-column-sorting-by-status ()
  "Status column should be sortable (has sort predicate set to t)."
  (with-examples
   (list-issues)
   (let ((buf (get-buffer "*issues*")))
     (with-current-buffer buf
       ;; Check that Status column (index 2) has a sort predicate
       (let ((status-col-spec (elt tabulated-list-format 2)))
         (should (equal (car status-col-spec) "Status"))
         (should (eq (nth 2 status-col-spec) t))))
     (kill-buffer buf))))

(ert-deftest warbo-issues-list-column-sorting-by-index ()
  "Index column should use numeric comparison for sorting."
  (with-examples
   (list-issues)
   (let ((buf (get-buffer "*issues*")))
     (with-current-buffer buf
       ;; Check that Index column (index 5) has a custom numeric sort predicate
       (let ((index-col-spec (elt tabulated-list-format 5)))
         (should (equal (car index-col-spec) "Index"))
         ;; Should have issues-compare-numeric as the sort function
         (should (eq (nth 2 index-col-spec) 'issues-compare-numeric))))
     (kill-buffer buf))))

(ert-deftest warbo-issues-list-default-sort-is-by-sort-column ()
  "The list should default to sorting by the Sort column, descending."
  (with-examples
   (list-issues)
   (let ((buf (get-buffer "*issues*")))
     (with-current-buffer buf
       ;; Default sort should be by "Sort" column, descending (t means descending)
       (should (equal tabulated-list-sort-key '("Sort" . t))))
     (kill-buffer buf))))

(ert-deftest warbo-issues-list-can-change-sort-column ()
  "Changing tabulated-list-sort-key should affect the displayed order."
  (with-examples
   (list-issues)
   (let ((buf (get-buffer "*issues*")))
     (with-current-buffer buf
       ;; Get initial order (sorted by Sort descending)
       (let ((initial-order (mapcar #'car tabulated-list-entries)))

         ;; Change to sort by Date ascending
         (setq tabulated-list-sort-key '("Date" . nil))
         (tabulated-list-print t)
         (let ((date-order (mapcar #'car tabulated-list-entries)))
           ;; Order should be different
           (should-not (equal initial-order date-order))

           ;; Dates should be in ascending order
           (let ((dates (mapcar (lambda (entry)
                                  (aref (cadr entry) 1))
                                tabulated-list-entries)))
             (should (equal dates (sort (copy-sequence dates) 'string<)))))))
     (kill-buffer buf))))

(ert-deftest warbo-issues-comments-appear-below-their-issue ()
  "Comments should always appear directly below their parent issue, in index order."
  (with-examples
   (list-issues)
   (let ((buf (get-buffer "*issues*")))
     (with-current-buffer buf
       ;; Test with default sort (by "Sort" descending)
       (should (equal tabulated-list-sort-key '("Sort" . t)))

       ;; Verify comment grouping and ordering
       (let ((entries tabulated-list-entries)
             (current-issue nil)
             (expected-index 0))
         (dolist (entry entries)
           (let* ((row (cadr entry))
                  (issue-id (aref row 3))  ; Issue column (column 3)
                  (index (string-to-number (aref row 5)))) ; Index column (column 5)
             (if (= index 0)
                 ;; This is an issue (index 0), start tracking this issue
                 (progn
                   (setq current-issue issue-id)
                   (setq expected-index 0))
               ;; This is a comment (index > 0), verify it belongs to current issue
               ;; and has the expected index
               (should (equal issue-id current-issue))
               (should (= index expected-index)))
             ;; Next entry should have the next index
             (setq expected-index (1+ index))))))
     (kill-buffer buf))))

(ert-deftest warbo-issues-comments-below-issue-after-date-sort ()
  "Comments should appear below their issue even when sorted by Date."
  (with-examples
   (list-issues)
   (let ((buf (get-buffer "*issues*")))
     (with-current-buffer buf
       ;; Sort by Date ascending
       (setq tabulated-list-sort-key '("Date" . nil))
       (tabulated-list-print t)

       ;; Verify that comments still follow their issues in order
       (let ((entries tabulated-list-entries)
             (current-issue nil)
             (expected-index 0))
         (dolist (entry entries)
           (let* ((row (cadr entry))
                  (issue-id (aref row 3))
                  (index (string-to-number (aref row 5))))
             (if (= index 0)
                 ;; This is an issue, start tracking
                 (progn
                   (setq current-issue issue-id)
                   (setq expected-index 0))
               ;; This is a comment, verify it's under the current issue
               (should (equal issue-id current-issue))
               (should (= index expected-index)))
             (setq expected-index (1+ index))))))
     (kill-buffer buf))))

(ert-deftest warbo-issues-comments-below-issue-after-status-sort ()
  "Comments should appear below their issue even when sorted by Status."
  (with-examples
   (list-issues)
   (let ((buf (get-buffer "*issues*")))
     (with-current-buffer buf
       ;; Sort by Status ascending
       (setq tabulated-list-sort-key '("Status" . nil))
       (tabulated-list-print t)

       ;; Verify that comments still follow their issues in order
       (let ((entries tabulated-list-entries)
             (current-issue nil)
             (expected-index 0))
         (dolist (entry entries)
           (let* ((row (cadr entry))
                  (issue-id (aref row 3))
                  (index (string-to-number (aref row 5))))
             (if (= index 0)
                 ;; This is an issue, start tracking
                 (progn
                   (setq current-issue issue-id)
                   (setq expected-index 0))
               ;; This is a comment, verify it's under the current issue
               (should (equal issue-id current-issue))
               (should (= index expected-index)))
             (setq expected-index (1+ index))))))
     (kill-buffer buf))))

;; Integration tests: these call the real artemis CLI against a temporary repo

(defvar warbo-issues-test-repo nil
  "Path to temporary git repo used by integration tests.")

(defun warbo-issues-integration-setup ()
  "Create a temporary git repo with known test issues.
Returns the repo directory path."
  (let* ((dir (make-temp-file (format "issues-test-%d-" (emacs-pid)) t))
         (default-directory (file-name-as-directory dir))
         (editor (expand-file-name "fake-editor.sh" dir)))
    ;; Initialise git repo
    (call-process "git" nil nil nil "init")
    (call-process "git" nil nil nil "config" "user.name" "Test")
    (call-process "git" nil nil nil "config" "user.email" "test@test")
    ;; Write a fake editor script that replaces the artemis placeholders
    ;; with values from $ISSUE_SUBJECT and $ISSUE_BODY env vars
    (with-temp-file editor
      (insert "#!/usr/bin/env bash\n"
              "sed -i \"s/brief description/${ISSUE_SUBJECT}/\" \"$1\"\n"
              "sed -i \"s/Detailed description\\./${ISSUE_BODY}/\" \"$1\"\n"))
    (set-file-modes editor #o755)
    ;; Create test issues using the real artemis CLI
    (let ((process-environment
           (append (list (concat "EDITOR=" editor)
                         "ISSUE_SUBJECT=First test issue"
                         "ISSUE_BODY=Body of first issue")
                   process-environment)))
      (call-process "artemis" nil nil nil "add"))
    (let ((process-environment
           (append (list (concat "EDITOR=" editor)
                         "ISSUE_SUBJECT=Second test issue"
                         "ISSUE_BODY=Body of second issue")
                   process-environment)))
      (call-process "artemis" nil nil nil "add"))
    ;; Add a comment to the first issue
    (let* ((list-output (let ((default-directory (file-name-as-directory dir)))
                          (shell-command-to-string "artemis list -a -o latest")))
           (first-id (car (split-string list-output " "))))
      (let ((process-environment
             (append (list (concat "EDITOR=" editor)
                           "ISSUE_SUBJECT=unused"
                           "ISSUE_BODY=A comment on the first issue")
                     process-environment)))
        (call-process "artemis" nil nil nil "add" first-id)))
    dir))

(defun warbo-issues-integration-teardown (dir)
  "Remove the temporary test repo at DIR."
  (when (and dir (file-directory-p dir))
    (delete-directory dir t)))

(defmacro with-test-repo (&rest body)
  "Run BODY with `default-directory' set to a temporary artemis repo."
  `(let* ((warbo-issues-test-repo (warbo-issues-integration-setup))
          (default-directory (file-name-as-directory warbo-issues-test-repo)))
     (unwind-protect
         (progn ,@body)
       (warbo-issues-integration-teardown warbo-issues-test-repo))))

(ert-deftest warbo-issues-integration-artemis-list-returns-lines ()
  "issue-artemis-list should return non-empty output from the real CLI."
  :tags '(integration)
  (with-test-repo
   (let ((lines (issue-artemis-list)))
     (should (listp lines))
     (should (> (length lines) 0))
     ;; At least some lines should be non-empty
     (should (> (length (seq-filter (lambda (l) (not (string= "" l))) lines)) 0)))))

(ert-deftest warbo-issues-integration-parse-real-lines ()
  "Every non-empty line from `artemis list' should parse successfully."
  :tags '(integration)
  (with-test-repo
   (let ((lines (seq-filter (lambda (l) (not (string= "" l)))
                            (issue-artemis-list))))
     (should (= (length lines) 2))
     (dolist (line lines)
       (let ((parsed (issue-parse-line line)))
         (should parsed)
         (should (stringp (plist-get parsed 'id)))
         (should (= (length (plist-get parsed 'id)) 16))
         (should (numberp (plist-get parsed 'comment-count)))
         (should (>= (plist-get parsed 'comment-count) 0))
         (should (stringp (plist-get parsed 'status)))
         (should (stringp (plist-get parsed 'description))))))))

(ert-deftest warbo-issues-integration-list-ids-are-hex ()
  "issue-list-ids should return 16-char hex strings."
  :tags '(integration)
  (with-test-repo
   (let ((ids (issue-list-ids)))
     (should (= (length ids) 2))
     (dolist (id ids)
       (should (string-match-p "^[0-9a-f]\\{16\\}$" id))))))

(ert-deftest warbo-issues-integration-get-comment-returns-headers ()
  "issue-get-comment should return text with Date and Message-Id headers."
  :tags '(integration)
  (with-test-repo
   (let* ((ids (issue-list-ids))
          (id  (car ids))
          (raw (issue-get-comment id 0)))
     (should (stringp raw))
     (should (string-match-p "Date:" raw))
     (should (string-match-p "Message-Id:" raw)))))

(ert-deftest warbo-issues-integration-parse-real-comment ()
  "issue-parse-comment should extract valid fields from a real comment."
  :tags '(integration)
  (with-test-repo
   (let* ((ids    (issue-list-ids))
          (id     (car ids))
          (raw    (issue-get-comment id 0))
          (parsed (issue-parse-comment raw)))
     ;; date should be a parsed-time list with year > 2000
     (should (listp (plist-get parsed 'date)))
     (should (> (nth 5 (plist-get parsed 'date)) 2000))
     ;; date-string should look like YYYY-MM-DD
     (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$"
                             (plist-get parsed 'date-string)))
     ;; message-id should be a non-empty string
     (should (stringp (plist-get parsed 'message-id)))
     (should (> (length (plist-get parsed 'message-id)) 0)))))

(ert-deftest warbo-issues-integration-chain-length-matches-comment-count ()
  "issue-chain should return 1 + comment-count entries for a real issue."
  :tags '(integration)
  (with-test-repo
   (let* ((lines (issue-artemis-lines))
          ;; Pick an issue that has at least one comment
          (with-comments (car (seq-filter
                               (lambda (l) (> (plist-get l 'comment-count) 0))
                               lines))))
     (should with-comments)
     (let* ((id    (plist-get with-comments 'id))
            (count (plist-get with-comments 'comment-count))
            (chain (issue-chain id)))
       (should (equal (length chain) (1+ count)))
       ;; Indices should be 0..count
       (should (equal (mapcar 'car chain)
                      (number-sequence 0 count)))))))

(ert-deftest warbo-issues-integration-all-details-structure ()
  "issue-all-details should return well-formed plists from real data."
  :tags '(integration)
  (with-test-repo
   (let ((details (issue-all-details)))
     (should (> (length details) 0))
     (dolist (entry details)
       ;; Every entry must have these fields
       (should (plist-get entry 'status))
       (should (plist-get entry 'sort-key))
       (should (plist-get entry 'date))
       ;; Issue entries (index 0) should have an id; comments may not
       (when (equal (plist-get entry 'index) 0)
         (should (plist-get entry 'id)))
       ;; index must be a non-negative integer
       (should (numberp (plist-get entry 'index)))
       (should (>= (plist-get entry 'index) 0))
       ;; comment-count must be a non-negative integer
       (should (numberp (plist-get entry 'comment-count)))
       (should (>= (plist-get entry 'comment-count) 0))))))

(ert-deftest warbo-issues-integration-all-details-count ()
  "issue-all-details should return one entry per issue + one per comment."
  :tags '(integration)
  (with-test-repo
   ;; Verify the test repo is set up correctly
   (should (file-directory-p default-directory))
   (should (file-directory-p (concat default-directory ".git")))
   (let* ((lines   (issue-artemis-lines))
          (expected (apply '+ (mapcar
                               (lambda (l) (1+ (plist-get l 'comment-count)))
                               lines)))
          (details (issue-all-details)))
     (should (equal (length details) expected)))))

(ert-deftest warbo-issues-integration-list-issues-populates-buffer ()
  "list-issues should create a buffer with real issue data."
  :tags '(integration)
  (with-test-repo
   ;; Clean up any existing *issues* buffer first
   (when (get-buffer "*issues*")
     (kill-buffer "*issues*"))
   (list-issues)
   (let ((buf (get-buffer "*issues*")))
     (should buf)
     (unwind-protect
         (with-current-buffer buf
           (should (eq major-mode 'issues-mode))
           (should (> (length tabulated-list-entries) 0))
           ;; Each entry should have 7 columns
           (dolist (entry tabulated-list-entries)
             (should (= (length (cadr entry)) 7)))
           ;; The buffer should contain rendered text
           (should (> (buffer-size) 0)))
       (when (buffer-live-p buf)
         (kill-buffer buf))))))

(ert-deftest warbo-issues-integration-statuses-are-known ()
  "All issue statuses from the real repo should be recognisable values."
  :tags '(integration)
  (with-test-repo
   (let* ((lines    (issue-artemis-lines))
          (statuses (seq-uniq (mapcar (lambda (l) (plist-get l 'status)) lines))))
     ;; Every status should be a non-empty string (all new in our test repo)
     (dolist (s statuses)
       (should (stringp s))
       (should (> (length s) 0))))))

(ert-deftest warbo-issues-integration-add-issue ()
  "issues-add-issue should create a new issue visible in the listing."
  :tags '(integration)
  (with-test-repo
   (let ((count-before (length (issue-list-ids))))
     ;; Simulate the interactive prompts
     (cl-letf (((symbol-function 'read-string)
                (lambda (prompt &rest _)
                  (pcase prompt
                    ("Subject: " "Brand new issue")
                    ("Body: "    "Created by test")))))
       (issues-add-issue))
     (let ((count-after (length (issue-list-ids))))
       (should (equal count-after (1+ count-before)))
       ;; The new issue should appear in the listing
       (let* ((lines (issue-artemis-lines))
              (descs (mapcar (lambda (l) (plist-get l 'description)) lines)))
         (should (member "Brand new issue" descs)))))))

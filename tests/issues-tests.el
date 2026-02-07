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
  "The issues-mode keymap should have RET and C-c C-c bound."
  (with-examples
   (list-issues)
   (let ((buf (get-buffer "*issues*")))
     (with-current-buffer buf
       ;; RET should be bound to issues-show-issue
       (should (eq (lookup-key issues-mode-map (kbd "RET")) 'issues-show-issue))
       ;; C-c C-c should be bound to issues-add-comment
       (should (eq (lookup-key issues-mode-map (kbd "C-c C-c")) 'issues-add-comment))
       ;; C-c C-k should be bound to issues-close
       (should (eq (lookup-key issues-mode-map (kbd "C-c C-k")) 'issues-close)))
     (kill-buffer buf))))

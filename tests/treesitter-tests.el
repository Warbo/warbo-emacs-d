;;; treesitter-tests.el --- Tests for treesitter mode support
;;
;; Tests that treesitter modes work without errors, grammars are found, and
;; basic syntax-aware functionality operates correctly.
;;; Code:

(defmacro with-treesitter-buffer (extension content &rest body)
  "Create temp buffer for test.EXTENSION with CONTENT and run BODY."
  `(let ((temp-file nil))
     (unwind-protect
         (progn
           (setq temp-file (make-temp-file "test" nil (concat "." ,extension)))
           (with-temp-file temp-file (insert ,content))
           (with-current-buffer (find-file temp-file)
             ,@body))
       (when temp-file (delete-file temp-file)))))

(ert-deftest warbo-treesitter-typescript ()
  "TypeScript files should have treesitter functionality."
  (with-treesitter-buffer
   "ts"
   "const greeting: string = \"hello\";\nconsole.log(greeting);"
   (goto-char (1+ (string-match "console" (buffer-string))))
   (let ((node (treesit-node-at (point))))
     (should (string-equal (treesit-node-text node) "console"))
     (should (string-equal (treesit-node-type node) "identifier"))
     (let ((parent (treesit-node-parent node)))
       (should parent)
       (should (string-equal (treesit-node-type parent)
                             "member_expression"))))))

(ert-deftest warbo-treesitter-javascript ()
  "JavaScript files should have treesitter functionality."
  (with-treesitter-buffer
   "js"
   "const greeting = 'hello';\nconsole.log(greeting);"
   (goto-char (+ 2 (string-match "'hello'" (buffer-string))))
   (let ((node (treesit-node-at (point))))
     (should (string-equal (treesit-node-text node) "hello"))
     (should (string-equal (treesit-node-type node) "string_fragment"))
     (let ((parent (treesit-node-parent node)))
       (should parent)
       (should (string-equal (treesit-node-type parent) "string"))))))

(ert-deftest warbo-treesitter-bash ()
  "Bash files should have treesitter functionality."
  (with-treesitter-buffer
   "sh"
   "#!/bin/bash\necho 'hello'\necho 'world'"
   (goto-char (+ 3 (string-match "echo" (buffer-string))))
   (let ((node (treesit-node-at (point))))
     (should (string-equal (treesit-node-text node) "echo"))
     (should (string-equal (treesit-node-type node) "word"))
     (let ((parent (treesit-node-parent node)))
       (should parent)
       (should (string-equal (treesit-node-type parent) "command_name"))))))

(ert-deftest warbo-treesitter-json ()
  "JSON files should have treesitter functionality."
  (with-treesitter-buffer
   "json"
   "{\"name\": \"test\", \"version\": \"1.0.0\"}"
   (goto-char (+ 2 (string-match "\"test\"" (buffer-string))))
   (let ((node (treesit-node-at (point))))
     (should (string-equal (treesit-node-text node) "test"))
     (should (string-equal (treesit-node-type node) "string_fragment"))
     (let ((parent (treesit-node-parent node)))
       (should parent)
       (should (string-equal (treesit-node-type parent) "string"))))))

(ert-deftest warbo-treesitter-yaml ()
  "YAML files should have treesitter functionality."
  (with-treesitter-buffer
   "yaml"
   "name: test\nversion: 1.0.0\nkey: value"
   (goto-char (1+ (string-match "test" (buffer-string))))
   (let ((node (treesit-node-at (point))))
     (should (string-equal (treesit-node-text node) "test"))
     (let ((parent (treesit-node-parent node)))
       (should parent)
       (should (string-equal (treesit-node-type parent) "plain_scalar"))))))

(ert-deftest warbo-treesitter-python ()
  "Python files should have treesitter functionality."
  (with-treesitter-buffer
   "py"
   "def greeting():\n    print('hello')\n\ngreeting()"
   (goto-char (string-match "print" (buffer-string)))
   (let ((node (treesit-node-at (point))))
     (should (string-equal (treesit-node-text node) "print"))
     (should (string-equal (treesit-node-type node) "identifier"))
     (let ((parent (treesit-node-parent node)))
       (should parent)
       (should (string-equal (treesit-node-type parent) "call"))))))

(ert-deftest warbo-treesitter-rust ()
  "Rust files should have treesitter functionality."
  (with-treesitter-buffer
   "rs"
   "fn main() {\n    println!(\"hello\");\n}"
   (goto-char (+ 2 (string-match "\"hello\"" (buffer-string))))
   (let ((node (treesit-node-at (point))))
     (should (string-equal (treesit-node-text node) "hello"))
     (should (string-equal (treesit-node-type node) "string_content"))
     (let ((parent (treesit-node-parent node)))
       (should parent)
       (should (string-equal (treesit-node-type parent) "string_literal"))))))

(ert-deftest warbo-treesitter-haskell ()
  "Haskell files should have treesitter functionality."
  (with-treesitter-buffer
   "hs"
   "greeting :: String\ngreeting = \"hello\"\n\nmain = putStrLn greeting"
   (goto-char (+ 2 (string-match "\"hello\"" (buffer-string))))
   (let ((node (treesit-node-at (point))))
     (should (string-equal (treesit-node-text node) "\"hello\""))
     (should (string-equal (treesit-node-type node) "string"))
     (let ((parent (treesit-node-parent node)))
       (should parent)
       (should (> (treesit-node-child-count parent) 0))
       (let ((grandparent (treesit-node-parent parent)))
         (should grandparent)
         (should (string-equal (treesit-node-type grandparent) "match")))))))

;;; treesitter-tests.el ends here

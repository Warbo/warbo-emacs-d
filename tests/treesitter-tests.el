;;; treesitter-tests.el --- Tests for treesitter mode support
;;
;; Tests that treesitter modes work without errors, grammars are found, and
;; basic syntax-aware functionality operates correctly.

(defmacro with-treesitter-buffer (extension content &rest body)
  "Create a temp buffer with CONTENT, set syntax highlighting to EXTENSION,
   and run BODY. This will fail if the treesitter grammar is not available."
  `(let ((temp-file nil))
     (unwind-protect
         (progn
           (setq temp-file (make-temp-file "test" nil (concat "." ,extension)))
           (with-temp-file temp-file (insert ,content))
           (with-current-buffer (find-file temp-file)
             (set-auto-mode)
             ,@body))
       (when temp-file (delete-file temp-file)))))

(ert-deftest warbo-treesitter-typescript ()
  "TypeScript files should have treesitter functionality."
  (with-treesitter-buffer
   "ts"
   "const greeting: string = \"hello\";\nconsole.log(greeting);"
   (goto-char (string-match "console" (buffer-string)))
   (let ((node (treesit-node-at (point))))
     (should (string-equal (treesit-node-text node) "console"))
     (should (member (treesit-node-type node) '("identifier" "property_identifier")))
     (let ((parent (treesit-node-parent node)))
       (should parent)
       (should (member (treesit-node-type parent) '("member_expression" "property_access")))))))

(ert-deftest warbo-treesitter-javascript ()
  "JavaScript files should have treesitter functionality."
  (with-treesitter-buffer
   "js"
   "const greeting = 'hello';\nconsole.log(greeting);"
   (goto-char (string-match "'hello'" (buffer-string)))
   (let ((node (treesit-node-at (point))))
     (should (string-equal (treesit-node-text node) "'hello'"))
     (should (member (treesit-node-type node) '("string" "template_string")))
     (let ((parent (treesit-node-parent node)))
       (should parent)
       (should (member (treesit-node-type parent) '("variable_declarator" "assignment")))))))

(ert-deftest warbo-treesitter-bash ()
  "Bash files should have treesitter functionality."
  (with-treesitter-buffer
   "sh"
   "#!/bin/bash\necho 'hello'\necho 'world'"
   (goto-char (string-match "echo" (buffer-string)))
   (let ((node (treesit-node-at (point))))
     (should (string-equal (treesit-node-text node) "echo"))
     (should (member (treesit-node-type node) '("command_name" "word")))
     (let ((parent (treesit-node-parent node)))
       (should parent)
       (should (member (treesit-node-type parent) '("command" "simple_command")))))))

(ert-deftest warbo-treesitter-json ()
  "JSON files should have treesitter functionality."
  (with-treesitter-buffer
   "json"
   "{\"name\": \"test\", \"version\": \"1.0.0\"}"
   (goto-char (string-match "\"test\"" (buffer-string)))
   (let ((node (treesit-node-at (point))))
     (should (string-equal (treesit-node-text node) "\"test\""))
     (should (member (treesit-node-type node) '("string" "value")))
     (let ((parent (treesit-node-parent node)))
       (should parent)
       (should (member (treesit-node-type parent) '("pair" "object_member")))))))

(ert-deftest warbo-treesitter-yaml ()
  "YAML files should have treesitter functionality."
  (with-treesitter-buffer
   "yaml"
   "name: test\nversion: 1.0.0\nkey: value"
   (goto-char (string-match "test" (buffer-string)))
   (let ((node (treesit-node-at (point))))
     (should (string-equal (treesit-node-text node) "test"))
     (let ((parent (treesit-node-parent node)))
       (should parent)
       (should (member (treesit-node-type parent) '("block_mapping_pair" "flow_sequence" "plain_scalar")))))))

(ert-deftest warbo-treesitter-python ()
  "Python files should have treesitter functionality."
  (with-treesitter-buffer
   "py"
   "def greeting():\n    print('hello')\n\ngreeting()"
   (goto-char (string-match "print" (buffer-string)))
   (let ((node (treesit-node-at (point))))
     (should (string-equal (treesit-node-text node) "print"))
     (should (member (treesit-node-type node) '("identifier")))
     (let ((parent (treesit-node-parent node)))
       (should parent)
       (should (member (treesit-node-type parent) '("call" "function_call")))))))

(ert-deftest warbo-treesitter-rust ()
  "Rust files should have treesitter functionality."
  (with-treesitter-buffer
   "rs"
   "fn main() {\n    println!(\"hello\");\n}"
   (goto-char (string-match "\"hello\"" (buffer-string)))
   (let ((node (treesit-node-at (point))))
     (should (string-equal (treesit-node-text node) "\"hello\""))
     (should (member (treesit-node-type node) '("string_literal")))
     (let ((parent (treesit-node-parent node)))
       (should parent)
       (should (member (treesit-node-type parent) '("macro_invocation" "arguments" "argument_list")))))))

(ert-deftest warbo-treesitter-haskell ()
  "Haskell files should have treesitter functionality."
  (with-treesitter-buffer
   "hs"
   "greeting :: String\ngreeting = \"hello\"\n\nmain = putStrLn greeting"
   (goto-char (string-match "\"hello\"" (buffer-string)))
   (let ((node (treesit-node-at (point))))
     (should (string-equal (treesit-node-text node) "\"hello\""))
     (should (member (treesit-node-type node) '("string" "literal")))
     (let ((parent (treesit-node-parent node)))
       (should parent)
       (should (> (treesit-node-child-count parent) 0))
       (let ((grandparent (treesit-node-parent parent)))
         (should grandparent)
         (should (member (treesit-node-type grandparent) '("bind_pattern" "bind"))))))))

;;; treesitter-tests.el ends here

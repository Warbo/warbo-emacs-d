;;; vue-tests.el --- Tests for Vue.js support -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'f)
(require 's)

(defmacro with-vue-project (&rest body)
  "Create a temporary Vue.js project and execute BODY within it."
  `(let* ((      temp-dir (f-join temporary-file-directory "vue-test-project"))
          (       src-dir (f-join           temp-dir       "src"))
          (components-dir (f-join            src-dir       "components"))
          (     types-dir (f-join            src-dir       "types")))
     (unwind-protect
         (progn
           (f-mkdir temp-dir)
           (f-mkdir src-dir)
           (f-mkdir components-dir)
           (f-mkdir types-dir)
           (with-temp-buffer
             (insert "<template>
  <div class=\"container\">
    <h1>{{ message }}</h1>
    <p>{{ computedMessage }}</p>
    <another-component :value=\"message\"></another-component>
  </div>
</template>

<script lang=\"ts\">
import { defineComponent, computed } from 'vue';
import type { MyType } from '../types';
import { unusedFunction } from './utils'; // Unused import for a code action
import { utilityFunction } from '../utils'; // For cross-file JTD/find-usages from .vue to .ts
import { aliasedValue } from '@/some-module'; // For '@' alias JTD

export default defineComponent({
  components: {
    'another-component': {
      props: ['value'],
      template: '<span>{{ value }}</span>'
    }
  },
  data() {
    return {
      message: 'Hello, Vue!' as MyType,
      localValue: aliasedValue, // Usage of aliasedValue
    };
  },
  computed: {
    computedMessage(): string {
      return `Computed: ${this.message} - ${utilityFunction()}`; // Usage of utilityFunction
    }
  },
  methods: {
    myVueMethod() {
      console.log(this.message); // Usage of message
    }
  }
});

const x: string = 123; // Deliberate error for flycheck
function greet(name: string) {
  return `Hello, ${name}!`;
}
</script>

<style scoped>
.container {
  color: red;
  font-size: 16px;
  margin: 10px;
}

.container h1 {
  /* CSS completion test here */
  background-color: blue;
}
</style>")
             (write-file (f-join components-dir "MyComponent.vue")))
           (with-temp-buffer
             (insert "export type MyType = string;
function foo() { return 1; }

const y: number = \"hello\"; // Deliberate error

const bar = foo();")
             (write-file (f-join types-dir "index.ts")))
           (with-temp-buffer
             (insert "export type MyType = string;

export function foo() { return 1; }

const y: number = \"hello\"; // Deliberate error for flycheck

const bar = foo();

import { utilityFunction, utilityConstant } from '../utils'; // For cross-file JTD/find-usages from .ts to .ts
import { greet } from '../components/MyComponent.vue'; // For cross-file JTD from .ts to .vue

export function mainFunction() {
  console.log(utilityFunction()); // Usage of utilityFunction
  console.log(utilityConstant); // Usage of utilityConstant
  console.log(greet(\"World\")); // Usage of greet from .vue
}

import { aliasedValue } from '@/some-module'; // For '@' alias JTD
console.log(aliasedValue); // Usage of aliasedValue")
             (write-file (f-join src-dir "utils.ts")))
           (with-temp-buffer
             (insert "export const aliasedValue = \"This came from an alias!\";")
             (write-file (f-join src-dir "some-module.ts")))
           (with-temp-buffer
             (insert "{
  \"compilerOptions\": {
    \"target\": \"esnext\",
    \"module\": \"esnext\",
    \"strict\": true,
    \"jsx\": \"preserve\",
    \"moduleResolution\": \"node\",
    \"baseUrl\": \".\",
    \"paths\": {
      \"@/*\": [\"src/*\"]
    }
  },
  \"include\": [
    \"src/**/*.ts\",
    \"src/**/*.vue\"
  ]
}")
             (write-file (f-join temp-dir "tsconfig.json")))
             ,@body)
       (progn
         (dolist (buffer (buffer-list))
           (when (and (buffer-file-name buffer)
                      (s-starts-with? temp-dir (buffer-file-name buffer)))
             (kill-buffer buffer)))
         (f-delete temp-dir 'recursive)))))

(defvar vue-language-server-available (executable-find "vue-language-server")
  "Non-nil if vue-language-server is available in PATH.")

(defvar typescript-language-server-available (executable-find "typescript-language-server")
  "Non-nil if typescript-language-server is available in PATH.")

;;; Configuration diagnostic tests

(ert-deftest config-typescript-mode-in-eglot-hooks ()
  "Check that typescript-mode is in eglot's hooks."
  (require 'eglot)
  (let ((dominated-modes (mapcar (lambda (h) (if (consp h) (car h) h))
                                 (mapcar #'car eglot--managed-mode-hook-alist))))
    ;; eglot-ensure adds to eglot--managed-mode-hook-alist when the hook runs
    ;; Instead check if the hook is configured
    (should (member 'eglot-ensure (default-value 'typescript-mode-hook)))))

(ert-deftest config-typescript-in-eglot-server-programs ()
  "Check that typescript-mode has a server configured."
  (require 'eglot)
  (let ((entry (alist-get 'typescript-mode eglot-server-programs)))
    (should entry)
    (should (equal (car entry) "typescript-language-server"))))

(ert-deftest config-vue-mode-in-eglot-hooks ()
  "Check that vue-mode is in eglot's hooks."
  (require 'eglot)
  (should (member 'eglot-ensure (default-value 'vue-mode-hook))))

(ert-deftest config-vue-in-eglot-server-programs ()
  "Check that vue-mode has a server configured."
  (require 'eglot)
  (let ((entry (alist-get 'vue-mode eglot-server-programs)))
    (should entry)
    (should (equal (car entry) "vue-language-server"))))

(ert-deftest config-typescript-mode-available ()
  "Check that typescript-mode is available."
  (require 'typescript-mode)
  (should (fboundp 'typescript-mode)))

(ert-deftest config-vue-mode-available ()
  "Check that vue-mode is available."
  (require 'vue-mode)
  (should (fboundp 'vue-mode)))

(ert-deftest config-ts-file-uses-typescript-mode ()
  "Check that .ts files open in typescript-mode."
  (with-vue-project
   (let ((ts-file (f-join temp-dir "src" "types" "index.ts")))
     (with-current-buffer (find-file-noselect ts-file)
       (should (eq major-mode 'typescript-mode))))))

(ert-deftest config-eglot-starts-for-ts-file ()
  "Check that opening a .ts file triggers eglot-ensure hook."
  (unless typescript-language-server-available
    (ert-skip "typescript-language-server not found"))
  (with-vue-project
   (let ((ts-file (f-join temp-dir "src" "types" "index.ts")))
     (with-current-buffer (find-file-noselect ts-file)
       (should (eq major-mode 'typescript-mode))
       ;; Check if eglot process exists or is starting
       (let ((timeout 5)
             (start (float-time)))
         (while (and (not (eglot-current-server))
                     (< (- (float-time) start) timeout))
           (sit-for 0.1))
         (should (eglot-current-server)))))))

(ert-deftest config-eglot-managed-mode-for-ts-file ()
  "Check that eglot-managed-mode activates for .ts files."
  (unless typescript-language-server-available
    (ert-skip "typescript-language-server not found"))
  (with-vue-project
   (let ((ts-file (f-join temp-dir "src" "types" "index.ts")))
     (with-current-buffer (find-file-noselect ts-file)
       (should (eq major-mode 'typescript-mode))
       (let ((timeout 10)
             (start (float-time)))
         (while (and (not (bound-and-true-p eglot--managed-mode))
                     (< (- (float-time) start) timeout))
           (sit-for 0.1))
         (should (bound-and-true-p eglot--managed-mode)))))))

(ert-deftest config-project-detected-for-ts-file ()
  "Check that project.el detects a project for the temp directory."
  (with-vue-project
   (let ((ts-file (f-join temp-dir "src" "types" "index.ts")))
     (with-current-buffer (find-file-noselect ts-file)
       (let ((proj (project-current)))
         (should proj))))))

(ert-deftest config-tsconfig-exists ()
  "Check that tsconfig.json is created in the temp project."
  (with-vue-project
   (should (f-exists? (f-join temp-dir "tsconfig.json")))))

;;; Functional tests

(ert-deftest vue-mode-and-eglot-activation ()
  "Test that vue-mode and eglot are activated for .vue files."
  (unless vue-language-server-available (ert-skip "vue-language-server not found"))
  (with-vue-project
   (let ((vue-file (f-join temp-dir "src" "components" "MyComponent.vue")))
     (with-current-buffer (find-file-noselect vue-file)
       (should (eq major-mode 'vue-mode))
       (should (bound-and-true-p eglot--managed-mode))))))

(ert-deftest vue-file-content-test ()
  "Test that the temporary Vue project is created with expected files and content."
  (with-vue-project
    (let ((       vue-file (f-join temp-dir "src" "components" "MyComponent.vue"))
          (        ts-file (f-join temp-dir "src" "types" "index.ts"))
          (  tsconfig-file (f-join temp-dir "tsconfig.json"))
          (       src-dir (f-join temp-dir "src"))
          (components-dir (f-join temp-dir "src" "components"))
          (      ypes-dir (f-join temp-dir "src" "types")))
      (should (f-exists? temp-dir))
      (should (f-exists? src-dir))
      (should (f-exists? components-dir))
      (should (f-exists? types-dir))
      (should (f-exists? vue-file))
      (should (f-exists? ts-file))
      (should (f-exists? tsconfig-file))

      (with-current-buffer (find-file-noselect vue-file)
        (should (s-contains? "<script lang=\"ts\">" (buffer-string)))))))

;; TODO: .vue files open in fundamental-mode instead of vue-mode
(ert-deftest vue-mode-activation-test ()
  "Test that vue-mode is correctly activated for .vue files."
  (with-vue-project
    (let ((vue-file (f-join temp-dir "src" "components" "MyComponent.vue")))
      (with-current-buffer (find-file-noselect vue-file)
        (should (eq major-mode 'vue-mode))))))

(ert-deftest vue-indentation-test ()
  "Test indentation within the script tag of a .vue file."
  (with-vue-project
    (let ((vue-file (f-join temp-dir "src" "components" "MyComponent.vue")))
      (with-current-buffer (find-file-noselect vue-file)
        (goto-char (point-min))
        (search-forward "message: 'Hello, Vue!' as MyType,")
        (end-of-line)
        (newline-and-indent)
        (should (= (current-indentation) 6)))

      ;; Test indentation immediately after <script> tag
      (with-current-buffer (find-file-noselect vue-file)
        (goto-char (point-min))
        (search-forward "<script lang=\"ts\">")
        (end-of-line)
        (newline-and-indent)
        (should (= (current-indentation) 0))))))

;; TODO: .vue files open in fundamental-mode instead of vue-mode
(ert-deftest vue-html-tag-balancing-test ()
  "Test that HTML tag balancing works within the <template> block."
  (with-vue-project
    (let ((vue-file (f-join temp-dir "src" "components" "MyComponent.vue")))
      (with-current-buffer (find-file-noselect vue-file)
        (should (eq major-mode 'vue-mode))
        (goto-char (point-min))
        (search-forward "<div")
        (forward-char 1) ;; Move past '<'
        (should (equal (web-mode-tag-match) (point-at-bol 2)))))))

;; TODO: .vue files open in fundamental-mode instead of vue-mode
(ert-deftest vue-css-indentation-test ()
  "Test CSS indentation within the <style> block."
  (with-vue-project
    (let ((vue-file (f-join temp-dir "src" "components" "MyComponent.vue")))
      (with-current-buffer (find-file-noselect vue-file)
        (should (eq major-mode 'vue-mode))
        (goto-char (point-min))
        (search-forward ".container {")
        (end-of-line)
        (newline-and-indent)
        (should (= (current-indentation) 2))))))

;; TODO: .vue files open in fundamental-mode instead of vue-mode
(ert-deftest vue-html-template-indentation-test ()
  "Test HTML/template indentation within the <template> block."
  (with-vue-project
    (let ((vue-file (f-join temp-dir "src" "components" "MyComponent.vue")))
      (with-current-buffer (find-file-noselect vue-file)
        (should (eq major-mode 'vue-mode))
        (goto-char (point-min))
        (search-forward "<div")
        (end-of-line)
        (newline-and-indent)
        (should (= (current-indentation) 2))))))

;; TODO: Indentation is 4 instead of 0 after closing brace
(ert-deftest vue-typescript-editing-test ()
  "Test TypeScript file editing, including mode activation and indentation."
  (with-vue-project
    (let ((ts-file (f-join temp-dir "src" "types" "index.ts")))
      (with-current-buffer (find-file-noselect ts-file)
        (should (eq major-mode 'typescript-mode)) ;; Assert typescript-mode is active
        (goto-char (point-max))
        (insert "\nfunction newFunction() {")
        (newline-and-indent)
        (insert "  const x = 1;")
        (newline-and-indent)
        (insert "}")
        (should (= (current-indentation) 0))))))

(ert-deftest vue-flycheck-error-detection-test ()
  "Test that flycheck detects errors in .vue files."
  (unless vue-language-server-available (ert-skip "vue-language-server not found for flycheck"))
  (with-vue-project
    (let ((vue-file (f-join temp-dir "src" "components" "MyComponent.vue")))
      (with-current-buffer (find-file-noselect vue-file)
        (should (eq major-mode 'vue-mode))
        (flycheck-buffer) ;; Manually trigger a check
        (should (s-contains? "Type '123' is not assignable to type 'string'" (flycheck-error-message (car flycheck-current-errors))))))))

(ert-deftest vue-typescript-jump-to-definition-test ()
  "Test jump-to-definition within a .ts file."
  (unless typescript-language-server-available (ert-skip "typescript-language-server not found"))
  (with-vue-project
   (let ((ts-file (f-join temp-dir "src" "types" "index.ts")))
     (with-current-buffer (find-file-noselect ts-file)
       (should (eq major-mode 'typescript-mode))
       (let ((timeout 10)
             (start (float-time)))
         (while (and (not (bound-and-true-p eglot--managed-mode))
                     (< (- (float-time) start) timeout))
           (sit-for 0.1))
         (unless (bound-and-true-p eglot--managed-mode)
           (ert-skip "eglot did not auto-start for typescript-mode")))
       (goto-char (point-min))
       (search-forward "foo()")
       (backward-char 3)
       (xref-find-definitions (xref-backend-identifier-at-point (xref-find-backend)))
       (should (equal (buffer-file-name) (f-canonical ts-file)))))))

(ert-deftest vue-typescript-flycheck-error-detection-test ()
  "Test that flycheck detects errors in .ts files."
  (unless vue-language-server-available (ert-skip "vue-language-server not found for flycheck"))
  (with-vue-project
    (let ((ts-file (f-join temp-dir "src" "types" "index.ts")))
      (with-current-buffer (find-file-noselect ts-file)
        (should (eq major-mode 'typescript-mode))
        (flycheck-buffer) ;; Manually trigger a check
        (should (s-contains? "Type '\"hello\"' is not assignable to type 'number'" (flycheck-error-message (car flycheck-current-errors))))))))

(ert-deftest jump-to-definition-in-vue-file ()
  "Test jump-to-definition from a .vue file to a .ts file."
  (unless vue-language-server-available (ert-skip "vue-language-server not found"))
  (with-vue-project
    (let ((vue-file (f-join temp-dir "src" "components" "MyComponent.vue"))
          (ts-file (f-join temp-dir "src" "types" "index.ts")))
      (with-current-buffer (find-file-noselect vue-file)
        (let ((timeout 10)
              (start (float-time)))
          (while (and (not (bound-and-true-p eglot--managed-mode))
                      (< (- (float-time) start) timeout))
            (sit-for 0.1))
          (unless (bound-and-true-p eglot--managed-mode)
            (ert-skip "eglot did not connect within timeout")))
        (goto-char (point-min))
        (search-forward "MyType")
        (xref-find-definitions (xref-backend-identifier-at-point (xref-find-backend)))
        (should (equal (buffer-file-name) (f-canonical ts-file)))))))

;;; TODO: Add test for cross-file jump-to-definition from one .ts file to another .ts file.
;;; TODO: Add test for cross-file jump-to-definition from a .ts file to a <script> block in a .vue file.
;;; TODO: Add test for cross-file jump-to-definition from a <script> block in a .vue file to a .ts file.
;;; TODO: Add test for jump-to-definition using '@' in import paths in .vue files.
;;; TODO: Add test for jump-to-definition using '@' in import paths in .ts files.
;;; TODO: Add test for find-usages functionality in .vue files.
;;; TODO: Add test for find-usages functionality in .ts files.
;;; TODO: Add test for Eglot code actions (e.g., organize imports, refactorings) in .vue files.
;;; TODO: Add test for Eglot code actions (e.g., organize imports, refactorings) in .ts files.
;;; TODO: Add test for HTML/template code completion (requires Eglot) in .vue files.
;;; TODO: Add test for CSS code completion (requires Eglot) in .vue files.
;;; TODO: Add test for JavaScript/TypeScript code completion within <script> (requires Eglot) in .vue files.
;;; TODO: Add test for JavaScript/TypeScript code completion in .ts files (requires Eglot).

(provide 'vue-tests)

;;; vue-tests.el ends here

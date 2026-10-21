(in-package #:cl-org-kit/test)

(defun babel-execution-test-source-block (document)
  (let ((block nil))
    (map-nodes (lambda (node)
                 (when (eq (node-type node) :src-block)
                   (setf block node)))
               (document-root document))
    block))

(defun babel-execution-test-named-source-block (document name)
  (let ((block nil))
    (map-nodes (lambda (node)
                 (when (and (eq (node-type node) :src-block)
                            (equal (node-property node :name) name))
                   (setf block node)))
               (document-root document))
    block))

(defun babel-execution-test-condition (function)
  (handler-case
      (progn (funcall function) nil)
    (error (condition) condition)))

(defun babel-execution-test-lines (&rest lines)
  (format nil "~{~A~^~%~}" lines))

(defvar *babel-execution-test-sentinel* nil)

(describe "Babel source execution"
  (it "passes expanded source, merged headers, and resolved variables to the executor"
    (let* ((document
             (parse-org
              (babel-execution-test-lines
               "#+NAME: fragment"
               "#+BEGIN_SRC text"
               "shared"
               "#+END_SRC"
               ""
               "#+NAME: consumer"
               "#+BEGIN_SRC shell :session build :var amount=7 :noweb yes"
               "echo <<fragment>>"
               "#+END_SRC")))
           (block (babel-execution-test-named-source-block document "consumer"))
           (registry (org-kit:make-callback-registry))
           (observed nil))
      (org-kit:register-babel-executor
       registry "shell"
       (lambda (body parameters variables)
         (setf observed (list body parameters variables))
         "finished"))
      (let ((result (org-kit:execute-babel-source-block document block registry)))
        (with-soft-assertions
          (expect (org-kit:babel-execution-result-status result) :to-be :executed)
          (expect (first observed) :to-equal "echo shared")
          (expect (cdr (assoc "SESSION" (second observed) :test #'string=))
                  :to-equal "build")
          (expect (third observed) :to-equal '(("amount" . 7)))
          (expect (org-kit:edit-result-p
                   (org-kit:babel-execution-result-edit-result result))
                  :to-be t)
          (expect (document-text document)
                  :to-contain "#+BEGIN_SRC shell :session build")))))

  (it "does not evaluate Common Lisp source when the executor is unregistered"
  (let ((*babel-execution-test-sentinel* nil))
    (let* ((document
             (parse-org
              (babel-execution-test-lines
               "#+NAME: task"
               "#+BEGIN_SRC common-lisp"
               "(setf cl-org-kit/test::*babel-execution-test-sentinel* t)"
               "#+END_SRC")))
           (block (babel-execution-test-named-source-block document "task"))
           (registry (org-kit:make-callback-registry))
           (condition
             (babel-execution-test-condition
              (lambda ()
                (org-kit:execute-babel-source-block document block registry)))))
      (with-soft-assertions
        (expect (typep condition 'org-callback-error) :to-be t)
        (when (typep condition 'org-callback-error)
          (expect (org-callback-error-kind condition) :to-equal "BABEL-EXECUTOR")
          (expect (org-callback-error-reason condition) :to-be :unregistered))
        (expect *babel-execution-test-sentinel* :to-be nil)))))

  (it "checks Common Lisp input limits before encoding and counts input in bounded chunks"
  (let* ((executor (org-kit:make-babel-common-lisp-reference-executor))
         (limited-document
           (parse-org "" :limits (make-limits :max-input-length 3)))
         (limit-condition nil)
         (limit-range-sizes nil)
         (body (make-string 2050 :initial-element #\Space))
         (document
           (parse-org "" :limits (make-limits :max-input-length 10000)))
         (result nil)
         (range-sizes nil))
    (babel-results-test-call-with-range-counter
     (lambda (get-range-sizes)
       (let ((org-kit::*babel-execution-document* limited-document))
         (setf limit-condition
               (babel-execution-test-condition
                (lambda () (funcall executor "four" nil nil)))))
       (setf limit-range-sizes (funcall get-range-sizes))))
    (babel-results-test-call-with-range-counter
     (lambda (get-range-sizes)
       (let ((org-kit::*babel-execution-document* document))
         (setf result (funcall executor body nil nil)))
       (setf range-sizes (funcall get-range-sizes))))
    (with-soft-assertions
      (expect (typep limit-condition 'org-resource-limit-error) :to-be t)
      (when (typep limit-condition 'org-resource-limit-error)
        (expect (org-resource-limit-error-limit-name limit-condition)
                :to-be :max-input-length)
        (expect (org-resource-limit-error-actual limit-condition) :to-be 4))
      (expect limit-range-sizes :to-equal nil)
      (expect result :to-be nil)
      (expect (notany (lambda (size) (> size 1024)) range-sizes) :to-be t)
      (expect (count 1024 range-sizes) :to-be 2))))

  (it "matches Org's session and async selection for one-shot runs"
  (let* ((runner-calls 0)
         (executor
           (org-kit:make-babel-shell-reference-executor
            (lambda (shell script stdin cmdline)
              (declare (ignore shell script stdin cmdline))
              (incf runner-calls)
              "runner result")))
         (session-condition
           (babel-execution-test-condition
            (lambda ()
              (funcall executor "body" '(("SESSION" . "build")) nil))))
         (async-value
           (funcall executor "body" '(("ASYNC" . "yes")) nil))
         (external-session-value
           (funcall executor
                    "body"
                    '(("SESSION" . "build") ("CMDLINE" . ""))
                    nil)))
    (with-soft-assertions
      (expect (typep session-condition 'org-babel-error) :to-be t)
      (expect (org-babel-error-reason session-condition)
              :to-be :unsupported-session)
      (expect async-value :to-equal "runner result")
      (expect external-session-value :to-equal "runner result")
      (expect runner-calls :to-be 2))))
  (it "enforces shell runner input and output limits"
  (let* ((input-document
           (parse-org "" :limits (make-limits :max-input-length 3)))
         (output-document
           (parse-org "" :limits (make-limits :max-output-length 3)))
         (output-octet-document
           (parse-org "" :limits (make-limits :max-output-octets 3)))
         (runner-calls 0)
         (runner-output "four")
         (executor
           (org-kit:make-babel-shell-reference-executor
            (lambda (shell script stdin cmdline)
              (declare (ignore shell script stdin cmdline))
              (incf runner-calls)
              runner-output)))
         (input-character-condition
           (let ((org-kit::*babel-execution-document* input-document))
             (babel-execution-test-condition
              (lambda () (funcall executor "xx" nil nil)))))
         (input-octet-condition
           (let ((org-kit::*babel-execution-document* input-document))
             (babel-execution-test-condition
              (lambda () (funcall executor "é" nil nil)))))
         (output-condition
           (let ((org-kit::*babel-execution-document* output-document))
             (babel-execution-test-condition
              (lambda () (funcall executor "x" nil nil)))))
         (output-octet-condition
           (let ((org-kit::*babel-execution-document* output-octet-document))
             (setf runner-output "éé")
             (babel-execution-test-condition
              (lambda () (funcall executor "x" nil nil))))))
    (with-soft-assertions
      (expect (typep input-character-condition 'org-resource-limit-error)
              :to-be t)
      (expect (org-resource-limit-error-limit-name input-character-condition)
              :to-be :max-input-length)
      (expect (org-resource-limit-error-actual input-character-condition)
              :to-be 4)
      (expect (typep input-octet-condition 'org-resource-limit-error)
              :to-be t)
      (expect (org-resource-limit-error-limit-name input-octet-condition)
              :to-be :max-input-length)
      (expect (org-resource-limit-error-actual input-octet-condition)
              :to-be 4)
      (expect (typep output-condition 'org-resource-limit-error) :to-be t)
      (expect (org-resource-limit-error-limit-name output-condition)
              :to-be :max-output-length)
      (expect (org-resource-limit-error-actual output-condition) :to-be 4)
      (expect (typep output-octet-condition 'org-resource-limit-error)
              :to-be t)
      (expect (org-resource-limit-error-limit-name output-octet-condition)
              :to-be :max-output-octets)
      (expect (org-resource-limit-error-actual output-octet-condition)
              :to-be 4)
      (expect runner-calls :to-be 2))))
  (it "returns and renders the shell exit status for value tables"
  (let* ((document
           (parse-org
            (babel-execution-test-lines
             "#+NAME: task"
             "#+BEGIN_SRC shell :results value table"
             "exit 0"
             "#+END_SRC")))
         (block (babel-execution-test-named-source-block document "task"))
         (registry (org-kit:make-callback-registry))
         (observed-script nil))
    (org-kit:register-babel-executor
     registry "shell"
     (org-kit:make-babel-shell-reference-executor
      (lambda (shell script stdin cmdline)
        (declare (ignore shell stdin cmdline))
        (setf observed-script script)
        (format nil "0~%"))))
    (let* ((result (org-kit:execute-babel-source-block document block registry))
           (updated (org-kit:edit-result-updated-document
                     (org-kit:babel-execution-result-edit-result result))))
      (with-soft-assertions
        (expect observed-script :to-equal (format nil "exit 0~%echo $?"))
        (expect (org-kit:babel-execution-result-value result) :to-be 0)
        (expect (document-text updated) :to-contain "| 0 |")))))
  (it "imports output tables like Org shell Babel"
  (let* ((document
           (parse-org
            (babel-execution-test-lines
             "#+NAME: task"
             "#+BEGIN_SRC shell :results output table"
             "printf rows"
             "#+END_SRC")))
         (block (babel-execution-test-named-source-block document "task"))
         (registry (org-kit:make-callback-registry))
         (runner-output (format nil "1~Calpha~%2~Cbeta~%" #\Tab #\Tab)))
    (org-kit:register-babel-executor
     registry "shell"
     (org-kit:make-babel-shell-reference-executor
      (lambda (shell script stdin cmdline)
        (declare (ignore shell script stdin cmdline))
        runner-output)))
    (let* ((result (org-kit:execute-babel-source-block document block registry))
           (edit (org-kit:babel-execution-result-edit-result result))
           (updated (org-kit:edit-result-updated-document edit)))
      (with-soft-assertions
        (expect (org-kit:babel-execution-result-value result)
                :to-equal '((1 "alpha") (2 "beta")))
        (expect (document-text updated)
                :to-contain (babel-execution-test-lines
                             "| 1 | alpha |" "| 2 | beta |"))))))
  (it "preserves empty raw shell output"
  (let ((executor
          (org-kit:make-babel-shell-reference-executor
           (lambda (shell script stdin cmdline)
             (declare (ignore shell script stdin cmdline))
             ""))))
    (expect (funcall executor "body" '(("RESULTS" . "output")) nil)
            :to-equal "")))
  (it "imports shell output and value using Org Babel table rules"
  (let* ((document (make-document ""))
         (runner-output nil)
         (executor
           (org-kit:make-babel-shell-reference-executor
            (lambda (shell script stdin cmdline)
              (declare (ignore shell script stdin cmdline))
              runner-output)))
         (table-cases
 (list
  (list (format nil "first,second~%\"with,comma\",3~%")
        '(("first" "second") ("with,comma" 3)))
  (list (format nil "first second~%one two~%")
        '(("first" "second") ("one" "two")))
  (list (format nil "---~Cignored~%1~Calpha~%" #\Tab #\Tab)
        '(("---" "ignored") (1 "alpha")))
  (list (format nil "|a|b|~%|c|d|~%")
        '(("" "a" "b") ("" "c" "d")))
  (list (format nil "| a | b |~%| 1 | 2 |~%")
        '(("" "" "a" "" "" "b" "")
          ("" "" 1 "" "" 2 "")))
  (list "---" "---")
  (list "1E3" 1000.0d0)
  (list (format nil "~{~A~%~}" (make-list 1000 :initial-element "row")) nil)
  (list (format nil "\"\"~Cempty~%" #\Tab) (list (list "" "empty")))
  (list (format nil "a~Cb~Cc~%1~C2~%" #\Tab #\Tab #\Tab #\Tab)
        '(("a" "b" "c") (1 2 "")))
  (list (format nil "a~Cb~%~%c~Cd~Ce~%" #\Tab #\Tab #\Tab #\Tab)
        '(("a" "b" "") ("" "" "") ("c" "d" "e")))
  (list (format nil "key,value~%\"a\\nb\",\"1\"~%")
        '(("key" "value") ("a\\nb" 1)))
  (list "" nil)
  (list (format nil "~%") nil)
  (list (format nil "a~%~%") nil)
  (list (format nil "a~%   ~%") (quote (("a") (""))))
  (list (format nil "~%   ~%") (quote (("") ("")))))))
    (let ((org-kit::*babel-execution-document* document))
      (setf runner-output "plain stdout")
      (expect (funcall executor "body" '(("RESULTS" . "output")) nil)
              :to-equal "plain stdout")
      (progn
  (dolist (case table-cases)
    (setf runner-output (first case))
    (expect
     (funcall executor "body" '(("RESULTS" . "output table")) nil)
     :to-equal (second case)))
  (setf runner-output (format nil "discarded~%1~Calpha~%" #\Tab))
  (expect (funcall executor "body" '(("RESULTS" . "value")) nil)
          :to-equal '((1 "alpha")))
  (setf runner-output (format nil "previous~%status~%"))
  (expect (funcall executor "body" '(("RESULTS" . "value output")) nil)
          :to-equal "status")))))
  (it "passes shell script, stdin, and raw cmdline to the runner"
  (let* ((document (make-document ""))
         (runner-arguments nil)
         (executor
           (org-kit:make-babel-shell-reference-executor
            (lambda (shell script stdin cmdline)
              (setf runner-arguments (list shell script stdin cmdline))
              "runner result")
            :shell "/bin/bash"))
         (parameters '(("PROLOGUE" . "setup")
                       ("EPILOGUE" . "finish")
                       ("RESULTS" . "output")
                       ("STDIN" . "\"input data\"")
                       ("CMDLINE" . "--flag \"two words\"")
                       ("SHEBANG" . "#!/usr/bin/env bash")
                       ("PADLINE" . "no")))
         (expected-script
           (babel-execution-test-lines
            "#!/usr/bin/env bash"
            "setup"
            "echo ready"
            "finish")))
    (let ((org-kit::*babel-execution-document* document))
      (funcall executor "echo ready" parameters nil))
    (expect runner-arguments
            :to-equal
            (list "/bin/bash" expected-script "input data"
                  "--flag \"two words\""))))
  (it "selects script mode from a shebang alone"
    (let* ((runner-arguments nil)
           (executor
             (org-kit:make-babel-shell-reference-executor
              (lambda (shell script stdin cmdline)
                (setf runner-arguments (list shell script stdin cmdline))
                "runner result"))))
      (funcall executor
               "body"
               (list (cons "SHEBANG" "#!/usr/bin/env bash")
                     (cons "PADLINE" "no"))
               nil)
      (expect runner-arguments
              :to-equal
              (list "sh" (format nil "#!/usr/bin/env bash~%body") nil nil))))
  (it "resolves named shell stdin tables through the executor"
  (let* ((document
           (parse-org
            (babel-execution-test-lines
             "#+NAME: payload"
             "| name | value |"
             "| one | 1 |"
             ""
             "#+NAME: task"
             "#+BEGIN_SRC shell :stdin payload :results output"
             "cat"
             "#+END_SRC")))
         (block (babel-execution-test-named-source-block document "task"))
         (registry (org-kit:make-callback-registry))
         (runner-arguments nil)
         (expected-stdin (format nil "name~Cvalue~%one~C1" #\Tab #\Tab))
         (executor
           (org-kit:make-babel-shell-reference-executor
            (lambda (shell script stdin cmdline)
              (setf runner-arguments (list shell script stdin cmdline))
              stdin))))
    (org-kit:register-babel-executor registry "shell" executor)
    (let ((result (org-kit:execute-babel-source-block document block registry)))
      (with-soft-assertions
        (expect runner-arguments
                :to-equal
                (list "sh"
                      (babel-execution-test-lines
                       "#!/usr/bin/env sh" "" "cat")
                      expected-stdin nil))
        (expect (org-kit:babel-execution-result-value result)
                :to-equal expected-stdin)))))
  (it "matches Org generic and fish shell variable assignments"
  (let* ((document (make-document ""))
         (parameters '(("PROLOGUE" . "setup")
                       ("EPILOGUE" . "finish")
                       ("RESULTS" . "output")))
         (variables '(("message" . "it's")
                      ("items" . ("red" "blue"))
                      ("records" . (("one" "1") ("two" "2")))))
         (records-text (format nil "records='one~C1~%two~C2'"
                               #\Tab #\Tab))
         (fish-records-text (format nil "set records 'one~C1~%two~C2'"
                                    #\Tab #\Tab))
         (cases
           (list
            (list "/bin/sh"
                  (babel-execution-test-lines
                   "setup" "message='it'\"'\"'s'" "items='red" "blue'"
                   records-text "echo ready" "finish"))
            (list "/usr/bin/fish"
                  (babel-execution-test-lines
                   "setup" "set message 'it'\"'\"'s'" "set items 'red"
                   "blue'" fish-records-text "echo ready" "finish")))))
    (dolist (case cases)
      (let* ((shell (first case))
             (expected-script (second case))
             (observed-script nil)
             (executor
               (org-kit:make-babel-shell-reference-executor
                (lambda (runner-shell script stdin cmdline)
                  (declare (ignore runner-shell stdin cmdline))
                  (setf observed-script script)
                  "runner result")
                :shell shell)))
        (let ((org-kit::*babel-execution-document* document))
          (funcall executor "echo ready" parameters variables))
        (expect observed-script :to-equal expected-script)))))
  (it "matches Emacs Org 9.8.10 shell variable assignments and body expansion"
  (let* ((document (make-document ""))
         (runner-arguments nil)
         (executor
           (org-kit:make-babel-shell-reference-executor
            (lambda (shell script stdin cmdline)
              (setf runner-arguments (list shell script stdin cmdline))
              "runner result")
            :shell "/bin/bash"))
         (parameters '( ("PROLOGUE" . "setup")
                        ("EPILOGUE" . "finish")
                        ("RESULTS" . "output")))
         (variables '(("message" . "it's")
                      ("items" . ("red" "blue"))
                      ("records" . (("one" "1") ("two" "2")))))
         (expected-script
           (babel-execution-test-lines
            "setup"
            "message='it'\"'\"'s'"
            "unset items"
            "declare -a items=( 'red' 'blue' )"
            "unset records"
            "declare -A records"
            "records['one']='1'"
            "records['two']='2'"
            "echo ready"
            "finish")))
    (let ((org-kit::*babel-execution-document* document))
      (funcall executor "echo ready" parameters variables))
    (expect runner-arguments
            :to-equal (list "/bin/bash" expected-script nil nil))))
  (it "uses the shell reference executor without starting a process"
  (let* ((document
           (parse-org
            (babel-execution-test-lines
             "#+NAME: task"
             "#+BEGIN_SRC shell"
             "echo ready"
             "#+END_SRC")))
         (block (babel-execution-test-named-source-block document "task"))
         (registry (org-kit:make-callback-registry))
         (runner-calls 0)
         (runner-arguments nil)
         (runner (lambda (shell script stdin cmdline)
                   (incf runner-calls)
                   (setf runner-arguments (list shell script stdin cmdline))
                   "runner result"))
         (adapter (org-kit:make-babel-shell-reference-executor runner)))
    (org-kit:register-babel-executor registry "shell" adapter)
    (let ((result (org-kit:execute-babel-source-block document block registry)))
      (with-soft-assertions
        (expect (eq adapter runner) :to-be nil)
        (expect runner-arguments :to-equal '("sh" "echo ready" nil nil))
        (expect (org-kit:babel-execution-result-value result)
                :to-equal "runner result")
        (expect runner-calls :to-be 1)))))

  (it "keeps the Common Lisp reference executor opt-in and resolves variables privately"
    (let* ((variable-name "executor-only-variable")
           (document
             (parse-org
              (babel-execution-test-lines
               "#+NAME: task"
               "#+BEGIN_SRC common-lisp :var executor-only-variable=41"
               "(+ executor-only-variable 1)"
               "#+END_SRC")))
           (block (babel-execution-test-named-source-block document "task"))
           (registry (org-kit:make-callback-registry))
           (missing
             (babel-execution-test-condition
              (lambda ()
                (org-kit:execute-babel-source-block document block registry)))))
      (org-kit:register-babel-executor
       registry "common-lisp"
       (org-kit:make-babel-common-lisp-reference-executor))
      (let ((result (org-kit:execute-babel-source-block document block registry)))
        (with-soft-assertions
          (expect (typep missing 'org-callback-error) :to-be t)
          (expect (org-kit:babel-execution-result-value result) :to-be 42)
          (expect (find-symbol (string-upcase variable-name) :cl-user) :to-be nil)))))
  (it "checks Common Lisp source size before reading forms"
    (let* ((document
             (make-document "" :limits (make-limits :max-input-length 2)))
           (executor (org-kit:make-babel-common-lisp-reference-executor))
           (condition
             (let ((org-kit::*babel-execution-document* document))
               (babel-execution-test-condition
                (lambda () (funcall executor "(+ 1 2)" nil nil))))))
      (with-soft-assertions
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when (typep condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name condition)
                  :to-be :max-input-length)
          (expect (org-resource-limit-error-actual condition) :to-be 7))))))

(describe "Babel evaluation gates and cache"
  (it "skips, requires query confirmation, and cancels without invoking executors"
    (let* ((no-document
             (parse-org
              (babel-execution-test-lines
               "#+NAME: no-task"
               "#+BEGIN_SRC shell :eval no"
               "body"
               "#+END_SRC")))
           (query-document
             (parse-org
              (babel-execution-test-lines
               "#+NAME: query-task"
               "#+BEGIN_SRC shell :eval query"
               "body"
               "#+END_SRC")))
           (no-block (babel-execution-test-named-source-block no-document "no-task"))
           (query-block
             (babel-execution-test-named-source-block query-document "query-task"))
           (registry (org-kit:make-callback-registry))
           (skipped (org-kit:execute-babel-source-block no-document no-block registry))
           (missing-confirmation
             (babel-execution-test-condition
              (lambda ()
                (org-kit:execute-babel-source-block query-document query-block registry))))
           (cancelled
             (org-kit:execute-babel-source-block
              query-document query-block registry
              :confirmation-function
              (lambda (block parameters)
                (expect (eq block query-block) :to-be t)
                (expect
                 (not (null (assoc "EVAL" parameters :test #'string=)))
                 :to-be t)
                nil))))
      (with-soft-assertions
        (expect (org-kit:babel-execution-result-status skipped) :to-be :skipped)
        (expect (typep missing-confirmation 'org-babel-error) :to-be t)
        (when (typep missing-confirmation 'org-babel-error)
          (expect (org-babel-error-reason missing-confirmation)
                  :to-equal :confirmation-required))
        (expect (org-kit:babel-execution-result-status cancelled) :to-be :cancelled))))

  (it "skips no-export only for export evaluation"
    (let* ((document
             (parse-org
              (babel-execution-test-lines
               "#+NAME: task"
               "#+BEGIN_SRC shell :eval no-export"
               "body"
               "#+END_SRC")))
           (block (babel-execution-test-named-source-block document "task"))
           (registry (org-kit:make-callback-registry))
           (skipped
             (org-kit:execute-babel-source-block
              document block registry :evaluation-context :export)))
      (expect (org-kit:babel-execution-result-status skipped) :to-be :skipped)))

  (it "returns cached results from adjacent hash markers without running an executor"
    (let* ((document
             (parse-org
              (babel-execution-test-lines
               "#+NAME: task"
               "#+BEGIN_SRC shell :cache yes"
               "echo same"
               "#+END_SRC"
               ""
               "#+RESULTS[stable]: task"
               "old result")))
           (block (babel-execution-test-named-source-block document "task"))
           (registry (org-kit:make-callback-registry))
           (hash-call nil)
           (original (document-text document))
           (result
             (org-kit:execute-babel-source-block
              document block registry
              :hash-function
              (lambda (body parameters variables)
                (setf hash-call (list body parameters variables))
                "stable"))))
      (with-soft-assertions
        (expect (org-kit:babel-execution-result-status result) :to-be :cached)
        (expect (org-kit:babel-execution-result-hash result) :to-equal "stable")
        (expect (first hash-call) :to-equal "echo same")
        (expect (third hash-call) :to-equal nil)
        (expect (document-text document) :to-equal original)
        (expect (org-kit:babel-execution-result-edit-result result) :to-be nil))))

  (it "writes the cache hash on a cache miss"
    (let* ((document
             (parse-org
              (babel-execution-test-lines
               "#+NAME: task"
               "#+BEGIN_SRC shell :cache yes"
               "echo new"
               "#+END_SRC")))
           (block (babel-execution-test-named-source-block document "task"))
           (registry (org-kit:make-callback-registry)))
      (org-kit:register-babel-executor registry "shell"
                                       (lambda (body parameters variables)
                                         (declare (ignore body parameters variables))
                                         "fresh"))
      (let* ((result
               (org-kit:execute-babel-source-block
                document block registry
                :hash-function (lambda (body parameters variables)
                                 (declare (ignore body parameters variables))
                                 "fresh-hash")))
             (updated (org-kit:edit-result-updated-document
                       (org-kit:babel-execution-result-edit-result result))))
        (with-soft-assertions
          (expect (org-kit:babel-execution-result-status result) :to-be :executed)
          (expect (search "#+RESULTS[fresh-hash]: task" (document-text updated))
                  :not :to-be nil)))))
  (it "misses a cache entry when a referenced named table value changes"
    (let* ((old-document
             (parse-org
              (babel-execution-test-lines
               "#+NAME: data"
               "| 1 |"
               ""
               "#+NAME: task"
               "#+BEGIN_SRC shell :cache yes :var input=data"
               "echo same"
               "#+END_SRC")))
           (new-document
             (parse-org
              (babel-execution-test-lines
               "#+NAME: data"
               "| 2 |"
               ""
               "#+NAME: task"
               "#+BEGIN_SRC shell :cache yes :var input=data"
               "echo same"
               "#+END_SRC"
               ""
               "#+RESULTS[input-1]: task"
               "old result")))
           (old-block (babel-execution-test-named-source-block old-document "task"))
           (new-block (babel-execution-test-named-source-block new-document "task"))
           (registry (org-kit:make-callback-registry))
           (executor-calls 0)
           (hash-calls '())
           (hash-function
             (lambda (body parameters variables)
               (push (list body parameters variables) hash-calls)
               (format nil "input-~D"
                       (caar (cdr (assoc "input" variables :test #'string=)))))))
      (org-kit:register-babel-executor
       registry "shell"
       (lambda (body parameters variables)
         (declare (ignore body parameters variables))
         (incf executor-calls)
         "fresh"))
      (let* ((old-result
               (org-kit:execute-babel-source-block
                old-document old-block registry :hash-function hash-function))
             (new-result
               (org-kit:execute-babel-source-block
                new-document new-block registry :hash-function hash-function))
             (new-call (first hash-calls))
             (old-call (second hash-calls)))
        (with-soft-assertions
          (expect (org-kit:babel-execution-result-status old-result) :to-be :executed)
          (expect (org-kit:babel-execution-result-hash old-result)
                  :to-equal "input-1")
          (expect (org-kit:babel-execution-result-status new-result) :to-be :executed)
          (expect (org-kit:babel-execution-result-hash new-result)
                  :to-equal "input-2")
          (expect executor-calls :to-be 2)
          (expect (first new-call) :to-equal (first old-call))
          (expect (second new-call) :to-equal (second old-call))
          (expect (third old-call) :to-equal '(("input" . ((1)))))
          (expect (third new-call) :to-equal '(("input" . ((2)))))))))
  (it "bounds Common Lisp executor output at write time"
    (labels ((run (body max-length max-octets)
               (let* ((document
                        (parse-org
                         (babel-execution-test-lines
                          "#+NAME: task"
                          "#+BEGIN_SRC common-lisp :results silent output"
                          body
                          "#+END_SRC")
                         :limits (make-limits :max-output-length max-length
                                              :max-output-octets max-octets)))
                      (block (babel-execution-test-named-source-block document "task"))
                      (registry (org-kit:make-callback-registry)))
                 (org-kit:register-babel-executor
                  registry "common-lisp"
                  (org-kit:make-babel-common-lisp-reference-executor))
                 (babel-execution-test-condition
                  (lambda ()
                    (org-kit:execute-babel-source-block document block registry))))))
      (let ((length-condition (run "(write-string \"four\")" 3 100))
            (octet-condition (run "(write-string \"éé\")" 10 3)))
        (with-soft-assertions
          (expect (typep length-condition 'org-resource-limit-error) :to-be t)
          (when (typep length-condition 'org-resource-limit-error)
            (expect (org-resource-limit-error-limit-name length-condition)
                    :to-be :max-output-length)
            (expect (org-resource-limit-error-actual length-condition)
                    :to-be 4))
          (expect (typep octet-condition 'org-resource-limit-error) :to-be t)
          (when (typep octet-condition 'org-resource-limit-error)
            (expect (org-resource-limit-error-limit-name octet-condition)
                    :to-be :max-output-octets)
            (expect (org-resource-limit-error-actual octet-condition)
                    :to-be 4)))))))

(describe "Babel executor error propagation"
  (it "propagates an executor condition unchanged"
    (let* ((document
             (parse-org
              (babel-execution-test-lines
               "#+NAME: task"
               "#+BEGIN_SRC shell"
               "body"
               "#+END_SRC")))
           (block (babel-execution-test-named-source-block document "task"))
           (registry (org-kit:make-callback-registry))
           (condition (make-condition 'simple-error
                                      :format-control "executor failed"
                                      :format-arguments nil))
           (caught nil))
      (org-kit:register-babel-executor registry "shell"
                                       (lambda (body parameters variables)
                                         (declare (ignore body parameters variables))
                                         (error condition)))
      (setf caught
            (babel-execution-test-condition
             (lambda ()
               (org-kit:execute-babel-source-block document block registry))))
      (expect (eq caught condition) :to-be t)))
  (it "validates executor options and captures output with reader evaluation disabled"
    (let* ((*babel-execution-test-sentinel* nil)
           (runner-error
             (babel-execution-test-condition
              (lambda () (org-kit:make-babel-shell-reference-executor nil))))
           (bad-package-error
             (babel-execution-test-condition
              (lambda () (org-kit::babel-execution-package 42))))
           (missing-package-error
             (babel-execution-test-condition
              (lambda ()
                (org-kit::babel-execution-package
                 "ORG-KIT-NO-SUCH-BABEL-EXECUTOR-PACKAGE"))))
           (package-object (org-kit::babel-execution-package (find-package :cl-user)))
           (executor (org-kit:make-babel-common-lisp-reference-executor))
           (captured-output
             (funcall executor
                      "(write-string \"captured\") (+ 1 2)"
                      '(("RESULTS" . "value output"))
                      nil))
           (reader-condition
             (babel-execution-test-condition
              (lambda ()
                (funcall executor
                         "#.(setf cl-org-kit/test::*babel-execution-test-sentinel* t)"
                         nil nil))))
           (document
             (parse-org
              (babel-execution-test-lines
               "#+NAME: task"
               "#+BEGIN_SRC shell"
               "body"
               "#+END_SRC")))
           (block (babel-execution-test-named-source-block document "task"))
           (lowercase-parameter-error
             (babel-execution-test-condition
              (lambda ()
                (org-kit::babel-execution-effective-parameters
                 document block '(("lower" . "value"))))))
           (non-string-parameter-error
             (babel-execution-test-condition
              (lambda ()
                (org-kit::babel-execution-effective-parameters
                 document block '(("UPPER" . 7))))))
           (unsupported-eval
             (babel-execution-test-condition
              (lambda ()
                (org-kit::babel-execution-eval-action
                 '(("EVAL" . "sometimes")) :execute))))
           (missing-hash
             (babel-execution-test-condition
              (lambda () (org-kit::babel-execution-hash nil "body" nil nil))))
           (invalid-hash-function
             (babel-execution-test-condition
              (lambda () (org-kit::babel-execution-hash 7 "body" nil nil))))
           (non-string-hash
             (babel-execution-test-condition
              (lambda ()
                (org-kit::babel-execution-hash
                 (lambda (body parameters variables)
                   (declare (ignore body parameters variables))
                   nil)
                 "body" nil nil))))
           (invalid-confirmation
             (babel-execution-test-condition
              (lambda ()
                (org-kit::babel-execution-confirmed-p block nil 7))))
           (invalid-registry
             (babel-execution-test-condition
              (lambda ()
                (org-kit::%execute-babel-source-block document block nil))))
           (invalid-result-node
             (babel-execution-test-condition
              (lambda ()
                (org-kit::%execute-babel-source-block
                 document block (org-kit:make-callback-registry)
                 :result-node 7)))))
      (with-soft-assertions
        (expect (typep runner-error 'type-error) :to-be t)
        (expect (typep bad-package-error 'type-error) :to-be t)
        (expect (typep missing-package-error 'package-error) :to-be t)
        (expect (eq package-object (find-package :cl-user)) :to-be t)
        (expect captured-output :to-equal "captured")
        (expect (typep reader-condition 'reader-error) :to-be t)
        (expect *babel-execution-test-sentinel* :to-be nil)
        (expect (typep lowercase-parameter-error 'type-error) :to-be t)
        (expect (typep non-string-parameter-error 'type-error) :to-be t)
        (expect (typep unsupported-eval 'org-kit:org-babel-error) :to-be t)
        (when (typep unsupported-eval 'org-kit:org-babel-error)
          (expect (org-kit:org-babel-error-reason unsupported-eval)
                  :to-be :unsupported-eval-option))
        (expect (typep missing-hash 'org-kit:org-babel-error) :to-be t)
        (when (typep missing-hash 'org-kit:org-babel-error)
          (expect (org-kit:org-babel-error-reason missing-hash)
                  :to-be :missing-hash-function))
        (expect (typep invalid-hash-function 'type-error) :to-be t)
        (expect (typep non-string-hash 'type-error) :to-be t)
        (expect (typep invalid-confirmation 'type-error) :to-be t)
        (expect (typep invalid-registry 'type-error) :to-be t)
        (expect (typep invalid-result-node 'type-error) :to-be t))))
  (it "validates parameter names and flag values"
    (let* ((document
             (parse-org
              (babel-execution-test-lines
               "#+NAME: task"
               "#+BEGIN_SRC shell"
               "body"
               "#+END_SRC")))
           (block (babel-execution-test-named-source-block document "task"))
           (invalid-name-error
             (babel-execution-test-condition
              (lambda ()
                (org-kit::babel-execution-effective-parameters
                 document block '((42 . "value"))))))
           (flag-parameters
             (org-kit::babel-execution-effective-parameters
              document block '(("FLAG")))))
      (with-soft-assertions
        (expect (typep invalid-name-error 'type-error) :to-be t)
        (expect (assoc "FLAG" flag-parameters :test #'string=)
                :not :to-be nil)
        (expect (cdr (assoc "FLAG" flag-parameters :test #'string=))
                :to-be nil))))

)
(describe "Babel query confirmation"
  (it "executes a query block when confirmation accepts it"
    (let* ((document
             (parse-org
              (format nil
                      "#+BEGIN_SRC emacs-lisp :eval query~%body~%#+END_SRC~%")))
           (block (babel-execution-test-source-block document))
           (registry (org-kit:make-callback-registry))
           (runs 0)
           (result nil))
      (org-kit:register-babel-executor
       registry "emacs-lisp"
       (lambda (body parameters variables)
         (declare (ignore body parameters variables))
         (incf runs)
         "accepted"))
      (setf result
            (org-kit:execute-babel-source-block
             document block registry
             :confirmation-function
             (lambda (source-block parameters)
               (declare (ignore source-block parameters))
               t)))
      (with-soft-assertions
        (expect (org-kit:babel-execution-result-status result)
                :to-be
                :executed)
        (expect runs :to-be 1)))))

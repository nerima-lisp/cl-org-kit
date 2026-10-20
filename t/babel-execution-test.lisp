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

  (it "uses the explicit shell runner adapter without starting a process"
    (let* ((document
             (parse-org
              (babel-execution-test-lines
               "#+NAME: task"
               "#+BEGIN_SRC shell :session one"
               "echo ready"
               "#+END_SRC")))
           (block (babel-execution-test-named-source-block document "task"))
           (registry (org-kit:make-callback-registry))
           (runner-calls 0)
           (runner
             (lambda (body parameters variables)
               (incf runner-calls)
               (expect body :to-equal "echo ready")
               (expect (cdr (assoc "SESSION" parameters :test #'string=))
                       :to-equal "one")
               (expect variables :to-equal nil)
               "runner result"))
           (adapter (org-kit:make-babel-shell-reference-executor runner)))
      (org-kit:register-babel-executor registry "shell" adapter)
      (let ((result (org-kit:execute-babel-source-block document block registry)))
        (with-soft-assertions
          (expect (eq adapter runner) :to-be t)
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
      (expect (eq caught condition) :to-be t))))

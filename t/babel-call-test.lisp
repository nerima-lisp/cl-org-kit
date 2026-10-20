;;;; t/babel-call-test.lisp

(in-package #:cl-org-kit/test)

(defun babel-call-test-nodes (document type)
  (let ((nodes '()))
    (map-nodes (lambda (node)
                 (when (eq (node-type node) type)
                   (push node nodes)))
               (document-root document))
    (nreverse nodes)))

(defun babel-call-test-parameter-values (parameters name)
  (loop for (key . value) in parameters
        when (string= key name)
          collect value))

(defun babel-call-test-source-document (header call-line)
  (parse-org
   (format nil
           "#+NAME: target~%#+BEGIN_SRC emacs-lisp~@[ ~A~]~%body~%#+END_SRC~%~A~%"
           header call-line)))

(defun babel-call-test-condition (function)
  (handler-case
      (progn (funcall function) nil)
    (error (condition) condition)))
(describe "Babel library variable context"
  (it "resolves target and call variable references in the caller document"
    (let* ((document
             (parse-org
              (format nil
                      "#+NAME: shared-table~%| 11 |~%~%#+NAME: caller-only~%| 22 |~%~%#+CALL: task(call-value=shared-table, caller-only=caller-only)~%")))
           (library
             (parse-org
              (format nil
                      "#+NAME: shared-table~%| 99 |~%~%#+NAME: task~%#+BEGIN_SRC emacs-lisp :var target-value=shared-table~%body~%#+END_SRC~%")))
           (call (first (babel-call-test-nodes document :babel-call)))
           (registry (org-kit:make-callback-registry))
           (observed nil))
      (org-kit:register-babel-executor
       registry "emacs-lisp"
       (lambda (body parameters variables)
         (declare (ignore body parameters))
         (setf observed variables)
         "library result"))
      (let ((result
              (org-kit:execute-babel-call
               document call registry :library-documents (list library))))
        (with-soft-assertions
          (expect (org-kit:babel-execution-result-status result)
                  :to-be
                  :executed)
          (expect observed
                  :to-equal
                  (list (cons "target-value" (list (list 11)))
                        (cons "call-value" (list (list 11)))
                        (cons "caller-only" (list (list 22))))))))))

(describe "Babel call execution"
  (it "executes a local #+CALL with Org parameter precedence"
    (let* ((document
             (parse-org
              (format nil
                      "* Target~%:PROPERTIES:~%:header-args:emacs-lisp: :session target-heading :var first=target :var shared=target~%:END:~%#+NAME: target~%#+BEGIN_SRC emacs-lisp :session block :var shared=block~%body~%#+END_SRC~%* Call~%:PROPERTIES:~%:header-args:emacs-lisp: :session caller-property :var caller=caller-property :var shared=caller-property~%:END:~%#+CALL: target[:session inside :var shared=inside :var inner=2](arg-first, named=3) :session end :var shared=end~%")))
           (library
             (parse-org
              (format nil
                      "#+NAME: target~%#+BEGIN_SRC emacs-lisp :var first=library~%library body~%#+END_SRC")))
           (call (first (babel-call-test-nodes document :babel-call)))
           (registry (org-kit:make-callback-registry))
           (observed nil)
           (original-source (emit-org (document-root document))))
      (org-kit:register-babel-executor
       registry "emacs-lisp"
       (lambda (body parameters variables)
         (setf observed (list body parameters variables))
         "done"))
      (let ((result
              (org-kit:execute-babel-call
               document call registry :library-documents (list library))))
        (with-soft-assertions
          (expect (org-kit:babel-execution-result-status result)
                  :to-be
                  :executed)
          (expect (first observed) :to-equal "body")
          (expect (babel-call-test-parameter-values (second observed) "SESSION")
                  :to-equal
                  '("end"))
          (expect (babel-call-test-parameter-values (second observed) "EXPORTS")
                  :to-equal
                  '("results"))
          (expect (babel-call-test-parameter-values (second observed) "LEXICAL")
                  :to-equal
                  '("no"))
          (expect (babel-call-test-parameter-values (second observed) "VAR")
                  :to-equal
                  '("first=arg-first" "caller=caller-property"
                    "inner=2" "named=3" "shared=end"))
          (expect (third observed)
                  :to-equal
                  '(("first" . "arg-first")
                    ("caller" . "caller-property")
                    ("inner" . 2)
                    ("named" . 3)
                    ("shared" . "end")))
          (progn
            (expect (org-kit:edit-result-p
                     (org-kit:babel-execution-result-edit-result result))
                    :to-be
                    t)
            (expect
             (org-kit:document-text
              (org-kit:edit-result-updated-document
               (org-kit:babel-execution-result-edit-result result)))
             :to-equal
             (format nil "~A#+RESULTS:~%done~%" original-source))
            (expect (emit-org (document-root document)) :to-equal original-source)))))

  (it "resolves supplied library documents after the local document"
    (let* ((document (parse-org "#+CALL: library-task(9)"))
           (call (first (babel-call-test-nodes document :babel-call)))
           (library
             (parse-org
              (format nil "#+NAME: library-task~%#+BEGIN_SRC emacs-lisp :var value=0~%body~%#+END_SRC")))
           (registry (org-kit:make-callback-registry))
           (observed nil))
      (org-kit:register-babel-executor
       registry "emacs-lisp"
       (lambda (body parameters variables)
         (declare (ignore body parameters))
         (setf observed variables)
         "library result"))
      (let ((result
              (org-kit:execute-babel-call
               document call registry :library-documents (list library))))
        (with-soft-assertions
          (expect (org-kit:babel-execution-result-status result)
                  :to-be
                  :executed)
          (expect observed :to-equal '(("value" . 9)))))))

  (it "executes inline calls without using a persistent result anchor"
    (let* ((document
             (parse-org
              (format nil "#+NAME: inline-task~%#+BEGIN_SRC emacs-lisp :var value=0~%body~%#+END_SRC~%~%call_inline-task(8)")))
           (call
             (first (babel-call-test-nodes document :inline-babel-call)))
           (registry (org-kit:make-callback-registry))
           (runs 0))
      (org-kit:register-babel-executor
       registry "emacs-lisp"
       (lambda (body parameters variables)
         (declare (ignore body parameters variables))
         (incf runs)
         "inline result"))
      (let ((result (org-kit:execute-babel-call document call registry)))
        (with-soft-assertions
          (expect (org-kit:babel-execution-result-status result)
                  :to-be
                  :executed)
          (expect runs :to-be 1)
          (expect (org-kit:babel-execution-result-edit-result result)
                  :to-be
                  nil)))))

  (it "applies eval gates and query confirmation to calls"
    (let* ((registry (org-kit:make-callback-registry))
           (runs 0))
      (org-kit:register-babel-executor
       registry "emacs-lisp"
       (lambda (body parameters variables)
         (declare (ignore body parameters variables))
         (incf runs)
         "result"))
      (labels ((run (header &key evaluation-context confirmation-function)
                 (let* ((document
                          (babel-call-test-source-document
                           header "#+CALL: target()"))
                        (call
                          (first
                           (babel-call-test-nodes document :babel-call))))
                   (org-kit:execute-babel-call
                    document call registry
                    :evaluation-context evaluation-context
                    :confirmation-function confirmation-function))))
        (let ((no-result (run ":eval no"))
              (never-result (run ":eval never"))
              (export-result
                (run ":eval no-export" :evaluation-context :export))
              (normal-result
                (run ":eval no-export")))
          (with-soft-assertions
            (expect (org-kit:babel-execution-result-status no-result)
                    :to-be
                    :skipped)
            (expect (org-kit:babel-execution-result-status never-result)
                    :to-be
                    :skipped)
            (expect (org-kit:babel-execution-result-status export-result)
                    :to-be
                    :skipped)
            (expect (org-kit:babel-execution-result-status normal-result)
                    :to-be
                    :executed)
            (expect runs :to-be 1)))
        (let* ((query-document
                 (babel-call-test-source-document
                  ":eval query" "#+CALL: target()"))
               (query-call
                 (first
                  (babel-call-test-nodes query-document :babel-call)))
               (condition
                 (babel-call-test-condition
                  (lambda ()
                    (org-kit:execute-babel-call
                     query-document query-call registry)))))
          (expect (typep condition 'org-kit:org-babel-error) :to-be t)
          (expect (org-kit:org-babel-error-reason condition)
                  :to-be
                  :confirmation-required))
        (let* ((query-document
                 (babel-call-test-source-document
                  ":eval query" "#+CALL: target()"))
               (query-call
                 (first
                  (babel-call-test-nodes query-document :babel-call)))
               (result
                 (org-kit:execute-babel-call
                  query-document query-call registry
                  :confirmation-function (lambda (block parameters)
                                           (declare (ignore block parameters))
                                           nil))))
          (expect (org-kit:babel-execution-result-status result)
                  :to-be
                  :cancelled)
          (expect runs :to-be 1)))))

  (it "uses a matching call result hash and executes on a cache miss"
    (let* ((document
             (parse-org
              (format nil
                      "#+NAME: target~%#+BEGIN_SRC emacs-lisp :cache yes :var value=1~%body~%#+END_SRC~%#+CALL: target()~%#+RESULTS[hit-hash]:~%: previous~%")))
           (call (first (babel-call-test-nodes document :babel-call)))
           (registry (org-kit:make-callback-registry))
           (runs 0))
      (org-kit:register-babel-executor
       registry "emacs-lisp"
       (lambda (body parameters variables)
         (declare (ignore body parameters variables))
         (incf runs)
         "fresh result"))
      (let ((hit
              (org-kit:execute-babel-call
               document call registry
               :hash-function (lambda (body parameters variables) (declare (ignore body parameters variables)) "hit-hash")))
            (miss
              (org-kit:execute-babel-call
               document call registry
               :hash-function (lambda (body parameters variables) (declare (ignore body parameters variables)) "new-hash"))))
        (with-soft-assertions
          (expect (org-kit:babel-execution-result-status hit) :to-be :cached)
          (expect (org-kit:babel-execution-result-status miss) :to-be :executed)
          (expect runs :to-be 1)))))

  (it "signals named reasons for unresolved targets and unregistered executors"
    (let* ((document (parse-org "#+CALL: missing()"))
           (call (first (babel-call-test-nodes document :babel-call)))
           (registry (org-kit:make-callback-registry))
           (missing
             (babel-call-test-condition
              (lambda () (org-kit:execute-babel-call document call registry)))))
      (with-soft-assertions
        (expect (typep missing 'org-kit:org-babel-error) :to-be t)
        (expect (org-kit:org-babel-error-reason missing)
                :to-be
                :unresolved-call-target)))
    (let* ((document (babel-call-test-source-document
                      nil "#+CALL: target()"))
           (call (first (babel-call-test-nodes document :babel-call)))
           (registry (org-kit:make-callback-registry))
           (condition
             (babel-call-test-condition
              (lambda () (org-kit:execute-babel-call document call registry)))))
      (expect (typep condition 'org-kit:org-callback-error) :to-be t)
      (expect (org-kit:org-callback-error-reason condition)
              :to-be
              :unregistered)))

  (it "propagates a named executor timeout through a Babel call"
  (let* ((document
           (babel-call-test-source-document nil "#+CALL: target()"))
         (call (first (babel-call-test-nodes document :babel-call)))
         (registry (org-kit:make-callback-registry))
         (timeout
           (make-condition 'org-kit:org-callback-timeout-error
                           :kind "BABEL-EXECUTOR"
                           :name "EMACS-LISP"))
         (caught nil))
    (org-kit:register-babel-executor
     registry "emacs-lisp"
     (lambda (body parameters variables)
       (declare (ignore body parameters variables))
       (error timeout)))
    (setf caught
          (babel-call-test-condition
           (lambda () (org-kit:execute-babel-call document call registry))))
    (with-soft-assertions
      (expect (typep caught 'org-kit:org-callback-timeout-error) :to-be t)
      (when caught
        (expect (eq caught timeout) :to-be t)
        (expect (org-kit:org-callback-error-kind caught)
                :to-equal "BABEL-EXECUTOR")
        (expect (org-kit:org-callback-error-name caught)
                :to-equal "EMACS-LISP")
        (expect (org-kit:org-callback-error-reason caught)
                :to-be :timeout))))))
)

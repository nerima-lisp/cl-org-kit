;;;; t/babel-vars-test.lisp
(in-package #:cl-org-kit/test)

(defun babel-vars-test-lines (&rest lines)
  (format nil "~{~A~^~%~}" lines))

(defun babel-vars-test-src-blocks (document)
  (let ((blocks '()))
    (map-nodes
     (lambda (node)
       (when (eq (node-type node) :src-block)
         (push node blocks)))
     (document-root document))
    (nreverse blocks)))

(defun babel-vars-test-named-src-block (document name)
  (find name (babel-vars-test-src-blocks document)
        :key (lambda (block) (node-property block :name))
        :test #'string=))

(defun babel-vars-test-condition (function)
  (handler-case
      (progn (funcall function) nil)
    (error (condition) condition)))

(describe "Babel variable resolution"
  (it "resolves literals and named tables and lists without changing values"
    (let* ((document
             (parse-org
              (babel-vars-test-lines
               "#+NAME: sample-table"
               "| 1 | 2 |"
               "| 3 | 4 |"
               ""
               "#+NAME: sample-list"
               "- alpha"
               "- beta"
               ""
               "#+RESULTS: named-result"
               "| 10 | 20 |"
               ""
               "#+NAME: consumer"
               "#+BEGIN_SRC emacs-lisp"
               "unused"
               "#+END_SRC")))
           (consumer (babel-vars-test-named-src-block document "consumer"))
           (variables
             (resolve-babel-variables
              document consumer
              '(("VAR" . "integer=42")
                ("VAR" . "greeting=\"hello, world\"")
                ("VAR" . "matrix=sample-table")
                ("VAR" . "named=named-result")
                ("VAR" . "items=sample-list")))))
      (with-soft-assertions
        (expect (node-type consumer) :to-be :src-block)
        (expect variables
                :to-equal
                '(("integer" . 42)
                  ("greeting" . "hello, world")
                  ("matrix" . ((1 2) (3 4)))
                  ("named" . ((10 20)))
                  ("items" . ("alpha" "beta")))))))

  (it "rejects Lisp-looking values without evaluating them"
    (let* ((document
             (parse-org
              (babel-vars-test-lines
               "#+NAME: consumer"
               "#+BEGIN_SRC emacs-lisp"
               "unused"
               "#+END_SRC")))
           (consumer (babel-vars-test-named-src-block document "consumer"))
           (condition nil)
           (output
             (with-output-to-string (stream)
               (let ((*standard-output* stream))
                 (setf condition
                       (babel-vars-test-condition
                        (lambda ()
                          (resolve-babel-variables
                           document consumer
                           '(("VAR" . "value=(write-char #\\X)"))))))))))
      (with-soft-assertions
        (expect (typep condition 'org-babel-error) :to-be t)
        (when (typep condition 'org-babel-error)
          (expect (org-babel-error-reason condition)
                  :to-equal :unsupported-expression))
        (expect output :to-equal ""))))

  (it "resolves a named source block through the callback with variables"
    (let* ((document
             (parse-org
              (babel-vars-test-lines
               "#+NAME: result-source"
               "#+BEGIN_SRC emacs-lisp :var amount=7"
               "(+ amount 1)"
               "#+END_SRC"
               ""
               "#+NAME: consumer"
               "#+BEGIN_SRC emacs-lisp"
               "unused"
               "#+END_SRC")))
           (consumer (babel-vars-test-named-src-block document "consumer"))
           (resolver-calls '())
           (resolver-result '("resolved result" 8 (9 10)))
           (variables
             (resolve-babel-variables
              document consumer '(("VAR" . "prior=result-source"))
              :result-resolver
              (lambda (block-name parameters)
                (push (list block-name parameters) resolver-calls)
                resolver-result))))
      (with-soft-assertions
        (expect variables
                :to-equal
                `(("prior" . ,resolver-result)))
        (expect (nreverse resolver-calls)
                :to-equal
                '(("result-source" (("amount" . 7))))))))

  (it "resolves a named call alias through its executable target"
    (let* ((document
             (parse-org
              (babel-vars-test-lines
               "#+NAME: call-target"
               "#+BEGIN_SRC emacs-lisp :var amount=7"
               "(+ amount 1)"
               "#+END_SRC"
               ""
               "#+NAME: call-alias"
               "#+CALL: call-target(amount=11)"
               ""
               "#+NAME: consumer"
               "#+BEGIN_SRC emacs-lisp"
               "unused"
               "#+END_SRC")))
           (consumer (babel-vars-test-named-src-block document "consumer"))
           (resolver-calls '())
           (resolver-result '(11 "resolved through call"))
           (variables
             (resolve-babel-variables
              document consumer '(("VAR" . "call-result=call-alias"))
              :result-resolver
              (lambda (block-name parameters)
                (push (list block-name parameters) resolver-calls)
                resolver-result))))
      (with-soft-assertions
        (expect variables
                :to-equal
                `(("call-result" . ,resolver-result)))
        (expect (nreverse resolver-calls)
                :to-equal
                '(("call-target" (("amount" . 11))))))))

  (it "signals a named error when a referenced result has no resolver"
    (let* ((document
             (parse-org
              (babel-vars-test-lines
               "#+NAME: result-source"
               "#+BEGIN_SRC emacs-lisp"
               "(error \"source execution probe\")"
               "#+END_SRC"
               ""
               "#+NAME: consumer"
               "#+BEGIN_SRC emacs-lisp"
               "unused"
               "#+END_SRC")))
           (consumer (babel-vars-test-named-src-block document "consumer"))
           (condition
             (babel-vars-test-condition
              (lambda ()
                (resolve-babel-variables
                 document consumer
                 '(("VAR" . "prior=result-source")))))))
      (with-soft-assertions
        (expect (typep condition 'org-babel-error) :to-be t)
        (when (typep condition 'org-babel-error)
          (expect (org-babel-error-reason condition)
                  :to-equal :missing-result-resolver))))))

  (it "rejects a cyclic value returned by the result resolver"
    (let* ((document
             (parse-org
              (babel-vars-test-lines
               "#+NAME: result-source"
               "#+BEGIN_SRC emacs-lisp"
               "unused"
               "#+END_SRC"
               ""
               "#+NAME: consumer"
               "#+BEGIN_SRC emacs-lisp"
               "unused"
               "#+END_SRC")))
           (consumer (babel-vars-test-named-src-block document "consumer"))
           (condition
             (babel-vars-test-condition
              (lambda ()
                (resolve-babel-variables
                 document consumer '(("VAR" . "value=result-source"))
                 :result-resolver
                 (lambda (block-name variables)
                   (declare (ignore block-name variables))
                   (let ((cycle (list nil)))
                     (setf (car cycle) cycle)
                     cycle)))))))
      (with-soft-assertions
        (expect (typep condition 'org-babel-error) :to-be t)
        (when (typep condition 'org-babel-error)
          (expect (org-babel-error-reason condition)
                  :to-equal :cyclic-result)))))

  (it "resolves numeric literals and rejects non-ASCII digits and unsupported resolver results"
    (let* ((document
             (parse-org
              (babel-vars-test-lines
               "#+NAME: result-source"
               "#+BEGIN_SRC emacs-lisp"
               "unused"
               "#+END_SRC"
               ""
               "#+NAME: consumer"
               "#+BEGIN_SRC emacs-lisp"
               "unused"
               "#+END_SRC")))
           (consumer (babel-vars-test-named-src-block document "consumer"))
           (variables
             (resolve-babel-variables
              document consumer
              '(("VAR" . "integer=42")
                ("VAR" . "negative=-7")
                ("VAR" . "ratio=3/4")
                ("VAR" . "decimal=1.25e1"))))
           (fullwidth-condition
             (babel-vars-test-condition
              (lambda ()
                (resolve-babel-variables
                 document consumer '(("VAR" . "value=４２"))))))
           (arabic-condition
             (babel-vars-test-condition
              (lambda ()
                (resolve-babel-variables
                 document consumer '(("VAR" . "value=٤٢"))))))
           (unsupported-condition
             (babel-vars-test-condition
              (lambda ()
                (resolve-babel-variables
                 document consumer '(("VAR" . "value=result-source"))
                 :result-resolver
                 (lambda (block-name parameters)
                   (declare (ignore block-name parameters))
                   (make-hash-table :test #'equal)))))))
      (with-soft-assertions
        (expect variables
                :to-equal
                '(("integer" . 42)
                  ("negative" . -7)
                  ("ratio" . 3/4)
                  ("decimal" . 12.5d0)))
        (expect (typep fullwidth-condition 'org-babel-error) :to-be t)
        (expect (typep arabic-condition 'org-babel-error) :to-be t)
        (expect (typep unsupported-condition 'org-babel-error) :to-be t)
        (when (typep fullwidth-condition 'org-babel-error)
          (expect (org-babel-error-reason fullwidth-condition)
                  :to-equal :unresolved-reference))
        (when (typep arabic-condition 'org-babel-error)
          (expect (org-babel-error-reason arabic-condition)
                  :to-equal :unresolved-reference))
        (when (typep unsupported-condition 'org-babel-error)
          (expect (org-babel-error-reason unsupported-condition)
                  :to-equal :unsupported-result)))))

;;;; t/babel-noweb-test.lisp
(in-package #:cl-org-kit/test)

(defun babel-noweb-test-lines (&rest lines)
  (format nil "~{~A~^~%~}" lines))

(defun babel-noweb-test-src-blocks (document)
  (let ((blocks '()))
    (map-nodes
     (lambda (node)
       (when (eq (node-type node) :src-block)
         (push node blocks)))
     (document-root document))
    (nreverse blocks)))

(defun babel-noweb-test-named-src-block (document name)
  (find name (babel-noweb-test-src-blocks document)
        :key (lambda (block) (node-property block :name))
        :test #'string=))

(defun babel-noweb-test-expand (document name context &optional result-resolver)
  (expand-babel-noweb
   document (babel-noweb-test-named-src-block document name)
   :context context :result-resolver result-resolver))

(defun babel-noweb-test-condition (function)
  (handler-case
      (progn (funcall function) nil)
    (error (condition) condition)))

(describe "Babel noweb expansion"
  (it "applies noweb modes in each expansion context"
    (let* ((document
             (parse-org
              (babel-noweb-test-lines
               "#+NAME: fragment"
               "#+BEGIN_SRC text"
               "fragment"
               "#+END_SRC"
               ""
               "#+NAME: yes-block"
               "#+BEGIN_SRC text :noweb \"yes\""
               "prefix"
               "<<fragment>>"
               "suffix"
               "#+END_SRC"
               ""
               "#+NAME: no-block"
               "#+BEGIN_SRC text :noweb no"
               "prefix"
               "<<fragment>>"
               "suffix"
               "#+END_SRC"
               ""
               "#+NAME: tangle-block"
               "#+BEGIN_SRC text :noweb tangle"
               "prefix"
               "<<fragment>>"
               "suffix"
               "#+END_SRC"
               ""
               "#+NAME: strip-export-block"
               "#+BEGIN_SRC text :noweb strip-export"
               "prefix"
               "<<fragment>>"
               "suffix"
               "#+END_SRC"
               ""
               "#+NAME: eval-block"
               "#+BEGIN_SRC text :noweb eval"
               "prefix"
               "<<fragment>>"
               "suffix"
               "#+END_SRC"
               ""
               "#+NAME: no-export-block"
               "#+BEGIN_SRC text :noweb no-export"
               "prefix"
               "<<fragment>>"
               "suffix"
               "#+END_SRC"
               ""
               "#+NAME: strip-tangle-block"
               "#+BEGIN_SRC text :noweb strip-tangle"
               "prefix"
               "<<fragment>>"
               "suffix"
               "#+END_SRC")))
           (expanded (babel-noweb-test-lines "prefix" "fragment" "suffix"))
           (unexpanded
             (babel-noweb-test-lines "prefix" "<<fragment>>" "suffix"))
           (stripped (babel-noweb-test-lines "prefix" "" "suffix"))
           (cases
             (list
              (list "yes-block" :eval expanded)
              (list "yes-block" :tangle expanded)
              (list "yes-block" :export expanded)
              (list "no-block" :eval unexpanded)
              (list "no-block" :tangle unexpanded)
              (list "no-block" :export unexpanded)
              (list "tangle-block" :eval unexpanded)
              (list "tangle-block" :tangle expanded)
              (list "tangle-block" :export unexpanded)
              (list "strip-export-block" :eval expanded)
              (list "strip-export-block" :tangle expanded)
              (list "strip-export-block" :export stripped)
              (list "eval-block" :eval expanded)
              (list "eval-block" :tangle unexpanded)
              (list "eval-block" :export unexpanded)
              (list "no-export-block" :eval expanded)
              (list "no-export-block" :tangle expanded)
              (list "no-export-block" :export unexpanded)
              (list "strip-tangle-block" :eval expanded)
              (list "strip-tangle-block" :tangle stripped)
              (list "strip-tangle-block" :export expanded))))
      (with-soft-assertions
        (dolist (case cases)
          (destructuring-bind (name context expected) case
            (expect (babel-noweb-test-expand document name context)
                    :to-equal expected))))))

  (it "signals named conditions for unsupported modes and contexts"
    (let* ((document
             (parse-org
              (babel-noweb-test-lines
               "#+NAME: invalid-mode"
               "#+BEGIN_SRC text :noweb unsupported"
               "body"
               "#+END_SRC")))
           (block (babel-noweb-test-named-src-block document "invalid-mode"))
           (mode-condition
             (babel-noweb-test-condition
              (lambda () (expand-babel-noweb document block))))
           (context-condition
             (babel-noweb-test-condition
              (lambda ()
                (expand-babel-noweb document block :context :other)))))
      (with-soft-assertions
        (expect (typep mode-condition 'org-babel-error) :to-be t)
        (when (typep mode-condition 'org-babel-error)
          (expect (org-babel-error-reason mode-condition)
                  :to-equal :invalid-noweb-mode))
        (expect (typep context-condition 'type-error) :to-be t))))

  (it "expands named blocks and noweb-ref groups in document order"
    (let* ((document
             (parse-org
              (babel-noweb-test-lines
               "#+NAME: first-fragment"
               "#+BEGIN_SRC text :noweb-ref shared-fragments"
               "first"
               "#+END_SRC"
               ""
               "#+NAME: second-fragment"
               "#+BEGIN_SRC text :noweb-ref shared-fragments"
               "second"
               "#+END_SRC"
               ""
               "#+NAME: consumer"
               "#+BEGIN_SRC text :noweb yes"
               "before"
               "<<first-fragment>>"
               "<<shared-fragments>>"
               "<<missing-fragment>>"
               "after"
               "#+END_SRC"))))
      (with-soft-assertions
        (expect (babel-noweb-test-expand document "consumer" :eval)
                :to-equal
                (babel-noweb-test-lines
                 "before" "first" "first" "second" "" "after")))))

  (it "reads quoted noweb parameters and reference groups as strings"
    (let* ((document
             (parse-org
              (babel-noweb-test-lines
               "#+NAME: first-fragment"
               "#+BEGIN_SRC text :noweb-ref \"shared-fragments\" :noweb-sep \" & \""
               "first"
               "#+END_SRC"
               ""
               "#+NAME: second-fragment"
               "#+BEGIN_SRC text :noweb-ref \"shared-fragments\""
               "second"
               "#+END_SRC"
               ""
               "#+NAME: consumer"
               "#+BEGIN_SRC text :noweb \"yes\""
               "<<shared-fragments>>"
               "#+END_SRC"))))
      (expect (babel-noweb-test-expand document "consumer" :eval)
              :to-equal "first & second")))

  (it "repeats the source prefix on inserted lines unless disabled"
    (let* ((document
             (parse-org
              (babel-noweb-test-lines
               "#+NAME: multiline"
               "#+BEGIN_SRC text :noweb yes"
               "alpha"
               "beta"
               ""
               "gamma"
               "#+END_SRC"
               ""
               "#+NAME: prefix-enabled"
               "#+BEGIN_SRC text :noweb yes"
               "  <<multiline>>"
               "#+END_SRC"
               ""
               "#+NAME: prefix-disabled"
               "#+BEGIN_SRC text :noweb yes :noweb-prefix \"no\""
               "  <<multiline>>"
               "#+END_SRC"))))
      (with-soft-assertions
        (expect (babel-noweb-test-expand document "prefix-enabled" :eval)
                :to-equal (babel-noweb-test-lines
                           "  alpha" "  beta" "  " "  gamma"))
        (expect (babel-noweb-test-expand document "prefix-disabled" :eval)
                :to-equal (babel-noweb-test-lines "  alpha" "beta" "" "gamma")))))

  (it "resolves call references and inserts string results as written"
    (let* ((document
             (parse-org
              (babel-noweb-test-lines
               "#+NAME: calculation"
               "#+BEGIN_SRC emacs-lisp :var amount=7"
               "(+ amount 1)"
               "#+END_SRC"
               ""
               "#+NAME: consumer"
               "#+BEGIN_SRC text :noweb yes"
               "value=<<calculation()>>"
               "#+END_SRC"
               ""
               "#+NAME: string-consumer"
               "#+BEGIN_SRC text :noweb yes :noweb-prefix no"
               "<<calculation()>>"
               "#+END_SRC")))
           (resolver-calls '())
           (result
             (babel-noweb-test-expand
              document "consumer" :eval
              (lambda (block-name variables)
                (push (list block-name variables) resolver-calls)
                (list :value "computed"))))
           (string-result
             (babel-noweb-test-expand
              document "string-consumer" :eval
              (lambda (block-name variables)
                (declare (ignore block-name variables))
                (format nil "first~%second")))))
      (with-soft-assertions
        (expect result :to-equal "value=(:VALUE \"computed\")")
        (expect string-result :to-equal (format nil "first~%second"))
        (expect (nreverse resolver-calls)
                :to-equal (quote (("calculation" (("amount" . 7)))))))))

  (it "signals a circular-reference error for recursive named blocks"
    (let* ((document
             (parse-org
              (babel-noweb-test-lines
               "#+NAME: first"
               "#+BEGIN_SRC text :noweb yes"
               "<<second>>"
               "#+END_SRC"
               ""
               "#+NAME: second"
               "#+BEGIN_SRC text :noweb yes"
               "<<first>>"
               "#+END_SRC"
               ""
               "#+NAME: consumer"
               "#+BEGIN_SRC text :noweb yes"
               "<<first>>"
               "#+END_SRC")))
           (condition
             (babel-noweb-test-condition
              (lambda ()
                (babel-noweb-test-expand document "consumer" :eval)))))
      (with-soft-assertions
        (expect (typep condition 'org-babel-error) :to-be t)
        (when (typep condition 'org-babel-error)
          (expect (org-babel-error-reason condition)
                  :to-equal :circular-reference)))))

  (it "enforces expansion depth and character limits at the boundary"
    (let* ((over-limit-document
             (parse-org
              (babel-noweb-test-lines
               "#+NAME: fragment"
               "#+BEGIN_SRC text"
               "content"
               "#+END_SRC"
               ""
               "#+NAME: consumer"
               "#+BEGIN_SRC text :noweb yes"
               "<<fragment>>"
               "#+END_SRC")
              :limits (make-limits :max-babel-expansion-depth 0)))
           (condition
             (babel-noweb-test-condition
              (lambda ()
                (babel-noweb-test-expand over-limit-document "consumer" :eval))))
           (at-limit-document
             (parse-org
              (babel-noweb-test-lines
               "#+NAME: fragment"
               "#+BEGIN_SRC text :noweb yes"
               "abc"
               "#+END_SRC"
               ""
               "#+NAME: consumer"
               "#+BEGIN_SRC text :noweb yes"
               "<<fragment>>"
               "#+END_SRC")
              :limits (make-limits :max-babel-expansion-depth 1
                                  :max-babel-expanded-characters 6)))
           (expanded
             (babel-noweb-test-expand at-limit-document "consumer" :eval)))
      (with-soft-assertions
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when (typep condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name condition)
                  :to-equal :max-babel-expansion-depth))
        (expect expanded :to-equal "abc"))))

  (it "enforces the cumulative noweb expanded-character limit"
    (let* ((document
             (parse-org
              (babel-noweb-test-lines
               "#+NAME: fragment"
               "#+BEGIN_SRC text"
               "abc"
               "#+END_SRC"
               ""
               "#+NAME: consumer"
               "#+BEGIN_SRC text :noweb yes"
               "<<fragment>><<fragment>>"
               "#+END_SRC")
              :limits (make-limits :max-babel-expanded-characters 5)))
           (condition
             (babel-noweb-test-condition
              (lambda ()
                (babel-noweb-test-expand document "consumer" :eval)))))
      (with-soft-assertions
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when (typep condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name condition)
                  :to-equal :max-babel-expanded-characters)))))

  (it "bounds resolver result traversal for lists and vectors"
    (let* ((document
             (parse-org
              (babel-noweb-test-lines
               "#+NAME: calculation"
               "#+BEGIN_SRC emacs-lisp"
               "unused"
               "#+END_SRC"
               ""
               "#+NAME: consumer"
               "#+BEGIN_SRC text :noweb yes"
               "<<calculation()>>"
               "#+END_SRC")))
           (result-values (list (loop repeat 1000001 collect 1)
                                (make-array 1000001 :initial-element 1))))
      (dolist (value result-values)
        (let ((condition
                (babel-noweb-test-condition
                 (lambda ()
                   (babel-noweb-test-expand
                    document "consumer" :eval
                    (lambda (block-name variables)
                      (declare (ignore block-name variables))
                      value))))))
          (with-soft-assertions
            (expect (typep condition 'org-resource-limit-error) :to-be t)
            (when (typep condition 'org-resource-limit-error)
              (expect (org-resource-limit-error-limit-name condition)
                      :to-equal :max-babel-result-elements)
              (expect (org-resource-limit-error-limit condition) :to-be 1000000)
              (expect (org-resource-limit-error-actual condition)
                      :to-be 1000001)))))))

  (it "checks the character budget while formatting resolver results"
    (let* ((document
             (parse-org
              (babel-noweb-test-lines
               "#+NAME: calculation"
               "#+BEGIN_SRC emacs-lisp"
               "unused"
               "#+END_SRC"
               ""
               "#+NAME: consumer"
               "#+BEGIN_SRC text :noweb yes"
               "<<calculation()>>"
               "#+END_SRC")
              :limits (make-limits :max-babel-expanded-characters 16)))
           (condition
             (babel-noweb-test-condition
              (lambda ()
                (babel-noweb-test-expand
                 document "consumer" :eval
                 (lambda (block-name variables)
                   (declare (ignore block-name variables))
                   (loop repeat 10000 collect 1)))))))
      (with-soft-assertions
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when (typep condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name condition)
                  :to-equal :max-babel-expanded-characters)
          (expect (org-resource-limit-error-limit condition) :to-be 16)
          (expect (org-resource-limit-error-actual condition) :to-be 17)))))

  (it "requires a result resolver for call references"
    (let* ((document
             (parse-org
              (babel-noweb-test-lines
               "#+NAME: calculation"
               "#+BEGIN_SRC emacs-lisp"
               "(+ 1 2)"
               "#+END_SRC"
               ""
               "#+NAME: consumer"
               "#+BEGIN_SRC text :noweb yes"
               "<<calculation()>>"
               "#+END_SRC")))
           (condition
             (babel-noweb-test-condition
              (lambda ()
                (babel-noweb-test-expand document "consumer" :eval)))))
      (with-soft-assertions
        (expect (typep condition 'org-babel-error) :to-be t)
        (when (typep condition 'org-babel-error)
          (expect (org-babel-error-reason condition)
                  :to-equal :missing-result-resolver)))))
  (it "normalizes mixed line endings and prefixes inserted lines"
    (let* ((document (parse-org ""))
           (context (org-kit::make-babel-noweb-context :document document))
           (mixed-lines
             (concatenate 'string
                          "alpha" (string #\Return) (string #\Newline)
                          "beta" (string #\Return)
                          "gamma" (string #\Newline))))
      (with-soft-assertions
        (expect (org-kit::babel-noweb-normalize-body mixed-lines)
                :to-equal
                (concatenate 'string "alpha" (string #\Return) (string #\Newline)
                             "beta" (string #\Return) "gamma"))
        (expect (org-kit::babel-noweb-normalize-body
                 (concatenate 'string "alpha" (string #\Return) (string #\Newline)))
                :to-equal "alpha")
        (expect (org-kit::babel-noweb-normalize-body
                 (concatenate 'string "alpha" (string #\Return)))
                :to-equal "alpha")
        (expect (org-kit::babel-noweb-normalize-body "alpha")
                :to-equal "alpha")
        (expect (org-kit::babel-noweb-prefix-later-lines context mixed-lines "  ")
                :to-equal (format nil "alpha~%  beta~%  gamma~%"))
        (expect (org-kit::babel-noweb-prefix-later-lines context "line" "")
                :to-equal "line")
        (expect (org-kit::babel-noweb-prefix-later-lines context "" "  ")
                :to-equal ""))))
  (it "keeps malformed delimiters and names result-reference failures"
    (let* ((document
             (parse-org
              (babel-noweb-test-lines
               "#+NAME: malformed-delimiters"
               "#+BEGIN_SRC text :noweb yes"
               "before <<>> | << leading>> | <<trailing >> | <<unterminated"
               "#+END_SRC"
               ""
               "#+NAME: malformed-result"
               "#+BEGIN_SRC text :noweb yes"
               "<<calculation(amount=7>>"
               "#+END_SRC"
               ""
               "#+NAME: missing-result"
               "#+BEGIN_SRC text :noweb yes"
               "<<absent()>>"
               "#+END_SRC")))
           (preserved
             (babel-noweb-test-expand document "malformed-delimiters" :eval))
           (malformed-condition
             (babel-noweb-test-condition
              (lambda ()
                (babel-noweb-test-expand document "malformed-result" :eval))))
           (missing-condition
             (babel-noweb-test-condition
              (lambda ()
                (babel-noweb-test-expand document "missing-result" :eval)))))
      (with-soft-assertions
        (expect preserved
                :to-equal "before <<>> | << leading>> | <<trailing >> | <<unterminated")
        (expect (typep malformed-condition 'org-babel-error) :to-be t)
        (when (typep malformed-condition 'org-babel-error)
          (expect (org-babel-error-reason malformed-condition)
                  :to-equal :invalid-noweb-reference))
        (expect (typep missing-condition 'org-babel-error) :to-be t)
        (when (typep missing-condition 'org-babel-error)
          (expect (org-babel-error-reason missing-condition)
                  :to-equal :unresolved-reference)))))
  (it "resolves result arguments and named call aliases without evaluating source blocks"
    (let* ((document
             (parse-org
              (babel-noweb-test-lines
               "#+NAME: calculation"
               "#+BEGIN_SRC emacs-lisp :var amount=7"
               "(+ amount 1)"
               "#+END_SRC"
               ""
               "#+NAME: call-alias"
               "#+CALL: calculation(amount=11)"
               ""
               "#+NAME: block-consumer"
               "#+BEGIN_SRC text :noweb yes"
               "<<calculation(amount=9)>>"
               "#+END_SRC"
               ""
               "#+NAME: call-consumer"
               "#+BEGIN_SRC text :noweb yes"
               "<<call-alias()>>"
               "#+END_SRC"
               ""
               "#+NAME: data"
               "| 1 |"
               ""
               "#+NAME: unsupported-consumer"
               "#+BEGIN_SRC text :noweb yes"
               "<<data()>>"
               "#+END_SRC")))
           (resolver-calls '())
           (block-result
             (babel-noweb-test-expand
              document "block-consumer" :eval
              (lambda (block-name variables)
                (push (list block-name variables) resolver-calls)
                (format nil "~A=~A" block-name
                        (cdr (assoc "amount" variables :test #'string=))))))
           (call-result
             (babel-noweb-test-expand
              document "call-consumer" :eval
              (lambda (block-name variables)
                (push (list block-name variables) resolver-calls)
                (format nil "~A=~A" block-name
                        (cdr (assoc "amount" variables :test #'string=))))))
           (unsupported-condition
             (babel-noweb-test-condition
              (lambda ()
                (babel-noweb-test-expand document "unsupported-consumer" :eval)))))
      (with-soft-assertions
        (expect block-result :to-equal "calculation=9")
        (expect call-result :to-equal "calculation=11")
        (expect (nreverse resolver-calls)
                :to-equal
                (list (list "calculation" (list (cons "amount" 9)))
                      (list "calculation" (list (cons "amount" 11)))))
        (expect (typep unsupported-condition 'org-babel-error) :to-be t)
        (when (typep unsupported-condition 'org-babel-error)
          (expect (org-babel-error-reason unsupported-condition)
                  :to-equal :unsupported-reference)))))
  (it "strips references inside an expanded source block for export"
    (let ((document
            (parse-org
             (babel-noweb-test-lines
              "#+NAME: nested-target"
              "#+BEGIN_SRC text"
              "target"
              "#+END_SRC"
              ""
              "#+NAME: nested-fragment"
              "#+BEGIN_SRC text :noweb strip-export"
              "start <<nested-target>> end"
              "#+END_SRC"
              ""
              "#+NAME: consumer"
              "#+BEGIN_SRC text :noweb yes"
              "<<nested-fragment>>"
              "#+END_SRC"))))
      (expect (babel-noweb-test-expand document "consumer" :export)
              :to-equal "start  end")))
  (it "validates the noweb target and resolver argument"
    (let* ((document
             (parse-org
              (babel-noweb-test-lines
               "#+NAME: consumer"
               "#+BEGIN_SRC text :noweb yes"
               "body"
               "#+END_SRC")))
           (block (babel-noweb-test-named-src-block document "consumer"))
           (target-condition
             (babel-noweb-test-condition
              (lambda ()
                (expand-babel-noweb document (document-root document)))))
           (resolver-condition
             (babel-noweb-test-condition
              (lambda ()
                (expand-babel-noweb document block :result-resolver :not-a-function)))))
      (with-soft-assertions
        (expect (typep target-condition 'org-babel-error) :to-be t)
        (when (typep target-condition 'org-babel-error)
          (expect (org-babel-error-reason target-condition)
                  :to-equal :invalid-target))
        (expect (typep resolver-condition 'type-error) :to-be t)))))

(describe "Babel noweb reference edge cases"
  (it "uses noweb-ref names in circular-reference diagnostics"
    (let* ((document
             (parse-org
              (babel-noweb-test-lines
               "#+BEGIN_SRC text :noweb yes :noweb-ref cycle-a"
               "<<cycle-b>>"
               "#+END_SRC"
               ""
               "#+BEGIN_SRC text :noweb yes :noweb-ref cycle-b"
               "<<cycle-a>>"
               "#+END_SRC"
               ""
               "#+NAME: consumer"
               "#+BEGIN_SRC text :noweb yes"
               "<<cycle-a>>"
               "#+END_SRC")))
           (condition
             (babel-noweb-test-condition
              (lambda ()
                (babel-noweb-test-expand document "consumer" :eval)))))
      (with-soft-assertions
        (expect (typep condition 'org-babel-error) :to-be t)
        (when (typep condition 'org-babel-error)
          (expect (org-babel-error-reason condition)
                  :to-equal :circular-reference)))))

  (it "rejects an empty result-reference name and keeps the first duplicate name"
    (let* ((malformed-document
             (parse-org
              (babel-noweb-test-lines
               "#+NAME: consumer"
               "#+BEGIN_SRC text :noweb yes"
               "<<()>>"
               "#+END_SRC")))
           (malformed-condition
             (babel-noweb-test-condition
              (lambda ()
                (babel-noweb-test-expand malformed-document "consumer" :eval))))
           (duplicate-document
             (parse-org
              (babel-noweb-test-lines
               "#+NAME: duplicate"
               "#+BEGIN_SRC text"
               "first"
               "#+END_SRC"
               ""
               "#+NAME: duplicate"
               "#+BEGIN_SRC text"
               "second"
               "#+END_SRC"
               ""
               "#+NAME: consumer"
               "#+BEGIN_SRC text :noweb yes"
               "<<duplicate>>"
               "#+END_SRC"))))
      (with-soft-assertions
        (expect (typep malformed-condition 'org-babel-error) :to-be t)
        (when (typep malformed-condition 'org-babel-error)
          (expect (org-babel-error-reason malformed-condition)
                  :to-equal :invalid-noweb-reference))
        (expect (babel-noweb-test-expand duplicate-document "consumer" :eval)
                :to-equal "first"))))

  (it "normalizes empty and one-newline bodies"
    (with-soft-assertions
      (expect (org-kit::babel-noweb-normalize-body "") :to-equal "")
      (expect (org-kit::babel-noweb-normalize-body (string #\Newline))
              :to-equal "")))

  (it "covers default context and reference helper edge cases"
    (let* ((trailing-newline (format nil "line~%"))
           (trailing-return (format nil "line~C" #\Return))
           (document
             (parse-org
              (babel-noweb-test-lines
               "#+NAME: fragment"
               "#+BEGIN_SRC text"
               "fragment"
               "#+END_SRC"
               ""
               "#+NAME: consumer"
               "#+BEGIN_SRC text :noweb yes"
               "<<fragment>>"
               "#+END_SRC")))
           (block (babel-noweb-test-named-src-block document "consumer"))
           (context (org-kit::make-babel-noweb-context :document document)))
      (with-soft-assertions
        (expect (expand-babel-noweb document block) :to-equal "fragment")
        (expect (org-kit::babel-noweb-normalize-parameter-value :opaque)
                :to-equal :opaque)
        (expect (org-kit::babel-noweb-prefix-later-lines
                 context trailing-newline "  ")
                :to-equal trailing-newline)
        (expect (org-kit::babel-noweb-prefix-later-lines
                 context trailing-return "  ")
                :to-equal trailing-newline)
        (expect (null (org-kit::babel-noweb-delimiters "<< leading>>" 0))
                :to-be t)
        (expect (null (org-kit::babel-noweb-delimiters "<<trailing >>" 0))
                :to-be t)))))

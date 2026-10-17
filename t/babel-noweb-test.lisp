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
                :to-equal
                '((\"calculation\" ((\"amount\" . 7))))))))

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
                  :to-equal :missing-result-resolver))))))

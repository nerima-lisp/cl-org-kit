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
(defun babel-vars-test-resolve-with-variables (document src-block variables &rest resolver-options)
  (setf (node-property src-block :header)
        (format nil "~{ :var ~A~}"
                (mapcar (function cdr) variables)))
  (apply (function org-kit:resolve-babel-variables)
         document src-block resolver-options))

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
             (babel-vars-test-resolve-with-variables document consumer
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
                          (babel-vars-test-resolve-with-variables document consumer
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
             (babel-vars-test-resolve-with-variables document consumer '(("VAR" . "prior=result-source"))
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
             (babel-vars-test-resolve-with-variables document consumer '(("VAR" . "call-result=call-alias"))
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
                (babel-vars-test-resolve-with-variables document consumer
                 '(("VAR" . "prior=result-source")))))))
      (with-soft-assertions
        (expect (typep condition 'org-babel-error) :to-be t)
        (when (typep condition 'org-babel-error)
          (expect (org-babel-error-reason condition)
                  :to-equal :missing-result-resolver))))))
(describe "Babel variable edge cases"

(it "covers assignment and numeric token boundaries"
    (let* ((document
            (parse-org
             (babel-vars-test-lines
              "#+NAME: number-table"
              "| +7 | .5 | -.25 | 1.25e+2 | 1e-2 | /3 | 1e |"
              ""
              "#+NAME: duplicate-table"
              "| 1 |"
              ""
              "#+NAME: duplicate-table"
              "| 2 |"
              ""
              "#+NAME: consumer"
              "#+BEGIN_SRC emacs-lisp"
              "unused"
              "#+END_SRC")))
           (consumer (babel-vars-test-named-src-block document "consumer"))
           (variables
            (babel-vars-test-resolve-with-variables
             document consumer
             '(("VAR" . "empty=")
               ("VAR" . "numbers=number-table")
               ("VAR" . "duplicate=duplicate-table")
               ("VAR" . "same=1")
               ("VAR" . "other=2")
               ("VAR" . "same=3"))))
           (missing-equals
            (babel-vars-test-condition
             (lambda ()
               (babel-vars-test-resolve-with-variables
                document consumer '(("VAR" . "malformed"))))))
           (empty-name
            (babel-vars-test-condition
             (lambda ()
               (babel-vars-test-resolve-with-variables
                document consumer '(("VAR" . "=value"))))))
           (zero-denominator
            (babel-vars-test-condition
             (lambda ()
               (babel-vars-test-resolve-with-variables
                document consumer '(("VAR" . "bad-number=3/0")))))))
      (with-soft-assertions
       (expect variables
               :to-equal
               '(("empty" . "") ("numbers" . ((7 0.5d0 -0.25d0 125.0d0 0.01d0 "/3" "1e"))) ("duplicate" . ((1))) ("other" . 2) ("same" . 3)))
       (progn
         (dolist (condition (list missing-equals empty-name))
           (expect (typep condition (quote org-babel-error)) :to-be t)
           (when (typep condition (quote org-babel-error))
             (expect (org-babel-error-reason condition)
                     :to-equal :invalid-variable-assignment)))
         (expect (typep zero-denominator (quote org-babel-error)) :to-be t)
         (when (typep zero-denominator (quote org-babel-error))
           (expect (org-babel-error-reason zero-denominator)
                   :to-equal :invalid-literal))))))

(it "decodes supported string escapes and rejects malformed literals"
  (let* ((document
           (parse-org
            (babel-vars-test-lines
             "#+NAME: consumer"
             "#+BEGIN_SRC emacs-lisp"
             "unused"
             "#+END_SRC")))
         (consumer (babel-vars-test-named-src-block document "consumer"))
         (literal
           (format nil "\"a~C\"b~C~Cc~Cnd~Cre~Ctf\""
                   #\\ #\\ #\\ #\\ #\\ #\\))
         (variables
           (babel-vars-test-resolve-with-variables
            document consumer
            (list (cons "VAR" (concatenate 'string "decoded=" literal)))))
         (malformed
           (babel-vars-test-condition
            (lambda () (org-kit::babel-parse-quoted-string "not-quoted"))))
         (incomplete-escape
           (babel-vars-test-condition
            (lambda ()
              (org-kit::babel-parse-quoted-string
               (format nil "\"bad~C\"" #\\)))))
         (unsupported-escape
           (babel-vars-test-condition
            (lambda ()
              (org-kit::babel-parse-quoted-string
               (format nil "\"bad~Cq\"" #\\)))))
         (unescaped-quote
           (babel-vars-test-condition
            (lambda ()
              (org-kit::babel-parse-quoted-string "\"bad\"quote\"")))))
    (with-soft-assertions
      (expect variables
              :to-equal
              (list (cons "decoded"
                          (format nil "a~Cb~Cc~Cd~Ce~Cf"
                                  (code-char 34) #\\ #\Newline
                                  #\Return #\Tab))))
      (dolist (condition
                (list malformed incomplete-escape unsupported-escape
                      unescaped-quote))
        (expect (typep condition 'org-babel-error) :to-be t)
        (when (typep condition 'org-babel-error)
          (expect (org-babel-error-reason condition)
                  :to-equal :invalid-literal))))))

(it "validates callback result values and circular structures"
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
           (accepted-values
            (loop for result in (list nil 7 #\x 'resolved-symbol '(1 . 2))
                  collect
                  (cdr
                   (assoc
                    "value"
                    (babel-vars-test-resolve-with-variables
                     document consumer '(("VAR" . "value=result-source"))
                     :result-resolver
                     (lambda (block-name variables)
                       (declare (ignore block-name variables))
                       result))
                    :test #'string=))))
           (vector-result (make-array 2 :initial-contents (list 4 "item")))
           (returned-vector
            (cdr
             (assoc
              "value"
              (babel-vars-test-resolve-with-variables
               document consumer '(("VAR" . "value=result-source"))
               :result-resolver
               (lambda (block-name variables)
                 (declare (ignore block-name variables))
                 vector-result))
              :test #'string=)))
           (cyclic-vector
            (babel-vars-test-condition
             (lambda ()
               (babel-vars-test-resolve-with-variables
                document consumer '(("VAR" . "value=result-source"))
                :result-resolver
                (lambda (block-name variables)
                  (declare (ignore block-name variables))
                  (let ((vector (make-array 1)))
                    (setf (aref vector 0) vector)
                    vector))))))
           (cyclic-tail
            (babel-vars-test-condition
             (lambda ()
               (babel-vars-test-resolve-with-variables
                document consumer '(("VAR" . "value=result-source"))
                :result-resolver
                (lambda (block-name variables)
                  (declare (ignore block-name variables))
                  (let ((cycle (cons nil nil)))
                    (setf (cdr cycle) cycle)
                    cycle)))))))
      (with-soft-assertions
       (expect accepted-values :to-equal '(nil 7 #\x resolved-symbol (1 . 2)))
       (expect (eq returned-vector vector-result) :to-be t)
       (dolist (condition (list cyclic-vector cyclic-tail))
         (expect (typep condition 'org-babel-error) :to-be t)
         (when (typep condition 'org-babel-error)
           (expect (org-babel-error-reason condition)
                   :to-equal :cyclic-result))))))

(it "enforces callback result depth and character limits"
    (let* ((source
            (babel-vars-test-lines
             "#+NAME: result-source"
             "#+BEGIN_SRC emacs-lisp"
             "unused"
             "#+END_SRC"
             ""
             "#+NAME: consumer"
             "#+BEGIN_SRC emacs-lisp"
             "unused"
             "#+END_SRC"))
           (depth-document
            (parse-org source :limits (make-limits :max-object-depth 1)))
           (depth-consumer
            (babel-vars-test-named-src-block depth-document "consumer"))
           (depth-limit
            (babel-vars-test-condition
             (lambda ()
               (babel-vars-test-resolve-with-variables
                depth-document depth-consumer
                '(("VAR" . "value=result-source"))
                :result-resolver
                (lambda (block-name variables)
                  (declare (ignore block-name variables))
                  '((nested)))))))
           (character-document
            (parse-org source
                       :limits (make-limits
                                :max-babel-expanded-characters 13)))
           (character-consumer
            (babel-vars-test-named-src-block character-document "consumer"))
           (character-limit
            (babel-vars-test-condition
             (lambda ()
               (babel-vars-test-resolve-with-variables
                character-document character-consumer
                '(("VAR" . "value=result-source"))
                :result-resolver
                (lambda (block-name variables)
                  (declare (ignore block-name variables))
                  "fourteen chars"))))))
      (with-soft-assertions
       (expect (typep depth-limit 'org-resource-limit-error) :to-be t)
       (when (typep depth-limit 'org-resource-limit-error)
         (expect (org-resource-limit-error-limit-name depth-limit)
                 :to-equal :max-object-depth))
       (expect (typep character-limit 'org-resource-limit-error) :to-be t)
       (when (typep character-limit 'org-resource-limit-error)
         (expect (org-resource-limit-error-limit-name character-limit)
                 :to-equal :max-babel-expanded-characters)))))

(it "covers empty and malformed list items and non-static node kinds"
    (let* ((document (parse-org ""))
           (context
            (org-kit::make-babel-resolution-context :document document))
           (empty-item (make-node :item))
           (missing-bounds-item
            (make-node :item :children (list (make-node :paragraph))))
           (multi-part-item
            (make-node :item
                       :children (list (make-node :paragraph)
                                       (make-node :plain-list))))
           (foreign-table
            (make-node :table
                       :properties '(:type :ascii :name "foreign-table")))
           (named-paragraph
            (make-node :paragraph :properties '(:name "named-paragraph")))
           (results-paragraph
            (make-node :paragraph :properties '(:results "named-results")))
           (multi-part-condition
            (babel-vars-test-condition
             (lambda ()
               (org-kit::babel-item-text context multi-part-item))))
           (foreign-table-condition
            (babel-vars-test-condition
             (lambda ()
               (org-kit::babel-static-node-value context foreign-table))))
           (named-node-condition
            (babel-vars-test-condition
             (lambda ()
               (org-kit::babel-static-node-value context named-paragraph))))
           (results-node-condition
            (babel-vars-test-condition
             (lambda ()
               (org-kit::babel-static-node-value context results-paragraph)))))
      (with-soft-assertions
       (expect (org-kit::babel-item-text context empty-item) :to-equal "")
       (expect (org-kit::babel-item-text context missing-bounds-item)
               :to-equal "")
       (progn
         (expect (typep multi-part-condition (quote org-babel-error)) :to-be t)
         (when (typep multi-part-condition (quote org-babel-error))
           (expect (org-babel-error-reason multi-part-condition)
                   :to-equal :unsupported-list-item))
         (expect (typep foreign-table-condition (quote org-babel-error)) :to-be t)
         (when (typep foreign-table-condition (quote org-babel-error))
           (expect (org-babel-error-reason foreign-table-condition)
                   :to-equal :unsupported-reference))
         (expect (typep named-node-condition (quote org-babel-error)) :to-be t)
         (when (typep named-node-condition (quote org-babel-error))
           (expect (org-babel-error-reason named-node-condition)
                   :to-equal :unsupported-reference))
         (expect (typep results-node-condition (quote org-babel-error)) :to-be t)
         (when (typep results-node-condition (quote org-babel-error))
           (expect (org-babel-error-reason results-node-condition)
                   :to-equal :unsupported-reference))))))

(it "detects circular named variables and replaces earlier assignments"
  (let* ((document
           (parse-org
            (babel-vars-test-lines
             "#+NAME: first-source"
             "#+BEGIN_SRC emacs-lisp :var next=second-source"
             "unused"
             "#+END_SRC"
             ""
             "#+NAME: second-source"
             "#+BEGIN_SRC emacs-lisp :var next=first-source"
             "unused"
             "#+END_SRC"
             ""
             "#+NAME: consumer"
             "#+BEGIN_SRC emacs-lisp"
             "unused"
             "#+END_SRC")))
         (consumer (babel-vars-test-named-src-block document "consumer"))
         (variables
           (babel-vars-test-resolve-with-variables
            document consumer
            '(("VAR" . "same=1")
              ("VAR" . "other=2")
              ("VAR" . "same=3"))))
         (condition
           (babel-vars-test-condition
            (lambda ()
              (babel-vars-test-resolve-with-variables
               document consumer '(("VAR" . "value=first-source"))
               :result-resolver
               (lambda (block-name parameters)
                 (declare (ignore block-name parameters))
                 nil))))))
    (with-soft-assertions
      (expect variables :to-equal '(("other" . 2) ("same" . 3)))
      (expect (typep condition 'org-babel-error) :to-be t)
      (when (typep condition 'org-babel-error)
        (expect (org-babel-error-reason condition)
                :to-equal :circular-reference)))))

(it "handles call references and rejects malformed resolver contracts"
    (let* ((document
            (parse-org
             (babel-vars-test-lines
              "#+NAME: empty-target"
              "#+BEGIN_SRC emacs-lisp"
              "unused"
              "#+END_SRC"
              ""
              "#+NAME: no-argument-call"
              "#+CALL: empty-target()"
              ""
              "#+NAME: missing-call"
              "#+CALL: absent-target()"
              ""
              "#+NAME: paragraph-reference"
              "named paragraph"
              ""
              "#+NAME: consumer"
              "#+BEGIN_SRC emacs-lisp"
              "unused"
              "#+END_SRC")))
           (consumer (babel-vars-test-named-src-block document "consumer"))
           (resolver-calls '())
           (call-result
            (babel-vars-test-resolve-with-variables
             document consumer '(("VAR" . "value=no-argument-call"))
             :result-resolver
             (lambda (block-name variables)
               (push (list block-name variables) resolver-calls)
               '(resolved))))
           (missing-target
            (babel-vars-test-condition
             (lambda ()
               (babel-vars-test-resolve-with-variables
                document consumer '(("VAR" . "value=missing-call"))
                :result-resolver
                (lambda (block-name variables)
                  (declare (ignore block-name variables))
                  nil)))))
           (invalid-resolver
            (babel-vars-test-condition
             (lambda ()
               (org-kit:resolve-babel-variables
                document consumer :result-resolver 17))))
           (unnamed-context
            (org-kit::make-babel-resolution-context
             :document document
             :result-resolver (lambda (block-name variables)
                                (declare (ignore block-name variables))
                                nil)))
           (unnamed-block (make-node :src-block))
           (unnamed-result
            (babel-vars-test-condition
             (lambda ()
               (org-kit::babel-resolve-source-block-result
                unnamed-context unnamed-block '(("LANGUAGE" . "emacs-lisp")))))))
      (with-soft-assertions
       (expect call-result :to-equal '(("value" . (resolved))))
       (expect (nreverse resolver-calls)
               :to-equal '(("empty-target" nil)))
       (expect (typep missing-target 'org-babel-error) :to-be t)
       (when (typep missing-target 'org-babel-error)
         (expect (org-babel-error-reason missing-target)
                 :to-equal :unresolved-call-target))
       (expect (typep invalid-resolver 'type-error) :to-be t)
       (expect (typep unnamed-result 'org-babel-error) :to-be t)
       (when (typep unnamed-result 'org-babel-error)
         (expect (org-babel-error-reason unnamed-result)
                 :to-equal :unnamed-result-block)))))
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
                (babel-vars-test-resolve-with-variables document consumer '(("VAR" . "value=result-source"))
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
             (babel-vars-test-resolve-with-variables document consumer
              '(("VAR" . "integer=42")
                ("VAR" . "negative=-7")
                ("VAR" . "ratio=3/4")
                ("VAR" . "decimal=1.25e1"))))
           (fullwidth-condition
             (babel-vars-test-condition
              (lambda ()
                (babel-vars-test-resolve-with-variables document consumer '(("VAR" . "value=４２"))))))
           (arabic-condition
             (babel-vars-test-condition
              (lambda ()
                (babel-vars-test-resolve-with-variables document consumer '(("VAR" . "value=٤٢"))))))
           (unsupported-condition
             (babel-vars-test-condition
              (lambda ()
                (babel-vars-test-resolve-with-variables document consumer '(("VAR" . "value=result-source"))
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
                  :to-equal :unsupported-result))))))

(describe "Babel variable helper edge cases"
  (it "rejects empty numeric tokens and replaces repeated variables"
    (let* ((document (parse-org ""))
           (context
             (org-kit::make-babel-resolution-context :document document))
           (variables
             (org-kit::babel-resolve-parameters-variables
              context '(("VAR" . "same=1,same=3")))))
      (with-soft-assertions
        (expect (org-kit::babel-numeric-token-p "") :to-be nil)
        (expect variables :to-equal '(("same" . 3))))))

  (it "reports unnamed unsupported static nodes and unsupported named references"
    (let* ((document
             (parse-org
              (babel-vars-test-lines
               "#+NAME: paragraph-reference"
               "named paragraph"
               ""
               "#+NAME: consumer"
               "#+BEGIN_SRC emacs-lisp"
               "unused"
               "#+END_SRC")))
           (consumer (babel-vars-test-named-src-block document "consumer"))
           (context
             (org-kit::make-babel-resolution-context
              :document document
              :names (org-kit::babel-build-name-index document)))
           (unnamed-condition
             (babel-vars-test-condition
              (lambda ()
                (org-kit::babel-static-node-value
                 context (make-node :paragraph)))))
           (reference-condition
             (babel-vars-test-condition
              (lambda ()
                (babel-vars-test-resolve-with-variables
                 document consumer '(("VAR" . "value=paragraph-reference")))))))
      (with-soft-assertions
        (expect (typep unnamed-condition 'org-babel-error) :to-be t)
        (when (typep unnamed-condition 'org-babel-error)
          (expect (org-babel-error-reason unnamed-condition)
                  :to-equal :unsupported-reference))
        (expect (typep reference-condition 'org-babel-error) :to-be t)
        (when (typep reference-condition 'org-babel-error)
          (expect (org-babel-error-reason reference-condition)
                  :to-equal :unsupported-reference)))))

  (it "checks numeric reader results and missing call targets"
    (let* ((document (parse-org ""))
           (context
             (org-kit::make-babel-resolution-context :document document))
           (condition
             (babel-vars-test-condition
              (lambda ()
                (org-kit::babel-resolve-call-result
                 context (make-node :babel-call))))))
      (with-soft-assertions
        (expect (org-kit::babel-numeric-token-p "3/") :to-be nil)
        (expect (multiple-value-bind (value valid)
                    (org-kit::babel-parse-number "plain")
                  (list value valid))
                :to-equal '(nil nil))
        (expect (multiple-value-bind (value valid)
                    (org-kit::babel-parse-number-result 'not-a-number 1 "x")
                  (list value valid))
                :to-equal '(nil nil))
        (expect (multiple-value-bind (value valid)
                    (org-kit::babel-parse-number-result 1 0 "1")
                  (list value valid))
                :to-equal '(nil nil))
        (expect (typep condition 'org-babel-error) :to-be t)
        (when (typep condition 'org-babel-error)
          (expect (org-babel-error-reason condition)
                  :to-equal :unresolved-call-target))))))

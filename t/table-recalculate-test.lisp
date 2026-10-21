(in-package #:cl-org-kit/test)

(defun table-recalculate-test-fixture (relative-path)
  (uiop:read-file-string
   (asdf:system-relative-pathname
    "cl-org-kit" (concatenate 'string "t/fixtures/table/" relative-path))))

(defun table-recalculate-test-find-node (root type)
  (or (when (eq (node-type root) type) root)
      (some (lambda (child) (table-recalculate-test-find-node child type))
            (node-children root))))

(defun table-recalculate-test-find-nodes (root type)
  (append (when (eq (node-type root) type) (list root))
          (mapcan (lambda (child) (table-recalculate-test-find-nodes child type))
                  (node-children root))))

(defun table-recalculate-test-expect-table-error (condition reason)
  (with-soft-assertions
    (expect (typep condition 'org-table-error) :to-be t)
    (when (typep condition 'org-table-error)
      (expect (org-table-error-reason condition) :to-be reason)
      (expect (stringp (org-table-error-target condition)) :to-be t))))

(defun table-recalculate-test-condition (thunk)
  (handler-case
      (progn (funcall thunk) '(:no-error))
    (org-edit-error (condition)
      (list :edit (org-edit-error-reason condition)))
    (org-table-unsupported-syntax (condition)
      (list :unsupported (org-table-unsupported-syntax-feature condition)))
    (org-table-error (condition)
      (list :table (org-table-error-reason condition)))
    (org-resource-limit-error (condition)
      (list :limit
            (org-resource-limit-error-limit-name condition)
            (org-resource-limit-error-limit condition)
            (org-resource-limit-error-actual condition)))))

(defun table-recalculate-test-caught-condition (thunk)
  (handler-case
      (progn (funcall thunk) nil)
    (org-table-unsupported-syntax (condition) condition)
    (org-table-error (condition) condition)))

(defun table-recalculate-test-assignment (formula)
  (first (org-kit::table-calc-program-assignments
          (org-kit::table-calc-parse-formula formula))))

(defun table-recalculate-test-run-fixture (source-path expected-path)
  (let* ((source (table-recalculate-test-fixture source-path))
         (expected (table-recalculate-test-fixture expected-path))
         (document (parse-org source))
         (table (table-recalculate-test-find-node (document-root document) :table))
         (result (table-recalculate document table))
         (updated (edit-result-updated-document result)))
    (with-soft-assertions
      (expect (edit-result-p result) :to-be t)
      (expect (document-text updated) :to-equal expected)
      (expect (document-text document) :to-equal source)
      (expect (node-property
               (table-recalculate-test-find-node (document-root updated) :table)
               :tblfm)
              :to-equal (node-property table :tblfm)))
    result))

(describe "table formula recalculation"
  (it "matches the pinned Org oracle including field-over-column priority"
    (let ((result
            (table-recalculate-test-run-fixture
             "corpus/recalculate/field-column-overlap.org"
             "expected/recalculate/field-column-overlap.org")))
      (expect (length (edit-result-spans result)) :to-be 1)))
  (it "executes restricted org-sbe calls through the registered Babel executor"
  (labels ((source-for (formula)
             (format nil
                     "#+name: sample~%#+begin_src mock :cache no~%body~%#+end_src~%| value | result |~%|-------+--------|~%| alpha | old |~%#+TBLFM: @2$2=~A~%"
                     formula))
           (recalculate (formula executor)
             (let* ((source (source-for formula))
                    (document (parse-org source))
                    (table (table-recalculate-test-find-node
                            (document-root document) :table))
                    (callbacks (make-callback-registry))
                    (received-parameters nil)
                    (received-variables nil))
               (register-babel-executor
                callbacks "mock"
                (lambda (body parameters variables)
                  (setf received-parameters parameters
                        received-variables variables)
                  (funcall executor body parameters variables)))
               (let* ((result (table-recalculate document table
                                                 :callbacks callbacks))
                      (updated (edit-result-updated-document result)))
                 (values (document-text updated)
                         received-parameters received-variables)))))
    (multiple-value-bind (text parameters variables)
        (recalculate
         "'(org-sbe sample \":cache yes\" (one 7) (many 1 2) (literal $ \"a b\"))"
         (lambda (body parameters variables)
           (declare (ignore body parameters variables))
           "  trimmed result  "))
      (with-soft-assertions
        (expect (not (null (search "| alpha | trimmed result |" text))) :to-be t)
        (expect (cdr (assoc "CACHE" parameters :test #'string=))
                :to-equal "yes")
        (expect (cdr (assoc "one" variables :test #'string=)) :to-equal 7)
        (progn
  (expect (cdr (assoc "many" variables :test #'string=))
          :to-equal '(1 2))
  (let ((var-parameters
          (remove-if-not (lambda (parameter)
                           (string= (car parameter) "VAR"))
                         parameters)))
    (expect (cdr (second var-parameters))
            :to-equal "many='(1 2)")))
        (expect (cdr (assoc "literal" variables :test #'string=))
                :to-equal "a b")))
    (multiple-value-bind (text parameters variables)
        (recalculate
         "'(org-sbe \"sample\")"
         (lambda (body parameters variables)
           (declare (ignore body parameters variables))
           42))
      (declare (ignore parameters variables))
      (expect (not (null (search "| alpha | 42 |" text))) :to-be t))
    (multiple-value-bind (text parameters variables)
        (recalculate
         "'(org-sbe \"\")"
         (lambda (body parameters variables)
           (declare (ignore body parameters variables))
           (error "Empty org-sbe block must not execute")))
      (declare (ignore parameters variables))
      (expect (not (null (search "| alpha |  |" text))) :to-be t))
    (let* ((source (source-for "'(org-sbe sample)"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (callbacks (make-callback-registry))
           (calls 0)
           (condition nil))
      (register-callback callbacks :evaluator :emacs-lisp
                         (lambda (expression)
                           (declare (ignore expression))
                           (incf calls)))
      (register-babel-executor callbacks "other"
                               (lambda (&rest arguments)
                                 (declare (ignore arguments))
                                 (incf calls)))
      (setf condition
            (handler-case
                (progn (table-recalculate document table :callbacks callbacks)
                       nil)
              (org-elisp-evaluator-error (condition) condition)))
      (with-soft-assertions
        (expect (typep condition 'org-elisp-evaluator-error) :to-be t)
        (when condition
          (expect (org-callback-error-kind condition)
                  :to-equal "BABEL-EXECUTOR")
          (expect (org-callback-error-name condition) :to-equal "mock")
          (expect (org-callback-error-reason condition) :to-be :unregistered))
        (expect calls :to-be 0)))
    (let* ((source (source-for "'(org-sbe sample (one . 7))"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (callbacks (make-callback-registry))
           (calls 0)
           (condition nil))
      (register-callback callbacks :evaluator :emacs-lisp
                         (lambda (expression)
                           (declare (ignore expression))
                           (incf calls)))
      (setf condition
            (handler-case
                (progn (table-recalculate document table :callbacks callbacks)
                       nil)
              (org-table-unsupported-syntax (condition) condition)))
      (with-soft-assertions
        (expect (typep condition 'org-table-unsupported-syntax) :to-be t)
        (when condition
          (expect (org-table-unsupported-syntax-feature condition)
                  :to-be :org-sbe))
        (expect calls :to-be 0)))))

  (it "passes the Org-expanded quoted Lisp form to the evaluator callback"
    (let* ((source
             (format nil "| value | result |~%|-------+--------|~%| alpha | old |~%#+TBLFM: @2$2='(identity $1)~%"))
           (expected
             (format nil "| value | result |~%|-------+--------|~%| alpha | done |~%#+TBLFM: @2$2='(identity $1)~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (callbacks (make-callback-registry))
           (received nil))
      (register-callback callbacks :evaluator :emacs-lisp
                         (lambda (expression)
                           (setf received expression)
                           "done"))
      (let ((result (table-recalculate document table :callbacks callbacks)))
        (with-soft-assertions
          (expect (edit-result-p result) :to-be t)
          (expect received :to-equal "'(identity \"alpha\")")
          (expect (document-text (edit-result-updated-document result))
                  :to-equal expected)
          (expect (document-text document) :to-equal source)))))

  (it "expands quoted Lisp references across ranges and Calc modes"
    (labels ((expand (formula cell-value-function &optional reference-value-function)
               (let* ((assignment (table-recalculate-test-assignment formula))
                      (expression (org-kit::table-calc-assignment-expression assignment))
                      (source (org-kit::table-calc-expression-value expression)))
                 (org-kit::table-recalculate-expand-elisp-expression
                  source expression assignment 2 4 3 4 (make-hash-table) nil nil
                  cell-value-function reference-value-function (make-limits)
                  (org-kit::make-table-calc-work-budget)))))
      (let* ((cell-values
               (lambda (row column)
                 (declare (ignore row))
                 (nth (1- column) '("one" "" "three"))))
             (plain
               (expand "@2$4='(list $1..$3)" cell-values))
             (keep-empty
               (expand "@2$4='(list $1..$3);E" cell-values))
             (numeric
               (expand "@2$4='(list $1..$3);N"
                       (lambda (row column)
                         (declare (ignore row))
                         (nth (1- column) '("1" "" "3")))))
             (keep-empty-numeric
               (expand "@2$4='(list $1..$3);EN"
                       (lambda (row column)
                         (declare (ignore row))
                         (nth (1- column) '("1" "" "3")))))
             (rectangle
               (expand "@2$4='(list @2$1..@3$2)"
                       (lambda (row column)
                         (cdr (assoc (list row column)
                                     '(((2 1) . "a")
                                       ((2 2) . "b")
                                       ((3 1) . "c")
                                       ((3 2) . "d"))
                                     :test #'equal)))))
             (property
               (expand "@2$4='(identity $PROP_rate)"
                       (lambda (row column)
                         (declare (ignore row column))
                         "cell")
                       (lambda (reference)
                         (declare (ignore reference))
                         (values "configured" t)))))
        (with-soft-assertions
          (expect plain :to-equal "'(list \"one\" \"three\")")
          (expect keep-empty :to-equal "'(list \"one\" \"\" \"three\")")
          (expect numeric :to-equal "'(list 1 3)")
          (expect keep-empty-numeric :to-equal "'(list 1 0 3)")
          (expect rectangle :to-equal "'(list \"a\" \"b\" \"c\" \"d\")")
          (expect property :to-equal "'(identity \"configured\")"))))
    (let* ((limits (make-limits))
           (decimal (org-kit::parse-safe-decimal "1.25" :limits limits)))
      (with-soft-assertions
        (expect (org-kit::table-recalculate-elisp-string
                 (format nil "quote: \" slash: \\~C~C~C~C~C ordinary"
                         #\Newline #\Return #\Tab #\Page #\Backspace))
                :to-equal
                (concatenate 'string
                             "\"" "quote: " "\\\"" " slash: " "\\\\"
                             "\\n" "\\r" "\\t" "\\f" "\\b"
                             " ordinary" "\""))
        (expect (org-kit::table-recalculate-elisp-number-string "3.5" limits)
                :to-equal "3.5")
        (expect (org-kit::table-recalculate-elisp-number-string "not-a-number" limits)
                :to-equal "0")
        (expect (org-kit::table-recalculate-elisp-reference-text decimal nil limits)
                :to-equal "\"1.25\"")
        (expect (org-kit::table-recalculate-elisp-reference-text 2.5 nil limits)
                :to-equal "\"2.5\"")
        (expect (org-kit::table-recalculate-elisp-reference-text :unsupported nil limits)
                :to-equal "\"\""))))

  (it "passes Lisp references through unchanged for the Org literal mode"
    (let* ((source
             (format nil "| value | result |~%|-------+--------|~%| alpha | old |~%#+TBLFM: @2$2='(identity $1);L~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (callbacks (make-callback-registry))
           (received nil))
      (register-callback callbacks :evaluator :emacs-lisp
                         (lambda (expression)
                           (setf received expression)
                           "literal"))
      (table-recalculate document table :callbacks callbacks)
      (with-soft-assertions
        (expect received :to-equal "'(identity $1)"))))
  (it "uses cell lookup when no property callback is registered"
      (let* ((formula "@2$4='(identity $1)")
             (assignment (table-recalculate-test-assignment formula))
             (expression (org-kit::table-calc-assignment-expression assignment))
             (result
              (org-kit::table-recalculate-expand-elisp-expression
               (org-kit::table-calc-expression-value expression)
               expression assignment 2 4 3 4 (make-hash-table) nil nil
               (lambda (row column)
                 (declare (ignore row column))
                 "one")
               nil (make-limits) (org-kit::make-table-calc-work-budget))))
        (expect result :to-equal "'(identity \"one\")")))

  (it "reports a named unregistered evaluator condition"
    (let* ((source
             (format nil "| value | result |~%|-------+--------|~%| alpha | old |~%#+TBLFM: @2$2='(identity $1)~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (condition
             (handler-case
                 (progn (table-recalculate document table) nil)
               (org-elisp-evaluator-error (condition) condition))))
      (with-soft-assertions
        (expect (typep condition 'org-elisp-evaluator-error) :to-be t)
        (when condition
          (expect (org-callback-error-kind condition) :to-equal "EVALUATOR")
          (expect (org-callback-error-name condition) :to-equal "emacs-lisp")
          (expect (org-callback-error-reason condition) :to-be :unregistered)))))

  (it "rejects reader dispatch before invoking the evaluator callback"
    (let* ((source
             (format nil "| value | result |~%|-------+--------|~%| alpha | old |~%#+TBLFM: @2$2='(progn #.(error \"blocked\"))~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (callbacks (make-callback-registry))
           (calls 0)
           (condition nil))
      (register-callback callbacks :evaluator :emacs-lisp
                         (lambda (expression)
                           (declare (ignore expression))
                           (incf calls)))
      (setf condition
            (handler-case
                (progn (table-recalculate document table :callbacks callbacks) nil)
              (org-table-unsupported-syntax (condition) condition)))
      (with-soft-assertions
        (expect (org-table-unsupported-syntax-feature condition) :to-be :reader-eval)
        (expect calls :to-be 0))))

  (it "accepts real callback values and rejects other callback results"
    (let* ((source
             (format nil "| value | result |~%|-------+--------|~%| alpha | old |~%#+TBLFM: @2$2='(identity $1)~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (number-callbacks (make-callback-registry))
           (invalid-callbacks (make-callback-registry)))
      (register-callback number-callbacks :evaluator :emacs-lisp
                         (lambda (expression)
                           (declare (ignore expression))
                           42))
      (register-callback invalid-callbacks :evaluator :emacs-lisp
                         (lambda (expression)
                           (declare (ignore expression))
                           :invalid))
      (with-soft-assertions
        (expect
         (not (null
               (search "| alpha | 42 |"
                       (document-text
                        (edit-result-updated-document
                         (table-recalculate document table
                                            :callbacks number-callbacks))))))
         :to-be t)
        (expect (table-recalculate-test-condition
                 (lambda ()
                   (table-recalculate document table :callbacks invalid-callbacks)))
                :to-equal '(:table :invalid-function-result)))))

  (it "applies output and formula-work limits to quoted Lisp evaluation"
    (let* ((source
             (format nil "| value | result |~%|-------+--------|~%| alpha | old |~%#+TBLFM: @2$2='(identity $1)~%"))
           (limited-output-document
             (parse-org source :limits (make-limits :max-output-length 3)))
           (table (table-recalculate-test-find-node
                   (document-root limited-output-document) :table))
           (callbacks (make-callback-registry))
           (assignment
             (table-recalculate-test-assignment "@2$2='(identity $1)"))
           (expression (org-kit::table-calc-assignment-expression assignment))
           (work-condition
             (handler-case
                 (progn
                   (org-kit::table-recalculate-expand-elisp-expression
                    (org-kit::table-calc-expression-value expression)
                    expression assignment 2 2 2 2 (make-hash-table) nil nil
                    (lambda (row column)
                      (declare (ignore row column))
                      "alpha")
                    nil (make-limits :max-formula-work 1)
                    (org-kit::make-table-calc-work-budget))
                   nil)
               (org-resource-limit-error (condition) condition))))
      (register-callback callbacks :evaluator :emacs-lisp
                         (lambda (expression)
                           (declare (ignore expression))
                           "long"))
      (with-soft-assertions
        (expect (table-recalculate-test-condition
                 (lambda ()
                   (table-recalculate limited-output-document table
                                      :callbacks callbacks)))
                :to-equal '(:limit :max-output-length 3 4))
        (expect (typep work-condition 'org-resource-limit-error) :to-be t)
        (when (typep work-condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name work-condition)
                  :to-be :max-formula-work)
          (expect (org-resource-limit-error-limit work-condition) :to-be 1)))))

  (it "does not let a field formula hide a column formula error"
    (let* ((source
             (format nil "| value | result |~%|-------+--------|~%| 0 | old |~%| 2 | old |~%#+TBLFM: $2=$1/$1::@2$2=42~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (condition
             (table-recalculate-test-condition
              (lambda () (table-recalculate document table)))))
      (with-soft-assertions
        (expect condition :to-equal '(:table :division-by-zero))
        (expect (document-text document) :to-equal source))))

  (it "reparses formula cell markup and reads field formulas from original cells"
    (let* ((source
             (format nil "| a | b | c |~%|---+---+---|~%| 1 | 2 | 0 |~%#+TBLFM: @2$2=9::$3=$2+1~%"))
           (expected
             (format nil "| a | b | c |~%|---+---+---|~%| 1 | 9 | 3 |~%#+TBLFM: @2$2=9::$3=$2+1~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (result (table-recalculate document table)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result)) :to-equal expected)
        (expect (document-text document) :to-equal source)))
    (let* ((source (format nil "| Value |~%|-------|~%| old |~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (work-budget (org-kit::make-table-calc-work-budget)))
      (multiple-value-bind (standard-rows)
          (org-kit::table-recalculate-standard-rows
           table (document-limits document) work-budget)
        (let* ((cell (org-kit::table-recalculate-cell standard-rows 2 1))
               (values (make-hash-table :test #'equal))
               (new-value "*new*")
               (patch (org-kit::edit-make-patch (node-contents-begin cell) (node-contents-end cell) new-value)))
          (setf (gethash (cons 2 1) values) new-value)
          (let* ((expected-canonical
                   (org-kit::table-recalculate-expected-canonical
                    document values standard-rows))
                 (result
                   (org-kit::edit-apply-patches-result document (list patch) :target table :expected expected-canonical))
                 (updated-document (edit-result-updated-document result))
                 (updated-table
                   (table-recalculate-test-find-node
                    (document-root updated-document) :table))
                 (updated-rows
                   (nth-value
                    0
                    (org-kit::table-recalculate-standard-rows
                     updated-table (document-limits updated-document)
                     (org-kit::make-table-calc-work-budget))))
                 (updated-cell (org-kit::table-recalculate-cell updated-rows 2 1)))
            (with-soft-assertions
              (expect (document-text updated-document)
                      :to-equal (format nil "| Value |~%|-------|~%| *new* |~%"))
              (expect (some (lambda (child) (eq (node-type child) :bold))
                            (node-children updated-cell))
                      :to-be t)))))))

  (it "uses earlier column results in source order"
    (let ((result
            (table-recalculate-test-run-fixture
             "policy/recalculate/source-order-columns.org"
             "policy/expected/recalculate/source-order-columns.org")))
      (expect (length (edit-result-spans result)) :to-be 2)))

  (it "evaluates field formulas in source order"
    (let ((result
            (table-recalculate-test-run-fixture
             "policy/recalculate/original-field-snapshot.org"
             "policy/expected/recalculate/original-field-snapshot.org")))
      (expect (length (edit-result-spans result)) :to-be 2)))

  (it "accepts an acyclic field range dependency and deduplicates repeated range edges"
    (let* ((source
             (format nil "| a | b | c |~%|---+---+---|~%| 1 | 2 | 0 |~%#+TBLFM: @2$1=10::@2$3=vsum(@2$1..@2$2)+vsum(@2$1..@2$2)~%"))
           (expected
             (format nil "| a | b | c |~%|---+---+---|~%| 10 | 2 | 24 |~%#+TBLFM: @2$1=10::@2$3=vsum(@2$1..@2$2)+vsum(@2$1..@2$2)~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (result (table-recalculate document table)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result)) :to-equal expected)
        (expect (document-text document) :to-equal source))))

  (it "orders field targets before reading them through a vertical range"
    (let* ((source
             (format nil "| a | b | c | d |~%|---+---+---+---|~%| x | 1 | 0 | 0 |~%| x | 2 | 0 | 0 |~%| x | 3 | 0 | 0 |~%| x | 4 | 0 | 0 |~%#+TBLFM: @2$2=10::@4$2=20::@2$4=30::@5$3=vsum(@2$2..@4$2)~%"))
           (expected
             (format nil "| a | b | c | d |~%|---+---+---+---|~%| x | 10 | 0 | 30 |~%| x | 2 | 0 | 0 |~%| x | 20 | 0 | 0 |~%| x | 4 | 32 | 0 |~%#+TBLFM: @2$2=10::@4$2=20::@2$4=30::@5$3=vsum(@2$2..@4$2)~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (result (table-recalculate document table)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result)) :to-equal expected)
        (expect (document-text document) :to-equal source))))

  (it "deduplicates repeated single-cell field dependencies"
    (let* ((source
             (format nil "| a | b | c |~%|---+---+---|~%| 1 | 2 | 0 |~%#+TBLFM: @2$1=10::@2$3=@2$1+@2$1~%"))
           (expected
             (format nil "| a | b | c |~%|---+---+---|~%| 10 | 2 | 20 |~%#+TBLFM: @2$1=10::@2$3=@2$1+@2$1~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (result (table-recalculate document table)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result)) :to-equal expected)
        (expect (document-text document) :to-equal source))))

  (it "uses only the first of multiple TBLFM directives"
    (let* ((source
             (format nil "| input | result |~%|-------+--------|~%| 1 | old |~%| 2 | old |~%#+TBLFM: $2=$1+10~%#+TBLFM: $2=$1+20~%"))
           (expected
             (format nil "| input | result |~%|-------+--------|~%| 1 | 11 |~%| 2 | 12 |~%#+TBLFM: $2=$1+10~%#+TBLFM: $2=$1+20~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (result (table-recalculate document table)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result)) :to-equal expected)
        (expect (document-text document) :to-equal source))))

  (it "resolves absolute, relative, ordinal, offset, and range hline references"
    (let* ((source
             (format nil
                     "| key | value |~%|-----+-------|~%| a | 10 |~%| b | 20 |~%|-----+-------|~%| c | 30 |~%| d | 40 |~%|-----+-------|~%| e | 50 |~%| f | 60 |~%|-----+-------|~%| g | 70 |~%| h | 80 |~%| i | 90 |~%#+TBLFM: @3$2=@I$2+@-I$2+@+I$2+@II$2+@III+2$2::@>$2=vsum(@I$2..@II$2)~%"))
           (expected
             (format nil
                     "| key | value |~%|-----+-------|~%| a | 10 |~%| b | 140 |~%|-----+-------|~%| c | 30 |~%| d | 40 |~%|-----+-------|~%| e | 50 |~%| f | 60 |~%|-----+-------|~%| g | 70 |~%| h | 80 |~%| i | 150 |~%#+TBLFM: @3$2=@I$2+@-I$2+@+I$2+@II$2+@III+2$2::@>$2=vsum(@I$2..@II$2)~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (result (table-recalculate document table)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result)) :to-equal expected)
        (expect (document-text document) :to-equal source))))

  (it "rejects unavailable relative hline ordinals"
    (let* ((document (parse-org (format nil "| a | b |~%|---+---|~%| 1 | 0 |~%")))
           (table (table-recalculate-test-find-node (document-root document) :table)))
      (dolist (case '((1 1 :above)
                      (3 2 :above)
                      (3 0 :above)
                      (3 :invalid :above)))
        (expect (table-recalculate-test-condition
                 (lambda ()
                   (org-kit::table-recalculate-relative-hline-row
                    #(2) (first case) (second case) (third case) table)))
                :to-equal '(:table :invalid-reference)))))

  (it "rejects non-positive hline offsets"
    (let* ((document (parse-org (format nil "| a |~%|---|~%| 1 |~%")))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (base-reference
             (org-kit::table-calc-assignment-target
              (table-recalculate-test-assignment "$1=1"))))
      (dolist (offset '(0 :invalid))
        (let ((reference (org-kit::copy-table-calc-reference base-reference)))
          (setf (org-kit::table-calc-reference-row-kind reference) :hline-following
                (org-kit::table-calc-reference-row-value reference) 1
                (org-kit::table-calc-reference-row-offset reference) offset)
          (expect (table-recalculate-test-condition
                   (lambda ()
                     (org-kit::table-recalculate-reference-position
                      reference 1 1 2 1 (make-hash-table :test #'equal) #(1) table)))
                  :to-equal '(:table :invalid-reference))))))

  (it "collects direct expression regions without a remote resolver"
    (let* ((document (parse-org (format nil "| a | b |~%|---+---|~%| 1 | 0 |~%")))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (expression
             (org-kit::table-calc-assignment-expression
              (table-recalculate-test-assignment "@2$2=@1$1")))
           (regions
             (org-kit::table-recalculate-expression-regions
              expression 2 2 2 2 (make-hash-table :test #'equal) table
              (make-limits) (org-kit::make-table-calc-work-budget))))
      (expect regions :to-equal '((1 1 1 1)))))

  (it "rejects an unavailable hline anchor"
    (let* ((source
             (format nil "| a | b |~%|---+---|~%| 1 | 0 |~%#+TBLFM: @2$2=@II$1~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (condition
             (table-recalculate-test-caught-condition
              (lambda () (table-recalculate document table)))))
      (table-recalculate-test-expect-table-error condition :invalid-reference)
      (expect (document-text document) :to-equal source)))

  (it "rejects an out-of-bounds assignment range without changing the document"
    (let* ((source
             (format nil "| a | b |~%|---+---|~%| 1 | 0 |~%| 2 | 0 |~%#+TBLFM: @2$1..@4$2=9~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (condition
             (table-recalculate-test-caught-condition
              (lambda () (table-recalculate document table)))))
      (table-recalculate-test-expect-table-error condition :invalid-reference)
      (expect (document-text document) :to-equal source)))

  (it "applies a rectangular range target to every selected field"
    (let* ((source
             (format nil "| a | b | c |~%|---+---+---|~%| 0 | 0 | 0 |~%| 0 | 0 | 0 |~%#+TBLFM: @2$2..@3$3=7~%"))
           (expected
             (format nil "| a | b | c |~%|---+---+---|~%| 0 | 7 | 7 |~%| 0 | 7 | 7 |~%#+TBLFM: @2$2..@3$3=7~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (result (table-recalculate document table)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result)) :to-equal expected)
        (expect (document-text document) :to-equal source))))

  (it "rejects a malformed range target without changing the document"
    (let* ((source
             (format nil "| a | b |~%|---+---|~%| 1 | 0 |~%#+TBLFM: @2$1..=9~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (condition
             (table-recalculate-test-caught-condition
              (lambda () (table-recalculate document table)))))
      (with-soft-assertions
        (expect (typep condition 'org-table-unsupported-syntax) :to-be t)
        (when (typep condition 'org-table-unsupported-syntax)
          (expect (org-table-unsupported-syntax-feature condition)
                  :to-be :invalid-range))
        (expect (document-text document) :to-equal source))))

  (it "applies a field range target ahead of its overlapping column formula"
    (let* ((source
             (format nil "| amount | total |~%|--------+-------|~%| 1 | 0 |~%| 2 | 0 |~%#+TBLFM: @2$2..@3$2=9::$2=$1+1~%"))
           (expected
             (format nil "| amount | total |~%|--------+-------|~%| 1 | 9 |~%| 2 | 9 |~%#+TBLFM: @2$2..@3$2=9::$2=$1+1~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (result (table-recalculate document table)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result)) :to-equal expected)
        (expect (document-text document) :to-equal source))))

  (it "applies a row target to every cell in the selected data row"
    (let* ((source
             (format nil "| a | b | c |~%|---+---+---|~%| 1 | 2 | 3 |~%| 4 | 5 | 6 |~%#+TBLFM: @3=9~%"))
           (expected
             (format nil "| a | b | c |~%|---+---+---|~%| 1 | 2 | 3 |~%| 9 | 9 | 9 |~%#+TBLFM: @3=9~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (result (table-recalculate document table)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result)) :to-equal expected)
        (expect (document-text document) :to-equal source))))

  (it "resolves constants and inherited properties in formula expressions"
    (let* ((source
             (format nil "#+CONSTANTS: pi=3.5~%* Settings~%:PROPERTIES:~%:rate: 2~%:END:~%** Table~%| input | result |~%|-------+--------|~%| 0 | 0 |~%#+TBLFM: @2$2=$pi*$PROP_rate~%"))
           (expected
             (format nil "#+CONSTANTS: pi=3.5~%* Settings~%:PROPERTIES:~%:rate: 2~%:END:~%** Table~%| input | result |~%|-------+--------|~%| 0 | 7 |~%#+TBLFM: @2$2=$pi*$PROP_rate~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (result (table-recalculate document table)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result)) :to-equal expected)
        (expect (document-text document) :to-equal source))))

  (it "strips Org's trailing decimal point from constant values"
    (expect (org-kit::table-recalculate-constant-pairs "ratio=3.")
            :to-equal '(("ratio" . "3"))))

  (it "rejects unknown constants and properties without changing the document"
    (dolist (case '(("$missing" :unknown-constant)
                    ("$PROP_missing" :unknown-property)))
      (let* ((source
               (format nil "#+CONSTANTS: present=7~%* Settings~%| input | result |~%|-------+--------|~%| 0 | 0 |~%#+TBLFM: @2$2=~A~%"
                       (first case)))
             (document (parse-org source))
             (table (table-recalculate-test-find-node (document-root document) :table))
             (condition
               (table-recalculate-test-caught-condition
                (lambda () (table-recalculate document table)))))
        (table-recalculate-test-expect-table-error condition (second case))
        (expect (document-text document) :to-equal source))))

  (it "reads saved values from case-insensitive named remote tables"
    (let* ((source
             (format nil "#+NAME: Inputs~%| value |~%|-------|~%| 40 |~%#+TBLFM: @2$1=99~%#+NAME: result~%| value |~%|-------|~%| 0 |~%#+TBLFM: @2$1=remote(inputs,@2$1)+2~%"))
           (expected
             (format nil "#+NAME: Inputs~%| value |~%|-------|~%| 40 |~%#+TBLFM: @2$1=99~%#+NAME: result~%| value |~%|-------|~%| 42 |~%#+TBLFM: @2$1=remote(inputs,@2$1)+2~%"))
           (document (parse-org source))
           (tables (table-recalculate-test-find-nodes (document-root document) :table))
           (result (table-recalculate document (second tables))))
      (with-soft-assertions
        (expect (length tables) :to-be 2)
        (expect (document-text (edit-result-updated-document result)) :to-equal expected)
        (expect (document-text document) :to-equal source))))

  (it "sums a range from a named remote table"
    (let* ((source
             (format nil "#+NAME: inputs~%| value |~%|-------|~%| 1 |~%| 2 |~%#+NAME: result~%| total |~%|-------|~%| 0 |~%#+TBLFM: @2$1=vsum(remote(inputs,@2$1..@3$1))~%"))
           (expected
             (format nil "#+NAME: inputs~%| value |~%|-------|~%| 1 |~%| 2 |~%#+NAME: result~%| total |~%|-------|~%| 3 |~%#+TBLFM: @2$1=vsum(remote(inputs,@2$1..@3$1))~%"))
           (document (parse-org source))
           (tables (table-recalculate-test-find-nodes (document-root document) :table))
           (result (table-recalculate document (second tables))))
      (with-soft-assertions
        (expect (length tables) :to-be 2)
        (expect (document-text (edit-result-updated-document result)) :to-equal expected)
        (expect (document-text document) :to-equal source))))

  (it "rejects a non-string remote table name"
    (let* ((document (parse-org (format nil "| a |~%|---|~%| 1 |~%")))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (context (org-kit::make-table-recalculate-context))
           (remote-function
             (org-kit::table-recalculate-remote-reference-function context table))
           (expression
             (org-kit::table-calc-assignment-expression
              (table-recalculate-test-assignment "$1=1"))))
      (expect (table-recalculate-test-condition
               (lambda () (funcall remote-function 42 expression 1 1)))
              :to-equal '(:table :unknown-remote-table))))

  (it "rejects remote expressions that do not select a field or range"
    (let* ((document (parse-org (format nil "| a |~%|---|~%| 1 |~%")))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (tables (make-hash-table :test #'equal))
           (remote-cache (make-hash-table :test #'eq))
           (state
             (org-kit::make-table-recalculate-state
              :standard-rows #(#(t)) :data-rows nil :header-row nil
              :hline-rows #() :values (make-hash-table :test #'equal)
              :column-count 1 :header-columns (make-hash-table :test #'equal)))
           (context
             (org-kit::make-table-recalculate-context
              :tables tables :remote-cache remote-cache))
           (expression
             (org-kit::table-calc-assignment-expression
              (table-recalculate-test-assignment "$1=1")))
           (remote-function nil))
      (setf (gethash "remote" tables) table
            (gethash table remote-cache) state
            remote-function
            (org-kit::table-recalculate-remote-reference-function context table))
      (expect (table-recalculate-test-condition
               (lambda () (funcall remote-function "remote" expression 1 1)))
              :to-equal '(:table :invalid-reference))))
  (it "caches remote table metadata and charges it to the formula work budget"
    (let* ((document
             (parse-org (format nil
                                "#+NAME: source~%| one | two |~%|---+---|~%| 1 | 2 |~%")))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (budget (org-kit::make-table-calc-work-budget))
           (limits (make-limits :max-formula-work 100))
           (context
             (org-kit::make-table-recalculate-context
              :limits limits :work-budget budget))
           (state (org-kit::table-recalculate-remote-state context table))
           (used (org-kit::table-calc-work-budget-used budget))
           (cached-state (org-kit::table-recalculate-remote-state context table))
           (limited-context
             (org-kit::make-table-recalculate-context
              :limits (make-limits :max-formula-work (1- used))
              :work-budget (org-kit::make-table-calc-work-budget))))
      (with-soft-assertions
        (expect (plusp used) :to-be t)
        (expect (eq cached-state state) :to-be t)
        (expect (org-kit::table-calc-work-budget-used budget) :to-equal used)
        (expect (table-recalculate-test-condition
                 (lambda ()
                   (org-kit::table-recalculate-remote-state limited-context table)))
                :to-equal (list :limit :max-formula-work (1- used) used)))))

  (it "returns cached state and rejects an active table cycle"
    (let* ((table :cached-table)
           (state (org-kit::make-table-recalculate-state))
           (cache (make-hash-table :test (function eq)))
           (context (org-kit::make-table-recalculate-context :cache cache)))
      (setf (gethash table cache) state)
      (expect (eq (org-kit::table-recalculate-compute-state context table) state)
              :to-be t))
    (let* ((document (parse-org (format nil "| value |~%|---|~%| 1 |~%")))
           (table (table-recalculate-test-find-node (document-root document) :table)))
      (table-recalculate-test-expect-table-error
       (table-recalculate-test-caught-condition
        (lambda ()
          (org-kit::table-recalculate-compute-state
           (org-kit::make-table-recalculate-context
            :active (list table)
            :cache (make-hash-table :test (function eq)))
           table)))
       :cyclic-remote-reference)))

  (it "evaluates duration modes from HH:MM and HH:MM:SS cell references"
    (let* ((source
             (format nil "| start | duration | T | U | t |~%|-------+----------+---+---+---|~%| 1:02 | 2:03 | old | old | old |~%| 1:30:00 | 2:00:00 | old | old | old |~%#+TBLFM: @2$3=@2$1+@2$2;T::@2$4=@2$1+@2$2;U::@3$5=@3$1+@3$2;t~%"))
           (expected
             (format nil "| start | duration | T | U | t |~%|-------+----------+---+---+---|~%| 1:02 | 2:03 | 03:05:00 | 03:05 | old |~%| 1:30:00 | 2:00:00 | old | old | 3.50 |~%#+TBLFM: @2$3=@2$1+@2$2;T::@2$4=@2$1+@2$2;U::@3$5=@3$1+@3$2;t~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (result (table-recalculate document table)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result)) :to-equal expected)
        (expect (document-text document) :to-equal source))))

  (it "rejects an unresolved remote table without changing the document"
    (let* ((source
             (format nil "| value |~%|-------|~%| 0 |~%#+TBLFM: @2$1=remote(missing,@2$1)~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (condition
             (table-recalculate-test-caught-condition
              (lambda () (table-recalculate document table)))))
      (table-recalculate-test-expect-table-error condition :unknown-remote-table)
      (expect (document-text document) :to-equal source)))

  (it "reads saved values without recursively recalculating remote table formulas"
    (let* ((source
             (format nil "#+NAME: left~%| value |~%|-------|~%| 1 |~%#+TBLFM: @2$1=remote(right,@2$1)~%#+NAME: right~%| value |~%|-------|~%| 2 |~%#+TBLFM: @2$1=remote(left,@2$1)~%"))
           (expected
             (format nil "#+NAME: left~%| value |~%|-------|~%| 2 |~%#+TBLFM: @2$1=remote(right,@2$1)~%#+NAME: right~%| value |~%|-------|~%| 2 |~%#+TBLFM: @2$1=remote(left,@2$1)~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (result (table-recalculate document table)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result)) :to-equal expected)
        (expect (document-text document) :to-equal source))))

  (it "calls registered formula functions after built-in resolution"
    (let* ((source
             (format nil "| value |~%|-------|~%| 0 |~%#+TBLFM: @2$1=double(21)~%"))
           (expected
             (format nil "| value |~%|-------|~%| 42 |~%#+TBLFM: @2$1=double(21)~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (arguments nil)
           (result
             (table-recalculate
              document table
              :functions (list (cons "double"
                                     (lambda (value)
                                       (setf arguments (list value))
                                       (* 2 value)))))))
      (with-soft-assertions
        (expect arguments :to-equal '(21))
        (expect (document-text (edit-result-updated-document result)) :to-equal expected)
        (expect (document-text document) :to-equal source)))
    (let* ((source
             (format nil "| value |~%|-------|~%| 0 |~%#+TBLFM: @2$1=exp(1)~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (result
             (table-recalculate
              document table
              :functions (list (cons "exp" (lambda (value)
                                              (declare (ignore value))
                                              99))))))
      (expect (document-text (edit-result-updated-document result))
              :to-equal
              (format nil "| value |~%|-------|~%| 2.7182818 |~%#+TBLFM: @2$1=exp(1)~%"))))

  (it "passes evaluated strings to registered formula functions"
    (let* ((source
             (format nil "| value |~%|-------|~%| old |~%#+TBLFM: @2$1=echo(string(42))~%"))
           (expected
             (format nil "| value |~%|-------|~%| 42 |~%#+TBLFM: @2$1=echo(string(42))~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (argument nil)
           (result
             (table-recalculate
              document table
              :functions (list (cons "echo"
                                     (lambda (value)
                                       (setf argument value)
                                       value))))))
      (with-soft-assertions
        (expect argument :to-equal "42")
        (expect (document-text (edit-result-updated-document result)) :to-equal expected)
        (expect (document-text document) :to-equal source))))

  (it "rejects an unregistered formula function without changing the document"
    (let* ((source
             (format nil "| value |~%|-------|~%| 0 |~%#+TBLFM: @2$1=unknown(21)~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (condition
             (table-recalculate-test-caught-condition
              (lambda () (table-recalculate document table)))))
      (with-soft-assertions
        (expect (typep condition 'org-table-unsupported-syntax) :to-be t)
        (when (typep condition 'org-table-unsupported-syntax)
          (expect (org-table-unsupported-syntax-feature condition)
                  :to-be :unknown-function))
        (expect (document-text document) :to-equal source))))

  (it "rejects an invalid registered formula result without changing the document"
    (let* ((source
             (format nil "| value |~%|-------|~%| 0 |~%#+TBLFM: @2$1=badresult(21)~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (condition
             (table-recalculate-test-caught-condition
              (lambda ()
                (table-recalculate
                 document table
                 :functions (list (cons "badresult" (lambda (value)
                                                        (declare (ignore value))
                                                        :not-a-number))))))))
      (table-recalculate-test-expect-table-error condition :invalid-function-result)
      (expect (document-text document) :to-equal source)))

  (it "rejects duplicate field targets"
    (let* ((source (table-recalculate-test-fixture
                    "policy/recalculate/duplicate-fields.org"))
           (expected (table-recalculate-test-fixture
                      "policy/expected/recalculate/duplicate-fields.org"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table)))
      (with-soft-assertions
        (expect (table-recalculate-test-condition
                 (lambda () (table-recalculate document table)))
                :to-equal '(:table :duplicate-target))
        (expect (document-text document) :to-equal expected))))

  (it "rejects cycles between column targets"
    (let* ((source (table-recalculate-test-fixture
                    "policy/recalculate/cyclic-columns.org"))
           (expected (table-recalculate-test-fixture
                      "policy/expected/recalculate/cyclic-columns.org"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table)))
      (with-soft-assertions
        (expect (table-recalculate-test-condition
                 (lambda () (table-recalculate document table)))
                :to-equal '(:table :cyclic-formula))
        (expect (document-text document) :to-equal expected))))

  (it "rejects cycles between field targets"
    (let* ((source (table-recalculate-test-fixture
                    "policy/recalculate/cyclic-fields.org"))
           (expected (table-recalculate-test-fixture
                      "policy/expected/recalculate/cyclic-fields.org"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table)))
      (with-soft-assertions
        (expect (table-recalculate-test-condition
                 (lambda () (table-recalculate document table)))
                :to-equal '(:table :cyclic-formula))
        (expect (document-text document) :to-equal expected))))

  (it "rejects duplicate column targets"
    (let* ((document (parse-org
                      (format nil "| a | b |~%|---+---|~%| 1 | 2 |~%#+TBLFM: $1=1::$1=2~%")))
           (table (table-recalculate-test-find-node (document-root document) :table)))
      (expect (table-recalculate-test-condition
               (lambda () (table-recalculate document table)))
              :to-equal '(:table :duplicate-target))))

  (it "resolves named column targets and accepts a trailing separator"
    (let* ((source (format nil "| amount | total |~%|--------+-------|~%| 2 | 0 |~%#+TBLFM: $total=$amount+1::~%"))
           (expected (format nil "| amount | total |~%|--------+-------|~%| 2 | 3 |~%#+TBLFM: $total=$amount+1::~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (result (table-recalculate document table)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result)) :to-equal expected)
        (expect (document-text document) :to-equal source))))

  (it "resolves named field targets against header columns"
    (let* ((source (format nil "| amount | total |~%|--------+-------|~%| 2 | 0 |~%#+TBLFM: @2$total=$amount+2::~%"))
           (expected (format nil "| amount | total |~%|--------+-------|~%| 2 | 4 |~%#+TBLFM: @2$total=$amount+2::~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (result (table-recalculate document table)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result)) :to-equal expected)
        (expect (document-text document) :to-equal source))))

  (it "resolves named first-row field targets"
    (let* ((source (format nil "| amount | total |~%|--------+-------|~%| 2 | 0 |~%#+TBLFM: @<$total=9~%"))
           (expected (format nil "| amount | 9 |~%|--------+-------|~%| 2 | 0 |~%#+TBLFM: @<$total=9~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (result (table-recalculate document table)))
      (expect (document-text (edit-result-updated-document result)) :to-equal expected)))

  (it "resolves last-row field targets"
    (let* ((source (format nil "| amount | total |~%|--------+-------|~%| 2 | 0 |~%| 4 | 0 |~%#+TBLFM: @>$2=9~%"))
           (expected (format nil "| amount | total |~%|--------+-------|~%| 2 | 0 |~%| 4 | 9 |~%#+TBLFM: @>$2=9~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (result (table-recalculate document table)))
      (expect (document-text (edit-result-updated-document result)) :to-equal expected)))

  (it "resolves first-row expression references"
    (let* ((source (format nil "| 1 | 0 |~%| 2 | 0 |~%#+TBLFM: @2$2=@<$1~%"))
           (expected (format nil "| 1 | 0 |~%| 2 | 1 |~%#+TBLFM: @2$2=@<$1~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (result (table-recalculate document table)))
      (expect (document-text (edit-result-updated-document result)) :to-equal expected)))

  (it "rejects invalid reference rows"
    (let* ((document (parse-org (format nil "| a |~%|---|~%| 1 |~%")))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (reference
             (org-kit::copy-table-calc-reference
              (org-kit::table-calc-assignment-target
               (table-recalculate-test-assignment "$1=1")))))
      (setf (org-kit::table-calc-reference-row-kind reference) :invalid)
      (expect (table-recalculate-test-condition
               (lambda ()
                 (org-kit::table-recalculate-reference-position
                  reference 1 1 2 1 (make-hash-table :test #'equal) #() table)))
              :to-equal '(:table :invalid-reference))))

  (it "rejects hline-anchored reference rows"
    (let* ((document (parse-org (format nil "| a |~%|---|~%| 1 |~%")))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (reference
             (org-kit::copy-table-calc-reference
              (org-kit::table-calc-assignment-target
               (table-recalculate-test-assignment "$1=1")))))
      (setf (org-kit::table-calc-reference-row-kind reference) :hline-anchor)
      (expect (table-recalculate-test-condition
               (lambda ()
                 (org-kit::table-recalculate-reference-position
                  reference 1 1 2 1 (make-hash-table :test #'equal) #() table)))
              :to-equal '(:table :invalid-reference))))

  (it "rejects unknown reference row kinds"
    (let* ((document (parse-org (format nil "| a |~%|---|~%| 1 |~%")))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (reference
             (org-kit::copy-table-calc-reference
              (org-kit::table-calc-assignment-target
               (table-recalculate-test-assignment "$1=1")))))
      (setf (org-kit::table-calc-reference-row-kind reference) :future)
      (expect (table-recalculate-test-condition
               (lambda ()
                 (org-kit::table-recalculate-reference-position
                  reference 1 1 2 1 (make-hash-table :test #'equal) #() table)))
              :to-equal '(:table :invalid-reference))))

  (it "rejects unsupported reference column kinds"
    (let* ((document (parse-org (format nil "| a |~%|---|~%| 1 |~%")))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (reference
             (org-kit::copy-table-calc-reference
              (org-kit::table-calc-assignment-target
               (table-recalculate-test-assignment "$1=1")))))
      (setf (org-kit::table-calc-reference-column-kind reference) :future)
      (expect (table-recalculate-test-condition
               (lambda ()
                 (org-kit::table-recalculate-reference-position
                  reference 1 1 2 1 (make-hash-table :test #'equal) #() table)))
              :to-equal '(:table :invalid-reference))))

  (it "rejects invalid assignment target columns"
    (let* ((document (parse-org (format nil "| a | b |~%|---+---|~%| 1 | 2 |~%")))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (assignment (table-recalculate-test-assignment "@2$1=1")))
      (setf (org-kit::table-calc-reference-column-kind
             (org-kit::table-calc-assignment-target assignment))
            :relative)
      (expect (table-recalculate-test-condition
               (lambda ()
                 (org-kit::table-recalculate-normalize-assignment-target
                  assignment 2 2 (make-hash-table :test #'equal) #() table)))
              :to-equal '(:table :invalid-reference))))

  (it "rejects unsupported column kinds while normalizing column assignments"
    (let* ((document (parse-org (format nil "| a | b |~%|---+---|~%| 1 | 2 |~%")))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (assignment (table-recalculate-test-assignment "$1=1")))
      (setf (org-kit::table-calc-reference-column-kind
             (org-kit::table-calc-assignment-target assignment))
            :relative)
      (expect (table-recalculate-test-condition
               (lambda ()
                 (org-kit::table-recalculate-normalize-assignment-target
                  assignment 2 2 (make-hash-table :test #'equal) #() table)))
              :to-equal '(:table :invalid-reference))))

  (it "rejects an out-of-bounds position returned by reference resolution"
    (let* ((document (parse-org (format nil "| a | b |~%|---+---|~%| 1 | 2 |~%")))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (reference
             (org-kit::table-calc-assignment-target
              (table-recalculate-test-assignment "$1=1")))
           (symbol 'org-kit::table-recalculate-reference-position)
           (original (symbol-function symbol)))
      (unwind-protect
           (progn
             (setf (symbol-function symbol)
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (values 0 1)))
             (expect (table-recalculate-test-condition
                      (lambda ()
                        (org-kit::table-recalculate-target-position
                         reference 2 2 (make-hash-table :test #'equal) #() table)))
                     :to-equal '(:table :invalid-reference)))
        (setf (symbol-function symbol) original))))

  (it "rejects invalid assignment target rows"
    (let* ((document (parse-org (format nil "| a | b |~%|---+---|~%| 1 | 2 |~%")))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (assignment (table-recalculate-test-assignment "@2$1=1")))
      (setf (org-kit::table-calc-reference-row-kind
             (org-kit::table-calc-assignment-target assignment))
            :relative)
      (expect (table-recalculate-test-condition
               (lambda ()
                 (org-kit::table-recalculate-normalize-assignment-target
                  assignment 2 2 (make-hash-table :test #'equal) #() table)))
              :to-equal '(:table :invalid-reference))))

  (it "rejects a non-integer field target row during normalization"
    (let* ((document (parse-org (format nil "| a | b |~%|---+---|~%| 1 | 2 |~%")))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (assignment (table-recalculate-test-assignment "@2$1=1"))
           (reference (org-kit::table-calc-assignment-target assignment)))
      (setf (org-kit::table-calc-reference-row-value reference) :invalid)
      (expect (table-recalculate-test-condition
               (lambda ()
                 (org-kit::table-recalculate-normalize-assignment-target
                  assignment 2 2 (make-hash-table :test #'equal) #() table)))
              :to-equal '(:table :invalid-reference))))

  (it "rejects non-current row targets for column formulas"
    (let* ((document (parse-org (format nil "| a | b |~%|---+---|~%| 1 | 2 |~%")))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (assignment (table-recalculate-test-assignment "$1=1"))
           (reference (org-kit::table-calc-assignment-target assignment)))
      (setf (org-kit::table-calc-reference-row-kind reference) :absolute
            (org-kit::table-calc-reference-row-value reference) 1)
      (expect (table-recalculate-test-condition
               (lambda ()
                 (org-kit::table-recalculate-normalize-assignment-target
                  assignment 2 2 (make-hash-table :test #'equal) #() table)))
              :to-equal '(:table :invalid-reference))))

  (it "rejects out-of-table column assignment plans"
    (let* ((document (parse-org (format nil "| a | b |~%|---+---|~%| 1 | 2 |~%")))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (assignment (table-recalculate-test-assignment "$3=1")))
      (expect (table-recalculate-test-condition
               (lambda ()
                 (org-kit::table-recalculate-plan-column-assignments
                  (list assignment) nil #() 2 table
                  (make-limits) (org-kit::make-table-calc-work-budget))))
              :to-equal '(:table :invalid-reference))))

  (it "rejects non-integer column targets during column planning"
    (let* ((document (parse-org (format nil "| a | b |~%|---+---|~%| 1 | 2 |~%")))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (assignment (table-recalculate-test-assignment "$1=1")))
      (setf (org-kit::table-calc-reference-column-value
             (org-kit::table-calc-assignment-target assignment))
            :invalid)
      (expect (table-recalculate-test-condition
               (lambda ()
                 (org-kit::table-recalculate-plan-column-assignments
                  (list assignment) nil #() 2 table
                  (make-limits) (org-kit::make-table-calc-work-budget))))
              :to-equal '(:table :invalid-reference))))

  (it "rejects non-integer and out-of-range coordinates during field planning"
    (let* ((document (parse-org (format nil "| a | b |~%|---+---|~%| 1 | 2 |~%")))
           (table (table-recalculate-test-find-node (document-root document) :table)))
      (dolist (case '((:row :invalid) (:row 0) (:column :invalid)))
        (let* ((assignment (table-recalculate-test-assignment "@2$1=1"))
               (reference (org-kit::table-calc-assignment-target assignment)))
          (ecase (first case)
            (:row
             (setf (org-kit::table-calc-reference-row-value reference)
                   (second case)))
            (:column
             (setf (org-kit::table-calc-reference-row-value reference) 1
                   (org-kit::table-calc-reference-column-value reference)
                   (second case))))
          (expect (table-recalculate-test-condition
                   (lambda ()
                     (org-kit::table-recalculate-plan-field-assignments
                      (list assignment) #(#(t)) 1 2 table
                      (make-limits) (org-kit::make-table-calc-work-budget))))
                  :to-equal '(:table :invalid-reference))))))
  (it "rejects a range end beyond planner row bounds"
    (let* ((document
             (parse-org
              (format nil
                      "| a | b |~%|---+---|~%| 1 | 2 |~%| 3 | 4 |~%")))
           (table
             (table-recalculate-test-find-node
              (document-root document) :table))
           (assignment
             (table-recalculate-test-assignment "@1$1..@3$2=1")))
      (expect
       (table-recalculate-test-condition
        (lambda ()
          (org-kit::table-recalculate-plan-field-assignments
           (list assignment) #(#(t t) #(t t)) 2 2 table
           (make-limits) (org-kit::make-table-calc-work-budget))))
       :to-equal (quote (:table :invalid-reference)))))

  (it "rejects field ranges that extend past a ragged row"
    (let* ((document (parse-org (format nil "| a | b |~%|---+---|~%| 1 | 2 |~%")))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (assignment (table-recalculate-test-assignment "@1$1..@1$2=1")))
      (expect (table-recalculate-test-condition
               (lambda ()
                 (org-kit::table-recalculate-plan-field-assignments
                  (list assignment) #(#(t)) 1 2 table
                  (make-limits) (org-kit::make-table-calc-work-budget))))
              :to-equal '(:table :invalid-reference))))

  (it "detects duplicate targets expressed by a name and a number"
    (let* ((document (parse-org
                      (format nil "| amount | total |~%|--------+-------|~%| 2 | 0 |~%#+TBLFM: $total=1::$2=2~%")))
           (table (table-recalculate-test-find-node (document-root document) :table)))
      (expect (table-recalculate-test-condition
               (lambda () (table-recalculate document table)))
              :to-equal '(:table :duplicate-target))))

  (it "rejects invalid and unknown assignment target kinds"
    (let* ((document (parse-org
                      (format nil "| a | b |~%|---+---|~%| 1 | 2 |~%#+TBLFM: @INVALID$2=1~%")))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (assignment
             (first (org-kit::table-calc-program-assignments
                     (org-kit::table-calc-parse-formula "$1=1"))))
           (unknown (org-kit::copy-table-calc-assignment assignment)))
      (setf (org-kit::table-calc-assignment-kind unknown) :future)
      (with-soft-assertions
        (expect (table-recalculate-test-condition
                 (lambda () (table-recalculate document table)))
                :to-equal '(:table :invalid-reference))
        (expect (table-recalculate-test-condition
                 (lambda ()
                   (org-kit::table-recalculate-assignments-by-kind
                    (list unknown) :column)))
                :to-equal '(:table :invalid-reference)))))

  (it "reads a self-referencing column formula from its original cell"
    (let* ((source (format nil "| a | b |~%|---+---|~%| 0 | 1 |~%| 0 | 2 |~%#+TBLFM: $2=$2+1~%"))
           (expected (format nil "| a | b |~%|---+---|~%| 0 | 2 |~%| 0 | 3 |~%#+TBLFM: $2=$2+1~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (result (table-recalculate document table)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result)) :to-equal expected)
        (expect (document-text document) :to-equal source))))

  (it "reads a self-referencing field formula from the original cell"
    (let* ((source (format nil "| a | b |~%|---+---|~%| 0 | 0 |~%#+TBLFM: @2$2=@2$2+1~%"))
           (expected (format nil "| a | b |~%|---+---|~%| 0 | 1 |~%#+TBLFM: @2$2=@2$2+1~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (result (table-recalculate document table)))
      (expect (document-text (edit-result-updated-document result)) :to-equal expected)))

  (it "validates every expression before evaluating any formula"
    (let* ((source (format nil "| a | b |~%|---+---|~%| 1 | 2 |~%#+TBLFM: @2$1=1/0::@2$2=@3$1~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table)))
      (with-soft-assertions
        (expect (table-recalculate-test-condition
                 (lambda () (table-recalculate document table)))
                :to-equal '(:table :invalid-reference))
        (expect (document-text document) :to-equal source))))

  (it "shares the formula work budget across expanded cells"
    (let* ((source (format nil "| a | b |~%|---+---|~%| 1 | 2 |~%| 3 | 4 |~%#+TBLFM: $2=$1+1~%"))
           (document (parse-org source :limits (make-limits :max-formula-work 37)))
           (table (table-recalculate-test-find-node (document-root document) :table)))
      (with-soft-assertions
        (expect (table-recalculate-test-condition
                 (lambda () (table-recalculate document table)))
                :to-equal '(:limit :max-formula-work 37 38))
        (expect (document-text document) :to-equal source))))

  (it "charges public range dependency inspection to the formula work budget"
    (let* ((row-count 5)
           (source
             (with-output-to-string (stream)
               (dotimes (index row-count)
                 (format stream "| ~D | 0 |~%" (1+ index)))
               (format stream "#+TBLFM: @1$2=vsum(@1$1..@5$1)~%")))
           (document (parse-org source :limits (make-limits :max-formula-work 33)))
           (table (table-recalculate-test-find-node (document-root document) :table)))
      (with-soft-assertions
        (expect (table-recalculate-test-condition
                 (lambda () (table-recalculate document table)))
                :to-equal '(:limit :max-formula-work 33 34))
        (expect (document-text document) :to-equal source))))

  (it "charges range dependency inspection to the formula work budget"
    (let* ((document (parse-org (format nil "| a | b |~%|---+---|~%| 1 | 2 |~%")))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (assignment
             (first (org-kit::table-calc-program-assignments
                     (org-kit::table-calc-parse-formula
                      "@2$2=vsum(@1$1..@101$1)"))))
           (entry (org-kit::make-table-recalculate-entry
                   :assignment assignment :row 2 :column 2 :owner 0)))
      (expect (table-recalculate-test-condition
               (lambda ()
                 (org-kit::table-recalculate-check-cycles
                  (list entry) 1 (make-hash-table :test #'equal)
                  101 2 (make-hash-table :test #'equal) table
                  (make-limits :max-formula-work 2)
                  (org-kit::make-table-calc-work-budget))))
              :to-equal '(:limit :max-formula-work 2 3))))

  (it "charges cell indexing to the formula work budget"
    (let* ((source (format nil "| a | b | c |~%|---+---+---|~%| 1 | 2 | 3 |~%#+TBLFM: $3=$1~%"))
           (document (parse-org source :limits (make-limits :max-formula-work 8)))
           (table (table-recalculate-test-find-node (document-root document) :table)))
      (with-soft-assertions
        (expect (table-recalculate-test-condition
                 (lambda () (table-recalculate document table)))
                :to-equal '(:limit :max-formula-work 8 9))
        (expect (document-text document) :to-equal source))))

  (it "stores standard table rows in a vector"
    (let* ((document (parse-org
                      (format nil "| a | b |~%|---+---|~%| 1 | 2 |~%")))
           (table (table-recalculate-test-find-node (document-root document) :table)))
      (multiple-value-bind (rows data-rows header-row)
          (org-kit::table-recalculate-standard-rows
           table (document-limits document) (org-kit::make-table-calc-work-budget))
        (declare (ignore data-rows))
        (with-soft-assertions
          (expect (vectorp rows) :to-be t)
          (when (vectorp rows)
            (expect (vectorp (aref rows 0)) :to-be t)
            (when (plusp (length (aref rows 0)))
              (expect (aref (aref rows 0) 0) :to-be
                      (first (node-children header-row)))))))))

  (it "charges repeated rule rows even when the table has no standard rows"
    (let* ((document (parse-org (format nil "|---|~%|---|~%")))
           (table (table-recalculate-test-find-node (document-root document) :table)))
      (expect (table-recalculate-test-condition
               (lambda ()
                 (org-kit::table-recalculate-standard-rows
                  table (make-limits :max-formula-work 1)
                  (org-kit::make-table-calc-work-budget))))
              :to-equal '(:limit :max-formula-work 1 2))))

  (it "uses named, relative, last-row, and current-row references"
    (let* ((source
             (format nil "| amount | total |~%|--------+-------|~%| 2      | 0     |~%| 4      | 0     |~%#+TBLFM: @2$2=$amount+1::@3$2=@>$1+1~%"))
           (expected
             (format nil "| amount | total |~%|--------+-------|~%| 2      | 3     |~%| 4      | 5     |~%#+TBLFM: @2$2=$amount+1::@3$2=@>$1+1~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (result (table-recalculate document table)))
      (expect (document-text (edit-result-updated-document result)) :to-equal expected)))

  (it "rejects oversized relative references and formula target columns"
    (let* ((oversized (write-to-string (1+ most-positive-fixnum)))
           (formulas (list (format nil "@2$1=@+~A$-0" oversized)
                           (format nil "@2$1=@+0$-~A" oversized)
                           (format nil "$~A=1" oversized)
                           (format nil "@2$~A=1" oversized))))
      (dolist (formula formulas)
        (let* ((source (format nil "| a | b |~%|---+---|~%| 1 | 2 |~%#+TBLFM: ~A~%" formula))
               (document (parse-org source))
               (table (table-recalculate-test-find-node (document-root document) :table)))
          (with-soft-assertions
            (expect (table-recalculate-test-condition
                     (lambda () (table-recalculate document table)))
                    :to-equal '(:table :invalid-reference))
            (expect (document-text document) :to-equal source))))))

  (it "accepts an acyclic forward relative-row column formula before a ragged final row"
    (let* ((source (format nil "| a | b |~%|---+---|~%| 1 | 10 |~%| 2 | 20 |~%| 3 | 30 |~%| 4 |~%#+TBLFM: $2=@+1$-0;N~%"))
           (expected (format nil "| a | b |~%|---+---|~%| 1 | 20 |~%| 2 | 30 |~%| 3 | 0 |~%| 4 |~%#+TBLFM: $2=@+1$-0;N~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (result (table-recalculate document table)))
      (expect (document-text (edit-result-updated-document result)) :to-equal expected)))

  (it "evaluates range, relative row and column, and last-row current-column references"
    (let* ((source
             (format nil "| a | b | c | d |~%|---+---+---+---|~%| 1 | 2 | 0 | 0 |~%| 3 | 4 | 7 | 0 |~%| 5 | 6 | 0 | 4 |~%#+TBLFM: @2$3=vsum(@2$1..@4$1)::@2$4=@+1$-1::@2$2=@>+1~%"))
           (expected
             (format nil "| a | b | c | d |~%|---+---+---+---|~%| 1 | 7 | 9 | 7 |~%| 3 | 4 | 7 | 0 |~%| 5 | 6 | 0 | 4 |~%#+TBLFM: @2$3=vsum(@2$1..@4$1)::@2$4=@+1$-1::@2$2=@>+1~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (result (table-recalculate document table)))
      (expect (document-text (edit-result-updated-document result)) :to-equal expected)))

  (it "treats a ragged referenced field as empty and skips a missing target cell"
    (let* ((source
             (format nil "| a | b | c |~%|---+---+---|~%| 1 | 2 |~%| 4 | 5 | 6 |~%#+TBLFM: @2$2=@2$3;N::$3=$1+1~%"))
           (expected
             (format nil "| a | b | c |~%|---+---+---|~%| 1 | 0 |~%| 4 | 5 | 5 |~%#+TBLFM: @2$2=@2$3;N::$3=$1+1~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (result (table-recalculate document table)))
      (expect (document-text (edit-result-updated-document result)) :to-equal expected)))

  (it "rejects an in-bounds field target when its ragged cell is absent"
    (let* ((source (format nil "| a | b |~%|---+---|~%| 1 |~%#+TBLFM: @2$2=9~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table)))
      (with-soft-assertions
        (expect (table-recalculate-test-condition
                 (lambda () (table-recalculate document table)))
                :to-equal '(:table :invalid-reference))
        (expect (document-text document) :to-equal source))))

  (it "applies column formulas only after the first rule across multiple rules"
    (let* ((source
             (format nil "| 1 |~%| 2 |~%|---|~%| 3 |~%|---|~%| 4 |~%#+TBLFM: $1=9~%"))
           (expected
             (format nil "| 1 |~%| 2 |~%|---|~%| 9 |~%|---|~%| 9 |~%#+TBLFM: $1=9~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (result (table-recalculate document table)))
      (expect (document-text (edit-result-updated-document result)) :to-equal expected)))

  (it "applies column formulas to every row when the table has no hline"
    (let* ((source (format nil "| 2 | 3 |~%| 4 | 9 |~%#+TBLFM: $2=$1+1~%"))
           (expected (format nil "| 2 | 3 |~%| 4 | 5 |~%#+TBLFM: $2=$1+1~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (result (table-recalculate document table)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result)) :to-equal expected)
        (expect (mapcar (lambda (span)
                          (subseq source (edit-span-start span)
                                  (edit-span-end span)))
                        (edit-result-spans result))
                :to-equal '("9")))))

  (it "does not apply column formulas when a separator has no following data rows"
    (let* ((source (format nil "| a | b |~%|---+---|~%#+TBLFM: $2=9~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (result (table-recalculate document table)))
      (with-soft-assertions
        (expect (edit-result-updated-document result) :to-be document)
        (expect (edit-result-spans result) :to-equal nil))))

  (it "returns an empty edit result when there is no formula or no changed value"
    (let* ((plain (parse-org (format nil "| a |~%|---|~%| 1 |~%")))
           (plain-table
             (table-recalculate-test-find-node (document-root plain) :table))
           (plain-result (table-recalculate plain plain-table))
           (same-source (format nil "| a |~%|---|~%| 2 |~%#+TBLFM: @2$1=2~%"))
           (same-document (parse-org same-source))
           (same-table
             (table-recalculate-test-find-node (document-root same-document) :table))
           (same-result (table-recalculate same-document same-table)))
      (with-soft-assertions
        (expect (edit-result-updated-document plain-result) :to-be plain)
        (expect (edit-result-spans plain-result) :to-equal nil)
        (expect (edit-result-updated-document same-result) :to-be same-document)
        (expect (edit-result-spans same-result) :to-equal nil))))

  (it "rejects unknown and ambiguous column names and out-of-range targets"
    (dolist (case '(("| left | right |~%|------+-------|~%| 1 | 2 |~%#+TBLFM: @2$2=$missing~%"
                     :unknown-column-name)
                    ("| x | x |~%|---+---|~%| 1 | 2 |~%#+TBLFM: @2$2=$x~%"
                     :ambiguous-column-name)
                    ("| a | b |~%|---+---|~%| 1 | 2 |~%#+TBLFM: $3=1~%"
                     :invalid-reference)
                    ("| a | b |~%|---+---|~%| 1 | 2 |~%#+TBLFM: @3$1=9~%"
                     :invalid-reference)
                    ("| a | b |~%|---+---|~%| 1 | 2 |~%#+TBLFM: @2$3=9~%"
                     :invalid-reference)))
      (let* ((document (parse-org (format nil (first case))))
             (table (table-recalculate-test-find-node (document-root document) :table)))
        (expect (table-recalculate-test-condition
                 (lambda () (table-recalculate document table)))
                :to-equal (list :table (second case))))))

  (it "does not treat the first row of a headerless table as a named header"
    (let* ((source (format nil "| amount | 2 |~%| total | 0 |~%#+TBLFM: @2$2=$amount~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table)))
      (with-soft-assertions
        (expect (table-recalculate-test-condition
                 (lambda () (table-recalculate document table)))
                :to-equal '(:table :unknown-column-name))
        (expect (document-text document) :to-equal source))))

  (it "rejects table.el tables and foreign targets"
    (let* ((table-el-document
             (parse-org (format nil "+---+---+~%| a | b |~%+---+---+~%| 1 | 2 |~%+---+---+~%")))
           (table-el (table-recalculate-test-find-node
                      (document-root table-el-document) :table))
           (document (parse-org (format nil "| a |~%|---|~%| 1 |~%")))
           (foreign-document (parse-org (format nil "| b |~%|---|~%| 2 |~%")))
           (foreign-table
             (table-recalculate-test-find-node (document-root foreign-document) :table)))
      (with-soft-assertions
        (expect (table-recalculate-test-condition
                 (lambda () (table-recalculate table-el-document table-el)))
                :to-equal (list :table :unsupported-table-type))
        (expect (table-recalculate-test-condition
                 (lambda ()
                   (org-kit::table-recalculate-remote-state
                    (org-kit::make-table-recalculate-context) table-el)))
                :to-equal (list :table :unsupported-table-type))
        (expect (table-recalculate-test-condition
                 (lambda () (table-recalculate document foreign-table)))
                :to-equal (list :edit :foreign-node))
        (expect (document-text document) :to-equal
                (format nil "| a |~%|---|~%| 1 |~%")))))

  (it "uses the table as the target for reference resolution errors"
    (let* ((document (parse-org
                      (format nil "| a | b |~%|---+---|~%| 1 | 0 |~%#+TBLFM: @2$2=@99$1~%")))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (assignment
             (first (org-kit::table-calc-program-assignments
                     (org-kit::table-calc-parse-formula "@2$2=@99$1"))))
           (entry (org-kit::make-table-recalculate-entry
                   :assignment assignment :row 2 :column 2 :owner 0))
           (condition
             (multiple-value-bind (rows data-rows header-row)
                 (org-kit::table-recalculate-standard-rows
                  table (document-limits document)
                  (org-kit::make-table-calc-work-budget))
               (declare (ignore data-rows header-row))
               (table-recalculate-test-caught-condition
                (lambda ()
                  (org-kit::table-recalculate-evaluate-entries
                   (list entry) (length rows) 2
                   (make-hash-table :test #'equal) (document-limits document)
                   (org-kit::make-table-calc-work-budget)
                   (make-hash-table :test #'equal)
                   (lambda (work-map row column)
                     (declare (ignore work-map))
                     (org-kit::table-recalculate-cell-value rows row column))
                   table))))))
      (with-soft-assertions
        (expect (typep condition 'org-table-error) :to-be t)
        (when (typep condition 'org-table-error)
          (expect (org-table-error-target condition) :to-equal "table")))))

  (it "returns no cell for non-integer row or column indexes"
    (let ((rows #(#(t))))
      (with-soft-assertions
        (expect (org-kit::table-recalculate-cell rows :invalid 1) :to-be nil)
        (expect (org-kit::table-recalculate-cell rows 1 :invalid) :to-be nil))))

  (it "preserves CRLF and untouched source text when patching cell spans"
    (let* ((source (format nil "#+TITLE: Keep~C~%| a | b |~C~%|---+---|~C~%| 1 | 8 |~C~%#+TBLFM: @2$2=$1+1~C~%" #\Return #\Return #\Return #\Return #\Return))
           (expected (format nil "#+TITLE: Keep~C~%| a | b |~C~%|---+---|~C~%| 1 | 2 |~C~%#+TBLFM: @2$2=$1+1~C~%" #\Return #\Return #\Return #\Return #\Return))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (result (table-recalculate document table)))
      (expect (document-text (edit-result-updated-document result)) :to-equal expected)))
  (it "covers malformed constants and formula normalization fallbacks"
    (let* ((limits (make-limits))
           (tables (make-hash-table :test #'equal))
           (context (org-kit::make-table-recalculate-context :tables tables)))
      (setf (gethash "duplicate" tables) :ambiguous)
      (with-soft-assertions
        (expect (org-kit::table-recalculate-constant-pairs
                 "ok=1 =no no= tail=")
                :to-equal '(("ok" . "1")))
        (expect (org-kit::table-recalculate-constant-value 42 limits)
                :to-be 42)
        (dolist (case '(("$1=" :empty-expression)
                        ("$1=1+" :expected-expression)
                        ("$1=1..+2" :trailing-syntax)))
          (expect
           (table-recalculate-test-condition
            (lambda ()
              (org-kit::table-recalculate-parse-formula
               (first case) limits nil)))
           :to-equal (list :unsupported (second case))))
        (table-recalculate-test-expect-table-error
         (table-recalculate-test-caught-condition
          (lambda ()
            (org-kit::table-recalculate-remote-table
             context "duplicate" "table")))
         :unknown-remote-table))))
  (it "preserves parser errors before range normalization"
    (let* ((limits (make-limits))
           (condition
             (table-recalculate-test-caught-condition
              (lambda ()
                (org-kit::table-recalculate-parse-formula "x" limits nil)))))
      (expect (typep condition (quote org-table-unsupported-syntax))
              :to-be t)
      (expect
       (< (org-kit::org-table-unsupported-syntax-position condition) 2)
       :to-be t)))
  (it "serializes numeric callback values in quoted Lisp ranges"
    (let* ((quote-character (code-char 39))
           (formula (format nil "@2$4=~A(list $1..$3)" quote-character))
           (assignment (table-recalculate-test-assignment formula))
           (expression (org-kit::table-calc-assignment-expression assignment))
           (source (org-kit::table-calc-expression-value expression))
           (expanded
             (org-kit::table-recalculate-expand-elisp-expression
              source expression assignment 2 4 3 4 (make-hash-table) nil nil
              (lambda (row column)
                (declare (ignore row))
                (case column
                  (1 1)
                  (2 "")
                  (3 3)))
              nil (make-limits) (org-kit::make-table-calc-work-budget)))
           (expected
             (format nil "~A(list ~S ~S)" quote-character "1" "3")))
      (expect expanded :to-equal expected)))
  (it "matches Org org-sbe argument coercion and resolves named source blocks"
      (let* ((quote-character (code-char 39))
             (callbacks (make-callback-registry))
             (received-parameters nil)
             (received-variables nil)
             (calls 0)
             (source
              (format nil
                      "#+name: nested-source~%#+begin_src mock~%nested body~%#+end_src~%#+name: value-source~%#+begin_src mock :var inner=nested-source~%value body~%#+end_src~%#+name: sample~%#+begin_src mock :var one=3 :var inner=value-source~%sample body~%#+end_src~%| number | text | result |~%|--------+------+--------|~%| 42 | alpha | old |~%#+TBLFM: @2$3=~A(org-sbe sample (one 7) (number $1) (literal $$1) (many $1 $2) (forced-many $$1 $2))~%"
                      quote-character))
             (document (parse-org source))
             (table (table-recalculate-test-find-node (document-root document) :table)))
        (register-babel-executor
         callbacks "mock"
         (lambda (body parameters variables)
           (incf calls)
           (cond
            ((search "nested body" body) "nested result")
            ((search "value body" body) "source result")
            ((search "sample body" body)
             (setf received-parameters parameters
                   received-variables variables)
             "  completed  ")
            (t "unexpected body"))))
        (let* ((result (table-recalculate document table :callbacks callbacks))
               (updated (edit-result-updated-document result))
               (many (cdr (assoc "many" received-variables :test #'string=)))
               (forced-many
                (cdr (assoc "forced-many" received-variables :test #'string=)))
               (var-values
                (mapcar #'cdr
                        (remove-if-not
                         (lambda (parameter)
                           (string= (car parameter) "VAR"))
                         received-parameters))))
          (with-soft-assertions
           (expect (not (null (search "| 42 | alpha | completed |"
                                      (document-text updated))))
                   :to-be t)
           (expect (cdr (assoc "one" received-variables :test #'string=))
                   :to-equal 7)
           (expect (cdr (assoc "number" received-variables :test #'string=))
                   :to-equal 42)
           (expect (cdr (assoc "literal" received-variables :test #'string=))
                   :to-equal "42")
           (expect (first many) :to-equal 42)
           (expect (symbol-name (second many)) :to-equal "alpha")
           (expect (first forced-many) :to-equal "42")
           (expect (symbol-name (second forced-many)) :to-equal "alpha")
           (expect var-values
                   :to-equal
                   (list "inner=value-source"
                       "one=7"
                       "number=42"
                       "literal=\"42\""
                       "many='(42 alpha)"
                       "forced-many='(\"42\" alpha)"))
           (expect (list calls (cdr (assoc "inner" received-variables :test #'string=)))
                   :to-equal (list 3 "source result"))))
        (let* ((missing-source
                (format nil
                        "#+name: sample~%#+begin_src mock~%body~%#+end_src~%| value | result |~%|-------+--------|~%| alpha | old |~%#+TBLFM: @2$2=~A(org-sbe missing)~%"
                        quote-character))
               (missing-document (parse-org missing-source))
               (missing-table
                (table-recalculate-test-find-node
                 (document-root missing-document) :table))
               (condition
                (handler-case
                    (progn
                      (table-recalculate missing-document missing-table
                                         :callbacks callbacks)
                      nil)
                  (org-babel-error (condition) condition))))
          (with-soft-assertions
           (expect (typep condition 'org-babel-error) :to-be t)
           (when condition
             (expect (org-babel-error-reason condition)
                     :to-be :unresolved-call-target))
           (expect calls :to-be 3)))))
  (it "formats Org-sbe arguments and resolved variables for supported value shapes"
    (let ((limits (make-limits))
          (symbol (make-symbol "sample")))
      (with-soft-assertions
        (expect (org-kit::table-recalculate-org-sbe-printed-data nil)
                :to-equal "nil")
        (expect (org-kit::table-recalculate-org-sbe-printed-data "text")
                :to-equal "\"text\"")
        (expect (org-kit::table-recalculate-org-sbe-printed-data :sample)
                :to-equal ":SAMPLE")
        (expect (org-kit::table-recalculate-org-sbe-printed-data symbol)
                :to-equal "sample")
        (expect (org-kit::table-recalculate-org-sbe-printed-data
                 (list 1 :sample "text"))
                :to-equal "(1 :SAMPLE \"text\")")
        (expect (org-kit::table-recalculate-org-sbe-printed-data 7)
                :to-equal "7")
        (expect (org-kit::table-recalculate-org-sbe-argument-text nil nil)
                :to-equal "nil")
        (expect (org-kit::table-recalculate-org-sbe-argument-text "text" nil)
                :to-equal "text")
        (expect (org-kit::table-recalculate-org-sbe-argument-text "text" t)
                :to-equal "\"text\"")
        (expect (org-kit::table-recalculate-org-sbe-argument-text :sample nil)
                :to-equal ":SAMPLE")
        (expect (org-kit::table-recalculate-org-sbe-argument-text symbol nil)
                :to-equal "sample")
        (expect (org-kit::table-recalculate-org-sbe-argument-text
                 (list 1 :sample) nil)
                :to-equal "(1 :SAMPLE)")
        (expect (org-kit::table-recalculate-org-sbe-argument-text 7 nil)
                :to-equal "7")
        (expect (org-kit::table-recalculate-org-sbe-parameter-text
                 "empty" nil nil limits)
                :to-equal "empty=")
        (expect (org-kit::table-recalculate-org-sbe-parameter-text
                 "single" (list "text") (list t) limits)
                :to-equal "single=\"text\"")
        (expect (org-kit::table-recalculate-org-sbe-parameter-text
                 "multiple" (list 1 "text") (list nil t) limits)
                :to-equal
                (format nil "multiple=~A(1 \"text\")" (code-char 39)))
        (expect (org-kit::table-recalculate-org-sbe-resolved-parameter-text
                 "empty" nil limits)
                :to-equal (format nil "empty=~A()" (code-char 39)))
        (expect (org-kit::table-recalculate-org-sbe-resolved-parameter-text
                 "multiple" (list "text" 7) limits)
                :to-equal
                (format nil "multiple=~A(\"text\" 7)" (code-char 39)))
        (expect (org-kit::table-recalculate-org-sbe-resolved-parameter-text
                 "string" "text" limits)
                :to-equal "string=\"text\"")
        (expect (org-kit::table-recalculate-org-sbe-resolved-parameter-text
                 "number" 7 limits)
                :to-equal "number=7"))))
  (it "resolves Org-sbe overrides and reports missing or cyclic source references"
    (let* ((document
             (parse-org
              (format nil "#+name: sample~%#+begin_src mock~%body~%#+end_src~%")))
           (calls 0)
           (context
             (org-kit::make-babel-resolution-context
              :document document
              :names (org-kit::babel-build-name-index document)
              :result-resolver
              (lambda (name variables)
                (declare (ignore name variables))
                (incf calls)
                "resolved")))
           (symbol (make-symbol "sample"))
           (quote-character (code-char 39))
           (callbacks (make-callback-registry))
           (executor-calls 0))
      (register-babel-executor
       callbacks "mock"
       (lambda (body parameters variables)
         (declare (ignore body parameters variables))
         (incf executor-calls)
         "value"))
      (labels ((recalculate-reason (blocks)
                 (let* ((source
                          (format nil
                                  "~A| value | result |~%|-------+--------|~%| alpha | old |~%#+TBLFM: @2$2=~A(org-sbe sample)~%"
                                  blocks quote-character))
                        (document (parse-org source))
                        (table
                          (table-recalculate-test-find-node
                           (document-root document) :table)))
                   (handler-case
                       (progn
                         (table-recalculate document table :callbacks callbacks)
                         nil)
                     (org-babel-error (condition)
                       (org-babel-error-reason condition))))))
        (with-soft-assertions
          (expect (org-kit::table-recalculate-org-sbe-resolve-value
                   "sample" nil context)
                  :to-equal "resolved")
          (expect (org-kit::table-recalculate-org-sbe-resolve-value
                   symbol nil context)
                  :to-equal "resolved")
          (expect (org-kit::table-recalculate-org-sbe-resolve-value
                   "sample" t context)
                  :to-equal "sample")
          (expect (org-kit::table-recalculate-org-sbe-resolve-value
                   nil nil context)
                  :to-equal nil)
          (expect (org-kit::table-recalculate-org-sbe-resolve-value
                   7 nil context)
                  :to-equal 7)
          (expect
           (cdr (org-kit::table-recalculate-org-sbe-override
                 (list "empty" nil nil) context))
           :to-equal "")
          (expect
           (cdr (org-kit::table-recalculate-org-sbe-override
                 (list "single" (list "sample") (list nil)) context))
           :to-equal "resolved")
          (expect
           (cdr (org-kit::table-recalculate-org-sbe-override
                 (list "literal" (list "sample") (list t)) context))
           :to-equal "sample")
          (expect
           (cdr (org-kit::table-recalculate-org-sbe-override
                 (list "multiple" (list "one" "two") (list nil nil)) context))
           :to-equal (list "one" "two"))
          (expect calls :to-be 3)
          (expect
           (recalculate-reason
            (format nil
                    "#+name: sample~%#+begin_src mock :var inner=missing~%body~%#+end_src~%"))
           :to-be :unresolved-reference)
          (expect
           (recalculate-reason
            (format nil
                    "#+name: sample~%#+begin_src mock :var inner=dependency~%sample body~%#+end_src~%#+name: dependency~%#+begin_src mock :var inner=sample~%dependency body~%#+end_src~%"))
           :to-be :circular-reference)
          (expect executor-calls :to-be 0)))))
)

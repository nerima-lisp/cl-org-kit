(in-package #:cl-org-kit/test)

(defun table-recalculate-test-fixture (relative-path)
  (uiop:read-file-string
   (asdf:system-relative-pathname
    "cl-org-kit" (concatenate 'string "t/fixtures/table/" relative-path))))

(defun table-recalculate-test-find-node (root type)
  (or (when (eq (node-type root) type) root)
      (some (lambda (child) (table-recalculate-test-find-node child type))
            (node-children root))))

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

  (it "evaluates field formulas against the original table snapshot"
    (let ((result
            (table-recalculate-test-run-fixture
             "policy/recalculate/original-field-snapshot.org"
             "policy/expected/recalculate/original-field-snapshot.org")))
      (expect (length (edit-result-spans result)) :to-be 2)))

  (it "accepts an acyclic field range dependency and deduplicates repeated range edges"
    (let* ((source
             (format nil "| a | b | c |~%|---+---+---|~%| 1 | 2 | 0 |~%#+TBLFM: @2$1=10::@2$3=vsum(@2$1..@2$2)+vsum(@2$1..@2$2)~%"))
           (expected
             (format nil "| a | b | c |~%|---+---+---|~%| 10 | 2 | 6 |~%#+TBLFM: @2$1=10::@2$3=vsum(@2$1..@2$2)+vsum(@2$1..@2$2)~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (result (table-recalculate document table)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result)) :to-equal expected)
        (expect (document-text document) :to-equal source))))

  (it "orders multiple field targets referenced by a vertical range"
    (let* ((source
             (format nil "| a | b | c | d |~%|---+---+---+---|~%| x | 1 | 0 | 0 |~%| x | 2 | 0 | 0 |~%| x | 3 | 0 | 0 |~%| x | 4 | 0 | 0 |~%#+TBLFM: @2$2=10::@4$2=20::@2$4=30::@5$3=vsum(@2$2..@4$2)~%"))
           (expected
             (format nil "| a | b | c | d |~%|---+---+---+---|~%| x | 10 | 0 | 30 |~%| x | 2 | 0 | 0 |~%| x | 20 | 0 | 0 |~%| x | 4 | 6 | 0 |~%#+TBLFM: @2$2=10::@4$2=20::@2$4=30::@5$3=vsum(@2$2..@4$2)~%"))
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
             (format nil "| a | b | c |~%|---+---+---|~%| 10 | 2 | 2 |~%#+TBLFM: @2$1=10::@2$3=@2$1+@2$1~%"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (result (table-recalculate document table)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result)) :to-equal expected)
        (expect (document-text document) :to-equal source))))

  (it "bounds the formula in a multiple-directive condition"
    (let* ((long-formula (format nil "$1=~A" (make-string 96 :initial-element #\1)))
           (source (format nil "| a |~%|---|~%| 1 |~%#+TBLFM: ~A~%#+TBLFM: $1=2~%"
                           long-formula))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (condition (table-recalculate-test-caught-condition
                       (lambda () (table-recalculate document table)))))
      (with-soft-assertions
        (expect (typep condition 'org-table-unsupported-syntax) :to-be t)
        (when (typep condition 'org-table-unsupported-syntax)
          (expect (org-table-unsupported-syntax-feature condition)
                  :to-be :multiple-tblfm-directives)
          (expect (org-table-unsupported-syntax-formula condition)
                  :to-equal (org-kit::bounded-text long-formula)))
        (expect (document-text document) :to-equal source))))

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
                  reference 1 1 2 1 (make-hash-table :test #'equal) table)))
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
                  reference 1 1 2 1 (make-hash-table :test #'equal) table)))
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
                  reference 1 1 2 1 (make-hash-table :test #'equal) table)))
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
                  reference 1 1 2 1 (make-hash-table :test #'equal) table)))
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
                  assignment 2 2 (make-hash-table :test #'equal) table)))
              :to-equal '(:table :invalid-reference))))

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
                  assignment 2 2 (make-hash-table :test #'equal) table)))
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
                  assignment 2 2 (make-hash-table :test #'equal) table)))
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
                  assignment 2 2 (make-hash-table :test #'equal) table)))
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
                :to-equal '(:table :unsupported-table-type))
        (expect (table-recalculate-test-condition
                 (lambda () (table-recalculate document foreign-table)))
                :to-equal '(:edit :foreign-node))
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
)

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

  (it "rejects multiple directives without changing the input"
    (let* ((source (table-recalculate-test-fixture
                    "policy/recalculate/multiple-directives.org"))
           (expected (table-recalculate-test-fixture
                      "policy/expected/recalculate/multiple-directives.org"))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table)))
      (with-soft-assertions
        (expect (table-recalculate-test-condition
                 (lambda () (table-recalculate document table)))
                :to-equal '(:unsupported :multiple-tblfm-directives))
        (expect (document-text document) :to-equal expected))))

  (it "rejects duplicate targets and dependency cycles before patching"
    (dolist (case '("duplicate-fields" "cyclic-columns" "cyclic-fields"))
      (let* ((source-path (format nil "policy/recalculate/~A.org" case))
             (expected-path (format nil "policy/expected/recalculate/~A.org" case))
             (source (table-recalculate-test-fixture source-path))
             (expected (table-recalculate-test-fixture expected-path))
             (document (parse-org source))
             (table (table-recalculate-test-find-node (document-root document) :table))
             (condition (table-recalculate-test-condition
                         (lambda () (table-recalculate document table)))))
        (with-soft-assertions
          (expect condition :to-equal
                  (if (string= case "duplicate-fields")
                      '(:table :duplicate-target)
                      '(:table :cyclic-formula)))
          (expect (document-text document) :to-equal expected)))))

  (it "rejects duplicate column targets"
    (let* ((document (parse-org
                      (format nil "| a | b |~%|---+---|~%| 1 | 2 |~%#+TBLFM: $1=1::$1=2~%")))
           (table (table-recalculate-test-find-node (document-root document) :table)))
      (expect (table-recalculate-test-condition
               (lambda () (table-recalculate document table)))
              :to-equal '(:table :duplicate-target))))

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
           (document (parse-org source :limits (make-limits :max-formula-work 4)))
           (table (table-recalculate-test-find-node (document-root document) :table)))
      (with-soft-assertions
        (expect (table-recalculate-test-condition
                 (lambda () (table-recalculate document table)))
                :to-equal '(:limit :max-formula-work 4 5))
        (expect (document-text document) :to-equal source))))

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

  (it "preserves CRLF and untouched source text when patching cell spans"
    (let* ((source (format nil "#+TITLE: Keep~C~%| a | b |~C~%|---+---|~C~%| 1 | 8 |~C~%#+TBLFM: @2$2=$1+1~C~%" #\Return #\Return #\Return #\Return #\Return))
           (expected (format nil "#+TITLE: Keep~C~%| a | b |~C~%|---+---|~C~%| 1 | 2 |~C~%#+TBLFM: @2$2=$1+1~C~%" #\Return #\Return #\Return #\Return #\Return))
           (document (parse-org source))
           (table (table-recalculate-test-find-node (document-root document) :table))
           (result (table-recalculate document table)))
      (expect (document-text (edit-result-updated-document result)) :to-equal expected)))
)

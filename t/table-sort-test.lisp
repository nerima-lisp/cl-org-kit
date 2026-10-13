;;;; t/table-sort-test.lisp
(in-package #:cl-org-kit/test)

(defun table-sort-test-find-table (root)
  (or (when (eq (node-type root) :table) root)
      (some #'table-sort-test-find-table (node-children root))))

(defun table-sort-test-standard-rows (table)
  (remove-if-not (lambda (row)
                   (eq (node-property row :type) :standard))
                 (node-children table)))

(defun table-sort-test-fixture (tree filename)
  (uiop:read-file-string
   (asdf:system-relative-pathname
    "cl-org-kit"
    (format nil "t/fixtures/table/~A/sort/~A" tree filename))))

(defun table-sort-test-table-source (source)
  (let* ((document (parse-org source))
         (table (table-sort-test-find-table (document-root document))))
    (subseq (document-text document) (node-begin table) (node-end table))))

(defun table-sort-test-condition (thunk)
  (handler-case
      (progn (funcall thunk) '(:no-error))
    (org-edit-error (condition)
      (list :edit
            (symbol-name (class-name (class-of condition)))
            (org-kit-error-message condition)
            (org-edit-error-reason condition)
            (org-edit-error-target condition)
            (princ-to-string condition)))
    (org-table-error (condition)
      (list :table
            (symbol-name (class-name (class-of condition)))
            (org-kit-error-message condition)
            (org-table-error-reason condition)
            (org-table-error-target condition)
            (princ-to-string condition)))
    (org-resource-limit-error (condition)
      (list :limit
            (symbol-name (class-name (class-of condition)))
            (org-kit-error-message condition)
            (org-resource-limit-error-limit-name condition)
            (org-resource-limit-error-limit condition)
            (org-resource-limit-error-actual condition)
            (princ-to-string condition)))))

(defun table-sort-test-table-error (message reason)
  (list :table "ORG-TABLE-ERROR" message reason "table"
        (format nil "~A (~(~A~)) for target table" message reason)))

(defun table-sort-test-edit-error (message reason target)
  (list :edit "ORG-EDIT-ERROR" message reason target
        (format nil "~A (~(~A~)) for target ~A" message reason target)))

(defun table-sort-test-limit-error (limit-name limit actual)
  (list :limit "ORG-RESOURCE-LIMIT-ERROR" "Resource limit exceeded"
        limit-name limit actual
        (format nil "Resource limit exceeded: ~(~A~) is ~D, above the limit of ~D"
                limit-name actual limit)))

(defun table-sort-test-edit (source column &rest arguments)
  (let* ((document (parse-org source))
         (table (table-sort-test-find-table (document-root document)))
         (result (apply #'table-sort-rows document table column arguments)))
    (values document result)))

(defun table-sort-test-oracle-fixture (filename column order type start end)
  (let* ((source (table-sort-test-fixture "corpus" filename))
         (expected (table-sort-test-fixture "expected" filename))
         (document (parse-org source))
         (table (table-sort-test-find-table (document-root document)))
         (result (table-sort-rows document table column
                                  :order order :type type
                                  :start start :end end)))
    (values source expected document result)))

(defun table-sort-test-utf8-bom-octets (text)
  (let* ((body (cl-codec-kit:string-to-octets text :encoding :utf-8))
         (octets (make-array (+ 3 (length body))
                             :element-type '(unsigned-byte 8))))
    (replace octets #(239 187 191))
    (replace octets body :start1 3)
    octets))

(defun table-sort-test-limit-result (limit-name value
                                     &optional
                                       (source (format nil "| b |~%| a |~%"))
                                       encoded-p)
  (let* ((source-input (if encoded-p
                           (table-sort-test-utf8-bom-octets source)
                           source))
         (other-limit-name
           (case limit-name
             (:max-output-length :max-output-octets)
             (:max-output-octets :max-output-length)))
         (limits (apply #'make-limits
                        (list limit-name value other-limit-name 100)))
         (document (if encoded-p
                       (parse-org source-input :encoding :utf-8 :limits limits)
                       (parse-org source-input :limits limits)))
         (table (table-sort-test-find-table (document-root document))))
    (handler-case
        (list :ok
              (document-text
               (edit-result-updated-document
                (table-sort-rows document table 1
                                 :order :ascending :type :lexical))))
      (org-resource-limit-error (condition)
        (list :limit
              (symbol-name (class-name (class-of condition)))
              (org-kit-error-message condition)
              (org-resource-limit-error-limit-name condition)
              (org-resource-limit-error-limit condition)
              (org-resource-limit-error-actual condition)
              (princ-to-string condition))))))

(describe "stable table row sorting"
  (it "matches checked-in Emacs Org sort fixtures"
    (with-soft-assertions
      (dolist (case '(("lexical-stable.org" 1 :ascending :lexical 2 6)
                     ("lexical-descending.org" 1 :descending :lexical 2 6)
                     ("lexical-empty-ascending.org"
                      1 :ascending :lexical nil nil)
                     ("lexical-empty-descending.org"
                      1 :descending :lexical nil nil)
                     ("numeric-valid-stable.org" 1 :ascending :numeric 2 6)
                     ("numeric-valid-descending.org" 1 :descending :numeric 2 6)
                     ("selected-range.org" 1 :ascending :lexical 3 4)
                     ("hline-first-section.org" 1 :ascending :lexical 2 3)
                     ("hline-second-section.org" 1 :ascending :lexical 4 5)
                     ("one-row-section-trailing-hline.org"
                      1 :ascending :lexical nil nil)
                     ("no-hline-whole-table.org" 1 :ascending :lexical 1 4)))
        (destructuring-bind (filename column order type start end) case
          (multiple-value-bind (source expected document result)
              (table-sort-test-oracle-fixture filename column order type start end)
            (expect (edit-result-p result) :to-be t)
            (expect (document-text (edit-result-updated-document result))
                    :to-equal expected)
            (expect (document-text document) :to-equal source))))))

  (it "matches policy fixtures for empty and invalid numeric values"
    (let* ((empty-source (table-sort-test-fixture "policy" "empty-last.org"))
           (empty-expected
             (table-sort-test-fixture "policy/expected" "empty-last.org"))
           (empty-document (parse-org empty-source))
           (empty-table
             (table-sort-test-find-table (document-root empty-document)))
           (empty-result
             (table-sort-rows empty-document empty-table 1
                              :order :ascending :type :numeric
                              :start 2 :end 5)))
      (with-soft-assertions
        (expect (table-sort-test-table-source
                 (document-text (edit-result-updated-document empty-result)))
                :to-equal (table-sort-test-table-source empty-expected))
        (expect (document-text empty-document) :to-equal empty-source)))
    (let* ((source
             (table-sort-test-fixture "policy" "empty-last-descending.org"))
           (expected
             (table-sort-test-fixture "policy/expected"
                                      "empty-last-descending.org"))
           (document (parse-org source))
           (table (table-sort-test-find-table (document-root document)))
           (result (table-sort-rows document table 1
                                    :order :descending :type :numeric
                                    :start 2 :end 5)))
      (with-soft-assertions
        (expect (table-sort-test-table-source
                 (document-text (edit-result-updated-document result)))
                :to-equal (table-sort-test-table-source expected))
        (expect (document-text document) :to-equal source)))
    (with-soft-assertions
      (dolist (case '(("nan.org" :numeric 2 3 :invalid-number
                       "Table cell is not a valid decimal number")
                     ("infinity.org" :numeric 2 3 :invalid-number
                      "Table cell is not a valid decimal number")
                     ("invalid-number-prefix.org" :numeric 2 4 :invalid-number
                      "Table cell is not a valid decimal number")
                     ("ambiguous-range.org" :lexical nil nil :ambiguous-range
                      "A row range is required for a table with sections")
                     ("cross-section-range.org" :numeric 2 5 :invalid-range
                      "Table sort range crosses a section boundary")))
        (destructuring-bind (filename type start end reason message) case
          (let* ((source (table-sort-test-fixture "policy" filename))
                 (dot (position #\. filename))
                 (expected-name
                   (concatenate 'string (subseq filename 0 dot) ".sexp"))
                 (expected
                   (table-sort-test-fixture "policy/expected" expected-name))
                 (expected-record
                   (let ((*read-eval* nil))
                     (read-from-string expected)))
                 (document (parse-org source))
                 (table (table-sort-test-find-table (document-root document)))
                 (failure
                   (table-sort-test-condition
                    (lambda ()
                      (apply #'table-sort-rows document table 1
                             :order :ascending :type type
                             (when start (list :start start :end end)))))))
            (expect failure
                    :to-equal
                    (list :table
                          (symbol-name (getf expected-record :class))
                          (getf expected-record :message)
                          (getf expected-record :reason)
                          (getf expected-record :target)
                          (getf expected-record :report)))
            (expect (getf expected-record :condition) :to-be :org-table-error)
            (expect (getf expected-record :reason) :to-be reason)
            (expect message :to-equal (getf expected-record :message))
            (expect (document-text document) :to-equal source))))))

  (it "sorts lexical keys stably while preserving equal-key row order"
    (let* ((source
             (format nil
                     "| key | value |~%| b | second |~%| a | first |~%| b | first |~%"))
           (expected
             (format nil
                     "| key | value |~%| a | first |~%| b | second |~%| b | first |~%")))
      (multiple-value-bind (document result)
          (table-sort-test-edit source 1 :order :ascending :type :lexical
                                     :start 2 :end 4)
        (with-soft-assertions
          (expect (edit-result-p result) :to-be t)
          (expect (document-text (edit-result-updated-document result))
                  :to-equal expected)
          (expect (document-text document) :to-equal source))))
    (let* ((source (format nil "| b |~%| a |~%| B |~%| A |~%"))
           (expected (format nil "| A |~%| B |~%| a |~%| b |~%")))
      (multiple-value-bind (document result)
          (table-sort-test-edit source 1 :order :ascending :type :lexical)
        (with-soft-assertions
          (expect (document-text (edit-result-updated-document result))
                  :to-equal expected)
          (expect (document-text document) :to-equal source)))))

  (it "sorts safe decimal keys and keeps empty values last in either direction"
    (let* ((source
             (format nil
                     "| key | value |~%| 10 | ten |~%| 1.0 | one-a |~%| 1e0 | one-b |~%| | empty |~%| -2 | minus |~%"))
           (ascending
             (format nil
                     "| key | value |~%| -2 | minus |~%| 1.0 | one-a |~%| 1e0 | one-b |~%| 10 | ten |~%| | empty |~%"))
           (descending
             (format nil
                     "| key | value |~%| 10 | ten |~%| 1.0 | one-a |~%| 1e0 | one-b |~%| -2 | minus |~%| | empty |~%")))
      (multiple-value-bind (document result)
          (table-sort-test-edit source 1 :order :ascending :type :numeric
                                     :start 2 :end 6)
        (with-soft-assertions
          (expect (document-text (edit-result-updated-document result))
                  :to-equal ascending)
          (expect (document-text document) :to-equal source)))
      (multiple-value-bind (document result)
          (table-sort-test-edit source 1 :order :descending :type :numeric
                                     :start 2 :end 6)
        (with-soft-assertions
          (expect (document-text (edit-result-updated-document result))
                  :to-equal descending)
          (expect (document-text document) :to-equal source)))))

  (it "keeps explicit and ragged missing keys last in their original order"
    (let* ((source
             (format nil
                     "| id | key |~%|----+-----|~%| r1 | 20 |~%| r2 | |~%| r3 |~%| r4 | 5 |~%"))
           (expected
             (format nil
                     "| id | key |~%|----+-----|~%| r4 | 5 |~%| r1 | 20 |~%| r2 | |~%| r3 |~%")))
      (multiple-value-bind (document result)
          (table-sort-test-edit source 2 :order :ascending :type :numeric
                                     :start 2 :end 5)
        (with-soft-assertions
          (expect (document-text (edit-result-updated-document result))
                  :to-equal expected)
          (expect (document-text document) :to-equal source)))))

  (it "sorts only the requested row range inside its hline section"
    (let* ((source
             (format nil
                     "| header |~%|---|~%| zed |~%| amber |~%|---|~%| delta |~%| charlie |~%"))
           (expected
             (format nil
                     "| header |~%|---|~%| amber |~%| zed |~%|---|~%| delta |~%| charlie |~%")))
      (multiple-value-bind (document result)
          (table-sort-test-edit source 1 :order :ascending :type :lexical
                                      :start 2 :end 3)
        (with-soft-assertions
          (expect (document-text (edit-result-updated-document result))
                  :to-equal expected)
          (expect (document-text document) :to-equal source)))))

  (it "sorts the whole table by default when there are no hline sections"
    (let* ((source (format nil "| b |~%| a |~%| c |~%"))
           (expected (format nil "| a |~%| b |~%| c |~%")))
      (multiple-value-bind (document result)
          (table-sort-test-edit source 1 :order :ascending :type :lexical)
        (with-soft-assertions
          (expect (document-text (edit-result-updated-document result))
                  :to-equal expected)
          (expect (document-text document) :to-equal source)))))

  (it "returns an empty edit result when the selected rows are already sorted"
    (let* ((source (format nil "| alpha |~%| beta |~%"))
           (document (parse-org source))
           (table (table-sort-test-find-table (document-root document)))
           (result (table-sort-rows document table 1
                                    :order :ascending :type :lexical)))
      (with-soft-assertions
        (expect (edit-result-p result) :to-be t)
        (expect (edit-result-spans result) :to-equal nil)
        (expect (document-text (edit-result-updated-document result))
                :to-equal source)
        (expect (document-text document) :to-equal source))))

  (it "sorts the last row without adding a missing final newline"
    (let* ((newline (string #\Newline))
           (source (concatenate 'string "| zed |" newline "| amber |"))
           (expected (concatenate 'string "| amber |" newline "| zed |"))
           (document (parse-org source))
           (table (table-sort-test-find-table (document-root document)))
           (result (table-sort-rows document table 1
                                    :order :ascending :type :lexical
                                    :start 1 :end 2)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result))
                :to-equal expected)
        (expect (char= (char (document-text
                              (edit-result-updated-document result))
                             (1- (length expected)))
                       #\|)
                :to-be t)
        (expect (document-text document) :to-equal source))))

  (it "preserves hlines and rejects omitted ranges for multiple sections"
    (let* ((source
             (format nil
                     "| header |~%|---|~%| zed |~%| amber |~%|---|~%| delta |~%| charlie |~%"))
           (document (parse-org source))
           (table (table-sort-test-find-table (document-root document)))
           (omitted
             (table-sort-test-condition
              (lambda ()
                (table-sort-rows document table 1
                                 :order :ascending :type :lexical))))
           (crossing
             (table-sort-test-condition
              (lambda ()
                (table-sort-rows document table 1
                                 :order :ascending :type :lexical
                                 :start 2 :end 4)))))
      (with-soft-assertions
        (expect omitted
                :to-equal
                (table-sort-test-table-error
                 "A row range is required for a table with sections"
                 :ambiguous-range))
        (expect crossing
                :to-equal
                (table-sort-test-table-error
                 "Table sort range crosses a section boundary" :invalid-range))
        (expect (document-text document) :to-equal source))))

  (it "keeps one-sided row ranges defaulted without hlines and ambiguous with hlines"
    (let* ((source (format nil "| c |~%| b |~%| a |~%"))
           (start-only-expected (format nil "| c |~%| a |~%| b |~%"))
           (end-only-expected (format nil "| b |~%| c |~%| a |~%")))
      (multiple-value-bind (document result)
          (table-sort-test-edit source 1 :start 2)
        (with-soft-assertions
          (expect (document-text (edit-result-updated-document result))
                  :to-equal start-only-expected)
          (expect (document-text document) :to-equal source)))
      (multiple-value-bind (document result)
          (table-sort-test-edit source 1 :end 2)
        (with-soft-assertions
          (expect (document-text (edit-result-updated-document result))
                  :to-equal end-only-expected)
          (expect (document-text document) :to-equal source))))
    (let* ((source (format nil "| z |~%| a |~%|---|~%"))
           (document (parse-org source))
           (table (table-sort-test-find-table (document-root document)))
           (start-only
             (table-sort-test-condition
              (lambda ()
                (table-sort-rows document table 1
                                 :order :ascending :type :lexical
                                 :start 1))))
           (end-only
             (table-sort-test-condition
              (lambda ()
                (table-sort-rows document table 1
                                 :order :ascending :type :lexical
                                 :end 2)))))
      (with-soft-assertions
        (expect start-only
                :to-equal
                (table-sort-test-table-error
                 "A row range is required for a table with sections"
                 :ambiguous-range))
        (expect end-only
                :to-equal
                (table-sort-test-table-error
                 "A row range is required for a table with sections"
                 :ambiguous-range))
        (expect (document-text document) :to-equal source))))

  (it "rejects a pipe table with no standard rows"
    (let* ((source (format nil "|---|~%"))
           (document (parse-org source))
           (table (table-sort-test-find-table (document-root document)))
           (failure
             (table-sort-test-condition
              (lambda ()
                (table-sort-rows document table 1
                                 :order :ascending :type :lexical)))))
      (with-soft-assertions
        (expect failure
                :to-equal
                (table-sort-test-table-error
                 "Table has no standard rows to sort" :invalid-range))
        (expect (document-text document) :to-equal source))))

  (it "rejects invalid public targets, indices, ranges, and sort options"
    (let* ((source (format nil "| b |~%| a |~%"))
           (document (parse-org source))
           (table (table-sort-test-find-table (document-root document)))
           (table-el-document
             (parse-org (format nil "+---+~%| x |~%+---+~%")))
           (table-el
             (table-sort-test-find-table (document-root table-el-document)))
           (foreign-document (parse-org (format nil "| elsewhere |~%")))
           (foreign-table
             (table-sort-test-find-table (document-root foreign-document)))
           (ambiguous-nodes (list table table))
           (wrong-target
             (table-sort-test-condition
              (lambda ()
                (table-sort-rows document (document-root document) 1
                                 :order :ascending :type :lexical))))
           (empty-target
             (table-sort-test-condition
              (lambda ()
                (table-sort-rows document '() 1
                                 :order :ascending :type :lexical))))
           (foreign-target
             (table-sort-test-condition
              (lambda ()
                (table-sort-rows document foreign-table 1
                                 :order :ascending :type :lexical))))
           (unsupported-table
             (table-sort-test-condition
              (lambda ()
                (table-sort-rows table-el-document table-el 1
                                 :order :ascending :type :lexical))))
           (ambiguous-target
             (table-sort-test-condition
              (lambda ()
                (table-sort-rows document ambiguous-nodes 1
                                 :order :ascending :type :lexical))))
           (bad-column
             (table-sort-test-condition
              (lambda ()
                (table-sort-rows document table 0
                                 :order :ascending :type :lexical))))
           (bad-column-type
             (table-sort-test-condition
              (lambda ()
                (table-sort-rows document table :first
                                 :order :ascending :type :lexical))))
           (bad-row-index
             (table-sort-test-condition
              (lambda ()
                (table-sort-rows document table 1
                                 :order :ascending :type :lexical
                                 :start 0 :end 1))))
           (bad-range
             (table-sort-test-condition
              (lambda ()
                (table-sort-rows document table 1
                                 :order :ascending :type :lexical
                                 :start 2 :end 1))))
           (bad-order
             (table-sort-test-condition
              (lambda ()
                (table-sort-rows document table 1
                                 :order :sideways :type :lexical))))
           (bad-type
             (table-sort-test-condition
              (lambda ()
                (table-sort-rows document table 1
                                 :order :ascending :type :custom)))))
      (with-soft-assertions
        (expect wrong-target
                :to-equal
                (table-sort-test-edit-error
                 "Node type is not valid for this edit" :wrong-node-type "org-data"))
        (expect empty-target
                :to-equal
                (table-sort-test-edit-error
                 "Edit target was not found" :target-not-found "target"))
        (expect foreign-target
                :to-equal
                (table-sort-test-edit-error
                 "Node does not belong to the input document" :foreign-node "table"))
        (expect unsupported-table
                :to-equal
                (table-sort-test-table-error
                 "Table.el tables cannot be sorted" :unsupported-table-type))
        (expect (first ambiguous-target) :to-be :edit)
        (expect (second ambiguous-target) :to-equal "ORG-EDIT-ERROR")
        (expect (third ambiguous-target)
                :to-equal "Edit target resolves to multiple nodes")
        (expect (fourth ambiguous-target) :to-be :ambiguous-target)
        (expect (stringp (fifth ambiguous-target)) :to-be t)
        (expect (<= (length (fifth ambiguous-target)) 80) :to-be t)
        (expect (sixth ambiguous-target)
                :to-equal
                (format nil "Edit target resolves to multiple nodes (ambiguous-target) for target ~A"
                        (fifth ambiguous-target)))
        (expect bad-column
                :to-equal
                (table-sort-test-table-error
                 "Table sort column is outside the table width" :invalid-index))
        (expect bad-column-type
                :to-equal
                (table-sort-test-table-error
                 "Table sort column is outside the table width" :invalid-index))
        (expect bad-row-index
                :to-equal
                (table-sort-test-table-error
                 "Table sort row is outside the table" :invalid-index))
        (expect bad-range
                :to-equal
                (table-sort-test-table-error
                 "Table sort start must not follow its end" :invalid-range))
        (expect bad-order
                :to-equal
                (table-sort-test-table-error
                 "Table sort order must be ascending or descending"
                 :invalid-value))
        (expect bad-type
                :to-equal
                (table-sort-test-table-error
                 "Table sort type must be lexical or numeric" :invalid-value))
        (expect (document-text document) :to-equal source))))

  (it "rejects NaN and reader syntax as numeric values without evaluating them"
    (dolist (invalid-value '("NaN" "#.(progn 1)"))
      (let* ((source (format nil "| 1 |~%| ~A |~%" invalid-value))
             (document (parse-org source))
             (table (table-sort-test-find-table (document-root document)))
             (condition
               (table-sort-test-condition
                (lambda ()
                  (table-sort-rows document table 1
                                   :order :ascending :type :numeric)))))
        (with-soft-assertions
          (expect condition
                  :to-equal
                  (table-sort-test-table-error
                   "Table cell is not a valid decimal number" :invalid-number))
          (expect (document-text document) :to-equal source)))))

  (it "keeps source outside selected rows unchanged, including UTF-8 bytes"
    (let* ((crlf (format nil "~C~C" #\Return #\Newline))
           (source (format nil
                           (concatenate 'string
                                        "before" crlf
                                        "| header |" crlf
                                        "|---|" crlf
                                        "| zed |" crlf
                                        "| amber |" crlf
                                        "#+TBLFM: $1=$1" crlf
                                        "after" crlf)))
           (expected (format nil
                             (concatenate 'string
                                          "before" crlf
                                          "| header |" crlf
                                          "|---|" crlf
                                          "| amber |" crlf
                                          "| zed |" crlf
                                          "#+TBLFM: $1=$1" crlf
                                          "after" crlf)))
           (document (parse-org (table-sort-test-utf8-bom-octets source)
                                :encoding :utf-8))
           (table (table-sort-test-find-table (document-root document)))
           (rows (table-sort-test-standard-rows table))
           (selected-start (node-begin (second rows)))
           (selected-end (node-end (third rows)))
           (result (table-sort-rows document table 1
                                    :order :ascending :type :lexical
                                    :start 2 :end 3))
           (updated (edit-result-updated-document result)))
      (with-soft-assertions
        (expect (document-text updated) :to-equal expected)
        (expect (write-org updated :output-type :octets)
                :to-equalp (table-sort-test-utf8-bom-octets expected))
        (expect (document-text document) :to-equal source)
        (expect (every (lambda (span)
                         (and (<= selected-start (edit-span-start span))
                              (<= (edit-span-end span) selected-end)))
                       (edit-result-spans result))
                :to-be t)
        (expect (edit-result-spans result) :not :to-equal '()))))

  (it "checks output limits before parsing numeric sort keys"
    (let* ((source (format nil "| 1 |~%| invalid-number |~%"))
           (limit (1- (length source)))
           (document
             (parse-org source
                        :limits (make-limits :max-output-length limit)))
           (table (table-sort-test-find-table (document-root document)))
           (failure
             (table-sort-test-condition
              (lambda ()
                (table-sort-rows document table 1
                                 :order :ascending :type :numeric
                                 :start 1 :end 2)))))
      (with-soft-assertions
        (expect failure
                :to-equal
                (table-sort-test-limit-error :max-output-length limit
                                             (length source)))
        (expect (document-text document) :to-equal source))))

  (it "checks output character and octet limits below, at, and above the result size"
    (let ((source (format nil "| b |~%| a |~%"))
          (expected (format nil "| a |~%| b |~%")))
      (with-soft-assertions
        (dolist (limit-name '(:max-output-length :max-output-octets))
          (expect (table-sort-test-limit-result limit-name (1- (length source)))
                  :to-equal
                  (table-sort-test-limit-error limit-name
                                               (1- (length source))
                                               (length source)))
          (expect (table-sort-test-limit-result limit-name (length source))
                  :to-equal (list :ok expected))
          (expect (table-sort-test-limit-result limit-name (1+ (length source)))
                  :to-equal (list :ok expected)))))
    (let* ((source (format nil "| 猫 |~%| 犬 |~%"))
           (expected (format nil "| 犬 |~%| 猫 |~%"))
           (octet-count (length (table-sort-test-utf8-bom-octets source))))
      (with-soft-assertions
        (expect (table-sort-test-limit-result :max-output-octets
                                              (1- octet-count) source t)
                :to-equal
                (table-sort-test-limit-error :max-output-octets
                                             (1- octet-count) octet-count))
        (expect (table-sort-test-limit-result :max-output-octets
                                              octet-count source t)
                :to-equal (list :ok expected))
        (expect (table-sort-test-limit-result :max-output-octets
                                              (1+ octet-count) source t)
                :to-equal (list :ok expected))))))

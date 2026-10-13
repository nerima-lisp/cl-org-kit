(in-package #:cl-org-kit/test)

(defun table-edit-test-path (relative-path)
  (asdf:system-relative-pathname
   "cl-org-kit" (concatenate 'string "t/fixtures/table/" relative-path)))

(defun table-edit-test-read (relative-path)
  (uiop:read-file-string (table-edit-test-path relative-path)))

(defun table-edit-test-oracle-output (id key)
  (let* ((encoded-path
           (format nil "expected/edit/encoded/~A.sexp" id))
         (encoded-file (table-edit-test-path encoded-path)))
    (if (probe-file encoded-file)
        (with-input-from-string (stream (uiop:read-file-string encoded-file))
          (let* ((*read-eval* nil)
                 (data (read stream nil :eof))
                 (missing (gensym "MISSING"))
                 (lines (and (listp data) (getf data key missing))))
            (unless (and lines
                         (not (eq lines missing))
                         (every #'stringp lines)
                         (eq (read stream nil :eof) :eof))
              (error "Malformed encoded T3 oracle output: ~A" encoded-path))
            (with-output-to-string (output)
              (dolist (line lines)
                (write-string line output)
                (write-char #\Newline output)))))
        (table-edit-test-read
         (format nil "expected/edit/~A/~A.~A"
                 (if (eq key :source-after) "source-after" "diff")
                 id
                 (if (eq key :source-after) "org" "diff"))))))

(defun table-edit-test-find-node (root type)
  (or (when (eq (node-type root) type) root)
      (some (lambda (child)
              (table-edit-test-find-node child type))
            (node-children root))))

(defun table-edit-test-row-values (row)
  (mapcar (lambda (cell) (node-property cell :value))
          (node-children row)))

(defun table-edit-test-descriptors ()
  (with-input-from-string (stream (table-edit-test-read "operations-t3.sexp"))
    (let* ((*read-eval* nil)
           (descriptors (read stream nil nil)))
      (unless (and descriptors (eq (read stream nil :eof) :eof))
        (error "Malformed T3 operation fixture"))
      descriptors)))

(defun table-edit-test-apply (document table descriptor)
  (let ((action (getf descriptor :action))
        (index (getf descriptor :index))
        (from (getf descriptor :from))
        (to (getf descriptor :to))
        (values (getf descriptor :values)))
    (ecase action
      (:insert-row (table-insert-row document table index :values values))
      (:delete-row (table-delete-row document table index))
      (:move-row (table-move-row document table from to))
      (:insert-column (table-insert-column document table index :values values))
      (:delete-column (table-delete-column document table index))
      (:move-column (table-move-column document table from to)))))

(defun table-edit-test-oracle-diff (descriptor actual)
  (let* ((id (getf descriptor :id))
         (source-name (getf descriptor :source))
         (source-label (concatenate 'string "corpus/" source-name))
         (result-label (format nil "source-after/~A.org" id))
         (expected-directory (table-edit-test-path "expected/edit/"))
         (source-path (table-edit-test-path
                       (concatenate 'string "corpus/edit/" source-name))))
    (uiop:with-temporary-file (:pathname result-path :stream result-stream
                               :direction :output)
      (write-string actual result-stream)
      (finish-output result-stream)
      (uiop:run-program
       (list "diff" "-u" "-L" source-label "-L" result-label
             (namestring source-path) (namestring result-path))
       :directory expected-directory
       :output :string :error-output :string :ignore-error-status t))))

(defun table-edit-test-check-outside (source updated table updated-table)
  (let* ((start (node-begin table))
         (end (reduce #'max (node-property table :reader-tblfm-ranges)
                      :key #'cdr :initial-value (node-end table)))
         (updated-source (document-text updated))
         (updated-start (if updated-table (node-begin updated-table) start))
         (updated-end
           (if updated-table
               (node-end updated-table)
               (- (length updated-source) (- (length source) end)))))
    (and (string= source updated-source :end1 start :end2 updated-start)
         (string= source updated-source :start1 end :start2 updated-end))))

(defun table-edit-test-utf8-bom-octets (text)
  (let* ((body (cl-codec-kit:string-to-octets text :encoding :utf-8))
         (octets (make-array (+ 3 (length body))
                             :element-type '(unsigned-byte 8))))
    (replace octets #(239 187 191))
    (replace octets body :start1 3)
    octets))

(defun table-edit-test-condition (thunk)
  (handler-case
      (progn (funcall thunk) '(:no-error))
    (org-table-error (condition)
      (list :table
            (org-kit-error-message condition)
            (org-table-error-reason condition)
            (org-table-error-target condition)))
    (org-edit-error (condition)
      (list :edit
            (org-kit-error-message condition)
            (org-edit-error-reason condition)
            (org-edit-error-target condition)))
    (org-resource-limit-error (condition)
      (list :limit
            (org-kit-error-message condition)
            (org-resource-limit-error-limit-name condition)
            (org-resource-limit-error-limit condition)
            (org-resource-limit-error-actual condition)))))

(describe "Org table row and column editing"
  (it "matches each Org source-after and unified-diff fixture"
    (let ((descriptors (table-edit-test-descriptors)))
      (with-soft-assertions
        (expect (length descriptors) :to-be 27)
        (dolist (descriptor descriptors)
          (let* ((id (getf descriptor :id))
                 (source (table-edit-test-read
                          (format nil "corpus/edit/~A" (getf descriptor :source))))
                 (expected (table-edit-test-oracle-output id :source-after))
                 (document (parse-org source))
                 (table (table-edit-test-find-node (document-root document) :table))
                 (table-start (node-begin table))
                 (table-end (node-end table))
                 (result (table-edit-test-apply document table descriptor))
                 (updated (edit-result-updated-document result))
                 (actual (document-text updated))
                 (diff (table-edit-test-oracle-diff descriptor actual))
                 (updated-table (table-edit-test-find-node
                                 (document-root updated) :table)))
            (expect (list id (edit-result-p result)) :to-equal (list id t))
            (expect (list id actual) :to-equal (list id expected))
            (expect (list id (document-text document)) :to-equal (list id source))
            (expect (list id diff)
                    :to-equal (list id (table-edit-test-oracle-output id :diff)))
            (expect (list id
                          (every (lambda (span)
                                   (and (<= table-start (edit-span-start span))
                                        (<= (edit-span-end span) table-end)))
                                 (edit-result-spans result)))
                    :to-equal (list id t))
            (expect (list id
                          (table-edit-test-check-outside
                           source updated table updated-table))
                    :to-equal (list id t)))))))

  (it "preserves UTF-8 BOM, CRLF, and source bytes outside the edited table"
    (let* ((crlf (concatenate 'string (string #\Return) (string #\Newline)))
           (source (concatenate 'string "前" crlf
                                "| A | B |" crlf
                                "| x | y |" crlf
                                "後" crlf))
           (expected (concatenate 'string "前" crlf
                                  "| A   | B     |" crlf
                                  "| new | value |" crlf
                                  "| x   | y     |" crlf
                                  "後" crlf))
           (document (parse-org (table-edit-test-utf8-bom-octets source)
                                :encoding :utf-8))
           (table (table-edit-test-find-node (document-root document) :table))
           (result (table-insert-row document table 2 :values '("new" "value")))
           (updated (edit-result-updated-document result)))
      (with-soft-assertions
        (expect (document-text updated) :to-equal expected)
        (expect (write-org updated :output-type :octets)
                :to-equalp (table-edit-test-utf8-bom-octets expected))
        (expect (document-bom-p updated) :to-be t)
        (expect (document-text document) :to-equal source))))

  (it "keeps deleted-row reference sentinels parseable across later edits"
    (let* ((source (table-edit-test-read "corpus/edit/formula-table.org"))
           (document (parse-org source))
           (table (table-edit-test-find-node (document-root document) :table))
           (delete-result (table-delete-row document table 3))
           (after-delete (edit-result-updated-document delete-result))
           (after-delete-source (document-text after-delete))
           (after-delete-table
             (table-edit-test-find-node (document-root after-delete) :table))
           (insert-result
             (table-insert-column after-delete after-delete-table 3))
           (after-insert (edit-result-updated-document insert-result))
           (formula (first (node-property
                            (table-edit-test-find-node
                             (document-root after-insert) :table)
                            :tblfm))))
      (with-soft-assertions
        (expect (search "@INVALID$1" formula) :not :to-be nil)
        (expect (document-text after-delete) :to-equal after-delete-source)
        (expect (document-text document) :to-equal source))))

  (it "preserves invalid-row formula targets while remapping later assignments"
    (let* ((source (format nil
                           "| A | B |~%|---+---|~%| x | y |~%| z | w |~%#+TBLFM: @INVALID$1=3::@2$2=@2$1~%"))
           (document (parse-org source))
           (table (table-edit-test-find-node (document-root document) :table))
           (result (table-insert-row document table 2))
           (updated (document-text (edit-result-updated-document result))))
      (with-soft-assertions
        (expect (edit-result-p result) :to-be t)
        (expect updated :to-equal
                (format nil
                        "| A | B |~%|---+---|~%|   |   |~%| x | y |~%| z | w |~%#+TBLFM: @INVALID$1=3::@3$2=@3$1~%"))
        (expect (document-text document) :to-equal source))))

  (it "keeps row-qualified deleted-column references parseable across later edits"
    (let* ((source (format nil
                           "| A | B |~%| x | y |~%| p | q |~%#+TBLFM: $1=@3$2~%"))
           (document (parse-org source))
           (table (table-edit-test-find-node (document-root document) :table))
           (delete-result (table-delete-column document table 2))
           (after-delete (edit-result-updated-document delete-result))
           (after-delete-table
             (table-edit-test-find-node (document-root after-delete) :table))
           (insert-result (table-insert-row after-delete after-delete-table 1))
           (after-insert (edit-result-updated-document insert-result))
           (formula (first (node-property
                            (table-edit-test-find-node
                             (document-root after-insert) :table)
                            :tblfm))))
      (with-soft-assertions
        (expect (first (node-property
                        (table-edit-test-find-node
                         (document-root after-delete) :table)
                        :tblfm))
                :to-equal "$1=@3$INVALID")
        (expect formula :to-equal "$1=@4$INVALID")
        (expect (document-text document) :to-equal source))))

  (it "keeps INVALID and invalid as distinct named columns during edits"
    (let* ((source (format nil
                           "| INVALID | invalid | Result |~%| upper | lower | old |~%#+TBLFM: $3=$INVALID+$invalid~%"))
           (document (parse-org source))
           (table (table-edit-test-find-node (document-root document) :table))
           (result
             (table-insert-row document table 2
                               :values '("new upper" "new lower" "new result")))
           (updated (edit-result-updated-document result))
           (updated-table
             (table-edit-test-find-node (document-root updated) :table)))
      (with-soft-assertions
        (expect (edit-result-p result) :to-be t)
        (expect (table-edit-test-row-values (first (node-children updated-table)))
                :to-equal '("INVALID" "invalid" "Result"))
        (expect (first (node-property updated-table :tblfm))
                :to-equal "$3=$INVALID+$invalid")
        (expect (document-text document) :to-equal source))))

  (it "uses a collision-free deleted-column reference when INVALID survives"
    (let* ((source (format nil
                           "| Key | INVALID | Other | Result |~%| key | name | other | result |~%#+TBLFM: $4=$1+$INVALID+@2$3~%"))
           (document (parse-org source))
           (table (table-edit-test-find-node (document-root document) :table))
           (delete-result (table-delete-column document table 1))
           (after-delete (edit-result-updated-document delete-result))
           (after-delete-table
             (table-edit-test-find-node (document-root after-delete) :table))
           (after-delete-formula
             (first (node-property after-delete-table :tblfm)))
           (insert-result
             (table-insert-column after-delete after-delete-table 2
                                  :values '("new" "new")))
           (after-insert (edit-result-updated-document insert-result))
           (after-insert-formula
             (first (node-property
                     (table-edit-test-find-node
                      (document-root after-insert) :table)
                     :tblfm))))
      (with-soft-assertions
        (expect (edit-result-p delete-result) :to-be t)
        (expect after-delete-formula :to-equal "$3=$0+$INVALID+@2$2")
        (expect (edit-result-p insert-result) :to-be t)
        (expect after-insert-formula :to-equal "$4=$0+$INVALID+@2$3")
        (expect (document-text document) :to-equal source))))

  (it "does not treat an INVALID row after an hline as the column header"
    (let* ((source (format nil
                           "|-----+-------+--------|~%| INVALID | Other | Result |~%| key | other | old |~%#+TBLFM: $3=$2~%"))
           (document (parse-org source))
           (table (table-edit-test-find-node (document-root document) :table))
           (result (table-delete-column document table 2))
           (updated (edit-result-updated-document result))
           (updated-table
             (table-edit-test-find-node (document-root updated) :table)))
      (with-soft-assertions
        (expect (edit-result-p result) :to-be t)
        (expect (first (node-property updated-table :tblfm))
                :to-equal "$2=$INVALID")
        (expect (document-text document) :to-equal source))))

  (it "preserves existing and inserted rich cell semantics"
    (let* ((source (format nil
                           "| Key | Value |~%| [[https://example.org][old]] | plain |~%"))
           (document (parse-org source))
           (table (table-edit-test-find-node (document-root document) :table))
           (result (table-insert-row
                    document table 2
                    :values '("[[https://example.org][new]]")))
           (updated (edit-result-updated-document result))
           (updated-source (document-text updated)))
      (with-soft-assertions
        (expect (edit-result-p result) :to-be t)
        (expect (search "[[https://example.org][old]]" updated-source)
                :not :to-be nil)
        (expect (search "[[https://example.org][new]]" updated-source)
                :not :to-be nil)
        (expect (document-text document) :to-equal source))))

  (it "remaps oversized absolute row and column references"
    (let* ((digits (make-string 40 :initial-element #\9))
           (incremented (concatenate 'string "1"
                                     (make-string 40 :initial-element #\0)))
           (decremented (concatenate 'string
                                     (make-string 39 :initial-element #\9)
                                     "8"))
           (source (format nil
                           "| A | B |~%| x | y |~%#+TBLFM: @~A$~A=@~A$~A~%"
                           digits digits digits digits))
           (document (parse-org source))
           (table (table-edit-test-find-node (document-root document) :table))
           (row-result (table-insert-row document table 1))
           (row-document (edit-result-updated-document row-result))
           (row-table
             (table-edit-test-find-node (document-root row-document) :table))
           (column-result
             (table-insert-column row-document row-table 1))
           (column-document (edit-result-updated-document column-result))
           (formula
             (first (node-property
                     (table-edit-test-find-node
                      (document-root column-document) :table)
                     :tblfm)))
           (delete-document (parse-org source))
           (delete-table
             (table-edit-test-find-node (document-root delete-document) :table))
           (row-delete-result
             (table-delete-row delete-document delete-table 1))
           (row-delete-document
             (edit-result-updated-document row-delete-result))
           (row-delete-table
             (table-edit-test-find-node (document-root row-delete-document) :table))
           (row-delete-formula
             (first (node-property row-delete-table :tblfm)))
           (column-delete-result
             (table-delete-column row-delete-document row-delete-table 1))
           (column-delete-table
             (table-edit-test-find-node
              (document-root (edit-result-updated-document column-delete-result))
              :table))
           (column-delete-formula
             (first (node-property column-delete-table :tblfm))))
      (with-soft-assertions
        (expect formula :to-equal
                (format nil "@~A$~A=@~A$~A"
                        incremented incremented incremented incremented))
        (expect row-delete-formula :to-equal
                (format nil "@~A$~A=@~A$~A"
                        decremented digits decremented digits))
        (expect column-delete-formula :to-equal
                (format nil "@~A$~A=@~A$~A"
                        decremented decremented decremented decremented))
        (expect (document-text document) :to-equal source))))

  (it "reports invalid indices, hline crossings, and values in condition slots"
    (let* ((source (format nil
                           "| A | B |~%|---+---|~%| x | y |~%|---+---|~%| z | w |~%"))
           (document (parse-org source))
           (table (table-edit-test-find-node (document-root document) :table))
           (invalid-index
             (table-edit-test-condition
              (lambda () (table-delete-row document table 0))))
           (above-upper-bound
             (table-edit-test-condition
              (lambda () (table-delete-row document table 4))))
           (noninteger-index
             (table-edit-test-condition
              (lambda () (table-delete-column document table "1"))))
           (crossing
             (table-edit-test-condition
              (lambda () (table-move-row document table 2 3))))
           (too-many-row-values
             (table-edit-test-condition
              (lambda ()
                (table-insert-row document table 1
                                  :values '("a" "b" "c")))))
           (too-many-column-values
             (table-edit-test-condition
              (lambda ()
                (table-insert-column document table 1
                                     :values '("a" "b" "c" "d")))))
           (nonstring-values
             (table-edit-test-condition
              (lambda ()
                (table-insert-row document table 1 :values '("a" 2)))))
           (results (list invalid-index above-upper-bound noninteger-index crossing
                          too-many-row-values too-many-column-values
                          nonstring-values)))
      (with-soft-assertions
        (dolist (condition results)
          (expect (first condition) :to-equal :table)
          (expect (stringp (second condition)) :to-be t)
          (expect (fourth condition) :to-equal "table"))
        (expect (second invalid-index) :to-equal
                "Table row or column index is out of range")
        (expect (second above-upper-bound) :to-equal
                "Table row or column index is out of range")
        (expect (second noninteger-index) :to-equal
                "Table row or column index is out of range")
        (expect (second crossing) :to-equal
                "A row cannot move across an hline boundary")
        (expect (second too-many-row-values) :to-equal
                "Too many values for the table structure")
        (expect (second too-many-column-values) :to-equal
                "Too many values for the table structure")
        (expect (second nonstring-values) :to-equal
                "Table values must be single-line strings")
        (expect (third invalid-index) :to-be :invalid-index)
        (expect (third above-upper-bound) :to-be :invalid-index)
        (expect (third noninteger-index) :to-be :invalid-index)
        (expect (third crossing) :to-be :invalid-range)
        (expect (third too-many-row-values) :to-be :invalid-value)
        (expect (third too-many-column-values) :to-be :invalid-value)
        (expect (third nonstring-values) :to-be :invalid-value)
        (expect (document-text document) :to-equal source))))

  (it "rejects improper and multiline inserted values and table.el targets"
    (let* ((source (format nil "| A |~%| x |~%"))
           (document (parse-org source))
           (table (table-edit-test-find-node (document-root document) :table))
           (improper-values
             (table-edit-test-condition
              (lambda ()
                (table-insert-row document table 1 :values (cons "a" "b")))))
           (carriage-return-value
             (table-edit-test-condition
              (lambda ()
                (table-insert-row
                 document table 1
                 :values (list (concatenate 'string "a" (string #\Return) "b"))))))
           (newline-value
             (table-edit-test-condition
              (lambda ()
                (table-insert-row document table 1
                                  :values (list (format nil "a~%b"))))))
           (table-el-document
             (parse-org (format nil "+---+---+~%| one | two |~%+---+---+~%")))
           (table-el
             (table-edit-test-find-node (document-root table-el-document) :table))
           (table-el-condition
             (table-edit-test-condition
              (lambda () (table-insert-row table-el-document table-el 1)))))
      (with-soft-assertions
        (expect improper-values :to-equal
                '(:table "Table values must be a proper list" :invalid-value "table"))
        (expect carriage-return-value :to-equal
                '(:table "Table values must be single-line strings"
                  :invalid-value "table"))
        (expect newline-value :to-equal
                '(:table "Table values must be single-line strings"
                  :invalid-value "table"))
        (expect table-el-condition :to-equal
                '(:table "Only Org pipe tables can be edited"
                  :unsupported-table-type "table"))
        (expect (document-text document) :to-equal source))))

  (it "leaves row and column moves unchanged when their endpoints match"
    (let* ((source (format nil
                           "| A | B |~%| x | y |~%#+TBLFM: @2$1=1::$1=@2$2~%"))
           (row-document (parse-org source))
           (row-table
             (table-edit-test-find-node (document-root row-document) :table))
           (row-result (table-move-row row-document row-table 2 2))
           (row-updated (edit-result-updated-document row-result))
           (column-document (parse-org source))
           (column-table
             (table-edit-test-find-node (document-root column-document) :table))
           (column-result
             (table-move-column column-document column-table 1 1))
           (column-updated (edit-result-updated-document column-result)))
      (with-soft-assertions
        (expect (document-text row-updated) :to-equal source)
        (expect (document-text column-updated) :to-equal source)
        (expect (edit-result-spans row-result) :to-be nil)
        (expect (edit-result-spans column-result) :to-be nil)
        (expect (document-text row-document) :to-equal source)
        (expect (document-text column-document) :to-equal source))))

  (it "maps unsupported formula syntax to an edit error"
    (let* ((source (format nil
                           "| A |~%| x |~%#+TBLFM: $1=#.(progn 1)~%"))
           (document (parse-org source))
           (table (table-edit-test-find-node (document-root document) :table))
           (condition
             (table-edit-test-condition
              (lambda () (table-insert-column document table 1)))))
      (with-soft-assertions
        (expect (first condition) :to-be :edit)
        (expect (second condition) :to-equal
                "TBLFM syntax reader-eval at position 3 cannot be remapped")
        (expect (third condition) :to-be :uneditable-structure)
        (expect (fourth condition) :to-equal "table")
        (expect (document-text document) :to-equal source))))

  (it "rejects multiple attached TBLFM directives with a contract reason"
    (let* ((source (format nil
                           "| A |~%| x |~%#+TBLFM: $1=1~%#+TBLFM: $1=2~%"))
           (document (parse-org source))
           (table (table-edit-test-find-node (document-root document) :table))
           (condition
             (table-edit-test-condition
              (lambda () (table-insert-row document table 1)))))
      (with-soft-assertions
        (expect (first condition) :to-be :edit)
        (expect (second condition) :to-equal
                "Multiple TBLFM directives cannot be remapped safely")
        (expect (third condition) :to-be :uneditable-structure)
        (expect (fourth condition) :to-equal "table")
        (expect (document-text document) :to-equal source))))

  (it "preserves absent final newlines when inserting and moving rows"
    (let* ((source (format nil "| A |~%| x |~%| y |"))
           (insert-document (parse-org source))
           (insert-table
             (table-edit-test-find-node (document-root insert-document) :table))
           (insert-result
             (table-insert-row insert-document insert-table 3 :values '("z")))
           (inserted (document-text (edit-result-updated-document insert-result)))
           (move-document (parse-org source))
           (move-table
             (table-edit-test-find-node (document-root move-document) :table))
           (move-result (table-move-row move-document move-table 2 3))
           (moved (document-text (edit-result-updated-document move-result))))
      (with-soft-assertions
        (expect inserted :to-equal (format nil "| A |~%| x |~%| z |~%| y |"))
        (expect moved :to-equal (format nil "| A |~%| y |~%| x |"))
        (expect (char= (char inserted (1- (length inserted))) #\Newline)
                :to-be nil)
        (expect (char= (char moved (1- (length moved))) #\Newline)
                :to-be nil)
        (expect (document-text insert-document) :to-equal source)
        (expect (document-text move-document) :to-equal source))))

  (it "separates rows inserted into a one-row source without adding a final newline"
    (let* ((source "| only |")
           (document (parse-org source))
           (table (table-edit-test-find-node (document-root document) :table))
           (result (table-insert-row document table 2 :values '("new")))
           (updated (document-text (edit-result-updated-document result))))
      (with-soft-assertions
        (expect (edit-result-p result) :to-be t)
        (expect updated :to-equal (format nil "| only |~%| new  |"))
        (expect (char= (char updated (1- (length updated))) #\Newline)
                :to-be nil)
        (expect (document-text document) :to-equal source))))

  (it "inserts before an hline-only table as Org does"
    (let* ((source (format nil "|---|~%"))
           (document (parse-org source))
           (table (table-edit-test-find-node (document-root document) :table))
           (result (table-insert-row document table 1))
           (updated (document-text (edit-result-updated-document result))))
      (with-soft-assertions
        (expect (edit-result-p result) :to-be t)
        (expect updated :to-equal (format nil "|   |~%|---|~%"))
        (expect (document-text document) :to-equal source))))

  (it "normalizes ragged rows when deleting and moving rows"
    (let* ((source (format nil
                           "| A | B | C |~%| short |~%| one | two | three |~%"))
           (delete-document (parse-org source))
           (delete-table
             (table-edit-test-find-node (document-root delete-document) :table))
           (delete-result (table-delete-row delete-document delete-table 3))
           (deleted-document (edit-result-updated-document delete-result))
           (deleted-table
             (table-edit-test-find-node (document-root deleted-document) :table))
           (deleted-rows (node-children deleted-table))
           (move-document (parse-org source))
           (move-table
             (table-edit-test-find-node (document-root move-document) :table))
           (move-result (table-move-row move-document move-table 2 3))
           (moved-document (edit-result-updated-document move-result))
           (moved-table
             (table-edit-test-find-node (document-root moved-document) :table))
           (moved-rows (node-children moved-table)))
      (with-soft-assertions
        (expect (edit-result-p delete-result) :to-be t)
        (expect (edit-result-p move-result) :to-be t)
        (expect (mapcar (lambda (row) (length (node-children row))) deleted-rows)
                :to-equal '(3 3))
        (expect (table-edit-test-row-values (car (last deleted-rows)))
                :to-equal '("short" "" ""))
        (expect (mapcar (lambda (row) (length (node-children row))) moved-rows)
                :to-equal '(3 3 3))
        (expect (table-edit-test-row-values (car (last moved-rows)))
                :to-equal '("short" "" ""))
        (expect (document-text delete-document) :to-equal source)
        (expect (document-text move-document) :to-equal source))))

  (it "keeps existing backslash escapes before inserted pipes balanced"
    (let* ((source (format nil "| A | B |~%| old | row |~%"))
           (document (parse-org source))
           (table (table-edit-test-find-node (document-root document) :table))
           (result (table-insert-row document table 2
                                     :values '("one\\|pipe" "two\\\\|pipe")))
           (updated (edit-result-updated-document result))
           (updated-table
             (table-edit-test-find-node (document-root updated) :table))
           (inserted-row (second (node-children updated-table))))
      (with-soft-assertions
        (expect (edit-result-p result) :to-be t)
        (expect (length (node-children inserted-row)) :to-be 2)
        (expect (table-edit-test-row-values inserted-row)
                :to-equal '("one\\|pipe" "two\\\\\\|pipe"))
        (expect (document-text document) :to-equal source))))

  (it "checks output limits before returning an edit result"
    (let* ((source (format nil "| A |~%| x |~%"))
           (base-document (parse-org source))
           (base-table
             (table-edit-test-find-node (document-root base-document) :table))
           (expected-length
             (length
              (document-text
               (edit-result-updated-document
                (table-insert-row base-document base-table 1)))))
           (boundary-document
             (parse-org source
                        :limits (make-limits :max-output-length expected-length
                                             :max-output-octets expected-length)))
           (boundary-table
             (table-edit-test-find-node
              (document-root boundary-document) :table))
           (boundary-result
             (table-insert-row boundary-document boundary-table 1))
           (length-document
             (parse-org source
                        :limits (make-limits
                                 :max-output-length (1- expected-length))))
           (length-table
             (table-edit-test-find-node (document-root length-document) :table))
           (length-condition
             (table-edit-test-condition
              (lambda () (table-insert-row length-document length-table 1))))
           (octet-document
             (parse-org source
                        :limits (make-limits :max-output-octets (1- expected-length))))
           (octet-table
             (table-edit-test-find-node (document-root octet-document) :table))
           (octet-condition
             (table-edit-test-condition
              (lambda () (table-insert-row octet-document octet-table 1)))))
      (with-soft-assertions
        (expect (edit-result-p boundary-result) :to-be t)
        (expect (first length-condition) :to-be :limit)
        (expect (third length-condition) :to-be :max-output-length)
        (expect (fourth length-condition) :to-be (1- expected-length))
        (expect (fifth length-condition) :to-be expected-length)
        (expect (first octet-condition) :to-be :limit)
        (expect (third octet-condition) :to-be :max-output-octets)
        (expect (fourth octet-condition) :to-be (1- expected-length))
        (expect (fifth octet-condition) :to-be expected-length)
        (expect (document-text base-document) :to-equal source)
        (expect (document-text boundary-document) :to-equal source)
        (expect (document-text length-document) :to-equal source)
        (expect (document-text octet-document) :to-equal source))))

  (it "checks the output character lower bound before parsing TBLFM syntax"
    (let* ((source (format nil
                           "| A |~%| a longer value |~%#+TBLFM: $1=#.(progn 1)~%"))
           (document (parse-org source
                                :limits (make-limits :max-output-length 12)))
           (table (table-edit-test-find-node (document-root document) :table))
           (condition
             (table-edit-test-condition
              (lambda () (table-insert-column document table 1)))))
      (with-soft-assertions
        (expect (first condition) :to-be :limit)
        (expect (second condition) :to-equal "Resource limit exceeded")
        (expect (third condition) :to-be :max-output-length)
        (expect (fourth condition) :to-be 12)
        (expect (> (fifth condition) 12) :to-be t)
        (expect (document-text document) :to-equal source))))

  (it "checks a lower-bound output octet limit before parsing TBLFM syntax"
    (let* ((source (format nil
                           "| A |~%| a longer value |~%#+TBLFM: $1=#.(progn 1)~%"))
           (document (parse-org source
                                :limits (make-limits :max-output-octets 1)))
           (table (table-edit-test-find-node (document-root document) :table))
           (condition
             (table-edit-test-condition
              (lambda () (table-insert-column document table 1)))))
      (with-soft-assertions
        (expect (first condition) :to-be :limit)
        (expect (second condition) :to-equal "Resource limit exceeded")
        (expect (third condition) :to-be :max-output-octets)
        (expect (fourth condition) :to-be 1)
        (expect (> (fifth condition) 1) :to-be t)
        (expect (document-text document) :to-equal source))))

  (it "checks UTF-8 output octet lower bounds before parsing TBLFM syntax"
    (let* ((source (format nil
                           "| A |~%| 日本語日本語日本語日本語 |~%#+TBLFM: $1=#.(progn 1)~%"))
           (document (parse-org source
                                :limits (make-limits :max-output-octets 64)))
           (table (table-edit-test-find-node (document-root document) :table))
           (condition
             (table-edit-test-condition
              (lambda () (table-insert-column document table 1)))))
      (with-soft-assertions
        (expect (first condition) :to-be :limit)
        (expect (second condition) :to-equal "Resource limit exceeded")
        (expect (third condition) :to-be :max-output-octets)
        (expect (fourth condition) :to-be 64)
        (expect (> (fifth condition) 64) :to-be t)
        (expect (document-text document) :to-equal source))))

  (it "accounts for leading-zero reference normalization at output limits"
    (let ((source (format nil
                          "| A |~%| x |~%#+TBLFM: @02$01=@02$01~%")))
      (labels ((apply-insert (document axis)
                 (let ((table
                         (table-edit-test-find-node
                          (document-root document) :table)))
                   (ecase axis
                     (:row (table-insert-row document table 1))
                     (:column (table-insert-column
                               document table 1 :values '("" ""))))))
               (inserted-output (axis &optional limits)
                 (let* ((document (if limits
                                      (parse-org source :limits limits)
                                      (parse-org source)))
                        (result (apply-insert document axis)))
                   (document-text (edit-result-updated-document result))))
               (output-at-boundary (axis output)
                 (inserted-output
                  axis
                  (make-limits :max-output-length (length output)
                               :max-output-octets
                               (cl-codec-kit:string-size-in-octets
                                output :encoding :utf-8)))))
        (let ((row-output (inserted-output :row))
              (column-output (inserted-output :column)))
          (with-soft-assertions
            (expect (search "#+TBLFM: @3$01=@3$01" row-output)
                    :not :to-be nil)
            (expect (search "#+TBLFM: @02$2=@02$2" column-output)
                    :not :to-be nil)
            (expect (output-at-boundary :row row-output) :to-equal row-output)
            (expect (output-at-boundary :column column-output)
                    :to-equal column-output))))))

  (it "accounts for retained formulas and inserted absolute-reference growth"
    (let* ((digits (make-string 40 :initial-element #\9))
           (base-source (format nil "| A |~%| x |~%"))
           (row-formula (format nil "#+TBLFM: @~A$1=1~%" digits))
           (column-formula (format nil "#+TBLFM: $~A=1~%" digits))
           (row-source (concatenate 'string base-source row-formula))
           (column-source (concatenate 'string base-source column-formula)))
      (labels ((apply-insert (document action)
                 (let ((table
                         (table-edit-test-find-node
                          (document-root document) :table)))
                   (ecase action
                     (:row (table-insert-row document table 1))
                     (:column (table-insert-column
                               document table 1 :values '("" ""))))))
               (inserted-output (source action &optional limits)
                 (let* ((document (if limits
                                      (parse-org source :limits limits)
                                      (parse-org source)))
                        (result (apply-insert document action)))
                   (document-text (edit-result-updated-document result))))
               (table-only-delta (action)
                 (- (length (inserted-output base-source action))
                    (length base-source)))
               (condition-for (source action limits)
                 (let ((document (parse-org source :limits limits)))
                   (table-edit-test-condition
                    (lambda () (apply-insert document action))))))
        (let* ((row-valid-output (inserted-output row-source :row))
               (column-valid-output (inserted-output column-source :column))
               (row-valid-length (length row-valid-output))
               (column-valid-length (length column-valid-output))
               (row-boundary-output
                 (inserted-output
                  row-source :row
                  (make-limits :max-output-length row-valid-length
                               :max-output-octets
                               (cl-codec-kit:string-size-in-octets
                                row-valid-output :encoding :utf-8))))
               (column-boundary-output
                 (inserted-output
                  column-source :column
                  (make-limits :max-output-length column-valid-length
                               :max-output-octets
                               (cl-codec-kit:string-size-in-octets
                                column-valid-output :encoding :utf-8))))
               (row-unsupported-source
                 (concatenate 'string base-source
                              (format nil "#+TBLFM: @~A$1=0+0+#.(progn 1)~%"
                                      digits)))
               (column-unsupported-source
                 (concatenate 'string base-source
                              (format nil "#+TBLFM: $~A=0+0+#.(progn 1)~%"
                                      digits)))
               (row-limit (+ (length row-unsupported-source)
                             (table-only-delta :row)))
               (column-octet-limit
                 (+ (cl-codec-kit:string-size-in-octets
                     column-unsupported-source :encoding :utf-8)
                    (table-only-delta :column)))
               (row-condition
                 (condition-for row-unsupported-source :row
                                (make-limits :max-output-length row-limit)))
               (column-condition
                 (condition-for column-unsupported-source :column
                                (make-limits
                                 :max-output-octets column-octet-limit))))
          (with-soft-assertions
            (expect row-boundary-output :to-equal row-valid-output)
            (expect column-boundary-output :to-equal column-valid-output)
            (expect (first row-condition) :to-be :limit)
            (expect (second row-condition)
                    :to-equal "Resource limit exceeded")
            (expect (third row-condition) :to-be :max-output-length)
            (expect (fourth row-condition) :to-be row-limit)
            (expect (fifth row-condition) :to-be (1+ row-limit))
            (expect (first column-condition) :to-be :limit)
            (expect (second column-condition)
                    :to-equal "Resource limit exceeded")
            (expect (third column-condition) :to-be :max-output-octets)
            (expect (fourth column-condition) :to-be column-octet-limit)
            (expect (fifth column-condition) :to-be (1+ column-octet-limit)))))))

  (it "checks delete and move reference growth before parsing TBLFM syntax"
    (let* ((delete-column-table
             (format nil "| A | B | C |~%| 一 | 二 | 三 |~%"))
           (delete-row-table
             (format nil "| Item |~%| alpha |~%| 日本 |~%| omega |~%"))
           (move-row-table
             (with-output-to-string (stream)
               (format stream "| Item |~%")
               (loop for index from 1 to 9
                     do (format stream "| r~D |~%" index))))
           (move-column-table
             (format nil "| ~{~A~^ | ~} |~%| ~{~A~^ | ~} |~%"
                     '("A1" "A2" "A3" "A4" "A5"
                       "A6" "A7" "A8" "A9" "A10")
                     '("一" "二" "三" "四" "五"
                       "六" "七" "八" "九" "十")))
           (cases
             (list
              (list :delete-column delete-column-table
                    (format nil "#+TBLFM: $3=$2+1+#.(progn 1)~%")
                    6 :max-output-length)
              (list :delete-row delete-row-table
                    (format nil "#+TBLFM: @4$1=@3$1+1+#.(progn 1)~%")
                    6 :max-output-octets)
              (list :move-row move-row-table
                    (format nil "#+TBLFM: @9$1=@9$1+1+#.(progn 1)~%")
                    2 :max-output-length)
              (list :move-column move-column-table
                    (format nil "#+TBLFM: $9=$9+1+#.(progn 1)~%")
                    2 :max-output-octets))))
      (labels ((edit (source action)
                 (let* ((document (parse-org source))
                        (table
                          (table-edit-test-find-node
                           (document-root document) :table))
                        (result
                          (case action
                            (:delete-column
                             (table-delete-column document table 2))
                            (:delete-row
                             (table-delete-row document table 3))
                            (:move-row
                             (table-move-row document table 9 10))
                            (:move-column
                             (table-move-column document table 9 10)))))
                   (document-text (edit-result-updated-document result))))
               (size (text limit-name)
                 (if (eq limit-name :max-output-length)
                     (length text)
                     (cl-codec-kit:string-size-in-octets
                      text :encoding :utf-8)))
               (condition-for (source action limit-name maximum)
                 (let* ((limits
                          (if (eq limit-name :max-output-length)
                              (make-limits :max-output-length maximum)
                              (make-limits :max-output-octets maximum)))
                        (document (parse-org source :limits limits))
                        (table
                          (table-edit-test-find-node
                           (document-root document) :table)))
                   (table-edit-test-condition
                    (lambda ()
                      (case action
                        (:delete-column
                         (table-delete-column document table 2))
                        (:delete-row
                         (table-delete-row document table 3))
                        (:move-row
                         (table-move-row document table 9 10))
                        (:move-column
                         (table-move-column document table 9 10))))))))
        (with-soft-assertions
          (dolist (case-data cases)
            (destructuring-bind (action table-source formula growth limit-name)
                case-data
              (let* ((source (concatenate 'string table-source formula))
                     (table-delta (- (size (edit table-source action) limit-name)
                                     (size table-source limit-name)))
                     (maximum (+ (size source limit-name)
                                 table-delta growth -1))
                     (condition
                       (condition-for source action limit-name maximum)))
                (expect (first condition) :to-be :limit)
                (expect (second condition) :to-equal "Resource limit exceeded")
                (expect (third condition) :to-be limit-name)
                (expect (fourth condition) :to-be maximum)
                (expect (fifth condition) :to-be (1+ maximum)))))))))

  (it "counts a required terminal formula separator before parsing TBLFM syntax"
    (let* ((table-source (format nil "| A | B |~%| x | y |~%"))
           (valid-source
             (concatenate 'string table-source
                          (format nil "#+TBLFM: $1=1::$2=1~%")))
           (unsupported-source
             (concatenate 'string table-source
                          (format nil "#+TBLFM: $1=1::$2=#.(progn 1)~%")))
           (valid-document (parse-org valid-source))
           (valid-table
             (table-edit-test-find-node (document-root valid-document) :table))
           (valid-result (table-delete-column valid-document valid-table 2))
           (valid-output
             (document-text (edit-result-updated-document valid-result)))
           (character-limit (1- (length valid-output)))
           (octet-limit
             (1- (cl-codec-kit:string-size-in-octets
                  valid-output :encoding :utf-8))))
      (labels ((condition-for (limit-name maximum)
                 (let* ((limits
                          (if (eq limit-name :max-output-length)
                              (make-limits :max-output-length maximum)
                              (make-limits :max-output-octets maximum)))
                        (document (parse-org unsupported-source :limits limits))
                        (table
                          (table-edit-test-find-node
                           (document-root document) :table)))
                   (table-edit-test-condition
                    (lambda () (table-delete-column document table 2))))))
        (let ((character-condition
                (condition-for :max-output-length character-limit))
              (octet-condition
                (condition-for :max-output-octets octet-limit)))
          (with-soft-assertions
            (expect (search "#+TBLFM: $1=1::" valid-output) :not :to-be nil)
            (expect (first character-condition) :to-be :limit)
            (expect (second character-condition)
                    :to-equal "Resource limit exceeded")
            (expect (third character-condition) :to-be :max-output-length)
            (expect (fourth character-condition) :to-be character-limit)
            (expect (> (fifth character-condition) character-limit) :to-be t)
            (expect (first octet-condition) :to-be :limit)
            (expect (second octet-condition)
                    :to-equal "Resource limit exceeded")
            (expect (third octet-condition) :to-be :max-output-octets)
            (expect (fourth octet-condition) :to-be octet-limit)
            (expect (> (fifth octet-condition) octet-limit) :to-be t))))))

  (it "checks inserted-value character limits before encoding the value"
    (let* ((source (format nil "| A |~%| x |~%"))
           (document (parse-org source :encoding :ascii
                                :limits (make-limits :max-output-length 0)))
           (table (table-edit-test-find-node (document-root document) :table))
           (condition
             (table-edit-test-condition
              (lambda ()
                (table-insert-row document table 1 :values '("é"))))))
      (with-soft-assertions
        (expect (first condition) :to-be :limit)
        (expect (second condition) :to-equal "Resource limit exceeded")
        (expect (third condition) :to-be :max-output-length)
        (expect (fourth condition) :to-be 0)
        (expect (document-text document) :to-equal source))))

  (it "deletes the sole row and leaves a standalone blank TBLFM directive"
    (let* ((source (table-edit-test-read "corpus/edit/sole-row.org"))
           (expected (table-edit-test-oracle-output "row-delete-sole" :source-after))
           (document (parse-org source))
           (table (table-edit-test-find-node (document-root document) :table))
           (result (table-delete-row document table 1))
           (updated (edit-result-updated-document result)))
      (with-soft-assertions
        (expect (edit-result-p result) :to-be t)
        (expect (document-text updated) :to-equal expected)
        (expect (search "#+TBLFM: " (document-text updated))
                :not :to-be nil)
        (expect (document-text document) :to-equal source))))

  (it "deletes the sole column into empty rows and a blank TBLFM directive"
    (let* ((source (table-edit-test-read "corpus/edit/sole-column.org"))
           (expected
             (table-edit-test-oracle-output "column-delete-sole" :source-after))
           (document (parse-org source))
           (table (table-edit-test-find-node (document-root document) :table))
           (result (table-delete-column document table 1))
           (updated (edit-result-updated-document result))
           (updated-source (document-text updated)))
      (with-soft-assertions
        (expect (edit-result-p result) :to-be t)
        (expect updated-source :to-equal expected)
        (expect (search "||" updated-source) :not :to-be nil)
        (expect (search "#+TBLFM: " updated-source) :not :to-be nil)
        (expect (document-text document) :to-equal source))))

  (it "preserves TBLFM separators, format suffixes, spacing, and empty formulas"
    (let* ((empty-source (format nil "| A |~%| x |~%#+TBLFM:~%"))
           (empty-document (parse-org empty-source))
           (empty-table
             (table-edit-test-find-node (document-root empty-document) :table))
           (empty-result (table-insert-row empty-document empty-table 2))
           (terminal-source
             (format nil
                     "| A | B | C |~%|---+---+---|~%| x | y | z |~%#+TBLFM: $1=1::$2=2::$3=3::~%"))
           (terminal-document (parse-org terminal-source))
           (terminal-table
             (table-edit-test-find-node (document-root terminal-document) :table))
           (terminal-result
             (table-delete-column terminal-document terminal-table 2))
           (column-input
             (format nil "| A | B |~%| x | y |~%"))
           (column-expected
             (format nil "|   | A | B |~%|   | x | y |~%"))
           (suffix-source
             (concatenate 'string column-input
                          (format nil "#+TBLFM: $1=1;%.2f::$2=2~%")))
           (suffix-terminal-source
             (concatenate 'string column-input
                          (format nil "#+TBLFM: $1=1;%.2f~%")))
           (spaced-source
             (concatenate 'string column-input
                          (format nil
                                  "#+TBLFM:   $1=1   ::   $2=2   ~%")))
           (crlf (concatenate 'string (string #\Return) (string #\Newline)))
           (crlf-source
             (concatenate 'string "| A |" crlf "| x |" crlf
                          "#+TBLFM: $1=1   " crlf))
           (crlf-expected
             (concatenate 'string "| A |" crlf "|   |" crlf "| x |" crlf
                          "#+TBLFM: $1=1   " crlf)))
      (labels ((insert-first-column (source)
                 (let* ((document (parse-org source))
                        (table
                          (table-edit-test-find-node
                           (document-root document) :table))
                        (result (table-insert-column document table 1)))
                   (document-text (edit-result-updated-document result)))))
        (with-soft-assertions
          (expect (document-text (edit-result-updated-document empty-result))
                  :to-equal (format nil "| A |~%|   |~%| x |~%#+TBLFM:~%"))
          (expect (document-text terminal-document) :to-equal terminal-source)
          (expect (document-text
                   (edit-result-updated-document terminal-result))
                  :to-equal
                  (format nil
                          "| A | C |~%|---+---|~%| x | z |~%#+TBLFM: $1=1::$2=3::~%"))
          (expect (insert-first-column suffix-source)
                  :to-equal
                  (concatenate 'string column-expected
                               (format nil
                                       "#+TBLFM: $2=1;%.2f::$3=2~%")))
          (expect (insert-first-column suffix-terminal-source)
                  :to-equal
                  (concatenate 'string column-expected
                               (format nil "#+TBLFM: $2=1;%.2f~%")))
          (expect (insert-first-column spaced-source)
                  :to-equal
                  (concatenate 'string column-expected
                               (format nil
                                       "#+TBLFM:   $2=1   ::   $3=2   ~%")))
          (let* ((document (parse-org crlf-source))
                 (table
                   (table-edit-test-find-node (document-root document) :table))
                 (result (table-insert-row document table 2)))
            (expect (document-text (edit-result-updated-document result))
                    :to-equal crlf-expected)
            (expect (document-text document) :to-equal crlf-source))
          (expect (document-text empty-document) :to-equal empty-source)))))

  (it "preserves TBLFM assignment whitespace when deleting a column"
    (let* ((source
             (format nil
                     "| A | B |~%| x | y |~%#+TBLFM:   $1=1   ::   $2=2   ~%"))
           (last-expected
             (format nil
                     "| A |~%| x |~%#+TBLFM:   $1=1   ::   ~%"))
           (last-document (parse-org source))
           (last-table
             (table-edit-test-find-node (document-root last-document) :table))
           (last-result (table-delete-column last-document last-table 2))
           (first-expected
             (format nil
                     "| B |~%| y |~%#+TBLFM:      $1=2   ~%"))
           (first-document (parse-org source))
           (first-table
             (table-edit-test-find-node (document-root first-document) :table))
           (first-result (table-delete-column first-document first-table 1)))
      (with-soft-assertions
        (expect (edit-result-p last-result) :to-be t)
        (expect (document-text (edit-result-updated-document last-result))
                :to-equal last-expected)
        (expect (edit-result-p first-result) :to-be t)
        (expect (document-text (edit-result-updated-document first-result))
                :to-equal first-expected)
        (expect (document-text last-document) :to-equal source)
        (expect (document-text first-document) :to-equal source))))

  (it "remaps absolute references across decimal insertion and deletion boundaries"
    (let* ((increment-source
             (format nil "| A |~%| x |~%#+TBLFM: @2$1=@2$1~%"))
           (increment-document (parse-org increment-source))
           (increment-table
             (table-edit-test-find-node
              (document-root increment-document) :table))
           (increment-result
             (table-insert-row increment-document increment-table 2))
           (index-ten-source
             (with-output-to-string (stream)
               (loop for index from 1 to 10
                     do (format stream "| x~D |~%" index))
               (format stream "#+TBLFM: @9$1=@9$1~%")))
           (index-ten-expected
             (with-output-to-string (stream)
               (loop for index from 1 to 9
                     do (format stream "| x~D  |~%" index))
               (format stream "|     |~%| x10 |~%#+TBLFM: @9$1=@9$1~%")))
           (index-ten-document (parse-org index-ten-source))
           (index-ten-table
             (table-edit-test-find-node
              (document-root index-ten-document) :table))
           (index-ten-result
             (table-insert-row index-ten-document index-ten-table 10))
           (delete-source
             (with-output-to-string (stream)
               (format stream "| ~{~A~^ | ~} |~%"
                       (loop for index from 1 to 10
                             collect (format nil "c~2,'0D" index)))
               (format stream "| ~{~A~^ | ~} |~%"
                       (loop for index from 1 to 10
                             collect (format nil "v~2,'0D" index)))
               (format stream "#+TBLFM: $10=$10~%")))
           (delete-expected
             (with-output-to-string (stream)
               (format stream "| ~{~A~^ | ~} |~%"
                       (loop for index from 2 to 10
                             collect (format nil "c~2,'0D" index)))
               (format stream "| ~{~A~^ | ~} |~%"
                       (loop for index from 2 to 10
                             collect (format nil "v~2,'0D" index)))
               (format stream "#+TBLFM: $9=$9~%")))
           (delete-document (parse-org delete-source))
           (delete-table
             (table-edit-test-find-node
              (document-root delete-document) :table))
           (delete-result (table-delete-column delete-document delete-table 1))
           (zero-source
             (format nil "| x |~%| y |~%#+TBLFM: @000$1=@000$1~%"))
           (zero-document (parse-org zero-source))
           (zero-table
             (table-edit-test-find-node (document-root zero-document) :table))
           (zero-result (table-delete-row zero-document zero-table 1))
           (large-digits
             (concatenate 'string "1" (make-string 39 :initial-element #\0)))
           (large-incremented-digits
             (concatenate 'string "1" (make-string 38 :initial-element #\0)
                          "1"))
           (large-ones (make-string 39 :initial-element #\9))
           (large-increment-source
             (format nil "| A |~%| x |~%#+TBLFM: @~A$1=@~A$1~%"
                     large-digits large-digits))
           (large-increment-document (parse-org large-increment-source))
           (large-increment-table
             (table-edit-test-find-node
              (document-root large-increment-document) :table))
           (large-increment-result
             (table-insert-row large-increment-document large-increment-table 1))
           (large-delete-source
             (format nil "| A | B |~%| x | y |~%#+TBLFM: $~A=$~A~%"
                     large-digits large-digits))
           (large-delete-document (parse-org large-delete-source))
           (large-delete-table
             (table-edit-test-find-node
              (document-root large-delete-document) :table))
           (large-delete-result
             (table-delete-column large-delete-document large-delete-table 1)))
      (with-soft-assertions
        (expect (document-text
                 (edit-result-updated-document increment-result))
                :to-equal
                (format nil "| A |~%|   |~%| x |~%#+TBLFM: @3$1=@3$1~%"))
        (expect (document-text
                 (edit-result-updated-document index-ten-result))
                :to-equal index-ten-expected)
        (expect (document-text
                 (edit-result-updated-document delete-result))
                :to-equal delete-expected)
        (expect (document-text
                 (edit-result-updated-document zero-result))
                :to-equal
                (format nil "| y |~%#+TBLFM: @000$1=@000$1~%"))
        (expect (document-text
                 (edit-result-updated-document large-increment-result))
                :to-equal
                (format nil "|   |~%| A |~%| x |~%#+TBLFM: @~A$1=@~A$1~%"
                        large-incremented-digits large-incremented-digits))
        (expect (document-text
                 (edit-result-updated-document large-delete-result))
                :to-equal
                (format nil "| B |~%| y |~%#+TBLFM: $~A=$~A~%"
                        large-ones large-ones))
        (expect (document-text index-ten-document) :to-equal index-ten-source)
        (expect (document-text delete-document) :to-equal delete-source)
        (expect (document-text zero-document) :to-equal zero-source)
        (expect (document-text large-increment-document)
                :to-equal large-increment-source)
        (expect (document-text large-delete-document)
                :to-equal large-delete-source))))

  (it "increments all-nine absolute references when inserting a column"
    (let* ((source (format nil "| A |~%| x |~%#+TBLFM: $9=$9~%"))
           (expected
             (format nil "|   | A |~%|   | x |~%#+TBLFM: $10=$10~%"))
           (document (parse-org source))
           (table (table-edit-test-find-node
                   (document-root document) :table))
           (result (table-insert-column document table 1)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result))
                :to-equal expected)
        (expect (document-text document) :to-equal source))))

  (it "renders empty table columns and honors alignment and width cookies"
    (let* ((hline-source
             (format nil "| A |~%|---|~%| x |~%#+TBLFM: $1=1~%"))
           (hline-document (parse-org hline-source))
           (hline-table
             (table-edit-test-find-node (document-root hline-document) :table))
           (hline-result (table-delete-column hline-document hline-table 1))
           (sibling-source (format nil "| only |~%After~%"))
           (sibling-document (parse-org sibling-source))
           (sibling-table
             (table-edit-test-find-node
              (document-root sibling-document) :table))
           (sibling-result
             (table-delete-row sibling-document sibling-table 1))
           (alignment-source (format nil "| <r> | <c> |~%| x | y |~%"))
           (alignment-document (parse-org alignment-source))
           (alignment-table
             (table-edit-test-find-node
              (document-root alignment-document) :table))
           (alignment-result
             (table-insert-row alignment-document alignment-table 2))
           (width-source (format nil "| <10> |~%| x |~%"))
           (width-document (parse-org width-source))
           (width-table
             (table-edit-test-find-node (document-root width-document) :table))
           (width-result (table-insert-row width-document width-table 2)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document hline-result))
                :to-equal (format nil "||~%||~%||~%#+TBLFM: ~%"))
        (expect (document-text (edit-result-updated-document sibling-result))
                :to-equal (format nil "After~%"))
        (expect (document-text
                 (edit-result-updated-document alignment-result))
                :to-equal
                (format nil
                        "| <r> | <c> |~%|     |     |~%|   x |  y  |~%"))
        (expect (document-text (edit-result-updated-document width-result))
                :to-equal (format nil "| <10> |~%|      |~%| x    |~%"))
        (expect (document-text hline-document) :to-equal hline-source)
        (expect (document-text sibling-document) :to-equal sibling-source)
        (expect (document-text alignment-document) :to-equal alignment-source)
        (expect (document-text width-document) :to-equal width-source))))

  (it "accounts for generic UTF-16 BOMs when sizing edited table output"
    (let* ((source (format nil "| A |~%| x |~%"))
           (expected (format nil "| A |~%|   |~%| x |~%"))
           (input-octets
             (cl-codec-kit:string-to-octets source :encoding :utf-16))
           (expected-octets
             (cl-codec-kit:string-to-octets expected :encoding :utf-16))
           (document (parse-org input-octets :encoding :utf-16))
           (table (table-edit-test-find-node (document-root document) :table))
           (result (table-insert-row document table 2))
           (updated (edit-result-updated-document result)))
      (with-soft-assertions
        (expect (document-bom-p document) :to-be t)
        (expect (document-encoding document) :to-be :utf-16)
        (expect (document-text updated) :to-equal expected)
        (expect (write-org updated :output-type :octets)
                :to-equalp expected-octets)
        (expect (document-text document) :to-equal source))))
)

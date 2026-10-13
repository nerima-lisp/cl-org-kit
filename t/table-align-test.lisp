;;;; t/table-align-test.lisp
(in-package #:cl-org-kit/test)

(defun table-align-test-find-node (root type)
  (or (when (eq (node-type root) type) root)
      (some (lambda (child) (table-align-test-find-node child type))
            (node-children root))))

(defun table-align-test-fixture (relative-path)
  (uiop:read-file-string
   (asdf:system-relative-pathname
    "cl-org-kit" (concatenate 'string "t/fixtures/table/" relative-path))))

(defun table-align-test-condition (thunk)
  (handler-case
      (progn (funcall thunk) '(:no-error))
    (org-edit-error (condition)
      (list :edit
            (org-edit-error-reason condition)
            (org-edit-error-target condition)))
    (org-table-error (condition)
      (list :table
            (org-kit-error-message condition)
            (org-table-error-reason condition)
            (org-table-error-target condition)
            (princ-to-string condition)))))

(defun table-align-test-utf8-bom-octets (text)
  (let* ((body (cl-codec-kit:string-to-octets text :encoding :utf-8))
         (octets (make-array (+ 3 (length body))
                            :element-type '(unsigned-byte 8))))
    (replace octets #(239 187 191))
    (replace octets body :start1 3)
    octets))

(describe "table width and alignment"
  (it "aligns code-point width cases through the public table API"
    (let* ((source (table-align-test-fixture
                    "policy/align/code-point-widths.org"))
           (expected (table-align-test-fixture
                      "policy/expected/align/code-point-widths.org"))
           (document (parse-org source))
           (table (table-align-test-find-node (document-root document) :table))
           (updated (edit-result-updated-document (table-align document table))))
      (with-soft-assertions
        (expect (document-text updated) :to-equal expected)
        (expect (document-text document) :to-equal source))))

  (it "matches the checked-in Org align fixtures where the policies agree"
    (dolist (fixture '("corpus/align/ascii.org"
                       "corpus/align/alignment-cookie.org"
                       "corpus/align/ambiguous.org"
                       "corpus/align/cookies.org"
                       "corpus/align/single-emoji.org"
                       "corpus/align/wide-combining.org"))
      (let* ((source (table-align-test-fixture fixture))
             (expected
               (table-align-test-fixture
                (concatenate 'string
                             "expected/align/"
                             (subseq fixture (length "corpus/align/")))))
             (document (parse-org source))
             (table (table-align-test-find-node (document-root document) :table))
             (result (table-align document (list table))))
        (with-soft-assertions
          (expect (edit-result-p result) :to-be t)
          (expect (document-text (edit-result-updated-document result))
                  :to-equal expected)
          (expect (document-text document) :to-equal source)))))

  (it "uses left alignment by default while filling ragged rows across hlines"
    (let* ((source (table-align-test-fixture "corpus/align/ragged-hlines.org"))
           (document (parse-org source))
           (table (table-align-test-find-node (document-root document) :table))
           (table-start (node-begin table))
           (table-end (node-end table))
           (result (table-align document table))
           (updated (edit-result-updated-document result))
           (expected-table
             (format nil
                     "  | key | value |   |~%  |-----+-------+---|~%  | a   | 1     |   |~%  |-----+-------+---|~%  | bb  |       |   |~%  |-----+-------+---|~%  | ccc | 22    | z |~%"))
           (expected
             (concatenate 'string
                          (subseq source 0 table-start)
                          expected-table
                          (subseq source table-end))))
      (with-soft-assertions
        (expect (document-text updated) :to-equal expected)
        (expect (document-text document) :to-equal source)
        (expect (count :rule (node-children
                              (table-align-test-find-node
                               (document-root updated) :table))
                       :key (lambda (row)
                              (node-property row :type)))
                :to-be 3)
        (expect (every (lambda (span)
                         (and (<= table-start (edit-span-start span))
                              (<= (edit-span-end span) table-end)))
                       (edit-result-spans result))
                :to-be t)
        (expect (some (lambda (span)
                        (and (= (edit-span-start span) table-start)
                             (= (edit-span-end span) table-end)))
                      (edit-result-spans result))
                :to-be nil))))

  (it "aligns emoji sequences by summing their code point widths"
    (let* ((source (table-align-test-fixture
                    "policy/align/emoji-sequence-sum.org"))
           (expected (table-align-test-fixture
                      "policy/expected/align/emoji-sequence-sum.org"))
           (document (parse-org source))
           (table (table-align-test-find-node (document-root document) :table))
           (result (table-align document table)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result))
                :to-equal expected)
        (expect (document-text document) :to-equal source))))

  (it "treats East Asian Ambiguous characters as narrow by kit policy"
    (let* ((source (table-align-test-fixture
                    "policy/align/ambiguous-narrow.org"))
           (expected (table-align-test-fixture
                      "policy/expected/align/ambiguous-narrow.org"))
           (document (parse-org source))
           (table (table-align-test-find-node (document-root document) :table))
           (updated (edit-result-updated-document (table-align document table))))
      (with-soft-assertions
        (expect (document-text updated) :to-equal expected)
        (expect (document-text document) :to-equal source))))

  (it "collapses excess hline segments to the logical table width"
    (let* ((source (table-align-test-fixture
                    "regression/align/extra-hline-segments.org"))
           (expected (table-align-test-fixture
                      "regression/expected/align/extra-hline-segments.org"))
           (document (parse-org source))
           (table (table-align-test-find-node (document-root document) :table))
           (updated (edit-result-updated-document (table-align document table))))
      (with-soft-assertions
        (expect (document-text updated) :to-equal expected)
        (expect (document-text document) :to-equal source))))

  (it "lets an emoji sequence determine the aligned column width"
    (let* ((source (format nil "| x |~%| 👩‍💻 |~%"))
           (expected (format nil "| x     |~%| 👩‍💻 |~%"))
           (document (parse-org source))
           (table (table-align-test-find-node (document-root document) :table))
           (updated (edit-result-updated-document (table-align document table))))
      (with-soft-assertions
        (expect (document-text updated) :to-equal expected)
        (expect (document-text document) :to-equal source))))

  (it "keeps empty columns valid with at least three dashes per hline segment"
    (let* ((source (format nil "|   |   |~%|--+--|~%|   |   |~%"))
           (expected (format nil "|   |   |~%|---+---|~%|   |   |~%"))
           (document (parse-org source))
           (table (table-align-test-find-node (document-root document) :table))
           (updated (edit-result-updated-document (table-align document table))))
      (with-soft-assertions
        (expect (document-text updated) :to-equal expected)
        (expect (document-text document) :to-equal source))))

  (it "honors alignment and width cookies without truncating cell values"
    (let* ((source
             (format nil
                     "| <c> | <r> | <l> | <10> | <3> |~%"
                     ))
           (source (concatenate
                    'string source
                    (format nil "|---+---+---+----+---|~%| x | q | z | tiny | longer |~%")))
           (document (parse-org source))
           (table (table-align-test-find-node (document-root document) :table))
           (edited (edit-result-updated-document (table-align document table)))
           (expected
             (concatenate
              'string
              (format nil "| <c> | <r> | <l> | <10>~A| <3>~A|~%"
                      (make-string 7 :initial-element #\Space)
                      (make-string 4 :initial-element #\Space))
              (format nil "|-----+-----+-----+------------+--------|~%")
              (format nil "|  x  |   q | z   | tiny~A| longer |~%"
                      (make-string 7 :initial-element #\Space)))))
      (with-soft-assertions
        (expect (document-text edited) :to-equal expected)
        (expect (search "longer" (document-text edited)) :not :to-be nil)
        (expect (search "tiny" (document-text edited)) :not :to-be nil)
        (expect (document-text document) :to-equal source))))

  (it "preserves source outside the table and reversible UTF-8 bytes"
    (let* ((crlf (concatenate 'string (string #\Return) (string #\Newline)))
           (source (concatenate 'string
                                "前" crlf
                                "| a | 日本 |" crlf
                                "|---+----|" crlf
                                "| x | y |" crlf
                                "後" crlf))
           (document (parse-org (table-align-test-utf8-bom-octets source)
                                :encoding :utf-8))
           (table (table-align-test-find-node (document-root document) :table))
           (edited (edit-result-updated-document (table-align document table)))
           (expected (concatenate 'string
                                  "前" crlf
                                  "| a | 日本 |" crlf
                                  "|---+------|" crlf
                                  "| x | y    |" crlf
                                  "後" crlf)))
      (with-soft-assertions
        (expect (write-org edited :output-type :octets)
                :to-equalp (table-align-test-utf8-bom-octets expected))
        (expect (document-text document) :to-equal source))))

  (it "rejects unsupported table types and invalid table targets in their condition slots"
    (let* ((pipe-document
             (parse-org (format nil "#+TBLFM: $1=$1~%~%| a |~%")))
           (pipe-table (table-align-test-find-node
                        (document-root pipe-document) :table))
           (table-el-document
             (parse-org (format nil "+---+~%| x |~%+---+~%")))
           (table-el (table-align-test-find-node
                      (document-root table-el-document) :table))
           (orphan (table-align-test-find-node
                    (document-root pipe-document) :keyword))
           (foreign-document (parse-org (format nil "| other |~%")))
           (foreign-table (table-align-test-find-node
                           (document-root foreign-document) :table)))
      (with-soft-assertions
        (expect (table-align-test-condition
                 (lambda () (table-align table-el-document table-el)))
                :to-equal
                '(:table "Only Org pipe tables can be aligned"
                  :unsupported-table-type "table"
                  "Only Org pipe tables can be aligned (unsupported-table-type) for target table"))
        (expect (table-align-test-condition
                 (lambda () (table-align pipe-document orphan)))
                :to-equal '(:edit :wrong-node-type "keyword"))
        (let ((condition
                (table-align-test-condition
                 (lambda ()
                   (table-align pipe-document (list pipe-table pipe-table))))))
          (expect (first condition) :to-be :edit)
          (expect (second condition) :to-be :ambiguous-target)
          (expect (stringp (third condition)) :to-be t)
          (expect (<= (length (third condition)) 80) :to-be t))
        (expect (table-align-test-condition
                 (lambda () (table-align pipe-document foreign-table)))
                :to-equal '(:edit :foreign-node "table"))
        (expect (table-align-test-condition
                 (lambda () (table-align pipe-document "table/path")))
                :to-equal '(:edit :target-not-found "table/path"))
        (expect (table-align-test-condition
                 (lambda () (table-align pipe-document '())))
                :to-equal '(:edit :target-not-found "target"))
        (expect (document-text pipe-document)
                :to-equal (format nil "#+TBLFM: $1=$1~%~%| a |~%")))))

  (it "rejects tables without standard rows"
    (let* ((source (format nil "|---|~%"))
           (document (parse-org source))
           (table (table-align-test-find-node (document-root document) :table)))
      (with-soft-assertions
        (expect (table-align-test-condition
                 (lambda () (table-align document table)))
                :to-equal '(:edit :uneditable-structure "table"))
        (expect (document-text document) :to-equal source))))

  (it "reports an output limit failure without changing the input document"
    (let* ((source (format nil "| a | long |~%|---+----|~%| x | y |~%"))
           (document (parse-org source :limits (make-limits :max-output-length 5)))
           (table (table-align-test-find-node (document-root document) :table))
           (failure
             (handler-case
                 (progn (table-align document table) nil)
               (org-resource-limit-error (condition)
                 (list (org-resource-limit-error-limit-name condition)
                       (org-resource-limit-error-limit condition)
                       (org-resource-limit-error-actual condition))))))
      (with-soft-assertions
        (expect (first failure) :to-be :max-output-length)
        (expect (second failure) :to-be 5)
        (expect (> (third failure) 5) :to-be t)
        (expect (document-text document) :to-equal source))))

  (it "succeeds when the output length is exactly at its configured limit"
    (let* ((source (format nil "| a |~%"))
           (document
             (parse-org source
                        :limits (make-limits :max-output-length (length source))))
           (table (table-align-test-find-node (document-root document) :table))
           (result (table-align document table)))
      (with-soft-assertions
        (expect (edit-result-p result) :to-be t)
        (expect (document-text (edit-result-updated-document result))
                :to-equal source)
        (expect (document-text document) :to-equal source))))

  (it "checks max-output-octets and max-input-length immediately below, at, and above the output size"
    (let ((source (format nil "|a|~%"))
          (expected (format nil "| a |~%")))
      (flet ((run-with-limit (name value)
               (let* ((other-limit
                        (case name
                          (:max-output-octets :max-input-length)
                          (:max-input-length :max-output-octets)))
                      (limits
                        (apply #'make-limits
                               (list name value other-limit 100)))
                      (document (parse-org source :limits limits))
                      (table (table-align-test-find-node
                              (document-root document) :table)))
                 (handler-case
                     (list :ok
                           (document-text
                            (edit-result-updated-document
                             (table-align document table))))
                   (org-resource-limit-error (condition)
                     (list :limit
                           (org-resource-limit-error-limit-name condition)
                           (org-resource-limit-error-limit condition)
                           (org-resource-limit-error-actual condition)))))))
        (with-soft-assertions
          (dolist (name '(:max-output-octets :max-input-length))
            (expect (run-with-limit name 5)
                    :to-equal (list :limit name 5 6))
            (expect (run-with-limit name 6)
                    :to-equal (list :ok expected))
            (expect (run-with-limit name 7)
                    :to-equal (list :ok expected)))))))

  (it "rejects enormous width cookies before allocating padded cells"
    (let* ((source
             (format nil
                     "| <9999999999999999999999999999999999999999> |~%|---|~%| x |~%"))
           (limit 256)
           (document
             (parse-org source :limits (make-limits :max-output-length limit)))
           (table (table-align-test-find-node (document-root document) :table))
           (failure
             (handler-case
                 (progn (table-align document table) nil)
               (org-resource-limit-error (condition)
                 (list (org-resource-limit-error-limit-name condition)
                       (org-resource-limit-error-limit condition)
                       (org-resource-limit-error-actual condition))))))
      (with-soft-assertions
        (expect (first failure) :to-be :max-output-length)
        (expect (second failure) :to-be limit)
        (expect (> (third failure) limit) :to-be t)
        (expect (document-text document) :to-equal source)))))

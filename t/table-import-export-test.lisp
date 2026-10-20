(in-package #:cl-org-kit/test)

(defun table-import-export-test-find-table (node)
  (or (when (eq (node-type node) :table) node)
      (some #'table-import-export-test-find-table (node-children node))))

(defun table-import-export-test-error (thunk)
  (handler-case
      (progn (funcall thunk) :no-error)
    (org-table-error (condition)
      (list (class-name (class-of condition))
            (org-table-error-reason condition)))
    (org-resource-limit-error (condition)
      (list (class-name (class-of condition))
            (org-resource-limit-error-limit-name condition)))))

(describe "table import and export"
  (it "imports CSV with Org 9.8.10 alignment and preserves the input document"
    (let* ((source (format nil "name,note~%Ada,\"hello, world\"~%Bob,\"say \"\"hi\"\"\"~%"))
           (document (parse-org ""))
           (result (table-import document 0 source :format :auto))
           (updated (edit-result-updated-document result)))
      (with-soft-assertions
        (expect (edit-result-p result) :to-be t)
        (expect (document-text updated)
                :to-equal
                (format nil "| name | note         |~%| Ada  | hello, world |~%| Bob  | say \"hi\"     |~%"))
        (expect (document-text document) :to-equal ""))))

  (it "imports TSV with explicit and automatic detection and reads CRLF records"
    (let* ((source (format nil "name~Cvalue~%Ada~Cshort~%Robert~Clong value~%"
                           #\Tab #\Tab #\Tab))
           (document (parse-org ""))
           (explicit (table-import document 0 source :format :tsv))
           (automatic (table-import document 0 source :format :auto))
           (crlf-source (format nil "name,note~C~CAda,short~C~C"
                                #\Return #\Newline #\Return #\Newline))
           (crlf-result (table-import (parse-org "") 0 crlf-source :format :csv))
           (crlf-document (edit-result-updated-document crlf-result))
           (crlf-table
             (table-import-export-test-find-table (document-root crlf-document)))
           (empty-result (table-import (parse-org "") 0 ""))
           (empty-document (edit-result-updated-document empty-result))
           (empty-table
             (table-import-export-test-find-table (document-root empty-document))))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document automatic))
                :to-equal (document-text (edit-result-updated-document explicit)))
        (expect (table-export crlf-document crlf-table :format :csv)
                :to-equal (format nil "name,note~%Ada,short~%"))
        (expect (table-export empty-document empty-table :format :csv)
                :to-equal (string #\Newline)))))

  (it "exports CSV and TSV exactly as Org 9.8.10, including hline and empty-table behavior"
    (let* ((source
             (format nil
                     "| name | note |~%|------+------|~%| Ada  | hello, world |~%| Bob  | say \"hi\" |~%"))
           (document (parse-org source))
           (table (table-import-export-test-find-table (document-root document)))
           (empty-document (parse-org ""))
           (empty-table
             (append-child
              (document-root empty-document)
              (make-node :table :properties (list :type :org)))))
      (with-soft-assertions
        (expect (table-export document table :format :csv)
                :to-equal
                (format nil "name,note~%Ada,\"hello, world\"~%Bob,\"say \"\"hi\"\"\"~%"))
        (expect (table-export document table :format :tsv)
                :to-equal
                (format nil "name~Cnote~%Ada~Chello, world~%Bob~Csay \"hi\"~%"
                        #\Tab #\Tab #\Tab))
        (expect (table-export empty-document empty-table :format :csv)
                :to-equal ""))))

  (it "quotes CSV fields only when Org does and doubles embedded quotes"
    (let* ((document (parse-org (format nil
                                        "| value |~%| plain |~%| a,b |~%| say \"hi\" |~%")))
           (table (table-import-export-test-find-table (document-root document))))
      (expect (table-export document table :format :csv)
              :to-equal
              (format nil "value~%plain~%\"a,b\"~%\"say \"\"hi\"\"\"~%"))))

  (it "round-trips both formats for tables without horizontal rules"
    (let* ((source (format nil "| A | 1 |~%| B | 2 |~%"))
           (document (parse-org source))
           (table (table-import-export-test-find-table (document-root document))))
      (dolist (format '(:csv :tsv))
        (let* ((exported (table-export document table :format format))
               (imported (table-import (parse-org "") 0 exported :format format))
               (updated (edit-result-updated-document imported))
               (updated-table
                 (table-import-export-test-find-table (document-root updated))))
          (expect (table-export updated updated-table :format format)
                  :to-equal exported)))))

  (it "signals named errors for invalid formats and malformed CSV quoting"
    (let* ((document (parse-org ""))
           (export-document (parse-org (format nil "| value |~%")))
           (export-table
             (table-import-export-test-find-table (document-root export-document))))
      (with-soft-assertions
        (expect (table-import-export-test-error
                 (lambda () (table-import document 0 "a,b" :format :bad)))
                :to-equal (list (quote org-table-error) :invalid-format))
        (expect (table-import-export-test-error
                 (lambda () (table-import document 0 "\"open" :format :csv)))
                :to-equal (list (quote org-table-error) :invalid-quoting))
        (expect (table-import-export-test-error
                 (lambda () (table-import document 0 "\"closed\"x" :format :csv)))
                :to-equal (list (quote org-table-error) :invalid-quoting))
        (expect (table-import-export-test-error
                 (lambda () (table-import document 0 "a\"b,c" :format :csv)))
                :to-equal (list (quote org-table-error) :invalid-quoting))
        (expect (table-import-export-test-error
                 (lambda ()
                   (table-import document 0
                                 (format nil "a,\"multi~Cline\"" #\Newline)
                                 :format :csv)))
                :to-equal (list (quote org-table-error) :multiline-field))
        (signals type-error (table-import document 0 42))
        (expect (table-import-export-test-error
                 (lambda () (table-export export-document export-table :format :bad)))
                :to-equal (list (quote org-table-error) :invalid-format)))))

  (it "inserts at boundaries and preserves surrounding line endings"
    (flet ((imported-text (source offset)
             (document-text
              (edit-result-updated-document
               (table-import (parse-org source) offset
                             (format nil "A~CB~%" #\Tab) :format :tsv)))))
      (let* ((crlf (format nil "~C~C" #\Return #\Newline))
             (previous-source
               (format nil "before~C~Cafter~C~C"
                       #\Return #\Newline #\Return #\Newline))
             (following-crlf-source (format nil "after~C~C" #\Return #\Newline))
             (following-lf-source (format nil "after~%"))
             (fallback-source "before")
             (split-source (format nil "before~C~Cafter" #\Return #\Newline))
             (split-offset (1+ (length "before"))))
        (with-soft-assertions
          (let* ((suffix (format nil "AFTER~%"))
                 (source (concatenate (quote string) (format nil "before~%") suffix))
                 (document (parse-org source))
                 (offset (length (format nil "before~%")))
                 (result (table-import document offset (format nil "A~CB~%" #\Tab)
                                       :format :tsv))
                 (updated (document-text (edit-result-updated-document result))))
            (expect updated :to-equal (format nil "before~%| A | B |~%AFTER~%"))
            (expect (subseq updated 0 offset) :to-equal (subseq source 0 offset))
            (expect (subseq updated (- (length updated) (length suffix)))
                    :to-equal suffix)
            (expect (mapcar (lambda (span)
                              (list (edit-span-start span) (edit-span-end span)))
                            (edit-result-spans result))
                    :to-equal (list (list offset offset)))
            (expect (document-text document) :to-equal source))
          (expect (imported-text previous-source (+ (length "before") (length crlf)))
                  :to-equal
                  (format nil "before~C~C| A | B |~C~Cafter~C~C"
                          #\Return #\Newline #\Return #\Newline
                          #\Return #\Newline))
          (expect (imported-text following-crlf-source 0)
                  :to-equal
                  (format nil "| A | B |~C~Cafter~C~C"
                          #\Return #\Newline #\Return #\Newline))
          (expect (imported-text following-lf-source 0)
                  :to-equal (format nil "| A | B |~%after~%"))
          (expect (imported-text fallback-source (length fallback-source))
                  :to-equal (format nil "before~%| A | B |~%"))
          (let ((updated (imported-text split-source split-offset)))
            (expect (subseq updated 0 split-offset)
                    :to-equal (subseq split-source 0 split-offset))
            (expect (subseq updated (- (length updated)
                                       (- (length split-source) split-offset)))
                    :to-equal (subseq split-source split-offset))
            (expect (not (null (search "| A | B |" updated))) :to-be t))))))

  (it "rejects opaque table.el nodes"
    (let* ((source (format nil "+---+---+~%| 1 | 2 |~%+---+---+~%"))
           (document (parse-org source))
           (table (table-import-export-test-find-table (document-root document))))
      (expect (table-import-export-test-error
               (lambda () (table-export document table)))
              :to-equal (list 'org-table-error :unsupported-table-type))))

  (it "enforces import input, generated node, and document output limits"
    (let* ((input-document
             (parse-org "" :limits (make-limits :max-input-length 2)))
           (node-document
             (parse-org "" :limits (make-limits :max-node-count 4)))
           (output-document
             (parse-org "" :limits (make-limits :max-output-length 5)))
           (octet-document
             (parse-org "" :limits (make-limits :max-output-octets 5))))
      (with-soft-assertions
        (expect (table-import-export-test-error
                 (lambda () (table-import input-document 0 "a,b")))
                :to-equal
                (list 'org-resource-limit-error :max-input-length))
        (expect (table-import-export-test-error
                 (lambda () (table-import node-document 0 "x")))
                :to-equal
                (list 'org-resource-limit-error :max-node-count))
        (expect (table-import-export-test-error
                 (lambda () (table-import output-document 0 "x")))
                :to-equal
                (list 'org-resource-limit-error :max-output-length))
        (expect (table-import-export-test-error
                 (lambda () (table-import octet-document 0 "x")))
                :to-equal
                (list 'org-resource-limit-error :max-output-octets)))))

  (it "enforces output limits for returned delimited text"
    (let* ((document
             (parse-org (format nil "| é |~%")
                        :limits (make-limits :max-output-octets 2)))
           (table (table-import-export-test-find-table (document-root document))))
      (expect (table-import-export-test-error
               (lambda () (table-export document table :format :tsv)))
              :to-equal
              (list 'org-resource-limit-error :max-output-octets)))))

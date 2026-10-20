(in-package #:cl-org-kit/test)

(defun table-transpose-test-find-table (root)
  (or (when (eq (node-type root) :table) root)
      (some #'table-transpose-test-find-table (node-children root))))

(defun table-transpose-test-condition (thunk)
  (handler-case
      (progn (funcall thunk) :no-error)
    (org-table-error (condition)
      (list :table (org-table-error-reason condition)))
    (org-resource-limit-error (condition)
      (list :limit (org-resource-limit-error-limit-name condition)))))

(describe "Org table transposition"
  (it "transposes cells, removes hlines, and keeps formulas unchanged"
    (let* ((source (format nil
                           "| a | b |~%|---+---|~%| c | d |~%#+TBLFM: $2=$1~%"))
           (expected (format nil
                             "| a | c |~%| b | d |~%#+TBLFM: $2=$1~%"))
           (document (parse-org source))
           (table (table-transpose-test-find-table (document-root document)))
           (formula (first (node-property table :tblfm)))
           (result (table-transpose document table))
           (updated (edit-result-updated-document result)))
      (with-soft-assertions
        (expect (edit-result-p result) :to-be t)
        (expect (document-text updated) :to-equal expected)
        (expect (document-text document) :to-equal source)
        (expect formula :to-equal "$2=$1")
        (expect (node-property
                 (table-transpose-test-find-table (document-root updated)) :tblfm)
                :to-equal (list formula)))))

  (it "preserves text outside the table and reports the original table span"
    (let* ((crlf (concatenate 'string (string #\Return) (string #\Newline)))
           (source (concatenate 'string
                                "before" crlf
                                "  | x | longer |" crlf
                                "  | q | z |" crlf
                                "after" crlf))
           (expected (concatenate 'string
                                  "before" crlf
                                  "  | x      | q |" crlf
                                  "  | longer | z |" crlf
                                  "after" crlf))
           (document (parse-org source))
           (table (table-transpose-test-find-table (document-root document)))
           (start (node-begin table))
           (end (node-contents-end table))
           (result (table-transpose document table))
           (updated (edit-result-updated-document result))
           (updated-table
             (table-transpose-test-find-table (document-root updated)))
           (updated-source (document-text updated))
           (updated-start (node-begin updated-table))
           (updated-end (node-contents-end updated-table))
           (span (first (edit-result-spans result))))
      (with-soft-assertions
        (expect updated-source :to-equal expected)
        (expect (subseq source 0 start)
                :to-equal (subseq updated-source 0 updated-start))
        (expect (subseq source end)
                :to-equal (subseq updated-source updated-end))
        (expect (list (edit-span-start span) (edit-span-end span))
                :to-equal (list start end)))))

  (it "rejects ragged tables with a named table error"
    (let* ((document (parse-org (format nil "| a | b |~%| c |~%")))
           (table (table-transpose-test-find-table (document-root document))))
      (expect (table-transpose-test-condition
               (lambda () (table-transpose document table)))
              :to-equal '(:table :ragged-table))))

  (it "rejects table.el tables"
    (let* ((source (format nil "+---+---+~%| one | two |~%+---+---+~%"))
           (document (parse-org source))
           (table (table-transpose-test-find-table (document-root document))))
      (expect (table-transpose-test-condition
               (lambda () (table-transpose document table)))
              :to-equal '(:table :unsupported-table-type))))

  (it "checks the output character limit before returning a result"
    (let* ((source (format nil "|x|longer|~%|y|z|"))
           (limits (make-limits :max-output-length (length source)))
           (document (parse-org source :limits limits))
           (table (table-transpose-test-find-table (document-root document))))
      (expect (table-transpose-test-condition
               (lambda () (table-transpose document table)))
              :to-equal '(:limit :max-output-length))))

  (it "checks the output octet limit before returning a result"
    (let* ((source (format nil "|x|longer|~%|y|z|"))
           (limits (make-limits :max-output-octets (length source)))
           (document (parse-org source :limits limits))
           (table (table-transpose-test-find-table (document-root document))))
      (expect (table-transpose-test-condition
               (lambda () (table-transpose document table)))
              :to-equal '(:limit :max-output-octets)))))

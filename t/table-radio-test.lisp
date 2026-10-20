(in-package #:cl-org-kit/test)

(defun table-radio-test-find-table (node)
  (or (when (eq (node-type node) :table) node)
      (some #'table-radio-test-find-table (node-children node))))

(defun table-radio-test-error (thunk)
  (handler-case
      (progn (funcall thunk) :no-error)
    (org-table-error (condition)
      (list (class-name (class-of condition))
            (org-table-error-reason condition)))
    (org-resource-limit-error (condition)
      (list (class-name (class-of condition))
            (org-resource-limit-error-limit-name condition)))))

(defun table-radio-test-source (transform &optional parameters (value "one"))
  (format nil
          "BEFORE~%#+ORGTBL: SEND radio ~A~A~%| Name | Value |~%|------+-------|~%| Ada  | ~A |~%| Bob  | two   |~%<!-- BEGIN RECEIVE ORGTBL radio -->~%old~%<!-- END RECEIVE ORGTBL radio -->~%AFTER~%"
          transform (if parameters (concatenate 'string " " parameters) "")
          value))

(defun table-radio-test-send (source &optional (limits (make-limits)))
  (let* ((document (parse-org source :limits limits))
         (table (table-radio-test-find-table (document-root document))))
    (list (table-send-radio document table) document)))

(defun table-radio-test-expected-source (source replacement)
  (let* ((old-start (search (format nil "old~%") source))
         (old-end (+ old-start (length (format nil "old~%")))))
    (concatenate 'string (subseq source 0 old-start)
                 replacement (subseq source old-end))))

(describe "radio table send"
  (it "matches Org 9.8.10 for orgtbl, generic, CSV, and TSV"
    (let ((cases
            (list
             (list "orgtbl-to-orgtbl" nil
                   (format nil
                           "| Name | Value |~%|------+-------|~%| Ada  | one   |~%| Bob  | two   |~%"))
             (list "orgtbl-to-generic" ":sep \" / \" :tstart \"BEGIN\" :tend \"END\""
                   (format nil "BEGIN~%Name / Value~%Ada / one~%Bob / twoEND~%"))
             (list "orgtbl-to-csv" nil
                   (format nil "Name,Value~%Ada,one~%Bob,two~%"))
             (list "orgtbl-to-tsv" nil
                   (format nil "Name~CValue~%Ada~Cone~%Bob~Ctwo~%"
                           #\Tab #\Tab #\Tab)))))
      (dolist (case cases)
        (let* ((source (table-radio-test-source (first case) (second case)))
               (replacement (third case))
               (expected (table-radio-test-expected-source source replacement))
               (result (table-radio-test-send source)))
          (expect (document-text
                   (edit-result-updated-document (first result)))
                  :to-equal expected)))))

  (it "matches Org 9.8.10 LaTeX, Texinfo, and HTML output"
    (let ((cases
            (list
             (list "orgtbl-to-latex" nil
                   (format nil
                           "\\begin{tabular}{ll}~%Name & Value\\\\~%\\hline~%Ada & one\\\\~%Bob & two\\\\~%\\end{tabular}~%"))
             (list "orgtbl-to-texinfo" nil
                   (format nil
                           "@multitable {aaaa} {aaaaa}~%@headitem Name~%@tab Value~%@item Ada~%@tab one~%@item Bob~%@tab two~%@end multitable~%"))
             (list "orgtbl-to-html" nil
                   (format nil
                           "<table border=\"2\" cellspacing=\"0\" cellpadding=\"6\" rules=\"groups\" frame=\"hsides\">~%~%~%<colgroup>~%<col  class=\"org-left\" />~%~%<col  class=\"org-left\" />~%</colgroup>~%<thead>~%<tr>~%<th scope=\"col\" class=\"org-left\">Name</th>~%<th scope=\"col\" class=\"org-left\">Value</th>~%</tr>~%</thead>~%<tbody>~%<tr>~%<td class=\"org-left\">Ada</td>~%<td class=\"org-left\">one</td>~%</tr>~%~%<tr>~%<td class=\"org-left\">Bob</td>~%<td class=\"org-left\">two</td>~%</tr>~%</tbody>~%</table>~%"))
             (list "orgtbl-to-latex" nil
                   (format nil
                           "\\begin{tabular}{ll}~%Name & Value\\\\~%\\hline~%Ada & \\# \\$ \\% \\& \\_ \\{ \\} ~A \\^{} < >\\\\~%Bob & two\\\\~%\\end{tabular}~%"
                           "\\textasciitilde{}")
                   "# $ % & _ { } ~ ^ < >")
             (list "orgtbl-to-html" nil
                   (format nil
                           "<table border=\"2\" cellspacing=\"0\" cellpadding=\"6\" rules=\"groups\" frame=\"hsides\">~%~%~%<colgroup>~%<col  class=\"org-left\" />~%~%<col  class=\"org-left\" />~%</colgroup>~%<thead>~%<tr>~%<th scope=\"col\" class=\"org-left\">Name</th>~%<th scope=\"col\" class=\"org-left\">Value</th>~%</tr>~%</thead>~%<tbody>~%<tr>~%<td class=\"org-left\">Ada</td>~%<td class=\"org-left\"># $ % &amp; _ { } ~A ^ &lt; &gt;</td>~%</tr>~%~%<tr>~%<td class=\"org-left\">Bob</td>~%<td class=\"org-left\">two</td>~%</tr>~%</tbody>~%</table>~%"
                           "~")
                   "# $ % & _ { } ~ ^ < >"))))
      (dolist (case cases)
        (let* ((source (table-radio-test-source
                        (first case) (second case) (or (fourth case) "one")))
               (expected (table-radio-test-expected-source source (third case)))
               (result (table-radio-test-send source)))
          (expect (document-text
                   (edit-result-updated-document (first result)))
                  :to-equal expected)))))

  (it "updates every matching arbitrary RECEIVE wrapper and preserves outside source"
    (let* ((source
             (format nil
                     "outside-before~%#+ORGTBL: SEND radio orgtbl-to-csv~%| Name | Value |~%|------+-------|~%| Ada  | one   |~%| Bob  | two   |~%/* BEGIN RECEIVE ORGTBL radio */~%old-one~%/* END RECEIVE ORGTBL radio */~%<!-- BEGIN RECEIVE ORGTBL radio -->~%old-two~%<!-- END RECEIVE ORGTBL radio -->~%outside-after~%"))
           (replacement (format nil "Name,Value~%Ada,one~%Bob,two~%"))
           (expected
             (format nil
                     "outside-before~%#+ORGTBL: SEND radio orgtbl-to-csv~%| Name | Value |~%|------+-------|~%| Ada  | one   |~%| Bob  | two   |~%/* BEGIN RECEIVE ORGTBL radio */~%~A/* END RECEIVE ORGTBL radio */~%<!-- BEGIN RECEIVE ORGTBL radio -->~%~A<!-- END RECEIVE ORGTBL radio -->~%outside-after~%"
                     replacement replacement))
           (result (table-radio-test-send source))
           (updated (document-text
                     (edit-result-updated-document (first result)))))
      (with-soft-assertions
        (expect updated :to-equal expected)
        (expect (document-text (second result)) :to-equal source)
        (expect (length (edit-result-spans (first result))) :to-equal 2))))

  (it "rejects missing directives, receivers, unknown transforms, and unsafe parameters"
    (let* ((missing-send
             (parse-org (format nil "| A |~%| 1 |~%")))
           (unsupported-source
             (table-radio-test-source "orgtbl-to-eval"))
           (unsafe-source
             (table-radio-test-source "orgtbl-to-generic"
                                      ":sep (progn (error \"must not run\"))"))
           (missing-receiver-source
             (format nil
                     "#+ORGTBL: SEND radio orgtbl-to-csv~%| A |~%| 1 |~%"))
           (unsupported-result
             (table-radio-test-error
              (lambda () (table-radio-test-send unsupported-source))))
           (unsafe-result
             (table-radio-test-error
              (lambda () (table-radio-test-send unsafe-source))))
           (missing-receiver-result
             (table-radio-test-error
              (lambda () (table-radio-test-send missing-receiver-source)))))
      (with-soft-assertions
        (expect (table-radio-test-error
                 (lambda ()
                   (table-send-radio
                    missing-send
                    (table-radio-test-find-table
                     (document-root missing-send)))))
                :to-equal (list 'org-table-error :missing-send-directive))
        (expect unsupported-result
                :to-equal (list 'org-table-error :unsupported-format))
        (expect unsafe-result
                :to-equal (list 'org-table-error :invalid-parameters))
        (expect missing-receiver-result
                :to-equal (list 'org-table-error :missing-receive-marker)))))

  (it "enforces encoded and character output limits through edit patches"
    (let* ((source (table-radio-test-source "orgtbl-to-csv"))
           (length-limits (make-limits :max-output-length 4))
           (octet-limits (make-limits :max-output-octets 4)))
      (with-soft-assertions
        (expect (table-radio-test-error
                 (lambda () (table-radio-test-send source length-limits)))
                :to-equal
                (list 'org-resource-limit-error :max-output-length))
        (expect (table-radio-test-error
                 (lambda () (table-radio-test-send source octet-limits)))
                :to-equal
                (list 'org-resource-limit-error :max-output-octets))))))

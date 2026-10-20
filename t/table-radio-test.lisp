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

(defun table-radio-test-source-rows (directives rows)
  (format nil
          "~{~A~%~}~{~A~%~}<!-- BEGIN RECEIVE ORGTBL radio -->~%old~%<!-- END RECEIVE ORGTBL radio -->~%"
          directives rows))

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
                (list 'org-resource-limit-error :max-output-octets)))))

  (it "validates SEND literals and decodes escaped quoted parameters"
    (let ((invalid-parameters
            (list ":sep"
                  ":unknown 1"
                  ":sep \"unterminated"
                  ":sep \"trailing\\"
                  ":sep \"joined\"suffix"
                  ":sep 01"
                  ":sep 1x"
                  ":sep +1"
                  ":backend unknown"
                  ":booktabs t"
                  ":columns \"0.5\"")))
      (dolist (parameters invalid-parameters)
        (expect (table-radio-test-error
                 (lambda ()
                   (table-radio-test-send
                    (table-radio-test-source "orgtbl-to-generic" parameters))))
                :to-equal (list 'org-table-error :invalid-parameters))))
    (dolist (directive
             '("#+ORGTBL: SEND"
               "#+ORGTBL: SEND radio"
               "#+ORGTBL: SEND \"radio name\" csv"))
      (expect (table-radio-test-error
               (lambda ()
                 (table-radio-test-send
                  (format nil "~A~%| A |~%| 1 |~%<!-- BEGIN RECEIVE ORGTBL radio -->~%old~%<!-- END RECEIVE ORGTBL radio -->~%"
                          directive))))
              :to-equal (list 'org-table-error :invalid-send-directive)))
    (let* ((source (table-radio-test-source "orgtbl-to-csv" ":sep \"x\\y\""))
           (expected (table-radio-test-expected-source
                      source
                      (format nil "NamexyValue~%Adaxyone~%Bobxytwo~%")))
           (result (table-radio-test-send source)))
      (expect (document-text
               (edit-result-updated-document (first result)))
              :to-equal expected)))

  (it "applies delimited output options to headers, rules, cells, and skipped rows"
    (let* ((source (table-radio-test-source
                    "orgtbl-to-csv"
                    ":sep \":\" :hsep \"|\" :hline \"---\" :skip 0"
                    "a,b \"c\""))
           (expected (table-radio-test-expected-source
                      source
                      (format nil "Name|Value~%---~%Ada:\"a,b \"\"c\"\"\"~%Bob:two~%")))
           (result (table-radio-test-send source)))
      (expect (document-text
               (edit-result-updated-document (first result)))
              :to-equal expected))
    (let* ((source (table-radio-test-source
                    "orgtbl-to-csv"
                    ":hfmt \"[%s]\" :fmt \"(%s)\""))
           (expected (table-radio-test-expected-source
                      source
                      (format nil "[Name],[Value]~%(Ada),(one)~%(Bob),(two)~%")))
           (result (table-radio-test-send source)))
      (expect (document-text
               (edit-result-updated-document (first result)))
              :to-equal expected))
    (let* ((source (table-radio-test-source-rows
                    '("#+ORGTBL: SEND radio orgtbl-to-tsv :sep \"/\" :skip 99")
                    '("| Name | Value |" "|------+-------|" "| Ada | one |")))
           (expected (table-radio-test-expected-source source (format nil "~%")))
           (result (table-radio-test-send source)))
      (expect (document-text
               (edit-result-updated-document (first result)))
              :to-equal expected))
    (dolist (parameters '(":skip -1" ":skip \"bad\""))
      (expect (table-radio-test-error
               (lambda ()
                 (table-radio-test-send
                  (table-radio-test-source "orgtbl-to-tsv" parameters))))
              :to-equal (list 'org-table-error :invalid-parameters))))

  (it "renders generic line formats, separators, wrappers, and backend dispatch"
    (let* ((source (table-radio-test-source-rows
                    '("#+ORGTBL: SEND radio generic :sep \",\" :hsep \"/\" :hlstart \"H\" :hlend \"h\" :hllstart \"HH\" :hllend \"hh\" :lstart \"<\" :lend \">\" :llstart \"[\" :llend \"]\"")
                    '("| H1 | H2 |" "| H3 | H4 |" "|----+----|" "| A | B |" "| C | D |")))
           (expected (table-radio-test-expected-source
                      source
                      (format nil "HH1/H2h~%HHH3/H4hh~%<A,B>~%[C,D]~%")))
           (result (table-radio-test-send source)))
      (expect (document-text
               (edit-result-updated-document (first result)))
              :to-equal expected))
    (let* ((source (table-radio-test-source-rows
                    '("#+ORGTBL: SEND radio generic :hlfmt \"H(%s,%s)\" :hllfmt \"HH(%s,%s,%s)\" :lfmt \"L(%s,%s)\" :llfmt \"LL(%s,%s)\"")
                    '("| H1 | H2 |" "| H3 | H4 |" "|----+----|" "| A | B |" "| C | D |")))
           (expected (table-radio-test-expected-source
                      source
                      (format nil "H(H1,H2)~%HH(H3,H4,)~%L(A,B)~%LL(C,D)~%")))
           (result (table-radio-test-send source)))
      (expect (document-text
               (edit-result-updated-document (first result)))
              :to-equal expected))
    (let* ((source (table-radio-test-source
                    "orgtbl-to-generic"
                    ":tstart \"BEGIN\" :tend \"END\" :splice t :hfmt \"[%s]\" :fmt \"(%s)\""))
           (expected (table-radio-test-expected-source
                      source
                      (format nil "[Name][Value]~%(Ada)(one)~%(Bob)(two)~%")))
           (result (table-radio-test-send source)))
      (expect (document-text
               (edit-result-updated-document (first result)))
              :to-equal expected))
    (let* ((source (table-radio-test-source-rows
                    '("#+ORGTBL: SEND radio generic :sep \":\"")
                    '("| A | B |" "| one | two |")))
           (expected (table-radio-test-expected-source
                      source (format nil "A:B~%one:two~%")))
           (result (table-radio-test-send source)))
      (expect (document-text
               (edit-result-updated-document (first result)))
              :to-equal expected))
    (dolist (case '(("org" "| Name | Value |")
                    ("html" "<table border=\"2\"")
                    ("latex" "\\begin{tabular}")
                    ("texinfo" "@multitable")))
      (let* ((source (table-radio-test-source
                      "orgtbl-to-generic"
                      (format nil ":backend ~A" (first case))))
             (result (table-radio-test-send source))
             (updated (document-text
                       (edit-result-updated-document (first result)))))
        (expect (not (null (search (second case) updated))) :to-be t))))

  (it "supports LaTeX and Texinfo options and HTML tables without a header"
    (let* ((source (table-radio-test-source
                    "orgtbl-to-latex"
                    ":booktabs t :environment \"longtable\""))
           (expected (table-radio-test-expected-source
                      source
                      (format nil "\\begin{longtable}{ll}~%\\toprule~%Name & Value\\\\~%\\midrule~%Ada & one\\\\~%Bob & two\\\\~%\\bottomrule~%\\end{longtable}~%")))
           (result (table-radio-test-send source)))
      (expect (document-text
               (edit-result-updated-document (first result)))
              :to-equal expected))
    (dolist (columns '("0.5 0.5" "{aaaa} {aaaaa}" "@columnfractions 0.5 0.5"))
      (let* ((source (table-radio-test-source
                      "orgtbl-to-texinfo"
                      (format nil ":columns \"~A\"" columns)))
             (result (table-radio-test-send source))
             (updated (document-text
                       (edit-result-updated-document (first result)))))
        (expect (not (null (search (if (or (search "{" columns)
                                           (search "@columnfractions " columns))
                                       columns
                                       (concatenate 'string "@columnfractions " columns))
                                   updated)))
                :to-be t)))
    (let* ((source (table-radio-test-source-rows
                    '("#+ORGTBL: SEND radio orgtbl-to-html")
                    '("| A | B |" "| one | two |")))
           (result (table-radio-test-send source))
           (updated (document-text
                     (edit-result-updated-document (first result)))))
      (with-soft-assertions
        (expect (not (null (search "<tbody>" updated))) :to-be t)
        (expect (not (null (search "<thead>" updated))) :to-be nil))))

  (it "rejects malformed table values and empty tables for rendered formats"
    (let* ((source (table-radio-test-source "orgtbl-to-csv"))
           (document (parse-org source))
           (table (table-radio-test-find-table (document-root document)))
           (cell (first (node-children (first (node-children table))))))
      (setf (node-property cell :value) nil)
      (expect (table-radio-test-error
               (lambda () (table-send-radio document table)))
              :to-equal (list 'org-table-error :invalid-table)))
    (let* ((source (table-radio-test-source "orgtbl-to-csv"))
           (document (parse-org source))
           (table (table-radio-test-find-table (document-root document))))
      (setf (node-property table :type) :table.el)
      (expect (table-radio-test-error
               (lambda () (table-send-radio document table)))
              :to-equal (list 'org-table-error :unsupported-table-type)))
    (dolist (transform '("orgtbl-to-orgtbl" "orgtbl-to-latex"
                         "orgtbl-to-texinfo" "orgtbl-to-html"))
      (let* ((source (table-radio-test-source transform))
             (document (parse-org source))
             (table (table-radio-test-find-table (document-root document))))
        (setf (node-children table) nil)
        (expect (table-radio-test-error
                 (lambda () (table-send-radio document table)))
                :to-equal (list 'org-table-error :invalid-table))))
    (expect (table-radio-test-error
             (lambda ()
               (table-radio-test-send
                (format nil "#+ORGTBL: SEND radio orgtbl-to-csv~%| A |~%| 1 |~%BEGIN RECEIVE ORGTBL radio~%old~%"))))
            :to-equal (list 'org-table-error :missing-receive-end)))

  (it "handles CRLF and EOF receive markers and rejects overlapping regions"
    (let* ((lf-source (table-radio-test-source "orgtbl-to-csv"))
           (source (with-output-to-string (stream)
                     (loop for character across lf-source
                           do (when (char= character #\Newline)
                                (write-char #\Return stream))
                              (write-char character stream))))
           (result (table-radio-test-send source))
           (updated (document-text
                     (edit-result-updated-document (first result)))))
      (with-soft-assertions
        (expect (not (null (search (format nil "BEFORE~C~%" #\Return) updated)))
                :to-be t)
        (expect (not (null (search (format nil "Name,Value~%Ada,one~%Bob,two~%")
                                   updated)))
                :to-be t)
        (expect (not (null (search (format nil "AFTER~C~%" #\Return) updated)))
                :to-be t)))
    (let* ((source
             (format nil
                     "#+ORGTBL: SEND one orgtbl-to-csv~%#+ORGTBL: SEND two orgtbl-to-tsv~%| A |~%|---|~%| x |~%BEGIN RECEIVE ORGTBL one~%outer~%BEGIN RECEIVE ORGTBL two~%inner~%END RECEIVE ORGTBL two~%after~%END RECEIVE ORGTBL one~%")))
      (expect (table-radio-test-error
               (lambda () (table-radio-test-send source)))
              :to-equal (list 'org-table-error :overlapping-receive-regions)))))

(describe "radio table edge conditions"
  (it "accepts nil scalar and raw options in delimited output"
    (let* ((source (table-radio-test-source "orgtbl-to-tsv" ":sep nil :hsep nil :hline nil :raw t"))
           (expected (table-radio-test-expected-source source (format nil "NameValue~%Adaone~%Bobtwo~%")))
           (result (table-radio-test-send source)))
      (expect (document-text (edit-result-updated-document (first result))) :to-equal expected)))
  (it "pads a ragged Org radio table with an empty cell"
    (let* ((source (table-radio-test-source-rows
                    (list "#+ORGTBL: SEND radio orgtbl-to-orgtbl")
                    (list "| A | B |" "|---+---|" "| x |")))
           (result (table-radio-test-send source))
           (updated (document-text (edit-result-updated-document (first result)))))
      (expect (not (null (search (format nil "| x |   |") updated))) :to-be t)))
  (it "escapes Texinfo control characters in cell values"
    (let* ((source (table-radio-test-source "orgtbl-to-texinfo" nil "@{}"))
           (result (table-radio-test-send source))
           (updated (document-text (edit-result-updated-document (first result)))))
      (expect (not (null (search "@tab @@@{@}" updated))) :to-be t)))
  (it "covers radio rendering failure paths and header line formatting"
      (let* ((source (table-radio-test-source "orgtbl-to-generic"))
             (document (parse-org source))
             (table (table-radio-test-find-table (document-root document)))
             (rows (table-radio-table-rows table)))
        (with-soft-assertions
         (expect (table-radio-parse-send-line "ordinary text" table) :to-be nil)
         (expect (table-radio-test-error
                  (lambda () (table-radio-apply-format nil "value" table)))
                 :to-equal (list 'org-table-error :invalid-parameters))
         (expect (table-radio-test-error
                  (lambda () (table-radio-apply-row-format nil '("value") table)))
                 :to-equal (list 'org-table-error :invalid-parameters))
         (expect (table-radio-test-error
                  (lambda ()
                    (table-radio-generic rows '(:backend :unsupported) table)))
                 :to-equal (list 'org-table-error :invalid-parameters))
         (expect (table-radio-test-error
                  (lambda ()
                    (table-radio-render
                     table '(:format :unsupported :parameters nil))))
                 :to-equal (list 'org-table-error :unsupported-format))
         (expect (table-radio-generic
                  rows '(:sep "|" :hllfmt "%s") table)
                 :to-equal (format nil "Name~%Ada|one~%Bob|two")))))
  (it "stops at a non-SEND ORGTBL directive and handles a marker at EOF"
    (let* ((no-send
             (format nil "#+ORGTBL: TBLFM $1=$1~%| A |~%| 1 |~%<!-- BEGIN RECEIVE ORGTBL radio -->~%old~%<!-- END RECEIVE ORGTBL radio -->~%"))
           (eof-source
             (format nil "#+ORGTBL: SEND radio orgtbl-to-csv~%| A |~%| 1 |~%BEGIN RECEIVE ORGTBL radio~%old~%END RECEIVE ORGTBL radio"))
           (result (table-radio-test-send eof-source))
           (updated (document-text (edit-result-updated-document (first result)))))
      (with-soft-assertions
        (expect (table-radio-test-error (lambda () (table-radio-test-send no-send)))
                :to-equal (list (quote org-table-error) :missing-send-directive))
        (expect (not (null (search (format nil "BEGIN RECEIVE ORGTBL radio~%A~%1~%END RECEIVE ORGTBL radio") updated))) :to-be t)))))

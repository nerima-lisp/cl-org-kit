(in-package #:cl-org-kit/test)

(defun columnview-test-capture-condition (thunk)
  (handler-case
      (progn (funcall thunk) nil)
    (condition (condition) condition)))

(defun columnview-test-heading (document title)
  (let ((found nil))
    (map-nodes (lambda (node)
                 (when (and (null found)
                            (eq (node-type node) :headline)
                            (string= title (node-property node :raw-value "")))
                   (setf found node)))
               (document-root document))
    found))

(defun columnview-test-dynamic-block (document)
  (let ((found nil))
    (map-nodes (lambda (node)
                 (when (and (null found) (eq (node-type node) :dynamic-block))
                   (setf found node)))
               (document-root document))
    found))

(defun columnview-test-summary-error (summary value)
  (let* ((source
           (format nil
                   "#+COLUMNS: %ITEM %Value{~A}~%* Parent~%** Child~%:PROPERTIES:~%:Value: ~A~%:END:~%"
                   summary value))
         (document (parse-org source)))
    (columnview-test-capture-condition
     (lambda () (org-kit::column-view-data document)))))

(defun columnview-test-expect-error (condition reason)
  (expect (typep condition 'org-kit::org-column-view-error) :to-be t)
  (when (typep condition 'org-kit::org-column-view-error)
    (expect (org-kit::org-column-view-error-reason condition) :to-equal reason)
    (expect (or (null (org-kit::org-column-view-error-context condition))
                (stringp (org-kit::org-column-view-error-context condition)))
            :to-be t)))

(describe "Org column view data"
  (it "reads COLUMNS and computes the supported summary types over direct children"
    (let* ((format-prefix
             (format nil
                     "#+COLUMNS: %ITEM(Task) %Effort{+} %Money{$} %Spent{:} %ColonTime{:} %Flag{X} %Checked{X/} %Percent{X%} %Partial{X} %Minimum{min} %Maximum{max} %Mean{mean} %OneDecimal{+;%.1f} %TimeMinimum{:min} %TimeMaximum{:max} %TimeMean{:mean} %AgeMinimum{@min} %AgeMaximum{@max} %AgeMean{@mean} %AgeSeconds{@mean} %Estimate{est+}~%"
                     ))
           (source
             (concatenate
              'string format-prefix
              (format nil
                      "* Project~%:PROPERTIES:~%:Effort: 900~%:Money: 100~%:Spent: 999~%:ColonTime: 999~%:Flag: [ ]~%:Checked: [X]~%:Percent: [X]~%:Partial: [ ]~%:Minimum: 10~%:Maximum: 10~%:Mean: 10~%:OneDecimal: 10~%:TimeMinimum: 0:00~%:TimeMaximum: 0:00~%:TimeMean: 0:00~%:AgeMinimum: 1d~%:AgeMaximum: 1d~%:AgeMean: 1d~%:AgeSeconds: 99s~%:Estimate: 100-200~%:END:~%"
                      )
              (format nil
                      "** Alpha~%:PROPERTIES:~%:Effort: 1~%:Money: 1.25~%:Spent: 60~%:ColonTime: 1:00~%:Flag: [X]~%:Checked: [X]~%:Percent: [X]~%:Partial: [X]~%:Minimum: 2~%:Maximum: 2~%:Mean: 2~%:OneDecimal: 1.23~%:TimeMinimum: 1h~%:TimeMaximum: 1h~%:TimeMean: 1h~%:AgeMinimum: 1d~%:AgeMaximum: 1d~%:AgeMean: 1d~%:AgeSeconds: 1s~%:Estimate: 5-6~%:END:~%"
                      )
              (format nil
                      "** Beta~%:PROPERTIES:~%:Effort: 2~%:Money: 2.5~%:Spent: 30~%:ColonTime: 0:30~%:Flag: [X]~%:Checked: [ ]~%:Percent: [ ]~%:Partial: [ ]~%:Minimum: 4~%:Maximum: 4~%:Mean: 4~%:OneDecimal: 2~%:TimeMinimum: 30min~%:TimeMaximum: 30min~%:TimeMean: 30min~%:AgeMinimum: 2d~%:AgeMaximum: 2d~%:AgeMean: 2d~%:AgeSeconds: 2s~%:Estimate: 5-6~%:END:~%")))
           (document (parse-org source))
           (source-copy (copy-seq source))
           (rows (org-kit::column-view-data document)))
      (expect rows
              :to-equal
              '(("Task" "Effort" "Money" "Spent" "ColonTime" "Flag" "Checked" "Percent" "Partial"
                 "Minimum" "Maximum" "Mean" "OneDecimal" "TimeMinimum"
                 "TimeMaximum" "TimeMean" "AgeMinimum" "AgeMaximum" "AgeMean"
                 "AgeSeconds" "Estimate")
                ("Project" "3" "3.75" "1:30" "1:30" "[X]" "[1/2]" "[50%]" "[-]" "2"
                 "4" "3" "3.2" "0:30" "1:00" "0:45" "1d" "2d" "1d 12h"
                 "0min" "10-12")
                ("Alpha" "1" "1.25" "60" "1:00" "[X]" "[X]" "[X]" "[X]" "2" "2" "2"
                 "1.23" "1h" "1h" "1h" "1d" "1d" "1d" "1s" "5-6")
                ("Beta" "2" "2.5" "30" "0:30" "[X]" "[ ]" "[ ]" "[ ]" "4" "4" "4"
                 "2" "30min" "30min" "30min" "2d" "2d" "2d" "2s" "5-6")))
      (expect (equal (document-text document) source-copy) :to-be t)))

  (it "uses the nearest inherited COLUMNS property for a selected subtree"
    (let* ((document
             (parse-org
              (format nil
                      "#+COLUMNS: %ITEM(Global) %TODO~%* Root~%:PROPERTIES:~%:COLUMNS: %25ITEM(Scoped) %TODO~%:END:~%** TODO Child~%")))
           (root (columnview-test-heading document "Root"))
           (child (columnview-test-heading document "Child")))
      (expect (org-kit::column-view-data document)
              :to-equal '(("Global" "TODO") ("Root" "") ("Child" "TODO")))
      (expect (org-kit::column-view-data document :heading child)
              :to-equal '(("Scoped" "TODO") ("Child" "TODO")))
      (expect (org-kit::column-view-data document :heading root)
              :to-equal '(("Scoped" "TODO") ("Root" "") ("Child" "TODO")))))

  (it "reads a file-level COLUMNS property when no keyword or subtree format exists"
    (let ((document
            (parse-org
             (format nil
                     ":PROPERTIES:~%:COLUMNS: %ITEM(File property)~%:END:~%* Root~%"))))
      (expect (org-kit::column-view-data document)
              :to-equal '(("File property") ("Root")))))

  (it "ignores empty child values and formats currency summaries to two decimals"
    (let ((document
            (parse-org
             (format nil
                     "#+COLUMNS: %ITEM %Count{+} %Cost{$}~%* Parent~%** Present~%:PROPERTIES:~%:Count: 1~%:Cost: 1.5~%:END:~%** Empty~%"))))
      (expect (org-kit::column-view-data document)
              :to-equal '(("ITEM" "Count" "Cost")
                          ("Parent" "1" "1.50")
                          ("Present" "1" "1.5")
                          ("Empty" "" "")))))

  (it "formats mixed day and clock summaries and preserves seconds"
    (let ((document
            (parse-org
             (format nil
                     "#+COLUMNS: %ITEM %Long{:} %Precise{:}~%* Parent~%** Long day~%:PROPERTIES:~%:Long: 1d~%:Precise: 1:00:30~%:END:~%** Short clock~%:PROPERTIES:~%:Long: 2h~%:Precise: 0:00:30~%:END:~%"))))
      (expect (org-kit::column-view-data document)
              :to-equal '(("ITEM" "Long" "Precise")
                          ("Parent" "1d 2:00" "1:01:00")
                          ("Long day" "1d" "1:00:30")
                          ("Short clock" "2h" "0:00:30")))))

  (it "returns empty cells for every summary family when all child values are empty"
    (let* ((document
             (parse-org
              (format nil
                      "#+COLUMNS: %ITEM %Sum{+} %Money{$} %Minimum{min} %Maximum{max} %Mean{mean} %Clock{:} %TimeMinimum{:min} %TimeMaximum{:max} %TimeMean{:mean} %AgeMinimum{@min} %AgeMaximum{@max} %AgeMean{@mean} %Estimate{est+} %All{X} %Count{X/} %Percent{X%}~%* Parent~%** Empty~%")))
           (rows (org-kit::column-view-data document))
           (empty-row (cons "Parent" (make-list 16 :initial-element ""))))
      (expect (first rows)
              :to-equal '("ITEM" "Sum" "Money" "Minimum" "Maximum" "Mean"
                          "Clock" "TimeMinimum" "TimeMaximum" "TimeMean"
                          "AgeMinimum" "AgeMaximum" "AgeMean" "Estimate"
                          "All" "Count" "Percent"))
      (expect (rest rows) :to-equal (list empty-row
                                          (cons "Empty"
                                                (make-list 16 :initial-element ""))))))

  (it "reports invalid child values for every summary family with the exact reason"
    (dolist (case '(("+" "invalid") ("$" "invalid") ("min" "invalid")
                    ("max" "invalid") ("mean" "invalid")
                    ("+;%.1f" "invalid") (":" "invalid")
                    (":min" "invalid") (":max" "invalid")
                    (":mean" "invalid") ("@min" "invalid")
                    ("@max" "invalid") ("@mean" "invalid")
                    ("X" "yes") ("X/" "yes") ("X%" "yes")
                    ("est+" "8-2")))
      (let ((condition (columnview-test-summary-error (first case)
                                                       (second case))))
        (columnview-test-expect-error condition :invalid-summary-value))))

  (it "combines duration estimates and formats the result as an age range"
    (let ((document
            (parse-org
             (format nil
                     "#+COLUMNS: %ITEM %Estimate{est+}~%* Parent~%** Child~%:PROPERTIES:~%:Estimate: 5h-6h~%:END:~%"))))
      (expect (org-kit::column-view-data document)
              :to-equal '(("ITEM" "Estimate")
                          ("Parent" "5h-6h")
                          ("Child" "5h-6h")))))

  (it "computes timestamp ages against the supplied time context"
    (let* ((document
             (parse-org
              (format nil
                      "#+COLUMNS: %ITEM %Updated(AgeMin){@min} %Updated(AgeMax){@max} %Updated(AgeMean){@mean}~%* Parent~%** Recent~%:PROPERTIES:~%:Updated: <2025-01-03 Fri>~%:END:~%** Older~%:PROPERTIES:~%:Updated: [2025-01-01 Wed]~%:END:~%")))
           (time-context
             (make-time-context :reference-time (make-org-time-value "2025-01-04T00:00:00")
                                :zone (make-fixed-time-zone 0)))
           (condition
             (columnview-test-capture-condition
              (lambda () (org-kit::column-view-data document))))
           (rows (org-kit::column-view-data document :time-context time-context)))
      (columnview-test-expect-error condition :time-context-required)
      (expect rows
              :to-equal
              '(("ITEM" "AgeMin" "AgeMax" "AgeMean")
                ("Parent" "1d" "3d" "2d")
                ("Recent" "<2025-01-03 Fri>" "<2025-01-03 Fri>"
                 "<2025-01-03 Fri>")
                ("Older" "[2025-01-01 Wed]" "[2025-01-01 Wed]"
                 "[2025-01-01 Wed]")))))

  (it "uses Org's distinct checkbox summary rules for checkbox cookies"
    (let ((document
            (parse-org
             (format nil
                     "#+COLUMNS: %ITEM %Check{X} %Count{X/} %Percent{X%}~%* Parent~%** One~%:PROPERTIES:~%:Check: [100%]~%:Count: [2/2]~%:Percent: [100%]~%:END:~%** Two~%:PROPERTIES:~%:Check: [ ]~%:Count: [1/2]~%:Percent: [ ]~%:END:~%"))))
      (expect (org-kit::column-view-data document)
              :to-equal
              '(("ITEM" "Check" "Count" "Percent")
                ("Parent" "[ ]" "[1/2]" "[50%]")
                ("One" "[100%]" "[2/2]" "[100%]")
                ("Two" "[ ]" "[1/2]" "[ ]")))))

  (it "uses the default columns and renders special properties as strings"
    (let* ((document
             (parse-org
              (format nil "#+FILETAGS: common~%* TODO [#A] Task :local:~%")))
           (source (document-text document)))
      (expect (org-kit::column-view-data document)
              :to-equal '(("ITEM" "TODO" "PRIORITY" "TAGS")
                          ("Task" "TODO" "A" ":local:") ))
      (expect (document-text document) :to-equal source)))

  (it "updates a columnview block in its surrounding subtree and preserves the source"
    (let* ((source
             (format nil
                     "#+COLUMNS: %ITEM(Task) %Count{+}~%* Project~%#+BEGIN: columnview~%stale~%#+END:~%** Alpha~%:PROPERTIES:~%:Count: 1~%:END:~%** Beta~%:PROPERTIES:~%:Count: 2~%:END:~%"))
           (document (parse-org source))
           (block (columnview-test-dynamic-block document))
           (source-octets (write-org document :output-type :octets))
           (edit (org-kit::update-dynamic-block document block))
           (updated (edit-result-updated-document edit)))
      (expect (document-text updated)
              :to-equal
              (format nil
                      "#+COLUMNS: %ITEM(Task) %Count{+}~%* Project~%#+BEGIN: columnview~%| Task    | Count |~%|---------+-------|~%| Project | 3     |~%| Alpha   | 1     |~%| Beta    | 2     |~%#+END:~%** Alpha~%:PROPERTIES:~%:Count: 1~%:END:~%** Beta~%:PROPERTIES:~%:Count: 2~%:END:~%"))
      (expect (length (edit-result-spans edit)) :to-be 1)
      (expect (document-text document) :to-equal source)
      (expect (write-org document :output-type :octets)
              :to-equalp source-octets)))

  (it "keeps the source unchanged when a columnview definition has an unknown summary"
    (let* ((source
             (format nil
                     "#+COLUMNS: %ITEM %Count{unknown}~%* Project~%#+BEGIN: columnview~%stale~%#+END:~%** Alpha~%:PROPERTIES:~%:Count: 1~%:END:~%"))
           (document (parse-org source))
           (block (columnview-test-dynamic-block document))
           (source-octets (write-org document :output-type :octets))
           (condition
             (columnview-test-capture-condition
              (lambda () (org-kit::update-dynamic-block document block)))))
      (columnview-test-expect-error condition :invalid-summary-type)
      (expect (document-text document) :to-equal source)
      (expect (write-org document :output-type :octets)
              :to-equalp source-octets)))

  (it "signals named errors for malformed column syntax and summary values"
    (let* ((syntax-document
             (parse-org (format nil "#+COLUMNS: ITEM~%* Root~%")))
           (summary-document
             (parse-org (format nil "#+COLUMNS: %ITEM{unknown}~%* Root~%")))
           (value-document
             (parse-org
              (format nil
                      "#+COLUMNS: %ITEM %Count{mean}~%* Parent~%** Child~%:PROPERTIES:~%:Count: nope~%:END:~%")))
           (syntax-error
             (columnview-test-capture-condition
              (lambda () (org-kit::column-view-data syntax-document))))
           (summary-error
             (columnview-test-capture-condition
              (lambda () (org-kit::column-view-data summary-document))))
           (value-error
             (columnview-test-capture-condition
              (lambda () (org-kit::column-view-data value-document)))))
      (columnview-test-expect-error syntax-error :invalid-columns)
      (columnview-test-expect-error summary-error :invalid-summary-type)
      (columnview-test-expect-error value-error :invalid-summary-value)))

  (it "rejects malformed widths, checkbox values, and estimates with named reasons"
    (let* ((width-document
             (parse-org (format nil "#+COLUMNS: %0ITEM~%* Root~%")))
           (checkbox-document
             (parse-org
              (format nil
                      "#+COLUMNS: %ITEM %Flag{X}~%* Parent~%** Child~%:PROPERTIES:~%:Flag: yes~%:END:~%")))
           (estimate-document
             (parse-org
              (format nil
                      "#+COLUMNS: %ITEM %Estimate{est+}~%* Parent~%** Child~%:PROPERTIES:~%:Estimate: 8-2~%:END:~%")))
           (width-error
             (columnview-test-capture-condition
              (lambda () (org-kit::column-view-data width-document))))
           (checkbox-error
             (columnview-test-capture-condition
              (lambda () (org-kit::column-view-data checkbox-document))))
           (estimate-error
             (columnview-test-capture-condition
              (lambda () (org-kit::column-view-data estimate-document)))))
      (columnview-test-expect-error width-error :invalid-column-width)
      (columnview-test-expect-error checkbox-error :invalid-summary-value)
      (columnview-test-expect-error estimate-error :invalid-summary-value)))

  (it "rejects an unparsed document and a heading outside the document"
    (let* ((unparsed (make-document "* Root"))
           (document (parse-org "* Root"))
           (unparsed-error
             (columnview-test-capture-condition
              (lambda () (org-kit::column-view-data unparsed))))
           (scope-error
             (columnview-test-capture-condition
              (lambda ()
                (org-kit::column-view-data document
                                           :heading (document-root document))))))
      (columnview-test-expect-error unparsed-error :document-not-parsed)
      (columnview-test-expect-error scope-error :invalid-heading))))

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
      (expect (not (null (search "document-not-parsed"
                                 (princ-to-string unparsed-error))))
              :to-be t)
      (columnview-test-expect-error scope-error :invalid-heading)))

  (it "reports each incomplete column-definition form with a named reason"
    (dolist (source (list (format nil "#+COLUMNS: %~%* Root~%")
                          (format nil "#+COLUMNS: %ITEM(Title~%* Root~%")
                          (format nil "#+COLUMNS: %ITEM{+~%* Root~%")
                          (format nil "#+COLUMNS:~%* Root~%")))
      (let* ((document (parse-org source))
             (condition
               (columnview-test-capture-condition
                (lambda () (org-kit::column-view-data document)))))
        (columnview-test-expect-error condition :invalid-columns)))
    (dolist (summary '("+;%.f" "+;%.13f"))
      (let* ((document
               (parse-org
                (format nil "#+COLUMNS: %ITEM{~A}~%* Root~%" summary)))
             (condition
               (columnview-test-capture-condition
                (lambda () (org-kit::column-view-data document)))))
        (columnview-test-expect-error condition :invalid-summary-type))))

  (it "keeps empty parser inputs and unsupported summary fallback explicit"
    (let* ((number-column
             (org-kit::%make-column-view-column :property "Number"))
           (duration-column
             (org-kit::%make-column-view-column :property "Duration"))
           (unsupported-column
             (org-kit::%make-column-view-column :property "Value"
                                                 :summary :unsupported))
           (number-error
             (columnview-test-capture-condition
              (lambda ()
                (org-kit::column-view-parse-number number-column ""))))
           (duration-error
             (columnview-test-capture-condition
              (lambda ()
                (org-kit::column-view-parse-duration duration-column "")))))
      (columnview-test-expect-error number-error :invalid-summary-value)
      (columnview-test-expect-error duration-error :invalid-summary-value)
      (expect (org-kit::column-view-summary-value unsupported-column nil)
              :to-equal "")))

  (it "reads COLUMNS from a top-level property drawer and skips other children"
    (let* ((document (parse-org (format nil "* Root~%")))
           (root (document-root document))
           (drawer
             (make-node
              :property-drawer
              :children
              (list (make-node :paragraph)
                    (make-node :node-property
                               :properties '(:key "COLUMNS"
                                             :value "%ITEM(Drawer)"))))))
      (append-child root drawer)
      (expect (org-kit::column-view-data document)
              :to-equal '(("Drawer") ("Root")))))

  (it "renders special properties, timestamps, categories, and source names"
    (let* ((document
             (parse-org
              (format nil
                      "#+COLUMNS: %ITEM %TODO %PRIORITY %TAGS %ALLTAGS %CATEGORY %FILE %SCHEDULED %DEADLINE %CLOSED %TIMESTAMP %TIMESTAMP_IA %LEVEL %BLOCKED %CLOCKSUM %CLOCKSUM_T %Custom~%#+FILETAGS: global~%#+CATEGORY: file-category~%* TODO [#A] Parent :outer:~%SCHEDULED: <2025-01-01 Wed> DEADLINE: <2025-01-02 Thu> CLOSED: [2024-12-31 Tue]~%:PROPERTIES:~%:CATEGORY: local-category~%:Custom: text~%:END:~%active <2024-12-30 Mon> inactive [2024-12-29 Sun]~%** Child :inner:~%** Untagged~%")
              :source-name "tasks.org"))
           (child (columnview-test-heading document "Child"))
           (rows (org-kit::column-view-data document)))
      (expect rows
              :to-equal
              '(("ITEM" "TODO" "PRIORITY" "TAGS" "ALLTAGS" "CATEGORY"
                 "FILE" "SCHEDULED" "DEADLINE" "CLOSED" "TIMESTAMP"
                 "TIMESTAMP_IA" "LEVEL" "BLOCKED" "CLOCKSUM" "CLOCKSUM_T"
                 "Custom")
                ("Parent" "TODO" "A" ":outer:" ":global:outer:"
                 "local-category" "tasks.org" "<2025-01-01 Wed>"
                 "<2025-01-02 Thu>" "[2024-12-31 Tue]" "<2024-12-30 Mon>"
                 "[2024-12-29 Sun]" "1" "" "" "" "text")
                ("Child" "" "" ":inner:" ":global:outer:inner:"
                 "local-category" "tasks.org" "" "" "" "" "" "2"
                 "" "" "" "")
                ("Untagged" "" "" "" ":global:outer:" "local-category"
                 "tasks.org" "" "" "" "" "" "2" "" "" "" "")))
      (setf (node-property child :priority) "B")
      (expect (third (third (org-kit::column-view-data document)))
              :to-equal "B")
      (setf (node-property child :priority) #\C)
      (expect (third (third (org-kit::column-view-data document)))
              :to-equal "C")
      (setf (node-property child :priority) #xD800)
      (expect (third (third (org-kit::column-view-data document)))
              :to-equal (if (code-char #xD800)
                            (string (code-char #xD800))
                            ""))
      (setf (node-property child :priority) #x110000)
      (expect (third (third (org-kit::column-view-data document)))
              :to-equal "")
      (let ((custom-property nil))
        (map-nodes (lambda (node)
                     (when (and (eq (node-type node) :node-property)
                                (string-equal (node-property node :key "")
                                              "Custom"))
                       (setf custom-property node)))
                   (document-root document))
        (setf (node-property custom-property :value) 7)
        (expect (car (last (second (org-kit::column-view-data document))))
                :to-equal "7")
        (setf (node-property custom-property :value) nil)
        (expect (car (last (second (org-kit::column-view-data document))))
                :to-equal ""))))

  (it "uses settings and source-name fallbacks for CATEGORY"
    (let* ((settings-document
             (parse-org (format nil "#+COLUMNS: %CATEGORY %FILE~%* Root~%")
                        :settings '(:category "settings-category")))
           (source-document
             (parse-org (format nil "#+COLUMNS: %CATEGORY %FILE~%* Root~%")
                        :source-name "folder/tasks.org"))
           (empty-source-document
             (parse-org (format nil "#+COLUMNS: %CATEGORY~%* Root~%")))
           (directory-source-document
             (parse-org (format nil "#+COLUMNS: %CATEGORY %FILE~%* Root~%")
                        :source-name "folder/")))
      (expect (org-kit::column-view-data settings-document)
              :to-equal '(("CATEGORY" "FILE")
                          ("settings-category" "")))
      (expect (org-kit::column-view-data source-document)
              :to-equal '(("CATEGORY" "FILE")
                          ("tasks" "folder/tasks.org")))
      (expect (org-kit::column-view-data empty-source-document)
              :to-equal '(("CATEGORY") ("")))
      (expect (org-kit::column-view-data directory-source-document)
              :to-equal '(("CATEGORY" "FILE") ("" "folder/")))))

  (it "parses signed decimals and multi-unit durations and rejects malformed values"
    (let* ((document
             (parse-org
              (format nil
                      "#+COLUMNS: %ITEM %Number(Sum){+} %Number(Min){min} %Number(Max){max} %Number(Mean){mean} %Number(Precise){+;%.2f} %Duration{:} %NegativeClock{:}~%* Parent~%** Negative~%:PROPERTIES:~%:Number: -.5~%:Duration: -1h 30min~%:NegativeClock: -1h~%:END:~%** Positive~%:PROPERTIES:~%:Number: +1.~%:Duration: 1h 30min~%:NegativeClock: -30min~%:END:~%")))
           (rows (org-kit::column-view-data document)))
      (expect rows
              :to-equal
              '(("ITEM" "Sum" "Min" "Max" "Mean" "Precise" "Duration"
                 "NegativeClock")
                ("Parent" "0.5" "-0.5" "1" "0.25" "0.50" "0:00"
                 "-1:30")
                ("Negative" "-.5" "-.5" "-.5" "-.5" "-.5"
                 "-1h 30min" "-1h")
                ("Positive" "+1." "+1." "+1." "+1." "+1." "1h 30min"
                 "-30min")))
      (dolist (case '(("+" "1.2.3") (":" "1fortnight")
                      (":" "1h nope") (":" "1h -30min")
                      (":min" "1:x")))
        (let ((condition
                (columnview-test-summary-error (first case) (second case))))
          (columnview-test-expect-error condition :invalid-summary-value)))))

  (it "formats parser boundary values and rejects repeated decimal points"
    (let* ((document
             (parse-org
              (format nil
                      "#+COLUMNS: %ITEM %Duration{:}~%* Parent~%** Bare number~%:PROPERTIES:~%:Duration: 90~%:END:~%** Decimal unit~%:PROPERTIES:~%:Duration: 1.5h~%:END:~%"))))
      (expect (org-kit::column-view-parse-number-value "12") :to-equal 12)
      (expect (org-kit::column-view-parse-number-value "1.25") :to-equal 5/4)
      (expect (org-kit::column-view-format-number 1.0d0) :to-equal "1")
      (expect (org-kit::column-view-format-age 61) :to-equal "1h 1min")
      (expect (org-kit::column-view-data document)
              :to-equal '(("ITEM" "Duration")
                          ("Parent" "3:00")
                          ("Bare number" "90")
                          ("Decimal unit" "1.5h")))
      (columnview-test-expect-error
       (columnview-test-summary-error ":" "1.2.3h")
       :invalid-summary-value)))

  (it "reports a recognized timestamp that fails to parse"
    (let* ((document
             (parse-org
              (format nil
                      "#+COLUMNS: %ITEM %Age{@min}~%* Parent~%** Broken~%:PROPERTIES:~%:Age: today~%:END:~%")))
           (time-context
             (make-time-context
              :reference-time (make-org-time-value "2025-01-04T00:00:00")
              :zone (make-fixed-time-zone 0)))
           (original (symbol-function 'org-kit::query-parse-org-date-literal))
           (condition nil))
      (unwind-protect
           (progn
             (setf (symbol-function 'org-kit::query-parse-org-date-literal)
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     nil))
             (setf condition
                   (columnview-test-capture-condition
                    (lambda ()
                      (org-kit::column-view-data
                       document :time-context time-context)))))
        (setf (symbol-function 'org-kit::query-parse-org-date-literal)
              original))
      (columnview-test-expect-error condition :invalid-summary-value)))

  (it "aggregates long duration units and relative timestamp ages"
    (let* ((document
             (parse-org
              (format nil
                      "#+COLUMNS: %ITEM %Long{:} %AgeMin{@min} %AgeMax{@max} %AgeMean{@mean}~%* Parent~%** Week~%:PROPERTIES:~%:Long: 1w~%:AgeMin: [+1d]~%:AgeMax: [-1d]~%:AgeMean: today~%:END:~%** Month year~%:PROPERTIES:~%:Long: 1m 1y~%:AgeMin: now~%:AgeMax: tomorrow~%:AgeMean: 2025-01-05~%:END:~%")))
           (time-context
             (make-time-context
              :reference-time (make-org-time-value "2025-01-04T00:00:00")
              :zone (make-fixed-time-zone 0))))
      (expect (org-kit::column-view-data document :time-context time-context)
              :to-equal
              '(("ITEM" "Long" "AgeMin" "AgeMax" "AgeMean")
                ("Parent" "402d 6:00" "-1d" "1d" "-12h")
                ("Week" "1w" "[+1d]" "[-1d]" "today")
                ("Month year" "1m 1y" "now" "tomorrow" "2025-01-05")))
      (let* ((zero-document
               (parse-org
                (format nil
                        "#+COLUMNS: %ITEM %Age{@min}~%* Parent~%** Now~%:PROPERTIES:~%:Age: now~%:END:~%** Today~%:PROPERTIES:~%:Age: today~%:END:~%"))))
        (expect (org-kit::column-view-data zero-document
                                           :time-context time-context)
                :to-equal '(("ITEM" "Age")
                            ("Parent" "0min")
                            ("Now" "now")
                            ("Today" "today"))))))

  (it "rejects invalid document and time-context argument types"
    (let* ((document-error
             (columnview-test-capture-condition
              (lambda () (org-kit::column-view-data :not-a-document))))
           (context-error
             (columnview-test-capture-condition
              (lambda () (org-kit::column-view-data (parse-org "* Root")
                                                    :time-context :bad))))
           (report-condition
             (columnview-test-summary-error "mean" "nope"))
           (report (princ-to-string report-condition)))
      (expect (typep document-error 'type-error) :to-be t)
      (expect (typep context-error 'type-error) :to-be t)
      (expect (not (null (search "invalid-summary-value" report))) :to-be t)
      (expect (not (null (search "Value=" report))) :to-be t)))

  (it "preserves special columns instead of aggregating their values"
    (let ((document
            (parse-org
             (format nil
                     "#+COLUMNS: %ITEM{+} %TODO{+} %PRIORITY{mean}~%* Parent~%** TODO Child~%"))))
      (expect (org-kit::column-view-data document)
              :to-equal '(("ITEM" "TODO" "PRIORITY")
                          ("Parent" "" "")
                          ("Child" "TODO" "")))))

  (it "summarizes partial cookies, negative estimates, and percentage cookies"
    (let* ((cookie-document
             (parse-org
              (format nil
                      "#+COLUMNS: %ITEM %All{X} %Count{X/} %Percent{X%}~%* Parent~%** Half~%:PROPERTIES:~%:All: [50%]~%:Count: [2/4]~%:Percent: [50%]~%:END:~%** Done~%:PROPERTIES:~%:All: [X]~%:Count: [4/4]~%:Percent: [100%]~%:END:~%** Lower~%:PROPERTIES:~%:All: [x]~%:Count: [1/1]~%:Percent: [100%]~%:END:~%** Zero~%:PROPERTIES:~%:All: [ ]~%:Count: [0/0]~%:Percent: [ ]~%:END:~%")))
           (estimate-document
             (parse-org
              (format nil
                      "#+COLUMNS: %ITEM %Estimate{est+}~%* Parent~%** Negative~%:PROPERTIES:~%:Estimate: -2--1~%:END:~%** Point~%:PROPERTIES:~%:Estimate: 5~%:END:~%")))
           (mixed-estimate-document
             (parse-org
              (format nil
                      "#+COLUMNS: %ITEM %Estimate{est+}~%* Parent~%** Number~%:PROPERTIES:~%:Estimate: 5~%:END:~%** Duration~%:PROPERTIES:~%:Estimate: 1h~%:END:~%"))))
      (expect (org-kit::column-view-data cookie-document)
              :to-equal '(("ITEM" "All" "Count" "Percent")
                          ("Parent" "[-]" "[2/4]" "[50%]")
                          ("Half" "[50%]" "[2/4]" "[50%]")
                          ("Done" "[X]" "[4/4]" "[100%]")
                          ("Lower" "[x]" "[1/1]" "[100%]")
                          ("Zero" "[ ]" "[0/0]" "[ ]")))
      (expect (org-kit::column-view-data estimate-document)
              :to-equal '(("ITEM" "Estimate")
                          ("Parent" "3-4")
                          ("Negative" "-2--1")
                          ("Point" "5")))
      (columnview-test-expect-error
       (columnview-test-capture-condition
        (lambda () (org-kit::column-view-data mixed-estimate-document)))
       :invalid-summary-value)
      (dolist (case '(("X/" "[2/1]") ("X%" "[101%]")))
        (let ((condition
                (columnview-test-summary-error (first case) (second case))))
          (columnview-test-expect-error condition :invalid-summary-value)))))

  (it "covers numeric and duration parser boundaries"
    (let ((column (org-kit::%make-column-view-column :property "Value")))
      (multiple-value-bind (token number end)
          (org-kit::column-view-read-number "" 0)
        (expect (list token number end) :to-equal (list nil nil 0)))
      (expect (org-kit::column-view-parse-number-value "+1") :to-equal 1)
      (expect (org-kit::column-view-parse-number-value ".5") :to-equal 1/2)
      (columnview-test-expect-error
       (columnview-test-capture-condition
        (lambda () (org-kit::column-view-parse-duration column "1h 30")))
       :invalid-summary-value)
      (columnview-test-expect-error
       (columnview-test-capture-condition
        (lambda ()
          (org-kit::column-view-checkbox-complete-p
           column "x2/4]" :checkbox-count)))
       :invalid-summary-value)
      (columnview-test-expect-error
       (columnview-test-capture-condition
        (lambda ()
          (org-kit::column-view-checkbox-complete-p
           column "[50%x" :checkbox-percent)))
       :invalid-summary-value)
      (columnview-test-expect-error
       (columnview-test-capture-condition
        (lambda () (org-kit::column-view-parse-estimate column "1h-2")))
       :invalid-summary-value)))

  (it "reads property drawer text and ignores nested global COLUMNS keywords"
    (let* ((source (format nil ":COLUMNS: %ITEM(Text)~%* Root~%"))
           (document (parse-org source))
           (drawer (make-node :drawer :properties '(:drawer-name "PROPERTIES")))
           (paragraph
             (make-node :paragraph :begin 0
                        :end (position #\Newline source))))
      (append-child drawer (make-node :comment))
      (append-child drawer paragraph)
      (append-child (document-root document) drawer)
      (expect (multiple-value-list
               (org-kit::column-view-global-property-format document))
              :to-equal '("%ITEM(Text)" t)))
    (let ((document
            (parse-org (format nil "* Root~%#+COLUMNS: %ITEM(Nested)~%"))))
      (expect (multiple-value-list
               (org-kit::column-view-global-keyword-format document))
              :to-equal '(nil nil))))

  (it "rejects malformed checkbox cookies and mixed estimate endpoints"
    (let ((column (org-kit::%make-column-view-column :property "Value")))
      (dolist (value '("x2/4]" "[50%x"))
        (expect (typep
                 (columnview-test-capture-condition
                  (lambda ()
                    (org-kit::column-view-checkbox-complete-p
                     column value :checkbox-percent)))
                 'org-kit::org-column-view-error)
                :to-be t)))))

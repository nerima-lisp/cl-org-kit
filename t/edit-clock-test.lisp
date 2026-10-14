;;;; t/edit-clock-test.lisp
(in-package #:cl-org-kit/test)

(defun edit-clock-test-fixture (relative)
  (read-conformance-tree
   (conformance-fixture-path "behavior" relative)))

(defun edit-clock-test-source ()
  (parse-org
   (read-conformance-octets
    (conformance-fixture-path "behavior" "corpus/h1-clock.org"))))

(defun edit-clock-test-find-case (cases id)
  (find id cases :key (lambda (case) (getf case :id)) :test #'string=))

(defun edit-clock-test-error (thunk)
  (handler-case (progn (funcall thunk) nil)
    (org-clock-error (condition) condition)))

(defun edit-clock-test-condition (thunk)
  (handler-case (progn (funcall thunk) nil)
    (error (condition) condition)))

(defun edit-clock-test-heading (document title)
  (let ((found nil))
    (map-nodes (lambda (node)
                 (when (and (eq (node-type node) :headline)
                            (string= (node-property node :raw-value)
                                     title))
                   (setf found node)))
               (document-root document))
    found))

(defun edit-clock-test-heading-path (document path)
  (let ((found nil))
    (map-nodes
     (lambda (node)
       (when (eq (node-type node) :headline)
         (let ((actual-path
                 (loop for ancestor = node then (node-parent ancestor)
                       while ancestor
                       when (eq (node-type ancestor) :headline)
                         collect (node-property ancestor :raw-value))))
           (when (equal (nreverse actual-path) path)
             (setf found node)))))
     (document-root document))
    found))

(defun edit-clock-test-context (reference)
  (make-time-context :reference-time (make-org-time-value reference)
                     :zone (resolve-time-zone "UTC")))

(defun edit-clock-test-assert-clock-error (condition reason message context)
  (with-soft-assertions
    (expect (typep condition 'org-clock-error) :to-be t)
    (when (typep condition 'org-clock-error)
      (expect (org-kit-error-message condition) :to-equal message)
      (expect (org-clock-error-reason condition) :to-equal reason)
      (expect (org-clock-error-context condition) :to-equal context))))

(defun edit-clock-test-open-value (line)
  (subseq line (position #\[ line)))

(defun edit-clock-test-clock-line (timestamp)
  (format nil "CLOCK: ~A" timestamp))

(defun edit-clock-test-closed-value (line)
  (let ((start (position #\[ line))
        (arrow (search "=>" line)))
    (format nil "~A => ~A"
            (string-right-trim '(#\Space) (subseq line start arrow))
            (string-trim '(#\Space) (subseq line (+ arrow 2))))))

(defun edit-clock-test-single-heading (document)
  (edit-clock-test-heading document "Task"))

(describe "format-preserving clock edits"
  (it "matches the H1 oracle timestamp, drawer, and metadata placement cases"
    (let* ((cases (getf (edit-clock-test-fixture "cases/h1-clock.sexp") :cases))
           (oracle (edit-clock-test-fixture "expected/h1-clock.sexp"))
           (source-document (edit-clock-test-source))
           (source (document-text source-document))
           (default-case (edit-clock-test-find-case cases "default-logbook"))
           (default-expected (edit-clock-test-find-case oracle "default-logbook"))
           (default-heading
             (edit-clock-test-heading-path source-document
                                           (getf default-case :heading-path))))
      (multiple-value-bind (result line)
          (clock-in source-document default-heading
                    :at (make-org-time-value (getf default-case :at))
                    :time-context (edit-clock-test-context (getf default-case :at)))
        (with-soft-assertions
          (expect (length cases) :to-be 7)
          (expect (length oracle) :to-be 7)
          (expect line :to-equal
                  (edit-clock-test-open-value
                   (getf default-expected :org-open-line)))
          (expect (document-text source-document) :to-equal source)
          (expect (search (format nil ":LOGBOOK:~%CLOCK: [2026-10-14 Wed 09:05]~%:END:")
                          (document-text (edit-result-updated-document result)))
                  :to-be-truthy)))
      (dolist (id '("local-property-overrides-setting"
                    "inherited-property-overrides-setting"))
        (let* ((case (edit-clock-test-find-case cases id))
               (expected (edit-clock-test-find-case oracle id))
               (property-source
                 (concatenate 'string
                              (format nil "#+PROPERTY: CLOCK_INTO_DRAWER GLOBAL~%")
                              source))
               (property-document (parse-org property-source))
               (heading (edit-clock-test-heading-path
                         property-document (getf case :heading-path))))
          (multiple-value-bind (result line)
              (clock-in property-document heading
                        :at (make-org-time-value (getf case :at))
                        :time-context (edit-clock-test-context (getf case :at)))
            (let ((updated-source
                    (document-text (edit-result-updated-document result))))
              (with-soft-assertions
                (expect line :to-equal
                        (edit-clock-test-open-value
                         (getf expected :org-open-line)))
                (expect (search (format nil ":~A:~%~A~%:END:"
                                        (getf expected :org-drawer-name)
                                        (edit-clock-test-clock-line line))
                                updated-source)
                        :to-be-truthy)
                (expect (search ":GLOBAL:" updated-source) :to-be nil)
                (expect (document-text property-document) :to-equal property-source))))))
      (let* ((case (edit-clock-test-find-case
                    cases "insertion-after-metadata-before-body"))
             (expected (edit-clock-test-find-case
                        oracle "insertion-after-metadata-before-body"))
             (heading (edit-clock-test-heading-path
                       source-document (getf case :heading-path))))
        (multiple-value-bind (result line)
            (clock-in source-document heading
                      :at (make-org-time-value (getf case :at))
                      :time-context (edit-clock-test-context (getf case :at)))
          (with-soft-assertions
            (expect line :to-equal
                    (edit-clock-test-open-value
                     (getf expected :org-open-line)))
            (expect (document-text (edit-result-updated-document result))
                    :to-equal (getf expected :org-source-after-in)))))))

  (it "closes one existing row and returns the canonical interval"
    (let* ((oracle (edit-clock-test-fixture "expected/h1-clock.sexp"))
           (closed-expected (edit-clock-test-find-case oracle "closed-line-format"))
           (source (format nil "* Task~%Body~%"))
           (document (parse-org source))
           (heading (edit-clock-test-single-heading document))
           (context (edit-clock-test-context "2026-10-14T10:37:00"))
           (insertion-offset (1+ (position #\Newline source)))
           (expected-source
             (format nil "* Task~%:LOGBOOK:~%~A~%:END:~%Body~%"
                     (getf closed-expected :org-closed-line))))
      (multiple-value-bind (started start-line)
          (clock-in document heading :at (make-org-time-value "2026-10-14T09:05:00")
                    :time-context context)
        (let* ((open-document (edit-result-updated-document started))
               (open-heading (edit-clock-test-single-heading open-document))
               (open-source (document-text open-document))
               (clock-row (edit-clock-test-clock-line start-line))
               (clock-start (search clock-row open-source))
               (clock-end (+ clock-start (length clock-row)))
               (insertion-span (first (edit-result-spans started))))
          (multiple-value-bind (closed closed-line)
              (clock-out open-document open-heading
                         :at (make-org-time-value "2026-10-14T10:37:00")
                         :time-context context :drawer "LOGBOOK")
            (let* ((updated-document (edit-result-updated-document closed))
                   (replacement-span (first (edit-result-spans closed))))
              (with-soft-assertions
                (expect start-line :to-equal "[2026-10-14 Wed 09:05]")
                (expect (edit-span-start insertion-span) :to-equal insertion-offset)
                (expect (edit-span-end insertion-span) :to-equal insertion-offset)
                (expect closed-line :to-equal
                        (edit-clock-test-closed-value
                         (getf closed-expected :org-closed-line)))
                (expect (document-text updated-document) :to-equal expected-source)
                (expect (write-org updated-document :output-type :octets)
                        :to-equalp (cl-codec-kit:string-to-octets expected-source
                                                                   :encoding :utf-8))
                (expect (edit-span-start replacement-span) :to-equal clock-start)
                (expect (edit-span-end replacement-span) :to-equal clock-end)
                (expect (document-text document) :to-equal source))))))))

  (it "limits running clocks to the target heading when editing siblings"
    (let* ((source
             (format nil "* Task~%* Other~%CLOCK: [2026-10-14 Wed 09:00]~%"))
           (document (parse-org source))
           (task (edit-clock-test-heading document "Task"))
           (context (edit-clock-test-context "2026-10-14T10:30:00"))
           (other-row "CLOCK: [2026-10-14 Wed 09:00]")
           (open-row "CLOCK: [2026-10-14 Wed 10:00]")
           (closed-row
             "CLOCK: [2026-10-14 Wed 10:00]--[2026-10-14 Wed 10:30] =>  0:30")
           (expected-after-in
             (format nil "* Task~%:LOGBOOK:~%~A~%:END:~%* Other~%~A~%"
                     open-row other-row))
           (expected-after-out
             (format nil "* Task~%:LOGBOOK:~%~A~%:END:~%* Other~%~A~%"
                     closed-row other-row)))
      (multiple-value-bind (started start-line)
          (clock-in document task :at (make-org-time-value "2026-10-14T10:00:00")
                    :time-context context)
        (let* ((open-document (edit-result-updated-document started))
               (open-task (edit-clock-test-heading open-document "Task")))
          (multiple-value-bind (closed closed-line)
              (clock-out open-document open-task
                         :at (make-org-time-value "2026-10-14T10:30:00")
                         :time-context context)
            (with-soft-assertions
              (expect start-line :to-equal "[2026-10-14 Wed 10:00]")
              (expect (document-text open-document) :to-equal expected-after-in)
              (expect closed-line :to-equal
                      "[2026-10-14 Wed 10:00]--[2026-10-14 Wed 10:30] => 0:30")
              (expect (document-text (edit-result-updated-document closed))
                      :to-equal expected-after-out)
              (expect (document-text open-document) :to-equal expected-after-in)
              (expect (document-text document) :to-equal source)))))))

  (it "limits running clocks to the target entry without child headings"
    (let* ((child-line "CLOCK: [2026-10-14 Wed 09:00]")
           (source (format nil "* Parent~%** Child~%~A~%" child-line))
           (document (parse-org source))
           (parent (edit-clock-test-heading document "Parent"))
           (context (edit-clock-test-context "2026-10-14T10:00:00"))
           (missing (edit-clock-test-error
                     (lambda ()
                       (clock-out document parent
                                  :at (make-org-time-value "2026-10-14T10:00:00")
                                  :time-context context)))))
      (edit-clock-test-assert-clock-error missing :clock-not-running
                                          "No running CLOCK entry" nil)
      (multiple-value-bind (started start-value)
          (clock-in document parent
                    :at (make-org-time-value "2026-10-14T10:00:00")
                    :time-context context)
        (let* ((open-document (edit-result-updated-document started))
               (open-parent (edit-clock-test-heading open-document "Parent"))
               (expected-open
                 (format nil "* Parent~%:LOGBOOK:~%CLOCK: [2026-10-14 Wed 10:00]~%:END:~%** Child~%~A~%"
                         child-line)))
          (multiple-value-bind (closed interval)
              (clock-out open-document open-parent
                         :at (make-org-time-value "2026-10-14T10:30:00")
                         :time-context (edit-clock-test-context "2026-10-14T10:30:00"))
            (with-soft-assertions
              (expect start-value :to-equal "[2026-10-14 Wed 10:00]")
              (expect (document-text open-document) :to-equal expected-open)
              (expect interval :to-equal
                      "[2026-10-14 Wed 10:00]--[2026-10-14 Wed 10:30] => 0:30")
              (expect (document-text (edit-result-updated-document closed))
                      :to-equal
                      (format nil "* Parent~%:LOGBOOK:~%CLOCK: [2026-10-14 Wed 10:00]--[2026-10-14 Wed 10:30] =>  0:30~%:END:~%** Child~%~A~%"
                              child-line))
              (expect (document-text document) :to-equal source)))))))

  (it "inserts after headings with empty, final-newline, and planning-only sections"
    (let* ((context (edit-clock-test-context "2026-10-14T09:05:00"))
           (line "[2026-10-14 Wed 09:05]")
           (cases
             (list
              (list "* Task"
                    (format nil "* Task~%:LOGBOOK:~%~A~%:END:"
                            (edit-clock-test-clock-line line)))
              (list (format nil "* Task~%")
                    (format nil "* Task~%:LOGBOOK:~%~A~%:END:~%"
                            (edit-clock-test-clock-line line)))
              (list (format nil "* Task~%SCHEDULED: <2026-10-14 Wed 08:00>~%")
                    (format nil "* Task~%SCHEDULED: <2026-10-14 Wed 08:00>~%:LOGBOOK:~%~A~%:END:~%"
                            (edit-clock-test-clock-line line))))))
      (dolist (case cases)
        (let* ((source (first case))
               (expected (second case))
               (document (parse-org source))
               (heading (edit-clock-test-single-heading document)))
          (multiple-value-bind (result actual-line)
              (clock-in document heading
                        :at (make-org-time-value "2026-10-14T09:05:00")
                        :time-context context)
            (with-soft-assertions
              (expect actual-line :to-equal "[2026-10-14 Wed 09:05]")
              (expect (document-text (edit-result-updated-document result))
                      :to-equal expected)
              (expect (document-text document) :to-equal source)))))))

  (it "uses LOGBOOK when CLOCK_INTO_DRAWER is t without a LOG_INTO_DRAWER setting"
    (let* ((source
             (format nil "#+PROPERTY: CLOCK_INTO_DRAWER t~%* Task~%:LOGBOOK:~%:END:~%"))
           (document (parse-org source))
           (heading (edit-clock-test-single-heading document))
           (context (edit-clock-test-context "2026-10-14T09:05:00")))
      (multiple-value-bind (result line)
          (clock-in document heading
                    :at (make-org-time-value "2026-10-14T09:05:00")
                    :time-context context)
        (with-soft-assertions
          (expect line :to-equal "[2026-10-14 Wed 09:05]")
          (expect (document-text (edit-result-updated-document result))
                  :to-equal
                  (format nil "#+PROPERTY: CLOCK_INTO_DRAWER t~%* Task~%:LOGBOOK:~%CLOCK: [2026-10-14 Wed 09:05]~%:END:~%"))
          (expect (document-text document) :to-equal source)))))

  (it "rejects invalid drawer types and names through the public clock-in API"
    (let* ((source (format nil "* Task~%"))
           (document (parse-org source))
           (heading (edit-clock-test-single-heading document))
           (context (edit-clock-test-context "2026-10-14T09:05:00")))
      (dolist (case (list (list "" "drawer \"\"")
                          (list "BAD:NAME" "drawer \"BAD:NAME\"")
                          (list (format nil "BAD~%NAME") "drawer \"BAD\\nNAME\"")
                          (list (format nil "BAD~CNAME" #\Return)
                                "drawer \"BAD\\rNAME\"")
                          (list "END" "drawer \"END\"")
                          (list "PROPERTIES" "drawer \"PROPERTIES\"")))
        (let ((drawer (first case))
              (expected-context (second case)))
          (let ((condition
                  (edit-clock-test-error
                   (lambda ()
                     (clock-in document heading
                               :at (make-org-time-value "2026-10-14T09:05:00")
                               :time-context context :drawer drawer)))))
            (edit-clock-test-assert-clock-error
             condition :unsupported-option "Unsupported clock option"
             expected-context))))
      (let ((condition
              (edit-clock-test-condition
               (lambda ()
                 (clock-in document heading
                           :at (make-org-time-value "2026-10-14T09:05:00")
                           :time-context context :drawer nil)))))
        (expect (typep condition 'type-error) :to-be t)
        (expect (document-text document) :to-equal source))))

  (it "inserts into an empty existing drawer and finds its closing line"
    (let* ((source
             (format nil "#+PROPERTY: CLOCK_INTO_DRAWER t~%* Task~%:LOGBOOK:~%:END:~%"))
           (document (parse-org source))
           (heading (edit-clock-test-single-heading document))
           (context (edit-clock-test-context "2026-10-14T09:05:00")))
      (multiple-value-bind (result line)
          (clock-in document heading
                    :at (make-org-time-value "2026-10-14T09:05:00")
                    :time-context context)
        (with-soft-assertions
          (expect (document-text (edit-result-updated-document result))
                  :to-equal
                  (format nil "#+PROPERTY: CLOCK_INTO_DRAWER t~%* Task~%:LOGBOOK:~%~A~%:END:~%"
                          (edit-clock-test-clock-line line)))
          (expect (document-text document) :to-equal source)))))

  (it "closes a CRLF clock row without replacing its line ending"
    (let* ((line "CLOCK: [2026-10-14 Wed 09:05]")
           (source (format nil "* Task~C~%:LOGBOOK:~C~%~A~C~%:END:~%"
                           #\Return #\Return line #\Return))
           (expected
             (format nil "* Task~C~%:LOGBOOK:~C~%CLOCK: [2026-10-14 Wed 09:05]--[2026-10-14 Wed 10:37] =>  1:32~C~%:END:~%"
                     #\Return #\Return #\Return))
           (document (parse-org source))
           (heading (edit-clock-test-single-heading document))
           (context (edit-clock-test-context "2026-10-14T10:37:00"))
           (clock-start (search line source))
           (clock-end (+ clock-start (length line))))
      (multiple-value-bind (result closed-line)
          (clock-out document heading
                     :at (make-org-time-value "2026-10-14T10:37:00")
                     :time-context context)
        (let ((updated (edit-result-updated-document result))
              (span (first (edit-result-spans result))))
          (with-soft-assertions
              (expect closed-line :to-equal
                    "[2026-10-14 Wed 09:05]--[2026-10-14 Wed 10:37] => 1:32")
            (expect (document-text updated) :to-equal expected)
            (expect (write-org updated :output-type :octets)
                    :to-equalp (cl-codec-kit:string-to-octets expected
                                                               :encoding :utf-8))
            (expect (edit-span-start span) :to-equal clock-start)
            (expect (edit-span-end span) :to-equal clock-end)
            (expect (document-text document) :to-equal source))))))

  (it "closes a final clock row without a trailing newline"
    (let* ((source (format nil "* Task~%CLOCK: [2026-10-14 Wed 09:05]"))
           (expected
             (format nil "* Task~%CLOCK: [2026-10-14 Wed 09:05]--[2026-10-14 Wed 10:37] =>  1:32"))
           (document (parse-org source))
           (heading (edit-clock-test-single-heading document))
           (context (edit-clock-test-context "2026-10-14T10:37:00")))
      (multiple-value-bind (result line)
          (clock-out document heading
                     :at (make-org-time-value "2026-10-14T10:37:00")
                     :time-context context)
        (with-soft-assertions
          (expect line :to-equal
                  "[2026-10-14 Wed 09:05]--[2026-10-14 Wed 10:37] => 1:32")
          (expect (document-text (edit-result-updated-document result))
                  :to-equal expected)
          (expect (document-text document) :to-equal source)))))

  (it "lets an explicit drawer override a local property and appends to an existing drawer"
    (let* ((source (format nil "* Task~%:PROPERTIES:~%:CLOCK_INTO_DRAWER: LOCAL~%:END:~%Body~%"))
           (document (parse-org source))
           (heading (edit-clock-test-single-heading document))
           (context (edit-clock-test-context "2026-10-14T09:05:00")))
      (multiple-value-bind (result line)
          (clock-in document heading :at (make-org-time-value "2026-10-14T09:05:00")
                    :time-context context :drawer "explicit")
        (let ((edited (edit-result-updated-document result)))
          (with-soft-assertions
            (expect line :to-equal "[2026-10-14 Wed 09:05]")
            (expect (search (format nil ":EXPLICIT:~%CLOCK:") (document-text edited))
                    :to-be-truthy)
            (expect (search ":CLOCK_INTO_DRAWER: LOCAL" (document-text edited))
                    :to-be-truthy)
            (expect (search (format nil ":CLOCK_INTO_DRAWER: LOCAL~%CLOCK:")
                            (document-text edited))
                    :to-be nil))))
      (let* ((drawer-source
               (format nil "* Task~%:LOGBOOK:~%CLOCK: [2026-10-13 Tue 09:00]--[2026-10-13 Tue 09:15] =>  0:15~%:END:~%"))
             (drawer-document (parse-org drawer-source))
             (drawer-heading (edit-clock-test-single-heading drawer-document)))
        (multiple-value-bind (result line)
            (clock-in drawer-document drawer-heading
                      :at (make-org-time-value "2026-10-14T09:05:00")
                      :time-context context)
          (with-soft-assertions
            (expect (search (format nil "0:15~%~A~%:END:"
                                    (edit-clock-test-clock-line line))
                            (document-text (edit-result-updated-document result)))
                    :to-be-truthy)
            (expect (document-text drawer-document) :to-equal drawer-source))))))

  (it "uses parsed drawer properties and delegates t to LOG_INTO_DRAWER"
    (let ((cases (getf (edit-clock-test-fixture "cases/h1-clock.sexp") :cases))
          (oracle (edit-clock-test-fixture "expected/h1-clock.sexp"))
          (fixture-source (document-text (edit-clock-test-source))))
      (dolist (id '("clock-setting" "clock-property-true-uses-log-setting"))
        (let* ((case (edit-clock-test-find-case cases id))
               (expected (edit-clock-test-find-case oracle id))
               (setting-line
                 (if (string= id "clock-setting")
                     (format nil "#+PROPERTY: CLOCK_INTO_DRAWER ~A~%"
                             (getf case :clock-into-drawer))
                     (format nil "#+PROPERTY: LOG_INTO_DRAWER ~A~%"
                             (getf case :log-into-drawer))))
               (source (concatenate 'string setting-line fixture-source))
             (document (parse-org source))
               (heading (edit-clock-test-heading-path
                         document (getf case :heading-path)))
               (at (getf case :at))
               (context (edit-clock-test-context at)))
          (multiple-value-bind (result line)
              (clock-in document heading :at (make-org-time-value at)
                        :time-context context)
            (with-soft-assertions
              (expect (search (format nil ":~A:~%~A~%:END:"
                                      (getf expected :org-drawer-name)
                                      (edit-clock-test-clock-line line))
                              (document-text (edit-result-updated-document result)))
                      :to-be-truthy)
              (expect (document-text document) :to-equal source)))))))

  (it "uses an integer CLOCK_INTO_DRAWER as a target-entry threshold"
    (let* ((cases (getf (edit-clock-test-fixture
                         "cases/h1-clock-threshold.sexp") :cases))
           (oracle (edit-clock-test-fixture
                    "expected/h1-clock-threshold.sexp"))
           (source
             (parse-org
              (read-conformance-octets
               (conformance-fixture-path
                "behavior" "corpus/h1-clock-threshold.org"))))
           (source-text (document-text source)))
      (expect (length cases) :to-be 3)
      (expect (length oracle) :to-be 3)
      (dolist (case cases)
        (let* ((expected (edit-clock-test-find-case oracle (getf case :id)))
               (document (parse-org source-text))
               (heading (edit-clock-test-heading-path
                         document (getf case :heading-path)))
               (at (getf case :at))
               (context (edit-clock-test-context at)))
          (multiple-value-bind (result timestamp)
              (clock-in document heading
                        :at (make-org-time-value at)
                        :time-context context
                        :drawer (getf case :clock-into-drawer))
            (let ((updated-source
                    (document-text (edit-result-updated-document result)))
                  (line (getf expected :org-open-line))
                  (drawer-name (getf expected :org-drawer-name)))
              (with-soft-assertions
                (expect timestamp :to-equal (edit-clock-test-open-value line))
                (if drawer-name
                    (expect (search (format nil ":~A:~%~A~%:END:"
                                            drawer-name line)
                                    updated-source)
                            :to-be-truthy)
                    (progn
                      (expect (search line updated-source) :to-be-truthy)
                      (when (string= (getf case :id)
                                     "below-threshold-40-existing")
                        (expect (search ":LOGBOOK:" updated-source)
                                :to-be nil))))
                (when (string= (getf case :id)
                               "child-clocks-do-not-count-for-parent")
                  (expect (search (format nil "* Parent with child clocks~%~A~%** Child with clocks"
                                          line)
                                  updated-source)
                          :to-be-truthy))
                (expect (document-text document) :to-equal source-text))))))
      (let* ((case (edit-clock-test-find-case cases "at-threshold-41-existing"))
             (expected (edit-clock-test-find-case oracle "at-threshold-41-existing"))
             (property-source
               (format nil "#+PROPERTY: CLOCK_INTO_DRAWER 42~%~A" source-text))
             (document (parse-org property-source))
             (heading (edit-clock-test-heading-path
                       document (getf case :heading-path)))
             (at (getf case :at))
             (context (edit-clock-test-context at)))
        (multiple-value-bind (result timestamp)
            (clock-in document heading :at (make-org-time-value at)
                      :time-context context)
          (with-soft-assertions
            (expect timestamp :to-equal
                    (edit-clock-test-open-value
                     (getf expected :org-open-line)))
            (expect (search (format nil ":LOGBOOK:~%~A~%:END:"
                                    (getf expected :org-open-line))
                            (document-text (edit-result-updated-document result)))
                    :to-be-truthy)
            (expect (search ":42:" (document-text
                                     (edit-result-updated-document result)))
                    :to-be nil)
            (expect (document-text document) :to-equal property-source))))))

  (it "rejects a non-string CLOCK_INTO_DRAWER setting through clock-in"
    (let* ((source (format nil "* Task~%"))
           (document
             (parse-org source
                        :settings '(:property-settings
                                    (("CLOCK_INTO_DRAWER" . :not-a-drawer)))))
           (heading (edit-clock-test-single-heading document))
           (context (edit-clock-test-context "2026-10-14T09:05:00"))
           (condition
             (edit-clock-test-condition
              (lambda ()
                (clock-in document heading
                          :at (make-org-time-value "2026-10-14T09:05:00")
                          :time-context context)))))
      (with-soft-assertions
        (expect (typep condition 'type-error) :to-be t)
        (when (typep condition 'type-error)
          (expect (type-error-datum condition) :to-be :not-a-drawer))
        (expect (document-text document) :to-equal source))))

  (it "preserves CRLF source bytes outside the inserted drawer"
    (let* ((source (format nil "* Task~C~%Body~C~%" #\Return #\Return))
           (document (parse-org source))
           (heading (edit-clock-test-single-heading document))
           (context (edit-clock-test-context "2026-10-14T09:05:00"))
           (expected (format nil "* Task~C~%:LOGBOOK:~C~%CLOCK: [2026-10-14 Wed 09:05]~C~%:END:~C~%Body~C~%"
                             #\Return #\Return #\Return #\Return #\Return)))
      (multiple-value-bind (result line)
          (clock-in document heading :at (make-org-time-value "2026-10-14T09:05:00")
                    :time-context context)
        (declare (ignore line))
        (with-soft-assertions
          (expect (document-text (edit-result-updated-document result))
                  :to-equal expected)
          (expect (write-org (edit-result-updated-document result)
                             :output-type :octets)
                  :to-equalp (cl-codec-kit:string-to-octets expected
                                                             :encoding :utf-8))
          (expect (document-text document) :to-equal source)))))

  (it "reports missing, competing, and already-running clocks with their reasons and slots"
    (let* ((context (edit-clock-test-context "2026-10-14T10:00:00"))
           (empty (parse-org (format nil "* Task~%")))
           (empty-heading (edit-clock-test-single-heading empty))
           (missing (edit-clock-test-error
                     (lambda ()
                       (clock-out empty empty-heading
                                  :at (make-org-time-value "2026-10-14T10:00:00")
                                  :time-context context)))))
      (edit-clock-test-assert-clock-error missing :clock-not-running
                                          "No running CLOCK entry" nil)
      (let* ((source
               (format nil "* Task~%CLOCK: [2026-10-13 Tue 09:00]--[2026-10-13 Tue 09:15] =>  0:15~%"))
             (document (parse-org source))
             (heading (edit-clock-test-single-heading document))
             (closed-only (edit-clock-test-error
                           (lambda ()
                             (clock-out document heading
                                        :at (make-org-time-value "2026-10-14T10:00:00")
                                        :time-context context)))))
        (edit-clock-test-assert-clock-error closed-only :clock-not-running
                                            "No running CLOCK entry" nil)
        (expect (document-text document) :to-equal source))
      (let* ((one (parse-org
                   (format nil "* Task~%CLOCK: [2026-10-14 Wed 09:00]~%")))
             (one-heading (edit-clock-test-single-heading one))
             (line "CLOCK: [2026-10-14 Wed 09:00]")
             (already (edit-clock-test-error
                       (lambda ()
                         (clock-in one one-heading
                                   :at (make-org-time-value "2026-10-14T10:00:00")
                                   :time-context context)))))
        (edit-clock-test-assert-clock-error already :clock-already-running
                                            "A CLOCK entry is already running" line))
      (let* ((source (format nil "* Task~%CLOCK: [2026-10-14 Wed 09:00]~%CLOCK: [2026-10-14 Wed 09:30]~%"))
             (document (parse-org source))
             (heading (edit-clock-test-single-heading document))
             (multiple (edit-clock-test-error
                        (lambda ()
                          (clock-out document heading
                                     :at (make-org-time-value "2026-10-14T10:00:00")
                                     :time-context context))))
             (multiple-in (edit-clock-test-error
                           (lambda ()
                             (clock-in document heading
                                       :at (make-org-time-value "2026-10-14T10:00:00")
                                       :time-context context))))
             (expected-source source))
        (edit-clock-test-assert-clock-error multiple :multiple-running-clocks
                                            "Multiple running CLOCK entries" nil)
        (edit-clock-test-assert-clock-error multiple-in :multiple-running-clocks
                                            "Multiple running CLOCK entries" nil)
        (expect (document-text document) :to-equal expected-source))))

  (it "rejects an end before its start and requires both explicit time inputs"
    (let* ((source (format nil "* Task~%CLOCK: [2026-10-14 Wed 09:00]~%"))
           (document (parse-org source))
           (heading (edit-clock-test-single-heading document))
           (context (edit-clock-test-context "2026-10-14T08:59:00"))
           (reversed (edit-clock-test-error
                      (lambda ()
                        (clock-out document heading
                                   :at (make-org-time-value "2026-10-14T08:59:00")
                                   :time-context context))))
           (missing-at (edit-clock-test-condition
                        (lambda () (clock-in document heading
                                             :time-context context))))
           (missing-context (edit-clock-test-condition
                             (lambda ()
                               (clock-in document heading
                                         :at (make-org-time-value
                                              "2026-10-14T09:00:00")))))
           (line "2026-10-14T08:59:00 ends before 2026-10-14T09:00:00"))
      (edit-clock-test-assert-clock-error reversed :invalid-range
                                          "Invalid clock range" line)
      (with-soft-assertions
        (expect (typep missing-at 'type-error) :to-be t)
        (expect (typep missing-context 'type-error) :to-be t)
        (expect (document-text document) :to-equal source)))))

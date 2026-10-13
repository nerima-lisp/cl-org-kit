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
                            (string= (string-right-trim '(#\Return)
                                                        (node-property node :raw-value))
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
          (expect line :to-equal (getf default-expected :org-open-line))
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
                (expect line :to-equal (getf expected :org-open-line))
                (expect (search (format nil ":~A:~%~A~%:END:"
                                        (getf expected :org-drawer-name) line)
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
            (expect line :to-equal (getf expected :org-open-line))
            (expect (document-text (edit-result-updated-document result))
                    :to-equal (getf expected :org-source-after-in)))))))

  (it "closes one existing row with Org's exact duration spacing and returns its line"
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
               (clock-start (search start-line open-source))
               (clock-end (+ clock-start (length start-line)))
               (insertion-span (first (edit-result-spans started))))
          (multiple-value-bind (closed closed-line)
              (clock-out open-document open-heading
                         :at (make-org-time-value "2026-10-14T10:37:00")
                         :time-context context)
            (let* ((updated-document (edit-result-updated-document closed))
                   (replacement-span (first (edit-result-spans closed))))
              (with-soft-assertions
                (expect start-line :to-equal "CLOCK: [2026-10-14 Wed 09:05]")
                (expect (edit-span-start insertion-span) :to-equal insertion-offset)
                (expect (edit-span-end insertion-span) :to-equal insertion-offset)
                (expect closed-line :to-equal (getf closed-expected :org-closed-line))
                (expect (document-text updated-document) :to-equal expected-source)
                (expect (write-org updated-document :output-type :octets)
                        :to-equalp (cl-codec-kit:string-to-octets expected-source
                                                                   :encoding :utf-8))
                (expect (edit-span-start replacement-span) :to-equal clock-start)
                (expect (edit-span-end replacement-span) :to-equal clock-end)
                (expect (document-text document) :to-equal source))))))))

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
            (expect line :to-equal "CLOCK: [2026-10-14 Wed 09:05]")
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
            (expect (search (format nil "0:15~%~A~%:END:" line)
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
                                      (getf expected :org-drawer-name) line)
                              (document-text (edit-result-updated-document result)))
                      :to-be-truthy)
              (expect (document-text document) :to-equal source)))))))

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

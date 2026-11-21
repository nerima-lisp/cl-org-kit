;;;; t/clock-test.lisp
(in-package #:cl-org-kit/test)

(defun clock-test-heading (document title)
  (let ((found nil))
    (map-nodes (lambda (node)
                 (when (and (eq (node-type node) :headline)
                            (string= (node-property node :raw-value) title))
                   (setf found node)))
               (document-root document))
    found))

(defun clock-test-heading-path (document path)
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

(defun clock-test-first-clock-node (document)
  (let ((found nil))
    (map-nodes (lambda (node)
                 (when (and (null found) (eq (node-type node) :clock))
                   (setf found node)))
               (document-root document))
    found))

(defun clock-test-behavior-fixture (tree)
  (let ((path (asdf:system-relative-pathname
               "cl-org-kit"
               (format nil "t/fixtures/behavior/~A/g1-clock.sexp" tree))))
    (with-open-file (stream path :direction :input)
      (let* ((*read-eval* nil)
             (eof (gensym "EOF"))
             (fixture (read stream nil eof)))
        (when (eq fixture eof)
          (error "Empty G1 CLOCK fixture at ~A." path))
        (unless (eq (read stream nil eof) eof)
          (error "Extra form in G1 CLOCK fixture at ~A." path))
        fixture))))

(defun clock-test-behavior-source-octets ()
  (read-conformance-octets
   (asdf:system-relative-pathname
    "cl-org-kit" "t/fixtures/behavior/corpus/g1-clock.org")))

(defun clock-test-value (text)
  (make-org-time-value text))

(defun clock-test-context (reference &optional (zone (make-fixed-time-zone 0))
                                     (dst-policy :compatible))
  (make-time-context :reference-time (clock-test-value reference)
                     :zone zone :dst-policy dst-policy))

(defun clock-test-sum (source heading-title from to context &rest options)
  (let* ((document (parse-org source))
         (heading (clock-test-heading document heading-title)))
    (unless heading
      (error "Test CLOCK heading ~S was not parsed." heading-title))
    (values (apply #'sum-clocks document heading
                   (clock-test-value from) (clock-test-value to)
                   (append options (list :time-context context)))
            document)))

(defun clock-test-node-count (document)
  (let ((count 0))
    (map-nodes (lambda (node)
                 (declare (ignore node))
                 (incf count))
               (document-root document))
    count))

(defun clock-test-error (function)
  (handler-case
      (progn (funcall function) nil)
    (org-kit-error (condition) condition)))

(defun clock-test-condition (function)
  (handler-case
      (progn (funcall function) nil)
    (error (condition) condition)))

(defun clock-test-expect-clock-error (condition reason context message)
  (expect (typep condition 'org-clock-error) :to-be t)
  (when (typep condition 'org-clock-error)
    (expect (org-kit-error-message condition) :to-equal message)
    (expect (org-clock-error-reason condition) :to-equal reason)
    (expect (org-clock-error-context condition) :to-equal context)))

(defun clock-test-expect-time-error (condition reason context message)
  (expect (typep condition 'org-time-error) :to-be t)
  (when (typep condition 'org-time-error)
    (expect (org-kit-error-message condition) :to-equal message)
    (expect (org-time-error-reason condition) :to-equal reason)
    (expect (org-time-error-context condition) :to-equal context)))

(defun clock-test-expect-type-error (condition datum expected-type)
  (expect (typep condition 'type-error) :to-be t)
  (when (typep condition 'type-error)
    (expect (type-error-datum condition) :to-equal datum)
    (expect (type-error-expected-type condition) :to-equal expected-type)))

(describe "clock summaries"
  (it "matches the fixed Org oracle cases and preserves the source octets"
    (let* ((case-form (clock-test-behavior-fixture "cases"))
           (oracle (clock-test-behavior-fixture "expected"))
           (cases (getf case-form :cases))
           (source-octets (clock-test-behavior-source-octets))
           (source-copy (copy-seq source-octets))
           (document (parse-org source-octets)))
      (with-soft-assertions
        (expect (length cases) :to-be 11)
        (expect (length oracle) :to-be 11)
        (dolist (case cases)
          (unless (string= (getf case :id) "ambiguous-local-start")
            (let* ((id (getf case :id))
                   (expected (find id oracle :key (lambda (row) (getf row :id))
                                   :test #'string=))
                   (heading (clock-test-heading-path document
                                                     (getf case :heading-path)))
                   (context (clock-test-context (getf case :reference-time)
                                                (resolve-time-zone (getf case :zone))
                                                (getf case :dst-policy)))
                   (options
                     (append (list :include-running-p (getf case :include-running-p)
                                   :include-unscoped-p (getf case :include-unscoped-p)
                                   :time-context context)
                             (when (getf case :as-of)
                               (list :as-of (clock-test-value (getf case :as-of))))))
                   (summary
                     (apply #'sum-clocks document heading
                            (clock-test-value (getf case :from))
                            (clock-test-value (getf case :to))
                            options))
                   (actual-minutes (clock-summary-total-minutes summary))
                   (oracle-minutes (getf expected :org-clock-minutes)))
              (expect (list id (node-p heading)) :to-equal (list id t))
              (expect (list id (integerp oracle-minutes))
                      :to-equal (list id t))
              (when (string= id "fractional-clipping")
                (expect (list (clock-summary-scoped-minutes summary)
                              (clock-summary-clipped-interval-count summary))
                        :to-equal '(0 2)))
              (expect (list id actual-minutes)
                      :to-equal (list id oracle-minutes))))
          (expect (equalp source-octets source-copy) :to-be t)
          (expect (equalp source-copy
                          (write-org document :output-type :octets))
                  :to-be t)))))

  (it "uses compatible elapsed time for the ambiguous-local-start DST overlap"
    (let* ((cases (getf (clock-test-behavior-fixture "cases") :cases))
           (case (find "ambiguous-local-start" cases
                       :key (lambda (row) (getf row :id)) :test #'string=))
           (oracle (clock-test-behavior-fixture "expected"))
           (expected (find "ambiguous-local-start" oracle
                           :key (lambda (row) (getf row :id)) :test #'string=))
           (document (parse-org (clock-test-behavior-source-octets)))
           (heading (clock-test-heading-path document (getf case :heading-path)))
           (context (clock-test-context (getf case :reference-time)
                                        (resolve-time-zone (getf case :zone))
                                        (getf case :dst-policy)))
           (options
             (append (list :include-running-p (getf case :include-running-p)
                           :include-unscoped-p (getf case :include-unscoped-p)
                           :time-context context)
                     (when (getf case :as-of)
                       (list :as-of (clock-test-value (getf case :as-of))))))
           (summary
             (apply #'sum-clocks document heading
                    (clock-test-value (getf case :from))
                    (clock-test-value (getf case :to))
                    options)))
      (with-soft-assertions
        (expect (node-p heading) :to-be t)
        (expect (getf expected :org-clock-minutes) :to-be 60)
        (expect (clock-summary-total-minutes summary) :to-be 120))))

  (it "clips each interval before flooring minutes and counts positive overlaps separately"
    (let* ((source (format nil "~{~A~^~%~}"
                           '("* Scope"
                             "CLOCK: [2026-10-12 Mon 08:50]--[2026-10-12 Mon 09:20] => 0:30"
                             "CLOCK: [2026-10-12 Mon 09:30]--[2026-10-12 Mon 10:45] => 1:15"
                             "CLOCK: [2026-10-12 Mon 10:00]--[2026-10-12 Mon 10:50] => 0:50"
                             "CLOCK: [2026-10-12 Mon 11:00]--[2026-10-12 Mon 11:01] => 0:01")))
           (context (clock-test-context "2026-10-12T12:00:00"))
           (summary (clock-test-sum source "Scope"
                                    "2026-10-12T09:00:30"
                                    "2026-10-12T10:40:30"
                                    context)))
      (with-soft-assertions
        (expect (clock-summary-p summary) :to-be t)
        (expect (clock-summary-scoped-minutes summary) :to-be 129)
        (expect (clock-summary-unscoped-minutes summary) :to-be 0)
        (expect (clock-summary-total-minutes summary) :to-be 129)
        (expect (clock-summary-clipped-interval-count summary) :to-be 3))))

  (it "uses the heading subtree as scope and leaves the input document unchanged"
    (let* ((source (format nil "~{~A~^~%~}"
                           '("* Scope"
                             "CLOCK: [2026-10-12 Mon 09:00]--[2026-10-12 Mon 09:10] => 0:10"
                             "** Child"
                             "CLOCK: [2026-10-12 Mon 09:10]--[2026-10-12 Mon 09:20] => 0:10"
                             "* Outside"
                             "CLOCK: [2026-10-12 Mon 09:20]--[2026-10-12 Mon 09:50] => 0:30")))
           (document (parse-org source))
           (heading (clock-test-heading document "Scope"))
           (before (document-text document))
           (context (clock-test-context "2026-10-12T12:00:00"))
           (summary (sum-clocks document heading
                                (clock-test-value "2026-10-12T09:00:00")
                                (clock-test-value "2026-10-12T10:00:00")
                                :time-context context)))
      (with-soft-assertions
        (expect (clock-summary-scoped-minutes summary) :to-be 20)
        (expect (clock-summary-clipped-interval-count summary) :to-be 2)
        (expect (document-text document) :to-equal before))))

  (it "treats touching boundaries and equal endpoints as empty intersections"
    (let* ((source (format nil "~{~A~^~%~}"
                           '("* Scope"
                             "CLOCK: [2026-10-12 Mon 08:00]--[2026-10-12 Mon 09:00] => 1:00"
                             "CLOCK: [2026-10-12 Mon 10:00]--[2026-10-12 Mon 11:00] => 1:00"
                             "CLOCK: => 0:15")))
           (context (clock-test-context "2026-10-12T12:00:00"))
           (touching (clock-test-sum source "Scope"
                                     "2026-10-12T09:00:00"
                                     "2026-10-12T10:00:00" context))
           (empty (clock-test-sum source "Scope"
                                  "2026-10-12T10:00:00"
                                  "2026-10-12T10:00:00" context)))
      (with-soft-assertions
        (expect (list (clock-summary-scoped-minutes touching)
                      (clock-summary-unscoped-minutes touching)
                      (clock-summary-total-minutes touching)
                      (clock-summary-clipped-interval-count touching))
                :to-equal '(0 15 15 0))
        (expect (list (clock-summary-scoped-minutes empty)
                      (clock-summary-unscoped-minutes empty)
                      (clock-summary-total-minutes empty)
                      (clock-summary-clipped-interval-count empty))
                :to-equal '(0 15 15 0)))))

  (it "includes duration-only rows by default and lets callers opt out"
    (let* ((source (format nil "~{~A~^~%~}"
                           '("* Scope" "CLOCK: => 1:15")))
           (context (clock-test-context "2026-10-12T10:00:00"))
           (included (clock-test-sum source "Scope"
                                     "2026-10-12T09:00:00"
                                     "2026-10-12T10:00:00" context))
           (excluded (clock-test-sum source "Scope"
                                     "2026-10-12T09:00:00"
                                     "2026-10-12T10:00:00" context
                                     :include-unscoped-p nil)))
      (with-soft-assertions
        (expect (list (clock-summary-scoped-minutes included)
                      (clock-summary-unscoped-minutes included)
                      (clock-summary-total-minutes included))
                :to-equal '(0 75 75))
        (expect (list (clock-summary-unscoped-minutes excluded)
                      (clock-summary-total-minutes excluded))
                :to-equal '(75 0)))))

  (it "removes CR from CRLF CLOCK rows before parsing their durations"
    (let* ((source (format nil "* Scope~%CLOCK: => 0:10~C~%" #\Return))
           (document (parse-org source))
           (clock (clock-test-first-clock-node document))
           (context (clock-test-context "2026-10-12T10:00:00"))
           (summary
             (sum-clocks document (clock-test-heading document "Scope")
                         (clock-test-value "2026-10-12T09:00:00")
                         (clock-test-value "2026-10-12T10:00:00")
                         :time-context context)))
      (with-soft-assertions
        (expect (find #\Return (node-text clock)) :to-be-truthy)
        (expect (clock-summary-scoped-minutes summary) :to-be 0)
        (expect (clock-summary-unscoped-minutes summary) :to-be 10)
        (expect (clock-summary-total-minutes summary) :to-be 10))))

  (it "excludes open entries by default and uses the context reference as their end when opted in"
    (let* ((source (format nil "~{~A~^~%~}"
                           '("* Scope" "CLOCK: [2026-10-12 Mon 09:00]")))
           (context (clock-test-context "2026-10-12T10:00:00"))
           (excluded (clock-test-sum source "Scope"
                                     "2026-10-12T09:00:00"
                                     "2026-10-12T11:00:00" context))
           (included (clock-test-sum source "Scope"
                                     "2026-10-12T09:00:00"
                                     "2026-10-12T11:00:00" context
                                     :include-running-p t))
           (asserted (clock-test-sum source "Scope"
                                     "2026-10-12T09:00:00"
                                     "2026-10-12T11:00:00" context
                                     :include-running-p t
                                     :as-of (clock-test-value "2026-10-12T10:00:00"))))
      (with-soft-assertions
        (expect (clock-summary-total-minutes excluded) :to-be 0)
        (expect (list (clock-summary-total-minutes included)
                      (clock-summary-clipped-interval-count included))
                :to-equal '(60 1))
        (expect (clock-summary-total-minutes asserted) :to-be 60))))

  (it "resolves daylight-saving gaps and overlaps through the time context"
    (let* ((source (format nil "~{~A~^~%~}"
                           '("* Spring"
                             "CLOCK: [2024-03-10 Sun 01:30]--[2024-03-10 Sun 03:30] => 2:00"
                             "* Gap"
                             "CLOCK: [2024-03-10 Sun 02:30]--[2024-03-10 Sun 03:30] => 1:00"
                             "* Overlap"
                             "CLOCK: [2024-11-03 Sun 01:30]--[2024-11-03 Sun 02:30] => 1:00")))
           (zone (resolve-time-zone "America/New_York"))
           (spring-context (clock-test-context "2024-03-10T12:00:00" zone))
           (strict-gap-context
             (clock-test-context "2024-03-10T12:00:00" zone :strict))
           (overlap-context (clock-test-context "2024-11-03T12:00:00" zone))
           (spring (clock-test-sum source "Spring"
                                   "2024-03-10T00:00:00"
                                   "2024-03-10T23:59:00" spring-context))
           (gap (clock-test-sum source "Gap"
                                "2024-03-10T00:00:00"
                                "2024-03-10T23:59:00" spring-context))
           (gap-error (clock-test-error
                       (lambda ()
                         (clock-test-sum source "Gap"
                                         "2024-03-10T00:00:00"
                                         "2024-03-10T23:59:00"
                                         strict-gap-context))))
           (overlap (clock-test-sum source "Overlap"
                                    "2024-11-03T00:00:00"
                                    "2024-11-03T23:59:00" overlap-context)))
      (with-soft-assertions
        (expect (clock-summary-total-minutes spring) :to-be 60)
        (expect (list (clock-summary-total-minutes gap)
                      (clock-summary-clipped-interval-count gap))
                :to-equal '(0 0))
        (clock-test-expect-time-error gap-error :dst-gap
                                      "2024-03-10T02:30:00"
                                      "Nonexistent local time")
        (expect (clock-summary-total-minutes overlap) :to-be 120))))

  (it "sums timestamps and naked durations while warning about invalid ranges"
    (let* ((source (format nil "~{~A~^~%~}"
                           '("* Scope"
                             "CLOCK: [2026-10-11 Sun 23:30]--[2026-10-12 Mon 09:30] => 1:0"
                             "CLOCK: [2026-10-12 Mon 09:40]--[2026-10-12 Mon 10:10]"
                             "CLOCK: [2026-10-12 Mon 10:30]--[2026-10-12 Mon 10:00] => 0:30"
                             "CLOCK: [2026-10-12 Mon 08:00]--[2026-10-12 Mon 08:30] => 0:30"
                             "CLOCK: [2026-10-12 Mon 11:00]--[2026-10-12 Mon 12:00] => 1:00"
                             "CLOCK: [2026-10-12 Mon 09:00]"
                             "CLOCK: => 1:0"
                             "CLOCK: => 1:60"
                             "CLOCK: [2026-13-99 Mon 09:00]--[2026-13-99 Mon 10:00] => 0:10"
                             "CLOCK: [2026-10-12 Mon]--[2026-10-13 Tue]")))
           (context (clock-test-context "2026-10-12T12:00:00"))
           (warnings nil)
           (summary
             (handler-bind ((warning (lambda (condition)
                                       (push (princ-to-string condition) warnings)
                                       (muffle-warning condition))))
               (clock-test-sum source "Scope" "2026-10-12T09:00:00"
                               "2026-10-12T10:00:00" context))))
      (with-soft-assertions
        (expect (clock-summary-scoped-minutes summary) :to-be 50)
        (expect (clock-summary-unscoped-minutes summary) :to-be 180)
        (expect (clock-summary-total-minutes summary) :to-be 230)
        (expect (clock-summary-clipped-interval-count summary) :to-be 2)
        (expect (length warnings) :to-be 2)
        (expect (mapcar (lambda (warning) (not (null (search "line 10" warning)))) warnings) :to-equal (list nil t)))))
  (it "normalizes extended hours before clipping intervals"
    (let ((context (clock-test-context "2026-10-12T12:00:00")))
      (dolist (case '(("CLOCK: [2026-10-11 Sun 24:00]--[2026-10-12 Mon 01:00]"
                       "2026-10-12T00:00:00" "2026-10-12T06:00:00" 60)
                      ("CLOCK: [2026-10-11 Sun 29:00]--[2026-10-12 Mon 06:00]"
                       "2026-10-12T00:00:00" "2026-10-12T06:00:00" 60)
                      ("CLOCK: [2026-10-11 Sun 23:30]--[2026-10-11 Sun 24:30]"
                       "2026-10-12T00:00:00" "2026-10-12T01:00:00" 30)
                      ("CLOCK: [2026-10-11 Sun 28:30]--[2026-10-11 Sun 29:30]"
                       "2026-10-12T05:00:00" "2026-10-12T06:00:00" 30)))
        (destructuring-bind (line from to expected) case
          (let ((summary (clock-test-sum (format nil "* Scope~%~A" line)
                                         "Scope" from to context)))
            (expect (clock-summary-total-minutes summary) :to-be expected))))
      (let* ((document (parse-org (format nil "* Scope~%CLOCK: [2026-10-11 Sun 24:00]--[2026-10-12 Mon 01:00]")))
             (condition (clock-test-error
                         (lambda () (check-clock-consistency document :time-context context)))))
        (expect (typep condition 'org-time-error) :to-be t))))
  (it "normalizes extended hours across month, year and DST boundaries"
    (let* ((context (clock-test-context "2027-01-01T12:00:00"))
           (month (clock-test-sum
                   (format nil "* Scope~%CLOCK: [2026-10-31 Sat 29:00]--[2026-11-01 Sun 06:00]")
                   "Scope" "2026-11-01T00:00:00" "2026-11-01T07:00:00" context))
           (year (clock-test-sum
                  (format nil "* Scope~%CLOCK: [2026-12-31 Thu 24:00]--[2027-01-01 Fri 01:00]")
                  "Scope" "2027-01-01T00:00:00" "2027-01-01T02:00:00" context))
           (dst-context (clock-test-context "2024-03-10T12:00:00"
                                            (resolve-time-zone "America/New_York")))
           (dst (clock-test-sum
                 (format nil "* Scope~%CLOCK: [2024-03-09 Sat 24:00]--[2024-03-09 Sat 29:00]")
                 "Scope" "2024-03-10T00:00:00" "2024-03-10T06:00:00" dst-context)))
      (with-soft-assertions
        (expect (clock-summary-total-minutes month) :to-be 60)
        (expect (clock-summary-total-minutes year) :to-be 60)
        (expect (clock-summary-total-minutes dst) :to-be 240))))
  (it "warns for invalid dates and minutes in extended-hour ranges"
    (let ((context (clock-test-context "2026-10-12T12:00:00"))
          (warnings nil))
      (let ((summary
              (handler-bind ((warning (lambda (condition)
                                        (push (princ-to-string condition) warnings)
                                        (muffle-warning condition))))
                (clock-test-sum
                 (format nil "* Scope~%CLOCK: [2026-13-99 Mon 24:00]--[2026-10-12 Mon 01:00]~%CLOCK: [2026-10-11 Sun 29:99]--[2026-10-12 Mon 06:00]")
                 "Scope" "2026-10-12T00:00:00" "2026-10-12T06:00:00" context))))
        (with-soft-assertions
          (expect (clock-summary-total-minutes summary) :to-be 0)
          (expect (length warnings) :to-be 2)))))

  (it "ignores active, diary and incomplete timestamps with durations"
    (let* ((context (clock-test-context "2026-10-12T12:00:00"))
           (source (format nil "* Scope~%CLOCK: <%%(diary-float t 4 2)> => 0:10~%CLOCK: [2026-10-12 Mon 09:00] => 1:00~%CLOCK:=>1:00~%CLOCK: <2026-10-12 Mon 09:00>--<2026-10-12 Mon 10:00> => 1:00"))
           (summary (handler-bind ((warning #'muffle-warning))
                      (clock-test-sum source "Scope" "2026-10-12T09:00:00"
                                      "2026-10-12T10:00:00" context))))
      (expect (clock-summary-total-minutes summary) :to-be 0)))

  (it "reads naked durations like org-clock-sum"
    (let ((context (clock-test-context "2026-10-12T12:00:00")))
      (dolist (case '(("CLOCK: => 1:30" 90)
                      ("CLOCK: => 1:5" 65)
                      ("CLOCK: => 1:30 trailing" 90)))
        (destructuring-bind (line expected) case
          (let ((summary (clock-test-sum (format nil "* Scope~%~A" line)
                                         "Scope" "2026-10-12T09:00:00"
                                         "2026-10-12T10:00:00" context)))
            (expect (clock-summary-unscoped-minutes summary) :to-be expected))))))
  (it "warns and skips invalid CLOCK data"
    (let* ((context (clock-test-context "2026-10-12T12:00:00"))
           (source (format nil "* Scope~%CLOCK: => x:10~%CLOCK: invalid~%CLOCK:"))
           (warnings nil)
           (summary (handler-bind ((warning (lambda (condition)
                                              (push (princ-to-string condition) warnings)
                                              (muffle-warning condition))))
                      (clock-test-sum source "Scope" "2026-10-12T09:00:00"
                                      "2026-10-12T10:00:00" context))))
      (with-soft-assertions
        (expect (clock-summary-total-minutes summary) :to-be 0)
        (expect (length warnings) :to-be 3))))
  (it "preserves strict CLOCK parsing in consistency checks"
    (let ((context (clock-test-context "2026-10-12T12:00:00")))
      (flet ((check-line (line)
               (check-clock-consistency (parse-org (format nil "* Scope~%~A" line))
                                        :time-context context)))
        (with-soft-assertions
          (dolist (line '("CLOCK: => x:10" "CLOCK: => :10"
                          "CLOCK: invalid" "CLOCK: <%%(diary-float t 4 2)>"))
            (let ((condition (clock-test-error (lambda () (check-line line)))))
              (clock-test-expect-clock-error condition :malformed-clock line
                                             "Malformed CLOCK entry")))
          (expect (check-line "CLOCK: <%%(diary-float t 4 2)> => 0:10") :to-equal nil)
          (expect (check-line "CLOCK: [2026-13-99 Mon 09:00]--[2026-13-99 Mon 10:00] => 0:10") :to-equal nil)
          (expect (check-line "CLOCK: [2026-10-12 Mon 09:00] => 0:10") :to-equal nil)
          (clock-test-expect-time-error
           (clock-test-error (lambda () (check-line "CLOCK: [2026-13-99 Mon 09:00]")))
           :invalid-time "timestamp has no valid start" "Invalid date or time")))))
  (it "warns about an invalid running timestamp when included"
    (let ((warnings nil)
          (context (clock-test-context "2026-10-12T12:00:00")))
      (let ((summary
              (handler-bind ((warning (lambda (condition)
                                        (push (princ-to-string condition) warnings)
                                        (muffle-warning condition))))
                (clock-test-sum (format nil "* Scope~%CLOCK: [2026-13-99 Mon 09:00]")
                                "Scope" "2026-10-12T09:00:00" "2026-10-12T10:00:00"
                                context :include-running-p t))))
        (with-soft-assertions
          (expect (clock-summary-total-minutes summary) :to-be 0)
          (expect (length warnings) :to-be 1)
          (expect (search "line 2" (first warnings)) :to-be-truthy)))))

  (it "skips a DST gap interval whose compatible endpoints reverse"
    (let* ((source (format nil "* Scope~%CLOCK: [2024-03-10 Sun 02:30]--[2024-03-10 Sun 03:00] => 0:30"))
           (context (clock-test-context "2024-03-10T12:00:00"
                                        (resolve-time-zone "America/New_York")))
           (summary (clock-test-sum source "Scope" "2024-03-10T00:00:00"
                                    "2024-03-10T23:59:00" context)))
      (expect (clock-summary-total-minutes summary) :to-be 0)))

  (it "reports invalid ranges, mismatched as-of values, and unsupported options"
    (let* ((source (format nil "* Scope~%CLOCK: [2026-10-12 Mon 09:00]"))
           (context (clock-test-context "2026-10-12T10:00:00"))
           (reversed-range
             (clock-test-error
              (lambda ()
                (clock-test-sum source "Scope"
                                "2026-10-12T11:00:00" "2026-10-12T10:00:00"
                                context))))
           (mismatched-as-of
             (clock-test-error
              (lambda ()
                (clock-test-sum source "Scope"
                                "2026-10-12T09:00:00" "2026-10-12T11:00:00"
                                context :as-of (clock-test-value "2026-10-12T10:01:00")))))
           (unsupported-option
             (clock-test-error
              (lambda ()
                (clock-test-sum source "Scope"
                                "2026-10-12T09:00:00" "2026-10-12T11:00:00"
                                context :unknown-option t)))))
      (with-soft-assertions
        (clock-test-expect-clock-error reversed-range :invalid-range
                                       "FROM follows TO" "Invalid clock range")
        (clock-test-expect-clock-error mismatched-as-of :invalid-range
                                       "as-of differs from time-context"
                                       "Invalid clock range")
        (clock-test-expect-clock-error unsupported-option :unsupported-option
                                       "UNKNOWN-OPTION"
                                       "Unsupported clock option"))))

  (it "validates option shape, option values, the time context, and time arguments"
    (let* ((source (format nil "* Scope~%CLOCK: [2026-10-12 Mon 09:00]"))
           (document (parse-org source))
           (heading (clock-test-heading document "Scope"))
           (from (clock-test-value "2026-10-12T09:00:00"))
           (to (clock-test-value "2026-10-12T11:00:00"))
           (context (clock-test-context "2026-10-12T10:00:00"))
           (capture
             (lambda (from-value to-value &rest options)
               (clock-test-condition
                (lambda ()
                  (apply #'sum-clocks document heading from-value to-value options)))))
           (odd-options (funcall capture from to :include-running-p))
           (duplicate-key (funcall capture from to
                                   :include-running-p nil :include-running-p t
                                   :time-context context))
           (non-keyword-key (funcall capture from to t t))
           (running-flag (funcall capture from to :include-running-p "yes"
                                  :time-context context))
           (unscoped-flag (funcall capture from to :include-unscoped-p 1
                                   :time-context context))
           (missing-context (funcall capture from to))
           (wrong-context (funcall capture from to :time-context "context"))
           (wrong-as-of (funcall capture from to :time-context context
                                 :as-of "2026-10-12T10:00:00"))
           (wrong-from (funcall capture "2026-10-12T09:00:00" to
                                :time-context context))
           (wrong-to (funcall capture from "2026-10-12T11:00:00"
                              :time-context context))
           (missing-root-error
             (let ((empty-document (make-document "")))
               (clock-test-error
                (lambda ()
                  (sum-clocks empty-document heading from to
                              :time-context context))))))
      (with-soft-assertions
        (clock-test-expect-clock-error odd-options :unsupported-option
                                       "missing option value"
                                       "Unsupported clock option")
        (clock-test-expect-clock-error duplicate-key :unsupported-option
                                       "INCLUDE-RUNNING-P"
                                       "Unsupported clock option")
        (clock-test-expect-clock-error non-keyword-key :unsupported-option
                                       "T" "Unsupported clock option")
        (clock-test-expect-type-error running-flag "yes" 'boolean)
        (clock-test-expect-type-error unscoped-flag 1 'boolean)
        (clock-test-expect-type-error missing-context nil 'org-time-context)
        (clock-test-expect-type-error wrong-context "context" 'org-time-context)
        (clock-test-expect-type-error wrong-as-of "2026-10-12T10:00:00"
                                      'org-time-value)
        (clock-test-expect-type-error wrong-from "2026-10-12T09:00:00"
                                      'org-time-value)
        (clock-test-expect-type-error wrong-to "2026-10-12T11:00:00"
                                      'org-time-value)
        (expect (typep missing-root-error 'org-kit-error) :to-be t)
        (expect (org-kit-error-message missing-root-error)
                :to-equal "Heading does not belong to DOCUMENT"))))

  (it "rejects a running clock that starts after the context reference instant"
    (let* ((source (format nil "* Scope~%CLOCK: [2026-10-12 Mon 11:00]"))
           (context (clock-test-context "2026-10-12T10:00:00"))
           (condition
             (clock-test-error
              (lambda ()
                (clock-test-sum source "Scope"
                                "2026-10-12T09:00:00" "2026-10-12T12:00:00"
                                context :include-running-p t)))))
      (with-soft-assertions
        (clock-test-expect-clock-error condition :invalid-range
                                       "CLOCK: [2026-10-12 Mon 11:00]"
                                       "Invalid clock range"))))

  (it "validates document and heading arguments before clock options"
    (let* ((document (parse-org "* Scope"))
           (heading (clock-test-heading document "Scope"))
           (foreign-document (parse-org "* Foreign"))
           (foreign-heading (clock-test-heading foreign-document "Foreign"))
           (non-headline (make-node :paragraph))
           (from (clock-test-value "2026-10-12T09:00:00"))
           (to (clock-test-value "2026-10-12T10:00:00"))
           (invalid-document
             (clock-test-condition
              (lambda () (sum-clocks "not a document" heading from to))))
           (invalid-heading
             (clock-test-condition
              (lambda () (sum-clocks document "not a node" from to))))
           (non-headline-error
             (clock-test-condition
              (lambda () (sum-clocks document non-headline from to))))
           (foreign-heading-error
             (clock-test-condition
              (lambda () (sum-clocks document foreign-heading from to)))))
      (with-soft-assertions
        (clock-test-expect-type-error invalid-document "not a document" 'document)
        (clock-test-expect-type-error invalid-heading "not a node" 'node)
        (expect (typep non-headline-error 'type-error) :to-be t)
        (when (typep non-headline-error 'type-error)
          (expect (eq (type-error-datum non-headline-error) non-headline)
                  :to-be t)
          (expect (type-error-expected-type non-headline-error) :to-equal 'node))
        (expect (typep foreign-heading-error 'org-kit-error) :to-be t)
        (when (typep foreign-heading-error 'org-kit-error)
          (expect (org-kit-error-message foreign-heading-error)
                  :to-equal "Heading does not belong to DOCUMENT")))))

  (it "checks max-node-count immediately below, at, and above a clock document's node count"
    (let* ((source (format nil "* ~%CLOCK: => 0:02"))
           (baseline (parse-org source))
           (node-count (clock-test-node-count baseline))
           (limits (make-limits :max-node-count node-count))
           (below-limit-error
             (handler-case
                 (progn (parse-org source :limits
                                   (make-limits :max-node-count (1- node-count)))
                        nil)
               (org-resource-limit-error (condition) condition)))
           (at (parse-org source :limits limits))
           (above (parse-org source :limits
                             (make-limits :max-node-count (1+ node-count))))
           (context (clock-test-context "2026-10-12T10:00:00"))
           (mutated (parse-org source :limits limits))
           (oversized-error
             (progn
               (append-child (document-root mutated) (make-node :paragraph))
               (handler-case
                   (progn
                     (sum-clocks mutated (clock-test-heading mutated "")
                                 (clock-test-value "2026-10-12T09:00:00")
                                 (clock-test-value "2026-10-12T11:00:00")
                                 :time-context context)
                     nil)
                 (org-resource-limit-error (condition) condition))))
           (mutated-node-count (clock-test-node-count mutated)))
      (with-soft-assertions
        (expect (>= node-count 2) :to-be t)
        (expect (typep below-limit-error 'org-resource-limit-error) :to-be t)
        (when (typep below-limit-error 'org-resource-limit-error)
          (expect (org-kit-error-message below-limit-error)
                  :to-equal "Resource limit exceeded")
          (expect (org-resource-limit-error-limit-name below-limit-error)
                  :to-be :max-node-count)
          (expect (org-resource-limit-error-limit below-limit-error)
                  :to-be (1- node-count))
          (expect (org-resource-limit-error-actual below-limit-error)
                  :to-be node-count))
        (expect (clock-test-node-count at) :to-be node-count)
        (expect (clock-test-node-count above) :to-be node-count)
        (expect mutated-node-count :to-be (1+ node-count))
        (expect (typep oversized-error 'org-resource-limit-error) :to-be t)
        (when (typep oversized-error 'org-resource-limit-error)
          (expect (org-kit-error-message oversized-error)
                  :to-equal "Resource limit exceeded")
          (expect (org-resource-limit-error-limit-name oversized-error)
                  :to-be :max-node-count)
          (expect (org-resource-limit-error-limit oversized-error)
                  :to-be node-count)
          (expect (org-resource-limit-error-actual oversized-error)
                  :to-be (1+ node-count)))
        (dolist (document (list at above))
          (let ((summary
                  (sum-clocks document (clock-test-heading document "")
                              (clock-test-value "2026-10-12T09:00:00")
                              (clock-test-value "2026-10-12T11:00:00")
                              :time-context context)))
            (expect (clock-summary-unscoped-minutes summary) :to-be 2))))))

  (it "builds summaries with defaults and validated non-negative integer fields"
    (let ((defaults (make-clock-summary))
          (summary (make-clock-summary :scoped-minutes 3 :unscoped-minutes 2
                                       :total-minutes 5
                                       :clipped-interval-count 1)))
      (with-soft-assertions
        (expect (list (clock-summary-scoped-minutes defaults)
                      (clock-summary-unscoped-minutes defaults)
                      (clock-summary-total-minutes defaults)
                      (clock-summary-clipped-interval-count defaults))
                :to-equal '(0 0 0 0))
        (expect (clock-summary-p summary) :to-be t)
        (expect (clock-summary-scoped-minutes summary) :to-be 3)
        (expect (clock-summary-unscoped-minutes summary) :to-be 2)
        (expect (clock-summary-total-minutes summary) :to-be 5)
        (expect (clock-summary-clipped-interval-count summary) :to-be 1)
        (signals type-error (make-clock-summary :total-minutes -1)))))

  (it "keeps clock errors under ORG-KIT-ERROR with stable reason and context readers"
    (let* ((condition (make-condition 'org-clock-error
                                      :message "Malformed CLOCK entry"
                                      :reason :malformed-clock :context "CLOCK: => 1:60"))
           (bare (make-condition 'org-clock-error :message "Clock error")))
      (with-soft-assertions
        (expect (subtypep 'org-clock-error 'org-kit-error) :to-be t)
        (expect (org-clock-error-reason condition) :to-be :malformed-clock)
        (expect (org-clock-error-context condition) :to-equal "CLOCK: => 1:60")
        (expect (org-kit-error-message condition) :to-equal "Malformed CLOCK entry")
        (expect (search "Malformed CLOCK entry (malformed-clock)"
                        (princ-to-string condition))
                :to-be 0)
        (expect (org-clock-error-reason bare) :to-be nil)
        (expect (org-clock-error-context bare) :to-be nil)))))

(describe "clock consistency and timer items"
  (it "reports reversed, overlapping, open, and overlong clock intervals in source order"
    (let* ((source
             (format nil "~{~A~^~%~}"
                     '("* Task"
                       "CLOCK: [2026-10-12 Mon 09:00]--[2026-10-12 Mon 10:30] => 1:30"
                       "CLOCK: [2026-10-12 Mon 10:00]--[2026-10-12 Mon 10:15] => 0:15"
                       "CLOCK: [2026-10-12 Mon 10:15]--[2026-10-12 Mon 10:45] => 0:30"
                       "CLOCK: [2026-10-12 Mon 11:00]"
                       "CLOCK: [2026-10-12 Mon 13:00]--[2026-10-12 Mon 12:00] => 1:00"
                       "CLOCK: => 0:30"
                       "CLOCK: [2026-10-12 Mon 12:00]--[2026-10-12 Mon 12:00] => 0:00")))
           (document (parse-org source))
           (context (clock-test-context "2026-10-12T14:00:00"))
           (issues (check-clock-consistency
                    document :time-context context :max-duration-minutes 60))
           (rows
             (mapcar (lambda (issue)
                       (list (clock-consistency-issue-reason issue)
                             (clock-consistency-issue-line issue)
                             (clock-consistency-issue-other-line issue)
                             (clock-consistency-issue-duration-minutes issue)))
                     issues)))
      (expect rows :to-equal
              '((:overlap 2 3 nil)
                (:overlap 2 4 nil)
                (:too-long 2 nil 90)
                (:open-clock 5 nil 180)
                (:too-long 5 nil 180)
                (:end-before-start 6 nil nil)))
      (expect (every #'clock-consistency-issue-p issues) :to-be t)))
  (it "covers clock consistency validation and normalized interval edges"
      (let* ((context (clock-test-context "2024-03-10T11:00:00"))
             (future-document
              (parse-org
               (format nil "* Task~%CLOCK: [2024-03-10 Sun 12:00]")))
             (future-error
              (handler-case
                  (progn
                    (check-clock-consistency future-document :time-context context)
                    nil)
                (org-clock-error (condition) condition)))
             (overlap-document
              (parse-org
               (format nil
                       "* Task~%CLOCK: [2026-10-12 Mon 10:00]--[2026-10-12 Mon 10:30]~%CLOCK: [2026-10-12 Mon 09:30]--[2026-10-12 Mon 10:15]")))
             (overlap-issues
              (check-clock-consistency
               overlap-document
               :time-context (clock-test-context "2026-10-12T11:00:00")))
             (dst-document
              (parse-org
               (format nil
                       "* Task~%CLOCK: [2024-03-10 Sun 02:30]--[2024-03-10 Sun 03:00] => 0:30")))
             (dst-context
              (clock-test-context
               "2024-03-10T04:00:00"
               (org-kit::resolve-time-zone "America/New_York")))
             (dst-issues (check-clock-consistency dst-document :time-context dst-context)))
        (signals type-error
                 (make-clock-consistency-issue :reason :unknown :line 1))
        (signals type-error
                 (make-clock-consistency-issue :reason :overlap))
        (signals type-error
                 (make-clock-consistency-issue :reason :overlap :line 1 :other-line 0))
        (signals type-error
                 (make-clock-consistency-issue
                  :reason :overlap :line 1 :duration-minutes -1))
        (signals type-error
                 (make-clock-consistency-issue
                  :reason :overlap :line 1 :duration-minutes "bad"))
        (signals type-error
                 (check-clock-consistency
                  (make-document "* Task") :time-context context))
        (signals type-error
                 (check-clock-consistency (parse-org "* Task")))
        (expect (org-clock-error-reason future-error) :to-be :invalid-range)
        (expect (mapcar
                 (lambda (issue)
                   (list (clock-consistency-issue-reason issue)
                         (clock-consistency-issue-line issue)
                         (clock-consistency-issue-other-line issue)))
                 overlap-issues)
                :to-equal '((:overlap 2 3)))
        (expect (mapcar #'clock-consistency-issue-reason dst-issues)
                :to-equal '(:end-before-start))
        (expect
         (org-kit::clock-consistency-issue-before-p
          (make-clock-consistency-issue :reason :overlap :line 1 :other-line 2)
          (make-clock-consistency-issue :reason :overlap :line 1 :other-line 2))
         :to-be nil)
        (let* ((clock-without-end
                (make-node
                 :clock
                 :properties
                 (list :value
                       (make-node
                        :timestamp
                        :properties
                        (list :type :active-range
                              :year-start 2024 :month-start 3 :day-start 10
                              :hour-start 1 :minute-start 0)))))
               (clock-with-ordered-range
                (make-node
                 :clock
                 :properties
                 (list :value
                       (make-node
                        :timestamp
                        :properties
                        (list :type :active-range
                              :year-start 2024 :month-start 3 :day-start 10
                              :hour-start 1 :minute-start 0
                              :year-end 2024 :month-end 3 :day-end 10
                              :hour-end 2 :minute-end 0))))))
          (expect
           (org-kit::clock-consistency-reversed-range clock-without-end context)
           :to-be nil)
          (expect
           (org-kit::clock-consistency-reversed-range clock-with-ordered-range context)
           :to-be nil))))

  (it "signals named clock errors for malformed entries and an invalid duration threshold"
    (let* ((context (clock-test-context "2026-10-12T10:00:00"))
           (malformed-document (parse-org (format nil "* Task~%CLOCK: => 1:60")))
           (malformed
             (clock-test-error
              (lambda ()
                (check-clock-consistency malformed-document
                                         :time-context context))))
           (invalid-threshold
             (clock-test-error
              (lambda ()
                (check-clock-consistency (parse-org "* Task")
                                         :time-context context
                                         :max-duration-minutes -1)))))
      (clock-test-expect-clock-error malformed :malformed-clock "CLOCK: => 1:60"
                                     "Malformed CLOCK entry")
      (clock-test-expect-clock-error
       invalid-threshold :invalid-range
       "max-duration-minutes must be a non-negative integer"
       "Invalid clock range")))

  (it "caps clock consistency findings before emitting an unbounded overlap set"
    (let* ((source
             (with-output-to-string (stream)
               (write-line "* Task" stream)
               (dotimes (index 20000)
                 (declare (ignore index))
                 (write-line
                  "CLOCK: [2026-10-12 Mon 09:00]--[2026-10-12 Mon 10:00] => 1:00"
                  stream))))
           (document
             (parse-org source :limits (make-limits :max-clock-consistency-issues 10)))
           (condition
             (handler-case
                 (progn
                   (check-clock-consistency
                    document :time-context
                    (clock-test-context "2026-10-12T11:00:00"))
                   nil)
               (org-resource-limit-error (condition) condition))))
      (expect (typep condition 'org-resource-limit-error) :to-be t)
      (when condition
        (expect (org-resource-limit-error-limit-name condition)
                :to-be :max-clock-consistency-issues)
        (expect (org-resource-limit-error-limit condition) :to-be 10)
        (expect (org-resource-limit-error-actual condition) :to-be 11))))

  (it "inserts an Org timer item using the explicit reference instant and preserves source bytes"
    (let* ((source (format nil "* Task~%既存の本文~%"))
           (source-octets (cl-codec-kit:string-to-octets source :encoding :utf-8))
           (document (parse-org source-octets :encoding :utf-8))
           (heading (clock-test-heading document "Task"))
           (context (clock-test-context "2025-01-01T10:01:23"))
           (start (clock-test-value "2025-01-01T10:00:00"))
           (original-octets (write-org document :output-type :octets))
           (expected-source
             (format nil "* Task~%- 0:01:23 :: Review~%既存の本文~%"))
           (expected-octets
             (cl-codec-kit:string-to-octets expected-source :encoding :utf-8)))
      (expect (timer-item-value start context) :to-equal "0:01:23")
      (multiple-value-bind (result value)
          (insert-timer-item document heading start "Review"
                             :time-context context)
        (let* ((updated (edit-result-updated-document result))
               (span (first (edit-result-spans result))))
          (expect (edit-result-p result) :to-be t)
          (expect value :to-equal "0:01:23")
          (expect (write-org updated :output-type :octets) :to-equalp expected-octets)
          (expect (edit-span-start span) :to-be (length (format nil "* Task~%")))
          (expect (edit-span-end span) :to-be (length (format nil "* Task~%")))
          (expect (write-org document :output-type :octets) :to-equalp original-octets)))
      (let* ((plain-document (parse-org "* Task"))
             (plain-heading (clock-test-heading plain-document "Task"))
             (plain-context (clock-test-context "2025-01-01T10:01:00")))
        (multiple-value-bind (result value)
            (insert-timer-item plain-document plain-heading
                               (clock-test-value "2025-01-01T10:00:00")
                               "Review" :time-context plain-context)
          (expect value :to-equal "0:01:00")
          (expect (document-text (edit-result-updated-document result))
                  :to-equal (format nil "* Task~%- 0:01:00 :: Review"))))
      (let* ((crlf (format nil "* Task~C~CBody~C~C"
                           #\Return #\Newline #\Return #\Newline))
             (crlf-document (parse-org crlf))
             (crlf-heading (clock-test-heading crlf-document "Task"))
             (crlf-context (clock-test-context "2025-01-01T10:01:00")))
        (multiple-value-bind (result value)
            (insert-timer-item crlf-document crlf-heading
                               (clock-test-value "2025-01-01T10:00:00")
                               "Review" :time-context crlf-context)
          (expect value :to-equal "0:01:00")
          (expect (document-text (edit-result-updated-document result))
                  :to-equal
                  (format nil "* Task~C~C- 0:01:00 :: Review~C~CBody~C~C"
                          #\Return #\Newline #\Return #\Newline
                          #\Return #\Newline))))
      (let* ((metadata-document
               (parse-org
                (format nil "* Task~%:PROPERTIES:~%:ID: stable~%:END:~%")))
             (metadata-heading (clock-test-heading metadata-document "Task"))
             (metadata-context (clock-test-context "2025-01-01T10:01:00")))
        (multiple-value-bind (result value)
            (insert-timer-item metadata-document metadata-heading
                               (clock-test-value "2025-01-01T10:00:00")
                               "Review" :time-context metadata-context)
          (expect value :to-equal "0:01:00")
          (expect (document-text (edit-result-updated-document result))
                  :to-equal
                  (format nil
                          "* Task~%:PROPERTIES:~%:ID: stable~%:END:~%- 0:01:00 :: Review~%"))))))

  (it "inserts timer items after planning and property metadata"
    (let* ((source
             (format nil
                     "* Task~%SCHEDULED: <2025-01-01 Wed>~%:PROPERTIES:~%:ID: stable~%:END:~%Body~%"))
           (document (parse-org source))
           (heading (clock-test-heading document "Task"))
           (context (clock-test-context "2025-01-01T10:01:00")))
      (multiple-value-bind (result value)
          (insert-timer-item document heading
                             (clock-test-value "2025-01-01T10:00:00")
                             "Review" :time-context context)
        (let* ((updated (edit-result-updated-document result))
               (updated-heading (clock-test-heading updated "Task")))
          (expect value :to-equal "0:01:00")
          (expect (document-text updated)
                  :to-equal
                  (format nil
                          "* Task~%SCHEDULED: <2025-01-01 Wed>~%:PROPERTIES:~%:ID: stable~%:END:~%- 0:01:00 :: Review~%Body~%"))
          (expect (node-property updated-heading :scheduled) :to-be-truthy)
          (expect (effective-property updated updated-heading "ID")
                  :to-equal "stable")))))

  (it "inserts timer items after LOGBOOK and before the body"
    (let* ((source
             (format nil
                     "* Task~%:LOGBOOK:~%- State \"DONE\" from \"TODO\" [2025-01-01 Wed 09:00]~%:END:~%Body~%"))
           (document (parse-org source))
           (heading (clock-test-heading document "Task"))
           (context (clock-test-context "2025-01-01T10:01:00")))
      (multiple-value-bind (result value)
          (insert-timer-item document heading
                             (clock-test-value "2025-01-01T10:00:00")
                             "Review" :time-context context)
        (expect value :to-equal "0:01:00")
        (expect (document-text (edit-result-updated-document result))
                :to-equal
                (format nil
                        "* Task~%:LOGBOOK:~%- State \"DONE\" from \"TODO\" [2025-01-01 Wed 09:00]~%:END:~%- 0:01:00 :: Review~%Body~%")))))

  (it "rejects a timer start after the reference instant with a named reason"
    (let* ((context (clock-test-context "2025-01-01T10:00:00"))
           (condition
             (clock-test-error
              (lambda ()
                (timer-item-value (clock-test-value "2025-01-01T10:00:01")
                                  context)))))
      (clock-test-expect-clock-error
       condition :invalid-range
       "timer start follows the time-context reference instant"
       "Invalid clock range")))
  (it "validates timer references and heading line endings"
    (let* ((start (clock-test-value "2025-01-01T10:00:00"))
           (no-newline-document (parse-org "* Task"))
           (no-newline-heading (clock-test-heading no-newline-document "Task"))
           (newline-source (format nil "* Task~%Body~%"))
           (newline-document (parse-org newline-source))
           (newline-heading (clock-test-heading newline-document "Task")))
      (signals type-error (timer-item-value start nil))
      (expect (org-kit::clock-timer-item-line-end
               no-newline-document no-newline-heading)
              :to-be (length (document-text no-newline-document)))
      (expect (org-kit::clock-timer-item-line-end
               newline-document newline-heading)
              :to-be (length (format nil "* Task~%")))))

  (it "rejects a multi-line timer description with a named reason"
    (let* ((document (parse-org "* Task"))
           (heading (clock-test-heading document "Task"))
           (context (clock-test-context "2025-01-01T10:00:00"))
           (condition
             (clock-test-error
              (lambda ()
                (insert-timer-item
                 document heading (clock-test-value "2025-01-01T09:59:00")
                 (format nil "first~%second") :time-context context)))))
      (clock-test-expect-clock-error
       condition :unsupported-option
       "timer description must be a single line"
       "Unsupported clock option")
      (signals type-error
        (insert-timer-item document heading
                           (clock-test-value "2025-01-01T09:59:00")
                           42 :time-context context)))))

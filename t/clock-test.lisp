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
                :to-equal '(0 0 0 0)))))

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

  (it "falls back from malformed timestamps to a valid duration and rejects malformed data"
    (let* ((context (clock-test-context "2026-10-12T12:00:00"))
           (bad-duration
             (clock-test-error
              (lambda ()
                (clock-test-sum (format nil "* Scope~%CLOCK: => 1:60") "Scope"
                                "2026-10-12T09:00:00" "2026-10-12T10:00:00"
                                context))))
           (bad-duration-syntax
             (clock-test-error
              (lambda ()
                (clock-test-sum (format nil "* Scope~%CLOCK: => x:10") "Scope"
                                "2026-10-12T09:00:00" "2026-10-12T10:00:00"
                                context))))
           (missing-hour-digits
             (clock-test-error
              (lambda ()
                (clock-test-sum (format nil "* Scope~%CLOCK: => :10") "Scope"
                                "2026-10-12T09:00:00" "2026-10-12T10:00:00"
                                context))))
           (invalid-timestamp-source
             (format nil
                     "* Scope~%CLOCK: [2026-13-99 Mon 09:00]--[2026-13-99 Mon 10:00] => 0:10"))
           (invalid-timestamp-document (parse-org invalid-timestamp-source))
           (invalid-timestamp-clock
             (clock-test-first-clock-node invalid-timestamp-document))
           (invalid-timestamp-value
             (and invalid-timestamp-clock
                  (node-property invalid-timestamp-clock :value)))
           (fallback
             (sum-clocks invalid-timestamp-document
                         (clock-test-heading invalid-timestamp-document "Scope")
                         (clock-test-value "2026-10-12T09:00:00")
                         (clock-test-value "2026-10-12T10:00:00")
                         :time-context context))
           (reversed
             (clock-test-error
              (lambda ()
                (clock-test-sum
                 (format nil "* Scope~%CLOCK: [2026-10-12 Mon 10:00]--[2026-10-12 Mon 09:00] => 1:00")
                 "Scope" "2026-10-12T09:00:00" "2026-10-12T11:00:00" context)))))
      (with-soft-assertions
        (expect (list (clock-summary-scoped-minutes fallback)
                      (clock-summary-unscoped-minutes fallback)
                      (clock-summary-total-minutes fallback))
                :to-equal '(0 10 10))
        (expect (and (node-p invalid-timestamp-value)
                     (eq (node-type invalid-timestamp-value) :timestamp))
                :to-be t)
        (clock-test-expect-clock-error bad-duration :malformed-clock
                                       "CLOCK: => 1:60"
                                       "Malformed CLOCK entry")
        (clock-test-expect-clock-error bad-duration-syntax :malformed-clock
                                       "CLOCK: => x:10"
                                       "Malformed CLOCK entry")
        (clock-test-expect-clock-error missing-hour-digits :malformed-clock
                                       "CLOCK: => :10"
                                       "Malformed CLOCK entry")
        (clock-test-expect-clock-error
         reversed :malformed-clock
         "CLOCK: [2026-10-12 Mon 10:00]--[2026-10-12 Mon 09:00] => 1:00"
         "Malformed CLOCK entry"))))

  (it "handles diary CLOCK rows as unscoped durations and rejects absent durations"
    (let* ((context (clock-test-context "2026-10-12T12:00:00"))
           (duration-source
             (format nil "* Scope~%CLOCK: <%%(diary-float t 4 2)> => 0:10"))
           (duration-document (parse-org duration-source))
           (duration-clock (clock-test-first-clock-node duration-document))
           (diary-value (node-property duration-clock :value))
           (duration-summary
             (sum-clocks duration-document (clock-test-heading duration-document "Scope")
                         (clock-test-value "2026-10-12T09:00:00")
                         (clock-test-value "2026-10-12T10:00:00")
                         :time-context context))
           (no-duration-line "CLOCK: <%%(diary-float t 4 2)>")
           (no-duration-source (format nil "* Scope~%~A" no-duration-line))
           (no-duration-document (parse-org no-duration-source))
           (no-duration-error
             (clock-test-error
              (lambda ()
                (sum-clocks no-duration-document
                            (clock-test-heading no-duration-document "Scope")
                            (clock-test-value "2026-10-12T09:00:00")
                            (clock-test-value "2026-10-12T10:00:00")
                            :time-context context)))))
      (with-soft-assertions
        (expect (and (node-p diary-value)
                     (eq (node-type diary-value) :timestamp)
                     (eq (node-property diary-value :type) :diary))
                :to-be t)
        (expect (clock-summary-unscoped-minutes duration-summary) :to-be 10)
        (expect (clock-summary-total-minutes duration-summary) :to-be 10)
        (clock-test-expect-clock-error no-duration-error :malformed-clock
                                       no-duration-line
                                       "Malformed CLOCK entry"))))

  (it "rethrows invalid timestamps and rejects timestamp-free rows without durations"
    (let* ((context (clock-test-context "2026-10-12T12:00:00"))
           (invalid-timestamp-line "CLOCK: [2026-13-99 Mon 09:00]")
           (invalid-timestamp-document
             (parse-org (format nil "* Scope~%~A" invalid-timestamp-line)))
           (invalid-timestamp-error
             (clock-test-error
              (lambda ()
                (sum-clocks invalid-timestamp-document
                            (clock-test-heading invalid-timestamp-document "Scope")
                            (clock-test-value "2026-10-12T09:00:00")
                            (clock-test-value "2026-10-12T10:00:00")
                            :time-context context))))
           (missing-timestamp-line "CLOCK: invalid")
           (missing-timestamp-document
             (parse-org (format nil "* Scope~%~A" missing-timestamp-line)))
           (missing-timestamp-error
             (clock-test-error
              (lambda ()
                (sum-clocks missing-timestamp-document
                            (clock-test-heading missing-timestamp-document "Scope")
                            (clock-test-value "2026-10-12T09:00:00")
                            (clock-test-value "2026-10-12T10:00:00")
                            :time-context context)))))
      (with-soft-assertions
        (clock-test-expect-time-error invalid-timestamp-error :invalid-time
                                      "timestamp has no valid start"
                                      "Invalid date or time")
        (clock-test-expect-clock-error missing-timestamp-error :malformed-clock
                                       missing-timestamp-line
                                       "Malformed CLOCK entry"))))

  (it "rejects a DST gap interval whose compatible endpoints reverse"
    (let* ((source
             (format nil
                     "* Scope~%CLOCK: [2024-03-10 Sun 02:30]--[2024-03-10 Sun 03:00] => 0:30"))
           (context (clock-test-context "2024-03-10T12:00:00"
                                        (resolve-time-zone "America/New_York")))
           (condition
             (clock-test-error
              (lambda ()
                (clock-test-sum source "Scope"
                                "2024-03-10T00:00:00"
                                "2024-03-10T23:59:00" context)))))
      (with-soft-assertions
        (clock-test-expect-clock-error condition :malformed-clock
                                       "interval end precedes start"
                                       "Malformed CLOCK entry"))))

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

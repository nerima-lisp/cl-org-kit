;;;; t/agenda-test.lisp
(in-package #:cl-org-kit/test)

(defparameter +agenda-test-entry-fields+
  '(:date :kind :headline :heading-path :warning-state
    :scheduled-delay-state :todo-state :effective-tags
    :property-values :habit-p :source-line :match-p))

(defun agenda-test-fixture-form (relative-path)
  (with-open-file (stream (asdf:system-relative-pathname
                           "cl-org-kit" relative-path))
    (let* ((*read-eval* nil)
           (eof (gensym "EOF"))
           (form (read stream nil eof)))
      (when (eq form eof)
        (error "Agenda fixture ~A is empty." relative-path))
      (unless (eq (read stream nil eof) eof)
        (error "Agenda fixture ~A has more than one form." relative-path))
      form)))

(defun agenda-test-fixtures ()
  (let* ((case-data
           (agenda-test-fixture-form "t/fixtures/behavior/cases/agenda.case"))
         (expected
           (agenda-test-fixture-form
            "t/fixtures/behavior/expected/agenda.case.sexp"))
         (source-path
           (asdf:system-relative-pathname
            "cl-org-kit"
            (concatenate 'string "t/fixtures/behavior/corpus/"
                         (getf case-data :source))))
         (source (uiop:read-file-string source-path))
         (document (parse-org source :source-name (getf case-data :source))))
    (values case-data expected source document)))

(defun agenda-test-context (reference-time &optional (zone "UTC"))
  (make-time-context :reference-time (make-org-time-value reference-time)
                     :zone (resolve-time-zone zone)))

(defun agenda-test-entry-heading (entry)
  (loop for node = (agenda-entry-source-node entry) then (node-parent node)
        while node
        when (eq (node-type node) :headline)
          return node))

(defun agenda-test-entry-source-line (entry)
  (multiple-value-bind (start end)
      (node-line-range (agenda-test-entry-heading entry))
    (declare (ignore end))
    start))

(defun agenda-test-region-value (document heading)
  (multiple-value-bind (value present-p)
      (effective-property document heading "REGION" :inherit t)
    (if present-p (list (cons "REGION" value)) nil)))

(defun agenda-test-heading-path-titles (path)
  (mapcar (lambda (segment) (getf segment :title)) path))

(defun agenda-test-normalize-entry (entry document &optional match-p)
  (let* ((heading (agenda-test-entry-heading entry))
         (date (agenda-entry-date entry)))
    (list :date (and date (org-time-value-iso8601 date))
          :kind (agenda-entry-kind entry)
          :headline (agenda-entry-headline entry)
          :heading-path (agenda-test-heading-path-titles
                         (agenda-entry-heading-path entry))
          :warning-state (agenda-entry-warning-state entry)
          :scheduled-delay-state (agenda-entry-scheduled-delay-state entry)
          :todo-state (agenda-entry-todo-state entry)
          :effective-tags (agenda-entry-effective-tags entry)
          :property-values (and heading
                                (agenda-test-region-value document heading))
          :habit-p (agenda-entry-habit-p entry)
          :source-line (agenda-test-entry-source-line entry)
          :match-p match-p)))

(defun agenda-test-normalize-match (entry document)
  (let* ((node (agenda-match-entry-node entry))
         (heading node))
    (multiple-value-bind (style found-p)
        (effective-property document heading "STYLE")
      (declare (ignore found-p))
      (list :date nil
            :kind nil
            :headline (node-property heading :raw-value)
            :heading-path (agenda-test-heading-path-titles
                           (agenda-match-entry-heading-path entry))
            :warning-state nil
            :scheduled-delay-state nil
            :todo-state (node-property heading :todo-keyword)
            :effective-tags (effective-heading-tags document heading)
            :property-values (agenda-test-region-value document heading)
            :habit-p (and (stringp style) (string-equal style "habit"))
            :source-line (agenda-match-entry-line entry)
            :match-p t))))

(defun agenda-test-project-entry (entry)
  (loop for key in +agenda-test-entry-fields+
        collect (list key (getf entry key))))

(defun agenda-test-normalized-entries (entries document &optional match-p)
  (mapcar (lambda (entry)
            (agenda-test-project-entry
             (agenda-test-normalize-entry entry document match-p)))
          entries))

(defun agenda-test-expected-entries (entries)
  (mapcar #'agenda-test-project-entry entries))

(defun agenda-test-case-by-name (name cases)
  (find name cases :key (lambda (case) (getf case :name)) :test #'eq))

(defun agenda-test-snapshot-result (document snapshot)
  (agenda (list document)
          (make-org-time-value (getf snapshot :start-date))
          (make-org-time-value (getf snapshot :end-date))
          :time-context (agenda-test-context (getf snapshot :reference-time))
          :warning-days (getf snapshot :warning-days)
          :scheduled-delay (getf snapshot :scheduled-delay)
          :future-repeats (getf snapshot :future-repeats)))

(defun agenda-test-snapshot-expected (name snapshots)
  (agenda-test-case-by-name name snapshots))

(defun agenda-test-document-signature (document)
  (let ((nodes nil))
    (map-nodes
     (lambda (node)
       (push (list node (node-type node) (node-parent node)
                   (node-children node) (node-properties node)
                   (node-begin node) (node-end node)
                   (node-contents-begin node) (node-contents-end node))
             nodes))
     (document-root document))
    (list (document-text document) (document-settings document)
          (nreverse nodes))))

(defun agenda-test-captured-error (function)
  (handler-case (progn (funcall function) nil)
    (error (condition) condition)))

(defun agenda-test-titles-on-date (entries date)
  (loop for entry in entries
        when (and (agenda-entry-date entry)
                  (string= (org-time-value-iso8601 (agenda-entry-date entry))
                           date))
          collect (agenda-entry-headline entry)))

(describe "Agenda fixture conformance"
  (it "matches all fixed Org 9.8.10 snapshots, including warning, delay, range and repeat modes"
    (multiple-value-bind (case-data expected source document)
        (agenda-test-fixtures)
      (let ((case-snapshots (getf case-data :snapshots))
            (expected-snapshots (getf expected :snapshots)))
        (with-soft-assertions
          (expect (plusp (length source)) :to-be t)
          (expect (getf expected :org-version) :to-equal "9.8.10")
          (expect (length case-snapshots) :to-be 12)
          (expect (length expected-snapshots) :to-be 12)
          (dolist (snapshot case-snapshots)
            (let* ((name (getf snapshot :name))
                   (expected-snapshot
                     (agenda-test-snapshot-expected name expected-snapshots))
                   (result (agenda-test-snapshot-result document snapshot))
                   (entries (agenda-result-entries result)))
              (expect (and expected-snapshot
                           (string= (getf snapshot :reference-time)
                                    (getf expected-snapshot :reference-time)))
                      :to-be t)
              (expect (agenda-test-normalized-entries entries document)
                      :to-equal
                      (agenda-test-expected-entries
                       (getf expected-snapshot :entries)))))))))

  (it "keeps repeated agenda calls in stable date, kind and source order"
    (multiple-value-bind (case-data expected source document)
        (agenda-test-fixtures)
      (declare (ignore case-data source))
      (let* ((snapshot
               (agenda-test-snapshot-expected :repeating-warning-and-delay
                                               (getf expected :snapshots)))
             (result (agenda-test-snapshot-result document snapshot))
             (actual-entries (agenda-result-entries result))
             (first-pass
               (agenda-test-normalized-entries
                actual-entries document))
             (second-pass
               (agenda-test-normalized-entries
                (agenda-result-entries
                 (agenda-test-snapshot-result document snapshot))
                document)))
        (with-soft-assertions
          (expect first-pass :to-equal second-pass)
          (expect first-pass
                  :to-equal
                  (agenda-test-expected-entries (getf snapshot :entries)))
          (expect (agenda-test-titles-on-date actual-entries "2025-03-07")
                  :to-equal
                  (loop for entry in (getf snapshot :entries)
                        when (string= (getf entry :date) "2025-03-07")
                          collect (getf entry :headline))))))))

(describe "Agenda match and TODO fixture conformance"
  (it "matches all Org match and TODO rows with inherited tags and properties"
    (multiple-value-bind (case-data expected source document)
        (agenda-test-fixtures)
      (declare (ignore source))
      (let ((context (agenda-test-context "2025-03-07T12:00:00+00:00"))
            (match-cases (getf case-data :match-cases))
            (expected-matches (getf expected :match-cases))
            (todo-cases (getf case-data :todo-cases))
            (expected-todos (getf expected :todo-cases)))
        (with-soft-assertions
          (expect (length match-cases) :to-be 3)
          (expect (length todo-cases) :to-be 3)
          (dolist (case match-cases)
            (let* ((name (getf case :name))
                   (oracle (agenda-test-case-by-name name expected-matches))
                   (matches (agenda-match (list document) (getf case :match)
                                          :time-context context)))
              (expect (mapcar (lambda (entry)
                                (agenda-test-project-entry
                                 (agenda-test-normalize-match entry document)))
                              matches)
                      :to-equal
                      (agenda-test-expected-entries (getf oracle :entries)))))
          (dolist (case todo-cases)
            (let* ((name (getf case :name))
                   (oracle (agenda-test-case-by-name name expected-todos))
                   (result (agenda-todo-list
                            (list document) :match (getf case :match)
                            :time-context context)))
              (expect (agenda-test-normalized-entries
                       (agenda-result-entries result) document
                       (not (null (getf case :match))))
                      :to-equal
                      (agenda-test-expected-entries (getf oracle :entries))))))))))

(describe "Agenda calendar and filtering boundaries"
  (it "includes both ends of a civil-date range across a daylight-saving transition"
    (let* ((document
             (parse-org
              (format nil "* TODO DST range~%<2025-03-29 Sat 09:00>--<2025-03-31 Mon 09:00>")))
           (result
             (agenda (list document)
                     (make-org-time-value "2025-03-29")
                     (make-org-time-value "2025-03-31")
                     :time-context
                     (agenda-test-context "2025-03-29T12:00:00" "Europe/Berlin")))
           (entries (agenda-result-entries result)))
      (expect (mapcar (lambda (entry)
                        (org-time-value-iso8601 (agenda-entry-date entry)))
                      entries)
              :to-equal '("2025-03-29" "2025-03-30" "2025-03-31"))))

  (it "includes active timestamps and excludes inactive and diary timestamps"
    (multiple-value-bind (case-data expected source document)
        (agenda-test-fixtures)
      (declare (ignore source))
      (let* ((snapshot (agenda-test-snapshot-expected
                        :warning-d0 (getf expected :snapshots)))
             (entries (agenda-result-entries
                       (agenda-test-snapshot-result
                        document
                        (agenda-test-case-by-name :warning-d0
                                                  (getf case-data :snapshots)))))
             (titles (mapcar #'agenda-entry-headline entries)))
        (with-soft-assertions
          (expect (not (null snapshot)) :to-be t)
          (expect (not (null (member "Active single date" titles
                                     :test #'string=)))
                  :to-be t)
          (expect (not (null (member "Active date range" titles
                                     :test #'string=)))
                  :to-be t)
          (expect (member "Inactive single date" titles :test #'string=) :to-be nil)
          (expect (some (lambda (title) (search "diary" title :test #'char-equal))
                        titles)
                  :to-be nil))))))

(describe "Agenda resource limits and errors"
  (it "enforces inclusive range and row limits below, at and above the ceiling"
    (let* ((context (agenda-test-context "2025-01-01T12:00:00+00:00"))
           (empty (parse-org "" :limits
                             (make-limits :max-agenda-range-days 3
                                          :max-agenda-entries 1)))
           (one-row (parse-org (format nil "* TODO One~%<2025-01-01>")
                               :limits (make-limits :max-agenda-range-days 3
                                                    :max-agenda-entries 1)))
           (two-rows (parse-org (format nil
                                       "* TODO Two~%<2025-01-01>~%<2025-01-01>")
                                :limits (make-limits :max-agenda-range-days 3
                                                     :max-agenda-entries 1)))
           (below-range
             (agenda (list empty) (make-org-time-value "2025-01-01")
                     (make-org-time-value "2025-01-02") :time-context context))
           (at-range
             (agenda (list empty) (make-org-time-value "2025-01-01")
                     (make-org-time-value "2025-01-03") :time-context context))
           (over-range
             (agenda-test-captured-error
              (lambda ()
                (agenda (list empty) (make-org-time-value "2025-01-01")
                        (make-org-time-value "2025-01-04")
                        :time-context context))))
           (at-rows
             (agenda (list one-row) (make-org-time-value "2025-01-01")
                     (make-org-time-value "2025-01-01") :time-context context))
           (over-rows
             (agenda-test-captured-error
              (lambda ()
                (agenda (list two-rows) (make-org-time-value "2025-01-01")
                        (make-org-time-value "2025-01-01")
                        :time-context context)))))
      (with-soft-assertions
        (expect (length (agenda-result-entries below-range)) :to-be 0)
        (expect (length (agenda-result-entries at-range)) :to-be 0)
        (expect (length (agenda-result-entries at-rows)) :to-be 1)
        (expect (typep over-range 'org-resource-limit-error) :to-be t)
        (when (typep over-range 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name over-range)
                  :to-be :max-agenda-range-days)
          (expect (org-resource-limit-error-limit over-range) :to-be 3)
          (expect (org-resource-limit-error-actual over-range) :to-be 4)
          (expect (org-kit-error-message over-range)
                  :to-equal "Resource limit exceeded"))
        (expect (typep over-rows 'org-resource-limit-error) :to-be t)
        (when (typep over-rows 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name over-rows)
                  :to-be :max-agenda-entries)
          (expect (org-resource-limit-error-limit over-rows) :to-be 1)
          (expect (org-resource-limit-error-actual over-rows) :to-be 2)
          (expect (org-kit-error-message over-rows)
                  :to-equal "Resource limit exceeded")))))

  (it "counts generated repeat occurrences before filtering and aggregates them across documents"
    (let* ((context (agenda-test-context "2025-01-01T12:00:00+00:00"))
           (empty (parse-org "" :limits (make-limits :max-future-repeats 0)))
           (one-repeat
             (parse-org (format nil "* TODO Repeat~%SCHEDULED: <2025-01-01 +1d>")
                        :limits (make-limits :max-future-repeats 1)))
           (two-repeat-limit-one
             (parse-org (format nil "* TODO Repeat~%SCHEDULED: <2025-01-01 +1d>")
                        :limits (make-limits :max-future-repeats 1)))
           (stale-repeat-limit-two
             (parse-org (format nil "* TODO Stale repeat~%SCHEDULED: <2025-01-01 +1d>")
                        :limits (make-limits :max-future-repeats 2)))
           (stale-repeat-limit-five
             (parse-org (format nil "* TODO Stale repeat~%SCHEDULED: <2025-01-01 +1d>")
                        :limits (make-limits :max-future-repeats 5)))
           (one-day (make-org-time-value "2025-01-01"))
           (two-days (make-org-time-value "2025-01-02"))
           (three-days (make-org-time-value "2025-01-03"))
           (six-days (make-org-time-value "2025-01-06"))
           (stale-context (agenda-test-context "2025-01-05T12:00:00+00:00"))
           (below
             (agenda (list empty) one-day one-day :time-context context))
           (at
             (agenda (list one-repeat) two-days two-days :time-context context))
           (over
             (agenda-test-captured-error
              (lambda ()
                (agenda (list two-repeat-limit-one) two-days three-days
                        :time-context context))))
           (aggregate-over
             (agenda-test-captured-error
              (lambda ()
                (agenda (list one-repeat one-repeat) two-days two-days
                        :time-context context))))
           (stale-at
             (agenda (list stale-repeat-limit-five) six-days six-days
                     :time-context stale-context))
           (stale-over
             (agenda-test-captured-error
              (lambda ()
                (agenda (list stale-repeat-limit-two) six-days six-days
                        :time-context stale-context)))))
      (with-soft-assertions
        (expect (length (agenda-result-entries below)) :to-be 0)
        (expect (length (agenda-result-entries at)) :to-be 1)
        (expect (length (agenda-result-entries stale-at)) :to-be 1)
        (expect (typep over 'org-resource-limit-error) :to-be t)
        (when (typep over 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name over)
                  :to-be :max-future-repeats)
          (expect (org-resource-limit-error-limit over) :to-be 1)
          (expect (org-resource-limit-error-actual over) :to-be 2))
        (expect (typep aggregate-over 'org-resource-limit-error) :to-be t)
        (when (typep aggregate-over 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name aggregate-over)
                  :to-be :max-future-repeats)
          (expect (org-resource-limit-error-limit aggregate-over) :to-be 1)
          (expect (org-resource-limit-error-actual aggregate-over) :to-be 2))
        (expect (typep stale-over 'org-resource-limit-error) :to-be t)
        (when (typep stale-over 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name stale-over)
                  :to-be :max-future-repeats)
          (expect (org-resource-limit-error-limit stale-over) :to-be 2)
          (expect (org-resource-limit-error-actual stale-over) :to-be 3)))))

  (it "reports agenda error reason and context and preserves malformed query details"
    (let* ((document (parse-org "* TODO Task"))
           (context (agenda-test-context "2025-01-01T12:00:00+00:00"))
           (bad-option
             (agenda-test-captured-error
              (lambda ()
                (agenda (list document) (make-org-time-value "2025-01-01")
                        (make-org-time-value "2025-01-01")
                        :time-context context :warning-days -1))))
           (bad-range
             (agenda-test-captured-error
              (lambda ()
                (agenda (list document) (make-org-time-value "2025-01-02")
                        (make-org-time-value "2025-01-01")
                        :time-context context))))
           (bad-query
             (agenda-test-captured-error
              (lambda ()
                (agenda-match (list document) "work&(laptop|night)"
                              :time-context context))))
           (condition (make-condition 'org-agenda-error
                                      :message "Invalid agenda option WARNING-DAYS"
                                      :reason :invalid-option :context "-1")))
      (with-soft-assertions
        (expect (typep bad-option 'org-agenda-error) :to-be t)
        (when (typep bad-option 'org-agenda-error)
          (expect (org-kit-error-message bad-option)
                  :to-equal "Invalid agenda option WARNING-DAYS")
          (expect (org-agenda-error-reason bad-option) :to-be :invalid-option)
          (expect (org-agenda-error-context bad-option) :to-equal "-1"))
        (expect (typep bad-range 'org-agenda-error) :to-be t)
        (when (typep bad-range 'org-agenda-error)
          (expect (org-agenda-error-reason bad-range) :to-be :invalid-range)
          (expect (and (stringp (org-agenda-error-context bad-range))
                       (plusp (length (org-agenda-error-context bad-range))))
                  :to-be t))
        (expect (list (org-kit-error-message condition)
                      (org-agenda-error-reason condition)
                      (org-agenda-error-context condition))
                :to-equal
                '("Invalid agenda option WARNING-DAYS" :invalid-option "-1"))
        (expect (typep bad-query 'org-query-error) :to-be t)
        (when (typep bad-query 'org-query-error)
          (expect (org-query-error-query bad-query)
                  :to-equal "work&(laptop|night)")
          (expect (org-query-error-position bad-query) :to-be 5))))))

(describe "Agenda source immutability"
  (it "does not mutate parsed source, node properties, tree links or spans"
    (multiple-value-bind (case-data expected source document)
        (agenda-test-fixtures)
      (let* ((before (agenda-test-document-signature document))
             (snapshot
               (agenda-test-snapshot-expected :repeaters-all
                                               (getf expected :snapshots))))
        (agenda-test-snapshot-result document snapshot)
        (agenda-match (list document) "outer"
                      :time-context (agenda-test-context
                                     (getf snapshot :reference-time)))
        (agenda-todo-list (list document) :match "TODO=\"TODO\""
                          :time-context (agenda-test-context
                                         (getf snapshot :reference-time)))
        (with-soft-assertions
          (expect (agenda-test-document-signature document) :to-equal before)
          (expect (write-org document) :to-equal source)
          (expect (length (getf case-data :snapshots)) :to-be 12))))))

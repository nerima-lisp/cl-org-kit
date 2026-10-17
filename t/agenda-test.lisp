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

(defun agenda-test-count-source-data-visits (function)
  (let ((node-visits 0))
    (let ((org-kit::*agenda-data-walk-observer*
            (lambda (node)
              (declare (ignore node))
              (incf node-visits))))
      (let ((result (funcall function)))
        (values result node-visits)))))

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

  (it "keeps agenda source-data visits bounded for agenda and matched TODO rows"
    (let* ((heading-count 2000)
           (source
             (with-output-to-string (stream)
               (dotimes (index heading-count)
                 (format stream "* TODO Entry ~D~%<2025-01-01>~%" index))))
           (document (parse-org source))
           (node-count
             (let ((count 0))
               (map-nodes (lambda (node)
                            (declare (ignore node))
                            (incf count))
                          (document-root document))
               count))
           (date (make-org-time-value "2025-01-01"))
           (context (agenda-test-context "2025-01-01T12:00:00+00:00"))
           (agenda-observation
             (multiple-value-list
              (agenda-test-count-source-data-visits
               (lambda ()
                 (agenda (list document) date date :time-context context)))))
           (todo-observation
             (multiple-value-list
              (agenda-test-count-source-data-visits
               (lambda ()
                 (agenda-todo-list (list document) :match "TODO=\"TODO\""
                                   :time-context context))))))
      (with-soft-assertions
        (expect (length (agenda-result-entries (first agenda-observation)))
                :to-be heading-count)
        (expect (length (agenda-result-entries (first todo-observation)))
                :to-be heading-count)
        (expect (<= node-count (second agenda-observation)) :to-be t)
        (expect (<= (second agenda-observation) (* 2 node-count)) :to-be t)
        (expect (<= node-count (second todo-observation)) :to-be t)
        (expect (<= (second todo-observation) (* 2 node-count))
                :to-be t))))

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
                      (agenda-test-expected-entries (getf oracle :entries)))
              (expect (list (agenda-result-start-date result)
                            (agenda-result-end-date result))
                      :to-equal '(nil nil))))))))

  (it "uses the query default of no inherited properties for agenda filters"
    (let* ((document
             (parse-org
              (format nil
                      "#+TODO: TODO | DONE~%* TODO Parent~%:PROPERTIES:~%:REGION: west~%:END:~%** TODO Child~%")))
           (context (agenda-test-context "2025-01-01T12:00:00+00:00"))
           (matches (agenda-match (list document) "REGION=*{.*}"
                                  :time-context context))
           (todos (agenda-result-entries
                   (agenda-todo-list (list document) :match "REGION=*{.*}"
                                     :time-context context))))
      (with-soft-assertions
        (expect (mapcar (lambda (entry)
                          (node-property (agenda-match-entry-node entry)
                                         :raw-value))
                        matches)
                :to-equal '("Parent"))
        (expect (mapcar #'agenda-entry-headline todos)
                :to-equal '("Parent"))))))

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
                  :to-be nil)))))

  (it "ignores an inactive timestamp in parsed planning data"
    (let* ((document
             (parse-org
              (format nil "* TODO Inactive planning~%SCHEDULED: [2025-01-01 Wed]")))
           (context (agenda-test-context "2025-01-01T12:00:00+00:00")))
      (let ((planning nil))
        (map-nodes (lambda (node)
                     (when (eq (node-type node) :planning)
                       (setf planning node)))
                   (document-root document))
        (let ((timestamp (and planning (node-property planning :scheduled))))
          (with-soft-assertions
            (expect (not (null planning)) :to-be t)
            (expect (not (null timestamp)) :to-be t)
            (when timestamp
              (expect (org-timestamp-view-kind (timestamp-view timestamp))
                      :to-be :inactive))
            (expect (agenda-result-entries
                     (agenda (list document) (make-org-time-value "2025-01-01")
                             (make-org-time-value "2025-01-01")
                             :time-context context))
                    :to-equal '()))))))

  (it "covers deadline warning boundaries around the reference and range"
    (labels ((rows (source reference start end &optional (warning-days 3))
               (mapcar (lambda (entry)
                         (list (org-time-value-iso8601
                                (agenda-entry-date entry))
                               (agenda-entry-warning-state entry)))
                       (agenda-result-entries
                        (agenda (list (parse-org source))
                                (make-org-time-value start)
                                (make-org-time-value end)
                                :time-context (agenda-test-context reference)
                                :warning-days warning-days)))))
      (with-soft-assertions
        (expect (rows (format nil "* TODO Deadline~%DEADLINE: <2025-01-10>")
                      "2025-01-08T12:00:00+00:00" "2025-01-05" "2025-01-10")
                :to-equal '(("2025-01-08" :prewarning) ("2025-01-10" :due)))
        (expect (rows (format nil "* TODO Deadline~%DEADLINE: <2025-01-10>")
                      "2025-01-08T12:00:00+00:00" "2025-01-09" "2025-01-10")
                :to-equal '(("2025-01-10" :due)))
        (expect (rows (format nil "* TODO Deadline~%DEADLINE: <2025-01-10>")
                      "2025-01-05T12:00:00+00:00" "2025-01-05" "2025-01-10")
                :to-equal '(("2025-01-10" :due)))
        (expect (rows (format nil "* TODO Deadline~%DEADLINE: <2025-01-10>")
                      "2025-01-05T12:00:00+00:00" "2025-01-05" "2025-01-10" 0)
                :to-equal '(("2025-01-10" :due)))
        (expect (rows (format nil "* TODO Deadline~%DEADLINE: <2025-01-10>")
                      "2025-01-10T12:00:00+00:00" "2025-01-10" "2025-01-10")
                :to-equal '(("2025-01-10" :due)))
        (expect (rows (format nil "* DONE Deadline~%DEADLINE: <2025-01-10>")
                      "2025-01-08T12:00:00+00:00" "2025-01-05" "2025-01-10")
                :to-equal '(("2025-01-10" :due))))))

  (it "limits repeating deadline and scheduled rows to their visible windows"
    (let* ((context (agenda-test-context "2025-01-01T12:00:00+00:00"))
           (start (make-org-time-value "2025-01-04"))
           (end (make-org-time-value "2025-01-05"))
           (deadline-documents
             (list (parse-org
                    (format nil
                            "* TODO Repeating deadline~%DEADLINE: <2025-01-01 ++1d>"))
                   (parse-org
                    (format nil
                            "* DONE Repeating deadline~%DEADLINE: <2025-01-01 ++1d>"))))
           (scheduled-documents
             (list (parse-org
                    (format nil
                            "* TODO Visible repeat~%SCHEDULED: <2025-01-01 ++1d>"))
                   (parse-org
                    (format nil
                            "* TODO Delayed repeat~%SCHEDULED: <2025-01-01 ++1d --3d>"))))
           (deadline-entries
             (agenda-result-entries
              (agenda deadline-documents start start :time-context context)))
           (scheduled-entries
             (agenda-result-entries
              (agenda scheduled-documents start end :time-context context))))
      (with-soft-assertions
        (expect (mapcar (lambda (entry)
                          (list (agenda-entry-headline entry)
                                (org-time-value-iso8601
                                 (agenda-entry-date entry))
                                (agenda-entry-warning-state entry)))
                        deadline-entries)
                :to-equal '(("Repeating deadline" "2025-01-04" :due)))
        (expect (mapcar (lambda (entry)
                          (list (agenda-entry-headline entry)
                                (org-time-value-iso8601
                                 (agenda-entry-date entry))))
                        scheduled-entries)
                :to-equal '(("Visible repeat" "2025-01-04")
                            ("Delayed repeat" "2025-01-04")
                            ("Visible repeat" "2025-01-05")
                            ("Delayed repeat" "2025-01-05"))))))
)

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

  (it "uses the minimum limits across documents"
    (let* ((context (agenda-test-context "2025-01-01T12:00:00+00:00"))
           (date (make-org-time-value "2025-01-01"))
           (end-date (make-org-time-value "2025-01-03"))
           (high-row-limit
             (parse-org (format nil "* TODO High~%<2025-01-01>")
                        :limits (make-limits :max-agenda-entries 4)))
           (low-row-limit
             (parse-org (format nil "* TODO Low~%<2025-01-01>")
                        :limits (make-limits :max-agenda-entries 1)))
           (high-todo-limit
             (parse-org "* TODO High" :limits
                        (make-limits :max-agenda-entries 4)))
           (low-todo-limit
             (parse-org "* TODO Low" :limits
                        (make-limits :max-agenda-entries 1)))
           (high-range-limit
             (parse-org "" :limits (make-limits :max-agenda-range-days 4)))
           (low-range-limit
             (parse-org "" :limits (make-limits :max-agenda-range-days 2)))
           (row-result
             (agenda-test-captured-error
              (lambda ()
                (agenda (list high-row-limit low-row-limit) date date
                        :time-context context))))
           (todo-result
             (agenda-test-captured-error
              (lambda ()
                (agenda-todo-list (list high-todo-limit low-todo-limit)
                                  :time-context context))))
           (range-error
             (agenda-test-captured-error
              (lambda ()
                (agenda (list high-range-limit low-range-limit) date end-date
                        :time-context context)))))
      (with-soft-assertions
        (expect (typep row-result 'org-resource-limit-error) :to-be t)
        (when (typep row-result 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name row-result)
                  :to-be :max-agenda-entries)
          (expect (org-resource-limit-error-limit row-result) :to-be 1)
          (expect (org-resource-limit-error-actual row-result) :to-be 2))
        (expect (typep todo-result 'org-resource-limit-error) :to-be t)
        (when (typep todo-result 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name todo-result)
                  :to-be :max-agenda-entries)
          (expect (org-resource-limit-error-limit todo-result) :to-be 1)
          (expect (org-resource-limit-error-actual todo-result) :to-be 2))
        (expect (typep range-error 'org-resource-limit-error) :to-be t)
        (when (typep range-error 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name range-error)
                  :to-be :max-agenda-range-days)
          (expect (org-resource-limit-error-limit range-error) :to-be 2)
          (expect (org-resource-limit-error-actual range-error) :to-be 3)))))

  (it "enforces a zero row limit before expanding a long repeating timestamp"
    (let* ((document
             (parse-org (format nil "* TODO Repeating~%<2025-01-01 +1d>")
                        :limits (make-limits :max-agenda-entries 0
                                             :max-future-repeats 0)))
           (condition
             (agenda-test-captured-error
              (lambda ()
                (agenda (list document)
                        (make-org-time-value "2025-01-01")
                        (make-org-time-value "2034-12-31")
                        :time-context
                        (agenda-test-context "2024-12-31T12:00:00+00:00"))))))
      (with-soft-assertions
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when (typep condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name condition)
                  :to-be :max-agenda-entries)
          (expect (org-resource-limit-error-limit condition) :to-be 0)
          (expect (org-resource-limit-error-actual condition) :to-be 1)))))

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

  (it "bounds active timestamp repeats by the range and minimum document cap"
    (let* ((context (agenda-test-context "2025-01-01T12:00:00+00:00"))
           (stale-context (agenda-test-context "2025-01-03T12:00:00+00:00"))
           (source (format nil "* TODO Active repeat~%<2025-01-01 +1d>"))
           (empty (parse-org "" :limits
                             (make-limits :max-future-repeats 0)))
           (repeat-at-limit
             (parse-org source :limits (make-limits :max-future-repeats 1)))
           (repeat-above-limit
             (parse-org source :limits (make-limits :max-future-repeats 2)))
           (repeat-over-limit
             (parse-org source :limits (make-limits :max-future-repeats 0)))
           (stale-at-limit
             (parse-org source :limits (make-limits :max-future-repeats 3)))
           (stale-over-limit
             (parse-org source :limits (make-limits :max-future-repeats 2)))
           (aggregate-high
             (parse-org source :limits (make-limits :max-future-repeats 3)))
           (aggregate-low
             (parse-org source :limits (make-limits :max-future-repeats 1)))
           (jan-1 (make-org-time-value "2025-01-01"))
           (jan-2 (make-org-time-value "2025-01-02"))
           (jan-3 (make-org-time-value "2025-01-03"))
           (jan-4 (make-org-time-value "2025-01-04"))
           (before-limit
             (agenda (list empty) jan-2 jan-2 :time-context context))
           (at-limit
             (agenda (list repeat-at-limit) jan-2 jan-2
                     :time-context context))
           (above-limit
             (agenda (list repeat-above-limit) jan-2 jan-2
                     :time-context context))
           (over-limit
             (agenda-test-captured-error
              (lambda ()
                (agenda (list repeat-over-limit) jan-2 jan-2
                        :time-context context))))
           (stale-entries
             (agenda-result-entries
              (agenda (list stale-at-limit) jan-3 jan-4
                      :time-context stale-context)))
           (stale-over
             (agenda-test-captured-error
              (lambda ()
                (agenda (list stale-over-limit) jan-3 jan-4
                        :time-context stale-context))))
           (aggregate-over
             (agenda-test-captured-error
              (lambda ()
                (agenda (list aggregate-high aggregate-low) jan-2 jan-2
                        :time-context context)))))
      (with-soft-assertions
        (expect (agenda-result-entries before-limit) :to-equal nil)
        (expect (mapcar (lambda (entry)
                          (org-time-value-iso8601 (agenda-entry-date entry)))
                        (agenda-result-entries at-limit))
                :to-equal '("2025-01-02"))
        (expect (mapcar (lambda (entry)
                          (org-time-value-iso8601 (agenda-entry-date entry)))
                        (agenda-result-entries above-limit))
                :to-equal '("2025-01-02"))
        (expect (mapcar (lambda (entry)
                          (org-time-value-iso8601 (agenda-entry-date entry)))
                        stale-entries)
                :to-equal '("2025-01-03" "2025-01-04"))
        (expect (typep over-limit 'org-resource-limit-error) :to-be t)
        (when (typep over-limit 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit over-limit) :to-be 0)
          (expect (org-resource-limit-error-actual over-limit) :to-be 1))
        (expect (typep stale-over 'org-resource-limit-error) :to-be t)
        (when (typep stale-over 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit stale-over) :to-be 2)
          (expect (org-resource-limit-error-actual stale-over) :to-be 3))
        (expect (typep aggregate-over 'org-resource-limit-error) :to-be t)
        (when (typep aggregate-over 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit aggregate-over) :to-be 1)
          (expect (org-resource-limit-error-actual aggregate-over) :to-be 2)))))

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

(describe "Agenda record API and validation"
  (it "constructs and reads every public agenda record field"
    (let* ((default-entry (make-agenda-entry))
           (default-result (make-agenda-result))
           (default-match-entry (make-agenda-match-entry))
           (document (parse-org (format nil "* TODO Row :tag:~%<2025-01-01 Wed>")))
           (heading nil)
           (timestamp nil))
      (map-nodes (lambda (node)
                   (case (node-type node)
                     (:headline (setf heading node))
                     (:timestamp (setf timestamp node))))
                 (document-root document))
      (let* ((date (make-org-time-value "2025-01-01"))
             (context (agenda-test-context "2025-01-01T12:00:00+00:00"))
             (view (timestamp-view timestamp))
             (path (node-heading-path heading))
             (entry (make-agenda-entry
                     :date date :kind :timestamp :headline "Row"
                     :heading-path path :timestamp view
                     :warning-state :due :scheduled-delay-state :delayed
                     :todo-state "TODO" :effective-tags '("tag")
                     :habit-p t :source-node timestamp))
             (result (make-agenda-result :entries (list entry) :start-date date
                                         :end-date date :time-context context))
             (match-entry (make-agenda-match-entry
                           :document document :node heading :heading-path path
                           :line 1 :end-line 2)))
        (with-soft-assertions
          (expect (list (agenda-entry-p default-entry)
                        (agenda-entry-date default-entry)
                        (agenda-entry-kind default-entry)
                        (agenda-entry-headline default-entry)
                        (agenda-entry-heading-path default-entry)
                        (agenda-entry-timestamp default-entry)
                        (agenda-entry-warning-state default-entry)
                        (agenda-entry-scheduled-delay-state default-entry)
                        (agenda-entry-todo-state default-entry)
                        (agenda-entry-effective-tags default-entry)
                        (agenda-entry-habit-p default-entry)
                        (agenda-entry-habit-consistency default-entry)
                        (agenda-entry-source-node default-entry))
                  :to-equal (list t nil nil nil nil nil nil nil nil nil nil nil nil))
          (expect (list (agenda-result-p default-result)
                        (agenda-result-entries default-result)
                        (agenda-result-start-date default-result)
                        (agenda-result-end-date default-result)
                        (agenda-result-time-context default-result))
                  :to-equal (list t nil nil nil nil))
          (expect (list (agenda-match-entry-p default-match-entry)
                        (agenda-match-entry-document default-match-entry)
                        (agenda-match-entry-node default-match-entry)
                        (agenda-match-entry-heading-path default-match-entry)
                        (agenda-match-entry-line default-match-entry)
                        (agenda-match-entry-end-line default-match-entry))
                  :to-equal (list t nil nil nil nil nil))
          (expect (agenda-entry-p entry) :to-be t)
          (expect (list (agenda-entry-date entry) (agenda-entry-kind entry)
                        (agenda-entry-headline entry)
                        (agenda-entry-heading-path entry)
                        (agenda-entry-timestamp entry)
                        (agenda-entry-warning-state entry)
                        (agenda-entry-scheduled-delay-state entry)
                        (agenda-entry-todo-state entry)
                        (agenda-entry-effective-tags entry)
                        (agenda-entry-habit-p entry)
                        (agenda-entry-habit-consistency entry)
                        (agenda-entry-source-node entry))
                  :to-equal (list date :timestamp "Row" path view :due :delayed
                                  "TODO" '("tag") t nil timestamp))
          (expect (agenda-result-p result) :to-be t)
          (expect (list (agenda-result-entries result)
                        (agenda-result-start-date result)
                        (agenda-result-end-date result)
                        (agenda-result-time-context result))
                  :to-equal (list (list entry) date date context))
          (expect (agenda-match-entry-p match-entry) :to-be t)
          (expect (list (agenda-match-entry-document match-entry)
                        (agenda-match-entry-node match-entry)
                        (agenda-match-entry-heading-path match-entry)
                        (agenda-match-entry-line match-entry)
                        (agenda-match-entry-end-line match-entry))
                  :to-equal (list document heading path 1 2))))))

  (it "reconstructs restart habit due dates from DONE log history"
    (let* ((source
             (format nil
                     "* TODO Drink water~%SCHEDULED: <2025-01-10 Fri .+1d>~%:PROPERTIES:~%:STYLE: habit~%:END:~%:LOGBOOK:~%- State \"DONE\" from \"TODO\" [2025-01-05 Sun]~%- State \"DONE\" from \"TODO\" [2025-01-09 Thu]~%:END:~%"))
           (document (parse-org source))
           (entries
             (agenda-result-entries
              (agenda (list document)
                      (make-org-time-value "2025-01-05")
                      (make-org-time-value "2025-01-10")
                      :time-context
                      (agenda-test-context "2025-01-10T12:00:00+00:00")
                      :future-repeats :none)))
           (habit-entry (find-if #'agenda-entry-habit-p entries))
           (consistency
             (and habit-entry
                  (mapcar (lambda (day)
                            (list (org-time-value-iso8601 (getf day :date))
                                  (getf day :status)))
                          (agenda-entry-habit-consistency habit-entry)))))
      (with-soft-assertions
        (expect (agenda-entry-habit-p habit-entry) :to-be t)
        (expect consistency
                :to-equal '(("2025-01-05" :completed)
                            ("2025-01-06" :within-schedule)
                            ("2025-01-07" :late)
                            ("2025-01-08" :late)
                            ("2025-01-09" :completed)
                            ("2025-01-10" :within-schedule)))
        (expect (every (lambda (entry)
                         (or (agenda-entry-habit-p entry)
                             (null (agenda-entry-habit-consistency entry))))
                       entries)
                :to-be t))))
  (it "reports a restart habit's overdue history inside a later window"
    (let* ((source
             (format nil
                     "* TODO Drink water~%SCHEDULED: <2025-01-10 Fri .+1d>~%:PROPERTIES:~%:STYLE: habit~%:END:~%:LOGBOOK:~%- State \"DONE\" from \"TODO\" [2025-01-05 Sun]~%- State \"DONE\" from \"TODO\" [2025-01-09 Thu]~%:END:~%"))
           (document (parse-org source))
           (entries
             (agenda-result-entries
              (agenda (list document)
                      (make-org-time-value "2025-01-07")
                      (make-org-time-value "2025-01-10")
                      :time-context
                      (agenda-test-context "2025-01-10T12:00:00+00:00")
                      :future-repeats :none)))
           (habit-entry (find-if #'agenda-entry-habit-p entries))
           (consistency
             (and habit-entry
                  (mapcar (lambda (day)
                            (list (org-time-value-iso8601 (getf day :date))
                                  (getf day :status)))
                          (agenda-entry-habit-consistency habit-entry)))))
      (expect consistency
              :to-equal '(("2025-01-07" :late)
                          ("2025-01-08" :late)
                          ("2025-01-09" :completed)
                          ("2025-01-10" :within-schedule)))))
  (it "collects only a habit heading's DONE logs and CLOSED date"
    (let* ((document
             (parse-org
              (format nil
                      "#+TODO: TODO | DONE~%* DONE Water~%SCHEDULED: <2025-01-06 Mon> CLOSED: [2025-01-06 Mon 09:30]~%:PROPERTIES:~%:STYLE: habit~%:END:~%:LOGBOOK:~%- State \"DONE\" from \"TODO\" [2025-01-05 Sun 08:00]~%- State \"TODO\" from \"DONE\" [2025-01-07 Tue 12:00]~%- State \"DONE\" from \"TODO\"~%:END:~%** DONE Child~%:LOGBOOK:~%- State \"DONE\" from \"TODO\" [2025-01-09 Thu]~%:END:~%")))
           (headings
             (nth-value
              0 (org-kit::agenda-document-heading-data
                 document :collect-headlines-p t)))
           (dates
  (let* ((heading (first headings))
         (planning (org-kit::agenda-planning-node heading)))
    (expect (node-property planning :closed) :to-be-truthy)
    (setf (node-property heading :closed) nil)
    (org-kit::agenda-habit-completion-dates document heading))))
      (expect (mapcar (lambda (ordinal)
                        (org-time-value-iso8601
                         (org-kit::agenda-date-value
                         (cl-date-kit:local-date-from-epoch-day ordinal))))
                      dates)
              :to-equal '("2025-01-05" "2025-01-06"))))

  (it "keeps cumulate and catch-up habit due dates distinct"
    (labels ((states (scheduled done-1 done-2 end-date
                       &optional
                         (reference-time "2025-01-10T12:00:00+00:00"))
               (let* ((source
                        (format nil
                                "* TODO Drink water~%SCHEDULED: <~A>~%:PROPERTIES:~%:STYLE: habit~%:END:~%:LOGBOOK:~%- State \"DONE\" from \"TODO\" [~A]~%- State \"DONE\" from \"TODO\" [~A]~%:END:~%"
                                scheduled done-1 done-2))
                      (document (parse-org source))
                      (entries
                        (agenda-result-entries
                         (agenda (list document)
                                 (make-org-time-value "2025-01-05")
                                 (make-org-time-value end-date)
                                 :time-context
                                 (agenda-test-context reference-time)
                                 :future-repeats :none)))
                      (habit-entry (find-if #'agenda-entry-habit-p entries)))
                 (mapcar (lambda (day)
                           (list (org-time-value-iso8601 (getf day :date))
                                 (getf day :status)))
                         (agenda-entry-habit-consistency habit-entry)))))
      (with-soft-assertions
        (expect (states "2025-01-10 Fri +1d"
                        "2025-01-05 Sun" "2025-01-09 Thu" "2025-01-11")
                :to-equal '(("2025-01-05" :completed)
                            ("2025-01-06" :within-schedule)
                            ("2025-01-07" :within-schedule)
                            ("2025-01-08" :within-schedule)
                            ("2025-01-09" :completed)
                            ("2025-01-10" :within-schedule)
                            ("2025-01-11" :late)))
        (expect (states "2025-01-09 Thu ++2d"
                        "2025-01-05 Sun" "2025-01-08 Wed" "2025-01-11")
                :to-equal '(("2025-01-05" :completed)
                            ("2025-01-06" :within-schedule)
                            ("2025-01-07" :within-schedule)
                            ("2025-01-08" :completed)
                            ("2025-01-09" :within-schedule)
                            ("2025-01-10" :late)
                            ("2025-01-11" :late)))
        (expect (states "2025-01-10 Fri ++2d"
                        "2025-01-05 Sun" "2025-01-08 Wed" "2025-01-11")
                :to-equal '(("2025-01-05" :completed)
                            ("2025-01-06" :within-schedule)
                            ("2025-01-07" :late)
                            ("2025-01-08" :completed)
                            ("2025-01-09" :within-schedule)
                            ("2025-01-10" :within-schedule)
                            ("2025-01-11" :late)))
        (expect (states "2025-01-15 Wed ++5d"
                        "2025-01-05 Sun" "2025-01-06 Mon" "2025-01-17"
                        "2025-01-16T12:00:00+00:00")
                :to-equal '(("2025-01-05" :completed)
                            ("2025-01-06" :completed)
                            ("2025-01-07" :within-schedule)
                            ("2025-01-08" :within-schedule)
                            ("2025-01-09" :within-schedule)
                            ("2025-01-10" :within-schedule)
                            ("2025-01-11" :within-schedule)
                            ("2025-01-12" :within-schedule)
                            ("2025-01-13" :within-schedule)
                            ("2025-01-14" :within-schedule)
                            ("2025-01-15" :within-schedule)
                            ("2025-01-16" :late)
                            ("2025-01-17" :late))))))
  (it "projects catch-up deadlines across successive overdue completions"
    (let* ((source
             (format nil
                     "#+TODO: TODO | DONE~%* TODO Drink water~%SCHEDULED: <2025-01-15 Wed ++2d>~%:PROPERTIES:~%:STYLE: habit~%:END:~%:LOGBOOK:~%- State \"DONE\" from \"TODO\" [2025-01-05 Sun]~%- State \"DONE\" from \"TODO\" [2025-01-09 Thu]~%- State \"DONE\" from \"TODO\" [2025-01-13 Mon]~%:END:~%"))
           (document (parse-org source))
           (entries
             (agenda-result-entries
              (agenda (list document)
                      (make-org-time-value "2025-01-05")
                      (make-org-time-value "2025-01-15")
                      :time-context
                      (agenda-test-context "2025-01-15T12:00:00+00:00")
                      :future-repeats :none)))
           (habit-entry (find-if #'agenda-entry-habit-p entries))
           (consistency
             (mapcar (lambda (day)
                       (list (org-time-value-iso8601 (getf day :date))
                             (getf day :status)))
                     (agenda-entry-habit-consistency habit-entry))))
      (expect consistency
              :to-equal '(("2025-01-05" :completed)
                          ("2025-01-06" :within-schedule)
                          ("2025-01-07" :within-schedule)
                          ("2025-01-08" :late)
                          ("2025-01-09" :completed)
                          ("2025-01-10" :within-schedule)
                          ("2025-01-11" :within-schedule)
                          ("2025-01-12" :late)
                          ("2025-01-13" :completed)
                          ("2025-01-14" :within-schedule)
                          ("2025-01-15" :within-schedule)))))
  (it "uses Org whole-day repeater lengths and mode-specific habit deadlines"
      (labels ((repeat-days-for (repeater)
                                (let* ((document
                                        (parse-org
                                         (format nil "* TODO Task~%SCHEDULED: <2025-01-01 Wed ~A>~%"
                                                 repeater)))
                                       (planning nil))
                                  (map-nodes (lambda (node)
                                               (when (eq (node-type node) :planning)
                                                 (setf planning node)))
                                             (document-root document))
                                  (org-kit::agenda-habit-repeat-days
                                   (timestamp-view (node-property planning :scheduled)))))
               (states (scheduled done-1 done-2 end-date
                                  &optional (start-date "2025-01-05")
                                  (reference-time "2025-01-10T12:00:00+00:00"))
                       (let* ((source
                               (format nil
                                       "* TODO Drink water~%SCHEDULED: <~A>~%:PROPERTIES:~%:STYLE: habit~%:END:~%:LOGBOOK:~%- State \"DONE\" from \"TODO\" [~A]~%- State \"DONE\" from \"TODO\" [~A]~%:END:~%"
                                       scheduled done-1 done-2))
                              (document (parse-org source))
                              (entries
                               (agenda-result-entries
                                (agenda (list document)
                                        (make-org-time-value start-date)
                                        (make-org-time-value end-date)
                                        :time-context
                                        (agenda-test-context reference-time)
                                        :future-repeats :none)))
                              (habit-entry (find-if #'agenda-entry-habit-p entries)))
                         (mapcar (lambda (day)
                                   (list (org-time-value-iso8601 (getf day :date))
                                         (getf day :status)))
                                 (agenda-entry-habit-consistency habit-entry))))
               (status-on (scheduled done-1 done-2 end-date start-date
                                     reference-time date)
                          (second (assoc date
                                         (states scheduled done-1 done-2 end-date
                                                 start-date reference-time)
                                         :test #'string=))))
        (with-soft-assertions
         (expect (repeat-days-for ".+1w") :to-be 7)
         (expect (repeat-days-for "++1m") :to-be 30)
         (expect (repeat-days-for "++1y") :to-be 365)
         (expect (states "2025-01-10 Fri +2d"
                         "2025-01-05 Sun" "2025-01-09 Thu" "2025-01-12")
                 :to-equal '(("2025-01-05" :completed)
                             ("2025-01-06" :within-schedule)
                             ("2025-01-07" :within-schedule)
                             ("2025-01-08" :within-schedule)
                             ("2025-01-09" :completed)
                             ("2025-01-10" :within-schedule)
                             ("2025-01-11" :late)
                             ("2025-01-12" :late)))
         (expect (states "2025-01-10 Fri ++2d"
                         "2025-01-05 Sun" "2025-01-09 Thu" "2025-01-12")
                 :to-equal '(("2025-01-05" :completed)
                             ("2025-01-06" :within-schedule)
                             ("2025-01-07" :late)
                             ("2025-01-08" :late)
                             ("2025-01-09" :completed)
                             ("2025-01-10" :within-schedule)
                             ("2025-01-11" :late)
                             ("2025-01-12" :late)))
         (expect (status-on "2025-03-31 Mon .+1m"
                            "2025-02-01 Sat" "2025-03-05 Wed"
                            "2025-03-31" "2025-02-01"
                            "2025-03-31T12:00:00+00:00" "2025-03-02")
                 :to-be :within-schedule)
         (expect (status-on "2025-03-31 Mon .+1m"
                            "2025-02-01 Sat" "2025-03-05 Wed"
                            "2025-03-31" "2025-02-01"
                            "2025-03-31T12:00:00+00:00" "2025-03-04")
                 :to-be :late)
         (expect (status-on "2025-03-31 Mon +1m"
                            "2025-02-01 Sat" "2025-03-05 Wed"
                            "2025-03-31" "2025-02-01"
                            "2025-03-31T12:00:00+00:00" "2025-03-01")
                 :to-be :within-schedule)
         (expect (status-on "2025-03-31 Mon +1m"
                            "2025-02-01 Sat" "2025-03-05 Wed"
                            "2025-03-31" "2025-02-01"
                            "2025-03-31T12:00:00+00:00" "2025-03-02")
                 :to-be :late)
         (expect (status-on "2025-03-31 Mon ++1m"
                            "2025-02-01 Sat" "2025-03-05 Wed"
                            "2025-03-31" "2025-02-01"
                            "2025-03-31T12:00:00+00:00" "2025-03-01")
                 :to-be :within-schedule)
         (expect (status-on "2025-03-31 Mon ++1m"
                            "2025-02-01 Sat" "2025-03-05 Wed"
                            "2025-03-31" "2025-02-01"
                            "2025-03-31T12:00:00+00:00" "2025-03-02")
                 :to-be :late))))
  (it "uses the scheduled day as the initial habit deadline"
    (let* ((source
             (format nil
                     "* TODO Drink water~%SCHEDULED: <2025-01-10 Fri ++2d>~%:PROPERTIES:~%:STYLE: habit~%:END:~%"))
           (document (parse-org source))
           (entries
             (agenda-result-entries
              (agenda (list document)
                      (make-org-time-value "2025-01-10")
                      (make-org-time-value "2025-01-12")
                      :time-context
                      (agenda-test-context "2025-01-10T12:00:00+00:00")
                      :future-repeats :none)))
           (habit-entry (find-if #'agenda-entry-habit-p entries))
           (consistency
             (mapcar (lambda (day)
                       (list (org-time-value-iso8601 (getf day :date))
                             (getf day :status)))
                     (agenda-entry-habit-consistency habit-entry))))
      (expect consistency
              :to-equal '(("2025-01-10" :within-schedule)
                          ("2025-01-11" :late)
                          ("2025-01-12" :late)))))

  (it "signals a named agenda reason for invalid habit metadata"
    (labels ((reason-for (source)
               (let* ((document (parse-org source))
                      (condition
                        (agenda-test-captured-error
                         (lambda ()
                           (agenda
                            (list document)
                            (make-org-time-value "2025-01-01")
                            (make-org-time-value "2025-01-03")
                            :time-context
                            (agenda-test-context "2025-01-01T12:00:00+00:00"))))))
                 (when (typep condition 'org-agenda-error)
                   (org-agenda-error-reason condition)))))
      (with-soft-assertions
        (expect
         (reason-for
          (format nil
                  "* TODO Drink water~%SCHEDULED: <2025-01-02 Thu>~%:PROPERTIES:~%:STYLE: habit~%:END:~%"))
         :to-be :habit-without-repeater)
        (expect
         (reason-for
          (format nil
                  "* Drink water~%SCHEDULED: <2025-01-02 Thu +1d>~%:PROPERTIES:~%:STYLE: habit~%:END:~%"))
         :to-be :habit-not-todo)
        (expect
         (reason-for
          (format nil
                  "* TODO Drink water~%:PROPERTIES:~%:STYLE: habit~%:END:~%"))
         :to-be :habit-without-schedule))))

  (it "rejects malformed document lists, invalid contexts and date-time range endpoints"
    (let* ((document (parse-org "* TODO One"))
           (date (make-org-time-value "2025-01-01"))
           (date-time (make-org-time-value "2025-01-01T12:00:00+00:00"))
           (context (agenda-test-context "2025-01-01T12:00:00+00:00"))
           (improper-documents
             (agenda-test-captured-error
              (lambda ()
                (agenda (cons document :tail) date date :time-context context))))
           (invalid-document
             (agenda-test-captured-error
              (lambda ()
                (agenda (list "not a document") date date :time-context context))))
           (invalid-context
             (agenda-test-captured-error
              (lambda () (agenda nil date date))))
           (invalid-date-type
             (agenda-test-captured-error
              (lambda () (agenda nil nil date :time-context context))))
           (invalid-date
             (agenda-test-captured-error
              (lambda () (agenda nil date-time date :time-context context)))))
      (with-soft-assertions
        (expect (typep improper-documents 'org-agenda-error) :to-be t)
        (when (typep improper-documents 'org-agenda-error)
          (expect (org-agenda-error-reason improper-documents)
                  :to-be :unsupported-entry))
        (expect (typep invalid-document 'org-agenda-error) :to-be t)
        (when (typep invalid-document 'org-agenda-error)
          (expect (org-agenda-error-context invalid-document)
                  :to-equal "not a document"))
        (expect (typep invalid-context 'org-time-error) :to-be t)
        (when (typep invalid-context 'org-time-error)
          (expect (org-time-error-reason invalid-context) :to-be :invalid-time)
          (expect (org-time-error-context invalid-context) :to-be nil)
          (expect (org-kit-error-message invalid-context)
                  :to-equal "Invalid date or time"))
        (expect (typep invalid-date-type 'org-time-error) :to-be t)
        (when (typep invalid-date-type 'org-time-error)
          (expect (org-time-error-reason invalid-date-type) :to-be :invalid-time)
          (expect (org-time-error-context invalid-date-type) :to-be nil))
        (expect (typep invalid-date 'org-time-error) :to-be t)
        (when (typep invalid-date 'org-time-error)
          (expect (org-time-error-reason invalid-date) :to-be :invalid-time)
          (expect (and (stringp (org-time-error-context invalid-date))
                       (plusp (length (org-time-error-context invalid-date))))
                  :to-be t)
          (expect (org-kit-error-message invalid-date)
                  :to-equal "Invalid date or time")))))

  (it "returns nil date bounds for an empty default agenda and TODO result"
    (let* ((start (make-org-time-value "2025-01-01"))
           (end (make-org-time-value "2025-01-02"))
           (context (agenda-test-context "2025-01-01T12:00:00+00:00"))
           (unparsed (make-document ""))
           (result (agenda nil start end :time-context context))
           (unparsed-result
             (agenda (list unparsed) start end :time-context context))
           (todos (agenda-todo-list nil :time-context context)))
      (with-soft-assertions
        (expect (agenda-result-start-date result) :to-equal start)
        (expect (agenda-result-end-date result) :to-equal end)
        (expect (agenda-result-time-context result) :to-equal context)
        (expect (agenda-result-entries result) :to-equal '())
        (expect (agenda-result-entries unparsed-result) :to-equal '())
        (expect (agenda-result-start-date todos) :to-be nil)
        (expect (agenda-result-end-date todos) :to-be nil)
        (expect (agenda-result-entries todos) :to-equal '())))))

(describe "Agenda timestamp repeater boundaries"
  (it "emits one occurrence per date for a daily repeater"
    (let* ((document
             (parse-org (format nil "* TODO Repeater~%<2025-03-03 Mon +1d>")))
           (timestamp nil)
           (entries
             (agenda-result-entries
              (agenda (list document)
                      (make-org-time-value "2025-03-03")
                      (make-org-time-value "2025-03-05")
                      :time-context
                      (agenda-test-context "2025-03-01T12:00:00+00:00")))))
      (map-nodes (lambda (node)
                   (when (eq (node-type node) :timestamp)
                     (setf timestamp node)))
                 (document-root document))
      (with-soft-assertions
        (expect (mapcar (lambda (entry)
                          (org-time-value-iso8601 (agenda-entry-date entry)))
                        entries)
                :to-equal '("2025-03-03" "2025-03-04" "2025-03-05"))
        (expect (every (lambda (entry)
                         (eq (agenda-entry-source-node entry) timestamp))
                       entries)
                :to-be t))))

  (it "marks future deadline repeats due and a reference-date reminder overdue"
    (let* ((document
             (parse-org
              (format nil
                      "* TODO Repeater~%DEADLINE: <2025-03-03 Mon ++2d -1d>")))
           (future-entries
             (agenda-result-entries
              (agenda (list document)
                      (make-org-time-value "2025-03-05")
                      (make-org-time-value "2025-03-05")
                      :time-context
                      (agenda-test-context "2025-03-03T12:00:00+00:00"))))
           (reference-date-entries
             (agenda-result-entries
              (agenda (list document)
                      (make-org-time-value "2025-03-05")
                      (make-org-time-value "2025-03-05")
                      :time-context
                      (agenda-test-context "2025-03-05T12:00:00+00:00")))))
      (with-soft-assertions
        (expect (mapcar (lambda (entry)
                          (list (org-time-value-iso8601
                                 (agenda-entry-date entry))
                                (agenda-entry-kind entry)
                                (agenda-entry-warning-state entry)))
                        future-entries)
                :to-equal '(("2025-03-05" :deadline :due)))
        (expect (mapcar (lambda (entry)
                          (list (org-time-value-iso8601
                                 (agenda-entry-date entry))
                                (agenda-entry-kind entry)
                                (agenda-entry-warning-state entry)))
                        reference-date-entries)
                :to-equal '(("2025-03-05" :deadline :overdue))))))

  (it "shifts date-only repeaters by day, week, month and year"
    (let* ((document
             (parse-org
              (format nil "* TODO Day~%<2025-01-01 Wed +1d>~%* TODO Week~%<2025-01-01 Wed +1w>~%* TODO Month~%<2025-01-01 Wed +1m>~%* TODO Year~%<2025-01-01 Wed +1y>")))
           (entries
             (agenda-result-entries
              (agenda (list document) (make-org-time-value "2025-01-01")
                      (make-org-time-value "2025-01-02")
                      :time-context
                      (agenda-test-context "2025-01-01T00:00:00+00:00")))))
      (expect (length entries) :to-be 5)))

  (it "shifts date-time repeaters by calendar units and preserves a range end"
    (let* ((document
             (parse-org
              (format nil "* TODO Day~%<2025-01-01 Wed 09:00 +1d>~%* TODO Week~%<2025-01-01 Wed 09:00 +1w>~%* TODO Month~%<2025-01-01 Wed 09:00 +1m>~%* TODO Year~%<2025-01-01 Wed 09:00 +1y>")))
           (entries
             (agenda-result-entries
              (agenda (list document) (make-org-time-value "2025-01-01")
                      (make-org-time-value "2025-01-02")
                      :time-context
                      (agenda-test-context "2025-01-01T00:00:00+00:00")))))
      (expect (length entries) :to-be 5))
    (let* ((document (parse-org
                      (format nil "* TODO Range~%<2025-01-01 Wed 09:00-10:00 ++1d>")))
           (result (agenda (list document) (make-org-time-value "2025-01-02")
                           (make-org-time-value "2025-01-02")
                           :time-context
                           (agenda-test-context "2025-01-02T08:00:00+00:00")))
           (view (agenda-entry-timestamp (first (agenda-result-entries result)))))
      (expect (org-time-value-iso8601 (org-timestamp-view-start view))
              :to-equal "2025-01-02T09:00:00")
      (expect (org-time-value-iso8601 (org-timestamp-view-end view))
              :to-equal "2025-01-02T10:00:00"))
    (let* ((document (parse-org
                      (format nil "* TODO Restart range~%<2025-01-01 Wed 09:00-10:00 .+1d>")))
           (entries (agenda-result-entries
                     (agenda (list document) (make-org-time-value "2025-01-01")
                             (make-org-time-value "2025-01-03")
                             :time-context
                             (agenda-test-context "2025-01-02T08:00:00+00:00"))))
           (view (agenda-entry-timestamp (first entries))))
      (expect (org-time-value-iso8601 (org-timestamp-view-start view))
              :to-equal "2025-01-03T08:00:00")
      (expect (org-time-value-iso8601 (org-timestamp-view-end view))
              :to-equal "2025-01-03T10:00:00")))

  (it "handles catch-up, restart, next and none repeat policies for active timestamps"
    (let* ((context (agenda-test-context "2025-01-03T12:00:00+00:00"))
           (start (make-org-time-value "2025-01-01"))
           (end (make-org-time-value "2025-01-05"))
           (all (parse-org
                 (format nil "* TODO Catch-up~%<2025-01-01 Wed ++1d>~%* TODO Restart~%<2025-01-01 Wed .+1d>")))
           (next (parse-org (format nil "* TODO Next~%<2025-01-01 Wed +1d>")))
           (none (parse-org (format nil "* TODO None~%<2025-01-01 Wed +1d>")))
           (all-entries (agenda-result-entries
                         (agenda (list all) start end :time-context context)))
           (next-entries (agenda-result-entries
                          (agenda (list next) start end :time-context context
                                  :future-repeats :next)))
           (none-entries (agenda-result-entries
                          (agenda (list none) start end :time-context context
                                  :future-repeats :none))))
      (with-soft-assertions
        (expect (mapcar (lambda (entry)
                          (list (agenda-entry-headline entry)
                                (org-time-value-iso8601 (agenda-entry-date entry))))
                        all-entries)
                :to-equal '(("Catch-up" "2025-01-03")
                            ("Catch-up" "2025-01-04")
                            ("Restart" "2025-01-04")
                            ("Catch-up" "2025-01-05")
                            ("Restart" "2025-01-05")))
        (expect (mapcar (lambda (entry)
                          (org-time-value-iso8601 (agenda-entry-date entry)))
                        next-entries)
                :to-equal '("2025-01-03"))
        (expect (mapcar (lambda (entry)
                          (org-time-value-iso8601 (agenda-entry-date entry)))
                        none-entries)
                :to-equal '("2025-01-01")))))

  (it "uses the explicit zone for hourly repeaters across a daylight-saving jump"
    (let* ((document (parse-org
                      (format nil "* TODO Hourly~%<2025-03-30 Sun 01:30 +2h>")))
           (entries (agenda-result-entries
                     (agenda (list document) (make-org-time-value "2025-03-30")
                             (make-org-time-value "2025-03-30")
                             :time-context
                             (agenda-test-context "2025-03-30T03:45:00"
                                                  "Europe/Berlin")
                             :future-repeats :next)))
           (times (mapcar (lambda (entry)
                            (org-time-value-iso8601
                             (org-timestamp-view-start
                              (agenda-entry-timestamp entry))))
                          entries)))
      (with-soft-assertions
        (expect times :to-equal '("2025-03-30T04:30:00")))))

  (it "applies strict DST gap and overlap policies to agenda repeaters"
    (let* ((zone (resolve-time-zone "America/New_York"))
           (gap-context
             (make-time-context
              :reference-time (make-org-time-value "2025-03-08T00:00:00")
              :zone zone :dst-policy :strict))
           (overlap-context
             (make-time-context
              :reference-time (make-org-time-value "2025-11-01T00:00:00")
              :zone zone :dst-policy :strict))
           (gap-error
             (agenda-test-captured-error
              (lambda ()
                (agenda (list (parse-org
                               (format nil "* TODO Gap~%<2025-03-08 02:30 +1d>")))
                        (make-org-time-value "2025-03-08")
                        (make-org-time-value "2025-03-09")
                        :time-context gap-context))))
           (overlap-error
             (agenda-test-captured-error
              (lambda ()
                (agenda (list (parse-org
                               (format nil "* TODO Overlap~%<2025-11-01 01:30 +1d>")))
                        (make-org-time-value "2025-11-01")
                        (make-org-time-value "2025-11-02")
                        :time-context overlap-context)))))
      (with-soft-assertions
        (expect (typep gap-error 'org-time-error) :to-be t)
        (when (typep gap-error 'org-time-error)
          (expect (org-time-error-reason gap-error) :to-be :dst-gap))
        (expect (typep overlap-error 'org-time-error) :to-be t)
        (when (typep overlap-error 'org-time-error)
          (expect (org-time-error-reason overlap-error) :to-be :dst-overlap)))))

  (it "uses negative hourly warning shifts and default agenda options"
    (let* ((document (parse-org
                      (format nil "* TODO Deadline~%DEADLINE: <2025-01-02 Thu -2h>")))
           (result (agenda (list document) (make-org-time-value "2025-01-01")
                           (make-org-time-value "2025-01-02")
                           :time-context
                           (agenda-test-context "2025-01-01T12:00:00+00:00")))
           (rows (agenda-result-entries result))
           (default-warning
             (parse-org (format nil "* TODO Default warning~%DEADLINE: <2025-01-10 Fri>")))
           (default-rows
             (agenda-result-entries
              (agenda (list default-warning)
                      (make-org-time-value "2025-01-01")
                      (make-org-time-value "2025-01-01")
                      :time-context
                      (agenda-test-context "2025-01-01T12:00:00+00:00")))))
      (with-soft-assertions
        (expect (mapcar #'agenda-entry-warning-state rows)
                :to-equal '(:prewarning :due))
        (expect (mapcar #'agenda-entry-warning-state default-rows)
                :to-equal '(:prewarning)))))

  (it "enforces zero row limits for agenda matching and TODO listing"
    (let* ((document
             (parse-org "* TODO Limited"
                        :limits (make-limits :max-agenda-entries 0)))
           (context (agenda-test-context "2025-01-01T12:00:00+00:00"))
           (match-error
             (agenda-test-captured-error
              (lambda ()
                (agenda-match (list document) "TODO=\"TODO\""
                              :time-context context))))
           (todo-error
             (agenda-test-captured-error
              (lambda ()
                (agenda-todo-list (list document) :time-context context)))))
      (with-soft-assertions
        (expect (typep match-error 'org-resource-limit-error) :to-be t)
        (expect (typep todo-error 'org-resource-limit-error) :to-be t)
        (when (typep todo-error 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name todo-error)
                  :to-be :max-agenda-entries)
          (expect (org-resource-limit-error-actual todo-error) :to-be 1)))))
)

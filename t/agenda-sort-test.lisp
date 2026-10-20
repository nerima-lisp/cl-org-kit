(in-package #:cl-org-kit/test)

(defun agenda-sort-test-source ()
  (format nil
          "#+TODO: TODO WAIT | DONE~%#+PRIORITIES: A C B~%* TODO [#A] Beta :z:~%SCHEDULED: <2025-03-05 Wed 15:30 +1w> DEADLINE: <2025-03-07 Fri 13:00>~%:PROPERTIES:~%:CATEGORY: Gamma~%:EFFORT: 30min~%:END:~%<2025-03-04 Tue 10:00> [2025-03-03 Mon 09:00]~%* WAIT [#C] Alpha :a:~%SCHEDULED: <2025-03-04 Tue 12:00> DEADLINE: <2025-03-06 Thu 09:00>~%:PROPERTIES:~%:CATEGORY: Alpha~%:EFFORT: 2h~%:END:~%<2025-03-02 Sun 11:00> [2025-03-05 Wed 10:00]~%* DONE [#B] Delta :m:~%SCHEDULED: <2025-03-03 Mon> DEADLINE: <2025-03-05 Wed>~%:PROPERTIES:~%:CATEGORY: Gamma~%:EFFORT: 1h~%:END:~%<2025-03-06 Thu> [2025-03-02 Sun]~%* Plain~%:PROPERTIES:~%:CATEGORY: Beta~%:END:~%"))

(defun agenda-sort-test-matches (document)
  (let ((entries nil))
    (map-nodes
     (lambda (node)
       (when (eq (node-type node) :headline)
         (multiple-value-bind (line end-line) (node-line-range node)
           (push (make-agenda-match-entry
                  :document document :node node :line line :end-line end-line)
                 entries))))
     (document-root document))
    (nreverse entries)))

(defun agenda-sort-test-name (entry)
  (node-property (agenda-match-entry-node entry) :raw-value))

(defun agenda-sort-test-names (entries)
  (mapcar #'agenda-sort-test-name entries))

(defun agenda-sort-test-context (reference-time)
  (make-time-context :reference-time (make-org-time-value reference-time)
                     :zone (resolve-time-zone "UTC")))

(defun agenda-sort-test-error-reason (function)
  (handler-case (progn (funcall function) nil)
    (org-agenda-error (condition) (org-agenda-error-reason condition))))

(defun agenda-sort-test-heading (document title)
  (find title (agenda-sort-test-matches document)
        :key #'agenda-sort-test-name :test #'string=))

(defun agenda-sort-test-agenda-entry (document heading kind date &key habit-p)
  (let* ((node (agenda-match-entry-node heading))
         (planning (org-kit::agenda-planning-node node))
         (timestamp-node
           (case kind
             (:scheduled (and planning (node-property planning :scheduled)))
             (:deadline (and planning (node-property planning :deadline)))
             (:timestamp (org-kit::query-entry-timestamp node :active)))))
    (make-agenda-entry
     :date (and date (make-org-time-value date))
     :kind kind :headline (node-property node :raw-value)
     :todo-state (node-property node :todo-keyword)
     :timestamp (and timestamp-node (timestamp-view timestamp-node))
     :habit-p habit-p :source-node (or planning timestamp-node node))))

(describe "Agenda view sorting"
  (it "preserves the caller sequence when no sorting strategy is selected"
    (let* ((entries (agenda-sort-test-matches
                     (parse-org (agenda-sort-test-source))))
           (vector (coerce entries 'vector)))
      (with-soft-assertions
        (expect (org-kit::agenda-sort-view-entries entries nil) :to-be entries)
        (expect (org-kit::agenda-sort-view-entries vector nil) :to-be vector))))

  (it "matches Org 9.8.10 direction and missing-value behavior for every strategy"
    (let* ((document (parse-org (agenda-sort-test-source)))
           (entries (agenda-sort-test-matches document))
           (context (agenda-sort-test-context "2025-03-04T12:00:00+00:00"))
           (sorter
             (lambda (left right)
               (let ((left-name (agenda-sort-test-name left))
                     (right-name (agenda-sort-test-name right)))
                 (cond ((string< left-name right-name) -1)
                       ((string> left-name right-name) 1)))))
           (cases
             '(((:time-up) ("Beta" "Alpha" "Delta" "Plain"))
               ((:time-down) ("Beta" "Alpha" "Delta" "Plain"))
               ((:timestamp-up) ("Delta" "Alpha" "Beta" "Plain"))
               ((:timestamp-down) ("Plain" "Beta" "Alpha" "Delta"))
               ((:scheduled-up) ("Delta" "Alpha" "Beta" "Plain"))
               ((:scheduled-down) ("Plain" "Beta" "Alpha" "Delta"))
               ((:deadline-up) ("Delta" "Alpha" "Beta" "Plain"))
               ((:deadline-down) ("Plain" "Beta" "Alpha" "Delta"))
               ((:ts-up) ("Alpha" "Beta" "Delta" "Plain"))
               ((:ts-down) ("Plain" "Delta" "Beta" "Alpha"))
               ((:tsia-up) ("Delta" "Beta" "Alpha" "Plain"))
               ((:tsia-down) ("Plain" "Alpha" "Beta" "Delta"))
               ((:category-keep) ("Beta" "Alpha" "Delta" "Plain"))
               ((:category-up) ("Alpha" "Plain" "Beta" "Delta"))
               ((:category-down) ("Beta" "Delta" "Plain" "Alpha"))
               ((:tag-up) ("Alpha" "Delta" "Beta" "Plain"))
               ((:tag-down) ("Plain" "Beta" "Delta" "Alpha"))
               ((:priority-up) ("Alpha" "Delta" "Plain" "Beta"))
               ((:priority-down) ("Beta" "Delta" "Plain" "Alpha"))
               ((:stats-up) ("Beta" "Alpha" "Delta" "Plain"))
               ((:stats-down) ("Beta" "Alpha" "Delta" "Plain"))
               ((:urgency-up) ("Alpha" "Delta" "Plain" "Beta"))
               ((:urgency-down) ("Beta" "Delta" "Plain" "Alpha"))
               ((:todo-state-up) ("Delta" "Beta" "Alpha" "Plain"))
               ((:todo-state-down) ("Delta" "Beta" "Alpha" "Plain"))
               ((:effort-up) ("Beta" "Delta" "Alpha" "Plain"))
               ((:effort-down) ("Plain" "Alpha" "Delta" "Beta"))
               ((:habit-up) ("Beta" "Alpha" "Delta" "Plain"))
               ((:habit-down) ("Beta" "Alpha" "Delta" "Plain"))
               ((:alpha-up) ("Alpha" "Beta" "Delta" "Plain"))
               ((:alpha-down) ("Plain" "Delta" "Beta" "Alpha"))
               ((:user-defined-up) ("Alpha" "Beta" "Delta" "Plain"))
               ((:user-defined-down) ("Plain" "Delta" "Beta" "Alpha")))))
      (with-soft-assertions
        (expect (length cases) :to-be 33)
        (dolist (case cases)
          (let* ((strategy (first case))
                 (options
                   (append (list :time-context context)
                           (when (member :user-defined-up strategy)
                             (list :user-defined-sorter sorter))
                           (when (member :user-defined-down strategy)
                             (list :user-defined-sorter sorter))))
                 (actual
                   (apply #'org-kit::agenda-sort-view-entries entries strategy options)))
            (expect (agenda-sort-test-names actual) :to-equal (second case)))))))

  (it "sorts dates independently and reads timestamps from Org properties"
    (let* ((document (parse-org (agenda-sort-test-source)))
           (entries (agenda-sort-test-matches document))
           (first (first entries))
           (second (second entries))
           (third (third entries))
           (fourth (fourth entries))
           (scope-entries
             (list (agenda-sort-test-agenda-entry
                    document first :todo "2025-03-04")
                   (agenda-sort-test-agenda-entry
                    document second :todo "2025-03-04")
                   (agenda-sort-test-agenda-entry
                    document fourth :todo "2025-03-05")
                   (agenda-sort-test-agenda-entry
                    document third :todo "2025-03-05")))
           (property-document
             (parse-org
              (format nil
                      "* Active Later~%:PROPERTIES:~%:NOTE: <2025-03-04 Tue>~%:END:~%* Active Earlier~%:PROPERTIES:~%:NOTE: <2025-03-02 Sun>~%:END:~%* Inactive Later~%:PROPERTIES:~%:NOTE: [2025-03-03 Mon]~%:END:~%* Inactive Earlier~%:PROPERTIES:~%:NOTE: [2025-03-01 Sat]~%:END:~%")))
           (property-entries (agenda-sort-test-matches property-document)))
      (with-soft-assertions
        (expect (agenda-sort-test-names
                 (org-kit::agenda-sort-view-entries (list first second)
                                                    '(:category-keep :priority-up)))
                :to-equal '("Beta" "Alpha"))
        (expect (agenda-sort-test-names
                 (org-kit::agenda-sort-view-entries (list first third)
                                                    '(:category-keep :priority-up)))
                :to-equal '("Delta" "Beta"))
        (expect (agenda-sort-test-names
                 (org-kit::agenda-sort-view-entries (list third first)
                                                    '(:priority-up)))
                :to-equal '("Delta" "Beta"))
        (expect (mapcar #'agenda-entry-headline
                        (org-kit::agenda-sort-view-entries
                         scope-entries '(:priority-up)))
                :to-equal '("Alpha" "Beta" "Plain" "Delta"))
        (expect (agenda-sort-test-names
                 (org-kit::agenda-sort-view-entries property-entries '(:ts-up)))
                :to-equal '("Active Earlier" "Active Later"
                            "Inactive Later" "Inactive Earlier"))
        (expect (agenda-sort-test-names
                 (org-kit::agenda-sort-view-entries property-entries '(:tsia-up)))
                :to-equal '("Inactive Earlier" "Inactive Later"
                            "Active Later" "Active Earlier"))
        (expect (agenda-sort-test-names
                 (org-kit::agenda-sort-view-entries property-entries
                                                    '(:timestamp-up)))
                :to-equal '("Inactive Earlier" "Active Earlier"
                            "Inactive Later" "Active Later")))))

  (it "uses alphabetic and numeric document priority ranges and their defaults"
    (let* ((alphabetic
             (parse-org (format nil
                                "#+PRIORITIES: A E C~%* [#A] High~%* [#E] Low~%* Missing~%")))
           (numeric
             (parse-org (format nil
                                "#+PRIORITIES: 1 10 5~%* [#1] High~%* [#10] Low~%* Missing~%")))
           (alpha-entries (agenda-sort-test-matches alphabetic))
           (numeric-entries (agenda-sort-test-matches numeric))
           (default-entries
             (agenda-sort-test-matches
              (parse-org (format nil "* [#A] High~%* Missing~%* [#C] Low~%")))))
      (with-soft-assertions
        (expect (mapcar #'org-kit::agenda-sort-view-entry-priority alpha-entries)
                :to-equal '(4000 0 2000))
        (expect (agenda-sort-test-names
                 (org-kit::agenda-sort-view-entries alpha-entries '(:priority-up)))
                :to-equal '("Low" "Missing" "High"))
        (expect (mapcar #'org-kit::agenda-sort-view-entry-priority numeric-entries)
                :to-equal '(9000 0 5000))
        (expect (agenda-sort-test-names
                 (org-kit::agenda-sort-view-entries numeric-entries '(:priority-up)))
                :to-equal '("Low" "Missing" "High"))
        (expect (mapcar #'org-kit::agenda-sort-view-entry-priority default-entries)
                :to-equal '(2000 1000 0)))))
  (it "matches Org statistics cookies and sorting directions"
    (let* ((document
             (parse-org
              (format nil
                      "* Parent [1/2]~%** Inherited~%* Percent [10%]~%* Quarter [1/4]~%* Zero denominator [1/0]~%* Missing~%* Three quarters [3/4]~%")))
           (entries (agenda-sort-test-matches document)))
      (with-soft-assertions
        (expect (mapcar #'org-kit::agenda-sort-view-entry-statistics entries)
                :to-equal
                '(50 50 10 25 0 0 75))
        (expect
         (mapcar #'org-kit::agenda-sort-view-entry-statistics
                 (org-kit::agenda-sort-view-entries entries '(:stats-up)))
         :to-equal
         '(0 0 10 25 50 50 75))
        (expect
         (mapcar #'org-kit::agenda-sort-view-entry-statistics
                 (org-kit::agenda-sort-view-entries entries '(:stats-down)))
         :to-equal
         '(75 50 50 25 10 0 0)))))

  (it "uses normalized planning and habit data for explicit-context urgency"
    (let* ((document
             (parse-org
              (format nil
                      "#+TODO: TODO | DONE~%* TODO [#A] Todo~%SCHEDULED: <2025-03-04 Tue 12:00> DEADLINE: <2025-03-06 Thu 09:00>~%* Scheduled~%SCHEDULED: <2025-03-04 Tue 12:00>~%* Deadline~%DEADLINE: <2025-03-06 Thu 09:00>~%* Habit~%SCHEDULED: <2025-03-05 Wed 10:00 -2d +1w>~%:PROPERTIES:~%:STYLE: habit~%:END:~%* [#C] Timestamp~%<2025-03-02 Sun 11:00>~%")))
           (headings (agenda-sort-test-matches document))
           (todo (agenda-sort-test-agenda-entry document (first headings) :todo "2025-03-04"))
           (scheduled (agenda-sort-test-agenda-entry document (second headings)
                                                     :scheduled "2025-03-04"))
           (deadline (agenda-sort-test-agenda-entry document (third headings)
                                                    :deadline "2025-03-04"))
           (habit (agenda-sort-test-agenda-entry document (fourth headings) :scheduled "2025-03-04" :habit-p t))
           (timestamp (agenda-sort-test-agenda-entry document (fifth headings) :timestamp "2025-03-04"))
           (context (agenda-sort-test-context "2025-03-04T12:00:00+00:00")))
      (with-soft-assertions
        (expect (mapcar (lambda (entry)
                          (org-kit::agenda-sort-view-entry-urgency entry context))
                        (list todo scheduled deadline habit timestamp))
                :to-equal '(2001 1099 998 990 0))
        (expect (mapcar #'agenda-entry-headline
                        (org-kit::agenda-sort-view-entries
                         (list todo scheduled deadline habit timestamp)
                         '(:urgency-up) :time-context context))
                :to-equal '("Timestamp" "Habit" "Deadline" "Scheduled" "Todo"))
        (progn
          (expect (mapcar #'agenda-entry-headline
                          (org-kit::agenda-sort-view-entries
                           (list todo scheduled deadline habit timestamp)
                           '(:urgency-down) :time-context context))
                  :to-equal '("Todo" "Scheduled" "Deadline" "Habit" "Timestamp"))
          (expect (mapcar #'agenda-entry-headline
                          (org-kit::agenda-sort-view-entries
                           (list todo scheduled deadline habit timestamp) '(:habit-up)))
                  :to-equal '("Habit" "Todo" "Scheduled" "Deadline" "Timestamp"))
          (expect (mapcar #'agenda-entry-headline
                          (org-kit::agenda-sort-view-entries
                           (list todo scheduled deadline habit timestamp) '(:habit-down)))
                  :to-equal '("Todo" "Scheduled" "Deadline" "Timestamp" "Habit"))
          (expect (mapcar #'agenda-entry-headline
                          (org-kit::agenda-sort-view-entries
                           (list todo scheduled deadline habit timestamp) '(:time-up)))
                  :to-equal '("Deadline" "Habit" "Timestamp" "Scheduled" "Todo"))
          (expect (mapcar #'agenda-entry-headline
                          (org-kit::agenda-sort-view-entries
                           (list todo scheduled deadline habit timestamp) '(:time-down)))
                  :to-equal '("Todo" "Scheduled" "Timestamp" "Habit" "Deadline"))))))
  (it "matches Org habit window urgency, duration conversion, and invalid periods"
    (let* ((document
             (parse-org
              (format nil
                      "* Weekly~%SCHEDULED: <2025-03-05 Wed +1w/2w>~%:PROPERTIES:~%:STYLE: habit~%:END:~%* Monthly~%SCHEDULED: <2025-03-05 Wed +1m/2m>~%:PROPERTIES:~%:STYLE: habit~%:END:~%* Invalid window~%SCHEDULED: <2025-03-05 Wed +1w/1w>~%:PROPERTIES:~%:STYLE: habit~%:END:~%* Missing repeat~%SCHEDULED: <2025-03-05 Wed>~%:PROPERTIES:~%:STYLE: habit~%:END:~%")))
           (headings (agenda-sort-test-matches document))
           (weekly
             (agenda-sort-test-agenda-entry
              document (first headings) :scheduled "2025-03-12" :habit-p t))
           (monthly
             (agenda-sort-test-agenda-entry
              document (second headings) :scheduled "2025-04-04" :habit-p t))
           (invalid-window
             (agenda-sort-test-agenda-entry
              document (third headings) :scheduled "2025-03-12" :habit-p t))
           (invalid-repeat
             (agenda-sort-test-agenda-entry
              document (fourth headings) :scheduled "2025-03-05" :habit-p t)))
      (with-soft-assertions
        (expect (org-kit::agenda-sort-view-entry-urgency
                 weekly (agenda-sort-test-context "2025-03-12T12:00:00+00:00"))
                :to-be
                1220)
        (expect (org-kit::agenda-sort-view-entry-urgency
                 weekly (agenda-sort-test-context "2025-03-11T12:00:00+00:00"))
                :to-be
                1060)
        (expect (org-kit::agenda-sort-view-entry-urgency
                 weekly (agenda-sort-test-context "2025-03-13T12:00:00+00:00"))
                :to-be
                1280)
        (expect (org-kit::agenda-sort-view-entry-urgency
                 monthly (agenda-sort-test-context "2025-04-04T12:00:00+00:00"))
                :to-be
                1450)
        (dolist (invalid-entry (list invalid-window invalid-repeat))
          (expect (agenda-sort-test-error-reason
                   (lambda ()
                     (org-kit::agenda-sort-view-entry-urgency
                      invalid-entry
                      (agenda-sort-test-context
                       "2025-03-12T12:00:00+00:00"))))
                  :to-be
                  :unsupported-entry)))))

  (it "rejects malformed strategies, entries, sorter results, and missing urgency context"
    (let* ((entries (agenda-sort-test-matches
                     (parse-org (agenda-sort-test-source))))
           (circular (list :alpha-up)))
      (setf (cdr circular) circular)
      (with-soft-assertions
        (expect (agenda-sort-test-error-reason
                 (lambda () (org-kit::agenda-sort-view-entries entries '(:unknown))))
                :to-be :invalid-option)
        (expect (agenda-sort-test-error-reason
                 (lambda () (org-kit::agenda-sort-view-entries entries '(:alpha-up . :priority-up))))
                :to-be :invalid-option)
        (expect (agenda-sort-test-error-reason
                 (lambda () (org-kit::agenda-sort-view-entries entries circular)))
                :to-be :invalid-option)
        (expect (agenda-sort-test-error-reason
                 (lambda () (org-kit::agenda-sort-view-entries entries '(:user-defined-down))))
                :to-be :invalid-option)
        (expect (agenda-sort-test-error-reason
                 (lambda () (org-kit::agenda-sort-view-entries entries '(:user-defined-up)
                                                      :user-defined-sorter
                                                      (lambda (left right)
                                                        (declare (ignore left right))
                                                        0))))
                :to-be :invalid-option)
        (expect (agenda-sort-test-error-reason
                 (lambda () (org-kit::agenda-sort-view-entries '(1) '(:alpha-up))))
                :to-be :unsupported-entry)
        (expect (agenda-sort-test-error-reason
                 (lambda () (org-kit::agenda-sort-view-entries entries '(:urgency-up))))
                :to-be :time-context-required))))
  (it "sorts match headlines by the first plain Org time and ignores timestamp times"
    (let* ((document
             (parse-org
              (format nil
                      "* Untimed 8~%* Timestamp only <2025-03-05 Wed 01:00>~%* Evening 8PM~%* Morning [2025-03-05 Wed 02:00] 8:30~%* Range 7am-9am~%* Midnight 12am~%")))
           (entries (agenda-sort-test-matches document)))
      (with-soft-assertions
        (expect (agenda-sort-test-names
                 (org-kit::agenda-sort-view-entries entries '(:time-up)))
                :to-equal
                '("Midnight 12am" "Range 7am-9am"
                  "Morning [2025-03-05 Wed 02:00] 8:30" "Evening 8PM"
                  "Untimed 8" "Timestamp only <2025-03-05 Wed 01:00>"))
        (expect (agenda-sort-test-names
                 (org-kit::agenda-sort-view-entries entries '(:time-down)))
                :to-equal
                '("Untimed 8" "Timestamp only <2025-03-05 Wed 01:00>"
                  "Evening 8PM" "Morning [2025-03-05 Wed 02:00] 8:30"
                  "Range 7am-9am" "Midnight 12am")))))
  (it "reads agenda-row keys and filters timestamps by source type"
    (let* ((rows
             (list
              (make-agenda-entry :kind :todo :headline "Zebra" :todo-state "TODO"
                                 :effective-tags '("project" "z") :category "Beta")
              (make-agenda-entry :kind :todo :headline "alpha" :todo-state "WAIT"
                                 :effective-tags '("project" "a") :category "Gamma")
              (make-agenda-entry :kind :todo :headline "Beta" :todo-state "DONE"
                                 :effective-tags nil :category "Alpha")))
           (source-document
             (parse-org
              (format nil
                      "* Active~%:PROPERTIES:~%:NOTE: <2025-03-04 Tue>~%:END:~%* Inactive~%:PROPERTIES:~%:NOTE: [2025-03-03 Mon]~%:END:~%")))
           (source-headings (agenda-sort-test-matches source-document))
           (active-node
             (org-kit::agenda-sort-view-entry-timestamp-node
              (first source-headings) :active))
           (inactive-node
             (org-kit::agenda-sort-view-entry-timestamp-node
              (second source-headings) :inactive))
           (active-row
             (make-agenda-entry :kind :timestamp :source-node active-node))
           (inactive-row
             (make-agenda-entry :kind :timestamp :source-node inactive-node)))
      (with-soft-assertions
        (expect (mapcar (function agenda-entry-headline)
                 (org-kit::agenda-sort-view-entries
                  rows (quote (:alpha-up))))
                :to-equal '("alpha" "Beta" "Zebra"))
        (expect (mapcar (function agenda-entry-headline)
                 (org-kit::agenda-sort-view-entries
                  rows (quote (:category-up))))
                :to-equal '("Beta" "Zebra" "alpha"))
        (expect (mapcar (function agenda-entry-headline)
                 (org-kit::agenda-sort-view-entries
                  rows (quote (:tag-up))))
                :to-equal '("alpha" "Zebra" "Beta"))
        (expect (mapcar (function agenda-entry-headline)
                 (org-kit::agenda-sort-view-entries
                  rows (quote (:tag-down))))
                :to-equal '("Beta" "Zebra" "alpha"))
        (expect (mapcar #'org-kit::agenda-sort-view-entry-todo-state rows)
                :to-equal '("TODO" "WAIT" "DONE"))
        (expect (org-kit::agenda-sort-view-entry-priority (first rows))
                :to-be 1000)
        (expect (list (org-kit::agenda-sort-view-compare-strings nil nil :up)
                      (org-kit::agenda-sort-view-compare-strings nil "A" :up)
                      (org-kit::agenda-sort-view-compare-strings "A" nil :up))
                :to-equal '(nil 1 -1))
        (expect (mapcar
                 (lambda (kind)
                   (not (null
                         (org-kit::agenda-sort-view-entry-timestamp
                          active-row kind))))
                 '(:active :inactive :any))
                :to-equal '(t nil t))
        (expect (mapcar
                 (lambda (kind)
                   (not (null
                         (org-kit::agenda-sort-view-entry-timestamp
                          inactive-row kind))))
                 '(:active :inactive :any))
                :to-equal '(nil t t)))))
  (it "preserves sorted vectors and rejects malformed entry sequences"
    (let* ((entries (agenda-sort-test-matches
                     (parse-org (agenda-sort-test-source))))
           (vector (coerce entries 'vector))
           (sorted-vector
             (org-kit::agenda-sort-view-entries vector '(:alpha-up)))
           (pair (subseq entries 0 2))
           (circular (list (first entries))))
      (setf (cdr circular) circular)
      (with-soft-assertions
        (expect (typep sorted-vector 'vector) :to-be-truthy)
        (expect (agenda-sort-test-names (coerce sorted-vector 'list))
                :to-equal '("Alpha" "Beta" "Delta" "Plain"))
        (dolist (criterion '(:user-defined-up :user-defined-down))
          (expect (agenda-sort-test-names
                   (org-kit::agenda-sort-view-entries
                    pair (list criterion)
                    :user-defined-sorter
                    (lambda (left right)
                      (declare (ignore left right))
                      nil)))
                  :to-equal '("Beta" "Alpha")))
        (expect (agenda-sort-test-error-reason
                 (lambda ()
                   (org-kit::agenda-sort-view-entries
                    entries '(:user-defined-up))))
                :to-be :invalid-option)
        (expect (agenda-sort-test-error-reason
                 (lambda ()
                   (org-kit::agenda-sort-view-entries 42 '(:alpha-up))))
                :to-be :unsupported-entry)
        (expect (agenda-sort-test-error-reason
                 (lambda ()
                   (org-kit::agenda-sort-view-entries circular '(:alpha-up))))
                :to-be :unsupported-entry))))
  (it "handles bare timestamp closers, line breaks, and time token boundaries"
    (let* ((document
             (parse-org
              (format nil
                      "* Bare <2025-03-05> 6:15pm~%* Invalid suffix 7:15pmx~%* Boundary~%")))
           (entries (agenda-sort-test-matches document))
           (bare (first entries))
           (invalid (second entries))
           (boundary (third entries))
           (boundary-node (agenda-match-entry-node boundary)))
      (with-soft-assertions
        (expect (org-kit::agenda-sort-view-match-entry-time-key bare)
                :to-be 1815)
        (expect (org-kit::agenda-sort-view-match-entry-time-key invalid)
                :to-be nil)
        (dolist (line-break (list #\Newline #\Return))
          (setf (node-property boundary-node :raw-value)
                (format nil "Unclosed <2025-03-05 Wed~c8:30>" line-break))
          (expect (org-kit::agenda-sort-view-match-entry-time-key boundary)
                  :to-be
                  830)))))
  (it "keeps planning and timestamp lookup within the source headline"
    (let* ((document
             (parse-org
              (format nil
                      "* Parent~%** Child~%<2025-03-05 Wed>~%* Deadline~%DEADLINE: <2025-03-06 Thu>~%* Short~%SCHEDULED: <2025-03-05 Wed +1d>~%* Habit~%SCHEDULED: <2025-03-05 Wed +1w>~%:PROPERTIES:~%:STYLE: habit~%:END:~%")))
           (headings (agenda-sort-test-matches document))
           (parent (first headings))
           (child (second headings))
           (deadline-heading (third headings))
           (short-heading (fourth headings))
           (habit-heading (fifth headings))
           (child-timestamp
             (org-kit::agenda-sort-view-entry-timestamp-node child :active))
           (deadline-timestamp
             (org-kit::agenda-sort-view-entry-timestamp-node
              deadline-heading :deadline))
           (short-entry
             (agenda-sort-test-agenda-entry
              document short-heading :scheduled "2025-03-05" :habit-p t))
           (habit-entry
             (agenda-sort-test-agenda-entry
              document habit-heading :scheduled "2025-03-05"))
           (deadline-entry
             (agenda-sort-test-agenda-entry
              document deadline-heading :deadline "2025-03-05"))
           (timestamp-entry
             (make-agenda-entry
              :date (make-org-time-value "2025-03-05")
              :kind :timestamp :headline "Habit timestamp" :habit-p t
              :source-node (agenda-match-entry-node habit-heading)))
           (missing-scheduled (make-agenda-entry :kind :scheduled))
           (missing-deadline (make-agenda-entry :kind :deadline))
           (context (agenda-sort-test-context "2025-03-05T12:00:00+00:00")))
      (with-soft-assertions
        (expect (org-kit::agenda-sort-view-entry-timestamp-node parent :active)
                :to-be nil)
        (expect (node-property child-timestamp :day-start) :to-be 5)
        (expect (node-property deadline-timestamp :day-start) :to-be 6)
        (expect (list (not (null
                            (org-kit::agenda-sort-view-entry-habit-p habit-entry)))
                      (null
                       (org-kit::agenda-sort-view-entry-habit-p deadline-entry)))
                :to-equal '(t t))
        (expect (mapcar #'agenda-entry-headline
                        (org-kit::agenda-sort-view-entries
                         (list deadline-entry habit-entry) '(:habit-up)))
                :to-equal '("Habit" "Deadline"))
        (expect (mapcar #'agenda-entry-headline
                        (org-kit::agenda-sort-view-entries
                         (list deadline-entry habit-entry) '(:habit-down)))
                :to-equal '("Deadline" "Habit"))
        (expect (agenda-sort-test-error-reason
                 (lambda ()
                   (org-kit::agenda-sort-view-entry-urgency
                    missing-scheduled context)))
                :to-be :unsupported-entry)
        (expect (agenda-sort-test-error-reason
                 (lambda ()
                   (org-kit::agenda-sort-view-entry-urgency
                    missing-deadline context)))
                :to-be :unsupported-entry)
        (let ((original-repeat-days
              (symbol-function
               (quote org-kit::agenda-habit-repeat-days))))
          (unwind-protect
               (progn
                 (expect
                  (list
                   (org-kit:org-timestamp-view-repeater-amount
                    (agenda-entry-timestamp short-entry))
                   (org-kit:org-timestamp-view-repeater-unit
                    (agenda-entry-timestamp short-entry)))
                  :to-equal (quote (1 :day)))
                 (setf (symbol-function
                        (quote org-kit::agenda-habit-repeat-days))
                       (constantly 0))
                 (expect
                  (agenda-sort-test-error-reason
                   (lambda ()
                     (org-kit::agenda-sort-view-entry-urgency
                      short-entry context)))
                  :to-be :unsupported-entry))
            (setf (symbol-function
                   (quote org-kit::agenda-habit-repeat-days))
                  original-repeat-days)))
        (expect (org-kit::agenda-sort-view-entry-urgency timestamp-entry context)
                :to-be
                (org-kit::agenda-sort-view-entry-urgency habit-entry context)))))
  (it "ranks canonical structured TODO keyword settings"
    (let* ((document
             (parse-org
              (format nil
                      "#+TODO: TODO | DONE~%* TODO Open~%* DONE Finished~%")))
           (entries (agenda-sort-test-matches document)))
      (setf (document-settings document)
            (list :todo-keywords
                  (list (list :keyword "TODO")
                        (list :keyword "DONE"))
                  :todo-sequences nil))
      (with-soft-assertions
        (expect (mapcar (function org-kit::agenda-sort-view-entry-todo-rank)
                        entries)
                :to-equal (quote (-2 -1)))
        (expect (agenda-sort-test-names
                 (org-kit::agenda-sort-view-entries
                  entries (quote (:todo-state-up))))
                :to-equal (quote ("Finished" "Open"))))))
  (it "ranks TODO states from string keyword settings"
    (let* ((document
             (parse-org
              (format nil
                      "#+TODO: TODO NEXT WAIT | DONE~%* TODO First~%* NEXT Second~%* WAIT Third~%")))
           (entries (agenda-sort-test-matches document)))
      (setf (document-settings document)
            (list :todo-keywords
                  (list "TODO" "NEXT" "WAIT" "DONE")
                  :todo-sequences nil))
      (with-soft-assertions
        (expect (mapcar (function org-kit::agenda-sort-view-entry-todo-rank)
                        entries)
                :to-equal (quote (-4 -3 -2)))
        (expect (org-kit::agenda-sort-view-compare-todo-state
                 (first entries) (second entries) :up)
                :to-be -1)
        (expect (org-kit::agenda-sort-view-compare-todo-state
                 (second entries) (first entries) :up)
                :to-be 1)
        (expect (agenda-sort-test-names
                 (org-kit::agenda-sort-view-entries
                  entries (quote (:todo-state-up))))
                :to-equal (quote ("First" "Second" "Third"))))))
  (it "uses a deadline timestamp for :any when schedule is absent"
    (let* ((document
             (parse-org
              (format nil
                      "* Deadline only~%DEADLINE: <2025-03-06 Thu>~%")))
           (entry (first (agenda-sort-test-matches document)))
           (timestamp
             (org-kit::agenda-sort-view-entry-timestamp-node entry :any)))
      (with-soft-assertions
        (expect (list (node-property timestamp :type)
                      (node-property timestamp :day-start))
                :to-equal (quote (:active 6)))))))

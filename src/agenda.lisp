;;;; src/agenda.lisp
(in-package #:org-kit)

(defstruct (agenda-entry (:constructor %make-agenda-entry)
                         (:conc-name %agenda-entry-)
                         (:copier nil)
                         (:predicate %agenda-entry-p))
  "One normalized agenda row associated with a source headline and timestamp."
  date date-order-key kind headline heading-path timestamp warning-state
  scheduled-delay-state todo-state effective-tags category habit-p
  habit-consistency source-node)

(defun make-agenda-entry (&key date kind headline heading-path timestamp
                               warning-state scheduled-delay-state todo-state
                               effective-tags category habit-p habit-consistency source-node)
  "Return an agenda row with its date, kind, headline, and category."
  (let ((date-order-key
          (when (and (org-time-value-p date)
                     (eq (org-time-value-kind date) :date))
            (cl-date-kit:local-date-to-epoch-day
             (cl-date-kit:make-local-date (org-time-value-year date)
                                          (org-time-value-month date)
                                          (org-time-value-day date))))))
    (%make-agenda-entry :date date :date-order-key date-order-key
                        :kind kind :headline headline
                        :heading-path heading-path :timestamp timestamp
                        :warning-state warning-state
                        :scheduled-delay-state scheduled-delay-state
                        :todo-state todo-state :effective-tags effective-tags
                        :category category :habit-p habit-p
                        :habit-consistency habit-consistency
                        :source-node source-node)))

(defun agenda-entry-p (object)
  "Return true when OBJECT is an agenda entry."
  (%agenda-entry-p object))

(defun agenda-entry-date (entry)
  "Return ENTRY's agenda date as an ORG-TIME-VALUE."
  (%agenda-entry-date entry))

(defun agenda-entry-kind (entry)
  "Return ENTRY's source kind, such as :DEADLINE or :SCHEDULED."
  (%agenda-entry-kind entry))

(defun agenda-entry-headline (entry)
  "Return ENTRY's raw headline title."
  (%agenda-entry-headline entry))

(defun agenda-entry-heading-path (entry)
  "Return ENTRY's node-heading-path value."
  (%agenda-entry-heading-path entry))

(defun agenda-entry-timestamp (entry)
  "Return ENTRY's normalized ORG-TIMESTAMP-VIEW."
  (%agenda-entry-timestamp entry))

(defun agenda-entry-warning-state (entry)
  "Return ENTRY's deadline warning state."
  (%agenda-entry-warning-state entry))

(defun agenda-entry-scheduled-delay-state (entry)
  "Return ENTRY's scheduled delay state."
  (%agenda-entry-scheduled-delay-state entry))

(defun agenda-entry-todo-state (entry)
  "Return ENTRY's TODO keyword, or NIL when it has none."
  (%agenda-entry-todo-state entry))

(defun agenda-entry-effective-tags (entry)
  "Return ENTRY's effective tags, including inherited tags."
  (%agenda-entry-effective-tags entry))

(defun agenda-entry-category (entry)
  "Return the effective Org category for ENTRY."
  (%agenda-entry-category entry))

(defun agenda-entry-habit-p (entry)
  "Return true when ENTRY's headline has the habit style."
  (%agenda-entry-habit-p entry))

(defun agenda-entry-habit-consistency (entry)
  "Return habit consistency rows for ENTRY, or NIL for other agenda rows.

Each row is a plist with :DATE as a date-only ORG-TIME-VALUE and :STATUS as
:COMPLETED, :WITHIN-SCHEDULE, or :LATE. A DONE state log timestamp, or CLOSED
on a currently done heading, marks its date completed. The current SCHEDULED
timestamp anchors the active occurrence and sets its due date; this is the due
date after the latest recorded completion. Earlier due dates are reconstructed
from completion dates according to repeater mode. Dates after their due date
are :LATE."
  (%agenda-entry-habit-consistency entry))

(defun agenda-entry-source-node (entry)
  "Return ENTRY's source timestamp, planning, or diary node."
  (%agenda-entry-source-node entry))

(defstruct (agenda-result (:constructor %make-agenda-result)
                         (:conc-name %agenda-result-)
                         (:copier nil)
                         (:predicate %agenda-result-p))
  "A sequence of normalized agenda entries and its explicit time context."
  entries start-date end-date time-context)

(defun make-agenda-result (&key entries start-date end-date time-context)
  "Return an agenda result containing ENTRIES and the requested date range."
  (%make-agenda-result :entries entries :start-date start-date :end-date end-date
                       :time-context time-context))

(defun agenda-result-p (object)
  "Return true when OBJECT is an agenda result."
  (%agenda-result-p object))

(defun agenda-result-entries (result)
  "Return RESULT's agenda entries in stable order."
  (%agenda-result-entries result))

(defun agenda-result-start-date (result)
  "Return RESULT's inclusive start date."
  (%agenda-result-start-date result))

(defun agenda-result-end-date (result)
  "Return RESULT's inclusive end date."
  (%agenda-result-end-date result))

(defun agenda-result-time-context (result)
  "Return RESULT's explicit time context."
  (%agenda-result-time-context result))

(defstruct (agenda-match-entry (:constructor %make-agenda-match-entry)
                               (:conc-name %agenda-match-entry-)
                               (:copier nil)
                               (:predicate %agenda-match-entry-p))
  "A matched headline with its source document and line range."
  document node heading-path line end-line)

(defun make-agenda-match-entry (&key document node heading-path line end-line)
  "Return a match result for NODE in DOCUMENT."
  (%make-agenda-match-entry :document document :node node
                            :heading-path heading-path :line line
                            :end-line end-line))

(defun agenda-match-entry-p (object)
  "Return true when OBJECT is an agenda match entry."
  (%agenda-match-entry-p object))

(defun agenda-match-entry-document (entry)
  "Return ENTRY's source document."
  (%agenda-match-entry-document entry))

(defun agenda-match-entry-node (entry)
  "Return ENTRY's matched headline node."
  (%agenda-match-entry-node entry))

(defun agenda-match-entry-heading-path (entry)
  "Return ENTRY's node-heading-path value."
  (%agenda-match-entry-heading-path entry))

(defun agenda-match-entry-line (entry)
  "Return ENTRY's 1-based first source line."
  (%agenda-match-entry-line entry))

(defun agenda-match-entry-end-line (entry)
  "Return ENTRY's inclusive 1-based final source line."
  (%agenda-match-entry-end-line entry))

(defun agenda-signal-error (reason context message)
  "Signal an ORG-AGENDA-ERROR with bounded CONTEXT and REASON."
  (error 'org-agenda-error :message message :reason reason
         :context
         (bounded-text
          (if (stringp context)
              context
              (with-output-to-string (stream)
                (let ((*print-circle* t) (*print-length* 8) (*print-level* 3))
                  (prin1 context stream)))))))

(defun agenda-proper-list-length (value)
  "Return the length of proper VALUE, or NIL for an improper or circular list."
  (handler-case (list-length value)
    (type-error () nil)))

(defun agenda-normalize-documents (documents)
  "Validate DOCUMENTS and return its elements in caller order."
  (when (null (agenda-proper-list-length documents))
    (agenda-signal-error :unsupported-entry documents
                         "Agenda documents must be a proper list"))
  (dolist (document documents)
    (unless (document-p document)
      (agenda-signal-error :unsupported-entry document
                           "Agenda input must be a document")))
  documents)

(defun agenda-operation-limits (documents)
  "Return agenda limits set to the minimum across DOCUMENTS."
  (let ((limits (mapcar #'document-limits documents)))
    (make-limits
     :max-agenda-range-days
     (minimum-limit-value limits :max-agenda-range-days)
     :max-agenda-entries
     (minimum-limit-value limits :max-agenda-entries)
     :max-diary-work
     (minimum-limit-value limits :max-diary-work)
     :max-diary-expression-length
     (minimum-limit-value limits :max-diary-expression-length)
     :max-diary-expression-depth
     (minimum-limit-value limits :max-diary-expression-depth)
     :max-future-repeats
     (minimum-limit-value limits :max-future-repeats))))

(defun agenda-require-time-context (time-context)
  "Return TIME-CONTEXT or signal when it is not an Org time context."
  (unless (org-time-context-p time-context)
    (%org-time-error :invalid-time time-context))
  time-context)

(defun agenda-require-date (value)
  "Return VALUE when it is an ORG-TIME-VALUE date."
  (unless (and (org-time-value-p value)
               (eq (org-time-value-kind value) :date))
    (%org-time-error :invalid-time value))
  value)

(defun agenda-local-date (value)
  "Convert a date-only ORG-TIME-VALUE to a cl-date-kit local date."
  (cl-date-kit:make-local-date (org-time-value-year value)
                               (org-time-value-month value)
                               (org-time-value-day value)))

(defun agenda-date-value (date)
  "Convert a cl-date-kit local date to an ORG-TIME-VALUE."
  (make-org-time-value (cl-date-kit:format-local-date date)))

(defun agenda-date-ordinal (date)
  "Return DATE's integer calendar-day ordinal."
  (cl-date-kit:local-date-to-epoch-day date))

(defun agenda-range-size (start end)
  "Return the inclusive number of calendar days in START through END."
  (1+ (- (agenda-date-ordinal end) (agenda-date-ordinal start))))

(defun agenda-reference-date (time-context)
  "Return TIME-CONTEXT's reference date without consulting a system clock."
  (cl-date-kit:local-date-time-date
   (agenda-query-reference-time time-context)))

(defun agenda-query-reference-time (time-context)
  "Adapt TIME-CONTEXT's explicit instant to the current query API's local time."
  (cl-date-kit:local-date-time-of-instant
   (org-time-context-reference-instant time-context)
   (org-time-zone-backend (org-time-context-zone time-context))))

(defun agenda-heading-data (document heading indices)
  "Return normalized title, path, TODO state, tags, habit flag, and category."
  (multiple-value-bind (style found-p)
      (effective-property document heading "STYLE")
    (declare (ignore found-p))
    (values (or (node-property heading :raw-value) "")
            (node-heading-path-with-indices heading indices)
            (node-property heading :todo-keyword)
            (node-property heading :todo-type)
            (effective-heading-tags document heading)
            (and (stringp style) (string-equal style "habit"))
            (agenda-heading-category document heading))))

(defun agenda-planning-node (heading)
  "Return HEADING's planning child, if present."
  (loop for child in (node-children heading)
        thereis (and (eq (node-type child) :section)
                     (find :planning (node-children child) :key #'node-type))))

;; Tests bind this per thread to count the collector without redefining shared functions.
(defvar *agenda-data-walk-observer* nil)

(defun agenda-data-walk-observe (node)
  (when *agenda-data-walk-observer*
    (funcall *agenda-data-walk-observer* node)))

(defun agenda-document-heading-data (document &key collect-headlines-p
                                                collect-timestamps-p
                                                collect-diary-p)
  "Collect selected heading data and sibling indices for DOCUMENT."
  (let* ((root (document-root document))
         (headlines nil)
         (diary-nodes nil)
         (timestamps (when collect-timestamps-p
                       (make-hash-table :test #'eq)))
         (indices (if root
                      (query-heading-sibling-indices root)
                      (make-hash-table :test #'eq))))
    (labels ((visit (node heading)
               (agenda-data-walk-observe node)
               (let* ((type (node-type node))
                      (owner (if (eq type :headline) node heading))
                      (timestamp (and (eq type :timestamp)
                                      node))
                      (view (and collect-diary-p timestamp
                                 (timestamp-view timestamp))))
                 (when (eq type :headline)
                   (when collect-headlines-p
                     (push node headlines)))
                 (when (and collect-timestamps-p
                            (eq type :timestamp) owner)
                   (push node (gethash owner timestamps)))
                 (when (and collect-diary-p
                            (or (eq type :diary-sexp)
                                (and view
                                     (eq (org-timestamp-view-kind view)
                                         :diary))))
                   (push (list node owner) diary-nodes))
                 (dolist (child (node-children node))
                   (visit child owner)))))
      (when root
        (visit root nil)))
    (values (when collect-headlines-p (nreverse headlines))
            timestamps indices (when collect-diary-p
                                 (nreverse diary-nodes)))))
(defun agenda-prepare-diary-items (document indices diary-nodes limits)
  "Parse DOCUMENT's diary nodes under the operation's LIMITS."
  (loop for (node owner) in diary-nodes
        for source = (diary-sexp-source-for-node node limits)
        when source
          collect (list source node owner document indices)))
(defun agenda-preflight-diary-work (limits start-date end-date diary-items)
  "Check the operation-wide candidate-day and diary-expression work budget."
  (check-limit limits :max-diary-work
               (* (agenda-range-size start-date end-date)
                  (length diary-items))))
(defun agenda-add-diary-rows (state diary-items start-date end-date
                                callback-registry)
  "Add one timestamp row for each candidate date matched by a diary source."
  (dolist (item diary-items)
    (destructuring-bind (source node owner document indices) item
      (let ((heading-data
              (and owner
                   (multiple-value-list
                    (agenda-heading-data document owner indices)))))
        (agenda-intersect-date-ranges
         start-date end-date start-date end-date
         (lambda (date)
           (when (diary-sexp-source-match-p source date callback-registry)
             (if heading-data
                 (agenda-add-heading-entry
                  state heading-data :timestamp date
                  (%diary-sexp-source-view source) nil nil node)
                 (progn
                   (agenda-build-check-entry-limit state)
                   (agenda-build-store-entry
                    state
                    (make-agenda-entry
                     :date (agenda-date-value date)
                     :kind :timestamp
                     :headline (%diary-sexp-source-entry source)
                     :timestamp (%diary-sexp-source-view source)
                     :source-node node)))))))))))

(defun agenda-time-value-date (value)
  "Return VALUE's civil calendar date."
  (cl-date-kit:make-local-date (org-time-value-year value)
                               (org-time-value-month value)
                               (org-time-value-day value)))

(defun agenda-date-time-value (value)
  "Convert VALUE's local date-time to an ORG-TIME-VALUE."
  (make-org-time-value (cl-date-kit:format-local-date-time value)))

(defun agenda-add-interval (value amount unit time-context)
  "Return VALUE advanced by AMOUNT of UNIT in TIME-CONTEXT's explicit zone."
  (let* ((date (agenda-time-value-date value))
         (date-only-p (eq (org-time-value-kind value) :date)))
    (if (and date-only-p (member unit '(:day :week :month :year)))
        (agenda-date-value
         (case unit
           (:day (cl-date-kit:local-date-plus-days date amount))
           (:week (cl-date-kit:local-date-plus-weeks date amount))
           (:month (cl-date-kit:local-date-plus-months date amount))
           (:year (cl-date-kit:local-date-plus-years date amount))))
        (let* ((local (cl-date-kit:make-local-date-time
                       date
                       (cl-date-kit:make-local-time
                        (or (org-time-value-hour value) 0)
                        (or (org-time-value-minute value) 0)
                        (or (org-time-value-second value) 0))))
               (zone (org-time-context-zone time-context))
               (backend (org-time-zone-backend zone))
               (instant (%time-value-to-instant value
                                                zone
                                                (org-time-context-dst-policy
                                                 time-context))))
          (if (eq unit :hour)
              (agenda-date-time-value
               (cl-date-kit:local-date-time-of-instant
                (if (minusp amount)
                    (cl-date-kit:instant-minus-hours instant (- amount))
                    (cl-date-kit:instant-plus-hours instant amount))
                backend))
              (let* ((shifted
                       (case unit
                         (:day (cl-date-kit:local-date-time-plus-days local amount))
                         (:week (cl-date-kit:local-date-time-plus-weeks local amount))
                         (:month (cl-date-kit:local-date-time-plus-months local amount))
                         (:year (cl-date-kit:local-date-time-plus-years local amount))))
                     (shifted-value (agenda-date-time-value shifted)))
                (%time-value-to-instant shifted-value
                                        zone
                                        (org-time-context-dst-policy time-context))
                shifted-value))))))

(defun agenda-habit-log-keyword (item)
  "Return the state keyword recorded by ITEM, or NIL for other list items."
  (let* ((text (node-text item))
         (state-position (and text (search "State \"" text)))
         (keyword-start (and state-position (+ state-position 7)))
         (keyword-end (and keyword-start
                           (position #\" text :start keyword-start))))
    (when (and keyword-end (> keyword-end keyword-start))
      (subseq text keyword-start keyword-end))))

(defun agenda-habit-item-timestamp (item)
  "Return ITEM's first timestamp descendant, or NIL."
  (labels ((find-timestamp (node)
             (if (eq (node-type node) :timestamp)
                 node
                 (loop for child in (node-children node)
                       thereis (find-timestamp child)))))
    (loop for child in (node-children item)
          thereis (find-timestamp child))))

(defun agenda-habit-timestamp-date (timestamp)
  "Return TIMESTAMP's local date, or NIL if it has no concrete date."
  (let ((start (timestamp-start timestamp)))
    (when start
      (if (node-property timestamp :hour-start)
          (cl-date-kit:local-date-time-date start)
          start))))

(defun agenda-habit-completion-dates (document heading)
  "Collect DONE log dates and the CLOSED date of a done HEADING."
  (let ((dates nil))
    (labels ((visit (node)
               (unless (eq (node-type node) :headline)
                 (when (eq (node-type node) :item)
                   (let ((keyword (agenda-habit-log-keyword node)))
                     (when (and keyword (query-done-keyword-p keyword document))
                       (let ((timestamp (agenda-habit-item-timestamp node)))
                         (when timestamp
                           (let ((date (agenda-habit-timestamp-date timestamp)))
                             (when date (push date dates))))))))
                 (dolist (child (node-children node))
                   (visit child)))))
      (dolist (child (node-children heading))
        (visit child)))
    (when (eq (node-property heading :todo-type) :done)
      (let* ((planning (agenda-planning-node heading))
             (closed (or (node-property heading :closed)
                         (and planning (node-property planning :closed)))))
        (when (and (node-p closed) (eq (node-type closed) :timestamp))
          (let ((date (agenda-habit-timestamp-date closed)))
            (when date (push date dates))))))
    (sort (remove-duplicates (mapcar #'agenda-date-ordinal dates)) #'<)))

(defun agenda-habit-repeat-dates (view end-date state time-context)
  "Return scheduled repeat dates from VIEW through END-DATE, inclusive."
  (let ((amount (org-timestamp-view-repeater-amount view))
        (unit (org-timestamp-view-repeater-unit view))
        (current view)
        (end-ordinal (agenda-date-ordinal end-date))
        (dates nil))
    (loop for ordinal = (agenda-date-ordinal (agenda-view-start-date current))
          do (push ordinal dates)
             (when (> ordinal end-ordinal)
               (return))
             (agenda-build-add-repeat state)
             (setf current (agenda-shifted-timestamp-view
                            current amount unit time-context)))
    (nreverse dates)))

(defun agenda-habit-duration-days (amount unit)
  (floor (* amount
            (ecase unit
              (:day 1)
              (:week 7)
              (:month 30.4)
              (:year 365.25)))))

(defun agenda-habit-repeat-days (view)
  (agenda-habit-duration-days
   (org-timestamp-view-repeater-amount view)
   (org-timestamp-view-repeater-unit view)))

(defun agenda-habit-next-due-ordinal
    (completion-ordinal view scheduled-ordinal later-completion-count previous-due)
  "Return the historical deadline after COMPLETION-ORDINAL for VIEW."
  (let ((repeat-days (agenda-habit-repeat-days view)))
    (if (zerop later-completion-count)
        scheduled-ordinal
      (ecase (org-timestamp-view-repeater-mode view)
        (:restart (+ completion-ordinal repeat-days))
        (:cumulate (- scheduled-ordinal
                      (* repeat-days later-completion-count)))
        (:catch-up
         (let* ((next-occurrence
                  (or previous-due
                      (+ completion-ordinal
                         (let ((offset
                                 (mod (- scheduled-ordinal completion-ordinal)
                                      repeat-days)))
                           (if (zerop offset) repeat-days offset)))))
                (next-occurrence
                  (if previous-due
                      (+ next-occurrence
                         (* (1+ (floor (max (- completion-ordinal previous-due) 0)
                                          repeat-days))
                            repeat-days))
                      next-occurrence)))
           next-occurrence))))))

(defun agenda-habit-consistency-data (document heading planning start-date
                                     end-date time-context state)
  "Return date/status rows for a valid scheduled habit HEADING."
  (unless (node-property heading :todo-keyword)
    (agenda-signal-error :habit-not-todo (node-property heading :raw-value)
                         "Habit consistency requires a TODO heading"))
  (let* ((scheduled (and planning (node-property planning :scheduled)))
         (view (and (node-p scheduled)
                    (eq (node-type scheduled) :timestamp)
                    (timestamp-view scheduled))))
    (unless (and view (agenda-active-view-p view))
      (agenda-signal-error :habit-without-schedule
                           (node-property heading :raw-value)
                           "Habit consistency requires an active scheduled timestamp"))
    (unless (org-timestamp-view-repeater-amount view)
      (agenda-signal-error :habit-without-repeater
                           (node-text scheduled)
                           "Habit consistency requires a scheduled repeater"))
    (let* ((repeat-dates (agenda-habit-repeat-dates view end-date state
                                                    time-context))
           (scheduled-ordinal (first repeat-dates))
           (completion-dates (agenda-habit-completion-dates document heading))
           (completion-due-dates
             (let ((later-completion-count (1- (length completion-dates)))
                   (previous-due nil))
               (loop for completion-ordinal in completion-dates
                     for due-ordinal =
                       (agenda-habit-next-due-ordinal
                        completion-ordinal view scheduled-ordinal
                        later-completion-count previous-due)
                     do (setf previous-due due-ordinal)
                        (decf later-completion-count)
                     collect due-ordinal)))
           (completion-tail completion-dates)
           (due-tail completion-due-dates)
           (start-ordinal (agenda-date-ordinal start-date))
           (next-due scheduled-ordinal)
           (rows nil))
      (loop while (and completion-tail due-tail
                       (< (car completion-tail) start-ordinal))
            do (pop completion-tail)
               (setf next-due (pop due-tail)))
      (loop for ordinal from start-ordinal
              to (agenda-date-ordinal end-date)
            for completed-p = (and completion-tail
                                   (= (car completion-tail) ordinal))
            for status = (cond
                           (completed-p :completed)
                           ((or (and (or (null completion-dates)
                                           (< ordinal (first completion-dates)))
                                       (< ordinal scheduled-ordinal))
                                    (and next-due (<= ordinal next-due)))
                             :within-schedule)
                           (t :late))
            do (when completed-p
                 (pop completion-tail)
                 (setf next-due (pop due-tail)))
               (push (list :date (agenda-date-value
                                  (cl-date-kit:local-date-from-epoch-day ordinal))
                           :status status)
                     rows))
      (nreverse rows))))

(defun agenda-shifted-timestamp-view (view amount unit time-context)
  "Return VIEW advanced by AMOUNT of UNIT, retaining its interval metadata."
  (let* ((start (agenda-add-interval (org-timestamp-view-start view) amount unit
                                     time-context))
         (end (and (org-timestamp-view-end view)
                   (agenda-add-interval (org-timestamp-view-end view) amount unit
                                        time-context))))
    (make-org-timestamp-view
     :kind (org-timestamp-view-kind view)
     :start start :end end
     :start-inclusive-p (org-timestamp-view-start-inclusive-p view)
     :end-inclusive-p (org-timestamp-view-end-inclusive-p view)
     :repeater-amount (org-timestamp-view-repeater-amount view)
     :repeater-unit (org-timestamp-view-repeater-unit view)
     :repeater-mode (org-timestamp-view-repeater-mode view)
     :warning-amount (org-timestamp-view-warning-amount view)
     :warning-unit (org-timestamp-view-warning-unit view)
     :warning-mode (org-timestamp-view-warning-mode view))))

(defun agenda-timestamp-date-span (view)
  "Return VIEW's inclusive start and end dates."
  (values (agenda-time-value-date (org-timestamp-view-start view))
          (agenda-time-value-date (or (org-timestamp-view-end view)
                                      (org-timestamp-view-start view)))))

(defun agenda-match (documents match
                     &key time-context sorting-strategy user-defined-sorter)
  "Return matched headlines in document order using an explicit TIME-CONTEXT.
SORTING-STRATEGY may override source order with Org agenda sort keys."
  (let* ((documents (agenda-normalize-documents documents))
         (time-context (agenda-require-time-context time-context))
         (limits (agenda-operation-limits documents))
         (reference-time (agenda-query-reference-time time-context))
         (count 0)
         (matches nil))
    (dolist (document documents)
      (multiple-value-bind (headings timestamps-by-heading indices)
          (agenda-document-heading-data document)
        (declare (ignore headings timestamps-by-heading))
        (dolist (node (query-match-at document match reference-time))
          (incf count)
          (check-limit limits :max-agenda-entries count)
          (multiple-value-bind (line end-line) (node-line-range node)
            (push (make-agenda-match-entry
                   :document document :node node
                   :heading-path (node-heading-path-with-indices node indices)
                   :line line :end-line end-line)
                  matches)))))
    (agenda-sort-view-entries (nreverse matches) sorting-strategy
                              :user-defined-sorter user-defined-sorter
                              :time-context time-context)))

(defstruct (agenda-build-state (:constructor %make-agenda-build-state))
  limits entry-count repeat-count entries)

(defun agenda-validate-option (predicate value name)
  "Signal an agenda option error unless PREDICATE accepts VALUE."
  (unless (funcall predicate value)
    (agenda-signal-error :invalid-option value
                         (format nil "Invalid agenda option ~A" name)))
  value)

(defun agenda-nonnegative-integer-p (value)
  (typep value '(integer 0)))

(defun agenda-build-add-repeat (state)
  "Count one repeater occurrence before it is created or filtered."
  (let ((next (1+ (agenda-build-state-repeat-count state))))
    (check-limit (agenda-build-state-limits state)
                 :max-future-repeats next)
    (setf (agenda-build-state-repeat-count state) next)))

(defun agenda-build-check-entry-limit (state)
  "Count the next agenda row before constructing it."
  (let ((next (1+ (agenda-build-state-entry-count state))))
    (check-limit (agenda-build-state-limits state)
                 :max-agenda-entries next)
    (setf (agenda-build-state-entry-count state) next)))

(defun agenda-build-store-entry (state entry)
  "Store one entry after its operation-wide row limit has been checked."
  (setf (agenda-build-state-entries state)
        (cons entry (agenda-build-state-entries state)))
  entry)

(defun agenda-active-view-p (view)
  "Return true for active point or range timestamps."
  (member (org-timestamp-view-kind view) '(:active :active-range) :test #'eq))

(defun agenda-view-start-date (view)
  "Return VIEW's start as a local calendar date."
  (agenda-time-value-date (org-timestamp-view-start view)))

(defun agenda-view-end-date (view)
  "Return VIEW's end date, or its start date when it is a point."
  (agenda-time-value-date (or (org-timestamp-view-end view)
                              (org-timestamp-view-start view))))

(defun agenda-view-before-reference-p (view time-context)
  "Return true when VIEW starts before TIME-CONTEXT's local reference."
  (let* ((start (org-timestamp-view-start view))
         (reference
           (%time-value-from-local-value
            (agenda-query-reference-time time-context))))
    (if (eq (org-time-value-kind start) :date)
        (< (agenda-date-ordinal (agenda-time-value-date start))
           (agenda-date-ordinal (agenda-reference-date time-context)))
        (minusp (%time-values-compare start reference)))))

(defun agenda-view-with-start (view start time-context)
  "Return VIEW relocated to START, preserving a timestamp range's date width."
  (let* ((old-start (agenda-time-value-date (org-timestamp-view-start view)))
         (new-start (agenda-time-value-date start))
         (old-end (org-timestamp-view-end view))
         (day-delta (- (agenda-date-ordinal new-start)
                       (agenda-date-ordinal old-start)))
         (end (and old-end
                   (agenda-add-interval old-end day-delta :day time-context))))
    (make-org-timestamp-view
     :kind (org-timestamp-view-kind view)
     :start start :end end
     :start-inclusive-p (org-timestamp-view-start-inclusive-p view)
     :end-inclusive-p (org-timestamp-view-end-inclusive-p view)
     :repeater-amount (org-timestamp-view-repeater-amount view)
     :repeater-unit (org-timestamp-view-repeater-unit view)
     :repeater-mode (org-timestamp-view-repeater-mode view)
     :warning-amount (org-timestamp-view-warning-amount view)
     :warning-unit (org-timestamp-view-warning-unit view)
     :warning-mode (org-timestamp-view-warning-mode view))))

(defun agenda-restart-view (view time-context)
  "Return VIEW anchored to the explicit reference time for a restart repeater."
  (let* ((start (org-timestamp-view-start view))
         (reference (agenda-query-reference-time time-context))
         (new-start
           (if (eq (org-time-value-kind start) :date)
               (agenda-date-value (cl-date-kit:local-date-time-date reference))
               (agenda-date-time-value reference))))
    (agenda-view-with-start view new-start time-context)))

(defun agenda-occurrence-relevant-p (view end-date)
  "Return whether VIEW might contribute a row through END-DATE."
  (<= (agenda-date-ordinal (agenda-view-start-date view))
      (agenda-date-ordinal end-date)))

(defun agenda-call-with-timestamp-occurrences (view end-date time-context
                                              future-repeats
                                              state function)
  "Call FUNCTION for timestamp occurrences using the agenda repeater policy."
  (let ((amount (org-timestamp-view-repeater-amount view))
        (unit (org-timestamp-view-repeater-unit view))
        (mode (org-timestamp-view-repeater-mode view)))
    (unless amount
      (funcall function view t)
      (return-from agenda-call-with-timestamp-occurrences nil))
    (when (eq future-repeats :none)
      (funcall function view t)
      (return-from agenda-call-with-timestamp-occurrences nil))
    (let* ((catch-up-p (eq mode :catch-up))
           (restart-p (eq mode :restart))
           (current (if restart-p
                        (agenda-restart-view view time-context)
                        view))
           (first-p t)
           (reference-before-p (agenda-view-before-reference-p current
                                                                time-context)))
      (when (and catch-up-p reference-before-p)
        (loop while (agenda-view-before-reference-p current time-context)
              do (agenda-build-add-repeat state)
                 (setf current (agenda-shifted-timestamp-view
                                current amount unit time-context)
                       first-p nil)))
      (when restart-p
        (agenda-build-add-repeat state)
        (setf current (agenda-shifted-timestamp-view
                       current amount unit time-context)
              first-p nil))
      (when (and (eq future-repeats :next)
                 (not restart-p)
                 (agenda-view-before-reference-p current time-context))
        (loop while (agenda-view-before-reference-p current time-context)
              do (agenda-build-add-repeat state)
                 (setf current (agenda-shifted-timestamp-view
                                current amount unit time-context)
                       first-p nil)))
      (loop while (agenda-occurrence-relevant-p current end-date)
            do (when (or catch-up-p restart-p
                         (not (agenda-view-before-reference-p
                               current time-context)))
                 (funcall function current first-p))
               (when (eq future-repeats :next)
                 (return))
               (when (>= (agenda-date-ordinal
                          (agenda-view-start-date current))
                         (agenda-date-ordinal end-date))
                 (return))
               (agenda-build-add-repeat state)
               (setf current (agenda-shifted-timestamp-view
                              current amount unit time-context)
                     first-p nil)))))

(defun agenda-call-with-planning-occurrences (view end-date time-context
                                             future-repeats state function)
  "Call FUNCTION for VIEW and selected future repeats. Repeats start strictly
after the later of VIEW's date and the reference date."
  (let ((amount (org-timestamp-view-repeater-amount view))
        (unit (org-timestamp-view-repeater-unit view)))
    (funcall function view t view)
    (when (or (null amount) (eq future-repeats :none))
      (return-from agenda-call-with-planning-occurrences nil))
    (let* ((reference-date (agenda-reference-date time-context))
           (base-date (agenda-view-start-date view))
           (threshold (if (> (agenda-date-ordinal base-date)
                             (agenda-date-ordinal reference-date))
                          base-date reference-date))
           (current view))
      (when (and (eq future-repeats :next)
                 (> (agenda-date-ordinal base-date)
                    (agenda-date-ordinal reference-date)))
        (return-from agenda-call-with-planning-occurrences nil))
      (when (<= (agenda-date-ordinal end-date)
                (agenda-date-ordinal threshold))
        (return-from agenda-call-with-planning-occurrences nil))
      (loop while (<= (agenda-date-ordinal
                       (agenda-view-start-date current))
                      (agenda-date-ordinal threshold))
            do (agenda-build-add-repeat state)
               (setf current (agenda-shifted-timestamp-view
                              current amount unit time-context)))
      (loop while (<= (agenda-date-ordinal
                       (agenda-view-start-date current))
                      (agenda-date-ordinal end-date))
            do (funcall function current nil view)
               (when (eq future-repeats :next)
                 (return))
               (when (>= (agenda-date-ordinal
                          (agenda-view-start-date current))
                         (agenda-date-ordinal end-date))
                 (return))
               (agenda-build-add-repeat state)
               (setf current (agenda-shifted-timestamp-view
                              current amount unit time-context))))))

(defun agenda-call-with-occurrences (view kind end-date time-context
                                    future-repeats
                                    state function)
  "Call FUNCTION for selected timestamp or planning occurrences."
  (if (member kind '(:deadline :scheduled) :test #'eq)
      (agenda-call-with-planning-occurrences
       view end-date time-context future-repeats state function)
      (agenda-call-with-timestamp-occurrences
       view end-date time-context future-repeats state function)))

(defun agenda-intersect-date-ranges (from-date to-date start-date end-date function)
  "Call FUNCTION for each date in the inclusive intersection of two ranges."
  (loop for ordinal from (max (agenda-date-ordinal from-date)
                              (agenda-date-ordinal start-date))
          to (min (agenda-date-ordinal to-date)
                  (agenda-date-ordinal end-date))
        do (funcall function (cl-date-kit:local-date-from-epoch-day ordinal))))

(defun agenda-make-entry-for-heading (heading-data kind date
                                      view warning-state scheduled-delay-state
                                      source-node)
  "Build one normalized row using precomputed HEADING-DATA."
  (destructuring-bind (headline path todo-state todo-type tags habit-p category
                       &optional habit-consistency)
      heading-data
    (declare (ignore todo-type))
    (make-agenda-entry
     :date (and date (agenda-date-value date))
     :kind kind
     :headline headline
     :heading-path path
     :timestamp view
     :warning-state warning-state
     :scheduled-delay-state scheduled-delay-state
     :todo-state todo-state
     :effective-tags tags
     :category category
     :habit-p (and (eq kind :scheduled) habit-p)
     :habit-consistency (and (eq kind :scheduled) habit-p habit-consistency)
     :source-node source-node)))

(defun agenda-add-heading-entry (state heading-data kind date
                                view warning-state scheduled-delay-state source-node)
  "Check the row budget before constructing and appending one heading entry."
  (agenda-build-check-entry-limit state)
  (agenda-build-store-entry
   state (agenda-make-entry-for-heading heading-data kind date view
                                        warning-state scheduled-delay-state
                                        source-node)))

(defun agenda-deadline-warning-start (view warning-days time-context)
  "Return the first local date warned for VIEW's deadline."
  (agenda-time-value-date
   (agenda-add-interval
    (or (org-timestamp-view-end view) (org-timestamp-view-start view))
    (- (or (org-timestamp-view-warning-amount view) warning-days))
    (or (org-timestamp-view-warning-unit view) :day)
    time-context)))

(defun agenda-date-in-range-p (date start-date end-date)
  (let ((ordinal (agenda-date-ordinal date)))
    (<= (agenda-date-ordinal start-date) ordinal
        (agenda-date-ordinal end-date))))

(defun agenda-add-deadline-rows (state heading-data planning source-view
                                occurrence-view start-date end-date warning-days
                                time-context first-occurrence-p)
  "Add source warnings, its overdue reminder, or one future repeat row."
  (let* ((deadline-date (agenda-view-end-date source-view))
         (reference-date (agenda-reference-date time-context))
         (warning-date (agenda-deadline-warning-start
                        source-view warning-days time-context))
         (done-p (eq (fourth heading-data) :done)))
    (if first-occurrence-p
        (progn
          (when (and (not done-p)
                     (< (agenda-date-ordinal warning-date)
                        (agenda-date-ordinal deadline-date))
                     (agenda-date-in-range-p reference-date
                                             start-date end-date)
                     (<= (agenda-date-ordinal warning-date)
                         (agenda-date-ordinal reference-date))
                     (< (agenda-date-ordinal reference-date)
                        (agenda-date-ordinal deadline-date)))
            (agenda-add-heading-entry state heading-data :deadline
                                      reference-date source-view :prewarning
                                      nil planning))
          (when (agenda-date-in-range-p deadline-date start-date end-date)
            (agenda-add-heading-entry state heading-data :deadline
                                      deadline-date source-view :due nil
                                      planning))
          (when (and (not done-p)
                     (< (agenda-date-ordinal deadline-date)
                        (agenda-date-ordinal reference-date))
                     (agenda-date-in-range-p reference-date
                                             start-date end-date))
            (agenda-add-heading-entry state heading-data :deadline
                                      reference-date source-view :overdue nil
                                      planning)))
        (let ((repeat-date (agenda-view-end-date occurrence-view)))
          (when (and (not done-p)
                     (agenda-date-in-range-p repeat-date start-date end-date))
            (agenda-add-heading-entry state heading-data :deadline repeat-date
                                      source-view :due nil planning))))))

(defun agenda-scheduled-delay (view scheduled-delay)
  "Return VIEW's explicit scheduled delay amount and unit, or call defaults."
  (values (or (org-timestamp-view-warning-amount view) scheduled-delay)
          (or (org-timestamp-view-warning-unit view) :day)))

(defun agenda-add-scheduled-rows (state heading-data planning view
                                 start-date end-date scheduled-delay
                                 first-occurrence-p time-context source-view)
  "Add a scheduled date, today's stale reminder, or a future repeat date."
  (multiple-value-bind (delay-amount delay-unit)
      (agenda-scheduled-delay source-view scheduled-delay)
    (let* ((scheduled-date (agenda-view-start-date source-view))
           (reference-date (agenda-reference-date time-context))
           (visible-date
             (agenda-time-value-date
              (agenda-add-interval (org-timestamp-view-start source-view)
                                   delay-amount delay-unit time-context)))
           (delayed-p (and (> delay-amount 0)
                           (< (agenda-date-ordinal scheduled-date)
                              (agenda-date-ordinal reference-date)))))
      (if first-occurrence-p
          (progn
            (when (and (agenda-date-in-range-p scheduled-date
                                               start-date end-date)
                       (<= (agenda-date-ordinal visible-date)
                           (agenda-date-ordinal scheduled-date)))
              (agenda-add-heading-entry state heading-data :scheduled
                                        scheduled-date source-view nil nil
                                        planning))
            (when (and (< (agenda-date-ordinal scheduled-date)
                          (agenda-date-ordinal reference-date))
                       (<= (agenda-date-ordinal visible-date)
                           (agenda-date-ordinal reference-date))
                       (agenda-date-in-range-p reference-date
                                               start-date end-date))
              (agenda-add-heading-entry state heading-data :scheduled
                                        reference-date source-view nil
                                        (and delayed-p :delayed) planning)))
          (let ((repeat-date (agenda-view-start-date view)))
            (when (and (<= (agenda-date-ordinal visible-date)
                           (agenda-date-ordinal repeat-date))
                       (agenda-date-in-range-p repeat-date start-date end-date))
              (agenda-add-heading-entry state heading-data :scheduled
                                        repeat-date source-view nil
                                        (and delayed-p :delayed) planning)))))))

(defun agenda-add-timestamp-rows (state heading-data timestamp
                                 view start-date end-date)
  "Add one row for each selected date covered by active VIEW."
  (multiple-value-bind (view-start view-end) (agenda-timestamp-date-span view)
    (agenda-intersect-date-ranges
     start-date end-date view-start view-end
     (lambda (date)
       (agenda-add-heading-entry state heading-data :timestamp
                                 date view nil nil timestamp)))))

(defun agenda-build-planning-rows (state heading-data planning
                                   start-date end-date warning-days scheduled-delay
                                   future-repeats time-context)
  "Collect one heading's canonical deadline and scheduled entries."
  (when planning
    (dolist (specification '((:deadline :deadline)
                             (:scheduled :scheduled)))
      (destructuring-bind (kind source-property) specification
        (let ((timestamp (node-property planning source-property)))
          (when timestamp
            (let ((view (timestamp-view timestamp)))
              (when (agenda-active-view-p view)
                (agenda-call-with-occurrences
                 view kind end-date time-context future-repeats state
                 (lambda (occurrence first-occurrence-p source-view)
                   (if (eq kind :deadline)
                       (agenda-add-deadline-rows
                        state heading-data planning source-view occurrence
                        start-date end-date warning-days time-context
                        first-occurrence-p)
                       (agenda-add-scheduled-rows
                        state heading-data planning occurrence
                        start-date end-date scheduled-delay first-occurrence-p
                        time-context source-view))))))))))))

(defun agenda-build-timestamp-rows (state timestamps heading-data
                                    start-date end-date future-repeats
                                    time-context)
  "Collect active non-planning timestamps owned by HEADING."
  (dolist (timestamp timestamps)
    (let ((view (timestamp-view timestamp)))
      (when (agenda-active-view-p view)
        (agenda-call-with-occurrences
         view :timestamp end-date time-context future-repeats state
         (lambda (occurrence first-occurrence-p)
           (declare (ignore first-occurrence-p))
           (agenda-add-timestamp-rows state heading-data
                                      timestamp occurrence start-date end-date)))))))

(defun agenda-entry-kind-order (entry)
  "Return the stable same-day source priority for ENTRY."
  (ecase (agenda-entry-kind entry)
    (:deadline 0)
    (:scheduled 1)
    (:timestamp 2)))

(defun agenda-sort-entries (entries)
  "Sort rows by local date and source kind while preserving equal-key order."
  (stable-sort
   entries
   (lambda (left right)
     (let ((left-date (%agenda-entry-date-order-key left))
           (right-date (%agenda-entry-date-order-key right)))
       (if (= left-date right-date)
           (< (agenda-entry-kind-order left)
              (agenda-entry-kind-order right))
           (< left-date right-date))))))

(defun agenda (documents start-date end-date
               &key time-context callback-registry
                 (warning-days 14)
                 (scheduled-delay 0)
                 (future-repeats :all)
                 sorting-strategy user-defined-sorter)
  "Return agenda rows for inclusive START-DATE through END-DATE.
TIME-CONTEXT is required and is the only source of reference time. For unknown
diary forms, CALLBACK-REGISTRY supplies the Emacs Lisp evaluator; an empty
registry is used when it is omitted. SORTING-STRATEGY may override the existing
date and kind order."
  (let* ((documents (agenda-normalize-documents documents))
         (time-context (agenda-require-time-context time-context))
         (start-date (agenda-require-date start-date))
         (end-date (agenda-require-date end-date))
         (start-local (agenda-local-date start-date))
         (end-local (agenda-local-date end-date))
         (limits (agenda-operation-limits documents))
         (callback-registry
           (if callback-registry
               (agenda-validate-option #'callback-registry-p callback-registry
                                       :callback-registry)
               (make-callback-registry))))
    (agenda-validate-option #'agenda-nonnegative-integer-p warning-days
                            :warning-days)
    (agenda-validate-option #'agenda-nonnegative-integer-p scheduled-delay
                            :scheduled-delay)
    (agenda-validate-option (lambda (value)
                              (member value '(:all :next :none) :test #'eq))
                            future-repeats :future-repeats)
    (when (> (agenda-date-ordinal start-local) (agenda-date-ordinal end-local))
      (agenda-signal-error :invalid-range (list start-date end-date)
                           "Agenda start date follows its end date"))
    (check-limit limits :max-agenda-range-days
                 (agenda-range-size start-local end-local))
    (let ((state (%make-agenda-build-state :limits limits
                                           :entry-count 0 :repeat-count 0
                                           :entries nil))
          (document-data nil)
          (diary-sources nil))
      (dolist (document documents)
        (multiple-value-bind (headings timestamps-by-heading indices diary-nodes)
            (agenda-document-heading-data
             document :collect-headlines-p t :collect-timestamps-p t
                      :collect-diary-p t)
          (let ((items (agenda-prepare-diary-items document indices diary-nodes
                                                   limits)))
            (push (list document headings timestamps-by-heading indices items)
                  document-data)
            (dolist (item items)
              (push item diary-sources)))))
      (setf document-data (nreverse document-data)
            diary-sources (nreverse diary-sources))
      (agenda-preflight-diary-work limits start-local end-local diary-sources)
      (dolist (document-record document-data)
        (destructuring-bind (document headings timestamps-by-heading indices
                             document-diary-sources)
            document-record
          (dolist (heading headings)
            (let* ((heading-data
                     (multiple-value-list
                      (agenda-heading-data document heading indices)))
                   (planning (agenda-planning-node heading)))
              (when (sixth heading-data)
                (setf heading-data
                      (append heading-data
                              (list (agenda-habit-consistency-data
                                     document heading planning start-local end-local
                                     time-context state)))))
              (agenda-build-planning-rows state heading-data
                                          planning start-local end-local
                                          warning-days
                                          scheduled-delay future-repeats time-context)
              (agenda-build-timestamp-rows
               state (nreverse (gethash heading timestamps-by-heading))
               heading-data
               start-local end-local future-repeats time-context)))
          (agenda-add-diary-rows state document-diary-sources
                                 start-local end-local callback-registry)))
      (let ((entries (agenda-sort-entries
                      (nreverse (agenda-build-state-entries state)))))
        (make-agenda-result
         :entries (agenda-sort-view-entries
                   entries sorting-strategy
                   :user-defined-sorter user-defined-sorter
                   :time-context time-context)
         :start-date start-date :end-date end-date :time-context time-context)))))

(defun agenda-todo-list (documents
                         &key match time-context sorting-strategy
                           user-defined-sorter)
  "Return non-DONE TODO headlines in document order.
MATCH uses Org match syntax and TIME-CONTEXT is required. SORTING-STRATEGY
may override source order."
  (let* ((documents (agenda-normalize-documents documents))
         (time-context (agenda-require-time-context time-context))
         (limits (agenda-operation-limits documents))
         (reference-time (agenda-query-reference-time time-context))
         (state (%make-agenda-build-state :limits limits
                                          :entry-count 0 :repeat-count 0
                                          :entries nil)))
    (dolist (document documents)
      (multiple-value-bind (headings timestamps-by-heading indices)
          (agenda-document-heading-data document :collect-headlines-p t)
        (declare (ignore timestamps-by-heading))
        (let* ((matching (and match (query-match-at document match reference-time)))
               (matching-set (and match (make-hash-table :test #'eq))))
          (dolist (heading matching)
            (setf (gethash heading matching-set) t))
          (dolist (heading headings)
            (let ((todo-state (node-property heading :todo-keyword)))
              (when (and todo-state
                         (not (eq (node-property heading :todo-type) :done))
                         (or (null match) (gethash heading matching-set)))
                (agenda-build-check-entry-limit state)
                (agenda-build-store-entry
                 state (agenda-make-entry-for-heading
                        (multiple-value-list
                         (agenda-heading-data document heading indices))
                        :todo nil nil nil nil heading))))))))
    (make-agenda-result
     :entries (agenda-sort-view-entries
               (nreverse (agenda-build-state-entries state))
               sorting-strategy
               :user-defined-sorter user-defined-sorter
               :time-context time-context)
     :start-date nil :end-date nil
     :time-context time-context)))

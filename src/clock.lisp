;;;; src/clock.lisp
(in-package #:org-kit)

(defstruct (clock-summary (:constructor %make-clock-summary)
                          (:conc-name %clock-summary-)
                          (:copier nil)
                          (:predicate %clock-summary-p))
  "Minutes accumulated from scoped and unscoped CLOCK entries."
  (scoped-minutes 0 :read-only t)
  (unscoped-minutes 0 :read-only t)
  (total-minutes 0 :read-only t)
  (clipped-interval-count 0 :read-only t))

(defun make-clock-summary (&key (scoped-minutes 0) (unscoped-minutes 0)
                                (total-minutes 0) (clipped-interval-count 0))
  "Return a clock summary whose minute and interval counts are non-negative integers."
  (dolist (value (list scoped-minutes unscoped-minutes total-minutes
                       clipped-interval-count))
    (unless (typep value '(integer 0))
      (error 'type-error :datum value :expected-type '(integer 0))))
  (%make-clock-summary :scoped-minutes scoped-minutes
                       :unscoped-minutes unscoped-minutes
                       :total-minutes total-minutes
                       :clipped-interval-count clipped-interval-count))

(defun clock-summary-p (object)
  "Return true when OBJECT is a clock summary."
  (%clock-summary-p object))

(defun clock-summary-scoped-minutes (summary)
  "Return the minutes from dated CLOCK intervals after clipping."
  (%clock-summary-scoped-minutes summary))

(defun clock-summary-unscoped-minutes (summary)
  "Return the minutes from duration-only CLOCK rows."
  (%clock-summary-unscoped-minutes summary))

(defun clock-summary-total-minutes (summary)
  "Return the sum of scoped and included unscoped minutes."
  (%clock-summary-total-minutes summary))

(defun clock-summary-clipped-interval-count (summary)
  "Return the count of dated intervals with positive length after clipping."
  (%clock-summary-clipped-interval-count summary))

(defstruct (clock-consistency-issue
            (:constructor %make-clock-consistency-issue)
            (:conc-name %clock-consistency-issue-)
            (:copier nil)
            (:predicate %clock-consistency-issue-p))
  "A clock consistency finding and its source line or lines."
  (reason nil :read-only t)
  (line nil :read-only t)
  (other-line nil :read-only t)
  (duration-minutes nil :read-only t))

(defun make-clock-consistency-issue (&key reason line other-line duration-minutes)
  "Make a clock consistency issue with 1-based source line numbers."
  (unless (member reason '(:end-before-start :overlap :open-clock :too-long))
    (error 'type-error :datum reason
           :expected-type '(member :end-before-start :overlap :open-clock :too-long)))
  (unless (typep line '(integer 1))
    (error 'type-error :datum line :expected-type '(integer 1)))
  (unless (or (null other-line) (typep other-line '(integer 1)))
    (error 'type-error :datum other-line :expected-type '(or null (integer 1))))
  (unless (or (null duration-minutes)
              (and (rationalp duration-minutes) (>= duration-minutes 0)))
    (error 'type-error :datum duration-minutes :expected-type '(or null (rational 0))))
  (%make-clock-consistency-issue :reason reason :line line
                                 :other-line other-line
                                 :duration-minutes duration-minutes))

(defun clock-consistency-issue-p (object)
  "Return true when OBJECT is a clock consistency issue."
  (%clock-consistency-issue-p object))

(defun clock-consistency-issue-reason (issue)
  "Return ISSUE's reason: :END-BEFORE-START, :OVERLAP, :OPEN-CLOCK, or :TOO-LONG."
  (%clock-consistency-issue-reason issue))

(defun clock-consistency-issue-line (issue)
  "Return the 1-based source line of ISSUE's first clock."
  (%clock-consistency-issue-line issue))

(defun clock-consistency-issue-other-line (issue)
  "Return the other 1-based source line for an overlap, or NIL."
  (%clock-consistency-issue-other-line issue))

(defun clock-consistency-issue-duration-minutes (issue)
  "Return the elapsed minutes for ISSUE when its clock interval is valid."
  (%clock-consistency-issue-duration-minutes issue))

(defun %org-clock-error (reason &optional context)
  (error 'org-clock-error
         :message (ecase reason
                    (:clock-not-running "No running CLOCK entry")
                    (:multiple-running-clocks "Multiple running CLOCK entries")
                    (:clock-already-running "A CLOCK entry is already running")
                    (:invalid-range "Invalid clock range")
                    (:malformed-clock "Malformed CLOCK entry")
                    (:unsupported-option "Unsupported clock option"))
         :reason reason
         :context (and context
                       (bounded-text (if (stringp context)
                                         context
                                         (princ-to-string context))))))

(defun clock-option-context (key)
  (if (keywordp key) (symbol-name key) key))

(defun clock-validate-options (options)
  (unless (evenp (length options))
    (%org-clock-error :unsupported-option "missing option value"))
  (let ((seen (make-hash-table :test #'eq)))
    (loop for tail on options by #'cddr
          for key = (first tail)
          do (unless (member key '(:include-running-p :as-of
                                   :include-unscoped-p :time-context)
                             :test #'eq)
               (%org-clock-error :unsupported-option
                                 (clock-option-context key)))
             (when (gethash key seen)
               (%org-clock-error :unsupported-option
                                 (clock-option-context key)))
             (setf (gethash key seen) t)))
  options)

(defun clock-row-source-line (clock)
  (let* ((text (node-text clock))
         (newline (position #\Newline text))
         (line (subseq text 0 (or newline (length text)))))
    (when (char= (char line (1- (length line))) #\Return)
      (setf line (subseq line 0 (1- (length line)))))
    line))

(defun clock-horizontal-space-p (character)
  (find character '(#\Space #\Tab)))

(defun clock-skip-horizontal-space (text position)
  (loop while (and (< position (length text))
                   (clock-horizontal-space-p (char text position)))
        do (incf position)
        finally (return position)))

(defun clock-ascii-digits-p (text start end)
  (and (< start end)
       (loop for position from start below end
             for code = (char-code (char text position))
             always (<= 48 code 57))))

(defun clock-row-duration-minutes (line arrow-position)
  (let* ((start (clock-skip-horizontal-space line (+ arrow-position 2)))
         (colon (position #\: line :start start))
         (minute-start (and colon (1+ colon)))
         (minute-end (and minute-start (+ minute-start 2)))
         (end (and minute-end (clock-skip-horizontal-space line minute-end))))
    (unless (and colon minute-start minute-end (= end (length line))
                 (clock-ascii-digits-p line start colon)
                 (= (- minute-end minute-start) 2)
                 (clock-ascii-digits-p line minute-start minute-end))
      (%org-clock-error :malformed-clock line))
    (let ((hours (parse-integer line :start start :end colon))
          (minutes (parse-integer line :start minute-start :end minute-end)))
      (unless (< minutes 60)
        (%org-clock-error :malformed-clock line))
      (+ (* 60 hours) minutes))))

(defun clock-row-timestamp-view (clock)
  (timestamp-view (node-property clock :value)))

(defun clock-reversed-range-error-p (condition)
  (eq (org-time-error-reason condition) :reversed-range))

(defun clock-row-unscoped-duration (line duration)
  (unless duration
    (%org-clock-error :malformed-clock line))
  (values :unscoped nil nil duration))

(defun clock-row-values (clock)
  (let* ((line (clock-row-source-line clock))
         (arrow-position (search "=>" line))
         (timestamp (node-property clock :value))
         (duration (and arrow-position
                        (clock-row-duration-minutes line arrow-position))))
    (cond
      (timestamp
       (handler-case
           (let* ((view (clock-row-timestamp-view clock))
                  (kind (org-timestamp-view-kind view))
                  (start (org-timestamp-view-start view))
                  (end (org-timestamp-view-end view)))
             (if (and (member kind '(:active :inactive
                                     :active-range :inactive-range))
                      start
                      (or end (not arrow-position)))
                 (if end
                     (values :closed start end duration)
                     (values :running start nil nil))
                 (clock-row-unscoped-duration line duration)))
         (org-time-error (condition)
           (if (and duration
                    (eq (org-time-error-reason condition) :invalid-time))
               (clock-row-unscoped-duration line duration)
               (if (clock-reversed-range-error-p condition)
                   (%org-clock-error :malformed-clock line)
                   (error condition))))))
      (arrow-position
       (clock-row-unscoped-duration line duration))
      (t (%org-clock-error :malformed-clock line)))))

(defun clock-context-instant-seconds (value context)
  (unless (org-time-value-p value)
    (error 'type-error :datum value :expected-type 'org-time-value))
  (cl-date-kit:instant-epoch-second
   (%time-value-to-instant
    value
    (org-time-context-zone context)
    (org-time-context-dst-policy context))))

(defun clock-consistency-reversed-range (clock time-context)
  (let* ((timestamp (node-property clock :value))
         (type (and timestamp (node-property timestamp :type))))
    (when (member type '(:active-range :inactive-range))
      (let ((start (timestamp-start timestamp))
            (end (timestamp-end timestamp)))
        (when (and start end)
          (let ((start-seconds
                  (clock-context-instant-seconds
                   (%time-value-from-local-value start) time-context))
                (end-seconds
                  (clock-context-instant-seconds
                   (%time-value-from-local-value end) time-context)))
            (when (> start-seconds end-seconds)
              (values start-seconds end-seconds))))))))

(defun clock-consistency-line (document clock)
  (nth-value 0 (offset-line-column document (node-begin clock))))

(defun clock-consistency-duration-minutes (start-seconds end-seconds)
  (/ (- end-seconds start-seconds) 60))

(defun clock-consistency-reason-rank (reason)
  (position reason '(:end-before-start :overlap :open-clock :too-long)
            :test #'eq))

(defun clock-consistency-issue-before-p (left right)
  (let ((left-line (clock-consistency-issue-line left))
        (right-line (clock-consistency-issue-line right)))
    (or (< left-line right-line)
        (and (= left-line right-line)
             (let ((left-rank
                     (clock-consistency-reason-rank
                      (clock-consistency-issue-reason left)))
                   (right-rank
                     (clock-consistency-reason-rank
                      (clock-consistency-issue-reason right))))
               (or (< left-rank right-rank)
                   (and (= left-rank right-rank)
                        (< (or (clock-consistency-issue-other-line left) 0)
                           (or (clock-consistency-issue-other-line right) 0)))))))))

(defun clock-timer-item-value-string (seconds)
  (let* ((hours (floor seconds 3600))
         (minutes (floor (mod seconds 3600) 60))
         (remainder (mod seconds 60)))
    (format nil "~D:~2,'0D:~2,'0D" hours minutes remainder)))

(defun timer-item-value (start-time time-context)
  "Return the elapsed H:MM:SS value from START-TIME to the context reference."
  (unless (org-time-context-p time-context)
    (error 'type-error :datum time-context :expected-type 'org-time-context))
  (let* ((start-seconds
           (clock-context-instant-seconds start-time time-context))
         (end-seconds
           (cl-date-kit:instant-epoch-second
            (org-time-context-reference-instant time-context))))
    (when (> start-seconds end-seconds)
      (%org-clock-error :invalid-range
                        "timer start follows the time-context reference instant"))
    (clock-timer-item-value-string (- end-seconds start-seconds))))

(defun clock-timer-item-line-end (document heading)
  (let* ((source (document-text document))
         (start (node-begin heading))
         (newline (position #\Newline source :start start)))
    (if newline (1+ newline) (length source))))

(defun clock-timer-item-metadata-p (node)
  (or (member (node-type node) '(:planning :property-drawer) :test #'eq)
      (and (eq (node-type node) :drawer)
           (string-equal (node-property node :drawer-name) "LOGBOOK"))))

(defun clock-timer-item-insertion-offset (document heading)
  (let* ((section (find :section (node-children heading) :key #'node-type))
         (children (and section (node-children section))))
    (cond
      ((null section) (clock-timer-item-line-end document heading))
      ((find-if-not #'clock-timer-item-metadata-p children)
       (node-begin (find-if-not #'clock-timer-item-metadata-p children)))
      (t (node-end (car (last children)))))))

(defun clock-timer-item-newline (document)
  (let* ((source (document-text document))
         (position (position #\Newline source)))
    (if (and position (plusp position)
             (char= (char source (1- position)) #\Return))
        (format nil "~C~C" #\Return #\Newline)
        (string #\Newline))))

(defun clock-timer-item-insertion-text (document offset item-line)
  (let* ((source (document-text document))
         (newline (clock-timer-item-newline document))
         (eof-p (= offset (length source)))
         (needs-leading-newline-p
           (and eof-p
                (not (char= (char source (1- offset)) #\Newline))))
         (preserve-final-newline-p
           (and eof-p (document-final-newline-p document))))
    (concatenate 'string
                 (if needs-leading-newline-p newline "")
                 item-line
                 (if (or (not eof-p) preserve-final-newline-p) newline ""))))

(defun clock-check-heading (document heading)
  (check-type document document)
  (check-type heading node)
  (unless (eq (node-type heading) :headline)
    (error 'type-error :datum heading :expected-type 'node))
  (unless (and (document-root document)
               (eq (node-document heading) document))
    (error 'org-kit-error :message "Heading does not belong to DOCUMENT"))
  heading)

(defun clock-check-document-node-count (document)
  (let ((count 0))
    (map-nodes (lambda (node)
                 (declare (ignore node))
                 (incf count)
                 (check-limit (document-limits document)
                              :max-node-count count))
               (document-root document))
    count))

(defun clock-check-document-clock-nodes (document root)
  (let ((count 0)
        (clocks '()))
    (map-nodes (lambda (node)
                 (incf count)
                 (check-limit (document-limits document)
                              :max-node-count count)
                 (when (eq (node-type node) :clock)
                   (push node clocks)))
               root)
    (nreverse clocks)))

(defun clock-consistency-record-overlaps (document intervals record-issue)
  (let ((active '())
        (ordered (stable-sort (copy-list intervals) #'< :key #'second)))
    (dolist (interval ordered)
      (destructuring-bind (clock start end) interval
        (when (< start end)
          (setf active
                (delete-if (lambda (active-interval)
                             (<= (third active-interval) start))
                           active))
          (dolist (active-interval active)
            (let ((other-clock (first active-interval))
                  (line (clock-consistency-line document clock))
                  (other-line
                    (clock-consistency-line document
                                            (first active-interval))))
              (if (< line other-line)
                  (funcall record-issue :overlap clock :other-clock other-clock)
                  (funcall record-issue :overlap other-clock
                           :other-clock clock))))
          (push interval active))))))

(defun check-clock-consistency (document &key time-context max-duration-minutes)
  "Return clock consistency findings in DOCUMENT.
TIME-CONTEXT is required and its reference instant ends running clocks.
When MAX-DURATION-MINUTES is supplied, intervals longer than that threshold
also produce a finding. Exceeding the document's clock consistency issue limit
signals ORG-RESOURCE-LIMIT-ERROR."
  (check-type document document)
  (let ((root (document-root document)))
    (unless root
      (error 'type-error :datum root :expected-type 'node))
    (unless (org-time-context-p time-context)
      (error 'type-error :datum time-context :expected-type 'org-time-context))
    (when (and max-duration-minutes
               (not (typep max-duration-minutes '(integer 0))))
      (%org-clock-error :invalid-range
                        "max-duration-minutes must be a non-negative integer"))
    (let ((clocks (clock-check-document-clock-nodes document root))
          (limits (document-limits document))
          (reference-seconds
            (cl-date-kit:instant-epoch-second
             (org-time-context-reference-instant time-context)))
          (issues '())
          (intervals '())
          (issue-count 0))
      (labels ((record-issue (reason clock &key other-clock duration-minutes)
                 (incf issue-count)
                 (check-limit limits :max-clock-consistency-issues issue-count)
                 (push (make-clock-consistency-issue
                        :reason reason
                        :line (clock-consistency-line document clock)
                        :other-line
                        (and other-clock
                             (clock-consistency-line document other-clock))
                        :duration-minutes duration-minutes)
                       issues))
               (record-duration (clock start-seconds end-seconds)
                 (let ((duration-minutes
                         (clock-consistency-duration-minutes
                          start-seconds end-seconds)))
                   (when (and max-duration-minutes
                              (> duration-minutes max-duration-minutes))
                     (record-issue :too-long clock
                                   :duration-minutes duration-minutes)))))
        (dolist (clock clocks)
          (handler-case
              (multiple-value-bind (kind start end duration)
                  (clock-row-values clock)
                (declare (ignore duration))
                (case kind
                  (:unscoped nil)
                  (:closed
                   (let* ((start-seconds
                            (clock-context-instant-seconds start time-context))
                          (end-seconds
                            (clock-context-instant-seconds end time-context)))
                     (if (> start-seconds end-seconds)
                         (record-issue :end-before-start clock)
                         (progn
                           (push (list clock start-seconds end-seconds) intervals)
                           (record-duration clock start-seconds end-seconds)))))
                  (:running
                   (let ((start-seconds
                           (clock-context-instant-seconds start time-context)))
                     (when (> start-seconds reference-seconds)
                       (%org-clock-error :invalid-range
                                         (clock-row-source-line clock)))
                     (record-issue
                      :open-clock clock
                      :duration-minutes
                      (clock-consistency-duration-minutes
                       start-seconds reference-seconds))
                     (push (list clock start-seconds reference-seconds) intervals)
                     (record-duration clock start-seconds reference-seconds)))))
            (org-clock-error (condition)
              (multiple-value-bind (start-seconds end-seconds)
                  (clock-consistency-reversed-range clock time-context)
                (if (and (eq (org-clock-error-reason condition) :malformed-clock)
                         start-seconds end-seconds)
                    (record-issue :end-before-start clock)
                    (error condition))))))
        (clock-consistency-record-overlaps document (nreverse intervals)
                                           #'record-issue)
        (stable-sort issues #'clock-consistency-issue-before-p)))))

(defun insert-timer-item (document heading start-time description &key time-context)
  "Insert a description-list timer item after HEADING's line.
Return the edit result and the inserted H:MM:SS value. TIME-CONTEXT's reference
instant supplies the timer's current time."
  (clock-check-heading document heading)
  (unless (stringp description)
    (error 'type-error :datum description :expected-type 'string))
  (when (or (find #\Newline description) (find #\Return description))
    (%org-clock-error :unsupported-option
                      "timer description must be a single line"))
  (clock-check-document-node-count document)
  (let* ((value (timer-item-value start-time time-context))
         (source (document-text document))
         (offset (clock-timer-item-insertion-offset document heading))
         (item-line (format nil "- ~A :: ~A" value description))
         (patch (edit-make-patch
                 offset offset
                 (clock-timer-item-insertion-text document offset item-line)
                 heading))
         (new-source (edit-splice-source source (list patch)))
         (expected
           (edit-canonical-document
            (edit-reparse-source document new-source heading)))
         (result
           (edit-apply-patches-result document (list patch)
                                      :target heading :expected expected)))
    (values result value)))

(defun clock-collect-range (heading from to time-context
                            include-running-p include-unscoped-p
                            &optional included-headings provided-as-of-seconds)
  (let* ((as-of-seconds
           (or provided-as-of-seconds
               (cl-date-kit:instant-epoch-second
                (org-time-context-reference-instant time-context))))
         (from-seconds (clock-context-instant-seconds from time-context))
         (to-seconds (clock-context-instant-seconds to time-context))
         (scoped-minutes 0)
         (unscoped-minutes 0)
         (clipped-interval-count 0)
         (heading-minutes (when included-headings
                            (make-hash-table :test #'eq))))
    (when (> from-seconds to-seconds)
      (%org-clock-error :invalid-range "FROM follows TO"))
    (labels ((add-heading-minutes (owner minutes)
               (when heading-minutes
                 (incf (gethash owner heading-minutes 0) minutes)))
             (add-interval (owner start-seconds end-seconds)
               (when (> start-seconds end-seconds)
                 (%org-clock-error :malformed-clock
                                   "interval end precedes start"))
               (let ((clipped-start (max from-seconds start-seconds))
                     (clipped-end (min to-seconds end-seconds)))
                 (when (< clipped-start clipped-end)
                   (let ((minutes (floor (- clipped-end clipped-start) 60)))
                     (incf clipped-interval-count)
                     (incf scoped-minutes minutes)
                     (add-heading-minutes owner minutes))))))
      (map-nodes
       (lambda (node)
         (when (eq (node-type node) :clock)
           (let ((owner (and included-headings
                             (query-nearest-headline node))))
             (when (or (null included-headings)
                       (gethash owner included-headings))
               (multiple-value-bind (kind start end duration)
                   (clock-row-values node)
                 (case kind
                   (:unscoped
                    (when (< from-seconds to-seconds)
                      (incf unscoped-minutes duration)
                      (add-heading-minutes owner duration)))
                   (:closed
                    (add-interval owner
                                  (clock-context-instant-seconds start time-context)
                                  (clock-context-instant-seconds end time-context)))
                   (:running
                    (when include-running-p
                      (let ((start-seconds
                              (clock-context-instant-seconds start time-context)))
                        (when (> start-seconds as-of-seconds)
                          (%org-clock-error :invalid-range
                                            (clock-row-source-line node)))
                        (add-interval owner start-seconds as-of-seconds))))))))))
       heading))
    (values (make-clock-summary
             :scoped-minutes scoped-minutes
             :unscoped-minutes unscoped-minutes
             :total-minutes (+ scoped-minutes
                               (if include-unscoped-p unscoped-minutes 0))
             :clipped-interval-count clipped-interval-count)
            heading-minutes)))

(defun sum-clocks (document heading from to &rest options)
  "Return clock minutes in HEADING's subtree, clipped to [FROM, TO).
TIME-CONTEXT is required and supplies the explicit zone and DST policy.
Running entries are excluded unless :INCLUDE-RUNNING-P is true; then the
context reference time is their end. :AS-OF, when supplied, asserts that same
instant. Duration-only entries are included by default regardless of range."
  (clock-check-heading document heading)
  (clock-validate-options options)
  (clock-check-document-node-count document)
  (let* ((include-running-p (getf options :include-running-p))
         (include-unscoped-p (if (member :include-unscoped-p options)
                                 (getf options :include-unscoped-p)
                                 t))
         (as-of-present-p (member :as-of options))
         (as-of (getf options :as-of))
         (time-context (getf options :time-context)))
    (unless (typep include-running-p 'boolean)
      (error 'type-error :datum include-running-p :expected-type 'boolean))
    (unless (typep include-unscoped-p 'boolean)
      (error 'type-error :datum include-unscoped-p :expected-type 'boolean))
    (unless (org-time-context-p time-context)
      (error 'type-error :datum time-context :expected-type 'org-time-context))
    (let ((as-of-seconds
            (cl-date-kit:instant-epoch-second
             (org-time-context-reference-instant time-context))))
      (when as-of-present-p
        (unless (org-time-value-p as-of)
          (error 'type-error :datum as-of :expected-type 'org-time-value))
        (unless (= (clock-context-instant-seconds as-of time-context)
                   as-of-seconds)
          (%org-clock-error :invalid-range "as-of differs from time-context")))
      (multiple-value-bind (summary heading-minutes)
          (clock-collect-range heading from to time-context
                               include-running-p include-unscoped-p nil
                               as-of-seconds)
        (declare (ignore heading-minutes))
        summary))))

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

(defun %org-clock-error (reason &optional context)
  (error 'org-clock-error
         :message (ecase reason
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
  (string= (org-time-error-context condition)
           "range start is after range end"))

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
                    (eq (org-time-error-reason condition) :invalid-time)
                    (not (clock-reversed-range-error-p condition)))
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
   (org-time-context-reference-instant
    (make-time-context :reference-time value
                       :zone (org-time-context-zone context)
                       :dst-policy (org-time-context-dst-policy context)))))

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

(in-package #:org-kit)

(defun %require-timestamp-node (node)
  (check-type node node)
  (unless (eq (node-type node) :timestamp)
    (error 'type-error
           :datum
           (node-type node)
           :expected-type
           '(member :timestamp)))
  node)

(defun %timestamp-date-time (node year-key
                                  month-key
                                  day-key
                                  hour-key
                                  minute-key)
  (let ((year (node-property node year-key))
        (month (node-property node month-key))
        (day (node-property node day-key))
        (hour (node-property node hour-key))
        (minute (node-property node minute-key)))
    (when (and (integerp year) (integerp month) (integerp day))
      (handler-case (let ((date (cl-date-kit:make-local-date year month day)))
                      (cond
                        ((and (null hour) (null minute)) date)
                        ((and (typep hour '(integer 0 23))
                              (typep minute '(integer 0 59)))
                         (cl-date-kit:make-local-date-time date
                                                           (cl-date-kit:make-local-time
                                                            hour
                                                            minute
                                                            0)))
                        (t nil)))
        (cl-date-kit:invalid-date ()
          nil)))))

(defun timestamp-start (node)
  "Return NODE's start as a cl-date-kit local-date or local-date-time.

Return NIL when the timestamp has no concrete start date, such as a diary
timestamp, or its date or time fields are invalid."
  (%require-timestamp-node node)
  (%timestamp-date-time node
                        :year-start
                        :month-start
                        :day-start
                        :hour-start
                        :minute-start))

(defun timestamp-end (node)
  "Return NODE's end as a cl-date-kit local-date or local-date-time.

Only range timestamps have an end; point timestamps return NIL even though
org-element repeats their start fields in its end properties. Invalid or
date-less range endpoints also return NIL."
  (%require-timestamp-node node)
  (when (member (node-property node :type) '(:active-range :inactive-range))
    (%timestamp-date-time node
                          :year-end
                          :month-end
                          :day-end
                          :hour-end
                          :minute-end)))

(defun %timestamp-adjustment-valid-p (kind amount unit mode)
  (and (integerp amount)
       (plusp amount)
       (case kind
         (:repeater
          (and (member unit '(:hour :day :week :month :year))
               (member mode '(:cumulate :catch-up :restart))))
         (:warning
          (and (member unit '(:hour :day :week :month :year))
               (member mode '(:all :first)))))))

(defun %timestamp-interval (amount unit allow-hours)
  (case unit
    (:hour
     (when allow-hours
       (cl-date-kit:duration-of-hours amount)))
    (:day (cl-date-kit:make-period :days amount))
    (:week (cl-date-kit:period-of-weeks amount))
    (:month (cl-date-kit:make-period :months amount))
    (:year (cl-date-kit:make-period :years amount))))

(defun timestamp-repeater (node)
  "Return NODE's repeater interval and mode as two values.

The first value is a cl-date-kit duration for hours or period for days, weeks,
months, and years. The second is :CUMULATE, :CATCH-UP, or :RESTART. Return two
NIL values when NODE has no complete, valid repeater."
  (%require-timestamp-node node)
  (let ((amount (node-property node :repeater-value))
        (unit (node-property node :repeater-unit))
        (mode (node-property node :repeater-type)))
    (if (%timestamp-adjustment-valid-p :repeater amount unit mode)
        (values (%timestamp-interval amount unit t) mode)
        (values nil nil))))

(defun timestamp-warning-delay (node)
  "Return NODE's warning delay and warning mode as two values.

The first value is a cl-date-kit period for days, weeks, months, or years. The
second is :ALL or :FIRST. Return two NIL values when no complete, valid warning
delay is present."
  (%require-timestamp-node node)
  (let ((amount (node-property node :warning-value))
        (unit (node-property node :warning-unit))
        (mode (node-property node :warning-type)))
    (if (%timestamp-adjustment-valid-p :warning amount unit mode)
        (let ((delay (%timestamp-interval amount unit nil)))
          (if delay (values delay mode) (values nil nil)))
        (values nil nil))))

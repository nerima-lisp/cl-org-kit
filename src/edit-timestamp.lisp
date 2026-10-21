;;;; src/edit-timestamp.lisp
;;;; Format-preserving timestamp edits.
(in-package #:org-kit)

(defun edit-timestamp-check-document (document target)
  (unless (document-p document)
    (edit-signal-error target :invalid-value
                       "Edit document must be an Org document")))

(defun edit-timestamp-resolve-target (document target)
  (edit-timestamp-check-document document target)
  (edit-resolve-node document target '(:timestamp)))

(defun edit-timestamp-endpoint-ranges (document timestamp target)
  (let* ((source (document-text document))
         (cursor (node-begin timestamp))
         (limit (node-end timestamp))
         (ranges nil))
    (unless (and (integerp cursor) (integerp limit) (< cursor limit))
      (edit-signal-error target :invalid-value
                         "Timestamp has no editable source range"))
    (loop
      (when (>= cursor limit)
        (return))
      (let* ((opening (char source cursor))
             (closing (case opening (#\< #\>) (#\[ #\]))))
        (unless closing
          (edit-signal-error target :invalid-value
                             "Timestamp source has unsupported delimiters"))
        (let ((close-position (position closing source :start (1+ cursor)
                                                       :end limit)))
          (unless close-position
            (edit-signal-error target :invalid-value
                               "Timestamp source has no closing delimiter"))
          (push (list cursor close-position) ranges)
          (setf cursor (1+ close-position))
          (if (and (<= (+ cursor 2) limit)
                   (string= source "--" :start1 cursor :end1 (+ cursor 2)))
              (incf cursor 2)
              (return)))))
    (setf ranges (nreverse ranges))
    (unless (<= 1 (length ranges) 2)
      (edit-signal-error target :invalid-value
                         "Timestamp source has an unsupported range shape"))
    ranges))

(defun edit-timestamp-endpoint-index (endpoint ranges target)
  (unless (member endpoint '(:start :end))
    (edit-signal-error target :invalid-value
                       "Timestamp endpoint must be :START or :END"))
  (cond
    ((eq endpoint :start) 0)
    ((= (length ranges) 2) 1)
    (t (edit-signal-error target :invalid-value
                          "Timestamp has no separate end endpoint"))))

(defun edit-timestamp-field-key (field endpoint)
  (ecase field
    (:year (if (eq endpoint :start) :year-start :year-end))
    (:month (if (eq endpoint :start) :month-start :month-end))
    (:day (if (eq endpoint :start) :day-start :day-end))
    (:hour (if (eq endpoint :start) :hour-start :hour-end))
    (:minute (if (eq endpoint :start) :minute-start :minute-end))))

(defun edit-timestamp-date-property-changes (endpoint range-count date)
  (let ((year (cl-date-kit:local-date-year date))
        (month (cl-date-kit:local-date-month date))
        (day (cl-date-kit:local-date-day date)))
    (if (= range-count 1)
        (list :year-start year :month-start month :day-start day
              :year-end year :month-end month :day-end day)
        (list (edit-timestamp-field-key :year endpoint) year
              (edit-timestamp-field-key :month endpoint) month
              (edit-timestamp-field-key :day endpoint) day))))

(defun edit-timestamp-weekday-span (source date-end close-position)
  (let ((cursor (%timestamp-skip-whitespace source date-end close-position)))
    (when (< cursor close-position)
      (multiple-value-bind (hour minute after-time)
          (%timestamp-time-at source cursor close-position)
        (declare (ignore hour minute))
        (unless (or after-time
                    (member (char source cursor) '(#\+ #\- #\.)))
          (values cursor (%timestamp-token-end source cursor close-position)))))))

(defun edit-timestamp-date-patches
    (document endpoint-range date target)
  (let* ((source (document-text document))
         (opening (first endpoint-range))
         (close-position (second endpoint-range)))
    (multiple-value-bind (year month day date-end)
        (%timestamp-date-at source (1+ opening) close-position)
      (declare (ignore year month day))
      (unless date-end
        (edit-signal-error target :invalid-value
                           "Timestamp endpoint has no concrete date"))
      (let* ((formatted-date (edit-todo-format-point-timestamp date))
             (weekday (edit-todo-weekday-name date))
             (year-position (1+ opening))
             (month-position (+ year-position 5))
             (day-position (+ year-position 8))
             (patches
               (list (edit-make-patch
                      year-position (+ year-position 4)
                      (subseq formatted-date 0 4) target)
                     (edit-make-patch
                      month-position (+ month-position 2)
                      (subseq formatted-date 5 7) target)
                     (edit-make-patch
                      day-position (+ day-position 2)
                      (subseq formatted-date 8 10) target))))
        (multiple-value-bind (weekday-start weekday-end)
            (edit-timestamp-weekday-span source date-end close-position)
          (if weekday-start
              (setf patches
                    (append patches
                            (list (edit-make-patch weekday-start weekday-end
                                                   weekday target))))
              (let* ((token-start
                       (%timestamp-skip-whitespace source date-end close-position))
                     (replacement
                       (format nil "~A~A~A"
                               (if (> token-start date-end) "" " ")
                               weekday
                               (if (< token-start close-position) " " ""))))
                (setf patches
                      (append patches
                              (list (edit-make-patch token-start token-start
                                                     replacement target))))))
        patches)))))

(defun edit-timestamp-adjusted-date (source endpoint-range component delta target)
  (let ((opening (first endpoint-range))
        (close-position (second endpoint-range)))
    (multiple-value-bind (year month day date-end)
        (%timestamp-date-at source (1+ opening) close-position)
      (declare (ignore date-end))
      (unless year
        (edit-signal-error target :invalid-value
                           "Timestamp endpoint has no concrete date"))
      (let* ((date (cl-date-kit:make-local-date year month day))
             (month-index (+ (* year 12) (1- month)
                             (if (eq component :month) delta 0)))
             (updated-year
               (case component
                 (:year (+ year delta))
                 (:month (floor month-index 12))
                 (:day year)))
             (updated-month
               (if (eq component :month)
                   (1+ (mod month-index 12))
                   month)))
        (unless (<= 0 updated-year 9999)
          (edit-signal-error target :invalid-value
                             "Edited timestamp year is outside Org range"))
        (let ((updated
                (if (eq component :day)
                    (cl-date-kit:local-date-plus-days date delta)
                    (cl-date-kit:local-date-plus-days
                     (cl-date-kit:make-local-date updated-year updated-month 1)
                     (1- day)))))
          (unless (<= 0 (cl-date-kit:local-date-year updated) 9999)
            (edit-signal-error target :invalid-value
                               "Edited timestamp year is outside Org range"))
          updated)))))

(defun edit-timestamp-find-time (source endpoint-range target)
  (let ((opening (first endpoint-range))
        (close-position (second endpoint-range)))
    (multiple-value-bind (year month day date-end)
        (%timestamp-date-at source (1+ opening) close-position)
      (declare (ignore year month day))
      (unless date-end
        (edit-signal-error target :invalid-value
                           "Timestamp endpoint has no concrete date"))
      (loop for position from date-end below close-position
            do (multiple-value-bind (hour minute after-start)
                   (%timestamp-time-at source position close-position)
                 (when after-start
                   (let ((end-time
                          (when (and (< after-start close-position)
                                     (char= (char source after-start) #\-))
                            (multiple-value-bind (end-hour end-minute after-end)
                                (%timestamp-time-at source (1+ after-start)
                                                    close-position)
                              (when after-end
                                (list (1+ after-start) end-hour end-minute))))))
                     (return (list position hour minute end-time)))))))))

(defun edit-timestamp-shift-time (hour minute component delta)
  (let* ((total (+ (* hour 60) minute
                   (if (eq component :hour) (* delta 60) delta)))
         (day-delta (floor total 1440))
         (minute-of-day (mod total 1440)))
    (values (floor minute-of-day 60) (mod minute-of-day 60) day-delta)))

(defun edit-timestamp-clock-patches
    (time-fields updated-hour updated-minute end-hour end-minute target)
  (let ((position (first time-fields))
        (end-time (fourth time-fields))
        (patches nil))
    (push (edit-make-patch position (+ position 2)
                           (format nil "~2,'0D" updated-hour) target)
          patches)
    (push (edit-make-patch (+ position 3) (+ position 5)
                           (format nil "~2,'0D" updated-minute) target)
          patches)
    (when end-time
      (let ((end-position (first end-time)))
        (push (edit-make-patch end-position (+ end-position 2)
                               (format nil "~2,'0D" end-hour) target)
              patches)
        (push (edit-make-patch (+ end-position 3) (+ end-position 5)
                               (format nil "~2,'0D" end-minute) target)
              patches)))
    (nreverse patches)))

(defun edit-timestamp-clock-property-changes
    (endpoint range-count time-fields hour minute end-hour end-minute)
  (let ((end-time (fourth time-fields)))
    (if (= range-count 1)
        (list :hour-start hour :minute-start minute
              :hour-end (if end-time end-hour hour)
              :minute-end (if end-time end-minute minute))
        (list (edit-timestamp-field-key :hour endpoint) hour
              (edit-timestamp-field-key :minute endpoint) minute))))

(defun edit-timestamp-expected (document timestamp target changes)
  (let ((expected (edit-canonical-copy (edit-canonical-document document)))
        (path (edit-node-path document timestamp target)))
    (loop for (key value) on changes by #'cddr
          do (edit-canonical-set-property expected path key value))
    expected))

(defun edit-timestamp-apply
    (document timestamp target patches changes)
  (edit-apply-patches-result
   document patches :target target
   :expected (edit-timestamp-expected document timestamp target changes)))

(defun edit-timestamp-adjust-date-component
    (document timestamp target component delta endpoint endpoint-range ranges)
  (let* ((date (edit-timestamp-adjusted-date
                (document-text document) endpoint-range component delta target))
         (patches (edit-timestamp-date-patches
                   document endpoint-range date target))
         (changes (edit-timestamp-date-property-changes
                   endpoint (length ranges) date)))
    (edit-timestamp-apply document timestamp target patches changes)))

(defun edit-timestamp-adjust-clock-component
    (document timestamp target component delta endpoint endpoint-range ranges)
  (let* ((source (document-text document))
         (time-fields (edit-timestamp-find-time source endpoint-range target)))
    (unless time-fields
      (edit-signal-error target :invalid-value
                         "Timestamp endpoint has no clock time"))
    (when (and (= (length ranges) 2) (fourth time-fields))
      (edit-signal-error target :invalid-value
                         "External ranges with clock ranges are unsupported"))
    (let* ((old-hour (second time-fields))
           (old-minute (third time-fields))
           (end-time (fourth time-fields))
           (old-end-hour (if end-time (second end-time) old-hour))
           (old-end-minute (if end-time (third end-time) old-minute)))
      (multiple-value-bind (hour minute day-delta)
          (edit-timestamp-shift-time old-hour old-minute component delta)
        (multiple-value-bind (end-hour end-minute)
            (if end-time
                (edit-timestamp-shift-time old-end-hour old-end-minute
                                           component delta)
                (values hour minute))
          (let* ((date-patches nil)
                 (date-changes nil))
            (when (not (zerop day-delta))
              (let* ((date (edit-timestamp-adjusted-date
                            source endpoint-range :day day-delta target)))
                (setf date-patches
                      (edit-timestamp-date-patches
                       document endpoint-range date target)
                      date-changes
                      (edit-timestamp-date-property-changes
                       endpoint (length ranges) date))))
            (edit-timestamp-apply
             document timestamp target
             (append date-patches
                     (edit-timestamp-clock-patches
                      time-fields hour minute end-hour end-minute target))
             (append date-changes
                     (edit-timestamp-clock-property-changes
                      endpoint (length ranges) time-fields
                      hour minute end-hour end-minute)))))))))

(defun edit-timestamp-adjustment-layout
    (source endpoint-range adjustment-kind target)
  (let* ((opening (first endpoint-range))
         (close-position (second endpoint-range)))
    (multiple-value-bind (year month day cursor)
        (%timestamp-date-at source (1+ opening) close-position)
      (declare (ignore year month day))
      (unless cursor
        (edit-signal-error target :invalid-value
                           "Timestamp endpoint has no concrete date"))
      (loop
        (setf cursor (%timestamp-skip-whitespace source cursor close-position))
        (when (>= cursor close-position)
          (return))
        (let ((token-end (%timestamp-token-end source cursor close-position)))
          (multiple-value-bind (kind mode amount unit valid-p
                                window-value window-unit)
              (%timestamp-adjustment-token source cursor token-end)
            (declare (ignore mode window-value window-unit))
            (when (and valid-p (eq kind adjustment-kind))
              (let* ((prefix-length
                       (if (or (and (eq adjustment-kind :repeater)
                                    (< (1+ cursor) token-end)
                                    (char= (char source (1+ cursor)) #\+))
                               (and (eq adjustment-kind :repeater)
                                    (char= (char source cursor) #\.))
                               (and (eq adjustment-kind :warning)
                                    (< (1+ cursor) token-end)
                                    (char= (char source (1+ cursor)) #\-)))
                           2
                           1))
                     (digits-start (+ cursor prefix-length))
                     (digits-end
                       (loop for position from digits-start below token-end
                             while (%timestamp-digit-p (char source position))
                             finally (return position))))
                (return (list cursor digits-start digits-end token-end
                              amount unit))))
          (setf cursor token-end)))))))

(defun edit-timestamp-unit-character (unit)
  (case unit
    (:hour #\h)
    (:day #\d)
    (:week #\w)
    (:month #\m)
    (:year #\y)))

(defun edit-timestamp-next-unit (unit delta)
  (case unit
    (:hour :hour)
    (:day (if (plusp delta) :week :day))
    (:week (if (plusp delta) :month :day))
    (:month (if (plusp delta) :year :week))
    (:year (if (plusp delta) :year :month))))

(defun edit-timestamp-adjust-adjustment
    (document timestamp target component delta adjustment-kind endpoint-range
     update-node-properties-p)
  (let* ((layout (edit-timestamp-adjustment-layout
                 (document-text document) endpoint-range adjustment-kind target))
         (amount (and layout (fifth layout)))
         (unit (and layout (sixth layout))))
    (unless layout
      (edit-signal-error target :invalid-value
                         "Timestamp has no matching repeater or warning"))
    (let* ((position (ecase component
                       ((:repeater-amount :warning-amount) (second layout))
                       ((:repeater-unit :warning-unit) (third layout))))
           (end (if (member component '(:repeater-amount :warning-amount))
                    (third layout)
                    (fourth layout)))
           (new-value
             (if (member component '(:repeater-amount :warning-amount))
                 (if (eq unit :hour)
                     amount
                     (max (if (eq adjustment-kind :repeater) 1 0)
                          (+ amount delta)))
                 (edit-timestamp-next-unit unit delta)))
           (replacement
             (if (member component '(:repeater-amount :warning-amount))
                 (princ-to-string new-value)
                 (string (edit-timestamp-unit-character new-value))))
           (old-value (if (member component '(:repeater-amount :warning-amount))
                          amount
                          unit))
           (patches (if (eql new-value old-value)
                        nil
                        (list (edit-make-patch position end replacement target))))
           (property (if (eq adjustment-kind :repeater)
                         (if (member component '(:repeater-amount :warning-amount))
                             :repeater-value
                             :repeater-unit)
                         (if (member component '(:repeater-amount :warning-amount))
                             :warning-value
                             :warning-unit)))
           (changes (if (and patches update-node-properties-p)
                        (list property new-value)
                        nil)))
      (edit-timestamp-apply document timestamp target patches changes))))

(defun adjust-timestamp
    (document target component delta &key (endpoint :start))
  "Adjust a timestamp component by DELTA.
For a range, ENDPOINT selects its endpoint and defaults to :START; :END is
valid only for a range. Repeater and warning edits patch that endpoint's
suffix, while node properties describe the start endpoint."
  (unless (member component
                  '(:year :month :day :hour :minute
                    :repeater-amount :repeater-unit
                    :warning-amount :warning-unit))
    (edit-signal-error target :invalid-value
                       "Unsupported timestamp component"))
  (unless (member delta '(-1 1))
    (edit-signal-error target :invalid-value
                       "Timestamp delta must be -1 or +1"))
  (unless (member endpoint '(:start :end))
    (edit-signal-error target :invalid-value
                       "Timestamp endpoint must be :START or :END"))
  (let* ((timestamp (edit-timestamp-resolve-target document target))
         (ranges (edit-timestamp-endpoint-ranges document timestamp target))
         (index (edit-timestamp-endpoint-index endpoint ranges target))
         (endpoint-range (nth index ranges)))
    (case component
      ((:year :month :day)
       (edit-timestamp-adjust-date-component
        document timestamp target component delta endpoint endpoint-range ranges))
      ((:hour :minute)
       (edit-timestamp-adjust-clock-component
        document timestamp target component delta endpoint endpoint-range ranges))
      ((:repeater-amount :repeater-unit)
       (edit-timestamp-adjust-adjustment
        document timestamp target component delta :repeater endpoint-range
        (eq endpoint :start)))
      ((:warning-amount :warning-unit)
       (edit-timestamp-adjust-adjustment
        document timestamp target component delta :warning endpoint-range
        (eq endpoint :start))))))

(defun edit-timestamp-toggled-type (type target)
  (case type
    (:active :inactive)
    (:inactive :active)
    (:active-range :inactive-range)
    (:inactive-range :active-range)
    (t (edit-signal-error target :invalid-value
                          "Diary timestamps have no active delimiter"))))

(defun toggle-timestamp-active (document target)
  "Toggle delimiters around TARGET's complete timestamp or date range."
  (let* ((timestamp (edit-timestamp-resolve-target document target))
         (ranges (edit-timestamp-endpoint-ranges document timestamp target))
         (source (document-text document))
         (type (node-property timestamp :type))
         (new-type (edit-timestamp-toggled-type type target))
         (patches
           (loop for (opening closing) in ranges
                 append (list
                         (edit-make-patch
                          opening (1+ opening)
                          (if (char= (char source opening) #\<) "[" "<")
                          target)
                         (edit-make-patch
                          closing (1+ closing)
                          (if (char= (char source closing) #\>) "]" ">")
                          target))))
         (changes (list :type new-type)))
    (edit-timestamp-apply document timestamp target patches changes)))

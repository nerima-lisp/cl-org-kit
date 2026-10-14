;;;; src/time-values.lisp
(in-package #:org-kit)

(defstruct (org-time-value (:constructor %make-org-time-value)
                           (:conc-name %org-time-value-)
                           (:copier nil)
                           (:predicate org-time-value-p))
  "An ORG-KIT-owned date or date-time parsed from a strict ISO-8601 string.
Dates have no clock fields or UTC offset."
  (kind nil :read-only t)
  (iso8601 nil :read-only t)
  (year nil :read-only t)
  (month nil :read-only t)
  (day nil :read-only t)
  (hour nil :read-only t)
  (minute nil :read-only t)
  (second nil :read-only t)
  (offset-minutes nil :read-only t))

(defun %time-value-adapt (value)
  (cond
    ((org-time-value-p value) value)
    ((or (cl-date-kit:local-date-p value)
         (cl-date-kit:local-date-time-p value))
     (%time-value-from-local-value value))
    (t
     (error 'type-error
            :datum value
            :expected-type '(or org-time-value
                                cl-date-kit:local-date
                                cl-date-kit:local-date-time)))))

(defun org-time-value-kind (value)
  "Return VALUE's kind, either :DATE or :DATE-TIME.
VALUE may be an ORG-KIT value or a legacy cl-date-kit local date/time."
  (%org-time-value-kind (%time-value-adapt value)))

(defun org-time-value-iso8601 (value)
  "Return VALUE's ISO-8601 string, accepting legacy cl-date-kit date/time values."
  (%org-time-value-iso8601 (%time-value-adapt value)))

(defun org-time-value-year (value)
  "Return VALUE's signed calendar year, accepting legacy cl-date-kit values."
  (%org-time-value-year (%time-value-adapt value)))

(defun org-time-value-month (value)
  "Return VALUE's calendar month, from 1 through 12, including for legacy values."
  (%org-time-value-month (%time-value-adapt value)))

(defun org-time-value-day (value)
  "Return VALUE's day of month, accepting legacy cl-date-kit values."
  (%org-time-value-day (%time-value-adapt value)))

(defun org-time-value-hour (value)
  "Return VALUE's hour, or NIL for a date, accepting legacy values."
  (%org-time-value-hour (%time-value-adapt value)))

(defun org-time-value-minute (value)
  "Return VALUE's minute, or NIL for a date, accepting legacy values."
  (%org-time-value-minute (%time-value-adapt value)))

(defun org-time-value-second (value)
  "Return VALUE's second, or NIL for a date, accepting legacy values."
  (%org-time-value-second (%time-value-adapt value)))

(defun org-time-value-offset-minutes (value)
  "Return VALUE's explicit UTC offset, or NIL for legacy date/time values."
  (%org-time-value-offset-minutes (%time-value-adapt value)))

(defun %org-time-error (reason &optional context)
  (error 'org-time-error
         :message (ecase reason
                    (:invalid-time "Invalid date or time")
                    (:reversed-range "Invalid date or time")
                    (:unknown-zone "Unknown time zone")
                    (:dst-gap "Nonexistent local time")
                    (:dst-overlap "Ambiguous local time"))
         :reason reason
         :context (and context
                       (bounded-text (if (stringp context)
                                         context
                                         (princ-to-string context))))))

(defun %time-digits-p (string start end)
  (loop for index from start below end
        for code = (char-code (char string index))
        always (<= 48 code 57)))

(defun %time-parse-integer (string start end)
  (parse-integer string :start start :end end))

(defun %time-string-date-fields (string)
  (values (%time-parse-integer string 0 4)
          (%time-parse-integer string 5 7)
          (%time-parse-integer string 8 10)))

(defun %time-string-date-time-fields (string)
  (multiple-value-bind (year month day) (%time-string-date-fields string)
    (values year month day
            (%time-parse-integer string 11 13)
            (%time-parse-integer string 14 16)
            (%time-parse-integer string 17 19))))

(defun %time-check-date (year month day text)
  (handler-case
      (cl-date-kit:make-local-date year month day)
    (cl-date-kit:invalid-date () (%org-time-error :invalid-time text))))

(defun %time-check-date-time (year month day hour minute second text)
  (let ((date (%time-check-date year month day text)))
    (handler-case
        (cl-date-kit:make-local-date-time
         date (cl-date-kit:make-local-time hour minute second))
      (cl-date-kit:invalid-time () (%org-time-error :invalid-time text)))))

(defun make-org-time-value (text)
  "Parse TEXT as YYYY-MM-DD, YYYY-MM-DDThh:mm:ss, or an offset date-time."
  (check-type text string)
  (let ((length (length text)))
    (cond
      ((and (= length 10)
            (char= (char text 4) #\-)
            (char= (char text 7) #\-)
            (%time-digits-p text 0 4)
            (%time-digits-p text 5 7)
            (%time-digits-p text 8 10))
       (multiple-value-bind (year month day) (%time-string-date-fields text)
         (%time-check-date year month day text)
         (%make-org-time-value :kind :date :iso8601 text
                               :year year :month month :day day)))
      ((and (= length 19)
            (char= (char text 4) #\-)
            (char= (char text 7) #\-)
            (char= (char text 10) #\T)
            (char= (char text 13) #\:)
            (char= (char text 16) #\:)
            (%time-digits-p text 0 4)
            (%time-digits-p text 5 7)
            (%time-digits-p text 8 10)
            (%time-digits-p text 11 13)
            (%time-digits-p text 14 16)
            (%time-digits-p text 17 19))
       (multiple-value-bind (year month day hour minute second)
           (%time-string-date-time-fields text)
         (%time-check-date-time year month day hour minute second text)
         (%make-org-time-value :kind :date-time :iso8601 text
                               :year year :month month :day day
                               :hour hour :minute minute :second second)))
      ((and (= length 25)
            (char= (char text 4) #\-)
            (char= (char text 7) #\-)
            (char= (char text 10) #\T)
            (char= (char text 13) #\:)
            (char= (char text 16) #\:)
            (member (char text 19) '(#\+ #\-))
            (char= (char text 22) #\:)
            (%time-digits-p text 0 4)
            (%time-digits-p text 5 7)
            (%time-digits-p text 8 10)
            (%time-digits-p text 11 13)
            (%time-digits-p text 14 16)
            (%time-digits-p text 17 19)
            (%time-digits-p text 20 22)
            (%time-digits-p text 23 25))
       (multiple-value-bind (year month day hour minute second)
           (%time-string-date-time-fields text)
         (let* ((offset-hour (%time-parse-integer text 20 22))
                (offset-minute (%time-parse-integer text 23 25))
                (sign (if (char= (char text 19) #\-) -1 1))
                (offset (* sign (+ (* offset-hour 60) offset-minute))))
           (%time-check-date-time year month day hour minute second text)
           (unless (and (<= 0 offset-hour 14)
                        (<= 0 offset-minute 59)
                        (or (< offset-hour 14) (zerop offset-minute)))
             (%org-time-error :invalid-time text))
           (%make-org-time-value :kind :date-time :iso8601 text
                                 :year year :month month :day day
                                 :hour hour :minute minute :second second
                                 :offset-minutes offset))))
      (t (%org-time-error :invalid-time text)))))

(defun %time-value-from-local-value (value)
  (if (cl-date-kit:local-date-p value)
      (%make-org-time-value
       :kind :date
       :iso8601 (cl-date-kit:format-local-date value)
       :year (cl-date-kit:local-date-year value)
       :month (cl-date-kit:local-date-month value)
       :day (cl-date-kit:local-date-day value))
      (%make-org-time-value
       :kind :date-time
       :iso8601 (cl-date-kit:format-local-date-time value)
       :year (cl-date-kit:local-date-time-year value)
       :month (cl-date-kit:local-date-time-month value)
       :day (cl-date-kit:local-date-time-day value)
       :hour (cl-date-kit:local-date-time-hour value)
       :minute (cl-date-kit:local-date-time-minute value)
       :second (cl-date-kit:local-date-time-second value))))

(defun %time-value-local-date-time (value)
  (let* ((date (cl-date-kit:make-local-date
                (org-time-value-year value)
                (org-time-value-month value)
                (org-time-value-day value)))
         (time (cl-date-kit:make-local-time
                (or (org-time-value-hour value) 0)
                (or (org-time-value-minute value) 0)
                (or (org-time-value-second value) 0))))
    (cl-date-kit:make-local-date-time date time)))

(defstruct (org-time-zone (:constructor %make-org-time-zone)
                          (:conc-name org-time-zone-)
                          (:copier nil)
                          (:predicate org-time-zone-p))
  "An explicit fixed-offset or named time zone owned by ORG-KIT.
Fixed zone names are UTC at zero or UTC±hh:mm otherwise. Fixed offsets use
minutes from UTC and range from -840 through 840; named zones have no fixed
offset."
  (name nil :read-only t)
  (offset-minutes nil :read-only t)
  (backend nil :read-only t))

(defun %fixed-time-zone-name (offset-minutes)
  (if (zerop offset-minutes)
      "UTC"
      (multiple-value-bind (hours minutes) (floor (abs offset-minutes) 60)
        (format nil "UTC~A~2,'0D:~2,'0D"
                (if (minusp offset-minutes) "-" "+") hours minutes))))

(defun make-fixed-time-zone (offset-minutes)
  "Return a fixed time zone for OFFSET-MINUTES, within -840 through 840."
  (unless (and (integerp offset-minutes) (<= -840 offset-minutes 840))
    (%org-time-error :invalid-time offset-minutes))
  (%make-org-time-zone
   :name (%fixed-time-zone-name offset-minutes)
   :offset-minutes offset-minutes
   :backend (cl-date-kit:zone-offset-of-total-seconds (* 60 offset-minutes))))

(defun resolve-time-zone (name)
  "Resolve the explicitly requested NAME using the time-zone database."
  (check-type name string)
  (handler-case
      (let ((backend (cl-date-kit:find-time-zone name)))
        (%make-org-time-zone :name name :offset-minutes nil :backend backend))
    (cl-date-kit:time-zone-not-found () (%org-time-error :unknown-zone name))))

(defstruct (org-time-context (:constructor %make-org-time-context)
                             (:conc-name org-time-context-)
                             (:copier nil)
                             (:predicate org-time-context-p))
  "Explicit reference time, zone, and daylight-saving policy for time work.
Date-only references resolve at the start of day with :COMPATIBLE, regardless
of the policy used for local date-times. Local date-times use :COMPATIBLE,
:EARLIER, :LATER, or :STRICT to resolve DST gaps and overlaps. The reference
instant is the absolute instant resolved from REFERENCE-TIME in ZONE."
  (reference-time nil :read-only t)
  (zone nil :read-only t)
  (dst-policy :compatible :read-only t)
  (reference-instant nil :read-only t))

(defun %time-value-to-instant (value zone dst-policy)
  "Resolve VALUE to an instant using ZONE and DST-POLICY."
  (let* ((backend (org-time-zone-backend zone))
         (offset-minutes (org-time-value-offset-minutes value)))
    (handler-case
        (cl-date-kit:zoned-date-time-to-instant
         (if (eq (org-time-value-kind value) :date)
             (cl-date-kit:local-date-at-start-of-day-in-zone
              (cl-date-kit:make-local-date
               (org-time-value-year value)
               (org-time-value-month value)
               (org-time-value-day value))
              backend
              :disambiguation :compatible)
             (let ((local (%time-value-local-date-time value)))
               (if offset-minutes
                   (cl-date-kit:zoned-date-time-of-strict
                    local
                    (cl-date-kit:zone-offset-of-total-seconds (* 60 offset-minutes))
                    backend)
                   (cl-date-kit:zoned-date-time-of-local
                    local backend :disambiguation dst-policy)))))
      (cl-date-kit:nonexistent-local-time ()
        (%org-time-error :dst-gap (org-time-value-iso8601 value)))
      (cl-date-kit:ambiguous-local-time ()
        (%org-time-error :dst-overlap (org-time-value-iso8601 value)))
      (cl-date-kit:invalid-zoned-date-time-offset ()
        (%org-time-error :invalid-time (org-time-value-iso8601 value))))))

(defun make-time-context (&key reference-time zone (dst-policy :compatible))
  "Make a validated time context from an explicit reference time and zone.
Date-only references remain civil dates; their start of day resolves with
:COMPATIBLE even when DST-POLICY is :STRICT, because the date has no wall-clock
time to reject."
  (unless (org-time-value-p reference-time)
    (%org-time-error :invalid-time reference-time))
  (unless (org-time-zone-p zone)
    (%org-time-error :invalid-time zone))
  (unless (member dst-policy '(:compatible :earlier :later :strict))
    (%org-time-error :invalid-time dst-policy))
  (let ((instant (%time-value-to-instant reference-time zone dst-policy)))
    (%make-org-time-context :reference-time reference-time
                            :zone zone :dst-policy dst-policy
                            :reference-instant instant)))

(defstruct (org-time-range (:constructor %make-org-time-range)
                           (:conc-name org-time-range-)
                           (:copier nil)
                           (:predicate org-time-range-p))
  "An ordered interval of ORG-KIT time values with explicit endpoint inclusion.
Endpoints must both carry UTC offsets or both be local values."
  (start nil :read-only t)
  (end nil :read-only t)
  (include-start-p nil :read-only t)
  (include-end-p nil :read-only t))

(defun %time-value-order-key (value)
  (+ (* 86400
        (cl-date-kit:local-date-to-epoch-day
         (cl-date-kit:make-local-date
          (org-time-value-year value)
          (org-time-value-month value)
          (org-time-value-day value))))
     (* 3600 (or (org-time-value-hour value) 0))
     (* 60 (or (org-time-value-minute value) 0))
     (or (org-time-value-second value) 0)))

(defun %time-values-compare (start end)
  (let ((start-offset (org-time-value-offset-minutes start))
        (end-offset (org-time-value-offset-minutes end)))
    (when (not (eql (null start-offset) (null end-offset)))
      (%org-time-error :invalid-time "range endpoints mix offset and local values"))
    (let ((start-key (%time-value-order-key start))
          (end-key (%time-value-order-key end)))
      (when start-offset
        (decf start-key (* 60 start-offset))
        (decf end-key (* 60 end-offset)))
      (cond ((< start-key end-key) -1)
            ((> start-key end-key) 1)
            (t 0)))))

(defun make-time-range (start end &key include-start-p include-end-p)
  "Make an ordered time range with explicit start and end inclusion flags."
  (unless (and (org-time-value-p start) (org-time-value-p end)
               (typep include-start-p 'boolean)
               (typep include-end-p 'boolean))
    (%org-time-error :invalid-time "invalid range fields"))
  (when (plusp (%time-values-compare start end))
    (%org-time-error :reversed-range "range start is after range end"))
  (%make-org-time-range :start start :end end
                        :include-start-p include-start-p
                        :include-end-p include-end-p))

(defun %time-interval-components (interval)
  (cond
    ((cl-date-kit:duration-p interval)
     (let ((seconds (cl-date-kit:duration-seconds interval))
           (nanos (cl-date-kit:duration-nanos interval)))
       (if (and (plusp seconds) (zerop nanos) (zerop (mod seconds 3600)))
           (values :duration (/ seconds 3600) :hour)
           (error 'type-error :datum interval
                  :expected-type 'cl-date-kit:duration))))
    ((cl-date-kit:period-p interval)
     (let ((years (cl-date-kit:period-years interval))
           (months (cl-date-kit:period-months interval))
           (days (cl-date-kit:period-days interval)))
       (cond
         ((and (plusp years) (zerop months) (zerop days))
          (values :period years :year))
         ((and (plusp months) (zerop years) (zerop days))
          (values :period months :month))
         ((and (plusp days) (zerop years) (zerop months))
          ;; PERIOD stores weeks as a day count, so the original unit is lost.
          (values :period days :day))
         (t (error 'type-error :datum interval
                   :expected-type 'cl-date-kit:period)))))
    (t (error 'type-error :datum interval
              :expected-type '(or cl-date-kit:duration cl-date-kit:period)))))

(defun org-time-interval-kind (interval)
  "Return :PERIOD or :DURATION for a legacy timestamp interval value."
  (nth-value 0 (%time-interval-components interval)))

(defun org-time-interval-amount (interval)
  "Return the positive integer amount represented by a legacy interval."
  (nth-value 1 (%time-interval-components interval)))

(defun org-time-interval-unit (interval)
  "Return the unit encoded by a legacy interval.
PERIOD stores weeks as days, so use TIMESTAMP-VIEW when original week units
matter."
  (nth-value 2 (%time-interval-components interval)))

(defstruct (org-timestamp-view (:constructor %make-org-timestamp-view)
                               (:conc-name org-timestamp-view-)
                               (:copier nil)
                               (:predicate org-timestamp-view-p))
  "ORG-KIT's normalized view of a source timestamp node.
Diary timestamps have no endpoints; points have only a start, and ranges have
both endpoints with boolean inclusion flags. Repeater and warning fields are
complete triples or absent; amounts are positive integers, and units are
:HOUR, :DAY, :WEEK, :MONTH, or :YEAR. Repeater modes are :CUMULATE, :CATCH-UP,
or :RESTART; warning modes are :ALL or :FIRST."
  (kind nil :read-only t)
  (start nil :read-only t)
  (end nil :read-only t)
  (start-inclusive-p nil :read-only t)
  (end-inclusive-p nil :read-only t)
  (repeater-amount nil :read-only t)
  (repeater-unit nil :read-only t)
  (repeater-mode nil :read-only t)
  (warning-amount nil :read-only t)
  (warning-unit nil :read-only t)
  (warning-mode nil :read-only t))

(defun %time-optional-triplet-valid-p (amount unit mode kind)
  (or (and (null amount) (null unit) (null mode))
      (%timestamp-adjustment-valid-p kind amount unit mode)))

(defun make-org-timestamp-view (&key kind start end start-inclusive-p end-inclusive-p
                                     repeater-amount repeater-unit repeater-mode
                                     warning-amount warning-unit warning-mode)
  "Make a normalized timestamp view with endpoints and optional repeaters."
  (unless (member kind '(:active :inactive :active-range :inactive-range :diary))
    (%org-time-error :invalid-time kind))
  (unless (or (null start) (org-time-value-p start))
    (%org-time-error :invalid-time start))
  (unless (or (null end) (org-time-value-p end))
    (%org-time-error :invalid-time end))
  (unless (%time-optional-triplet-valid-p repeater-amount repeater-unit repeater-mode
                                          :repeater)
    (%org-time-error :invalid-time "invalid repeater fields"))
  (unless (%time-optional-triplet-valid-p warning-amount warning-unit warning-mode
                                          :warning)
    (%org-time-error :invalid-time "invalid warning fields"))
  (case kind
    (:diary
     (when (or start end start-inclusive-p end-inclusive-p)
       (%org-time-error :invalid-time "diary timestamps have no concrete endpoints")))
    ((:active :inactive)
     (unless (and (org-time-value-p start) (null end)
                  (null start-inclusive-p) (null end-inclusive-p))
       (%org-time-error :invalid-time "point timestamp shape is invalid")))
    ((:active-range :inactive-range)
     (unless (and (org-time-value-p start) (org-time-value-p end)
                  (typep start-inclusive-p 'boolean)
                  (typep end-inclusive-p 'boolean))
       (%org-time-error :invalid-time "range timestamp shape is invalid"))
     (make-time-range start end :include-start-p start-inclusive-p
                      :include-end-p end-inclusive-p)))
  (%make-org-timestamp-view
   :kind kind :start start :end end
   :start-inclusive-p start-inclusive-p :end-inclusive-p end-inclusive-p
   :repeater-amount repeater-amount :repeater-unit repeater-unit
   :repeater-mode repeater-mode
   :warning-amount warning-amount :warning-unit warning-unit
   :warning-mode warning-mode))

(defun %timestamp-view-interval (node kind)
  (let* ((amount-key (if (eq kind :repeater) :repeater-value :warning-value))
         (unit-key (if (eq kind :repeater) :repeater-unit :warning-unit))
         (mode-key (if (eq kind :repeater) :repeater-type :warning-type))
         (amount (node-property node amount-key))
         (unit (node-property node unit-key))
         (mode (node-property node mode-key)))
    (unless (%time-optional-triplet-valid-p amount unit mode kind)
      (%org-time-error :invalid-time (format nil "invalid ~A fields" kind)))
    (if amount
        (values amount unit mode)
        (values nil nil nil))))

(defun timestamp-view (node)
  "Return NODE as an ORG-KIT view, preserving diary suffixes and validating concrete timestamps.
For ranges, signal ORG-TIME-ERROR when the end precedes the start."
  (%require-timestamp-node node)
  (let ((kind (node-property node :type)))
    (cond
      ((eq kind :diary)
       (multiple-value-bind (repeater-amount repeater-unit repeater-mode)
           (%timestamp-view-interval node :repeater)
         (multiple-value-bind (warning-amount warning-unit warning-mode)
             (%timestamp-view-interval node :warning)
           (make-org-timestamp-view
            :kind :diary
            :repeater-amount repeater-amount :repeater-unit repeater-unit
            :repeater-mode repeater-mode
            :warning-amount warning-amount :warning-unit warning-unit
            :warning-mode warning-mode))))
      ((not (member kind '(:active :inactive :active-range :inactive-range)))
       (%org-time-error :invalid-time "unknown timestamp kind"))
      (t
       (let* ((legacy-start (timestamp-start node))
              (start (and legacy-start (%time-value-from-local-value legacy-start)))
              (range-p (member kind '(:active-range :inactive-range)))
              (legacy-end (and range-p (timestamp-end node)))
              (end (and legacy-end (%time-value-from-local-value legacy-end))))
         (unless start
           (%org-time-error :invalid-time "timestamp has no valid start"))
         (when (and range-p (null end))
           (%org-time-error :invalid-time "range timestamp has no valid end"))
         (multiple-value-bind (repeater-amount repeater-unit repeater-mode)
             (%timestamp-view-interval node :repeater)
           (multiple-value-bind (warning-amount warning-unit warning-mode)
               (%timestamp-view-interval node :warning)
             (make-org-timestamp-view
              :kind kind :start start :end end
              :start-inclusive-p (and range-p t)
              :end-inclusive-p (and range-p t)
              :repeater-amount repeater-amount :repeater-unit repeater-unit
              :repeater-mode repeater-mode
              :warning-amount warning-amount :warning-unit warning-unit
              :warning-mode warning-mode))))))))

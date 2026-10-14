(in-package #:cl-org-kit/test)

(defun timestamp-node (&rest properties)
  (make-node :timestamp :properties properties))

(defun timestamp-node-from-org (source)
  (let ((timestamp nil))
    (map-nodes (lambda (node)
                 (when (and (null timestamp) (eq (node-type node) :timestamp))
                   (setf timestamp node)))
               (document-root (parse-org source)))
    timestamp))

(defun timestamp-time-error (thunk)
  (handler-case (funcall thunk)
    (org-kit:org-time-error (condition) condition)))

(defun timestamp-context-epoch-second (context)
  (cl-date-kit:instant-epoch-second
   (org-kit:org-time-context-reference-instant context)))

(defun timestamp-expected-epoch-second (text)
  (cl-date-kit:instant-epoch-second (cl-date-kit:parse-instant text)))

(describe "timestamp legacy compatibility"
  (it "keeps local date and date-time return values"
    (let* ((date-node (timestamp-node-from-org "<2026-10-12 Mon>"))
           (date-time-node (timestamp-node-from-org "<2026-10-12 Mon 09:30>"))
           (date (org-kit:timestamp-start date-node))
           (date-time (org-kit:timestamp-start date-time-node)))
      (with-soft-assertions
        (expect (cl-date-kit:local-date-p date) :to-be t)
        (expect (list (cl-date-kit:local-date-year date)
                      (cl-date-kit:local-date-month date)
                      (cl-date-kit:local-date-day date))
                :to-equal
                '(2026 10 12))
        (expect (cl-date-kit:local-date-time-p date-time) :to-be t)
        (expect (list (cl-date-kit:local-date-time-hour date-time)
                      (cl-date-kit:local-date-time-minute date-time)
                      (cl-date-kit:local-date-time-second date-time))
                :to-equal
                '(9 30 0)))))

  (it "returns an end only for a valid legacy range timestamp"
    (let* ((range
             (timestamp-node :type :active-range
                             :year-start 2026 :month-start 10 :day-start 12
                             :hour-start 9 :minute-start 0
                             :year-end 2026 :month-end 10 :day-end 12
                             :hour-end 10 :minute-end 30))
           (point
             (timestamp-node :type :active
                             :year-start 2026 :month-start 10 :day-start 12
                             :year-end 2026 :month-end 10 :day-end 12)))
      (with-soft-assertions
        (expect (cl-date-kit:local-date-time-hour (org-kit:timestamp-start range))
                :to-be
                9)
        (expect (cl-date-kit:local-date-time-minute (org-kit:timestamp-end range))
                :to-be
                30)
        (expect (org-kit:timestamp-end point) :to-be nil)
        (expect (org-kit:timestamp-end
                 (timestamp-node :type :active-range
                                 :year-end 2026 :month-end 13 :day-end 99))
                :to-be
                nil))))

  (it "preserves legacy repeater and warning return types and modes"
    (let ((weekly (timestamp-node :repeater-type :cumulate
                                  :repeater-value 2 :repeater-unit :week))
          (hourly (timestamp-node :repeater-type :catch-up
                                  :repeater-value 3 :repeater-unit :hour))
          (warning (timestamp-node :warning-type :first
                                   :warning-value 3 :warning-unit :week)))
      (multiple-value-bind (interval mode) (org-kit:timestamp-repeater weekly)
        (with-soft-assertions
          (expect (cl-date-kit:period-p interval) :to-be t)
          (expect (cl-date-kit:period-days interval) :to-be 14)
          (expect mode :to-be :cumulate)))
      (multiple-value-bind (interval mode) (org-kit:timestamp-repeater hourly)
        (with-soft-assertions
          (expect (cl-date-kit:duration-p interval) :to-be t)
          (expect (cl-date-kit:duration-seconds interval) :to-be 10800)
          (expect mode :to-be :catch-up)))
      (multiple-value-bind (delay mode) (org-kit:timestamp-warning-delay warning)
        (with-soft-assertions
          (expect (cl-date-kit:period-p delay) :to-be t)
          (expect (cl-date-kit:period-days delay) :to-be 21)
          (expect mode :to-be :first)))
      (expect (multiple-value-list
               (org-kit:timestamp-repeater
                (timestamp-node :repeater-type :restart
                                :repeater-value 0 :repeater-unit :day)))
              :to-equal
              '(nil nil))
      (expect (multiple-value-list
               (org-kit:timestamp-warning-delay
                (timestamp-node :warning-type :all
                                :warning-value 2 :warning-unit :hour)))
              :to-equal
              '(nil nil))
      (expect (multiple-value-list
               (org-kit:timestamp-repeater
                (timestamp-node :repeater-type :unknown
                                :repeater-value 1 :repeater-unit :day)))
              :to-equal
              '(nil nil))
      (expect (multiple-value-list
               (org-kit:timestamp-warning-delay
                (timestamp-node :warning-type :unknown
                                :warning-value 1 :warning-unit :day)))
              :to-equal
              '(nil nil))
      (expect (multiple-value-list
               (org-kit:timestamp-repeater
                (timestamp-node :repeater-type :restart
                                :repeater-value 1.5 :repeater-unit :day)))
              :to-equal
              '(nil nil))))

  (it "preserves calendar source units in views and legacy interval amounts"
    (dolist (case '((:day 3) (:week 2) (:month 4) (:year 1)))
      (destructuring-bind (unit amount) case
        (let* ((node (timestamp-node :type :active
                                     :year-start 2026 :month-start 10 :day-start 12
                                     :repeater-type :restart
                                     :repeater-value amount
                                     :repeater-unit unit))
               (view (org-kit:timestamp-view node)))
          (multiple-value-bind (interval mode) (org-kit:timestamp-repeater node)
            (with-soft-assertions
              (expect (list (org-kit:org-timestamp-view-repeater-amount view)
                            (org-kit:org-timestamp-view-repeater-unit view)
                            (org-kit:org-timestamp-view-repeater-mode view))
                      :to-equal
                      (list amount unit :restart))
              (expect (cl-date-kit:period-p interval) :to-be t)
              (expect (ecase unit
                        (:day (cl-date-kit:period-days interval))
                        (:week (cl-date-kit:period-days interval))
                        (:month (cl-date-kit:period-months interval))
                        (:year (cl-date-kit:period-years interval)))
                      :to-be
                      (if (eq unit :week) (* amount 7) amount))
              (expect mode :to-be :restart))))))
    (let* ((node (timestamp-node-from-org "<2026-10-12 Mon +3h>"))
           (view (org-kit:timestamp-view node)))
      (multiple-value-bind (interval mode) (org-kit:timestamp-repeater node)
        (with-soft-assertions
          (expect (list (org-kit:org-timestamp-view-repeater-amount view)
                        (org-kit:org-timestamp-view-repeater-unit view)
                        (org-kit:org-timestamp-view-repeater-mode view))
                  :to-equal
                  '(3 :hour :cumulate))
          (expect (list (org-kit:org-time-interval-kind interval)
                        (org-kit:org-time-interval-amount interval)
                        (org-kit:org-time-interval-unit interval))
                  :to-equal
                  '(:duration 3 :hour))
          (expect mode :to-be :cumulate)))))

  (it "returns nil for diary timestamps and invalid legacy start dates"
    (with-soft-assertions
      (expect (org-kit:timestamp-start
               (timestamp-node :type :diary
                               :year-start nil :month-start nil :day-start nil
                               :hour-start 12 :minute-start 0))
              :to-be
              nil)
      (expect (org-kit:timestamp-start
               (timestamp-node :type :active
                               :year-start 2026 :month-start 13 :day-start 99))
              :to-be
              nil)
      (expect (org-kit:timestamp-start
               (timestamp-node :year-start 2026 :month-start 10 :day-start 12
                               :hour-start 9 :minute-start nil))
              :to-be
              nil)
      (expect (org-kit:timestamp-start
               (timestamp-node :year-start 2026 :month-start 10 :day-start 12
                               :hour-start 24 :minute-start 0))
              :to-be
              nil)))

  (it "signals a type error for non-timestamp nodes"
    (signals type-error (org-kit:timestamp-start (make-node :paragraph)))))

(describe "timestamp public value facade"
  (it "parses dates, local date-times, and offset date-times into ORG-KIT values"
    (let ((date (org-kit:make-org-time-value "2026-10-12"))
          (date-time (org-kit:make-org-time-value "2026-10-12T09:30:00"))
          (offset-date-time
            (org-kit:make-org-time-value "2026-10-12T09:30:00+09:00")))
      (with-soft-assertions
        (expect (org-kit:org-time-value-p date) :to-be t)
        (expect (list (org-kit:org-time-value-kind date)
                      (org-kit:org-time-value-year date)
                      (org-kit:org-time-value-month date)
                      (org-kit:org-time-value-day date)
                      (org-kit:org-time-value-hour date)
                      (org-kit:org-time-value-minute date)
                      (org-kit:org-time-value-second date)
                      (org-kit:org-time-value-offset-minutes date)
                      (org-kit:org-time-value-iso8601 date))
                :to-equal
                '(:date 2026 10 12 nil nil nil nil "2026-10-12"))
        (expect (list (org-kit:org-time-value-kind date-time)
                      (org-kit:org-time-value-year date-time)
                      (org-kit:org-time-value-month date-time)
                      (org-kit:org-time-value-day date-time)
                      (org-kit:org-time-value-hour date-time)
                      (org-kit:org-time-value-minute date-time)
                      (org-kit:org-time-value-second date-time)
                      (org-kit:org-time-value-offset-minutes date-time)
                      (org-kit:org-time-value-iso8601 date-time))
                :to-equal
                '(:date-time 2026 10 12 9 30 0 nil "2026-10-12T09:30:00"))
        (expect (list (org-kit:org-time-value-kind offset-date-time)
                      (org-kit:org-time-value-hour offset-date-time)
                      (org-kit:org-time-value-offset-minutes offset-date-time)
                      (org-kit:org-time-value-iso8601 offset-date-time))
                :to-equal
                '(:date-time 9 540 "2026-10-12T09:30:00+09:00")))))

  (it "rejects invalid values and fixed offsets outside the contract range"
    (let ((invalid-value
            (timestamp-time-error
             (lambda () (org-kit:make-org-time-value "2026-13-40"))))
          (full-width-date
            (timestamp-time-error
             (lambda () (org-kit:make-org-time-value "２０２６-１０-１２"))))
          (invalid-zone
            (timestamp-time-error
             (lambda () (org-kit:make-fixed-time-zone 841)))))
      (with-soft-assertions
        (expect (org-kit:org-time-error-reason invalid-value) :to-be :invalid-time)
        (expect (org-kit:org-kit-error-message invalid-value)
                :to-equal
                "Invalid date or time")
        (expect (org-kit:org-time-error-reason full-width-date) :to-be :invalid-time)
        (expect (org-kit:org-time-error-reason invalid-zone) :to-be :invalid-time)
        (expect (org-kit:org-time-zone-offset-minutes
                 (org-kit:make-fixed-time-zone -840))
                :to-be
                -840)
        (expect (org-kit:org-time-zone-offset-minutes
                 (org-kit:make-fixed-time-zone 840))
                :to-be
                840))))

  (it "keeps timestamp ranges inclusive and point endpoints absent"
    (let* ((point-view
             (org-kit:timestamp-view (timestamp-node-from-org "<2026-10-12 Mon>")))
           (range-view
             (org-kit:timestamp-view
              (timestamp-node-from-org "<2026-10-12 Mon 09:00-10:30>")))
           (range-start (org-kit:org-timestamp-view-start range-view))
           (range-end (org-kit:org-timestamp-view-end range-view)))
      (with-soft-assertions
        (expect (org-kit:org-time-value-iso8601
                 (org-kit:org-timestamp-view-start point-view))
                :to-equal
                "2026-10-12")
        (expect (org-kit:org-timestamp-view-end point-view) :to-be nil)
        (expect (org-kit:org-timestamp-view-start-inclusive-p point-view) :to-be nil)
        (expect (org-kit:org-timestamp-view-end-inclusive-p point-view) :to-be nil)
        (expect (org-kit:org-time-value-iso8601 range-start)
                :to-equal
                "2026-10-12T09:00:00")
        (expect (org-kit:org-time-value-iso8601 range-end)
                :to-equal
                "2026-10-12T10:30:00")
        (expect (org-kit:org-timestamp-view-start-inclusive-p range-view) :to-be t)
        (expect (org-kit:org-timestamp-view-end-inclusive-p range-view) :to-be t))))

  (it "rejects timestamp views whose date or time range runs backwards"
    (let* ((date-range (timestamp-node-from-org
                        "<2024-01-05 Fri>--<2024-01-03 Wed>"))
           (time-range (timestamp-node-from-org
                        "<2024-01-05 Fri 11:00-10:00>"))
           (date-error (timestamp-time-error
                        (lambda () (org-kit:timestamp-view date-range))))
           (time-error (timestamp-time-error
                        (lambda () (org-kit:timestamp-view time-range)))))
      (with-soft-assertions
        (expect (org-kit:org-time-error-reason date-error) :to-be :reversed-range)
        (expect (org-kit:org-time-error-reason time-error) :to-be :reversed-range))))

  (it "exposes repeater and warning amount, source unit, and mode"
    (let* ((node (timestamp-node-from-org "<2026-10-12 Mon ++2d --3w>"))
           (view (org-kit:timestamp-view node))
           (all-warning-view
             (org-kit:timestamp-view
              (timestamp-node-from-org "<2026-10-12 Mon -2d>"))))
      (with-soft-assertions
        (expect (list (org-kit:org-timestamp-view-repeater-amount view)
                      (org-kit:org-timestamp-view-repeater-unit view)
                      (org-kit:org-timestamp-view-repeater-mode view))
                :to-equal
                '(2 :day :catch-up))
        (expect (list (org-kit:org-timestamp-view-warning-amount view)
                      (org-kit:org-timestamp-view-warning-unit view)
                      (org-kit:org-timestamp-view-warning-mode view))
                :to-equal
                '(3 :week :first))
        (expect (list (org-kit:org-timestamp-view-warning-amount all-warning-view)
                      (org-kit:org-timestamp-view-warning-unit all-warning-view)
                      (org-kit:org-timestamp-view-warning-mode all-warning-view))
                :to-equal
                '(2 :day :all)))
      (multiple-value-bind (legacy-interval mode) (org-kit:timestamp-repeater node)
        (with-soft-assertions
          (expect mode :to-be :catch-up)
          (expect (list (org-kit:org-time-interval-kind legacy-interval)
                        (org-kit:org-time-interval-amount legacy-interval)
                        (org-kit:org-time-interval-unit legacy-interval))
                  :to-equal
                  '(:period 2 :day))))
      (multiple-value-bind (legacy-delay mode) (org-kit:timestamp-warning-delay node)
        (with-soft-assertions
          (expect mode :to-be :first)
          (expect (list (org-kit:org-time-interval-kind legacy-delay)
                        (org-kit:org-time-interval-amount legacy-delay)
                        (org-kit:org-time-interval-unit legacy-delay))
                  :to-equal
                  '(:period 21 :day))))))

  (it "preserves warning source units in views and legacy delay amounts"
    (dolist (case '((:day 2) (:week 3) (:month 4) (:year 1)))
      (destructuring-bind (unit amount) case
        (let* ((node (timestamp-node :type :active
                                     :year-start 2026 :month-start 10 :day-start 12
                                     :warning-type :all
                                     :warning-value amount
                                     :warning-unit unit))
               (view (org-kit:timestamp-view node)))
          (multiple-value-bind (delay mode) (org-kit:timestamp-warning-delay node)
            (with-soft-assertions
              (expect (list (org-kit:org-timestamp-view-warning-amount view)
                            (org-kit:org-timestamp-view-warning-unit view)
                            (org-kit:org-timestamp-view-warning-mode view))
                      :to-equal
                      (list amount unit :all))
              (expect (cl-date-kit:period-p delay) :to-be t)
              (expect (ecase unit
                        (:day (cl-date-kit:period-days delay))
                        (:week (cl-date-kit:period-days delay))
                        (:month (cl-date-kit:period-months delay))
                        (:year (cl-date-kit:period-years delay)))
                      :to-be
                      (if (eq unit :week) (* amount 7) amount))
              (expect mode :to-be :all)))))))

  (it "accepts hour warning units in timestamp views"
    (let* ((node (timestamp-node-from-org "<2024-01-05 Fri 10:00 -1h>"))
           (view (org-kit:timestamp-view node)))
      (with-soft-assertions
        (expect (org-kit:node-property node :warning-unit) :to-be :hour)
        (expect (list (org-kit:org-timestamp-view-warning-amount view)
                      (org-kit:org-timestamp-view-warning-unit view)
                      (org-kit:org-timestamp-view-warning-mode view))
                :to-equal
                '(1 :hour :all)))))

  (it "returns diary absence distinctly from an invalid concrete timestamp"
    (let* ((diary (org-kit:timestamp-view
                   (timestamp-node-from-org
                    "<%%(diary-float t 4 2) 12:00-14:00>")))
           (invalid (timestamp-time-error
                     (lambda ()
                       (org-kit:timestamp-view
                        (timestamp-node-from-org "<2026-13-40>"))))))
      (with-soft-assertions
        (expect (org-kit:org-timestamp-view-kind diary) :to-be :diary)
        (expect (org-kit:org-timestamp-view-start diary) :to-be nil)
        (expect (org-kit:org-timestamp-view-end diary) :to-be nil)
        (expect (org-kit:org-time-error-reason invalid) :to-be :invalid-time))))

  (it "preserves parsed diary suffixes without endpoints and rejects incomplete intervals"
    (let* ((diary-node
             (timestamp-node-from-org
              "<%%(diary-float t 4 2) 12:00-14:00 ++2d --3w>"))
           (diary-view (org-kit:timestamp-view diary-node))
           (incomplete-repeater
             (timestamp-time-error
              (lambda ()
                (org-kit:timestamp-view
                 (timestamp-node :type :diary
                                 :repeater-type :catch-up :repeater-value 2)))))
           (incomplete-warning
             (timestamp-time-error
              (lambda ()
                (org-kit:timestamp-view
                 (timestamp-node :type :diary
                                 :warning-type :first :warning-value 3))))))
      (with-soft-assertions
        (expect (org-kit:node-type diary-node) :to-be :timestamp)
        (expect (org-kit:node-property diary-node :type) :to-be :diary)
        (expect (list (org-kit:org-timestamp-view-kind diary-view)
                      (org-kit:org-timestamp-view-repeater-amount diary-view)
                      (org-kit:org-timestamp-view-repeater-unit diary-view)
                      (org-kit:org-timestamp-view-repeater-mode diary-view)
                      (org-kit:org-timestamp-view-warning-amount diary-view)
                      (org-kit:org-timestamp-view-warning-unit diary-view)
                      (org-kit:org-timestamp-view-warning-mode diary-view)
                      (org-kit:org-timestamp-view-start diary-view)
                      (org-kit:org-timestamp-view-end diary-view))
                :to-equal
                '(:diary 2 :day :catch-up 3 :week :first nil nil))
        (expect (org-kit:org-time-error-reason incomplete-repeater)
                :to-be
                :invalid-time)
        (expect (org-kit:org-time-error-reason incomplete-warning)
                :to-be
                :invalid-time))))

  (it "rejects an incomplete facade repeater while keeping legacy absence behavior"
    (let* ((node (timestamp-node :type :active
                                 :year-start 2026 :month-start 10 :day-start 12
                                 :repeater-type :cumulate
                                 :repeater-value 2))
           (invalid (timestamp-time-error
                     (lambda () (org-kit:timestamp-view node)))))
      (with-soft-assertions
        (expect (org-kit:org-time-error-reason invalid) :to-be :invalid-time)
        (expect (multiple-value-list (org-kit:timestamp-repeater node))
                :to-equal
                '(nil nil)))))

  (it "constructs time ranges with the requested inclusive bounds"
    (let* ((start (org-kit:make-org-time-value "2026-10-12"))
           (end (org-kit:make-org-time-value "2026-10-13"))
           (range (org-kit:make-time-range start end
                                           :include-start-p t
                                           :include-end-p nil)))
      (with-soft-assertions
        (expect (org-kit:org-time-range-p range) :to-be t)
        (expect (org-kit:org-time-value-iso8601
                 (org-kit:org-time-range-start range))
                :to-equal
                "2026-10-12")
        (expect (org-kit:org-time-value-iso8601
                 (org-kit:org-time-range-end range))
                :to-equal
                "2026-10-13")
        (expect (org-kit:org-time-range-include-start-p range) :to-be t)
        (expect (org-kit:org-time-range-include-end-p range) :to-be nil))))

  (it "requires and retains an explicit fixed or named time zone"
    (let* ((reference (org-kit:make-org-time-value "2026-10-12T09:30:00"))
           (fixed-zone (org-kit:make-fixed-time-zone 540))
           (fixed-context
             (org-kit:make-time-context :reference-time reference :zone fixed-zone))
           (named-zone (org-kit:resolve-time-zone "America/New_York"))
           (named-context
             (org-kit:make-time-context :reference-time reference :zone named-zone
                                        :dst-policy :later)))
      (with-soft-assertions
        (expect (org-kit:org-time-context-p fixed-context) :to-be t)
        (let ((context-reference
                (org-kit:org-time-context-reference-time fixed-context)))
          (expect (org-kit:org-time-value-p context-reference) :to-be t)
          (expect (list (org-kit:org-time-value-year context-reference)
                        (org-kit:org-time-value-month context-reference)
                        (org-kit:org-time-value-day context-reference)
                        (org-kit:org-time-value-hour context-reference)
                        (org-kit:org-time-value-minute context-reference)
                        (org-kit:org-time-value-second context-reference))
                  :to-equal
                  '(2026 10 12 9 30 0)))
        (expect (org-kit:org-time-zone-offset-minutes
                 (org-kit:org-time-context-zone fixed-context))
                :to-be
                540)
        (expect (org-kit:org-time-context-dst-policy fixed-context) :to-be :compatible)
        (expect (org-kit:org-time-zone-name named-zone) :to-equal "America/New_York")
        (expect (org-kit:org-time-zone-name
                 (org-kit:org-time-context-zone named-context))
                :to-equal
                "America/New_York")
        (expect (org-kit:org-time-context-dst-policy named-context) :to-be :later))))

  (it "resolves date-only references at the first valid local time"
    (dolist (case '(("2018-11-04" "America/Sao_Paulo" "2018-11-04T03:00:00Z")
                    ("2024-03-10" "America/Havana" "2024-03-10T05:00:00Z")))
      (destructuring-bind (date zone-name expected-instant)
          case
        (let* ((reference (org-kit:make-org-time-value date))
               (context (org-kit:make-time-context
                         :reference-time reference
                         :zone (org-kit:resolve-time-zone zone-name)
                         :dst-policy :strict)))
          (with-soft-assertions
            (expect (org-kit:org-time-value-kind
                     (org-kit:org-time-context-reference-time context))
                    :to-be
                    :date)
            (expect (timestamp-context-epoch-second context)
                    :to-be
                    (timestamp-expected-epoch-second expected-instant))
            (expect (org-kit:org-time-context-dst-policy context)
                    :to-be
                    :strict))))))

  (it "reports unknown zones and rejects offsets that conflict with an explicit zone"
    (let ((unknown (timestamp-time-error
                    (lambda () (org-kit:resolve-time-zone "Not/A_Real_Zone"))))
          (missing-zone (timestamp-time-error
                         (lambda ()
                           (org-kit:make-time-context
                            :reference-time
                            (org-kit:make-org-time-value "2026-06-15T12:00:00")))))
          (mismatch (timestamp-time-error
                     (lambda ()
                       (org-kit:make-time-context
                        :reference-time
                        (org-kit:make-org-time-value "2026-06-15T12:00:00+09:00")
                        :zone (org-kit:resolve-time-zone "America/New_York"))))))
      (with-soft-assertions
        (expect (org-kit:org-time-error-reason unknown) :to-be :unknown-zone)
        (expect (org-kit:org-kit-error-message unknown)
                :to-equal
                "Unknown time zone")
        (expect (org-kit:org-time-error-reason missing-zone) :to-be :invalid-time)
        (expect (org-kit:org-time-error-reason mismatch) :to-be :invalid-time))))

  (it "applies strict DST errors and retains each accepted gap and overlap policy"
    (let* ((zone (org-kit:resolve-time-zone "America/New_York"))
           (gap (org-kit:make-org-time-value "2024-03-10T02:30:00"))
           (overlap (org-kit:make-org-time-value "2024-11-03T01:30:00"))
           (gap-compatible (org-kit:make-time-context :reference-time gap :zone zone))
           (gap-earlier (org-kit:make-time-context :reference-time gap :zone zone
                                                   :dst-policy :earlier))
           (gap-later (org-kit:make-time-context :reference-time gap :zone zone
                                                 :dst-policy :later))
           (overlap-compatible
             (org-kit:make-time-context :reference-time overlap :zone zone))
           (overlap-earlier
             (org-kit:make-time-context :reference-time overlap :zone zone
                                        :dst-policy :earlier))
           (overlap-later
             (org-kit:make-time-context :reference-time overlap :zone zone
                                        :dst-policy :later))
           (gap-error (timestamp-time-error
                       (lambda ()
                         (org-kit:make-time-context :reference-time gap :zone zone
                                                    :dst-policy :strict))))
           (overlap-error (timestamp-time-error
                           (lambda ()
                             (org-kit:make-time-context :reference-time overlap :zone zone
                                                        :dst-policy :strict)))))
      (with-soft-assertions
        (expect (org-kit:org-time-error-reason gap-error) :to-be :dst-gap)
        (expect (org-kit:org-kit-error-message gap-error)
                :to-equal
                "Nonexistent local time")
        (expect (org-kit:org-time-error-reason overlap-error) :to-be :dst-overlap)
        (expect (org-kit:org-kit-error-message overlap-error)
                :to-equal
                "Ambiguous local time")
        (expect (mapcar #'timestamp-context-epoch-second
                        (list gap-compatible gap-earlier gap-later))
                :to-equal
                (list (timestamp-expected-epoch-second "2024-03-10T07:30:00Z")
                      (timestamp-expected-epoch-second "2024-03-10T06:30:00Z")
                      (timestamp-expected-epoch-second "2024-03-10T07:30:00Z")))
        (expect (mapcar #'timestamp-context-epoch-second
                        (list overlap-compatible overlap-earlier overlap-later))
                :to-equal
                (list (timestamp-expected-epoch-second "2024-11-03T05:30:00Z")
                      (timestamp-expected-epoch-second "2024-11-03T05:30:00Z")
                      (timestamp-expected-epoch-second "2024-11-03T06:30:00Z")))
        (expect (mapcar #'org-kit:org-time-context-dst-policy
                        (list gap-compatible gap-earlier gap-later))
                :to-equal
                '(:compatible :earlier :later))
        (expect (mapcar #'org-kit:org-time-context-dst-policy
                        (list overlap-compatible overlap-earlier overlap-later))
                :to-equal
                '(:compatible :earlier :later))))))

(describe "time facade validation and compatibility"
  (it "adapts legacy cl-date-kit values and rejects unsupported reader inputs"
    (let* ((legacy-date (cl-date-kit:make-local-date 2026 10 12))
           (legacy-time
             (cl-date-kit:make-local-date-time
              legacy-date (cl-date-kit:make-local-time 9 30 7))))
      (with-soft-assertions
        (expect (org-kit:org-time-value-kind legacy-date) :to-be :date)
        (expect (org-kit:org-time-value-p legacy-date) :to-be nil)
        (expect (org-kit:org-time-value-iso8601 legacy-date)
                :to-equal
                "2026-10-12")
        (expect (org-kit:org-time-value-hour legacy-date) :to-be nil)
        (expect (org-kit:org-time-value-kind legacy-time) :to-be :date-time)
        (expect (org-kit:org-time-value-iso8601 legacy-time)
                :to-equal
                "2026-10-12T09:30:07")
        (expect (org-kit:org-time-value-second legacy-time) :to-be 7)
        (expect (org-kit:org-time-value-offset-minutes legacy-time) :to-be nil)
        (signals type-error (org-kit:org-time-value-kind :unsupported)))))

  (it "rejects malformed local times and offset bounds while retaining negative offsets"
    (let ((negative-offset
            (org-kit:make-org-time-value "2026-10-12T09:30:00-05:30"))
          (invalid-local-time
            (timestamp-time-error
             (lambda () (org-kit:make-org-time-value "2026-10-12T25:00:00"))))
          (invalid-offset-minute
            (timestamp-time-error
             (lambda () (org-kit:make-org-time-value "2026-10-12T09:30:00+00:60"))))
          (invalid-offset-boundary
            (timestamp-time-error
             (lambda () (org-kit:make-org-time-value "2026-10-12T09:30:00+14:01"))))
          (invalid-offset-hour
            (timestamp-time-error
             (lambda () (org-kit:make-org-time-value "2026-10-12T09:30:00+15:00"))))
          (invalid-syntax
            (timestamp-time-error
             (lambda () (org-kit:make-org-time-value "2026/10/12")))))
      (with-soft-assertions
        (expect (org-kit:org-time-value-offset-minutes negative-offset) :to-be -330)
        (expect (org-kit:org-time-error-reason invalid-local-time) :to-be :invalid-time)
        (expect (org-kit:org-time-error-reason invalid-offset-minute) :to-be :invalid-time)
        (expect (org-kit:org-time-error-reason invalid-offset-boundary) :to-be :invalid-time)
        (expect (org-kit:org-time-error-reason invalid-offset-hour) :to-be :invalid-time)
        (expect (org-kit:org-time-error-reason invalid-syntax) :to-be :invalid-time))))

  (it "rejects malformed date, local date-time, and offset date-time fields"
    (dolist (text '("2026-10/12"
                    "20x6-10-12" "2026-1x-12" "2026-10-1x"
                    "2026/10-12T09:30:00" "2026-10/12T09:30:00"
                    "2026-10-12 09:30:00" "2026-10-12T09;30:00"
                    "2026-10-12T09:30;00"
                    "20x6-10-12T09:30:00" "2026-1x-12T09:30:00"
                    "2026-10-1xT09:30:00" "2026-10-12T0x:30:00"
                    "2026-10-12T09:3x:00" "2026-10-12T09:30:0x"
                    "2026/10-12T09:30:00+05:00" "2026-10/12T09:30:00+05:00"
                    "2026-10-12 09:30:00+05:00" "2026-10-12T09;30:00+05:00"
                    "2026-10-12T09:30;00+05:00" "2026-10-12T09:30:00Z05:00"
                    "2026-10-12T09:30:00+05;00"
                    "20x6-10-12T09:30:00+05:00" "2026-1x-12T09:30:00+05:00"
                    "2026-10-1xT09:30:00+05:00" "2026-10-12T0x:30:00+05:00"
                    "2026-10-12T09:3x:00+05:00" "2026-10-12T09:30:0x+05:00"
                    "2026-10-12T09:30:00+0x:00" "2026-10-12T09:30:00+05:0x"))
      (expect (org-kit:org-time-error-reason
               (timestamp-time-error (lambda () (org-kit:make-org-time-value text))))
              :to-be
              :invalid-time))
    )

  (it "validates explicit context arguments and accepts a matching zoned offset"
    (let* ((zone (org-kit:resolve-time-zone "America/New_York"))
           (reference (org-kit:make-org-time-value "2024-11-03T01:30:00-04:00"))
           (context (org-kit:make-time-context :reference-time reference :zone zone))
           (invalid-reference
             (timestamp-time-error
              (lambda () (org-kit:make-time-context :reference-time :unsupported :zone zone))))
           (invalid-zone
             (timestamp-time-error
              (lambda () (org-kit:make-time-context :reference-time reference
                                                    :zone :unsupported))))
           (invalid-policy
             (timestamp-time-error
              (lambda () (org-kit:make-time-context :reference-time reference :zone zone
                                                    :dst-policy :unknown))))
           (invalid-fixed-zone
             (timestamp-time-error
              (lambda () (org-kit:make-fixed-time-zone 540.5)))))
      (with-soft-assertions
        (expect (org-kit:org-time-context-p context) :to-be t)
        (expect (timestamp-context-epoch-second context)
                :to-be
                (timestamp-expected-epoch-second "2024-11-03T05:30:00Z"))
        (expect (org-kit:org-time-error-reason invalid-reference) :to-be :invalid-time)
        (expect (org-kit:org-time-error-reason invalid-zone) :to-be :invalid-time)
        (expect (org-kit:org-time-error-reason invalid-policy) :to-be :invalid-time)
        (expect (org-kit:org-time-error-reason invalid-fixed-zone) :to-be :invalid-time)
        (expect (org-kit:org-time-zone-name (org-kit:make-fixed-time-zone 0))
                :to-equal
                "UTC")
        (expect (org-kit:org-time-zone-name (org-kit:make-fixed-time-zone -330))
                :to-equal
                "UTC-05:30"))))

  (it "orders inclusive ranges across local and offset values"
    (let* ((start (org-kit:make-org-time-value "2026-10-12T10:00:00+02:00"))
           (end (org-kit:make-org-time-value "2026-10-12T09:30:00+00:00"))
           (offset-range (org-kit:make-time-range start end
                                                 :include-start-p t :include-end-p t))
           (equal-range (org-kit:make-time-range start start
                                                :include-start-p nil :include-end-p t))
           (mixed-values
             (timestamp-time-error
              (lambda ()
                (org-kit:make-time-range
                 (org-kit:make-org-time-value "2026-10-12T08:00:00") end))))
           (reversed-values
             (timestamp-time-error
              (lambda ()
                (org-kit:make-time-range
                 (org-kit:make-org-time-value "2026-10-13")
                 (org-kit:make-org-time-value "2026-10-12")))))
           (invalid-range
             (timestamp-time-error
              (lambda ()
                (org-kit:make-time-range nil end
                                         :include-start-p t :include-end-p t))))
           (missing-end
             (timestamp-time-error
              (lambda ()
                (org-kit:make-time-range start nil
                                         :include-start-p t :include-end-p t))))
           (invalid-start-inclusion
             (timestamp-time-error
              (lambda ()
                (org-kit:make-time-range start end
                                         :include-start-p :yes :include-end-p t))))
           (invalid-end-inclusion
             (timestamp-time-error
              (lambda ()
                (org-kit:make-time-range start end
                                         :include-start-p t :include-end-p :yes)))))
      (with-soft-assertions
        (expect (org-kit:org-time-range-p offset-range) :to-be t)
        (expect (org-kit:org-time-range-p equal-range) :to-be t)
        (expect (org-kit:org-time-range-include-start-p offset-range) :to-be t)
        (expect (org-kit:org-time-range-include-end-p equal-range) :to-be t)
        (expect (org-kit:org-time-error-reason mixed-values) :to-be :invalid-time)
        (expect (org-kit:org-time-error-reason reversed-values) :to-be :reversed-range)
        (expect (org-kit:org-time-error-reason invalid-range) :to-be :invalid-time)
        (expect (org-kit:org-time-error-reason missing-end) :to-be :invalid-time)
        (expect (org-kit:org-time-error-reason invalid-start-inclusion) :to-be :invalid-time)
        (expect (org-kit:org-time-error-reason invalid-end-inclusion) :to-be :invalid-time))))

  (it "converts legacy interval units and rejects unrepresentable interval shapes"
    (with-soft-assertions
      (expect (list (org-kit:org-time-interval-kind
                     (cl-date-kit:make-period :years 2))
                    (org-kit:org-time-interval-amount
                     (cl-date-kit:make-period :years 2))
                    (org-kit:org-time-interval-unit
                     (cl-date-kit:make-period :years 2)))
              :to-equal
              '(:period 2 :year))
      (expect (list (org-kit:org-time-interval-kind
                     (cl-date-kit:make-period :months 3))
                    (org-kit:org-time-interval-amount
                     (cl-date-kit:make-period :months 3))
                    (org-kit:org-time-interval-unit
                     (cl-date-kit:make-period :months 3)))
              :to-equal
              '(:period 3 :month))
      (signals type-error
        (org-kit:org-time-interval-kind (cl-date-kit:duration-of-minutes 1)))
      (signals type-error
        (org-kit:org-time-interval-kind (cl-date-kit:duration-of-seconds 0)))
      (signals type-error
        (org-kit:org-time-interval-kind (cl-date-kit:duration-of-seconds 3600 1)))
      (signals type-error
        (org-kit:org-time-interval-kind
         (cl-date-kit:make-period :years 1 :days 1)))
      (signals type-error (org-kit:org-time-interval-kind :unsupported))))

  (it "validates standalone and node-backed timestamp view shapes"
    (let* ((start (org-kit:make-org-time-value "2026-10-12"))
           (end (org-kit:make-org-time-value "2026-10-13"))
           (view (org-kit:make-org-timestamp-view :kind :active :start start))
           (unknown-kind
             (timestamp-time-error
              (lambda () (org-kit:make-org-timestamp-view :kind :unknown))))
           (invalid-start
             (timestamp-time-error
              (lambda () (org-kit:make-org-timestamp-view :kind :active :start "bad"))))
           (invalid-end
             (timestamp-time-error
              (lambda () (org-kit:make-org-timestamp-view :kind :active-range
                                                          :start nil :end "bad"))))
           (invalid-repeater
             (timestamp-time-error
              (lambda () (org-kit:make-org-timestamp-view :kind :diary
                                                          :repeater-amount 1))))
           (invalid-warning
             (timestamp-time-error
              (lambda () (org-kit:make-org-timestamp-view :kind :diary
                                                          :warning-amount 1))))
           (zero-repeater
             (timestamp-time-error
              (lambda () (org-kit:make-org-timestamp-view :kind :diary
                                                          :repeater-amount 0
                                                          :repeater-unit :day
                                                          :repeater-mode :restart))))
           (nonnumeric-repeater
             (timestamp-time-error
              (lambda () (org-kit:make-org-timestamp-view :kind :diary
                                                          :repeater-amount "2"
                                                          :repeater-unit :day
                                                          :repeater-mode :restart))))
           (zero-warning
             (timestamp-time-error
              (lambda () (org-kit:make-org-timestamp-view :kind :diary
                                                          :warning-amount 0
                                                          :warning-unit :day
                                                          :warning-mode :all))))
           (nonnumeric-warning
             (timestamp-time-error
              (lambda () (org-kit:make-org-timestamp-view :kind :diary
                                                          :warning-amount "2"
                                                          :warning-unit :day
                                                          :warning-mode :all))))
           (diary-with-date
             (timestamp-time-error
              (lambda () (org-kit:make-org-timestamp-view :kind :diary :start start))))
           (point-without-date
             (timestamp-time-error
              (lambda () (org-kit:make-org-timestamp-view :kind :inactive))))
           (point-with-end
             (timestamp-time-error
              (lambda () (org-kit:make-org-timestamp-view :kind :active
                                                          :start start :end end))))
           (range-without-end
             (timestamp-time-error
              (lambda () (org-kit:make-org-timestamp-view :kind :active-range
                                                          :start start
                                                          :start-inclusive-p t
                                                          :end-inclusive-p t))))
           (unknown-node-kind
             (timestamp-time-error
              (lambda () (org-kit:timestamp-view (timestamp-node :type :unknown)))))
           (invalid-range-end
             (timestamp-time-error
              (lambda ()
                (org-kit:timestamp-view
                 (timestamp-node :type :active-range
                                 :year-start 2026 :month-start 10 :day-start 12
                                 :year-end 2026 :month-end 13 :day-end 1)))))
           (invalid-node-warning
             (timestamp-time-error
              (lambda ()
                (org-kit:timestamp-view
                 (timestamp-node :type :active
                                 :year-start 2026 :month-start 10 :day-start 12
                                 :warning-type :all :warning-value 2 :warning-unit :century))))))
      (with-soft-assertions
        (expect (org-kit:org-timestamp-view-p view) :to-be t)
        (expect (org-kit:org-timestamp-view-start view) :to-be start)
        (expect (list (org-kit:org-timestamp-view-kind
                       (org-kit:make-org-timestamp-view :kind :active-range
                                                       :start start :end end
                                                       :start-inclusive-p t
                                                       :end-inclusive-p nil))
                      (org-kit:org-timestamp-view-start-inclusive-p
                       (org-kit:make-org-timestamp-view :kind :active-range
                                                       :start start :end end
                                                       :start-inclusive-p t
                                                       :end-inclusive-p nil)))
                :to-equal
                '(:active-range t))
        (dolist (condition (list unknown-kind invalid-start invalid-end
                                 invalid-repeater invalid-warning zero-repeater
                                 nonnumeric-repeater zero-warning nonnumeric-warning
                                 diary-with-date point-without-date point-with-end
                                 range-without-end
                                 unknown-node-kind invalid-range-end invalid-node-warning))
          (expect (org-kit:org-time-error-reason condition) :to-be :invalid-time))))))

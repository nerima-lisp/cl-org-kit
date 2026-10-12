(in-package #:cl-org-kit/test)

(defun timestamp-node (&rest properties)
  (make-node :timestamp :properties properties))

(describe "timestamp values"
          (it "converts a date-only start to a local date and a timed start to a local date-time"
              (let ((date
                     (org-kit::timestamp-start
                      (timestamp-node :type
                                      :active
                                      :year-start
                                      2026
                                      :month-start
                                      10
                                      :day-start
                                      12
                                      :hour-start
                                      nil
                                      :minute-start
                                      nil)))
                    (date-time
                     (org-kit::timestamp-start
                      (timestamp-node :type
                                      :inactive
                                      :year-start
                                      2026
                                      :month-start
                                      10
                                      :day-start
                                      12
                                      :hour-start
                                      9
                                      :minute-start
                                      30))))
                (with-soft-assertions (expect (cl-date-kit:local-date-p date) :to-be t)
                                      (expect
                                       (list (cl-date-kit:local-date-year date)
                                             (cl-date-kit:local-date-month date)
                                             (cl-date-kit:local-date-day date))
                                       :to-equal
                                       '(2026 10 12))
                                      (expect (cl-date-kit:local-date-time-p date-time) :to-be t)
                                      (expect
                                       (list (cl-date-kit:local-date-time-hour date-time)
                                             (cl-date-kit:local-date-time-minute date-time)
                                             (cl-date-kit:local-date-time-second date-time))
                                       :to-equal
                                       '(9 30 0)))))
          (it "returns an endpoint only for a valid range timestamp"
              (let* ((range
                      (timestamp-node :type
                                      :active-range
                                      :year-start
                                      2026
                                      :month-start
                                      10
                                      :day-start
                                      12
                                      :hour-start
                                      9
                                      :minute-start
                                      0
                                      :year-end
                                      2026
                                      :month-end
                                      10
                                      :day-end
                                      12
                                      :hour-end
                                      10
                                      :minute-end
                                      30))
                     (point
                      (timestamp-node :type
                                      :active
                                      :year-start
                                      2026
                                      :month-start
                                      10
                                      :day-start
                                      12
                                      :year-end
                                      2026
                                      :month-end
                                      10
                                      :day-end
                                      12)))
                (with-soft-assertions
                 (expect (cl-date-kit:local-date-time-hour (org-kit::timestamp-start range))
                         :to-be
                         9)
                 (expect (cl-date-kit:local-date-time-minute (org-kit::timestamp-end range))
                         :to-be
                         30)
                 (expect (org-kit::timestamp-end point) :to-be nil)
                 (expect
                  (org-kit::timestamp-end
                   (timestamp-node :type :active-range :year-end 2026 :month-end 13 :day-end 99))
                  :to-be
                  nil))))
          (it "returns NIL for diary timestamps and invalid start dates"
              (with-soft-assertions
               (expect
                (org-kit::timestamp-start
                 (timestamp-node :type
                                 :diary
                                 :year-start
                                 nil
                                 :month-start
                                 nil
                                 :day-start
                                 nil
                                 :hour-start
                                 12
                                 :minute-start
                                 0))
                :to-be
                nil)
               (expect
                (org-kit::timestamp-start
                 (timestamp-node :type :active :year-start 2026 :month-start 13 :day-start 99))
                :to-be
                nil)))
          (it "converts repeaters to calendar periods or elapsed durations and preserves mode"
              (let ((weekly
                     (timestamp-node :repeater-type
                                     :cumulate
                                     :repeater-value
                                     2
                                     :repeater-unit
                                     :week))
                    (hourly
                     (timestamp-node :repeater-type
                                     :catch-up
                                     :repeater-value
                                     3
                                     :repeater-unit
                                     :hour)))
                (multiple-value-bind (interval mode) (org-kit::timestamp-repeater weekly)
                  (with-soft-assertions (expect (cl-date-kit:period-p interval) :to-be t)
                                        (expect (cl-date-kit:period-days interval) :to-be 14)
                                        (expect mode :to-be :cumulate)))
                (multiple-value-bind (interval mode) (org-kit::timestamp-repeater hourly)
                  (with-soft-assertions (expect (cl-date-kit:duration-p interval) :to-be t)
                                        (expect (cl-date-kit:duration-seconds interval)
                                                :to-be
                                                10800)
                                        (expect mode :to-be :catch-up)))
                (expect
                 (multiple-value-list
                  (org-kit::timestamp-repeater
                   (timestamp-node :repeater-type :restart :repeater-value 0 :repeater-unit :day)))
                 :to-equal
                 '(nil nil))))
          (it "converts warning delays to periods and preserves warning mode"
              (let ((warning
                     (timestamp-node :warning-type :first :warning-value 3 :warning-unit :week)))
                (multiple-value-bind (delay mode) (org-kit::timestamp-warning-delay warning)
                  (with-soft-assertions (expect (cl-date-kit:period-p delay) :to-be t)
                                        (expect (cl-date-kit:period-days delay) :to-be 21)
                                        (expect mode :to-be :first)))
                (expect
                 (multiple-value-list
                  (org-kit::timestamp-warning-delay
                   (timestamp-node :warning-type :all :warning-value 2 :warning-unit :hour)))
                 :to-equal
                 '(nil nil))
                (expect (multiple-value-list (org-kit::timestamp-warning-delay (timestamp-node)))
                        :to-equal
                        '(nil nil))))
          (it "converts every calendar repeater unit and rejects incomplete repeaters"
              (let ((daily
                     (timestamp-node :repeater-type :restart :repeater-value 3 :repeater-unit :day))
                    (monthly
                     (timestamp-node :repeater-type
                                     :restart
                                     :repeater-value
                                     2
                                     :repeater-unit
                                     :month))
                    (yearly
                     (timestamp-node :repeater-type :restart :repeater-value 1 :repeater-unit :year)))
                (multiple-value-bind (interval mode) (org-kit::timestamp-repeater daily)
                  (expect
                   (list (cl-date-kit:period-p interval) (cl-date-kit:period-days interval) mode)
                   :to-equal
                   '(t 3 :restart)))
                (multiple-value-bind (interval mode) (org-kit::timestamp-repeater monthly)
                  (expect
                   (list (cl-date-kit:period-p interval) (cl-date-kit:period-months interval) mode)
                   :to-equal
                   '(t 2 :restart)))
                (multiple-value-bind (interval mode) (org-kit::timestamp-repeater yearly)
                  (expect
                   (list (cl-date-kit:period-p interval) (cl-date-kit:period-years interval) mode)
                   :to-equal
                   '(t 1 :restart)))
                (expect
                 (multiple-value-list
                  (org-kit::timestamp-repeater
                   (timestamp-node :repeater-type :restart :repeater-value 1 :repeater-unit :decade)))
                 :to-equal
                 '(nil nil))
                (expect
                 (multiple-value-list
                  (org-kit::timestamp-repeater
                   (timestamp-node :repeater-type :unknown :repeater-value 1 :repeater-unit :day)))
                 :to-equal
                 '(nil nil))))
          (it "converts day, month, and year warning delays"
              (dolist
                  (case '((:day :days 2) (:month :months 3) (:year :years 1)))
                (destructuring-bind (unit component value) case
                  (multiple-value-bind (delay mode)
                      (org-kit::timestamp-warning-delay
                       (timestamp-node :warning-type :all :warning-value value :warning-unit unit))
                    (expect (cl-date-kit:period-p delay) :to-be t)
                    (expect
                     (ecase component
                       (:days (cl-date-kit:period-days delay))
                       (:months (cl-date-kit:period-months delay))
                       (:years (cl-date-kit:period-years delay)))
                     :to-be
                     value)
                    (expect mode :to-be :all)))))
          (it "returns NIL for incomplete or out-of-range date-time fields"
              (with-soft-assertions
               (expect
                (org-kit::timestamp-start
                 (timestamp-node :year-start
                                 2026
                                 :month-start
                                 10
                                 :day-start
                                 12
                                 :hour-start
                                 9
                                 :minute-start
                                 nil))
                :to-be
                nil)
               (expect
                (org-kit::timestamp-start
                 (timestamp-node :year-start
                                 2026
                                 :month-start
                                 10
                                 :day-start
                                 12
                                 :hour-start
                                 24
                                 :minute-start
                                 0))
                :to-be
                nil)
               (expect
                (org-kit::timestamp-start
                 (timestamp-node :year-start "2026" :month-start 10 :day-start 12))
                :to-be
                nil)))
          (it "rejects nodes that are not timestamps"
              (signals type-error (org-kit::timestamp-start (make-node :paragraph)))))

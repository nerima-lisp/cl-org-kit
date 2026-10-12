;;;; t/object-timestamp-test.lisp
(in-package #:cl-org-kit/test)

(defun timestamp-object-match (source &optional (start 0) (end (length source)))
  (org-kit::try-parse-timestamp-object (make-document source)
                                       start
                                       end
                                       :paragraph))

(defun timestamp-object-property (match key)
  (getf (org-kit::object-match-properties match) key))

(describe "timestamp object recognition"
          (it
           "recognizes fixture 05 active and inactive timestamps with repeaters and warnings"
           (let* ((source "<2026-10-12 Mon 09:30 +1w -2d>")
                  (match (timestamp-object-match source))
                  (properties (org-kit::object-match-properties match)))
             (with-soft-assertions
              (expect (org-kit::object-match-type match) :to-be :timestamp)
              (expect (org-kit::object-match-end match) :to-be (length source))
              (expect (org-kit::object-match-contents-begin match) :to-be nil)
              (expect (org-kit::object-match-contents-end match) :to-be nil)
              (expect properties
                      :to-equal
                      '(:type :active
                              :year-start
                              2026
                              :month-start
                              10
                              :day-start
                              12
                              :hour-start
                              9
                              :minute-start
                              30
                              :year-end
                              2026
                              :month-end
                              10
                              :day-end
                              12
                              :hour-end
                              9
                              :minute-end
                              30
                              :repeater-type
                              :cumulate
                              :repeater-value
                              1
                              :repeater-unit
                              :week
                              :warning-type
                              :all
                              :warning-value
                              2
                              :warning-unit
                              :day))))
           (let ((match (timestamp-object-match "[2026-10-11 Sun]")))
             (expect (org-kit::object-match-properties match)
                     :to-equal
                     '(:type :inactive
                             :year-start
                             2026
                             :month-start
                             10
                             :day-start
                             11
                             :hour-start
                             nil
                             :minute-start
                             nil
                             :year-end
                             2026
                             :month-end
                             10
                             :day-end
                             11
                             :hour-end
                             nil
                             :minute-end
                             nil
                             :repeater-type
                             nil
                             :repeater-value
                             nil
                             :repeater-unit
                             nil
                             :warning-type
                             nil
                             :warning-value
                             nil
                             :warning-unit
                             nil))))
          (it "preserves the distinct repeater and warning modes"
              (let ((catch-up (timestamp-object-match "<2026-10-12 ++2d --3w>"))
                    (restart (timestamp-object-match "<2026-10-12 .+4m>")))
                (with-soft-assertions
                 (expect
                  (list (timestamp-object-property catch-up :repeater-type)
                        (timestamp-object-property catch-up :repeater-value)
                        (timestamp-object-property catch-up :repeater-unit)
                        (timestamp-object-property catch-up :warning-type)
                        (timestamp-object-property catch-up :warning-value)
                        (timestamp-object-property catch-up :warning-unit))
                  :to-equal
                  '(:catch-up 2 :day :first 3 :week))
                 (expect
                  (list (timestamp-object-property restart :repeater-type)
                        (timestamp-object-property restart :repeater-value)
                        (timestamp-object-property restart :repeater-unit))
                  :to-equal
                  '(:restart 4 :month)))))
          (it
           "recognizes date ranges and time ranges using fixture 05 spellings"
           (let ((date-range
                  (timestamp-object-match "<2026-10-12 Mon>--<2026-10-14 Wed>"))
                 (time-range
                  (timestamp-object-match "<2026-10-12 Mon 09:00-10:30>"))
                 (date-only (timestamp-object-match "<2026-10-12>")))
             (with-soft-assertions
              (expect (timestamp-object-property date-range :type)
                      :to-be
                      :active-range)
              (expect (timestamp-object-property date-only :day-start)
                      :to-be
                      12)
              (expect
               (list (timestamp-object-property date-range :year-start)
                     (timestamp-object-property date-range :month-start)
                     (timestamp-object-property date-range :day-start)
                     (timestamp-object-property date-range :year-end)
                     (timestamp-object-property date-range :month-end)
                     (timestamp-object-property date-range :day-end))
               :to-equal
               '(2026 10 12 2026 10 14))
              (expect (timestamp-object-property time-range :type)
                      :to-be
                      :active-range)
              (expect
               (list (timestamp-object-property time-range :hour-start)
                     (timestamp-object-property time-range :minute-start)
                     (timestamp-object-property time-range :hour-end)
                     (timestamp-object-property time-range :minute-end))
               :to-equal
               '(9 0 10 30)))))
          (it "recognizes diary timestamps without inventing a date"
              (let ((match
                     (timestamp-object-match
                      "<%%(diary-float t 4 2) 12:00-14:00>")))
                (with-soft-assertions
                 (expect (timestamp-object-property match :type) :to-be :diary)
                 (expect
                  (list (timestamp-object-property match :year-start)
                        (timestamp-object-property match :month-start)
                        (timestamp-object-property match :day-start)
                        (timestamp-object-property match :hour-start)
                        (timestamp-object-property match :minute-start)
                        (timestamp-object-property match :year-end)
                        (timestamp-object-property match :hour-end)
                        (timestamp-object-property match :minute-end))
                  :to-equal
                  '(nil nil nil 12 0 nil 14 0)))))
          (it
           "preserves numerically shaped invalid dates for value conversion to reject"
           (let* ((match (timestamp-object-match "<2026-13-99 NotADay>"))
                  (timestamp
                   (make-node :timestamp
                              :properties
                              (org-kit::object-match-properties match))))
             (with-soft-assertions
              (expect (timestamp-object-property match :type) :to-be :active)
              (expect
               (list (timestamp-object-property match :year-start)
                     (timestamp-object-property match :month-start)
                     (timestamp-object-property match :day-start))
               :to-equal
               '(2026 13 99))
              (expect (org-kit::timestamp-start timestamp) :to-be nil))))
          (it
           "does not accept unterminated or range-truncated timestamp boundaries"
           (with-soft-assertions
            (expect (timestamp-object-match "<2026-10-12 Mon") :to-equal nil)
            (expect (timestamp-object-match "[2026-10-12 Sun") :to-equal nil)
            (expect (timestamp-object-match "<2026-10-12 Mon" 0 10)
                    :to-equal
                    nil)
            (let ((source
                   (concatenate 'string
                                "<2026-10-12 Mon"
                                (string #\Newline)
                                "Angle-link: <https:unterminated-angle-path>.")))
              (expect (timestamp-object-match source) :to-equal nil))
            (let* ((source "<2026-10-12 Mon>--<2026-10-14 Wed")
                   (match (timestamp-object-match source 0 (length source))))
              (expect (org-kit::object-match-end match) :to-be 16)
              (expect (timestamp-object-property match :type) :to-be :active))
            (let* ((source "Text <2026-10-12 Mon> tail")
                   (start (search "<2026-10-12" source))
                   (match (timestamp-object-match source start (length source))))
              (expect (org-kit::object-match-end match) :to-be (+ start 17))
              (expect (org-kit::object-match-post-blank match) :to-be 1)))))

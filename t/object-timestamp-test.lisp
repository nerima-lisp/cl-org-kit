;;;; t/object-timestamp-test.lisp
(in-package #:cl-org-kit/test)

(defun timestamp-object-match (source &optional (start 0) (end (length source)))
  (let* ((document (make-document source))
         (objects (org-kit::parse-objects document start end :paragraph)))
    (find-if (lambda (object) (eq (node-type object) :timestamp)) objects)))

(defun timestamp-object-property (object key)
  (node-property object key))

(defun timestamp-test-parse-org-matches (source)
  (let ((matches nil))
    (map-nodes (lambda (node)
                 (when (eq (node-type node) :timestamp)
                   (push node matches)))
               (document-root (org-kit:parse-org source)))
    (nreverse matches)))

(describe "timestamp object recognition"
          (it
           "recognizes fixture 05 active and inactive timestamps with repeaters and warnings"
           (let* ((source "<2026-10-12 Mon 09:30 +1w -2d>")
                  (match (timestamp-object-match source))
                  (properties (node-properties match)))
             (with-soft-assertions
              (expect (node-type match) :to-be :timestamp)
              (expect (node-end match) :to-be (length source))
              (expect (node-contents-begin match) :to-be nil)
              (expect (node-contents-end match) :to-be nil)
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
             (expect (node-properties match)
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
          (it "parses simple and malformed dates through the public Org API"
              (let* ((source
                      (format nil
                              "* Schedule~%~%<2026-10-12 Mon 09:30> <2026-10-13 09:30x> <2026-1x-12> <+026-10-12> <2026-10-14 .x> <2026-10-15 --1d> <2026-10-16 -1d>~%"))
                     (matches (timestamp-test-parse-org-matches source))
                     (simple-time (first matches))
                     (non-range-time (second matches))
                     (catch-up-warning (third matches))
                     (all-warning (fourth matches)))
                (with-soft-assertions
                  (expect (length matches) :to-be 4)
                  (expect (mapcar (lambda (match)
                                    (timestamp-object-property match :day-start))
                                  matches)
                          :to-equal
                          '(12 13 15 16))
                  (expect (timestamp-object-property simple-time :type)
                          :to-be
                          :active)
                  (expect
                   (list (timestamp-object-property simple-time :hour-start)
                         (timestamp-object-property simple-time :minute-start)
                         (timestamp-object-property simple-time :hour-end)
                         (timestamp-object-property simple-time :minute-end))
                   :to-equal
                   '(9 30 9 30))
                  (expect
                   (list (timestamp-object-property non-range-time :hour-start)
                         (timestamp-object-property non-range-time :minute-start))
                   :to-equal
                   '(nil nil))
                  (expect (timestamp-object-property catch-up-warning
                                                      :warning-type)
                          :to-be
                          :first)
                  (expect (timestamp-object-property all-warning :warning-type)
                          :to-be
                          :all))))
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
                   (node-properties match))))
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
              (expect (node-end match) :to-be 16)
              (expect (timestamp-object-property match :type) :to-be :active))
            (let* ((source "Text <2026-10-12 Mon> tail")
                   (start (search "<2026-10-12" source))
                   (match (timestamp-object-match source start (length source))))
              (expect (node-end match) :to-be (+ start 17))
              (expect (node-post-blank match) :to-be 1)))))

(describe "timestamp parser edge branches"
  (it "covers all adjustment units and rejects malformed suffix sequences"
    (let ((hour-repeater (timestamp-object-match "<2026-10-12 +1h>"))
          (year-repeater (timestamp-object-match "<2026-10-12 +1y>"))
          (hour-warning (timestamp-object-match "<2026-10-12 -2h>"))
          (malformed-time
            (timestamp-object-match "<2026-10-12 09:00-10>")))
      (with-soft-assertions
        (expect (timestamp-object-property hour-repeater :repeater-unit)
                :to-be
                :hour)
        (expect (timestamp-object-property year-repeater :repeater-unit)
                :to-be
                :year)
        (expect (timestamp-object-property hour-warning :warning-unit)
                :to-be
                :hour)
        (expect (timestamp-object-property malformed-time :hour-start)
                :to-be
                nil)
        (expect (timestamp-object-match "<2026-10-12 +>") :to-be nil)
        (expect (timestamp-object-match "<2026-10-12 +x>") :to-be nil)
        (expect (timestamp-object-match "<2026-10-12 +1q>") :to-be nil)
        (expect (timestamp-object-match "<2026-10-12 09:00 10:00>")
                :to-be
                nil)
        (expect (timestamp-object-match "<2026-10-12 +1d +2d>")
                :to-be
                nil)
        (expect (timestamp-object-match "<2026-10-12 -1d -2d>")
                :to-be
                nil)
        (expect (timestamp-object-match "<2026-10-12 Mon Tue>")
                :to-be
                nil)
        (expect (timestamp-object-match "<2026-10-12Mon>") :to-be nil)
        (expect (timestamp-object-match "<2026-10-12 09:00-10:00>")
                :to-be-truthy))))
  (it "parses diary Org text with escapes and nested expressions"
    (let* ((diary
             (concatenate 'string
                          "<%%(diary-float \"ignored "
                          (string #\\)
                          "\" )\" (nested))>"))
           (diary-match (timestamp-object-match diary))
           (nested-range
             (timestamp-object-match
              "<%%(diary-float t 4 2)>--<%%(diary-float t 4 3)>"))
           (inactive-range
             (timestamp-object-match "[2026-10-12]--[2026-10-13]")))
      (with-soft-assertions
        (expect (node-end diary-match) :to-be (length diary))
        (expect (timestamp-object-match "<%%x>") :to-be nil)
        (expect (timestamp-object-match "<%%") :to-be nil)
        (expect (timestamp-object-match "<%%(open>") :to-be nil)
        (expect (timestamp-object-property nested-range :type) :to-be :diary)
        (expect (timestamp-object-property nested-range :day-end) :to-be nil)
        (expect (timestamp-object-property inactive-range :type)
                :to-be
                :inactive-range)
        (expect (timestamp-object-match "<2026-10-12Mon>") :to-be nil)
        (expect (timestamp-object-match "<2026/10/12>") :to-be nil)
        (expect (timestamp-object-match "<2026-1x-12>") :to-be nil)
        (expect (timestamp-object-match
                 (format nil "<%%(f~c(x))>" #\Newline))
                :to-be
                nil)
        (expect (timestamp-object-match
                 (format nil "<2026-10-12~c>" #\Newline))
                :to-be
                nil)))))
(describe "diary timestamp scanner boundaries"
  (it "recognizes Elisp character literal boundaries"
    (with-soft-assertions
      (expect (org-kit::%timestamp-diary-character-token-boundary-p "?(" 0)
              :to-be-truthy)
      (expect (org-kit::%timestamp-diary-character-token-boundary-p "x?(" 1)
              :to-be nil)
      (expect (org-kit::%timestamp-diary-character-literal-end "?" 0 1)
              :to-be nil)
      (expect (org-kit::%timestamp-diary-character-literal-end "?(" 0 2)
              :to-be 2)
      (expect (org-kit::%timestamp-diary-character-literal-end "?\\(" 0 3)
              :to-be 3)
      (expect (org-kit::%timestamp-diary-character-literal-end "?\\C-a" 0 5)
              :to-be 5)
      (expect (org-kit::%timestamp-diary-character-literal-end "?\\C-\\(" 0 6)
              :to-be 6)
      (expect (org-kit::%timestamp-diary-character-literal-end "?\\^(" 0 4)
              :to-be 4)
      (expect (org-kit::%timestamp-diary-character-literal-end "?\\N{A}" 0 6)
              :to-be 6)
      (expect (org-kit::%timestamp-diary-character-literal-end "?\\N{A" 0 5)
              :to-be 3)
      (expect (org-kit::%timestamp-diary-character-literal-end "x?(" 1 3)
              :to-be nil)))
  (it "ignores parentheses in strings and comments until the line ending"
    (let* ((quote-character (code-char 34))
           (source (format nil "<%%(f ~C; )~C )>" quote-character quote-character))
           (string-literal (timestamp-object-match source)))
      (expect (node-end string-literal) :to-be (length source)))
    (dolist (line-ending (list #\Newline #\Return))
      (let* ((expression (format nil "%%(f ; )~C )" line-ending))
             (timestamp (format nil "<%%(f ; )~C )>" line-ending)))
        (with-soft-assertions
          (expect (org-kit::%timestamp-diary-expression-end
                   expression 0 (length expression))
                  :to-be (length expression))
          (expect (timestamp-object-match timestamp) :to-be nil))))))
(describe "diary character literal guard boundaries"
  (it "rejects empty ranges and non-character starters"
    (with-soft-assertions
      (expect
       (org-kit::%timestamp-diary-character-literal-end "" 0 0)
       :to-be nil)
      (expect
       (org-kit::%timestamp-diary-character-literal-end "x" 0 1)
       :to-be nil))))
(describe "diary character literal modifier EOF"
  (it "ends a control modifier at the scanner boundary"
    (expect
     (org-kit::%timestamp-diary-character-literal-end "?\\C-" 0 4)
     :to-be 4)))

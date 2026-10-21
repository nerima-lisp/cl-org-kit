;;;; t/edit-timestamp-test.lisp
(in-package #:cl-org-kit/test)

(defun edit-timestamp-test-first-node (document type)
  (let ((found nil))
    (map-nodes (lambda (node)
                 (when (and (null found) (eq (node-type node) type))
                   (setf found node)))
               (document-root document))
    found))

(defun edit-timestamp-test-octets (text)
  (cl-codec-kit:string-to-octets text :encoding :utf-8))

(defun edit-timestamp-test-unpatched-octets-match-p (source output spans)
  (let ((source-cursor 0)
        (output-cursor 0)
        (output-octets (edit-timestamp-test-octets output)))
    (dolist (span spans)
      (let* ((gap (subseq source source-cursor (edit-span-start span)))
             (gap-octets (edit-timestamp-test-octets gap)))
        (unless (zerop (length gap-octets))
          (let ((position (search gap-octets output-octets
                                  :start2 output-cursor)))
            (unless (and position
                         (or (plusp source-cursor) (zerop position)))
              (return-from edit-timestamp-test-unpatched-octets-match-p nil))
            (setf output-cursor (+ position (length gap-octets)))))
        (setf source-cursor (edit-span-end span))))
    (let* ((suffix (subseq source source-cursor))
           (suffix-octets (edit-timestamp-test-octets suffix)))
      (or (zerop (length suffix-octets))
          (let ((position (search suffix-octets output-octets
                                  :start2 output-cursor)))
            (and position
                 (= (+ position (length suffix-octets))
                    (length output-octets))))))))

(defun edit-timestamp-test-check-edit (source expected edit-function)
  (let* ((document (parse-org source))
         (timestamp (edit-timestamp-test-first-node document :timestamp))
         (source-bytes (edit-timestamp-test-octets source)))
    (with-soft-assertions
      (expect (plusp (length source-bytes)) :to-be t)
      (expect (node-p timestamp) :to-be t)
      (when (node-p timestamp)
        (let* ((result (funcall edit-function document timestamp))
               (updated (edit-result-updated-document result))
               (output (document-text updated))
               (spans (edit-result-spans result)))
          (expect (edit-result-p result) :to-be t)
          (expect output :to-equal expected)
          (expect (document-text document) :to-equal source)
          (expect (write-org document :output-type :octets)
                  :to-equalp source-bytes)
          (expect (edit-timestamp-test-unpatched-octets-match-p
                   source output spans)
                  :to-be t)
          (loop for tail on spans
                while (cdr tail)
                for left = (first tail)
                for right = (second tail)
                do (expect (<= (edit-span-end left)
                               (edit-span-start right))
                           :to-be t)))))))

(defun edit-timestamp-test-condition (thunk)
  (handler-case
      (progn (funcall thunk) nil)
    (error (condition) condition)))

(defun edit-timestamp-test-assert-error (condition reason)
  (with-soft-assertions
    (expect (typep condition 'org-edit-error) :to-be t)
    (when (typep condition 'org-edit-error)
      (expect (org-edit-error-reason condition) :to-be reason)
      (expect (stringp (org-edit-error-target condition)) :to-be t))))

(describe "timestamp edits"
  (it "adjusts dates and regenerates weekdays with Org calendar overflow"
    (edit-timestamp-test-check-edit
     "前 <2024-02-28 Wed> 後"
     "前 <2024-02-29 Thu> 後"
     (lambda (document timestamp)
       (org-kit::adjust-timestamp document timestamp :day 1)))
    (edit-timestamp-test-check-edit
     "<2024-02-29 Thu>"
     "<2025-03-01 Sat>"
     (lambda (document timestamp)
       (org-kit::adjust-timestamp document timestamp :year 1)))
    (edit-timestamp-test-check-edit
     "<2024-01-31 Wed>"
     "<2024-03-02 Sat>"
     (lambda (document timestamp)
       (org-kit::adjust-timestamp document timestamp :month 1)))
    (edit-timestamp-test-check-edit
     "<2024-01-31>"
     "<2024-02-01 Thu>"
     (lambda (document timestamp)
       (org-kit::adjust-timestamp document timestamp :day 1))))

  (it "carries clock changes across dates and shifts both times in a clock range"
    (edit-timestamp-test-check-edit
     "<2024-01-31 Wed 23:30>"
     "<2024-02-01 Thu 00:30>"
     (lambda (document timestamp)
       (org-kit::adjust-timestamp document timestamp :hour 1)))
    (edit-timestamp-test-check-edit
     "<2024-01-31 Wed 10:25>"
     "<2024-01-31 Wed 10:26>"
     (lambda (document timestamp)
       (org-kit::adjust-timestamp document timestamp :minute 1)))
    (edit-timestamp-test-check-edit
     "<2024-01-31 Wed 10:25-11:30>"
     "<2024-01-31 Wed 10:26-11:31>"
     (lambda (document timestamp)
       (org-kit::adjust-timestamp document timestamp :minute 1))))

  (it "selects either date and clock endpoint of an external range"
    (edit-timestamp-test-check-edit
     "<2024-01-31 Wed 09:00>--<2024-02-05 Mon 12:00>"
     "<2024-02-01 Thu 09:00>--<2024-02-05 Mon 12:00>"
     (lambda (document timestamp)
       (org-kit::adjust-timestamp document timestamp :day 1)))
    (edit-timestamp-test-check-edit
     "<2024-01-31 Wed 09:00>--<2024-02-05 Mon 12:00>"
     "<2024-01-31 Wed 09:00>--<2024-02-06 Tue 12:00>"
     (lambda (document timestamp)
       (org-kit::adjust-timestamp document timestamp :day 1 :endpoint :end)))
    (edit-timestamp-test-check-edit
     "<2024-01-31 Wed 09:00>--<2024-02-05 Mon 12:00>"
     "<2024-01-31 Wed 10:00>--<2024-02-05 Mon 12:00>"
     (lambda (document timestamp)
       (org-kit::adjust-timestamp document timestamp :hour 1)))
    (edit-timestamp-test-check-edit
     "<2024-01-31 Wed 09:00>--<2024-02-05 Mon 12:00>"
     "<2024-01-31 Wed 09:00>--<2024-02-05 Mon 13:00>"
     (lambda (document timestamp)
       (org-kit::adjust-timestamp document timestamp :hour 1 :endpoint :end))))

  (it "adjusts repeater and warning amounts and steps their units"
    (edit-timestamp-test-check-edit
     "<2024-01-31 Wed +1d>"
     "<2024-01-31 Wed +2d>"
     (lambda (document timestamp)
       (org-kit::adjust-timestamp document timestamp :repeater-amount 1)))
    (edit-timestamp-test-check-edit
     "<2024-01-31 Wed +1d>"
     "<2024-01-31 Wed +1w>"
     (lambda (document timestamp)
       (org-kit::adjust-timestamp document timestamp :repeater-unit 1)))
    (edit-timestamp-test-check-edit
     "<2024-01-31 Wed -2d>"
     "<2024-01-31 Wed -3d>"
     (lambda (document timestamp)
       (org-kit::adjust-timestamp document timestamp :warning-amount 1)))
    (edit-timestamp-test-check-edit
     "<2024-01-31 Wed -2d>"
     "<2024-01-31 Wed -2w>"
     (lambda (document timestamp)
       (org-kit::adjust-timestamp document timestamp :warning-unit 1)))
    (edit-timestamp-test-check-edit
     "<2024-01-31 Wed +1h>"
     "<2024-01-31 Wed +1h>"
     (lambda (document timestamp)
       (org-kit::adjust-timestamp document timestamp :repeater-amount 1)))
    (edit-timestamp-test-check-edit
     "<2024-01-31 Wed -1h>"
     "<2024-01-31 Wed -1h>"
     (lambda (document timestamp)
       (org-kit::adjust-timestamp document timestamp :warning-amount 1))))

  (it "edits a repeater amount without losing its window"
    (edit-timestamp-test-check-edit
     "<2024-01-31 Wed +1w/2d>"
     "<2024-01-31 Wed +2w/2d>"
     (lambda (document timestamp)
       (org-kit::adjust-timestamp document timestamp :repeater-amount 1))))

  (it "adjusts repeater and warning suffixes on the selected range endpoint"
    (let ((source "<2024-01-31 Wed +1d -2d>--<2024-02-05 Mon +1w -3w>"))
      (edit-timestamp-test-check-edit
       source
       "<2024-01-31 Wed +1d -2d>--<2024-02-05 Mon +2w -3w>"
       (lambda (document timestamp)
         (org-kit::adjust-timestamp document timestamp :repeater-amount 1
                                    :endpoint :end)))
      (edit-timestamp-test-check-edit
       source
       "<2024-01-31 Wed +1d -2d>--<2024-02-05 Mon +1m -3w>"
       (lambda (document timestamp)
         (org-kit::adjust-timestamp document timestamp :repeater-unit 1
                                    :endpoint :end)))
      (edit-timestamp-test-check-edit
       source
       "<2024-01-31 Wed +1d -2d>--<2024-02-05 Mon +1w -4w>"
       (lambda (document timestamp)
         (org-kit::adjust-timestamp document timestamp :warning-amount 1
                                    :endpoint :end)))
      (edit-timestamp-test-check-edit
       source
       "<2024-01-31 Wed +1d -2d>--<2024-02-05 Mon +1w -3m>"
       (lambda (document timestamp)
         (org-kit::adjust-timestamp document timestamp :warning-unit 1
                                    :endpoint :end))))
    (let* ((source "<2024-01-31 Wed +1d -2d>--<2024-02-05 Mon +1w -3w>")
           (document (parse-org source))
           (timestamp (edit-timestamp-test-first-node document :timestamp))
           (result (org-kit::adjust-timestamp document timestamp
                                              :repeater-amount 1
                                              :endpoint :end))
           (updated (edit-result-updated-document result))
           (updated-timestamp
             (edit-timestamp-test-first-node updated :timestamp)))
      (with-soft-assertions
        (expect (document-text updated)
                :to-equal
                "<2024-01-31 Wed +1d -2d>--<2024-02-05 Mon +2w -3w>")
        (expect (node-property updated-timestamp :repeater-value) :to-be 1)
        (expect (node-property updated-timestamp :repeater-unit) :to-be :day)
        (expect (node-property updated-timestamp :warning-value) :to-be 2)
        (expect (node-property updated-timestamp :warning-unit) :to-be :day)
        (expect (document-text document) :to-equal source))))

  (it "preserves Org amount and unit boundaries as no-ops"
    (edit-timestamp-test-check-edit
     "<2024-01-31 Wed +1d>"
     "<2024-01-31 Wed +1d>"
     (lambda (document timestamp)
       (org-kit::adjust-timestamp document timestamp :repeater-amount -1)))
    (edit-timestamp-test-check-edit
     "<2024-01-31 Wed -0d>"
     "<2024-01-31 Wed -0d>"
     (lambda (document timestamp)
       (org-kit::adjust-timestamp document timestamp :warning-amount -1)))
    (edit-timestamp-test-check-edit
     "<2024-01-31 Wed +1d>"
     "<2024-01-31 Wed +1d>"
     (lambda (document timestamp)
       (org-kit::adjust-timestamp document timestamp :repeater-unit -1)))
    (edit-timestamp-test-check-edit
     "<2024-01-31 Wed -1y>"
     "<2024-01-31 Wed -1y>"
     (lambda (document timestamp)
       (org-kit::adjust-timestamp document timestamp :warning-unit 1))))

  (it "toggles delimiters around complete points and external ranges"
    (edit-timestamp-test-check-edit
     "<2024-01-31 Wed>"
     "[2024-01-31 Wed]"
     #'org-kit::toggle-timestamp-active)
    (edit-timestamp-test-check-edit
     "[2024-01-31 Wed]"
     "<2024-01-31 Wed>"
     #'org-kit::toggle-timestamp-active)
    (edit-timestamp-test-check-edit
     "<2024-01-31 Wed>--<2024-02-01 Thu>"
     "[2024-01-31 Wed]--[2024-02-01 Thu]"
     #'org-kit::toggle-timestamp-active))

  (it "signals named errors for invalid components, deltas, endpoints, and targets"
    (let* ((source "<2024-01-31 Wed>")
           (document (parse-org source))
           (timestamp (edit-timestamp-test-first-node document :timestamp))
           (condition
             (edit-timestamp-test-condition
              (lambda ()
                (org-kit::adjust-timestamp document timestamp :day 0)))))
      (edit-timestamp-test-assert-error condition :invalid-value)
      (expect (document-text document) :to-equal source))
    (let* ((source "<2024-01-31 Wed>")
           (document (parse-org source))
           (timestamp (edit-timestamp-test-first-node document :timestamp))
           (condition
             (edit-timestamp-test-condition
              (lambda ()
                (org-kit::adjust-timestamp document timestamp :second 1)))))
      (edit-timestamp-test-assert-error condition :invalid-value)
      (expect (document-text document) :to-equal source))
    (let* ((source "<2024-01-31 Wed>")
           (document (parse-org source))
           (timestamp (edit-timestamp-test-first-node document :timestamp))
           (condition
             (edit-timestamp-test-condition
              (lambda ()
                (org-kit::adjust-timestamp document timestamp :day 1
                                           :endpoint :end)))))
      (edit-timestamp-test-assert-error condition :invalid-value)
      (expect (document-text document) :to-equal source))
    (let* ((document (parse-org "* Heading\n"))
           (heading (edit-timestamp-test-first-node document :headline))
           (condition
             (edit-timestamp-test-condition
              (lambda ()
                (org-kit::adjust-timestamp document heading :day 1)))))
      (edit-timestamp-test-assert-error condition :wrong-node-type))
    (let* ((document (parse-org "<2024-01-31 Wed>"))
           (other-document (parse-org "<2024-02-01 Thu>"))
           (foreign-timestamp
             (edit-timestamp-test-first-node other-document :timestamp))
           (condition
             (edit-timestamp-test-condition
              (lambda ()
                (org-kit::toggle-timestamp-active document
                                                 foreign-timestamp)))))
      (edit-timestamp-test-assert-error condition :foreign-node)
      (expect (document-text document) :to-equal "<2024-01-31 Wed>")))
  (it "rejects malformed timestamp source ranges"
      (flet ((expect-invalid (thunk)
                             (edit-timestamp-test-assert-error
                              (edit-timestamp-test-condition thunk)
                              :invalid-value)))
        (let* ((document (make-document "x"))
               (timestamp (make-node :timestamp)))
          (expect-invalid
           (lambda ()
             (org-kit::edit-timestamp-endpoint-ranges
              document timestamp timestamp))))
        (let* ((source "<x>--")
               (document (make-document source))
               (timestamp (make-node :timestamp :begin 0 :end (length source))))
          (expect (org-kit::edit-timestamp-endpoint-ranges
                   document timestamp timestamp)
                  :to-equal '((0 2))))
        (let* ((source "abc")
               (document (make-document source))
               (timestamp (make-node :timestamp :begin 0 :end (length source))))
          (expect-invalid
           (lambda ()
             (org-kit::edit-timestamp-endpoint-ranges
              document timestamp timestamp))))
        (let* ((source "<abc")
               (document (make-document source))
               (timestamp (make-node :timestamp :begin 0 :end (length source))))
          (expect-invalid
           (lambda ()
             (org-kit::edit-timestamp-endpoint-ranges
              document timestamp timestamp))))
        (let* ((source "<2024-01-01>--<2024-01-02>--<2024-01-03>")
               (document (make-document source))
               (timestamp (make-node :timestamp :begin 0 :end (length source))))
          (expect-invalid
           (lambda ()
             (org-kit::edit-timestamp-endpoint-ranges
              document timestamp timestamp))))
        (expect-invalid
         (lambda ()
           (org-kit::edit-timestamp-endpoint-index
            :middle '((0 2)) "timestamp")))
        (let* ((source "<bad>")
               (document (make-document source))
               (timestamp (make-node :timestamp :begin 0 :end (length source)))
               (date (cl-date-kit:make-local-date 2024 2 1)))
          (expect-invalid
           (lambda ()
             (org-kit::edit-timestamp-date-patches
              document '(0 4) date timestamp)))
          (expect-invalid
           (lambda ()
             (org-kit::edit-timestamp-adjusted-date
              source '(0 4) :day 1 timestamp)))
          (expect-invalid
           (lambda ()
             (org-kit::edit-timestamp-find-time
              source '(0 4) timestamp)))
          (expect-invalid
           (lambda ()
             (org-kit::edit-timestamp-adjustment-layout
              source '(0 4) :repeater timestamp))))))
  (it "preserves weekday placement and rejects invalid timestamp edits"
      (edit-timestamp-test-check-edit
       "<2024-01-31 >"
       "<2024-02-01 Thu>"
       (lambda (document timestamp)
         (org-kit::adjust-timestamp document timestamp :day 1)))
      (edit-timestamp-test-check-edit
       "<2024-01-31 10:25>"
       "<2024-02-01 Thu 10:25>"
       (lambda (document timestamp)
         (org-kit::adjust-timestamp document timestamp :day 1)))
      (edit-timestamp-test-check-edit
       "<2024-01-31 +1d>"
       "<2024-02-01 Thu +1d>"
       (lambda (document timestamp)
         (org-kit::adjust-timestamp document timestamp :day 1)))
      (edit-timestamp-test-check-edit
       "[2024-01-31 Wed]--[2024-02-01 Thu]"
       "<2024-01-31 Wed>--<2024-02-01 Thu>"
       #'org-kit::toggle-timestamp-active)
      (flet ((expect-invalid (thunk)
                             (edit-timestamp-test-assert-error
                              (edit-timestamp-test-condition thunk)
                              :invalid-value)))
        (expect-invalid
         (lambda ()
           (org-kit::adjust-timestamp nil nil :day 1)))
        (let* ((document (parse-org "<2024-01-31 Wed>"))
               (timestamp (edit-timestamp-test-first-node document :timestamp)))
          (expect-invalid
           (lambda ()
             (org-kit::adjust-timestamp document timestamp :day 1
                                        :endpoint :middle))))
        (let* ((document (parse-org "<2024-01-31 Wed>"))
               (timestamp (edit-timestamp-test-first-node document :timestamp)))
          (expect-invalid
           (lambda ()
             (org-kit::adjust-timestamp document timestamp :hour 1))))
        (let* ((source "<2024-01-31 09:00-10:00>--<2024-02-01 12:00-13:00>")
               (document (parse-org source))
               (timestamp (edit-timestamp-test-first-node document :timestamp)))
          (expect-invalid
           (lambda ()
             (org-kit::adjust-timestamp document timestamp :hour 1))))
        (let* ((source "<9999-12-31>")
               (document (parse-org source))
               (timestamp (edit-timestamp-test-first-node document :timestamp)))
          (expect-invalid
           (lambda ()
             (org-kit::adjust-timestamp document timestamp :year 1)))
          (expect-invalid
           (lambda ()
             (org-kit::adjust-timestamp document timestamp :day 1))))
        (expect-invalid
         (lambda ()
           (org-kit::edit-timestamp-toggled-type :diary nil)))))
  (it "steps every repeater unit and retains its prefix"
      (edit-timestamp-test-check-edit
       "<2024-01-31 +1h>"
       "<2024-01-31 +1h>"
       (lambda (document timestamp)
         (org-kit::adjust-timestamp document timestamp :repeater-unit -1)))
      (edit-timestamp-test-check-edit
       "<2024-01-31 +1d>"
       "<2024-01-31 +1w>"
       (lambda (document timestamp)
         (org-kit::adjust-timestamp document timestamp :repeater-unit 1)))
      (edit-timestamp-test-check-edit
       "<2024-01-31 +1d>"
       "<2024-01-31 +1d>"
       (lambda (document timestamp)
         (org-kit::adjust-timestamp document timestamp :repeater-unit -1)))
      (edit-timestamp-test-check-edit
       "<2024-01-31 +1w>"
       "<2024-01-31 +1m>"
       (lambda (document timestamp)
         (org-kit::adjust-timestamp document timestamp :repeater-unit 1)))
      (edit-timestamp-test-check-edit
       "<2024-01-31 +1w>"
       "<2024-01-31 +1d>"
       (lambda (document timestamp)
         (org-kit::adjust-timestamp document timestamp :repeater-unit -1)))
      (edit-timestamp-test-check-edit
       "<2024-01-31 +1m>"
       "<2024-01-31 +1y>"
       (lambda (document timestamp)
         (org-kit::adjust-timestamp document timestamp :repeater-unit 1)))
      (edit-timestamp-test-check-edit
       "<2024-01-31 +1m>"
       "<2024-01-31 +1w>"
       (lambda (document timestamp)
         (org-kit::adjust-timestamp document timestamp :repeater-unit -1)))
      (edit-timestamp-test-check-edit
       "<2024-01-31 +1y>"
       "<2024-01-31 +1m>"
       (lambda (document timestamp)
         (org-kit::adjust-timestamp document timestamp :repeater-unit -1)))
      (edit-timestamp-test-check-edit
       "<2024-01-31 +1y>"
       "<2024-01-31 +1y>"
       (lambda (document timestamp)
         (org-kit::adjust-timestamp document timestamp :repeater-unit 1)))
      (edit-timestamp-test-check-edit
       "<2024-01-31 ++1d>"
       "<2024-01-31 ++1w>"
       (lambda (document timestamp)
         (org-kit::adjust-timestamp document timestamp :repeater-unit 1)))
      (edit-timestamp-test-check-edit
       "<2024-01-31 .+1d>"
       "<2024-01-31 .+1w>"
       (lambda (document timestamp)
         (org-kit::adjust-timestamp document timestamp :repeater-unit 1)))
      (edit-timestamp-test-check-edit
       "<2024-01-31 --1d>"
       "<2024-01-31 --1w>"
       (lambda (document timestamp)
         (org-kit::adjust-timestamp document timestamp :warning-unit 1)))
      (let* ((document (parse-org "<2024-01-31 Wed>"))
             (timestamp (edit-timestamp-test-first-node document :timestamp))
             (condition
              (edit-timestamp-test-condition
               (lambda ()
                 (org-kit::adjust-timestamp document timestamp
                                            :repeater-amount 1)))))
        (edit-timestamp-test-assert-error condition :invalid-value))))

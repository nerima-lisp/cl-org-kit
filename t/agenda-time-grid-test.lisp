;;;; t/agenda-time-grid-test.lisp
(in-package #:cl-org-kit/test)

(defun agenda-time-grid-test-context (reference-time)
  (make-time-context :reference-time (make-org-time-value reference-time)
                     :zone (resolve-time-zone "UTC")))

(defun agenda-time-grid-test-condition (function)
  (handler-case (progn (funcall function) nil)
    (condition (condition) condition)))

(defun agenda-time-grid-test-row-data (rows)
  (mapcar (lambda (row)
            (list (org-time-value-iso8601 (agenda-time-grid-row-date row))
                  (agenda-time-grid-row-time row)
                  (agenda-time-grid-row-current-p row)))
          rows))

(describe "Agenda time grid"
  (it "uses Org 9.8.10's default ticks over an inclusive date range"
    (let* ((document (make-document ""))
           (rows (agenda-time-grid
                  (list document)
                  (make-org-time-value "2025-01-01")
                  (make-org-time-value "2025-01-02"))))
      (expect (agenda-time-grid-test-row-data rows)
              :to-equal
              '(("2025-01-01" 800 nil)
                ("2025-01-01" 1000 nil)
                ("2025-01-01" 1200 nil)
                ("2025-01-01" 1400 nil)
                ("2025-01-01" 1600 nil)
                ("2025-01-01" 1800 nil)
                ("2025-01-01" 2000 nil)
                ("2025-01-02" 800 nil)
                ("2025-01-02" 1000 nil)
                ("2025-01-02" 1200 nil)
                ("2025-01-02" 1400 nil)
                ("2025-01-02" 1600 nil)
                ("2025-01-02" 1800 nil)
                ("2025-01-02" 2000 nil)))))

  (it "preserves explicit tick order and duplicates without generating intervals"
    (let* ((document (make-document ""))
           (date (make-org-time-value "2025-01-01")))
      (expect (agenda-time-grid-test-row-data
               (agenda-time-grid (list document) date date
                                 :times '(930 930 1000 2400)))
              :to-equal
              '(("2025-01-01" 930 nil)
                ("2025-01-01" 930 nil)
                ("2025-01-01" 1000 nil)
                ("2025-01-01" 2400 nil)))
      (expect (agenda-time-grid (list document) date date :times nil)
              :to-equal nil)))

  (it "adds one separate current-time marker from the supplied context"
    (let* ((document (make-document ""))
           (rows (agenda-time-grid
                  (list document)
                  (make-org-time-value "2025-01-01")
                  (make-org-time-value "2025-01-03")
                  :time-context
                  (agenda-time-grid-test-context "2025-01-02T09:45:27")
                  :times '(900 945 1000))))
      (expect (agenda-time-grid-test-row-data rows)
              :to-equal
              '(("2025-01-01" 900 nil)
                ("2025-01-01" 945 nil)
                ("2025-01-01" 1000 nil)
                ("2025-01-02" 900 nil)
                ("2025-01-02" 945 nil)
                ("2025-01-02" 1000 nil)
                ("2025-01-02" 945 t)
                ("2025-01-03" 900 nil)
                ("2025-01-03" 945 nil)
                ("2025-01-03" 1000 nil)))))

  (it "removes matching agenda ticks by date and time but retains the current marker"
    (let* ((document
             (parse-org
              (format nil
                      "* All day~%<2025-01-02 Thu>~%* Timed~%<2025-01-02 Thu 09:00>~%")))
           (rows (agenda-time-grid
                  (list document)
                  (make-org-time-value "2025-01-01")
                  (make-org-time-value "2025-01-03")
                  :time-context
                  (agenda-time-grid-test-context "2025-01-02T09:00:00")
                  :times '(900 900 1200)
                  :remove-match t)))
      (expect (agenda-time-grid-test-row-data rows)
              :to-equal
              '(("2025-01-01" 900 nil)
                ("2025-01-01" 900 nil)
                ("2025-01-01" 1200 nil)
                ("2025-01-02" 1200 nil)
                ("2025-01-02" 900 t)
                ("2025-01-03" 900 nil)
                ("2025-01-03" 900 nil)
                ("2025-01-03" 1200 nil)))))

  (it "rejects malformed dates, reversed ranges, tick values, and options"
    (let* ((document (make-document ""))
           (start (make-org-time-value "2025-01-02"))
           (end (make-org-time-value "2025-01-01"))
           (reversed
             (agenda-time-grid-test-condition
              (lambda () (agenda-time-grid (list document) start end))))
           (malformed-date
             (agenda-time-grid-test-condition
              (lambda () (agenda-time-grid (list document) "2025-01-01" start))))
           (bad-minute
             (agenda-time-grid-test-condition
              (lambda () (agenda-time-grid (list document) start start
                                           :times '(2360)))))
           (after-midnight
             (agenda-time-grid-test-condition
              (lambda () (agenda-time-grid (list document) start start
                                           :times '(2401)))))
           (bad-time-list
             (agenda-time-grid-test-condition
              (lambda () (agenda-time-grid (list document) start start
                                           :times '(900 . 1000)))))
           (bad-time-value
             (agenda-time-grid-test-condition
              (lambda () (agenda-time-grid (list document) start start
                                           :times '("0900")))))
           (bad-option
             (agenda-time-grid-test-condition
              (lambda () (agenda-time-grid (list document) start start
                                           :remove-match :yes)))))
      (expect (org-agenda-error-reason reversed) :to-equal :invalid-range)
      (expect (org-time-error-reason malformed-date) :to-equal :invalid-time)
      (expect (org-agenda-error-reason bad-minute) :to-equal :invalid-option)
      (expect (org-agenda-error-reason after-midnight) :to-equal :invalid-option)
      (expect (org-agenda-error-reason bad-time-list) :to-equal :invalid-option)
      (expect (org-agenda-error-reason bad-time-value) :to-equal :invalid-option)
      (expect (org-agenda-error-reason bad-option) :to-equal :invalid-option)))

  (it "requires a context only when removing agenda matches"
    (let* ((document (make-document ""))
           (date (make-org-time-value "2025-01-01"))
           (context (agenda-time-grid-test-context "2025-01-01T08:15:00"))
           (missing-context
             (agenda-time-grid-test-condition
              (lambda () (agenda-time-grid (list document) date date
                                           :remove-match t))))
           (invalid-context
             (agenda-time-grid-test-condition
              (lambda () (agenda-time-grid (list document) date date
                                           :time-context "not a context"))))
           (empty-match-rows
             (agenda-time-grid
              (list document) date date :time-context context
              :times '(900) :remove-match t)))
      (expect (org-agenda-error-reason missing-context)
              :to-equal :time-context-required)
      (expect (org-time-error-reason invalid-context) :to-equal :invalid-time)
      (expect (agenda-time-grid-test-row-data empty-match-rows)
              :to-equal
              '(("2025-01-01" 900 nil)
                ("2025-01-01" 815 t)))))

  (it "enforces inclusive range and generated-row limits"
    (let* ((range-limited
             (make-document "" :limits (make-limits :max-agenda-range-days 2)))
           (rows-limited
             (make-document "" :limits (make-limits :max-agenda-entries 3)))
           (range-error
             (agenda-time-grid-test-condition
              (lambda ()
                (agenda-time-grid
                 (list range-limited)
                 (make-org-time-value "2025-01-01")
                 (make-org-time-value "2025-01-03")
                 :times '(900)))))
           (row-error
             (agenda-time-grid-test-condition
              (lambda ()
                (agenda-time-grid
                 (list rows-limited)
                 (make-org-time-value "2025-01-01")
                 (make-org-time-value "2025-01-02")
                 :times '(900 1000))))))
      (expect (org-resource-limit-error-limit-name range-error)
              :to-equal :max-agenda-range-days)
      (expect (org-resource-limit-error-actual range-error) :to-be 3)
      (expect (org-resource-limit-error-limit-name row-error)
              :to-equal :max-agenda-entries)
      (expect (org-resource-limit-error-actual row-error) :to-be 4))))

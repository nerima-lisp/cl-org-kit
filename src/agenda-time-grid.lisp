;;;; src/agenda-time-grid.lisp
(in-package #:org-kit)

(defparameter +agenda-time-grid-default-times+
  '(800 1000 1200 1400 1600 1800 2000))

(defstruct (agenda-time-grid-row
            (:constructor %make-agenda-time-grid-row (&key date time current-p))
            (:copier nil))
  "A dated tick or current-time marker in an agenda grid."
  date time current-p)

(defun agenda-time-grid-time-p (value)
  (and (integerp value)
       (or (and (<= 0 value 2359)
                (< (mod value 100) 60))
           (= value 2400))))

(defun agenda-time-grid-current-time (time-context)
  (when time-context
    (let ((local-time (agenda-query-reference-time time-context)))
      (list (agenda-date-ordinal
             (cl-date-kit:local-date-time-date local-time))
            (+ (* 100 (cl-date-kit:local-date-time-hour local-time))
               (cl-date-kit:local-date-time-minute local-time))))))

(defun agenda-time-grid-matched-times (entries)
  (let ((matched-times (make-hash-table :test #'equal)))
    (dolist (entry entries)
      (let* ((date (agenda-date-ordinal
                    (agenda-local-date (agenda-entry-date entry))))
             (start (org-timestamp-view-start (agenda-entry-timestamp entry)))
             (hour (org-time-value-hour start)))
        (when hour
          (setf (gethash
                 (list date (+ (* 100 hour) (org-time-value-minute start)))
                 matched-times)
                t))))
    matched-times))

(defun agenda-time-grid (documents start-date end-date
                         &key time-context (times nil times-supplied-p)
                           remove-match)
  "Return explicit time-grid rows across the inclusive date range."
  (let* ((documents (agenda-normalize-documents documents))
         (start-date (agenda-require-date start-date))
         (end-date (agenda-require-date end-date))
         (start-ordinal (agenda-date-ordinal (agenda-local-date start-date)))
         (end-ordinal (agenda-date-ordinal (agenda-local-date end-date)))
         (limits (agenda-operation-limits documents))
         (times (if times-supplied-p times
                    +agenda-time-grid-default-times+))
         (times-count (agenda-proper-list-length times))
         (time-context (and time-context
                            (agenda-require-time-context time-context)))
         (current-time (agenda-time-grid-current-time time-context))
         (matched-times nil)
         (rows nil)
         (row-count 0))
    (agenda-validate-option (lambda (value) (typep value 'boolean))
                            remove-match :remove-match)
    (unless times-count
      (agenda-signal-error :invalid-option times "Times must be a proper list"))
    (dolist (time times)
      (agenda-validate-option #'agenda-time-grid-time-p time :times))
    (when (> start-ordinal end-ordinal)
      (agenda-signal-error :invalid-range (list start-date end-date)
                           "Agenda start date follows its end date"))
    (check-limit limits :max-agenda-range-days
                 (agenda-range-size (agenda-local-date start-date)
                                    (agenda-local-date end-date)))
    (check-limit limits :max-agenda-entries times-count)
    (when (and remove-match (null time-context))
      (agenda-signal-error :time-context-required nil
                           "Removing matching agenda times requires a time context"))
    (when remove-match
      (setf matched-times
            (agenda-time-grid-matched-times
             (agenda-result-entries
              (agenda documents start-date end-date
                      :time-context time-context)))))
    (loop for date-ordinal from start-ordinal to end-ordinal
          for date = (agenda-date-value
                      (cl-date-kit:local-date-from-epoch-day date-ordinal))
          do (dolist (time times)
               (unless (and matched-times
                            (gethash (list date-ordinal time) matched-times))
                 (incf row-count)
                 (check-limit limits :max-agenda-entries row-count)
                 (push (%make-agenda-time-grid-row
                        :date date :time time :current-p nil)
                       rows)))
             (when (and current-time (= date-ordinal (first current-time)))
               (incf row-count)
               (check-limit limits :max-agenda-entries row-count)
               (push (%make-agenda-time-grid-row
                      :date date :time (second current-time) :current-p t)
                     rows)))
    (nreverse rows)))

;;;; src/edit-clocktable.lisp
(in-package #:org-kit)

(defun edit-clocktable-body-span (document block target)
  (let* ((source (document-text document))
         (start (node-begin block))
         (limit (node-end block))
         (context (%make-reader-context :text source))
         (opening-newline (position #\Newline source :start start :end limit)))
    (unless opening-newline
      (edit-signal-error target :uneditable-structure
                         "Dynamic block opener has no body boundary"))
    (let ((body-start (1+ opening-newline)))
      (multiple-value-bind (end after-line)
          (reader-blocks-find-close context body-start limit "#+END:")
        (declare (ignore after-line))
        (when (= end limit)
          (edit-signal-error target :uneditable-structure
                             "Dynamic block has no #+END line"))
        (values body-start end)))))

(defun edit-clocktable-title-range (title)
  (if (> (length title) 40)
      (values (or (position #\Space title :end 37 :from-end t) 37) t)
      (values (length title) nil)))

(defun edit-clocktable-emphasize (text level emphasize-p)
  (if emphasize-p
      (case level
        (1 (format nil "*~A*" text))
        (2 (format nil "/~A/" text))
        (t text))
      text))

(defun edit-clocktable-headline (row &optional emphasize-p)
  (let ((level (clocktable-row-level row))
        (title (clocktable-row-title row)))
    (multiple-value-bind (end shortened-p)
        (edit-clocktable-title-range title)
      (let* ((shortened
              (if shortened-p
                  (concatenate 'string (subseq title 0 end) "...")
                  title))
             (display-title
               (edit-clocktable-emphasize shortened level emphasize-p)))
        (if (> level 1)
            (concatenate 'string "\\_"
                         (make-string (* 2 (1- level)) :initial-element #\Space)
                         display-title)
            display-title)))))

(defun edit-clocktable-check-character-lower-bound (document characters)
  (let ((limits (document-limits document)))
    (check-limit limits :max-output-length characters)
    (dolist (limit-name
              (if (document-encoding document)
                  '(:max-output-octets :max-input-length)
                  '(:max-input-length :max-output-octets)))
      (check-limit limits limit-name characters))))

(defun edit-clocktable-check-indentation-size (document row)
  (let ((level (clocktable-row-level row)))
    (when (> level 1)
      (edit-clocktable-check-character-lower-bound
       document (+ 2 (* 2 (1- level)))))))

(defun edit-clocktable-time-text (minutes)
  (multiple-value-bind (days remainder) (floor minutes 1440)
    (if (plusp days)
        (format nil "~Dd ~D:~2,'0D"
                days (floor remainder 60) (mod remainder 60))
        (format nil "~D:~2,'0D" (floor minutes 60) (mod minutes 60)))))

(defun edit-clocktable-time-cell-length-lower-bound (minutes total-p)
  (let ((days (floor minutes 1440))
        (wrapping-length (if total-p 2 0)))
    (+ wrapping-length
       (if (plusp days)
           ;; 2^(4n) >= 10^n, so bound digits without formatting DAYS.
           (+ 6 (1+ (floor (1- (integer-length days)) 4)))
           4))))

(defun edit-clocktable-check-time-size (document minutes total-p)
  (let* ((lower-bound
           (edit-clocktable-time-cell-length-lower-bound minutes total-p)))
    (edit-clocktable-check-character-lower-bound document lower-bound)
    (let* ((time (edit-clocktable-time-text minutes))
           (cell (if total-p (concatenate 'string "*" time "*") time))
           (characters (length cell)))
      (edit-clocktable-check-character-lower-bound document characters))))

(defun edit-clocktable-headline-cells (headline)
  (let ((position 0)
        (cells nil)
        (limit (length headline)))
    (loop
      for delimiter = (reader-next-table-delimiter headline position limit)
      for end = (or delimiter limit)
      do (multiple-value-bind (start trimmed-end)
             (reader-table-cell-bounds headline position end)
           (push (subseq headline start trimmed-end) cells))
         (if delimiter
             (setf position (1+ delimiter))
             (return)))
    (coerce (nreverse cells) 'vector)))

(defun edit-clocktable-data-cells (row headline minutes time-columns
                                   tags-p emphasize-p percent-p total-minutes)
  (let* ((headline-cells (edit-clocktable-headline-cells headline))
         (headline-count (length headline-cells))
         (headline-offset (if tags-p 1 0))
         (time-offset (+ headline-count headline-offset))
         (cells (make-array (+ headline-count headline-offset time-columns
                               (if percent-p 1 0))
                            :initial-element "")))
    (replace cells headline-cells :start1 headline-offset)
    (when tags-p
      (setf (aref cells 0)
            (let ((tags (clocktable-row-tags row)))
              (if tags (format nil "~{~A~^, ~}" tags) ""))))
    (setf (aref cells (+ time-offset
                         (1- (min time-columns (clocktable-row-level row)))))
          (edit-clocktable-emphasize minutes (clocktable-row-level row)
                                     emphasize-p))
    (when percent-p
      (setf (aref cells (1- (length cells)))
            (if (zerop total-minutes)
                "0.0"
                (format nil "~,1F"
                        (* 100.0 (/ (clocktable-row-minutes row)
                                    total-minutes))))))
    cells))

(defun edit-clocktable-time-columns (rows)
  (if (some (lambda (row) (> (clocktable-row-level row) 1)) rows) 2 1))

(defun edit-clocktable-table-columns (rows time-columns tags-p percent-p)
  (loop with columns = (+ 1 time-columns (if tags-p 1 0)
                         (if percent-p 1 0))
        for row in rows
        for headline = (edit-clocktable-headline row)
        for row-columns = (+ time-columns
                             (if tags-p 1 0)
                             (if percent-p 1 0)
                             (length (edit-clocktable-headline-cells headline)))
        do (setf columns (max columns row-columns))
        finally (return columns)))

(defun edit-clocktable-fixed-cells (column-count first second)
  (let ((cells (make-array column-count :initial-element "")))
    (setf (aref cells 0) first
          (aref cells 1) second)
    cells))

(defun edit-clocktable-write-row (stream cells newline)
  (write-char #\| stream)
  (loop for cell across cells
        do (write-char #\Space stream)
           (write-string cell stream)
           (write-char #\Space stream)
           (write-char #\| stream))
  (write-string newline stream))

(defun edit-clocktable-write-rule (stream column-count newline)
  (write-char #\| stream)
  (loop repeat column-count
        for column from 0
        do (write-string "---" stream)
           (when (< column (1- column-count))
             (write-char #\+ stream)))
  (write-char #\| stream)
  (write-string newline stream))

(defun edit-clocktable-body-text (result rows time-columns column-count newline
                                  &key tags-p emphasize-p percent-p)
  (let* ((header (if (and (not tags-p) (not percent-p))
                     (edit-clocktable-fixed-cells
                      column-count "Headline" "Time")
                     (make-array column-count :initial-element "")))
         (total (if (and (not tags-p) (not percent-p))
                    (edit-clocktable-fixed-cells
                     column-count "*Total time*"
                     (concatenate 'string "*"
                                  (edit-clocktable-time-text
                                   (clocktable-result-total-minutes result))
                                  "*"))
                    (make-array column-count :initial-element "")))
         (total-minutes (clocktable-result-total-minutes result))
         (headline-offset (if tags-p 1 0))
         (time-offset (1+ headline-offset)))
    (setf (aref header headline-offset) "Headline"
          (aref header time-offset) "Time"
          (aref total headline-offset) "*Total time*"
          (aref total time-offset)
          (concatenate 'string "*"
                       (edit-clocktable-time-text total-minutes) "*"))
    (when tags-p
      (setf (aref header 0) "Tags"))
    (when percent-p
      (setf (aref header (1- column-count)) "%"
            (aref total (1- column-count))
            (if (zerop total-minutes) "0.0" "100.0")))
    (with-output-to-string (stream)
      (edit-clocktable-write-row stream header newline)
      (edit-clocktable-write-rule stream column-count newline)
      (edit-clocktable-write-row stream total newline)
      (when rows
        (edit-clocktable-write-rule stream column-count newline)
        (dolist (row rows)
          (edit-clocktable-write-row
           stream
           (edit-clocktable-data-cells
            row (edit-clocktable-headline row emphasize-p)
            (edit-clocktable-time-text (clocktable-row-minutes row))
            time-columns tags-p emphasize-p percent-p total-minutes)
           newline))))))

(defun edit-clocktable-find-table (document)
  (let ((table nil))
    (map-nodes (lambda (node)
                 (when (and (null table) (eq (node-type node) :table))
                   (setf table node)))
               (document-root document))
    table))

(defun edit-clocktable-subtree-node-count (root)
  (let ((count 0))
    (map-nodes (lambda (node)
                 (declare (ignore node))
                 (incf count))
               root)
    count))

(defun edit-clocktable-check-node-budget (document block rows column-count)
  (let* ((row-count (length rows))
         (line-count (+ 3 row-count (if (plusp row-count) 1 0)))
         (cell-count (* column-count (+ 2 row-count)))
         (preserved-node-count
           (- (edit-clocktable-subtree-node-count (document-root document))
              (loop for child in (node-children block)
                    sum (edit-clocktable-subtree-node-count child))))
         (minimum-node-count
           (+ preserved-node-count 1 line-count cell-count)))
    (check-limit (document-limits document) :max-node-count minimum-node-count)))

(defun edit-clocktable-render-body (document result newline
                                   &key tags-p emphasize-p formula target)
  (let ((rows (%clocktable-result-rows result))
        (percent-p (and formula (string= formula "%"))))
    (edit-clocktable-check-time-size
     document (clocktable-result-total-minutes result) t)
    (dolist (row rows)
      (edit-clocktable-check-time-size document (clocktable-row-minutes row) nil)
      (edit-clocktable-check-indentation-size document row)
      (when (or (find #\Newline (clocktable-row-title row))
                (find #\Return (clocktable-row-title row)))
        (edit-signal-error (or target row) :invalid-value
                           "Clocktable headline contains a line break")))
    (let* ((time-columns (edit-clocktable-time-columns rows))
           (column-count
             (edit-clocktable-table-columns rows time-columns tags-p percent-p))
           (raw-body
             (edit-clocktable-body-text
              result rows time-columns column-count newline
              :tags-p tags-p :emphasize-p emphasize-p :percent-p percent-p))
           (raw-body
             (if (and formula (not percent-p))
                 (concatenate 'string raw-body
                              (format nil "#+TBLFM: ~A~A" formula newline))
                 raw-body))
           (temporary (parse-org raw-body :limits (document-limits document)
                                          :settings (document-settings document)))
           (table (edit-clocktable-find-table temporary))
           (aligned-document
             (if table
                 (edit-result-updated-document (table-align temporary table))
                 temporary)))
      (when (and formula (not percent-p))
        (let ((table (edit-clocktable-find-table aligned-document)))
          (when table
            (setf aligned-document
                  (handler-case
                      (edit-result-updated-document
                       (table-recalculate aligned-document table))
                    (org-table-error ()
                      (clocktable-parameter-error
                       :invalid-value :formula formula))
                    (org-table-unsupported-syntax ()
                      (clocktable-parameter-error
                       :invalid-value :formula formula)))))))
      (document-text aligned-document))))

(defun write-clocktable (document dynamic-block clocktable-result
                         &key tags-p emphasize-p formula)
  "Replace a clocktable block body with RESULT and optional formatting."
  (unless (clocktable-result-p clocktable-result)
    (error 'type-error :datum clocktable-result
                        :expected-type 'clocktable-result))
  (let ((block (edit-resolve-node document dynamic-block '(:dynamic-block))))
    (unless (equal (node-property block :block-name) "clocktable")
      (edit-signal-error dynamic-block :wrong-node-type
                         "Dynamic block is not a clocktable"))
    (let* ((rows (%clocktable-result-rows clocktable-result))
           (source (document-text document))
           (newline (dynamic-block-newline source block)))
      (multiple-value-bind (start end)
          (edit-clocktable-body-span document block dynamic-block)
        (let* ((time-columns (edit-clocktable-time-columns rows))
               (column-count
                 (edit-clocktable-table-columns
                  rows time-columns tags-p
                  (and formula (string= formula "%"))))
               (replacement
                 (edit-clocktable-render-body
                  document clocktable-result newline
                  :tags-p tags-p :emphasize-p emphasize-p :formula formula
                  :target dynamic-block))
               (expected (edit-canonical-copy
                          (edit-canonical-document document)))
               (temporary (parse-org replacement
                                     :limits (document-limits document)
                                     :settings (document-settings document))))
          (edit-clocktable-check-node-budget
           document block rows column-count)
          (edit-canonical-set-children
           expected (edit-node-path document block dynamic-block)
           (mapcar #'edit-canonical-node
                   (dynamic-block-document-children temporary)))
          (edit-apply-patches-result
           document
           (list (edit-make-patch start end replacement dynamic-block))
           :target block :expected expected))))))

(defun clocktable-parameter-error (reason parameter value)
  (error 'org-clocktable-parameter-error
         :message (ecase reason
                    (:unsupported-parameter "Unsupported clocktable parameter")
                    (:invalid-value "Invalid clocktable parameter value"))
         :reason reason
         :parameter (string-downcase
                     (if (symbolp parameter)
                         (symbol-name parameter)
                         parameter))
         :value (bounded-text
                 (if (stringp value) value (princ-to-string value)))))

(defun clocktable-parameter-value (parameters key)
  (getf parameters key))

(defun clocktable-parameter-present-p (parameters key)
  (loop for tail on parameters by #'cddr
        thereis (eq (first tail) key)))

(defun clocktable-validate-parameters (parameters)
  (let ((allowed '(:scope :maxlevel :block :tstart :tend :step :match :tags
                   :formula :fileskip0 :emphasize)))
    (loop for tail on parameters by #'cddr
          for key = (first tail)
          for value = (second tail)
          do (unless (member key allowed :test #'eq)
               (clocktable-parameter-error :unsupported-parameter key value)))
      (let ((maxlevel (clocktable-parameter-value parameters :maxlevel))
          (block (clocktable-parameter-value parameters :block))
          (step (clocktable-parameter-value parameters :step))
          (match (clocktable-parameter-value parameters :match))
          (formula (clocktable-parameter-value parameters :formula)))
      (when (and (clocktable-parameter-present-p parameters :maxlevel)
                 (not (and (integerp maxlevel) (>= maxlevel 0))))
        (clocktable-parameter-error :invalid-value :maxlevel maxlevel))
      (when (and (clocktable-parameter-present-p parameters :block)
                 (not (or (stringp block)
                          (and (integerp block) (<= 1000 block 9999)))))
        (clocktable-parameter-error :invalid-value :block block))
      (dolist (key '(:tstart :tend))
        (let ((value (clocktable-parameter-value parameters key)))
          (when (and (clocktable-parameter-present-p parameters key)
                     (not (stringp value)))
            (clocktable-parameter-error :invalid-value key value))))
      (unless (eql (clocktable-parameter-present-p parameters :tstart)
                   (clocktable-parameter-present-p parameters :tend))
        (clocktable-parameter-error
         :invalid-value :tstart
         "requires both :tstart and :tend"))
      (when (and (clocktable-parameter-present-p parameters :block)
                 (clocktable-parameter-present-p parameters :tstart))
        (clocktable-parameter-error
         :invalid-value :block
         "cannot be combined with :tstart and :tend"))
      (when (and (clocktable-parameter-present-p parameters :step)
                 (not (member step '("day" "week" "semimonth" "month"
                                     "quarter" "year")
                                 :test #'string-equal)))
        (clocktable-parameter-error :invalid-value :step step))
      (when (and step
                 (not (or block
                          (clocktable-parameter-present-p parameters :tstart))))
        (clocktable-parameter-error
         :invalid-value :step "requires :block or :tstart and :tend"))
      (when (and (clocktable-parameter-present-p parameters :match)
                 (not (stringp match)))
        (clocktable-parameter-error :invalid-value :match match))
      (when (and (clocktable-parameter-present-p parameters :formula)
                 (not (and (stringp formula) (plusp (length formula))
                           (not (find #\Newline formula))
                           (not (find #\Return formula)))))
        (clocktable-parameter-error :invalid-value :formula formula))
      (dolist (key '(:tags :fileskip0 :emphasize))
        (let ((value (clocktable-parameter-value parameters key)))
          (when (and (clocktable-parameter-present-p parameters key)
                     (not (or (null value) (eq value t)
                              (and (stringp value)
                                   (member value '("yes" "no")
                                           :test #'string-equal)))))
            (clocktable-parameter-error :invalid-value key value))))
      (let ((scope (clocktable-parameter-value parameters :scope)))
        (when (and (clocktable-parameter-present-p parameters :scope)
                   (not (or (null scope)
                            (and (stringp scope)
                                 (plusp (length scope)))
                            (and (listp scope) scope
                                 (every #'stringp scope)))))
          (clocktable-parameter-error :invalid-value :scope scope)))))
  parameters)

(defun clocktable-flag (parameters key &optional default)
  (let ((value (clocktable-parameter-value parameters key)))
    (if (clocktable-parameter-present-p parameters key)
        (if (stringp value) (string-equal value "yes") (not (null value)))
        default)))

(defun clocktable-current-date (time-context)
  (let ((value (org-time-context-reference-time time-context)))
    (cl-date-kit:make-local-date (org-time-value-year value)
                                 (org-time-value-month value)
                                 (org-time-value-day value))))

(defun clocktable-weekday-index (date)
  (mod (+ 3 (cl-date-kit:local-date-to-epoch-day date)) 7))

(defun clocktable-date-before-p (left right)
  (minusp (cl-date-kit:local-date-compare left right)))

(defun clocktable-week-start (date)
  (cl-date-kit:local-date-plus-days date
                                   (- (clocktable-weekday-index date))))

(defun clocktable-next-month-start (date amount)
  (cl-date-kit:local-date-plus-months
   (cl-date-kit:make-local-date (cl-date-kit:local-date-year date)
                                (cl-date-kit:local-date-month date) 1)
   amount))

(defun clocktable-next-year-start (date amount)
  (cl-date-kit:make-local-date (+ (cl-date-kit:local-date-year date) amount)
                               1 1))

(defun clocktable-date-value (date)
  (%time-value-from-local-value date))

(defun clocktable-date-range (start end)
  (make-time-range (clocktable-date-value start)
                   (clocktable-date-value end)))

(defun clocktable-string-digits-p (text start end)
  (and (<= 0 start end (length text))
       (loop for index from start below end
             always (digit-char-p (char text index)))))

(defun clocktable-absolute-block-range (block)
  (let ((length (length block)))
    (cond
      ((and (= length 4) (clocktable-string-digits-p block 0 4))
       (let ((year (parse-integer block)))
         (clocktable-date-range (cl-date-kit:make-local-date year 1 1)
                                (clocktable-next-year-start
                                 (cl-date-kit:make-local-date year 1 1) 1))))
      ((and (= length 7) (clocktable-string-digits-p block 0 4)
            (char= (char block 4) #\-)
            (clocktable-string-digits-p block 5 7))
       (let* ((year (parse-integer block :start 0 :end 4))
              (month (parse-integer block :start 5 :end 7))
              (start (cl-date-kit:make-local-date year month 1)))
         (clocktable-date-range start (clocktable-next-month-start start 1))))
      ((and (= length 10) (clocktable-string-digits-p block 0 4)
            (char= (char block 4) #\-)
            (char= (char block 7) #\-))
       (let* ((year (parse-integer block :start 0 :end 4))
              (month (parse-integer block :start 5 :end 7))
              (day (parse-integer block :start 8 :end 10))
              (start (cl-date-kit:make-local-date year month day)))
         (clocktable-date-range start
                                (cl-date-kit:local-date-plus-days start 1))))
      ((and (= length 8) (clocktable-string-digits-p block 0 4)
            (char= (char block 4) #\-)
            (char= (char block 5) #\W)
            (clocktable-string-digits-p block 6 8))
       (let* ((year (parse-integer block :start 0 :end 4))
              (week (parse-integer block :start 6 :end 8))
              (jan-fourth (cl-date-kit:make-local-date year 1 4))
              (start (cl-date-kit:local-date-plus-days
                      (clocktable-week-start jan-fourth) (* 7 (1- week))))
              (thursday (cl-date-kit:local-date-plus-days start 3)))
         (unless (and (<= 1 week 53)
                      (= (cl-date-kit:local-date-year thursday) year))
           (clocktable-parameter-error :invalid-value :block block))
         (clocktable-date-range start (cl-date-kit:local-date-plus-days start 7))))
      ((and (= length 7) (clocktable-string-digits-p block 0 4)
            (char= (char block 4) #\-)
            (char= (char block 5) #\Q)
            (member (char block 6) '(#\1 #\2 #\3 #\4)))
       (let* ((year (parse-integer block :start 0 :end 4))
              (quarter (digit-char-p (char block 6)))
              (start (cl-date-kit:make-local-date year
                                                  (1+ (* 3 (1- quarter))) 1)))
         (clocktable-date-range start (clocktable-next-month-start start 3))))
      (t nil))))

(defun clocktable-relative-block-range (block time-context)
  (labels ((offset-after (prefix)
             (cond
               ((string-equal block prefix) 0)
               ((and (> (length block) (length prefix))
                     (string-equal block prefix
                                   :end1 (length prefix)
                                   :end2 (length prefix))
                     (char= (char block (length prefix)) #\-)
                     (clocktable-string-digits-p
                      block (1+ (length prefix)) (length block)))
                (- (parse-integer block :start (1+ (length prefix)))))
               (t nil))))
    (let ((relative-kind
            (or (cond ((offset-after "today") :day)
                      ((offset-after "thisweek") :week)
                      ((offset-after "thismonth") :month)
                      ((offset-after "thisyear") :year))
                (cond ((string-equal block "yesterday") :day)
                      ((string-equal block "lastweek") :week)
                      ((string-equal block "lastmonth") :month)
                      ((string-equal block "lastyear") :year)
                      ((string-equal block "untilnow") :untilnow)))))
      (when (and relative-kind (null time-context))
        (clocktable-parameter-error :invalid-value :block
                                    "relative block requires :time-context"))
      (when relative-kind
        (let* ((today (clocktable-current-date time-context))
               (day-offset (offset-after "today"))
               (week-offset (offset-after "thisweek"))
               (month-offset (offset-after "thismonth"))
               (year-offset (offset-after "thisyear"))
               (base relative-kind)
               (amount
                 (cond ((string-equal block "yesterday") -1)
                       ((string-equal block "lastweek") -1)
                       ((string-equal block "lastmonth") -1)
                       ((string-equal block "lastyear") -1)
                       ((eq base :day) day-offset)
                       ((eq base :week) week-offset)
                       ((eq base :month) month-offset)
                       ((eq base :year) year-offset)
                       (t 0))))
          (case base
            (:day
             (let ((start (cl-date-kit:local-date-plus-days today amount)))
               (clocktable-date-range
                start (cl-date-kit:local-date-plus-days start 1))))
            (:week
             (let ((start (cl-date-kit:local-date-plus-weeks
                           (clocktable-week-start today) amount)))
               (clocktable-date-range
                start (cl-date-kit:local-date-plus-days start 7))))
            (:month
             (let ((start (clocktable-next-month-start today amount)))
               (clocktable-date-range start
                                      (clocktable-next-month-start start 1))))
            (:year
             (let ((start (clocktable-next-year-start today amount)))
               (clocktable-date-range start
                                      (clocktable-next-year-start start 1))))
            (:untilnow
             (let ((start (cl-date-kit:make-local-date 2003 1 1))
                   (end (org-time-context-reference-time time-context)))
               (make-time-range (clocktable-date-value start) end)))))))))

(defun clocktable-parse-org-time-value (text time-context parameter)
  (let ((reference (and time-context
                        (%time-value-local-date-time
                         (org-time-context-reference-time time-context)))))
    (let ((value (query-parse-org-date-literal text reference nil nil t)))
      (unless value
        (clocktable-parameter-error
         :invalid-value parameter
         (if (and (null reference)
                  (member (string-downcase text)
                          '("today" "tomorrow" "now" "yesterday")
                          :test #'string=))
             "relative value requires :time-context"
             text)))
      (%time-value-from-local-value value))))

(defun clocktable-range-from-parameters (parameters time-context)
  (let* ((block-value (clocktable-parameter-value parameters :block))
         (block (if (integerp block-value)
                    (write-to-string block-value)
                    block-value)))
    (cond
      ((clocktable-parameter-present-p parameters :tstart)
       (handler-case
           (make-time-range
            (clocktable-parse-org-time-value
             (clocktable-parameter-value parameters :tstart)
             time-context :tstart)
            (clocktable-parse-org-time-value
             (clocktable-parameter-value parameters :tend)
             time-context :tend))
         (org-time-error ()
           (clocktable-parameter-error
            :invalid-value :tend
            "range end is before range start"))))
      (block
       (or (handler-case (clocktable-absolute-block-range block)
             (cl-date-kit:invalid-date () nil))
           (clocktable-relative-block-range
            block time-context)
           (clocktable-parameter-error :invalid-value :block block)))
      (time-context
       (let* ((today (clocktable-current-date time-context))
              (start (cl-date-kit:make-local-date
                      (cl-date-kit:local-date-year today) 1 1)))
         (clocktable-date-range start (clocktable-next-year-start start 1))))
      (t
       (clocktable-parameter-error :invalid-value :block
                                   "default range requires :time-context")))))

(defun clocktable-context-for-range (time-context range)
  (or time-context
      (make-time-context
       :reference-time (org-time-range-start range)
       :zone (make-fixed-time-zone
              (or (org-time-value-offset-minutes
                   (org-time-range-start range))
                  0)))))

(defun clocktable-document-headings (document)
  (let ((headings nil))
    (map-nodes (lambda (node)
                 (when (and (eq (node-type node) :headline)
                            (null (query-nearest-parent-headline node)))
                   (push node headings)))
               (document-root document))
    (nreverse headings)))

(defun clocktable-nearest-heading (node)
  (loop for parent = (node-parent node) then (node-parent parent)
        while parent
        when (eq (node-type parent) :headline)
          return parent))

(defun clocktable-level-heading (document node level)
  (loop for heading = (clocktable-nearest-heading node)
        then (query-nearest-parent-headline heading)
        while heading
        when (= (query-logical-heading-level document heading) level)
          return heading))

(defun clocktable-resolver-documents (resolver selector parameter)
  (unless resolver
    (clocktable-parameter-error
     :invalid-value parameter "external scope requires :document-resolver"))
  (let ((resolved (funcall resolver selector)))
    (let ((documents (if (document-p resolved) (list resolved) resolved)))
      (unless (and (%query-proper-list-p documents)
                   documents (every #'document-p documents))
        (clocktable-parameter-error
         :invalid-value parameter "resolver must return a document or document list"))
      documents)))

(defun clocktable-file-sections (documents)
  (mapcar (lambda (entry)
            (destructuring-bind (document label) entry
              (list label document (clocktable-document-headings document) t)))
          documents))

(defun clocktable-current-scope-section (document block scope)
  (let* ((scope (if (stringp scope) (string-downcase scope) "file"))
         (heading
           (cond
             ((string= scope "subtree")
              (clocktable-nearest-heading block))
             ((string= scope "tree")
              (clocktable-level-heading document block 1))
             ((and (>= (length scope) 5)
                   (string= (subseq scope 0 4) "tree")
                   (clocktable-string-digits-p scope 4 (length scope)))
              (let ((level (parse-integer scope :start 4)))
                (when (plusp level)
                  (clocktable-level-heading document block level))))))
         (headings (if heading (list heading)
                       (if (member scope '("file" "nil") :test #'string=)
                           (clocktable-document-headings document)
                           nil))))
    (unless (or heading (member scope '("file" "nil") :test #'string=))
      (clocktable-parameter-error :invalid-value :scope scope))
    (list (list (document-source-name document) document headings nil))))

(defun clocktable-external-scope-sections (resolver selector parameter)
  (let ((documents (clocktable-resolver-documents resolver selector parameter)))
    (clocktable-file-sections
     (loop for document in documents
           collect (list document
                         (or (document-source-name document)
                             (if (stringp selector) selector
                                 (string-downcase (symbol-name selector)))))))))

(defun clocktable-scope-sections (document block parameters resolver)
  (let ((scope (clocktable-parameter-value parameters :scope)))
    (cond
      ((or (null scope)
           (and (stringp scope)
                (member scope '("file" "nil" "subtree" "tree")
                        :test #'string-equal))
           (and (stringp scope) (<= 5 (length scope))
                (string-equal (subseq scope 0 4) "tree")
                (clocktable-string-digits-p scope 4 (length scope))))
       (clocktable-current-scope-section document block scope))
      ((and (stringp scope)
            (member scope '("agenda" "agenda-with-archives"
                            "file-with-archives")
                    :test #'string-equal))
       (clocktable-external-scope-sections
        resolver
        (cond ((string-equal scope "agenda") :agenda)
              ((string-equal scope "agenda-with-archives")
               :agenda-with-archives)
              (t :file-with-archives))
        :scope))
      ((stringp scope)
       (clocktable-external-scope-sections resolver scope :scope))
      ((and (listp scope) (every #'stringp scope))
       (loop for selector in scope
             append (clocktable-external-scope-sections
                     resolver selector :scope)))
      (t (clocktable-parameter-error :invalid-value :scope scope)))))

(defun clocktable-result-for-headings
    (document headings range time-context match maxlevel)
  (let ((total 0)
        (rows nil))
    (dolist (heading headings)
      (let ((result
              (handler-case
                  (clocktable-data document heading :range range :match match
                                   :max-depth most-positive-fixnum
                                   :time-context time-context)
                (org-query-error ()
                  (clocktable-parameter-error
                   :invalid-value :match match))))
            (remaining-headings (clocktable-subtree-headings heading)))
        (incf total (clocktable-result-total-minutes result))
        (dolist (row (clocktable-result-rows result))
          (let ((matched-heading
                  (loop while remaining-headings
                        for candidate = (pop remaining-headings)
                        when (and (= (clocktable-row-level row)
                                     (query-logical-heading-level
                                      document candidate))
                                  (string= (clocktable-row-title row)
                                           (node-property candidate :raw-value ""))
                                  (equal (clocktable-row-tags row)
                                         (effective-heading-tags
                                          document candidate)))
                          return candidate)))
            (when matched-heading
              (let ((depth (loop for current = matched-heading
                                 then (query-nearest-parent-headline current)
                                 for distance from 1
                                 while current
                                 when (eq current heading)
                                   return distance)))
                (when (and depth (<= depth maxlevel))
                  (push (make-clocktable-row
                         :level depth
                         :title (clocktable-row-title row)
                         :tags (clocktable-row-tags row)
                         :range range
                         :minutes (clocktable-row-minutes row))
                        rows))))))))
    (make-clocktable-result :total-minutes total :rows (nreverse rows)
                            :range range)))

(defun clocktable-step-period-start (date step)
  (case step
    (:day date)
    (:week (clocktable-week-start date))
    (:semimonth
     (cl-date-kit:make-local-date
      (cl-date-kit:local-date-year date)
      (cl-date-kit:local-date-month date)
      (if (<= (cl-date-kit:local-date-day date) 15) 1 16)))
    (:month (clocktable-next-month-start date 0))
    (:quarter
     (cl-date-kit:make-local-date
      (cl-date-kit:local-date-year date)
      (+ 1 (* 3 (floor (1- (cl-date-kit:local-date-month date)) 3))) 1))
    (:year (clocktable-next-year-start date 0))))

(defun clocktable-step-kind (step)
  (cond ((string-equal step "day") :day)
        ((string-equal step "week") :week)
        ((string-equal step "semimonth") :semimonth)
        ((string-equal step "month") :month)
        ((string-equal step "quarter") :quarter)
        (t :year)))

(defun clocktable-step-next-period (date step)
  (case step
    (:day (cl-date-kit:local-date-plus-days date 1))
    (:week (cl-date-kit:local-date-plus-days date 7))
    (:semimonth
     (if (= (cl-date-kit:local-date-day date) 1)
         (cl-date-kit:make-local-date (cl-date-kit:local-date-year date)
                                      (cl-date-kit:local-date-month date) 16)
         (clocktable-next-month-start date 1)))
    (:month (clocktable-next-month-start date 1))
    (:quarter (clocktable-next-month-start date 3))
    (:year (clocktable-next-year-start date 1))))

(defun clocktable-value-date (value)
  (cl-date-kit:make-local-date (org-time-value-year value)
                               (org-time-value-month value)
                               (org-time-value-day value)))

(defun clocktable-step-ranges (range step-name)
  (let* ((step (clocktable-step-kind step-name))
         (range-start (org-time-range-start range))
         (range-end (org-time-range-end range))
         (start-date (clocktable-value-date range-start))
         (end-date (clocktable-value-date range-end))
         (position (clocktable-step-period-start start-date step))
         (ranges nil))
    (loop while (or (clocktable-date-before-p position end-date)
                    (and (not (clocktable-date-before-p end-date position))
                         (minusp (%time-values-compare range-start range-end))))
          for next = (clocktable-step-next-period position step)
          for period-start = (clocktable-date-value position)
          for start = (if (minusp (%time-values-compare
                                  period-start range-start))
                          range-start period-start)
          for end = (if (clocktable-date-before-p end-date next)
                        range-end (clocktable-date-value next))
          do (when (minusp (%time-values-compare start end))
               (push (make-time-range start end) ranges))
             (setf position next))
    (nreverse ranges)))

(defun clocktable-step-name (range step)
  (let* ((date (clocktable-step-period-start
                (clocktable-value-date (org-time-range-start range))
                (clocktable-step-kind step)))
         (date-label
           (format nil "[~A ~A]"
                   (cl-date-kit:format-local-date date)
                   (nth (clocktable-weekday-index date)
                        '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))))
         (label (cond ((string-equal step "day") "Daily report: ")
                      ((string-equal step "week")
                       "Weekly report starting on: ")
                      ((string-equal step "semimonth")
                       "Semimonthly report starting on: ")
                      ((string-equal step "month")
                       "Monthly report starting on: ")
                      ((string-equal step "quarter")
                       "Quarterly report starting on: ")
                      (t "Annual report starting on: "))))
    (concatenate 'string label date-label)))

(defun clocktable-render-section
    (target-document dynamic-block result newline parameters range-label file-label
     show-range-p show-file-p)
  (with-output-to-string (stream)
    (when show-file-p
      (format stream "File: ~A~A" (or file-label "") newline))
    (when show-range-p
      (format stream "~A~A" range-label newline))
    (write-string
     (edit-clocktable-render-body
      target-document result newline
      :tags-p (clocktable-flag parameters :tags)
      :emphasize-p (clocktable-flag parameters :emphasize)
      :formula (clocktable-parameter-value parameters :formula)
      :target dynamic-block)
     stream)))

(defun clocktable-dynamic-block-handler
    (document block parameters &key document-resolver time-context)
  (clocktable-validate-parameters parameters)
  (let* ((range (clocktable-range-from-parameters parameters time-context))
         (context (clocktable-context-for-range time-context range))
         (scope-sections
           (clocktable-scope-sections document block parameters document-resolver))
         (step-name (clocktable-parameter-value parameters :step))
         (ranges (if step-name (clocktable-step-ranges range step-name)
                     (list range)))
         (show-files-p (or (> (length scope-sections) 1)
                           (some (lambda (section) (fourth section))
                                 scope-sections)))
         (show-ranges-p (not (null step-name)))
         (newline (dynamic-block-newline (document-text document) block))
         (match (clocktable-parameter-value parameters :match))
         (maxlevel (or (clocktable-parameter-value parameters :maxlevel) 3))
         (fileskip0-p (clocktable-flag parameters :fileskip0)))
    (with-output-to-string (stream)
      (let ((written-p nil))
        (when show-ranges-p (terpri stream))
        (dolist (section scope-sections)
          (destructuring-bind (file-label source-document headings file-p) section
            (declare (ignore file-p))
            (dolist (subrange ranges)
              (let ((result
                      (clocktable-result-for-headings
                       source-document headings subrange context match maxlevel)))
                (unless (and show-files-p fileskip0-p
                             (zerop (clocktable-result-total-minutes result)))
                  (when written-p (terpri stream))
                  (write-string
                   (clocktable-render-section
                    document block result newline parameters
                    (and step-name (clocktable-step-name subrange step-name))
                    file-label show-ranges-p show-files-p)
                   stream)
                  (setf written-p t))))))))))

(eval-when (:load-toplevel :execute)
  (register-dynamic-block-handler "clocktable"
                                  #'clocktable-dynamic-block-handler
                                  :parameters '(:scope :maxlevel :block :tstart
                                                :tend :step :match :tags :formula
                                                :fileskip0 :emphasize)))

;;;; src/columnview.lisp
(in-package #:org-kit)

(define-condition org-column-view-error (org-kit-error)
  ((reason :initarg :reason :initform nil)
   (context :initarg :context :initform nil))
  (:report (lambda (condition stream)
             (format stream "~A~@[ (~(~A~))~]~@[ near \"~A\"~]"
                     (org-kit-error-message condition)
                     (org-column-view-error-reason condition)
                     (org-column-view-error-context condition))))
  (:documentation "Signalled when a column definition or summary value is invalid."))

(defun org-column-view-error-reason (condition)
  "Return the keyword classifying why a column view was refused."
  (slot-value condition 'reason))

(defun org-column-view-error-context (condition)
  "Return the bounded column or value context for a column view error."
  (slot-value condition 'context))

(defstruct (column-view-column (:constructor %make-column-view-column))
  property title summary precision)

(defparameter +column-view-default-format+
  "%25ITEM %TODO %3PRIORITY %TAGS")

(defparameter +column-view-special-properties+
  '("ALLTAGS" "BLOCKED" "CATEGORY" "CLOSED" "CLOCKSUM" "CLOCKSUM_T"
    "DEADLINE" "FILE" "ITEM" "PRIORITY" "SCHEDULED" "TAGS" "TODO"
    "TIMESTAMP" "TIMESTAMP_IA" "LEVEL"))

(defun column-view-error (reason context control &rest arguments)
  (error 'org-column-view-error
         :message (apply #'format nil control arguments)
         :reason reason
         :context (bounded-text context)))

(defun column-view-horizontal-space-p (character)
  (find character '(#\Space #\Tab)))

(defun column-view-skip-space (text position)
  (loop while (and (< position (length text))
                   (column-view-horizontal-space-p (char text position)))
        do (incf position)
        finally (return position)))

(defun column-view-format-summary (summary source)
  (let* ((normalized (string-upcase summary))
         (format-start (search "+;%." normalized)))
    (cond
      ((string= normalized "$")
       (values :sum 2))
      ((member normalized '("+" ":" "X" "X/" "X%" "MIN" "MAX" "MEAN"
                            ":MIN" ":MAX" ":MEAN" "@MIN" "@MAX" "@MEAN" "EST+")
               :test #'string=)
       (values (cond ((string= normalized "+") :sum)
                     ((string= normalized ":") :time-sum)
                     ((string= normalized "X") :checkbox-all)
                     ((string= normalized "X/") :checkbox-count)
                     ((string= normalized "X%") :checkbox-percent)
                     ((string= normalized "MIN") :min)
                     ((string= normalized "MAX") :max)
                     ((string= normalized "MEAN") :mean)
                     ((string= normalized ":MIN") :time-min)
                     ((string= normalized ":MAX") :time-max)
                     ((string= normalized ":MEAN") :time-mean)
                     ((string= normalized "@MIN") :age-min)
                     ((string= normalized "@MAX") :age-max)
                     ((string= normalized "@MEAN") :age-mean)
                     (t :estimate-sum))
               nil))
      ((and format-start (zerop format-start)
            (char= (char normalized (1- (length normalized))) #\F))
       (let* ((precision-start (+ format-start 4))
              (precision-end (1- (length normalized)))
              (precision-text (subseq normalized precision-start precision-end)))
         (unless (and (plusp (length precision-text))
                      (every #'digit-char-p precision-text)
                      (char= (char normalized (1- (length normalized))) #\F))
           (column-view-error :invalid-summary-type source
                              "Invalid column summary type ~S" summary))
         (let ((precision (parse-integer precision-text)))
           (unless (<= precision 12)
             (column-view-error :invalid-summary-type source
                                "Column summary precision is too large"))
           (values :sum precision))))
      (t
       (column-view-error :invalid-summary-type source
                          "Unknown column summary type ~S" summary)))))

(defun column-view-parse-format (format)
  (let ((position 0)
        (length (length format))
        (columns nil))
    (loop
      (setf position (column-view-skip-space format position))
      (when (= position length)
        (return))
      (unless (char= (char format position) #\%)
        (column-view-error :invalid-columns format
                           "Column definition must start with percent"))
      (incf position)
      (let* ((width-start position)
             (property-start nil)
             (property-end nil)
             (title nil)
             (summary nil)
             (precision nil))
        (loop while (and (< position length)
                         (digit-char-p (char format position)))
              do (incf position))
        (when (< width-start position)
          (unless (plusp (parse-integer format :start width-start :end position))
            (column-view-error :invalid-column-width format
                               "Column width must be positive")))
        (setf property-start position)
        (loop while (and (< position length)
                         (not (column-view-horizontal-space-p (char format position)))
                         (not (find (char format position) '(#\( #\{ #\) #\}))))
              do (incf position))
        (setf property-end position)
        (when (= property-start property-end)
          (column-view-error :invalid-columns format
                             "Column property name is empty"))
        (let ((property (subseq format property-start property-end)))
          (when (and (< position length) (char= (char format position) #\())
            (let ((title-start (1+ position))
                  (title-end (position #\) format :start (1+ position))))
              (unless title-end
                (column-view-error :invalid-columns format
                                   "Column title has no closing parenthesis"))
              (setf title (subseq format title-start title-end)
                    position (1+ title-end))))
          (when (and (< position length) (char= (char format position) #\{))
            (let* ((summary-start (1+ position))
                   (summary-end (position #\} format :start summary-start)))
              (unless summary-end
                (column-view-error :invalid-columns format
                                   "Column summary has no closing brace"))
              (setf summary (subseq format summary-start summary-end)
                    position (1+ summary-end))
              (multiple-value-setq (summary precision)
                (column-view-format-summary summary format))))
          (push (%make-column-view-column
                 :property property :title (or title property)
                 :summary summary :precision precision)
                columns))))
    (unless columns
      (column-view-error :invalid-columns format
                         "Column definition contains no columns"))
    (nreverse columns)))

(defun column-view-global-property-format (document)
  (let ((format nil)
        (found-p nil))
    (map-nodes
     (lambda (node)
       (when (null (query-nearest-headline node))
         (cond
           ((eq (node-type node) :property-drawer)
            (dolist (property (node-children node))
              (when (and (eq (node-type property) :node-property)
                         (string-equal (node-property property :key "")
                                       "COLUMNS"))
                (setf format (node-property property :value)
                      found-p t))))
           ((and (eq (node-type node) :drawer)
                 (string-equal (node-property node :drawer-name "")
                               "PROPERTIES"))
            (dolist (paragraph (node-children node))
              (when (eq (node-type paragraph) :paragraph)
                (let ((source (node-text paragraph)))
                  (when source
                    (loop with start = 0
                          for newline = (position #\Newline source :start start)
                          for end = (or newline (length source))
                          for line = (subseq source start end)
                          do (multiple-value-bind (key value)
                                 (reader-blocks-node-property-fields line)
                               (when (and key (string-equal key "COLUMNS"))
                                 (setf format value
                                       found-p t)))
                          when (null newline) return nil
                          do (setf start (1+ newline)))))))))))
     (document-root document))
    (values format found-p)))

(defun column-view-global-keyword-format (document)
  (let ((format nil)
        (found-p nil))
    (map-nodes
     (lambda (node)
       (when (and (eq (node-type node) :keyword)
                  (null (query-nearest-headline node))
                  (string-equal (node-property node :key "") "COLUMNS"))
         (setf format (node-property node :value)
               found-p t)))
     (document-root document))
    (values format found-p)))

(defun column-view-local-property-format (heading)
  (loop for current = heading then (query-nearest-parent-headline current)
        while current
        do (multiple-value-bind (value found-p)
               (effective-heading-property-value current "COLUMNS")
             (when found-p
               (return-from column-view-local-property-format
                 (values value t))))
        finally (return (values nil nil))))

(defun column-view-format-for (document heading)
  (multiple-value-bind (local-format local-found-p)
      (if heading
          (column-view-local-property-format heading)
          (values nil nil))
    (when local-found-p
      (return-from column-view-format-for local-format)))
  (multiple-value-bind (property-format property-found-p)
      (column-view-global-property-format document)
    (when property-found-p
      (return-from column-view-format-for property-format)))
  (multiple-value-bind (keyword-format keyword-found-p)
      (column-view-global-keyword-format document)
    (if keyword-found-p keyword-format +column-view-default-format+)))

(defun column-view-headings (root)
  (let ((headings nil))
    (map-nodes (lambda (node)
                 (when (eq (node-type node) :headline)
                   (push node headings)))
               root)
    (nreverse headings)))

(defun column-view-special-property-p (property)
  (member (string-upcase property) +column-view-special-properties+
          :test #'string=))

(defun column-view-tags-string (tags)
  (if tags
      (with-output-to-string (stream)
        (dolist (tag tags)
          (write-char #\: stream)
          (write-string tag stream))
        (write-char #\: stream))
      ""))

(defun column-view-category (document heading)
  (multiple-value-bind (property source)
      (effective-property document heading "CATEGORY" :inherit t)
    (declare (ignore source))
    (or property
        (getf (document-settings document) :category)
        (let ((source-name (document-source-name document)))
          (when source-name
            (or (pathname-name (pathname source-name))
                (file-namestring (pathname source-name)))))
        "")))

(defun column-view-timestamp-property (heading key)
  (let ((timestamp (node-property heading key)))
    (and (node-p timestamp)
         (string-trim '(#\Space #\Tab) (node-text timestamp)))))

(defun column-view-own-value (document heading column)
  (let* ((property (column-view-column-property column))
         (name (string-upcase property)))
    (cond
      ((string= name "ITEM") (or (node-property heading :raw-value) ""))
      ((string= name "TODO") (or (node-property heading :todo-keyword) ""))
      ((string= name "PRIORITY")
       (let ((priority (node-property heading :priority)))
         (cond ((characterp priority) (string priority))
               ((and (integerp priority) (<= 0 priority #x10ffff))
                (let ((character (code-char priority)))
                  (if character (string character) "")))
               ((stringp priority) priority)
               (t ""))))
      ((string= name "TAGS")
       (column-view-tags-string (node-property heading :tags)))
      ((string= name "ALLTAGS")
       (column-view-tags-string (effective-heading-tags document heading)))
      ((string= name "CATEGORY") (column-view-category document heading))
      ((string= name "FILE") (or (document-source-name document) ""))
      ((string= name "SCHEDULED")
       (or (column-view-timestamp-property heading :scheduled) ""))
      ((string= name "DEADLINE")
       (or (column-view-timestamp-property heading :deadline) ""))
      ((string= name "CLOSED")
       (or (column-view-timestamp-property heading :closed) ""))
      ((string= name "TIMESTAMP")
       (let ((timestamp (query-entry-timestamp heading :active)))
         (if timestamp
             (string-trim '(#\Space #\Tab) (node-text timestamp))
             "")))
      ((string= name "TIMESTAMP_IA")
       (let ((timestamp (query-entry-timestamp heading :inactive)))
         (if timestamp
             (string-trim '(#\Space #\Tab) (node-text timestamp))
             "")))
      ((string= name "LEVEL")
       (let ((level (query-logical-heading-level document heading)))
         (if level (write-to-string level) "")))
      ((member name '("BLOCKED" "CLOCKSUM" "CLOCKSUM_T") :test #'string=) "")
      (t
       (multiple-value-bind (value found-p)
           (effective-heading-property-value heading property)
         (if found-p
             (column-view-cell-string value)
             ""))))))

(defun column-view-cell-string (value)
  (cond ((null value) "")
        ((stringp value) (copy-seq value))
        (t (princ-to-string value))))

(defun column-view-invalid-summary-value (column value)
  (column-view-error :invalid-summary-value
                     (format nil "~A=~S" (column-view-column-property column) value)
                     "Invalid value for column summary ~A"
                     (column-view-column-property column)))

(defun column-view-parse-number (column value)
  (let* ((text (string-trim '(#\Space #\Tab #\Return #\Newline) value))
         (length (length text))
         (position 0)
         (sign 1)
         (dot nil)
         (digits-before "")
         (digits-after ""))
    (when (zerop length)
      (column-view-invalid-summary-value column value))
    (when (find (char text position) '(#\+ #\-))
      (when (char= (char text position) #\-)
        (setf sign -1))
      (incf position))
    (setf dot (position #\. text :start position))
    (setf digits-before (subseq text position (or dot length))
          digits-after (if dot (subseq text (1+ dot)) ""))
    (unless (and (or (plusp (length digits-before))
                     (plusp (length digits-after)))
                 (every #'digit-char-p digits-before)
                 (every #'digit-char-p digits-after))
      (column-view-invalid-summary-value column value))
    (* sign
       (/ (+ (if (plusp (length digits-before))
                 (parse-integer digits-before) 0)
             (if (plusp (length digits-after))
                 (/ (parse-integer digits-after)
                    (expt 10 (length digits-after)))
                 0))
          1))))

(defun column-view-read-number (text position)
  (let ((start position)
        (dot-seen-p nil)
        (digit-seen-p nil))
    (when (and (< position (length text))
               (find (char text position) '(#\+ #\-)))
      (incf position))
    (loop while (< position (length text))
          for character = (char text position)
          do (cond ((digit-char-p character)
                    (setf digit-seen-p t)
                    (incf position))
                   ((and (char= character #\.) (not dot-seen-p))
                    (setf dot-seen-p t)
                    (incf position))
                   (t (return))))
    (if digit-seen-p
        (values (subseq text start position)
                (column-view-parse-number-value (subseq text start position))
                position)
        (values nil nil start))))

(defun column-view-parse-number-value (text)
  (let* ((length (length text))
         (position 0)
         (sign 1)
         (dot nil)
         (digits-before "")
         (digits-after ""))
    (when (find (char text position) '(#\+ #\-))
      (when (char= (char text position) #\-)
        (setf sign -1))
      (incf position))
    (setf dot (position #\. text :start position)
          digits-before (subseq text position (or dot length))
          digits-after (if dot (subseq text (1+ dot)) ""))
    (* sign
       (+ (if (plusp (length digits-before))
              (parse-integer digits-before) 0)
          (if (plusp (length digits-after))
              (/ (parse-integer digits-after) (expt 10 (length digits-after)))
              0)))))

(defun column-view-duration-unit-factor (unit)
  (let ((unit (string-downcase unit)))
    (cond ((member unit '("min" "mins" "minute" "minutes")
                   :test #'string=) 1)
          ((member unit '("s" "sec" "secs" "second" "seconds")
                   :test #'string=) (/ 1 60))
          ((member unit '("h" "hr" "hrs" "hour" "hours") :test #'string=) 60)
          ((member unit '("d" "day" "days") :test #'string=) 1440)
          ((member unit '("w" "week" "weeks") :test #'string=) 10080)
          ((string= unit "m") 43200)
          ((string= unit "y") 525960)
          (t nil))))

(defun column-view-parse-duration (column value)
  (let* ((text (string-trim '(#\Space #\Tab #\Return #\Newline) value))
         (colon (position #\: text)))
    (when (zerop (length text))
      (column-view-invalid-summary-value column value))
    (if colon
        (multiple-value-bind (minutes valid-p) (effort-parse-duration text)
          (if valid-p minutes
              (column-view-invalid-summary-value column value)))
        (multiple-value-bind (first-token number next)
            (column-view-read-number text 0)
          (unless first-token
            (column-view-invalid-summary-value column value))
          (let ((position next)
                (negative-p (minusp number))
                (total 0)
                (token-count 0))
            (loop
              (incf token-count)
              (setf position (column-view-skip-space text position))
              (let ((unit-start position))
                (loop while (and (< position (length text))
                                 (alpha-char-p (char text position)))
                      do (incf position))
                (if (= unit-start position)
                    (if (and (= token-count 1) (= position (length text)))
                        (return-from column-view-parse-duration number)
                        (column-view-invalid-summary-value column value))
                    (let* ((unit (subseq text unit-start position))
                           (factor (column-view-duration-unit-factor unit)))
                      (unless factor
                        (column-view-invalid-summary-value column value))
                      (incf total (* (abs number) factor))))
                (setf position (column-view-skip-space text position))
                (when (= position (length text))
                  (return))
                (when (find (char text position) '(#\+ #\-))
                  (column-view-invalid-summary-value column value))
                (multiple-value-bind (token token-value next-position)
                    (column-view-read-number text position)
                  (unless token
                    (column-view-invalid-summary-value column value))
                  (setf number token-value
                        position next-position))))
            (if negative-p (- total) total))))))

(defun column-view-format-number (number &optional precision)
  (if precision
      (format nil "~,VF" precision (float number 1d0))
      (if (integerp number)
          (write-to-string number)
          (let* ((text (format nil "~,6F" (float number 1d0)))
                 (end (length text)))
            (loop while (char= (char text (1- end)) #\0)
                  do (decf end))
            (when (char= (char text (1- end)) #\.)
              (decf end))
            (subseq text 0 end)))))

(defun column-view-format-clock (minutes values)
  (let* ((whole-seconds (round (* minutes 60)))
         (negative-p (minusp whole-seconds))
         (absolute (abs whole-seconds))
         (days (floor absolute 86400))
         (remaining-seconds (mod absolute 86400))
         (hours (floor remaining-seconds 3600))
         (remaining-minutes (floor (mod remaining-seconds 3600) 60))
         (seconds (mod remaining-seconds 60))
         (has-units-p (some (lambda (value) (position-if #'alpha-char-p value))
                            values))
         (has-seconds-p
           (some (lambda (value)
                   (let ((first-colon (position #\: value)))
                     (and first-colon
                          (position #\: value :start (1+ first-colon)))))
                 values)))
    (with-output-to-string (stream)
      (when negative-p (write-char #\- stream))
      (when (and has-units-p (plusp days))
        (format stream "~Dd " days))
      (if (and (not has-units-p) has-seconds-p)
          (format stream "~D:~2,'0D:~2,'0D"
                  (+ (* days 24) hours) remaining-minutes seconds)
          (format stream "~D:~2,'0D"
                  (+ (* (if has-units-p 0 days) 24) hours)
                  remaining-minutes)))))

(defun column-view-format-age (minutes)
  (let* ((whole-minutes (floor (abs minutes)))
         (negative-p (minusp minutes))
         (days (floor whole-minutes 1440))
         (hours (floor (mod whole-minutes 1440) 60))
         (remaining-minutes (mod whole-minutes 60))
         (parts nil))
    (when (plusp days)
      (push (format nil "~Dd" days) parts))
    (when (plusp hours)
      (push (format nil "~Dh" hours) parts))
    (when (plusp remaining-minutes)
      (push (format nil "~Dmin" remaining-minutes) parts))
    (when (null parts)
      (push "0min" parts))
    (format nil "~:[~;-~]~{~A~^ ~}" negative-p (nreverse parts))))

(defun column-view-summary-numbers (column values operator precision)
  (let ((numbers (mapcar (lambda (value) (column-view-parse-number column value))
                         values)))
    (if (null numbers)
        ""
        (let ((result (ecase operator
                        (:sum (reduce #'+ numbers))
                        (:min (reduce #'min numbers))
                        (:max (reduce #'max numbers))
                        (:mean (/ (reduce #'+ numbers) (length numbers))))))
          (column-view-format-number result precision)))))

(defun column-view-checkbox-complete-p (column value summary)
  (let* ((text (string-trim '(#\Space #\Tab) value))
         (count (length text)))
    (cond
      ((member text '("[X]" "[x]") :test #'string=) t)
      ((member text '("[ ]" "[-]" "[100%]") :test #'string-equal)
       (and (eq summary :checkbox-percent)
            (string-equal text "[100%]")))
      ((and (>= count 5)
            (char= (char text 0) #\[)
            (char= (char text (1- count)) #\])
            (let ((slash (position #\/ text)))
              (and slash
                   (> slash 1)
                   (every #'digit-char-p (subseq text 1 slash))
                   (every #'digit-char-p (subseq text (1+ slash) (1- count))))))
       (let* ((slash (position #\/ text))
              (done (parse-integer text :start 1 :end slash))
              (total (parse-integer text :start (1+ slash) :end (1- count))))
         (unless (<= done total)
           (column-view-invalid-summary-value column value))
         (and (eq summary :checkbox-count)
              (plusp total)
              (= done total))))
      ((and (>= count 4)
            (char= (char text 0) #\[)
            (char= (char text (1- count)) #\])
            (char= (char text (- count 2)) #\%)
            (every #'digit-char-p (subseq text 1 (- count 2))))
       (let ((percent (parse-integer text :start 1 :end (- count 2))))
         (unless (<= 0 percent 100)
           (column-view-invalid-summary-value column value))
         (and (eq summary :checkbox-percent) (= percent 100))))
      (t (column-view-invalid-summary-value column value)))))

(defun column-view-summary-checkbox (column values summary)
  (let* ((total (length values))
         (done (count-if (lambda (value)
                           (column-view-checkbox-complete-p
                            column value summary))
                         values)))
    (if (zerop total)
        ""
        (case summary
          (:checkbox-all (cond ((= done total) "[X]")
                               ((plusp done) "[-]")
                               (t "[ ]")))
          (:checkbox-count (format nil "[~D/~D]" done total))
          (:checkbox-percent
           (format nil "[~D%]" (round (* 100 done) total)))))))

(defun column-view-age-date-literal-p (value)
  (let ((body (query-date-literal-body value)))
    (or (query-parse-org-date-literal body nil nil nil t)
        (member body '("today" "tomorrow" "now") :test #'string-equal)
        (and (>= (length body) 3)
             (find (char body 0) '(#\+ #\-))
             (find (char body (1- (length body))) '(#\d #\w #\m #\y))
             (every #'digit-char-p (subseq body 1 (1- (length body))))))))

(defun column-view-age-minutes (column value time-context)
  (if (column-view-age-date-literal-p value)
      (progn
        (unless time-context
          (column-view-error :time-context-required value
                             "Timestamp age summaries require a time context"))
        (let* ((zone (org-time-context-zone time-context))
               (reference-time
                 (cl-date-kit:local-date-time-of-instant
                  (org-time-context-reference-instant time-context)
                  (org-time-zone-backend zone)))
               (date-value
                 (query-parse-org-date-literal value reference-time nil nil t)))
          (unless date-value
            (column-view-invalid-summary-value column value))
          (/ (- (cl-date-kit:instant-epoch-second
                 (org-time-context-reference-instant time-context))
                (cl-date-kit:instant-epoch-second
                 (%time-value-to-instant
                  (%time-value-from-local-value date-value)
                  zone (org-time-context-dst-policy time-context))))
             60)))
      (column-view-parse-duration column value)))

(defun column-view-summary-times (column values operator time-context)
  (let ((minutes (mapcar
                  (lambda (value)
                    (if (member operator '(:age-min :age-max :age-mean)
                                :test #'eq)
                        (column-view-age-minutes column value time-context)
                        (column-view-parse-duration column value)))
                  values)))
    (if (null minutes)
        ""
        (let ((result (ecase operator
                        (:time-sum (reduce #'+ minutes))
                        (:time-min (reduce #'min minutes))
                        (:time-max (reduce #'max minutes))
                        (:time-mean (/ (reduce #'+ minutes) (length minutes)))
                        (:age-min (reduce #'min minutes))
                        (:age-max (reduce #'max minutes))
                        (:age-mean (/ (reduce #'+ minutes) (length minutes))))))
          (if (member operator '(:age-min :age-max :age-mean) :test #'eq)
              (column-view-format-age result)
              (column-view-format-clock result values))))))

(defun column-view-estimate-endpoint (column text)
  (if (or (position-if #'alpha-char-p text) (position #\: text))
      (values :duration (column-view-parse-duration column text))
      (values :number (column-view-parse-number column text))))

(defun column-view-parse-estimate (column value)
  (let* ((separator (position #\- value :start 1))
         (low-text (string-trim '(#\Space #\Tab)
                                (subseq value 0 separator)))
         (high-text (if separator
                        (string-trim '(#\Space #\Tab)
                                     (subseq value (1+ separator)))
                        low-text)))
    (multiple-value-bind (low-kind low)
        (column-view-estimate-endpoint column low-text)
      (multiple-value-bind (high-kind high)
          (column-view-estimate-endpoint column high-text)
        (unless (and (eq low-kind high-kind) (<= low high))
          (column-view-invalid-summary-value column value))
        (values low-kind low high)))))

(defun column-view-summary-estimates (column values)
  (let ((kind nil)
        (mean-sum 0)
        (variance-sum 0)
        (count 0))
    (dolist (value values)
      (multiple-value-bind (value-kind low high)
          (column-view-parse-estimate column value)
        (when (and kind (not (eq kind value-kind)))
          (column-view-invalid-summary-value column value))
        (setf kind value-kind)
        (incf count)
        (incf mean-sum (/ (+ low high) 2))
        (let ((mean (/ (+ low high) 2)))
          (incf variance-sum
                (- (/ (+ (expt low 2) (expt high 2)) 2)
                   (expt mean 2))))))
    (if (zerop count)
        ""
        (let* ((mean mean-sum)
               (margin (sqrt (float variance-sum 1d0)))
               (low (- mean margin))
               (high (+ mean margin)))
          (if (eq kind :duration)
              (format nil "~A-~A" (column-view-format-age low)
                      (column-view-format-age high))
              (format nil "~A-~A" (round low) (round high)))))))

(defun column-view-empty-value-p (value)
  (zerop (length (string-trim '(#\Space #\Tab #\Return #\Newline) value))))

(defun column-view-summary-value (column values &optional time-context)
  (let ((summary (column-view-column-summary column))
        (values (remove-if #'column-view-empty-value-p values)))
    (cond
      ((member summary '(:sum :min :max :mean) :test #'eq)
       (column-view-summary-numbers column values summary
                                    (column-view-column-precision column)))
      ((member summary '(:checkbox-all :checkbox-count :checkbox-percent)
               :test #'eq)
       (column-view-summary-checkbox column values summary))
      ((member summary '(:time-sum :time-min :time-max :time-mean
                         :age-min :age-max :age-mean)
               :test #'eq)
       (column-view-summary-times column values summary time-context))
      ((eq summary :estimate-sum)
       (column-view-summary-estimates column values))
      (t ""))))

(defun column-view-heading-rows (document headings columns time-context)
  (let ((members (make-hash-table :test #'eq))
        (children (make-hash-table :test #'eq))
        (values-by-heading (make-hash-table :test #'eq)))
    (dolist (heading headings)
      (setf (gethash heading members) t))
    (dolist (heading headings)
      (let ((parent (query-nearest-parent-headline heading)))
        (when (and parent (gethash parent members))
          (push heading (gethash parent children)))))
    (dolist (heading (reverse headings))
      (let* ((child-headings (nreverse (gethash heading children)))
             (row (make-array (length columns))))
        (loop for column in columns
              for index from 0
              for summary = (column-view-column-summary column)
              for value =
                (if (and summary
                         (not (column-view-special-property-p
                               (column-view-column-property column)))
                         child-headings)
                    (column-view-summary-value
                     column
                     (mapcar (lambda (child)
                               (aref (gethash child values-by-heading) index))
                             child-headings)
                     time-context)
                    (column-view-own-value document heading column))
              do (setf (aref row index) (column-view-cell-string value)))
        (setf (gethash heading values-by-heading) row)))
    (mapcar (lambda (heading)
              (coerce (gethash heading values-by-heading) 'list))
            headings)))

(defun column-view-check-output-length (rows limits)
  (check-limit
   limits :max-output-length
   (loop for row in rows
         sum (loop for cell in row sum (length cell)))))

(defun column-view-data (document &key heading time-context)
  "Return column-view data as string rows, with a header row first.

HEADING selects a headline and its subtree. Its nearest COLUMNS property
overrides the file-level COLUMNS property or #+COLUMNS keyword. Without HEADING,
the file-level definition or Org's default format is used. TIME-CONTEXT supplies
the reference time for age summaries over timestamp values."
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document))
  (unless (document-root document)
    (column-view-error :document-not-parsed nil "Document has not been parsed"))
  (unless (or (null time-context) (org-time-context-p time-context))
    (error 'type-error :datum time-context :expected-type 'org-time-context))
  (when heading
    (unless (and (node-p heading)
                 (eq (node-type heading) :headline)
                 (eq (node-document heading) document))
      (column-view-error :invalid-heading nil
                         "Column view scope must be a headline in the document")))
  (let* ((format (column-view-format-for document heading))
         (columns (column-view-parse-format format))
         (root (or heading (document-root document)))
         (headings (column-view-headings root)))
    (check-limit (document-limits document) :max-node-count (length headings))
    (let* ((header (mapcar (lambda (column)
                             (column-view-cell-string
                              (column-view-column-title column)))
                           columns))
           (rows (cons header
                       (column-view-heading-rows document headings columns
                                                 time-context))))
      (column-view-check-output-length rows (document-limits document))
      rows)))

(defun column-view-dynamic-block-handler
    (document block parameters &key document-resolver time-context)
  "Return the column-view rows for the heading containing BLOCK."
  (declare (ignore parameters document-resolver))
  (column-view-data document :heading (query-nearest-headline block)
                    :time-context time-context))

(eval-when (:load-toplevel :execute)
  (register-dynamic-block-handler "columnview"
                                  #'column-view-dynamic-block-handler))

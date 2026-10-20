;;;; src/object-timestamp.lisp
;;;;
;;;; Recognition for Org timestamps, without resolving relative dates or
;;;; evaluating diary expressions.
(in-package #:org-kit)

(defstruct
    (timestamp-fields (:conc-name %timestamp-fields-)
                      (:constructor %make-timestamp-fields
                                    (&key type
                                          year
                                          month
                                          day
                                          hour
                                          minute
                                          end-hour
                                          end-minute
                                          time-range-p
                                          repeater-type
                                          repeater-value
                                          repeater-unit
                                          repeater-window-value
                                          repeater-window-unit
                                          warning-type
                                          warning-value
                                          warning-unit)))
  type
  year
  month
  day
  hour
  minute
  end-hour
  end-minute
  time-range-p
  repeater-type
  repeater-value
  repeater-unit
  repeater-window-value
  repeater-window-unit
  warning-type
  warning-value
  warning-unit)

(defun %timestamp-digit-p (character)
  (and (char>= character #\0) (char<= character #\9)))

(defun %timestamp-digits-p (text start end)
  (loop for position from start below
        end
        always (%timestamp-digit-p (char text position))))

(defun %timestamp-number (text start end)
  (parse-integer text :start start :end end))

(defun %timestamp-date-at (text start end)
  (when
      (and (<= (+ start 10) end)
           (%timestamp-digits-p text start (+ start 4))
           (char= (char text (+ start 4)) #\-)
           (%timestamp-digits-p text (+ start 5) (+ start 7))
           (char= (char text (+ start 7)) #\-)
           (%timestamp-digits-p text (+ start 8) (+ start 10)))
    (values (%timestamp-number text start (+ start 4))
            (%timestamp-number text (+ start 5) (+ start 7))
            (%timestamp-number text (+ start 8) (+ start 10))
            (+ start 10))))

(defun %timestamp-time-at (text start end)
  (when
      (and (<= (+ start 5) end)
           (%timestamp-digits-p text start (+ start 2))
           (char= (char text (+ start 2)) #\:)
           (%timestamp-digits-p text (+ start 3) (+ start 5)))
    (values (%timestamp-number text start (+ start 2))
            (%timestamp-number text (+ start 3) (+ start 5))
            (+ start 5))))

(defun %timestamp-time-token (text start end)
  (multiple-value-bind (hour minute after-start)
      (%timestamp-time-at text start end)
    (when after-start
      (if (and (< after-start end) (char= (char text after-start) #\-))
          (multiple-value-bind (end-hour end-minute after-end)
              (%timestamp-time-at text (1+ after-start) end)
            (when (and after-end (= after-end end))
              (values hour minute end-hour end-minute t t)))
          (when (= after-start end)
            (values hour minute nil nil nil t))))))

(defun %timestamp-unit (character)
  (case character
    (#\h :hour)
    (#\d :day)
    (#\w :week)
    (#\m :month)
    (#\y :year)))

(defun %timestamp-adjustment-token (text start end)
  (let* ((first (char text start))
         (second (and (< (1+ start) end) (char text (1+ start)))))
    (multiple-value-bind (adjustment-type mode prefix-length)
        (cond
          ((and (char= first #\+) (eql second #\+))
           (values :repeater :catch-up 2))
          ((and (char= first #\.) (eql second #\+))
           (values :repeater :restart 2))
          ((char= first #\+) (values :repeater :cumulate 1))
          ((and (char= first #\-) (eql second #\-)) (values :warning :first 2))
          ((char= first #\-) (values :warning :all 1)))
      (when adjustment-type
        (let* ((digits-start (+ start prefix-length))
               (digits-end
                 (loop for position from digits-start below end
                       while (%timestamp-digit-p (char text position))
                       finally (return position)))
               (value (and (> digits-end digits-start)
                           (%timestamp-number text digits-start digits-end))))
          (when (and value (< digits-end end))
            (let* ((unit (%timestamp-unit (char text digits-end)))
                   (unit-end (1+ digits-end)))
              (when unit
                (if (= unit-end end)
                    (values adjustment-type mode value unit nil nil t)
                    (when (and (eq adjustment-type :repeater)
                               (char= (char text unit-end) #\/))
                      (let* ((window-start (1+ unit-end))
                             (window-digits-end
                               (loop for position from window-start below end
                                     while (%timestamp-digit-p (char text position))
                                     finally (return position)))
                             (window-unit
                               (and (< window-digits-end end)
                                    (%timestamp-unit
                                     (char text window-digits-end)))))
                        (when (and (> window-digits-end window-start)
                                   (= (1+ window-digits-end) end)
                                   (member window-unit
                                           '(:day :week :month :year)
                                           :test #'eq))
                          (values adjustment-type mode value unit
                                  (%timestamp-number text window-start
                                                     window-digits-end)
                                  window-unit t)))))))))))))

(defun %timestamp-whitespace-p (character)
  (member character '(#\Space #\Tab)))

(defun %timestamp-skip-whitespace (text position end)
  (loop while (and (< position end)
                   (%timestamp-whitespace-p (char text position)))
        do (incf position)
        finally (return position)))

(defun %timestamp-token-end (text position end)
  (loop while (and (< position end)
                   (not (%timestamp-whitespace-p (char text position))))
        do (incf position)
        finally (return position)))

(defun %timestamp-parse-suffix (text start close-position fields)
  (let ((cursor start)
        (weekday-seen-p nil)
        (time-seen-p nil))
    (loop
      (let ((before-whitespace cursor))
        (setf cursor (%timestamp-skip-whitespace text cursor close-position))
        (when (= cursor close-position)
          (return-from %timestamp-parse-suffix
            (values fields t)))
        (unless (> cursor before-whitespace)
          (return-from %timestamp-parse-suffix
            (values nil nil))))
      (let* ((token-start cursor)
             (token-end
               (%timestamp-token-end text cursor close-position)))
        (multiple-value-bind (hour minute end-hour end-minute range-p valid-p)
            (%timestamp-time-token text token-start token-end)
          (cond
            (valid-p
             (when time-seen-p
               (return-from %timestamp-parse-suffix
                 (values nil nil)))
             (setf time-seen-p t
                   (%timestamp-fields-hour fields) hour
                   (%timestamp-fields-minute fields) minute
                   (%timestamp-fields-end-hour fields) (or end-hour hour)
                   (%timestamp-fields-end-minute fields) (or end-minute minute)
                   (%timestamp-fields-time-range-p fields) range-p))
            ((or (char= (char text token-start) #\+)
                 (char= (char text token-start) #\-)
                 (char= (char text token-start) #\.))
             (multiple-value-bind (kind mode value unit
                                   window-value window-unit
                                   valid-adjustment-p)
                 (%timestamp-adjustment-token text token-start token-end)
               (unless valid-adjustment-p
                 (return-from %timestamp-parse-suffix
                   (values nil nil)))
               (case kind
                 (:repeater
                  (when (%timestamp-fields-repeater-type fields)
                    (return-from %timestamp-parse-suffix
                      (values nil nil)))
                  (setf (%timestamp-fields-repeater-type fields) mode
                        (%timestamp-fields-repeater-value fields) value
                        (%timestamp-fields-repeater-unit fields) unit
                        (%timestamp-fields-repeater-window-value fields)
                        window-value
                        (%timestamp-fields-repeater-window-unit fields)
                        window-unit))
                 (:warning
                  (when (%timestamp-fields-warning-type fields)
                    (return-from %timestamp-parse-suffix
                      (values nil nil)))
                  (setf (%timestamp-fields-warning-type fields) mode
                        (%timestamp-fields-warning-value fields) value
                        (%timestamp-fields-warning-unit fields) unit)))))
            (t
             (when weekday-seen-p
               (return-from %timestamp-parse-suffix
                 (values nil nil)))
             (setf weekday-seen-p t)))
          (setf cursor token-end))))))

(defun %timestamp-diary-character-token-boundary-p (text index)
  (or (zerop index)
      (member (char text (1- index))
              '(#\Space #\Tab #\Newline #\Return
                #\( #\) #\" #\' #\` #\, #\;))))

(defun %timestamp-diary-character-literal-end (text start end)
  (when (and (< start end)
             (char= (char text start) #\?)
             (%timestamp-diary-character-token-boundary-p text start)
             (< (1+ start) end))
    (let ((index (1+ start)))
      (if (char= (char text index) #\\)
          (progn
            (incf index)
            (loop while (and (< index end)
                             (member (char text index)
                                     (quote (#\C #\M #\S #\H #\A #\s #\h)))
                             (< (1+ index) end)
                             (char= (char text (1+ index)) #\-))
                  do (incf index 2)
                     (when (and (< index end)
                                (char= (char text index) #\\))
                       (incf index)))
            (cond
              ((and (< (1+ index) end)
                    (char= (char text index) #\^))
               (min end (+ index 2)))
              ((and (< (1+ index) end)
                    (char= (char text index) #\N)
                    (char= (char text (1+ index)) #\{))
               (let ((closing (position #\} text
                                        :start (+ index 2)
                                        :end end)))
                 (if closing (1+ closing) (1+ index))))
              (t (min end (1+ index)))))
          (1+ index)))))

(defun %timestamp-diary-expression-end (text start end)
  ;; The caller already checked that START begins with "%%(" within END.
  (let ((cursor (+ start 3))
        (depth 1)
        (in-string-p nil)
        (escaped-p nil)
        (comment-p nil))
    (loop while (< cursor end)
          for character = (char text cursor)
          for character-end =
            (and (not in-string-p)
                 (not comment-p)
                 (char= character #\?)
                 (%timestamp-diary-character-literal-end text cursor end))
          do (cond
               (comment-p
                (when (or (char= character #\Newline)
                          (char= character #\Return))
                  (setf comment-p nil)))
               (character-end
                (setf cursor (1- character-end)))
               (escaped-p
                (setf escaped-p nil))
               ((and in-string-p (char= character #\\))
                (setf escaped-p t))
               ((and (not in-string-p) (char= character #\;))
                (setf comment-p t))
               ((char= character #\")
                (setf in-string-p (not in-string-p)))
               ((not in-string-p)
                (case character
                  (#\( (incf depth))
                  (#\)
                    (decf depth)
                    (when (zerop depth)
                      (return-from %timestamp-diary-expression-end
                        (1+ cursor)))))))
          (incf cursor))))


(defun %timestamp-single-line-p (text start end)
  (not
   (or (position #\Newline text :start start :end end)
       (position #\Return text :start start :end end))))

(defun %parse-single-timestamp (text start end)
  (let* ((opening (char text start))
         (closing
          (case opening
            (#\< #\>)
            (#\[ #\]))))
    (when closing
      (let* ((inner-start (1+ start))
             (base-type
              (if (char= opening #\<)
                  :active
                  :inactive))
             (diary-expression-end
              (and (<= (+ inner-start 3) end)
                   (string= text
                            "%%("
                            :start1
                            inner-start
                            :end1
                            (+ inner-start 3))
                   (%timestamp-diary-expression-end text inner-start end)))
             (close-position
              (and diary-expression-end
                   (position closing
                             text
                             :start
                             diary-expression-end
                             :end
                             end))))
        (if diary-expression-end
            (when
                (and close-position
                     (%timestamp-single-line-p text start (1+ close-position)))
              (let ((fields (%make-timestamp-fields :type :diary)))
                (multiple-value-bind (parsed-fields valid-p)
                    (%timestamp-parse-suffix text
                                             diary-expression-end
                                             close-position
                                             fields)
                  (when valid-p
                    (values parsed-fields (1+ close-position))))))
            (let ((close-position
                   (position closing text :start inner-start :end end)))
              (when
                  (and close-position
                       (%timestamp-single-line-p text
                                                 start
                                                 (1+ close-position)))
                (multiple-value-bind (year month day date-end)
                    (%timestamp-date-at text inner-start close-position)
                  (when date-end
                    (let ((fields
                           (%make-timestamp-fields :type
                                                   base-type
                                                   :year
                                                   year
                                                   :month
                                                   month
                                                   :day
                                                   day)))
                      (multiple-value-bind (parsed-fields valid-p)
                          (%timestamp-parse-suffix text
                                                   date-end
                                                   close-position
                                                   fields)
                        (when valid-p
                          (values parsed-fields (1+ close-position))))))))))))))

(defun %timestamp-end-hour (fields)
  (if (%timestamp-fields-time-range-p fields)
      (%timestamp-fields-end-hour fields)
      (%timestamp-fields-hour fields)))

(defun %timestamp-end-minute (fields)
  (if (%timestamp-fields-time-range-p fields)
      (%timestamp-fields-end-minute fields)
      (%timestamp-fields-minute fields)))

(defun %timestamp-properties (start-fields &optional end-fields external-range-p)
  (let* ((base-type (%timestamp-fields-type start-fields))
         (range-p
           (or external-range-p (%timestamp-fields-time-range-p start-fields)))
         (type
           (if (eq base-type :diary)
               :diary
               (if range-p
                   (if (eq base-type :active) :active-range :inactive-range)
                   base-type)))
         (end-year
           (if end-fields
               (%timestamp-fields-year end-fields)
               (%timestamp-fields-year start-fields)))
         (end-month
           (if end-fields
               (%timestamp-fields-month end-fields)
               (%timestamp-fields-month start-fields)))
         (end-day
           (if end-fields
               (%timestamp-fields-day end-fields)
               (%timestamp-fields-day start-fields)))
         (end-hour
           (if end-fields
               (%timestamp-end-hour end-fields)
               (%timestamp-end-hour start-fields)))
         (end-minute
           (if end-fields
               (%timestamp-end-minute end-fields)
               (%timestamp-end-minute start-fields))))
    (append
     (list :type type
           :year-start (%timestamp-fields-year start-fields)
           :month-start (%timestamp-fields-month start-fields)
           :day-start (%timestamp-fields-day start-fields)
           :hour-start (%timestamp-fields-hour start-fields)
           :minute-start (%timestamp-fields-minute start-fields)
           :year-end end-year
           :month-end end-month
           :day-end end-day
           :hour-end end-hour
           :minute-end end-minute
           :repeater-type (%timestamp-fields-repeater-type start-fields)
           :repeater-value (%timestamp-fields-repeater-value start-fields)
           :repeater-unit (%timestamp-fields-repeater-unit start-fields)
           :warning-type (%timestamp-fields-warning-type start-fields)
           :warning-value (%timestamp-fields-warning-value start-fields)
           :warning-unit (%timestamp-fields-warning-unit start-fields))
     (when (%timestamp-fields-repeater-window-value start-fields)
       (list :repeater-window-value
             (%timestamp-fields-repeater-window-value start-fields)
             :repeater-window-unit
             (%timestamp-fields-repeater-window-unit start-fields))))))

(defun try-parse-timestamp-object (document start end container-type)
  "Recognize one Org timestamp starting at START within [START, END)."
  (declare (ignore container-type))
  (let ((text (document-text document)))
    (multiple-value-bind (start-fields first-end)
        (%parse-single-timestamp text start end)
      (when start-fields
        (let ((timestamp-end first-end)
              (end-fields nil)
              (external-range-p nil))
          (when
              (and (< (+ first-end 2) end)
                   (string= text "--" :start1 first-end :end1 (+ first-end 2))
                   (not (eq (%timestamp-fields-type start-fields) :diary)))
            (multiple-value-bind (candidate-fields candidate-end)
                (%parse-single-timestamp text (+ first-end 2) end)
              (when
                  (and candidate-fields
                       (eq (%timestamp-fields-type candidate-fields)
                           (%timestamp-fields-type start-fields)))
                (setf end-fields candidate-fields
                      timestamp-end candidate-end
                      external-range-p t))))
          (let* ((post-blank-end
                  (%timestamp-skip-whitespace text timestamp-end end))
                 (post-blank (- post-blank-end timestamp-end)))
            (%make-object-match :type
                                :timestamp
                                :end
                                post-blank-end
                                :contents-begin
                                nil
                                :contents-end
                                nil
                                :post-blank
                                post-blank
                                :properties
                                (%timestamp-properties start-fields
                                                       end-fields
                                                       external-range-p)
                                :opaque-p
                                nil)))))))

;;;; src/diary-sexp.lisp
(in-package #:org-kit)

(defstruct
    (diary-sexp-source (:constructor %make-diary-sexp-source)
                       (:conc-name %diary-sexp-source-)
                       (:copier nil))
  expression
  entry
  form
  view
  function-name)

(defstruct (diary-sexp-symbol (:constructor %make-diary-sexp-symbol)
                              (:conc-name %diary-sexp-symbol-)
                              (:copier nil))
  name)

(defstruct (diary-sexp-parser (:constructor %make-diary-sexp-parser)
                              (:conc-name %diary-sexp-parser-)
                              (:copier nil))
  text
  (index 0)
  limits)

(defun diary-sexp-invalid-expression ()
  (error 'org-agenda-error
         :message "Invalid diary expression"
         :reason :invalid-diary-expression))

(defun diary-sexp-whitespace-p (character)
  (member character '(#\Space #\Tab #\Newline #\Return)))

(defun diary-sexp-string-prefix-p (prefix text)
  (and (stringp text)
       (<= (length prefix) (length text))
       (string= prefix text :start2 0 :end2 (length prefix))))

(defun diary-sexp-source-parts (text &key timestamp-p limits)
  (let* ((limits (or limits (make-limits)))
         (start (if timestamp-p
                    (and (stringp text) (search "%%(" text))
                    (and (diary-sexp-string-prefix-p "%%(" text) 0))))
    (when start
      (let* ((form-start (+ start 2))
             (maximum (limit-value limits :max-diary-expression-length))
             (scan-end (min (length text) (+ form-start maximum 1)))
             (end (%timestamp-diary-expression-end text start scan-end)))
        (unless end
          (when (> (- (length text) form-start) maximum)
            (check-limit limits :max-diary-expression-length (1+ maximum)))
          (diary-sexp-invalid-expression))
        (check-limit limits :max-diary-expression-length (- end form-start))
        (diary-sexp-scan-expression text form-start end limits)
        (values (subseq text form-start end)
                (if timestamp-p "" (subseq text end))
                t)))))


(defun diary-sexp-scan-expression (text start end limits)
  (let ((index start)
        (depth 0)
        (in-string-p nil)
        (escaped-p nil)
        (comment-p nil))
    (loop while (< index end)
          for character = (char text index)
          do (cond
               (comment-p
                (when (or (char= character #\Newline)
                          (char= character #\Return))
                  (setf comment-p nil)))
               (in-string-p
                (cond
                  (escaped-p (setf escaped-p nil))
                  ((char= character #\\) (setf escaped-p t))
                  ((char= character #\") (setf in-string-p nil))))
               ((and (char= character #\?)
                     (%timestamp-diary-character-literal-end text index end))
                (setf index (1- (%timestamp-diary-character-literal-end
                                 text index end))))
               ((char= character #\;)
                (setf comment-p t))
               ((char= character #\")
                (setf in-string-p t))
               ((char= character #\#)
                (when (< (1+ index) end)
                  (let ((next (char text (1+ index))))
                    (when (or (char= next #\.)
                              (char-equal next #\s))
                      (diary-sexp-invalid-expression)))))
               ((and (char= character #\:)
                     (< (1+ index) end)
                     (char= (char text (1+ index)) #\:))
                (diary-sexp-invalid-expression))
               ((char= character #\()
                (incf depth)
                (check-limit limits :max-diary-expression-depth depth))
               ((char= character #\))
                (when (zerop depth)
                  (diary-sexp-invalid-expression))
                (decf depth)))
          (incf index))
    (when (or in-string-p escaped-p (/= depth 0))
      (diary-sexp-invalid-expression))
    t))

(defun diary-sexp-source-skip-trivia (text index)
  (loop while (< index (length text))
        for character = (char text index)
        do (cond
             ((diary-sexp-whitespace-p character)
              (incf index))
             ((char= character #\;)
              (loop while (and (< index (length text))
                               (not (member (char text index)
                                            '(#\Newline #\Return))))
                    do (incf index)))
             (t (return index)))
        finally (return index)))

(defun diary-sexp-top-level-operator (expression)
  (let* ((length (length expression))
         (index (diary-sexp-source-skip-trivia expression 0)))
    (when (and (< index length) (char= (char expression index) #\())
      (setf index (diary-sexp-source-skip-trivia expression (1+ index)))
      (let ((start index))
        (loop while (and (< index length)
                         (not (diary-sexp-whitespace-p (char expression index)))
                         (not (member (char expression index)
                                      '(#\( #\) #\; #\' #\"))))
              do (incf index))
        (when (> index start)
          (subseq expression start index))))))

(defun diary-sexp-built-in-name-p (name)
  (member name
          '("diary-anniversary" "diary-block" "diary-cyclic"
            "diary-float" "diary-date")
          :test #'string=))

(defun diary-sexp-parser-skip-trivia (parser)
  (let* ((text (%diary-sexp-parser-text parser))
         (length (length text))
         (index (%diary-sexp-parser-index parser)))
    (loop while (< index length)
          for character = (char text index)
          do (cond
               ((diary-sexp-whitespace-p character)
                (incf index))
               ((char= character #\;)
                (loop while (and (< index length)
                                 (not (member (char text index)
                                              '(#\Newline #\Return))))
                      do (incf index)))
               (t (loop-finish)))
          finally (setf (%diary-sexp-parser-index parser) index))))

(defun diary-sexp-parser-take (parser)
  (let ((index (%diary-sexp-parser-index parser)))
    (when (>= index (length (%diary-sexp-parser-text parser)))
      (diary-sexp-invalid-expression))
    (setf (%diary-sexp-parser-index parser) (1+ index))
    (char (%diary-sexp-parser-text parser) index)))

(defun diary-sexp-token-delimiter-p (character)
  (or (diary-sexp-whitespace-p character)
      (member character '(#\( #\) #\; #\' #\"))))

(defun diary-sexp-parse-string (parser)
  (diary-sexp-parser-take parser)
  (let* ((text (%diary-sexp-parser-text parser))
         (length (length text)))
    (with-output-to-string (output)
      (loop
        (when (>= (%diary-sexp-parser-index parser) length)
          (diary-sexp-invalid-expression))
        (let ((character (diary-sexp-parser-take parser)))
          (cond
            ((char= character #\")
             (return))
            ((char= character #\\)
             (when (>= (%diary-sexp-parser-index parser) length)
               (diary-sexp-invalid-expression))
             (let ((escaped (diary-sexp-parser-take parser)))
               (write-char (case escaped
                             (#\n #\Newline)
                             (#\r #\Return)
                             (#\t #\Tab)
                             (t escaped))
                           output)))
            (t (write-char character output))))))))

(defun diary-sexp-digit-p (character)
  (and character (digit-char-p character 10)))

(defun diary-sexp-parse-integer (parser)
  (let* ((text (%diary-sexp-parser-text parser))
         (length (length text))
         (start (%diary-sexp-parser-index parser))
         (index start)
         (negative-p nil)
         (value 0))
    (when (member (char text index) '(#\+ #\-))
      (setf negative-p (char= (char text index) #\-))
      (incf index))
    (unless (and (< index length) (diary-sexp-digit-p (char text index)))
      (diary-sexp-invalid-expression))
    (loop while (and (< index length) (diary-sexp-digit-p (char text index)))
          for digit = (digit-char-p (char text index) 10)
          do (when (> value (floor (- most-positive-fixnum digit) 10))
               (diary-sexp-invalid-expression))
             (setf value (+ (* value 10) digit))
             (incf index))
    (when (and (< index length)
               (not (diary-sexp-token-delimiter-p (char text index))))
      (diary-sexp-invalid-expression))
    (setf (%diary-sexp-parser-index parser) index)
    (if negative-p (- value) value)))

(defun diary-sexp-parse-symbol (parser)
  (let* ((text (%diary-sexp-parser-text parser))
         (length (length text))
         (start (%diary-sexp-parser-index parser))
         (index start))
    (loop while (and (< index length)
                     (not (diary-sexp-token-delimiter-p (char text index))))
          do (incf index))
    (when (= start index)
      (diary-sexp-invalid-expression))
    (setf (%diary-sexp-parser-index parser) index)
    (let ((name (subseq text start index)))
      (cond
        ((string= name "t") t)
        ((string= name "nil") nil)
        (t (%make-diary-sexp-symbol :name name))))))

(defun diary-sexp-parse-list (parser depth)
  (let ((limits (%diary-sexp-parser-limits parser))
        (items nil)
        (tail nil))
    (check-limit limits :max-diary-expression-depth depth)
    (diary-sexp-parser-take parser)
    (loop
      (diary-sexp-parser-skip-trivia parser)
      (when (>= (%diary-sexp-parser-index parser)
                (length (%diary-sexp-parser-text parser)))
        (diary-sexp-invalid-expression))
      (when (char= (char (%diary-sexp-parser-text parser)
                         (%diary-sexp-parser-index parser))
                   #\))
        (diary-sexp-parser-take parser)
        (return (or items '())))
      (let* ((value (diary-sexp-parse-value parser depth))
             (cell (list value)))
        (if tail
            (setf (cdr tail) cell)
            (setf items cell))
        (setf tail cell)))))

(defun diary-sexp-parse-value (parser depth)
  (diary-sexp-parser-skip-trivia parser)
  (when (>= (%diary-sexp-parser-index parser)
            (length (%diary-sexp-parser-text parser)))
    (diary-sexp-invalid-expression))
  (let ((character (char (%diary-sexp-parser-text parser)
                         (%diary-sexp-parser-index parser))))
    (cond
      ((char= character #\()
       (diary-sexp-parse-list parser (1+ depth)))
      ((char= character #\")
       (diary-sexp-parse-string parser))
      ((char= character #\')
       (diary-sexp-parser-take parser)
       (let ((quoted-depth (1+ depth)))
         (check-limit (%diary-sexp-parser-limits parser)
                      :max-diary-expression-depth
                      quoted-depth)
         (diary-sexp-parse-value parser quoted-depth)))
      ((or (diary-sexp-digit-p character)
           (and (member character '(#\+ #\-))
                (< (1+ (%diary-sexp-parser-index parser))
                   (length (%diary-sexp-parser-text parser)))
                (diary-sexp-digit-p
                 (char (%diary-sexp-parser-text parser)
                       (1+ (%diary-sexp-parser-index parser))))))
       (diary-sexp-parse-integer parser))
      (t (diary-sexp-parse-symbol parser)))))

(defun diary-sexp-read-built-in-form (expression limits)
  (let* ((parser (%make-diary-sexp-parser :text expression :limits limits))
         (form (diary-sexp-parse-value parser 0)))
    (diary-sexp-parser-skip-trivia parser)
    (unless (and (= (%diary-sexp-parser-index parser) (length expression))
                 (consp form)
                 (edit-proper-list-length form)
                 (diary-sexp-symbol-p (car form)))
      (diary-sexp-invalid-expression))
    (setf (car form) (%diary-sexp-symbol-name (car form)))
    form))

(defun diary-sexp-source-for-node (node &optional limits)
  (let* ((type (node-type node))
         (document (node-document node))
         (limits (or limits
                     (and document (document-limits document))
                     (make-limits)))
         (timestamp (and (eq type :timestamp) node))
         (view (and timestamp (timestamp-view timestamp))))
    (when
        (or (eq type :diary-sexp)
            (and view (eq (org-timestamp-view-kind view) :diary)))
      (multiple-value-bind (expression entry found-p)
          (if (eq type :diary-sexp)
              (diary-sexp-source-parts (node-property node :value)
                                       :limits limits)
              (diary-sexp-source-parts (node-text node)
                                       :timestamp-p t
                                       :limits limits))
        (when found-p
          (let* ((operator (diary-sexp-top-level-operator expression))
                 (function-name
                   (and (diary-sexp-built-in-name-p operator) operator))
                 (form (and function-name
                            (diary-sexp-read-built-in-form expression limits))))
            (%make-diary-sexp-source :expression expression
                                     :entry entry
                                     :form form
                                     :function-name function-name
                                     :view view)))))))

(defun diary-sexp-component-match-p (value component)
  (cond
    ((eq component t) t)
    ((integerp component) (= value component))
    ((consp component)
     (and (edit-proper-list-length component)
          (member value component :test #'eql)))
    (t nil)))

(defun diary-sexp-exact-date-ordinal (month day year)
  (when (and (integerp month) (integerp day) (integerp year))
    (handler-case (cl-date-kit:local-date-to-epoch-day
                   (cl-date-kit:make-local-date year month day))
      (cl-date-kit:invalid-date ()
        nil))))

(defun diary-sexp-month-last-day (month year)
  (let* ((first (cl-date-kit:make-local-date year month 1))
         (next
          (if (= month 12)
              (cl-date-kit:make-local-date (1+ year) 1 1)
              (cl-date-kit:make-local-date year (1+ month) 1))))
    (- (cl-date-kit:local-date-to-epoch-day next)
        (cl-date-kit:local-date-to-epoch-day first))))

(defun diary-sexp-weekday-index (date)
  (position (cl-date-kit:day-of-week date)
            '(:sunday :monday :tuesday :wednesday :thursday :friday :saturday)
            :test
            #'eq))

(defun diary-sexp-date-match-p (arguments date)
  (when (member (length arguments) '(3 4))
    (let ((month (cl-date-kit:local-date-month date))
          (day (cl-date-kit:local-date-day date))
          (year (cl-date-kit:local-date-year date)))
      (and (diary-sexp-component-match-p month (first arguments))
           (diary-sexp-component-match-p day (second arguments))
           (diary-sexp-component-match-p year (third arguments))))))

(defun diary-sexp-block-match-p (arguments date)
  (when (member (length arguments) '(6 7))
    (let ((start
           (diary-sexp-exact-date-ordinal (first arguments)
                                          (second arguments)
                                          (third arguments)))
          (end
           (diary-sexp-exact-date-ordinal (fourth arguments)
                                          (fifth arguments)
                                          (sixth arguments)))
          (ordinal (cl-date-kit:local-date-to-epoch-day date)))
      (and start end (<= start ordinal end)))))

(defun diary-sexp-cyclic-match-p (arguments date)
  (when (member (length arguments) '(4 5))
    (let* ((cycle (first arguments))
           (start (diary-sexp-exact-date-ordinal (second arguments)
                                                 (third arguments)
                                                 (fourth arguments)))
           (difference (- (cl-date-kit:local-date-to-epoch-day date)
                          (or start 0))))
      (and (integerp cycle)
           (plusp cycle)
           start
           (not (minusp difference))
           (zerop (mod difference cycle))))))

(defun diary-sexp-anniversary-match-p (arguments date)
  (when (member (length arguments) '(2 3 4))
    (let* ((month (first arguments))
           (day (second arguments))
           (year (third arguments))
           (candidate-year (cl-date-kit:local-date-year date))
           (candidate-month (cl-date-kit:local-date-month date))
           (candidate-day (cl-date-kit:local-date-day date))
           (valid-date (diary-sexp-exact-date-ordinal month day 2000)))
      (when (and valid-date (or (null year) (integerp year)))
        (when (and (= month 2)
                   (= day 29)
                   (null
                    (diary-sexp-exact-date-ordinal 2 29 candidate-year)))
          (setf month 3
                day 1))
        (and (> (if year (- candidate-year year) 100) 0)
             (= candidate-month month)
             (= candidate-day day))))))

(defun diary-sexp-float-match-p (arguments date)
  (when (member (length arguments) '(3 4 5))
    (let* ((month-selector (first arguments))
           (dayname (second arguments))
           (count (third arguments))
           (ordinal (cl-date-kit:local-date-to-epoch-day date))
           (first-match-ordinal
             (and (integerp count)
                  (not (zerop count))
                  (if (plusp count)
                      (- ordinal (* 7 (1- count)))
                      (+ ordinal (* 7 (1- (abs count))))))))
      (when (and (integerp dayname)
                 (<= 0 dayname 6)
                 (integerp count)
                 (not (zerop count))
                 (= dayname (diary-sexp-weekday-index date))
                 first-match-ordinal)
        (loop for distance from 0 below 7
              for boundary-ordinal =
                (+ first-match-ordinal
                   (* distance (if (plusp count) -1 1)))
              for boundary =
                (handler-case
                    (cl-date-kit:local-date-from-epoch-day boundary-ordinal)
                  (cl-date-kit:invalid-date ()
                    nil))
              thereis
                (when boundary
                  (let* ((boundary-year (cl-date-kit:local-date-year boundary))
                         (boundary-month (cl-date-kit:local-date-month boundary))
                         (boundary-date-day (cl-date-kit:local-date-day boundary))
                         (last-day
                           (diary-sexp-month-last-day boundary-month
                                                      boundary-year))
                         (boundary-day
                           (or (fourth arguments)
                               (if (plusp count) 1 last-day))))
                    (and (integerp boundary-day)
                         (<= 1 boundary-day last-day)
                         (diary-sexp-component-match-p boundary-month month-selector)
                         (= boundary-date-day boundary-day)))))))))

(defun diary-sexp-built-in-match-p (source date)
  (let ((name (%diary-sexp-source-function-name source))
        (arguments (cdr (%diary-sexp-source-form source))))
    (cond
      ((string= name "diary-date")
       (diary-sexp-date-match-p arguments date))
      ((string= name "diary-block")
       (diary-sexp-block-match-p arguments date))
      ((string= name "diary-cyclic")
       (diary-sexp-cyclic-match-p arguments date))
      ((string= name "diary-float")
       (diary-sexp-float-match-p arguments date))
      ((string= name "diary-anniversary")
       (diary-sexp-anniversary-match-p arguments date)))))

(defun diary-sexp-source-match-p (source date callback-registry)
  (if (%diary-sexp-source-function-name source)
      (diary-sexp-built-in-match-p source date)
      (not
       (null
        (evaluate-elisp callback-registry
                        (%diary-sexp-source-expression source)
                        (list :entry
                              (%diary-sexp-source-entry source)
                              :date
                              (list (cl-date-kit:local-date-month date)
                                    (cl-date-kit:local-date-day date)
                                    (cl-date-kit:local-date-year date))))))))



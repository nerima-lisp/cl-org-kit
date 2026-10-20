;;;; src/table-calc-eval.lisp
(in-package #:org-kit)

(defstruct (table-calc-work-budget
             (:constructor %make-table-calc-work-budget ()))
  (used 0))

(defstruct (table-calc-range-value
             (:constructor %make-table-calc-range-value
                 (first-row last-row first-column last-column)))
  first-row last-row first-column last-column)

(defstruct (table-calc-remote-range-value
             (:constructor %make-table-calc-remote-range-value (values)))
  values)

(defconstant +table-calc-nan+ :table-calc-nan)
(defconstant +table-calc-skip+ :table-calc-skip)

(defun make-table-calc-work-budget ()
  "Create a work counter shared by formula assignments in one recalculation."
  (%make-table-calc-work-budget))

(defun table-calc-charge-work (limits budget amount)
  (let ((actual (+ (table-calc-work-budget-used budget) amount)))
    (check-limit limits :max-formula-work actual)
    (setf (table-calc-work-budget-used budget) actual)))

(defun table-calc-evaluation-error (reason target)
  (error 'org-table-error
         :message "Table formula evaluation failed"
         :target target
         :reason reason))

(defun table-calc-assignment-target-description (assignment)
  (let* ((reference (table-calc-assignment-target assignment))
         (row-kind (table-calc-reference-row-kind reference))
         (row-value (table-calc-reference-row-value reference))
         (column-value (table-calc-reference-column-value reference)))
    (bounded-text
     (cond
       ((eql row-kind :current)
        (format nil "$~A" column-value))
       ((eql row-kind :first)
        (format nil "@<$~A" column-value))
       ((eql row-kind :last)
        (format nil "@>$~A" column-value))
       ((eql row-kind :invalid)
        (format nil "@INVALID$~A" column-value))
       (t
        (format nil "@~A$~A" row-value column-value))))))

(defun table-calc-validate-assignment-target (assignment target)
  (let* ((reference (table-calc-assignment-target assignment))
         (row-kind (table-calc-reference-row-kind reference))
         (column-kind (table-calc-reference-column-kind reference)))
    (cond
      ((eql row-kind :invalid)
       (table-calc-evaluation-error :invalid-target target))
      ((eql column-kind :name)
       (table-calc-evaluation-error :unsupported-target target)))))

(defun table-calc-reference-position
    (reference reference-position-function target)
  (when (eql (table-calc-reference-row-kind reference) :invalid)
    (table-calc-evaluation-error :invalid-reference target))
  (multiple-value-bind (row column)
      (funcall reference-position-function reference)
    (unless (and (integerp row) (plusp row)
                 (integerp column) (plusp column))
      (table-calc-evaluation-error :unsupported-reference target))
    (values row column)))

(defun table-calc-decimal-digit-count (integer)
  (length (write-to-string (abs integer) :base 10 :radix nil)))

(defun table-calc-round-integer (numerator denominator)
  (multiple-value-bind (quotient remainder) (floor numerator denominator)
    (cond ((> (* 2 remainder) denominator) (1+ quotient))
          ((< (* 2 remainder) denominator) quotient)
          ((oddp quotient) (1+ quotient))
          (t quotient))))

(defun table-calc-round-decimal (coefficient exponent negative-zero-p precision limits
                                &optional float-p)
  (let ((negative-p (or (minusp coefficient)
                        (and (zerop coefficient) negative-zero-p))))
    (when (zerop coefficient)
      (check-limit limits :max-formula-precision 1)
      (return-from table-calc-round-decimal
        (%make-safe-decimal :coefficient 0 :exponent 0
                            :negative-zero-p negative-p :float-p float-p)))
    (multiple-value-setq (coefficient exponent)
      (safe-decimal-strip-trailing-zeroes coefficient exponent))
    (let ((digits (table-calc-decimal-digit-count coefficient)))
      (when (> digits precision)
        (let* ((discarded-digits (- digits precision))
               (rounded-exponent (+ exponent discarded-digits)))
          (let* ((divisor (expt 10 discarded-digits))
                 (rounded (table-calc-round-integer (abs coefficient) divisor)))
            (setf coefficient (if negative-p (- rounded) rounded)
                  exponent rounded-exponent)))))
    (multiple-value-setq (coefficient exponent)
      (safe-decimal-strip-trailing-zeroes coefficient exponent))
    (check-limit limits :max-formula-precision
                 (table-calc-decimal-digit-count coefficient))
    (check-limit limits :max-formula-exponent (abs exponent))
    (%make-safe-decimal :coefficient coefficient :exponent exponent
                        :negative-zero-p nil :float-p float-p)))

(defun table-calc-decimal-negative-p (decimal)
  (let ((coefficient (safe-decimal-coefficient decimal)))
    (if (zerop coefficient)
        (safe-decimal-negative-zero-p decimal)
        (minusp coefficient))))

(defun table-calc-decimal-negate (decimal)
  (let ((coefficient (safe-decimal-coefficient decimal)))
    (%make-safe-decimal :coefficient (- coefficient)
                        :exponent (safe-decimal-exponent decimal)
                        :negative-zero-p
                        (and (zerop coefficient)
                             (not (safe-decimal-negative-zero-p decimal)))
                        :float-p (safe-decimal-float-p decimal))))

(defun table-calc-decimal-at-precision (value precision limits)
  (table-calc-round-decimal
   (safe-decimal-coefficient value) (safe-decimal-exponent value)
   (safe-decimal-negative-zero-p value) precision limits
   (safe-decimal-float-p value)))

(defun table-calc-decimal-add (left right limits &optional calculation-precision)
  (let* ((left-coefficient (safe-decimal-coefficient left))
         (right-coefficient (safe-decimal-coefficient right))
         (left-exponent (safe-decimal-exponent left))
         (right-exponent (safe-decimal-exponent right)))
    (when (zerop left-coefficient)
      (when (zerop right-coefficient)
        (return-from table-calc-decimal-add
          (table-calc-round-decimal
           0 0 (and (table-calc-decimal-negative-p left)
                    (table-calc-decimal-negative-p right))
           (or calculation-precision
               (limit-value limits :max-formula-precision)) limits
           (or (safe-decimal-float-p left) (safe-decimal-float-p right)))))
      (return-from table-calc-decimal-add
        (if calculation-precision
            (table-calc-decimal-at-precision right calculation-precision limits)
            right)))
    (when (zerop right-coefficient)
      (return-from table-calc-decimal-add
        (if calculation-precision
            (table-calc-decimal-at-precision left calculation-precision limits)
            left)))
    (let* ((left-order (+ (table-calc-decimal-digit-count left-coefficient)
                          left-exponent -1))
           (right-order (+ (table-calc-decimal-digit-count right-coefficient)
                           right-exponent -1))
           (precision (or calculation-precision
                          (limit-value limits :max-formula-precision))))
      (when (> (abs (- left-order right-order)) precision)
        (return-from table-calc-decimal-add
          (table-calc-decimal-at-precision
           (if (> (safe-decimal-magnitude-compare left right) 0) left right)
           precision limits)))
      (let* ((exponent (min left-exponent right-exponent))
             (left-shift (- left-exponent exponent))
             (right-shift (- right-exponent exponent))
             (coefficient (+ (* left-coefficient (expt 10 left-shift))
                             (* right-coefficient (expt 10 right-shift)))))
        (table-calc-round-decimal
         coefficient exponent nil precision limits
         (or (safe-decimal-float-p left) (safe-decimal-float-p right)))))))

(defun table-calc-factor-count (integer factor)
  (loop with value = (abs integer)
        for count from 0
        while (zerop (mod value factor))
        do (setf value (truncate value factor))
        finally (return count)))

(defun table-calc-nonterminating-rational-p (value)
  (let* ((remaining (denominator value))
         (twos (table-calc-factor-count remaining 2))
         (fives (table-calc-factor-count remaining 5)))
    (/= remaining (* (expt 2 twos) (expt 5 fives)))))

(defun table-calc-product-decimal-zeroes (left right)
  (min (+ (table-calc-factor-count left 2)
          (table-calc-factor-count right 2))
       (+ (table-calc-factor-count left 5)
          (table-calc-factor-count right 5))))

(defun table-calc-decimal-multiply (left right limits &optional calculation-precision)
  (let* ((left-coefficient (safe-decimal-coefficient left))
         (right-coefficient (safe-decimal-coefficient right))
         (negative-p (not (eq (table-calc-decimal-negative-p left)
                              (table-calc-decimal-negative-p right))))
         (precision (or calculation-precision
                        (limit-value limits :max-formula-precision))))
    (when (or (zerop left-coefficient) (zerop right-coefficient))
      (return-from table-calc-decimal-multiply
        (table-calc-round-decimal
         0 0 negative-p precision limits
         (or (safe-decimal-float-p left) (safe-decimal-float-p right)))))
    (let* ((exponent (+ (safe-decimal-exponent left)
                        (safe-decimal-exponent right)))
           (normalized-exponent
             (+ exponent
                (table-calc-product-decimal-zeroes left-coefficient
                                                   right-coefficient))))
      (check-limit limits :max-formula-exponent (abs normalized-exponent))
      (table-calc-round-decimal (* left-coefficient right-coefficient)
                                exponent nil precision limits
                                (or (safe-decimal-float-p left)
                                    (safe-decimal-float-p right))))))

(defun table-calc-decimal-ratio-order (numerator denominator)
  (let* ((numerator-coefficient (abs (safe-decimal-coefficient numerator)))
         (denominator-coefficient (abs (safe-decimal-coefficient denominator)))
         (numerator-digits (table-calc-decimal-digit-count numerator-coefficient))
         (denominator-digits (table-calc-decimal-digit-count denominator-coefficient))
         (difference (- numerator-digits denominator-digits))
         (scaled-numerator
           (%make-safe-decimal
            :coefficient numerator-coefficient
            :exponent (max 0 (- difference))
            :negative-zero-p nil))
         (scaled-denominator
           (%make-safe-decimal
            :coefficient denominator-coefficient
            :exponent (max 0 difference)
            :negative-zero-p nil)))
    (if (>= (safe-decimal-magnitude-compare scaled-numerator
                                             scaled-denominator)
            0)
        difference
        (1- difference))))

(defun table-calc-decimal-divide
    (numerator denominator limits target &optional calculation-precision)
  (let* ((numerator-coefficient (safe-decimal-coefficient numerator))
         (denominator-coefficient (safe-decimal-coefficient denominator))
         (precision (or calculation-precision
                        (limit-value limits :max-formula-precision)))
         (maximum-exponent (limit-value limits :max-formula-exponent))
         (float-p nil))
    (when (zerop denominator-coefficient)
      (table-calc-evaluation-error :division-by-zero target))
    (setf float-p
          (or (safe-decimal-float-p numerator)
              (safe-decimal-float-p denominator)
              (table-calc-nonterminating-rational-p
               (/ (safe-decimal-rational numerator)
                  (safe-decimal-rational denominator)))))
    (when (zerop numerator-coefficient)
      (return-from table-calc-decimal-divide
        (table-calc-round-decimal
         0 0 (not (eq (table-calc-decimal-negative-p numerator)
                      (table-calc-decimal-negative-p denominator)))
         precision limits float-p)))
    (let* ((ratio-order (table-calc-decimal-ratio-order numerator denominator))
           (result-order (+ ratio-order
                            (safe-decimal-exponent numerator)
                            (- (safe-decimal-exponent denominator))))
           (result-exponent (- result-order (1- precision))))
      (when (> result-exponent maximum-exponent)
        (check-limit limits :max-formula-exponent (abs result-exponent)))
      (when (< result-order (- maximum-exponent))
        (check-limit limits :max-formula-exponent (abs result-order)))
      (let* ((scale (- (1- precision) ratio-order))
             (scaled-numerator (* (abs numerator-coefficient)
                                 (expt 10 scale)))
             (rounded (table-calc-round-integer
                       scaled-numerator (abs denominator-coefficient)))
             (negative-p (not (eq (table-calc-decimal-negative-p numerator)
                                  (table-calc-decimal-negative-p denominator))))
             (coefficient (if negative-p (- rounded) rounded))
             (exponent result-exponent))
        (table-calc-round-decimal coefficient exponent
                                  nil
                                  precision limits float-p)))))

(defun table-calc-decimal-binary
    (operator left right limits target &optional calculation-precision)
  (cond
    ((or (eq left +table-calc-nan+) (eq right +table-calc-nan+))
     +table-calc-nan+)
    ((and (safe-decimal-p left) (safe-decimal-p right))
     (cond
       ((char= operator #\+)
        (table-calc-decimal-add left right limits calculation-precision))
       ((char= operator #\-)
        (table-calc-decimal-add left (table-calc-decimal-negate right) limits
                                calculation-precision))
       ((char= operator #\*)
        (table-calc-decimal-multiply left right limits calculation-precision))
       (t (table-calc-decimal-divide left right limits target
                                     calculation-precision))))
    (t
     (let ((left-rational (if (safe-decimal-p left)
                              (safe-decimal-rational left) left))
           (right-rational (if (safe-decimal-p right)
                               (safe-decimal-rational right) right)))
       (when (zerop right-rational)
         (when (char= operator #\/) (table-calc-evaluation-error :division-by-zero target)))
       (case operator
         (#\+ (+ left-rational right-rational))
         (#\- (- left-rational right-rational))
         (#\* (* left-rational right-rational))
         (otherwise (/ left-rational right-rational)))))))

(defun table-calc-number-rational (value)
  (if (safe-decimal-p value) (safe-decimal-rational value) value))

(defun table-calc-number-to-decimal (value limits &optional float-p)
  (cond
    ((safe-decimal-p value) value)
    ((integerp value)
     (%make-safe-decimal :coefficient value :exponent 0
                         :negative-zero-p nil :float-p float-p))
    ((rationalp value)
     (let* ((numerator (numerator value))
            (denominator (denominator value))
            (twos (table-calc-factor-count denominator 2))
            (fives (table-calc-factor-count denominator 5)))
       (unless (= denominator (* (expt 2 twos) (expt 5 fives)))
         (let* ((precision (limit-value limits :max-formula-precision))
                (order (floor (log (float (abs value) 1d0) 10d0)))
                (scale (max 0 (- precision 1 order)))
                (coefficient (table-calc-round-integer
                              (* (abs numerator) (expt 10 scale)) denominator)))
           (when (minusp numerator) (setf coefficient (- coefficient)))
           (return-from table-calc-number-to-decimal
             (table-calc-round-decimal coefficient (- scale) nil precision
                                       limits t))))
       (let* ((scale (max twos fives))
              (coefficient
                (* numerator
                   (expt 2 (- scale twos))
                   (expt 5 (- scale fives)))))
         (check-limit limits :max-formula-precision
                      (table-calc-decimal-digit-count coefficient))
         (check-limit limits :max-formula-exponent scale)
         (%make-safe-decimal :coefficient coefficient :exponent (- scale)
                             :negative-zero-p nil :float-p float-p))))
    ((floatp value)
     (table-calc-float-to-decimal value limits))
    (t (table-calc-evaluation-error :invalid-value :formula))))

(defun table-calc-float-to-decimal (value limits)
  (unless (and (realp value)
               (< (abs value) most-positive-double-float))
    (table-calc-evaluation-error :non-finite-result :formula))
  (let* ((text (substitute #\e #\d (format nil "~,17E" (float value 1d0))))
         (decimal (parse-safe-decimal text :limits limits)))
    (setf (safe-decimal-float-p decimal) t)
    decimal))

(defun table-calc-function-input-mode (mode)
  (cond ((member mode '(:e :en)) :e)
        ((member mode '(:n :en)) :n)
        (t :default)))

(defun table-calc-duration-cell-seconds (text)
  (let* ((negative-p (and (plusp (length text))
                          (char= (char text 0) #\-)))
         (start (if negative-p 1 0))
         (first-colon (position #\: text :start start))
         (second-colon (and first-colon
                            (position #\: text :start (1+ first-colon))))
         (hours-text (and first-colon (subseq text start first-colon)))
         (minutes-text (and first-colon
                            (subseq text (1+ first-colon)
                                    (or second-colon (length text)))))
         (seconds-text (and second-colon (subseq text (1+ second-colon))))
         (digits-p (lambda (value)
                     (and (plusp (length value))
                          (every #'table-calc-digit-p value)))))
    (unless (and first-colon
                 (funcall digits-p hours-text)
                 (= (length minutes-text) 2)
                 (funcall digits-p minutes-text)
                 (or (null second-colon)
                     (and (= (length seconds-text) 2)
                          (funcall digits-p seconds-text))))
      (return-from table-calc-duration-cell-seconds nil))
    (let ((hours (parse-integer hours-text))
          (minutes (parse-integer minutes-text))
          (seconds (if seconds-text (parse-integer seconds-text) 0)))
      (when (or (> minutes 59) (> seconds 59))
        (return-from table-calc-duration-cell-seconds nil))
      (let ((total (+ (* hours 3600) (* minutes 60) seconds)))
        (if negative-p (- total) total)))))

(defun table-calc-cell-number
    (raw mode in-range-p limits target &optional fraction-p duration-p unit-mode-p)
  (unless (stringp raw)
    (table-calc-evaluation-error :invalid-value target))
  (let* ((start (position-if-not #'table-calc-space-p raw))
         (end (position-if-not #'table-calc-space-p raw :from-end t)))
    (if (or (null start) (null end))
        (case mode
          (:default
           (if in-range-p
               +table-calc-skip+
               (%make-safe-decimal :coefficient 0 :exponent 0
                                   :negative-zero-p nil)))
          (:e +table-calc-nan+)
          (otherwise
           (%make-safe-decimal :coefficient 0 :exponent 0
                               :negative-zero-p nil)))
        (let* ((trimmed (subseq raw start (1+ end)))
               (decimal (parse-safe-decimal raw :start start :end (1+ end)
                                            :limits limits))
               (duration-seconds (and duration-p
                                      (table-calc-duration-cell-seconds trimmed)))
               (value (or decimal
                          (and duration-seconds
                               (%make-safe-decimal
                                :coefficient duration-seconds :exponent 0
                                :negative-zero-p nil)))))
          (or (and value (if fraction-p
                             (safe-decimal-rational value)
                             value))
              (cond
                (unit-mode-p
                 (table-calc-evaluation-error :unsupported-mode target))
                ((member mode '(:n :en))
                 (%make-safe-decimal :coefficient 0 :exponent 0
                                     :negative-zero-p nil))
                (t
                 (table-calc-evaluation-error :invalid-number target))))))))

(defun table-calc-resolve-range
    (expression reference-position-function limits budget target)
  (let* ((children (table-calc-expression-children expression))
         (first-reference (table-calc-expression-value (first children)))
         (last-reference (table-calc-expression-value (second children))))
    (table-calc-charge-work limits budget 2)
    (multiple-value-bind (first-row first-column)
        (table-calc-reference-position
         first-reference reference-position-function target)
      (multiple-value-bind (resolved-last-row last-column)
          (table-calc-reference-position
           last-reference reference-position-function target)
        (let ((last-row
                (if (eq (table-calc-reference-row-kind last-reference)
                        :hline-anchor)
                    (1- resolved-last-row)
                    resolved-last-row)))
          (when (< last-row 1)
            (table-calc-evaluation-error :invalid-reference target))
          (%make-table-calc-range-value
           (min first-row last-row) (max first-row last-row)
           (min first-column last-column) (max first-column last-column)))))))

(defun table-calc-number-add (left right limits &optional calculation-precision)
  (if (and (safe-decimal-p left) (safe-decimal-p right))
      (table-calc-decimal-add left right limits calculation-precision)
      (+ (table-calc-number-rational left) (table-calc-number-rational right))))

(defun table-calc-number-compare (left right)
  (if (and (safe-decimal-p left) (safe-decimal-p right))
      (safe-decimal-compare left right)
      (let ((left-number (table-calc-number-rational left))
            (right-number (table-calc-number-rational right)))
        (cond ((< left-number right-number) -1)
              ((> left-number right-number) 1)
        (t 0)))))

(defun table-calc-vector-function-name (function)
  (if (keywordp function)
      function
      (cdr (assoc function
                  '(("vsum" . :vsum) ("vmean" . :vmean)
                    ("vmin" . :vmin) ("vmax" . :vmax) ("vcount" . :vcount))
                  :test #'string=))))

(defun table-calc-evaluate-vector (function range mode cell-value-function
                                   limits budget target &optional
                                     fraction-p duration-p unit-mode-p
                                     calculation-precision)
  (let* ((remote-range-p (table-calc-remote-range-value-p range))
         (remote-values (and remote-range-p
                             (table-calc-remote-range-value-values range)))
         (first-row (if remote-range-p 1
                        (table-calc-range-value-first-row range)))
         (last-row (if remote-range-p 1
                       (table-calc-range-value-last-row range)))
         (first-column (if remote-range-p 1
                           (table-calc-range-value-first-column range)))
         (last-column (if remote-range-p (length remote-values)
                          (table-calc-range-value-last-column range)))
         (row-count (1+ (- last-row first-row)))
         (column-count (1+ (- last-column first-column)))
         (element-count (* row-count column-count))
         (count 0)
         (sum (if fraction-p 0
                  (%make-safe-decimal :coefficient 0 :exponent 0
                                      :negative-zero-p nil)))
         (minimum nil)
         (maximum nil)
         (nan-p nil))
    (setf function (table-calc-vector-function-name function))
    (unless function
      (table-calc-evaluation-error :invalid-function-result target))
    (table-calc-charge-work limits budget (* 2 element-count))
    (loop for row from first-row to last-row
          do (loop for column from first-column to last-column
                   for index from 0
                   for raw = (if remote-range-p
                                 (nth index remote-values)
                                 (funcall cell-value-function row column))
                   for value = (table-calc-cell-number raw mode t limits target
                                                       fraction-p duration-p
                                                       unit-mode-p)
                   unless (eq value +table-calc-skip+)
                     do (incf count)
                        (cond
                          ((eq value +table-calc-nan+)
                           (setf nan-p t))
                          ((eql function :vsum)
                           (setf sum (table-calc-number-add
                                      sum value limits calculation-precision)))
                          ((eql function :vmean)
                           (setf sum (table-calc-number-add
                                      sum value limits calculation-precision)))
                          ((eql function :vmin)
                           (when (or (null minimum)
                                     (minusp (table-calc-number-compare value minimum)))
                             (setf minimum value)))
                          ((eql function :vmax)
                           (when (or (null maximum)
                                     (plusp (table-calc-number-compare value maximum)))
                             (setf maximum value))))))
    (case function
      (:vcount
          (%make-safe-decimal :coefficient count :exponent 0
                              :negative-zero-p nil))
      ((:vsum :vmean :vmin :vmax)
       (when nan-p
         (return-from table-calc-evaluate-vector +table-calc-nan+))
       (case function
         (:vsum sum)
         (:vmean
          (when (zerop count)
            (table-calc-evaluation-error :empty-range target))
          (if fraction-p
              (/ sum count)
              (table-calc-decimal-divide
               sum (%make-safe-decimal :coefficient count :exponent 0
                                       :negative-zero-p nil)
               limits target calculation-precision)))
         (:vmin
          (or minimum (table-calc-evaluation-error :empty-range target)))
         (:vmax
          (or maximum (table-calc-evaluation-error :empty-range target))))))))

(defun table-calc-coerce-reference-value
    (value limits fraction-p &optional duration-p unit-mode-p target)
  (cond
    ((safe-decimal-p value) (if fraction-p (safe-decimal-rational value) value))
    ((numberp value) (if fraction-p value
                         (table-calc-number-to-decimal value limits)))
    ((stringp value)
     (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Newline) value)))
       (or (and (plusp (length trimmed))
                (let ((decimal (parse-safe-decimal trimmed :limits limits)))
                  (and decimal (if fraction-p
                                   (safe-decimal-rational decimal)
                                   decimal))))
           (and duration-p
                (let ((seconds (table-calc-duration-cell-seconds trimmed)))
                  (and seconds
                       (if fraction-p seconds
                           (%make-safe-decimal :coefficient seconds :exponent 0
                                               :negative-zero-p nil)))))
           (and unit-mode-p
                (table-calc-evaluation-error :unsupported-mode target))
           value)))
    (t value)))

(defun table-calc-reference-value
    (reference reference-position-function cell-value-function mode limits budget target
     &key fraction-p duration-p unit-mode-p reference-value-function)
  (table-calc-charge-work limits budget 1)
  (when reference-value-function
    (multiple-value-bind (value present-p)
        (funcall reference-value-function reference)
      (when present-p
        (return-from table-calc-reference-value
          (table-calc-coerce-reference-value value limits fraction-p
                                              duration-p unit-mode-p target)))))
  (multiple-value-bind (row column)
      (table-calc-reference-position
       reference reference-position-function target)
    (table-calc-cell-number (funcall cell-value-function row column)
                            mode nil limits target fraction-p duration-p
                            unit-mode-p)))

(defun table-calc-value-string (value limits)
  (cond
    ((stringp value) value)
    ((safe-decimal-p value) (table-calc-decimal-string value))
    ((numberp value)
     (table-calc-decimal-string (table-calc-number-to-decimal value limits)))
    ((eq value +table-calc-nan+) "nan")
    (t (table-calc-evaluation-error :invalid-function-result :formula))))

(defun table-calc-type-code (value)
  (cond
    ((eq value +table-calc-nan+) 12)
    ((stringp value) 101)
    ((safe-decimal-p value)
     (cond ((safe-decimal-float-p value) 3)
           ((minusp (safe-decimal-exponent value)) 2)
           (t 1)))
    ((integerp value) 1)
    ((rationalp value) 2)
    ((floatp value) 3)
    (t 0)))

(defun table-calc-compare-values (operator left right target)
  (let ((comparison
          (cond
            ((and (stringp left) (stringp right))
             (cond ((string= left right) 0)
                   ((string< left right) -1)
                   (t 1)))
            ((and (or (safe-decimal-p left) (numberp left))
                  (or (safe-decimal-p right) (numberp right)))
             (table-calc-number-compare left right))
            (t (table-calc-evaluation-error :invalid-value target)))))
    (case operator
      (:less (< comparison 0))
      (:greater (> comparison 0))
      (:equal (zerop comparison)))))

(defun table-calc-callback-argument (value target)
  (cond
    ((safe-decimal-p value) (safe-decimal-rational value))
    ((or (numberp value) (stringp value)) value)
    (t (table-calc-evaluation-error :invalid-function-result target))))

(defun table-calc-callback-result (value target)
  (cond
    ((stringp value) value)
    ((and (numberp value) (realp value)) value)
    (t (table-calc-evaluation-error :invalid-function-result target))))

(defun table-calc-flag-value (name flags)
  (loop for flag in flags
        when (and (consp flag) (eq (car flag) name))
          return (cdr flag)))

(defun table-calc-effective-precision (flags limits)
  (min (or (table-calc-flag-value :precision flags) 12)
       (limit-value limits :max-formula-precision)))

(defun table-calc-angle-degrees-p (flags)
  (let ((mode nil))
    (dolist (flag flags)
      (when (member flag '(:degrees :radians))
        (setf mode flag)))
    (or (null mode) (eql mode :degrees))))

(defun table-calc-function-value
    (name arguments flags limits budget target functions
     cell-value-function mode
     duration-p unit-mode-p calculation-precision)
  (let* ((fraction-p (member :fraction flags))
         (angle-degrees-p (table-calc-angle-degrees-p flags))
         (argument (first arguments)))
    (cond
      ((member name '("vsum" "vmean" "vmin" "vmax" "vcount")
               :test #'string=)
       (table-calc-evaluate-vector
        name argument mode cell-value-function limits budget target fraction-p
        duration-p unit-mode-p calculation-precision))
      ((member name '("exp" "sin" "tan") :test #'string=)
       (unless (or (safe-decimal-p argument) (realp argument))
         (table-calc-evaluation-error
          (if (and (member :symbolic flags) (stringp argument))
              :unsupported-mode
              :invalid-number)
          target))
       (handler-case
           (let* ((number (float (table-calc-number-rational argument) 1d0))
                  (angle (if angle-degrees-p (* number (/ pi 180d0)) number))
                  (result (cond ((string= name "exp") (exp number))
                                ((string= name "sin") (sin angle))
                                (t (tan angle)))))
             (table-calc-decimal-at-precision
              (table-calc-float-to-decimal result limits)
              calculation-precision limits))
         (arithmetic-error ()
           (table-calc-evaluation-error :non-finite-result target))))
      ((string= name "string")
       (table-calc-value-string argument limits))
      ((string= name "typeof")
       (%make-safe-decimal :coefficient (table-calc-type-code argument)
                           :exponent 0 :negative-zero-p nil))
      ((string= name "remote")
       (table-calc-evaluation-error :unknown-remote-table target))
      (t
       (let ((function (cdr (assoc name functions :test #'string=))))
         (unless function
           (table-calc-evaluation-error :unknown-function target))
         (let* ((callback-arguments
                  (mapcar (lambda (value)
                            (table-calc-callback-argument value target))
                          arguments))
                (result (apply function callback-arguments)))
           (table-calc-callback-result result target)))))))

(defun table-calc-taylor-number (expression reference-position-function
                                 cell-value-function mode limits budget target
                                 flags functions reference-value-function
                                 remote-function current-row current-column)
  (let* ((children (table-calc-expression-children expression))
         (function-expression (first children))
         (variable-assignment (second children))
         (degree-expression (third children))
         (variable (table-calc-expression-value
                    (first (table-calc-expression-children variable-assignment))))
         (point-expression (second (table-calc-expression-children variable-assignment)))
         (function-children (table-calc-expression-children function-expression)))
    (unless (and (eq (table-calc-expression-kind function-expression) :function)
                 (stringp (table-calc-expression-value function-expression))
                 (string= (table-calc-expression-value function-expression) "exp")
                 (= (length function-children) 1)
                 (eq (table-calc-expression-kind (first function-children)) :symbol)
                 (stringp variable)
                 (string= (table-calc-expression-value (first function-children))
                          variable))
      (table-calc-evaluation-error :unsupported-mode target))
    (let* ((point (table-calc-evaluate-expression
                   point-expression reference-position-function cell-value-function
                   mode limits budget target :flags flags :functions functions
                   :reference-value-function reference-value-function
                   :remote-function remote-function :current-row current-row
                   :current-column current-column))
           (degree (table-calc-evaluate-expression
                    degree-expression reference-position-function cell-value-function
                    mode limits budget target :flags flags :functions functions
                    :reference-value-function reference-value-function
                    :remote-function remote-function :current-row current-row
                    :current-column current-column)))
      (unless (or (safe-decimal-p point) (realp point))
        (table-calc-evaluation-error :unsupported-mode target))
      (unless (and (or (safe-decimal-p degree) (realp degree))
                   (integerp (table-calc-number-rational degree))
                   (= (table-calc-number-rational degree) 2))
        (table-calc-evaluation-error :unsupported-mode target))
      (when (zerop (table-calc-number-rational point))
        (return-from table-calc-taylor-number
          (format nil "~A + ~A^2 / 2 + 1" variable variable)))
      (handler-case
          (let* ((order (float (table-calc-number-rational point) 1d0))
                 (calculation-precision
                   (table-calc-effective-precision flags limits))
                 (coefficient-decimal
                   (table-calc-decimal-at-precision
                    (table-calc-float-to-decimal (exp order) limits)
                    calculation-precision limits))
                 (coefficient (float (safe-decimal-rational coefficient-decimal) 1d0))
                 (quadratic (/ coefficient 2d0))
                 (constant (* coefficient (- 1 order)))
                 (point-text (table-calc-value-string point limits))
                 (coefficient-text (table-calc-taylor-float-string coefficient))
                 (quadratic-text (table-calc-taylor-float-string quadratic))
                 (constant-text (table-calc-taylor-float-string (abs constant)))
                 (sign (if (minusp constant) "-" "+")))
            (format nil "~A ~A + ~A (~A - ~A)^2 ~A ~A"
                    coefficient-text variable quadratic-text variable point-text
                    sign constant-text))
        (arithmetic-error ()
          (table-calc-evaluation-error :non-finite-result target))))))

(defun table-calc-taylor-float-string (number)
  (let* ((order (if (zerop number) 0
                    (floor (log (abs number) 10d0))))
         (places (max 0 (- 11 order)))
         (text (format nil "~,VF" places number))
         (trimmed (string-right-trim "0" text)))
    (if (char= (char trimmed (1- (length trimmed))) #\.)
        (subseq trimmed 0 (1- (length trimmed)))
        trimmed)))

(defun table-calc-evaluate-expression
    (expression reference-position-function cell-value-function mode limits budget target
     &key flags functions reference-value-function remote-function
       current-row current-column)
  (let ((frames (list (cons expression nil)))
        (values '())
        (fraction-p (member :fraction flags))
        (duration-p (some (lambda (flag)
                            (member flag '(:duration-clock :duration-decimal
                                           :duration-minute)))
                          flags))
        (unit-mode-p (member :units flags))
        (calculation-precision (table-calc-effective-precision flags limits)))
    (loop while frames
          for frame = (pop frames)
          for node = (car frame)
          for kind = (table-calc-expression-kind node)
          do (if (eq (cdr frame) :finish)
                 (case kind
                   (:unary
                    (let ((value (pop values))
                          (operator (table-calc-expression-value node)))
                      (push (cond
                              ((eq value +table-calc-nan+) value)
                              ((char= operator #\+)
                               (if (or (safe-decimal-p value) (numberp value))
                                   value
                                   (table-calc-evaluation-error
                                    (if (member :symbolic flags)
                                        :unsupported-mode :invalid-number)
                                    target)))
                              ((safe-decimal-p value)
                               (table-calc-decimal-negate value))
                              ((numberp value) (- value))
                              (t (table-calc-evaluation-error
                                  (if (member :symbolic flags)
                                      :unsupported-mode :invalid-number)
                                  target)))
                            values)))
                   (:binary
                    (let ((right (pop values))
                          (left (pop values)))
                      (unless (and (or (safe-decimal-p left) (numberp left)
                                       (eq left +table-calc-nan+))
                                   (or (safe-decimal-p right) (numberp right)
                                       (eq right +table-calc-nan+)))
                        (table-calc-evaluation-error
                         (if (member :symbolic flags)
                             :unsupported-mode :invalid-number)
                         target))
                      (push (table-calc-decimal-binary
                             (table-calc-expression-value node) left right
                             limits target calculation-precision)
                            values)))
                   (:comparison
                    (let ((right (pop values))
                          (left (pop values)))
                      (push (table-calc-compare-values
                             (table-calc-expression-value node) left right target)
                            values)))
                   (:logical-or
                    (let ((right (pop values))
                          (left (pop values)))
                      (unless (and (member left '(t nil))
                                   (member right '(t nil)))
                        (table-calc-evaluation-error :invalid-value target))
                      (push (or left right) values)))
                   (:function
                    (let* ((name (table-calc-expression-value node))
                           (argument-count
                             (length (table-calc-expression-children node)))
                           (arguments
                             (nreverse (loop repeat argument-count
                                             collect (pop values)))))
                      (push (table-calc-function-value
                             name arguments flags limits budget target functions
                             cell-value-function mode duration-p unit-mode-p
                             calculation-precision)
                            values))))
                 (progn
                   (table-calc-charge-work limits budget 1)
                   (case kind
                     (:number
                      (let ((value (table-calc-expression-value node)))
                        (push (if fraction-p (safe-decimal-rational value) value)
                              values)))
                     (:duration
                      (let ((value (table-calc-expression-value node)))
                        (push (if fraction-p value
                                  (%make-safe-decimal :coefficient value :exponent 0
                                                      :negative-zero-p nil))
                              values)))
                     ((:string :symbol)
                      (push (table-calc-expression-value node) values))
                     (:reference
                      (push (table-calc-reference-value
                             (table-calc-expression-value node)
                             reference-position-function cell-value-function mode
                             limits budget target :fraction-p fraction-p
                             :duration-p duration-p :unit-mode-p unit-mode-p
                             :reference-value-function reference-value-function)
                            values))
                     (:range
                      (push (table-calc-resolve-range
                             node reference-position-function limits budget target)
                            values))
                     (:unary
                      (push (cons node :finish) frames)
                      (push (cons (first (table-calc-expression-children node)) nil)
                            frames))
                     ((:binary :comparison :logical-or)
                      (let ((children (table-calc-expression-children node)))
                        (push (cons node :finish) frames)
                        (push (cons (second children) nil) frames)
                        (push (cons (first children) nil) frames)))
                     (:function
                      (let ((name (table-calc-expression-value node))
                            (children (table-calc-expression-children node)))
                        (cond
                          ((string= name "remote")
                           (unless remote-function
                             (table-calc-evaluation-error :unknown-remote-table target))
                           (let* ((table-node (first children))
                                  (table-name (table-calc-expression-value table-node))
                                  (reference-node (second children))
                                  (value (funcall remote-function table-name
                                                  reference-node current-row
                                                  current-column)))
                             (table-calc-charge-work
                              limits budget
                              (if (eq (table-calc-expression-kind reference-node) :range)
                                  (if (and (or (listp value)
                                               (and (vectorp value)
                                                    (not (stringp value))))
                                           value)
                                      (length value)
                                      1)
                                  1))
                             (push
                              (if (eq (table-calc-expression-kind reference-node) :range)
                                  (if (or (listp value)
                                          (and (vectorp value)
                                               (not (stringp value))))
                                      (%make-table-calc-remote-range-value
                                       (coerce value 'list))
                                      (table-calc-evaluation-error
                                       :invalid-function-result target))
                                  (table-calc-coerce-reference-value
                                   value limits fraction-p duration-p unit-mode-p
                                   target))
                              values)))
                          ((string= name "if")
                           (let* ((condition
                                    (table-calc-evaluate-expression
                                     (first children) reference-position-function
                                     cell-value-function mode limits budget target
                                     :flags flags :functions functions
                                     :reference-value-function reference-value-function
                                     :remote-function remote-function
                                     :current-row current-row
                                     :current-column current-column)))
                             (unless (or (member condition '(t nil))
                                         (safe-decimal-p condition)
                                         (realp condition))
                               (table-calc-evaluation-error :invalid-value target))
                             (when (or (safe-decimal-p condition) (realp condition))
                               (setf condition
                                     (not (zerop
                                           (table-calc-number-rational condition)))))
                             (push
                              (table-calc-evaluate-expression
                               (if condition (second children) (third children))
                               reference-position-function cell-value-function mode
                               limits budget target :flags flags :functions functions
                               :reference-value-function reference-value-function
                               :remote-function remote-function
                               :current-row current-row :current-column current-column)
                              values)))
                          ((string= name "taylor")
                           (table-calc-charge-work limits budget
                                                   (length children))
                           (push (table-calc-taylor-number
                                  node reference-position-function cell-value-function
                                  mode limits budget target flags functions
                                  reference-value-function remote-function
                                  current-row current-column)
                                 values))
                          (t
                           (push (cons node :finish) frames)
                           (dolist (child (reverse children))
                             (push (cons child nil) frames))))))
                     (otherwise
                      (table-calc-evaluation-error :unsupported-mode target))))))
    (car values)))

(defun table-calc-decimal-string (decimal)
  (let* ((coefficient (safe-decimal-coefficient decimal))
         (exponent (safe-decimal-exponent decimal))
         (negative-p (table-calc-decimal-negative-p decimal)))
    (if (zerop coefficient)
        (if negative-p "-0" "0")
        (let* ((digits (write-to-string (abs coefficient) :base 10 :radix nil))
               (point (+ (length digits) exponent))
               (sign (if negative-p "-" "")))
          (cond
            ((>= point (length digits))
             (concatenate 'string sign digits
                          (make-string (- point (length digits)) :initial-element #\0)))
            ((plusp point)
             (concatenate 'string sign (subseq digits 0 point) "."
                          (subseq digits point)))
            (t
             (concatenate 'string sign "0."
                          (make-string (- point) :initial-element #\0)
                          digits)))))))

(defun table-calc-fixed-decimal-string (decimal places)
  (let* ((coefficient (safe-decimal-coefficient decimal))
         (exponent (safe-decimal-exponent decimal))
         (negative-p (table-calc-decimal-negative-p decimal))
         (scale (+ exponent places))
         (rounded
           (cond
             ((zerop coefficient) "0")
             ((>= scale 0)
              (concatenate 'string
                           (write-to-string (abs coefficient) :base 10 :radix nil)
                           (make-string scale :initial-element #\0)))
             (t
              (let* ((digits (table-calc-decimal-digit-count coefficient))
                     (discarded (- scale))
                     (magnitude
                       (if (> discarded digits)
                           0
                           (table-calc-round-integer
                            (abs coefficient) (expt 10 discarded)))))
                (write-to-string magnitude :base 10 :radix nil)))))
         (padded (if (< (length rounded) (1+ places))
                     (concatenate 'string
                                  (make-string (- (1+ places) (length rounded))
                                               :initial-element #\0)
                                  rounded)
                     rounded))
         (sign (if negative-p "-" "")))
    (if (zerop places)
        (concatenate 'string sign padded)
        (let ((point (- (length padded) places)))
          (concatenate 'string sign (subseq padded 0 point) "."
                       (subseq padded point))))))

(defun table-calc-rational-string (value)
  (if (integerp value)
      (write-to-string value :base 10 :radix nil)
      (format nil "~A:~A" (numerator value) (denominator value))))

(defun table-calc-rounded-seconds (value)
  (let ((rational (table-calc-number-rational value)))
    (if (integerp rational)
        rational
        (let ((rounded (table-calc-round-integer
                        (abs (numerator rational)) (denominator rational))))
          (if (minusp rational) (- rounded) rounded)))))

(defun table-calc-duration-string (value mode limits)
  (let* ((seconds (table-calc-rounded-seconds value))
         (negative-p (minusp seconds))
         (absolute (abs seconds))
         (sign (if negative-p "-" ""))
         (hours (floor absolute 3600))
         (minutes (floor (mod absolute 3600) 60))
         (remainder (mod absolute 60)))
    (case mode
      (:duration-clock
       (format nil "~A~2,'0D:~2,'0D:~2,'0D"
               sign hours minutes remainder))
      (:duration-minute
       (format nil "~A~2,'0D:~2,'0D" sign hours minutes))
      (:duration-decimal
       (table-calc-fixed-decimal-string
        (table-calc-number-to-decimal (/ (table-calc-number-rational value) 3600)
                                      limits)
        2)))))

(defun table-calc-significant-decimal-string (value precision limits)
  (let* ((decimal (table-calc-number-to-decimal value limits))
         (rounded
           (table-calc-round-decimal
            (safe-decimal-coefficient decimal)
            (safe-decimal-exponent decimal)
            (safe-decimal-negative-zero-p decimal)
            precision limits (safe-decimal-float-p decimal)))
         (text (table-calc-decimal-string rounded)))
    (when (and (safe-decimal-float-p rounded)
               (not (find #\. text)))
      (setf text (concatenate 'string text ".")))
    text))

(defun table-calc-number-base10-order (value limits)
  (let* ((decimal (table-calc-number-to-decimal value limits))
         (coefficient (abs (safe-decimal-coefficient decimal))))
    (if (zerop coefficient)
        0
        (1- (+ (table-calc-decimal-digit-count coefficient)
               (safe-decimal-exponent decimal))))))

(defun table-calc-scientific-string
    (value precision engineering-p limits &optional format-specifier-p)
  (let* ((number (table-calc-number-rational value))
         (order (table-calc-number-base10-order number limits))
         (exponent (if engineering-p (* 3 (floor order 3)) order))
         (scaled (/ number (expt 10 exponent)))
         (mantissa
           (if format-specifier-p
               (table-calc-fixed-decimal-string
                (table-calc-number-to-decimal scaled limits) precision)
               (table-calc-significant-decimal-string
                scaled precision limits)))
         (exponent-text (write-to-string (abs exponent) :base 10 :radix nil)))
    (if format-specifier-p
        (format nil "~Ae~A~A~A" mantissa
                (if (minusp exponent) "-" "+")
                (if (< (length exponent-text) 2) "0" "") exponent-text)
        (format nil "~Ae~A~A" mantissa
                (if (minusp exponent) "-" "") exponent-text))))

(defun table-calc-format-result (value format-precision flags limits target)
  (when (eq value +table-calc-nan+)
    (if (eql format-precision -1)
        (return-from table-calc-format-result "")
        (table-calc-evaluation-error :non-finite-result target)))
  (when (stringp value)
    (return-from table-calc-format-result value))
  (unless (or (safe-decimal-p value) (numberp value))
    (table-calc-evaluation-error :unsupported-mode target))
  (let* ((duration-mode (find-if (lambda (flag)
                                   (member flag '(:duration-clock
                                                  :duration-decimal
                                                  :duration-minute)))
                                 flags))
         (decimal (table-calc-number-to-decimal value limits))
         (float-result-p (safe-decimal-float-p decimal))
         (fraction-p (member :fraction flags))
         (precision (or (and float-result-p
                             (table-calc-flag-value :display-normal flags))
                        (min 8 (limit-value limits :max-formula-precision))))
         (fixed-precision (table-calc-flag-value :display-fixed flags))
         (scientific-precision (table-calc-flag-value :display-sci flags))
         (engineering-precision (table-calc-flag-value :display-eng flags)))
    (check-limit limits :max-formula-precision
                 (table-calc-decimal-digit-count
                  (safe-decimal-coefficient decimal)))
    (when duration-mode
      (return-from table-calc-format-result
        (table-calc-duration-string value duration-mode limits)))
    (when format-precision
      (return-from table-calc-format-result
        (cond
          ((eql format-precision -1)
           (table-calc-fixed-decimal-string
            (table-calc-number-to-decimal value limits) 1))
          ((consp format-precision)
           (table-calc-scientific-string
            value (cdr format-precision) nil limits t))
          (t
           (table-calc-fixed-decimal-string
            (table-calc-number-to-decimal value limits) format-precision)))))
    (when (and float-result-p fixed-precision)
      (return-from table-calc-format-result
        (table-calc-fixed-decimal-string
         (table-calc-number-to-decimal value limits) fixed-precision)))
    (when (and float-result-p scientific-precision)
      (return-from table-calc-format-result
        (table-calc-scientific-string value scientific-precision nil limits)))
    (when (and float-result-p engineering-precision)
      (return-from table-calc-format-result
        (table-calc-scientific-string value engineering-precision t limits)))
    (when fraction-p
      (return-from table-calc-format-result
        (table-calc-rational-string (table-calc-number-rational value))))
    (if float-result-p
        (let ((order (table-calc-number-base10-order value limits)))
          (if (or (< order -2) (>= order 12))
              (table-calc-scientific-string value precision nil limits)
              (table-calc-significant-decimal-string value precision limits)))
        (table-calc-decimal-string decimal))))

(defun table-calc-evaluate-assignment
    (assignment reference-position-function cell-value-function
     &key (limits (make-limits)) work-budget functions
       reference-value-function remote-function current-row current-column
       (callbacks (make-callback-registry)) elisp-reference-function)
  "Evaluate one assignment using absolute-position and cell-string callbacks.
REFERENCE-POSITION-FUNCTION maps a reference to absolute 1-based row and column
values. CELL-VALUE-FUNCTION maps those indices to the raw cell string. Reuse
WORK-BUDGET across assignments in one table recalculation."
  (let* ((budget (or work-budget (make-table-calc-work-budget)))
         (target (table-calc-assignment-target-description assignment)))
    (table-calc-validate-assignment-target assignment target)
    (let* ((expression (table-calc-assignment-expression assignment))
           (value
             (if (eq (table-calc-expression-kind expression) :quoted-lisp)
                 (let* ((source (table-calc-expression-value expression))
                        (expanded (if elisp-reference-function
                                      (funcall elisp-reference-function
                                               source expression assignment)
                                      source))
                        (result (evaluate-elisp callbacks expanded)))
                   (unless (or (stringp result) (realp result))
                     (table-calc-evaluation-error :invalid-function-result target))
                   (when (stringp result)
                     (check-limit limits :max-output-length (length result)))
                   result)
                 (table-calc-evaluate-expression
                  expression reference-position-function cell-value-function
                  (table-calc-assignment-mode assignment) limits budget target
                  :flags (table-calc-assignment-flags assignment)
                  :functions functions
                  :reference-value-function reference-value-function
                  :remote-function remote-function
                  :current-row current-row :current-column current-column))))
      (table-calc-format-result
       value (table-calc-assignment-format-precision assignment)
       (table-calc-assignment-flags assignment) limits target))))

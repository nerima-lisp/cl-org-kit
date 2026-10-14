;;;; src/table-calc-eval.lisp
(in-package #:org-kit)

(defstruct (table-calc-work-budget
             (:constructor %make-table-calc-work-budget ()))
  (used 0))

(defstruct (table-calc-range-value
             (:constructor %make-table-calc-range-value
                 (first-row last-row first-column last-column)))
  first-row last-row first-column last-column)

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
       ((eql row-kind :hline-anchor)
        (format nil "@~A$~A" (if (= row-value 1) "I" "II") column-value))
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

(defun table-calc-round-decimal (coefficient exponent negative-zero-p precision limits)
  (let ((negative-p (or (minusp coefficient)
                        (and (zerop coefficient) negative-zero-p))))
    (when (zerop coefficient)
      (check-limit limits :max-formula-precision 1)
      (return-from table-calc-round-decimal
        (%make-safe-decimal :coefficient 0 :exponent 0
                            :negative-zero-p negative-p)))
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
                        :negative-zero-p nil)))

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
                             (not (safe-decimal-negative-zero-p decimal))))))

(defun table-calc-decimal-add (left right limits)
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
           (limit-value limits :max-formula-precision) limits)))
      (return-from table-calc-decimal-add right))
    (when (zerop right-coefficient)
      (return-from table-calc-decimal-add left))
    (let* ((left-order (+ (table-calc-decimal-digit-count left-coefficient)
                          left-exponent -1))
           (right-order (+ (table-calc-decimal-digit-count right-coefficient)
                           right-exponent -1))
           (precision (limit-value limits :max-formula-precision)))
      (when (> (abs (- left-order right-order)) precision)
        (return-from table-calc-decimal-add
          (if (> (safe-decimal-magnitude-compare left right) 0) left right)))
      (let* ((exponent (min left-exponent right-exponent))
             (left-shift (- left-exponent exponent))
             (right-shift (- right-exponent exponent))
             (coefficient (+ (* left-coefficient (expt 10 left-shift))
                             (* right-coefficient (expt 10 right-shift)))))
        (table-calc-round-decimal
         coefficient exponent nil precision limits)))))

(defun table-calc-factor-count (integer factor)
  (loop with value = (abs integer)
        for count from 0
        while (zerop (mod value factor))
        do (setf value (truncate value factor))
        finally (return count)))

(defun table-calc-product-decimal-zeroes (left right)
  (min (+ (table-calc-factor-count left 2)
          (table-calc-factor-count right 2))
       (+ (table-calc-factor-count left 5)
          (table-calc-factor-count right 5))))

(defun table-calc-decimal-multiply (left right limits)
  (let* ((left-coefficient (safe-decimal-coefficient left))
         (right-coefficient (safe-decimal-coefficient right))
         (negative-p (not (eq (table-calc-decimal-negative-p left)
                              (table-calc-decimal-negative-p right))))
         (precision (limit-value limits :max-formula-precision)))
    (when (or (zerop left-coefficient) (zerop right-coefficient))
      (return-from table-calc-decimal-multiply
        (table-calc-round-decimal 0 0 negative-p precision limits)))
    (let* ((exponent (+ (safe-decimal-exponent left)
                        (safe-decimal-exponent right)))
           (normalized-exponent
             (+ exponent
                (table-calc-product-decimal-zeroes left-coefficient
                                                   right-coefficient))))
      (check-limit limits :max-formula-exponent (abs normalized-exponent))
      (table-calc-round-decimal (* left-coefficient right-coefficient)
                                exponent nil precision limits))))

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

(defun table-calc-decimal-divide (numerator denominator limits target)
  (let* ((numerator-coefficient (safe-decimal-coefficient numerator))
         (denominator-coefficient (safe-decimal-coefficient denominator))
         (precision (limit-value limits :max-formula-precision))
         (maximum-exponent (limit-value limits :max-formula-exponent)))
    (when (zerop denominator-coefficient)
      (table-calc-evaluation-error :division-by-zero target))
    (when (zerop numerator-coefficient)
      (return-from table-calc-decimal-divide
        (table-calc-round-decimal
         0 0 (not (eq (table-calc-decimal-negative-p numerator)
                      (table-calc-decimal-negative-p denominator)))
         precision limits)))
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
                                  precision limits)))))

(defun table-calc-decimal-binary (operator left right limits target)
  (cond
    ((or (eq left +table-calc-nan+) (eq right +table-calc-nan+))
     +table-calc-nan+)
    ((char= operator #\+)
     (table-calc-decimal-add left right limits))
    ((char= operator #\-)
     (table-calc-decimal-add left (table-calc-decimal-negate right) limits))
    ((char= operator #\*)
     (table-calc-decimal-multiply left right limits))
    (t
     (table-calc-decimal-divide left right limits target))))

(defun table-calc-cell-number (raw mode in-range-p limits target)
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
        (let ((decimal (parse-safe-decimal raw :start start :end (1+ end)
                                           :limits limits)))
          (or decimal
              (case mode
                ((:n :en)
                 (%make-safe-decimal :coefficient 0 :exponent 0
                                     :negative-zero-p nil))
                (otherwise
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
      (multiple-value-bind (last-row last-column)
          (table-calc-reference-position
           last-reference reference-position-function target)
        (%make-table-calc-range-value
         (min first-row last-row) (max first-row last-row)
         (min first-column last-column) (max first-column last-column))))))

(defun table-calc-evaluate-vector (function range mode cell-value-function
                                   limits budget target)
  (let* ((first-row (table-calc-range-value-first-row range))
         (last-row (table-calc-range-value-last-row range))
         (first-column (table-calc-range-value-first-column range))
         (last-column (table-calc-range-value-last-column range))
         (row-count (1+ (- last-row first-row)))
         (column-count (1+ (- last-column first-column)))
         (element-count (* row-count column-count))
         (count 0)
         (sum (%make-safe-decimal :coefficient 0 :exponent 0
                                  :negative-zero-p nil))
         (minimum nil)
         (maximum nil)
         (nan-p nil))
    (table-calc-charge-work limits budget (* 2 element-count))
    (loop for row from first-row to last-row
          do (loop for column from first-column to last-column
                   for raw = (funcall cell-value-function row column)
                   for value = (table-calc-cell-number raw mode t limits target)
                   unless (eq value +table-calc-skip+)
                     do (incf count)
                        (cond
                          ((eq value +table-calc-nan+)
                           (setf nan-p t))
                          ((eql function :vsum)
                           (setf sum (table-calc-decimal-add sum value limits)))
                          ((eql function :vmean)
                           (setf sum (table-calc-decimal-add sum value limits)))
                          ((eql function :vmin)
                           (when (or (null minimum)
                                     (minusp (safe-decimal-compare value minimum)))
                             (setf minimum value)))
                          ((eql function :vmax)
                           (when (or (null maximum)
                                     (plusp (safe-decimal-compare value maximum)))
                             (setf maximum value))))))
    (case function
      (:vcount
       (table-calc-round-decimal count 0 nil
                                 (limit-value limits :max-formula-precision) limits))
      ((:vsum :vmean :vmin :vmax)
       (when nan-p
         (return-from table-calc-evaluate-vector +table-calc-nan+))
       (case function
         (:vsum sum)
         (:vmean
          (when (zerop count)
            (table-calc-evaluation-error :empty-range target))
          (table-calc-decimal-divide
           sum (%make-safe-decimal :coefficient count :exponent 0
                                   :negative-zero-p nil)
           limits target))
         (:vmin
          (or minimum (table-calc-evaluation-error :empty-range target)))
         (:vmax
          (or maximum (table-calc-evaluation-error :empty-range target))))))))

(defun table-calc-reference-value (reference reference-position-function
                                   cell-value-function mode limits budget target)
  (table-calc-charge-work limits budget 1)
  (multiple-value-bind (row column)
      (table-calc-reference-position
       reference reference-position-function target)
    (table-calc-cell-number (funcall cell-value-function row column)
                            mode nil limits target)))

(defun table-calc-evaluate-expression (expression reference-position-function
                                      cell-value-function mode limits budget target)
  (let ((frames (list (cons expression nil)))
        (values '()))
    (loop while frames
          for frame = (pop frames)
          for node = (car frame)
          for kind = (table-calc-expression-kind node)
          do (if (eq (cdr frame) :finish)
                 (case kind
                   (:unary
                    (let ((value (pop values)))
                      (push (if (char= (table-calc-expression-value node) #\-)
                                (if (eq value +table-calc-nan+)
                                    value
                                    (table-calc-decimal-negate value))
                                value)
                            values)))
                   (:binary
                    (let ((right (pop values))
                          (left (pop values)))
                      (push (table-calc-decimal-binary
                             (table-calc-expression-value node) left right
                             limits target)
                            values)))
                   (:function
                    (push (table-calc-evaluate-vector
                           (table-calc-expression-value node) (pop values) mode
                           cell-value-function limits budget target)
                          values)))
                 (progn
                   (table-calc-charge-work limits budget 1)
                   (case kind
                     (:number
                      (push (table-calc-expression-value node) values))
                     (:reference
                      (push (table-calc-reference-value
                             (table-calc-expression-value node)
                             reference-position-function cell-value-function mode
                             limits budget target)
                            values))
                     (:range
                      (push (table-calc-resolve-range
                             node reference-position-function limits budget target)
                            values))
                     (:unary
                      (push (cons node :finish) frames)
                      (push (cons (first (table-calc-expression-children node)) nil)
                            frames))
                     (:binary
                      (let ((children (table-calc-expression-children node)))
                        (push (cons node :finish) frames)
                        (push (cons (second children) nil) frames)
                        (push (cons (first children) nil) frames)))
                     (:function
                      (push (cons node :finish) frames)
                      (push (cons (first (table-calc-expression-children node)) nil)
                            frames))))))
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

(defun table-calc-format-result (value format-precision limits target)
  (when (eq value +table-calc-nan+)
    (table-calc-evaluation-error :non-finite-result target))
  (if format-precision
      (table-calc-fixed-decimal-string value format-precision)
      (table-calc-decimal-string
       (table-calc-round-decimal
        (safe-decimal-coefficient value)
        (safe-decimal-exponent value)
        (safe-decimal-negative-zero-p value)
        (min 8 (limit-value limits :max-formula-precision))
        limits))))

(defun table-calc-evaluate-assignment
    (assignment reference-position-function cell-value-function
     &key (limits (make-limits)) work-budget)
  "Evaluate one assignment using absolute-position and cell-string callbacks.
REFERENCE-POSITION-FUNCTION maps a reference to absolute 1-based row and column
values. CELL-VALUE-FUNCTION maps those indices to the raw cell string. Reuse
WORK-BUDGET across assignments in one table recalculation."
  (let* ((budget (or work-budget (make-table-calc-work-budget)))
         (target (table-calc-assignment-target-description assignment)))
    (table-calc-validate-assignment-target assignment target)
    (let ((value (table-calc-evaluate-expression
                  (table-calc-assignment-expression assignment)
                  reference-position-function cell-value-function
                  (table-calc-assignment-mode assignment) limits budget target)))
      (table-calc-format-result
       value (table-calc-assignment-format-precision assignment) limits target))))

;;;; src/effort.lisp
(in-package #:org-kit)

(define-condition org-effort-error (org-kit-error)
  ((reason :initarg :reason :initform nil))
  (:report (lambda (condition stream)
             (format stream "~A~@[ (~(~A~))~]"
                     (org-kit-error-message condition)
                     (org-effort-error-reason condition))))
  (:documentation "Signalled when an effort duration or subtree target is invalid.
REASON classifies the failure."))

(defun org-effort-error-reason (condition)
  "Return the keyword classifying why the effort operation failed."
  (slot-value condition 'reason))

(defun %org-effort-error (reason)
  (error 'org-effort-error
         :message (ecase reason
                    (:invalid-duration "Invalid effort duration")
                    (:foreign-node "Effort heading does not belong to DOCUMENT")
                    (:wrong-node-type "Effort target is not a heading"))
         :reason reason))

(defun effort-horizontal-space-p (character)
  (find character '(#\Space #\Tab)))

(defun effort-skip-horizontal-space (text position end)
  (loop while (and (< position end)
                   (effort-horizontal-space-p (char text position)))
        do (incf position)
        finally (return position)))

(defun effort-digit-p (character)
  (<= 48 (char-code character) 57))

(defun effort-digits-p (text start end)
  (and (< start end)
       (loop for position from start below end
             always (effort-digit-p (char text position)))))

(defun effort-number-at (text start end)
  (let ((integer-end
          (loop for position from start below end
                while (effort-digit-p (char text position))
                finally (return position))))
    (when (> integer-end start)
      (let* ((dot-p (and (< integer-end end)
                         (char= (char text integer-end) #\.)))
             (fraction-start (and dot-p (1+ integer-end)))
             (number-end
               (if dot-p
                   (loop for position from fraction-start below end
                         while (effort-digit-p (char text position))
                         finally (return position))
                   integer-end)))
        (values
         (if dot-p
             (let* ((fraction-digits (- number-end fraction-start))
                    (denominator (expt 10 fraction-digits))
                    (whole (parse-integer text :start start :end integer-end))
                    (fraction (if (plusp fraction-digits)
                                  (parse-integer text :start fraction-start
                                                      :end number-end)
                                  0)))
               (/ (+ (* whole denominator) fraction) denominator))
             (parse-integer text :start start :end integer-end))
         number-end
         t)))))

(defun effort-clock-minutes (text start end)
  (let* ((first-colon (position #\: text :start start :end end))
         (second-colon (and first-colon
                            (position #\: text :start (1+ first-colon) :end end)))
         (third-colon (and second-colon
                           (position #\: text :start (1+ second-colon) :end end)))
         (minute-start (and first-colon (1+ first-colon)))
         (minute-end (or second-colon end)))
    (when (and first-colon
               (null third-colon)
               (effort-digits-p text start first-colon)
               (= (- minute-end minute-start) 2)
               (effort-digits-p text minute-start minute-end)
               (or (null second-colon)
                   (and (= (- end (1+ second-colon)) 2)
                        (effort-digits-p text (1+ second-colon) end))))
      (let ((hours (parse-integer text :start start :end first-colon))
            (minutes (parse-integer text :start minute-start :end minute-end))
            (seconds (if second-colon
                         (parse-integer text :start (1+ second-colon) :end end)
                         0)))
        (+ (* 60 hours) minutes (/ seconds 60))))))

(defun effort-duration-unit-minutes (unit)
  (cond ((string= unit "min") 1)
        ((string= unit "h") 60)
        ((string= unit "d") 1440)
        ((string= unit "w") 10080)
        ((string= unit "m") 43200)
        ((string= unit "y") 525960)
        (t nil)))

(defun effort-unit-duration (text start end)
  (let ((position (effort-skip-horizontal-space text start end))
        (minutes 0)
        (found-p nil))
    (when (and (< position end)
               (find (char text position) '(#\+ #\-)))
      (return-from effort-unit-duration (values nil nil)))
    (loop do (setf position (effort-skip-horizontal-space text position end))
          while (< position end)
          do (multiple-value-bind (number number-end number-p)
                 (effort-number-at text position end)
               (unless number-p
                 (return-from effort-unit-duration (values nil nil)))
               (let* ((unit-start
                        (effort-skip-horizontal-space text number-end end))
                      (unit-end
                        (loop for cursor from unit-start below end
                              while (let ((code (char-code (char text cursor))))
                                      (<= 97 code 122))
                              finally (return cursor)))
                      (unit (subseq text unit-start unit-end))
                      (modifier (effort-duration-unit-minutes unit)))
                 (unless modifier
                   (return-from effort-unit-duration (values nil nil)))
                 (setf found-p t)
                 (incf minutes (* number modifier))
                 (setf position unit-end))))
    (values minutes found-p)))

(defun effort-trim-bounds (text start end)
  (let ((left (effort-skip-horizontal-space text start end))
        (right end))
    (loop while (and (> right left)
                     (effort-horizontal-space-p (char text (1- right))))
          do (decf right))
    (values left right)))

(defun effort-clock-start (text colon start)
  (loop with position = colon
        while (and (> position start)
                   (effort-digit-p (char text (1- position))))
        do (decf position)
        finally (return position)))

(defun effort-parse-duration (text)
  (when (zerop (length text))
    (return-from effort-parse-duration (values 0 t)))
  (multiple-value-bind (start end) (effort-trim-bounds text 0 (length text))
    (when (= start end)
      (return-from effort-parse-duration (values nil nil)))
    (let ((colon (position #\: text :start start :end end)))
      (when colon
        (let* ((clock-start (effort-clock-start text colon start))
               (clock-minutes (effort-clock-minutes text clock-start end)))
          (when clock-minutes
            (if (= clock-start start)
                (return-from effort-parse-duration (values clock-minutes t))
                (multiple-value-bind (unit-minutes unit-p)
                    (effort-unit-duration text start clock-start)
                  (when unit-p
                    (return-from effort-parse-duration
                      (values (+ unit-minutes clock-minutes) t))))))))
      (unless colon
        (multiple-value-bind (number number-end number-p)
            (effort-number-at text start end)
          (when (and number-p (= number-end end))
            (return-from effort-parse-duration (values number t)))))
      (effort-unit-duration text start end))))

(defun effort-duration-minutes (duration &key (limits (make-limits)))
  "Parse an Org effort DURATION and return its exact number of minutes.

DURATION accepts H:MM, H:MM:SS, unit forms using min, h, d, w, m, and y,
mixed unit and clock forms, and bare numbers interpreted as minutes. LIMITS
bounds the input length. Fractional minutes are returned as exact rationals."
  (unless (limits-p limits)
    (error 'type-error :datum limits :expected-type 'limits))
  (check-type duration string)
  (check-limit limits :max-input-length (length duration))
  (multiple-value-bind (minutes valid-p) (effort-parse-duration duration)
    (if valid-p
        minutes
        (%org-effort-error :invalid-duration))))

(defun effort-check-heading (document heading)
  (check-type document document)
  (check-type heading node)
  (unless (eq (node-document heading) document)
    (%org-effort-error :foreign-node))
  (unless (member (node-type heading) '(:headline :inlinetask) :test #'eq)
    (%org-effort-error :wrong-node-type))
  heading)

(defun effort-check-document-node-count (document)
  (let ((count 0)
        (root (document-root document)))
    (when root
      (map-nodes (lambda (node)
                   (declare (ignore node))
                   (incf count)
                   (check-limit (document-limits document)
                                :max-node-count count))
                 root))
    count))

(defun subtree-effort-minutes (document heading)
  "Return the sum of direct EFFORT properties in HEADING's subtree.

Each heading's own property contributes once; inherited values are not repeated.
Effort strings use DOCUMENT's input-length limit."
  (effort-check-heading document heading)
  (effort-check-document-node-count document)
  (let ((total 0)
        (limits (document-limits document)))
    (map-nodes
     (lambda (node)
       (when (member (node-type node) '(:headline :inlinetask) :test #'eq)
         (multiple-value-bind (value found-p)
             (effective-heading-property-value node "EFFORT")
           (when found-p
             (incf total (effort-duration-minutes value :limits limits))))))
     heading)
    total))

;;;; src/table-recalculate.lisp
(in-package #:org-kit)

(defstruct table-recalculate-entry assignment row column owner)
(defstruct table-recalculate-state
  standard-rows data-rows header-row hline-rows values column-count header-columns)
(defstruct table-recalculate-context
  document tables cache active functions callbacks constants constants-declared-p
  limits work-budget (remote-cache (make-hash-table :test #'eq)))

(defun table-recalculate-signal-error (target reason message)
  (error 'org-table-error
         :message message
         :target (edit-target-description target)
         :reason reason))

(defun table-recalculate-row-cells (row limits work-budget)
  (table-calc-charge-work limits work-budget 1)
  (let ((cells (make-array 0 :adjustable t :fill-pointer 0)))
    (dolist (cell (node-children row) cells)
      (table-calc-charge-work limits work-budget 1)
      (vector-push-extend cell cells))))

(defun table-recalculate-standard-rows (table limits work-budget)
  (let ((rows (make-array 0 :adjustable t :fill-pointer 0))
        (hline-rows (make-array 0 :adjustable t :fill-pointer 0))
        (after-first-rule nil)
        (header nil)
        (row-index 0)
        (data-rows nil))
    (dolist (row (node-children table))
      (table-calc-charge-work limits work-budget 1)
      (cond
        ((eq (node-property row :type) :rule)
         (table-calc-charge-work limits work-budget 1)
         (vector-push-extend (1+ row-index) hline-rows)
         (unless after-first-rule
           (setf after-first-rule t
                 data-rows (make-array 0 :adjustable t :fill-pointer 0))))
        (t
         (incf row-index)
         (unless after-first-rule
           (unless header
             (setf header row)))
         (let ((cells (table-recalculate-row-cells row limits work-budget)))
           (table-calc-charge-work limits work-budget 1)
           (vector-push-extend cells rows))
         (when after-first-rule
           (table-calc-charge-work limits work-budget 1)
           (vector-push-extend row-index data-rows)))))
    ;; NIL denotes a table without a rule, whose rows are all data rows.
    (unless after-first-rule
      (setf header nil))
    (values rows data-rows header hline-rows)))

(defun table-recalculate-cell (standard-rows row column)
  (let ((row-node
          (and (integerp row)
               (<= 1 row (length standard-rows))
               (aref standard-rows (1- row)))))
    (when (and row-node
               (integerp column)
               (<= 1 column (length row-node)))
      (aref row-node (1- column)))))

(defun table-recalculate-cell-value (standard-rows row column)
  (let ((cell (table-recalculate-cell standard-rows row column)))
    (if cell (node-property cell :value) "")))

(defun table-recalculate-header-columns (header-row &optional limits work-budget)
  (let ((columns (make-hash-table :test #'equal)))
    (when header-row
      (loop for cell in (node-children header-row)
            for column from 1
            for name = (node-property cell :value)
            do (when limits
                 (table-calc-charge-work limits work-budget 1))
               (setf (gethash name columns)
                     (if (gethash name columns) :ambiguous column))))
    columns))

(defun table-recalculate-named-column (name header-columns target)
  (multiple-value-bind (column present) (gethash name header-columns)
    (unless present
      (table-recalculate-signal-error
       target :unknown-column-name "Formula references an unknown column name"))
    (when (eq column :ambiguous)
      (table-recalculate-signal-error
       target :ambiguous-column-name "Formula references an ambiguous column name"))
    column))

(defun table-recalculate-hline-row (hline-rows ordinal target)
  (unless (and (integerp ordinal) (<= 1 ordinal (length hline-rows)))
    (table-recalculate-signal-error
     target :invalid-reference "Formula references an unavailable hline"))
  (aref hline-rows (1- ordinal)))

(defun table-recalculate-relative-hline-row
    (hline-rows current-row ordinal direction target)
  (let ((matches
          (loop for row below (length hline-rows)
                for anchor-row = (aref hline-rows row)
                when (if (eq direction :above)
                         (<= anchor-row current-row)
                         (> anchor-row current-row))
                  collect anchor-row)))
    (unless (and (integerp ordinal) (plusp ordinal)
                 (<= ordinal (length matches)))
      (table-recalculate-signal-error
       target :invalid-reference "Formula references an unavailable relative hline"))
    (nth (1- ordinal)
         (if (eq direction :above) (reverse matches) matches))))

(defun table-recalculate-reference-position
    (reference current-row current-column row-count column-count header-columns
     hline-rows target)
  (let* ((row-kind (table-calc-reference-row-kind reference))
         (row-value (table-calc-reference-row-value reference))
         (column-kind (table-calc-reference-column-kind reference))
         (column-value (table-calc-reference-column-value reference))
         (row (case row-kind
                (:current current-row)
                (:first 1)
                (:last row-count)
                (:absolute row-value)
                (:relative
                 (and (integerp row-value) (+ current-row row-value)))
                (:invalid
                 (table-recalculate-signal-error
                  target :invalid-reference "Formula reference has an invalid row"))
                (:hline-anchor
                 (table-recalculate-hline-row hline-rows row-value target))
                (:hline-relative-above
                 (table-recalculate-relative-hline-row
                  hline-rows current-row row-value :above target))
                (:hline-relative-below
                 (table-recalculate-relative-hline-row
                  hline-rows current-row row-value :below target))
                (:hline-following
                 (let ((anchor (table-recalculate-hline-row
                                hline-rows row-value target))
                       (offset (table-calc-reference-row-offset reference)))
                   (unless (and (integerp offset) (plusp offset))
                     (table-recalculate-signal-error
                      target :invalid-reference "Formula has an invalid hline offset"))
                   (+ anchor (1- offset))))
                (otherwise
                 (table-recalculate-signal-error
                  target :invalid-reference
                  "Formula reference has an unsupported row kind"))))
         (column
           (case column-kind
             (:current current-column)
             (:absolute column-value)
             (:relative (and (integerp column-value)
                             (+ current-column column-value)))
             (:name
              (table-recalculate-named-column
               column-value header-columns target))
             (otherwise
              (table-recalculate-signal-error
               target :invalid-reference
               "Formula reference has an unsupported column kind")))))
    (unless (and (integerp row) (<= 1 row row-count)
                 (integerp column) (<= 1 column column-count))
      (table-recalculate-signal-error
       target :invalid-reference "Formula reference is outside the table"))
    (values row column)))

(defun table-recalculate-expression-regions
    (expression current-row current-column row-count column-count header-columns target
     limits work-budget &key hline-rows reference-value-function)
  (loop with pending = (list expression)
        with regions = nil
        while pending
        for node = (pop pending)
        for kind = (table-calc-expression-kind node)
        do (table-calc-charge-work limits work-budget 1)
           (case kind
             (:reference
              (let ((reference (table-calc-expression-value node)))
                (multiple-value-bind (value present-p)
                    (if reference-value-function
                        (funcall reference-value-function reference)
                        (values nil nil))
                  (declare (ignore value))
                  (unless present-p
                    (multiple-value-bind (row column)
                        (table-recalculate-reference-position
                         reference current-row current-column row-count
                         column-count header-columns hline-rows target)
                      (push (list row column row column) regions)))))
                )
             (:range
              (let* ((children (table-calc-expression-children node))
                     (first-reference (table-calc-expression-value (first children)))
                     (last-reference (table-calc-expression-value (second children))))
                (multiple-value-bind (first-row first-column)
                    (table-recalculate-reference-position
                     first-reference current-row current-column row-count column-count
                     header-columns hline-rows target)
                  (multiple-value-bind (last-row last-column)
                      (table-recalculate-reference-position
                       last-reference current-row current-column row-count column-count
                       header-columns hline-rows target)
                    (push (list (min first-row last-row)
                                (min first-column last-column)
                                (max first-row last-row)
                                (max first-column last-column))
                          regions)))))
             (:function
              (unless (string= (table-calc-expression-value node) "remote")
                (dolist (child (table-calc-expression-children node))
                  (push child pending))))
             (otherwise
              (dolist (child (table-calc-expression-children node))
                (push child pending))))
        finally (return (nreverse regions))))

(defun table-recalculate-target-key (row column)
  (cons row column))

(defun table-recalculate-assignment-kind (assignment target)
  (case (table-calc-assignment-kind assignment)
    (:column :column)
    (:field :field)
    (:row :row)
    (:range :range)
    (:invalid
     (table-recalculate-signal-error
      target :invalid-reference "Formula assignment has an invalid target"))
    (otherwise
     (table-recalculate-signal-error
      target :invalid-reference
      "Formula assignment has an unsupported target kind"))))

(defun table-recalculate-target-column (reference header-columns target)
  (case (table-calc-reference-column-kind reference)
    (:absolute (table-calc-reference-column-value reference))
    (:name (table-recalculate-named-column
            (table-calc-reference-column-value reference)
            header-columns target))
    (otherwise
     (table-recalculate-signal-error
      target :invalid-reference "Formula assignment has an invalid column target"))))

(defun table-recalculate-target-position
    (reference row-count column-count header-columns hline-rows target
     &optional assignment-target-p)
  (when (and assignment-target-p
             (not (member (table-calc-reference-column-kind reference)
                          '(:absolute :name))))
    (table-recalculate-signal-error
     target :invalid-reference "Formula assignment has an invalid column target"))
  (multiple-value-bind (row column)
      (table-recalculate-reference-position
       reference 1 1 row-count column-count header-columns hline-rows target)
    (unless (and (integerp row) (<= 1 row row-count)
                 (integerp column) (<= 1 column column-count))
      (table-recalculate-signal-error
       target :invalid-reference "Formula assignment target is outside the table"))
    (values row column)))

(defun table-recalculate-normalize-assignment-target
    (assignment row-count column-count header-columns hline-rows target)
  (let* ((kind (table-recalculate-assignment-kind assignment target))
         (reference (table-calc-assignment-target assignment))
         (row-kind (table-calc-reference-row-kind reference))
         (normalized-assignment (copy-table-calc-assignment assignment)))
    (case kind
      (:column
       (unless (eq row-kind :current)
         (table-recalculate-signal-error
          target :invalid-reference "Column formula has a non-current row target"))
       (let ((column (table-recalculate-target-column
                      reference header-columns target)))
         (unless (and (integerp column) (<= 1 column column-count))
           (table-recalculate-signal-error
            target :invalid-reference "Formula assignment column is outside the table"))
         (setf (table-calc-reference-column-kind
                (table-calc-assignment-target normalized-assignment)) :absolute
               (table-calc-reference-column-value
                (table-calc-assignment-target normalized-assignment)) column)))
      (:field
       (multiple-value-bind (row column)
           (table-recalculate-target-position
            reference row-count column-count header-columns hline-rows target t)
         (let ((normalized-reference
                 (copy-table-calc-reference reference)))
           (setf (table-calc-reference-row-kind normalized-reference) :absolute
                 (table-calc-reference-row-value normalized-reference) row
                 (table-calc-reference-row-offset normalized-reference) nil
                 (table-calc-reference-column-kind normalized-reference) :absolute
                 (table-calc-reference-column-value normalized-reference) column
                 (table-calc-assignment-target normalized-assignment)
                 normalized-reference))))
      (:row
       (multiple-value-bind (row ignored-column)
           (table-recalculate-target-position
            reference row-count column-count header-columns hline-rows target)
         (declare (ignore ignored-column))
         (let ((normalized-reference
                 (copy-table-calc-reference reference)))
           (setf (table-calc-reference-row-kind normalized-reference) :absolute
                 (table-calc-reference-row-value normalized-reference) row
                 (table-calc-reference-row-offset normalized-reference) nil
                 (table-calc-assignment-target normalized-assignment)
                 normalized-reference))))
      (:range
       (let ((end-reference (table-calc-assignment-target-end assignment)))
         (multiple-value-bind (first-row first-column)
             (table-recalculate-target-position
              reference row-count column-count header-columns hline-rows target t)
           (multiple-value-bind (last-row last-column)
               (table-recalculate-target-position
                end-reference row-count column-count header-columns hline-rows target t)
             (let ((first-reference (copy-table-calc-reference reference))
                   (normalized-end (copy-table-calc-reference end-reference)))
               (flet ((normalize (item row column)
                        (setf (table-calc-reference-row-kind item) :absolute
                              (table-calc-reference-row-value item) row
                              (table-calc-reference-row-offset item) nil
                              (table-calc-reference-column-kind item) :absolute
                              (table-calc-reference-column-value item) column)))
                 (normalize first-reference first-row first-column)
                 (normalize normalized-end last-row last-column))
               (setf (table-calc-assignment-target normalized-assignment)
                     first-reference
                     (table-calc-assignment-target-end normalized-assignment)
                     normalized-end)))))))
    normalized-assignment))

(defun table-recalculate-validate-duplicates
    (assignments standard-rows column-count target limits work-budget)
  (let ((columns (make-hash-table))
        (fields (make-hash-table :test #'equal)))
    (dolist (assignment assignments)
      (table-calc-charge-work limits work-budget 1)
      (let* ((kind (table-recalculate-assignment-kind assignment target))
             (reference (table-calc-assignment-target assignment)))
        (case kind
          (:column
           (let ((column (table-calc-reference-column-value reference)))
             (when (gethash column columns)
               (table-recalculate-signal-error
                target :duplicate-target "Column formula target is duplicated"))
             (setf (gethash column columns) t)))
          ((:field :row :range)
           (let* ((end-reference (table-calc-assignment-target-end assignment))
                  (first-row (table-calc-reference-row-value reference))
                  (first-column (table-calc-reference-column-value reference))
                  (last-row (if (eq kind :range)
                                (table-calc-reference-row-value end-reference)
                                first-row))
                  (last-column (case kind
                                 (:row column-count)
                                 (:range
                                  (table-calc-reference-column-value end-reference))
                                 (otherwise first-column)))
                  (row-start (min first-row last-row))
                  (row-end (max first-row last-row))
                  (column-start (if (eq kind :row) 1
                                    (min first-column last-column)))
                  (column-end (if (eq kind :row) column-count
                                  (max first-column last-column))))
             (loop for row from row-start to row-end
                   do (loop for column from column-start to column-end
                            do (table-calc-charge-work limits work-budget 1)
                               (unless (table-recalculate-cell
                                        standard-rows row column)
                                 (table-recalculate-signal-error
                                  target :invalid-reference
                                  "Formula assignment target is outside the table"))
                               (let ((key (table-recalculate-target-key row column)))
                                 (when (gethash key fields)
                                   (table-recalculate-signal-error
                                    target :duplicate-target
                                    "Field formula target is duplicated"))
                                 (setf (gethash key fields) t))))))
          )))))

(defun table-recalculate-plan-column-assignments
    (assignments data-rows standard-rows column-count target limits work-budget)
  (let ((entries nil)
        (owner 0))
    (dolist (assignment assignments)
      (let* ((reference (table-calc-assignment-target assignment))
             (column (table-calc-reference-column-value reference)))
        (unless (and (integerp column) (<= 1 column column-count))
          (table-recalculate-signal-error
           target :invalid-reference "Column formula target is outside the table"))
        (labels ((plan-row (row)
                   (table-calc-charge-work limits work-budget 1)
                   (when (table-recalculate-cell standard-rows row column)
                     (push (make-table-recalculate-entry
                            :assignment assignment :row row :column column :owner owner)
                           entries)
                     (incf owner))))
          (if data-rows
              (loop for row across data-rows do (plan-row row))
              (loop for row from 1 to (length standard-rows)
                    do (plan-row row))))))
    (values (nreverse entries) owner)))

(defun table-recalculate-plan-field-assignments
    (assignments standard-rows row-count column-count target limits work-budget)
  (let ((entries nil)
        (owner 0))
    (dolist (assignment assignments)
      (table-calc-charge-work limits work-budget 1)
      (let* ((reference (table-calc-assignment-target assignment))
             (kind (table-recalculate-assignment-kind assignment target))
             (end-reference (table-calc-assignment-target-end assignment))
             (first-row (table-calc-reference-row-value reference))
             (first-column (table-calc-reference-column-value reference))
             (last-row (if (eq kind :range)
                           (table-calc-reference-row-value end-reference)
                           first-row))
             (last-column (case kind
                            (:row column-count)
                            (:range
                             (table-calc-reference-column-value end-reference))
                            (otherwise first-column)))
             (row-start nil)
             (row-end nil)
             (column-start nil)
             (column-end nil))
        (unless (and (integerp first-row) (integerp last-row)
                     (or (eq kind :row)
                         (and (integerp first-column)
                              (integerp last-column))))
          (table-recalculate-signal-error
           target :invalid-reference "Formula assignment target has invalid coordinates"))
        (setf row-start (min first-row last-row)
              row-end (max first-row last-row)
              column-start (if (eq kind :row) 1
                               (min first-column last-column))
              column-end (if (eq kind :row) column-count
                             (max first-column last-column)))
        (unless (and (<= 1 row-start row-count)
                     (<= 1 row-end row-count)
                     (<= 1 column-start column-count)
                     (<= 1 column-end column-count))
          (table-recalculate-signal-error
           target :invalid-reference "Formula assignment target is outside the table"))
        (loop for row from row-start to row-end
              do (loop for column from column-start to column-end
                       do (table-calc-charge-work limits work-budget 1)
                          (unless (table-recalculate-cell standard-rows row column)
                            (table-recalculate-signal-error
                             target :invalid-reference
                             "Formula assignment target is outside the table"))
                          (push (make-table-recalculate-entry
                                 :assignment assignment :row row :column column
                                 :owner owner)
                                entries)
                          (incf owner)))))
    (values (nreverse entries) owner)))

(defun table-recalculate-entry-target-map (entries)
  (let ((targets (make-hash-table :test #'equal)))
    (dolist (entry entries targets)
      (setf (gethash (table-recalculate-target-key
                      (table-recalculate-entry-row entry)
                      (table-recalculate-entry-column entry))
                     targets)
            (table-recalculate-entry-owner entry)))))

(defun table-recalculate-add-cycle-dependency
    (owner dependency dependencies dependency-seen)
  (unless (= owner dependency)
    (let ((seen (aref dependency-seen owner)))
      (unless (gethash dependency seen)
        (setf (gethash dependency seen) t)
        (push dependency (aref dependencies owner))))))

(defun table-recalculate-check-cycles
    (entries owner-count target-map row-count column-count header-columns target
     limits work-budget &key hline-rows reference-value-function)
  (let ((dependencies (make-array owner-count :initial-element nil))
        (dependents (make-array owner-count :initial-element nil))
        (dependency-seen (make-array owner-count)))
    (dotimes (owner owner-count)
      (setf (aref dependency-seen owner) (make-hash-table)))
    (dolist (entry entries)
      (let* ((owner (table-recalculate-entry-owner entry))
             (assignment (table-recalculate-entry-assignment entry))
             (regions
               (table-recalculate-expression-regions
                (table-calc-assignment-expression assignment)
                (table-recalculate-entry-row entry)
                (table-recalculate-entry-column entry)
                row-count column-count header-columns target limits work-budget
                :hline-rows hline-rows
                :reference-value-function reference-value-function)))
        (dolist (region regions)
          (destructuring-bind (first-row first-column last-row last-column) region
            (loop for row from first-row to last-row
                  do (loop for column from first-column to last-column
                           do (table-calc-charge-work limits work-budget 1)
                              (multiple-value-bind (dependency present)
                                  (gethash (table-recalculate-target-key row column)
                                           target-map)
                                (when present
                                  (table-recalculate-add-cycle-dependency
                                   owner dependency dependencies dependency-seen)))))))))
    (loop for owner below owner-count
          do (dolist (dependency (aref dependencies owner))
               (push owner (aref dependents dependency))))
    (let ((pending (make-array owner-count :initial-element 0))
          (ready nil)
          (visited 0))
      (loop for owner below owner-count
            do (setf (aref pending owner) (length (aref dependencies owner)))
               (when (zerop (aref pending owner))
                 (push owner ready)))
      (loop while ready
            for owner = (pop ready)
            do (incf visited)
               (dolist (dependent (aref dependents owner))
                 (decf (aref pending dependent))
                 (when (zerop (aref pending dependent))
                   (push dependent ready))))
      (unless (= visited owner-count)
        (table-recalculate-signal-error
         target :cyclic-formula "Formula targets contain a dependency cycle")))))

(defun table-recalculate-validate-entry-references
    (entries row-count column-count header-columns target limits work-budget
     &key hline-rows reference-value-function)
  (dolist (entry entries)
    (table-recalculate-expression-regions
     (table-calc-assignment-expression
      (table-recalculate-entry-assignment entry))
     (table-recalculate-entry-row entry)
     (table-recalculate-entry-column entry)
     row-count column-count header-columns target limits work-budget
     :hline-rows hline-rows
     :reference-value-function reference-value-function)))

(defun table-recalculate-work-map-cell-value
    (work-map standard-rows row column &optional fallback-work-map)
  (multiple-value-bind (value present)
      (gethash (table-recalculate-target-key row column) work-map)
    (if present
        value
        (if fallback-work-map
            (table-recalculate-work-map-cell-value
             fallback-work-map standard-rows row column)
            (table-recalculate-cell-value standard-rows row column)))))

(defun table-recalculate-elisp-string (value)
  (with-output-to-string (stream)
    (write-char #\" stream)
    (loop for character across value
          do (case character
               (#\" (write-string "\\\"" stream))
               (#\\ (write-string "\\\\" stream))
               (#\Newline (write-string "\\n" stream))
               (#\Return (write-string "\\r" stream))
               (#\Tab (write-string "\\t" stream))
               (#\Page (write-string "\\f" stream))
               (#\Backspace (write-string "\\b" stream))
               (t (write-char character stream))))
    (write-char #\" stream)))

(defun table-recalculate-elisp-number-string (value limits)
  (let ((decimal (parse-safe-decimal value :limits limits)))
    (if decimal
        (table-calc-decimal-string decimal)
        "0")))

(defun table-recalculate-org-sbe-printed-data (value)
  (typecase value
    (null "nil")
    (string (table-recalculate-elisp-string value))
    (symbol (if (keywordp value)
                (format nil ":~A" (symbol-name value))
                (symbol-name value)))
    (cons
     (format nil "(~{~A~^ ~})"
             (mapcar #'table-recalculate-org-sbe-printed-data value)))
    (t (format nil "~S" value))))

(defun table-recalculate-org-sbe-argument-text (value literal-p)
  (if (and literal-p (stringp value))
      (table-recalculate-elisp-string value)
      (typecase value
        (null "nil")
        (string value)
        (symbol (if (keywordp value)
                    (format nil ":~A" (symbol-name value))
                    (symbol-name value)))
        (cons (table-recalculate-org-sbe-printed-data value))
        (t (format nil "~A" value)))))

(defun table-recalculate-org-sbe-parameter-text
    (name values literal-flags limits)
  (let* ((value-text
           (cond
             ((null values) "nil")
             ((rest values)
              (format nil "'(~{~A~^ ~})"
                      (loop for value in values
                            for literal-p in literal-flags
                            collect
                            (table-recalculate-org-sbe-argument-text
                             value literal-p))))
             (t
              (table-recalculate-org-sbe-argument-text
               (first values) (first literal-flags)))))
         (assignment (format nil "~A=~A" name value-text)))
    (check-limit limits :max-input-length (length assignment))
    assignment))

(defun table-recalculate-org-sbe-resolve-value (value literal-p context)
  (if literal-p
      value
      (typecase value
        (null nil)
        (string (babel-static-scalar context value :resolve-reference t))
        (symbol (babel-static-scalar context (symbol-name value)
                                     :resolve-reference t))
        (t value))))

(defun table-recalculate-org-sbe-override (spec context)
  (destructuring-bind (name values literal-flags) spec
    (cons name
          (cond
            ((null values)
             (table-recalculate-org-sbe-resolve-value
              "nil" nil context))
            ((null (rest values))
             (table-recalculate-org-sbe-resolve-value
              (first values) (first literal-flags) context))
            (t values)))))

(defun table-recalculate-org-sbe-resolved-parameter-text (name value limits)
  (cond
    ((null value)
     (let ((assignment (format nil "~A='()" name)))
       (check-limit limits :max-input-length (length assignment))
       assignment))
    ((consp value)
     (table-recalculate-org-sbe-parameter-text
      name value (mapcar (lambda (item) (stringp item)) value) limits))
    ((stringp value)
     (table-recalculate-org-sbe-parameter-text name (list value) '(t) limits))
    (t
     (table-recalculate-org-sbe-parameter-text name (list value) '(nil) limits))))

(defun table-recalculate-execute-org-sbe-block
    (document source-block callbacks
              &key header parameter-overrides variable-overrides
              (resolved-variables nil resolved-variables-p) result-resolver)
  (validate-babel-source-block document source-block)
  (let* ((language (node-property source-block :language ""))
         (executor (%lookup-callback callbacks
                                     :babel-executor
                                     language
                                     'org-elisp-evaluator-error
                                     "No Babel executor is registered"))
         (wrapped-registry (make-callback-registry)))
    (register-callback
     wrapped-registry
     :babel-executor
     language
     (lambda (body parameters variables)
       (when resolved-variables-p
         (setf variables resolved-variables))
       (dolist (assignment parameter-overrides)
         (setf parameters
               (babel-merge-header-parameter parameters "VAR" assignment)))
       (dolist (override variable-overrides)
         (let ((position
                (position (car override) variables :key #'car :test #'string-equal)))
           (if position
               (setf (cdr (nth position variables)) (cdr override))
             (setf variables (append variables (list override))))))
       (funcall executor body parameters variables)))
    (let* ((parameters (and header (babel-parse-header-arguments header)))
           (execution
            (apply (function %execute-babel-source-block)
                   document source-block wrapped-registry
                   (list :result-node nil
                         :parameters parameters
                         :resolved-variables resolved-variables
                         :resolved-variables-p resolved-variables-p
                         :result-resolver result-resolver
                         :evaluation-context :execute))))
      (babel-execution-result-value execution))))

(defun table-recalculate-elisp-reference-value
    (reference current-row current-column row-count column-count header-columns
     hline-rows target cell-value-function reference-value-function)
  (multiple-value-bind (value present-p)
      (if reference-value-function
          (funcall reference-value-function reference)
          (values nil nil))
    (if present-p
        value
        (multiple-value-bind (row column)
            (table-recalculate-reference-position
             reference current-row current-column row-count column-count
             header-columns hline-rows target)
          (funcall cell-value-function row column)))))

(defun table-recalculate-elisp-reference-text
    (value numbers-p limits)
  (let ((text (if (stringp value)
                  value
                  (if (safe-decimal-p value)
                      (table-calc-decimal-string value)
                      (if (realp value)
                          (table-calc-format-result value nil nil limits "formula")
                          "")))))
    (if numbers-p
        (table-recalculate-elisp-number-string text limits)
        (table-recalculate-elisp-string text))))

(defun table-recalculate-expand-elisp-expression
    (source expression assignment current-row current-column row-count
     column-count header-columns hline-rows target cell-value-function
     reference-value-function limits work-budget)
  (let ((references (table-calc-expression-children expression))
        (numbers-p (member (table-calc-assignment-mode assignment) '(:n :en)))
        (keep-empty-p (member (table-calc-assignment-mode assignment) '(:e :en)))
        (flags (table-calc-assignment-flags assignment)))
    (table-calc-charge-work limits work-budget (length source))
    (when (member :literal-references flags)
      (return-from table-recalculate-expand-elisp-expression source))
    (let ((output (make-string-output-stream))
          (position 0))
      (dolist (reference references)
        (let* ((start (table-calc-expression-start reference))
               (end (table-calc-expression-end reference)))
          (write-string source output :start position :end (- start
                                                           (table-calc-expression-start expression)))
          (table-calc-charge-work limits work-budget 1)
          (if (eq (table-calc-expression-kind reference) :range)
              (let* ((children (table-calc-expression-children reference))
                     (first-reference (table-calc-expression-value (first children)))
                     (last-reference (table-calc-expression-value (second children))))
                (multiple-value-bind (first-row first-column)
                    (table-recalculate-reference-position
                     first-reference current-row current-column row-count
                     column-count header-columns hline-rows target)
                  (multiple-value-bind (last-row last-column)
                      (table-recalculate-reference-position
                       last-reference current-row current-column row-count
                       column-count header-columns hline-rows target)
                    (let ((first-value t))
                      (loop for row from (min first-row last-row)
                              to (max first-row last-row)
                            do (loop for column from (min first-column last-column)
                                       to (max first-column last-column)
                                     for value =
                                       (funcall cell-value-function row column)
                                     for empty-p =
                                       (and (stringp value) (string= value ""))
                                     for text =
                                       (if (and empty-p numbers-p)
                                           "0"
                                           (table-recalculate-elisp-reference-text
                                            value numbers-p limits))
                                     do (table-calc-charge-work
                                         limits work-budget 1)
                                     when (or keep-empty-p (not empty-p))
                                       do (unless first-value
                                            (write-char #\Space output))
                                          (write-string text output)
                                          (setf first-value nil)))))))
              (let ((value
                      (table-recalculate-elisp-reference-value
                       (table-calc-expression-value reference)
                       current-row current-column row-count column-count
                       header-columns hline-rows target cell-value-function
                       reference-value-function)))
                (write-string
                 (table-recalculate-elisp-reference-text value numbers-p limits)
                 output)))
          (setf position (- end (table-calc-expression-start expression)))))
      (write-string source output :start position)
      (let ((expanded (get-output-stream-string output)))
        (check-limit limits :max-input-length (length expanded))
        expanded))))

(defun table-recalculate-evaluate-entries (entries row-count
                                                   column-count
                                                   header-columns
                                                   limits
                                                   work-budget
                                                   work-map
                                                   cell-value-function
                                                   target
                                                   &key
                                                   hline-rows
                                                   functions
                                                   reference-value-function
                                                   remote-function
                                                   (callbacks (make-callback-registry)))
  (labels ((execute-org-sbe
            (block-name header variable-overrides variable-parameters)
            (declare (ignore variable-overrides))
            (let* ((document (node-document target))
                   (name-index (babel-build-name-index document))
                   (source-block (gethash block-name name-index)))
              (unless (and source-block (eq (node-type source-block) :src-block))
                (babel-signal-error :unresolved-call-target
                                    "org-sbe target is not a named source block"
                                    block-name))
              (labels ((resolve-source-result (referenced-name resolved-variables)
                                              (let ((referenced-block (gethash referenced-name name-index)))
                                                (table-recalculate-execute-org-sbe-block
                                                 document referenced-block callbacks
                                                 :parameter-overrides
                                                 (mapcar
                                                  (lambda (variable)
                                                    (table-recalculate-org-sbe-resolved-parameter-text
                                                     (car variable) (cdr variable) limits))
                                                  resolved-variables)
                                                 :resolved-variables resolved-variables
                                                 :result-resolver (function resolve-source-result)))))
                (let* ((context
                        (make-babel-resolution-context
                         :document document
                         :result-resolver (function resolve-source-result)
                         :names name-index))
                       (overrides
                        (mapcar
                         (lambda (spec)
                           (table-recalculate-org-sbe-override spec context))
                         variable-parameters))
                       (parameter-overrides
                        (mapcar
                         (lambda (spec)
                           (table-recalculate-org-sbe-parameter-text
                            (first spec) (second spec) (third spec) limits))
                         variable-parameters))
                       (value
                        (table-recalculate-execute-org-sbe-block
                         document source-block callbacks
                         :header header
                         :parameter-overrides parameter-overrides
                         :variable-overrides overrides
                         :result-resolver (function resolve-source-result)))
                       (text (if (stringp value) value (format nil "~S" value))))
                  (string-trim (list #\Space #\Tab #\Newline #\Return) text))))))
    (dolist (entry entries)
      (let* ((row (table-recalculate-entry-row entry))
             (column (table-recalculate-entry-column entry))
             (key (table-recalculate-target-key row column))
             (value
              (table-calc-evaluate-assignment (table-recalculate-entry-assignment entry)
                                              (lambda (reference)
                                                (table-recalculate-reference-position reference
                                                                                      row
                                                                                      column
                                                                                      row-count
                                                                                      column-count
                                                                                      header-columns
                                                                                      hline-rows
                                                                                      target))
                                              (lambda (reference-row reference-column)
                                                (funcall cell-value-function work-map reference-row reference-column))
                                              :limits
                                              limits
                                              :work-budget
                                              work-budget
                                              :functions
                                              functions
                                              :reference-value-function
                                              reference-value-function
                                              :remote-function
                                              remote-function
                                              :current-row
                                              row
                                              :current-column
                                              column
                                              :callbacks
                                              callbacks
                                              :elisp-reference-function
                                              (lambda (source expression assignment)
                                                (table-recalculate-expand-elisp-expression source
                                                                                           expression
                                                                                           assignment
                                                                                           row
                                                                                           column
                                                                                           row-count
                                                                                           column-count
                                                                                           header-columns
                                                                                           hline-rows
                                                                                           target
                                                                                           (lambda
                                                                                               (reference-row
                                                                                                reference-column)
                                                                                             (funcall
                                                                                              cell-value-function
                                                                                              work-map
                                                                                              reference-row
                                                                                              reference-column))
                                                                                           reference-value-function
                                                                                           limits
                                                                                           work-budget))
                                              :org-sbe-function
                                              #'execute-org-sbe)))
        (setf (gethash key work-map) value)))
    work-map))

(defun table-recalculate-final-values (column-values field-values)
  (let ((values (make-hash-table :test #'equal)))
    (maphash (lambda (key value) (setf (gethash key values) value)) column-values)
    (maphash (lambda (key value) (setf (gethash key values) value)) field-values)
    values))

(defun table-recalculate-table-name-map (document)
  (let ((tables (make-hash-table :test #'equalp)))
    (map-nodes
     (lambda (node)
       (when (and (eq (node-type node) :table)
                  (stringp (node-property node :name)))
         (let* ((name (node-property node :name))
                (previous (gethash name tables)))
           (setf (gethash name tables)
                 (if previous :ambiguous node)))))
     (document-root document))
    tables))

(defun table-recalculate-constant-word (text start)
  (let ((end (position-if (lambda (character)
                            (find character '(#\Space #\Tab #\Newline #\Return)))
                          text :start start)))
    (values (subseq text start (or end (length text)))
            (or end (length text)))))

(defun table-recalculate-constant-pairs (text)
  (let ((position 0)
        (pairs nil))
    (loop
      (setf position
            (position-if-not (lambda (character)
                               (find character '(#\Space #\Tab #\Newline #\Return)))
                             text :start position))
      (unless position
        (return (nreverse pairs)))
      (multiple-value-bind (word next-position)
          (table-recalculate-constant-word text position)
        (let ((equals (position #\= word)))
          (when (and equals (> equals 0) (< equals (1- (length word))))
            (let ((value (subseq word (1+ equals))))
              (push (cons (subseq word 0 equals)
                          (if (and (> (length value) 1)
                                   (char= (char value (1- (length value))) #\.))
                              (subseq value 0 (1- (length value)))
                              value))
                    pairs)))
        (setf position next-position))))))

(defun table-recalculate-parse-formula (formula limits functions)
  (handler-case
      (table-calc-parse-formula formula limits functions)
    (org-table-unsupported-syntax (condition)
      (let ((position (org-table-unsupported-syntax-position condition)))
        (if (and (integerp position) (<= 2 position) (< position (length formula))
                 (string= (subseq formula (- position 2) position) "..")
                 (char= (char formula position) #\=))
            (table-calc-fail formula position :invalid-range)
            (error condition))))))

(defun table-recalculate-constant-value (value limits)
  (if (stringp value)
      (let ((decimal (parse-safe-decimal value :limits limits)))
        (if decimal
            (progn
              (setf (safe-decimal-float-p decimal) nil)
              decimal)
            value))
      value))

(defun table-recalculate-document-constants (document)
  (let ((constants (make-hash-table :test #'equal))
        (declared-p nil))
    (map-nodes
     (lambda (node)
       (when (and (eq (node-type node) :keyword)
                  (stringp (node-property node :key))
                  (string-equal (node-property node :key) "CONSTANTS"))
         (setf declared-p t)
         (dolist (pair (table-recalculate-constant-pairs
                        (or (node-property node :value) "")))
           (setf (gethash (car pair) constants) (cdr pair)))))
     (document-root document))
    (values constants declared-p)))

(defun table-recalculate-property-value (table name)
  (loop for ancestor = (node-parent table) then (node-parent ancestor)
        while ancestor
        when (member (node-type ancestor) '(:headline :inlinetask) :test #'eq)
          do (multiple-value-bind (value present-p)
                 (effective-heading-property-value ancestor name)
               (when present-p
                 (return-from table-recalculate-property-value
                   (values value t))))
        finally (return (values nil nil))))

(defun table-recalculate-reference-value-function
    (context table header-columns target)
  (lambda (reference)
    (block resolved
      (when (eq (table-calc-reference-column-kind reference) :name)
        (let ((name (table-calc-reference-column-value reference)))
          (when (and (>= (length name) 5)
                     (string-equal name "PROP_" :end1 5 :end2 5))
            (multiple-value-bind (value present-p)
                (table-recalculate-property-value table (subseq name 5))
              (unless present-p
                (table-recalculate-signal-error
                 target :unknown-property "Formula references an unknown property"))
              (return-from resolved (values value t))))
          (multiple-value-bind (column present-p) (gethash name header-columns)
            (declare (ignore column))
            (when present-p
              (return-from resolved (values nil nil))))
          (multiple-value-bind (value present-p)
              (gethash name (table-recalculate-context-constants context))
            (when present-p
              (return-from resolved
                (values (table-recalculate-constant-value
                         value (table-recalculate-context-limits context))
                        t))))
          (when (table-recalculate-context-constants-declared-p context)
            (table-recalculate-signal-error
             target :unknown-constant "Formula references an unknown constant"))))
      (values nil nil))))

(defun table-recalculate-state-cell-value (state row column)
  (multiple-value-bind (value present-p)
      (gethash (table-recalculate-target-key row column)
               (table-recalculate-state-values state))
    (if present-p
        value
        (table-recalculate-cell-value
         (table-recalculate-state-standard-rows state) row column))))

(defun table-recalculate-remote-table (context name target)
  (multiple-value-bind (table present-p)
      (gethash name (table-recalculate-context-tables context))
    (unless (and present-p (not (eq table :ambiguous)))
      (table-recalculate-signal-error
       target :unknown-remote-table "Formula references an unknown named table"))
    table))
(defun table-recalculate-remote-state (context table)
  (let ((cache (table-recalculate-context-remote-cache context)))
    (multiple-value-bind (cached present-p) (gethash table cache)
      (when present-p
        (return-from table-recalculate-remote-state cached)))
    (let ((limits (table-recalculate-context-limits context))
          (work-budget (table-recalculate-context-work-budget context)))
      (unless (eq (node-property table :type) :org)
        (table-recalculate-signal-error
         table :unsupported-table-type "Only Org pipe tables can be recalculated"))
      (multiple-value-bind (standard-rows data-rows header-row hline-rows)
          (table-recalculate-standard-rows table limits work-budget)
        (table-calc-charge-work limits work-budget (length standard-rows))
        (let* ((column-count
                 (reduce #'max standard-rows :key #'length :initial-value 0))
               (header-columns
                 (table-recalculate-header-columns
                  header-row limits work-budget))
               (state
                 (make-table-recalculate-state
                  :standard-rows standard-rows :data-rows data-rows
                  :header-row header-row :hline-rows hline-rows
                  :values (make-hash-table :test #'equal)
                  :column-count column-count :header-columns header-columns)))
          (setf (gethash table cache) state)
          state)))))

(defun table-recalculate-remote-reference-function (context target)
  (lambda (table-name reference-expression current-row current-column)
    (unless (stringp table-name)
      (table-recalculate-signal-error
       target :unknown-remote-table "Remote table name must be a string"))
    (let* ((table (table-recalculate-remote-table context table-name target))
           (state (table-recalculate-remote-state context table))
           (standard-rows (table-recalculate-state-standard-rows state))
           (row-count (length standard-rows))
           (column-count (table-recalculate-state-column-count state))
           (header-columns (table-recalculate-state-header-columns state))
           (hline-rows (table-recalculate-state-hline-rows state))
           (limits (table-recalculate-context-limits context))
           (work-budget (table-recalculate-context-work-budget context)))
      (case (table-calc-expression-kind reference-expression)
        (:reference
         (multiple-value-bind (row column)
             (table-recalculate-reference-position
              (table-calc-expression-value reference-expression)
              current-row current-column row-count column-count header-columns
              hline-rows target)
           (table-recalculate-state-cell-value state row column)))
        (:range
         (let ((range
                 (table-calc-resolve-range
                  reference-expression
                  (lambda (reference)
                    (table-recalculate-reference-position
                     reference current-row current-column row-count column-count
                     header-columns hline-rows target))
                  limits work-budget target)))
           (loop for row from (table-calc-range-value-first-row range)
                   to (table-calc-range-value-last-row range)
                 append (loop for column from
                              (table-calc-range-value-first-column range)
                                to (table-calc-range-value-last-column range)
                              collect (table-recalculate-state-cell-value
                                       state row column)))))
        (otherwise
         (table-recalculate-signal-error
          target :invalid-reference "Remote reference must select a field or range"))))))

(defun table-recalculate-compute-state (context table)
  (multiple-value-bind (cached present-p)
      (gethash table (table-recalculate-context-cache context))
    (when present-p
      (return-from table-recalculate-compute-state cached)))
  (when (member table (table-recalculate-context-active context) :test #'eq)
    (table-recalculate-signal-error
     table :cyclic-remote-reference "Remote table dependencies contain a cycle"))
  (push table (table-recalculate-context-active context))
  (unwind-protect
       (let ((state (table-recalculate-build-state context table)))
         (setf (gethash table (table-recalculate-context-cache context)) state)
         state)
    (setf (table-recalculate-context-active context)
          (remove table (table-recalculate-context-active context)
                  :test #'eq :count 1))))

(defun table-recalculate-build-state (context table)
  (let* ((limits (table-recalculate-context-limits context))
         (work-budget (table-recalculate-context-work-budget context))
         (functions (table-recalculate-context-functions context))
         (formulas (node-property table :tblfm)))
    (when (not (eq (node-property table :type) :org))
      (table-recalculate-signal-error
       table :unsupported-table-type "Only Org pipe tables can be recalculated"))
    (multiple-value-bind (standard-rows data-rows header-row hline-rows)
        (table-recalculate-standard-rows table limits work-budget)
      (let* ((row-count (length standard-rows))
             (column-count
               (reduce #'max standard-rows :key #'length :initial-value 0))
             (header-columns (table-recalculate-header-columns header-row))
             (program (when formulas
                        (table-recalculate-parse-formula
                         (first formulas) limits functions)))
             (parsed-assignments (and program
                                      (table-calc-program-assignments program)))
             (reference-value-function
               (table-recalculate-reference-value-function
                context table header-columns table))
             (assignments
               (mapcar (lambda (assignment)
                         (table-recalculate-normalize-assignment-target
                          assignment row-count column-count header-columns
                          hline-rows table))
                       parsed-assignments))
             (column-assignments
               (table-recalculate-assignments-by-kind
                assignments :column table))
             (field-assignments
               (append
                (table-recalculate-assignments-by-kind assignments :field table)
                (table-recalculate-assignments-by-kind assignments :row table)
                (table-recalculate-assignments-by-kind assignments :range table)))
             (column-values (make-hash-table :test #'equal))
             (field-values (make-hash-table :test #'equal)))
        (table-recalculate-validate-duplicates
         assignments standard-rows column-count table limits work-budget)
        (multiple-value-bind (column-entries column-owner-count)
            (table-recalculate-plan-column-assignments
             column-assignments data-rows standard-rows column-count table
             limits work-budget)
          (multiple-value-bind (field-entries field-owner-count)
              (table-recalculate-plan-field-assignments
               field-assignments standard-rows row-count column-count table
               limits work-budget)
            (table-recalculate-validate-entry-references
             column-entries row-count column-count header-columns table
             limits work-budget :hline-rows hline-rows
             :reference-value-function reference-value-function)
            (table-recalculate-validate-entry-references
             field-entries row-count column-count header-columns table
             limits work-budget :hline-rows hline-rows
             :reference-value-function reference-value-function)
            (table-recalculate-check-cycles
             column-entries column-owner-count
             (table-recalculate-entry-target-map column-entries)
             row-count column-count header-columns table limits work-budget
             :hline-rows hline-rows
             :reference-value-function reference-value-function)
            (table-recalculate-check-cycles
             field-entries field-owner-count
             (table-recalculate-entry-target-map field-entries)
             row-count column-count header-columns table limits work-budget
             :hline-rows hline-rows
             :reference-value-function reference-value-function)
             (table-recalculate-evaluate-entries
             column-entries row-count column-count header-columns
             limits work-budget column-values
             (lambda (work-map row column)
               (table-recalculate-work-map-cell-value
                work-map standard-rows row column))
             table :hline-rows hline-rows :functions functions
             :callbacks (table-recalculate-context-callbacks context)
             :reference-value-function reference-value-function
             :remote-function
             (table-recalculate-remote-reference-function context table))
            (table-recalculate-evaluate-entries
             field-entries row-count column-count header-columns
             limits work-budget field-values
             (lambda (work-map row column)
               (table-recalculate-work-map-cell-value
                work-map standard-rows row column column-values))
             table :hline-rows hline-rows :functions functions
             :callbacks (table-recalculate-context-callbacks context)
             :reference-value-function reference-value-function
             :remote-function
             (table-recalculate-remote-reference-function context table))
            (make-table-recalculate-state
             :standard-rows standard-rows :data-rows data-rows
             :header-row header-row :hline-rows hline-rows
             :values (table-recalculate-final-values
                      column-values field-values)
             :column-count column-count :header-columns header-columns)))))))

(defun table-recalculate-expected-canonical (document values standard-rows)
  (let ((expected (edit-canonical-copy (edit-canonical-document document))))
    (maphash
     (lambda (key value)
       (let* ((row (car key))
              (column (cdr key))
              (cell (table-recalculate-cell standard-rows row column))
              (canonical-cell
                (edit-canonical-node-at-path expected
                                             (edit-node-path document cell)))
              (properties (getf canonical-cell :properties))
              (value-document
                (make-document value :limits (document-limits document))))
         (setf (document-settings value-document)
               (document-settings document))
         (setf (getf properties :value) value
               (getf canonical-cell :properties) properties
               (getf canonical-cell :children)
               (mapcar #'edit-canonical-node
                       (parse-objects value-document 0 (length value)
                                      :table-cell)))))
     values)
    expected))

(defun table-recalculate-patches (values standard-rows)
  (let ((patches nil))
    (maphash
     (lambda (key value)
       (let* ((cell (table-recalculate-cell standard-rows (car key) (cdr key)))
              (original (node-property cell :value)))
         (unless (string= original value)
           (push (edit-make-patch (node-contents-begin cell)
                                  (node-contents-end cell) value)
                 patches))))
     values)
    patches))

(defun table-recalculate-assignments-by-kind (assignments kind &optional target)
  (loop for assignment in assignments
        when (eq (table-recalculate-assignment-kind assignment target) kind)
          collect assignment))

(defun table-recalculate (document target &key functions
                                                (callbacks (make-callback-registry)))
  "Recalculate a pipe table's TBLFM formulas and return its validated edit result.
FUNCTIONS is an alist of string names and call-scoped function designators.
CALLBACKS is a caller-owned callback registry used for quoted Lisp formulas."
  (let ((table (edit-resolve-node document target '(:table))))
    (let* ((limits (document-limits document))
           (work-budget (make-table-calc-work-budget)))
      (multiple-value-bind (constants constants-declared-p)
          (table-recalculate-document-constants document)
        (let* ((context
                 (make-table-recalculate-context
                  :document document
                  :tables (table-recalculate-table-name-map document)
                  :cache (make-hash-table :test #'eq)
                  :active nil
                  :functions functions
                  :callbacks callbacks
                  :constants constants
                  :constants-declared-p constants-declared-p
                  :limits limits
                  :work-budget work-budget))
               (state (table-recalculate-compute-state context table))
               (standard-rows (table-recalculate-state-standard-rows state))
               (values (table-recalculate-state-values state))
               (patches (table-recalculate-patches values standard-rows)))
          (if patches
              (edit-apply-patches-result
               document patches :target table
               :expected
               (table-recalculate-expected-canonical
                document values standard-rows))
              (make-edit-result :updated-document document :spans nil)))))))

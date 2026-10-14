;;;; src/table-recalculate.lisp
(in-package #:org-kit)

(defstruct table-recalculate-entry assignment row column owner)

(defun table-recalculate-signal-error (target reason message)
  (error 'org-table-error
         :message message
         :target (edit-target-description target)
         :reason reason))

(defun table-recalculate-standard-rows (table)
  (let ((rows nil)
        (after-first-rule nil)
        (header nil)
        (row-index 0)
        (data-rows nil))
    (dolist (row (node-children table))
      (cond
        ((eq (node-property row :type) :rule)
         (unless after-first-rule
           (setf after-first-rule t)))
        (t
         (incf row-index)
         (push row rows)
         (if after-first-rule
             (push row-index data-rows)
             (progn
               (unless header
                 (setf header row)))))))
    (setf rows (nreverse rows)
          data-rows (nreverse data-rows))
    (unless after-first-rule
      (setf data-rows (loop for row in rows for index from 1 collect index)
            header nil))
    (values rows data-rows header)))

(defun table-recalculate-cell (standard-rows row column)
  (let* ((row-node (nth (1- row) standard-rows))
         (cell (and (<= 1 column (length (node-children row-node)))
                    (nth (1- column) (node-children row-node)))))
    cell))

(defun table-recalculate-cell-value (standard-rows row column)
  (let ((cell (table-recalculate-cell standard-rows row column)))
    (if cell (node-property cell :value) "")))

(defun table-recalculate-header-columns (header-row)
  (let ((columns (make-hash-table :test #'equal)))
    (when header-row
      (loop for cell in (node-children header-row)
            for column from 1
            for name = (node-property cell :value)
            do (multiple-value-bind (prior present) (gethash name columns)
                 (declare (ignore prior))
                 (setf (gethash name columns)
                       (if present :ambiguous column)))))
    columns))

(defun table-recalculate-reference-position
    (reference current-row current-column row-count column-count header-columns target)
  (let* ((row-kind (table-calc-reference-row-kind reference))
         (row-value (table-calc-reference-row-value reference))
         (column-kind (table-calc-reference-column-kind reference))
         (column-value (table-calc-reference-column-value reference))
         (row (case row-kind
                (:current current-row)
                (:last row-count)
                (:absolute row-value)
                (:relative (and (integerp row-value) (+ current-row row-value)))))
         (column
           (case column-kind
             (:current current-column)
             (:absolute column-value)
             (:relative (and (integerp column-value)
                             (+ current-column column-value)))
             (:name
              (multiple-value-bind (value present)
                  (gethash column-value header-columns)
                (unless present
                  (table-recalculate-signal-error
                   target :unknown-column-name "Formula references an unknown column name"))
                (when (eq value :ambiguous)
                  (table-recalculate-signal-error
                   target :ambiguous-column-name "Formula references an ambiguous column name"))
                value)))))
    (unless (and (integerp row) (<= 1 row row-count)
                 (integerp column) (<= 1 column column-count))
      (table-recalculate-signal-error
       target :invalid-reference "Formula reference is outside the table"))
    (values row column)))

(defun table-recalculate-expression-regions
    (expression current-row current-column row-count column-count header-columns target)
  (loop with pending = (list expression)
        with regions = nil
        while pending
        for node = (pop pending)
        for kind = (table-calc-expression-kind node)
        do (case kind
             (:reference
              (multiple-value-bind (row column)
                  (table-recalculate-reference-position
                   (table-calc-expression-value node) current-row current-column
                   row-count column-count header-columns target)
                (push (list row column row column) regions)))
             (:range
              (let* ((children (table-calc-expression-children node))
                     (first-reference (table-calc-expression-value (first children)))
                     (last-reference (table-calc-expression-value (second children))))
                (multiple-value-bind (first-row first-column)
                    (table-recalculate-reference-position
                     first-reference current-row current-column row-count column-count
                     header-columns target)
                  (multiple-value-bind (last-row last-column)
                      (table-recalculate-reference-position
                       last-reference current-row current-column row-count column-count
                       header-columns target)
                    (push (list (min first-row last-row)
                                (min first-column last-column)
                                (max first-row last-row)
                                (max first-column last-column))
                          regions)))))
             (otherwise
              (dolist (child (table-calc-expression-children node))
                (push child pending))))
        finally (return (nreverse regions))))

(defun table-recalculate-target-key (row column)
  (cons row column))

(defun table-recalculate-validate-duplicates (assignments target)
  (let ((columns (make-hash-table))
        (fields (make-hash-table :test #'equal)))
    (dolist (assignment assignments)
      (let* ((kind (table-calc-assignment-kind assignment))
             (reference (table-calc-assignment-target assignment))
             (row (table-calc-reference-row-value reference))
             (column (table-calc-reference-column-value reference)))
        (if (eq kind :column)
            (multiple-value-bind (prior present) (gethash column columns)
              (declare (ignore prior))
              (when present
                (table-recalculate-signal-error
                 target :duplicate-target "Column formula target is duplicated"))
              (setf (gethash column columns) t))
            (let ((key (table-recalculate-target-key row column)))
              (multiple-value-bind (prior present) (gethash key fields)
                (declare (ignore prior))
                (when present
                  (table-recalculate-signal-error
                   target :duplicate-target "Field formula target is duplicated"))
                (setf (gethash key fields) t))))))))

(defun table-recalculate-plan-column-assignments
    (assignments data-rows standard-rows column-count target)
  (let ((entries nil)
        (owner 0))
    (dolist (assignment assignments)
      (let* ((reference (table-calc-assignment-target assignment))
             (column (table-calc-reference-column-value reference)))
        (unless (and (integerp column) (<= 1 column column-count))
          (table-recalculate-signal-error
           target :invalid-reference "Column formula target is outside the table"))
        (dolist (row data-rows)
          (when (table-recalculate-cell standard-rows row column)
            (push (make-table-recalculate-entry
                   :assignment assignment :row row :column column :owner owner)
                  entries)
            (incf owner)))))
    (values (nreverse entries) owner)))

(defun table-recalculate-plan-field-assignments
    (assignments standard-rows row-count column-count target)
  (let ((entries nil)
        (owner 0))
    (dolist (assignment assignments)
      (let* ((reference (table-calc-assignment-target assignment))
             (row (table-calc-reference-row-value reference))
             (column (table-calc-reference-column-value reference)))
        (unless (and (integerp row) (<= 1 row row-count)
                     (integerp column) (<= 1 column column-count)
                     (table-recalculate-cell standard-rows row column))
          (table-recalculate-signal-error
           target :invalid-reference "Field formula target is outside the table"))
        (push (make-table-recalculate-entry
               :assignment assignment :row row :column column :owner owner)
              entries))
      (incf owner))
    (values (nreverse entries) owner)))

(defun table-recalculate-entry-target-map (entries)
  (let ((targets (make-hash-table :test #'equal)))
    (dolist (entry entries targets)
      (setf (gethash (table-recalculate-target-key
                      (table-recalculate-entry-row entry)
                      (table-recalculate-entry-column entry))
                     targets)
            (table-recalculate-entry-owner entry)))))

(defun table-recalculate-region-includes-p (region row column)
  (destructuring-bind (first-row first-column last-row last-column) region
    (and (<= first-row row last-row)
         (<= first-column column last-column))))

(defun table-recalculate-check-cycles
    (entries owner-count target-map row-count column-count header-columns target)
  (let ((dependencies (make-array owner-count :initial-element nil))
        (dependents (make-array owner-count :initial-element nil)))
    (dolist (entry entries)
      (let* ((owner (table-recalculate-entry-owner entry))
             (assignment (table-recalculate-entry-assignment entry))
             (regions
               (table-recalculate-expression-regions
                (table-calc-assignment-expression assignment)
                (table-recalculate-entry-row entry)
                (table-recalculate-entry-column entry)
                row-count column-count header-columns target)))
        (dolist (region regions)
          (if (and (= (first region) (third region))
                   (= (second region) (fourth region)))
              (multiple-value-bind (dependency present)
                  (gethash (table-recalculate-target-key
                            (first region) (second region)) target-map)
                (when (and present (not (member dependency (aref dependencies owner))))
                  (push dependency (aref dependencies owner))))
              (maphash
               (lambda (position dependency)
                 (when (and (table-recalculate-region-includes-p
                             region (car position) (cdr position))
                            (not (member dependency (aref dependencies owner))))
                   (push dependency (aref dependencies owner))))
               target-map)))))
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
    (entries row-count column-count header-columns target)
  (dolist (entry entries)
    (table-recalculate-expression-regions
     (table-calc-assignment-expression
      (table-recalculate-entry-assignment entry))
     (table-recalculate-entry-row entry)
     (table-recalculate-entry-column entry)
     row-count column-count header-columns target)))

(defun table-recalculate-evaluate-entries
    (entries standard-rows row-count column-count header-columns limits work-budget
     work-map read-work-map-p)
  (dolist (entry entries)
    (let* ((row (table-recalculate-entry-row entry))
           (column (table-recalculate-entry-column entry))
           (key (table-recalculate-target-key row column))
           (value
             (table-calc-evaluate-assignment
              (table-recalculate-entry-assignment entry)
              (lambda (reference)
                (table-recalculate-reference-position
                 reference row column row-count column-count header-columns
                 (table-recalculate-entry-assignment entry)))
              (lambda (reference-row reference-column)
                (if read-work-map-p
                    (multiple-value-bind (prior present)
                        (gethash (table-recalculate-target-key
                                  reference-row reference-column)
                                 work-map)
                      (if present prior
                          (table-recalculate-cell-value
                           standard-rows reference-row reference-column)))
                    (table-recalculate-cell-value
                     standard-rows reference-row reference-column)))
              :limits limits :work-budget work-budget)))
      (setf (gethash key work-map) value)))
  work-map)

(defun table-recalculate-final-values (column-values field-values)
  (let ((values (make-hash-table :test #'equal)))
    (maphash (lambda (key value) (setf (gethash key values) value)) column-values)
    (maphash (lambda (key value) (setf (gethash key values) value)) field-values)
    values))

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
              (properties (getf canonical-cell :properties)))
         (setf (getf properties :value) value
               (getf canonical-cell :properties) properties)))
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
    (nreverse patches)))

(defun table-recalculate-assignments-by-kind (assignments kind)
  (remove-if-not (lambda (assignment)
                   (eq (table-calc-assignment-kind assignment) kind))
                 assignments))

(defun table-recalculate (document target)
  "Recalculate a pipe table's TBLFM formulas and return its validated edit result."
  (let ((table (edit-resolve-node document target '(:table))))
    (unless (eq (node-property table :type) :org)
      (table-recalculate-signal-error
       table :unsupported-table-type "Only Org pipe tables can be recalculated"))
    (let ((formulas (node-property table :tblfm)))
      (when (> (length formulas) 1)
        (error 'org-table-unsupported-syntax
               :message "Multiple table formula directives are unsupported"
               :formula (first formulas)
               :position 0
               :feature :multiple-tblfm-directives))
      (unless formulas
        (return-from table-recalculate
          (make-edit-result :updated-document document :spans nil)))
      (let* ((limits (document-limits document))
             (program (table-calc-parse-formula (first formulas) limits))
             (assignments (table-calc-program-assignments program))
             (column-assignments
               (table-recalculate-assignments-by-kind assignments :column))
             (field-assignments
               (table-recalculate-assignments-by-kind assignments :field)))
        (multiple-value-bind (standard-rows data-rows header-row)
            (table-recalculate-standard-rows table)
          (let* ((row-count (length standard-rows))
                 (column-count
                   (reduce #'max standard-rows
                           :key (lambda (row) (length (node-children row)))
                           :initial-value 0))
                 (header-columns (table-recalculate-header-columns header-row)))
            (table-recalculate-validate-duplicates assignments table)
            (multiple-value-bind (column-entries column-owner-count)
                (table-recalculate-plan-column-assignments
                 column-assignments data-rows standard-rows column-count table)
              (multiple-value-bind (field-entries field-owner-count)
                  (table-recalculate-plan-field-assignments
                   field-assignments standard-rows row-count column-count table)
                (table-recalculate-validate-entry-references
                 column-entries row-count column-count header-columns table)
                (table-recalculate-validate-entry-references
                 field-entries row-count column-count header-columns table)
                (table-recalculate-check-cycles
                 column-entries column-owner-count
                 (table-recalculate-entry-target-map column-entries)
                 row-count column-count header-columns table)
                (table-recalculate-check-cycles
                 field-entries field-owner-count
                 (table-recalculate-entry-target-map field-entries)
                 row-count column-count header-columns table)
                (let* ((work-budget (make-table-calc-work-budget))
                       (column-values
                         (table-recalculate-evaluate-entries
                          column-entries standard-rows row-count column-count
                          header-columns limits work-budget
                          (make-hash-table :test #'equal) t))
                       (field-values
                         (table-recalculate-evaluate-entries
                          field-entries standard-rows row-count column-count
                          header-columns limits work-budget
                          (make-hash-table :test #'equal) nil))
                       (values (table-recalculate-final-values
                                column-values field-values))
                       (patches (table-recalculate-patches
                                 values standard-rows)))
                  (if patches
                      (edit-apply-patches-result
                       document patches :target table
                       :expected
                       (table-recalculate-expected-canonical
                        document values standard-rows))
                      (make-edit-result :updated-document document :spans nil)))))))))))

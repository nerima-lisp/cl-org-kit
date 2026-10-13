;;;; src/table-sort.lisp
;;;;
;;;; Stable, format-preserving sorting for Org pipe-table rows.
(in-package #:org-kit)

(defstruct (table-sort-entry (:constructor %make-table-sort-entry (row canonical key))
                             (:copier nil))
  row
  canonical
  key)

(defun table-sort-check-output-limits (document source)
  (let* ((limits (document-limits document))
         (output-length (edit-prospective-output-length document source nil)))
    (check-limit limits :max-output-length output-length)
    (check-limit limits :max-output-octets
                 (edit-prospective-output-octets document source nil))))

(defun table-sort-table-metadata (table)
  (loop with row-count = 0
        with rule-count = 0
        with width = 0
        with row-section-count = 0
        with row-section-p = nil
        for row in (node-children table)
        for row-type = (node-property row :type)
        do (if (eq row-type :rule)
               (progn
                 (incf rule-count)
                 (setf row-section-p nil))
               (progn
                 (incf row-count)
                 (unless row-section-p
                   (incf row-section-count)
                   (setf row-section-p t))
                 (setf width (max width (length (node-children row))))))
        finally (return (values row-count rule-count width
                                row-section-count))))

(defun table-sort-range-sections (table start end)
  (loop with section = 0
        with row-index = 0
        with start-section = nil
        with end-section = nil
        for row in (node-children table)
        for row-type = (node-property row :type)
        do (if (eq row-type :rule)
               (incf section)
               (progn
                 (incf row-index)
                 (when (= row-index start)
                   (setf start-section section))
                 (when (= row-index end)
                   (setf end-section section))))
        finally (return (values start-section end-section))))

(defun table-sort-cell-value (row column)
  (loop for cell in (node-children row)
        for index from 1
        when (= index column)
          do (return (node-property cell :value))
        finally (return "")))

(defun table-sort-key (row column type limits target)
  (let ((value (table-sort-cell-value row column)))
    (case type
      (:lexical value)
      (:numeric
       (when (plusp (length value))
         (or (parse-safe-decimal value :limits limits)
             (table-align-signal-error target :invalid-number
                                       "Table cell is not a valid decimal number")))))))

(defun table-sort-entry-before-p (left right order type)
  (let ((left-key (table-sort-entry-key left))
        (right-key (table-sort-entry-key right)))
    (cond ((null left-key) nil)
          ((null right-key) t)
          ((eq type :lexical)
           (if (eq order :ascending)
               (string< left-key right-key)
               (string> left-key right-key)))
          (t
           (let ((comparison
                   (safe-decimal-compare left-key right-key)))
             (if (eq order :ascending)
                 (minusp comparison)
                 (plusp comparison)))))))

(defun table-sort-row-content-end (source row)
  (let ((end (node-end row)))
    (when (char= (char source (1- end)) #\Newline)
      (decf end)
      (when (char= (char source (1- end)) #\Return)
        (decf end)))
    end))

(defun table-sort-replacement (source original-entries sorted-entries)
  (with-output-to-string (stream)
    (loop for index below (length sorted-entries)
          for sorted-row = (table-sort-entry-row (aref sorted-entries index))
          for separator-row = (table-sort-entry-row (aref original-entries index))
          for sorted-content-end = (table-sort-row-content-end source sorted-row)
          for separator-start = (table-sort-row-content-end source separator-row)
          do (write-string source stream
                           :start (node-begin sorted-row)
                           :end sorted-content-end)
             (write-string source stream
                           :start separator-start
                           :end (node-end separator-row)))))

(defun table-sort-expected-children (table canonical-table start end sorted-entries)
  (let* ((source-children (node-children table))
         (canonical-children (getf canonical-table :children))
         (result (make-array (length source-children)))
         (row-index 0))
    (loop for row in source-children
          for canonical-row in canonical-children
          for child-index from 0
          for row-type = (node-property row :type)
          do (if (eq row-type :rule)
                 (setf (aref result child-index) canonical-row)
                 (progn
                   (incf row-index)
                   (setf (aref result child-index)
                         (if (<= start row-index end)
                             (table-sort-entry-canonical
                              (aref sorted-entries (- row-index start)))
                             canonical-row)))))
    (coerce result 'list)))

(defun table-sort-rows (document target column
                        &key (order :ascending) (type :lexical) start end)
  "Stable-sort rows in an Org pipe table and return its validated edit result."
  (let* ((table (edit-resolve-node document target '(:table)))
         (source (document-text document)))
    (when (eq (node-property table :type) :table.el)
      (table-align-signal-error target :unsupported-table-type
                                "Table.el tables cannot be sorted"))
    (unless (member order '(:ascending :descending))
      (table-align-signal-error target :invalid-value
                                "Table sort order must be ascending or descending"))
    (unless (member type '(:lexical :numeric))
      (table-align-signal-error target :invalid-value
                                "Table sort type must be lexical or numeric"))
    (table-sort-check-output-limits document source)
    (multiple-value-bind (row-count rule-count width row-section-count)
        (table-sort-table-metadata table)
      (when (zerop row-count)
        (table-align-signal-error target :invalid-range
                                  "Table has no standard rows to sort"))
      (unless (and (integerp column) (<= 1 column width))
        (table-align-signal-error target :invalid-index
                                  "Table sort column is outside the table width"))
      (when (and (or (null start) (null end))
                 (or (> row-section-count 1)
                     (and (or start end) (plusp rule-count))))
        (table-align-signal-error target :ambiguous-range
                                  "A row range is required for a table with sections"))
      (let ((range-start (or start 1))
            (range-end (or end row-count)))
        (unless (and (integerp range-start) (<= 1 range-start row-count)
                     (integerp range-end) (<= 1 range-end row-count))
          (table-align-signal-error target :invalid-index
                                    "Table sort row is outside the table"))
        (when (> range-start range-end)
          (table-align-signal-error target :invalid-range
                                    "Table sort start must not follow its end"))
        (multiple-value-bind (first-section last-section)
            (table-sort-range-sections table range-start range-end)
          (unless (eql first-section last-section)
            (table-align-signal-error target :invalid-range
                                      "Table sort range crosses a section boundary"))
          (let* ((limits (document-limits document))
                 (table-path (edit-node-path document table target))
                 (expected (edit-canonical-copy
                            (edit-canonical-document document)))
                 (canonical-table (edit-canonical-node-at-path expected table-path))
                 (selected-count (1+ (- range-end range-start)))
                 (original-entries (make-array selected-count))
                 (sorted-entries nil)
                 (selected-index 0)
                 (row-index 0))
            (loop for row in (node-children table)
                  for canonical-row in (getf canonical-table :children)
                  for row-type = (node-property row :type)
                  unless (eq row-type :rule)
                    do (incf row-index)
                       (when (<= range-start row-index range-end)
                         (setf (aref original-entries selected-index)
                               (%make-table-sort-entry
                                row canonical-row
                                (table-sort-key row column type limits target)))
                         (incf selected-index)))
            (setf sorted-entries (copy-seq original-entries))
            (stable-sort sorted-entries
                         (lambda (left right)
                           (table-sort-entry-before-p left right order type)))
            (setf (getf canonical-table :children)
                  (table-sort-expected-children table canonical-table
                                                range-start range-end
                                                sorted-entries))
            (let* ((first-row
                     (table-sort-entry-row (aref original-entries 0)))
                   (last-row
                     (table-sort-entry-row
                      (aref original-entries (1- selected-count))))
                   (patch-start (node-begin first-row))
                   (patch-end (node-end last-row))
                   (replacement
                     (table-sort-replacement source original-entries
                                             sorted-entries))
                   (patches
                     (if (string= replacement source
                                  :start2 patch-start :end2 patch-end)
                         nil
                         (list (edit-make-patch patch-start patch-end replacement
                                                target)))))
              (edit-apply-patches-result document patches
                                         :target target :expected expected))))))))

;;;; src/table-edit.lisp
(in-package #:org-kit)

(defstruct (table-edit-row (:constructor %make-table-edit-row)
                           (:conc-name %table-edit-row-)
                           (:copier nil))
  kind values cells section terminal-bar-p raw-content)

(defstruct (table-edit-formula (:constructor %make-table-edit-formula)
                               (:conc-name %table-edit-formula-)
                               (:copier nil))
  source edits kept-assignments assignments removed-p terminal-p
  value-start value-end output-length)

(defun table-edit-signal-error (target reason message)
  (error 'org-table-error
         :message message
         :target (edit-target-description target)
         :reason reason))

(defun table-edit-resolve-table (document target)
  (let ((table (edit-resolve-node document target '(:table))))
    (unless (eq (node-property table :type) :org)
      (table-edit-signal-error table :unsupported-table-type
                               "Only Org pipe tables can be edited"))
    table))

(defun table-edit-proper-values (values target maximum)
  (let ((count (edit-proper-list-length values)))
    (unless count
      (table-edit-signal-error target :invalid-value
                               "Table values must be a proper list"))
    (when (> count maximum)
      (table-edit-signal-error target :invalid-value
                               "Too many values for the table structure"))
    (dolist (value values)
      (unless (and (stringp value)
                   (not (find-if (lambda (character)
                                  (find character '(#\Return #\Newline)))
                                value)))
        (table-edit-signal-error target :invalid-value
                                 "Table values must be single-line strings")))
    values))

(defun table-edit-escaped-pipe-count (value)
  (loop with slash-count = 0
        with escaped-count = 0
        for character across value
        do (cond ((char= character #\\)
                  (incf slash-count))
                 ((char= character #\|)
                  (when (evenp slash-count)
                    (incf escaped-count))
                  (setf slash-count 0))
                 (t (setf slash-count 0)))
        finally (return escaped-count)))

(defun table-edit-escape-cell-value (value)
  (if (not (find #\| value))
      value
      (with-output-to-string (stream)
        (loop with slash-count = 0
              for character across value
              do (cond ((char= character #\\)
                        (write-char character stream)
                        (incf slash-count))
                       ((char= character #\|)
                        (when (evenp slash-count)
                          (write-char #\\ stream))
                        (write-char character stream)
                        (setf slash-count 0))
                       (t
                        (write-char character stream)
                        (setf slash-count 0)))))))

(defun table-edit-values-with-width (values width)
  (let ((result nil)
        (remaining values))
    (loop repeat width
          do (push (if remaining
                       (prog1 (car remaining)
                         (setf remaining (cdr remaining)))
                       "")
                   result))
    (nreverse result)))

(defun table-edit-cells-with-width (cells width)
  (append (copy-list cells)
          (make-list (- width (length cells)))))

(defun table-edit-provided-values-with-width (values width)
  (table-edit-values-with-width (mapcar #'table-edit-escape-cell-value values)
                                width))

(defun table-edit-check-value-lower-bound (document values)
  (let ((characters 0)
        (escaped-bars 0)
        (limits (document-limits document)))
    (dolist (value values)
      (incf characters (length value))
      (incf escaped-bars (table-edit-escaped-pipe-count value)))
    (when (plusp escaped-bars)
      (incf characters escaped-bars))
    (check-limit limits :max-output-length characters)
    (let ((octets 0)
          (encoding (or (document-encoding document) :utf-8)))
      (dolist (value values)
        (incf octets
              (edit-encoded-range-octets value 0 (length value) encoding)))
      (when (plusp escaped-bars)
        (incf octets
              (* escaped-bars
                 (edit-encoded-range-octets "\\" 0 1 encoding))))
      (if (document-encoding document)
          (check-limit limits :max-input-length octets)
          (check-limit limits :max-input-length characters))
      (check-limit limits :max-output-octets octets))))

(defun table-edit-values-count (values)
  (loop for value in values count t))

(defun table-edit-read-rows (source table)
  (let ((models nil)
        (section 0)
        (standard-count 0)
        (rule-width 0)
        (width 0))
    (dolist (row (node-children table))
      (let* ((kind (node-property row :type))
             (line-end (table-row-content-end source row))
             (terminal-p (table-row-terminal-bar-p source row line-end)))
        (if (eq kind :rule)
            (progn
              (setf rule-width
                    (max rule-width
                         (length (table-hline-segment-ranges
                                  source (node-begin row) line-end))))
              (push (%make-table-edit-row :kind :rule :values nil
                                          :cells nil
                                          :section section
                                          :terminal-bar-p terminal-p
                                          :raw-content
                                          (subseq source (node-begin row) line-end))
                    models)
              (incf section))
            (let* ((cells (node-children row))
                   (values (mapcar (lambda (cell)
                                     (node-property cell :value))
                                   cells)))
              (incf standard-count)
              (setf width (max width (table-edit-values-count values)))
              (push (%make-table-edit-row :kind :standard :values values
                                          :cells cells
                                          :section section
                                          :terminal-bar-p terminal-p
                                          :raw-content
                                          (subseq source (node-begin row) line-end))
                    models)))))
    (when (zerop standard-count)
      (setf width rule-width))
    (values (nreverse models) standard-count width)))

(defun table-edit-insertion-section (models row-count index)
  (let ((seen 0)
        (last-section 0)
        (target-section nil))
    (dolist (row models)
      (when (eq (%table-edit-row-kind row) :standard)
        (incf seen)
        (setf last-section (%table-edit-row-section row))
        (when (= seen index)
          (setf target-section (%table-edit-row-section row)))))
    (or target-section (if (plusp row-count) last-section 0))))

(defun table-edit-normalize-rows (models width)
  (mapcar (lambda (row)
            (if (eq (%table-edit-row-kind row) :standard)
                (%make-table-edit-row
                 :kind :standard
                 :values (table-edit-values-with-width
                          (%table-edit-row-values row) width)
                 :cells (table-edit-cells-with-width
                         (%table-edit-row-cells row) width)
                 :section (%table-edit-row-section row)
                 :terminal-bar-p (%table-edit-row-terminal-bar-p row)
                 :raw-content (%table-edit-row-raw-content row))
                row))
          models))

(defun table-edit-ragged-rows-p (models width)
  (some (lambda (row)
          (and (eq (%table-edit-row-kind row) :standard)
               (/= (length (%table-edit-row-values row)) width)))
        models))

(defun table-edit-index-valid-p (index maximum &key insertion-p)
  (and (integerp index)
       (<= 1 index)
       (if insertion-p (<= index (1+ maximum)) (<= index maximum))))

(defun table-edit-check-index (table index maximum &key insertion-p)
  (unless (table-edit-index-valid-p index maximum :insertion-p insertion-p)
    (table-edit-signal-error table :invalid-index
                             "Table row or column index is out of range")))

(defun table-edit-validate-move-indexes (table from to maximum)
  (table-edit-check-index table from maximum)
  (table-edit-check-index table to maximum))

(defun table-edit-insert-row-model (models index row standard-count)
  (if (zerop standard-count)
      (cons row models)
      (let ((result nil)
            (seen 0))
        (dolist (current models)
          (when (and (eq (%table-edit-row-kind current) :standard)
                     (= (1+ seen) index))
            (push row result))
          (push current result)
          (when (eq (%table-edit-row-kind current) :standard)
            (incf seen)
            (when (and (= seen standard-count)
                       (= index (1+ standard-count)))
              (push row result))))
        (nreverse result))))

(defun table-edit-delete-row-model (models index)
  (let ((result nil)
        (seen 0))
    (dolist (row models)
      (if (eq (%table-edit-row-kind row) :standard)
          (progn
            (incf seen)
            (unless (= seen index)
              (push row result)))
          (push row result)))
    (nreverse result)))

(defun table-edit-move-row-models (models from to)
  (let* ((standard-rows
           (coerce (remove-if-not
                    (lambda (row)
                      (eq (%table-edit-row-kind row) :standard))
                    models)
                   'vector))
         (moving (aref standard-rows (1- from))))
    (unless (= from to)
      (if (< from to)
          (loop for index from (1- from) below (1- to)
                do (setf (aref standard-rows index)
                         (aref standard-rows (1+ index))))
          (loop for index downfrom (1- from) above (1- to)
                do (setf (aref standard-rows index)
                         (aref standard-rows (1- index)))))
      (setf (aref standard-rows (1- to)) moving))
    (let ((result nil)
          (standard-index 0))
      (dolist (row models)
        (if (eq (%table-edit-row-kind row) :standard)
            (progn
              (incf standard-index)
              (push (aref standard-rows (1- standard-index)) result))
            (push row result)))
      (nreverse result))))

(defun table-edit-insert-column-values (values index value)
  (let ((result nil)
        (column 0)
        (inserted-p nil))
    (dolist (current values)
      (when (and (= column (1- index)) (not inserted-p))
        (push value result)
        (setf inserted-p t))
      (push current result)
      (incf column))
    (unless inserted-p
      (push value result))
    (nreverse result)))

(defun table-edit-delete-column-values (values index)
  (loop for value in values
        for column from 1
        unless (= column index)
          collect value))

(defun table-edit-move-column-values (values from to)
  (let* ((cells (coerce values 'vector))
         (moving (aref cells (1- from))))
    (if (< from to)
        (loop for index from (1- from) below (1- to)
              do (setf (aref cells index) (aref cells (1+ index))))
        (loop for index downfrom (1- from) above (1- to)
              do (setf (aref cells index) (aref cells (1- index)))))
    (setf (aref cells (1- to)) moving)
    (loop for value across cells collect value)))

(defun table-edit-transform-column-rows (models action index from to values)
  (let ((remaining-values values))
    (mapcar (lambda (row)
              (if (eq (%table-edit-row-kind row) :standard)
                  (let* ((old-values (%table-edit-row-values row))
                         (old-cells (%table-edit-row-cells row))
                         (new-values
                           (case action
                             (:insert
                              (table-edit-insert-column-values
                               old-values index
                               (if remaining-values
                                   (prog1 (table-edit-escape-cell-value
                                           (car remaining-values))
                                     (setf remaining-values
                                           (cdr remaining-values)))
                                   "")))
                             (:delete
                              (table-edit-delete-column-values old-values index))
                             (:move
                              (table-edit-move-column-values old-values from to))))
                         (new-cells
                           (case action
                             (:insert
                              (table-edit-insert-column-values old-cells index nil))
                             (:delete
                              (table-edit-delete-column-values old-cells index))
                             (:move
                              (table-edit-move-column-values old-cells from to)))))
                    (%make-table-edit-row
                     :kind :standard :values new-values :cells new-cells
                     :section (%table-edit-row-section row)
                     :terminal-bar-p (%table-edit-row-terminal-bar-p row)))
                  row))
            models)))

(defun table-edit-zero-width-column-delete-models (models)
  (mapcar (lambda (row)
            (if (eq (%table-edit-row-kind row) :rule)
                (%make-table-edit-row
                 :kind :standard :values nil :cells nil
                 :section (%table-edit-row-section row)
                 :terminal-bar-p (%table-edit-row-terminal-bar-p row))
                row))
          models))

(defun table-edit-coordinate-map (value operation)
  (let ((kind (getf operation :kind))
        (index (getf operation :index))
        (from (getf operation :from))
        (to (getf operation :to)))
    (case kind
      (:insert (if (>= value index) (1+ value) value))
      (:delete (cond ((= value index) :invalid)
                     ((> value index) (1- value))
                     (t value)))
      (:move (cond ((= from to) value)
                   ((= value from) to)
                   ((and (< from to) (> value from) (<= value to))
                    (1- value))
                   ((and (> from to) (>= value to) (< value from))
                    (1+ value))
                   (t value))))))

(defun table-edit-normalize-decimal (digits)
  (let ((first-nonzero (position-if-not (lambda (character)
                                         (char= character #\0))
                                       digits)))
    (if first-nonzero
        (subseq digits first-nonzero)
        "0")))

(defun table-edit-decimal-increment (digits)
  (let ((result (copy-seq (table-edit-normalize-decimal digits))))
    (loop for index downfrom (1- (length result)) to 0
          for digit = (char result index)
          do (if (char= digit #\9)
                 (setf (char result index) #\0)
                 (progn
                   (setf (char result index)
                         (code-char (1+ (char-code digit))))
                   (return-from table-edit-decimal-increment result)))
          finally
             (return-from table-edit-decimal-increment
               (concatenate 'string "1" result)))))

(defun table-edit-decimal-decrement (digits)
  (let ((result (copy-seq (table-edit-normalize-decimal digits))))
    (loop for index downfrom (1- (length result)) to 0
          for digit = (char result index)
          do (if (char= digit #\0)
                 (setf (char result index) #\9)
                 (progn
                   (setf (char result index)
                         (code-char (1- (char-code digit))))
                   (return-from table-edit-decimal-decrement
                     (table-edit-normalize-decimal result)))))))

(defun table-edit-decimal-coordinate-map (digits operation)
  (case (getf operation :kind)
    (:insert (table-edit-decimal-increment digits))
    (:delete (table-edit-decimal-decrement digits))))

(defun table-edit-reference-component-edit (reference axis operation formula)
  (let* ((kind (if (eq axis :row)
                   (table-calc-reference-row-kind reference)
                   (table-calc-reference-column-kind reference)))
         (value (if (eq axis :row)
                    (table-calc-reference-row-value reference)
                    (table-calc-reference-column-value reference)))
         (start (if (eq axis :row)
                    (table-calc-reference-row-start reference)
                    (table-calc-reference-column-start reference)))
         (end (if (eq axis :row)
                  (table-calc-reference-row-end reference)
                  (table-calc-reference-column-end reference))))
    (when (and (eq kind :absolute) start end)
      (cond
        ((integerp value)
         (let ((mapped (table-edit-coordinate-map value operation)))
           (unless (eql mapped value)
             (list start end
                   (if (eq mapped :invalid)
                       (if (and (eq axis :column)
                                (getf operation :invalid-name-survives-p))
                           "0"
                           "INVALID")
                       (write-to-string mapped :base 10 :radix nil))))))
        (t
         (let ((mapped
                 (table-edit-decimal-coordinate-map
                  (subseq formula start end) operation)))
           (when mapped
             (list start end mapped))))))))

(defun table-edit-expression-reference-edits (expression operation formula)
  (let ((edits nil))
    (when (eq (table-calc-expression-kind expression) :reference)
      (let ((reference (table-calc-expression-value expression)))
        (dolist (axis '(:row :column))
          (when (eq axis (getf operation :axis))
            (let ((edit (table-edit-reference-component-edit
                         reference axis operation formula)))
              (when edit (push edit edits)))))))
    (dolist (child (table-calc-expression-children expression))
      (dolist (edit (table-edit-expression-reference-edits
                     child operation formula))
        (push edit edits)))
    (nreverse edits)))

(defun table-edit-assignment-reference-edits (assignment operation formula)
  (let ((edits nil)
        (target (table-calc-assignment-target assignment)))
    (when (eq (getf operation :axis) :row)
      (let ((edit (table-edit-reference-component-edit
                   target :row operation formula)))
        (when edit (push edit edits))))
    (when (eq (getf operation :axis) :column)
      (let ((edit (table-edit-reference-component-edit
                   target :column operation formula)))
        (when edit (push edit edits))))
    (dolist (edit (table-edit-expression-reference-edits
                   (table-calc-assignment-expression assignment)
                   operation formula))
      (push edit edits))
    (nreverse edits)))

(defun table-edit-deleted-target-p (assignment operation)
  (and (eq (getf operation :kind) :delete)
       (let* ((target (table-calc-assignment-target assignment))
              (axis (getf operation :axis))
              (kind (if (eq axis :row)
                        (table-calc-reference-row-kind target)
                        (table-calc-reference-column-kind target)))
              (value (if (eq axis :row)
                         (table-calc-reference-row-value target)
                         (table-calc-reference-column-value target))))
         (and (eq kind :absolute)
              (eql value (getf operation :index))))))

(defun table-edit-edits-delta (edits)
  (loop for (start end replacement) in edits
        sum (- (length replacement) (- end start))))

(defun table-edit-apply-component-edits (source edits)
  (if (null edits)
      source
      (with-output-to-string (stream)
        (loop with cursor = 0
              for (start end replacement) in edits
              do (write-string source stream :start cursor :end start)
                 (write-string replacement stream)
                 (setf cursor end)
              finally (write-string source stream :start cursor)))))

(defun table-edit-formula-terminal-p (formula)
  (let ((tail-end (position-if-not
                   (lambda (character) (find character '(#\Space #\Tab)))
                   formula :from-end t)))
    (and tail-end
         (>= tail-end 1)
         (char= (char formula tail-end) #\:)
         (char= (char formula (1- tail-end)) #\:))))

(defun table-edit-formula-assignment-output-bounds (formula assignment)
  (let ((start (table-calc-assignment-start assignment))
        (end (table-calc-assignment-end assignment)))
    (loop while (and (plusp start)
                     (find (char formula (1- start))
                           '(#\Space #\Tab #\Return #\Newline)))
          do (decf start))
    (loop while (and (< end (length formula))
                     (find (char formula end)
                           '(#\Space #\Tab #\Return #\Newline)))
          do (incf end))
    (values start end)))

(defun table-edit-formula-output-length (formula kept-pairs removed-p terminal-p)
  (if (not removed-p)
      (+ (length formula)
         (loop for (assignment edits) in kept-pairs
               sum (table-edit-edits-delta edits)))
      (if (null kept-pairs)
          0
          (+ (loop for (assignment edits) in kept-pairs
                   sum (multiple-value-bind (start end)
                           (table-edit-formula-assignment-output-bounds
                            formula assignment)
                         (+ (- end start)
                            (table-edit-edits-delta edits))))
             (* 2 (1- (length kept-pairs)))
             (if terminal-p 2 0)))))

(defun table-edit-render-assignment (formula assignment edits)
  (multiple-value-bind (start end)
      (table-edit-formula-assignment-output-bounds formula assignment)
    (table-edit-apply-component-edits
     (subseq formula start end)
     (mapcar (lambda (edit)
               (list (- (first edit) start)
                     (- (second edit) start)
                     (third edit)))
             edits))))

(defun table-edit-formula-output (formula kept-pairs edits removed-p terminal-p)
  (if (not removed-p)
      (table-edit-apply-component-edits formula edits)
      (if (null kept-pairs)
          ""
          (with-output-to-string (stream)
            (loop for (assignment assignment-edits) in kept-pairs
                  for first-p = t then nil
                  unless first-p do (write-string "::" stream)
                  do (write-string (table-edit-render-assignment
                                    formula assignment assignment-edits)
                                   stream))
            (when terminal-p
              (write-string "::" stream))))))

(defun table-edit-formula-value-boundaries (source range)
  (let* ((start (car range))
         (end (cdr range))
         (newline (position #\Newline source :start start :end end))
         (line-end (or newline end))
         (content-end (if (char= (char source (1- line-end)) #\Return)
                          (1- line-end)
                          line-end))
         (colon (position #\: source :start start :end content-end)))
    (let ((value-start (1+ colon)))
      (loop while (and (< value-start content-end)
                       (find (char source value-start) '(#\Space #\Tab)))
            do (incf value-start))
      (loop while (and (> content-end value-start)
                       (find (char source (1- content-end)) '(#\Space #\Tab)))
            do (decf content-end))
      (values value-start content-end))))

(defun table-edit-formula-value-range (source range)
  (multiple-value-bind (start end)
      (table-edit-formula-value-boundaries source range)
    (cons start end)))

(defun table-edit-column-name-survives-delete-p (table name deleted-index)
  (block header
    (dolist (row (node-children table))
      (case (node-property row :type)
        (:rule (return-from header nil))
        (:standard
         (return-from header
           (loop for cell in (node-children row)
                 for index from 1
                 thereis (and (/= index deleted-index)
                              (string= (node-property cell :value) name)))))))))

(defun table-edit-plan-formulas (source table operation)
  (let ((formulas (node-property table :tblfm))
        (ranges (node-property table :reader-tblfm-ranges)))
    (when (and (eq (getf operation :axis) :column)
               (eq (getf operation :kind) :delete))
      (setf (getf operation :invalid-name-survives-p)
            (table-edit-column-name-survives-delete-p
             table "INVALID" (getf operation :index))))
    (when (> (length formulas) 1)
      (table-align-signal-error
       table :uneditable-structure
       "Multiple TBLFM directives cannot be remapped safely"))
    (loop for formula in formulas
          for range in ranges
          for value-range = (table-edit-formula-value-range source range)
          collect
          (multiple-value-bind (value-start value-end)
              (table-edit-formula-value-boundaries source range)
            (if (zerop (length formula))
                (%make-table-edit-formula
                 :source formula :edits nil :kept-assignments nil
                 :assignments nil :removed-p nil :terminal-p nil
                 :value-start value-start :value-end value-end
                 :output-length 0)
                (let* ((program
                         (handler-case
                             (table-calc-parse-formula
                              formula (document-limits (node-document table)))
                           (org-table-unsupported-syntax (condition)
                             (table-align-signal-error
                              table :uneditable-structure
                              (format nil
                                      "TBLFM syntax ~A at position ~D cannot be remapped"
                                      (string-downcase
                                       (symbol-name
                                        (org-table-unsupported-syntax-feature
                                         condition)))
                                      (org-table-unsupported-syntax-position
                                       condition))))))
                       (assignments (table-calc-program-assignments program))
                       (last-assignment (car (last assignments)))
                       (kept-pairs nil)
                       (all-edits nil)
                       (removed-p nil)
                       (removed-tail-p nil))
                  (dolist (assignment assignments)
                    (if (table-edit-deleted-target-p assignment operation)
                        (setf removed-p t
                              removed-tail-p
                              (or removed-tail-p
                                  (eq assignment last-assignment)))
                        (let ((edits
                                (table-edit-assignment-reference-edits
                                 assignment operation formula)))
                          (push (list assignment edits) kept-pairs)
                          (dolist (edit edits) (push edit all-edits)))))
                  (setf kept-pairs (nreverse kept-pairs)
                        all-edits (nreverse all-edits))
                  (let* ((terminal-p
                           (and kept-pairs
                                (or (table-edit-formula-terminal-p formula)
                                    removed-tail-p)))
                         (output-length
                           (table-edit-formula-output-length
                            formula kept-pairs removed-p terminal-p)))
                    (%make-table-edit-formula
                     :source formula :edits all-edits
                     :kept-assignments kept-pairs :assignments assignments
                     :removed-p removed-p
                     :terminal-p terminal-p
                     :value-start value-start :value-end value-end
                     :output-length output-length))))))))

(defun table-edit-render-row (row prefix widths alignments terminal-p)
  (or (%table-edit-row-raw-content row)
      (let ((column-count (length widths)))
        (with-output-to-string (stream)
          (write-string prefix stream)
          (write-char #\| stream)
          (cond
            ((eq (%table-edit-row-kind row) :rule)
             (loop for width across widths
                   for index from 0
                   do (when (plusp index) (write-char #\+ stream))
                      (write-string (table-hline-segment width) stream)
                   finally (when terminal-p (write-char #\| stream))))
            ((zerop column-count)
             (write-char #\| stream))
            (t
             (loop for value in (%table-edit-row-values row)
                   for column from 0
                   do (when (plusp column) (write-char #\| stream))
                      (write-string (table-format-cell
                                    value (aref widths column)
                                    (aref alignments column))
                                   stream)
                   finally (when terminal-p (write-char #\| stream)))))))))

(defun table-edit-row-text-length (row prefix-length widths terminal-p)
  (or (when (%table-edit-row-raw-content row)
        (length (%table-edit-row-raw-content row)))
      (let ((column-count (length widths)))
        (+ prefix-length 1
           (if (eq (%table-edit-row-kind row) :rule)
               (+ (loop for width across widths
                        sum (table-hline-formatted-segment-length width))
                  (1- column-count)
                  (if terminal-p 1 0))
               (if (zerop column-count)
                   1
                   (+ (loop for value in (%table-edit-row-values row)
                            for column from 0
                            sum (table-formatted-cell-length
                                 value (aref widths column)))
                      (1- column-count)
                      (if terminal-p 1 0))))))))

(defun table-edit-output-rows-length (models prefix-length widths
                                      newline-length final-newline-p)
  (+ (loop for row in models
           sum (table-edit-row-text-length row prefix-length widths
                                           (%table-edit-row-terminal-bar-p row)))
     (* newline-length
        (+ (max 0 (1- (length models))) (if final-newline-p 1 0)))))

(defun table-edit-one-character-octets (character encoding preamble)
  (- (edit-encoded-range-octets (string character) 0 1 encoding) preamble))

(defun table-edit-ascii-octets (count character encoding preamble)
  (* count (table-edit-one-character-octets character encoding preamble)))

(defun table-edit-rendered-row-octets (row source prefix-start prefix-end
                                       widths alignments terminal-p encoding
                                       preamble)
  (if (%table-edit-row-raw-content row)
      (- (edit-encoded-range-octets
          (%table-edit-row-raw-content row) 0
          (length (%table-edit-row-raw-content row)) encoding)
         preamble)
      (let ((total (- (edit-encoded-range-octets source prefix-start prefix-end
                                                  encoding)
                      preamble))
        (column-count (length widths)))
    (incf total (table-edit-ascii-octets 1 #\| encoding preamble))
    (if (eq (%table-edit-row-kind row) :rule)
        (loop for width across widths
              for index from 0
              do (when (plusp index)
                   (incf total (table-edit-ascii-octets 1 #\+ encoding
                                                        preamble)))
                 (incf total
                       (table-edit-ascii-octets
                        (table-hline-formatted-segment-length width)
                        #\- encoding preamble)))
        (if (zerop column-count)
            (incf total (table-edit-ascii-octets 1 #\| encoding preamble))
            (loop for value in (%table-edit-row-values row)
                  for column from 0
                  for extra = (table-cell-extra-padding value
                                                       (aref widths column))
                  for left = (case (aref alignments column)
                               (:right extra)
                               (:center (floor extra 2))
                               (otherwise 0))
                  do (when (plusp column)
                       (incf total (table-edit-ascii-octets 1 #\| encoding
                                                            preamble)))
                     (incf total (table-edit-ascii-octets (1+ left) #\Space
                                                          encoding preamble))
                     (incf total (- (edit-encoded-range-octets
                                     value 0 (length value) encoding)
                                    preamble))
                     (incf total (table-edit-ascii-octets
                                  (+ 1 (- extra left)) #\Space encoding
                                  preamble)))))
    (when (and terminal-p
               (or (plusp column-count)
                   (eq (%table-edit-row-kind row) :rule)))
      (incf total (table-edit-ascii-octets 1 #\| encoding preamble)))
        total)))

(defun table-edit-rendered-rows-octets (models source prefix-start prefix-end
                                        widths alignments newline final-newline-p
                                        encoding preamble)
  (+ (loop for row in models
           sum (table-edit-rendered-row-octets
                row source prefix-start prefix-end widths alignments
                (%table-edit-row-terminal-bar-p row) encoding preamble))
     (* (+ (max 0 (1- (length models))) (if final-newline-p 1 0))
        (- (edit-encoded-range-octets newline 0 (length newline) encoding)
           preamble))))

(defun table-edit-formula-output-octets (plan encoding preamble)
  (let* ((formula (%table-edit-formula-source plan))
         (kept (%table-edit-formula-kept-assignments plan))
         (edits (%table-edit-formula-edits plan)))
    (if (not (%table-edit-formula-removed-p plan))
        (+ (- (edit-encoded-range-octets formula 0 (length formula) encoding)
              preamble)
           (loop for (start end replacement) in edits
                 sum (- (- (edit-encoded-range-octets replacement 0
                                                        (length replacement)
                                                        encoding)
                           preamble)
                        (- (edit-encoded-range-octets formula start end encoding)
                           preamble))))
        (if (null kept)
            0
            (+ (loop for (assignment assignment-edits) in kept
                     sum (multiple-value-bind (start end)
                             (table-edit-formula-assignment-output-bounds
                              formula assignment)
                           (let ((old-bytes
                                   (- (edit-encoded-range-octets formula start end
                                                                 encoding)
                                      preamble)))
                             (+ old-bytes
                                (loop for (edit-start edit-end replacement)
                                      in assignment-edits
                                      sum (- (- (edit-encoded-range-octets
                                                 replacement 0 (length replacement)
                                                 encoding)
                                                preamble)
                                             (- (edit-encoded-range-octets
                                                 formula edit-start edit-end encoding)
                                                preamble)))))))
               (* 2 (1- (length kept))
                  (table-edit-one-character-octets #\: encoding preamble))
               (if (%table-edit-formula-terminal-p plan)
                   (* 2 (table-edit-one-character-octets #\: encoding preamble))
                   0))))))

(defun table-edit-output-octets (document source row-start row-end rows-octets
                                 formula-plans encoding)
  (let* ((preamble (edit-encoded-range-octets "" 0 0 encoding))
         (total (- (edit-encoded-range-octets source 0 (length source) encoding)
                   0)))
    (incf total
          (- rows-octets
             (- (edit-encoded-range-octets source row-start row-end encoding)
                preamble)))
    (dolist (plan formula-plans)
      (let ((start (%table-edit-formula-value-start plan))
            (end (%table-edit-formula-value-end plan)))
        (incf total
              (- (table-edit-formula-output-octets plan encoding preamble)
                 (- (edit-encoded-range-octets source start end encoding)
                    preamble)))))
    (when (and (document-bom-p document)
               (not (reader-generic-bom-encoding-p encoding)))
      (incf total (length (reader-output-bom encoding))))
    total))

(defun table-edit-preflight (document source row-start row-end row-output-length
                             formula-plans rows-octets)
  (let* ((limits (document-limits document))
         (output-length
           (+ (length source)
              (- row-output-length (- row-end row-start))
              (loop for plan in formula-plans
                    sum (- (%table-edit-formula-output-length plan)
                           (- (%table-edit-formula-value-end plan)
                              (%table-edit-formula-value-start plan))))
              (if (edit-output-bom-p document) 1 0))))
    (check-limit limits :max-output-length output-length)
    (unless (document-encoding document)
      (check-limit limits :max-input-length output-length))
    (let* ((encoding (or (document-encoding document) :utf-8))
           (output-octets
             (table-edit-output-octets document source row-start row-end
                                       rows-octets formula-plans encoding)))
      (when (document-encoding document)
        (check-limit limits :max-input-length output-octets))
      (check-limit limits :max-output-octets output-octets))))

(defun table-edit-render-rows (models prefix widths alignments newline final-newline-p)
  (with-output-to-string (stream)
    (loop for row in models
          for index from 0
          do (write-string
              (table-edit-render-row row prefix widths alignments
                                     (%table-edit-row-terminal-bar-p row))
              stream)
             (when (or (< index (1- (length models))) final-newline-p)
               (write-string newline stream)))))

(defun table-edit-formula-patches (formula-plans outputs)
  (loop for plan in formula-plans
        for output in outputs
        unless (string= output (%table-edit-formula-source plan))
          collect (edit-make-patch (%table-edit-formula-value-start plan)
                                   (%table-edit-formula-value-end plan)
                                   output)))

(defun table-edit-canonical-empty-cell ()
  (list :type :table-cell :post-blank 0
        :properties '(:value "") :children nil))

(defun table-edit-inserted-cell-canonicals (document models)
  (let ((values nil))
    (dolist (row models)
      (when (eq (%table-edit-row-kind row) :standard)
        (loop for cell in (%table-edit-row-cells row)
              for value in (%table-edit-row-values row)
              unless cell
                when (plusp (length value))
                  do (push value values))))
    (setf values (nreverse values))
    (when values
      (let* ((probe-source
               (with-output-to-string (stream)
                 (loop for value in values
                       for index from 0
                       do (write-string value stream)
                          (when (< index (1- (length values)))
                            (write-char #\Newline stream)))))
             (probe (make-document probe-source
                                   :limits (document-limits document)))
             (ranges nil)
             (position 0)
             (results nil)
             (original-context (%ensure-object-context document))
             (inserted-targets nil))
        (setf (document-settings probe) (document-settings document))
        (loop for value in values
              for index from 0
              do
          (push (cons position (+ position (length value))) ranges)
                (incf position (+ (length value)
                                  (if (< index (1- (length values))) 1 0))))
        (setf ranges (nreverse ranges))
        (setf (%document-object-context probe)
              (%make-object-context
               :radio-targets (%object-context-radio-targets original-context)
               :node-count 0))
        (dolist (range ranges)
          (setf inserted-targets
                (%radio-targets-in-range probe (car range) (cdr range)
                                         :table-cell inserted-targets)))
        (setf (%document-object-context probe)
              (%make-object-context
               :radio-targets
               (append (%object-context-radio-targets original-context)
                       inserted-targets)
               :node-count 0))
        (loop for value in values
              for range in ranges
              do (push
                  (list :type :table-cell :post-blank 0
                        :properties
                        (list :value (string-trim '(#\Space #\Tab) value))
                        :children
                        (mapcar #'edit-canonical-node
                                (parse-objects probe (car range) (cdr range)
                                               :table-cell)))
                  results))
        (nreverse results)))))

(defun table-edit-canonical-row (row cells)
  (list :type :table-row :post-blank 0
        :properties (list :type (%table-edit-row-kind row))
        :children cells))

(defun table-edit-canonical-cell-rows (document models width)
  (let ((inserted (table-edit-inserted-cell-canonicals document models)))
    (loop for row in models
          collect
          (if (eq (%table-edit-row-kind row) :rule)
              nil
              (let ((values (if (and (zerop width)
                                     (eq (%table-edit-row-kind row) :standard))
                                '("")
                                (%table-edit-row-values row)))
                    (source-cells (if (and (zerop width)
                                           (eq (%table-edit-row-kind row) :standard))
                                      '(nil)
                                      (%table-edit-row-cells row))))
                (loop for value in values
                      for source-cell in source-cells
                      collect
                      (cond (source-cell
                             (edit-canonical-node source-cell))
                            ((plusp (length value))
                             (pop inserted))
                            (t (table-edit-canonical-empty-cell)))))))))

(defun table-edit-canonical-expected (document table models width formulas)
  (let* ((expected (edit-canonical-copy (edit-canonical-document document)))
         (path (edit-node-path document table))
         (canonical-table (edit-canonical-node-at-path expected path))
         (properties (getf canonical-table :properties))
         (cell-rows (table-edit-canonical-cell-rows document models width)))
    (setf (getf canonical-table :children)
          (mapcar #'table-edit-canonical-row models cell-rows)
          (getf properties :tblfm)
          (mapcar (lambda (formula) (string-trim '(#\Space #\Tab) formula))
                  formulas)
          (getf canonical-table :properties) properties)
    expected))

(defun table-edit-keyword-canonical (formula)
  (list :type :keyword :post-blank 0
        :properties (list :key "TBLFM"
                          :value (string-trim '(#\Space #\Tab) formula)
                          :options nil)
        :children nil))

(defun table-edit-canonical-without-empty-table (document table formulas)
  (let* ((expected (edit-canonical-copy (edit-canonical-document document)))
         (path (edit-node-path document table))
         (parent-path (butlast path))
         (table-index (car (last path)))
         (parent (edit-canonical-node-at-path expected parent-path))
         (children (getf parent :children))
         (before nil)
         (after nil))
    (loop for child in children
          for index from 0
          do (cond ((< index table-index) (push child before))
                   ((> index table-index) (push child after))))
    (setf (getf parent :children)
          (append (nreverse before)
                  (mapcar #'table-edit-keyword-canonical formulas)
                  (nreverse after)))
    expected))

(defun table-edit-row-line-ending (source row)
  (let* ((content-end (table-row-content-end source row))
         (row-end (node-end row)))
    (subseq source content-end row-end)))

(defun table-edit-formula-digit-end (formula start)
  (loop for end from start below (length formula)
        while (table-calc-digit-p (char formula end))
        finally (return end)))

(defun table-edit-formula-coordinate-insert-adjustment
    (formula start end index-digits &optional digit-octets)
  (let* ((normalized-start
           (loop for position from start below end
                 when (char/= (char formula position) #\0)
                   return position
                 finally (return (1- end))))
         (normalized-length (- end normalized-start))
         (index-length (length index-digits))
         (comparison
           (cond ((< normalized-length index-length) :less)
                 ((> normalized-length index-length) :greater)
                 (t
                  (loop for offset below normalized-length
                        for source-digit =
                          (char formula (+ normalized-start offset))
                        for index-digit = (char index-digits offset)
                        when (char< source-digit index-digit) return :less
                        when (char> source-digit index-digit) return :greater
                        finally (return :equal))))))
    (if (eq comparison :less)
        0
        (let* ((all-nines-p
                 (loop for position from normalized-start below end
                       always (char= (char formula position) #\9)))
               (mapped-length (+ normalized-length (if all-nines-p 1 0)))
               (character-adjustment
                 (- mapped-length (- end start))))
          (if digit-octets
              (* character-adjustment digit-octets)
              character-adjustment)))))

(defun table-edit-formula-insert-growth (formula axis index &optional encoding)
  (let ((operation (list :kind :insert :axis axis :index index)))
    (if encoding
        (nth-value 1
          (table-edit-formula-reference-adjustment
           formula axis operation encoding))
        (table-edit-formula-reference-adjustment
         formula axis operation))))

(defun table-edit-insert-formula-adjustment
    (table operation &optional encoding)
  (let ((axis (getf operation :axis))
        (index (getf operation :index))
        (formulas (node-property table :tblfm))
        (adjustment 0))
    (loop for formula in formulas
          do (incf adjustment
                    (table-edit-formula-insert-growth
                     formula axis index encoding)))
    adjustment))

(defun table-edit-formula-span-normalized-start (formula start end)
  (loop for position from start below end
        when (char/= (char formula position) #\0)
          return position
        finally (return (1- end))))

(defun table-edit-formula-span-compare-integer (formula start end integer)
  (let* ((normalized-start
           (table-edit-formula-span-normalized-start formula start end))
         (normalized-length (- end normalized-start))
         (integer-digits (write-to-string integer :base 10 :radix nil)))
    (cond ((< normalized-length (length integer-digits)) :less)
          ((> normalized-length (length integer-digits)) :greater)
          (t
           (loop for offset below normalized-length
                 for source-digit = (char formula (+ normalized-start offset))
                 for integer-digit = (char integer-digits offset)
                 when (char< source-digit integer-digit) return :less
                 when (char> source-digit integer-digit) return :greater
                 finally (return :equal))))))

(defun table-edit-formula-span-all-p (formula start end character)
  (loop for position from start below end
        always (char= (char formula position) character)))

(defun table-edit-formula-span-decrement-length (formula start end)
  (let* ((normalized-start
           (table-edit-formula-span-normalized-start formula start end))
         (normalized-length (- end normalized-start)))
    (if (and (> normalized-length 1)
             (char= (char formula normalized-start) #\1)
             (table-edit-formula-span-all-p
              formula (1+ normalized-start) end #\0))
        (1- normalized-length)
        normalized-length)))

(defun table-edit-formula-span-increment-length (formula start end)
  (let* ((normalized-start
           (table-edit-formula-span-normalized-start formula start end))
         (normalized-length (- end normalized-start)))
    (+ normalized-length
       (if (table-edit-formula-span-all-p formula normalized-start end #\9)
           1
           0))))

(defun table-edit-formula-remap-coordinate-delta
    (formula start end axis operation encoding digit-octets)
  (let* ((old-length (- end start))
         (kind (getf operation :kind))
         (mapped-length nil)
         (sentinel nil))
    (case kind
      (:delete
       (case (table-edit-formula-span-compare-integer
              formula start end (getf operation :index))
         (:equal
          (setf sentinel
                (if (and (eq axis :column)
                         (getf operation :invalid-name-survives-p))
                    :zero
                    :invalid)))
         (:greater
          (setf mapped-length
                (table-edit-formula-span-decrement-length
                 formula start end)))))
      (:move
       (let ((from (getf operation :from))
             (to (getf operation :to)))
         (unless (= from to)
           (let ((from-order
                   (table-edit-formula-span-compare-integer
                    formula start end from))
                 (to-order
                   (table-edit-formula-span-compare-integer
                    formula start end to)))
             (cond
               ((eq from-order :equal)
                (setf mapped-length
                      (length (write-to-string to :base 10 :radix nil))))
               ((and (< from to) (eq from-order :greater)
                     (member to-order '(:less :equal)))
                (setf mapped-length
                      (table-edit-formula-span-decrement-length
                       formula start end)))
               ((and (> from to) (eq from-order :less)
                     (member to-order '(:greater :equal)))
                (setf mapped-length
                      (table-edit-formula-span-increment-length
                       formula start end)))))))))
    (let ((characters
            (cond (sentinel (- (if (eq sentinel :zero) 1 7) old-length))
                  (mapped-length (- mapped-length old-length))
                  (t 0))))
      (values
       characters
       (when encoding
         (let* ((old-octets (* old-length digit-octets))
                (new-octets
                  (cond (sentinel
                         (if (eq sentinel :zero)
                             digit-octets
                             (- (edit-encoded-range-octets
                                 "INVALID" 0 7 encoding)
                                (edit-encoded-range-octets "" 0 0 encoding))))
                        (mapped-length (* mapped-length digit-octets))
                        (t old-octets))))
           (- new-octets old-octets)))))))

(defun table-edit-formula-reference-adjustment
    (formula axis operation &optional encoding (start 0) (end (length formula)))
  (let* ((marker (if (eq axis :row) #\@ #\$))
         (position start)
         (characters 0)
         (octets 0)
         (preamble (and encoding
                        (edit-encoded-range-octets "" 0 0 encoding)))
         (digit-octets
           (and encoding
                (- (edit-encoded-range-octets "0" 0 1 encoding) preamble)))
         (index-digits
           (and (eq (getf operation :kind) :insert)
                (write-to-string (getf operation :index) :base 10 :radix nil))))
    (loop while (< position end)
          for character = (char formula position)
          do (cond
               ((or (char= character #\#) (char= character #\')
                    (= (char-code character) 96))
                (return))
               ((char= character #\;)
                (let ((separator (search "::" formula :start2 (1+ position)
                                                  :end2 end)))
                  (if separator
                      (setf position (+ separator 2))
                      (return))))
               ((char= character marker)
                (let* ((digits-start (1+ position))
                       (digits-end
                         (if (and (< digits-start end)
                                  (table-calc-digit-p
                                   (char formula digits-start)))
                             (loop for candidate from digits-start below end
                                   while (table-calc-digit-p
                                          (char formula candidate))
                                   finally (return candidate))
                             digits-start))
                       (coordinate-p
                         (and (> digits-end digits-start)
                              (or (eq axis :column)
                                  (and (< digits-end end)
                                       (char= (char formula digits-end) #\$))))))
                  (when coordinate-p
                    (if (eq (getf operation :kind) :insert)
                        (progn
                          (incf characters
                                (table-edit-formula-coordinate-insert-adjustment
                                 formula digits-start digits-end index-digits))
                          (when encoding
                            (incf octets
                                  (table-edit-formula-coordinate-insert-adjustment
                                   formula digits-start digits-end index-digits
                                   digit-octets))))
                        (multiple-value-bind (character-delta octet-delta)
                            (table-edit-formula-remap-coordinate-delta
                             formula digits-start digits-end axis operation
                             encoding digit-octets)
                          (incf characters character-delta)
                          (when encoding (incf octets octet-delta)))))
                  (setf position (if (> digits-end digits-start)
                                     digits-end
                                     (1+ position)))))
               (t (incf position))))
    (values characters (and encoding octets))))

(defun table-edit-formula-assignment-spans (formula)
  (let ((spans nil)
        (start 0)
        (end (length formula)))
    (loop while (< start end)
          for separator = (search "::" formula :start2 start)
          for assignment-end = (or separator end)
          do (push (cons start assignment-end) spans)
             (if separator
                 (setf start (+ separator 2))
                 (return)))
    (nreverse spans)))

(defun table-edit-formula-assignment-bounds (formula start end)
  (let ((trimmed-start start)
        (trimmed-end end))
    (loop while (and (< trimmed-start trimmed-end)
                     (find (char formula trimmed-start)
                           '(#\Space #\Tab #\Return #\Newline)))
          do (incf trimmed-start))
    (loop while (and (> trimmed-end trimmed-start)
                     (find (char formula (1- trimmed-end))
                           '(#\Space #\Tab #\Return #\Newline)))
          do (decf trimmed-end))
    (values trimmed-start trimmed-end)))

(defun table-edit-formula-target-deleted-p
    (formula start end axis operation)
  (let* ((equals (position #\= formula :start start :end end))
         (target-end (or equals end))
         (coordinate-start
           (if (eq axis :row)
               (and (< start target-end)
                    (char= (char formula start) #\@)
                    (< (1+ start) target-end)
                    (table-calc-digit-p (char formula (1+ start)))
                    (1+ start))
               (let ((dollar (position #\$ formula :start start :end target-end)))
                 (and dollar (< (1+ dollar) target-end)
                      (table-calc-digit-p (char formula (1+ dollar)))
                      (1+ dollar)))))
         (coordinate-end
           (when coordinate-start
             (table-edit-formula-digit-end formula coordinate-start))))
    (and equals coordinate-start coordinate-end
         (or (eq axis :column)
             (and (< coordinate-end target-end)
                  (char= (char formula coordinate-end) #\$)))
         (eq (table-edit-formula-span-compare-integer
              formula coordinate-start coordinate-end (getf operation :index))
             :equal))))

(defun table-edit-formula-remap-adjustment
    (source formula range axis operation &optional encoding)
  (multiple-value-bind (value-start value-end)
      (table-edit-formula-value-boundaries source range)
    (let* ((spans (table-edit-formula-assignment-spans formula))
           (last-span (car (last spans)))
           (kept nil)
           (removed-p nil)
           (removed-tail-p nil)
           (character-reference-delta 0)
           (octet-reference-delta 0)
           (preamble (and encoding
                          (edit-encoded-range-octets "" 0 0 encoding))))
      (dolist (span spans)
        (multiple-value-bind (start end)
            (table-edit-formula-assignment-bounds formula (car span) (cdr span))
          (if (and (eq (getf operation :kind) :delete)
                   (table-edit-formula-target-deleted-p
                    formula start end axis operation))
              (setf removed-p t
                    removed-tail-p (or removed-tail-p (eq span last-span)))
              (progn
                (push (cons start end) kept)
                (multiple-value-bind (characters octets)
                    (table-edit-formula-reference-adjustment
                     formula axis operation encoding start end)
                  (incf character-reference-delta characters)
                  (when octets (incf octet-reference-delta octets)))))))
      (setf kept (nreverse kept))
      (if (not removed-p)
          (values character-reference-delta
                  (and encoding octet-reference-delta))
          (let* ((terminal-p
                   (and kept
                        (or removed-tail-p
                            (table-edit-formula-terminal-p formula))))
                 (separator-count (max 0 (1- (length kept))))
                 (output-length
                   (+ (* 2 (+ separator-count (if terminal-p 1 0)))
                      (loop for (start . end) in kept sum (- end start))))
                 (character-delta
                   (+ (- output-length (length formula))
                      character-reference-delta)))
            (if (not encoding)
                (values character-delta nil)
                (let* ((separator-octets
                         (- (edit-encoded-range-octets "::" 0 2 encoding)
                            preamble))
                       (output-octets
                         (+ (* separator-count separator-octets)
                            (if terminal-p separator-octets 0)
                            (loop for (start . end) in kept
                                  sum (- (edit-encoded-range-octets
                                          source (+ value-start start)
                                          (+ value-start end) encoding)
                                         preamble))))
                       (source-octets
                         (- (edit-encoded-range-octets
                             source value-start value-end encoding)
                            preamble)))
                  (values character-delta
                          (+ (- output-octets source-octets)
                             octet-reference-delta)))))))))

(defun table-edit-remap-formula-adjustments
    (source table operation &optional encoding)
  (loop with characters = 0
        with octets = 0
        for formula in (node-property table :tblfm)
        for range in (node-property table :reader-tblfm-ranges)
        do (multiple-value-bind (character-delta octet-delta)
               (table-edit-formula-remap-adjustment
                source formula range (getf operation :axis) operation encoding)
             (incf characters character-delta)
             (when octet-delta (incf octets octet-delta)))
        finally (return (values characters (and encoding octets)))))

(defun table-edit-check-output-character-lower-bound
    (document source row-start row-end row-output-length table operation)
  (let* ((formula-adjustment
           (if (eq (getf operation :kind) :insert)
               (table-edit-insert-formula-adjustment table operation)
               (table-edit-remap-formula-adjustments
                source table operation)))
         (limits (document-limits document))
         (lower-bound
           (+ (length source)
              (- row-output-length (- row-end row-start))
              formula-adjustment
              (if (edit-output-bom-p document) 1 0))))
    (check-limit limits :max-output-length lower-bound)
    lower-bound))

(defun table-edit-check-output-octet-lower-bound
    (document source row-start row-end rows-octets table operation)
  (let* ((encoding (or (document-encoding document) :utf-8))
         (preamble (edit-encoded-range-octets "" 0 0 encoding))
         (total (edit-encoded-range-octets source 0 (length source) encoding))
         (formula-adjustment
           (if (eq (getf operation :kind) :insert)
               (table-edit-insert-formula-adjustment
                table operation encoding)
               (nth-value 1
                 (table-edit-remap-formula-adjustments
                  source table operation encoding)))))
    (incf total
          (- rows-octets
             (- (edit-encoded-range-octets source row-start row-end encoding)
                preamble)))
    (incf total formula-adjustment)
    (when (and (document-bom-p document)
               (not (reader-generic-bom-encoding-p encoding)))
      (incf total (length (reader-output-bom encoding))))
    (check-limit (document-limits document) :max-output-octets total)
    total))

(defun table-edit-operation-result
    (document table models width operation &optional original-ragged-p)
  (let* ((source (document-text document))
         (original-rows (node-children table))
         (row-start (node-begin table))
         (row-end (node-contents-end table))
         (preserve-raw-p
           (and (eq (getf operation :axis) :row)
                (member (getf operation :kind) '(:delete :move))
                (not original-ragged-p)))
         (normalized-models
           (progn
             (unless preserve-raw-p
               (dolist (row models)
                 (setf (%table-edit-row-raw-content row) nil)))
             (table-edit-normalize-rows models width)))
         (first-row (first original-rows))
         (line-end (table-row-content-end source first-row))
         (first-bar (table-row-first-bar source first-row line-end))
         (prefix (subseq source (node-begin first-row) first-bar))
         (prefix-start (node-begin first-row))
         (prefix-end first-bar)
         (last-row-end (node-contents-end table))
         (final-newline-p
           (char= (char source (1- last-row-end)) #\Newline))
         (newline (let ((line-ending
                          (table-edit-row-line-ending source first-row)))
                    (if (plusp (length line-ending))
                        line-ending
                        (string #\Newline))))
         (newline-length (length newline)))
    (when (and (eq (getf operation :axis) :column)
               (eq (getf operation :kind) :delete))
      (setf (getf operation :invalid-name-survives-p)
            (table-edit-column-name-survives-delete-p
             table "INVALID" (getf operation :index))))
    (multiple-value-bind (widths alignments)
        (table-edit-row-width-layout normalized-models width)
      (let ((row-output-length
              (table-edit-output-rows-length
               normalized-models (length prefix) widths newline-length
               final-newline-p)))
        (table-edit-check-output-character-lower-bound
         document source row-start row-end row-output-length table operation)
        (let* ((encoding (or (document-encoding document) :utf-8))
               (preamble (edit-encoded-range-octets "" 0 0 encoding))
               (rows-octets
                 (table-edit-rendered-rows-octets
                  normalized-models source prefix-start prefix-end widths
                  alignments newline final-newline-p encoding preamble)))
          (table-edit-check-output-octet-lower-bound
           document source row-start row-end rows-octets table operation)
          (let ((formats (table-edit-plan-formulas source table operation)))
            (table-edit-preflight document source row-start row-end
                                  row-output-length formats rows-octets)
          (let* ((row-output
                   (table-edit-render-rows normalized-models prefix widths alignments
                                           newline final-newline-p))
                 (formula-outputs
                   (mapcar (lambda (plan)
                             (table-edit-formula-output
                              (%table-edit-formula-source plan)
                              (%table-edit-formula-kept-assignments plan)
                              (%table-edit-formula-edits plan)
                              (%table-edit-formula-removed-p plan)
                              (%table-edit-formula-terminal-p plan)))
                           formats))
                 (formulas formula-outputs)
                 (patches (table-edit-formula-patches formats formula-outputs))
                 (expected
                   (if (and (null normalized-models)
                            (zerop (table-edit-values-count models)))
                       (table-edit-canonical-without-empty-table
                        document table formulas)
                       (table-edit-canonical-expected
                        document table normalized-models width formulas))))
            (unless (string= row-output (subseq source row-start row-end))
              (push (edit-make-patch row-start row-end row-output table) patches))
            (edit-apply-patches-result document (nreverse patches)
                                       :target table :expected expected))))))))

(defun table-edit-row-width-layout (models width)
  (let ((widths (make-array width :initial-element 1))
        (alignments (make-array width :initial-element nil)))
    (dolist (row models)
      (when (eq (%table-edit-row-kind row) :standard)
        (loop for value in (%table-edit-row-values row)
              for column from 0
              do (setf (aref widths column)
                       (max (aref widths column) (table-string-width value)))
                 (let ((alignment (table-cell-alignment-cookie value)))
                   (when (and alignment (null (aref alignments column)))
                     (setf (aref alignments column) alignment))))))
    (values widths
            (map 'vector (lambda (alignment) (or alignment :left)) alignments))))

(defun table-edit-run (document target action index &key from to values)
  (let* ((table (table-edit-resolve-table document target))
         (source (document-text document)))
    (multiple-value-bind (models row-count width)
        (table-edit-read-rows source table)
      (let* ((original-ragged-p (table-edit-ragged-rows-p models width))
             (models (table-edit-normalize-rows models width))
             (operation
               (list :axis (if (member action '(:insert-row :delete-row :move-row))
                               :row :column)
                     :kind (cond ((member action '(:insert-row :insert-column))
                                  :insert)
                                 ((member action '(:delete-row :delete-column))
                                  :delete)
                                 (t :move)))))
        (case action
          (:insert-row
           (table-edit-check-index table index row-count :insertion-p t)
           (let* ((provided (table-edit-proper-values values table width))
                  (new-row
                    (progn
                      (table-edit-check-value-lower-bound document provided)
                      (%make-table-edit-row
                       :kind :standard
                       :values (table-edit-provided-values-with-width provided width)
                       :cells (make-list width :initial-element nil)
                       :section
                       (table-edit-insertion-section models row-count index)
                       :terminal-bar-p
                       (%table-edit-row-terminal-bar-p (first models)))))
                  (new-models (table-edit-insert-row-model
                               models index new-row row-count)))
             (setf (getf operation :index) index)
             (table-edit-operation-result document table new-models width
                                          operation)))
          (:delete-row
           (table-edit-check-index table index row-count)
           (setf (getf operation :index) index)
           (table-edit-operation-result
            document table (table-edit-delete-row-model models index)
            width operation original-ragged-p))
          (:move-row
           (table-edit-validate-move-indexes table from to row-count)
           (setf (getf operation :from) from
                 (getf operation :to) to)
           (let ((source-row (loop with seen = 0
                                   for row in models
                                   when (eq (%table-edit-row-kind row) :standard)
                                     do (incf seen)
                                        (when (= seen from) (return row))))
                 (target-row (loop with seen = 0
                                   for row in models
                                   when (eq (%table-edit-row-kind row) :standard)
                                     do (incf seen)
                                        (when (= seen to) (return row)))))
             (unless (= from to)
               (unless (= (%table-edit-row-section source-row)
                          (%table-edit-row-section target-row))
                 (table-edit-signal-error table :invalid-range
                                          "A row cannot move across an hline boundary")))
             (table-edit-operation-result
              document table (table-edit-move-row-models models from to)
              width operation original-ragged-p)))
          ((:insert-column :delete-column :move-column)
           (let ((insertion-p (eq action :insert-column)))
             (if (eq action :move-column)
                 (table-edit-validate-move-indexes table from to width)
                 (table-edit-check-index table index width
                                         :insertion-p insertion-p)))
           (let* ((column-values
                    (when (eq action :insert-column)
                      (let ((provided
                              (table-edit-proper-values values table row-count)))
                        (table-edit-check-value-lower-bound document provided)
                        provided)))
                  (edit-kind (cond ((eq action :insert-column) :insert)
                                   ((eq action :delete-column) :delete)
                                   (t :move)))
                  (new-width (if (eq action :insert-column)
                                 (1+ width)
                                 (if (eq action :delete-column)
                                     (1- width)
                                     width)))
                  (transformed-models
                    (table-edit-transform-column-rows
                     models edit-kind index from to column-values))
                  (new-models
                    (if (and (eq action :delete-column) (zerop new-width))
                        (table-edit-zero-width-column-delete-models
                         transformed-models)
                        transformed-models)))
             (setf (getf operation :index) index
                   (getf operation :from) from
                   (getf operation :to) to)
             (table-edit-operation-result document table new-models new-width
                                          operation))))))))

(defun table-insert-row (document target index &key values)
  "Insert a row in a pipe table and return its validated edit result."
  (table-edit-run document target :insert-row index :values values))

(defun table-delete-row (document target index)
  "Delete a row from a pipe table and return its validated edit result."
  (table-edit-run document target :delete-row index))

(defun table-move-row (document target from to)
  "Move a row in a pipe table and return its validated edit result."
  (table-edit-run document target :move-row nil :from from :to to))

(defun table-insert-column (document target index &key values)
  "Insert a column in a pipe table and return its validated edit result."
  (table-edit-run document target :insert-column index :values values))

(defun table-delete-column (document target index)
  "Delete a column from a pipe table and return its validated edit result."
  (table-edit-run document target :delete-column index))

(defun table-move-column (document target from to)
  "Move a column in a pipe table and return its validated edit result."
  (table-edit-run document target :move-column nil :from from :to to))

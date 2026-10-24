;;;; src/table-align.lisp
(in-package #:org-kit)

(defun table-align-signal-error (target reason message)
  (error 'org-table-error
         :message message
         :target (edit-target-description target)
         :reason reason))

(defun table-row-content-end (source row)
  (let* ((start (node-begin row))
         (limit (node-end row))
         (newline (position #\Newline source :start start :end limit))
         (end (or newline limit)))
    (if (char= (char source (1- end)) #\Return)
        (1- end)
        end)))

(defun table-row-first-bar (source row line-end)
  (reader-next-table-delimiter source (node-begin row) line-end))

(defun table-row-last-bar (source first-bar line-end)
  (loop with position = first-bar
        with last-bar = first-bar
        for next-bar = (reader-next-table-delimiter source (1+ position) line-end)
        while next-bar
        do (setf last-bar next-bar
                 position next-bar)
        finally (return last-bar)))

(defun table-row-terminal-bar-p (source row line-end)
  (let* ((first-bar (table-row-first-bar source row line-end))
         (last-bar (table-row-last-bar source first-bar line-end)))
    (and (> last-bar first-bar)
         (loop for index from (1+ last-bar) below line-end
               always (find (char source index) '(#\Space #\Tab))))))

(defun table-line-prefix (source row first-bar)
  (subseq source (node-begin row) first-bar))

(defun table-cell-alignment-cookie (value)
  (cond ((string= value "<l>") :left)
        ((string= value "<c>") :center)
        ((string= value "<r>") :right)))

(defun table-cell-numeric-p (value)
  (let ((length (length value))
        (position 0)
        (digit-count 0))
    (when (and (< position length)
               (find (char value position) "<>"))
      (incf position))
    (when (and (< position length)
               (find (char value position) "+-"))
      (incf position))
    (let ((digits-start position))
      (loop while (and (< position length)
                       (table-calc-digit-p (char value position)))
            do (incf position))
      (incf digit-count (- position digits-start)))
    (when (and (< position length)
               (char= (char value position) #\.))
      (incf position)
      (let ((digits-start position))
        (loop while (and (< position length)
                         (table-calc-digit-p (char value position)))
              do (incf position))
        (incf digit-count (- position digits-start))))
    (unless (plusp digit-count)
      (return-from table-cell-numeric-p nil))
    (when (and (< position length)
               (find (char value position) "eE"))
      (incf position)
      (when (and (< position length)
                 (find (char value position) "+-"))
        (incf position))
      (let ((digits-start position))
        (loop while (and (< position length)
                         (table-calc-digit-p (char value position)))
              do (incf position))
        (when (= digits-start position)
          (return-from table-cell-numeric-p nil))))
    (= position length)))

(defun table-cell-width-cookie (value maximum-width)
  (let ((length (length value)))
    (when (and (> length 2)
               (char= (char value 0) #\<)
               (char= (char value (1- length)) #\>)
               (loop for index from 1 below (1- length)
                     always (digit-char-p (char value index) 10)))
      (loop with width = 0
            for index from 1 below (1- length)
            for digit = (digit-char-p (char value index) 10)
            do (setf width (min maximum-width (+ (* width 10) digit)))
            finally (return width)))))

(defun table-column-layout
    (rows column-count maximum-width values-for-row &key numeric-alignment-p)
  (let ((widths (make-array column-count :initial-element 1))
        (width-hints (make-array column-count :initial-element 0))
        (alignments (make-array column-count :initial-element nil))
        (numeric-columns (make-array column-count :initial-element nil))
        (header-row-p t))
    (dolist (row rows)
      (let ((row-values (funcall values-for-row row)))
        (loop for value in row-values
              for column from 0
              do (setf (aref widths column)
                       (max (aref widths column) (table-string-width value)))
                 (let ((alignment (table-cell-alignment-cookie value))
                       (width-hint
                         (table-cell-width-cookie value maximum-width)))
                   (when (and alignment (null (aref alignments column)))
                     (setf (aref alignments column) alignment))
                   (when width-hint
                     (setf (aref width-hints column)
                           (max (aref width-hints column) width-hint)))
                   (when (and numeric-alignment-p
                              (not header-row-p)
                              (plusp (length value))
                              (not alignment)
                              (not width-hint))
                     (setf (aref numeric-columns column)
                           (if (table-cell-numeric-p value)
                               (or (aref numeric-columns column) :numeric)
                               :non-numeric)))))
        (when row-values
          (setf header-row-p nil))))
    (loop for column below column-count
          do (setf (aref widths column)
                   (max (aref widths column) (aref width-hints column))))
    (values widths
            (map 'vector
                 (lambda (alignment numeric-column)
                   (or alignment
                       (when (and numeric-alignment-p
                                  (eq numeric-column :numeric))
                         :right)
                       :left))
                 alignments numeric-columns))))

(defun table-cell-extra-padding (value width)
  (max 0 (- width (table-string-width value))))

(defun table-format-cell (value width alignment)
  (let* ((extra (table-cell-extra-padding value width))
         (left-padding (case alignment
                         (:right extra)
                         (:center (floor extra 2))
                         (otherwise 0)))
         (right-padding (- extra left-padding)))
    (concatenate 'string
                 " "
                 (make-string left-padding :initial-element #\Space)
                 value
                 (make-string right-padding :initial-element #\Space)
                 " ")))

(defun table-formatted-cell-length (value width)
  (+ 2 (length value)
     (table-cell-extra-padding value width)))

(defun table-hline-segment-ranges (source start end)
  (let ((ranges nil)
        (segment-start start))
    (loop for index from start below end
          when (char= (char source index) #\+)
            do (push (cons segment-start index) ranges)
               (setf segment-start (1+ index)))
    (coerce (nreverse (cons (cons segment-start end) ranges)) 'vector)))

(defun table-hline-formatted-segment-length (width)
  (max 3 (+ width 2)))

(defun table-hline-segment (width)
  (make-string (table-hline-formatted-segment-length width)
               :initial-element #\-))

(defun table-row-indent-patch (source row line-end expected-prefix target)
  (let* ((first-bar (table-row-first-bar source row line-end))
         (prefix (table-line-prefix source row first-bar)))
    (unless (string= prefix expected-prefix)
      (list (edit-make-patch (node-begin row) first-bar expected-prefix target)))))

(defun table-standard-row-patches (source row line-end widths alignments target)
  (let* ((terminal-bar-p (table-row-terminal-bar-p source row line-end))
         (cells (node-children row))
         (cell-count (length cells))
         (column-count (length widths))
         (patches nil))
    (loop for cell in cells
          for column from 0
          for start = (node-begin cell)
          for end = (or (reader-next-table-delimiter source start line-end)
                        line-end)
          for value = (node-property cell :value)
          for replacement = (table-format-cell value
                                               (aref widths column)
                                               (aref alignments column))
          do (when (= column (1- cell-count))
               (setf replacement
                     (concatenate
                      'string replacement
                      (with-output-to-string (stream)
                        (loop for missing-column from cell-count below column-count
                              do (write-char #\| stream)
                                 (write-string
                                  (table-format-cell
                                   ""
                                   (aref widths missing-column)
                                   (aref alignments missing-column))
                                  stream))
                        (unless terminal-bar-p
                          (write-char #\| stream))))))
          unless (string= replacement (subseq source start end))
            do (push (edit-make-patch start end replacement target) patches))
    (nreverse patches)))

(defun table-rule-row-patches (source row line-end widths target)
  (let* ((first-bar (table-row-first-bar source row line-end))
         (last-bar (table-row-last-bar source first-bar line-end))
         (terminal-bar-p (table-row-terminal-bar-p source row line-end))
         (start (1+ first-bar))
         (end (if (> last-bar first-bar) last-bar line-end))
         (ranges (table-hline-segment-ranges source start end))
         (old-count (length ranges))
         (new-count (length widths))
         (patches nil))
    (if (= old-count new-count)
        (loop for range across ranges
              for index from 0
              for width across widths
              for segment = (table-hline-segment width)
              for replacement = (if (and (= index (1- new-count))
                                         (not terminal-bar-p))
                                    (concatenate 'string segment "|")
                                    segment)
              unless (string= replacement
                             (subseq source (car range) (cdr range)))
                do (push (edit-make-patch (car range) (cdr range)
                                          replacement target)
                         patches))
        (let ((tail-index (1- (min old-count new-count))))
          (loop for index below tail-index
                for range = (aref ranges index)
                for replacement = (table-hline-segment (aref widths index))
                unless (string= replacement
                               (subseq source (car range) (cdr range)))
                  do (push (edit-make-patch (car range) (cdr range)
                                            replacement target)
                           patches))
          (let* ((tail-range (aref ranges tail-index))
                 (replacement
                   (with-output-to-string (stream)
                     (loop for index from tail-index below new-count
                           do (when (> index tail-index)
                                (write-char #\+ stream))
                              (write-string
                               (table-hline-segment (aref widths index))
                               stream))
                     (unless terminal-bar-p
                       (write-char #\| stream)))))
            (push (edit-make-patch (car tail-range) end replacement target)
                  patches))))
    (nreverse patches)))

(defun table-standard-row-output-delta (source row line-end widths)
  (let* ((terminal-bar-p (table-row-terminal-bar-p source row line-end))
         (cells (node-children row))
         (cell-count (length cells))
         (delta 0))
    (loop for cell in cells
          for column from 0
          for start = (node-begin cell)
          for end = (or (reader-next-table-delimiter source start line-end)
                        line-end)
          for value = (node-property cell :value)
          do (incf delta
                   (- (table-formatted-cell-length value (aref widths column))
                      (- end start)))
             (when (= column (1- cell-count))
               (loop for missing-column from cell-count below (length widths)
                     do (incf delta)
                        (incf delta
                              (table-formatted-cell-length
                               "" (aref widths missing-column))))))
    (unless terminal-bar-p
      (incf delta))
    delta))

(defun table-rule-row-output-delta (source row line-end widths)
  (let* ((first-bar (table-row-first-bar source row line-end))
         (last-bar (table-row-last-bar source first-bar line-end))
         (terminal-bar-p (table-row-terminal-bar-p source row line-end))
         (start (1+ first-bar))
         (end (if (> last-bar first-bar) last-bar line-end))
         (new-length
           (+ (if terminal-bar-p
                  (1- (length widths))
                  (length widths))
              (loop for width across widths
                    sum (table-hline-formatted-segment-length width)))))
    (- new-length (- end start))))

(defun table-alignment-output-length (document rows widths expected-prefix)
  (let ((source (document-text document))
        (output-length (length (document-text document))))
    (dolist (row rows)
      (let* ((line-end (table-row-content-end source row))
             (first-bar (table-row-first-bar source row line-end))
             (prefix-length (- first-bar (node-begin row))))
        (incf output-length (- (length expected-prefix) prefix-length))
        (incf output-length
              (if (eq (node-property row :type) :rule)
                  (table-rule-row-output-delta source row line-end widths)
                  (table-standard-row-output-delta source row line-end widths)))))
    (+ output-length (if (edit-output-bom-p document) 1 0))))

(defun table-check-alignment-output-limits (document rows widths expected-prefix)
  (let* ((limits (document-limits document))
         (output-length
           (table-alignment-output-length document rows widths expected-prefix)))
    (check-limit limits :max-output-length output-length)
    ;; Every emitted character occupies at least one octet. This catches
    ;; oversized padding before constructing the replacement strings.
    ;; Preserve the established first-failure order for each encoding path.
    (dolist (limit-name
              (if (document-encoding document)
                  '(:max-output-octets :max-input-length)
                  '(:max-input-length :max-output-octets)))
      (check-limit limits limit-name output-length))))

(defun table-align-patches (document table rows widths alignments expected-prefix)
  (let ((source (document-text document))
        (patches nil))
    (dolist (row rows)
      (let* ((line-end (table-row-content-end source row))
             (prefix-patch
               (table-row-indent-patch source row line-end expected-prefix table))
             (row-patches
               (if (eq (node-property row :type) :rule)
                   (table-rule-row-patches source row line-end widths table)
                   (table-standard-row-patches
                    source row line-end widths alignments table))))
        (dolist (patch (append prefix-patch row-patches))
          (push patch patches))))
    (nreverse patches)))

(defun table-expected-canonical-document (document table column-count)
  (let* ((expected (edit-canonical-copy (edit-canonical-document document)))
         (path (edit-node-path document table))
         (canonical-table (edit-canonical-node-at-path expected path)))
    (setf (getf canonical-table :children)
          (mapcar
           (lambda (row)
             (when (eq (getf (getf row :properties) :type) :standard)
               (let* ((cells (getf row :children))
                      (missing (- column-count (length cells))))
                 (when (plusp missing)
                   (setf (getf row :children)
                         (append cells
                                 (loop repeat missing
                                       collect (list :type :table-cell
                                                     :post-blank 0
                                                     :properties '(:value "")
                                                     :children nil)))))))
             row)
           (getf canonical-table :children)))
    expected))

(defun table-align (document target &key numeric-alignment-p)
  "Align a pipe table and return its validated edit result."
  (let* ((table (edit-resolve-node document target '(:table)))
         (rows (node-children table)))
    (unless (eq (node-property table :type) :org)
      (table-align-signal-error table :unsupported-table-type
                                "Only Org pipe tables can be aligned"))
    (let* ((standard-rows
             (remove-if-not (lambda (row)
                              (eq (node-property row :type) :standard))
                            rows))
           (column-count
             (reduce #'max standard-rows
                     :key (lambda (row) (length (node-children row)))
                     :initial-value 0)))
      (unless (plusp column-count)
        (edit-signal-error table :uneditable-structure
                           "Pipe table has no standard cells"))
      (multiple-value-bind (widths alignments)
          (table-column-layout
           standard-rows column-count
           (1+ (limit-value (document-limits document) :max-output-length))
           (lambda (row)
             (mapcar (lambda (cell) (node-property cell :value))
                     (node-children row)))
           :numeric-alignment-p numeric-alignment-p)
        (let* ((source (document-text document))
               (first-row (first rows))
               (first-bar
                 (table-row-first-bar
                  source first-row (table-row-content-end source first-row)))
               (expected-prefix (table-line-prefix source first-row first-bar)))
          (table-check-alignment-output-limits
           document rows widths expected-prefix)
          (let ((patches (table-align-patches document table rows widths alignments
                                              expected-prefix))
                (expected
                  (table-expected-canonical-document
                   document table column-count)))
            (edit-apply-patches-result document patches
                                       :target table :expected expected)))))))

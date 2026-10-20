(in-package #:org-kit)

(defun table-transpose-models (standard-rows width)
  (loop for column below width
        collect (%make-table-edit-row
                 :kind :standard
                 :values (loop for row in standard-rows
                               collect (nth column (%table-edit-row-values row)))
                 :cells nil
                 :section 0
                 :terminal-bar-p t)))

(defun table-transpose-canonical-rows (standard-rows width)
  (loop for column below width
        collect (table-edit-canonical-row
                 (first standard-rows)
                 (loop for row in standard-rows
                       collect (edit-canonical-node
                                (nth column (%table-edit-row-cells row)))))))

(defun table-transpose-expected (document table standard-rows width)
  (let* ((expected (edit-canonical-copy (edit-canonical-document document)))
         (path (edit-node-path document table))
         (canonical-table (edit-canonical-node-at-path expected path)))
    (setf (getf canonical-table :children)
          (table-transpose-canonical-rows standard-rows width))
    expected))

(defun table-transpose-check-output-limits
    (document source row-start row-end row-output-length row-output-octets)
  (let* ((limits (document-limits document))
         (output-length
           (+ (- (length source) (- row-end row-start))
              row-output-length
              (if (edit-output-bom-p document) 1 0))))
    (check-limit limits :max-output-length output-length)
    (unless (document-encoding document)
      (check-limit limits :max-input-length output-length))
    (let* ((encoding (or (document-encoding document) :utf-8))
           (preamble (edit-encoded-range-octets "" 0 0 encoding))
           (source-octets
             (edit-encoded-range-octets source 0 (length source) encoding))
           (row-source-octets
             (- (edit-encoded-range-octets source row-start row-end encoding)
                preamble))
           (output-octets
             (+ source-octets (- row-output-octets row-source-octets))))
      (when (and (document-bom-p document)
                 (not (reader-generic-bom-encoding-p encoding)))
        (incf output-octets (length (reader-output-bom encoding))))
      (when (document-encoding document)
        (check-limit limits :max-input-length output-octets))
      (check-limit limits :max-output-octets output-octets))))

(defun table-transpose (document target)
  "Transpose a rectangular Org pipe table, remove hlines, and return an edit result.
TBLFM text is preserved unchanged."
  (let* ((table (table-edit-resolve-table document target))
         (source (document-text document))
         (rows (node-children table)))
    (unless (every (lambda (row)
                     (member (node-property row :type) '(:standard :rule)))
                   rows)
      (table-align-signal-error table :uneditable-structure
                                "Table contains an unsupported row type"))
    (multiple-value-bind (models standard-count width)
        (table-edit-read-rows source table)
      (let ((standard-rows
              (remove-if-not (lambda (row)
                               (eq (%table-edit-row-kind row) :standard))
                             models)))
        (unless (and (plusp standard-count) (plusp width))
          (table-align-signal-error table :uneditable-structure
                                    "Table has no standard cells"))
        (when (table-edit-ragged-rows-p models width)
          (table-align-signal-error table :ragged-table
                                    "Cannot transpose a ragged table"))
        (let* ((transposed (table-transpose-models standard-rows width))
               (row-start (node-begin table))
               (row-end (node-contents-end table))
               (first-row (first rows))
               (first-row-end (table-row-content-end source first-row))
               (first-bar (table-row-first-bar source first-row first-row-end))
               (prefix (table-line-prefix source first-row first-bar))
               (final-newline-p (char= (char source (1- row-end)) #\Newline))
               (newline (let ((line-ending
                                (table-edit-row-line-ending source first-row)))
                  (if (plusp (length line-ending))
                      line-ending
                      (string #\Newline))))
               (expected (table-transpose-expected
                          document table standard-rows width)))
          (multiple-value-bind (widths alignments)
              (table-column-layout
               transposed standard-count
               (1+ (limit-value (document-limits document)
                                :max-output-length))
               #'%table-edit-row-values :numeric-alignment-p t)
            (let* ((row-output-length
                     (table-edit-output-rows-length
                      transposed (length prefix) widths (length newline)
                      final-newline-p))
                   (encoding (or (document-encoding document) :utf-8))
                   (preamble (edit-encoded-range-octets "" 0 0 encoding))
                   (row-output-octets
                     (table-edit-rendered-rows-octets
                      transposed source row-start first-bar widths alignments
                      newline final-newline-p encoding preamble)))
              (table-transpose-check-output-limits
               document source row-start row-end row-output-length
               row-output-octets)
              (let ((replacement
                      (table-edit-render-rows transposed prefix widths alignments
                                              newline final-newline-p)))
                (edit-apply-patches-result document
                                           (if (string= replacement
                                                        (subseq source row-start row-end))
                                               nil
                                               (list (edit-make-patch row-start row-end
                                                                      replacement table)))
                                           :target table :expected expected)))))))))

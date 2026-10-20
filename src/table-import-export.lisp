(in-package #:org-kit)

(defun table-import-export-format (format text)
  (unless (member format '(:auto :csv :tsv))
    (table-align-signal-error "table import/export" :invalid-format
                              "Table format must be :auto, :csv, or :tsv"))
  (if (not (eq format :auto))
      format
      (let ((quoted-p nil)
            (tab-p nil)
            (comma-p nil))
        (loop with index = 0
              while (< index (length text))
              do (let ((character (char text index)))
                   (cond
                     ((char= character #\")
                      (if (and quoted-p
                               (< (1+ index) (length text))
                               (char= (char text (1+ index)) #\"))
                          (incf index)
                          (setf quoted-p (not quoted-p))))
                     ((not quoted-p)
                      (cond ((char= character #\Tab) (setf tab-p t))
                            ((char= character #\,) (setf comma-p t)))))
                   (incf index)))
        (if tab-p :tsv (if comma-p :csv :csv)))))

(defun table-import-export-parse-records (text format limits)
  (let ((separator (if (eq format :tsv) #\Tab #\,))
        (rows nil)
        (row nil)
        (field (make-string-output-stream))
        (quoted-p nil)
        (after-quote-p nil)
        (field-start-p t)
        (record-p nil)
        (node-count 3))
    (labels ((check-nodes ()
               (check-limit limits :max-node-count node-count))
             (append-character (character)
               (write-char character field)
               (setf record-p t field-start-p nil))
             (finish-field ()
               (incf node-count)
               (check-nodes)
               (push (get-output-stream-string field) row)
               (setf field (make-string-output-stream)
                     after-quote-p nil
                     field-start-p t
                     record-p t))
             (finish-row ()
               (finish-field)
               (incf node-count)
               (check-nodes)
               (push (nreverse row) rows)
               (setf row nil record-p nil)))
      (loop with index = 0
            while (< index (length text))
            do (let ((character (char text index)))
                 (cond
                   (quoted-p
                    (cond
                      ((char= character #\")
                       (if (and (< (1+ index) (length text))
                                (char= (char text (1+ index)) #\"))
                           (progn
                             (append-character #\")
                             (incf index))
                           (setf quoted-p nil after-quote-p t)))
                      ((find character '(#\Newline #\Return))
                       (table-align-signal-error
                        "table import" :multiline-field
                        "Delimited table fields must not contain line breaks"))
                      (t (append-character character))))
                   ((or (char= character #\Newline)
                        (char= character #\Return))
                    (when (and (char= character #\Return)
                               (< (1+ index) (length text))
                               (char= (char text (1+ index)) #\Newline))
                      (incf index))
                    (finish-row))
                   (after-quote-p
                    (unless (char= character separator)
                      (table-align-signal-error
                       "table import" :invalid-quoting
                       "Unexpected character after a quoted CSV field"))
                    (finish-field))
                   ((char= character separator)
                    (finish-field))
                   ((and (eq format :csv) (char= character #\"))
                    (if field-start-p
                        (setf quoted-p t field-start-p nil record-p t)
                        (table-align-signal-error
                         "table import" :invalid-quoting
                         "A quote may only begin a CSV field")))
                   (t (append-character character)))
                 (incf index)))
      (when quoted-p
        (table-align-signal-error "table import" :invalid-quoting
                                  "A quoted CSV field is not closed"))
      (when record-p (finish-row))
      (unless rows
        (incf node-count 2)
        (check-nodes)
        (push (list "") rows))
      (nreverse rows))))

(defun table-import-export-find-table (node)
  (or (when (eq (node-type node) :table) node)
      (some #'table-import-export-find-table (node-children node))))

(defun table-import-export-rows-source (rows newline)
  (with-output-to-string (stream)
    (dolist (row rows)
      (write-char #\| stream)
      (dolist (value row)
        (write-char #\Space stream)
        (write-string (table-edit-escape-cell-value value) stream)
        (write-string " |" stream))
      (write-string newline stream))))

(defun table-import-export-newline (source offset)
  (when (and (plusp offset)
             (< offset (length source))
             (char= (char source (1- offset)) #\Return)
             (char= (char source offset) #\Newline))
    (return-from table-import-export-newline (string #\Newline)))
  (let ((previous (position #\Newline source :end offset :from-end t)))
    (when previous
      (return-from table-import-export-newline
        (if (and (plusp previous)
                 (char= (char source (1- previous)) #\Return))
            (format nil "~C~C" #\Return #\Newline)
            (string #\Newline)))))
  (let ((next (position #\Newline source :start offset)))
    (when next
      (return-from table-import-export-newline
        (if (and (plusp next)
                 (char= (char source (1- next)) #\Return))
            (format nil "~C~C" #\Return #\Newline)
            (string #\Newline)))))
  (string #\Newline))

(defun table-import (document offset text &key (format :auto))
  "Insert CSV or TSV TEXT at OFFSET as an aligned Org pipe table.
FORMAT may be :AUTO, :CSV, or :TSV. Return the validated edit result."
  (unless (stringp text)
    (error 'type-error :datum text :expected-type 'string))
  (check-offset document offset)
  (let* ((limits (document-limits document))
         (source (document-text document)))
    (check-limit limits :max-input-length (length text))
    (let* ((format (table-import-export-format format text))
           (newline (table-import-export-newline source offset))
           (rows (table-import-export-parse-records text format limits))
           (table-source (table-import-export-rows-source rows newline))
           (temporary (parse-org table-source :limits limits))
           (table (table-import-export-find-table (document-root temporary))))
      (unless table
        (table-align-signal-error "table import" :invalid-value
                                  "Delimited input did not produce an Org table"))
      (let* ((aligned (edit-result-updated-document
                       (table-align temporary table)))
             (aligned-table (table-import-export-find-table
                             (document-root aligned)))
             (aligned-source (document-text aligned))
             (table-text (subseq aligned-source (node-begin aligned-table)
                                 (node-end aligned-table)))
             (insertion
               (concatenate
                'string
                (if (and (plusp offset)
                         (not (char= (char source (1- offset)) #\Newline)))
                    newline
                    "")
                table-text)))
        (let* ((patches (list (edit-make-patch offset offset insertion)))
               (prepared (edit-prepare-patches
                          document patches :target "table import"))
               (preview
                 (edit-reparse-source
                  document
                  (edit-splice-source
                   (edit-prepared-patches-source prepared)
                   (edit-prepared-patches-patches prepared))
                  "table import")))
          (setf (edit-prepared-patches-expected prepared)
                (edit-canonical-document preview))
          (edit-finish-prepared-patches prepared))))))

(defun table-import-export-table-rows (table)
  (unless (eq (node-property table :type) :org)
    (table-align-signal-error table :unsupported-table-type
                              "Only Org pipe tables can be exported"))
  (let* ((rows
           (loop for row in (node-children table)
                 unless (eq (node-property row :type) :rule)
                   collect (mapcar (lambda (cell) (node-property cell :value))
                                   (node-children row))))
         (width (reduce #'max rows :key #'length :initial-value 0)))
    (mapcar (lambda (row)
              (append row (make-list (- width (length row)) :initial-element "")))
            rows)))

(defun table-import-export-csv-quoted-p (value)
  (or (find #\, value) (find #\" value)))

(defun table-import-export-quoted-count (value)
  (count #\" value))

(defun table-import-export-output-length (rows format)
  (if rows
      (+ (length rows)
         (loop for row in rows
               sum (+ (max 0 (1- (length row)))
                      (loop for value in row
                            sum (+ (length value)
                                   (if (and (eq format :csv)
                                            (table-import-export-csv-quoted-p value))
                                       (+ 2 (table-import-export-quoted-count value))
                                       0)))))
      1)))

(defun table-import-export-output-octets (rows format encoding)
  (let ((separator (if (eq format :csv) "," (string #\Tab)))
        (line-feed (string #\Newline))
        (encoding (or encoding :utf-8)))
    (if rows
        (+ (* (length rows)
              (edit-encoded-range-octets line-feed 0 1 encoding))
           (loop for row in rows
                 sum (+ (* (max 0 (1- (length row)))
                           (edit-encoded-range-octets separator 0 1 encoding))
                        (loop for value in row
                              sum (+ (edit-encoded-range-octets
                                      value 0 (length value) encoding)
                                     (if (and (eq format :csv)
                                              (table-import-export-csv-quoted-p value))
                                         (* (+ 2 (table-import-export-quoted-count value))
                                            (edit-encoded-range-octets "\"" 0 1 encoding))
                                         0))))))
        (edit-encoded-range-octets line-feed 0 1 encoding))))

(defun table-import-export-write-rows (rows format &key (final-newline-p t))
  (with-output-to-string (stream)
    (loop for row in rows
          for remaining on rows
          do (loop for value in row
                   for first-p = t then nil
                   do (unless first-p
                        (write-char (if (eq format :csv) #\, #\Tab) stream))
                      (if (and (eq format :csv)
                               (table-import-export-csv-quoted-p value))
                          (progn
                            (write-char #\" stream)
                            (loop for character across value
                                  do (when (char= character #\")
                                       (write-char #\" stream))
                                     (write-char character stream))
                            (write-char #\" stream))
                          (write-string value stream)))
             (when (or final-newline-p (cdr remaining))
               (write-char #\Newline stream)))))

(defun table-export (document target &key (format :tsv))
  "Export an Org pipe table as CSV or TSV text without writing a file.
FORMAT may be :AUTO, :CSV, or :TSV; :AUTO follows Org's default TSV format."
  (unless (member format '(:auto :csv :tsv))
    (table-align-signal-error target :invalid-format
                              "Table format must be :auto, :csv, or :tsv"))
  (let* ((table (edit-resolve-node document target '(:table)))
         (format (if (eq format :auto) :tsv format))
         (rows (table-import-export-table-rows table))
         (limits (document-limits document))
         (length (table-import-export-output-length rows format))
         (octets (table-import-export-output-octets
                  rows format (document-encoding document))))
    (check-limit limits :max-output-length length)
    (check-limit limits :max-output-octets octets)
    (table-import-export-write-rows rows format)))

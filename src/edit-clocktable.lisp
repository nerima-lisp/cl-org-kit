;;;; src/edit-clocktable.lisp
(in-package #:org-kit)

(defun edit-clocktable-end-marker-p (source start end)
  (let ((trimmed-start start)
        (trimmed-end end))
    (loop while (and (< trimmed-start trimmed-end)
                     (find (char source trimmed-start) '(#\Space #\Tab)))
          do (incf trimmed-start))
    (loop while (and (> trimmed-end trimmed-start)
                     (find (char source (1- trimmed-end))
                           '(#\Space #\Tab #\Return)))
          do (decf trimmed-end))
    (and (= (- trimmed-end trimmed-start) 6)
         (string-equal source "#+END:" :start1 trimmed-start :end1 trimmed-end))))

(defun edit-clocktable-body-span (document block target)
  (let* ((source (document-text document))
         (start (node-begin block))
         (limit (node-end block))
         (opening-newline (position #\Newline source :start start :end limit)))
    (unless opening-newline
      (edit-signal-error target :uneditable-structure
                         "Dynamic block opener has no body boundary"))
    (let ((body-start (1+ opening-newline)))
      (let ((end
              (loop with line-start = body-start
                    for newline = (position #\Newline source
                                            :start line-start :end limit)
                    for line-end = (or newline limit)
                    when (edit-clocktable-end-marker-p
                          source line-start line-end)
                      return line-start
                    when (null newline)
                      return nil
                    do (setf line-start (1+ newline)))))
        (unless end
          (edit-signal-error target :uneditable-structure
                             "Dynamic block has no #+END line"))
        (values body-start end)))))

(defun edit-clocktable-newline (source block)
  (let ((newline
          (position #\Newline source :start (node-begin block)
                                      :end (node-end block))))
    (if (and newline (plusp newline)
             (char= (char source (1- newline)) #\Return))
        (format nil "~C~C" #\Return #\Newline)
        (string #\Newline))))

(defun edit-clocktable-title-range (title)
  (if (> (length title) 40)
      (values (or (position #\Space title :end 37 :from-end t) 37) t)
      (values (length title) nil)))

(defun edit-clocktable-headline (row)
  (let ((level (clocktable-row-level row))
        (title (clocktable-row-title row)))
    (multiple-value-bind (end shortened-p)
        (edit-clocktable-title-range title)
      (let ((shortened
              (if shortened-p
                  (concatenate 'string (subseq title 0 end) "...")
                  title)))
        (if (> level 1)
            (concatenate 'string "\\_"
                         (make-string (* 2 (1- level)) :initial-element #\Space)
                         shortened)
            shortened)))))

(defun edit-clocktable-check-indentation-size (document row)
  (let ((level (clocktable-row-level row)))
    (when (> level 1)
      (let* ((indent-length (* 2 (1- level)))
             (characters (+ 2 indent-length))
             (encoding (or (document-encoding document) :utf-8))
             (preamble-length
               (if (reader-generic-bom-encoding-p encoding)
                   (edit-encoded-range-octets "" 0 0 encoding)
                   0))
             (octets
               (+ (edit-clocktable-encoded-size "\\_" encoding preamble-length)
                  (* indent-length
                     (edit-clocktable-encoded-size
                      " " encoding preamble-length))))
             (limits (document-limits document)))
        (check-limit limits :max-output-length characters)
        (unless (document-encoding document)
          (check-limit limits :max-input-length characters))
        (when (document-encoding document)
          (check-limit limits :max-input-length octets))
        (check-limit limits :max-output-octets octets)))))

(defun edit-clocktable-time-text (minutes)
  (format nil "~D:~2,'0D" (floor minutes 60) (mod minutes 60)))

(defun edit-clocktable-decimal-digit-count (integer)
  (if (< integer 10)
      1
      (let* ((bits (integer-length integer))
             (upper (ceiling (* bits 3010299956639812)
                             10000000000000000)))
        (if (>= integer (expt 10 (1- upper))) upper (1- upper)))))

(defun edit-clocktable-encoded-size (text encoding preamble-length)
  (- (edit-encoded-range-octets text 0 (length text) encoding)
     preamble-length))

(defun edit-clocktable-check-time-size (document minutes total-p)
  (let* ((encoding (or (document-encoding document) :utf-8))
         (digits (edit-clocktable-decimal-digit-count (floor minutes 60)))
         (characters (+ digits 3 (if total-p 2 0)))
         (preamble-length
           (if (reader-generic-bom-encoding-p encoding)
               (edit-encoded-range-octets "" 0 0 encoding)
               0))
         (digit-octets
           (edit-clocktable-encoded-size "0" encoding preamble-length))
         (octets
           (+ (* (+ digits 2) digit-octets)
              (edit-clocktable-encoded-size ":" encoding preamble-length)
              (if total-p
                  (* 2 (edit-clocktable-encoded-size
                        "*" encoding preamble-length))
                  0)))
         (limits (document-limits document)))
    (check-limit limits :max-output-length characters)
    (unless (document-encoding document)
      (check-limit limits :max-input-length characters))
    (when (document-encoding document)
      (check-limit limits :max-input-length octets))
    (check-limit limits :max-output-octets octets)))

(defun edit-clocktable-headline-cells (headline)
  (let ((position 0)
        (cells nil)
        (limit (length headline)))
    (loop
      for delimiter = (reader-next-table-delimiter headline position limit)
      for end = (or delimiter limit)
      do (multiple-value-bind (start trimmed-end)
             (reader-table-cell-bounds headline position end)
           (push (subseq headline start trimmed-end) cells))
         (if delimiter
             (setf position (1+ delimiter))
             (return)))
    (coerce (nreverse cells) 'vector)))

(defun edit-clocktable-data-cells (row headline minutes time-columns)
  (let* ((headline-cells (edit-clocktable-headline-cells headline))
         (headline-count (length headline-cells))
         (cells (make-array (+ headline-count time-columns)
                            :initial-element "")))
    (replace cells headline-cells)
    (setf (aref cells
                (+ headline-count
                   (1- (min time-columns (clocktable-row-level row)))))
          minutes)
    cells))

(defun edit-clocktable-time-columns (rows)
  (if (some (lambda (row) (> (clocktable-row-level row) 1)) rows) 2 1))

(defun edit-clocktable-table-columns (rows time-columns)
  (loop with columns = (1+ time-columns)
        for row in rows
        for headline = (edit-clocktable-headline row)
        for row-columns = (+ time-columns
                             (length (edit-clocktable-headline-cells headline)))
        do (setf columns (max columns row-columns))
        finally (return columns)))

(defun edit-clocktable-update-widths (widths cells)
  (loop for cell across cells
        for column from 0
        do (setf (aref widths column)
                 (max (aref widths column) (table-string-width cell))))
  widths)

(defun edit-clocktable-fixed-cells (column-count first second)
  (let ((cells (make-array column-count :initial-element "")))
    (setf (aref cells 0) first
          (aref cells 1) second)
    cells))

(defun edit-clocktable-widths (result rows column-count time-columns)
  (let* ((widths (make-array column-count :initial-element 1))
         (total-time
           (concatenate 'string
                        "*"
                        (edit-clocktable-time-text
                         (clocktable-result-total-minutes result))
                        "*")))
    (edit-clocktable-update-widths
     widths (edit-clocktable-fixed-cells column-count "Headline" "Time"))
    (edit-clocktable-update-widths
     widths (edit-clocktable-fixed-cells column-count "*Total time*" total-time))
    (dolist (row rows)
      (edit-clocktable-update-widths
       widths
       (edit-clocktable-data-cells
        row (edit-clocktable-headline row)
        (edit-clocktable-time-text (clocktable-row-minutes row))
        time-columns)))
    widths))

(defun edit-clocktable-cell-char-length (value width)
  (+ 2 (length value)
     (max 0 (- width (table-string-width value)))))

(defun edit-clocktable-row-char-length (cells widths)
  (+ 1 (length widths)
     (loop for cell across cells
           for column from 0
           sum (edit-clocktable-cell-char-length cell (aref widths column)))
     (loop for column from (length cells) below (length widths)
           sum (edit-clocktable-cell-char-length "" (aref widths column)))))

(defun edit-clocktable-rule-char-length (widths)
  (+ 2 (1- (length widths))
     (loop for width across widths
           sum (table-hline-formatted-segment-length width))))

(defun edit-clocktable-row-octets (cells widths encoding preamble-length
                                   space-octets pipe-octets)
  (+ (* (1+ (length widths)) pipe-octets)
     (loop for cell across cells
           for column from 0
           for padding = (max 0 (- (aref widths column)
                                   (table-string-width cell)))
           sum (+ (* 2 space-octets)
                  (edit-clocktable-encoded-size cell encoding preamble-length)
                  (* padding space-octets)))
     (loop for column from (length cells) below (length widths)
           sum (+ (* 2 space-octets)
                  (* (aref widths column) space-octets)))))

(defun edit-clocktable-rule-octets (widths pipe-octets plus-octets hyphen-octets)
  (+ (* 2 pipe-octets)
     (* (1- (length widths)) plus-octets)
     (loop for width across widths
           sum (* (table-hline-formatted-segment-length width)
                  hyphen-octets))))

(defun edit-clocktable-body-metrics (result rows widths time-columns newline
                                    encoding)
  (let* ((preamble-length
           (if (reader-generic-bom-encoding-p encoding)
               (edit-encoded-range-octets "" 0 0 encoding)
               0))
         (space-octets
           (edit-clocktable-encoded-size " " encoding preamble-length))
         (pipe-octets
           (edit-clocktable-encoded-size "|" encoding preamble-length))
         (plus-octets
           (edit-clocktable-encoded-size "+" encoding preamble-length))
         (hyphen-octets
           (edit-clocktable-encoded-size "-" encoding preamble-length))
         (newline-octets
           (edit-clocktable-encoded-size newline encoding preamble-length))
         (header (edit-clocktable-fixed-cells
                  (length widths) "Headline" "Time"))
         (total (edit-clocktable-fixed-cells
                 (length widths) "*Total time*"
                 (concatenate 'string
                              "*"
                              (edit-clocktable-time-text
                               (clocktable-result-total-minutes result))
                              "*")))
         (characters 0)
         (octets 0))
    (labels ((add-row (cells)
               (incf characters
                     (+ (edit-clocktable-row-char-length cells widths)
                        (length newline)))
               (incf octets
                     (+ (edit-clocktable-row-octets
                         cells widths encoding preamble-length
                         space-octets pipe-octets)
                        newline-octets)))
             (add-rule ()
               (incf characters
                     (+ (edit-clocktable-rule-char-length widths)
                        (length newline)))
               (incf octets
                     (+ (edit-clocktable-rule-octets
                         widths pipe-octets plus-octets hyphen-octets)
                        newline-octets))))
      (add-row header)
      (add-rule)
      (add-row total)
      (when rows
        (add-rule)
        (dolist (row rows)
          (add-row
           (edit-clocktable-data-cells
            row (edit-clocktable-headline row)
            (edit-clocktable-time-text (clocktable-row-minutes row))
            time-columns))))
      (values characters octets))))

(defun edit-clocktable-source-base-octets (document start end encoding)
  (let* ((source (document-text document))
         (generic-preamble-p (reader-generic-bom-encoding-p encoding))
         (preamble-length
           (if generic-preamble-p
               (edit-encoded-range-octets "" 0 0 encoding)
               0)))
    (+ (- (edit-encoded-range-octets source 0 start encoding)
          preamble-length)
       (- (edit-encoded-range-octets source end (length source) encoding)
          preamble-length)
       preamble-length
       (if (and (document-bom-p document) (not generic-preamble-p))
           (length (reader-output-bom encoding))
           0))))

(defun edit-clocktable-check-output-limits (document start end body-characters
                                             body-octets)
  (let* ((source (document-text document))
         (encoding (or (document-encoding document) :utf-8))
         (limits (document-limits document))
         (output-length
           (+ (- (length source) (- end start))
              body-characters
              (if (edit-output-bom-p document) 1 0)))
         (output-octets
           (+ (edit-clocktable-source-base-octets
               document start end encoding)
              body-octets)))
    (check-limit limits :max-output-length output-length)
    (unless (document-encoding document)
      (check-limit limits :max-input-length output-length))
    (when (document-encoding document)
      (check-limit limits :max-input-length output-octets))
    (check-limit limits :max-output-octets output-octets)))

(defun edit-clocktable-write-row (stream cells newline)
  (write-char #\| stream)
  (loop for cell across cells
        do (write-char #\Space stream)
           (write-string cell stream)
           (write-char #\Space stream)
           (write-char #\| stream))
  (write-string newline stream))

(defun edit-clocktable-write-rule (stream column-count newline)
  (write-char #\| stream)
  (loop repeat column-count
        for column from 0
        do (write-string "---" stream)
           (when (< column (1- column-count))
             (write-char #\+ stream)))
  (write-char #\| stream)
  (write-string newline stream))

(defun edit-clocktable-body-text (result rows time-columns column-count newline)
  (let ((header (edit-clocktable-fixed-cells column-count "Headline" "Time"))
        (total (edit-clocktable-fixed-cells
                column-count "*Total time*"
                (concatenate 'string
                             "*"
                             (edit-clocktable-time-text
                              (clocktable-result-total-minutes result))
                             "*"))))
    (with-output-to-string (stream)
      (edit-clocktable-write-row stream header newline)
      (edit-clocktable-write-rule stream column-count newline)
      (edit-clocktable-write-row stream total newline)
      (when rows
        (edit-clocktable-write-rule stream column-count newline)
        (dolist (row rows)
          (edit-clocktable-write-row
           stream
           (edit-clocktable-data-cells
            row (edit-clocktable-headline row)
            (edit-clocktable-time-text (clocktable-row-minutes row))
            time-columns)
           newline))))))

(defun edit-clocktable-find-table (document)
  (let ((table nil))
    (map-nodes (lambda (node)
                 (when (and (null table) (eq (node-type node) :table))
                   (setf table node)))
               (document-root document))
    table))

(defun edit-clocktable-subtree-node-count (root)
  (let ((count 0))
    (map-nodes (lambda (node)
                 (declare (ignore node))
                 (incf count))
               root)
    count))

(defun edit-clocktable-check-node-budget (document block rows column-count)
  (let* ((row-count (length rows))
         (line-count (+ 3 row-count (if (plusp row-count) 1 0)))
         (cell-count (* column-count (+ 2 row-count)))
         (preserved-node-count
           (- (edit-clocktable-subtree-node-count (document-root document))
              (loop for child in (node-children block)
                    sum (edit-clocktable-subtree-node-count child))))
         (minimum-node-count
           (+ preserved-node-count 1 line-count cell-count)))
    (check-limit (document-limits document) :max-node-count minimum-node-count)))

(defun write-clocktable (document dynamic-block clocktable-result)
  "Replace a clocktable dynamic block body with RESULT's fixed table data."
  (unless (clocktable-result-p clocktable-result)
    (error 'type-error :datum clocktable-result
                        :expected-type 'clocktable-result))
  (let ((block (edit-resolve-node document dynamic-block '(:dynamic-block))))
    (unless (equal (node-property block :block-name) "clocktable")
      (edit-signal-error dynamic-block :wrong-node-type
                         "Dynamic block is not a clocktable"))
    (let* ((rows (%clocktable-result-rows clocktable-result))
           (source (document-text document))
           (newline (edit-clocktable-newline source block)))
      (multiple-value-bind (start end)
          (edit-clocktable-body-span document block dynamic-block)
        (edit-clocktable-check-time-size
         document (clocktable-result-total-minutes clocktable-result) t)
        (dolist (row rows)
          (edit-clocktable-check-time-size
           document (clocktable-row-minutes row) nil)
          (edit-clocktable-check-indentation-size document row)
          (when (or (find #\Newline (clocktable-row-title row))
                    (find #\Return (clocktable-row-title row)))
            (edit-signal-error dynamic-block :invalid-value
                               "Clocktable headline contains a line break")))
        (let* ((time-columns (edit-clocktable-time-columns rows))
               (column-count
                 (edit-clocktable-table-columns rows time-columns)))
          (edit-clocktable-check-node-budget
           document block rows column-count)
          (let ((widths (edit-clocktable-widths
                         clocktable-result rows column-count time-columns)))
            (multiple-value-bind (body-characters body-octets)
                (edit-clocktable-body-metrics
                 clocktable-result rows widths time-columns newline
                 (or (document-encoding document) :utf-8))
              (edit-clocktable-check-output-limits
               document start end body-characters body-octets)
              (let* ((raw-body (edit-clocktable-body-text
                                clocktable-result rows time-columns
                                column-count newline))
                     (temporary (parse-org raw-body
                                           :limits (document-limits document)
                                           :settings (document-settings document)))
                     (table (edit-clocktable-find-table temporary))
                     (aligned-result (table-align temporary table))
                     (aligned-document
                       (edit-result-updated-document aligned-result))
                     (aligned-table
                       (edit-clocktable-find-table aligned-document))
                     (replacement (document-text aligned-document))
                     (expected (edit-canonical-copy
                                (edit-canonical-document document))))
                (edit-canonical-set-children
                 expected (edit-node-path document block dynamic-block)
                 (list (edit-canonical-node aligned-table)))
                (edit-apply-patches-result
                 document
                 (list (edit-make-patch start end replacement dynamic-block))
                 :target block :expected expected)))))))))

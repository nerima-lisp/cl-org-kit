;;;; src/edit-clocktable.lisp
(in-package #:org-kit)

(defun edit-clocktable-body-span (document block target)
  (let* ((source (document-text document))
         (start (node-begin block))
         (limit (node-end block))
         (context (%make-reader-context :text source))
         (opening-newline (position #\Newline source :start start :end limit)))
    (unless opening-newline
      (edit-signal-error target :uneditable-structure
                         "Dynamic block opener has no body boundary"))
    (let ((body-start (1+ opening-newline)))
      (multiple-value-bind (end after-line)
          (reader-blocks-find-close context body-start limit "#+END:")
        (declare (ignore after-line))
        (when (= end limit)
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

(defun edit-clocktable-check-character-lower-bound (document characters)
  (let ((limits (document-limits document)))
    (check-limit limits :max-output-length characters)
    (dolist (limit-name
              (if (document-encoding document)
                  '(:max-output-octets :max-input-length)
                  '(:max-input-length :max-output-octets)))
      (check-limit limits limit-name characters))))

(defun edit-clocktable-check-indentation-size (document row)
  (let ((level (clocktable-row-level row)))
    (when (> level 1)
      (edit-clocktable-check-character-lower-bound
       document (+ 2 (* 2 (1- level)))))))

(defun edit-clocktable-time-text (minutes)
  (multiple-value-bind (days remainder) (floor minutes 1440)
    (if (plusp days)
        (format nil "~Dd ~D:~2,'0D"
                days (floor remainder 60) (mod remainder 60))
        (format nil "~D:~2,'0D" (floor minutes 60) (mod minutes 60)))))

(defun edit-clocktable-time-cell-length-lower-bound (minutes total-p)
  (let ((days (floor minutes 1440))
        (wrapping-length (if total-p 2 0)))
    (+ wrapping-length
       (if (plusp days)
           ;; 2^(4n) >= 10^n, so bound digits without formatting DAYS.
           (+ 6 (1+ (floor (1- (integer-length days)) 4)))
           4))))

(defun edit-clocktable-check-time-size (document minutes total-p)
  (let* ((lower-bound
           (edit-clocktable-time-cell-length-lower-bound minutes total-p)))
    (edit-clocktable-check-character-lower-bound document lower-bound)
    (let* ((time (edit-clocktable-time-text minutes))
           (cell (if total-p (concatenate 'string "*" time "*") time))
           (characters (length cell)))
      (edit-clocktable-check-character-lower-bound document characters))))

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

(defun edit-clocktable-fixed-cells (column-count first second)
  (let ((cells (make-array column-count :initial-element "")))
    (setf (aref cells 0) first
          (aref cells 1) second)
    cells))

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
             :target block :expected expected)))))))

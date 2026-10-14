;;;; src/clocktable.lisp
(in-package #:org-kit)

(defstruct (clocktable-row (:constructor %make-clocktable-row)
                           (:conc-name %clocktable-row-)
                           (:copier nil)
                           (:predicate %clocktable-row-p))
  "One normalized heading row in a clocktable result."
  (level nil :read-only t)
  (title nil :read-only t)
  (tags nil :read-only t)
  (range nil :read-only t)
  (minutes 0 :read-only t))

(defun make-clocktable-row (&key level title tags range (minutes 0))
  "Return a clocktable row with a positive level and non-negative minutes."
  (check-type level (integer 1 *))
  (check-type title string)
  (unless (and (%query-proper-list-p tags) (every #'stringp tags))
    (error 'type-error :datum tags :expected-type 'list))
  (check-type range org-time-range)
  (check-type minutes (integer 0 *))
  (%make-clocktable-row :level level :title title :tags (copy-list tags)
                        :range range :minutes minutes))

(defun clocktable-row-p (object)
  "Return true when OBJECT is a normalized clocktable row."
  (%clocktable-row-p object))

(defun clocktable-row-level (row)
  "Return ROW's absolute logical heading level."
  (%clocktable-row-level row))

(defun clocktable-row-title (row)
  "Return ROW's unformatted heading title."
  (%clocktable-row-title row))

(defun clocktable-row-tags (row)
  "Return a fresh list of ROW's effective tags."
  (copy-list (%clocktable-row-tags row)))

(defun clocktable-row-range (row)
  "Return ROW's explicit time range."
  (%clocktable-row-range row))

(defun clocktable-row-minutes (row)
  "Return ROW's clipped CLOCK minutes."
  (%clocktable-row-minutes row))

(defstruct (clocktable-result (:constructor %make-clocktable-result)
                              (:conc-name %clocktable-result-)
                              (:copier nil)
                              (:predicate %clocktable-result-p))
  "The total and normalized rows returned by CLOCKTABLE-DATA."
  (total-minutes 0 :read-only t)
  (rows nil :read-only t)
  (range nil :read-only t))

(defun make-clocktable-result (&key (total-minutes 0) rows range)
  "Return a clocktable result with non-negative minutes and clocktable rows."
  (check-type total-minutes (integer 0 *))
  (unless (and (%query-proper-list-p rows) (every #'clocktable-row-p rows))
    (error 'type-error :datum rows :expected-type 'list))
  (check-type range org-time-range)
  (%make-clocktable-result :total-minutes total-minutes
                           :rows (copy-list rows) :range range))

(defun clocktable-result-p (object)
  "Return true when OBJECT is a normalized clocktable result."
  (%clocktable-result-p object))

(defun clocktable-result-total-minutes (result)
  "Return RESULT's total minutes for the requested scope and match."
  (%clocktable-result-total-minutes result))

(defun clocktable-result-rows (result)
  "Return a fresh list of RESULT's normalized rows in document order."
  (copy-list (%clocktable-result-rows result)))

(defun clocktable-result-range (result)
  "Return RESULT's requested time range."
  (%clocktable-result-range result))

(defun clocktable-validate-options (options)
  (unless (evenp (length options))
    (%org-clock-error :unsupported-option "missing option value"))
  (let ((seen (make-hash-table :test #'eq)))
    (loop for tail on options by #'cddr
          for key = (first tail)
          do (unless (member key '(:range :match :max-depth :time-context)
                             :test #'eq)
               (%org-clock-error :unsupported-option (clock-option-context key)))
             (when (gethash key seen)
               (%org-clock-error :unsupported-option (clock-option-context key)))
             (setf (gethash key seen) t)))
  options)

(defun clocktable-subtree-headings (heading)
  (let ((headings nil))
    (map-nodes (lambda (node)
                 (when (eq (node-type node) :headline)
                   (push node headings)))
               heading)
    (nreverse headings)))

(defun clocktable-matching-headings (document headings match time-context)
  (if match
      (let ((subtree (make-hash-table :test #'eq))
            (reference-time
              (%time-value-local-date-time
               (org-time-context-reference-time time-context))))
        (dolist (heading headings)
          (setf (gethash heading subtree) t))
        (remove-if-not
         (lambda (candidate) (gethash candidate subtree))
         (query-match-at document match reference-time)))
      headings))

(defun clocktable-heading-relations (headings matching-headings)
  (let ((children (make-hash-table :test #'eq))
        (matching-p (make-hash-table :test #'eq)))
    (dolist (heading matching-headings)
      (setf (gethash heading matching-p) t))
    (dolist (heading headings)
      (let ((parent (query-nearest-parent-headline heading)))
        (push heading (gethash parent children))))
    (values children matching-p)))

(defun clocktable-matching-heading-minutes (headings children matching-p
                                            direct-minutes)
  (let ((matched-minutes (make-hash-table :test #'eq)))
    (dolist (heading (reverse headings))
      (let ((minutes (if (gethash heading matching-p)
                         (gethash heading direct-minutes 0)
                         0)))
        (dolist (child (gethash heading children))
          (incf minutes (gethash child matched-minutes 0)))
        (setf (gethash heading matched-minutes) minutes)))
    matched-minutes))

(defun clocktable-row-level-for-heading (document heading)
  (query-logical-heading-level document heading))

(defun clocktable-data (document heading &rest options)
  "Return normalized clocktable rows for HEADING's subtree.

RANGE is clipped as the half-open interval from its start to its end. MATCH is
an Org match string or NIL. MAX-DEPTH limits emitted absolute reduced outline
levels; TIME-CONTEXT supplies the explicit time zone and relative-match
reference. Running CLOCK rows are excluded."
  (clock-check-heading document heading)
  (clocktable-validate-options options)
  (clock-check-document-node-count document)
  (let* ((range (getf options :range))
         (match (getf options :match))
         (max-depth (if (member :max-depth options)
                        (getf options :max-depth)
                        30))
         (time-context (getf options :time-context)))
    (check-type range org-time-range)
    (check-type match (or null string))
    (check-type max-depth (integer 0 *))
    (unless (org-time-context-p time-context)
      (error 'type-error :datum time-context :expected-type 'org-time-context))
    (let* ((headings (clocktable-subtree-headings heading))
           (matching-headings
             (clocktable-matching-headings document headings match time-context)))
      (multiple-value-bind (children matching-p)
          (clocktable-heading-relations headings matching-headings)
        (multiple-value-bind (summary direct-minutes)
            (clock-collect-range heading
                                 (org-time-range-start range)
                                 (org-time-range-end range)
                                 time-context nil t matching-p)
          (declare (ignore summary))
          (let ((matched-minutes
                  (clocktable-matching-heading-minutes
                   headings children matching-p direct-minutes))
                (rows nil))
            (dolist (candidate headings)
              (let* ((level (clocktable-row-level-for-heading
                             document candidate))
                     (minutes (gethash candidate matched-minutes 0)))
                (when (and (<= level max-depth) (plusp minutes))
                  (push (make-clocktable-row
                         :level level
                         :title (node-property candidate :raw-value "")
                         :tags (effective-heading-tags document candidate)
                         :range range
                         :minutes minutes)
                        rows))))
            (make-clocktable-result
             :total-minutes (gethash heading matched-minutes 0)
             :rows (nreverse rows)
             :range range)))))))

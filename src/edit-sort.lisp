;;;; src/edit-sort.lisp
;;;; Stable, source-preserving sorting of direct headline children.
(in-package #:org-kit)

(defstruct (edit-sort-entry (:constructor %make-edit-sort-entry
                              (node canonical value missing-p)))
  node
  canonical
  value
  missing-p
  source-start
  source-end
  source-text)

(defun edit-sort-headline-p (node)
  (eq (node-type node) :headline))

(defun edit-sort-string-compare (left right)
  (loop for index below (min (length left) (length right))
        for left-code = (char-code (char left index))
        for right-code = (char-code (char right index))
        when (< left-code right-code) do (return -1)
        when (> left-code right-code) do (return 1)
        finally (return (cond ((< (length left) (length right)) -1)
                              ((> (length left) (length right)) 1)
                              (t 0)))))

(defun edit-sort-priority-token-value (token)
  (cond
    ((and (= (length token) 1)
          (<= (char-code #\A) (char-code (char token 0))
              (char-code #\Z)))
     (char-code (char token 0)))
    ((and (plusp (length token))
          (every #'table-calc-digit-p token))
     (parse-integer token))))

(defun edit-sort-priority-directive-value (document)
  (let* ((section (find :section (node-children (document-root document))
                        :key #'node-type))
         (keyword (and section
                       (find-if
                        (lambda (node)
                          (and (eq (node-type node) :keyword)
                               (string-equal (node-property node :key)
                                             "PRIORITIES")))
                        (node-children section))))
         (value (and keyword (node-property keyword :value))))
    (when (stringp value)
      (loop with length = (length value)
            with position = 0
            for token-index from 0
            do (loop while (and (< position length)
                                (find (char value position) '(#\Space #\Tab)))
                     do (incf position))
               (when (= position length)
                 (return nil))
               (let ((start position))
                 (loop while (and (< position length)
                                  (not (find (char value position)
                                             '(#\Space #\Tab))))
                       do (incf position))
                 (when (= token-index 2)
                   (return (edit-sort-priority-token-value
                            (subseq value start position)))))))))

(defun edit-sort-priority-default (document)
  (let ((settings (document-settings document)))
    (if (member :priority-default settings :test #'eq)
        (let ((value (getf settings :priority-default)))
          (typecase value
            (null nil)
            (integer value)
            (character (char-code value))
            (string (edit-sort-priority-token-value value))
            (t nil)))
        (or (edit-sort-priority-directive-value document)
            (char-code #\B)))))

(defun edit-sort-time-key (timestamp time-context)
  (when timestamp
    (let* ((view (timestamp-view timestamp))
           (start (org-timestamp-view-start view)))
      (when start
        (cl-date-kit:instant-epoch-second
         (%time-value-to-instant
          start (org-time-context-zone time-context)
          (org-time-context-dst-policy time-context)))))))

(defun edit-sort-created-key (document heading)
  (let ((entries (effective-heading-property-entries heading "CREATED")))
    (when (> (length entries) 1)
      (edit-signal-error heading :ambiguous-target
                         "CREATED property is ambiguous"))
    (when entries
      (let* ((entry (first entries))
             (value (node-property entry :value))
             (source (document-text document))
             (start (and (stringp value)
                         (search value source
                                 :start2 (node-begin entry)
                                 :end2 (node-end entry))))
             (timestamp
               (and start
                    (find-if (lambda (node) (eq (node-type node) :timestamp))
                             (parse-objects document start
                                            (+ start (length value))
                                            :paragraph))))
             (time-value
               (when timestamp
                 (handler-case
                     (org-timestamp-view-start (timestamp-view timestamp))
                   (org-time-error ()
                     (edit-signal-error heading :invalid-value
                                        "CREATED property is not a valid timestamp"))))))
        (unless time-value
          (edit-signal-error heading :invalid-value
                             "CREATED property is not a valid timestamp"))
        (%time-value-order-key time-value)))))

(defun edit-sort-local-property-value (heading property-name)
  (let ((entries (effective-heading-property-entries heading property-name)))
    (when (> (length entries) 1)
      (edit-signal-error heading :ambiguous-target
                         "Requested property is ambiguous"))
    (if entries
        (let ((value (node-property (first entries) :value)))
          (unless (stringp value)
            (edit-signal-error heading :invalid-value
                               "Requested property value is not a string"))
          (values value t))
        (values nil nil))))

(defun edit-sort-entry-key (document heading key property-name priority-default
                            time-context)
  (case key
    (:alpha
     (values (node-property heading :raw-value) nil))
    (:numeric
     (let ((value (parse-safe-decimal
                   (node-property heading :raw-value)
                   :limits (document-limits document))))
       (unless value
         (edit-signal-error heading :invalid-value
                            "Headline value is not a decimal number"))
       (values value nil)))
    (:time
     (let* ((timestamp
              (find-if
               (lambda (node)
                 (and (node-p node)
                      (eq (node-type node) :timestamp)
                      (member (node-property node :type)
                              '(:active :active-range))))
               (node-property heading :title)))
         (value (edit-sort-time-key timestamp time-context)))
       (values value (null value))))
    (:created
     (let ((value (edit-sort-created-key document heading)))
       (values value (null value))))
    ((:scheduled :deadline)
     (let* ((section (find :section (node-children heading) :key #'node-type))
            (planning (and section
                           (find :planning (node-children section)
                                 :key #'node-type)))
            (timestamp (and planning (node-property planning key)))
            (value (edit-sort-time-key timestamp time-context)))
       (values value (null value))))
    (:priority
     (let ((value (or (node-property heading :priority) priority-default)))
       (values value (null value))))
    (:todo
     (let* ((keyword (node-property heading :todo-keyword))
            (rank (and keyword
                       (position keyword (getf (document-settings document)
                                               :todo-keywords)
                                 :key #'car :test #'string=))))
       (values rank (null rank))))
    (:property
     (multiple-value-bind (value present-p)
         (edit-sort-local-property-value heading property-name)
       (values value (not present-p))))))

(defun edit-sort-entry-compare (left right key direction)
  (let ((left-missing (edit-sort-entry-missing-p left))
        (right-missing (edit-sort-entry-missing-p right)))
    (cond
      ((and left-missing right-missing) nil)
      (left-missing nil)
      (right-missing t)
      (t
       (let ((comparison
               (case key
                 ((:alpha :property)
                  (edit-sort-string-compare
                   (edit-sort-entry-value left)
                   (edit-sort-entry-value right)))
                 (:numeric
                  (safe-decimal-compare (edit-sort-entry-value left)
                                        (edit-sort-entry-value right)))
                 ((:time :created :scheduled :deadline :priority :todo)
                  (let ((left-value (edit-sort-entry-value left))
                        (right-value (edit-sort-entry-value right)))
                    (cond ((< left-value right-value) -1)
                          ((> left-value right-value) 1)
                          (t 0)))))))
         (if (eq direction :ascending)
             (minusp comparison)
             (plusp comparison)))))))

(defun edit-sort-entry-source-texts (document heading entries)
  (let ((source (document-text document)))
    (loop for remaining on entries
          for entry = (first remaining)
          for next = (second remaining)
          for child = (edit-sort-entry-node entry)
          for start = (node-begin child)
          do (multiple-value-bind (boundary-next end)
                 (if next
                     (values (edit-sort-entry-node next)
                             (node-begin (edit-sort-entry-node next)))
                     (edit-subtree-next-boundary document child))
               (edit-subtree-check-boundary document start end boundary-next
                                            heading)
               (setf (edit-sort-entry-source-text entry)
                     (subseq source start end)
                     (edit-sort-entry-source-start entry) start
                     (edit-sort-entry-source-end entry) end))
          finally (return entries))))

(defun edit-sort-reordered-canonical-children (children canonical-children entries)
  (loop with sorted = entries
        for child in children
        for canonical in canonical-children
        collect (if (edit-sort-headline-p child)
                    (edit-sort-entry-canonical (pop sorted))
                    canonical)))

(defun edit-sort-source-replacement (document entries final-span-p)
  (let* ((line-ending (edit-subtree-line-ending document 0))
         (replacement
           (with-output-to-string (stream)
             (loop for remaining on entries
                   for entry = (first remaining)
                   for next = (second remaining)
                   for source-text = (edit-sort-entry-source-text entry)
                   do (write-string source-text stream)
                      (when (and next
                                 (not (char= (char source-text
                                                   (1- (length source-text)))
                                             #\Newline)))
                        (write-string line-ending stream))))))
    (if (and final-span-p (not (document-final-newline-p document)))
        (string-right-trim '(#\Return #\Newline) replacement)
        replacement)))

(defun sort-subtree-children (document heading key
                              &key (property-name nil property-name-p)
                                (direction :ascending) time-context)
  "Sort HEADING's direct headline children by a built-in Org key.
Missing values stay last in both directions, and ties retain source order."
  (check-type document document)
  (unless (member key '(:alpha :numeric :time :created :scheduled :deadline
                        :priority :todo :property)
                   :test #'eq)
    (edit-signal-error heading :invalid-value "Unsupported subtree sort key"))
  (unless (member direction '(:ascending :descending) :test #'eq)
    (edit-signal-error heading :invalid-value "Sort direction is invalid"))
  (when (if (eq key :property)
            (not property-name-p)
            property-name-p)
    (edit-signal-error heading :invalid-value
                       "Property name is required only for property sorting"))
  (let ((property-name
          (when property-name-p
            (typecase property-name
              (string (and (plusp (length property-name))
                           (string-upcase property-name)))
              (symbol (unless (null property-name)
                        (string-upcase (symbol-name property-name))))))))
    (when (and property-name-p (null property-name))
      (edit-signal-error heading :invalid-value "Property name is invalid"))
    (when (and (member key '(:time :scheduled :deadline) :test #'eq)
               (not (org-time-context-p time-context)))
      (%org-time-error :invalid-time "A time context is required"))
    (let* ((heading (edit-resolve-node document heading '(:headline :inlinetask)))
           (children (node-children heading))
           (canonical (edit-canonical-document document))
           (path (edit-node-path document heading))
           (canonical-children
             (getf (edit-canonical-node-at-path canonical path) :children))
           (priority-default (and (eq key :priority)
                                  (edit-sort-priority-default document)))
           (entries
             (loop for child in children
                   for child-canonical in canonical-children
                   when (edit-sort-headline-p child)
                     collect (multiple-value-bind (value missing-p)
                                 (edit-sort-entry-key document child key
                                                     property-name
                                                     priority-default time-context)
                               (%make-edit-sort-entry child child-canonical
                                                      value missing-p))))
           (sorted
             (stable-sort (copy-list entries)
                          (lambda (left right)
                            (edit-sort-entry-compare left right key direction)))))
      (if (loop for original in entries
                for result in sorted
                always (eq (edit-sort-entry-node original)
                           (edit-sort-entry-node result)))
          (edit-apply-patches-result document nil :target heading
                                     :expected canonical)
          (let* ((source-order
                   (edit-sort-entry-source-texts document heading entries))
                 (ordered sorted)
                 (first-entry (first source-order))
                 (last-entry (car (last source-order)))
                 (start (edit-sort-entry-source-start first-entry))
                 (end (edit-sort-entry-source-end last-entry))
                 (replacement
                   (edit-sort-source-replacement
                    document ordered (= end (length (document-text document)))))
                 (expected (edit-canonical-copy canonical))
                 (reordered-children
                   (edit-sort-reordered-canonical-children
                    children canonical-children ordered)))
            (edit-canonical-set-children expected path reordered-children)
            (edit-apply-patches-result
             document (list (edit-make-patch start end replacement heading))
             :target heading :expected expected))))))

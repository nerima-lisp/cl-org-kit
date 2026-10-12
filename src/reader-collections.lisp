;;;; src/reader-collections.lisp
(in-package #:org-kit)

(defun reader-horizontal-space-p (character)
  "Return true when CHARACTER is horizontal whitespace accepted in a list line."
  (find character '(#\Space #\Tab)))

(defun reader-leading-space-count (line)
  "Return the number of leading spaces and tabs in LINE."
  (loop for index from 0 below (length line)
        while (reader-horizontal-space-p (char line index))
        finally (return index)))

(defun reader-line-indentation (context offset limit)
  "Return the leading horizontal whitespace count at OFFSET, or zero at LIMIT."
  (if (>= offset limit)
      0
      (multiple-value-bind (content-end after-line)
          (reader-line-bounds context offset)
        (declare (ignore after-line))
        (reader-leading-space-count
         (reader-line-text context offset (min content-end limit))))))

(defun reader-list-marker-from-line (line source-start)
    (let* ((indent (reader-leading-space-count line))
           (line-length (length line))
           (index indent)
           (bullet-end nil)
           (ordered-p nil))
      ;; Parser callers skip blank lines before checking for a list marker.
      (cond
        ((and (find (char line index) '(#\- #\+ #\*))
              (< (1+ index) line-length)
              (reader-horizontal-space-p (char line (1+ index))))
         (setf bullet-end (+ index 2)))
        ((digit-char-p (char line index))
         (let ((digits-end index))
           (loop while (and (< digits-end line-length)
                            (digit-char-p (char line digits-end)))
                 do (incf digits-end))
           (when (and (< (1+ digits-end) line-length)
                      (or (char= (char line digits-end) #\.)
                          (char= (char line digits-end) (char ")" 0)))
                      (reader-horizontal-space-p (char line (1+ digits-end))))
             (setf bullet-end (+ digits-end 2)
                   ordered-p t)))))
      (when bullet-end
        (let* ((bullet (subseq line index bullet-end))
               (content-index bullet-end)
               (checkbox nil)
               (counter nil)
               (tag-range nil))
          (when (and (<= (+ content-index 3) line-length)
                     (char= (char line content-index) #\[)
                     (char= (char line (+ content-index 2)) #\])
                     (find (char line (1+ content-index)) '(#\Space #\X #\x #\-))
                     (or (= (+ content-index 3) line-length)
                         (reader-horizontal-space-p (char line (+ content-index 3)))))
            (setf checkbox
                  (case (char line (1+ content-index))
                    (#\Space :off)
                    ((#\X #\x) :on)
                    (#\- :trans))
                  content-index (+ content-index 3))
            (when (and (< content-index line-length)
                       (reader-horizontal-space-p (char line content-index)))
              (incf content-index)))
          (when (and (< (+ content-index 3) line-length)
                     (char= (char line content-index) #\[)
                     (char= (char line (1+ content-index)) #\@))
            (let ((digits-end (+ content-index 2)))
              (loop while (and (< digits-end line-length)
                               (digit-char-p (char line digits-end)))
                    do (incf digits-end))
              (when (and (> digits-end (+ content-index 2))
                         (< digits-end line-length)
                         (char= (char line digits-end) #\]))
                (setf counter
                      (parse-integer line :start (+ content-index 2) :end digits-end)
                      content-index (1+ digits-end))
                (when (and (< content-index line-length)
                           (reader-horizontal-space-p (char line content-index)))
                  (incf content-index)))))
          (let ((separator (search "::" line :start2 content-index)))
            (when separator
              (let ((tag-start content-index)
                    (tag-end separator))
                (loop while (and (< tag-start tag-end)
                                 (reader-horizontal-space-p (char line tag-start)))
                      do (incf tag-start))
                (loop while (and (> tag-end tag-start)
                                 (reader-horizontal-space-p (char line (1- tag-end))))
                      do (decf tag-end))
                (when (< tag-start tag-end)
                  (setf tag-range (cons (+ source-start tag-start)
                                        (+ source-start tag-end))
                        content-index (+ separator 2))
                  (loop while (and (< content-index line-length)
                                   (reader-horizontal-space-p (char line content-index)))
                        do (incf content-index))))))
          (list :indent indent :bullet bullet :ordered-p ordered-p
                :checkbox checkbox :counter counter :tag-range tag-range
                :content-offset (+ source-start content-index))))))

  (defun reader-list-marker-at (context offset limit)
    (when (< offset limit)
      (multiple-value-bind (content-end after-line)
          (reader-line-bounds context offset)
        (let* ((line-end (min content-end limit))
               (line (reader-line-text context offset line-end))
               (marker (reader-list-marker-from-line line offset)))
          (when marker
            (setf (getf marker :line-end) line-end
                  (getf marker :after-line) (min after-line limit))
            marker)))))

  (defun reader-skip-blank-lines (context offset limit)
    (let ((position offset)
          (count 0))
      (loop while (and (< position limit)
                       (reader-blank-line-p context position))
            do (incf count)
               (setf position (min limit (reader-next-line context position))))
      (values position count)))

  (defun reader-nonblank-span-p (context start end)
    (let ((text (reader-context-text context)))
      (loop for index from start below end
            thereis (not (find (char text index)
                               '(#\Space #\Tab #\Return #\Newline))))))


  (defun reader-list-item-boundary (context limit base-indent marker)
  (let ((scan (getf marker :after-line)))
    (block boundary
      (loop while (< scan limit)
            do (cond
                 ((reader-blank-line-p context scan)
                  (multiple-value-bind (after count)
                      (reader-skip-blank-lines context scan limit)
                    (let ((next-marker (reader-list-marker-at context after limit))
                          (next-indent (reader-line-indentation context after limit)))
                      (cond
                        ((and next-marker (= next-indent base-indent))
                         (return-from boundary (values after scan count after t)))
                        ((and (< after limit) (> next-indent base-indent))
                         (setf scan after))
                        (t
                         (return-from boundary (values scan scan 0 scan nil)))))))
                 (t
                  (let ((next-marker (reader-list-marker-at context scan limit))
                        (indent (reader-line-indentation context scan limit)))
                    (cond
                      ((and next-marker (= indent base-indent))
                       (return-from boundary (values scan scan 0 scan t)))
                      ((<= indent base-indent)
                       (return-from boundary (values scan scan 0 scan nil)))
                      (t
                       (setf scan (min limit (reader-next-line context scan)))))))))
      (values limit limit 0 limit nil))))

  (defun reader-make-list-item (context start end content-end marker post-blank depth)
    (let* ((content-start (getf marker :content-offset))
           (has-contents (and (< content-start content-end)
                              (reader-nonblank-span-p context content-start content-end)))
           (contents-begin (and has-contents content-start))
           (contents-end (and has-contents content-end))
           (children (when has-contents
                       (reader-parse-elements context contents-begin contents-end
                                              (1+ depth))))
           (properties (list :bullet (getf marker :bullet)
                             :checkbox (getf marker :checkbox)
                             :counter (getf marker :counter)
                             :tag nil))
           (tag-range (getf marker :tag-range)))
      (when tag-range
        (setf (getf properties :reader-tag-range) tag-range))
      (make-reader-node context :item
                        :begin start :end end
                        :contents-begin contents-begin :contents-end contents-end
                        :post-affiliated start :post-blank post-blank
                        :properties properties :children children :depth depth)))

  (defun reader-list-type (marker)
    (cond ((getf marker :tag-range) :descriptive)
          ((getf marker :ordered-p) :ordered)
          (t :unordered)))

  (defun reader-parse-plain-list (context start limit depth first-marker)
    (let ((base-indent (getf first-marker :indent))
          (position start)
          (items '())
          (list-end start))
      (loop for marker = (reader-list-marker-at context position limit)
            do (multiple-value-bind (item-end content-end post-blank next-position sibling-p)
                   (reader-list-item-boundary context limit base-indent marker)
                 (push (reader-make-list-item context position item-end content-end marker
                                              post-blank (1+ depth))
                       items)
                 (setf list-end item-end)
                 (if sibling-p
                     (setf position next-position)
                     (return))))
      (when items
        (values (make-reader-node context :plain-list
                                  :begin start :end list-end
                                  :contents-begin start :contents-end list-end
                                  :post-affiliated start
                                  :properties (list :type (reader-list-type first-marker))
                                  :children (nreverse items) :depth depth)
                list-end))))


  (defun reader-table-row-start (context offset limit)
    (multiple-value-bind (content-end after-line)
        (reader-line-bounds context offset)
      (declare (ignore after-line))
      (let* ((line-end (min content-end limit))
             (line (reader-line-text context offset line-end))
             (indent (reader-leading-space-count line)))
        (when (and (< indent (length line))
                   (char= (char line indent) #\|))
          (+ offset indent)))))

  (defun reader-table-el-border-line-p (context offset limit)
    (multiple-value-bind (content-end after-line)
        (reader-line-bounds context offset)
      (declare (ignore after-line))
      (let* ((line-end (min content-end limit))
             (line (reader-line-text context offset line-end))
             (indent (reader-leading-space-count line))
             (body (subseq line indent)))
        (and (>= (length body) 3)
             (char= (char body 0) #\+)
             (some (lambda (character) (char= character #\-)) body)
             (every (lambda (character)
                      (find character '(#\+ #\- #\: #\Space #\Tab)))
                    body)))))

  (defun reader-table-formula-line-p (context offset)
    (multiple-value-bind (key value) (reader-keyword-fields context offset)
      (declare (ignore value))
      (and key (string= key "TBLFM"))))

  (defun reader-table-el-line-p (context offset limit)
    (or (reader-table-el-border-line-p context offset limit)
        (reader-table-row-start context offset limit)
        (reader-table-formula-line-p context offset)))

  (defun reader-parse-table-el (context start limit depth)
    (let ((position start)
          (table-end nil))
      (loop while (and (< position limit)
                       (reader-table-el-line-p context position limit))
            do (setf table-end (min limit (reader-next-line context position))
                     position table-end))
      (when table-end
        (values (make-reader-node context :table
                                  :begin start :end table-end
                                  :contents-begin nil :contents-end nil
                                  :post-affiliated start
                                  :properties '(:type :table.el :tblfm nil)
                                  :depth depth)
                table-end))))

  (defun reader-table-rule-row-p (context bar-position content-end)
    (let* ((text (reader-context-text context))
           (row-text (subseq text (1+ bar-position) content-end)))
      (and (some (lambda (character) (char= character #\-)) row-text)
           (every (lambda (character)
                    (find character '(#\- #\+ #\: #\| #\Space #\Tab)))
                  row-text))))

  (defun reader-next-table-delimiter (text start end)
    (loop for index from start below end
          when (and (char= (char text index) #\|)
                    (let ((slashes 0)
                          (prior (1- index)))
                      (loop while (and (>= prior start)
                                       (char= (char text prior) #\\))
                            do (incf slashes)
                               (decf prior))
                      (evenp slashes)))
            return index))


  (defun reader-table-cell-node (context start end content-start content-end depth)
    (make-reader-node context :table-cell
                      :begin start :end end
                      :contents-begin content-start :contents-end content-end
                      :post-affiliated start :properties '(:value nil) :depth depth))

  (defun reader-table-cell-bounds (text start end)
    (let ((content-start start)
          (content-end end))
      (loop while (and (< content-start content-end)
                       (reader-horizontal-space-p (char text content-start)))
            do (incf content-start))
      (loop while (and (> content-end content-start)
                       (reader-horizontal-space-p (char text (1- content-end))))
            do (decf content-end))
      (values content-start content-end)))

  (defun reader-parse-table-cells (context first-bar content-end depth)
    (let* ((text (reader-context-text context))
           (position (1+ first-bar))
           (cells '()))
      (loop
        for delimiter = (reader-next-table-delimiter text position content-end)
        do (if delimiter
               (if (every #'reader-horizontal-space-p
                          (subseq text (1+ delimiter) content-end))
                   (multiple-value-bind (content-start cell-content-end)
                       (reader-table-cell-bounds text position delimiter)
                     (push (reader-table-cell-node context position content-end
                                                   content-start cell-content-end depth)
                           cells)
                     (return))
                   (multiple-value-bind (content-start cell-content-end)
                       (reader-table-cell-bounds text position delimiter)
                     (push (reader-table-cell-node context position (1+ delimiter)
                                                   content-start cell-content-end depth)
                           cells)
                     (setf position (1+ delimiter))))
               (multiple-value-bind (content-start cell-content-end)
                   (reader-table-cell-bounds text position content-end)
                 (push (reader-table-cell-node context position content-end
                                               content-start cell-content-end depth)
                       cells)
                 (return))))
      (nreverse cells)))

  (defun reader-parse-table-row (context start limit depth)
    (multiple-value-bind (line-end after-line) (reader-line-bounds context start)
      (let* ((content-end (min line-end limit))
             (row-end (min after-line limit))
             (bar (reader-table-row-start context start limit)))
        (when bar
          (if (reader-table-rule-row-p context bar content-end)
              (make-reader-node context :table-row
                                :begin start :end row-end
                                :contents-begin nil :contents-end nil
                                :post-affiliated start :properties '(:type :rule)
                                :depth depth)
              (let* ((contents-begin (1+ bar))
                     (cells (reader-parse-table-cells context bar content-end
                                                      (1+ depth))))
                (make-reader-node context :table-row
                                  :begin start :end row-end
                                  :contents-begin contents-begin :contents-end content-end
                                  :post-affiliated start :properties '(:type :standard)
                                  :children cells :depth depth)))))))

  (defun reader-parse-org-table (context start limit depth)
  (let ((position start)
        (rows nil)
        (table-end nil)
        (table-contents-end nil))
    (loop while (and (< position limit)
                     (reader-table-row-start context position limit))
          do (let ((row (reader-parse-table-row context position limit (1+ depth))))
               (push row rows)
               (setf table-end (node-end row)
                     table-contents-end (node-end row)
                     position table-end)))
    (when rows
      (values (make-reader-node context :table
                                :begin start :end table-end
                                :contents-begin start :contents-end table-contents-end
                                :post-affiliated start
                                :properties (list :type :org :tblfm nil)
                                :children (nreverse rows) :depth depth)
              table-end))))


  (defmethod parse-collection-at ((context reader-context) start limit depth)
    (let ((table-row (reader-table-row-start context start limit))
          (table-el (or (reader-table-el-border-line-p context start limit)
                        (reader-table-formula-line-p context start)))
          (marker (reader-list-marker-at context start limit)))
      (cond
        (table-el (reader-parse-table-el context start limit depth))
        (table-row (reader-parse-org-table context start limit depth))
        (marker (reader-parse-plain-list context start limit depth marker))
        (t (values nil nil)))))

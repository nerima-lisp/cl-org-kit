;;;; src/reader-headings.lisp
(in-package #:org-kit)

(defun reader-heading-horizontal-space-p (character)
  (find character '(#\Space #\Tab)))

(defun reader-heading-skip-horizontal-space (text position limit)
  (loop while (and (< position limit)
                   (reader-heading-horizontal-space-p (char text position)))
        do (incf position)
        finally (return position)))

(defun reader-heading-trim-right (text start end)
  (loop while (and (> end start)
                   (reader-heading-horizontal-space-p (char text (1- end))))
        do (decf end)
        finally (return end)))

(defun reader-heading-words (text)
  (let ((words '())
        (word-start nil)
        (length (length text)))
    (loop for position from 0 to length
          do (if (or (= position length)
                     (reader-heading-horizontal-space-p (char text position)))
                 (when word-start
                   (push (subseq text word-start position) words)
                   (setf word-start nil))
                 (unless word-start
                   (setf word-start position))))
    (nreverse words)))

(defun reader-heading-todo-name (token)
  (let ((marker (position #\( token)))
    (if marker
        (and (> marker 0) (subseq token 0 marker))
        token)))

(defun reader-heading-todo-sequence (value)
  (let* ((tokens (reader-heading-words value))
         (separator (position "|" tokens :test #'string=))
         (todo-tokens
          (if separator
              (subseq tokens 0 separator)
              tokens))
         (done-tokens (and separator (nthcdr (1+ separator) tokens)))
         (todo-names
          (remove nil (mapcar #'reader-heading-todo-name todo-tokens)))
         (done-names
          (and separator
               (remove nil (mapcar #'reader-heading-todo-name done-tokens)))))
    (unless separator
      (setf done-names (last todo-names)
            todo-names (butlast todo-names)))
    (append
     (mapcar
      (lambda (name)
        (cons name :todo))
      todo-names)
     (mapcar
      (lambda (name)
        (cons name :done))
      done-names))))

(defun reader-heading-setting-kind (key)
  (when (stringp key)
    (cond
      ((member key (list "TODO" "SEQ_TODO" "TYP_TODO") :test (function string=))
       :todo)
      ((or (and (>= (length key) 5) (string= key "TODO[" :end1 5 :end2 5))
           (and (>= (length key) 9) (string= key "SEQ_TODO[" :end1 9 :end2 9))
           (and (>= (length key) 9) (string= key "TYP_TODO[" :end1 9 :end2 9)))
       (let ((open (position #\[ key)))
         (when (and open (char= (char key (1- (length key))) #\]))
           :todo)))
      ((string= key "STARTUP") :startup)
      ((string= key "LINK") :link))))

(defun reader-heading-block-end-key (context offset)
  (let* ((line (reader-line-text context offset))
         (prefix "#+BEGIN_")
         (prefix-length (length prefix)))
    (if (and (>= (length line) prefix-length)
             (string-equal prefix line :end1 prefix-length :end2 prefix-length))
        (let* ((name-start
                (reader-heading-skip-horizontal-space line
                                                      prefix-length
                                                      (length line)))
               (name-end
                (or (position-if #'reader-heading-horizontal-space-p
                                 line
                                 :start
                                 name-start)
                    (length line))))
          (when (< name-start name-end)
            (concatenate 'string
                         "END_"
                         (string-upcase (subseq line name-start name-end)))))
        (multiple-value-bind (key value)
            (reader-keyword-fields context offset)
          (declare (ignore value))
          (when (and key (string= key "BEGIN"))
            :dynamic)))))

(defun reader-heading-block-close-p (context offset end-key)
  (if (eq end-key :dynamic)
      (multiple-value-bind (key value)
          (reader-keyword-fields context offset)
        (declare (ignore value))
        (and key (string= key "END")))
      (let* ((line (reader-line-text context offset))
             (prefix "#+END_")
             (prefix-length (length prefix)))
        (and (>= (length line) prefix-length)
             (string-equal prefix line :end1 prefix-length :end2 prefix-length)
             (let* ((name-start
                     (reader-heading-skip-horizontal-space line
                                                           prefix-length
                                                           (length line)))
                    (name-end
                     (or (position-if #'reader-heading-horizontal-space-p
                                      line
                                      :start
                                      name-start)
                         (length line))))
               (and (< name-start name-end)
                    (string-equal
                     end-key
                     (concatenate 'string
                                  "END_"
                                  (string-upcase
                                   (subseq line name-start name-end))))
                    (every #'reader-heading-horizontal-space-p
                           (subseq line name-end))))))))

(defun reader-heading-link-abbreviation (value)
  (let* ((start (reader-heading-skip-horizontal-space value 0 (length value)))
         (end
          (position-if #'reader-heading-horizontal-space-p value :start start)))
    (when end
      (let* ((name (subseq value start end))
             (replacement-start
              (reader-heading-skip-horizontal-space value end (length value)))
             (replacement-end
              (reader-heading-trim-right value replacement-start (length value))))
        (cons name (subseq value replacement-start replacement-end))))))

(defmethod reader-buffer-settings ((context reader-context) explicit-settings)
  (let ((settings (reader-default-settings))
        (todo-keywords nil)
        (todo-seen nil)
        (startup nil)
        (link-abbrevs nil)
        (position 0)
        (limit (length (reader-context-text context)))
        (block-end-key nil))
    (loop while (< position limit)
          do (let ((next (min limit (reader-next-line context position))))
               (if block-end-key
                   (progn
                     (when
                         (reader-heading-block-close-p context
                                                       position
                                                       block-end-key)
                       (setf block-end-key nil))
                     (setf position next))
                   (let ((open-key
                          (reader-heading-block-end-key context position)))
                     (if open-key
                         (setf block-end-key open-key
                               position next)
                         (multiple-value-bind (key value)
                             (reader-keyword-fields context position)
                           (case (reader-heading-setting-kind key)
                             (:todo
                              (setf todo-seen t
                                    todo-keywords (append todo-keywords
                                                          (reader-heading-todo-sequence
                                                           value))))
                             (:startup
                              (setf startup (append startup
                                                    (mapcar
                                                     (function string-downcase)
                                                     (reader-heading-words
                                                      value)))))
                             (:link
                              (let ((abbreviation
                                     (reader-heading-link-abbreviation value)))
                                (when abbreviation
                                  (setf link-abbrevs (append link-abbrevs
                                                             (list abbreviation))))))
                             (otherwise nil))
                           (setf position next)))))))
    (when todo-seen
      (setf (getf settings :todo-keywords) todo-keywords))
    (when startup
      (setf (getf settings :startup) startup))
    (when link-abbrevs
      (setf (getf settings :link-abbrevs) link-abbrevs))
    (loop for tail on explicit-settings by (function cddr)
          do (setf (getf settings (first tail)) (second tail)))
    settings))

(defun reader-heading-odd-levels-p (context)
  (member "odd"
          (getf (reader-context-settings context) :startup)
          :test
          #'string-equal))

(defun reader-heading-effective-level (star-count odd-levels)
  (if odd-levels
      (1+ (floor star-count 2))
      star-count))

(defun reader-heading-inline-minimum-stars (context)
  (let ((minimum (getf (reader-context-settings context) :inlinetask-min-level)))
    (when minimum
      (check-type minimum (integer 1))
      (if (reader-heading-odd-levels-p context)
          (1- (* 2 minimum))
          minimum))))

(defun reader-heading-valid-tag-name-p (name)
  (and (plusp (length name))
       (every
        (lambda (character)
          (or (alphanumericp character) (find character '(#\_ #\@ #\# #\%))))
        name)))

(defun reader-heading-split-tags (text start end)
  (let ((tags '())
        (tag-start start))
    (loop for position from start to
          end
          do (when (or (= position end) (char= (char text position) #\:))
               (let ((tag (subseq text tag-start position)))
                 (unless (reader-heading-valid-tag-name-p tag)
                   (return-from reader-heading-split-tags
                     nil))
                 (push tag tags)
                 (setf tag-start (1+ position)))))
    (nreverse tags)))

(defun reader-heading-tags-at-end (text title-start line-end)
  (let* ((end (reader-heading-trim-right text title-start line-end))
         (closing-colon (1- end)))
    (when (and (> end title-start) (char= (char text closing-colon) #\:))
      (loop for group-start from title-start below closing-colon
            when (and (char= (char text group-start) #\:)
                      (or (= group-start title-start)
                          (reader-heading-horizontal-space-p
                           (char text (1- group-start)))))
              do (let ((tags
                        (reader-heading-split-tags text
                                                   (1+ group-start)
                                                   closing-colon)))
                   (when tags
                     (return-from reader-heading-tags-at-end
                       (values tags group-start))))))))

(defun reader-heading-priority-at (text position end)
  (when
      (and (<= (+ position 4) end)
           (char= (char text position) #\[)
           (char= (char text (1+ position)) #\#))
    (let ((close (position #\] text :start (+ position 2) :end end)))
      (when close
        (let* ((value (subseq text (+ position 2) close))
               (priority
                (cond
                  ((and (= (length value) 1)
                        (<= (char-code #\A)
                            (char-code (char value 0))
                            (char-code #\Z))) (char-code (char value 0)))
                  ((and (<= 1 (length value) 2)
                        (every (function digit-char-p) value)
                        (<= (parse-integer value) 64)) (parse-integer value)))))
          (when priority
            (values priority (1+ close))))))))

(defun reader-heading-properties (context start star-count line-end)
  (labels ((token-at (position token)
             (let ((token-end (+ position (length token)))
                   (text (reader-context-text context)))
               (and (<= token-end line-end)
                    (string= token text :start2 position :end2 token-end)
                    (or (= token-end line-end)
                        (reader-heading-horizontal-space-p
                         (char text token-end)))
                    token-end))))
    (let* ((text (reader-context-text context))
           (raw-line-end
            (if (and (< line-end (length text))
                     (char= (char text line-end) #\Return))
                (1+ line-end)
                line-end))
           (position
            (reader-heading-skip-horizontal-space text
                                                  (+ start star-count)
                                                  line-end))
           (todo-keywords
            (getf (reader-context-settings context) :todo-keywords))
           (todo-keyword nil)
           (todo-type nil)
           (priority nil)
           (commentedp nil))
      (dolist (entry todo-keywords)
        (let ((token-end (token-at position (car entry))))
          (when token-end
            (setf todo-keyword (car entry)
                  todo-type (cdr entry)
                  position (reader-heading-skip-horizontal-space text
                                                                 token-end
                                                                 line-end))
            (return))))
      (multiple-value-bind (value next)
          (reader-heading-priority-at text position line-end)
        (when value
          (setf priority value
                position (reader-heading-skip-horizontal-space text
                                                               next
                                                               line-end))))
      (let ((comment-end (token-at position "COMMENT")))
        (when comment-end
          (setf commentedp t
                position (reader-heading-skip-horizontal-space text
                                                               comment-end
                                                               line-end))))
      (let* ((title-start position)
             (tags nil)
             (title-end raw-line-end))
        (multiple-value-bind (found-tags tag-start)
            (reader-heading-tags-at-end text title-start line-end)
          (when found-tags
            (setf tags found-tags
                  title-end tag-start)))
        (setf title-end (reader-heading-trim-right text title-start title-end))
        (let* ((object-start
                (reader-heading-skip-horizontal-space text
                                                      title-start
                                                      title-end))
               (object-end
                (reader-heading-trim-right text object-start title-end))
               (raw-value
                (string-trim (list #\Space #\Tab)
                             (subseq text title-start title-end))))
          (list :level
                (reader-heading-effective-level star-count
                                                (reader-heading-odd-levels-p
                                                 context))
                :todo-keyword
                todo-keyword
                :todo-type
                todo-type
                :priority
                priority
                :raw-value
                raw-value
                :title-range
                (cons object-start object-end)
                :tags
                tags
                :archivedp
                (if (member "ARCHIVE" tags :test (function string=))
                    t
                    nil)
                :commentedp
                (if commentedp
                    t
                    nil)
                :scheduled
                nil
                :deadline
                nil
                :closed
                nil))))))

(defun reader-heading-skip-blank-lines (context start limit)
  (let ((position start)
        (count 0))
    (loop while (and (< position limit) (reader-blank-line-p context position))
          do (incf count) (setf position (min limit
                                              (reader-next-line context
                                                                position))))
    (values position count)))

(defun reader-heading-end-marker-p (context start limit minimum-stars)
  (let ((star-count (reader-heading-level context start limit)))
    (when (and star-count (>= star-count minimum-stars))
      (let* ((line-end
              (min limit (nth-value 0 (reader-line-bounds context start))))
             (text (reader-context-text context))
             (title-start
              (reader-heading-skip-horizontal-space text
                                                    (+ start star-count)
                                                    line-end))
             (title-end (reader-heading-trim-right text title-start line-end)))
        (and (= (- title-end title-start) 3)
             (string-equal "END" text :start2 title-start :end2 title-end))))))

(defun reader-heading-inline-close (context start limit minimum-stars)
  (let ((position start)
        (block-end-key nil))
    (loop while (< position limit)
          do (let ((next (min limit (reader-next-line context position))))
               (if block-end-key
                   (progn
                     (when
                         (reader-heading-block-close-p context
                                                       position
                                                       block-end-key)
                       (setf block-end-key nil))
                     (setf position next))
                   (let ((open-key
                          (reader-heading-block-end-key context position))
                         (star-count
                          (reader-heading-level context position limit)))
                     (cond
                       (open-key
                        (setf block-end-key open-key
                              position next))
                       (star-count
                        (if (reader-heading-end-marker-p context
                                                         position
                                                         limit
                                                         minimum-stars)
                            (multiple-value-bind (after-close blank-count)
                                (reader-heading-skip-blank-lines context
                                                                 next
                                                                 limit)
                              (return-from reader-heading-inline-close
                                (values position after-close blank-count)))
                            (return-from reader-heading-inline-close
                              (values nil nil nil))))
                       (t
                        (setf position next)))))))
    (values nil nil nil)))

(defun reader-heading-subtree-end (context after-heading limit star-count)
  (let ((position after-heading)
        (block-end-key nil)
        (minimum-stars (reader-heading-inline-minimum-stars context)))
    (loop while (< position limit)
          do (let ((next (min limit (reader-next-line context position))))
               (if block-end-key
                   (progn
                     (when
                         (reader-heading-block-close-p context
                                                       position
                                                       block-end-key)
                       (setf block-end-key nil))
                     (setf position next))
                   (let* ((open-key
                           (reader-heading-block-end-key context position))
                          (candidate-level
                           (reader-heading-level context position limit)))
                     (cond
                       (open-key
                        (setf block-end-key open-key
                              position next))
                       ((and candidate-level (<= candidate-level star-count))
                        (return-from reader-heading-subtree-end
                          position))
                       ((and candidate-level
                             minimum-stars
                             (>= candidate-level minimum-stars))
                        (multiple-value-bind (close-start after-close)
                            (reader-heading-inline-close context
                                                         next
                                                         limit
                                                         minimum-stars)
                          (declare (ignore close-start))
                          (setf position (or after-close next))))
                       (t
                        (setf position next)))))))
    limit))

(defun reader-heading-first-child-start (context start end star-count)
  (let ((position start)
        (block-end-key nil)
        (minimum-stars (reader-heading-inline-minimum-stars context)))
    (loop while (< position end)
          do (let ((next (min end (reader-next-line context position))))
               (if block-end-key
                   (progn
                     (when
                         (reader-heading-block-close-p context
                                                       position
                                                       block-end-key)
                       (setf block-end-key nil))
                     (setf position next))
                   (let* ((open-key
                           (reader-heading-block-end-key context position))
                          (candidate-level
                           (reader-heading-level context position end)))
                     (cond
                       (open-key
                        (setf block-end-key open-key
                              position next))
                       ((and candidate-level (<= candidate-level star-count))
                        (return-from reader-heading-first-child-start
                          nil))
                       ((and candidate-level
                             minimum-stars
                             (>= candidate-level minimum-stars))
                        (multiple-value-bind (close-start after-close)
                            (reader-heading-inline-close context
                                                         next
                                                         end
                                                         minimum-stars)
                          (declare (ignore close-start))
                          (setf position (or after-close next))))
                       (candidate-level
                        (return-from reader-heading-first-child-start
                          position))
                       (t
                        (setf position next)))))))
    nil))

(defun reader-heading-section (context nodes depth)
  (when nodes
    (let* ((first (first nodes))
           (last (car (last nodes)))
           (begin (node-begin first))
           (end (node-end last)))
      (make-reader-node context
                        :section
                        :begin
                        begin
                        :end
                        end
                        :contents-begin
                        begin
                        :contents-end
                        end
                        :post-affiliated
                        begin
                        :children
                        nodes
                        :depth
                        depth))))

(defun reader-heading-children (context contents-begin
                                        contents-end
                                        star-count
                                        depth
                                        &optional
                                        planning-node
                                        headline-properties)
  (let ((text (reader-context-text context)))
    (labels ((planning-line-ranges (start line-end)
               (let ((position start)
                     (ranges nil))
                 (loop
                   (setf position
                         (reader-heading-skip-horizontal-space text
                                                               position
                                                               line-end))
                   (when (= position line-end)
                     (return (nreverse ranges)))
                   (let* ((entry
                           (find-if
                            (lambda (candidate)
                              (let* ((keyword (car candidate))
                                     (keyword-end (+ position
                                                    (length keyword))))
                                (and (<= keyword-end line-end)
                                     (string-equal keyword
                                                   text
                                                   :start2
                                                   position
                                                   :end2
                                                   keyword-end))))
                            '(("CLOSED:" . :closed)
                              ("DEADLINE:" . :deadline)
                              ("SCHEDULED:" . :scheduled))))
                          (keyword-end
                           (and entry (+ position (length (car entry)))))
                          (value-start
                           (and keyword-end
                                (reader-heading-skip-horizontal-space
                                 text keyword-end line-end))))
                     (unless (and entry
                                  (< keyword-end value-start)
                                  (< value-start line-end))
                       (return nil))
                     (multiple-value-bind (fields timestamp-end)
                         (%parse-single-timestamp text value-start line-end)
                       (unless fields
                         (return nil))
                       (when (assoc (cdr entry) ranges)
                         (return nil))
                       (push (cons (cdr entry)
                                   (cons value-start timestamp-end))
                             ranges)
                       (setf position timestamp-end))))))
             (planning-at (start end node-depth)
               (multiple-value-bind (raw-line-end after-line)
                   (reader-line-bounds context start)
                 (let* ((line-end (min end raw-line-end))
                        (ranges (planning-line-ranges start line-end)))
                   (when ranges
                     (let ((properties
                            (append
                             (list :closed nil :deadline nil :scheduled nil)
                             (when (assoc :closed ranges)
                               (list :reader-closed-range
                                     (cdr (assoc :closed ranges))))
                             (when (assoc :deadline ranges)
                               (list :reader-deadline-range
                                     (cdr (assoc :deadline ranges))))
                             (when (assoc :scheduled ranges)
                               (list :reader-scheduled-range
                                     (cdr (assoc :scheduled ranges)))))))
                       (multiple-value-bind (planning-end blank-count)
                           (reader-heading-skip-blank-lines context
                                                            (min end after-line)
                                                            end)
                         (declare (ignore blank-count))
                         (values
                          (make-reader-node context
                                            :planning
                                            :begin
                                            start
                                            :end
                                            planning-end
                                            :properties
                                            properties
                                            :depth
                                            node-depth)
                          planning-end))))))))
      (multiple-value-bind (found-planning)
          (if planning-node
              (values planning-node (node-end planning-node))
              (if (and contents-begin (< contents-begin contents-end))
                  (planning-at contents-begin contents-end (+ depth 2))
                  (values nil nil)))
        (let* ((planning-node (or planning-node found-planning))
               (content-start
                (if planning-node
                    (node-end planning-node)
                    contents-begin))
               (child-start
                (reader-heading-first-child-start context
                                                  content-start
                                                  contents-end
                                                  star-count))
               (section-nodes
                (if child-start
                    (reader-parse-elements context
                                           content-start
                                           child-start
                                           (+ depth 2))
                    (reader-parse-elements context
                                           content-start
                                           contents-end
                                           (+ depth 2))))
               (headline-nodes
                (when child-start
                  (reader-parse-elements context
                                         child-start
                                         contents-end
                                         (1+ depth)))))
          (when (and planning-node headline-properties)
            (dolist (key
                     (list :reader-closed-range
                           :reader-deadline-range
                           :reader-scheduled-range))
              (setf (getf headline-properties key)
                    (node-property planning-node key))))
          (when planning-node
            (push planning-node section-nodes))
          (let ((section
                 (reader-heading-section context section-nodes (1+ depth))))
            (append
             (when section
               (list section))
             headline-nodes)))))))

(defmethod parse-headline-at ((context reader-context) start limit depth)
  (let ((star-count (reader-heading-level context start limit)))
    (when star-count
      (let* ((odd-levels (reader-heading-odd-levels-p context))
             (level (reader-heading-effective-level star-count odd-levels))
             (minimum-stars (reader-heading-inline-minimum-stars context))
             (inlinetask-p (and minimum-stars (>= star-count minimum-stars))))
        (check-limit (reader-context-limits context) :max-heading-depth level)
        (multiple-value-bind (line-end after-heading)
            (reader-line-bounds context start)
          (let* ((line-end (min limit line-end))
                 (properties
                  (reader-heading-properties context start star-count line-end)))
            (if inlinetask-p
                (multiple-value-bind (close-start after-close blank-count)
                    (reader-heading-inline-close context
                                                 after-heading
                                                 limit
                                                 minimum-stars)
                  (if close-start
                      (let* ((contents-begin
                              (nth-value 0
                                         (reader-heading-skip-blank-lines
                                          context
                                          after-heading
                                          close-start)))
                             (has-contents (< contents-begin close-start))
                             (children
                              (and has-contents
                                   (reader-parse-elements context
                                                          contents-begin
                                                          close-start
                                                          (1+ depth))))
                             (properties
                              (append properties
                                      (list :reader-title-range
                                            (getf properties :title-range))))
                             (properties
                              (loop for (key value) on properties by (function
                                                                      cddr)
                                    unless (eq key :title-range)
                                      append (list key value)))
                             (node
                              (make-reader-node context
                                                :inlinetask
                                                :begin
                                                start
                                                :end
                                                after-close
                                                :contents-begin
                                                (and has-contents
                                                     contents-begin)
                                                :contents-end
                                                (and has-contents close-start)
                                                :post-affiliated
                                                start
                                                :post-blank
                                                blank-count
                                                :properties
                                                properties
                                                :children
                                                children
                                                :depth
                                                depth)))
                        (values node after-close))
                      (multiple-value-bind (next blank-count)
                          (reader-heading-skip-blank-lines context
                                                           after-heading
                                                           limit)
                        (let* ((properties
                                (append properties
                                        (list :reader-title-range
                                              (getf properties :title-range))))
                               (properties
                                (loop for (key value) on properties by (function
                                                                        cddr)
                                      unless (eq key :title-range)
                                        append (list key value)))
                               (node
                                (make-reader-node context
                                                  :inlinetask
                                                  :begin
                                                  start
                                                  :end
                                                  next
                                                  :post-affiliated
                                                  start
                                                  :post-blank
                                                  blank-count
                                                  :properties
                                                  properties
                                                  :depth
                                                  depth)))
                          (values node next)))))
                (let* ((subtree-end
                        (reader-heading-subtree-end context
                                                    after-heading
                                                    limit
                                                    star-count))
                       (contents-begin
                        (nth-value 0
                                   (reader-heading-skip-blank-lines context
                                                                    after-heading
                                                                    subtree-end)))
                       (has-contents (< contents-begin subtree-end))
                       (children
                        (and has-contents
                             (reader-heading-children context
                                                      contents-begin
                                                      subtree-end
                                                      star-count
                                                      depth
                                                      nil
                                                      properties)))
                       (properties
                        (let* ((section
                                (find :section
                                      children
                                      :key
                                      (function node-type)))
                               (planning
                                (and section
                                     (find :planning
                                           (node-children section)
                                           :key
                                           (function node-type))))
                               (result (copy-list properties)))
                          (when planning
                            (dolist
                                (key
                                 (list :reader-closed-range
                                       :reader-deadline-range
                                       :reader-scheduled-range))
                              (let ((range (node-property planning key)))
                                (when range
                                  (setf (getf result key) range)))))
                          (setf (getf result :reader-title-range) (getf result
                                                                        :title-range))
                          result))
                       (properties
                        (loop for (key value) on properties by (function cddr)
                              unless (eq key :title-range)
                                append (list key value)))
                       (post-blank
                        (if has-contents
                            0
                            (nth-value 1
                                       (reader-heading-skip-blank-lines context
                                                                        after-heading
                                                                        subtree-end))))
                       (node
                        (make-reader-node context
                                          :headline
                                          :begin
                                          start
                                          :end
                                          subtree-end
                                          :contents-begin
                                          (and has-contents contents-begin)
                                          :contents-end
                                          (and has-contents subtree-end)
                                          :post-affiliated
                                          start
                                          :post-blank
                                          post-blank
                                          :properties
                                          properties
                                          :children
                                          children
                                          :depth
                                          depth)))
                  (values node subtree-end)))))))))

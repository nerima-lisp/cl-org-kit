;;;; src/reader-blocks.lisp
(in-package #:org-kit)

(defvar *reader-inside-property-drawer*
  nil)

(defvar *reader-inside-drawer*
  nil)

(defun reader-blocks-prefix-p (text prefix)
  "Return true when TEXT begins with PREFIX, ignoring case."
  (and (>= (length text) (length prefix))
       (string-equal text prefix :end1 (length prefix))))

(defun reader-blocks-trim (text)
  "Trim horizontal whitespace around TEXT."
  (string-trim '(#\Space #\Tab) text))

(defun reader-blocks-split-word (text)
  "Return the first whitespace-delimited word in TEXT and its trimmed remainder."
  (let ((separator
         (position-if
          (lambda (character)
            (find character '(#\Space #\Tab)))
          text)))
    (if separator
        (values (subseq text 0 separator)
                (reader-blocks-trim (subseq text separator)))
        (values text nil))))

(defun reader-blocks-opener (line)
  "Return the kind, name, and arguments of a block opener LINE."
  (cond
    ((reader-blocks-prefix-p line "#+BEGIN:")
     (multiple-value-bind (name arguments)
         (reader-blocks-split-word (reader-blocks-trim (subseq line 8)))
       (when (plusp (length name))
         (values :dynamic
                 (string-downcase name)
                 (and arguments (plusp (length arguments)) arguments)))))
    ((reader-blocks-prefix-p line "#+BEGIN_")
     (multiple-value-bind (name arguments)
         (reader-blocks-split-word (reader-blocks-trim (subseq line 8)))
       (when (plusp (length name))
         (values :standard
                 (string-downcase name)
                 (and arguments (plusp (length arguments)) arguments)))))))

(defun reader-blocks-find-close (context start limit marker)
  "Return the start and after-line offsets of MARKER, or LIMIT twice."
  (let ((position start))
    (loop while (< position limit)
          for line = (reader-line-text context position)
          when (string-equal (reader-blocks-trim line) marker)
            do (return-from reader-blocks-find-close
                 (values position
                         (min limit (reader-next-line context position))))
          do (setf position (min limit (reader-next-line context position))))
    (values limit limit)))

(defun reader-blocks-contents (context start end depth)
  "Parse the node children in the source interval START to END."
  (if (< start end)
      (reader-parse-elements context start end (1+ depth))
      nil))

(defun reader-blocks-child-bounds (children)
  "Return the outer content bounds of CHILDREN, or NIL bounds when empty."
  (if children
      (values (node-begin (first children)) (node-end (car (last children))))
      (values nil nil)))

(defun reader-blocks-drawer-name (line)
  "Return a drawer name from LINE, excluding the closing END marker."
  (when
      (and (> (length line) 2)
           (char= (char line 0) #\:)
           (char= (char line (1- (length line))) #\:))
    (let ((name (subseq line 1 (1- (length line)))))
      (unless
          (or (find #\: name) (string-equal name "END"))
        (string-upcase name)))))

(defun reader-blocks-previous-line-start (context start)
  "Return the source offset of the line immediately before START."
  (let* ((text (reader-context-text context))
         (last-newline (position #\Newline text :end start :from-end t)))
    (when last-newline
      (let ((previous-newline
             (position #\Newline text :end last-newline :from-end t)))
        (if previous-newline
            (1+ previous-newline)
            0)))))

(defun reader-blocks-planning-line-p (line)
  "Return true when LINE is a scheduled or deadline planning line."
  (or (reader-blocks-prefix-p line "SCHEDULED:")
      (reader-blocks-prefix-p line "DEADLINE:")))

(defun reader-blocks-node-property-fields (line)
  "Return the key and value from a property drawer line."
  (when (and (> (length line) 2) (char= (char line 0) #\:))
    (let ((separator (position #\: line :start 1)))
      (when (and separator (> separator 1))
        (values (string-upcase (subseq line 1 separator))
                (reader-blocks-trim (subseq line (1+ separator))))))))

(defun reader-blocks-property-drawer-p (context start)
  "Return true for a property drawer after a heading or planning line."
  (let ((previous-start (reader-blocks-previous-line-start context start)))
    (when previous-start
      (or (reader-heading-level context previous-start start)
          (reader-blocks-planning-line-p
           (reader-line-text context previous-start))))))

(defun reader-blocks-example-escape-p (line)
  (and (> (length line) 1)
       (char= (char line 0) #\,)
       (let ((syntax-start
               (position-if-not (lambda (character) (char= character #\,)) line)))
         (and syntax-start
              (or (char= (char line syntax-start) #\*)
                  (and (< (1+ syntax-start) (length line))
                       (char= (char line syntax-start) #\#)
                       (char= (char line (1+ syntax-start)) #\+)))))))

(defun reader-blocks-example-value (body)
  (with-output-to-string (stream)
    (loop with start = 0
          for newline = (position #\Newline body :start start)
          for end = (or newline (length body))
          for line = (subseq body start end)
          do (write-string (if (reader-blocks-example-escape-p line)
                               (subseq line 1)
                               line)
                           stream)
             (when newline (write-char #\Newline stream))
          when (null newline) return nil
          do (setf start (1+ newline)))))

(defun reader-blocks-parse-latex (context start limit depth)
  "Parse a delimited LaTeX environment at START."
  (let* ((line (reader-line-text context start))
         (prefix "\\begin{")
         (close-brace
          (and (reader-blocks-prefix-p line prefix)
               (position #\} line :start (length prefix)))))
    (when (and close-brace (> close-brace (length prefix)))
      (let* ((environment (subseq line (length prefix) close-brace))
             (marker (format nil "\\end{~A}" environment)))
        (multiple-value-bind (close-start after-line)
            (reader-blocks-find-close context
                                      (reader-next-line context start)
                                      limit
                                      marker)
          (declare (ignore close-start))
          (values
           (make-reader-node context
                             :latex-environment
                             :begin
                             start
                             :end
                             after-line
                             :properties
                             (list :value
                                   (subseq (reader-context-text context)
                                           start
                                           after-line))
                             :depth
                             depth)
           after-line))))))

(defun reader-blocks-standard-type (name arguments body)
  "Return the node type and properties for a standard block opener."
  (cond
    ((string= name "quote") (values :quote-block (list :type nil) :elements))
    ((string= name "center") (values :center-block (list :type nil) :elements))
    ((string= name "comment") (values :comment-block (list :value body) :raw))
    ((string= name "example")
     (values :example-block
             (list :switches nil :value (reader-blocks-example-value body))
             :raw))
    ((string= name "export")
     (values :export-block
             (list :type (string-upcase (or arguments "")) :value body)
             :raw))
    ((string= name "src")
     (multiple-value-bind (language switches)
         (reader-blocks-split-word (or arguments ""))
       (values :src-block
               (list :language
                     language
                     :value
                     body
                     :switches
                     switches
                     :parameters
                     nil
                     :name
                     nil)
               :raw)))
    ((string= name "verse") (values :verse-block (list :type nil) :verse))
    (t
     (values :special-block
             (list :type name :parameters arguments :name nil)
             :elements))))

(defun reader-blocks-parse-standard (context start limit depth name arguments)
  "Parse a standard block, including unknown names as special blocks."
  (let* ((marker (format nil "#+END_~A" name))
         (content-start (min limit (reader-next-line context start))))
    (multiple-value-bind (close-start after-line)
        (reader-blocks-find-close context content-start limit marker)
      (let ((body
             (subseq (reader-context-text context) content-start close-start)))
        (multiple-value-bind (type properties contents-kind)
            (reader-blocks-standard-type name arguments body)
          (let* ((children
                  (case contents-kind
                    (:elements
                     (reader-blocks-contents context
                                             content-start
                                             close-start
                                             depth))
                    (otherwise nil)))
                 (contents-bounds
                  (if (eq contents-kind :elements)
                      (multiple-value-list
                       (reader-blocks-child-bounds children))
                      (if (and (eq contents-kind :verse)
                               (< content-start close-start))
                          (list content-start close-start)
                          (list nil nil))))
                 (node
                  (make-reader-node context
                                    type
                                    :begin
                                    start
                                    :end
                                    after-line
                                    :contents-begin
                                    (first contents-bounds)
                                    :contents-end
                                    (second contents-bounds)
                                    :properties
                                    properties
                                    :children
                                    children
                                    :depth
                                    depth)))
            (values node after-line)))))))

(defun reader-blocks-parse-drawer (context start limit depth name)
  "Parse a drawer or property drawer at START."
  (multiple-value-bind (close-start after-line)
      (reader-blocks-find-close context
                                (reader-next-line context start)
                                limit
                                ":END:")
    (let* ((content-start (min limit (reader-next-line context start)))
           (property-drawer-p
            (and (string= name "PROPERTIES")
                 (reader-blocks-property-drawer-p context start)))
           (children
            (let ((*reader-inside-drawer* t)
                  (*reader-inside-property-drawer* property-drawer-p))
              (reader-blocks-contents context content-start close-start depth)))
           (bounds (multiple-value-list (reader-blocks-child-bounds children)))
           (type
            (if property-drawer-p
                :property-drawer
                :drawer))
           (properties
            (unless property-drawer-p
              (list :drawer-name name)))
           (node
            (make-reader-node context
                              type
                              :begin
                              start
                              :end
                              after-line
                              :contents-begin
                              (first bounds)
                              :contents-end
                              (second bounds)
                              :properties
                              properties
                              :children
                              children
                              :depth
                              depth)))
      (values node after-line))))

(defun reader-blocks-parse-dynamic (context start limit depth name arguments)
  "Parse a dynamic block and its element contents."
  (multiple-value-bind (close-start after-line)
      (reader-blocks-find-close context
                                (reader-next-line context start)
                                limit
                                "#+END:")
    (let* ((content-start (min limit (reader-next-line context start)))
           (children
            (reader-blocks-contents context content-start close-start depth))
           (bounds (multiple-value-list (reader-blocks-child-bounds children)))
           (node
            (make-reader-node context
                              :dynamic-block
                              :begin
                              start
                              :end
                              after-line
                              :contents-begin
                              (first bounds)
                              :contents-end
                              (second bounds)
                              :properties
                              (list :block-name name :arguments arguments)
                              :children
                              children
                              :depth
                              depth)))
      (values node after-line))))

(defun reader-blocks-footnote-opener (context offset)
  "Return a footnote label and body offset for a definition line."
  (let* ((line (reader-line-text context offset))
         (prefix "[FN:")
         (close-bracket
          (and (reader-blocks-prefix-p line prefix)
               (position #\] line :start (length prefix)))))
    (when (and close-bracket (> close-bracket (length prefix)))
      (let* ((after-label (1+ close-bracket))
             (body-offset
              (loop for index from after-label below (length line)
                    while (find (char line index) '(#\Space #\Tab))
                    finally (return index)))
             (body-start
              (if (= body-offset (length line))
                  (reader-next-line context offset)
                  (+ offset body-offset))))
        (values (subseq line (length prefix) close-bracket) body-start)))))

(defun reader-blocks-footnote-boundary-p (context offset limit)
  "Return true when OFFSET begins the next footnote or a headline."
  (or (nth-value 0 (reader-blocks-footnote-opener context offset))
      (reader-heading-level context offset limit)))

(defun reader-blocks-parse-footnote (context start limit depth label body-start)
  "Parse a footnote definition through its next definition or headline."
  (let ((close-start (min limit (reader-next-line context start))))
    (loop while (< close-start limit)
          do (if (reader-blocks-footnote-boundary-p context close-start limit)
                 (return)
                 (setf close-start (min limit
                                        (reader-next-line context close-start)))))
    (let* ((content-end
            (loop with position = body-start
                  with content-end = body-start
                  while (< position close-start)
                  do (let ((after-line
                            (min close-start
                                 (reader-next-line context position))))
                       (unless (reader-blank-line-p context position)
                         (setf content-end after-line))
                       (setf position after-line))
                  finally (return content-end)))
           (children
            (reader-blocks-contents context body-start content-end depth))
           (bounds (multiple-value-list (reader-blocks-child-bounds children)))
           (node
            (make-reader-node context
                              :footnote-definition
                              :begin
                              start
                              :end
                              close-start
                              :contents-begin
                              (first bounds)
                              :contents-end
                              (second bounds)
                              :properties
                              (list :label label)
                              :children
                              children
                              :depth
                              depth)))
      (values node close-start))))

(defun reader-blocks-parse-block-line (context start limit depth)
  "Parse a block construct at START unless inside a property drawer."
  (unless *reader-inside-property-drawer*
    (let ((line (reader-line-text context start)))
      (multiple-value-bind (kind name arguments) (reader-blocks-opener line)
        (when kind
          (return-from reader-blocks-parse-block-line
            (if (eq kind :dynamic)
                (reader-blocks-parse-dynamic context
                                             start
                                             limit
                                             depth
                                             name
                                             arguments)
                (reader-blocks-parse-standard context
                                              start
                                              limit
                                              depth
                                              name
                                              arguments)))))
      (let ((drawer-name
             (unless *reader-inside-drawer*
               (reader-blocks-drawer-name line))))
        (when drawer-name
          (return-from reader-blocks-parse-block-line
            (reader-blocks-parse-drawer context start limit depth drawer-name))))
      (multiple-value-bind (label body-start)
          (reader-blocks-footnote-opener context start)
        (when label
          (return-from reader-blocks-parse-block-line
            (reader-blocks-parse-footnote context
                                          start
                                          limit
                                          depth
                                          label
                                          body-start))))))
  (values nil nil))

(defmethod parse-block-at ((context reader-context) start limit depth)
  (block parse-block-at
    (dolist
        (parser
         (list #'reader-blocks-parse-latex #'reader-blocks-parse-block-line))
      (multiple-value-bind (node next)
          (funcall parser context start limit depth)
        (when node
          (return-from parse-block-at
            (values node next)))))
    (values nil nil)))

(defun reader-blocks-fixed-width-p (line)
  "Return true when LINE is a fixed-width line rather than a drawer marker."
  (and (plusp (length line))
       (char= (char line 0) #\:)
       (or (= (length line) 1) (find (char line 1) '(#\Space #\Tab)))))

(defun reader-blocks-fixed-width-value (line)
  "Return the text after the fixed-width marker in LINE."
  (let ((start 1))
    (when
        (and (< start (length line)) (find (char line start) '(#\Space #\Tab)))
      (incf start))
    (subseq line start)))

(defun reader-blocks-parse-fixed-width (context start limit depth)
  "Parse consecutive fixed-width lines at START."
  (let ((position start)
        (lines '()))
    (loop while (< position limit)
          for line = (reader-line-text context position)
          while (reader-blocks-fixed-width-p line)
          do (push (reader-blocks-fixed-width-value line) lines) (setf position (min
                                                                                 limit
                                                                                 (reader-next-line
                                                                                  context
                                                                                  position))))
    (when lines
      (values
       (make-reader-node context
                         :fixed-width
                         :begin
                         start
                         :end
                         position
                         :properties
                         (list :value
                               (format nil "~{~A~^~%~}" (nreverse lines)))
                         :depth
                         depth)
       position))))

(defun reader-blocks-parse-comment (context start limit depth line after-line)
  "Parse a regular comment line at START."
  (let ((value-start 1))
    (when
        (and (< value-start (length line))
             (char= (char line value-start) #\Space))
      (incf value-start))
    (values
     (make-reader-node context
                       :comment
                       :begin
                       start
                       :end
                       after-line
                       :properties
                       (list :value (subseq line value-start))
                       :depth
                       depth)
     (min limit after-line))))

(defun reader-blocks-call-fields (value)
  "Return a Babel call's name and argument source text."
  (let ((open (position #\( value)))
    (if open
        (let* ((close (position #\) value :start open))
               (name (reader-blocks-trim (subseq value 0 open)))
               (arguments
                (and close (reader-blocks-trim (subseq value (1+ open) close)))))
          (values name (and arguments (plusp (length arguments)) arguments)))
        (values (reader-blocks-trim value) nil))))

(defun reader-blocks-keyword-node (context start
                                           limit
                                           depth
                                           after-line
                                           key
                                           value)
  "Build a keyword or Babel-call node from parsed keyword fields."
  (let ((end (min limit after-line)))
    (if (string= key "CALL")
        (multiple-value-bind (call arguments) (reader-blocks-call-fields value)
          (make-reader-node context
                            :babel-call
                            :begin
                            start
                            :end
                            end
                            :properties
                            (list :call
                                  call
                                  :inside-header
                                  nil
                                  :arguments
                                  arguments
                                  :end-header
                                  nil)
                            :depth
                            depth))
        (make-reader-node context
                          :keyword
                          :begin
                          start
                          :end
                          end
                          :properties
                          (list :key key :value value :options nil)
                          :depth
                          depth))))

(defun reader-blocks-horizontal-rule-p (line)
  "Return true when LINE contains at least five hyphens and nothing else."
  (and (>= (length line) 5)
       (every
        (lambda (character)
          (char= character #\-))
        line)))

(defun reader-blocks-clock-value-range (line offset)
  "Return the source range of a clock timestamp or timestamp range."
  (let* ((clock-end (search "CLOCK:" line :test #'char-equal))
         (timestamp-start
          (and clock-end
               (position-if-not
                (lambda (character)
                  (find character '(#\Space #\Tab)))
                line
                :start
                (+ clock-end (length "CLOCK:"))))))
    (when timestamp-start
      (let* ((open-character (char line timestamp-start))
             (close-character
              (if (char= open-character #\[)
                  #\]
                  #\>))
             (first-close
              (and (find open-character '(#\[ #\<))
                   (position close-character line :start (1+ timestamp-start)))))
        (when first-close
          (let* ((first-end (1+ first-close))
                 (separator (search "--" line :start2 first-end))
                 (second-start
                  (and separator
                       (< (+ separator 2) (length line))
                       (+ separator 2)))
                 (second-close
                  (and second-start
                       (char= (char line second-start) open-character)
                       (position close-character line :start (1+ second-start))))
                 (range-end
                  (if second-close
                      (1+ second-close)
                      first-end))
                 (duration-start (search "=>" line :start2 range-end))
                 (value-end
                  (loop with position = range-end
                        while (and (< position (or duration-start (length line)))
                                   (find (char line position) '(#\Space #\Tab)))
                        do (incf position)
                        finally (return position))))
            (cons (+ offset timestamp-start)
                  (+ offset value-end))))))))

(defmethod parse-line-element-at ((context reader-context) start limit depth)
  (block parse-line-element-at
    (multiple-value-bind (line-end after-line)
          (reader-line-bounds context start)
        (let* ((line (reader-line-text context start (min line-end limit)))
               (next (min limit after-line)))
          (when *reader-inside-property-drawer*
            (multiple-value-bind (key value)
                (reader-blocks-node-property-fields line)
              (when key
                (return-from parse-line-element-at
                  (values
                   (make-reader-node context
                                     :node-property
                                     :begin
                                     start
                                     :end
                                     next
                                     :properties
                                     (list :key key :value value)
                                     :depth
                                     depth)
                   next)))))
          (multiple-value-bind (key value) (reader-keyword-fields context start)
            (when key
              (return-from parse-line-element-at
                (values
                 (reader-blocks-keyword-node context
                                             start
                                             limit
                                             depth
                                             next
                                             key
                                             value)
                 next))))
          (when (reader-blocks-prefix-p line "CLOCK:")
            (let* ((value-range (reader-blocks-clock-value-range line start))
                   (properties
                    (append
                     (list :value
                           nil
                           :status
                           (if (search "=>" line)
                               :closed
                               :running))
                     (when value-range
                       (list :reader-clock-value-range value-range)))))
              (return-from parse-line-element-at
                (values
                 (make-reader-node context
                                   :clock
                                   :begin
                                   start
                                   :end
                                   next
                                   :properties
                                   properties
                                   :depth
                                   depth)
                 next))))
          (when (reader-blocks-prefix-p line "%%(")
            (return-from parse-line-element-at
              (values
               (make-reader-node context
                                 :diary-sexp
                                 :begin
                                 start
                                 :end
                                 next
                                 :properties
                                 (list :value line)
                                 :depth
                                 depth)
               next)))
          (when (reader-blocks-horizontal-rule-p line)
            (return-from parse-line-element-at
              (values
               (make-reader-node context
                                 :horizontal-rule
                                 :begin
                                 start
                                 :end
                                 next
                                 :depth
                                 depth)
               next)))
          (when
              (and (plusp (length line))
                   (char= (char line 0) #\#)
                   (not (reader-blocks-prefix-p line "#+")))
            (return-from parse-line-element-at
              (reader-blocks-parse-comment context start limit depth line next)))
          (when (reader-blocks-fixed-width-p line)
            (return-from parse-line-element-at
              (reader-blocks-parse-fixed-width context start limit depth)))))
    (values nil nil)))

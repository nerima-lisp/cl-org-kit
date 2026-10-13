;;;; src/reader-core.lisp
(in-package #:org-kit)

(defstruct (reader-context (:constructor %make-reader-context)
                           (:conc-name reader-context-)
                           (:copier nil))
  document
  text
  limits
  settings
  (node-count 0))

;; The element-specific methods are loaded after this core file.
(declaim (ftype function reader-buffer-settings parse-headline-at parse-block-at
                  parse-collection-at parse-line-element-at))

(defun reader-default-settings ()
  "Return the settings used when a document has no settings directive."
  (list :todo-keywords '(("TODO" . :todo) ("DONE" . :done))
        :todo-sequences nil
        :startup nil
        :inlinetask-min-level 15
        :link-abbrevs nil))

(defun reader-call-parser (parser context start limit depth)
  "Call an element parser, which returns NIL values when it does not match."
  (funcall parser context start limit depth))

(defun reader-line-bounds (context offset)
  "Return the lexical line end and the offset after its newline."
  (let* ((text (reader-context-text context))
         (limit (length text))
         (newline (position #\Newline text :start offset :end limit)))
    (if newline
        (values (if (and (> newline offset)
                        (char= (char text (1- newline)) #\Return))
                   (1- newline)
                   newline)
                (1+ newline))
        (values limit limit))))

(defun reader-line-text (context offset &optional end)
  "Return the lexical text of the line at OFFSET, or through END."
  (let ((line-end (or end (nth-value 0 (reader-line-bounds context offset)))))
    (subseq (reader-context-text context) offset line-end)))

(defun reader-next-line (context offset)
  "Return the offset immediately after the line at OFFSET."
  (nth-value 1 (reader-line-bounds context offset)))

(defun reader-blank-line-p (context offset)
  "Return true when the line at OFFSET contains only horizontal whitespace."
  (every (lambda (character) (find character '(#\Space #\Tab)))
         (reader-line-text context offset)))

(defun reader-heading-level (context offset limit)
  "Return the star count when OFFSET begins a syntactically valid heading."
  (multiple-value-bind (end next) (reader-line-bounds context offset)
    (declare (ignore next))
    (let* ((text (reader-context-text context))
           (line-end (min end limit))
           (index offset))
      (loop while (and (< index line-end) (char= (char text index) #\*))
            do (incf index))
      (when (and (> index offset)
                 (or (= index line-end)
                     (find (char text index) '(#\Space #\Tab))))
        (- index offset)))))

(defun make-reader-node (context type &key begin end contents-begin contents-end
                                        (post-affiliated begin post-affiliated-p)
                                        (post-blank 0) properties children depth)
  "Build one parsed node while enforcing document depth and count limits."
  (let* ((count (1+ (reader-context-node-count context)))
         (node-depth (or depth 1)))
    (check-limit (reader-context-limits context) :max-element-depth node-depth)
    (check-limit (reader-context-limits context) :max-node-count count)
    (setf (reader-context-node-count context) count)
    (make-node type :begin begin :end end
                    :contents-begin contents-begin :contents-end contents-end
                    :post-affiliated (if post-affiliated-p post-affiliated begin)
                    :post-blank post-blank :properties properties :children children)))

(defun reader-keyword-fields (context offset)
  "Return an uppercase key, trimmed value, and source spelling, or NIL values."
  (let ((line (reader-line-text context offset)))
    (when (and (>= (length line) 3)
               (char= (char line 0) #\#)
               (char= (char line 1) #\+))
      (let ((colon (position #\: line :start 2)))
        (when colon
          (let ((source-key (string-trim '(#\Space #\Tab)
                                          (subseq line 2 colon))))
            (values (string-upcase source-key)
                  (string-trim '(#\Space #\Tab)
                               (subseq line (1+ colon)))
                    source-key)))))))

(defun reader-affiliated-keyword-p (key)
  "Return true when KEY can attach to the following element."
  (or (member key '("NAME" "CAPTION" "HEADER" "PLOT" "RESULTS") :test #'string=)
      (and (>= (length key) 5) (string= key "ATTR_" :end1 5 :end2 5))))

(defun reader-affiliation-property (key)
  (intern (if (and (>= (length key) 5) (string= key "ATTR_" :end1 5 :end2 5))
              (string-upcase key)
              key)
          :keyword))

(defun reader-affiliations-at (context start limit)
  "Return affiliated properties, the next offset, and whether any were found."
  (let ((position start)
        (properties '())
        (found nil))
    (loop while (< position limit)
          do (multiple-value-bind (key value) (reader-keyword-fields context position)
               (if (and key (reader-affiliated-keyword-p key))
                   (let ((end (reader-next-line context position)))
                     (setf found t)
                     (setf (getf properties (reader-affiliation-property key)) value)
                     (setf position (min end limit)))
                   (return))))
    (values properties position found)))

(defun reader-parse-recognized-at (context start limit depth)
  "Try the ordered non-paragraph element parsers at START."
  (let ((parsers (list #'parse-headline-at #'parse-block-at
                       #'parse-collection-at #'parse-line-element-at)))
    (dolist (parser parsers (values nil nil))
      (multiple-value-bind (node next)
          (reader-call-parser parser context start limit depth)
        (when node
          (when (eq (node-type node) :headline)
            (check-limit (reader-context-limits context) :max-heading-depth
                         (node-property node :level 0)))
          (return (values node next)))))))

(defun reader-parse-one-at (context start limit depth)
  "Parse one recognized element at START, including preceding affiliations."
  (multiple-value-bind (affiliations content-start affiliated-p)
      (reader-affiliations-at context start limit)
    (let* ((attach-p (and affiliated-p (< content-start limit)
                          (not (reader-blank-line-p context content-start))))
           (candidate-start (if attach-p content-start start)))
      (multiple-value-bind (node next)
          (reader-parse-recognized-at context candidate-start limit depth)
        (when node
          (setf (%node-begin node) start
                (%node-post-affiliated node) (if attach-p candidate-start start))
          (when attach-p
            (loop for (key value) on affiliations by #'cddr
                  do (setf (getf (%node-properties node) key) value)))
          (values node next))))))

(defun reader-extend-node-end (node end blank-count)
  ;; Parsed nodes end at their returned offsets, before this following blank run.
  (setf (%node-end node) (max end (node-end node)))
  (incf (%node-post-blank node) blank-count))

(defun reader-make-paragraph (context start contents-begin contents-end end depth
                              post-blank properties)
  (make-reader-node context :paragraph
                    :begin start :end end :contents-begin contents-begin
                    :contents-end contents-end :post-affiliated contents-begin
                    :post-blank post-blank :properties properties :depth depth))

(defun reader-parse-elements (context start end depth)
  "Parse a region into ordered element nodes, using paragraphs as fallback."
  (let ((position start)
        (nodes nil)
        (pending-node nil)
        (pending-start nil)
        (pending-end nil))
    (loop while (< position end)
          do (progn
               (when (reader-blank-line-p context position)
                 (let ((blank-start position)
                       (blank-count 0))
                   (loop while (and (< position end)
                                    (reader-blank-line-p context position))
                         do (incf blank-count)
                            (setf position
                                  (min end (reader-next-line context position))))
                   (when nodes
                     (reader-extend-node-end (car nodes) position blank-count))
                   (when (<= position blank-start)
                     (setf position end))
                   (when (>= position end)
                     (return))))
               (let ((node nil)
                     (next nil))
                 (if (and pending-node
                          (= position pending-start))
                     (setf node pending-node
                           next pending-end
                           pending-node nil
                           pending-start nil
                           pending-end nil)
                     (multiple-value-setq (node next)
                       (reader-parse-one-at context position end depth)))
                 (if node
                     (progn
                       (push node nodes)
                       (setf position next))
                     (let ((paragraph-start position)
                           (paragraph-contents-start position)
                           (cursor position)
                           (contents-end end)
                           (paragraph-end end)
                           (post-blank 0)
                           (paragraph-properties nil)
                           (boundary-found nil))
                       (multiple-value-bind (properties after-affiliated affiliated-p)
                           (reader-affiliations-at context position end)
                         (when affiliated-p
                           (setf paragraph-contents-start after-affiliated
                                 paragraph-properties properties)))
                       (setf cursor
                             (min end
                                  (reader-next-line context paragraph-contents-start)))
                       (loop while (< cursor end)
                             do (if (reader-blank-line-p context cursor)
                                    (progn
                                      (setf contents-end cursor)
                                      (loop while (and (< cursor end)
                                                       (reader-blank-line-p context cursor))
                                            do (incf post-blank)
                                               (setf cursor
                                                     (min end
                                                          (reader-next-line context cursor))))
                                      (setf paragraph-end cursor
                                            boundary-found t)
                                      (return))
                                    (multiple-value-bind (candidate candidate-end)
                                        (reader-parse-one-at context cursor end depth)
                                      (if candidate
                                          (progn
                                            (setf contents-end cursor
                                                  paragraph-end cursor
                                                  pending-node candidate
                                                  pending-start cursor
                                                  pending-end candidate-end
                                                  boundary-found t)
                                            (return))
                                          (multiple-value-bind (properties after-affiliated affiliated-p)
                                              (reader-affiliations-at context cursor end)
                                            (declare (ignore properties after-affiliated))
                                            (if affiliated-p
                                                (progn
                                                  (setf contents-end cursor
                                                        paragraph-end cursor
                                                        boundary-found t)
                                                  (return))
                                                (setf cursor
                                                      (min end
                                                           (reader-next-line context cursor)))))))))
                       (unless boundary-found
                         (setf contents-end end
                               paragraph-end end))
                       (push (reader-make-paragraph context
                                                   paragraph-start
                                                   paragraph-contents-start
                                                   contents-end
                                                   paragraph-end
                                                   depth
                                                   post-blank
                                                   paragraph-properties)
                             nodes)
                       (setf position paragraph-end))))))
    (nreverse nodes)))



(defun reader-root-children (context children)
  "Wrap top-level preamble elements in their Org section container."
  (let ((first-headline (position-if (lambda (node) (eq (node-type node) :headline))
                                     children)))
    (if (or (zerop (length children)) (eql first-headline 0))
        children
        (let* ((preamble-end (or first-headline (length children)))
               (preamble (subseq children 0 preamble-end))
               (first (first preamble))
               (last (car (last preamble)))
               (section (make-reader-node context :section
                                          :begin (node-begin first)
                                          :end (node-end last)
                                          :contents-begin (node-begin first)
                                          :contents-end (node-end last)
                                          :post-affiliated (node-begin first)
                                          :children preamble :depth 1)))
          (cons section (nthcdr preamble-end children))))))

(defun reader-take-property (node key)
  (multiple-value-bind (value present) (node-property node key)
    (when present
      (remf (%node-properties node) key))
    (values value present)))

(defun reader-objects-in-range (document range container-type)
  (when range
    ;; Parser-produced ranges are offsets bounded by the source scan.
    (parse-objects document (car range) (cdr range) container-type)))

(defun reader-set-object-property (document node property range-key container-type)
  (multiple-value-bind (range present) (reader-take-property node range-key)
    (when present
      (let ((object (find-if (lambda (child) (eq (node-type child) :timestamp))
                             (reader-objects-in-range document range container-type))))
        (when object
          (setf (%node-parent object) node))
        (setf (node-property node property) object)))))

(defun reader-title-segments (document node range &optional empty-title-p)
  (let ((objects (reader-objects-in-range document range (node-type node))))
    (if (and empty-title-p range (= (car range) (cdr range)))
        (list "")
        (loop for object in objects
              collect (if (eq (node-type object) :plain-text)
                          (subseq (document-text document)
                                  (node-begin object) (node-end object))
                          (progn
                            (setf (%node-parent object) node)
                            object))))))

(defun reader-parse-object-ranges (document root)
  (prepare-object-parsing document)
  (map-nodes
   (lambda (node)
     (multiple-value-bind (title-range has-title-range)
         (reader-take-property node :reader-title-range)
       (when has-title-range
         (setf (node-property node :title)
               (reader-title-segments document node title-range t))))
     (multiple-value-bind (tag-range has-tag-range)
         (reader-take-property node :reader-tag-range)
       (when has-tag-range
         (setf (node-property node :tag)
               (reader-title-segments document node tag-range))))
     (dolist (property '(:scheduled :deadline :closed))
       (reader-set-object-property document node property
                                   (intern (format nil "READER-~A-RANGE" property) :keyword)
                                   (node-type node)))
     (when (eq (node-type node) :clock)
       (reader-set-object-property document node :value :reader-clock-value-range :clock))
     (when (and (null (node-children node))
                (member (node-type node) '(:paragraph :item :table-cell :verse-block))
                (node-contents-begin node)
                (node-contents-end node))
       (setf (node-children node)
             (parse-objects document (node-contents-begin node)
                            (node-contents-end node) (node-type node)))))
   root))

(defun reader-check-element-depths (root limits)
  (labels ((check-subtree (node depth)
             (check-limit limits :max-element-depth depth)
             (dolist (child (node-children node))
               (check-subtree child (1+ depth)))))
    (dolist (child (node-children root))
      (check-subtree child 1))))

(defun reader-explicit-settings (settings todo-keywords todo-keywords-p startup startup-p
                                 inlinetask-min-level inlinetask-min-level-p
                                 link-abbrevs link-abbrevs-p)
  (let ((result (copy-list settings)))
    (when todo-keywords-p (setf (getf result :todo-keywords) todo-keywords))
    (when startup-p (setf (getf result :startup) startup))
    (when inlinetask-min-level-p
      (setf (getf result :inlinetask-min-level) inlinetask-min-level))
    (when link-abbrevs-p (setf (getf result :link-abbrevs) link-abbrevs))
    result))

(defun parse-org (input &key source-name encoding (limits (make-limits)) settings
                              (todo-keywords nil todo-keywords-p)
                              (startup nil startup-p)
                              (inlinetask-min-level nil inlinetask-min-level-p)
                              (link-abbrevs nil link-abbrevs-p))
  "Parse INPUT into a DOCUMENT whose root is an Org element tree.
INPUT is accepted by MAKE-DOCUMENT. In-buffer settings can be overridden by
SETTINGS or the corresponding keyword arguments. LIMITS bounds input, depth,
and node count."
  (unless (limits-p limits)
    (error 'type-error :datum limits :expected-type 'limits))
  (let* ((document (make-document input :source-name source-name
                                         :encoding encoding :limits limits))
         (context (%make-reader-context :document document
                                        :text (document-text document)
                                        :limits limits :settings nil))
         (explicit-settings
           (reader-explicit-settings settings todo-keywords todo-keywords-p
                                     startup startup-p inlinetask-min-level
                                     inlinetask-min-level-p link-abbrevs link-abbrevs-p))
         (settings (reader-buffer-settings context explicit-settings))
         (length (length (reader-context-text context))))
    (setf (reader-context-settings context) settings
          (document-settings document) settings)
    (let* ((children (reader-root-children
                      context (reader-parse-elements context 0 length 1)))
           (root (make-reader-node context :org-data
                                  :begin 0 :end length :contents-begin 0
                                  :contents-end length :post-affiliated 0
                                  :children children :depth 0)))
      (setf (document-root document) root)
      (reader-check-element-depths root limits)
      (reader-parse-object-ranges document root)
      document)))

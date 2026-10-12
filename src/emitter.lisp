;;;; src/emitter.lisp
;;;;
;;;; Canonical Org serialization for detached or parsed node trees.
(in-package #:org-kit)

(defvar *emitter-active-nodes* nil)
(defvar *emitter-node-count* 0)
(defvar *emitter-radio-targets* nil)
(defvar *emitter-element-depth* 0)
(defvar *emitter-object-depth* 0)
(defvar *emitter-current-limits* nil)
(defvar *emitter-todo-keywords* nil)
(defvar *emitter-todo-setting-seen* nil)
(defvar *emitter-odd-levels* nil)

(defun emitter-fail (node message &rest arguments)
  (error 'org-kit-error
         :message (apply #'format nil
                         (concatenate 'string
                                      "Cannot emit "
                                      (string-downcase
                                       (symbol-name (node-type node)))
                                      ": "
                                      message)
                         arguments)))

(defun emitter-join (strings)
  (with-output-to-string (stream)
    (dolist (string strings)
      (write-string string stream))))

(defun emitter-required-string (node key)
  (multiple-value-bind (value present) (node-property node key)
    (unless (and present (stringp value))
      (emitter-fail node "property ~S must be a string" key))
    value))

(defun emitter-line-value (node value description)
  (unless (and (stringp value)
               (not (position #\Newline value))
               (not (position #\Return value)))
    (emitter-fail node "~A must be a single line" description))
  value)

(defun emitter-trimmed-line-value (node value description)
  (let ((line (emitter-line-value node value description)))
    (unless (string= line (string-trim '(#\Space #\Tab) line))
      (emitter-fail node "~A must not have surrounding horizontal whitespace"
                    description))
    line))

(defun emitter-normalize-newlines (string)
  (with-output-to-string (stream)
    (loop for position from 0 below (length string)
          for character = (char string position)
          do (cond
               ((char= character #\Return)
                (unless (and (< (1+ position) (length string))
                             (char= (char string (1+ position)) #\Newline))
                  (return-from emitter-normalize-newlines
                    (error 'org-kit-error :message
                           "Cannot emit a bare carriage return")))
                (write-char #\Newline stream)
                (incf position))
               (t (write-char character stream))))))

(defun emitter-ensure-newline (string)
  (if (and (plusp (length string))
           (char= (char string (1- (length string))) #\Newline))
      string
      (concatenate 'string string (string #\Newline))))

(defun emitter-without-post-blank (node value)
  (let ((end (length value))
        (blank-count (node-post-blank node)))
    (unless (typep blank-count '(integer 0))
      (emitter-fail node "post-blank must be a non-negative integer"))
    (loop repeat blank-count
          do (when (and (plusp end) (char= (char value (1- end)) #\Newline))
               (decf end))
             (let ((line-start
                     (1+ (or (position #\Newline value :end end :from-end t) -1))))
               (unless (every (lambda (character)
                                (find character '(#\Space #\Tab)))
                              (subseq value line-start end))
                 (emitter-fail node "source text has an invalid post-blank span"))
               (setf end line-start)))
    (subseq value 0 end)))

(defun emitter-append-post-blank (string node)
  (let ((blank-count (node-post-blank node)))
    (unless (typep blank-count '(integer 0))
      (emitter-fail node "post-blank must be a non-negative integer"))
    (concatenate 'string
                 (emitter-ensure-newline string)
                 (make-string blank-count :initial-element #\Newline))))

(defun emitter-format-affiliations (node)
  (let ((properties (node-properties node)))
    (with-output-to-string (stream)
      (loop for (key value) on properties by #'cddr
            for name = (case key
                         (:name "NAME")
                         (:caption "CAPTION")
                         (:header "HEADER")
                         (:plot "PLOT")
                         (:results "RESULTS")
                         (otherwise
                          (let ((symbol-name (and (keywordp key)
                                                  (symbol-name key))))
                            (when (and symbol-name
                                       (>= (length symbol-name) 5)
                                       (string= "ATTR_" symbol-name
                                                :end2 5))
                              symbol-name))))
            when name
              do (format stream "#+~A: ~A~%"
                         name
                         (emitter-trimmed-line-value
                          node value "affiliated keyword value"))))))

(defun emitter-element-sequence (nodes)
  (emitter-join
   (loop for node in nodes
         collect (concatenate 'string
                              (emitter-format-affiliations node)
                              (emitter-render node)))))

(defun emitter-add-todo-setting (value)
  (unless *emitter-todo-setting-seen*
    (setf *emitter-todo-setting-seen* t
          *emitter-todo-keywords* nil))
  (setf *emitter-todo-keywords*
        (append *emitter-todo-keywords*
                (reader-heading-todo-sequence value))))

(defun emitter-track-settings (node)
  (when (eq (node-type node) :keyword)
    (let ((key (node-property node :key))
          (value (node-property node :value)))
      (when (and (stringp key) (stringp value))
        (case (reader-heading-setting-kind key)
          (:todo (emitter-add-todo-setting value))
          (:startup
           (when (member "odd" (reader-heading-words value)
                         :test #'string-equal)
             (setf *emitter-odd-levels* t))))))))

(defun emitter-raw-block-body (node value closer)
  (let ((body (emitter-normalize-newlines value)))
    (when (and (plusp (length body))
               (char/= (char body (1- (length body))) #\Newline))
      (emitter-fail node "a non-empty block body must end with a newline"))
    (loop with start = 0
          for newline = (position #\Newline body :start start)
          for end = (or newline (length body))
          for line = (subseq body start end)
          when (string-equal (string-trim '(#\Space #\Tab) line) closer)
            do (emitter-fail node "payload contains its closing marker ~A" closer)
          while newline
          do (setf start (1+ newline)))
    body))

(defun emitter-raw-block (node opener closer value)
  (let ((body (emitter-raw-block-body node value closer)))
    (format nil "~A~%~A~A~%" opener body closer)))

(defun emitter-headline-title (node)
  (multiple-value-bind (title present) (node-property node :title)
    (if present
        (progn
          (unless (and (listp title)
                       (every (lambda (segment)
                                (or (stringp segment)
                                    (and (node-p segment)
                                         (eq (node-type-category
                                              (node-type segment))
                                             :object))))
                              title))
            (emitter-fail node "title must be a list of text and inline objects"))
          (with-output-to-string (stream)
            (let ((text-run ""))
              (dolist (segment title)
                (if (stringp segment)
                    (progn
                      (setf text-run (concatenate 'string text-run segment))
                      (write-string segment stream))
                    (progn
                      (when (plusp (length text-run))
                        (emitter-object-plain-text
                         (make-node :plain-text
                                    :properties (list :value text-run))))
                      (setf text-run "")
                      (write-string (emitter-render segment) stream))))
              (when (plusp (length text-run))
                (emitter-object-plain-text
                 (make-node :plain-text :properties (list :value text-run)))))))
        (emitter-required-string node :raw-value))))

(defun emitter-title-metadata-prefix-p (title todo-keywords)
  (some (lambda (entry)
          (let* ((name (car entry))
                 (end (length name)))
            (and (<= end (length title))
                 (string= name title :end2 end)
                 (or (= end (length title))
                     (find (char title end) '(#\Space #\Tab))))))
        todo-keywords))

(defun emitter-title-priority-prefix-p (title)
  (nth-value 0 (reader-heading-priority-at title 0 (length title))))

(defun emitter-title-comment-prefix-p (title)
  (and (>= (length title) 7)
       (string-equal "COMMENT" title :end2 7)
       (or (= (length title) 7)
           (find (char title 7) '(#\Space #\Tab)))))

(defun emitter-title-tag-suffix-p (title)
  (nth-value 0 (reader-heading-tags-at-end title 0 (length title))))

(defun emitter-headline (node inline-task-p)
  (let* ((level (node-property node :level))
         (title (emitter-headline-title node))
         (todo (node-property node :todo-keyword))
         (todo-type (node-property node :todo-type))
         (priority (node-property node :priority))
         (commented (node-property node :commentedp))
         (tags (node-property node :tags))
         (stars (if (typep level '(integer 1))
                    (let ((effective-level
                            (if inline-task-p (max 15 level) level)))
                      (if *emitter-odd-levels*
                          (1- (* 2 effective-level))
                          effective-level))
                    (emitter-fail node
                                  "headline level must be a positive integer"))))
    (unless (typep stars '(integer 1))
      (emitter-fail node "headline level must be a positive integer"))
    (check-limit *emitter-current-limits* :max-heading-depth level)
    (unless (member (node-property node :commentedp) '(nil t))
      (emitter-fail node "commentedp must be boolean"))
    (unless (member (node-property node :archivedp) '(nil t))
      (emitter-fail node "archivedp must be boolean"))
    (when (and inline-task-p (< level 15))
      (emitter-fail node "inlinetask level must be at least 15"))
    (when (and todo
               (not (and (stringp todo)
                         (eq todo-type
                             (cdr (assoc todo *emitter-todo-keywords*
                                         :test #'string=))))))
      (emitter-fail node "TODO keyword is not declared with its parsed state"))
    (when (and (null todo) todo-type)
      (emitter-fail node "TODO type has no representable keyword"))
    (when (and priority
               (not (or (typep priority '(integer 1 64))
                        (typep priority '(integer 65 90)))))
      (emitter-fail node "priority must be an ASCII letter or an integer from 1 to 64"))
    (when (and tags
               (not (and (listp tags)
                         (every (lambda (tag)
                                  (and (stringp tag)
                                       (plusp (length tag))
                                       (every (lambda (character)
                                                (or (alphanumericp character)
                                                    (find character "_@#%")))
                                              tag)))
                                tags))))
      (emitter-fail node "tags contain an invalid tag name"))
    (when (or (position #\Newline title) (position #\Return title))
      (emitter-fail node "headline titles must be a single line"))
    (when (not (eql (not (null (member "ARCHIVE" tags :test #'string=)))
                    (not (null (node-property node :archivedp)))))
      (emitter-fail node "archivedp must agree with the ARCHIVE tag"))
    (unless (string= title (string-trim '(#\Space #\Tab) title))
      (emitter-fail node "headline title cannot have surrounding whitespace"))
    (when (or (emitter-title-metadata-prefix-p title *emitter-todo-keywords*)
              (emitter-title-priority-prefix-p title)
              (emitter-title-comment-prefix-p title)
              (emitter-title-tag-suffix-p title))
      (emitter-fail node "headline title begins or ends with parsed metadata"))
    (let ((header
            (with-output-to-string (stream)
              (write-string (make-string stars :initial-element #\*) stream)
              (write-char #\Space stream)
              (when todo
                (format stream "~A " todo))
              (when priority
                (format stream "[#~A] "
                        (if (<= priority 64)
                            (format nil "~D" priority)
                            (string (code-char priority)))))
              (when commented (write-string "COMMENT " stream))
              (write-string title stream)
              (when tags
                (format stream " :~{~A~^:~}:" tags))
              (terpri stream))))
      (if inline-task-p
          (format nil "~A~A~A END~%"
                  header
                  (emitter-element-sequence (node-children node))
                  (make-string stars :initial-element #\*))
          (concatenate 'string header
                       (emitter-element-sequence (node-children node)))))))

(defun emitter-check-enter-node (node)
  (when (member node *emitter-active-nodes* :test #'eq)
    (emitter-fail node "the node tree contains a cycle"))
  (incf *emitter-node-count*)
  (check-limit *emitter-current-limits*
               :max-node-count
               *emitter-node-count*))

(defun emitter-object-child-string (node)
  (check-type node node)
  (unless (eq (node-type-category (node-type node)) :object)
    (emitter-fail node "expected an inline object"))
  (emitter-check-enter-node node)
  (let ((*emitter-object-depth* (1+ *emitter-object-depth*))
        (*emitter-active-nodes* (cons node *emitter-active-nodes*)))
    (check-limit *emitter-current-limits* :max-object-depth
                 *emitter-object-depth*)
    (emitter-object-string node)))

(defun emitter-render (node)
  (check-type node node)
  (let ((category (node-type-category (node-type node))))
    (if (eq category :object)
        (emitter-object-child-string node)
        (progn
          (unless (member category '(:document :greater-element :lesser-element))
            (emitter-fail node "unknown node category"))
          (emitter-check-enter-node node)
          (let ((*emitter-element-depth* (1+ *emitter-element-depth*))
                (*emitter-active-nodes* (cons node *emitter-active-nodes*)))
            (check-limit *emitter-current-limits* :max-element-depth
                         *emitter-element-depth*)
            (let ((text (emitter-render-element node)))
              (if (eq (node-type node) :org-data)
                  text
                  (emitter-append-post-blank text node))))))))

(defun emitter-latex-environment (node)
  (let* ((value (emitter-normalize-newlines
                 (emitter-required-string node :value)))
         (first-newline (position #\Newline value))
         (first-end (or first-newline (length value)))
         (first-line (subseq value 0 first-end))
         (prefix "\\begin{")
         (environment-end
           (and (>= (length first-line) (length prefix))
                (string-equal prefix first-line :end1 (length prefix))
                (position #\} first-line :start (length prefix))))
         (environment
           (and environment-end
                (> environment-end (length prefix))
                (subseq first-line (length prefix) environment-end))))
    (unless (and environment first-newline
                 (plusp (length value))
                 (char= (char value (1- (length value))) #\Newline))
      (emitter-fail node "LaTeX environment needs a begin line and final newline"))
    (let ((closer (format nil "\\end{~A}" environment))
          (found-close nil))
      (loop with start = (1+ first-newline)
            for newline = (position #\Newline value :start start)
            for end = (or newline (length value))
            for line = (subseq value start end)
            do (when (string-equal (string-trim '(#\Space #\Tab) line) closer)
                 (unless (and newline (= (1+ newline) (length value)))
                   (emitter-fail node "LaTeX close marker must be the final line"))
                 (setf found-close t))
            while (and newline (not (= (1+ newline) (length value))))
            do (setf start (1+ newline)))
      (unless found-close
        (emitter-fail node "LaTeX environment needs its matching close line")))
    value))

(defun emitter-render-element (node)
  (let ((type (node-type node))
        (children (node-children node)))
    (case type
      (:org-data (emitter-element-sequence children))
      (:headline (emitter-headline node nil))
      (:section
       (unless children
         (emitter-fail node "an empty section has no Org representation"))
       (loop for child in children
             for position from 0
             when (or (eq (node-type child) :property-drawer)
                      (and (eq (node-type child) :drawer)
                           (stringp (node-property child :drawer-name))
                           (string-equal (node-property child :drawer-name)
                                         "PROPERTIES")))
               unless (or (eql position 0)
                          (eq (node-type (nth (1- position) children))
                              :planning))
                 do (emitter-fail child
                                  "a property drawer must immediately follow the heading or planning line"))
       (emitter-element-sequence children))
      (:inlinetask (emitter-headline node t))
      (:center-block
       (emitter-raw-block node "#+BEGIN_CENTER" "#+END_CENTER"
                          (emitter-element-sequence children)))
      (:quote-block
       (emitter-raw-block node "#+BEGIN_QUOTE" "#+END_QUOTE"
                          (emitter-element-sequence children)))
      (:special-block
       (let ((name (string-downcase
                    (emitter-required-string node :type)))
             (arguments (node-property node :parameters)))
         (unless (string= name (node-property node :type))
           (emitter-fail node "special block type must be lowercase"))
         (when (member name '("quote" "center" "comment" "example"
                              "export" "src" "verse")
                       :test #'string-equal)
           (emitter-fail node "block name ~A is reserved" name))
         (unless (and (plusp (length name))
                      (not (position-if (lambda (character)
                                          (find character '(#\Space #\Tab
                                                            #\Newline #\Return)))
                                        name)))
           (emitter-fail node "special block name is invalid"))
         (when (and arguments
                    (or (not (stringp arguments)) (zerop (length arguments))))
           (emitter-fail node "special block parameters must be non-empty strings"))
         (when (and arguments
                    (not (string= arguments
                                  (string-trim '(#\Space #\Tab) arguments))))
           (emitter-fail node "special block parameters must be trimmed"))
         (emitter-raw-block node
                            (format nil "#+BEGIN_~A~@[ ~A~]" name arguments)
                            (format nil "#+END_~A" name)
                            (emitter-element-sequence children))))
      (:drawer
       (let ((name (emitter-line-value node
                                       (emitter-required-string node :drawer-name)
                                       "drawer name")))
         (unless (and (string= name (string-upcase name))
                      (plusp (length name))
                      (not (find #\: name))
                      (not (string-equal name "END")))
           (emitter-fail node "drawer name is not representable"))
         (when (and (string= name "PROPERTIES")
                    (node-p (node-parent node))
                    (eq (node-type (node-parent node)) :section)
                    (node-p (node-parent (node-parent node)))
                    (eq (node-type (node-parent (node-parent node))) :headline)
                    (let* ((siblings (node-children (node-parent node)))
                           (position (position node siblings :test #'eq)))
                      (or (eql position 0)
                          (and (eql position 1)
                               (eq (node-type (first siblings)) :planning)))))
           (emitter-fail node "PROPERTIES would parse as a property drawer"))
         (emitter-raw-block node (format nil ":~A:" name) ":END:"
                            (emitter-element-sequence children))))
      (:property-drawer
       (unless (and (node-p (node-parent node))
                    (eq (node-type (node-parent node)) :section)
                    (node-p (node-parent (node-parent node)))
                    (eq (node-type (node-parent (node-parent node)))
                        :headline))
         (emitter-fail node "a property drawer must follow a headline"))
       (unless (every (lambda (child) (eq (node-type child) :node-property))
                      children)
         (emitter-fail node "property drawers can contain only node properties"))
       (format nil ":PROPERTIES:~%~A:END:~%"
               (emitter-element-sequence children)))
      (:dynamic-block
       (let ((name (emitter-required-string node :block-name))
             (arguments (node-property node :arguments)))
         (unless (and (string= name (string-downcase name))
                      (plusp (length name))
                      (not (position-if (lambda (character)
                                          (find character '(#\Space #\Tab
                                                            #\Newline #\Return)))
                                        name)))
           (emitter-fail node "dynamic block name must be lowercase and a single word"))
         (when (and arguments
                    (or (not (stringp arguments)) (zerop (length arguments))))
           (emitter-fail node "dynamic block arguments must be non-empty strings"))
         (when (and arguments
                    (not (string= arguments
                                  (string-trim '(#\Space #\Tab) arguments))))
           (emitter-fail node "dynamic block arguments must be trimmed"))
         (emitter-raw-block node
                            (format nil "#+BEGIN: ~A~@[ ~A~]" name arguments)
                            "#+END:"
                            (emitter-element-sequence children))))
      (:footnote-definition
       (let ((label (emitter-line-value node
                                        (emitter-required-string node :label)
                                        "footnote label")))
         (when (or (zerop (length label)) (position #\] label))
           (emitter-fail node "footnote label is empty or contains a closing bracket"))
         (emitter-ensure-newline
          (format nil "[fn:~A] ~A"
                  label (emitter-element-sequence children)))))
      (:plain-list (emitter-plain-list node 0))
      (:item (emitter-item node 0))
      (:table
       (case (node-property node :type)
         (:org
          (unless (every (lambda (child)
                           (eq (node-type child) :table-row))
                         children)
            (emitter-fail node "Org tables can contain only table rows"))
          (emitter-element-sequence children))
         (:table.el
          (let ((source (node-text node)))
            (unless source
              (emitter-fail node "detached table.el nodes have no row data"))
            (let* ((normalized (emitter-normalize-newlines source))
                   (begin (node-begin node))
                   (affiliated (node-post-affiliated node))
                   (prefix-length
                     (if (and (integerp begin) (integerp affiliated))
                         (- affiliated begin)
                         0)))
              (unless (<= 0 prefix-length (length normalized))
                (emitter-fail node "table.el affiliated span is invalid"))
              (emitter-without-post-blank
               node (subseq normalized prefix-length)))))
         (otherwise
          (emitter-fail node "unknown table type ~S"
                        (node-property node :type)))))
      (:babel-call
       (let ((call (emitter-line-value node
                                       (emitter-required-string node :call)
                                       "Babel call name"))
             (arguments (node-property node :arguments)))
         (when (or (node-property node :inside-header)
                   (node-property node :end-header))
           (emitter-fail node "Babel call headers are not retained by the parser"))
         (unless (and (string= call (string-trim '(#\Space #\Tab) call))
                      (not (find #\( call)) (not (find #\) call)))
           (emitter-fail node "Babel call name cannot be reparsed exactly"))
         (when (and arguments
                    (or (zerop (length arguments))
                        (not (string= arguments
                                      (string-trim '(#\Space #\Tab) arguments)))
                        (find #\( arguments) (find #\) arguments)))
           (emitter-fail node "Babel call arguments cannot be reparsed exactly"))
         (format nil "#+CALL: ~A~@[~A~]~%"
                 call
                 (and arguments
                      (format nil "(~A)" (emitter-line-value node arguments
                                                              "Babel arguments"))))))
      (:clock
       (let ((timestamp (node-property node :value))
             (status (node-property node :status)))
         (unless (node-p timestamp)
           (emitter-fail node "a clock needs its timestamp node"))
         (unless (member status '(:running :closed))
           (emitter-fail node "clock status must be :RUNNING or :CLOSED"))
         (format nil "CLOCK: ~A~@[ => 0:00~]~%"
                 (emitter-render timestamp)
                 (eq status :closed))))
      (:comment
       (format nil "# ~A~%"
               (emitter-line-value node
                                   (emitter-required-string node :value)
                                   "comment value")))
      (:comment-block
       (emitter-raw-block node "#+BEGIN_COMMENT" "#+END_COMMENT"
                          (emitter-required-string node :value)))
      (:diary-sexp
       (let ((value (emitter-line-value node
                                        (emitter-required-string node :value)
                                        "diary expression")))
         (unless (and (>= (length value) 3)
                      (string= "%%(" value :end2 3))
           (emitter-fail node "diary expression must start with %%("))
         (format nil "~A~%" value)))
      (:example-block
       (when (node-property node :switches)
         (emitter-fail node "example switches are not retained by the parser"))
       (emitter-raw-block node "#+BEGIN_EXAMPLE" "#+END_EXAMPLE"
                          (emitter-required-string node :value)))
      (:export-block
       (let ((export-type (emitter-trimmed-line-value
                           node (emitter-required-string node :type)
                           "export type")))
         (unless (string= export-type (string-upcase export-type))
           (emitter-fail node "export type must be uppercase"))
         (emitter-raw-block node
                          (format nil "#+BEGIN_EXPORT~@[ ~A~]"
                                  (and (plusp (length export-type)) export-type))
                          "#+END_EXPORT"
                          (emitter-required-string node :value))))
      (:fixed-width (emitter-fixed-width node))
      (:horizontal-rule (format nil "-----~%"))
      (:keyword
       (let ((key (emitter-line-value node
                                      (emitter-required-string node :key)
                                      "keyword name"))
             (value (emitter-trimmed-line-value
                     node (emitter-required-string node :value)
                     "keyword value")))
         (unless (and (string= key (string-upcase key))
                      (not (find #\: key)))
           (emitter-fail node "keyword name must be uppercase and cannot contain a colon"))
         (when (node-property node :options)
           (emitter-fail node "keyword options are not retained by the parser"))
         (format nil "#+~A: ~A~%" key value)))
      (:latex-environment
       (emitter-latex-environment node))
      (:node-property
       (unless (and (node-p (node-parent node))
                    (eq (node-type (node-parent node)) :property-drawer))
         (emitter-fail node "node properties must be inside a property drawer"))
       (let ((key (emitter-line-value node
                                      (emitter-required-string node :key)
                                      "property name"))
             (value (emitter-trimmed-line-value
                     node (emitter-required-string node :value)
                     "property value")))
         (unless (and (string= key (string-upcase key))
                      (plusp (length key))
                      (not (find #\: key)))
           (emitter-fail node "property name must be uppercase and cannot contain a colon"))
         (format nil ":~A: ~A~%" key value)))
      (:paragraph
       (unless children
         (emitter-fail node "an empty paragraph has no Org representation"))
       (emitter-ensure-newline
        (emitter-join (mapcar #'emitter-render children))))
      (:planning (emitter-planning node))
      (:src-block
       (when (node-property node :parameters)
         (emitter-fail node "src block parameters are not retained"))
       (let ((language (emitter-line-value node
                                           (emitter-required-string node :language)
                                           "source language"))
             (switches (node-property node :switches)))
         (when (and (plusp (length language))
                    (position-if (lambda (character)
                                   (find character '(#\Space #\Tab)))
                                 language))
           (emitter-fail node "source language must be a single word"))
         (when (and switches
                    (or (not (stringp switches))
                        (zerop (length switches))
                        (not (string= switches
                                      (string-trim '(#\Space #\Tab) switches)))))
           (emitter-fail node "source switches must be non-empty and trimmed"))
         (when (and (zerop (length language)) switches)
           (emitter-fail node "source switches require a language"))
         (emitter-raw-block node
                            (format nil "#+BEGIN_SRC~@[ ~A~]~@[ ~A~]"
                                    (and (plusp (length language)) language)
                                    (and switches
                                         (emitter-line-value node switches
                                                             "source switches")))
                            "#+END_SRC"
                            (emitter-required-string node :value))))
      (:table-row
       (unless (and (node-p (node-parent node))
                    (eq (node-type (node-parent node)) :table))
         (emitter-fail node "table rows must be inside a table"))
       (case (node-property node :type)
         (:rule
          (when children
            (emitter-fail node "rule rows cannot contain cells"))
          (format nil "|-----|~%"))
         (:standard
          (unless (every (lambda (child)
                           (eq (node-type child) :table-cell))
                         children)
            (emitter-fail node "standard table rows can contain only cells"))
          (format nil "|~{~A|~}~%"
                  (mapcar #'emitter-render children)))
         (otherwise
          (emitter-fail node "unknown table row type ~S"
                        (node-property node :type)))))
      (:verse-block
       (unless (every (lambda (child)
                        (eq (node-type-category (node-type child)) :object))
                      children)
         (emitter-fail node "verse blocks can contain only inline objects"))
       (emitter-raw-block node "#+BEGIN_VERSE" "#+END_VERSE"
                          (emitter-join (mapcar #'emitter-render children))))
      (otherwise
       (emitter-fail node "no element serializer is defined")))))

(defun emitter-fixed-width (node)
  (let ((value (emitter-normalize-newlines
                (emitter-required-string node :value))))
    (with-output-to-string (stream)
      (loop with start = 0
            for newline = (position #\Newline value :start start)
            for end = (or newline (length value))
            do (format stream ": ~A~%" (subseq value start end))
            while newline
            do (setf start (1+ newline))))))

(defun emitter-planning (node)
  (let* ((section (node-parent node))
         (headline (and (node-p section) (node-parent section)))
         (siblings (and (node-p section) (node-children section))))
    (unless (and (node-p section)
                 (eq (node-type section) :section)
                 (node-p headline)
                 (eq (node-type headline) :headline)
                 (eql (position node siblings :test #'eq) 0))
      (emitter-fail node "planning must be first in a headline section"))
    (let ((fields '()))
      (dolist (entry '((:scheduled "SCHEDULED:")
                       (:deadline "DEADLINE:")
                       (:closed "CLOSED:")))
        (destructuring-bind (key marker) entry
          (let ((timestamp (node-property node key)))
            (when timestamp
              (unless (and (node-p timestamp)
                           (eq (node-type timestamp) :timestamp))
                (emitter-fail node "~S must be a timestamp node" key))
              (push (cons marker (emitter-render timestamp)) fields)))))
      (unless fields
        (emitter-fail node "empty planning has no Org representation"))
      (let ((fields (nreverse fields)))
        (with-output-to-string (stream)
          (loop for (marker . timestamp) in fields
                for position from 0
                do (when (plusp position)
                     (write-char #\Space stream))
                   (format stream "~A " marker)
                   (write-string timestamp stream))
          (terpri stream))))))

(defun emitter-plain-list (node indentation)
  (let ((items (node-children node))
        (list-type (node-property node :type)))
    (unless (member list-type '(:unordered :ordered :descriptive))
      (emitter-fail node "unknown plain-list type ~S" list-type))
    (unless (every (lambda (child) (eq (node-type child) :item)) items)
      (emitter-fail node "plain lists can contain only items"))
    (unless items
      (emitter-fail node "an empty plain list has no Org representation"))
    (when (and items
               (not (eql (eq list-type :descriptive)
                         (not (null (node-property (first items) :tag))))))
      (emitter-fail node "the first item tag must agree with the list type"))
    (emitter-join
     (mapcar (lambda (item) (emitter-item item indentation)) items))))

(defun emitter-valid-list-bullet-p (bullet list-type)
  (and (stringp bullet)
       (if (eq list-type :ordered)
           (let ((end (position-if-not #'digit-char-p bullet)))
             (and end
                  (<= (+ end 2) (length bullet))
                  (find (char bullet end) '(#\. #\)))
                  (find (char bullet (1+ end)) '(#\Space #\Tab))
                  (= (+ end 2) (length bullet))))
           (and (= (length bullet) 2)
                (find (char bullet 0) '(#\- #\+ #\*))
                (find (char bullet 1) '(#\Space #\Tab))))))

(defun emitter-item-marker (node list-type)
  (let* ((bullet (node-property node :bullet))
         (checkbox (node-property node :checkbox))
         (counter (node-property node :counter))
         (tag (node-property node :tag)))
    (unless (member checkbox '(:off :on :trans nil))
      (emitter-fail node "unknown checkbox state ~S" checkbox))
    (when (and counter (not (typep counter '(integer 1))))
      (emitter-fail node "item counter must be a positive integer"))
    (when (and tag (not (listp tag)))
      (emitter-fail node "item tag must be a list of inline nodes"))
    (unless (emitter-valid-list-bullet-p bullet list-type)
      (emitter-fail node "item bullet is not valid for its list type"))
    (when (and tag
               (not (every (lambda (child)
                             (and (node-p child)
                                  (eq (node-type-category (node-type child))
                                      :object)))
                           tag)))
      (emitter-fail node "item tags can contain only inline objects"))
    (with-output-to-string (stream)
      (write-string bullet stream)
      (when checkbox
        (format stream "[~A] "
                (case checkbox
                  (:off #\Space)
                  (:on #\X)
                  (:trans #\-))))
      (when counter
        (format stream "[@~D] " counter))
      (when tag
        (let ((tag-text (emitter-join (mapcar #'emitter-render tag))))
          (when (or (position #\Newline tag-text)
                    (position #\Return tag-text)
                    (search "::" tag-text)
                    (not (string= tag-text
                                  (string-trim '(#\Space #\Tab) tag-text))))
            (emitter-fail node "item tag cannot be reparsed exactly"))
          (format stream "~A :: " tag-text)))
      (get-output-stream-string stream))))

(defun emitter-item-paragraph-string (node indentation first-paragraph-p)
  (let ((text (emitter-render node)))
    (loop with start = 0
          with line-number = 0
          for newline = (position #\Newline text :start start)
          for end = (or newline (length text))
          for line = (subseq text start end)
          for indentation-width = (reader-leading-space-count line)
          unless (every (lambda (character)
                          (find character '(#\Space #\Tab)))
                        line)
            do (if (and first-paragraph-p (zerop line-number))
                   (when (plusp indentation-width)
                     (emitter-fail node
                                   "the first list paragraph cannot begin with whitespace"))
                   (unless (> indentation-width indentation)
                     (emitter-fail node
                                   "list continuation lines must be indented beyond the bullet")))
          while newline
          do (incf line-number)
             (setf start (1+ newline)))
    text))

(defun emitter-item (node indentation)
  (unless (and (node-p (node-parent node))
               (eq (node-type (node-parent node)) :plain-list))
    (emitter-fail node "items must be inside a plain list"))
  (let* ((list-type (node-property (node-parent node) :type))
         (marker (emitter-item-marker node list-type))
         (children (node-children node))
         (line-prefix (make-string indentation :initial-element #\Space)))
    (when (and (zerop indentation) (string= marker "* "))
      (emitter-fail node "an unindented * bullet would parse as a headline"))
    (when (some (lambda (child)
                  (plusp (length (emitter-format-affiliations child))))
                children)
      (emitter-fail node "affiliated keywords inside list items are not representable"))
    (unless (every (lambda (child)
                     (or (eq (node-type child) :paragraph)
                         (eq (node-type child) :plain-list)))
                   children)
      (emitter-fail node "list item children must be paragraphs or nested lists"))
    (when (> (count :plain-list children :key #'node-type) 1)
      (emitter-fail node "adjacent nested lists have no separate Org representation"))
    (with-output-to-string (stream)
      (write-string line-prefix stream)
      (write-string marker stream)
      (if (and children (eq (node-type (first children)) :plain-list))
          (terpri stream)
          (when (null children) (terpri stream)))
      (loop for child in children
            for position from 0
            do (case (node-type child)
                 (:paragraph
                  (when (and (plusp position)
                             (eq (node-type (nth (1- position) children))
                                 :paragraph)
                             (zerop (node-post-blank
                                     (nth (1- position) children))))
                    (emitter-fail child
                                  "separate list paragraphs need a blank-line boundary"))
                  (write-string
                   (emitter-item-paragraph-string
                    child indentation (zerop position))
                   stream))
                 (:plain-list
                  (write-string
                   (emitter-plain-list child (+ indentation 2))
                   stream)))))))

(defun emitter-radio-target-matches-text-p (target text)
  (loop with position = 0
        for start = (search target text :start2 position :test #'char-equal)
        while start
        for end = (+ start (length target))
        when (and (or (zerop start)
                      (let ((previous (char text (1- start))))
                        (not (or (alphanumericp previous)
                                 (char= previous #\_)))))
                  (or (= end (length text))
                      (let ((next (char text end)))
                        (not (or (alphanumericp next)
                                 (char= next #\_))))))
          return t
        do (setf position (max (1+ start) end))
        finally (return nil)))

(defun emitter-radio-target-would-link-p (root target)
  (labels ((visit-value (value active)
             (typecase value
               (node (visit-node value active))
               (cons (some (lambda (item) (visit-value item active)) value))))
           (visit-node (node active)
             (when (member node active :test #'eq)
               (emitter-fail node "the node tree contains a cycle"))
             (let* ((next-active (cons node active))
                    (type (node-type node))
                    (parent (node-parent node))
                    (container-type (if (node-p parent)
                                        (node-type parent)
                                        :paragraph)))
               (or (and (eq type :plain-text)
                        (%object-type-allowed-p :link container-type)
                        (emitter-radio-target-matches-text-p
                         target
                         (or (node-property node :value)
                             (node-text node)
                             "")))
                   (and (not (eq type :radio-target))
                        (or (some (lambda (child)
                                    (visit-node child next-active))
                                  (node-children node))
                            (some (lambda (key)
                                    (visit-value (node-property node key)
                                                 next-active))
                                  '(:title :tag))))))))
    (visit-node root nil)))

(defun emit-org (node &key (limits (make-limits)))
  "Return canonical Org text for NODE without performing stream or encoding I/O.
Signal ORG-KIT-ERROR when the tree has no lossless Org representation and
ORG-RESOURCE-LIMIT-ERROR when the serialized text exceeds MAX-OUTPUT-LENGTH."
  (check-type node node)
  (check-type limits limits)
  (let* ((*emitter-active-nodes* nil)
         (*emitter-node-count* 0)
         (*emitter-radio-targets* nil)
         (*emitter-element-depth* 0)
         (*emitter-object-depth* 0)
         (*emitter-todo-keywords* '(("TODO" . :todo) ("DONE" . :done)))
         (*emitter-todo-setting-seen* nil)
         (*emitter-odd-levels* nil)
         (*emitter-current-limits* limits)
         (root node))
    (labels ((scan (current active seen settings-suppressed)
               (when (member current active :test #'eq)
                 (emitter-fail current "the node tree contains a cycle"))
               (unless (member current seen :test #'eq)
                 (unless settings-suppressed
                   (emitter-track-settings current))
                 (when (eq (node-type current) :radio-target)
                   (let ((value (node-property current :value)))
                     (when (stringp value)
                       (pushnew value *emitter-radio-targets* :test #'string=))))
                 (let ((child-settings-suppressed
                         (or settings-suppressed
                             (member (node-type current)
                                     '(:center-block :quote-block :special-block
                                       :comment-block :example-block :export-block
                                       :src-block :verse-block)))))
                 (dolist (child (node-children current))
                   (scan child (cons current active) (cons current seen)
                         child-settings-suppressed))
                 (dolist (key '(:title :tag))
                   (let ((value (node-property current key)))
                     (when (listp value)
                       (dolist (child value)
                         (when (node-p child)
                           (scan child (cons current active)
                                 (cons current seen)
                                 child-settings-suppressed))))))))))
      (scan root nil nil nil))
    (let ((text (emitter-render node)))
      (dolist (target *emitter-radio-targets*)
        (when (emitter-radio-target-would-link-p root target)
          (emitter-fail node
                        "radio target ~S would change matching plain text"
                        target)))
      (check-limit limits :max-output-length (length text))
      text)))

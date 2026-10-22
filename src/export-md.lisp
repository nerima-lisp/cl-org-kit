;;;; src/export-md.lisp
(in-package #:org-kit)

(defstruct (export-md-state (:constructor %make-export-md-state))
  preparation
  limits
  gfm-state
  html-state)

(defun export-md-make-state (context)
  (let* ((preparation
           (org-kit::%export-render-context-preparation context))
         (gfm-state (export-gfm-state-for-preparation preparation))
         (html-state (export-md-make-html-state context)))
    (%make-export-md-state
     :preparation preparation
     :limits (export-render-context-limits context)
     :gfm-state gfm-state
     :html-state html-state)))

(defun export-md-node-value (node)
  (or (node-property node :value) (node-text node) ""))

(defun export-md-source-text (node)
  (let* ((document (node-document node))
         (source (and document (document-text document)))
         (start (node-begin node))
         (end (node-end node)))
    (if (and source start end)
        (subseq source start end)
        (export-md-node-value node))))

(progn
  (defun export-md-plain-text-output-length (text preserve-breaks-p)
  (loop with output-length = 0
        for index from 0 below (length text)
        for character = (char text index)
        for line-start-p = (or (zerop index)
                               (char= (char text (1- index)) #\Newline))
        do (incf output-length)
           (when (or (member character
                              (list (code-char 96) #\* #\_ #\\)
                              :test #'char=)
                     (and line-start-p (char= character #\#))
                     (and (char= character #\!)
                          (< (1+ index) (length text))
                          (char= (char text (1+ index)) #\[)))
             (incf output-length))
           (when (and preserve-breaks-p
                      (char= character #\Newline))
             (incf output-length 2))
        finally (return output-length)))
  (defun export-md-escape-plain-text (text &optional preserve-breaks-p)
  (export-gfm-check-total-output-length
   (export-md-plain-text-output-length text preserve-breaks-p))
  (with-output-to-string (output)
    (loop for index from 0 below (length text)
          for character = (char text index)
          for line-start-p = (or (zerop index)
                                 (char= (char text (1- index)) #\Newline))
          do (when (or (member character
                                (list (code-char 96) #\* #\_ #\\)
                                :test #'char=)
                       (and line-start-p (char= character #\#))
                       (and (char= character #\!)
                            (< (1+ index) (length text))
                            (char= (char text (1+ index)) #\[)))
               (write-char #\\ output))
             (when (and preserve-breaks-p (char= character #\Newline))
               (write-string "  " output))
             (write-char character output)))))

(progn
  (progn
  (defun export-md-view-post-blank (view)
  (let* ((node (export-view-node view))
         (type (node-type node))
         (parent (node-parent node))
         (siblings (and parent (node-children parent)))
         (position (and siblings
                        (position node siblings :test #'eq)))
         (next (and position (nth (1+ position) siblings))))
    (cond
      ((member type '(:item :table-row) :test #'eq)
       (node-post-blank node))
      ((and (eq type :paragraph)
            (eq (and parent (node-type parent)) :item)
            (eql position 0)
            (eq (and next (node-type next)) :plain-list)
            (null (nthcdr (+ position 2) siblings)))
       0)
      (t 1))))
  (defun export-md-view-terminal-hard-break-p (view)
  (let* ((node (export-view-node view))
         (type (node-type node))
         (children (export-view-children view)))
    (if (eq type :paragraph)
        (and children
             (eq (node-type (export-view-node (car (last children))))
                 :line-break))
        (if (member type
                    (quote (:org-data :section :headline :drawer
                      :dynamic-block :plain-list :item :quote-block))
                    :test (function eq))
            (let ((last-child
                    (find-if-not
                     (lambda (child)
                       (eq (node-type (export-view-node child))
                           :footnote-definition))
                     children :from-end t)))
              (and last-child
                   (export-md-view-terminal-hard-break-p last-child)))
            nil))))
  (defun export-md-trim-structural-newlines (view text)
  (let ((keep (if (export-md-view-terminal-hard-break-p view) 1 0))
        (end (length text)))
    (loop while (and (> end keep)
                     (char= (char text (1- end)) #\Newline))
          do (decf end))
    (if (= end (length text))
        text
        (subseq text 0 end)))))
  (defun export-md-render-views
    (state views &key preserve-source-breaks-p preserve-leading-post-blank-p
                      (line-prefix-width 0) (newline-suffix-width 0))
  (with-output-to-string (output)
    (let ((previous-view nil)
          (first-p t)
          (output-length 0)
          (previous-newline-count 0))
      (dolist (view views)
        (let* ((rendered
                 (export-md-trim-structural-newlines
                  view
                  (export-gfm-render-block-view
                   (export-md-state-gfm-state state) view
                   :preserve-source-breaks-p preserve-source-breaks-p)))
               (separator-length
                 (if previous-view
                     (if first-p
                         (if preserve-leading-post-blank-p
                             (export-md-view-post-blank previous-view)
                             0)
                         (+ 1 (export-md-view-post-blank previous-view)))
                     0))
               (*export-gfm-output-prefix-length*
                 (+ *export-gfm-output-prefix-length*
                    output-length separator-length
                    (* line-prefix-width (1+ previous-newline-count))
                    (* newline-suffix-width previous-newline-count))))
          (if (zerop (length rendered))
              (when (and first-p preserve-leading-post-blank-p)
                (setf previous-view view))
              (progn
                (export-gfm-check-total-output-length
                 (+ separator-length (length rendered)))
                (loop repeat separator-length
                      do (write-char #\Newline output))
                (write-string rendered output)
                (incf output-length (+ separator-length (length rendered)))
                (incf previous-newline-count
                      (+ separator-length (count #\Newline rendered)))
                (setf previous-view view
                      first-p nil))))))))
  (defun export-md-html-index-attribute-ids (state)
    (let ((stack (reverse
                  (export-preparation-root-views
                   (export-html-state-preparation state)))))
      (loop while stack
            for view = (pop stack)
            for node = (export-view-node view)
            do (when (node-property node :ATTR_HTML)
                 (unless (member (node-type node)
                                 '(:link :paragraph :table :special-block)
                                 :test #'eq)
                   (export-html-attribute-error
                    node "ATTR_HTML is supported only on HTML links, images, and special blocks"))
                 (dolist (attribute (export-html-parse-attributes node state))
                   (when (string= (car attribute) "id")
                     (setf (gethash (cdr attribute)
                                    (export-html-state-used-references state))
                           t))))
               (dolist (child (reverse (export-view-children view)))
                 (push child stack))))
    state)
  (defun export-md-make-html-state (context)
    (let* ((preparation (org-kit::%export-render-context-preparation context))
           (state (%make-export-html-state
                   :preparation preparation
                   :limits (export-render-context-limits context))))
      (export-md-html-index-attribute-ids state)
      (export-html-validate-attribute-nodes state)
      (export-html-index-preparation state)
      (export-html-index-footnote-ids state)
      state))
  (defun export-md-capture-html-output (state thunk)
    (let* ((html-state (export-md-state-html-state state))
           (old-output (export-html-state-output html-state))
           (old-offset (export-html-state-output-offset html-state))
           (output (make-array 0 :element-type 'character
                               :adjustable t :fill-pointer 0)))
      (unwind-protect
           (progn
             (setf (export-html-state-output html-state) output
                   (export-html-state-output-offset html-state)
                   *export-current-output-offset*)
             (funcall thunk html-state)
             (coerce (export-html-state-output html-state) 'string))
        (setf (export-html-state-output html-state) old-output
              (export-html-state-output-offset html-state) old-offset))))
  (defun export-md-render-special-block (state node view)
  (let* ((html-state (export-md-state-html-state state))
         (attributes (and (node-property node :ATTR_HTML)
                          (export-html-parse-attributes node html-state)))
         (id (cdr (assoc "id" attributes :test #'string=)))
         (existing-class (cdr (assoc "class" attributes :test #'string=)))
         (base-class (let ((type (node-property node :type)))
                       (if (stringp type)
                           (string-downcase type)
                           "special-block")))
         (class-length (+ (length base-class)
                          (if existing-class (1+ (length existing-class)) 0)))
         (class (progn
                  (export-gfm-check-total-output-length class-length)
                  (if existing-class
                      (concatenate 'string existing-class " " base-class)
                      base-class))))
    (export-md-capture-html-output
     state
     (lambda (html-state)
       (export-html-write html-state "<div")
       (dolist (attribute attributes)
         (unless (string= (car attribute) "class")
           (export-html-write-attribute html-state
                                        (car attribute) (cdr attribute))))
       (export-html-write-attribute html-state "class" class)
       (unless id
         (export-html-write-attribute
          html-state "id" (export-html-reference html-state node)))
       (export-html-write html-state (format nil ">~%"))
       (dolist (child (export-view-children view))
         (export-html-render-view html-state child))
       (export-html-write html-state (format nil "~%</div>"))))))
  (defun export-md-render-inlinetask (state node view)
  (export-md-capture-html-output
   state
   (lambda (html-state)
     (export-html-write html-state "<div class=\"inlinetask\">")
     (export-html-write html-state (string #\Newline))
     (export-html-write html-state "<b>")
     (export-html-heading-title html-state node view)
     (export-html-write html-state "</b><br />")
     (export-html-write html-state (string #\Newline))
     (dolist (child (export-view-children view))
       (export-html-render-view html-state child))
     (export-html-write html-state "</div>"))))
  (progn
  (defun export-md-strip-link-destination-angles (text destination)
  (let* ((destination-length (length destination))
         (text-length (length text))
         (destination-start (- text-length (+ destination-length 3))))
    (if (and (>= destination-start 0)
             (> destination-length 2)
             (char= (char destination 0) #\<)
             (char= (char destination (1- destination-length)) #\>)
             (string= text "](" :start1 destination-start
                               :end1 (+ destination-start 2))
             (string= text destination
                      :start1 (+ destination-start 2)
                      :end1 (+ destination-start 2 destination-length))
             (char= (char text (1- text-length)) #\)))
        (concatenate (quote string)
                     (subseq text 0 (+ destination-start 2))
                     (subseq destination 1 (1- destination-length))
                     ")")
        text)))
  (defun export-md-adjust-table-columns (text)
  (let ((old "<col class=")
        (new "<col  class=")
        (count 0)
        (start 0))
    (loop for position = (search old text :start2 start)
          while position
          do (incf count)
             (setf start (+ position (length old))))
    (export-gfm-check-total-output-length (+ (length text) count))
    (with-output-to-string (output)
      (setf start 0)
      (loop for position = (search old text :start2 start)
            while position
            do (write-string text output :start start :end position)
               (write-string new output)
               (setf start (+ position (length old)))
            finally (write-string text output :start start)))))))
(defun export-md-indent-lines (text width)
  (let ((line-start-p t)
        (nonempty-line-count 0))
    (loop for index from 0 below (length text)
          for character = (char text index)
          do (when (and line-start-p (not (char= character #\Newline)))
               (incf nonempty-line-count)
               (setf line-start-p nil))
             (when (char= character #\Newline)
               (setf line-start-p t)))
    (export-gfm-check-total-output-length
     (+ (length text) (* width nonempty-line-count)))
    (let ((indent (make-string width :initial-element #\Space)))
      (with-output-to-string (output)
        (loop with line-start-p = t
              for index from 0 below (length text)
              for character = (char text index)
              do (when (and line-start-p (not (char= character #\Newline)))
                   (write-string indent output)
                   (setf line-start-p nil))
                 (write-char character output)
                 (when (char= character #\Newline)
                   (setf line-start-p t)))))))

(progn
  (defun export-md-render-inline-views
      (state views context &key table-cell-p preserve-source-breaks-p)
    (declare (ignore state))
    (with-output-to-string (output)
      (let ((output-length 0))
        (dolist (view views)
          (let* ((*export-gfm-output-prefix-length*
                   (+ *export-gfm-output-prefix-length* output-length))
                 (*export-current-output-offset*
                   (+ *export-current-output-offset* output-length))
                 (rendered
                   (export-render-view
                    context view :mode :inline
                    :table-cell-p table-cell-p
                    :preserve-source-breaks-p preserve-source-breaks-p)))
            (export-gfm-check-total-output-length (length rendered))
            (write-string rendered output)
            (incf output-length (length rendered)))))))
  (defun export-md-inline-contents (state view frame context)
    (export-md-render-inline-views
     state (export-view-children view) context
     :table-cell-p (export-render-frame-table-cell-p frame)
     :preserve-source-breaks-p
     (export-render-frame-preserve-source-breaks-p frame))))

(defun export-md-render-html-fallback (state node view)
  (let* ((html-state (export-md-state-html-state state))
         (old-output (export-html-state-output html-state))
         (old-offset (export-html-state-output-offset html-state))
         (output (make-array 0 :element-type 'character
                             :adjustable t :fill-pointer 0)))
    (unwind-protect
         (progn
           (setf (export-html-state-output html-state) output
                 (export-html-state-output-offset html-state)
                 *export-current-output-offset*)
           (if view
               (export-html-render-view html-state view)
               (export-html-render-node-legacy html-state node nil nil))
           (coerce (export-html-state-output html-state) 'string))
      (setf (export-html-state-output html-state) old-output
            (export-html-state-output-offset html-state) old-offset))))

(defun export-md-render-code-span (node)
  (multiple-value-bind (text start end)
      (export-gfm-inline-code-value-range node)
    (let* ((value-length (- end start))
           (tick (code-char 96))
           (contains-tick (find tick text :start start :end end))
           (ticks (if contains-tick
                      (make-string 2 :initial-element tick)
                      (string tick)))
           (pad-p
             (and contains-tick
                  (plusp value-length)
                  (or (char= (char text start) tick)
                      (char= (char text (1- end)) tick)))))
      (export-gfm-check-total-output-length
       (+ value-length (* 2 (length ticks)) (if pad-p 2 0)))
      (let ((value (subseq text start end)))
        (if pad-p
            (concatenate 'string ticks " " value " " ticks)
            (format nil "~A~A~A" ticks value ticks))))))

(progn
  (defun export-md-render-list-tag (state node context) (declare (ignore state)) (let* ((segments (node-property node :tag))
         (range (node-property node :reader-tag-range))
         (source (and range (document-text (node-document node)))))
    (cond
      ((and segments (listp segments))
       (let ((base-offset *export-current-output-offset*)
             (base-prefix *export-gfm-output-prefix-length*)
             (output-length 0))
         (with-output-to-string (output)
           (dolist (segment segments)
             (let* ((*export-current-output-offset*
                      (+ base-offset output-length))
                    (*export-gfm-output-prefix-length*
                      (+ base-prefix output-length))
                    (part
                      (if (node-p segment)
                          (export-render-view
                           context (export-md-title-object-view segment)
                           :mode :inline)
                          (export-md-escape-plain-text segment))))
               (export-gfm-check-total-output-length (length part))
               (write-string part output)
               (incf output-length (length part)))))))
      ((and range source)
       (let ((length (- (cdr range) (car range))))
         (export-gfm-check-total-output-length length)
         (subseq source (car range) (cdr range))))
      (t nil)))))

(progn
(progn
(defun export-md-list-marker-output-length (marker)
  (typecase marker
    (string
     (length (string-right-trim (list #\Space #\Tab) marker)))
    (character 1)
    (integer
     (loop with value = (abs marker)
           with digits = 1
           while (>= value 10)
           do (setf value (floor value 10))
              (incf digits)
           finally (return (+ digits (if (minusp marker) 1 0) 1))))
    (t (length (princ-to-string marker)))))

(defun export-md-list-marker-text (marker)
  (if (integerp marker)
      (format nil "~A." marker)
      (typecase marker
        (string (string-right-trim (list #\Space #\Tab) marker))
        (character (string marker))
        (t (princ-to-string marker)))))

(defun export-md-render-list-item (state view context)
  (let* ((node (export-view-node view))
         (children (export-view-children view))
         (paragraph (find-if
                     (lambda (child)
                       (eq (node-type (export-view-node child)) :paragraph))
                     children))
         (first-child (or paragraph (first children)))
         (remaining (if paragraph
                        (remove paragraph children :test #'eq)
                        (rest children)))
         (marker (or *export-gfm-list-marker*
                     (node-property node :bullet) "-"))
         (checkbox (node-property node :checkbox))
         (checkbox-text (case checkbox
                          (:off "[ ] ")
                          (:on "[X] ")
                          (:trans "[-] ")))
         (tag (export-md-render-list-tag state node context))
         (prefix-length (+ (export-md-list-marker-output-length marker) 3))
         (checkbox-length (length (or checkbox-text "")))
         (tag-length (if (and tag (plusp (length tag)))
                         (+ 6 (length tag))
                         0))
         (first-prefix-length (+ prefix-length checkbox-length tag-length)))
    (export-gfm-check-total-output-length first-prefix-length)
    (let* ((prefix
             (format nil "~A   " (export-md-list-marker-text marker)))
           (tag-prefix
             (when (and tag (plusp (length tag)))
               (format nil "**~A:** " tag)))
           (first-text
             (export-gfm-render-with-prefix
              first-prefix-length
              (lambda ()
                (if first-child
                    (export-render-view context first-child :mode :block)
                    ""))))
           (base-prefix *export-gfm-output-prefix-length*))
      (with-output-to-string (output)
        (let ((output-length (+ first-prefix-length (length first-text))))
          (export-gfm-check-total-output-length output-length)
          (write-string prefix output)
          (when checkbox-text (write-string checkbox-text output))
          (when tag-prefix (write-string tag-prefix output))
          (write-string first-text output)
          (dolist (child remaining)
            (let* ((*export-gfm-output-prefix-length*
                     (+ base-prefix output-length 1))
                   (rendered
                     (export-render-view context child :mode :block))
                   (indented (export-md-indent-lines rendered 4)))
              (unless (zerop (length indented))
                (export-gfm-check-total-output-length
                 (+ output-length 1 (length indented)))
                (write-char #\Newline output)
                (write-string indented output)
                (incf output-length (+ 1 (length indented))))))))))))
(defun export-md-render-list (state view context)
  (declare (ignore state))
  (let* ((node (export-view-node view))
         (numbered-p (eq (node-property node :type) :ordered))
         (items (export-view-children view)))
    (with-output-to-string (output)
      (let ((output-length 0)
            (first-p t)
            (previous-item nil))
        (loop for item in items
              for number from 1
              for item-node = (export-view-node item)
              for marker = (or (node-property item-node :counter)
                               (let* ((bullet (node-property item-node :bullet))
                                      (digits-end (and (stringp bullet)
                                                       (position-if-not (function digit-char-p) bullet))))
                                 (when (and digits-end
                                            (plusp digits-end)
                                            (< digits-end (length bullet))
                                            (find (char bullet digits-end) ".)"))
                                   (parse-integer bullet :end digits-end)))
                               (and numbered-p number)
                               "-")
              for separator-length =
                (if first-p
                    0
                    (+ 1 (node-post-blank (export-view-node previous-item))))
              do (export-gfm-check-total-output-length
                  (+ output-length separator-length
                     (export-md-list-marker-output-length marker) 3))
                 (let* ((*export-gfm-output-prefix-length*
                          (+ *export-gfm-output-prefix-length*
                             output-length separator-length))
                        (*export-gfm-list-marker* marker)
                        (*export-gfm-list-depth* 0)
                        (rendered
                          (export-md-trim-structural-newlines
                           item
                           (export-render-view context item :mode :block))))
                   (unless (zerop (length rendered))
                     (export-gfm-check-total-output-length
                      (+ output-length separator-length (length rendered)))
                     (loop repeat separator-length
                           do (write-char #\Newline output))
                     (write-string rendered output)
                     (incf output-length
                           (+ separator-length (length rendered)))
                     (setf first-p nil
                           previous-item item)))))))))

(defun export-md-render-footnote-reference (state view)
  (export-gfm-render-footnote-reference
   (export-md-state-gfm-state state) view))

(progn
  (progn
  (defun export-md-title-object-view (node)
    (make-export-view
     node
     :children (mapcar #'export-md-title-object-view (node-children node))
     :source-document (node-document node)))
  (defun export-md-render-heading-title (state node view context)
  (let* ((segments (node-property node :title))
         (title-macros (getf (export-view-payload view) :title-macros)))
    (if segments
        (let ((base-offset *export-current-output-offset*)
              (base-prefix *export-gfm-output-prefix-length*)
              (output-length 0))
          (with-output-to-string (output)
            (dolist (segment segments)
              (let* ((*export-current-output-offset*
                       (+ base-offset output-length))
                     (*export-gfm-output-prefix-length*
                       (+ base-prefix output-length))
                     (macro-entry
                       (and (node-p segment)
                            (assoc segment title-macros :test (function eq))))
                     (part
                       (cond
                         ((not (node-p segment))
                          (export-md-escape-plain-text segment))
                         (macro-entry
                          (export-gfm-render-inline-views
                           (export-md-state-gfm-state state)
                           (cdr macro-entry)))
                         (t (export-render-view
                             context (export-md-title-object-view segment)
                             :mode :inline))))
                     (manual-post-blank
                       (if macro-entry (node-post-blank segment) 0))
                     (part-length (+ (length part) manual-post-blank)))
                (export-gfm-check-total-output-length part-length)
                (write-string part output)
                (loop repeat manual-post-blank
                      do (write-char #\Space output))
                (incf output-length part-length)))))
        (export-md-escape-plain-text
         (or (node-property node :raw-value) ""))))))
  (defun export-md-indent-headline-content (text)
    (if (zerop (length text))
        ""
        (let* ((ends-with-newline-p
                 (char= (char text (1- (length text))) #\Newline))
               (line-count (- (1+ (count #\Newline text))
                              (if ends-with-newline-p 1 0)))
               (indent (make-string 4 :initial-element #\Space)))
          (export-gfm-check-total-output-length
           (+ (length text) (* 4 line-count)))
          (with-output-to-string (output)
            (loop with line-start-p = t
                  for character across text
                  do (when line-start-p
                       (write-string indent output)
                       (setf line-start-p nil))
                     (write-char character output)
                     (when (char= character #\Newline)
                       (setf line-start-p t)))))))
  (defun export-md-render-heading (state view context)
  (let* ((node (export-view-node view))
         (html-state (export-md-state-html-state state))
         (level (max 1 (or (node-property node :level) 1)))
         (deep-p (> level 6))
         (todo (and (export-html-option-enabled-p html-state :todo)
                    (node-property node :todo-keyword)))
         (priority-value (node-property node :priority))
         (priority-enabled-p
           (and priority-value
                (export-html-option-enabled-p html-state :priority nil)))
         (priority
           (when priority-enabled-p
             (if (and (integerp priority-value)
                      (<= (char-code #\A) priority-value
                          (char-code #\Z)))
                 (string (code-char priority-value))
                 (princ-to-string priority-value))))
         (tags (and (export-html-option-enabled-p html-state :tags)
                    (node-property node :tags)))
         (title (export-md-render-heading-title state node view context))
         (anchor-id
           (unless deep-p
             (export-md-heading-reference state node)))
         (anchored-p
           (and anchor-id
                (gethash node
                         (export-gfm-state-anchored-headings
                          (export-md-state-gfm-state state)))))
         (number
           (and deep-p
                (gethash node
                         (export-html-state-heading-numbers html-state))))
         (marker (if deep-p
                     (or number "-")
                     (make-string (min 6 level) :initial-element #\#)))
         (marker-space-count
           (if deep-p (max 1 (- 4 (length marker))) 1))
         (tag-length
           (if tags
               (+ 7
                  (reduce #'+ tags :key #'length :initial-value 0)
                  (max 0 (1- (length tags))))
               0))
         (todo-length (if todo (1+ (length todo)) 0))
         (priority-length (if priority (+ 4 (length priority)) 0))
         (anchor-length
           (if anchored-p
               (+ 15 (export-gfm-html-attribute-output-length anchor-id))
               0))
         (leading-newline-count 1)
         (separator-length 2)
         (heading-length
           (+ anchor-length (length marker) marker-space-count
              todo-length priority-length (length title) tag-length))
         (heading
           (progn
             (export-gfm-check-total-output-length heading-length)
             (with-output-to-string (output)
               (when anchored-p
                 (format output "<a id=\"~A\"></a>~%~%"
                         (export-gfm-escape-html-attribute anchor-id)))
               (write-string marker output)
               (loop repeat marker-space-count
                     do (write-char #\Space output))
               (when todo
                 (write-string todo output)
                 (write-char #\Space output))
               (when priority
                 (write-string "[#" output)
                 (write-string priority output)
                 (write-string "] " output))
               (write-string title output)
               (when tags
                 (write-string "     :" output)
                 (loop for tag in tags
                       for first-p = t then nil
                       do (unless first-p (write-char #\: output))
                          (write-string tag output))
                 (write-char #\: output)))))
         (children
           (export-gfm-render-with-prefix
            (+ 4 (length heading))
            (lambda ()
              (let ((text (export-md-render-views
                           state (export-view-children view))))
                (if deep-p
                    (export-md-indent-headline-content text)
                    text)))))
         (result-length
           (+ leading-newline-count (length heading) separator-length
              (length children))))
    (export-gfm-check-total-output-length result-length)
    (with-output-to-string (output)
      (write-char #\Newline output)
      (write-string heading output)
      (write-char #\Newline output)
      (write-char #\Newline output)
      (write-string children output)))))

(progn
  (defun export-md-normalize-link-destination (destination)
    (let* ((start (if (and (> (length destination) 1)
                           (char= (char destination 0) #\<))
                      1 0))
           (end (if (and (> (length destination) start)
                         (char= (char destination (1- (length destination)))
                                #\>))
                    (1- (length destination))
                    (length destination)))
           (output-length
             (loop with index = start
                   with length = 0
                   while (< index end)
                   do (if (and (<= (+ index 5) end)
                               (string= destination "&amp;"
                                        :start1 index
                                        :end1 (+ index 5)))
                          (progn (incf length) (incf index 5))
                          (progn (incf length) (incf index)))
                   finally (return length))))
      (export-gfm-check-total-output-length output-length)
      (with-output-to-string (output)
        (loop with index = start
              while (< index end)
              do (if (and (<= (+ index 5) end)
                          (string= destination "&amp;"
                                   :start1 index :end1 (+ index 5)))
                     (progn (write-char #\& output) (incf index 5))
                     (progn
                       (write-char (char destination index) output)
                       (incf index)))))))
  (defun export-md-render-link-label
      (state node view frame context prefix-reserve)
    (let* ((children (export-view-children view))
           (*export-gfm-output-prefix-length*
             (+ *export-gfm-output-prefix-length* prefix-reserve))
           (*export-current-output-offset*
             (+ *export-current-output-offset* prefix-reserve)))
      (if children
          (export-md-render-inline-views
           state children context
           :table-cell-p (export-render-frame-table-cell-p frame)
           :preserve-source-breaks-p
           (export-render-frame-preserve-source-breaks-p frame))
          (export-md-escape-plain-text
           (or (node-property node :path) "")))))
  (defun export-md-render-link-description (description prefix-reserve)
    (let ((*export-gfm-output-prefix-length*
            (+ *export-gfm-output-prefix-length* prefix-reserve))
          (*export-current-output-offset*
            (+ *export-current-output-offset* prefix-reserve)))
      (export-md-escape-plain-text (or description ""))))
  (defun export-md-render-link-output (label destination image-p)
    (let ((output-length (+ (length label) (length destination) 4
                            (if image-p 1 0))))
      (export-gfm-check-total-output-length output-length)
      (format nil "~A[~A](~A)" (if image-p "!" "") label destination)))
  (defun export-md-render-link-handler
      (node result table-cell-p)
    (let* ((type (node-property node :type))
           (path (node-property node :path))
           (destination (export-link-result-destination result))
           (description (export-link-result-description result))
           (kind (export-link-result-kind result))
           (scheme-end (export-explicit-url-scheme-end destination))
           (raw-parts (and (stringp path) (list path)))
           (destination-parts (list destination))
           (safe-p
             (and raw-parts
                  (export-safe-url-p path destination)
                  (export-gfm-link-destination-safe-p
                   type nil raw-parts destination-parts)
                  (or (eq kind :link)
                      (or (null scheme-end)
                          (member (subseq destination 0 scheme-end)
                                  '("http" "https") :test #'string-equal))))))
      (when safe-p
        (let* ((serialized
                 (export-gfm-serialize-destination-parts
                  destination-parts :table-cell-p table-cell-p))
               (md-destination
                 (export-md-normalize-link-destination serialized))
               (image-p (eq kind :image))
               (reserve (+ (length md-destination) 4
                           (if image-p 1 0)))
               (label
                 (export-md-render-link-description description reserve)))
          (export-md-render-link-output label md-destination image-p)))))
  (defun export-md-render-file-link-destination
      (node path table-cell-p)
    (let* ((type (node-property node :type))
           (new-path
             (concatenate 'string
                          (subseq path 0 (- (length path) 4))
                          ".md"))
           (destination-parts (list new-path)))
      (when (and (export-gfm-link-source-control-p node)
                 (export-gfm-preflight-link-destination
                  type path nil table-cell-p (list path) destination-parts))
        (export-md-normalize-link-destination
         (export-gfm-serialize-destination-parts
          destination-parts :table-cell-p table-cell-p)))))
  (defun export-md-render-link (state view frame context)
    (let* ((node (export-view-node view))
           (type (node-property node :type))
           (path (node-property node :path))
           (table-cell-p (export-render-frame-table-cell-p frame))
           (gfm-state (export-md-state-gfm-state state))
           (handler-result
             (export-call-link-type-handler-for-view
              context node view frame))
           (handler-output
             (and handler-result
                  (export-md-render-link-handler
                   node handler-result table-cell-p))))
      (or handler-output
          (let* ((org-file-p
                   (and (stringp type) (string-equal type "file")
                        (stringp path) (>= (length path) 4)
                        (string-equal path ".org"
                                      :start1 (- (length path) 4))))
                 (destination
                   (if org-file-p
                       (export-md-render-file-link-destination
                        node path table-cell-p)
                       (let ((serialized
                               (export-gfm-link-destination
                                gfm-state node :table-cell-p table-cell-p)))
                         (and serialized
                              (export-md-normalize-link-destination
                               serialized)))))
                 (image-p (and destination
                               (export-gfm-image-path-p path)))
                 (reserve (if destination
                              (+ (length destination) 4
                                 (if image-p 1 0))
                              0))
                 (label
                   (export-md-render-link-label
                    state node view frame context reserve)))
            (if destination
                (export-md-render-link-output label destination image-p)
                label))))))

(progn (declaim (ftype function export-md-render-list-item)) (defun export-md-render-node (state node view frame context)
  (case (node-type node)
    ((:center-block :table)
     (let ((html (export-md-render-html-fallback state node view)))
       (if (eq (node-type node) :table)
           (export-md-adjust-table-columns html)
           html)))
    (:special-block (export-md-render-special-block state node view))
    (:inlinetask (export-md-render-inlinetask state node view))
    (:underline (export-md-render-html-fallback state node view))
    (:code (export-md-render-code-span node))
    (:verbatim (export-md-render-code-span node))
    (:inline-src-block
     (let* ((parameters
              (babel-parse-header-arguments
               (node-property node :parameters)))
            (exports (assoc "EXPORTS" (reverse parameters) :test #'string=))
            (results (assoc "RESULTS" parameters :test #'string=))
            (code-p
              (if exports
                  (let ((mode (string-downcase (or (cdr exports) ""))))
                    (unless (or (string= mode "code")
                                (string= mode "both")
                                (string= mode "results")
                                (string= mode "none"))
                      (export-unsupported-syntax
                       :md node (export-source-position node) :src-header-argument
                       "Unsupported :exports inline source mode"))
                    (or (string= mode "code") (string= mode "both")))
                  (null results))))
       (if code-p
           (export-md-render-code-span node)
           "")))
    (:strike-through
     (let ((contents (export-md-inline-contents state view frame context)))
       (export-gfm-check-total-output-length (+ 11 (length contents)))
       (concatenate 'string "<del>" contents "</del>")))
    ((:example-block :fixed-width)
     (export-md-indent-lines (export-md-node-value node) 4))
    (:src-block
     (let* ((mode (getf (export-view-payload view) :exports))
            (code (export-md-indent-lines (export-md-node-value node) 4))
            (results (export-md-render-views
                      state (export-view-children view))))
       (case mode
         (:none "")
         (:results results)
         (:code code)
         (otherwise
          (if (zerop (length results))
              code
              (let* ((separator-length
                       (if (and (plusp (length code))
                                (char= (char code (1- (length code)))
                                       #\Newline))
                           1 2))
                     (output-length
                       (+ (length code) separator-length (length results))))
                (export-gfm-check-total-output-length output-length)
                (concatenate
                 'string code
                 (make-string separator-length :initial-element #\Newline)
                 results)))))))
    (:export-block
     (let ((backend (node-property node :type)))
       (cond
         ((and (stringp backend)
               (member backend '("md" "markdown") :test #'string-equal))
          (export-md-node-value node))
         ((and (stringp backend) (string-equal backend "html"))
          (if (export-render-context-raw-output-p context :html)
              (export-md-node-value node)
              (export-md-render-html-fallback state node view)))
         (t ""))))
    (:export-snippet
     (let ((backend (node-property node :back-end)))
       (cond
         ((and (stringp backend)
               (member backend '("md" "markdown") :test #'string-equal))
          (export-md-node-value node))
         ((and (stringp backend) (string-equal backend "html"))
          (if (export-render-context-raw-output-p context :html)
              (export-md-node-value node)
              (export-md-render-html-fallback state node view)))
         (t ""))))
    (:headline (export-md-render-heading state view context))
    (:item (export-md-render-list-item state view context))
    (:plain-list (export-md-render-list state view context))
    (:keyword
     (let ((key (node-property node :key)))
       (cond
         ((and (stringp key)
               (member key '("MD" "MARKDOWN") :test #'string-equal))
          (export-md-node-value node))
         ((and (stringp key) (string-equal key "HTML")
               (export-render-context-raw-output-p context :html))
          (export-md-node-value node))
         (t ""))))
    (:line-break
     (progn
       (export-gfm-check-total-output-length 3)
       (format nil "  ~%")))
    (:link (export-md-render-link state view frame context))
    (:plain-text
     (export-md-escape-plain-text
      (export-md-source-text node)
      (export-option-value context :preserve-breaks)))
    (:quote-block
     (let* ((text (export-md-render-views
                   state (export-view-children view)))
            (text-length (length text)))
       (when (and (plusp text-length)
                  (char= (char text (1- text-length)) #\Newline))
         (setf text (subseq text 0 (1- text-length))))
       (let ((line-count
               (if (zerop (length text))
                   0
                   (1+ (count #\Newline text)))))
         (export-gfm-check-total-output-length
          (+ (length text) (* 2 line-count))))
       (with-output-to-string (output)
         (loop with start = 0
               for newline = (position #\Newline text :start start)
               do (write-string "> " output)
                  (write-string text output :start start :end newline)
                  (when newline
                    (write-char #\Newline output)
                    (setf start (1+ newline)))
               while newline))))
    ((:citation :citation-reference)
     (export-gfm-check-total-output-length 10)
     "(??, ????)")
    (:statistics-cookie
     (export-md-capture-html-output
      state
      (lambda (html-state)
        (export-html-write html-state "<code>")
        (export-html-write-raw-text html-state node)
        (export-html-write html-state "</code>"))))
    (:timestamp
     (export-md-capture-html-output
      state
      (lambda (html-state)
        (export-html-write
         html-state
         "<span class=\"timestamp-wrapper\"><span class=\"timestamp\">")
        (export-html-write-raw-text html-state node)
        (export-html-write html-state "</span></span>"))))
    ((:drawer :dynamic-block :section)
     (export-md-render-views state (export-view-children view)))
    (:property-drawer
     (let ((contents (export-md-render-views
                      state (export-view-children view))))
       (if (zerop (length contents)) "" (export-md-indent-lines contents 4))))
    (:node-property
     (let* ((key (or (node-property node :key) ""))
            (value (node-property node :value))
            (output-length
              (+ (length key) 1
                 (if value (1+ (length value)) 0))))
       (export-gfm-check-total-output-length output-length)
       (with-output-to-string (output)
         (write-string key output)
         (write-char #\: output)
         (when value
           (write-char #\Space output)
           (write-string value output)))))
    (:latex-fragment
     (let ((value (export-md-node-value node)))
       (cond
         ((and (>= (length value) 4)
               (string= value "\\(" :end1 2))
          (export-gfm-check-total-output-length (- (length value) 2))
          (format nil "$~A$" (subseq value 2 (- (length value) 2))))
         ((and (>= (length value) 4)
               (string= value "\\[" :end1 2))
          (export-gfm-check-total-output-length (length value))
          (format nil "$$~A$$" (subseq value 2 (- (length value) 2))))
         (t value))))
    (:latex-environment (export-md-node-value node))
    (:entity (export-md-render-html-fallback state node view))
    (:verse-block (export-md-render-html-fallback state node view))
    (:diary-sexp "")
    (otherwise
     (if view
         (export-render-parent-view
          context view :mode (export-render-frame-mode frame)
          :table-cell-p (export-render-frame-table-cell-p frame)
          :preserve-source-breaks-p
          (export-render-frame-preserve-source-breaks-p frame))
         (export-render-parent-node
          context node :mode (export-render-frame-mode frame)
          :table-cell-p (export-render-frame-table-cell-p frame)
          :preserve-source-breaks-p
          (export-render-frame-preserve-source-breaks-p frame)))))))

(defun export-md-transcode-node (node view frame context)
  (let* ((state (export-render-context-state context))
         (gfm-state (export-md-state-gfm-state state))
         (parent-context (export-gfm-state-render-context gfm-state))
         (parent-frame (export-gfm-state-render-frame gfm-state))
         (parent-offset (export-gfm-state-output-offset gfm-state))
         (offset (max parent-offset
                      (export-render-frame-output-offset frame))))
    (setf (export-md-state-preparation state)
          (org-kit::%export-render-context-preparation context)
          (export-md-state-limits state) (export-render-context-limits context)
          (export-gfm-state-render-context gfm-state) context
          (export-gfm-state-render-frame gfm-state) frame
          (export-gfm-state-output-offset gfm-state) offset)
    (unwind-protect
         (let ((*export-gfm-output-limits* (export-render-context-limits context))
               (*export-gfm-output-prefix-length* offset)
               (*export-current-output-offset* offset))
           (export-md-render-node state node view frame context))
      (setf (export-gfm-state-render-context gfm-state) parent-context
            (export-gfm-state-render-frame gfm-state) parent-frame
            (export-gfm-state-output-offset gfm-state) parent-offset))))

(defun export-md-node-transcoders ()
  (mapcar (lambda (type) (cons type #'export-md-transcode-node))
          '(:center-block :citation :citation-reference :code :drawer
            :dynamic-block :diary-sexp :entity :example-block :export-block :export-snippet
            :fixed-width :headline :inlinetask :item :keyword
            :latex-environment :latex-fragment :line-break :link
            :node-property :paragraph :plain-list :plain-text
            :property-drawer :quote-block :section :special-block :src-block :statistics-cookie
            :strike-through :table :timestamp :underline :verse-block :verbatim
            :inline-src-block)))

(defun export-md-render-footnote-definitions (state output-prefix)
  (let ((entries (export-preparation-footnotes
                  (export-md-state-preparation state))))
    (when entries
      (let* ((heading (format nil "~%~%# Footnotes~%~%"))
             (output-length (length heading))
             (*export-gfm-output-prefix-length* output-prefix))
        (export-gfm-check-total-output-length output-length)
        (with-output-to-string (output)
          (write-string heading output)
          (loop for entry in entries
                do (let* ((number-text (princ-to-string (getf entry :number)))
                          (view (getf entry :definition))
                          (header-length
                            (+ (length "<sup><a id=\"fn.")
                               (* 3 (length number-text))
                               (length "\" href=\"#fnr.")
                               (length "\">")
                               (length "</a></sup> "))))
                     (export-gfm-check-total-output-length
                      (+ output-length header-length 1))
                     (let* ((*export-gfm-output-prefix-length*
                              (+ output-prefix output-length header-length))
                            (body
                              (export-md-render-views
                               state (export-view-children view)))
                            (body-start
                              (loop for index from 0 below (length body)
                                    while (find (char body index)
                                                (list #\Space #\Newline))
                                    finally (return index)))
                            (body-end
                              (loop for index downfrom (length body)
                                    above body-start
                                    while (find (char body (1- index))
                                                (list #\Space #\Newline))
                                    finally (return index)))
                            (body-length (- body-end body-start)))
                       (export-gfm-check-total-output-length
                        (+ output-length header-length body-length 1))
                       (write-string
                        (format nil
                                "<sup><a id=\"fn.~A\" href=\"#fnr.~A\">~A</a></sup> "
                                number-text number-text number-text)
                        output)
                       (write-string body output
                                     :start body-start :end body-end)
                       (write-char #\Newline output)
                       (incf output-length
                             (+ header-length body-length 1))))))))))

(progn
  (defun export-md-index-custom-heading-references (state)
    (let* ((gfm-state (export-md-state-gfm-state state))
           (document
             (export-preparation-document (export-md-state-preparation state)))
           (views
             (export-collect-views
              (export-preparation-root-views
               (export-md-state-preparation state)))))
      (dolist (view views)
        (let* ((node (export-view-node view))
               (source-document (or (node-document node) document)))
          (when (member (node-type node) '(:headline :inlinetask) :test #'eq)
            (multiple-value-bind (custom-id sources)
                (effective-property source-document node "CUSTOM_ID")
              (declare (ignore sources))
              (when (and (stringp custom-id)
                         (plusp (length custom-id))
                         (export-gfm-target-id-safe-p custom-id))
                (let* ((old-id
                         (gethash node
                                  (export-gfm-state-heading-ids gfm-state)))
                       (id
                         (if (equal old-id custom-id)
                             old-id
                             (export-gfm-unique-id gfm-state custom-id))))
                  (setf (gethash node
                                 (export-gfm-state-heading-ids gfm-state))
                        id
                        (gethash id
                                 (export-gfm-state-heading-targets gfm-state))
                        node)
                  (when old-id
                    (export-gfm-register-reference gfm-state old-id id))
                  (export-gfm-register-reference gfm-state custom-id id)
                  (export-gfm-register-reference
                   gfm-state (node-property node :custom-id) id)
                  (export-gfm-register-reference
                   gfm-state (node-property node :raw-value) id)))))))
      (export-gfm-mark-heading-anchors gfm-state)
      gfm-state))
  (defun export-md-heading-reference (state node)
    (export-gfm-heading-reference
     (export-md-state-gfm-state state) node))
  (defun export-md-toc-number-component (number)
    (when (stringp number)
      (let* ((last-dot (position #\. number :from-end t))
             (previous-dot
               (and last-dot
                    (position #\. number :end last-dot :from-end t))))
        (when last-dot
          (subseq number (if previous-dot (1+ previous-dot) 0) last-dot)))))
  (defun export-md-render-toc (state context)
  (let* ((preparation (export-md-state-preparation state))
         (entries (export-preparation-toc preparation))
         (html-state (export-md-state-html-state state))
         (options (export-preparation-options preparation))
         (tag-option (assoc :tags options))
         (minimum-level
           (and entries
                (reduce #'min entries :key (lambda (entry)
                                             (getf entry :level)))))
         (base-prefix *export-gfm-output-prefix-length*)
         (base-offset *export-current-output-offset*))
    (when entries
      (with-output-to-string (output)
        (let* ((header-length
                 (+ 1 (length "# Table of Contents") 2))
               (output-length 0)
               (first-p t))
          (export-gfm-check-total-output-length header-length)
          (write-char #\Newline output)
          (write-string "# Table of Contents" output)
          (write-char #\Newline output)
          (write-char #\Newline output)
          (setf output-length header-length)
          (dolist (entry entries)
            (let* ((separator-length (if first-p 0 1))
                   (view (getf entry :view))
                   (node (export-view-node view))
                   (level (getf entry :level))
                   (indent-length (* 4 (max 0 (- level minimum-level))))
                   (number
                     (gethash node
                              (export-html-state-heading-numbers html-state)))
                   (component (export-md-toc-number-component number))
                   (marker-length (if component (1+ (length component)) 1))
                   (marker-space-count
                     (if component
                         (max 1 (- 4 marker-length))
                         3))
                   (bullet-length (+ marker-length marker-space-count))
                   (reference (export-md-heading-reference state node))
                   (destination-length (1+ (length reference)))
                   (tags
                     (and (or (null tag-option) (cdr tag-option))
                          (not (eq (cdr tag-option) :not-in-toc))
                          (node-property node :tags)))
                   (tag-length
                     (if tags
                         (+ 1 (length tags)
                            (reduce #'+ tags :key #'length
                                    :initial-value 0))
                         0))
                   (line-prefix-length
                     (+ base-prefix output-length separator-length))
                   (title-tail-length
                     (+ indent-length bullet-length 1 2 destination-length
                        1 tag-length))
                   (*export-gfm-output-prefix-length*
                     (+ line-prefix-length title-tail-length))
                   (*export-current-output-offset*
                     (+ base-offset output-length separator-length
                        title-tail-length))
                   (title
                     (export-md-render-heading-title
                      state node view context))
                   (line-length
                     (+ indent-length bullet-length 1 (length title)
                        2 destination-length 1 tag-length))
                   (destination
                     (progn
                       (let ((*export-gfm-output-prefix-length*
                               line-prefix-length))
                         (export-gfm-check-total-output-length line-length))
                       (concatenate 'string "#" reference))))
              (unless first-p
                (write-char #\Newline output))
              (loop repeat indent-length
                    do (write-char #\Space output))
              (if component
                  (progn
                    (write-string component output)
                    (write-char #\. output))
                  (write-char #\- output))
              (loop repeat marker-space-count
                    do (write-char #\Space output))
              (write-char #\[ output)
              (write-string title output)
              (write-string "](" output)
              (write-string destination output)
              (write-char (code-char 41) output)
              (when tags
                (write-char #\: output)
                (loop for tag in tags
                      for first-tag-p = t then nil
                      do (unless first-tag-p
                           (write-char #\: output))
                         (write-string tag output))
                (write-char #\: output))
              (incf output-length (+ separator-length line-length))
              (setf first-p nil)))
          (export-gfm-check-total-output-length (+ output-length 2))
          (write-char #\Newline output)
          (write-char #\Newline output))))))
  (defun export-md-render-body (context)
    (let* ((state (export-render-context-state context))
           (gfm-state (export-md-state-gfm-state state))
           (preparation (org-kit::%export-render-context-preparation context))
           (root-views (export-preparation-root-views preparation))
           (base-offset *export-current-output-offset*)
           (parent-context (export-gfm-state-render-context gfm-state))
           (parent-frame (export-gfm-state-render-frame gfm-state))
           (parent-offset (export-gfm-state-output-offset gfm-state)))
      (setf (export-md-state-preparation state) preparation
            (export-md-state-limits state) (export-render-context-limits context)
            (export-gfm-state-render-context gfm-state) context
            (export-gfm-state-render-frame gfm-state) nil
            (export-gfm-state-output-offset gfm-state) base-offset)
      (unwind-protect
           (let ((*export-gfm-output-limits*
                   (export-render-context-limits context))
                 (*export-gfm-output-prefix-length* base-offset)
                 (*export-current-output-offset* base-offset))
             (unless (export-gfm-state-indexed-p gfm-state)
               (export-gfm-index-state gfm-state)
               (export-md-index-custom-heading-references state)
               (setf (export-gfm-state-indexed-p gfm-state) t))
             (let* ((toc (or (export-md-render-toc state context) ""))
                    (toc-length (length toc))
                    (body
                      (let ((*export-gfm-output-prefix-length*
                              (+ base-offset toc-length))
                            (*export-current-output-offset*
                              (+ base-offset toc-length)))
                        (export-md-render-views
                         state root-views :preserve-leading-post-blank-p t)))
                    (footnotes-prefix
                      (+ base-offset toc-length (length body) 1))
                    (footnotes
                      (let ((*export-gfm-output-prefix-length*
                              footnotes-prefix)
                            (*export-current-output-offset* footnotes-prefix))
                        (export-md-render-footnote-definitions
                         state footnotes-prefix)))
                    (body-trailing-post-blank
                      (if (and (zerop (length footnotes))
                               (plusp (length body))
                               root-views)
                          (export-md-view-post-blank (car (last root-views)))
                          0))
                    (trailing-length (+ 1 body-trailing-post-blank))
                    (result-length
                      (+ toc-length (length body) trailing-length
                         (if footnotes (length footnotes) 0))))
               (export-gfm-check-total-output-length result-length)
               (if footnotes
                   (concatenate 'string toc body (string #\Newline)
                                footnotes)
                   (concatenate
                    'string toc body
                    (make-string trailing-length :initial-element #\Newline)))))
        (setf (export-gfm-state-render-context gfm-state) parent-context
              (export-gfm-state-render-frame gfm-state) parent-frame
              (export-gfm-state-output-offset gfm-state) parent-offset)))))

(eval-when (:load-toplevel :execute)
  (%register-builtin-derived-export-backend
   :md :gfm
   :renderer #'export-md-render-body
   :make-state #'export-md-make-state
   :transcoders (export-md-node-transcoders)
   :node-policies '((:diary-sexp . :default))
   :accepted-keywords '("HTML")
   :retained-keywords '("HTML")
   :raw-output-options '((:html . :allow-raw-html))
   :export-block-names '("md" "markdown")
   :export-snippet-names '("md" "markdown" "html")))

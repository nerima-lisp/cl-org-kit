;;;; src/edit-properties.lisp
;;;;
;;;; Format-preserving edits for headline properties, planning and keywords.
(in-package #:org-kit)

(defun edit-properties-horizontal-space-p (character)
  (find character '(#\Space #\Tab)))

(defun edit-properties-single-line-value (value target)
  (unless (and (stringp value)
               (not (find #\Newline value))
               (not (find #\Return value)))
    (edit-signal-error target :invalid-value
                       "Metadata values must be single-line strings"))
  value)

(defun edit-properties-key (key target)
  (unless (and (stringp key)
               (plusp (length key))
               (string= key (string-trim '(#\Space #\Tab) key))
               (not (find #\: key))
               (not (find #\Newline key))
               (not (find #\Return key)))
    (edit-signal-error target :invalid-value "Invalid metadata key"))
  (string-upcase key))

(defun edit-properties-semantic-value (value)
  (string-trim '(#\Space #\Tab) value))

(defun edit-properties-line-bounds (source start)
  (let ((newline (position #\Newline source :start start)))
    (if newline
        (values (if (and (> newline start)
                         (char= (char source (1- newline)) #\Return))
                    (1- newline)
                    newline)
                (1+ newline))
        (values (length source) (length source)))))

(defun edit-properties-value-range (document node kind target)
  "Return the trimmed value range on NODE's source line."
  (let* ((source (document-text document))
         (line-start (node-begin node)))
    (unless (and (integerp line-start) (<= 0 line-start (length source)))
      (edit-signal-error target :uneditable-structure
                         "Metadata node has no source range"))
    (multiple-value-bind (line-end after-line)
        (edit-properties-line-bounds source line-start)
      (declare (ignore after-line))
      (let* ((delimiter-start (if (eq kind :keyword) (+ line-start 2) (1+ line-start)))
             (delimiter (position #\: source :start delimiter-start :end line-end)))
        (unless delimiter
          (edit-signal-error target :uneditable-structure
                             "Metadata source line has no value delimiter"))
        (let ((value-start (1+ delimiter))
              (value-end line-end))
          (loop while (and (< value-start value-end)
                           (edit-properties-horizontal-space-p
                            (char source value-start)))
                do (incf value-start))
          (loop while (and (> value-end value-start)
                           (edit-properties-horizontal-space-p
                            (char source (1- value-end))))
                do (decf value-end))
          (values value-start value-end))))))

(defun edit-properties-line-ending (source offset document)
  (let ((before (and (plusp offset)
                     (position #\Newline source :end offset :from-end t)))
        (after (position #\Newline source :start offset)))
    (cond
      ((and before (> before 0)
            (char= (char source (1- before)) #\Return))
       (format nil "~C~C" #\Return #\Newline))
      (before (string #\Newline))
      ((and after (> after 0)
            (char= (char source (1- after)) #\Return))
       (format nil "~C~C" #\Return #\Newline))
      (after (string #\Newline))
      ((eq (document-newline document) :crlf)
       (format nil "~C~C" #\Return #\Newline))
      (t (string #\Newline)))))

(defun edit-properties-join-lines (lines newline)
  (with-output-to-string (stream)
    (loop for line in lines
          for first-p = t then nil
          unless first-p do (write-string newline stream)
          do (write-string line stream))))

(defun edit-properties-insertion-text (document offset lines)
  (let* ((source (document-text document))
         (length (length source))
         (newline (edit-properties-line-ending source offset document))
         (prefix-p (and (plusp offset)
                        (not (char= (char source (1- offset)) #\Newline))))
         (suffix-p (or (< offset length) (document-final-newline-p document))))
    (concatenate 'string
                 (if prefix-p newline "")
                 (edit-properties-join-lines lines newline)
                 (if suffix-p newline ""))))

(defun edit-properties-line-after (source start)
  (nth-value 1 (edit-properties-line-bounds source start)))

(defun edit-properties-headline-section (headline)
  (find :section (node-children headline) :key #'node-type))

(defun edit-properties-section-planning (section)
  (and section
       (find :planning (node-children section) :key #'node-type)))

(defun edit-properties-section-drawer (section)
  (and section
       (find :property-drawer (node-children section) :key #'node-type)))

(defun edit-properties-property-nodes (drawer key)
  (remove-if-not
   (lambda (node)
     (and (eq (node-type node) :node-property)
          (stringp (node-property node :key))
          (string= key (node-property node :key))))
   (node-children drawer)))

(defun edit-properties-property-target (document headline key target)
  (let* ((section (edit-properties-headline-section headline))
         (drawer (edit-properties-section-drawer section))
         (matches (and drawer (edit-properties-property-nodes drawer key))))
    (when (> (length matches) 1)
      (edit-signal-error target :ambiguous-target
                         "Property key occurs more than once in the drawer"))
    (values section drawer (first matches))))

(defun edit-properties-index (node children target)
  (or (position node children :test #'eq)
      (edit-signal-error target :uneditable-structure
                         "Node is absent from its parent's children")))

(defun edit-properties-canonical-insert-child (tree parent-path index child)
  (let* ((parent (edit-canonical-node-at-path tree parent-path))
         (children (getf parent :children)))
    (unless (and (integerp index) (<= 0 index (length children)))
      (edit-signal-error parent-path :uneditable-structure
                         "Cannot locate metadata insertion point"))
    (edit-canonical-set-children
     tree parent-path
     (append (subseq children 0 index) (list child) (nthcdr index children)))))

(defun edit-properties-canonical-remove-child (tree parent-path index target)
  (let* ((parent (edit-canonical-node-at-path tree parent-path))
         (children (getf parent :children)))
    (unless (and (integerp index) (<= 0 index) (< index (length children)))
      (edit-signal-error target :uneditable-structure
                         "Cannot locate metadata node in the expected tree"))
    (edit-canonical-set-children
     tree parent-path
     (append (subseq children 0 index) (nthcdr (1+ index) children)))))

(defun edit-properties-canonical-property (key value)
  (list :type :node-property :post-blank 0
        :properties (list :key key :value value)
        :children nil))

(defun edit-properties-canonical-drawer (key value post-blank)
  (list :type :property-drawer :post-blank post-blank
        :properties nil
        :children (list (edit-properties-canonical-property key value))))

(defun edit-properties-blank-lines-after (source offset)
  (loop with position = offset
        with count = 0
        while (< position (length source))
        do (multiple-value-bind (line-end after-line)
               (edit-properties-line-bounds source position)
             (unless (every #'edit-properties-horizontal-space-p
                            (subseq source position line-end))
               (return count))
             (incf count)
             (setf position after-line))
        finally (return count)))

(defun edit-properties-drawer-close-start (source drawer)
  (let ((position (edit-properties-line-after source (node-begin drawer)))
        (limit (node-end drawer)))
    (loop while (and (< position limit) (< position (length source)))
          do (multiple-value-bind (line-end after-line)
                 (edit-properties-line-bounds source position)
               (when (string-equal (string-trim '(#\Space #\Tab)
                                                (subseq source position line-end))
                                   ":END:")
                 (return-from edit-properties-drawer-close-start position))
               (setf position after-line)))
    nil))

(defun edit-properties-set-new-drawer-expected
    (document headline section planning key value insertion-offset)
  (let* ((expected (edit-canonical-copy (edit-canonical-document document)))
         (headline-path (edit-node-path document headline))
         (drawer (edit-properties-canonical-drawer
                  key value
                  (edit-properties-blank-lines-after
                   (document-text document) insertion-offset))))
    (if section
        (let* ((section-index
                 (edit-properties-index section (node-children headline) headline))
               (section-path (append headline-path (list section-index)))
               (children (node-children section))
               (index (if planning
                          (1+ (edit-properties-index planning children headline))
                          0)))
          (edit-properties-canonical-insert-child expected section-path index drawer))
        (let ((section-canonical
                (list :type :section :post-blank 0 :properties nil
                      :children (list drawer))))
          (edit-properties-canonical-insert-child
           expected headline-path 0 section-canonical)))
    expected))

(defun edit-properties-remove-property-expected
    (document headline section drawer property target)
  (let* ((expected (edit-canonical-copy (edit-canonical-document document)))
         (headline-path (edit-node-path document headline))
         (section-index (edit-properties-index section (node-children headline) target))
         (section-path (append headline-path (list section-index)))
         (drawer-index (edit-properties-index drawer (node-children section) target))
         (drawer-path (append section-path (list drawer-index)))
         (property-index (edit-properties-index property (node-children drawer) target)))
    (if (= (length (node-children drawer)) 1)
        (progn
          (edit-properties-canonical-remove-child
           expected section-path drawer-index target)
          (when (null (cdr (node-children section)))
            (edit-properties-canonical-remove-child
             expected headline-path section-index target)))
        (edit-properties-canonical-remove-child
         expected drawer-path property-index target))
    expected))

(defun set-property (document target key value)
  "Set one headline property while preserving source outside its value or drawer."
  (let* ((headline (edit-resolve-node document target '(:headline)))
         (normalized-key (edit-properties-key key target))
         (raw-value (edit-properties-single-line-value value target))
         (semantic-value (edit-properties-semantic-value raw-value))
         (source (document-text document))
         (section (edit-properties-headline-section headline))
         (drawer (edit-properties-section-drawer section))
         (matches (and drawer
                       (edit-properties-property-nodes drawer normalized-key))))
    (when (> (length matches) 1)
      (edit-signal-error target :ambiguous-target
                         "Property key occurs more than once in the drawer"))
    (let* ((property (first matches))
           (expected (edit-canonical-copy (edit-canonical-document document)))
           (headline-path (edit-node-path document headline)))
      (if property
          (multiple-value-bind (value-start value-end)
              (edit-properties-value-range document property :property target)
            (edit-canonical-set-property
             expected (edit-node-path document property) :value semantic-value)
            (edit-apply-patches
             document
             (list (edit-make-patch value-start value-end raw-value target))
             :target target :expected expected))
          (if drawer
              (let ((close-start (edit-properties-drawer-close-start source drawer)))
                (unless close-start
                  (edit-signal-error target :uneditable-structure
                                     "Property drawer has no closing marker"))
                (let* ((newline (edit-properties-line-ending
                                 source close-start document))
                       (section-index
                         (edit-properties-index section
                                                (node-children headline) target))
                       (drawer-index
                         (edit-properties-index drawer
                                                (node-children section) target))
                       (drawer-path
                         (append headline-path (list section-index drawer-index)))
                       (drawer-canonical
                         (edit-canonical-node-at-path expected drawer-path)))
                  (setf (getf drawer-canonical :children)
                        (append (getf drawer-canonical :children)
                                (list (edit-properties-canonical-property
                                       normalized-key semantic-value))))
                  (edit-apply-patches
                   document
                   (list (edit-make-patch
                          close-start close-start
                          (concatenate 'string
                                       (format nil ":~A: ~A" normalized-key raw-value)
                                       newline)
                          target))
                   :target target :expected expected)))
              (let* ((planning (edit-properties-section-planning section))
                     (heading-start (or (and planning (node-begin planning))
                                        (node-post-affiliated headline)))
                     (insertion-offset
                       (and heading-start
                            (edit-properties-line-after source heading-start))))
                (unless insertion-offset
                  (edit-signal-error target :uneditable-structure
                                     "Headline has no source line"))
                (let* ((text (edit-properties-insertion-text
                              document insertion-offset
                              (list ":PROPERTIES:"
                                    (format nil ":~A: ~A" normalized-key raw-value)
                                    ":END:")))
                       (expected
                         (edit-properties-set-new-drawer-expected
                          document headline section
                          (edit-properties-section-planning section)
                          normalized-key semantic-value insertion-offset)))
                  (edit-apply-patches
                   document
                   (list (edit-make-patch insertion-offset insertion-offset text target))
                   :target target :expected expected))))))))

(defun remove-property (document target key)
  "Remove one headline property and remove its drawer when it becomes empty."
  (let* ((headline (edit-resolve-node document target '(:headline)))
         (normalized-key (edit-properties-key key target)))
    (multiple-value-bind (section drawer property)
        (edit-properties-property-target document headline normalized-key target)
      (unless property
        (edit-signal-error target :target-not-found "Property key was not found"))
      (let* ((start (if (= (length (node-children drawer)) 1)
                        (node-begin drawer)
                        (node-begin property)))
             (end (if (= (length (node-children drawer)) 1)
                      (node-end drawer)
                      (node-end property)))
             (expected
               (edit-properties-remove-property-expected
                document headline section drawer property target)))
        (unless (and (integerp start) (integerp end))
          (edit-signal-error target :uneditable-structure
                             "Property source node has no range"))
        (edit-apply-patches
         document (list (edit-make-patch start end "" target))
         :target target :expected expected)))))

(defun edit-properties-planning-kind-p (kind)
  (member kind '(:scheduled :deadline :closed)))

(defun edit-properties-planning-label (kind)
  (ecase kind
    (:scheduled "SCHEDULED")
    (:deadline "DEADLINE")
    (:closed "CLOSED")))

(defun edit-properties-planning-node (document kind timestamp-text target)
  (let* ((source (format nil "* Temporary~%~A: ~A~%"
                         (edit-properties-planning-label kind) timestamp-text))
         (temporary
           (parse-org source :limits (document-limits document)))
         (headline (find :headline (node-children (document-root temporary))
                         :key #'node-type))
         (section (and headline (edit-properties-headline-section headline)))
         (planning (edit-properties-section-planning section)))
    (unless (and planning (node-property planning kind))
      (edit-signal-error target :invalid-value "Invalid Org planning timestamp"))
    planning))

(defun edit-properties-planning-value (document kind timestamp-text target)
  (let ((planning (edit-properties-planning-node
                   document kind timestamp-text target)))
    (edit-canonical-value (node-property planning kind))))

(defun edit-properties-canonical-planning (kind value)
  (list :type :planning :post-blank 0
        :properties (loop for candidate in '(:closed :deadline :scheduled)
                          append (list candidate
                                       (if (eq candidate kind) value nil)))
        :children nil))

(defun edit-properties-set-planning-expected
    (document headline section planning kind value target)
  (let* ((expected (edit-canonical-copy (edit-canonical-document document)))
         (headline-path (edit-node-path document headline)))
    (if planning
        (edit-canonical-set-property
         expected (edit-node-path document planning) kind value)
        (let ((planning-canonical
                (edit-properties-canonical-planning kind value)))
          (if section
              (let ((section-index
                      (edit-properties-index section
                                             (node-children headline) target)))
                (edit-properties-canonical-insert-child
                 expected (append headline-path (list section-index)) 0
                 planning-canonical))
              (edit-properties-canonical-insert-child
               expected headline-path 0
               (list :type :section :post-blank 0 :properties nil
                     :children (list planning-canonical))))))
    expected))

(defun set-planning (document target kind timestamp-text)
  "Set one planning timestamp on a headline."
  (let* ((headline (edit-resolve-node document target '(:headline)))
         (source (document-text document)))
    (unless (edit-properties-planning-kind-p kind)
      (edit-signal-error target :invalid-value "Unknown planning timestamp kind"))
    (edit-properties-single-line-value timestamp-text target)
    (let* ((section (edit-properties-headline-section headline))
           (planning (edit-properties-section-planning section))
           (value (edit-properties-planning-value
                   document kind timestamp-text target))
           (range (and planning
                       (node-property
                        planning
                        (intern (format nil "READER-~A-RANGE"
                                        (string-upcase (symbol-name kind)))
                                :keyword))))
           (expected (edit-properties-set-planning-expected
                      document headline section planning kind value target)))
      (cond
        ((and planning (node-property planning kind))
         (unless (and (consp range) (integerp (car range)) (integerp (cdr range)))
           (edit-signal-error target :uneditable-structure
                              "Planning timestamp has no source range"))
         (edit-apply-patches
          document
          (list (edit-make-patch (car range) (cdr range) timestamp-text target))
          :target target :expected expected))
        (planning
         (let* ((line-start (node-begin planning))
                (line-end (nth-value 0
                                     (edit-properties-line-bounds source line-start)))
                (insert (format nil " ~A: ~A"
                                (edit-properties-planning-label kind)
                                 timestamp-text)))
           (edit-apply-patches
            document
            (list (edit-make-patch line-end line-end insert target))
            :target target :expected expected)))
        (t
         (let* ((heading-start (node-post-affiliated headline))
                (insertion-offset
                  (and heading-start
                       (edit-properties-line-after source heading-start))))
           (unless insertion-offset
             (edit-signal-error target :uneditable-structure
                                "Headline has no source line"))
           (edit-apply-patches
            document
            (list (edit-make-patch
                   insertion-offset insertion-offset
                   (edit-properties-insertion-text
                    document insertion-offset
                    (list (format nil "~A: ~A"
                                  (edit-properties-planning-label kind)
                                  timestamp-text)))
                   target))
            :target target :expected expected)))))))

(defun edit-properties-planning-field-start (source range kind)
  (let* ((label (concatenate 'string
                             (edit-properties-planning-label kind) ":"))
         (value-start (car range))
         (line-start (or (position #\Newline source :end value-start :from-end t)
                         -1))
         (start (search label source :start2 (1+ line-start)
                        :end2 value-start :test #'char-equal)))
    start))

(defun edit-properties-planning-removal-range (source planning range kind sole-field-p)
  (let* ((line-start (node-begin planning))
         (line-end (nth-value 0 (edit-properties-line-bounds source line-start)))
         (value-start (car range))
         (value-end (cdr range))
         (field-start (edit-properties-planning-field-start source range kind)))
    (unless (and (integerp field-start) (<= line-start field-start value-start)
                 (<= value-start value-end line-end))
      (return-from edit-properties-planning-removal-range
        (values nil nil)))
    (if sole-field-p
        (values line-start (node-end planning))
        (let ((end value-end)
              (start field-start))
          (loop while (and (< end line-end)
                           (edit-properties-horizontal-space-p (char source end)))
                do (incf end))
          (when (= end value-end)
            (loop while (and (> start line-start)
                             (edit-properties-horizontal-space-p
                              (char source (1- start))))
                  do (decf start)))
          (values start end)))))

(defun edit-properties-remove-planning-expected
    (document headline section planning kind has-other-p target)
  (let* ((expected (edit-canonical-copy (edit-canonical-document document)))
         (headline-path (edit-node-path document headline)))
    (if has-other-p
        (edit-canonical-set-property expected (edit-node-path document planning)
                                     kind nil)
        (let* ((section-index
                 (edit-properties-index section (node-children headline) target))
               (section-path (append headline-path (list section-index)))
               (planning-index
                 (edit-properties-index planning (node-children section) target)))
          (edit-properties-canonical-remove-child
           expected section-path planning-index target)
          (when (null (cdr (node-children section)))
            (edit-properties-canonical-remove-child
             expected headline-path section-index target))))
    expected))

(defun remove-planning (document target kind)
  "Remove one planning timestamp from a headline."
  (let* ((headline (edit-resolve-node document target '(:headline)))
         (source (document-text document)))
    (unless (edit-properties-planning-kind-p kind)
      (edit-signal-error target :invalid-value "Unknown planning timestamp kind"))
    (let* ((section (edit-properties-headline-section headline))
           (planning (edit-properties-section-planning section))
           (value (and planning (node-property planning kind))))
      (unless value
        (edit-signal-error target :target-not-found
                           "Planning timestamp was not found"))
      (let* ((range-key (intern (format nil "READER-~A-RANGE"
                                        (string-upcase (symbol-name kind)))
                                :keyword))
             (range (node-property planning range-key))
             (other-kinds (remove kind '(:scheduled :deadline :closed)))
             (has-other-p (some (lambda (candidate)
                                  (node-property planning candidate))
                                other-kinds))
             (expected (edit-properties-remove-planning-expected
                        document headline section planning kind has-other-p target)))
        (unless (and (consp range) (integerp (car range)) (integerp (cdr range)))
          (edit-signal-error target :uneditable-structure
                             "Planning timestamp has no source range"))
        (multiple-value-bind (start end)
            (edit-properties-planning-removal-range
             source planning range kind (not has-other-p))
          (unless (and start end)
            (edit-signal-error target :uneditable-structure
                               "Planning timestamp has no source field"))
          (edit-apply-patches
           document (list (edit-make-patch start end "" target))
           :target target :expected expected))))))

(defun edit-properties-keyword-selector-p (target)
  (and (consp target) (eq (first target) :keyword-key)))

(defun edit-properties-keyword-selector (target)
  (let ((length (edit-proper-list-length target)))
    (unless (and (integerp length) (member length '(2 3)))
      (edit-signal-error target :invalid-value "Invalid keyword-key selector"))
    (let ((key (edit-properties-key (second target) target))
          (occurrence (and (= length 3) (third target))))
      (when (and (= length 3)
                 (not (and (integerp occurrence) (plusp occurrence))))
        (edit-signal-error target :invalid-value
                           "Keyword occurrence must be a positive integer"))
      (values key occurrence (= length 3)))))

(defun edit-properties-keyword-container (document)
  (let* ((root (document-root document))
         (first (first (node-children root))))
    (if (and first (eq (node-type first) :section))
        (values first (node-children first) (edit-node-path document first))
        (values root (node-children root) nil))))

(defun edit-properties-document-keywords (document &optional key)
  (multiple-value-bind (container children path)
      (edit-properties-keyword-container document)
    (declare (ignore container path))
    (remove-if-not
     (lambda (node)
       (and (eq (node-type node) :keyword)
            (or (null key)
                (and (stringp (node-property node :key))
                     (string= key (node-property node :key))))))
     children)))

(defun edit-properties-resolve-keyword (document target &key allow-missing)
  (if (edit-properties-keyword-selector-p target)
      (multiple-value-bind (key occurrence occurrence-p)
          (edit-properties-keyword-selector target)
        (let* ((matches (edit-properties-document-keywords document key))
               (count (length matches)))
          (cond
            (occurrence-p
             (unless (<= occurrence count)
               (edit-signal-error target :target-not-found
                                  "Keyword occurrence was not found"))
             (values (nth (1- occurrence) matches) key))
            ((= count 1) (values (first matches) key))
            ((> count 1)
             (edit-signal-error target :ambiguous-target
                                "Document-level keyword key is ambiguous"))
            (allow-missing (values nil key))
            (t (edit-signal-error target :target-not-found
                                  "Document-level keyword was not found")))))
      (values (edit-resolve-node document target '(:keyword)) nil)))

(defun edit-properties-keyword-insertion-offset (document)
  (let* ((root (document-root document))
         (children (node-children root))
         (keywords (edit-properties-document-keywords document))
         (last-keyword (car (last keywords)))
         (first-headline (find :headline children :key #'node-type)))
    (cond (last-keyword (node-end last-keyword))
          (first-headline (node-begin first-headline))
          (t (length (document-text document))))))

(defun edit-properties-keyword-node (key value post-blank)
  (list :type :keyword :post-blank post-blank
        :properties (list :key key :value value :options nil)
        :children nil))

(defun edit-properties-insert-keyword-expected
    (document key value insertion-offset target)
  (let ((expected (edit-canonical-copy (edit-canonical-document document))))
    (multiple-value-bind (container children path)
        (edit-properties-keyword-container document)
      (let* ((index (or (position-if
                         (lambda (node)
                           (and (node-begin node)
                                (>= (node-begin node) insertion-offset)))
                         children)
                        (length children)))
             (keyword (edit-properties-keyword-node
                       key value
                       (edit-properties-blank-lines-after
                        (document-text document) insertion-offset))))
        (if (eq container (document-root document))
            (let ((section (list :type :section :post-blank 0 :properties nil
                                 :children (list keyword))))
              (edit-properties-canonical-insert-child
               expected nil index section))
            (edit-properties-canonical-insert-child
             expected path index keyword))))
    expected))

(defun edit-properties-remove-keyword-expected (document keyword target)
  (let* ((expected (edit-canonical-copy (edit-canonical-document document)))
         (path (edit-node-path document keyword))
         (parent-path (butlast path))
         (index (car (last path)))
         (parent (node-parent keyword)))
    (edit-properties-canonical-remove-child expected parent-path index target)
    (when (and (eq (node-type parent) :section)
               (null (cdr (node-children parent))))
      (let* ((grandparent-path (butlast parent-path))
             (parent-index (car (last parent-path))))
        (edit-properties-canonical-remove-child
         expected grandparent-path parent-index target)))
    expected))

(defun set-keyword-value (document target value)
  "Set a keyword's value, or add a document-level keyword by selector."
  (multiple-value-bind (keyword selector-key)
      (edit-properties-resolve-keyword document target :allow-missing t)
    (edit-properties-single-line-value value target)
    (let* ((source (document-text document))
           (semantic-value (edit-properties-semantic-value value))
           (expected (edit-canonical-copy (edit-canonical-document document))))
      (if keyword
          (multiple-value-bind (value-start value-end)
              (edit-properties-value-range document keyword :keyword target)
            (edit-canonical-set-property
             expected (edit-node-path document keyword) :value semantic-value)
            (edit-apply-patches
             document
             (list (edit-make-patch value-start value-end value target))
             :target target :expected expected))
          (let* ((insertion-offset (edit-properties-keyword-insertion-offset document))
                 (text (edit-properties-insertion-text
                        document insertion-offset
                        (list (format nil "#+~A: ~A" selector-key value))))
                 (expected (edit-properties-insert-keyword-expected
                            document selector-key semantic-value
                            insertion-offset target)))
            (edit-apply-patches
             document
             (list (edit-make-patch insertion-offset insertion-offset text target))
             :target target :expected expected))))))

(defun remove-keyword (document target)
  "Remove a keyword node or one unique document-level keyword selector."
  (multiple-value-bind (keyword)
      (edit-properties-resolve-keyword document target)
    (let ((start (node-begin keyword))
          (end (node-end keyword))
          (expected (edit-properties-remove-keyword-expected
                     document keyword target)))
      (unless (and (integerp start) (integerp end))
        (edit-signal-error target :uneditable-structure
                           "Keyword node has no source range"))
      (edit-apply-patches
       document (list (edit-make-patch start end "" target))
       :target target :expected expected))))

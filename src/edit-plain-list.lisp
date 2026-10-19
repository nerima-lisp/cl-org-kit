;;;; src/edit-plain-list.lisp
;;;; Format-preserving edits for plain-list items.
(in-package #:org-kit)

(defun edit-list-resolve-target (document target)
  (edit-list-check-document document)
  (let* ((node (edit-resolve-node document target '(:plain-list :item)))
         (list (if (eq (node-type node) :item)
                   (node-parent node)
                   node)))
    (unless (and list (eq (node-type list) :plain-list))
      (edit-signal-error target :uneditable-structure
                         "List item has no containing plain list"))
    (values list node)))

(defun edit-list-check-document (document)
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document))
  document)

(defun edit-list-line-end (source start)
  (let ((newline (position #\Newline source :start start)))
    (if newline
        (if (and (> newline start)
                 (char= (char source (1- newline)) #\Return))
            (1- newline)
            newline)
        (length source))))

(defun edit-list-line-ending (source offset)
  (let* ((before (and (plusp offset)
                      (position #\Newline source :end offset :from-end t)))
         (after (position #\Newline source :start offset)))
    (cond
      ((and before (plusp before) (char= (char source (1- before)) #\Return))
       (format nil "~C~C" #\Return #\Newline))
      (before (string #\Newline))
      ((and after (plusp after) (char= (char source (1- after)) #\Return))
       (format nil "~C~C" #\Return #\Newline))
      (after (string #\Newline))
      (t (string #\Newline)))))

(defun edit-list-line-indentation (source node)
  (let* ((start (node-post-affiliated node))
         (line-end (edit-list-line-end source start)))
    (reader-leading-space-count (subseq source start line-end))))

(defun edit-list-item-marker-span (document item target)
  (let* ((source (document-text document))
         (start (node-post-affiliated item))
         (line-end (edit-list-line-end source start))
         (marker-start
           (position-if-not #'reader-horizontal-space-p source
                            :start start :end line-end))
         (bullet (node-property item :bullet))
         (marker-end (and marker-start (stringp bullet)
                          (+ marker-start (length bullet)))))
    (unless (and marker-start (stringp bullet) (plusp (length bullet))
                 (<= marker-end line-end)
                 (string= bullet source :start2 marker-start :end2 marker-end))
      (edit-signal-error target :uneditable-structure
                         "List item marker does not match its source"))
    (values marker-start marker-end bullet)))

(defun edit-list-adjust-indentation (text amount target)
  (with-output-to-string (stream)
    (loop with cursor = 0
          while (< cursor (length text))
          for newline = (position #\Newline text :start cursor)
          for line-end = (if newline
                             (if (and (> newline cursor)
                                      (char= (char text (1- newline)) #\Return))
                                 (1- newline)
                                 newline)
                             (length text))
          for ending-end = (if newline (1+ newline) (length text))
          for indent-end = (loop for index from cursor below line-end
                                 while (reader-horizontal-space-p
                                        (char text index))
                                 finally (return index))
          for blank-p = (loop for index from cursor below line-end
                              always (reader-horizontal-space-p (char text index)))
          do (unless blank-p
               (if (minusp amount)
                   (when (< (- indent-end cursor) (- amount))
                     (edit-signal-error target :uneditable-structure
                                        "Item content cannot be unindented safely"))
                   (loop repeat amount
                         do (write-char #\Space stream))))
             (write-string text stream :start (if (and (minusp amount) (not blank-p))
                                                  (+ cursor (- amount))
                                                  cursor)
                                      :end line-end)
             (when newline
               (write-string text stream :start line-end :end ending-end))
             (setf cursor ending-end))))

(defun edit-list-preview-document (document patches target)
  (let* ((source (document-text document))
         (prepared (edit-prepare-patches document patches :target target))
         (normalized (edit-prepared-patches-patches prepared))
         (preview-source (edit-splice-source source normalized)))
    (edit-reparse-source document preview-source target)))

(defun edit-list-apply-structural-patches (document patches target expected
                                            &optional validator)
  (when validator
    (funcall validator (edit-list-preview-document document patches target)))
  (edit-apply-patches-result document patches :target target :expected expected))

(defun edit-list-find-node-at (document type offset)
  (let ((found nil))
    (map-nodes (lambda (node)
                 (when (and (eq (node-type node) type)
                            (= (node-post-affiliated node) offset))
                   (setf found node)))
               (document-root document))
    found))

(defun edit-list-assert-item-parent (document item-offset parent-type
                                    parent-offset target)
  (let* ((item (edit-list-find-node-at document :item item-offset))
         (parent (and item (node-parent item)))
         (expected-parent (edit-list-find-node-at document parent-type
                                                  parent-offset)))
    (unless (and parent expected-parent (eq parent expected-parent))
      (edit-signal-error target :unexpected-diff
                         "List item moved to an unexpected parent"))
    item))

(defun edit-list-assert-item-nested-under (document item-offset
                                           parent-item-offset target)
  (let* ((item (edit-list-find-node-at document :item item-offset))
         (list (and item (node-parent item)))
         (parent-item (edit-list-find-node-at document :item parent-item-offset)))
    (unless (and list (eq (node-type list) :plain-list)
                 parent-item (eq (node-parent list) parent-item))
      (edit-signal-error target :unexpected-diff
                         "List item was not nested under its preceding sibling"))
    item))

(defun edit-list-fragment (document raw-item target)
  (unless (and (stringp raw-item) (plusp (length raw-item))
               (not (find (char raw-item (1- (length raw-item)))
                          '(#\Return #\Newline))))
    (edit-signal-error target :invalid-value
                       "Inserted item must be nonempty and omit its final line ending"))
  (let* ((fragment
           (handler-case
               (parse-org raw-item
                          :settings (document-settings document)
                          :limits (document-limits document))
             (org-parse-error ()
               (edit-signal-error target :invalid-value
                                  "Inserted item is not valid Org list syntax"))))
         (root-children (node-children (document-root fragment)))
         (section (and (= (length root-children) 1)
                       (eq (node-type (first root-children)) :section)
                       (first root-children)))
         (elements (if section (node-children section) root-children))
         (list (and (= (length elements) 1)
                    (eq (node-type (first elements)) :plain-list)
                    (first elements)))
         (items (and list (node-children list)))
         (item (and (= (length items) 1) (first items))))
    (unless (and list item
                 (= (node-begin list) 0)
                 (= (node-end list) (length raw-item))
                 (= (node-begin item) 0)
                 (= (node-end item) (length raw-item)))
      (edit-signal-error target :invalid-value
                         "Inserted text must contain exactly one plain-list item"))
    (values fragment list item)))

(defun edit-list-insertion-text (source offset raw-item)
  (let* ((newline (edit-list-line-ending source offset))
         (prefix-p (and (plusp offset)
                        (not (char= (char source (1- offset)) #\Newline))))
         (suffix-p (or (< offset (length source))
                       (and (plusp (length source))
                            (char= (char source (1- (length source))) #\Newline))))
         (prefix (if prefix-p newline ""))
         (suffix (if suffix-p newline "")))
    (concatenate 'string prefix raw-item suffix)))

(defun insert-list-item (document target raw-item &key (position :after))
  "Insert one complete RAW-ITEM relative to TARGET and return an edit result.

RAW-ITEM is Org source for exactly one plain-list item without a final line
ending. TARGET is an item or plain list. For an item, POSITION is :BEFORE or
:AFTER; for a list, it is :FIRST or :LAST. The inserted item must use the same
list kind as its destination. The updated document is available from the
result, whose span identifies the zero-based insertion point in the original
source. Existing source bytes outside that point are retained. Invalid
fragments or positions signal ORG-EDIT-ERROR with reason :INVALID-VALUE; wrong
target types signal reason :WRONG-NODE-TYPE."
  (multiple-value-bind (list node) (edit-list-resolve-target document target)
    (let* ((items (node-children list))
           (node-is-list (eq node list))
           (allowed-positions (if node-is-list '(:first :last) '(:before :after)))
           (source (document-text document)))
      (unless (member position allowed-positions)
        (edit-signal-error target :invalid-value
                           "Invalid list item insertion position"))
      (multiple-value-bind (fragment fragment-list fragment-item)
          (edit-list-fragment document raw-item target)
        (unless (eq (node-property fragment-list :type) (node-property list :type))
          (edit-signal-error target :invalid-value
                             "Inserted item must match the destination list kind"))
        (let* ((fragment-indent (edit-list-line-indentation
                                 (document-text fragment) fragment-item))
               (list-indent (edit-list-line-indentation source (first items)))
               (shift (- list-indent fragment-indent))
               (adjusted (if (zerop shift)
                             raw-item
                             (edit-list-adjust-indentation raw-item shift target)))
               (index (if node-is-list
                          (if (eq position :first) 0 (length items))
                          (+ (position node items :test #'eq)
                             (if (eq position :after) 1 0))))
               (offset
                 (if node-is-list
                     (if (eq position :first)
                         (node-begin (first items))
                         (node-end (car (last items))))
                     (if (eq position :before)
                         (node-begin node)
                         (node-end node))))
               (expected (edit-canonical-copy (edit-canonical-document document)))
               (path (edit-node-path document list target))
               (canonical-list (edit-canonical-node-at-path expected path))
               (children (getf canonical-list :children))
               (canonical-item (edit-canonical-node fragment-item)))
          (edit-canonical-set-children
           expected path
           (append (subseq children 0 index)
                   (list canonical-item)
                   (nthcdr index children)))
          (edit-list-apply-structural-patches
           document
           (list (edit-make-patch offset offset
                                  (edit-list-insertion-text source offset adjusted)
                                  target))
           target expected))))))

(defun delete-list-item (document target)
  "Delete TARGET, including its nested list content, and return an edit result.

TARGET must be one list item. Source outside the item's half-open node span is
retained. The updated document and the item's original-source span are
available from the result. A wrong target type signals ORG-EDIT-ERROR with
reason :WRONG-NODE-TYPE; a node that cannot be edited signals reason
:UNEDITABLE-STRUCTURE."
  (edit-list-check-document document)
  (let* ((item (edit-resolve-node document target '(:item)))
         (start (node-begin item))
         (end (node-end item))
         (path (edit-node-path document item target))
         (parent-path (butlast path))
         (index (car (last path)))
         (expected (edit-canonical-copy (edit-canonical-document document))))
    (labels ((prune-empty (node-path)
               (when node-path
                 (let* ((node (edit-canonical-node-at-path expected node-path))
                        (type (getf node :type)))
                   (when (and (null (getf node :children))
                              (member type '(:plain-list :section)))
                     (let* ((ancestor-path (butlast node-path))
                            (ancestor (edit-canonical-node-at-path
                                       expected ancestor-path))
                            (ancestor-children (getf ancestor :children))
                            (node-index (car (last node-path))))
                       (edit-canonical-set-children
                        expected ancestor-path
                        (append (subseq ancestor-children 0 node-index)
                                (nthcdr (1+ node-index) ancestor-children)))
                       (prune-empty ancestor-path)))))))
      (let* ((parent (edit-canonical-node-at-path expected parent-path))
             (children (getf parent :children)))
        (edit-canonical-set-children
         expected parent-path
         (append (subseq children 0 index) (nthcdr (1+ index) children)))
        (prune-empty parent-path)))
    (edit-list-apply-structural-patches
     document (list (edit-make-patch start end "" target)) target expected)))

(defun edit-list-item-source-end (items item)
  (let* ((index (position item items :test #'eq))
         (next (nth (1+ index) items)))
    (if next (node-begin next) (node-end item))))

(defun edit-list-insert-relative (items target destination position)
  (let* ((remaining (remove target items :test #'eq))
         (destination-index (position destination remaining :test #'eq))
         (insert-index (+ destination-index (if (eq position :after) 1 0))))
    (append (subseq remaining 0 insert-index)
            (list target)
            (nthcdr insert-index remaining))))

(defun move-list-item (document target destination &key (position :before))
  "Move TARGET before or after DESTINATION in one plain list and return a result.

Both arguments must be sibling list items in the same list. Their exact source
chunks, including following inter-item whitespace, move together; source outside
that list is retained. POSITION must be :BEFORE or :AFTER. Invalid positions or
different containing lists signal ORG-EDIT-ERROR with reason :INVALID-VALUE;
wrong node types use reason :WRONG-NODE-TYPE. The result contains the updated
document and the original-source span for the list replacement; an unchanged
move returns an empty span list."
  (edit-list-check-document document)
  (let* ((item (edit-resolve-node document target '(:item)))
         (anchor (edit-resolve-node document destination '(:item)))
         (list (node-parent item)))
    (unless (member position '(:before :after))
      (edit-signal-error target :invalid-value "Move position must be :BEFORE or :AFTER"))
    (unless (and list (eq (node-type list) :plain-list)
                 (eq (node-parent anchor) list))
      (edit-signal-error target :invalid-value
                         "List items can only move within the same plain list"))
    (when (eq item anchor)
      (return-from move-list-item
        (edit-list-apply-structural-patches
         document nil target (edit-canonical-document document))))
    (let* ((items (node-children list))
           (ordered (edit-list-insert-relative items item anchor position))
           (source (document-text document))
           (newline (edit-list-line-ending source (node-begin list)))
           (replacement
             (with-output-to-string (stream)
               (loop for ordered-item in ordered
                     for index from 0
                     for chunk = (subseq source (node-begin ordered-item)
                                         (edit-list-item-source-end items ordered-item))
                     do (write-string chunk stream)
                        (when (and (< index (1- (length ordered)))
                                   (not (and (plusp (length chunk))
                                             (char= (char chunk (1- (length chunk)))
                                                    #\Newline))))
                          (write-string newline stream)))))
           (start (node-post-affiliated list))
           (end (node-end list))
           (replacement
             (if (and (= end (length source))
                      (not (document-final-newline-p document))
                      (>= (length replacement) (length newline))
                      (string= newline replacement
                               :start2 (- (length replacement) (length newline))))
                 (subseq replacement 0 (- (length replacement) (length newline)))
                 replacement))
           (expected (edit-canonical-copy (edit-canonical-document document)))
           (path (edit-node-path document list target))
           (canonical-items (getf (edit-canonical-node-at-path expected path)
                                  :children)))
      (edit-canonical-set-children
       expected path
       (mapcar (lambda (ordered-item)
                 (nth (position ordered-item items :test #'eq) canonical-items))
               ordered))
      (edit-list-apply-structural-patches
       document (list (edit-make-patch start end replacement target))
       target expected))))

(defun indent-list-item (document target &key (amount 2))
  "Indent TARGET by AMOUNT spaces beneath its preceding sibling item.

AMOUNT must be a positive integer and TARGET cannot be the first sibling. Every
nonblank line in the item span is shifted together, including nested content.
The result contains the updated document and the item's original-source span.
The operation signals ORG-EDIT-ERROR with reason :INVALID-VALUE for an invalid
amount and reason :UNEDITABLE-STRUCTURE when the item has no preceding sibling."
  (edit-list-check-document document)
  (let* ((item (edit-resolve-node document target '(:item)))
         (list (node-parent item))
         (items (and list (node-children list)))
         (index (position item items :test #'eq)))
    (unless (and (integerp amount) (> amount 0))
      (edit-signal-error target :invalid-value
                         "Indent amount must be a positive integer"))
    (unless (and list (eq (node-type list) :plain-list) index (> index 0))
      (edit-signal-error target :uneditable-structure
                         "The first item in a list cannot be indented"))
    (let* ((source (document-text document))
           (start (node-begin item))
           (end (node-end item))
           (replacement
             (edit-list-adjust-indentation
              (subseq source start end) amount target))
           (parent-item (nth (1- index) items))
           (expected (edit-canonical-copy (edit-canonical-document document)))
           (list-path (edit-node-path document list target))
           (canonical-list (edit-canonical-node-at-path expected list-path))
           (canonical-items (getf canonical-list :children))
           (canonical-item (nth index canonical-items))
           (remaining-items
             (append (subseq canonical-items 0 index)
                     (nthcdr (1+ index) canonical-items)))
           (parent-path (append list-path (list (1- index))))
           (canonical-parent (edit-canonical-node-at-path expected parent-path))
           (parent-children (getf canonical-parent :children))
           (last-child (car (last parent-children)))
           (list-type (node-property list :type)))
      (edit-canonical-set-children expected list-path remaining-items)
      (if (and last-child
               (eq (getf last-child :type) :plain-list)
               (eq (getf (getf last-child :properties) :type) list-type))
          (setf (getf last-child :children)
                (append (getf last-child :children) (list canonical-item)))
          (setf (getf canonical-parent :children)
                (append parent-children
                        (list (list :type :plain-list :post-blank 0
                                    :properties (list :type list-type)
                                    :children (list canonical-item))))))
      (edit-list-apply-structural-patches
       document (list (edit-make-patch start end replacement target))
       target expected
       (lambda (preview)
         (edit-list-assert-item-nested-under
          preview start (node-post-affiliated parent-item) target))))))

(defun unindent-list-item (document target)
  "Unindent an item from a nested list and return an edit result.

TARGET must be in a nested list that is the final content of its parent item.
It is moved after all remaining nested siblings before being outdented. This
source-order change keeps the remaining nested items under their original
parent. A non-nested or structurally ambiguous target signals ORG-EDIT-ERROR
with reason :UNEDITABLE-STRUCTURE. The result contains the updated document
and the replaced nested-list source span."
  (edit-list-check-document document)
  (let* ((item (edit-resolve-node document target '(:item)))
         (nested-list (node-parent item))
         (parent-item (and nested-list (node-parent nested-list)))
         (outer-list (and parent-item (node-parent parent-item)))
         (nested-items (and nested-list (node-children nested-list))))
    (unless (and nested-list (eq (node-type nested-list) :plain-list)
                 parent-item (eq (node-type parent-item) :item)
                 outer-list (eq (node-type outer-list) :plain-list)
                 (member item nested-items :test #'eq)
                 (eq nested-list (car (last (node-children parent-item)))))
      (edit-signal-error target :uneditable-structure
                         "Only an item in a final nested list can be unindented safely"))
    (let* ((source (document-text document))
           (amount (- (edit-list-line-indentation source item)
                      (edit-list-line-indentation source parent-item)))
           (start (node-begin nested-list))
           (end (node-end nested-list)))
      (unless (plusp amount)
        (edit-signal-error target :uneditable-structure
                           "Nested item has no removable indentation"))
      (let* ((item-index (position item nested-items :test #'eq))
             (remaining-items (remove item nested-items :test #'eq))
             (remaining-source
               (with-output-to-string (stream)
                 (dolist (remaining-item remaining-items)
                   (write-string source stream
                                 :start (node-begin remaining-item)
                                 :end (edit-list-item-source-end nested-items
                                                                 remaining-item)))))
             (target-source
               (edit-list-adjust-indentation
                (subseq source (node-begin item)
                        (edit-list-item-source-end nested-items item))
                (- amount) target))
             (replacement (concatenate 'string remaining-source target-source))
             (target-offset (+ start (length remaining-source)))
             (expected (edit-canonical-copy (edit-canonical-document document)))
             (nested-path (edit-node-path document nested-list target))
             (parent-path (butlast nested-path))
             (parent-index (car (last nested-path)))
             (canonical-nested (edit-canonical-node-at-path expected nested-path))
             (canonical-nested-items (getf canonical-nested :children))
             (canonical-item (nth item-index canonical-nested-items))
             (remaining-canonical
               (append (subseq canonical-nested-items 0 item-index)
                       (nthcdr (1+ item-index) canonical-nested-items)))
             (parent-canonical (edit-canonical-node-at-path expected parent-path))
             (outer-path (butlast parent-path))
             (parent-item-index (car (last parent-path)))
             (outer-canonical (edit-canonical-node-at-path expected outer-path))
             (outer-children (getf outer-canonical :children)))
        (if remaining-canonical
            (edit-canonical-set-children expected nested-path remaining-canonical)
            (edit-canonical-set-children
             expected parent-path
             (append (subseq (getf parent-canonical :children) 0 parent-index)
                     (nthcdr (1+ parent-index)
                             (getf parent-canonical :children)))))
        (edit-canonical-set-children
         expected outer-path
         (append (subseq outer-children 0 (1+ parent-item-index))
                 (list canonical-item)
                 (nthcdr (1+ parent-item-index) outer-children)))
        (edit-list-apply-structural-patches
         document
         (list (edit-make-patch start end replacement target))
         target expected
         (lambda (preview)
           (edit-list-assert-item-parent
            preview target-offset :plain-list
            (node-post-affiliated outer-list) target)))))))

(defun edit-list-bullet-spec (bullet target)
  (unless (and (stringp bullet) (plusp (length bullet)))
    (edit-signal-error target :invalid-value "List bullet must be a nonempty string"))
  (cond
    ((member bullet '("-" "+" "*") :test #'string=)
     (values :unordered bullet nil))
    (t
     (let ((digits-end (position-if-not #'digit-char-p bullet)))
       (unless (and digits-end (plusp digits-end)
                    (= digits-end (1- (length bullet)))
                    (find (char bullet digits-end) '(#\. #\))))
         (edit-signal-error target :invalid-value
                            "Bullet must be -, +, *, or a numbered marker"))
       (values :ordered (char bullet digits-end)
               (parse-integer bullet :end digits-end))))))

(defun edit-list-marker-separator (bullet target)
  (unless (and (stringp bullet) (plusp (length bullet))
               (reader-horizontal-space-p (char bullet (1- (length bullet)))))
    (edit-signal-error target :uneditable-structure
                       "List marker has no source separator"))
  (string (char bullet (1- (length bullet)))))

(defun edit-list-numbered-marker (number delimiter separator)
  (concatenate 'string (write-to-string number) (string delimiter) separator))

(defun edit-list-set-canonical-item-bullet (canonical-item bullet)
  (let ((properties (getf canonical-item :properties)))
    (setf (getf properties :bullet) bullet
          (getf canonical-item :properties) properties)))

(defun set-list-bullet (document target bullet)
  "Change every direct item marker in TARGET's plain list and return a result.

TARGET is a list or one of its items. BULLET is -, +, *, a number followed by
. or ), such as 1. or 1). Ordered numbering starts at the supplied number and
increases for each item, using an item's explicit [@N] counter as its number.
Existing checkboxes, descriptions, and marker spacing remain intact. Top-level
star bullets receive one leading space so the Org reader recognizes them as
list markers rather than headlines. Invalid markers signal ORG-EDIT-ERROR with
reason :INVALID-VALUE; unsupported source markers use reason
:UNEDITABLE-STRUCTURE. The result contains the updated document and the
original-source spans of changed markers."
  (multiple-value-bind (list node) (edit-list-resolve-target document target)
    (declare (ignore node))
    (multiple-value-bind (kind delimiter start-number)
        (edit-list-bullet-spec bullet target)
      (let* ((items (node-children list))
             (source (document-text document))
             (star-heading-space-p
               (and (eq kind :unordered)
                    (string= delimiter "*")
                    (zerop (edit-list-line-indentation source (first items)))))
             (patches '())
             (new-markers '())
             (number start-number))
        (loop for item in items
              do (when (and (eq kind :ordered)
                            (integerp (node-property item :counter)))
                   (setf number (node-property item :counter)))
                 (multiple-value-bind (start end old-bullet)
                     (edit-list-item-marker-span document item target)
                   (let* ((separator (edit-list-marker-separator old-bullet target))
                          (new-bullet
                            (if (eq kind :ordered)
                                (prog1 (edit-list-numbered-marker
                                        number delimiter separator)
                                  (incf number))
                                (concatenate 'string delimiter separator))))
                     (push new-bullet new-markers)
                     (unless (and (string= old-bullet new-bullet)
                                  (not star-heading-space-p))
                       (push (edit-make-patch
                              start end
                              (concatenate 'string
                                           (if star-heading-space-p " " "")
                                           new-bullet)
                              target)
                             patches)))))
        (let* ((expected (edit-canonical-copy (edit-canonical-document document)))
               (path (edit-node-path document list target))
               (canonical-list (edit-canonical-node-at-path expected path))
               (canonical-items (getf canonical-list :children))
               (new-type (if (eq (node-property list :type) :descriptive)
                             :descriptive
                             (if (eq kind :ordered) :ordered :unordered))))
          (edit-canonical-set-property expected path :type new-type)
          (loop for canonical-item in canonical-items
                for new-bullet in (nreverse new-markers)
                do (edit-list-set-canonical-item-bullet canonical-item new-bullet))
          (edit-apply-patches-result document (nreverse patches)
                                     :target target :expected expected))))))

(defun edit-list-ordered-marker-details (bullet target)
  (let* ((separator (and (stringp bullet) (plusp (length bullet))
                         (char bullet (1- (length bullet)))))
         (token-end (and separator (reader-horizontal-space-p separator)
                         (1- (length bullet))))
         (delimiter-position (and token-end (plusp token-end) (1- token-end)))
         (digits-p (and delimiter-position (plusp delimiter-position)
                        (loop for index below delimiter-position
                              always (digit-char-p (char bullet index))))))
    (unless (and digits-p
                 (find (char bullet delimiter-position) '(#\. #\))))
      (edit-signal-error target :uneditable-structure
                         "Ordered item has an invalid numbering marker"))
    (values (parse-integer bullet :end delimiter-position)
            (char bullet delimiter-position)
            (string separator))))

(defun renumber-ordered-list (document target &key (start 1))
  "Renumber every item in an ordered plain list and return an edit result.

TARGET is the ordered list or one of its items. START must be a nonnegative
integer. Each item's period or closing parenthesis and source spacing are
preserved. An item's explicit [@N] counter sets its number and the following
item continues from that value. Checkboxes, descriptions, and counters remain
intact. An unordered target or invalid START signals ORG-EDIT-ERROR with reason
:INVALID-VALUE; malformed ordered markers use reason :UNEDITABLE-STRUCTURE. The
result contains the updated document and the original-source spans of changed
markers."
  (multiple-value-bind (list node) (edit-list-resolve-target document target)
    (declare (ignore node))
    (unless (eq (node-property list :type) :ordered)
      (edit-signal-error target :invalid-value "Target is not an ordered list"))
    (unless (and (integerp start) (>= start 0))
      (edit-signal-error target :invalid-value "Start must be a nonnegative integer"))
    (let* ((items (node-children list))
           (expected (edit-canonical-copy (edit-canonical-document document)))
           (path (edit-node-path document list target))
           (canonical-items (getf (edit-canonical-node-at-path expected path)
                                  :children))
           (patches '())
           (number start))
      (loop for item in items
            for canonical-item in canonical-items
            do (when (integerp (node-property item :counter))
                 (setf number (node-property item :counter)))
               (multiple-value-bind (marker-start marker-end old-bullet)
                   (edit-list-item-marker-span document item target)
                 (multiple-value-bind (old-number delimiter separator)
                     (edit-list-ordered-marker-details old-bullet target)
                   (declare (ignore old-number))
                   (let ((new-bullet
                           (edit-list-numbered-marker number delimiter separator)))
                     (incf number)
                     (edit-list-set-canonical-item-bullet canonical-item new-bullet)
                     (unless (string= old-bullet new-bullet)
                       (push (edit-make-patch marker-start marker-end new-bullet target)
                             patches))))))
      (edit-apply-patches-result document (nreverse patches)
                                 :target target :expected expected))))

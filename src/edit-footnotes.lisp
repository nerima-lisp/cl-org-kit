;;;; src/edit-footnotes.lisp
;;;; Format-preserving footnote insertion, renumbering and normalization.
(in-package #:org-kit)

(defun edit-footnote-nodes (document type)
  (let ((nodes '()))
    (map-nodes (lambda (node)
                 (when (eq (node-type node) type)
                   (push node nodes)))
               (document-root document))
    (nreverse nodes)))

(defun edit-footnote-labels (document type)
  (remove nil (mapcar (lambda (node) (node-property node :label))
                      (edit-footnote-nodes document type))))

(defun edit-footnote-unique-labels (labels)
  (remove-duplicates labels :from-end t :test #'string=))

(defun edit-footnote-diagnostics (document)
  (let* ((references (edit-footnote-unique-labels
                      (edit-footnote-labels document :footnote-reference)))
         (definitions (edit-footnote-unique-labels
                       (edit-footnote-labels document :footnote-definition))))
    (values (remove-if (lambda (label) (member label definitions :test #'string=))
                       references)
            (remove-if (lambda (label) (member label references :test #'string=))
                       definitions))))

(defun edit-footnote-definition-source-end (document node)
  (let ((source (document-text document))
        (end (node-end node)))
    (dotimes (count (node-post-blank node) end)
      (let* ((newline (position #\Newline source :end end :from-end t))
             (previous-newline
               (and newline
                    (position #\Newline source :end newline :from-end t))))
        (setf end (if previous-newline
                      (1+ previous-newline)
                      0))))))
(defun edit-footnote-canonical-insert-paragraph-reference
    (source paragraph offset reference prefix-p suffix-p)
  (let ((children '())
        (inserted nil))
    (labels ((append-child (child)
               (if (and children child
                        (eq (getf child :type) :plain-text)
                        (eq (getf (car (last children)) :type) :plain-text))
                   children
                   (append children (list child))))
             (insert-reference ()
               (when prefix-p
                 (setf children
                       (append-child
                        (list :type :plain-text :post-blank 0
                              :properties nil :children nil))))
               (setf children (append-child reference))
               (when suffix-p
                 (setf children
                       (append-child
                        (list :type :plain-text :post-blank 0
                              :properties nil :children nil))))
               (setf inserted t)))
      (loop for child in (node-children paragraph)
            for canonical = (edit-canonical-node child)
            do (cond
                 ((and (not inserted)
                       (eq (node-type child) :plain-text)
                       (<= (node-begin child) offset (node-end child)))
                  (let ((before (subseq source (node-begin child) offset))
                        (after (subseq source offset (node-end child))))
                    (when (or (plusp (length before)) prefix-p)
                      (setf children
                            (append-child
                             (list :type :plain-text :post-blank 0
                                   :properties nil :children nil))))
                    (setf children (append-child reference))
                    (when (or (plusp (length after)) suffix-p)
                      (setf children
                            (append-child
                             (list :type :plain-text :post-blank 0
                                   :properties nil :children nil))))
                    (setf inserted t)))
                 ((and (not inserted) (<= offset (node-begin child)))
                  (insert-reference)
                  (setf children (append-child canonical)))
                 (t
                  (setf children (append-child canonical)))))
      (unless inserted (insert-reference)))
    children))
(defun edit-footnote-canonical-insert-title-reference
    (headline offset reference)
  (let* ((range (node-property headline :reader-title-range))
         (cursor (car range))
         (segments '())
         (inserted nil))
    (labels ((append-segment (segment)
               (if (and (stringp segment) segments
                        (stringp (car (last segments))))
                   (setf (car (last segments))
                         (concatenate 'string (car (last segments)) segment))
                   (setf segments (append segments (list segment)))))
             (insert-reference ()
               (append-segment reference)
               (setf inserted t)))
      (dolist (part (node-property headline :title))
        (if (stringp part)
            (let ((end (+ cursor (length part))))
              (if (and (not inserted) (<= cursor offset end))
                  (let ((split (- offset cursor)))
                    (when (plusp split)
                      (append-segment (subseq part 0 split)))
                    (insert-reference)
                    (when (< split (length part))
                      (append-segment (subseq part split))))
                  (append-segment part))
              (setf cursor end))
            (progn
              (when (and (not inserted) (<= offset (node-begin part)))
                (insert-reference))
              (append-segment (edit-canonical-node part))
              (setf cursor (node-end part)))))
      (unless inserted
        (insert-reference)))
    segments))

(defun edit-footnote-line-ending (source)
  (let ((newline (position #\Newline source)))
    (if (and newline (plusp newline)
             (char= (char source (1- newline)) #\Return))
        (format nil "~C~C" #\Return #\Newline)
        (string #\Newline))))

(defun edit-footnote-line-start-p (source offset)
  (or (zerop offset)
      (char= (char source (1- offset)) #\Newline)))

(defun edit-footnote-numeric-label-p (label)
  (and (stringp label) (plusp (length label))
       (every #'digit-char-p label)))

(defun edit-footnote-next-label (document)
  (let ((maximum 0))
    (dolist (label (append (edit-footnote-labels document :footnote-reference)
                           (edit-footnote-labels document :footnote-definition)))
      (when (edit-footnote-numeric-label-p label)
        (setf maximum (max maximum (parse-integer label)))))
    (write-to-string (1+ maximum))))

(defun edit-footnote-label-patches (nodes labels target) (loop for node in nodes for label = (node-property node :label) for replacement = (and label (gethash label labels)) for label-start = (or (node-post-affiliated node) (node-begin node)) when (and replacement (not (string= label replacement))) collect (edit-make-patch (+ label-start 4) (+ label-start 4 (length label)) replacement target)))

(defun edit-footnote-report (result)
  (multiple-value-bind (undefined unreferenced)
      (edit-footnote-diagnostics (edit-result-updated-document result))
    (values result undefined unreferenced)))

(defun insert-footnote (document offset body)
  "Insert a numbered footnote reference at OFFSET and append BODY as its definition.

Return an EDIT-RESULT, the assigned label, undefined reference labels, and
unreferenced definition labels, in that order. BODY is raw Org content without
a trailing line ending. OFFSET must be in paragraph text or a headline title."
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document))
  (unless (and (integerp offset) (<= 0 offset (length (document-text document))))
    (edit-signal-error offset :invalid-value "Footnote insertion offset is invalid"))
  (when (and (plusp offset)
             (< offset (length (document-text document)))
             (char= (char (document-text document) (1- offset)) #\Return)
             (char= (char (document-text document) offset) #\Newline))
    (edit-signal-error offset :invalid-value "Footnote insertion splits a CRLF line ending"))
  (unless (and (stringp body) (plusp (length body))
               (not (or (char= (char body (1- (length body))) #\Newline)
                        (char= (char body (1- (length body))) #\Return))))
    (edit-signal-error "footnote body" :invalid-value
                       "Footnote body must be a non-empty string without a trailing line ending"))
  (let* ((source (document-text document))
         (body-fragment
           (handler-case
               (parse-org body :settings (document-settings document)
                               :limits (document-limits document))
             (org-parse-error ()
               (edit-signal-error "footnote body" :invalid-value
                                  "Footnote body is not valid Org content"))))
         (label (edit-footnote-next-label document))
         (newline (edit-footnote-line-ending source))
         (reference (format nil "[fn:~A]" label))
         (line-start-p (edit-footnote-line-start-p source offset))
         (reference-text (concatenate 'string
                                      (if line-start-p " " "")
                                      reference
                                      (if line-start-p newline "")))
         (definition-text
           (concatenate 'string
                        (if (= offset (length source))
                            (if line-start-p "" newline)
                            (if (and (plusp (length source))
                                     (not (char= (char source (1- (length source)))
                                                 #\Newline)))
                                newline ""))
                        "[fn:" label "] " body
                        (if (document-final-newline-p document) newline "")))
         (paragraph nil)
         (headline nil))
    (let ((contains-headline nil))
      (map-nodes (lambda (node)
                   (when (eq (node-type node) :headline)
                     (setf contains-headline t)))
                 (document-root body-fragment))
      (when contains-headline
        (edit-signal-error "footnote body" :invalid-value
                           "Footnote body cannot contain a headline")))
    (map-nodes
     (lambda (node)
       (when (and (eq (node-type node) :paragraph)
                  (node-contents-begin node)
                  (<= (node-contents-begin node) offset
                      (node-contents-end node)))
         (setf paragraph node))
       (when (and (eq (node-type node) :headline)
                  (let ((range (node-property node :reader-title-range)))
                    (and range (<= (car range) offset (cdr range)))))
         (setf headline node)))
     (document-root document))
    (unless (or paragraph headline
                (and (zerop offset) (zerop (length source))))
      (edit-signal-error offset :invalid-value
                         "Footnote references can only be inserted in paragraph text or a headline title"))
    (when (or
           (and paragraph
                (some (lambda (node)
                        (and (not (eq (node-type node) :plain-text))
                             (< (node-begin node) offset (node-end node))))
                      (node-children paragraph)))
           (and headline
                (some (lambda (part)
                        (and (node-p part)
                             (< (node-begin part) offset (node-end part))))
                      (node-property headline :title))))
      (edit-signal-error offset :invalid-value
                         "Footnote insertion cannot split an inline object"))
    (let* ((reference-canonical
             (list :type :footnote-reference :post-blank 0
                   :properties (list :label label :type :standard) :children nil))
           (body-root (node-children (document-root body-fragment)))
           (body-section (and (= (length body-root) 1)
                              (eq (node-type (first body-root)) :section)
                              (first body-root)))
           (definition-canonical
             (list :type :footnote-definition :post-blank 0
                   :properties (list :label label)
                   :children (mapcar #'edit-canonical-node
                                     (if body-section
                                         (node-children body-section)
                                         body-root))))
           (expected (edit-canonical-copy (edit-canonical-document document))))
      (cond
        (paragraph
         (edit-canonical-set-children
          expected (edit-node-path document paragraph offset)
          (edit-footnote-canonical-insert-paragraph-reference
           source paragraph offset reference-canonical line-start-p line-start-p)))
        (headline
         (edit-canonical-set-property
          expected (edit-node-path document headline offset) :title
          (edit-footnote-canonical-insert-title-reference
           headline offset reference-canonical)))
        (t
         (let ((plain-text (list :type :plain-text :post-blank 0
                                 :properties nil :children nil)))
           (setf (getf expected :children)
                 (list (list :type :section :post-blank 0 :properties nil
                             :children
                             (list
                              (list :type :paragraph :post-blank 0
                                    :properties nil
                                    :children (list plain-text
                                                    reference-canonical
                                                    plain-text)))))))))
      (let* ((sections (edit-footnote-nodes document :section))
             (destination
               (car (last
                     (remove-if-not
                      (lambda (node) (= (node-end node) (length source)))
                      sections))))
             (headlines (edit-footnote-nodes document :headline))
             (last-headline
               (car (last
                     (remove-if-not
                      (lambda (node) (= (node-end node) (length source)))
                      headlines)))))
        (cond
          ((zerop (length source))
           (let* ((section (first (getf expected :children)))
                  (children (getf section :children)))
             (setf (getf section :children)
                   (append children (list definition-canonical)))))
          (destination
           (let* ((path (edit-node-path document destination offset))
                  (section (edit-canonical-node-at-path expected path)))
             (setf (getf section :children)
                   (append (getf section :children)
                           (list definition-canonical)))))
          (last-headline
           (let* ((path (edit-node-path document last-headline offset))
                  (node (edit-canonical-node-at-path expected path)))
             (setf (getf node :children)
                   (append (getf node :children)
                           (list (list :type :section :post-blank 0
                                       :properties nil
                                       :children (list definition-canonical)))))))
          (t
           (setf (getf expected :children)
                 (append (getf expected :children)
                         (list (list :type :section :post-blank 0
                                     :properties nil
                                     :children (list definition-canonical)))))))
      (let* ((patches
               (if (= offset (length source))
                   (list (edit-make-patch
                          offset offset
                          (concatenate 'string reference-text definition-text)
                          offset))
                   (list (edit-make-patch offset offset reference-text offset)
                         (edit-make-patch (length source) (length source)
                                          definition-text offset))))
             (result (edit-apply-patches-result
                      document patches :target offset :expected expected)))
        (multiple-value-bind (undefined unreferenced)
            (edit-footnote-diagnostics (edit-result-updated-document result))
          (values result label undefined unreferenced)))))))

(defun renumber-footnotes (document)
  "Renumber numeric footnote labels by first reference, then unused definition order.

Return an EDIT-RESULT followed by undefined reference labels and unreferenced
definition labels from the updated document. Named labels remain unchanged."
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document))
  (let* ((references (edit-footnote-nodes document :footnote-reference))
         (definitions (edit-footnote-nodes document :footnote-definition))
         (ordered-labels '())
         (labels (make-hash-table :test #'equal))
         (expected (edit-canonical-copy (edit-canonical-document document))))
    (dolist (node references)
      (let ((label (node-property node :label)))
        (when (and (edit-footnote-numeric-label-p label)
                   (not (member label ordered-labels :test #'string=)))
          (setf ordered-labels (append ordered-labels (list label))))))
    (dolist (node definitions)
      (let ((label (node-property node :label)))
        (when (and (edit-footnote-numeric-label-p label)
                   (not (member label ordered-labels :test #'string=)))
          (setf ordered-labels (append ordered-labels (list label))))))
    (loop for label in ordered-labels
          for number from 1
          do (setf (gethash label labels) (write-to-string number)))
    (dolist (node (append references definitions))
      (let* ((label (node-property node :label))
             (replacement (and label (gethash label labels))))
        (when replacement
          (edit-canonical-set-property
           expected (edit-node-path document node "footnotes")
           :label replacement))))
    (let* ((patches (append (edit-footnote-label-patches
                             references labels "footnotes")
                            (edit-footnote-label-patches
                             definitions labels "footnotes")))
           (result (edit-apply-patches-result document patches
                                              :target "footnotes" :expected expected)))
      (edit-footnote-report result))))

(defun edit-footnote-normalization-order (document definitions)
  (let ((reference-order (edit-footnote-unique-labels
                          (edit-footnote-labels document :footnote-reference))))
    (stable-sort
     (copy-list definitions)
     (lambda (left right)
       (let* ((left-position (position (node-property left :label)
                                       reference-order :test #'string=))
              (right-position (position (node-property right :label)
                                        reference-order :test #'string=)))
         (cond ((and left-position right-position) (< left-position right-position))
               (left-position t)
               (right-position nil)
               (t (< (node-begin left) (node-begin right)))))))))

(defun normalize-footnotes (document)
  "Move existing footnote definitions to the document end in reference order.

Unreferenced definitions follow in their original order. This operation only
relocates existing definitions; it does not renumber labels or convert inline
or anonymous footnotes. Return an EDIT-RESULT followed by undefined reference
labels and unreferenced definition labels from the updated document."
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document))
  (let* ((source (document-text document))
         (definitions (edit-footnote-nodes document :footnote-definition))
         (ordered (edit-footnote-normalization-order document definitions)))
    (if (null definitions)
        (edit-footnote-report
         (edit-apply-patches-result
          document nil :target "footnotes"
          :expected (edit-canonical-document document)))
      (let* ((moved-text
              (with-output-to-string (stream)
                (loop for node in ordered
                      for index from 0
                      for start = (node-begin node)
                      for end = (edit-footnote-definition-source-end document node)
                      for chunk = (subseq source start end)
                      do (write-string chunk stream)
                      (when (and (< index (1- (length ordered)))
                                 (plusp (length chunk))
                                 (not (char= (char chunk (1- (length chunk)))
                                             #\Newline)))
                        (write-string (edit-footnote-line-ending source)
                                      stream)))))
             (deletions
              (mapcar (lambda (node)
                        (edit-make-patch
                         (node-begin node)
                         (edit-footnote-definition-source-end document node)
                         "" "footnotes"))
                      definitions))
             (prepared-deletions
              (edit-prepare-patches document deletions :target "footnotes"))
             (retained-source
              (edit-splice-source
               source (edit-prepared-patches-patches prepared-deletions)))
             (prefix
              (if (and (plusp (length retained-source))
                       (not (char= (char retained-source
                                         (1- (length retained-source)))
                                   #\Newline)))
                  (edit-footnote-line-ending source)
                ""))
             (patches
              (append deletions
                      (list (edit-make-patch
                             (length source) (length source)
                             (concatenate 'string prefix moved-text)
                             "footnotes"))))
             (prepared-patches
              (edit-prepare-patches document patches :target "footnotes"))
             (new-source (edit-splice-source source
                                             (edit-prepared-patches-patches
                                              prepared-patches)))
             (effective-patches (if (string= source new-source) nil patches)))
        (labels ((without-definitions (canonical)
                                      (if (eq (getf canonical :type) :footnote-definition)
                                          nil
                                        (let ((copy (copy-tree canonical))
                                              (children '()))
                                          (dolist (child (getf canonical :children))
                                            (if (eq (getf child :type) :footnote-definition)
                                                (when children
                                                  (incf (getf (car (last children)) :post-blank)
                                                        (getf child :post-blank)))
                                              (let ((cleaned (without-definitions child)))
                                                (when cleaned
                                                  (setf children
                                                        (append children (list cleaned)))))))
                                          (setf (getf copy :children) children)
                                          (unless (and (eq (getf copy :type) :section)
                                                       (null children))
                                            copy))))
                 (find-last-node (tree type)
                                 (let ((found nil))
                                   (labels ((walk (node)
                                                  (when (eq (getf node :type) type)
                                                    (setf found node))
                                                  (dolist (child (getf node :children))
                                                    (walk child))))
                                     (walk tree))
                                   found))
                 (expected-tree ()
                                (let* ((expected
                                        (without-definitions
                                         (edit-canonical-document document)))
                                       (canonical-definitions
                                        (mapcar (lambda (node)
                                                  (let ((canonical (edit-canonical-node node)))
                                                    (setf (getf canonical :post-blank) 0)
                                                    canonical))
                                                ordered))
                                       (headline (find-last-node expected :headline)))
                                  (if headline
                                      (let ((section
                                             (find :section (getf headline :children)
                                                   :key (lambda (node) (getf node :type)))))
                                        (if section
                                            (setf (getf section :children)
                                                  (append (getf section :children)
                                                          canonical-definitions))
                                          (setf (getf headline :children)
                                                (append (getf headline :children)
                                                        (list
                                                         (list :type :section :post-blank 0
                                                               :properties nil
                                                               :children
                                                               canonical-definitions))))))
                                    (let ((section (find-last-node expected :section)))
                                      (if section
                                          (setf (getf section :children)
                                                (append (getf section :children)
                                                        canonical-definitions))
                                        (setf (getf expected :children)
                                              (append (getf expected :children)
                                                      (list
                                                       (list :type :section :post-blank 0
                                                             :properties nil
                                                             :children
                                                             canonical-definitions)))))))
                                  expected)))
          (let* ((expected (if effective-patches
                               (expected-tree)
                             (edit-canonical-document document)))
                 (result (edit-apply-patches-result
                          document effective-patches
                          :target "footnotes" :expected expected)))
            (edit-footnote-report result)))))))

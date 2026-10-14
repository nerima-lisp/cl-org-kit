;;;; src/edit-relocate.lisp
;;;;
;;;; Format-preserving movement and archival of headline subtrees.
(in-package #:org-kit)

(defstruct (relocation-result
            (:constructor make-relocation-result
                (&key source-edit target-edit mode source-path target-path))
            (:conc-name relocation-result-)
            (:copier nil)
            (:predicate relocation-result-p))
  "The validated edit results and heading paths for a relocation."
  (source-edit nil :read-only t)
  (target-edit nil :read-only t)
  (mode nil :read-only t)
  (source-path nil :read-only t)
  (target-path nil :read-only t))

(eval-when (:load-toplevel :execute)
  (setf (documentation 'make-relocation-result 'function)
        "Return a relocation result containing the validated edits and paths."
        (documentation 'relocation-result-p 'function)
        "Return true when OBJECT is a relocation result."
        (documentation 'relocation-result-source-edit 'function)
        "Return RESULT's source edit, or NIL when the operation copied a subtree."
        (documentation 'relocation-result-target-edit 'function)
        "Return RESULT's target edit."
        (documentation 'relocation-result-mode 'function)
        "Return RESULT's :MOVE or :COPY mode."
        (documentation 'relocation-result-source-path 'function)
        "Return the source heading path before the relocation."
        (documentation 'relocation-result-target-path 'function)
        "Return the inserted heading path in the updated target document."))

(defun edit-relocate-check-document (document)
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document)))

(defun edit-relocate-node-at-path (document path)
  (loop with current = (document-root document)
        for index in path
        do (setf current
                  (loop for child in (node-children current)
                        for child-index from 0
                        when (= child-index index)
                          return child))
        finally (return current)))

(defun edit-relocate-list-item-at (items index)
  (loop for item in items
        for position from 0
        when (= position index)
          return item))

(defun edit-relocate-adjust-path-after-removal (path removed-path)
  (let* ((removed-parent-path (butlast removed-path))
         (common
           (loop for path-index in path
                 for parent-index in removed-parent-path
                 while (= path-index parent-index)
                 count t)))
    (if (and (= common (length removed-parent-path))
             (< common (length path))
             (> (edit-relocate-list-item-at path common)
                (edit-relocate-list-item-at removed-path common)))
        (loop for index in path for position from 0
              collect (if (= position common) (1- index) index))
        path)))

(defun edit-relocate-heading-ancestor-p (ancestor heading)
  (loop for current = heading then (node-parent current)
        while current
        thereis (eq current ancestor)))

(defun edit-relocate-direct-headlines (parent)
  (remove-if-not (lambda (node) (eq (node-type node) :headline))
                 (node-children parent)))

(defun edit-relocate-swap-children (children first-index second-index)
  (loop for child in children
        for index from 0
        collect (cond ((= index first-index)
                       (edit-relocate-list-item-at children second-index))
                      ((= index second-index)
                       (edit-relocate-list-item-at children first-index))
                      (t child))))

(defun move-subtree (document heading &key direction)
  "Move HEADING one sibling position UP or DOWN and return its edit result."
  (edit-relocate-check-document document)
  (let* ((headline (edit-resolve-node document heading '(:headline)))
         (parent (node-parent headline))
         (siblings (edit-relocate-direct-headlines parent))
         (sibling-index (position headline siblings :test #'eq)))
    (unless (member direction '(:up :down))
      (edit-signal-error heading :invalid-value
                         "Subtree direction must be :UP or :DOWN"))
    (let* ((neighbor-index (if (eq direction :up)
                               (1- sibling-index)
                               (1+ sibling-index)))
           (neighbor (and (<= 0 neighbor-index)
                          (< neighbor-index (length siblings))
                          (edit-relocate-list-item-at siblings
                                                      neighbor-index))))
      (unless neighbor
        (edit-signal-error heading :invalid-value
                           "No adjacent heading exists in that direction"))
      (multiple-value-bind (headline-start headline-end)
          (edit-subtree-safe-span document headline heading)
        (multiple-value-bind (neighbor-start neighbor-end)
            (edit-subtree-safe-span document neighbor heading)
          (let* ((source (document-text document))
                 (first (if (eq direction :up) neighbor headline))
                 (second (if (eq direction :up) headline neighbor))
                 (first-start (if (eq direction :up)
                                  neighbor-start headline-start))
                 (first-end (if (eq direction :up)
                                neighbor-end headline-end))
                 (second-start (if (eq direction :up)
                                   headline-start neighbor-start))
                 (second-end (if (eq direction :up)
                                 headline-end neighbor-end))
                 (first-index (position first (node-children parent) :test #'eq))
                 (second-index (position second (node-children parent) :test #'eq))
                 (path (edit-node-path document parent heading))
                 (expected (edit-canonical-copy
                            (edit-canonical-document document)))
                 (children (edit-subtree-canonical-children expected path))
                 (second-slice (subseq source second-start second-end))
                 (first-slice (subseq source first-start first-end))
                 (untrimmed-replacement
                   (concatenate 'string second-slice
                                (if (char= (char second-slice
                                                (1- (length second-slice)))
                                           #\Newline)
                                    ""
                                    (edit-subtree-fragment-separator
                                     (edit-subtree-line-ending document
                                                               first-start)
                                     (edit-subtree-trailing-headline-cr-p
                                      document second-start second-end)))
                                first-slice))
                 (final-crlf-p
                   (and (>= (length first-slice) 2)
                        (char= (char first-slice
                                     (- (length first-slice) 2))
                               #\Return)
                        (char= (char first-slice
                                     (1- (length first-slice)))
                               #\Newline)))
                 (preserved-trailing-headline
                   (and (= second-end (length source))
                        (not (document-final-newline-p document))
                        final-crlf-p
                        (edit-relocate-final-fragment-headline
                         document first-start first-end)))
                 (replacement
                   (if (and (= second-end (length source))
                            (not (document-final-newline-p document)))
                       (if (and preserved-trailing-headline
                                (null (node-property
                                       preserved-trailing-headline :tags)))
                           ;; Retain the CR because Org includes it in an untagged CRLF title.
                           (subseq untrimmed-replacement
                                   0 (1- (length untrimmed-replacement)))
                           (string-right-trim '(#\Return #\Newline)
                                              untrimmed-replacement))
                       untrimmed-replacement))
                 (patch
                   (edit-make-patch first-start second-end replacement heading)))
            (edit-canonical-set-children
             expected path
             (edit-relocate-swap-children children first-index second-index))
            (when (and preserved-trailing-headline
                       (null (node-property
                              preserved-trailing-headline :tags)))
              (let* ((original-path
                       (edit-node-path document preserved-trailing-headline))
                     (trailing-path
                       (append path (list second-index)
                               (nthcdr (1+ (length path)) original-path))))
                (edit-relocate-sync-trailing-headline-properties
                 expected document source patch trailing-path heading)))
            (edit-apply-patches-result
             document
             (list patch)
             :target heading :expected expected)))))))

(defun edit-relocate-check-todo-compatibility (source-document heading
                                               target-document target)
  (map-nodes
   (lambda (node)
     (when (and (eq (node-type node) :headline)
                (node-property node :todo-keyword))
       (let* ((keyword (node-property node :todo-keyword))
              (source-entry
                (assoc keyword (getf (document-settings source-document)
                                     :todo-keywords)
                      :test #'string=))
              (target-entry
                (assoc keyword (getf (document-settings target-document)
                                     :todo-keywords)
                      :test #'string=)))
         (unless (and source-entry target-entry
                      (eql (cdr source-entry) (cdr target-entry)))
           (edit-signal-error target :uneditable-structure
                              "Target settings assign a different TODO meaning")))))
   heading))

(defun edit-relocate-destination-level (target-heading)
  (if target-heading
      (1+ (node-property target-heading :level))
      1))

(defun edit-relocate-relevel-patches (source-document heading start
                                      target-document target-heading)
  (let ((base-level (node-property heading :level))
        (destination-level (edit-relocate-destination-level target-heading))
        (patches nil)
        (max-level 0))
    (map-nodes
     (lambda (node)
       (when (eq (node-type node) :headline)
         (let* ((logical-level (node-property node :level))
                (new-level (+ destination-level (- logical-level base-level))))
           (setf max-level (max max-level new-level))
           (multiple-value-bind (marker-start marker-end stars)
               (edit-subtree-marker source-document node)
             (declare (ignore stars))
             (push (edit-make-patch
                    (- marker-start start) (- marker-end start)
                    (make-string (source-heading-level target-document new-level)
                                 :initial-element #\*)
                    heading)
                   patches)))))
     heading)
    (check-limit (document-limits target-document) :max-heading-depth max-level)
    (nreverse patches)))

(defun edit-relocate-effective-inherited-tags (document heading)
  (let ((tags nil))
    (dolist (tag (getf (document-settings document) :file-tags))
      (setf tags (effective-tag-add tags tag)))
    (dolist (ancestor (reverse (effective-heading-ancestors heading)))
      (dolist (tag (node-property ancestor :tags))
        (setf tags (effective-tag-add tags tag))))
    tags))

(defun edit-relocate-archive-context (document heading)
  (let* ((ancestors (reverse (effective-heading-ancestors heading)))
         (olpath (and ancestors
                      (format nil "~{~A~^/~}"
                              (mapcar (lambda (node)
                                        (node-property node :raw-value ""))
                                      ancestors))))
         (file (document-source-name document))
         (category (multiple-value-bind (value source)
                       (effective-property document heading "CATEGORY"
                                           :inherit t)
                     (declare (ignore source))
                     (or value (getf (document-settings document) :category)
                         (and (stringp file)
                              (pathname-name (pathname file))))))
         (todo (node-property heading :todo-keyword))
         (inherited-tags (edit-relocate-effective-inherited-tags document
                                                                  heading)))
    (remove nil
            (list (and (stringp file) (cons "ARCHIVE_FILE" file))
                  (and (stringp olpath) (plusp (length olpath))
                       (cons "ARCHIVE_OLPATH" olpath))
                  (and (stringp category) (plusp (length category))
                       (cons "ARCHIVE_CATEGORY" category))
                  (and (stringp todo) (plusp (length todo))
                       (cons "ARCHIVE_TODO" todo))
                  (and inherited-tags
                       (cons "ARCHIVE_ITAGS"
                             (format nil "~{~A~^ ~}" inherited-tags)))))))

(defun edit-relocate-root-property-drawers (heading)
  (loop for child in (node-children heading)
        when (eq (node-type child) :section)
          append (remove-if-not
                  (lambda (node) (eq (node-type node) :property-drawer))
                  (node-children child))))

(defun edit-relocate-property-line-value-start (source start end)
  (let ((separator (position #\: source :start (1+ start) :end end)))
    (loop for position from (1+ separator) below end
          while (reader-heading-horizontal-space-p (char source position))
          finally (return position))))

(defun edit-relocate-drawer-close-offset (source drawer target)
  (let ((start (node-begin drawer))
        (end (node-end drawer)))
    (loop for line-start = start then (1+ newline)
          for newline = (position #\Newline source :start line-start :end end)
          for line-end = (or newline end)
          for content-end = (if (and (> line-end line-start)
                                     (char= (char source (1- line-end)) #\Return))
                                (1- line-end)
                                line-end)
          when (string-equal source ":END:" :start1 line-start
                             :end1 content-end)
            return line-start
          while newline
          finally (edit-signal-error target :uneditable-structure
                                     "Property drawer has no closing marker"))))

(defun edit-relocate-property-drawer-location (headline raw target)
  (let ((drawers (edit-relocate-root-property-drawers headline)))
    (when (> (length drawers) 1)
      (edit-signal-error target :uneditable-structure
                         "Headline has multiple property drawers"))
    (if drawers
        (values (first drawers) nil)
        (let* ((section (find :section (node-children headline)
                              :key #'node-type))
               (planning (and section
                              (find :planning (node-children section)
                                    :key #'node-type)))
               (line-start (if planning
                               (node-end planning)
                               (node-post-affiliated headline)))
               (newline (position #\Newline raw :start line-start))
               (offset (if newline (1+ newline) (length raw))))
          (values nil offset)))))

(defun edit-relocate-archive-metadata (source-document source-heading start
                                      target-document raw target)
  (let ((metadata (edit-relocate-archive-context source-document source-heading)))
    (if (null metadata)
        raw
        (multiple-value-bind (fragment headline canonical)
            (edit-subtree-fragment target-document raw target)
          (declare (ignore fragment canonical))
          (let* ((newline (edit-subtree-line-ending source-document start))
                 (patches nil)
                 (missing nil))
            (dolist (entry metadata)
              (let* ((key (car entry))
                     (value (cdr entry))
                     (properties (effective-heading-property-entries headline key)))
                (when (> (length properties) 1)
                  (edit-signal-error target :uneditable-structure
                                     "Archive metadata property is duplicated"))
                (if properties
                    (let* ((property (first properties))
                           (line-start (node-begin property))
                           (line-end (edit-subtree-source-line-end
                                      raw line-start))
                           (value-start
                             (edit-relocate-property-line-value-start
                              raw line-start line-end)))
                      (push (edit-make-patch value-start line-end value target)
                            patches))
                    (push entry missing))))
            (multiple-value-bind (drawer insertion-offset)
                (edit-relocate-property-drawer-location headline raw
                                                        target)
              (if drawer
                  (when missing
                    (let ((offset (edit-relocate-drawer-close-offset raw drawer
                                                                      target)))
                      (push (edit-make-patch
                             offset offset
                             (with-output-to-string (stream)
                               (dolist (entry (nreverse missing))
                                 (format stream ":~A: ~A~A" (car entry) (cdr entry)
                                         newline)))
                             target)
                            patches)))
                  (let ((prefix (if (not (char= (char raw
                                                  (1- insertion-offset))
                                                #\Newline))
                                    newline
                                    "")))
                    (push (edit-make-patch
                           insertion-offset insertion-offset
                           (with-output-to-string (stream)
                             (write-string prefix stream)
                             (format stream ":PROPERTIES:~A" newline)
                             (dolist (entry (nreverse missing))
                               (format stream ":~A: ~A~A" (car entry) (cdr entry)
                                       newline))
                             (format stream ":END:~A" newline))
                           target)
                          patches)))
            (if patches
                (edit-splice-source
                 raw (edit-validate-patches raw patches target))
                raw)))))))

(defun edit-relocate-transform-fragment (source-document heading start end
                                         target-document target-heading
                                         archive-p target)
  (edit-relocate-check-todo-compatibility source-document heading
                                           target-document target)
  (let* ((source (document-text source-document))
         (raw (subseq source start end))
         (patches (edit-relocate-relevel-patches source-document heading start
                                                  target-document target-heading)))
    (when archive-p
      (let ((tags (effective-heading-tags source-document heading)))
        (unless (equal tags (node-property heading :tags))
          (setf patches
                (append patches
                        (mapcar (lambda (patch)
                                  (edit-make-patch
                                   (- (edit-patch-start patch) start)
                                   (- (edit-patch-end patch) start)
                                   (edit-patch-replacement patch) target))
                                (edit-headline-tag-patch
                                 source-document heading target tags)))))))
    (setf raw (edit-splice-source raw
                                  (edit-validate-patches raw patches target)))
    (if archive-p
        (edit-relocate-archive-metadata source-document heading start
                                        target-document raw target)
        raw)))

(defun edit-relocate-normalize-line-endings (source newline)
  (with-output-to-string (stream)
    (loop for position from 0 below (length source)
          for character = (char source position)
          do (cond
               ((char= character #\Return)
                (if (and (< (1+ position) (length source))
                         (char= (char source (1+ position)) #\Newline))
                    (progn
                      (write-string newline stream)
                      (incf position))
                    (write-char character stream)))
               ((char= character #\Newline)
                (write-string newline stream))
               (t (write-char character stream))))))

(defun edit-relocate-line-ending (source-document source-offset
                                  target-document target-offset)
  (if (plusp (length (document-text target-document)))
      (edit-subtree-line-ending target-document target-offset)
      (edit-subtree-line-ending source-document source-offset)))

(defun edit-relocate-final-fragment-headline (document start end)
  (let* ((source (document-text document))
         (previous-newline
           (position #\Newline source :start start :end (- end 2)
                     :from-end t))
         (line-start (if previous-newline (1+ previous-newline) start)))
    (find line-start (edit-subtree-headlines document)
          :key #'node-post-affiliated :test #'=)))

(defun edit-relocate-sync-trailing-headline-properties
    (expected document source patch path target)
  (let* ((updated-source (edit-splice-source source (list patch)))
         (updated-document (edit-reparse-source document updated-source target))
         (updated-headline (edit-relocate-node-at-path updated-document path)))
    (dolist (property '(:raw-value :title))
      (edit-canonical-set-property expected path property
                                   (node-property updated-headline property)))
    expected))

(defun edit-relocate-target-boundary (document target-heading target)
  (if target-heading
      (multiple-value-bind (next offset)
          (edit-subtree-next-boundary document target-heading)
        (when (and next
                   (edit-subtree-affiliation-p (document-text document) next))
          (edit-signal-error target :uneditable-structure
                             "Adjacent affiliated keywords have ambiguous ownership"))
        (when (or (edit-subtree-trailing-comment-p
                   (document-text document) (node-begin target-heading) offset)
                  (edit-relocate-trailing-affiliation-p
                   (document-text document) offset))
          (edit-signal-error target :uneditable-structure
                             "Trailing comments or affiliations have ambiguous ownership"))
        offset)
      (let* ((source (document-text document))
             (offset (length source)))
        (when (or (and (plusp offset)
                       (edit-subtree-trailing-comment-p source 0 offset))
                  (edit-relocate-trailing-affiliation-p source offset))
          (edit-signal-error target :uneditable-structure
                             "Trailing comments or affiliations have ambiguous ownership"))
        offset)))

(defun edit-relocate-trailing-affiliation-p (source end)
  (loop with line-end = end
        while (plusp line-end)
        do (when (char= (char source (1- line-end)) #\Newline)
             (decf line-end))
           (when (and (plusp line-end)
                      (char= (char source (1- line-end)) #\Return))
             (decf line-end))
           (let* ((newline (position #\Newline source :end line-end :from-end t))
                  (line-start (if newline (1+ newline) 0))
                  (line (subseq source line-start line-end)))
             (cond
               ((every (lambda (character)
                         (find character '(#\Space #\Tab #\Return)))
                       line)
                (setf line-end (or newline 0)))
               ((edit-subtree-affiliated-keyword-p line)
                (return t))
               (t (return nil))))))

(defun edit-relocate-canonical-remove (canonical path)
  (multiple-value-bind (parent-path index)
      (edit-subtree-path-position path)
    (let ((children (edit-subtree-canonical-children canonical parent-path)))
      (edit-canonical-set-children canonical parent-path
                                   (edit-subtree-list-delete children index)))))

(defun edit-relocate-canonical-append (canonical parent-path child)
  (let* ((children (edit-subtree-canonical-children canonical parent-path))
         (index (length children)))
    (edit-canonical-set-children canonical parent-path
                                 (append children (list child)))
    index))

(defun edit-relocate-subtree (source-document heading target-document
                              target-heading mode archive-p)
  (edit-relocate-check-document source-document)
  (edit-relocate-check-document target-document)
  (let* ((source-heading (edit-resolve-node source-document heading '(:headline)))
         (target-heading
           (and target-heading
                (edit-resolve-node target-document target-heading '(:headline))))
         (same-document-p (eq source-document target-document)))
    (unless (member mode '(:move :copy))
      (edit-signal-error heading :invalid-value
                         "Relocation mode must be :MOVE or :COPY"))
    (when (and target-heading
               (or (eq source-heading target-heading)
                   (and same-document-p
                        (edit-relocate-heading-ancestor-p source-heading
                                                          target-heading))))
      (edit-signal-error heading :uneditable-structure
                         "A subtree cannot be inserted into itself or a descendant"))
    (multiple-value-bind (source-start source-end)
        (edit-subtree-safe-span source-document source-heading heading)
      (let* ((source-path (node-heading-path source-heading))
             (target-offset (edit-relocate-target-boundary target-document
                                                           target-heading
                                                           (or target-heading
                                                               heading)))
             (line-ending (edit-relocate-line-ending
                           source-document source-start
                           target-document target-offset))
             (raw
               (edit-relocate-normalize-line-endings
                (edit-relocate-transform-fragment
                 source-document source-heading source-start source-end
                 target-document target-heading archive-p heading)
                line-ending))
             (fragment-info
               (multiple-value-list
                (edit-subtree-fragment target-document raw heading)))
             (fragment-canonical (third fragment-info))
             (trailing-headline-cr-p (fourth fragment-info))
             (destination-source (document-text target-document))
             (suffix-p (or (< target-offset (length destination-source))
                           (document-final-newline-p target-document)))
             (target-replacement
               (edit-subtree-join-fragment target-document target-offset raw
                                           suffix-p line-ending
                                           trailing-headline-cr-p))
             (source-patches
               (when (eq mode :move)
                 (list (edit-make-patch source-start source-end "" heading))))
             (target-patches
               (list (edit-make-patch target-offset target-offset
                                      target-replacement target-heading)))
             (target-parent-path-numeric (if target-heading
                                             (edit-node-path target-document
                                                             target-heading)
                                             nil))
             (expected-source
               (edit-canonical-copy (edit-canonical-document source-document)))
             (expected-target
               (if same-document-p expected-source
                   (edit-canonical-copy (edit-canonical-document target-document))))
             (path-after-remove
               (if (and same-document-p (eq mode :move))
                   (edit-relocate-adjust-path-after-removal
                    target-parent-path-numeric (edit-node-path source-document
                                                               source-heading))
                   target-parent-path-numeric)))
        (when (eq mode :move)
          (edit-relocate-canonical-remove expected-source
                                          (edit-node-path source-document
                                                          source-heading)))
        (when same-document-p
          (setf expected-target expected-source))
        (let* ((insert-index
                 (edit-relocate-canonical-append expected-target path-after-remove
                                                 fragment-canonical))
               (source-operation
                 (when (and (eq mode :move) (not same-document-p))
                   (list source-document source-patches
                         :target heading :expected expected-source)))
               (target-operation
                 (list target-document
                       (if same-document-p
                           (append source-patches target-patches)
                           target-patches)
                       :target target-heading
                       :expected expected-target))
               (operations (if source-operation
                               (list source-operation target-operation)
                               (list target-operation)))
               (results (edit-apply-patches-transaction operations))
               (source-result (if (eq mode :copy) nil (first results)))
               (target-result (if source-operation
                                  (second results)
                                  (first results)))
               (updated-target
                 (edit-result-updated-document target-result))
               (new-headline
                 (edit-relocate-node-at-path
                  updated-target (append path-after-remove (list insert-index)))))
          (make-relocation-result
           :source-edit source-result
           :target-edit target-result
           :mode mode
           :source-path source-path
           :target-path (node-heading-path new-headline)))))))

(defun refile-subtree (source-document heading target-document
                       target-heading-or-nil &key (mode :move))
  "Move or copy HEADING to the last-child position in TARGET-DOCUMENT."
  (edit-relocate-subtree source-document heading target-document
                         target-heading-or-nil mode nil))

(defun set-heading-archive-tag (document heading archived-p)
  "Add or remove the local ARCHIVE tag on HEADING."
  (edit-relocate-check-document document)
  (check-type archived-p boolean)
  (let* ((headline (edit-resolve-node document heading '(:headline)))
         (tags (copy-list (node-property headline :tags)))
         (updated-tags
           (if archived-p
               (if (member "ARCHIVE" tags :test #'string=)
                   tags
                   (append tags (list "ARCHIVE")))
               (remove "ARCHIVE" tags :test #'string=))))
    (edit-apply-patches-result
     document (edit-headline-tag-patch document headline heading updated-tags)
     :target heading
     :expected
     (edit-headline-expected document headline
                             (list :tags updated-tags
                                   :archivedp archived-p)))))

(defun archive-subtree-to (source-document heading destination-document
                           &key target-heading)
  "Move HEADING to the archive root or TARGET-HEADING's last-child position."
  (edit-relocate-subtree source-document heading destination-document
                         target-heading :move t))

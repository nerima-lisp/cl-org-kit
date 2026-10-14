;;;; src/edit-subtree.lisp
;;;;
;;;; Format-preserving edits for headline subtrees.
(in-package #:org-kit)

(defun edit-subtree-source-line-end (source start)
  (let ((newline (position #\Newline source :start start)))
    (if newline
        (if (and (> newline start)
                 (char= (char source (1- newline)) #\Return))
            (1- newline)
            newline)
        (length source))))

(defun edit-subtree-marker (document headline)
  "Return the source marker bounds and physical star count for HEADLINE."
  (let* ((source (document-text document))
         (start (node-post-affiliated headline))
         (line-end (edit-subtree-source-line-end source start))
         (end start))
    (loop while (and (< end line-end) (char= (char source end) #\*))
          do (incf end))
    (values start end (- end start))))

(defun edit-subtree-headlines (document)
  (let ((headlines '()))
    (map-nodes (lambda (node)
                 (when (eq (node-type node) :headline)
                   (push node headlines)))
               (document-root document))
    (nreverse headlines)))

(defun edit-subtree-trailing-headline-cr-p (document start end)
  (let* ((source (document-text document))
         (newline (position #\Newline source :start start :end end
                            :from-end t))
         (line-start (if newline (1+ newline) start)))
    (and (< start end)
         (char= (char source (1- end)) #\Return)
         (find line-start (edit-subtree-headlines document)
               :key #'node-post-affiliated :test #'=))))

(defun edit-subtree-fragment-separator (line-ending bare-cr-before-p)
  (if (and bare-cr-before-p
           (char= (char line-ending 0) #\Newline))
      ;; A space keeps the data CR distinct from the CRLF delimiter.
      (format nil " ~C" #\Newline)
      line-ending))

(defun edit-subtree-next-boundary (document headline)
  "Return the next same-or-shallower headline and its safe source boundary."
  (multiple-value-bind (start end stars)
      (edit-subtree-marker document headline)
    (declare (ignore start end))
    (let* ((headline-position (node-post-affiliated headline))
           (next
             (find-if
              (lambda (candidate)
                (and (> (node-post-affiliated candidate) headline-position)
                     (multiple-value-bind (candidate-start candidate-end count)
                         (edit-subtree-marker document candidate)
                       (declare (ignore candidate-start candidate-end))
                       (<= count stars))))
              (edit-subtree-headlines document))))
      (values next (if next (node-begin next) (node-end headline))))))

(defun edit-subtree-line-text (source start end)
  (let ((line-end (edit-subtree-source-line-end source start)))
    (subseq source start (min end line-end))))

(defun edit-subtree-comment-end-marker-p (line)
  (string-equal (string-trim '(#\Space #\Tab) line) "#+END_COMMENT"))

(defun edit-subtree-comment-line-p (line)
  (or (and (plusp (length line))
           (char= (char line 0) #\#)
           (or (= (length line) 1)
               (not (char= (char line 1) #\+))))
      (and (>= (length line) 10)
           (string-equal line "#+COMMENT:" :end1 10 :end2 10))
      (edit-subtree-comment-end-marker-p line)))

(defun edit-subtree-trailing-comment-p (source start end)
  "Return true when the last nonblank line in [START, END) is a comment."
  (loop with line-end = end
        while (plusp line-end)
        do (when (char= (char source (1- line-end)) #\Newline)
             (decf line-end))
           (when (and (plusp line-end)
                      (char= (char source (1- line-end)) #\Return))
             (decf line-end))
           (let* ((newline (position #\Newline source :start start :end line-end
                                     :from-end t))
                  (line-start (if newline (1+ newline) start))
                  (line (subseq source line-start line-end)))
             (unless (every (lambda (character)
                              (find character '(#\Space #\Tab)))
                            line)
               (return (edit-subtree-comment-line-p line)))
             (setf line-end line-start))))

(defun edit-subtree-affiliated-keyword-p (line)
  (when (and (>= (length line) 3)
             (char= (char line 0) #\#)
             (char= (char line 1) #\+))
    (let* ((colon (position #\: line :start 2))
           (key (and colon
                     (string-upcase
                      (string-trim '(#\Space #\Tab)
                                   (subseq line 2 colon))))))
      (and key
           (or (member key '("NAME" "CAPTION" "HEADER" "PLOT" "RESULTS")
                       :test #'string=)
               (and (>= (length key) 5)
                    (string= key "ATTR_" :end1 5 :end2 5)))))))

(defun edit-subtree-affiliation-p (source node)
  (let* ((marker (node-post-affiliated node))
         (line-end (position #\Newline source :end marker :from-end t))
         (previous-line-end
           (and line-end
                (position #\Newline source :end line-end :from-end t)))
         (line-start (and line-end (if previous-line-end
                                       (1+ previous-line-end)
                                       0))))
    (and line-end
         (edit-subtree-affiliated-keyword-p
          (edit-subtree-line-text source line-start marker)))))

(defun edit-subtree-check-boundary (document start end next target)
  (when next
    (when (edit-subtree-affiliation-p (document-text document) next)
      (edit-signal-error target :uneditable-structure
                         "Adjacent affiliated keywords have ambiguous ownership"))
    (when (edit-subtree-trailing-comment-p (document-text document) start end)
      (edit-signal-error target :uneditable-structure
                         "Adjacent comments have ambiguous ownership")))
  (values start end))

(defun edit-subtree-safe-span (document headline target)
  (multiple-value-bind (next end)
      (edit-subtree-next-boundary document headline)
    (edit-subtree-check-boundary document (node-begin headline) end next target)))

(defun edit-subtree-fragment (document raw-subtree target)
  "Parse RAW-SUBTREE and require exactly one top-level headline."
  (unless (and (stringp raw-subtree) (plusp (length raw-subtree)))
    (edit-signal-error target :invalid-value
                       "Subtree text must be a nonempty Org string"))
  (let* ((fragment
           (parse-org raw-subtree
                      :limits (document-limits document)
                      :settings (document-settings document)))
         (children (node-children (document-root fragment)))
         (headline (and (= (length children) 1)
                        (eq (node-type (first children)) :headline)
                        (first children))))
    (unless (and headline (= (node-begin headline) 0)
                 (= (node-end headline) (length raw-subtree)))
      (edit-signal-error target :invalid-value
                         "Subtree text must contain exactly one headline"))
    (values fragment headline
            (first (getf (edit-canonical-document fragment) :children))
            (edit-subtree-trailing-headline-cr-p
             fragment 0 (length raw-subtree)))))

(defun edit-subtree-line-ending (document offset)
  (let* ((source (document-text document))
         (before (and (plusp offset)
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

(defun edit-subtree-trim-final-line-endings (text)
  (let ((end (length text)))
    (loop while (and (plusp end)
                     (char= (char text (1- end)) #\Newline))
          do (decf end)
             (when (and (plusp end)
                        (char= (char text (1- end)) #\Return))
               (decf end)))
    (subseq text 0 end)))

(defun edit-subtree-join-fragment (document offset raw-subtree suffix-p
                                  &optional line-ending
                                    trailing-headline-cr-p)
  (let* ((source (document-text document))
         (newline (or line-ending
                      (edit-subtree-line-ending document offset)))
         (prefix-p (and (plusp offset)
                        (not (char= (char source (1- offset)) #\Newline))))
         (raw-final-newline-p
           (char= (char raw-subtree (1- (length raw-subtree))) #\Newline)))
    (concatenate 'string
                 (if prefix-p
                     (edit-subtree-fragment-separator
                      newline
                      (char= (char source (1- offset)) #\Return))
                     "")
                 raw-subtree
                 (if (and suffix-p (not raw-final-newline-p))
                     (edit-subtree-fragment-separator
                      newline trailing-headline-cr-p)
                     ""))))

(defun edit-subtree-canonical-children (canonical path)
  (getf (edit-canonical-node-at-path canonical path) :children))

(defun edit-subtree-list-replace (items index replacement)
  (append (subseq items 0 index) (list replacement) (nthcdr (1+ index) items)))

(defun edit-subtree-list-insert (items index value)
  (append (subseq items 0 index) (list value) (nthcdr index items)))

(defun edit-subtree-list-delete (items index)
  (append (subseq items 0 index) (nthcdr (1+ index) items)))

(defun edit-subtree-path-position (path)
  (values (butlast path) (car (last path))))

(defun edit-subtree-headline-child-index (children)
  (position :headline children :key (lambda (child) (getf child :type))))

(defun insert-subtree (document parent-target raw-subtree &key (position :last-child))
  "Insert one parsed headline subtree as the first or last child of PARENT-TARGET."
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document))
  (let ((parent (edit-resolve-node document parent-target '(:headline))))
    (unless (member position '(:first-child :last-child))
      (edit-signal-error parent-target :invalid-value
                         "Subtree position must be :FIRST-CHILD or :LAST-CHILD"))
    (multiple-value-bind (fragment fragment-headline canonical-fragment
                          trailing-headline-cr-p)
        (edit-subtree-fragment document raw-subtree parent-target)
      (multiple-value-bind (parent-start parent-end parent-stars)
          (edit-subtree-marker document parent)
        (declare (ignore parent-start parent-end))
        (multiple-value-bind (fragment-start fragment-end fragment-stars)
            (edit-subtree-marker fragment fragment-headline)
          (declare (ignore fragment-start fragment-end))
          (unless (and (integerp (node-property parent :level))
                       (= (node-property fragment-headline :level)
                          (1+ (node-property parent :level)))
                       (> fragment-stars parent-stars))
            (edit-signal-error parent-target :invalid-value
                               "Inserted headline must be one logical level below its parent"))))
      (let* ((direct-headlines
               (remove-if-not (lambda (node) (eq (node-type node) :headline))
                              (node-children parent)))
             (source (document-text document))
             (parent-path (edit-node-path document parent parent-target))
             (offset nil)
             (next nil))
        (if (and (eq position :first-child) direct-headlines)
            (setf next (first direct-headlines)
                  offset (node-begin (first direct-headlines)))
            (multiple-value-setq (next offset)
              (edit-subtree-next-boundary document parent)))
        (when (and next (edit-subtree-affiliation-p source next))
          (edit-signal-error parent-target :uneditable-structure
                             "Adjacent affiliated keywords have ambiguous ownership"))
        (when (and next direct-headlines (eq position :last-child)
                   (edit-subtree-trailing-comment-p
                    source (node-begin (car (last direct-headlines))) offset))
          (edit-signal-error parent-target :uneditable-structure
                             "Adjacent comments have ambiguous ownership"))
        (let* ((suffix-p (or (< offset (length source))
                             (document-final-newline-p document)))
               (replacement (edit-subtree-join-fragment document offset
                                                        raw-subtree suffix-p nil
                                                        trailing-headline-cr-p))
               (expected (edit-canonical-copy (edit-canonical-document document)))
               (children (edit-subtree-canonical-children expected parent-path))
               (index (if (eq position :first-child)
                          (or (edit-subtree-headline-child-index children)
                              (length children))
                          (length children))))
          (edit-canonical-set-children
           expected parent-path
           (edit-subtree-list-insert children index canonical-fragment))
          (edit-apply-patches document
                              (list (edit-make-patch offset offset replacement
                                                    parent-target))
                              :target parent-target :expected expected))))))

(defun delete-subtree (document target)
  "Delete TARGET and its descendants while retaining source outside its span."
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document))
  (let* ((headline (edit-resolve-node document target '(:headline)))
         (path (edit-node-path document headline target)))
    (multiple-value-bind (start end)
        (edit-subtree-safe-span document headline target)
      (multiple-value-bind (parent-path index)
          (edit-subtree-path-position path)
        (let* ((expected (edit-canonical-copy (edit-canonical-document document)))
               (children (edit-subtree-canonical-children expected parent-path)))
          (edit-canonical-set-children expected parent-path
                                       (edit-subtree-list-delete children index))
          (edit-apply-patches document
                              (list (edit-make-patch start end "" target))
                              :target target :expected expected))))))

(defun replace-subtree (document target raw-subtree)
  "Replace TARGET and its descendants with one same-level headline subtree."
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document))
  (let* ((headline (edit-resolve-node document target '(:headline)))
         (path (edit-node-path document headline target)))
    (multiple-value-bind (fragment fragment-headline canonical-fragment
                          trailing-headline-cr-p)
        (edit-subtree-fragment document raw-subtree target)
      (unless (and (= (node-property headline :level)
                      (node-property fragment-headline :level)))
        (edit-signal-error target :invalid-value
                           "Replacement headline must keep the target logical level"))
      (multiple-value-bind (target-start target-end target-stars)
          (edit-subtree-marker document headline)
        (declare (ignore target-start target-end))
        (multiple-value-bind (fragment-start fragment-end fragment-stars)
            (edit-subtree-marker fragment fragment-headline)
          (declare (ignore fragment-start fragment-end))
          (unless (= target-stars fragment-stars)
            (edit-signal-error target :invalid-value
                               "Replacement headline must keep the target source level"))))
      (multiple-value-bind (start end)
          (edit-subtree-safe-span document headline target)
        (multiple-value-bind (parent-path index)
            (edit-subtree-path-position path)
          (let* ((source (document-text document))
                 (suffix-p (or (< end (length source))
                               (document-final-newline-p document)))
                 (replacement (edit-subtree-join-fragment document start
                                                          raw-subtree suffix-p nil
                                                          trailing-headline-cr-p))
                 (expected (edit-canonical-copy (edit-canonical-document document)))
                 (children (edit-subtree-canonical-children expected parent-path)))
            (edit-canonical-set-children
             expected parent-path
             (edit-subtree-list-replace children index canonical-fragment))
            (edit-apply-patches document
                                (list (edit-make-patch start end replacement target))
                                :target target :expected expected)))))))

(defun edit-subtree-canonical-level-change (canonical target-path entries delta)
  (dolist (entry entries)
    (destructuring-bind (path old-level new-level) entry
      (declare (ignore old-level))
      (edit-canonical-set-property canonical path :level new-level)))
  (let* ((target-node (edit-canonical-node-at-path canonical target-path))
         (target-parent-path (butlast target-path))
         (target-index (car (last target-path)))
         (parent-node (edit-canonical-node-at-path canonical target-parent-path))
         (parent-type (getf parent-node :type)))
    (cond
      ((and (minusp delta) (eq parent-type :headline))
       (let* ((target-old-level (second (first entries)))
              (parent-level (getf (getf parent-node :properties) :level)))
         (when (<= (+ target-old-level delta) parent-level)
           (let* ((grandparent-path (butlast target-parent-path))
                  (parent-index (car (last target-parent-path)))
                  (parent-children (getf parent-node :children))
                  (grandparent-children
                    (edit-subtree-canonical-children canonical grandparent-path)))
             (edit-canonical-set-children
              canonical target-parent-path
              (edit-subtree-list-delete parent-children target-index))
             (edit-canonical-set-children
              canonical grandparent-path
              (edit-subtree-list-insert grandparent-children (1+ parent-index)
                                        target-node))))))
      ((plusp delta)
       (let* ((target-old-level (second (first entries)))
              (parent-children (getf parent-node :children))
              (prior-indexes
                (loop for child in parent-children
                      for index from 0
                      when (and (< index target-index)
                                (eq (getf child :type) :headline)
                                (= (getf (getf child :properties) :level)
                                   target-old-level))
                        collect index))
              (previous-index (car (last prior-indexes))))
         (when previous-index
           (let* ((previous-path (append target-parent-path (list previous-index)))
                  (siblings-without-target
                    (edit-subtree-list-delete parent-children target-index))
                  (previous-children
                    (edit-subtree-canonical-children canonical previous-path)))
             (edit-canonical-set-children canonical target-parent-path
                                          siblings-without-target)
             (edit-canonical-set-children canonical previous-path
                                          (append previous-children
                                                  (list target-node))))))))
  canonical))

(defun edit-subtree-level-change (document target delta)
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document))
  (let* ((headline (edit-resolve-node document target '(:headline)))
         (target-path (edit-node-path document headline target))
         (entries '())
         (max-new-level 0)
         (patches '()))
    (map-nodes
     (lambda (node)
       (when (eq (node-type node) :headline)
         (multiple-value-bind (start end stars)
             (edit-subtree-marker document node)
           (let* ((old-level (logical-heading-level document stars))
                  (new-level (+ old-level delta)))
             (when (< new-level 1)
               (edit-signal-error target :invalid-value
                                  "Cannot promote a logical level-one headline"))
             (setf max-new-level (max max-new-level new-level))
             (push (list (edit-node-path document node target)
                         old-level new-level)
                   entries)
             (push (edit-make-patch
                    start end
                    (make-string (source-heading-level document new-level)
                                 :initial-element #\*)
                    target)
                   patches)))))
     headline)
    (setf entries (nreverse entries)
          patches (nreverse patches))
    (check-limit (document-limits document) :max-heading-depth max-new-level)
    (let* ((expected (edit-canonical-copy (edit-canonical-document document)))
           (expected (edit-subtree-canonical-level-change
                      expected target-path entries delta)))
      (edit-apply-patches document patches :target target :expected expected))))

(defun promote-subtree (document target)
  "Promote TARGET and its descendants by one Org logical heading level."
  (edit-subtree-level-change document target -1))

(defun demote-subtree (document target)
  "Demote TARGET and its descendants by one Org logical heading level."
  (edit-subtree-level-change document target 1))

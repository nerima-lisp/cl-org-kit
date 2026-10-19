;;;; src/edit-drawers.lisp
;;;; Format-preserving edits to ordinary headline drawers.
(in-package #:org-kit)

(defun edit-drawer-name (name target)
  (unless (and (stringp name) (plusp (length name))
               (every (lambda (character)
                        (or (alphanumericp character)
                            (find character "_-")))
                      name))
    (edit-signal-error target :invalid-value "Drawer name is invalid"))
  (let ((normalized (string-upcase name)))
    (when (string= normalized "END")
      (edit-signal-error target :invalid-value "END is reserved for drawer closing markers"))
    (when (member normalized '("PROPERTIES" "LOGBOOK") :test #'string=)
      (edit-signal-error target :unsupported-option
                         "PROPERTIES and LOGBOOK use dedicated editing operations"))
    normalized))

(defun edit-drawer-body-value (document body target)
  (unless (stringp body)
    (edit-signal-error target :invalid-value "Drawer body must be a string"))
  (loop with start = 0
        for newline = (position #\Newline body :start start)
        for end = (or newline (length body))
        for line-end = (if (and (> end start)
                                (char= (char body (1- end)) #\Return))
                           (1- end)
                           end)
        for line = (string-trim '(#\Space #\Tab) (subseq body start line-end))
        do (when (string-equal line ":END:")
             (edit-signal-error target :invalid-value
                                "Drawer body contains an unescaped closing marker"))
        while newline
        do (setf start (1+ newline)))
  (let ((fragment
          (handler-case
              (parse-org body :settings (document-settings document)
                              :limits (document-limits document))
            (org-parse-error ()
              (edit-signal-error target :invalid-value
                                 "Drawer body is not valid Org content")))))
    (let ((headline-p nil))
      (map-nodes (lambda (node)
                   (when (eq (node-type node) :headline)
                     (setf headline-p t)))
                 (document-root fragment))
      (when headline-p
        (edit-signal-error "headline" :invalid-value
                           "Drawer body cannot contain a headline")))
    (values body fragment)))

(defun edit-drawer-line-ending (source)
  (let ((newline (position #\Newline source)))
    (if (and newline (plusp newline)
             (char= (char source (1- newline)) #\Return))
        (format nil "~C~C" #\Return #\Newline)
        (string #\Newline))))

(defun edit-drawer-body-with-line-ending (body newline)
  (if (and (plusp (length body))
           (not (char= (char body (1- (length body))) #\Newline)))
      (concatenate 'string body newline)
      body))

(defun edit-drawer-headline-section (headline)
  (find :section (node-children headline) :key #'node-type))

(defun edit-drawer-matches (section name)
  (when section
    (remove-if-not
     (lambda (node)
       (and (eq (node-type node) :drawer)
            (string= name (node-property node :drawer-name))))
     (node-children section))))

(defun edit-drawer-line-after (source offset)
  (let ((newline (position #\Newline source :start offset)))
    (if newline (1+ newline) (length source))))

(defun edit-drawer-insertion-offset (document headline section)
  (let* ((source (document-text document))
         (children (and section (node-children section)))
         (first-content
           (find-if-not (lambda (node)
                          (member (node-type node) '(:planning :property-drawer)
                                  :test #'eq))
                        children)))
    (cond (first-content (node-begin first-content))
          ((and children (car (last children))) (node-end (car (last children))))
          (t (edit-drawer-line-after source (node-begin headline))))))

(defun edit-drawer-create-text (document offset name body)
  (let* ((source (document-text document))
         (newline (edit-drawer-line-ending source))
         (body-text (edit-drawer-body-with-line-ending body newline))
         (prefix-p (and (plusp offset)
                        (not (char= (char source (1- offset)) #\Newline))))
         (suffix-p (or (< offset (length source))
                       (document-final-newline-p document))))
    (concatenate 'string
                 (if prefix-p newline "")
                 ":" name ":" newline
                 body-text
                 (if (and (plusp (length body-text))
                          (not (char= (char body-text (1- (length body-text)))
                                      #\Newline)))
                     newline "")
                 ":END:"
                 (if suffix-p newline ""))))

(defun edit-drawer-close-start (document drawer target)
  (let* ((source (document-text document))
         (start (edit-drawer-line-after source (node-begin drawer)))
         (limit (node-end drawer)))
    (loop while (< start limit)
          do (let* ((newline (position #\Newline source :start start :end limit))
                    (end (or newline limit))
                    (line-end (if (and (> end start)
                                       (char= (char source (1- end)) #\Return))
                                  (1- end)
                                  end))
                    (line (string-trim '(#\Space #\Tab)
                                       (subseq source start line-end))))
               (when (string-equal line ":END:")
                 (return-from edit-drawer-close-start start))
               (unless newline (return)))
             (setf start (1+ (position #\Newline source :start start :end limit))))
    (edit-signal-error target :uneditable-structure
                       "Drawer has no closing marker")))

(defun edit-drawer-body-range (document drawer target)
  (let* ((source (document-text document))
         (body-start (edit-drawer-line-after source (node-begin drawer)))
         (close-start (edit-drawer-close-start document drawer target)))
    (values body-start close-start)))

(defun set-drawer-body (document target name body)
  "Create a headline drawer or replace the body of its unique matching drawer.

NAME is case-insensitive and is stored uppercase. BODY is raw Org content
without drawer markers. Return an EDIT-RESULT whose spans refer to the original
source. PROPERTIES and LOGBOOK are reserved for their dedicated editing APIs."
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document))
  (let ((headline (edit-resolve-node document target '(:headline)))
        (normalized-name (edit-drawer-name name target)))
    (multiple-value-bind (raw-body fragment)
        (edit-drawer-body-value document body target)
      (let* ((section (edit-drawer-headline-section headline))
             (matches (edit-drawer-matches section normalized-name))
             (root-children (node-children (document-root fragment)))
             (fragment-section
               (and (= (length root-children) 1)
                    (eq (node-type (first root-children)) :section)
                    (first root-children)))
             (content-nodes (mapcar #'edit-canonical-node
                                    (if fragment-section
                                        (node-children fragment-section)
                                        root-children)))
             (expected (edit-canonical-copy (edit-canonical-document document))))
        (when (> (length matches) 1)
          (edit-signal-error target :ambiguous-target
                             "Drawer name occurs more than once under the headline"))
        (if matches
            (let* ((drawer (first matches))
                   (path (edit-node-path document drawer target)))
              (multiple-value-bind (start end)
                  (edit-drawer-body-range document drawer target)
                (edit-canonical-set-children expected path content-nodes)
                (edit-apply-patches-result
                 document
                 (list (edit-make-patch
                        start end
                        (edit-drawer-body-with-line-ending
                         raw-body (edit-drawer-line-ending (document-text document)))
                        target))
                 :target target :expected expected)))
            (let* ((offset (edit-drawer-insertion-offset document headline section))
                   (replacement (edit-drawer-create-text
                                 document offset normalized-name raw-body))
                   (drawer-canonical
                     (list :type :drawer :post-blank 0
                           :properties (list :drawer-name normalized-name)
                           :children content-nodes)))
              (if section
                  (let* ((section-path (edit-node-path document section target))
                         (children (node-children section))
                         (first-content
                           (find-if-not
                            (lambda (node)
                              (member (node-type node)
                                      '(:planning :property-drawer) :test #'eq))
                            children))
                         (index (if first-content
                                    (position first-content children :test #'eq)
                                    (length children)))
                         (canonical-section
                           (edit-canonical-node-at-path expected section-path))
                         (canonical-children
                           (getf canonical-section :children)))
                    (edit-canonical-set-children
                     expected section-path
                     (append (subseq canonical-children 0 index)
                             (list drawer-canonical)
                             (nthcdr index canonical-children))))
                  (let* ((headline-path (edit-node-path document headline target))
                         (canonical-headline
                           (edit-canonical-node-at-path expected headline-path))
                         (children (getf canonical-headline :children))
                         (index (or (position-if
                                     (lambda (child)
                                       (eq (getf child :type) :headline))
                                     children)
                                    (length children)))
                         (section-canonical
                           (list :type :section :post-blank 0 :properties nil
                                 :children (list drawer-canonical))))
                    (edit-canonical-set-children
                     expected headline-path
                     (append (subseq children 0 index)
                             (list section-canonical)
                             (nthcdr index children)))))
              (edit-apply-patches-result
               document
               (list (edit-make-patch offset offset replacement target))
               :target target :expected expected)))))))

(defun remove-drawer (document target name)
  "Remove the unique ordinary drawer named NAME from headline TARGET.

Return an EDIT-RESULT whose span covers the removed drawer. PROPERTIES and
LOGBOOK are reserved for their dedicated editing APIs."
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document))
  (let* ((headline (edit-resolve-node document target '(:headline)))
         (normalized-name (edit-drawer-name name target))
         (section (edit-drawer-headline-section headline))
         (matches (edit-drawer-matches section normalized-name)))
    (when (> (length matches) 1)
      (edit-signal-error target :ambiguous-target
                         "Drawer name occurs more than once under the headline"))
    (unless matches
      (edit-signal-error target :target-not-found "Drawer was not found"))
    (let* ((drawer (first matches))
           (drawer-path (edit-node-path document drawer target))
           (section-path (butlast drawer-path))
           (drawer-index (car (last drawer-path)))
           (section-node (edit-canonical-node-at-path
                          (edit-canonical-document document) section-path))
           (section-children (getf section-node :children))
           (remaining (append (subseq section-children 0 drawer-index)
                              (nthcdr (1+ drawer-index) section-children)))
           (expected (edit-canonical-copy (edit-canonical-document document))))
      (edit-drawer-close-start document drawer target)
      (if remaining
          (edit-canonical-set-children expected section-path remaining)
          (let* ((headline-path (butlast section-path))
                 (section-index (car (last section-path)))
                 (canonical-headline
                   (edit-canonical-node-at-path expected headline-path))
                 (children (getf canonical-headline :children)))
            (edit-canonical-set-children
             expected headline-path
             (append (subseq children 0 section-index)
                     (nthcdr (1+ section-index) children)))))
      (edit-apply-patches-result
       document
       (list (edit-make-patch (node-begin drawer) (node-end drawer) "" target))
       :target target :expected expected))))

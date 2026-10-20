(in-package #:org-kit)

(defun refile-target-selector (selector)
  "Return the internal kind and value for a supported SELECTOR."
  (cond
    ((eq selector t) (values :all nil))
    ((and (consp selector) (eq (car selector) :level))
     (let ((level (cdr selector)))
       (unless (typep level '(integer 1 *))
         (edit-signal-error :refile-target-selector
                            :invalid-value
                            "Refile target level must be a positive integer"))
       (values :level level)))
    ((and (consp selector) (eq (car selector) :maxlevel))
     (let ((level (cdr selector)))
       (unless (typep level '(integer 1 *))
         (edit-signal-error :refile-target-selector
                            :invalid-value
                            "Refile target maximum level must be a positive integer"))
       (values :maxlevel level)))
    ((and (consp selector) (eq (car selector) :tag))
     (let ((tag (cdr selector)))
       (unless (stringp tag)
         (edit-signal-error :refile-target-selector
                            :invalid-value
                            "Refile target tag must be a string"))
       (values :tag tag)))
    ((and (consp selector) (eq (car selector) :todo))
     (let ((keyword (cdr selector)))
       (unless (stringp keyword)
         (edit-signal-error :refile-target-selector
                            :invalid-value
                            "Refile target TODO keyword must be a string"))
       (values :todo keyword)))
    (t
     (edit-signal-error :refile-target-selector
                        :invalid-value
                        "Unsupported refile target selector"))))

(defun refile-target-documents (current-document specification)
  "Resolve a target source SPECIFICATION to parsed documents."
  (cond
    ((null specification) (list current-document))
    ((document-p specification) (list specification))
    ((listp specification)
      (unless (edit-proper-list-length specification)
        (edit-signal-error :refile-target-document
                           :invalid-value
                           "Refile target documents must be a proper list"))
      (dolist (document specification)
        (unless (document-p document)
          (edit-signal-error :refile-target-document
                             :invalid-value
                             "Refile target source must contain parsed documents")))
      specification)
    (t
     (edit-signal-error :refile-target-document
                        :invalid-value
                        "Refile target source must be a document or document list"))))

(defun refile-target-selector-matches-p (node kind value)
  "Return whether headline NODE matches selector KIND and VALUE."
  (let ((level (node-property node :level)))
    (case kind
      (:all t)
      (:level (eql level value))
      (:maxlevel (and (integerp level) (<= level value)))
      (:tag
       (not (null (member value (node-property node :tags) :test #'string=))))
      (:todo
       (let ((keyword (node-property node :todo-keyword)))
         (and (stringp keyword) (string= keyword value)))))))

(defun refile-target-candidates (current-document target-specifications
                                                  &key
                                                  limits)
  "Return unique headline entries matching TARGET-SPECIFICATIONS.

Each specification is a two-element list of a document source and selector.
A NIL document source means CURRENT-DOCUMENT. NIL TARGET-SPECIFICATIONS selects
top-level headlines in CURRENT-DOCUMENT. Tag matching uses local tags only."
  (unless (document-p current-document)
    (error 'type-error :datum current-document :expected-type 'document))
  (when (and limits (not (limits-p limits)))
    (error 'type-error :datum limits :expected-type 'limits))
  (let ((specifications
         (or target-specifications (list (list nil (cons :level 1)))))
        (entries nil)
        (seen (make-hash-table :test #'eq)))
    (unless (edit-proper-list-length specifications)
      (edit-signal-error :refile-target-specifications
                         :invalid-value
                         "Refile target specifications must be a proper list"))
    (dolist (specification specifications)
      (unless (eql (edit-proper-list-length specification) 2)
        (edit-signal-error :refile-target-specification
                           :invalid-value
                           "Each refile target specification must have two elements"))
      (let ((documents
             (refile-target-documents current-document (first specification))))
        (multiple-value-bind (kind value)
            (refile-target-selector (second specification))
          (dolist (document documents)
            (let ((root (document-root document))
                  (node-count 0))
              (when root
                (map-nodes
                 (lambda (node)
                   (incf node-count)
                   (check-limit (document-limits document)
                                :max-node-count
                                node-count)
                   (when limits
                     (check-limit limits :max-node-count node-count))
                   (when
                       (and (eq (node-type node) :headline)
                            (refile-target-selector-matches-p node kind value)
                            (not (gethash node seen)))
                     (setf (gethash node seen) t)
                     (push (outline-entry-for-node document node) entries)))
                 root)))))))
    (nreverse entries)))

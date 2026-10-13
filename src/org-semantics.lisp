;;;; src/org-semantics.lisp
(in-package #:org-kit)

(defparameter +org-setting-keys+
  '(("CATEGORY" . :category)
    ("FILETAGS" . :file-tags)
    ("INLINETASK-MIN-LEVEL" . :inlinetask-min-level)
    ("LINK" . :link-abbrevs)
    ("LINK-ABBREVS" . :link-abbrevs)
    ("ODD-LEVELS-ONLY-P" . :odd-levels-only-p)
    ("STARTUP" . :startup)
    ("TAG-ALIST" . :tag-alist)
    ("TAGS" . :tag-alist)
    ("TODO-KEYWORDS" . :todo-keywords)))

(defun org-setting-key (key)
  "Return the document-settings key for an Org setting name, or NIL if unknown.
KEY may be a string or symbol; names are case-insensitive."
  (let* ((source-name (if (stringp key) key (symbol-name key)))
         (name (string-upcase source-name)))
    (cond
      ((member name '("TODO" "SEQ_TODO" "TYP_TODO") :test #'string=)
       :todo-keywords)
      ((and (>= (length name) 6)
            (or (string= name "TODO[" :end1 5 :end2 5)
                (and (>= (length name) 9)
                     (string= name "SEQ_TODO[" :end1 9 :end2 9))
                (and (>= (length name) 9)
                     (string= name "TYP_TODO[" :end1 9 :end2 9)))
            (char= (char name (1- (length name))) #\]))
       :todo-keywords)
      (t (cdr (assoc name +org-setting-keys+ :test #'string=))))))

(defun org-startup-odd-levels-p (startup)
  "Return whether the last recognized odd-level option in STARTUP is ODD."
  (loop with odd-levels-p = nil
        for option in startup
        do (cond
             ((string-equal option "odd")
              (setf odd-levels-p t))
             ((string-equal option "oddeven")
              (setf odd-levels-p nil)))
        finally (return odd-levels-p)))

(defun org-setting-value (document key &optional default)
  "Return DOCUMENT's setting named by KEY, or DEFAULT when KEY is unknown.
The derived :ODD-LEVELS-ONLY-P setting honors an explicit override, then the
last ODD or ODDEVEN token in the document's STARTUP setting."
  (let* ((settings (document-settings document))
         (resolved-key (org-setting-key key)))
    (cond
      ((null resolved-key) default)
      ((eq resolved-key :odd-levels-only-p)
       (if (member :odd-levels-only-p settings :test #'eq)
           (not (null (getf settings :odd-levels-only-p)))
           (org-startup-odd-levels-p (getf settings :startup))))
      (t (getf settings resolved-key default)))))

(defun logical-heading-level (document star-count)
  "Return Org's logical heading level for STAR-COUNT source stars in DOCUMENT.
With :STARTUP odd, source levels map by 1 + floor(STAR-COUNT / 2)."
  (if (org-setting-value document :odd-levels-only-p)
      (1+ (floor star-count 2))
      star-count))

(defun source-heading-level (document logical-level)
  "Return a canonical source star count for LOGICAL-LEVEL in DOCUMENT.
In odd-level mode this returns the odd star count that maps to LOGICAL-LEVEL."
  (if (org-setting-value document :odd-levels-only-p)
      (1- (* 2 logical-level))
      logical-level))

(defun effective-semantics-error (target reason message)
  (error 'org-edit-error
         :message message
         :target (bounded-text
                  (if (node-p target)
                      (or (node-property target :raw-value)
                          (string-downcase (symbol-name (node-type target))))
                      (princ-to-string target)))
         :reason reason))

(defun effective-semantics-heading (document heading)
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document))
  (unless (node-p heading)
    (effective-semantics-error heading :target-not-found
                               "Heading target was not found"))
  (unless (eq (node-document heading) document)
    (effective-semantics-error heading :foreign-node
                               "Heading does not belong to the input document"))
  (unless (member (node-type heading) '(:headline :inlinetask) :test #'eq)
    (effective-semantics-error heading :wrong-node-type
                               "Target node is not a heading"))
  heading)

(defun effective-query-error (selector position message)
  (error 'org-query-error
         :message message
         :query (bounded-text (prin1-to-string selector))
         :position position))

(defun effective-proper-list-p (value)
  (and (listp value)
       (handler-case (integerp (list-length value))
         (type-error () nil))))

(defun effective-selector-name (name selector)
  (cond
    ((stringp name) name)
    ((symbolp name) (symbol-name name))
    (t (effective-query-error selector 0
                              "Selector entries must be strings or symbols"))))

(defun effective-selector-predicate (selector)
  (labels ((compile-selector (spec nested-p)
             (cond
               ((null spec) (constantly nil))
               ((eq spec t) (constantly t))
               ((and (stringp spec) nested-p)
                (let ((name spec))
                  (lambda (candidate) (string= candidate name))))
               ((and (symbolp spec) nested-p)
                (let ((name (symbol-name spec)))
                  (lambda (candidate) (string= candidate name))))
               ((and (consp spec) (eq (first spec) :regexp))
                (unless (and (effective-proper-list-p spec)
                             (= (length spec) 2)
                             (stringp (second spec)))
                  (effective-query-error selector 0
                                         "Regexp selector must contain one string"))
                (let* ((pattern (second spec))
                       (regex
                         (handler-case
                             (cl-regex-kit:compile-regex pattern)
                           (cl-regex-kit:regex-syntax-error (condition)
                             (effective-query-error
                              selector
                              (cl-regex-kit:regex-syntax-error-position condition)
                              "Invalid regexp selector")))))
                  (lambda (candidate)
                    (cl-regex-kit:is-match-p regex candidate))))
               ((and (consp spec) (eq (first spec) :exclude))
                (unless (effective-proper-list-p spec)
                  (effective-query-error selector 0
                                         "Exclude selector must be a proper list"))
                (let ((predicates
                        (mapcar (lambda (entry)
                                  (compile-selector entry t))
                                (rest spec))))
                  (lambda (candidate)
                    (not (some (lambda (predicate)
                                 (funcall predicate candidate))
                               predicates)))))
               ((effective-proper-list-p spec)
                (let ((names (mapcar (lambda (entry)
                                       (effective-selector-name entry selector))
                                     spec)))
                  (lambda (candidate) (member candidate names :test #'string=))))
               (t
                (effective-query-error selector 0
                                       "Unsupported inheritance selector")))))
    (unless (or (null selector) (eq selector t)
                (effective-proper-list-p selector))
      (effective-query-error selector 0 "Unsupported inheritance selector"))
    (compile-selector selector nil)))

(defun effective-heading-ancestors (heading)
  (loop for ancestor = (node-parent heading) then (node-parent ancestor)
        while ancestor
        when (member (node-type ancestor) '(:headline :inlinetask) :test #'eq)
          collect ancestor))

(defun effective-tag-add (tags tag)
  (append (remove tag tags :test #'string=) (list tag)))

(defun effective-heading-tags (document heading &key (inherit t))
  "Return unique effective tags for HEADING, using INHERIT to select ancestors.
The nearest occurrence of a tag determines its position in the result."
  (let* ((heading (effective-semantics-heading document heading))
         (matches-p (effective-selector-predicate inherit))
         (ancestors (effective-heading-ancestors heading))
         (tags nil))
    (when inherit
      (dolist (tag (getf (document-settings document) :file-tags))
        (when (funcall matches-p tag)
          (setf tags (effective-tag-add tags tag))))
      (dolist (ancestor (reverse ancestors))
        (dolist (tag (node-property ancestor :tags))
          (when (funcall matches-p tag)
            (setf tags (effective-tag-add tags tag))))))
    (dolist (tag (node-property heading :tags))
      (setf tags (effective-tag-add tags tag)))
    tags))

(defun effective-property-name (name)
  (string-upcase
   (typecase name
     (string name)
     (symbol (symbol-name name))
     (t (error 'type-error :datum name :expected-type '(or string symbol))))))

(defun effective-heading-property-entries (heading key)
  (loop for child in (node-children heading)
        append (loop for drawer in
                       (if (eq (node-type child) :section)
                           (remove-if-not
                            (lambda (node) (eq (node-type node) :property-drawer))
                            (node-children child))
                           (if (eq (node-type child) :property-drawer)
                               (list child)
                               nil))
                     append (loop for property in (node-children drawer)
                                  when (and (eq (node-type property) :node-property)
                                            (stringp (node-property property :key))
                                            (string-equal
                                             (node-property property :key) key))
                                    collect property))))

(defun effective-heading-property-value (heading key)
  (let ((entry (first (effective-heading-property-entries heading key))))
    (if entry
        (values (node-property entry :value) t)
        (values nil nil))))

(defun effective-property-global-value (document name)
  (let ((found-p nil)
        (value nil))
    (dolist (entry (getf (document-settings document) :property-settings))
      (when (and (consp entry)
                 (stringp (car entry))
                 (string-equal (car entry) name))
        (setf found-p t
              value (cdr entry))))
    (values value found-p)))

(defun effective-property-hierarchy-value (headings name global-value global-found-p)
  (let ((values (when global-found-p (list global-value)))
        (sources (when global-found-p (list :document)))
        (plus-p nil))
    (dolist (heading headings)
      (multiple-value-bind (value found-p)
          (effective-heading-property-value heading name)
        (when found-p
          (setf values (list value)
                sources (list heading)
                plus-p nil)))
      (multiple-value-bind (value found-p)
          (effective-heading-property-value heading
                                            (concatenate 'string name "+"))
        (when found-p
          (setf values (append values (list value))
                plus-p t)
          (unless (member heading sources :test #'eq)
            (setf sources (append sources (list heading)))))))
    (values values sources plus-p)))

(defun effective-property (document heading name &key (inherit nil))
  "Return HEADING's effective property value and its source.
The source is a heading node, a root-to-local contributor list when PROPERTY+
contributes, :DOCUMENT for a #+PROPERTY fallback, or NIL when absent."
  (let* ((heading (effective-semantics-heading document heading))
         (name (effective-property-name name))
         (matches-p (effective-selector-predicate inherit))
         (inherit-p (funcall matches-p name))
         (headings (append
                    (when inherit-p
                      (reverse (effective-heading-ancestors heading)))
                    (list heading)))
         (global-value nil)
         (global-found-p nil))
    (when inherit-p
      (multiple-value-setq (global-value global-found-p)
        (effective-property-global-value document name)))
    (multiple-value-bind (values sources plus-p)
        (effective-property-hierarchy-value headings name
                                            global-value global-found-p)
      (when values
        (return-from effective-property
          (if plus-p
              (values (format nil "~{~A~^~%~}" values) sources)
              (values (car values) (car (last sources))))))
      (values nil nil))))

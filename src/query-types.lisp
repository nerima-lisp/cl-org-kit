;;;; src/query-types.lisp
(in-package #:org-kit)

(defstruct (query-context (:constructor %make-query-context)
                          (:conc-name %query-context-)
                          (:predicate %query-context-p)
                          (:copier nil))
  "Caller-supplied values used when resolving Org properties and matching tags."
  (tag-groups nil :type list)
  (property-inheritance-p nil :type boolean)
  (tag-inheritance-p t :type boolean)
  (file nil)
  (category nil)
  (blocked-provider nil)
  (clock-sum-provider nil)
  (clock-sum-today-provider nil)
  (case-fold-p nil :type boolean))

(defstruct (query-ir (:constructor %make-query-ir (scope predicate context))
                     (:conc-name query-ir-)
                     (:copier nil))
  "Internal canonical query shared by the public query parsers."
  scope
  predicate
  context)

(defstruct (query-regex (:constructor %make-query-regex (source compiled source-map))
                        (:conc-name query-regex-)
                        (:copier nil))
  "Internal Emacs-regex source and its compiled cl-regex-kit matcher."
  source
  compiled
  source-map)

(defun query-context-p (object)
  "Return true when OBJECT is a query context accepted by query functions."
  (%query-context-p object))

(defun %query-proper-list-p (value)
  (and (listp value)
       (handler-case (integerp (list-length value))
         (type-error () nil))))

(defun %query-copy-proper-tree (value)
  (let* ((limits (make-limits))
         (active (make-hash-table :test #'eq))
         (cons-count 0))
    (labels ((copy-value (item depth)
               (if (consp item)
                   (copy-list-value item depth)
                   item))
             (copy-list-value (list depth)
               (check-limit limits :max-element-depth depth)
               (let ((cursor list)
                     (cells '())
                     (values '()))
                 (unwind-protect
                      (progn
                        (loop while (consp cursor)
                              do (when (gethash cursor active)
                                   (error 'type-error :datum :invalid-list
                                          :expected-type 'list))
                                 (incf cons-count)
                                 (check-limit limits :max-node-count cons-count)
                                 (setf (gethash cursor active) t)
                                 (push cursor cells)
                                 (push (copy-value (car cursor) (1+ depth)) values)
                                 (setf cursor (cdr cursor)))
                        (unless (null cursor)
                          (error 'type-error :datum :invalid-list
                                 :expected-type 'list))
                        (nreverse values))
                   (dolist (cell cells)
                     (remhash cell active))))))
      (copy-value value 0))))

(defun make-query-context (&key tag-groups
                                (property-inheritance-p nil)
                                (tag-inheritance-p t)
                                file category
                                blocked-provider clock-sum-provider
                                clock-sum-today-provider
                                (case-fold-p nil))
  "Return a query context with tag, property, provider and case-fold settings.
TAG-GROUPS is a proper list in the same shape as document :TAG-ALIST entries.
PROPERTY-INHERITANCE-P defaults to false; TAG-INHERITANCE-P defaults to true.
FILE and CATEGORY override document-derived values. Provider functions receive
the nearest headline node and return a value or NIL. CASE-FOLD-P controls only
regex matching."
  (unless (%query-proper-list-p tag-groups)
    (error 'type-error :datum tag-groups :expected-type 'list))
  (check-type property-inheritance-p boolean)
  (check-type tag-inheritance-p boolean)
  (check-type case-fold-p boolean)
  (check-type file (or null string))
  (check-type category (or null string))
  (check-type blocked-provider (or null function))
  (check-type clock-sum-provider (or null function))
  (check-type clock-sum-today-provider (or null function))
  (%make-query-context :tag-groups (%query-copy-proper-tree tag-groups)
                       :property-inheritance-p property-inheritance-p
                       :tag-inheritance-p tag-inheritance-p
                       :file file
                       :category category
                       :blocked-provider blocked-provider
                       :clock-sum-provider clock-sum-provider
                       :clock-sum-today-provider clock-sum-today-provider
                       :case-fold-p case-fold-p))

(defun query-context-tag-groups (context)
  "Return a fresh copy of CONTEXT's caller-supplied tag group definitions."
  (%query-copy-proper-tree (%query-context-tag-groups context)))

(defun query-context-property-inheritance-p (context)
  "Return whether property lookup inherits properties from ancestor headings."
  (%query-context-property-inheritance-p context))

(defun query-context-tag-inheritance-p (context)
  "Return whether tag lookup includes ancestor and file tags."
  (%query-context-tag-inheritance-p context))

(defun query-context-file (context)
  "Return CONTEXT's explicit FILE override, or NIL to use document metadata."
  (%query-context-file context))

(defun query-context-category (context)
  "Return CONTEXT's explicit CATEGORY override, or NIL to derive it."
  (%query-context-category context))

(defun query-context-blocked-provider (context)
  "Return CONTEXT's BLOCKED provider function, or NIL when none is configured."
  (%query-context-blocked-provider context))

(defun query-context-clock-sum-provider (context)
  "Return CONTEXT's CLOCKSUM provider function, or NIL when none is configured."
  (%query-context-clock-sum-provider context))

(defun query-context-clock-sum-today-provider (context)
  "Return CONTEXT's CLOCKSUM_T provider function, or NIL when none is configured."
  (%query-context-clock-sum-today-provider context))

(defun query-context-case-fold-p (context)
  "Return whether regex matching uses the query context's case-fold option."
  (%query-context-case-fold-p context))

(defun query-nearest-headline (node)
  (loop for current = node then (node-parent current)
        while current
        when (eq (node-type current) :headline)
          return current))

(defun query-nearest-parent-headline (headline)
  (loop for current = (node-parent headline) then (node-parent current)
        while current
        when (eq (node-type current) :headline)
          return current))

(defun query-heading-sibling-index (headline)
  (let* ((root (node-root headline))
         (parent-headline (query-nearest-parent-headline headline))
         (title (node-property headline :raw-value ""))
         (index nil)
         (ordinal 0))
    (map-nodes
     (lambda (candidate)
       (when (and (eq (node-type candidate) :headline)
                  (eq (query-nearest-parent-headline candidate) parent-headline)
                  (equal (node-property candidate :raw-value "") title))
         (incf ordinal)
         (when (eq candidate headline)
           (setf index ordinal))))
     root)
    (or index 1)))

(defun node-heading-path (node)
  "Return NODE's headline ancestors from the root as title/index plists.
Each segment is (:TITLE raw-value :INDEX sibling-ordinal); a headline includes
itself, while a non-headline node includes only its headline ancestors."
  (let ((headlines '()))
    (loop for current = (if (eq (node-type node) :headline)
                            node
                            (node-parent node))
            then (node-parent current)
          while current
          when (eq (node-type current) :headline)
            do (push current headlines))
    (mapcar (lambda (headline)
              (list :title (node-property headline :raw-value "")
                    :index (query-heading-sibling-index headline)))
            headlines)))

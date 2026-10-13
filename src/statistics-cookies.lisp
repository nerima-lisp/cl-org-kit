;;;; src/statistics-cookies.lisp
(in-package #:org-kit)

(defstruct (statistics-cookie-update
            (:constructor make-statistics-cookie-update (&key node old-value new-value))
            (:copier nil)
            (:predicate statistics-cookie-update-p))
  "A changed statistics cookie and its source and replacement values."
  (node nil :read-only t)
  (old-value nil :read-only t)
  (new-value nil :read-only t))

(eval-when (:load-toplevel :execute)
  (setf (documentation 'make-statistics-cookie-update 'function)
        "Construct a statistics cookie update from its node and old/new values."
        (documentation 'statistics-cookie-update-p 'function)
        "Return true for a statistics cookie update value."
        (documentation 'statistics-cookie-update-node 'function)
        "Return the cookie node in the input document."
        (documentation 'statistics-cookie-update-old-value 'function)
        "Return the cookie's source spelling before the edit."
        (documentation 'statistics-cookie-update-new-value 'function)
        "Return the cookie's replacement spelling."))

(defun statistics-cookie-data-tokens (value)
  (when (stringp value)
    (loop with start = nil
          with tokens = nil
          for index from 0 to (length value)
          for boundary-p = (or (= index (length value))
                               (find (char value index)
                                     '(#\Space #\Tab #\Newline #\Return #\Page)))
          do (cond
               ((and boundary-p start)
                (push (string-downcase (subseq value start index)) tokens)
                (setf start nil))
               ((and (not boundary-p) (null start))
                (setf start index)))
          finally (return (nreverse tokens)))))

(defun statistics-cookie-data-mode (tokens)
  (cond ((member "checkbox" tokens :test #'string=) :checkbox)
        ((member "todo" tokens :test #'string=) :todo)
        ((member "recursive" tokens :test #'string=) :checkbox)))

(defun statistics-cookie-title-nodes (heading)
  (let ((nodes nil))
    (dolist (segment (node-property heading :title))
      (when (node-p segment)
        (map-nodes (lambda (node) (push node nodes)) segment)))
    (nreverse nodes)))

(defstruct statistics-cookie-count
  (done 0)
  (total 0))

(defstruct statistics-cookie-context
  owner-heading
  active-item
  parent-item)

(defstruct statistics-cookie-analysis
  cookies
  title-cookie-set
  cookie-items
  todo-direct
  todo-recursive
  checkbox-direct
  checkbox-recursive
  item-descendant-counts
  item-direct-counts)

(defun statistics-cookie-empty-count ()
  (make-statistics-cookie-count))

(defun statistics-cookie-count-add (counts done total)
  (incf (statistics-cookie-count-done counts) done)
  (incf (statistics-cookie-count-total counts) total)
  counts)

(defun statistics-cookie-analyze-heading (heading)
  (let ((title-cookies nil)
        (body-cookies nil)
        (title-cookie-set (make-hash-table :test #'eq))
        (cookie-items (make-hash-table :test #'eq))
        (contexts (make-hash-table :test #'eq))
        (item-subtree-counts (make-hash-table :test #'eq))
        (item-descendant-counts (make-hash-table :test #'eq))
        (item-direct-counts (make-hash-table :test #'eq))
        (visited-reverse nil)
        (todo-direct (statistics-cookie-empty-count))
        (todo-recursive (statistics-cookie-empty-count))
        (checkbox-direct (statistics-cookie-empty-count))
        (checkbox-recursive (statistics-cookie-empty-count)))
    (dolist (node (statistics-cookie-title-nodes heading))
      (when (eq (node-type node) :statistics-cookie)
        (push node title-cookies)
        (setf (gethash node title-cookie-set) t)))
    (map-nodes
     (lambda (node)
       (push node visited-reverse)
       (let* ((type (node-type node))
              (heading-p (eq type :headline))
              (parent-context (gethash (node-parent node) contexts))
              (owner-heading (if (and heading-p
                                      (or (eq node heading)
                                          (eq type :headline)))
                                 node
                                 (and parent-context
                                      (statistics-cookie-context-owner-heading
                                       parent-context))))
              (parent-item (and (eq owner-heading heading)
                                parent-context
                                (statistics-cookie-context-active-item
                                 parent-context)))
              (item-p (eq type :item))
              (item-parent (and item-p parent-item))
              (active-item (and (eq owner-heading heading)
                                (if item-p node parent-item))))
         (setf (gethash node contexts)
               (make-statistics-cookie-context
                :owner-heading owner-heading
                :active-item active-item
                :parent-item item-parent))
         (when (and heading-p (not (eq node heading)))
           (let ((state (node-property node :todo-type)))
             (case state
               (:todo
                (incf (statistics-cookie-count-total todo-recursive))
                (when (eq (and parent-context
                               (statistics-cookie-context-owner-heading parent-context))
                          heading)
                  (incf (statistics-cookie-count-total todo-direct))))
               (:done
                (incf (statistics-cookie-count-done todo-recursive))
                (incf (statistics-cookie-count-total todo-recursive))
                (when (eq (and parent-context
                               (statistics-cookie-context-owner-heading parent-context))
                          heading)
                  (incf (statistics-cookie-count-done todo-direct))
                  (incf (statistics-cookie-count-total todo-direct)))))))
         (when (and item-p (eq owner-heading heading))
           (let* ((checkbox (node-property node :checkbox))
                  (checked-p (member checkbox '(:on :off :trans) :test #'eq))
                  (done (if (eq checkbox :on) 1 0))
                  (total (if checked-p 1 0)))
             (setf (gethash node item-subtree-counts)
                   (make-statistics-cookie-count :done done :total total)
                   (gethash node item-direct-counts)
                   (statistics-cookie-empty-count))
             (when checked-p
               (statistics-cookie-count-add checkbox-recursive done total)
               (if item-parent
                   (statistics-cookie-count-add
                    (gethash item-parent item-direct-counts) done total)
                   (statistics-cookie-count-add checkbox-direct done total)))))
         (when (and (eq type :statistics-cookie)
                    (eq owner-heading heading)
                    active-item)
           (push node body-cookies)
           (setf (gethash node cookie-items) active-item))))
     heading)
    (dolist (node visited-reverse)
      (when (and (eq (node-type node) :item)
                 (eq (statistics-cookie-context-owner-heading
                      (gethash node contexts))
                     heading))
        (let* ((counts (gethash node item-subtree-counts))
               (checkbox (node-property node :checkbox))
               (own-total (if (member checkbox '(:on :off :trans) :test #'eq)
                              1 0))
               (own-done (if (eq checkbox :on) 1 0))
               (parent-item (statistics-cookie-context-parent-item
                             (gethash node contexts))))
          (setf (gethash node item-descendant-counts)
                (make-statistics-cookie-count
                 :done (- (statistics-cookie-count-done counts) own-done)
                 :total (- (statistics-cookie-count-total counts) own-total)))
          (when parent-item
            (statistics-cookie-count-add
             (gethash parent-item item-subtree-counts)
             (statistics-cookie-count-done counts)
             (statistics-cookie-count-total counts))))))
    (setf title-cookies (nreverse title-cookies)
          body-cookies (nreverse body-cookies))
    (make-statistics-cookie-analysis
     :cookies (append title-cookies body-cookies)
     :title-cookie-set title-cookie-set
     :cookie-items cookie-items
     :todo-direct todo-direct
     :todo-recursive todo-recursive
     :checkbox-direct checkbox-direct
     :checkbox-recursive checkbox-recursive
     :item-descendant-counts item-descendant-counts
     :item-direct-counts item-direct-counts)))

(defun statistics-cookie-local-data-tokens (heading)
  (let ((values nil))
    (multiple-value-bind (value found-p)
        (effective-heading-property-value heading "COOKIE_DATA")
      (when found-p (push value values)))
    (dolist (entry (effective-heading-property-entries heading "COOKIE_DATA+"))
      (push (node-property entry :value) values))
    (loop for value in (nreverse values)
          append (statistics-cookie-data-tokens value))))

(defun statistics-cookie-settings (document heading default-mode)
  (multiple-value-bind (data source)
      (effective-property document heading "COOKIE_DATA" :inherit t)
    (let* ((tokens (statistics-cookie-data-tokens data))
           (local-tokens (statistics-cookie-local-data-tokens heading))
           (configured-mode (statistics-cookie-data-mode tokens))
           (mode (or configured-mode
                     (if source :todo default-mode)))
           (checkbox-recursive-p
             (member "recursive" local-tokens :test #'string=))
           (todo-recursive-p (member "recursive" tokens :test #'string=)))
      (values mode (not (null checkbox-recursive-p))
              (not (null todo-recursive-p))
              (eq configured-mode :todo)))))

(defun statistics-cookie-replacement (cookie done total)
  (let* ((value (node-property cookie :value))
         (kind (char value (- (length value) 2))))
    (if (char= kind #\%)
        (format nil "[~D%]" (if (zerop total) 0 (floor (* done 100) total)))
        (format nil "[~D/~D]" done total))))

(defun statistics-cookie-index-canonical-nodes (original canonical index)
  (cond
    ((node-p original)
     (setf (gethash original index) canonical)
     (let ((canonical-properties (getf canonical :properties)))
       (loop for (key value) on (node-properties original) by #'cddr
             unless (edit-source-property-p key)
               do (statistics-cookie-index-canonical-nodes
                   value (getf canonical-properties key) index)))
     (loop for original-child in (node-children original)
           for canonical-child in (getf canonical :children)
           do (statistics-cookie-index-canonical-nodes
               original-child canonical-child index)))
    ((consp original)
     (statistics-cookie-index-canonical-nodes (car original) (car canonical) index)
     (statistics-cookie-index-canonical-nodes (cdr original) (cdr canonical) index)))
  index)

(defun statistics-cookie-update-canonical-values (document expected updates)
  (let ((index (make-hash-table :test #'eq)))
    (statistics-cookie-index-canonical-nodes
     (document-root document) expected index)
    (dolist (update updates)
      (let* ((canonical (gethash (statistics-cookie-update-node update) index))
             (properties (getf canonical :properties)))
        (setf (getf properties :value)
              (statistics-cookie-update-new-value update)
              (getf canonical :properties) properties)))
    expected))

(defun statistics-cookie-update-title-raw-value
    (document expected heading title-cookie-set updates)
  (let ((title-updates
          (remove-if-not
           (lambda (update)
             (gethash (statistics-cookie-update-node update) title-cookie-set))
           (reverse updates))))
    (when title-updates
      (multiple-value-bind (title-start title-end)
          (edit-headline-title-span document heading)
        (declare (ignore title-end))
        (let* ((raw-value (node-property heading :raw-value))
               (patches
                 (mapcar
                  (lambda (update)
                    (let* ((cookie (statistics-cookie-update-node update))
                           (start (- (node-begin cookie) title-start)))
                      (edit-make-patch
                       start (+ start (length (statistics-cookie-update-old-value update)))
                       (statistics-cookie-update-new-value update))))
                  title-updates))
               (new-value (edit-splice-source raw-value patches))
               (path (edit-node-path document heading)))
          (edit-canonical-set-property expected path :raw-value new-value))))))

(defun update-statistics-cookies (document heading)
  "Recalculate statistics cookies owned by HEADING and return their edit result.

The second value lists only cookies whose source spelling changed, in source order."
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document))
  (let* ((heading (edit-resolve-node document heading '(:headline :inlinetask)))
         (expected (edit-canonical-copy (edit-canonical-document document)))
         (patches nil)
         (updates nil)
         (analysis (statistics-cookie-analyze-heading heading)))
      (multiple-value-bind (mode checkbox-recursive-p todo-recursive-p todo-mode-p)
          (statistics-cookie-settings
           document heading
           (if (plusp (statistics-cookie-count-total
                       (statistics-cookie-analysis-checkbox-direct analysis)))
               :checkbox :todo))
        (dolist (cookie (statistics-cookie-analysis-cookies analysis))
          (let* ((owner-item
                   (gethash cookie (statistics-cookie-analysis-cookie-items analysis)))
                 (cookie-mode (if owner-item :checkbox mode))
                 (counts
                   (if (eq cookie-mode :checkbox)
                       (if owner-item
                           (gethash owner-item
                                    (if checkbox-recursive-p
                                        (statistics-cookie-analysis-item-descendant-counts
                                         analysis)
                                        (statistics-cookie-analysis-item-direct-counts
                                         analysis)))
                           (if checkbox-recursive-p
                               (statistics-cookie-analysis-checkbox-recursive analysis)
                               (statistics-cookie-analysis-checkbox-direct analysis)))
                       (if todo-recursive-p
                           (statistics-cookie-analysis-todo-recursive analysis)
                           (statistics-cookie-analysis-todo-direct analysis)))))
            (unless (and owner-item todo-mode-p)
              (let* ((done (statistics-cookie-count-done counts))
                     (total (statistics-cookie-count-total counts))
                     (old-value (node-property cookie :value))
                     (new-value
                       (statistics-cookie-replacement cookie done total)))
                (unless (string= old-value new-value)
                  (let ((start (node-begin cookie))
                        (end (- (node-end cookie) (node-post-blank cookie))))
                    (push (edit-make-patch start end new-value heading) patches)
                    (push (make-statistics-cookie-update
                           :node cookie :old-value old-value :new-value new-value)
                          updates)))))))
      (statistics-cookie-update-title-raw-value
       document expected heading
       (statistics-cookie-analysis-title-cookie-set analysis) updates)
    (when updates
      (statistics-cookie-update-canonical-values document expected updates))
    (values (edit-apply-patches-result document (nreverse patches)
                                       :target heading :expected expected)
            (nreverse updates)))))

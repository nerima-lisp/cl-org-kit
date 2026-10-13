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

(defun statistics-cookie-count-vector ()
  (vector 0 0))

(defun statistics-cookie-count-vector-add (counts done total)
  (incf (aref counts 0) done)
  (incf (aref counts 1) total)
  counts)

(defun statistics-cookie-analysis (heading)
  (let ((title-cookies nil)
        (body-cookies nil)
        (title-cookie-set (make-hash-table :test #'eq))
        (cookie-items (make-hash-table :test #'eq))
        (contexts (make-hash-table :test #'eq))
        (item-subtree-counts (make-hash-table :test #'eq))
        (item-descendant-counts (make-hash-table :test #'eq))
        (item-direct-counts (make-hash-table :test #'eq))
        (visited-reverse nil)
        (todo-direct (statistics-cookie-count-vector))
        (todo-recursive (statistics-cookie-count-vector))
        (checkbox-direct (statistics-cookie-count-vector))
        (checkbox-recursive (statistics-cookie-count-vector)))
    (dolist (node (statistics-cookie-title-nodes heading))
      (when (eq (node-type node) :statistics-cookie)
        (push node title-cookies)
        (setf (gethash node title-cookie-set) t)))
    (map-nodes
     (lambda (node)
       (push node visited-reverse)
       (let* ((type (node-type node))
              (heading-p (member type '(:headline :inlinetask) :test #'eq))
              (parent-context (gethash (node-parent node) contexts))
              (owner-heading (if heading-p
                                 node
                                 (and parent-context
                                      (aref parent-context 0))))
              (parent-item (and (eq owner-heading heading)
                                parent-context
                                (aref parent-context 1)))
              (item-p (eq type :item))
              (item-parent (and item-p parent-item))
              (active-item (and (eq owner-heading heading)
                                (if item-p node parent-item))))
         (setf (gethash node contexts)
               (vector owner-heading active-item item-parent))
         (when (and heading-p (not (eq node heading)))
           (let ((state (node-property node :todo-type)))
             (case state
               (:todo
                (incf (aref todo-recursive 1))
                (when (eq (and parent-context (aref parent-context 0)) heading)
                  (incf (aref todo-direct 1))))
               (:done
                (incf (aref todo-recursive 0))
                (incf (aref todo-recursive 1))
                (when (eq (and parent-context (aref parent-context 0)) heading)
                  (incf (aref todo-direct 0))
                  (incf (aref todo-direct 1)))))))
         (when (and item-p (eq owner-heading heading))
           (let* ((checkbox (node-property node :checkbox))
                  (checked-p (member checkbox '(:on :off :trans) :test #'eq))
                  (done (if (eq checkbox :on) 1 0))
                  (total (if checked-p 1 0)))
             (setf (gethash node item-subtree-counts)
                   (vector done total)
                   (gethash node item-direct-counts)
                   (statistics-cookie-count-vector))
             (when checked-p
               (statistics-cookie-count-vector-add checkbox-recursive done total)
               (if item-parent
                   (statistics-cookie-count-vector-add
                    (gethash item-parent item-direct-counts) done total)
                   (statistics-cookie-count-vector-add checkbox-direct done total)))))
         (when (and (eq type :statistics-cookie)
                    (eq owner-heading heading)
                    active-item)
           (push node body-cookies)
           (setf (gethash node cookie-items) active-item))))
     heading)
    (dolist (node visited-reverse)
      (when (and (eq (node-type node) :item)
                 (eq (aref (gethash node contexts) 0) heading))
        (let* ((counts (gethash node item-subtree-counts))
               (checkbox (node-property node :checkbox))
               (own-total (if (member checkbox '(:on :off :trans) :test #'eq)
                              1 0))
               (own-done (if (eq checkbox :on) 1 0))
               (parent-item (aref (gethash node contexts) 2)))
          (setf (gethash node item-descendant-counts)
                (vector (- (aref counts 0) own-done)
                        (- (aref counts 1) own-total)))
          (when parent-item
            (statistics-cookie-count-vector-add
             (gethash parent-item item-subtree-counts)
             (aref counts 0) (aref counts 1))))))
    (setf title-cookies (nreverse title-cookies)
          body-cookies (nreverse body-cookies))
    (values (append title-cookies body-cookies)
            title-cookie-set
            cookie-items
            todo-direct todo-recursive
            checkbox-direct checkbox-recursive
            item-descendant-counts item-direct-counts)))

(defun statistics-cookie-settings (document heading)
  (multiple-value-bind (data source)
      (effective-property document heading "COOKIE_DATA" :inherit t)
    (let* ((tokens (statistics-cookie-data-tokens data))
           (mode (or (statistics-cookie-data-mode tokens) :todo))
           (local-p (or (eq source heading)
                        (and (listp source)
                             (member heading source :test #'eq))))
           (checkbox-recursive-p
             (and local-p (member "recursive" tokens :test #'string=)))
           (todo-recursive-p (member "recursive" tokens :test #'string=)))
      (values mode (not (null checkbox-recursive-p))
              (not (null todo-recursive-p))))))

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
     (statistics-cookie-index-canonical-nodes (cdr original) (cdr canonical) index))
    ((and (vectorp original) (not (stringp original)))
     (loop for position below (length original)
           do (statistics-cookie-index-canonical-nodes
               (aref original position) (aref canonical position) index))))
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
         (updates nil))
    (multiple-value-bind
          (cookies title-cookie-set cookie-items
           todo-direct todo-recursive checkbox-direct checkbox-recursive
           item-descendant-counts item-direct-counts)
        (statistics-cookie-analysis heading)
      (multiple-value-bind (mode checkbox-recursive-p todo-recursive-p)
          (statistics-cookie-settings document heading)
        (dolist (cookie cookies)
          (let* ((owner-item (gethash cookie cookie-items))
                 (cookie-mode (if owner-item :checkbox mode))
                 (counts
                   (if (eq cookie-mode :checkbox)
                       (if owner-item
                           (gethash owner-item
                                    (if checkbox-recursive-p
                                        item-descendant-counts
                                        item-direct-counts))
                           (if checkbox-recursive-p
                               checkbox-recursive
                               checkbox-direct))
                       (if todo-recursive-p todo-recursive todo-direct)))
                 (done (aref counts 0))
                 (total (aref counts 1))
                 (old-value (node-property cookie :value))
                 (new-value (statistics-cookie-replacement cookie done total)))
            (unless (string= old-value new-value)
              (let ((start (node-begin cookie))
                    (end (- (node-end cookie) (node-post-blank cookie))))
                (push (edit-make-patch start end new-value heading) patches)
                (push (make-statistics-cookie-update
                       :node cookie :old-value old-value :new-value new-value)
                      updates)))))
      (statistics-cookie-update-title-raw-value
       document expected heading title-cookie-set updates))
    (when updates
      (statistics-cookie-update-canonical-values document expected updates))
    (values (edit-apply-patches-result document (nreverse patches)
                                       :target heading :expected expected)
            (nreverse updates)))))

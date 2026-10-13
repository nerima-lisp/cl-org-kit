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

(defun statistics-cookie-ancestor (node types)
  (loop for parent = (node-parent node) then (node-parent parent)
        while parent
        when (member (node-type parent) types :test #'eq)
          return parent))

(defun statistics-cookie-owner-heading (node)
  (statistics-cookie-ancestor node '(:headline :inlinetask)))

(defun statistics-cookie-owner-item (node)
  (statistics-cookie-ancestor node '(:item)))

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

(defun statistics-cookie-title-node-p (heading cookie)
  (member cookie (statistics-cookie-title-nodes heading) :test #'eq))

(defun statistics-cookie-context (document heading cookie)
  (let ((item (statistics-cookie-owner-item cookie)))
    (multiple-value-bind (data source)
        (effective-property document heading "COOKIE_DATA" :inherit t)
      (let* ((tokens (statistics-cookie-data-tokens data))
             (mode (if item :checkbox
                       (and (statistics-cookie-title-node-p heading cookie)
                            (or (statistics-cookie-data-mode tokens) :todo))))
             (local-p (or (eq source heading)
                          (and (listp source)
                               (member heading source :test #'eq))))
             (recursive-p
               (if (eq mode :checkbox)
                   (and local-p
                        (member "recursive" tokens :test #'string=))
                   (or (member "recursive" tokens :test #'string=)
                       (multiple-value-bind
                             (indicator hierarchical-value)
                           (get-properties
                            (document-settings document)
                            '(:org-hierarchical-todo-statistics))
                         (and (eq indicator :org-hierarchical-todo-statistics)
                              (not hierarchical-value)))))))
        (values mode item (not (null recursive-p)))))))

(defun statistics-cookie-in-scope-p (heading cookie title-nodes)
  (and (eq (statistics-cookie-owner-heading cookie) heading)
       (or (statistics-cookie-owner-item cookie)
           (member cookie title-nodes :test #'eq))))

(defun statistics-cookie-nodes (heading)
  (let ((title-nodes (statistics-cookie-title-nodes heading))
        (cookies nil))
    (dolist (node title-nodes)
      (when (eq (node-type node) :statistics-cookie)
        (push node cookies)))
    (map-nodes (lambda (node)
                 (when (and (eq (node-type node) :statistics-cookie)
                            (statistics-cookie-in-scope-p heading node title-nodes))
                   (push node cookies)))
               heading)
    (stable-sort (remove-duplicates cookies :test #'eq) #'< :key #'node-begin)))

(defun statistics-cookie-node-below-p (node ancestor)
  (loop for parent = (node-parent node) then (node-parent parent)
        while parent
        thereis (eq parent ancestor)))

(defun statistics-cookie-todo-count (heading recursive-p)
  (let ((done 0)
        (total 0))
    (map-nodes
     (lambda (node)
       (when (and (not (eq node heading))
                  (member (node-type node) '(:headline :inlinetask) :test #'eq)
                  (if recursive-p
                      (statistics-cookie-node-below-p node heading)
                      (eq (statistics-cookie-owner-heading node) heading)))
         (case (node-property node :todo-type)
           (:todo (incf total))
           (:done (incf total) (incf done)))))
     heading)
    (values done total)))

(defun statistics-cookie-nearest-item-ancestor (node heading)
  (loop for parent = (node-parent node) then (node-parent parent)
        while (and parent (not (eq parent heading)))
        when (eq (node-type parent) :item)
          return parent))

(defun statistics-cookie-checkbox-count (heading owner-item recursive-p)
  (let ((done 0)
        (total 0))
    (map-nodes
     (lambda (node)
       (when (and (eq (node-type node) :item)
                  (not (eq node owner-item))
                  (eq (statistics-cookie-owner-heading node) heading)
                  (or (null owner-item)
                      (statistics-cookie-node-below-p node owner-item))
                  (or recursive-p
                      (eq (statistics-cookie-nearest-item-ancestor node heading)
                          owner-item)))
         (let ((checkbox (node-property node :checkbox)))
           (when (member checkbox '(:on :off :trans) :test #'eq)
             (incf total)
             (when (eq checkbox :on)
               (incf done))))))
     heading)
    (values done total)))

(defun statistics-cookie-replacement (cookie done total)
  (let* ((value (node-property cookie :value))
         (kind (char value (- (length value) 2))))
    (if (char= kind #\%)
        (format nil "[~D%]" (if (zerop total) 0 (floor (* done 100) total)))
        (format nil "[~D/~D]" done total))))

(defun statistics-cookie-update-canonical-value (original canonical target value)
  (cond
    ((eq original target)
     (let ((properties (getf canonical :properties)))
       (setf (getf properties :value) value
             (getf canonical :properties) properties)
       (values canonical t)))
    ((node-p original)
     (let ((properties (getf canonical :properties)))
       (loop for (key original-value) on (node-properties original) by #'cddr
             unless (edit-source-property-p key)
               do (multiple-value-bind (updated found-p)
                      (statistics-cookie-update-canonical-value
                       original-value (getf properties key) target value)
                    (when found-p
                      (setf (getf properties key) updated
                            (getf canonical :properties) properties)
                      (return-from statistics-cookie-update-canonical-value
                        (values canonical t)))))
       (loop for original-child in (node-children original)
             for index from 0
             for canonical-child in (getf canonical :children)
             do (multiple-value-bind (updated found-p)
                    (statistics-cookie-update-canonical-value
                     original-child canonical-child target value)
                  (when found-p
                    (setf (nth index (getf canonical :children)) updated)
                    (return-from statistics-cookie-update-canonical-value
                      (values canonical t)))))
       (values canonical nil)))
    ((consp original)
     (multiple-value-bind (updated found-p)
         (statistics-cookie-update-canonical-value
          (car original) (car canonical) target value)
       (when found-p
         (setf (car canonical) updated)
         (return-from statistics-cookie-update-canonical-value
           (values canonical t))))
     (multiple-value-bind (updated found-p)
         (statistics-cookie-update-canonical-value
          (cdr original) (cdr canonical) target value)
       (when found-p
         (setf (cdr canonical) updated)
         (return-from statistics-cookie-update-canonical-value
           (values canonical t))))
     (values canonical nil))
    ((and (vectorp original) (not (stringp original)))
     (loop for original-value across original
           for index from 0
           do (multiple-value-bind (updated found-p)
                  (statistics-cookie-update-canonical-value
                   original-value (aref canonical index) target value)
                (when found-p
                  (let ((copy (copy-seq canonical)))
                    (setf (aref copy index) updated)
                    (return-from statistics-cookie-update-canonical-value
                      (values copy t))))))
     (values canonical nil))
    (t (values canonical nil))))

(defun statistics-cookie-update-title-raw-value (document expected heading updates)
  (let ((title-updates
          (remove-if-not
           (lambda (update)
             (statistics-cookie-title-node-p
              heading (statistics-cookie-update-node update)))
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
    (dolist (cookie (statistics-cookie-nodes heading))
      (multiple-value-bind (mode owner-item recursive-p)
          (statistics-cookie-context document heading cookie)
        (multiple-value-bind (done total)
            (if (eq mode :checkbox)
                (statistics-cookie-checkbox-count heading owner-item recursive-p)
                (statistics-cookie-todo-count heading recursive-p))
          (let* ((old-value (node-property cookie :value))
                 (new-value (statistics-cookie-replacement cookie done total)))
            (unless (string= old-value new-value)
              (let* ((start (node-begin cookie))
                     (end (- (node-end cookie) (node-post-blank cookie))))
                (push (edit-make-patch start end new-value heading) patches)
                (push (make-statistics-cookie-update
                       :node cookie :old-value old-value :new-value new-value)
                      updates)
                (setf expected
                      (statistics-cookie-update-canonical-value
                       (document-root document) expected cookie new-value))))))))
    (statistics-cookie-update-title-raw-value document expected heading updates)
    (values (edit-apply-patches-result document (nreverse patches)
                                       :target heading :expected expected)
            (nreverse updates))))

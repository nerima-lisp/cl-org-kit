;;;; src/agenda-stuck-projects.lisp
(in-package #:org-kit)

(defun agenda-stuck-string-list-p (value)
  (let ((length (agenda-proper-list-length value)))
    (and length (every #'stringp value))))

(defun agenda-stuck-projects-options (stuck-projects)
  (let ((length (agenda-proper-list-length stuck-projects)))
    (unless (and length (= length 4))
      (agenda-signal-error :invalid-option stuck-projects
                           "Stuck-project criteria must contain four values")))
  (destructuring-bind (matcher todo-veto tags-veto subtree-regexp)
      stuck-projects
    (unless (and (stringp matcher)
                 (agenda-stuck-string-list-p todo-veto)
                 (agenda-stuck-string-list-p tags-veto)
                 (or (null subtree-regexp) (stringp subtree-regexp)))
      (agenda-signal-error :invalid-option stuck-projects
                           "Stuck-project criteria have an invalid value"))
    (when (and (null todo-veto) (null tags-veto)
               (or (null subtree-regexp) (zerop (length subtree-regexp))))
      (agenda-signal-error :missing-unstuck-criteria stuck-projects
                           "Stuck-project criteria need at least one veto"))
    (values matcher todo-veto tags-veto
            (and subtree-regexp (plusp (length subtree-regexp))
                 subtree-regexp))))

(defun agenda-stuck-projects-operation-limits (documents)
  (let ((limits (mapcar #'document-limits documents)))
    (make-limits
     :max-input-length (minimum-limit-value limits :max-input-length)
     :max-element-depth (minimum-limit-value limits :max-element-depth)
     :max-search-regex-length
     (minimum-limit-value limits :max-search-regex-length)
     :max-agenda-entries (minimum-limit-value limits :max-agenda-entries))))

(defun agenda-stuck-compile-subtree-regexp (pattern limits)
  (when pattern
    (check-limit limits :max-search-regex-length (length pattern))
    (query-compile-emacs-regex pattern (make-query-context)
                               limits pattern)))

(defun agenda-stuck-todo-veto-p (heading document todo-veto)
  (let ((keyword (query-todo-keyword heading)))
    (and keyword
         (not (eq (query-todo-state heading document) :done))
         (or (member "*" todo-veto :test #'string=)
             (member keyword todo-veto :test #'string=)))))

(defun agenda-stuck-explicit-tag-veto-p (heading tags-veto)
  (let ((tags (node-property heading :tags)))
    (and tags
         (or (member "*" tags-veto :test #'string=)
             (some (lambda (tag)
                     (member tag tags-veto :test #'string=))
                   tags)))))

(defun agenda-stuck-descendant-veto-map (document todo-veto tags-veto)
  (let ((vetoes (make-hash-table :test #'eq)))
    (labels ((visit (node)
               (let ((descendant-veto-p nil))
                 (dolist (child (node-children node))
                   (when (visit child)
                     (setf descendant-veto-p t)))
                 (let* ((heading-p (eq (node-type node) :headline))
                        (own-veto-p
                          (and heading-p
                               (or (agenda-stuck-todo-veto-p
                                    node document todo-veto)
                                   (agenda-stuck-explicit-tag-veto-p
                                    node tags-veto))))
                        (veto-p (or own-veto-p descendant-veto-p)))
                   (when heading-p
                     (setf (gethash node vetoes) veto-p))
                   veto-p))))
      (let ((root (document-root document)))
        (when root (visit root))))
    vetoes))

(defun agenda-stuck-subtree-regexp-veto-p (heading regexp work-state)
  (when regexp
    (let* ((text (or (node-text heading) ""))
           (actual (+ (agenda-search-work-state-used work-state)
                      (length text)))
           (limits (agenda-search-work-state-limits work-state)))
      (check-limit limits :max-search-work actual)
      (setf (agenda-search-work-state-used work-state) actual)
      (cl-regex-kit:is-match-p (query-regex-compiled regexp) text))))

(defun agenda-stuck-match-entry (document heading indices)
  (multiple-value-bind (line end-line) (node-line-range heading)
    (make-agenda-match-entry
     :document document :node heading
     :heading-path (node-heading-path-with-indices heading indices)
     :line line :end-line end-line)))

(defun agenda-stuck-projects (documents stuck-projects
                              &key time-context sorting-strategy
                                user-defined-sorter)
  "Return project headings whose subtrees have no configured veto match."
  (let* ((documents (agenda-normalize-documents documents))
         (time-context (agenda-require-time-context time-context))
         (limits (agenda-stuck-projects-operation-limits documents)))
    (multiple-value-bind (matcher todo-veto tags-veto subtree-pattern)
        (agenda-stuck-projects-options stuck-projects)
      (let* ((regexp
               (agenda-stuck-compile-subtree-regexp subtree-pattern limits))
             (reference-time (agenda-query-reference-time time-context))
             (seen (make-hash-table :test #'eq))
             (count 0)
             (entries nil))
        (dolist (document documents)
          (multiple-value-bind (headings timestamps indices)
              (agenda-document-heading-data document :collect-headlines-p t)
            (declare (ignore headings timestamps))
            (let ((descendant-vetoes
                    (agenda-stuck-descendant-veto-map
                     document todo-veto tags-veto))
                  (work-state (%make-agenda-search-work-state
                               (document-limits document))))
              (dolist (heading
                       (query-match-at document matcher reference-time))
                (unless (or (gethash heading seen)
                            (gethash heading descendant-vetoes)
                            (agenda-stuck-subtree-regexp-veto-p
                             heading regexp work-state))
                  (setf (gethash heading seen) t)
                  (incf count)
                  (check-limit limits :max-agenda-entries count)
                  (push (agenda-stuck-match-entry document heading indices)
                        entries))))))
        (agenda-sort-view-entries (nreverse entries) sorting-strategy
                                  :time-context time-context
                                  :user-defined-sorter user-defined-sorter)))))

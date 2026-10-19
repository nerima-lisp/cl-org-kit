;;;; src/export-types.lisp
(in-package #:org-kit)

(defun export-proper-list-p (value)
  (handler-case
      (or (null value) (not (null (list-length value))))
    (type-error () nil)))

(defstruct (export-context (:constructor %make-export-context)
                           (:conc-name %export-context-)
                           (:copier nil)
                           (:predicate %export-context-p))
  "Caller-provided metadata used while preparing an Org export."
  (title nil :read-only t)
  (author nil :read-only t)
  (date nil :read-only t)
  (language nil :read-only t)
  (time-context nil :read-only t)
  (extensions nil :read-only t))

(defun export-copy-alist (alist)
  (mapcar (lambda (entry) (cons (car entry) (cdr entry))) alist))

(defun export-copy-snapshot-value (value)
  (let ((copies (make-hash-table :test #'eq)))
    (labels ((copy-value (object)
               (cond
                 ((stringp object) (copy-seq object))
                 ((consp object)
                  (multiple-value-bind (copy present-p) (gethash object copies)
                    (if present-p
                        copy
                        (let ((cell (cons nil nil)))
                          (setf (gethash object copies) cell
                                (car cell) (copy-value (car object))
                                (cdr cell) (copy-value (cdr object)))
                          cell))))
                 ((vectorp object)
                  (multiple-value-bind (copy present-p) (gethash object copies)
                    (if present-p
                        copy
                        (let ((copy (copy-seq object)))
                          (setf (gethash object copies) copy)
                          (loop for index below (length object)
                                do (setf (aref copy index)
                                          (copy-value (aref object index))))
                          copy))))
                 (t object))))
      (copy-value value))))

(defun make-export-context (&key title author date language time-context extensions)
  "Return immutable overrides; NIL means unspecified, while an empty string is explicit."
  (dolist (value (list title author date language))
    (unless (or (null value) (stringp value))
      (error 'type-error :datum value :expected-type '(or null string))))
  (unless (or (null time-context) (org-time-context-p time-context))
    (error 'type-error :datum time-context :expected-type '(or null org-time-context)))
  (unless (and (export-proper-list-p extensions)
               (every (lambda (entry)
                        (and (consp entry) (keywordp (car entry))))
                      extensions))
    (error 'type-error :datum extensions :expected-type 'list))
  (%make-export-context :title (and title (copy-seq title))
                        :author (and author (copy-seq author))
                        :date (and date (copy-seq date))
                        :language (and language (copy-seq language))
                        :time-context time-context
                        :extensions (export-copy-alist extensions)))

(defun export-context-p (object)
  "Return true when OBJECT is an EXPORT-CONTEXT."
  (%export-context-p object))

(defun export-context-title (context)
  "Return CONTEXT's caller-provided title, or NIL."
  (let ((value (%export-context-title context)))
    (and value (copy-seq value))))

(defun export-context-author (context)
  "Return CONTEXT's caller-provided author, or NIL."
  (let ((value (%export-context-author context)))
    (and value (copy-seq value))))

(defun export-context-date (context)
  "Return CONTEXT's caller-provided date, or NIL."
  (let ((value (%export-context-date context)))
    (and value (copy-seq value))))

(defun export-context-language (context)
  "Return CONTEXT's caller-provided language, or NIL."
  (let ((value (%export-context-language context)))
    (and value (copy-seq value))))

(defun export-context-time-context (context)
  "Return CONTEXT's explicit time context, or NIL."
  (%export-context-time-context context))

(defun export-context-extensions (context)
  "Return a copy of CONTEXT's extension association list."
  (export-copy-alist (%export-context-extensions context)))

(defstruct (export-view (:constructor %make-export-view)
                        (:conc-name %export-view-)
                        (:copier nil)
                        (:predicate %export-view-p))
  (node nil :read-only t)
  (children nil :read-only t)
  payload
  (source-document nil :read-only t))

(defun make-export-view (node &key children source-document)
  "Return a read-only view over NODE and its child views."
  (unless (node-p node)
    (error 'type-error :datum node :expected-type 'node))
  (unless (and (export-proper-list-p children) (every #'%export-view-p children))
    (error 'type-error :datum children :expected-type 'list))
  (unless (or (null source-document) (document-p source-document))
    (error 'type-error :datum source-document :expected-type '(or null document)))
  (%make-export-view :node node :children (copy-list children)
                     :source-document source-document))

(defun export-view-p (object)
  "Return true when OBJECT is an EXPORT-VIEW."
  (%export-view-p object))

(defun export-view-node (view)
  "Return VIEW's source node."
  (unless (%export-view-p view)
    (error 'type-error :datum view :expected-type 'export-view))
  (%export-view-node view))

(defun export-view-children (view)
  "Return a copy of VIEW's child list."
  (unless (%export-view-p view)
    (error 'type-error :datum view :expected-type 'export-view))
  (copy-list (%export-view-children view)))

(defun export-view-source-document (view)
  "Return VIEW's source document, or NIL."
  (unless (%export-view-p view)
    (error 'type-error :datum view :expected-type 'export-view))
  (%export-view-source-document view))

(defun export-view-payload (view)
  (%export-view-payload view))

(defstruct (export-preparation (:constructor %make-export-preparation)
                               (:conc-name export-preparation-)
                               (:copier nil))
  document
  backend
  metadata
  options
  root-views
  toc
  footnotes
  allow-raw-html
  allow-raw-latex)

(defvar *export-current-export-phase* :input-validation)

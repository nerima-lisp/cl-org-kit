;;;; src/export-types.lisp
(in-package #:org-kit)

(defstruct (export-context (:constructor %make-export-context)
                           (:conc-name %export-context-)
                           (:copier nil)
                           (:predicate %export-context-p))
  "Caller-provided metadata used while preparing an Org export."
  (title nil :read-only t)
  (author nil :read-only t)
  (date nil :read-only t)
  (language nil :read-only t)
  (time-context nil :read-only t))

(defun make-export-context (&key title author date language time-context)
  "Return an immutable EXPORT-CONTEXT containing caller metadata overrides."
  (dolist (value (list title author date language))
    (unless (or (null value) (stringp value))
      (error 'type-error :datum value :expected-type '(or null string))))
  (unless (or (null time-context) (org-time-context-p time-context))
    (error 'type-error :datum time-context :expected-type '(or null org-time-context)))
  (%make-export-context :title title :author author :date date
                        :language language :time-context time-context))

(defun export-context-p (object)
  "Return true when OBJECT is an EXPORT-CONTEXT."
  (%export-context-p object))

(defun export-context-title (context)
  "Return CONTEXT's caller-provided title, or NIL."
  (%export-context-title context))

(defun export-context-author (context)
  "Return CONTEXT's caller-provided author, or NIL."
  (%export-context-author context))

(defun export-context-date (context)
  "Return CONTEXT's caller-provided date, or NIL."
  (%export-context-date context))

(defun export-context-language (context)
  "Return CONTEXT's caller-provided language, or NIL."
  (%export-context-language context))

(defun export-context-time-context (context)
  "Return CONTEXT's explicit time context, or NIL."
  (%export-context-time-context context))

(defstruct (export-view (:constructor %make-export-view)
                        (:conc-name export-view-)
                        (:copier nil))
  node
  (children nil)
  payload
  source-document)

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
  allow-raw-html)

(defstruct (export-output-state (:constructor %make-export-output-state)
                                (:conc-name export-output-state-)
                                (:copier nil))
  limits
  backend
  output-type
  stream
  octet-output-p
  buffer
  (character-count 0)
  (octet-count 0))

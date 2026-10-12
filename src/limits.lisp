;;;; src/limits.lisp
(in-package #:org-kit)

(defstruct (limits (:constructor %make-limits) (:conc-name %limits-) (:copier nil)
                   (:predicate %limits-p))
  "Resource ceilings for parsing and writing. Build one with MAKE-LIMITS."
  (max-input-length 0 :type (integer 0) :read-only t)
  (max-heading-depth 0 :type (integer 0) :read-only t)
  (max-element-depth 0 :type (integer 0) :read-only t)
  (max-object-depth 0 :type (integer 0) :read-only t)
  (max-node-count 0 :type (integer 0) :read-only t)
  (max-output-length 0 :type (integer 0) :read-only t))

(defun make-limits (&key (max-input-length (* 64 1024 1024))
                         (max-heading-depth 256)
                         (max-element-depth 512)
                         (max-object-depth 128)
                         (max-node-count (* 4 1024 1024))
                         (max-output-length (* 64 1024 1024)))
  "Return a LIMITS object. Every value is a non-negative integer.
MAX-INPUT-LENGTH bounds input octets before decoding and characters after;
MAX-HEADING-DEPTH bounds headline level; MAX-ELEMENT-DEPTH and MAX-OBJECT-DEPTH
bound element and object nesting; MAX-NODE-COUNT bounds the nodes in one
document; MAX-OUTPUT-LENGTH bounds written characters. Exceeding one signals
ORG-RESOURCE-LIMIT-ERROR."
  (%make-limits :max-input-length max-input-length
                :max-heading-depth max-heading-depth
                :max-element-depth max-element-depth
                :max-object-depth max-object-depth
                :max-node-count max-node-count
                :max-output-length max-output-length))

(defun limits-p (object)
  "Return true when OBJECT is a LIMITS object."
  (%limits-p object))

(defun limit-value (limits name)
  "Return the value of the limit NAME, one of the MAKE-LIMITS keywords, in LIMITS."
  (ecase name
    (:max-input-length (%limits-max-input-length limits))
    (:max-heading-depth (%limits-max-heading-depth limits))
    (:max-element-depth (%limits-max-element-depth limits))
    (:max-object-depth (%limits-max-object-depth limits))
    (:max-node-count (%limits-max-node-count limits))
    (:max-output-length (%limits-max-output-length limits))))

(defun check-limit (limits name actual)
  "Signal ORG-RESOURCE-LIMIT-ERROR when ACTUAL exceeds limit NAME in LIMITS;
otherwise return ACTUAL."
  (let ((limit (limit-value limits name)))
    (when (> actual limit)
      (error 'org-resource-limit-error
             :message "Resource limit exceeded"
             :limit-name name :limit limit :actual actual))
    actual))

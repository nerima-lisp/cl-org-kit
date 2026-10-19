;;;; src/conditions.lisp
(in-package #:org-kit)

(defparameter *condition-text-limit* 80
  "Longest input excerpt, in characters, a condition stores before truncating.")

(defun bounded-text (string)
  "Escape control characters, Unicode format controls, and line separators in
STRING, then cap its input length in characters for condition reports."
  (when string
    (with-output-to-string (out)
      (loop for char across string
            for index from 0
            when (>= index *condition-text-limit*)
              do (write-string "..." out)
                 (loop-finish)
            do (let ((code (char-code char)))
                 (cond ((char= char #\\) (write-string "\\\\" out))
                       ((char= char #\Newline) (write-string "\\n" out))
                       ((char= char #\Return) (write-string "\\r" out))
                       ((char= char #\Tab) (write-string "\\t" out))
                       ((or (< code 32)
                            (<= #x7F code #x9F)
                            (= code #x2028)
                            (= code #x2029))
                        (format out "\\x~2,'0X" code))
                       ((eq (sb-unicode:general-category char) :cf)
                        (format out "\\u~4,'0X" code))
                       (t (write-char char out))))))))

(define-condition org-kit-error (error)
  ((message :initarg :message :initform "cl-org-kit error"))
  (:report (lambda (condition stream)
             (write-string (org-kit-error-message condition) stream)))
  (:documentation "Root of every condition cl-org-kit signals."))

(define-condition org-parse-error (org-kit-error)
  ((offset :initarg :offset :initform nil)
   (line :initarg :line :initform nil)
   (column :initarg :column :initform nil)
   (context :initarg :context :initform nil)
   (source-name :initarg :source-name :initform nil))
  (:report (lambda (condition stream)
             (format stream "~@[~A: ~]~A~@[ at line ~D~]~@[, column ~D~]~@[ (offset ~D)~]~@[ near \"~A\"~]"
                     (org-parse-error-source-name condition)
                     (org-kit-error-message condition)
                     (org-parse-error-line condition)
                     (org-parse-error-column condition)
                     (org-parse-error-offset condition)
                     (org-parse-error-context condition))))
  (:documentation "Signalled when input cannot be decoded or parsed. OFFSET is the
0-based position (an octet index for decoding failures, a character index
otherwise), LINE and COLUMN are 1-based, and CONTEXT is a bounded, escaped
excerpt of the input."))

(define-condition org-query-error (org-kit-error)
  ((query :initarg :query :initform nil)
   (position :initarg :position :initform nil))
  (:report (lambda (condition stream)
             (format stream "~A~@[ at position ~D~]~@[ in query \"~A\"~]"
                     (org-kit-error-message condition)
                     (org-query-error-position condition)
                     (org-query-error-query condition))))
  (:documentation "Signalled for a malformed query. QUERY is a bounded, escaped copy
of the query text and POSITION the 0-based character index of the fault."))

(define-condition org-edit-error (org-kit-error)
  ((target :initarg :target :initform nil)
   (reason :initarg :reason :initform nil))
  (:report (lambda (condition stream)
             (format stream "~A~@[ (~(~A~))~]~@[ for target ~A~]"
                     (org-kit-error-message condition)
                     (org-edit-error-reason condition)
                     (org-edit-error-target condition))))
  (:documentation "Signalled when an edit cannot be applied or its result fails
validation. TARGET is a bounded description of the addressed node and REASON a
keyword classifying the failure. The input is never modified."))

(define-condition org-table-unsupported-syntax (org-kit-error)
  ((formula :initarg :formula :initform nil)
   (position :initarg :position :initform nil)
   (feature :initarg :feature :initform nil))
  (:report (lambda (condition stream)
             (format stream "~A~@[ at position ~D~]~@[ in formula \"~A\"~]"
                     (org-kit-error-message condition)
                     (org-table-unsupported-syntax-position condition)
                     (org-table-unsupported-syntax-formula condition))))
  (:documentation "Signalled when a table formula uses unsupported syntax.
FORMULA is a bounded, escaped excerpt, POSITION the 0-based character offset,
and FEATURE the syntax class that was refused."))

(define-condition org-table-error (org-kit-error)
  ((target :initarg :target :initform nil)
   (reason :initarg :reason :initform nil))
  (:report (lambda (condition stream)
             (format stream "~A~@[ (~(~A~))~]~@[ for target ~A~]"
                     (org-kit-error-message condition)
                     (org-table-error-reason condition)
                     (org-table-error-target condition))))
  (:documentation "Signalled when a table edit fails or a table formula cannot be evaluated.
TARGET describes the addressed table and REASON classifies the table-specific failure."))

(define-condition org-resource-limit-error (org-kit-error)
  ((limit-name :initarg :limit-name)
   (limit :initarg :limit)
   (actual :initarg :actual))
  (:report (lambda (condition stream)
             (format stream "~A: ~(~A~) is ~D, above the limit of ~D"
                     (org-kit-error-message condition)
                     (org-resource-limit-error-limit-name condition)
                     (org-resource-limit-error-actual condition)
                     (org-resource-limit-error-limit condition))))
  (:documentation "Signalled when input or output exceeds a configured limit.
LIMIT-NAME is the MAKE-LIMITS keyword, LIMIT its value and ACTUAL the measured
amount that exceeded it."))

(define-condition org-callback-error (org-kit-error)
  ((kind :initarg :kind)
   (name :initarg :name)
   (reason :initarg :reason))
  (:report (lambda (condition stream)
             (let ((kind (org-callback-error-kind condition))
                   (name (org-callback-error-name condition)))
               (flet ((callback-text (value)
                        (bounded-text (if (stringp value)
                                          value
                                          (format nil "~A" value)))))
                 (format stream "~A (~(~A~)/~A, ~(~A~))"
                         (org-kit-error-message condition)
                         (callback-text kind)
                         (callback-text name)
                         (org-callback-error-reason condition))))))
  (:documentation "Signalled when a callback lookup or execution fails.
KIND and NAME identify the callback, and REASON classifies the failure."))

(define-condition org-elisp-evaluator-error (org-callback-error)
  ()
  (:documentation "Signalled when Emacs Lisp evaluation is requested without a
registered Emacs Lisp evaluator."))

(define-condition org-callback-timeout-error (org-callback-error)
  ()
  (:default-initargs :reason :timeout)
  (:documentation "Signalled by a caller-owned callback when its work exceeds its time budget.
KIND and NAME identify the callback, and REASON defaults to :TIMEOUT."))

(define-condition org-write-plan-error (org-kit-error)
  ((path :initarg :path :initform nil)
   (reason :initarg :reason :initform nil)
   (cause :initarg :cause :initform nil))
  (:report (lambda (condition stream)
             (format stream "~A (~(~A~))~@[ for ~A~]"
                     (org-kit-error-message condition)
                     (org-write-plan-error-reason condition)
                     (org-write-plan-error-path condition))))
  (:documentation "Signalled when a write plan is invalid or cannot be applied.
PATH is a bounded path excerpt, REASON classifies the failure, and CAUSE is the
underlying condition for an I/O failure when available."))

;; Readers are plain functions rather than :READER options so each carries a
;; docstring and is measured by the coverage gate.
(defun org-kit-error-message (condition)
  "Return the bounded description of the failure CONDITION reports."
  (slot-value condition 'message))

(defun org-parse-error-offset (condition)
  "Return the 0-based position of the fault: an octet index when decoding failed,
otherwise a character index into the document text. NIL when unknown."
  (slot-value condition 'offset))

(defun org-parse-error-line (condition)
  "Return the 1-based line of the fault, or NIL when unknown."
  (slot-value condition 'line))

(defun org-parse-error-column (condition)
  "Return the 1-based column of the fault, or NIL when unknown."
  (slot-value condition 'column))

(defun org-parse-error-context (condition)
  "Return the bounded, escaped excerpt of the input around the fault, or NIL."
  (slot-value condition 'context))

(defun org-parse-error-source-name (condition)
  "Return the :SOURCE-NAME given for the input, or NIL."
  (slot-value condition 'source-name))

(defun org-query-error-query (condition)
  "Return the bounded, escaped copy of the query text, or NIL."
  (slot-value condition 'query))

(defun org-query-error-position (condition)
  "Return the 0-based character index of the fault in the query, or NIL."
  (slot-value condition 'position))

(defun org-edit-error-target (condition)
  "Return the bounded description of the node or path the edit addressed, or NIL."
  (slot-value condition 'target))

(defun org-edit-error-reason (condition)
  "Return the keyword classifying why the edit was refused, or NIL."
  (slot-value condition 'reason))

(defun org-table-unsupported-syntax-formula (condition)
  "Return the bounded, escaped formula excerpt that was refused, or NIL."
  (slot-value condition 'formula))

(defun org-table-unsupported-syntax-position (condition)
  "Return the 0-based character offset of the unsupported formula syntax, or NIL."
  (slot-value condition 'position))

(defun org-table-unsupported-syntax-feature (condition)
  "Return the keyword identifying the unsupported formula syntax, or NIL."
  (slot-value condition 'feature))

(defun org-table-error-target (condition)
  "Return the bounded description of the table addressed by this error, or NIL."
  (slot-value condition 'target))

(defun org-table-error-reason (condition)
  "Return the keyword classifying why the table operation was refused, or NIL."
  (slot-value condition 'reason))

(defun org-resource-limit-error-limit-name (condition)
  "Return the MAKE-LIMITS keyword of the exceeded limit."
  (slot-value condition 'limit-name))

(defun org-resource-limit-error-limit (condition)
  "Return the configured value of the exceeded limit."
  (slot-value condition 'limit))

(defun org-resource-limit-error-actual (condition)
  "Return the measured amount that exceeded the limit."
  (slot-value condition 'actual))

(defun org-callback-error-kind (condition)
  "Return the kind of callback requested by CONDITION."
  (slot-value condition 'kind))

(defun org-callback-error-name (condition)
  "Return the name of the callback requested by CONDITION."
  (slot-value condition 'name))

(defun org-callback-error-reason (condition)
  "Return the keyword that classifies CONDITION's callback failure, including
:TIMEOUT."
  (slot-value condition 'reason))

(defun org-write-plan-error-path (condition)
  "Return the bounded path excerpt associated with CONDITION, or NIL."
  (slot-value condition 'path))

(defun org-write-plan-error-reason (condition)
  "Return the keyword classifying the write-plan failure."
  (slot-value condition 'reason))

(defun org-write-plan-error-cause (condition)
  "Return the underlying condition for a write-plan I/O failure, or NIL."
  (slot-value condition 'cause))

(define-condition org-time-error (org-kit-error)
  ((reason :initarg :reason :initform nil)
   (context :initarg :context :initform nil))
  (:report (lambda (condition stream)
             (format stream "~A~@[ (~(~A~))~]~@[ near \"~A\"~]"
                     (org-kit-error-message condition)
                     (org-time-error-reason condition)
                     (org-time-error-context condition))))
  (:documentation "Signalled when an Org date, time, zone, or range is invalid.
REASON classifies the failure and CONTEXT is a bounded, escaped excerpt."))

(define-condition org-clock-error (org-kit-error)
  ((reason :initarg :reason :initform nil)
   (context :initarg :context :initform nil))
  (:report (lambda (condition stream)
             (format stream "~A~@[ (~(~A~))~]~@[ near \"~A\"~]"
                     (org-kit-error-message condition)
                     (org-clock-error-reason condition)
                     (org-clock-error-context condition))))
  (:documentation "Signalled when a CLOCK entry or clock summary request is invalid.
REASON classifies the failure and CONTEXT is a bounded, escaped excerpt."))

(defun org-time-error-reason (condition)
  "Return the keyword classifying the time failure CONDITION reports."
  (slot-value condition 'reason))

(defun org-time-error-context (condition)
  "Return the bounded, escaped time or zone excerpt, or NIL."
  (slot-value condition 'context))

(defun org-clock-error-reason (condition)
  "Return the keyword classifying why the clock operation failed."
  (slot-value condition 'reason))

(defun org-clock-error-context (condition)
  "Return the bounded, escaped CLOCK line or option excerpt, or NIL."
  (slot-value condition 'context))

(define-condition org-agenda-error (org-kit-error)
  ((reason :initarg :reason :initform nil)
   (context :initarg :context :initform nil))
  (:report (lambda (condition stream)
             (format stream "~A~@[ (~(~A~))~]~@[ near \"~A\"~]"
                     (org-kit-error-message condition)
                     (org-agenda-error-reason condition)
                     (org-agenda-error-context condition))))
  (:documentation "Signalled when an agenda option or entry cannot be handled.
REASON classifies the failure and CONTEXT is a bounded, escaped excerpt."))

(defun org-agenda-error-reason (condition)
  "Return the keyword classifying the agenda failure CONDITION reports."
  (slot-value condition 'reason))

(defun org-agenda-error-context (condition)
  "Return the bounded, escaped agenda option or entry excerpt, or NIL."
  (slot-value condition 'context))

(define-condition org-babel-error (org-kit-error)
  ((reason :initarg :reason :initform nil)
   (context :initarg :context :initform nil))
  (:report (lambda (condition stream)
             (format stream "~A~@[ (~(~A~))~]~@[ near \"~A\"~]"
                     (org-kit-error-message condition)
                     (org-babel-error-reason condition)
                     (org-babel-error-context condition))))
  (:documentation "Signalled when Babel parameters or references cannot be resolved.
REASON classifies the failure, and CONTEXT is a bounded, escaped excerpt."))

(defun org-babel-error-reason (condition)
  "Return the keyword classifying the Babel resolution failure."
  (slot-value condition 'reason))

(defun org-babel-error-context (condition)
  "Return the bounded, escaped excerpt associated with the Babel failure, or NIL."
  (slot-value condition 'context))

(define-condition org-crypt-error (org-kit-error)
  ((reason :initarg :reason :initform nil))
  (:report (lambda (condition stream)
             (format stream "~A~@[ (~(~A~))~]"
                     (org-kit-error-message condition)
                     (org-crypt-error-reason condition))))
  (:documentation "Signalled when an Org crypt operation or callback fails.
REASON classifies the failure. Callback conditions are not retained."))

(defun org-crypt-error-reason (condition)
  "Return the keyword classifying the Org crypt failure CONDITION reports."
  (slot-value condition 'reason))

(defun org-crypt-error-cause (condition)
  "Return NIL; callback conditions are not retained by Org crypt errors."
  (declare (ignore condition))
  nil)

(define-condition org-export-error (org-kit-error)
  ((backend :initarg :backend :initform nil)
   (reason :initarg :reason :initform nil)
   (source :initarg :source :initform nil))
  (:report (lambda (condition stream)
             (format stream "~A~@[ (~(~A~))~]~@[ for ~A~]~@[ near \"~A\"~]"
                     (org-kit-error-message condition)
                     (org-export-error-reason condition)
                     (org-export-error-backend condition)
                     (org-export-error-source condition))))
  (:documentation "Signalled when an Org export cannot be prepared or written.
BACKEND identifies the requested serializer, REASON classifies the failure, and
SOURCE is a bounded, escaped excerpt when one is available."))

(defun org-export-error-backend (condition)
  "Return the backend associated with the export failure, or NIL."
  (slot-value condition 'backend))

(defun org-export-error-reason (condition)
  "Return the keyword classifying the export failure, or NIL."
  (slot-value condition 'reason))

(defun org-export-error-source (condition)
  "Return the bounded, escaped source excerpt associated with the export failure."
  (slot-value condition 'source))

(define-condition org-export-unsupported-syntax (org-kit-error)
  ((backend :initarg :backend :initform nil)
   (source :initarg :source :initform nil)
   (position :initarg :position :initform 0)
   (feature :initarg :feature :initform nil))
  (:report (lambda (condition stream)
             (format stream "~A~@[ (~(~A~))~]~@[ at position ~D~]~@[ near \"~A\"~]"
                     (org-kit-error-message condition)
                     (org-export-unsupported-syntax-feature condition)
                     (org-export-unsupported-syntax-position condition)
                     (org-export-unsupported-syntax-source condition))))
  (:documentation "Signalled when an export construct or option is unsupported.
SOURCE is a bounded, escaped excerpt, POSITION is its 0-based character index,
and FEATURE identifies the unsupported syntax."))

(defun org-export-unsupported-syntax-backend (condition)
  "Return the backend associated with the unsupported syntax, or NIL."
  (slot-value condition 'backend))

(defun org-export-unsupported-syntax-source (condition)
  "Return the bounded, escaped source excerpt that contains unsupported syntax."
  (slot-value condition 'source))

(defun org-export-unsupported-syntax-position (condition)
  "Return the 0-based character position of the unsupported syntax."
  (slot-value condition 'position))

(defun org-export-unsupported-syntax-feature (condition)
  "Return the feature name classifying unsupported export syntax."
  (slot-value condition 'feature))

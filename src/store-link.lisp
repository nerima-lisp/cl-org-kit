;;;; src/store-link.lisp
(in-package #:org-kit)

(define-condition org-store-link-error (org-kit-error)
  ((reason :initarg :reason :initform nil)
   (context :initarg :context :initform nil))
  (:report (lambda (condition stream)
             (format stream "~A~@[ (~(~A~))~]~@[ near \"~A\"~]"
                     (org-kit-error-message condition)
                     (org-store-link-error-reason condition)
                     (org-store-link-error-context condition))))
  (:documentation "Signalled when a heading cannot be converted to the requested
Org link style. REASON classifies the failure and CONTEXT is a bounded excerpt."))

(defun org-store-link-error-reason (condition)
  "Return the keyword classifying why the store-link operation failed."
  (slot-value condition 'reason))

(defun org-store-link-error-context (condition)
  "Return the bounded context associated with a store-link failure, or NIL."
  (slot-value condition 'context))

(defun store-link-error (reason context message)
  (error 'org-store-link-error
         :message message
         :reason reason
         :context (bounded-text (if (stringp context)
                                    context
                                    (princ-to-string context)))))

(defun store-link-context (heading)
  (if (node-p heading)
      (or (node-property heading :raw-value)
          (string-downcase (symbol-name (node-type heading))))
      (princ-to-string heading)))

(defun store-link-property-path (heading name)
  (let ((entries (effective-heading-property-entries heading name)))
    (cond
      ((null entries)
       (store-link-error :missing-property name
                         "The requested local heading property is missing"))
      ((cdr entries)
       (store-link-error :ambiguous-property name
                         "The requested local heading property occurs more than once"))
      (t
       (let ((value (node-property (first entries) :value)))
         (unless (and (stringp value)
                      (plusp (length (string-trim '(#\Space #\Tab) value)))
                      (notany (lambda (character)
                                (let ((code (char-code character)))
                                  (or (<= code #x1f)
                                      (<= #x7f code #x9f))))
                              value))
           (store-link-error :invalid-property-value name
                             "The requested local heading property is invalid"))
         value)))))

(defun store-link-escape-path (path)
  (with-output-to-string (stream)
    (loop for character across path
          do (when (find character "[]\\")
               (write-char #\\ stream))
             (write-char character stream))))

(defun store-link (heading &key (style :id))
  "Return a bracket link for HEADING using :ID, :CUSTOM-ID, or :HEADING style.
Property styles require one valid local ID or CUSTOM_ID value."
  (unless (member style '(:id :custom-id :heading) :test #'eq)
    (store-link-error :invalid-style style
                      "Unknown store-link style"))
  (unless (and (node-p heading)
               (member (node-type heading) '(:headline :inlinetask) :test #'eq)
               (node-document heading))
    (store-link-error :invalid-heading (store-link-context heading)
                      "Target is not a heading in a parsed document"))
  (let ((path
          (case style
            (:id
             (concatenate 'string "id:"
                          (store-link-escape-path
                           (store-link-property-path heading "ID"))))
            (:custom-id
             (concatenate 'string "#"
                          (store-link-escape-path
                           (store-link-property-path heading "CUSTOM_ID"))))
            (:heading
             (let ((title (node-property heading :raw-value)))
               (unless (and (stringp title) (plusp (length title)))
                 (store-link-error :missing-heading-title
                                   (store-link-context heading)
                                   "Target heading has no title"))
               (concatenate 'string "*" (store-link-escape-path title)))))))
    (concatenate 'string "[[" path "]]")))

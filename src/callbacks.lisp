;;;; src/callbacks.lisp
(in-package #:org-kit)

(defstruct (callback-registry
            (:constructor %make-callback-registry)
            (:conc-name %callback-registry-)
            (:copier nil)
            (:predicate %callback-registry-p))
  "A caller-owned registry for explicit, named callbacks."
  (callbacks (make-hash-table :test 'equal) :read-only t))

(defun make-callback-registry ()
  "Return an empty callback registry with no callbacks pre-registered."
  (%make-callback-registry))

(defun callback-registry-p (object)
  "Return true when OBJECT is a callback registry."
  (%callback-registry-p object))

(defun callback-key-part (value case)
  (let ((text (typecase value
                (string value)
                (symbol (symbol-name value))
                (t (error 'type-error :datum value
                          :expected-type '(or string symbol))))))
    (if (eq case :upper) (string-upcase text) (string-downcase text))))

(defun callback-key (kind name)
  (cons (callback-key-part kind :upper)
        (callback-key-part name :lower)))

(defun register-callback (registry kind name function)
  "Register FUNCTION under KIND and NAME in the caller-owned REGISTRY.
KIND is case-folded to uppercase and NAME to lowercase; a later registration
for the same key replaces the earlier callback."
  (unless (callback-registry-p registry)
    (error 'type-error :datum registry :expected-type 'callback-registry))
  (unless (functionp function)
    (error 'type-error :datum function :expected-type 'function))
  (setf (gethash (callback-key kind name)
                 (%callback-registry-callbacks registry))
        function))

(defun %lookup-callback (registry kind name condition-class message)
  (unless (callback-registry-p registry)
    (error 'type-error :datum registry :expected-type 'callback-registry))
  (let* ((key (callback-key kind name))
         (callback (gethash key (%callback-registry-callbacks registry))))
    (unless callback
      (error condition-class
             :message message
             :kind (car key) :name (cdr key) :reason :unregistered))
    callback))

(defun invoke-callback (registry kind name &rest arguments)
  "Invoke the callback registered under KIND and NAME with ARGUMENTS.
Missing callbacks signal ORG-CALLBACK-ERROR. Callback conditions propagate
unchanged."
  (apply (%lookup-callback registry kind name 'org-callback-error
                           "No callback is registered")
         arguments))

(defun evaluate-elisp (registry expression)
  "Pass EXPRESSION unchanged to the registered Emacs Lisp evaluator.
The callback is registered as KIND :EVALUATOR and NAME :EMACS-LISP. This
library does not read or evaluate EXPRESSION."
  (funcall (%lookup-callback registry :evaluator :emacs-lisp
                             'org-elisp-evaluator-error
                             "No Emacs Lisp evaluator is registered")
           expression))

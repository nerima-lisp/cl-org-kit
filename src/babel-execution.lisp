(in-package #:org-kit)
(defvar *babel-execution-document* nil)

(defstruct (babel-execution-result
            (:constructor %make-babel-execution-result
                (&key status value hash edit-result)))
  "One source-block execution outcome and its optional result edit."
  status value hash edit-result)

(defun register-babel-executor (registry language executor)
  "Register EXECUTOR for LANGUAGE under the :BABEL-EXECUTOR callback kind."
  (register-callback registry :babel-executor language executor))

(defun make-babel-shell-reference-executor (runner)
  "Check that RUNNER is a function and return it unchanged.

This helper never starts a process."
  (unless (functionp runner)
    (error 'type-error :datum runner :expected-type 'function))
  runner)

(defun babel-execution-package (designator)
  (let ((package (typecase designator
                   (package designator)
                   ((or string symbol) (find-package designator))
                   (t (error 'type-error :datum designator
                             :expected-type '(or package string symbol))))))
    (or package
        (error 'package-error :package designator))))

(defun babel-execution-output-requested-p (parameters)
  (let ((position (position "RESULTS" parameters :key #'car :test #'string=)))
    (and position
         (member "output"
                 (babel-split-header-list (cdr (nth position parameters))
                                          :whitespace)
                 :test #'string-equal))))

(defun babel-execution-common-lisp-forms (body package)
  (let ((*package* package)
        (*read-eval* nil)
        (eof (gensym "EOF")))
    (with-input-from-string (input body)
      (loop for form = (read input nil eof)
            until (eq form eof)
            collect form))))

(defun babel-execution-common-lisp-bindings (variables package)
  (mapcar (lambda (variable)
            (list (intern (string-upcase (car variable)) package)
                  (list 'quote (cdr variable))))
          variables))

(defun make-babel-common-lisp-reference-executor (&optional package)
  "Return an opt-in executor for trusted document code.

It reads source with *READ-EVAL* disabled and evaluates forms in PACKAGE.
Variables are interned in the executor package. Register this executor only
for trusted documents because each invocation evaluates document code. When
the RESULTS channel is OUTPUT, captured output follows the document limits."
  (let* ((package
           (babel-execution-package
            (or package
                (or (find-package "ORG-KIT.BABEL-EXECUTOR")
                    (make-package "ORG-KIT.BABEL-EXECUTOR"
                                  :use '("COMMON-LISP")))))))
    (lambda (body parameters variables)
      (let* ((document (or *babel-execution-document* (make-document "")))
             (output (make-instance 'babel-results-bounded-stream
                                    :document document))
             (forms
               (progn
                 (check-limit (document-limits document) :max-input-length
                              (length body))
                 (check-limit
                  (document-limits document) :max-input-length
                  (babel-results-encoded-range-octets
                   body 0 (length body)
                   (or (document-encoding document) :utf-8)))
                 (babel-execution-common-lisp-forms body package)))
             (bindings (babel-execution-common-lisp-bindings variables package))
             (result nil))
        (let ((*package* package)
              (*standard-output* output)
              (*error-output* output)
              (*trace-output* output))
          (setf result
                (eval (append (list 'let bindings) forms))))
        (if (babel-execution-output-requested-p parameters)
            (get-output-stream-string (babel-results-stream-output output))
            result)))))

(defun babel-execution-effective-parameters (document source-block parameters)
  (let ((effective (resolve-babel-parameters document source-block)))
    (dolist (entry parameters effective)
      (unless (and (consp entry)
                   (stringp (car entry))
                   (string= (car entry) (string-upcase (car entry)))
                   (or (null (cdr entry)) (stringp (cdr entry))))
        (error 'type-error :datum entry
               :expected-type '(cons string (or string null))))
      (setf effective
            (babel-merge-header-parameter
             effective (car entry) (cdr entry))))))

(defun babel-execution-parameter (parameters name &optional default)
  (let ((position (position name parameters :key #'car :test #'string=)))
    (if position (cdr (nth position parameters)) default)))

(defun babel-execution-eval-action (parameters context)
  (let ((mode (string-downcase
               (or (babel-execution-parameter parameters "EVAL") "yes"))))
    (cond
      ((member mode '("no" "never") :test #'string=) :skip)
      ((string= mode "no-export") (if (eq context :export) :skip :execute))
      ((string= mode "query") :query)
      ((string= mode "yes") :execute)
      (t (babel-results-error :unsupported-eval-option
                              "Unsupported Babel :eval option" mode)))))

(defun babel-execution-cache-enabled-p (parameters)
  (string-equal (or (babel-execution-parameter parameters "CACHE") "no") "yes"))

(defun babel-execution-hash (function body parameters variables)
  (unless function
    (babel-results-error :missing-hash-function
                         "A Babel hash function is required for caching" nil))
  (unless (functionp function)
    (error 'type-error :datum function :expected-type 'function))
  (let ((hash (funcall function body parameters variables)))
    (babel-results-validate-hash hash)
    (unless (stringp hash)
      (error 'type-error :datum hash :expected-type 'string))
    hash))

(defun babel-execution-confirmed-p (block parameters function)
  (unless function
    (babel-results-error :confirmation-required
                         "Babel evaluation requires confirmation" nil))
  (unless (functionp function)
    (error 'type-error :datum function :expected-type 'function))
  (funcall function block parameters))

(defun babel-execution-cached-p (document result-node hash)
  (when result-node
    (multiple-value-bind (old-hash found-p)
        (babel-results-adjacent-hash document result-node)
      (and found-p (stringp old-hash) (string= hash old-hash)))))

(defun %execute-babel-source-block
    (document source-block registry
     &key result-node parameters result-resolver confirmation-function
       hash-function evaluation-context variable-document
       resolved-variables resolved-variables-p)
  (validate-babel-source-block document source-block)
  (unless (callback-registry-p registry)
    (error 'type-error :datum registry :expected-type 'callback-registry))
  (when result-node
    (unless (node-p result-node)
      (error 'type-error :datum result-node :expected-type 'node))
    (babel-results-anchor-offset (node-document result-node) result-node))
  (let* ((effective-parameters
           (babel-execution-effective-parameters document source-block parameters))
         (evaluation-context (or evaluation-context :execute))
         (action (babel-execution-eval-action effective-parameters
                                              evaluation-context)))
    (when (eq action :skip)
      (return-from %execute-babel-source-block
        (%make-babel-execution-result :status :skipped)))
    (when (and (eq action :query)
               (not (babel-execution-confirmed-p
                     source-block effective-parameters confirmation-function)))
      (return-from %execute-babel-source-block
        (%make-babel-execution-result :status :cancelled)))
    (let* ((expanded-body
             (expand-babel-noweb document source-block
                                 :context :eval
                                 :result-resolver result-resolver
                                 :parameters effective-parameters))
           (variable-document (or variable-document document))
           (resolution (make-babel-resolution-context
                        :document variable-document
                        :result-resolver result-resolver
                        :names (babel-build-name-index variable-document)))
           (variables
             (if resolved-variables-p
                 resolved-variables
                 (babel-resolve-parameters-variables
                  resolution effective-parameters)))
           (cache-enabled (and result-node
                               (babel-execution-cache-enabled-p
                                effective-parameters)))
           (hash (and cache-enabled
                      (babel-execution-hash hash-function expanded-body
                                            effective-parameters variables))))
      (when (and cache-enabled
                 (babel-execution-cached-p (node-document result-node)
                                           result-node hash))
        (return-from %execute-babel-source-block
          (%make-babel-execution-result :status :cached :hash hash)))
      (let* ((language (node-property source-block :language ""))
             (value (let ((*babel-execution-document* document))
                      (invoke-callback registry :babel-executor language
                                       expanded-body effective-parameters variables)))
             (edit-result
               (when result-node
                 (let ((result-document (node-document result-node)))
                   (update-babel-results result-document result-node value
                                         :parameters effective-parameters
                                         :hash hash :language language)))))
        (%make-babel-execution-result :status :executed :value value
                                      :hash hash :edit-result edit-result)))))

(defun execute-babel-source-block
    (document source-block registry
     &key result-resolver confirmation-function hash-function evaluation-context)
  "Execute SRC-BLOCK through its registered executor and return its outcome.

The returned edit result contains a reparsed document with results written
back. DOCUMENT itself remains unchanged."
  (%execute-babel-source-block
   document source-block registry
   :result-node source-block
   :result-resolver result-resolver
   :confirmation-function confirmation-function
   :hash-function hash-function
   :evaluation-context evaluation-context))

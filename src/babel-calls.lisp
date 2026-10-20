;;;; src/babel-calls.lisp

(in-package #:org-kit)

(defun babel-call-site-property-parameters (document call language)
  (let* ((heading (babel-containing-heading call))
         (language-name (format nil "header-args:~A" language))
         (parameters nil))
    (setf parameters
          (babel-merge-header-text
           parameters (babel-global-property-value document "header-args")))
    (setf parameters
          (babel-merge-header-text
           parameters (babel-global-property-value document language-name)))
    (setf parameters
          (babel-merge-header-text
           parameters (babel-effective-property heading "header-args")))
    (babel-merge-header-text
     parameters (babel-effective-property heading language-name))))

(defun babel-call-target (document call-name library-documents)
  (dolist (candidate (cons document library-documents))
    (unless (document-p candidate)
      (error 'type-error :datum candidate :expected-type 'document))
    (let ((node (gethash call-name (babel-build-name-index candidate))))
      (when node
        (unless (eq (node-type node) :src-block)
          (babel-signal-error :unresolved-call-target
                              "Babel call target is not a named source block"
                              call-name))
        (return-from babel-call-target (values candidate node)))))
  (babel-signal-error :unresolved-call-target
                      "Babel call target was not found"
                      call-name))

(defun babel-merge-call-arguments (parameters arguments)
  (loop for assignment in (babel-call-variable-assignments
                           parameters arguments)
        for argument in (babel-split-header-list arguments #\,)
        do (if (position #\= argument)
               (setf parameters
                     (babel-merge-variable-assignments
                      parameters (list assignment)))
             (let ((parameter
       (find-if
        (lambda (entry)
          (let ((name (and (string= (car entry) "VAR")
                           (babel-var-assignment-name (cdr entry)))))
            (and name (string= name (car assignment)))))
        parameters)))
  (unless parameter
    (babel-signal-error
     :invalid-variable-assignment
     "Babel positional argument has no matching variable"
     argument))
  (setf (cdr parameter)
        (format nil "~A=~A"
                (car assignment) (cdr assignment))))))
  parameters)

(defun babel-call-effective-parameters (call-document call target-document
                                        target-block)
  (let* ((language (node-property target-block :language ""))
         (parameters (resolve-babel-parameters target-document target-block)))
    (unless (babel-parameter-position parameters "LEXICAL")
      (setf parameters (append parameters (list (cons "LEXICAL" "no")))))
    (setf parameters
          (babel-merge-header-parameter parameters "EXPORTS" "results"))
    (dolist (entry (babel-call-site-property-parameters
                    call-document call language))
      (setf parameters
            (babel-merge-header-parameter parameters
                                          (car entry) (cdr entry))))
    (setf parameters
          (babel-merge-header-text parameters
                                   (node-property call :inside-header)))
    (setf parameters
          (babel-merge-call-arguments
           parameters (node-property call :arguments)))
    (babel-merge-header-text parameters (node-property call :end-header))))

(defun execute-babel-call (document call-node registry
                           &key library-documents result-resolver
                             confirmation-function hash-function
                             evaluation-context)
  "Execute CALL-NODE through a registered Babel executor.

The call is resolved in DOCUMENT before LIBRARY-DOCUMENTS.  Call-site
parameters and arguments are merged over the target block's parameters."
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document))
  (unless (node-p call-node)
    (error 'type-error :datum call-node :expected-type 'node))
  (unless (member (node-type call-node) '(:babel-call :inline-babel-call)
                  :test #'eq)
    (babel-signal-error :invalid-target
                        "Babel call execution requires a call node"
                        (princ-to-string (node-type call-node))))
  (unless (eq (node-document call-node) document)
    (babel-signal-error :document-mismatch
                        "Babel call belongs to another document"
                        (node-property call-node :call)))
  (unless (or (null library-documents)
              (and (listp library-documents)
                   (integerp (list-length library-documents))))
    (error 'type-error :datum library-documents
           :expected-type '(or null list)))
  (let ((call-name (node-property call-node :call)))
    (unless (and (stringp call-name) (plusp (length call-name)))
      (babel-signal-error :unresolved-call-target
                          "Babel call has no target name"
                          call-name))
    (multiple-value-bind (target-document target-block)
        (babel-call-target document call-name library-documents)
      (let ((parameters (babel-call-effective-parameters
                         document call-node target-document target-block)))
        (%execute-babel-source-block
         target-document target-block registry
         :result-node (and (eq (node-type call-node) :babel-call) call-node)
         :parameters parameters
         :variable-document document
         :result-resolver result-resolver
         :confirmation-function confirmation-function
         :hash-function hash-function
         :evaluation-context evaluation-context)))))

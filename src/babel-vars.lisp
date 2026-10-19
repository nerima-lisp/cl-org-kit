;;;; src/babel-vars.lisp
(in-package #:org-kit)

(defstruct (babel-resolution-context
            (:constructor make-babel-resolution-context))
  document
  result-resolver
  (names (make-hash-table :test #'equal))
  (active-names (make-hash-table :test #'equal))
  (expanded-characters 0))

(defun babel-build-name-index (document)
  (let ((names (make-hash-table :test #'equal)))
    (map-nodes
     (lambda (node)
       (dolist (key '(:name :results))
         (let ((name (node-property node key)))
           (when (and (stringp name) (plusp (length name)))
             (multiple-value-bind (value found-p) (gethash name names)
               (declare (ignore value))
               (unless found-p (setf (gethash name names) node)))))))
     (document-root document))
    names))

(defun babel-signal-error (reason message context)
  (error 'org-babel-error :message message :reason reason
         :context (and context (bounded-text context))))

(defun babel-parse-assignment (assignment)
  (let ((equals (position #\= assignment)))
    (unless equals
      (babel-signal-error :invalid-variable-assignment
                          "Babel variable assignment has no equals sign"
                          assignment))
    (let ((name (string-trim '(#\Space #\Tab)
                             (subseq assignment 0 equals)))
          (value (string-trim '(#\Space #\Tab)
                              (subseq assignment (1+ equals)))))
      (when (zerop (length name))
        (babel-signal-error :invalid-variable-assignment
                            "Babel variable assignment has no name"
                            assignment))
      (cons name value))))

(defun babel-parameter-variables (parameters)
  (loop for (name . value) in parameters
        when (and (string= name "VAR") value)
          append (mapcar #'babel-parse-assignment
                         (babel-split-header-list value #\,))))

(defun babel-ascii-digit-p (character)
  (char<= #\0 character #\9))

(defun babel-numeric-token-p (text)
  (and (plusp (length text))
       (every (lambda (character)
                (or (babel-ascii-digit-p character)
                    (find character "e.+- ")))
              text)))

(defun babel-parse-number-result (number end text)
  (if (and (numberp number)
           (= end (length text)))
      (values number t)
      (values nil nil)))

(defun babel-parse-number (text)
  (when (babel-numeric-token-p text)
    (handler-case
        (let ((*read-eval* nil)
              (*read-default-float-format* 'double-float)
              (*readtable* (copy-readtable nil)))
          (multiple-value-bind (number end) (read-from-string text nil nil)
            (babel-parse-number-result number end text)))
      (error () (values nil nil)))))

(defun babel-parse-quoted-string (text)
  (unless (and (>= (length text) 2)
               (char= (char text 0) #\")
               (char= (char text (1- (length text))) #\"))
    (babel-signal-error :invalid-literal
                        "Malformed Babel string literal" text))
  (with-output-to-string (output)
    (loop with end = (1- (length text))
          for position from 1 below end
          for character = (char text position)
          do (cond
               ((char= character #\\)
                (when (= (1+ position) end)
                  (babel-signal-error :invalid-literal
                                      "Incomplete escape in Babel string literal"
                                      text))
                (incf position)
                (write-char
                 (case (char text position)
                   (#\" #\")
                   (#\\ #\\)
                   (#\n #\Newline)
                   (#\r #\Return)
                   (#\t #\Tab)
                   (otherwise
                    (babel-signal-error :invalid-literal
                                        "Unsupported escape in Babel string literal"
                                        text)))
                 output))
               ((char= character #\")
                (babel-signal-error :invalid-literal
                                    "Unescaped quote in Babel string literal" text))
               (t (write-char character output))))))

(defun babel-check-expanded-characters (context amount)
  (let* ((document (babel-resolution-context-document context))
         (limits (document-limits document))
         (actual (+ (babel-resolution-context-expanded-characters context)
                    amount)))
    (check-limit limits :max-babel-expanded-characters actual)
    (setf (babel-resolution-context-expanded-characters context) actual)))

(defun babel-check-result-value (context value)
  (let ((active (make-hash-table :test #'eq))
        (limits (document-limits (babel-resolution-context-document context))))
    (labels ((walk (object depth)
               (check-limit limits :max-object-depth depth)
               (typecase object
                 (string
                  (babel-check-expanded-characters context (length object)))
                 ((or null number character symbol) nil)
                 (cons
                  (when (gethash object active)
                    (babel-signal-error :cyclic-result
                                        "Babel result contains a circular list"
                                        "resolver result"))
                  (setf (gethash object active) t)
                  (let ((tail-seen (make-hash-table :test #'eq))
                        (tail object))
                    (loop while (consp tail)
                          do (when (gethash tail tail-seen)
                               (babel-signal-error :cyclic-result
                                                   "Babel result contains a circular list"
                                                   "resolver result"))
                             (setf (gethash tail tail-seen) t)
                             (walk (car tail) (1+ depth))
                             (setf tail (cdr tail)))
                    (unless (null tail) (walk tail (1+ depth))))
                  (remhash object active))
                 (vector
                  (when (gethash object active)
                    (babel-signal-error :cyclic-result
                                        "Babel result contains a circular vector"
                                        "resolver result"))
                  (setf (gethash object active) t)
                  (loop for item across object do (walk item (1+ depth)))
                  (remhash object active))
                 (t
                  (babel-signal-error :unsupported-result
                                      "Babel resolver returned an unsupported value"
                                      (format nil "~A" (type-of object)))))))
      (walk value 0)))
  value)

(defun babel-static-scalar (context value &key resolve-reference)
  (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
    (check-limit (document-limits
                  (babel-resolution-context-document context))
                 :max-babel-expanded-characters (length text))
    (cond
      ((zerop (length text)) "")
      ((char= (char text 0) #\")
       (babel-parse-quoted-string text))
      ((babel-numeric-token-p text)
       (multiple-value-bind (number found-p) (babel-parse-number text)
         (if found-p
             (return-from babel-static-scalar number)
             (return-from babel-static-scalar text))))
      ((and resolve-reference
            (every (lambda (character)
                     (or (babel-ascii-digit-p character)
                         (find character "eE.+-/ ")))
                   text))
       text)
      ((and resolve-reference
            (find (char text 0) '(#\( #\# #\' #\` #\, #\;)))
       (babel-signal-error :unsupported-expression
                           "Babel Lisp expressions are not evaluated"
                           text))
      (resolve-reference
       (babel-resolve-reference context text))
      (t text))))

(defun babel-item-text (context item)
  (let ((children (node-children item)))
    (cond
      ((null children) "")
      ((and (= (length children) 1)
            (eq (node-type (first children)) :paragraph))
       (let* ((paragraph (first children))
              (begin (node-contents-begin paragraph))
              (end (node-contents-end paragraph)))
         (if (and begin end)
             (string-trim '(#\Space #\Tab #\Newline #\Return)
                          (subseq (document-text
                                   (babel-resolution-context-document context))
                                  begin end))
             "")))
      (t
       (babel-signal-error :unsupported-list-item
                           "Nested or multi-part Babel list items are unsupported"
                           (or (node-text item) "list item"))))))

(defun babel-static-node-value (context node)
  (case (node-type node)
    (:table
     (unless (eq (node-property node :type) :org)
       (babel-signal-error :unsupported-reference
                           "Only Org pipe tables can be resolved as Babel values"
                           (node-property node :name)))
     (loop for row in (node-children node)
           unless (eq (node-property row :type) :rule)
             collect (loop for cell in (node-children row)
                           collect (babel-static-scalar
                                    context (node-property cell :value "")))))
    (:plain-list
     (loop for item in (node-children node)
           collect (babel-static-scalar
                    context (babel-item-text context item))))
    (otherwise
     (babel-signal-error :unsupported-reference
                         "Named Babel reference is not static data"
                         (or (node-property node :name)
                             (node-property node :results)
                              (format nil "~A" (node-type node)))))))

(defun babel-with-reference (context name function)
  (let ((active (babel-resolution-context-active-names context)))
    (when (gethash name active)
      (babel-signal-error :circular-reference
                          "Babel variable references are circular" name))
    (let* ((document (babel-resolution-context-document context))
           (depth (1+ (gethash :depth active 0))))
      (check-limit (document-limits document) :max-babel-expansion-depth depth)
      (setf (gethash name active) t
            (gethash :depth active) depth)
      (unwind-protect (funcall function)
        (remhash name active)
        (if (= depth 1)
            (remhash :depth active)
            (setf (gethash :depth active) (1- depth)))))))

(defun babel-resolve-parameters-variables (context parameters)
  (let ((variables '()))
    (dolist (assignment (babel-parameter-variables parameters))
      (let* ((name (car assignment))
             (value (babel-static-scalar context (cdr assignment)
                                         :resolve-reference t))
             (position (position name variables :key #'car :test #'string=)))
        (if position
            (setf (cdr (nth position variables)) value)
          (setf variables (append variables (list (cons name value)))))))
    variables))

(defun babel-resolve-source-block-result (context block &optional parameters)
  (let* ((block-name (node-property block :name))
         (parameters (or parameters
                         (resolve-babel-parameters
                          (babel-resolution-context-document context) block)))
         (variables (babel-resolve-parameters-variables context parameters))
         (resolver (babel-resolution-context-result-resolver context)))
    (unless (and (stringp block-name) (plusp (length block-name)))
      (babel-signal-error :unnamed-result-block
                          "Babel result block has no name" (node-text block)))
    (unless resolver
      (babel-signal-error :missing-result-resolver
                          "A result resolver is required for source block results"
                          block-name))
    (babel-check-result-value context (funcall resolver block-name variables))))

(defun babel-merge-variable-assignments (parameters assignments)
  (dolist (assignment assignments parameters)
    (setf parameters
          (babel-merge-header-parameter
           parameters "VAR"
           (format nil "~A=~A" (car assignment) (cdr assignment))))))

(defun babel-call-variable-assignments (parameters arguments)
  (let ((variables (babel-parameter-variables parameters))
        (variable-index 0)
        (assignments '()))
    (dolist (argument (babel-split-header-list arguments #\,)
             (nreverse assignments))
      (if (position #\= argument)
          (push (babel-parse-assignment argument) assignments)
          (let ((variable (nth variable-index variables)))
            (unless variable
              (babel-signal-error
               :invalid-variable-assignment
               "Babel call positional argument has no target variable"
               argument))
            (push (cons (car variable) argument) assignments)
            (incf variable-index))))))

(defun babel-resolve-call-result (context call &optional additional-variables)
  (let* ((target-name (node-property call :call))
         (target (and (stringp target-name)
                      (gethash target-name
                               (babel-resolution-context-names context)))))
    (unless (and target (eq (node-type target) :src-block))
      (babel-signal-error :unresolved-call-target
                          "Babel call target is not a named source block"
                          target-name))
    (let* ((document (babel-resolution-context-document context))
           (parameters (resolve-babel-parameters document target))
           (assignments (node-property call :arguments))
           (call-variables (append
                            (babel-call-variable-assignments
                             parameters assignments)
                            additional-variables)))
      (setf parameters
            (babel-merge-header-text
             parameters (node-property call :inside-header)))
      (setf parameters
            (babel-merge-header-text
             parameters (node-property call :end-header)))
      (setf parameters
            (babel-merge-variable-assignments parameters call-variables))
      (babel-resolve-source-block-result context target parameters))))

(defun babel-resolve-reference (context name)
  (babel-with-reference
   context name
   (lambda ()
     (let ((node (gethash name (babel-resolution-context-names context))))
       (unless node
         (babel-signal-error :unresolved-reference
                             "Babel variable reference was not found" name))
       (case (node-type node)
         (:table (babel-static-node-value context node))
         (:plain-list (babel-static-node-value context node))
         (:src-block (babel-resolve-source-block-result context node))
         (:babel-call (babel-resolve-call-result context node))
         (otherwise
          (babel-signal-error :unsupported-reference
                              "Named Babel reference has an unsupported node type"
                              (format nil "~A" (node-type node)))))))))

(defun resolve-babel-variables (document src-block &key result-resolver)
  "Resolve SRC-BLOCK's :var parameters without evaluating source code.

Return an ordered alist from variable names to Common Lisp values. RESULT-RESOLVER,
when needed for a named source block or #+CALL result, is called with the executable
block name and its resolved variable alist. Static numbers, strings, Org tables and
simple lists are read locally; Lisp expressions are rejected rather than evaluated."
  (validate-babel-source-block document src-block)
  (unless (or (null result-resolver) (functionp result-resolver))
    (error 'type-error :datum result-resolver :expected-type '(or null function)))
  (let ((context (make-babel-resolution-context
                  :document document
                  :result-resolver result-resolver
                  :names (babel-build-name-index document))))
    (babel-resolve-parameters-variables
     context (resolve-babel-parameters document src-block))))

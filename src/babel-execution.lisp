(in-package #:org-kit)
(defvar *babel-execution-document* nil)
(defvar *babel-execution-resolution-context* nil)

(defstruct (babel-execution-result
            (:constructor %make-babel-execution-result
                (&key status value hash edit-result)))
  "One source-block execution outcome and its optional result edit."
  status value hash edit-result)

(defun register-babel-executor (registry language executor)
  "Register EXECUTOR for LANGUAGE under the :BABEL-EXECUTOR callback kind."
  (register-callback registry :babel-executor language executor))

(defun make-babel-shell-reference-executor (runner &key (shell "sh"))
  "Return an opt-in shell executor that delegates process execution to RUNNER.
SHELL is passed to RUNNER with the expanded SCRIPT, optional STDIN, and raw CMDLINE.
RUNNER must return standard output as a string. This helper starts no process.
Persistent shell sessions are unsupported; :stdin and :cmdline use one-shot scripts."
  (unless (functionp runner)
    (error 'type-error :datum runner :expected-type 'function))
  (unless (stringp shell)
    (error 'type-error :datum shell :expected-type 'string))
  (lambda (body parameters variables)
    (let* ((document (or *babel-execution-document* (make-document "")))
           (session (babel-execution-parameter parameters "SESSION"))
           (stdin (babel-shell-stdin-text parameters))
           (cmdline (babel-execution-parameter parameters "CMDLINE")))
      (when (and session
                 (not (string= session "none"))
                 (not (or stdin cmdline)))
        (babel-results-error
         :unsupported-session
         "Persistent shell sessions are not supported by the reference executor"
         session))
      (let* ((script (babel-shell-build-script
                      body parameters variables shell stdin cmdline)))
        (babel-shell-check-inputs document (list shell script stdin cmdline))
        (let ((stdout (funcall runner shell script stdin cmdline)))
          (unless (stringp stdout)
            (error 'type-error :datum stdout :expected-type 'string))
          (babel-shell-check-output document stdout)
          (babel-shell-import-result stdout parameters))))))
(defun babel-shell-quote (text)
  (with-output-to-string (output)
    (write-char #\' output)
    (loop for character across text
          do (if (char= character #\')
                 (write-string "'\"'\"'" output)
               (write-char character output)))
    (write-char #\' output)))
(defun babel-shell-import-result (text parameters)
  (let* ((tokens (mapcar #'string-downcase
                         (babel-split-header-list
                          (babel-execution-parameter parameters "RESULTS")
                          :whitespace)))
         (value-p (member "value" tokens :test #'string=))
         (table-p (member "table" tokens :test #'string=))
         (discard-p (member "discard" tokens :test #'string=))
         (scalar-p (some (lambda (token)
                           (member token
                                   '("scalar" "verbatim" "html" "code"
                                     "pp" "file")
                                   :test #'string=))
                         tokens))
         (raw-output-p (and (not table-p)
                            (some (lambda (token)
                                    (member token '("output" "raw" "org")
                                            :test #'string=))
                                  tokens)))
         (result (if value-p
                     (let ((lines (remove "" (babel-shell-output-lines text)
                                          :test #'string=)))
                       (car (last lines)))
                   text)))
    (cond
      (discard-p nil)
      ((and (not value-p) (not table-p)
            (or raw-output-p (null tokens)))
       result)
      ((or (null result) (string= result "")) nil)
      (scalar-p result)
      (t (babel-shell-import-table result)))))
(defun babel-shell-import-table (text)
  (let ((lines (babel-shell-output-lines text)))
    (if (or (null lines)
            (> (length lines) 999)
            (string= "" (car (last lines))))
        nil
        (let ((separator
                (cond
                  ((not (some (lambda (line)
                                (and (plusp (length line))
                                     (not (find #\Tab line))))
                              lines))
                   :tab)
                  ((not (some (lambda (line)
                                (and (plusp (length line))
                                     (not (find #\, line))))
                              lines))
                   :csv)
                  ((= (length lines) 1) :single)
                  (t :whitespace))))
          (labels ((split-pipes (cell)
                     (let ((fields (babel-shell-split-char cell #\|)))
                       (if (and (plusp (length cell))
                                (char= (char cell (1- (length cell))) #\|))
                           (butlast fields)
                           fields)))
                   (split-whitespace-line (line)
                     (let* ((start (or (position-if-not
                                        (lambda (character)
                                          (char= character #\Space))
                                        line)
                                       (length line)))
                            (index start)
                            (fields nil)
                            (current (make-string-output-stream))
                            (separator-at-end-p nil))
                       (loop while (< index (length line))
                             for character = (char line index)
                             do (cond
                                  ((char= character #\|)
                                   (push (get-output-stream-string current)
                                         fields)
                                   (setf current (make-string-output-stream)
                                         separator-at-end-p t)
                                   (incf index))
                                  ((babel-whitespace-p character)
                                   (push (get-output-stream-string current)
                                         fields)
                                   (setf current (make-string-output-stream)
                                         separator-at-end-p t)
                                   (loop while (and (< index (length line))
                                                    (babel-whitespace-p
                                                     (char line index)))
                                         do (incf index)))
                                  (t
                                   (write-char character current)
                                   (setf separator-at-end-p nil)
                                   (incf index)))
                             finally
                                (unless separator-at-end-p
                                  (push (get-output-stream-string current)
                                        fields)))
                       (nreverse fields))))
            (let* ((rows
                     (mapcar (lambda (line)
                               (case separator
                                 (:tab (mapcan #'split-pipes
                                               (babel-shell-split-char
                                                line #\Tab)))
                                 (:csv (mapcan #'split-pipes
                                               (babel-shell-split-csv line)))
                                 (:single (split-pipes line))
                                 (otherwise (split-whitespace-line line))))
                             lines))
                   (column-count (reduce #'max rows :key #'length
                                         :initial-value 0))
                   (rows
                     (mapcar (lambda (row)
                               (append row
                                       (make-list (- column-count (length row))
                                                  :initial-element "")))
                             rows))
                   (rows (mapcar (lambda (row)
                                   (mapcar #'babel-shell-read-cell row))
                                 rows)))
              (if (and (= (length rows) 1)
                       (= (length (first rows)) 1))
                  (caar rows)
                  rows)))))))
(defun babel-shell-read-cell (cell)
  (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return) cell)))
    (when (and (>= (length text) 2)
               (char= (char text 0) #\")
               (char= (char text (1- (length text))) #\"))
      (setf text (subseq text 1 (1- (length text)))))
    (multiple-value-bind (number found-p)
        (if (babel-numeric-token-p text)
            (babel-parse-number text)
            (values nil nil))
      (if found-p number text))))
(defun babel-shell-split-csv (text)
  (let ((output (make-string-output-stream))
        (fields nil)
        (quoted-p nil))
    (loop for index from 0 below (length text)
          for character = (char text index)
          do (cond
               ((and quoted-p (char= character #\"))
                (if (and (< (1+ index) (length text))
                         (char= (char text (1+ index)) #\"))
                    (progn
                      (write-char character output)
                      (incf index))
                    (setf quoted-p nil)))
               ((char= character #\")
                (setf quoted-p t))
               ((and (not quoted-p) (char= character #\,))
                (push (get-output-stream-string output) fields)
                (setf output (make-string-output-stream)))
               (t (write-char character output)))
          finally (push (get-output-stream-string output) fields))
    (nreverse fields)))
(defun babel-shell-split-char (text delimiter)
  (let ((start 0))
    (loop for position = (position delimiter text :start start)
          collect (subseq text start (or position (length text)))
          while position
          do (setf start (1+ position)))))
(defun babel-shell-output-lines (text)
  (when (plusp (length text))
    (let ((start 0)
          (lines nil))
      (loop for position = (position #\Newline text :start start)
            do (push (string-right-trim '(#\Return)
                                         (subseq text start position))
                     lines)
            while position
            do (setf start (1+ position)))
      (setf lines (nreverse lines))
      (if (char= (char text (1- (length text))) #\Newline)
          (butlast lines)
          lines))))
(defun babel-shell-check-output (document text)
  (check-limit (document-limits document) :max-output-length (length text))
  (check-limit
   (document-limits document) :max-output-octets
   (babel-results-encoded-range-octets
    text 0 (length text) (or (document-encoding document) :utf-8))))
(defun babel-shell-check-inputs (document strings)
  (let ((characters 0)
        (octets 0)
        (encoding (or (document-encoding document) :utf-8)))
    (dolist (text strings)
      (when text
        (unless (stringp text)
          (error 'type-error :datum text :expected-type 'string))
        (incf characters (length text))
        (incf octets
              (babel-results-encoded-range-octets
               text 0 (length text) encoding))))
    (check-limit (document-limits document) :max-input-length characters)
    (check-limit (document-limits document) :max-input-length octets)))
(defun babel-shell-stdin-text (parameters)
  (let ((stdin (babel-execution-parameter parameters "STDIN")))
    (when stdin
      (let* ((context (or *babel-execution-resolution-context*
                          (make-babel-resolution-context
                           :document (or *babel-execution-document*
                                         (make-document "")))))
             (value (babel-static-scalar
                     context stdin
                     :resolve-reference
                     (not (null *babel-execution-resolution-context*)))))
        (babel-shell-value-text value parameters)))))
(defun babel-shell-cell-text (cell)
  (cond
    ((stringp cell) cell)
    ((null cell) "nil")
    ((symbolp cell) (string-downcase (symbol-name cell)))
    ((or (numberp cell) (characterp cell)) (princ-to-string cell))
    (t (string-downcase (format nil "~S" cell)))))
(defun babel-shell-build-script (body parameters variables shell stdin cmdline)
  (let* ((expanded (babel-shell-expand-body body parameters variables shell))
         (tokens (mapcar #'string-downcase
                         (babel-split-header-list
                          (babel-execution-parameter parameters "RESULTS")
                          :whitespace)))
         (value-p (member "value" tokens :test #'string=))
         (expanded (if value-p
                       (concatenate 'string expanded (string #\Newline)
                                    "echo $?")
                       expanded))
         (shebang (babel-execution-parameter parameters "SHEBANG"))
         (shebang-script-p
           (and shebang
                (position-if-not #'babel-whitespace-p shebang)))
         (external-p (or stdin cmdline shebang-script-p))
         (padline-p (not (string=
                          (or (babel-execution-parameter parameters "PADLINE")
                              "yes")
                          "no")))
         (newline (string #\Newline)))
    (if external-p
        (concatenate 'string
                     (or shebang (format nil "#!/usr/bin/env ~A" shell))
                     newline
                     (if padline-p newline "")
                     expanded)
        (string-trim '(#\Space #\Tab #\Newline #\Return) expanded))))
(defun babel-shell-expand-body (body parameters variables shell)
  (let ((parts (append
                (let ((prologue (babel-execution-parameter
                                 parameters "PROLOGUE")))
                  (when prologue (list prologue)))
                (mapcar (lambda (variable)
                          (babel-shell-variable-assignment
                           (car variable) (cdr variable) parameters shell))
                        variables)
                (list body)
                (let ((epilogue (babel-execution-parameter
                                 parameters "EPILOGUE")))
                  (when epilogue (list epilogue))))))
    (format nil "~{~A~^~%~}" parts)))
(defun babel-shell-variable-assignment (name value parameters shell)
  (let* ((bash-p (and (>= (length shell) 4)
                      (string= "bash" shell :start2 (- (length shell) 4))))
         (fish-p (and (>= (length shell) 4)
                      (string= "fish" shell :start2 (- (length shell) 4))))
         (bash-table-p (and (consp value)
                            (consp (car value))
                            (consp (cdr (car value)))))
         (list-p (and (listp value)
                      (integerp (edit-proper-list-length value))
                      (consp value))))
    (cond
      ((and bash-p bash-table-p)
       (with-output-to-string (output)
         (format output "unset ~A~%declare -A ~A" name name)
         (dolist (row value)
           (terpri output)
           (format output "~A[~A]=~A"
                   name
                   (babel-shell-quote
                    (babel-shell-value-text (car row) parameters))
                   (babel-shell-quote
                    (babel-shell-value-text (cdr row) parameters))))))
      ((and bash-p list-p)
       (format nil "unset ~A~%declare -a ~A=( ~{~A~^ ~} )"
               name name
               (mapcar (lambda (item)
                         (babel-shell-quote
                          (babel-shell-value-text item parameters)))
                       value)))
      ((and fish-p)
       (format nil "set ~A ~A" name
               (babel-shell-quote
                (babel-shell-value-text value parameters))))
      (t
       (format nil "~A=~A" name
               (babel-shell-quote
                (babel-shell-value-text value parameters)))))))
(defun babel-shell-value-text (value parameters)
  (cond
    ((and (consp value)
          (or (consp (car value)) (babel-shell-hline-p (car value))))
     (babel-shell-table-text value parameters))
    ((and (listp value) (integerp (edit-proper-list-length value)))
     (babel-shell-join-values value (string #\Newline)))
    (t (babel-shell-cell-text value))))
(defun babel-shell-table-text (rows parameters)
  (let* ((separator (or (babel-execution-parameter parameters "SEPARATOR")
                        (string #\Tab)))
         (hline (when (string=
                       (or (babel-execution-parameter parameters "HLINES")
                           "no")
                       "yes")
                  (or (babel-execution-parameter parameters "HLINE-STRING")
                      "hline")))
         (written-p nil))
    (with-output-to-string (output)
      (dolist (row rows)
        (unless (and (babel-shell-hline-p row) (null hline))
          (when written-p (terpri output))
          (if (babel-shell-hline-p row)
              (write-string hline output)
              (write-string (babel-shell-join-values row separator) output))
          (setf written-p t))))))
(defun babel-shell-hline-p (value)
  (and (symbolp value)
       (string-equal (symbol-name value) "hline")))
(defun babel-shell-join-values (items separator)
  (with-output-to-string (output)
    (loop for item in items
          for first-p = t then nil
          do (unless first-p (write-string separator output))
             (write-string (babel-shell-cell-text item) output))))

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
             (value (let ((*babel-execution-document* document)
                          (*babel-execution-resolution-context* resolution))
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

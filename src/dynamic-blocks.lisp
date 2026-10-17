;;;; src/dynamic-blocks.lisp
(in-package #:org-kit)

(define-condition org-dynamic-block-error (org-kit-error)
  ((reason :initarg :reason :initform nil)
   (name :initarg :name :initform nil)
   (value :initarg :value :initform nil))
  (:report (lambda (condition stream)
             (format stream "~A~@[ (~(~A~))~]~@[ for ~A~]"
                     (org-kit-error-message condition)
                     (org-dynamic-block-error-reason condition)
                     (org-dynamic-block-error-name condition))))
  (:documentation "Signalled when a dynamic block cannot be dispatched."))

(define-condition org-clocktable-parameter-error (org-kit-error)
  ((reason :initarg :reason :initform nil)
   (parameter :initarg :parameter :initform nil)
   (value :initarg :value :initform nil))
  (:report (lambda (condition stream)
             (format stream "~A~@[ (~(~A~))~]~@[ for :~A~]~@[ with ~A~]"
                     (org-kit-error-message condition)
                     (org-clocktable-parameter-error-reason condition)
                     (org-clocktable-parameter-error-parameter condition)
                     (org-clocktable-parameter-error-value condition))))
  (:documentation "Signalled when a clocktable parameter is unsupported or invalid."))

(defun org-dynamic-block-error-reason (condition)
  "Return the reason the dynamic block operation failed."
  (slot-value condition 'reason))

(defun org-dynamic-block-error-name (condition)
  "Return the normalized block or parameter name associated with the failure."
  (slot-value condition 'name))

(defun dynamic-block-error-value (condition)
  (slot-value condition 'value))

(defun org-clocktable-parameter-error-reason (condition)
  "Return the reason the clocktable parameter was refused."
  (slot-value condition 'reason))

(defun org-clocktable-parameter-error-parameter (condition)
  "Return the clocktable parameter name associated with the failure."
  (slot-value condition 'parameter))

(defun org-clocktable-parameter-error-value (condition)
  "Return the bounded clocktable parameter value associated with the failure."
  (slot-value condition 'value))

(defvar *dynamic-block-handlers* (make-hash-table :test #'equal))
(defvar *dynamic-block-handler-parameters* (make-hash-table :test #'equal))

(defun dynamic-block-normalize-name (name)
  (let ((text (etypecase name
                (string name)
                (symbol (symbol-name name)))))
    (string-downcase text)))

(defun register-dynamic-block-handler (name function &key parameters)
  "Register FUNCTION to update dynamic blocks named NAME.

FUNCTION receives DOCUMENT, the block node, parsed parameters, and optional
:DOCUMENT-RESOLVER and :TIME-CONTEXT arguments. It returns replacement body
text, an EDIT-RESULT, or a list of string-cell rows, with the header row first.
PARAMETERS is a list of keyword names accepted from the block."
  (let ((normalized (dynamic-block-normalize-name name)))
    (unless (plusp (length normalized))
      (error 'type-error :datum name :expected-type '(or string symbol)))
    (check-type function function)
    (unless (and (%query-proper-list-p parameters)
                 (every (lambda (parameter)
                          (and (symbolp parameter)
                               (eq (symbol-package parameter) (find-package "KEYWORD"))))
                        parameters))
      (error 'type-error :datum parameters
                         :expected-type '(or null list)))
    (setf (gethash normalized *dynamic-block-handlers*) function)
    (setf (gethash normalized *dynamic-block-handler-parameters*)
          (copy-list parameters))
    normalized))

(defun dynamic-block-skip-space (text position)
  (loop while (and (< position (length text))
                   (find (char text position) '(#\Space #\Tab)))
        do (incf position)
        finally (return position)))

(defun dynamic-block-parameter-name-character-p (character)
  (or (and (char<= #\a character #\z))
      (and (char<= #\A character #\Z))
      (and (char<= #\0 character #\9))
      (member character '(#\- #\_))))

(defun dynamic-block-parse-string-value (text start)
  (with-output-to-string (stream)
    (loop with position = (1+ start)
          while (< position (length text))
          for character = (char text position)
          do (cond
               ((char= character #\\)
                (when (= (1+ position) (length text))
                  (return-from dynamic-block-parse-string-value
                    (values nil position nil)))
                (incf position)
                (write-char (char text position) stream))
               ((char= character #\")
                (return-from dynamic-block-parse-string-value
                  (values (get-output-stream-string stream) (1+ position) t)))
               (t (write-char character stream)))
             (incf position)
          finally (return-from dynamic-block-parse-string-value
                    (values nil position nil)))))

(defun dynamic-block-parse-atom-value (text start)
  (let* ((end (or (position-if
                   (lambda (character)
                     (or (find character '(#\Space #\Tab #\) #\())))
                   text :start start)
                  (length text)))
         (atom (subseq text start end)))
    (cond
      ((zerop (length atom)) (values nil start nil))
      ((string-equal atom "nil") (values nil end t))
      ((string-equal atom "t") (values t end t))
      ((every #'digit-char-p atom)
       (values (parse-integer atom) end t))
      ((and (> (length atom) 1)
            (char= (char atom 0) #\-)
            (loop for index from 1 below (length atom)
                  always (digit-char-p (char atom index))))
       (values (parse-integer atom) end t))
      (t (values atom end t)))))

(defun dynamic-block-parse-list-value (text start)
  (let ((position (1+ start))
        (values nil))
    (loop
      (setf position (dynamic-block-skip-space text position))
      (when (>= position (length text))
        (return-from dynamic-block-parse-list-value
          (values nil position nil)))
      (when (char= (char text position) #\))
        (return-from dynamic-block-parse-list-value
          (values (nreverse values) (1+ position) t)))
      (multiple-value-bind (value next valid-p)
          (dynamic-block-parse-parameter-value text position)
        (unless valid-p
          (return-from dynamic-block-parse-list-value
            (values nil position nil)))
        (push value values)
        (setf position next)))))

(defun dynamic-block-parse-parameter-value (text start)
  (let ((position (dynamic-block-skip-space text start)))
    (cond
      ((>= position (length text)) (values nil position nil))
      ((char= (char text position) #\")
       (dynamic-block-parse-string-value text position))
      ((char= (char text position) #\()
       (dynamic-block-parse-list-value text position))
      ((char= (char text position) #\)) (values nil position nil))
      (t (dynamic-block-parse-atom-value text position)))))

(defun dynamic-block-parse-parameters (text allowed-parameters)
  "Parse dynamic block parameter text into a keyword property list.

Quoted strings and parenthesized lists are parsed without evaluating Lisp.
Only names declared by the registered handler are accepted."
  (check-type text string)
  (let ((position (dynamic-block-skip-space text 0))
        (parameters nil)
        (seen (make-hash-table :test #'equal)))
    (loop while (< position (length text))
          do (unless (char= (char text position) #\:)
               (error 'org-dynamic-block-error
                      :message "Malformed dynamic block parameters"
                      :reason :invalid-parameters))
             (let* ((name-start (1+ position))
                    (name-end (position-if-not
                               #'dynamic-block-parameter-name-character-p
                               text :start name-start)))
               (when (or (= name-start (length text))
                         (and name-end (= name-end name-start)))
                 (error 'org-dynamic-block-error
                        :message "Malformed dynamic block parameters"
                        :reason :invalid-parameters))
               (unless name-end
                 (setf name-end (length text)))
               (let* ((name-text (string-downcase
                                  (subseq text name-start name-end)))
                      (name (find-symbol (string-upcase name-text) :keyword))
                      (value-start (dynamic-block-skip-space text name-end)))
                 (when (or (= value-start (length text))
                           (char= (char text value-start) #\:))
                   (error 'org-dynamic-block-error
                          :message "Missing dynamic block parameter value"
                          :reason :missing-parameter-value
                          :name name-text))
                 (multiple-value-bind (value next valid-p)
                     (dynamic-block-parse-parameter-value text value-start)
                   (unless valid-p
                     (error 'org-dynamic-block-error
                            :message "Malformed dynamic block parameter value"
                            :reason :invalid-parameters
                            :name name-text))
                   (when (gethash name-text seen)
                     (error 'org-dynamic-block-error
                            :message "Duplicate dynamic block parameter"
                            :reason :duplicate-parameter
                            :name name-text
                            :value value))
                   (unless (and name (member name allowed-parameters :test #'eq))
                     (error 'org-dynamic-block-error
                            :message "Dynamic block parameter is not registered"
                            :reason :unsupported-parameter
                            :name name-text
                            :value value))
                   (setf (gethash name-text seen) t
                         parameters (append parameters (list name value))
                         position (dynamic-block-skip-space text next))))))
    parameters))

(defun dynamic-block-row-data-p (value)
  (and (listp value)
       (every (lambda (row)
                (and (listp row) (every #'stringp row)))
              value)))

(defun dynamic-block-render-row-data (rows)
  (with-output-to-string (stream)
    (loop for row in rows
          for row-index from 0
          do (write-char #\| stream)
             (dolist (cell row)
               (write-char #\Space stream)
               (write-string (table-edit-escape-cell-value cell) stream)
               (write-string " |" stream))
             (terpri stream)
             (when (zerop row-index)
               (write-char #\| stream)
               (loop repeat (length row)
                     for index from 0
                     do (write-string "---" stream)
                        (when (< index (1- (length row)))
                          (write-char #\+ stream)))
               (write-char #\| stream)
               (terpri stream)))))

(defun dynamic-block-find-table (document)
  (let ((found nil))
    (map-nodes (lambda (node)
                 (when (and (null found) (eq (node-type node) :table))
                   (setf found node)))
               (document-root document))
    found))

(defun dynamic-block-document-children (document)
  (loop for child in (node-children (document-root document))
        append (if (eq (node-type child) :section)
                   (node-children child)
                   (list child))))

(defun dynamic-block-body-span (document block)
  (edit-clocktable-body-span document block block))

(defun dynamic-block-newline (source block)
  (let ((newline
          (position #\Newline source :start (node-begin block)
                                      :end (node-end block))))
    (if (and newline (plusp newline)
             (char= (char source (1- newline)) #\Return))
        (format nil "~C~C" #\Return #\Newline)
        (string #\Newline))))

(defun dynamic-block-normalized-body-length (body newline)
  (let ((size 0)
        (ends-with-newline-p nil))
    (loop for index from 0 below (length body)
          for character = (char body index)
          do (cond
               ((char= character #\Return)
                (if (and (< (1+ index) (length body))
                         (char= (char body (1+ index)) #\Newline))
                    (progn
                      (incf size (length newline))
                      (incf index)
                      (setf ends-with-newline-p t))
                    (progn
                      (incf size)
                      (setf ends-with-newline-p nil))))
               ((char= character #\Newline)
                (incf size (length newline))
                (setf ends-with-newline-p t))
               (t
                (incf size)
                (setf ends-with-newline-p nil)))
          finally (when (and (plusp (length body))
                             (not ends-with-newline-p))
                    (incf size (length newline))))
    size))

(defun dynamic-block-normalize-body (body newline &optional limits)
  (when limits
    (check-limit limits :max-input-length
                 (dynamic-block-normalized-body-length body newline)))
  (let ((normalized
          (with-output-to-string (stream)
            (loop for index from 0 below (length body)
                  for character = (char body index)
                  do (cond
                       ((char= character #\Return)
                        (if (and (< (1+ index) (length body))
                                 (char= (char body (1+ index)) #\Newline))
                            (progn
                              (write-string newline stream)
                              (incf index))
                            (write-char character stream)))
                       ((char= character #\Newline)
                        (write-string newline stream))
                       (t (write-char character stream)))))))
    (if (and (plusp (length normalized))
             (char/= (char normalized (1- (length normalized))) #\Newline))
        (concatenate 'string normalized newline)
        normalized)))

(defun dynamic-block-update-body (document block body)
  (let* ((body (dynamic-block-normalize-body
                body (dynamic-block-newline (document-text document) block)
                (document-limits document)))
         (temporary (parse-org body :limits (document-limits document)
                                    :settings (document-settings document)))
         (table (dynamic-block-find-table temporary))
         (aligned-document
           (if table
               (edit-result-updated-document (table-align temporary table))
               temporary))
         (replacement (document-text aligned-document)))
    (multiple-value-bind (start end)
        (dynamic-block-body-span document block)
      (let ((expected (edit-canonical-copy (edit-canonical-document document))))
        (edit-canonical-set-children
         expected (edit-node-path document block)
         (mapcar #'edit-canonical-node
                 (dynamic-block-document-children aligned-document)))
        (edit-apply-patches-result
         document
         (list (edit-make-patch start end replacement block))
         :target block :expected expected)))))

(defun update-dynamic-block (document dynamic-block
                             &key document-resolver time-context)
  "Update one dynamic block using its registered handler.

DOCUMENT-RESOLVER resolves external scope identifiers to parsed documents.
TIME-CONTEXT supplies the explicit reference time for time-dependent handlers."
  (let* ((block (edit-resolve-node document dynamic-block '(:dynamic-block)))
         (name (dynamic-block-normalize-name
                (node-property block :block-name)))
         (handler (gethash name *dynamic-block-handlers*)))
    (unless handler
      (error 'org-dynamic-block-error
             :message "No handler is registered for this dynamic block"
             :reason :unregistered-name
             :name name))
    (unless (or (null document-resolver) (functionp document-resolver))
      (error 'type-error :datum document-resolver :expected-type 'function))
    (unless (or (null time-context) (org-time-context-p time-context))
      (error 'type-error :datum time-context :expected-type 'org-time-context))
    (let* ((argument-text (or (node-property block :arguments) ""))
           (parameters
             (handler-case
                 (dynamic-block-parse-parameters
                  argument-text
                  (gethash name *dynamic-block-handler-parameters*))
               (org-dynamic-block-error (condition)
                 (if (string= name "clocktable")
                     (let ((reason
                             (if (eq (org-dynamic-block-error-reason condition)
                                     :unsupported-parameter)
                                 :unsupported-parameter
                                 :invalid-value))
                           (parameter
                             (or (org-dynamic-block-error-name condition)
                                 "syntax"))
                           (value (dynamic-block-error-value condition)))
                       (error 'org-clocktable-parameter-error
                              :message (if (eq reason :unsupported-parameter)
                                           "Unsupported clocktable parameter"
                                           "Invalid clocktable parameter value")
                              :reason reason
                              :parameter parameter
                              :value (bounded-text
                                      (if value (princ-to-string value)
                                          argument-text))))
                     (error condition)))))
           (result (funcall handler document block parameters
                            :document-resolver document-resolver
                            :time-context time-context)))
      (cond
        ((edit-result-p result) result)
        ((stringp result) (dynamic-block-update-body document block result))
        ((dynamic-block-row-data-p result)
         (dynamic-block-update-body
          document block (dynamic-block-render-row-data result)))
        (t
         (error 'type-error :datum result
                             :expected-type '(or edit-result string list)))))))

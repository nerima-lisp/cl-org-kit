;;;; src/babel-noweb.lisp
(in-package #:org-kit)

(defstruct (babel-noweb-context (:constructor make-babel-noweb-context))
  document
  resolution
  context
  (source-blocks (make-hash-table :test #'equal))
  (reference-groups (make-hash-table :test #'equal))
  (active-blocks (make-hash-table :test #'eq))
  (expanded-characters 0))

(defvar *babel-noweb-root-parameters* nil)

(defun babel-noweb-normalize-body (body)
  (let ((length (length body)))
    (cond
      ((and (plusp length) (char= (char body (1- length)) #\Newline))
       (subseq body 0 (if (and (> length 1)
                              (char= (char body (- length 2)) #\Return))
                         (- length 2)
                         (1- length))))
      ((and (plusp length) (char= (char body (1- length)) #\Return))
       (subseq body 0 (1- length)))
      (t body))))

(defun babel-noweb-normalize-parameter-value (value)
  (if (and (stringp value)
           (plusp (length value))
           (char= (char value 0) #\"))
      (babel-parse-quoted-string value)
      value))

(defun babel-noweb-build-indexes (document)
  (let ((source-blocks (make-hash-table :test #'equal))
        (reference-groups (make-hash-table :test #'equal)))
    (map-nodes
     (lambda (node)
       (when (eq (node-type node) :src-block)
         (let* ((name (node-property node :name))
                (parameters (resolve-babel-parameters document node))
                (noweb-ref-position
                  (position "NOWEB-REF" parameters :key #'car :test #'string=))
                (noweb-ref
                  (and noweb-ref-position
                       (babel-noweb-normalize-parameter-value
                        (cdr (nth noweb-ref-position parameters))))))
           (when (and (stringp name) (plusp (length name)))
             (multiple-value-bind (value found-p) (gethash name source-blocks)
               (declare (ignore value))
               (unless found-p (setf (gethash name source-blocks) node))))
           (when (and (stringp noweb-ref) (plusp (length noweb-ref)))
             (push node (gethash noweb-ref reference-groups))))))
     (document-root document))
    (maphash (lambda (name blocks)
               (setf (gethash name reference-groups) (nreverse blocks)))
             reference-groups)
    (values source-blocks reference-groups)))

(defun babel-noweb-mode-action (parameters context)
  (let* ((position (position "NOWEB" parameters :key #'car :test #'string=))
         (mode (if position
                   (string-downcase
                    (or (babel-noweb-normalize-parameter-value
                         (cdr (nth position parameters)))
                        "no"))
                   "no")))
    (cond
      ((string= mode "yes") :expand)
      ((string= mode "no") :preserve)
      ((string= mode "eval") (if (eq context :eval) :expand :preserve))
      ((string= mode "tangle") (if (eq context :tangle) :expand :preserve))
      ((string= mode "no-export")
       (if (eq context :export) :preserve :expand))
      ((string= mode "strip-export")
       (if (eq context :export) :strip :expand))
      ((string= mode "strip-tangle")
       (if (eq context :tangle) :strip :expand))
      (t
       (babel-signal-error :invalid-noweb-mode
                           "Unsupported Babel :noweb mode" mode)))))

(defun babel-noweb-parameter (block name document)
  (let* ((override *babel-noweb-root-parameters*)
         (parameters
           (if (and override (eq block (car override)))
               (cdr override)
               (resolve-babel-parameters document block)))
         (position (position name parameters :key #'car :test #'string=)))
    (and position
         (babel-noweb-normalize-parameter-value
          (cdr (nth position parameters))))))

(defun babel-noweb-reference-group (context name)
  (gethash name (babel-noweb-context-reference-groups context)))

(defun babel-noweb-check-output-length (context length)
  (check-limit
   (document-limits (babel-noweb-context-document context))
   :max-babel-expanded-characters
   (+ (babel-noweb-context-expanded-characters context) length)))

(defun babel-noweb-check-output-chunk (context written amount)
  (let* ((limits (document-limits (babel-noweb-context-document context)))
         (limit-name :max-babel-expanded-characters)
         (limit (limit-value limits limit-name))
         (current (+ (babel-noweb-context-expanded-characters context)
                     written))
         (remaining (- limit current))
         (actual (+ current amount)))
    (check-limit
     limits limit-name
     (if (> amount remaining) (1+ limit) actual))))

(defun babel-noweb-count-output-length (context length)
  (babel-noweb-check-output-length context length)
  (incf (babel-noweb-context-expanded-characters context) length))

(defclass babel-noweb-bounded-output-stream
    (sb-gray:fundamental-character-output-stream)
  ((context :initarg :context :reader babel-noweb-bounded-output-context)
   (output :initform (make-string-output-stream)
           :reader babel-noweb-bounded-output-stream-output)
   (length :initform 0 :accessor babel-noweb-bounded-output-stream-length)))

(defmethod sb-gray:stream-write-char
    ((stream babel-noweb-bounded-output-stream) character)
  (babel-noweb-check-output-length
   (babel-noweb-bounded-output-context stream)
   (1+ (babel-noweb-bounded-output-stream-length stream)))
  (write-char character (babel-noweb-bounded-output-stream-output stream))
  (incf (babel-noweb-bounded-output-stream-length stream))
  character)

(defmethod sb-gray:stream-write-string
    ((stream babel-noweb-bounded-output-stream) string &optional (start 0) end)
  (let* ((end (or end (length string)))
         (amount (- end start)))
    (babel-noweb-check-output-chunk
     (babel-noweb-bounded-output-context stream)
     (babel-noweb-bounded-output-stream-length stream)
     amount)
    (write-string string (babel-noweb-bounded-output-stream-output stream)
                  :start start :end end)
    (incf (babel-noweb-bounded-output-stream-length stream) amount)
    string))

(defun babel-noweb-format-result (context value)
  (let ((stream (make-instance 'babel-noweb-bounded-output-stream
                               :context context)))
    (format stream "~S" value)
    (get-output-stream-string
     (babel-noweb-bounded-output-stream-output stream))))

(defun babel-noweb-write-range (context stream current-length text start end)
  (let ((length (- end start)))
    (babel-noweb-count-output-length context length)
    (write-string text stream :start start :end end)
    (+ current-length length)))

(defun babel-noweb-write (context stream current-length text)
  (babel-noweb-write-range context stream current-length text 0 (length text)))

(defun babel-noweb-line-prefix (text position)
  (let* ((newline (position #\Newline text :end position :from-end t))
         (return (position #\Return text :end position :from-end t))
         (start (1+ (max (or newline -1) (or return -1)))))
    (subseq text start position)))

(defun babel-noweb-prefix-later-lines (context text prefix)
  (if (zerop (length prefix))
      text
      (let ((length (length text))
            (prefix-size (length prefix)))
        (multiple-value-bind (prefix-count crlf-count)
            (loop with position = 0
                  with prefix-count = 0
                  with crlf-count = 0
                  while (< position length)
                  for character = (char text position)
                  do (cond
                       ((char= character #\Return)
                        (incf position)
                        (when (and (< position length)
                                   (char= (char text position) #\Newline))
                          (incf position)
                          (incf crlf-count))
                        (when (< position length) (incf prefix-count)))
                       ((char= character #\Newline)
                        (incf position)
                        (when (< position length) (incf prefix-count)))
                       (t (incf position)))
                  finally (return (values prefix-count crlf-count)))
          (let ((expanded-length (+ (- length crlf-count)
                                    (* prefix-count prefix-size))))
            (babel-noweb-check-output-length context expanded-length)
            (with-output-to-string (output)
              (let ((line-start 0))
                (loop for position from 0 below length
                      for character = (char text position)
                      when (or (char= character #\Newline)
                               (char= character #\Return))
                        do (write-string (subseq text line-start position) output)
                           (write-char #\Newline output)
                           (when (and (char= character #\Return)
                                      (< (1+ position) length)
                                      (char= (char text (1+ position)) #\Newline))
                             (incf position))
                           (when (< (1+ position) length)
                             (write-string prefix output))
                           (setf line-start (1+ position)))
                (write-string (subseq text line-start) output))))))))

(defun babel-noweb-delimiters (text start)
  (let ((open (search "<<" text :start2 start)))
    (when open
      (let* ((line-end (or (position-if (lambda (character)
                                          (or (char= character #\Newline)
                                              (char= character #\Return)))
                                        text :start (1+ open))
                            (length text)))
             (close (search ">>" text :start2 (+ open 2) :end2 line-end)))
        (when close
          (let ((reference (subseq text (+ open 2) close)))
            (when (and (plusp (length reference))
                       (not (find (char reference 0) '(#\Space #\Tab)))
                       (not (find (char reference (1- (length reference)))
                                  '(#\Space #\Tab))))
              (values open close reference))))))))

(defun babel-noweb-result-reference (reference)
  (let* ((open (position #\( reference))
         (end (1- (length reference))))
    (when open
      (unless (and (> open 0) (char= (char reference end) #\)))
        (babel-signal-error :invalid-noweb-reference
                            "Malformed Babel noweb result reference" reference))
      (values (subseq reference 0 open)
              (subseq reference (1+ open) end)
              t))))

(defun babel-noweb-merge-reference-arguments (parameters arguments)
  (if (zerop (length arguments))
      parameters
      (babel-merge-variable-assignments
       parameters
       (mapcar #'babel-parse-assignment
               (babel-split-header-list arguments #\,)))))

(defun babel-noweb-resolve-result-reference (context name arguments)
  (let* ((document (babel-noweb-context-document context))
         (resolution (babel-noweb-context-resolution context))
         (node (gethash name (babel-resolution-context-names resolution)))
         (variables (and arguments
                         (mapcar #'babel-parse-assignment
                                 (babel-split-header-list arguments #\,)))))
    (unless node
      (babel-signal-error :unresolved-reference
                          "Babel noweb result reference was not found" name))
    (cond
      ((eq (node-type node) :src-block)
       (let ((parameters (resolve-babel-parameters document node)))
         (babel-resolve-source-block-result
          resolution node
          (babel-noweb-merge-reference-arguments parameters arguments))))
      ((eq (node-type node) :babel-call)
       (babel-resolve-call-result resolution node variables))
      (t
       (babel-signal-error :unsupported-reference
                           "Babel noweb result reference is not executable"
                           name)))))

(defun babel-noweb-resolve-reference (context reference depth)
  (multiple-value-bind (name arguments result-reference-p)
      (babel-noweb-result-reference reference)
    (if result-reference-p
        (progn
          (check-limit (document-limits (babel-noweb-context-document context))
                       :max-babel-expansion-depth (1+ depth))
          (let ((value
                  (babel-noweb-resolve-result-reference
                   context name arguments)))
            (if (stringp value) value (babel-noweb-format-result context value))))
        (let* ((named (gethash reference
                               (babel-noweb-context-source-blocks context)))
               (group (unless named
                        (babel-noweb-reference-group context reference))))
          (cond
            (named (babel-noweb-expand-source-block context named (1+ depth)))
            (group (babel-noweb-expand-reference-group
                    context group (1+ depth)))
            (t ""))))))

(defun babel-noweb-expand-reference-group (context blocks depth)
  (with-output-to-string (output)
    (let ((length 0)
          (previous nil))
      (loop for block in blocks
            do
        (when previous
          (let ((separator (or (babel-noweb-parameter
                                previous "NOWEB-SEP"
                                (babel-noweb-context-document context))
                               (string #\Newline))))
            (setf length
                  (babel-noweb-write context output length separator))))
        (setf length
              (babel-noweb-write
               context output length
               (babel-noweb-expand-source-block context block depth)))
        (setf previous block)))))

(defun babel-noweb-expand-source-block (context block depth)
  (let* ((document (babel-noweb-context-document context))
         (parameters (resolve-babel-parameters document block))
         (action (babel-noweb-mode-action parameters
                                          (babel-noweb-context-context context)))
         (body (babel-noweb-normalize-body
                (or (node-property block :value) ""))))
    (check-limit (document-limits document)
                 :max-babel-expansion-depth depth)
    (cond
      ((eq action :preserve) body)
      ((eq action :strip) (babel-noweb-strip-references context body))
      (t
       (let ((active (babel-noweb-context-active-blocks context)))
         (when (gethash block active)
           (babel-signal-error :circular-reference
                               "Babel noweb references are circular"
                               (or (node-property block :name)
                                   (node-property block :noweb-ref))))
         (setf (gethash block active) t)
         (unwind-protect
              (babel-noweb-expand-text context block body depth)
           (remhash block active)))))))

(defun babel-noweb-expand-text (context block text depth)
  (let* ((document (babel-noweb-context-document context))
         (prefix-enabled
          (not (string-equal (or (babel-noweb-parameter
                                  block "NOWEB-PREFIX" document)
                                 "yes")
                             "no")))
         (position 0))
    (with-output-to-string (output)
      (let ((length 0))
        (loop
         (multiple-value-bind (open close reference)
             (babel-noweb-delimiters text position)
           (unless open
             (return (babel-noweb-write-range
                      context output length text position (length text))))
           (setf length
                 (babel-noweb-write-range
                  context output length text position open))
           (let* ((prefix (and prefix-enabled
                               (babel-noweb-line-prefix text open)))
                  (replacement
                   (babel-noweb-resolve-reference context reference depth))
                  (replacement (if prefix
                                   (babel-noweb-prefix-later-lines
                                    context replacement prefix)
                                 replacement)))
             (setf length
                   (babel-noweb-write context output length replacement))
             (setf position (+ close 2)))))))))

(defun babel-noweb-strip-references (context text)
  (with-output-to-string (output)
    (let ((position 0)
          (length 0))
      (loop
        (multiple-value-bind (open close reference)
            (babel-noweb-delimiters text position)
          (declare (ignore reference))
          (unless open
            (babel-noweb-write-range
             context output length text position (length text))
            (return))
          (setf length
                (babel-noweb-write-range
                 context output length text position open))
          (setf position (+ close 2)))))))

(defun expand-babel-noweb (document src-block
                          &key (context nil context-supplied-p)
                            result-resolver parameters)
  "Expand SRC-BLOCK's noweb references in DOCUMENT for CONTEXT.

CONTEXT is :EVAL, :TANGLE, or :EXPORT. PARAMETERS, when supplied, is the
already-resolved header alist to use for SRC-BLOCK. Plain references insert
named source text; parenthesized references such as <<name()>> use
RESULT-RESOLVER, called with the executable block name and resolved variable
alist. The returned body has its final line ending removed. Source code is never
run here."
  (validate-babel-source-block document src-block)
  (unless context-supplied-p
    (setf context :eval))
  (unless (member context '(:eval :tangle :export) :test #'eq)
    (error 'type-error :datum context :expected-type '(member :eval :tangle :export)))
  (unless (or (null result-resolver) (functionp result-resolver))
    (error 'type-error :datum result-resolver :expected-type '(or null function)))
  (multiple-value-bind (source-blocks reference-groups)
      (babel-noweb-build-indexes document)
    (let* ((resolution (make-babel-resolution-context
                        :document document
                        :result-resolver result-resolver
                        :names (babel-build-name-index document)))
           (noweb (make-babel-noweb-context
                   :document document
                   :resolution resolution
                   :context context
                   :source-blocks source-blocks
                   :reference-groups reference-groups))
           (effective-parameters (or parameters
                                     (resolve-babel-parameters document src-block)))
           (body (babel-noweb-normalize-body
                  (or (node-property src-block :value) "")))
           (action (babel-noweb-mode-action effective-parameters context))
           (*babel-noweb-root-parameters*
             (and parameters (cons src-block parameters))))
      (case action
        (:preserve body)
        (:strip (babel-noweb-strip-references noweb body))
        (:expand
         (setf (gethash src-block (babel-noweb-context-active-blocks noweb)) t)
         (unwind-protect
              (babel-noweb-expand-text noweb src-block body 0)
           (remhash src-block (babel-noweb-context-active-blocks noweb))))))))

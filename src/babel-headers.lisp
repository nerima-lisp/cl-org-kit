;;;; src/babel-headers.lisp
(in-package #:org-kit)

(defparameter +babel-default-header-arguments+
  '(("SESSION" . "none")
    ("RESULTS" . "replace")
    ("EXPORTS" . "code")
    ("CACHE" . "no")
    ("NOWEB" . "no")
    ("HLINES" . "no")
    ("TANGLE" . "no")))

(defparameter +babel-results-exclusive-groups+
  '(("file" "list" "vector" "table" "scalar" "verbatim")
    ("raw" "html" "latex" "org" "code" "pp" "drawer" "link" "graphics")
    ("replace" "silent" "none" "discard" "append" "prepend")
    ("output" "value")))

(defparameter +babel-exports-exclusive-groups+
  '(("code" "results" "both" "none")))

(defun babel-whitespace-p (character)
  (or (char= character #\Space)
      (char= character #\Tab)
      (char= character #\Newline)
      (char= character #\Return)))

(defun babel-header-name-character-p (character)
  (or (char<= #\A character #\Z)
      (char<= #\a character #\z)
      (char<= #\0 character #\9)
      (char= character #\-)
      (char= character #\_)))

(defun babel-header-name-end (text start)
  (loop with position = (1+ start)
        while (and (< position (length text))
                   (babel-header-name-character-p (char text position)))
        do (incf position)
        finally (return position)))

(defun babel-header-argument-starts (text)
  (let ((starts '())
        (quoted-p nil)
        (escaped-p nil)
        (parenthesis-depth 0)
        (brace-depth 0)
        (bracket-depth 0))
    (loop for position from 0 below (length text)
          for character = (char text position)
          do (cond
               (quoted-p
                (cond (escaped-p (setf escaped-p nil))
                      ((char= character #\\) (setf escaped-p t))
                      ((char= character #\") (setf quoted-p nil))))
               ((char= character #\") (setf quoted-p t))
               ((char= character #\() (incf parenthesis-depth))
               ((char= character #\))
                (when (plusp parenthesis-depth) (decf parenthesis-depth)))
               ((char= character #\{) (incf brace-depth))
               ((char= character #\})
                (when (plusp brace-depth) (decf brace-depth)))
               ((char= character #\[) (incf bracket-depth))
               ((char= character #\])
                (when (plusp bracket-depth) (decf bracket-depth)))
               ((and (char= character #\:)
                     (zerop parenthesis-depth)
                     (zerop brace-depth)
                     (zerop bracket-depth)
                     (or (zerop position)
                         (babel-whitespace-p (char text (1- position))))
                     (< (1+ position) (length text))
                     (babel-header-name-character-p (char text (1+ position))))
                (push (cons position (babel-header-name-end text position)) starts))))
    (nreverse starts)))

(defun babel-parse-header-arguments (text)
  (when (and (stringp text) (plusp (length text)))
    (let ((starts (babel-header-argument-starts text)))
      (loop for (start . name-end) in starts
            for next-entry in (append (cdr starts) (list nil))
            for value-end = (if next-entry (car next-entry) (length text))
            for value = (string-trim '(#\Space #\Tab #\Newline #\Return)
                                     (subseq text name-end value-end))
            collect (cons (string-upcase (subseq text (1+ start) name-end))
                          (unless (zerop (length value)) value))))))

(defun babel-split-header-list (text delimiter)
  (when (stringp text)
    (let ((segments nil)
          (start 0)
          (quoted-p nil)
          (escaped-p nil)
          (parenthesis-depth 0)
          (brace-depth 0)
          (bracket-depth 0))
      (loop for position from 0 below (length text)
            for character = (char text position)
            do (cond
                 (quoted-p
                  (cond (escaped-p (setf escaped-p nil))
                        ((char= character #\\) (setf escaped-p t))
                        ((char= character #\") (setf quoted-p nil))))
                 ((char= character #\") (setf quoted-p t))
                 ((char= character #\() (incf parenthesis-depth))
                 ((char= character #\))
                  (when (plusp parenthesis-depth) (decf parenthesis-depth)))
                 ((char= character #\{) (incf brace-depth))
                 ((char= character #\})
                  (when (plusp brace-depth) (decf brace-depth)))
                 ((char= character #\[) (incf bracket-depth))
                 ((char= character #\])
                  (when (plusp bracket-depth) (decf bracket-depth)))
                 ((and (if (eq delimiter :whitespace)
                           (babel-whitespace-p character)
                           (char= character delimiter))
                       (zerop parenthesis-depth)
                       (zerop brace-depth)
                       (zerop bracket-depth))
                  (let ((segment (string-trim (list #\Space #\Tab #\Newline #\Return)
                                              (subseq text start position))))
                    (unless (zerop (length segment)) (push segment segments)))
                  (setf start (1+ position)))))
      (let ((segment (string-trim (list #\Space #\Tab #\Newline #\Return)
                                  (subseq text start))))
        (unless (zerop (length segment)) (push segment segments)))
      (nreverse segments))))

(defun babel-var-assignment-name (assignment)
  (let ((equals (position #\= assignment)))
    (when equals
      (let ((name (string-trim '(#\Space #\Tab)
                               (subseq assignment 0 equals))))
        (unless (zerop (length name)) name)))))

(defun babel-parameter-position (parameters name)
  (position name parameters :key #'car :test #'string=))

(defun babel-merge-exclusive-words (old-value new-value groups)
  (let ((words nil))
    (dolist (word (append (babel-split-header-list old-value :whitespace)
                          (babel-split-header-list new-value :whitespace)))
      (let ((group (find-if (lambda (values)
                              (member word values :test (function string=)))
                            groups)))
        (when group
          (setf words
                (remove-if (lambda (existing)
                             (member existing group :test (function string=)))
                           words)))
        (push word words)))
    (format nil "~{~A~^ ~}"
            (remove-duplicates words :test (function string=) :from-end t))))

(defun babel-merge-var-parameter (parameters value)
  (dolist (assignment (babel-split-header-list value #\,))
    (let ((name (babel-var-assignment-name assignment)))
      (setf parameters
            (append
             (if name
                 (remove-if (lambda (parameter)
                              (and (string= (car parameter) "VAR")
                                   (string= name
                                            (babel-var-assignment-name
                                             (cdr parameter)))))
                            parameters)
                 parameters)
             (list (cons "VAR" assignment))))))
  parameters)

(defun babel-merge-header-parameter (parameters name value)
  (cond
    ((and (string= name "VAR") value)
     (setf parameters (babel-merge-var-parameter parameters value)))
    ((string= name "VAR") parameters)
    ((and value (string= name "RESULTS"))
     (let* ((position (babel-parameter-position parameters name))
            (old-value (and position (cdr (nth position parameters))))
            (merged (babel-merge-exclusive-words
                     old-value value +babel-results-exclusive-groups+)))
       (if position
           (setf (cdr (nth position parameters)) merged)
           (setf parameters (append parameters (list (cons name merged)))))))
    ((and value (string= name "EXPORTS"))
     (let* ((position (babel-parameter-position parameters name))
            (old-value (and position (cdr (nth position parameters))))
            (merged (babel-merge-exclusive-words
                     old-value value +babel-exports-exclusive-groups+)))
       (if position
           (setf (cdr (nth position parameters)) merged)
           (setf parameters (append parameters (list (cons name merged)))))))
    (t
     (let ((position (babel-parameter-position parameters name)))
       (if position
           (setf (cdr (nth position parameters)) value)
            (setf parameters (append parameters (list (cons name value))))))))
  parameters)

(defun babel-merge-header-text (parameters text)
  (dolist (argument (babel-parse-header-arguments text) parameters)
    (setf parameters
          (babel-merge-header-parameter parameters (car argument) (cdr argument)))))

(defun babel-containing-heading (node)
  (loop for parent = (node-parent node) then (node-parent parent)
        while parent
        when (member (node-type parent) '(:headline :inlinetask) :test #'eq)
          return parent))

(defun babel-global-property-value (document name)
  (let ((found-p nil)
        (value nil)
        (plus-name (concatenate 'string name "+")))
    (dolist (entry (getf (document-settings document) :property-settings))
      (when (and (consp entry) (stringp (car entry)))
        (cond ((string-equal (car entry) name)
               (setf value (cdr entry)
                     found-p t))
              ((string-equal (car entry) plus-name)
               (setf value (if found-p
                               (format nil "~A ~A" value (cdr entry))
                               (cdr entry))
                     found-p t)))))
    (values value found-p)))

(defun babel-heading-property-chain (heading)
  (loop for current = heading then (node-parent current)
        while current
        when (member (node-type current) '(:headline :inlinetask) :test #'eq)
          collect current))

(defun babel-effective-property (document heading name)
  (let ((name (string-upcase name))
        (base nil)
        (base-found-p nil)
        (plus-values nil))
    (dolist (current (when heading (babel-heading-property-chain heading)))
      (multiple-value-bind (value found-p)
          (effective-heading-property-value current name)
        (when found-p
          (setf base value
                base-found-p t)
          (multiple-value-bind (plus plus-found-p)
              (effective-heading-property-value
               current (concatenate 'string name "+"))
            (when plus-found-p (push plus plus-values)))
          (return)))
      (multiple-value-bind (plus found-p)
          (effective-heading-property-value
           current (concatenate 'string name "+"))
        (when found-p (push plus plus-values))))
    (unless base-found-p
      (multiple-value-setq (base base-found-p)
        (babel-global-property-value document name)))
    (when base-found-p
      (format nil "~{~A~^ ~}" (cons base plus-values)))))

(defun validate-babel-source-block (document src-block)
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document))
  (unless (node-p src-block)
    (error 'type-error :datum src-block :expected-type 'node))
  (unless (eq (node-type src-block) :src-block)
    (error 'org-babel-error
           :message "Babel resolution requires a source block"
           :reason :invalid-target
           :context (bounded-text "target node is not a source block")))
  (unless (eq (node-document src-block) document)
    (error 'org-babel-error
           :message "Babel source block belongs to another document"
           :reason :document-mismatch
           :context (bounded-text "source block does not belong to the document")))
  src-block)

(defun resolve-babel-parameters (document src-block)
  "Resolve SRC-BLOCK's Babel header arguments in DOCUMENT.

Return an ordered alist with uppercase string keys and raw string or NIL values.
Repeated :var assignments remain separate entries, with later assignments to
the same variable replacing earlier ones. No source block is executed."
  (validate-babel-source-block document src-block)
  (let* ((language (node-property src-block :language ""))
         (heading (babel-containing-heading src-block))
         (parameters (copy-tree +babel-default-header-arguments+)))
    (setf parameters
          (babel-merge-header-text
           parameters (babel-effective-property document heading "header-args")))
    (setf parameters
          (babel-merge-header-text
           parameters
           (babel-effective-property
            document heading (format nil "header-args:~A" language))))
    (setf parameters
          (babel-merge-header-text parameters (node-property src-block :header)))
    (setf parameters
          (babel-merge-header-text parameters (node-property src-block :switches)))
    parameters))

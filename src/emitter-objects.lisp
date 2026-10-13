;;;; src/emitter-objects.lisp
;;;;
;;;; Canonical Org serialization for inline objects.
(in-package #:org-kit)

(declaim (ftype function prepare-object-parsing
                %try-parse-object
                %object-type-allowed-p
                object-match-type
                emitter-object-child-string))

(defun emitter-object-fail (node message &rest arguments)
  (error 'org-kit-error
         :message (apply #'format nil
                         (concatenate 'string
                                      "Cannot emit "
                                      (string-downcase
                                       (symbol-name (node-type node)))
                                      ": "
                                      message)
                         arguments)))

(defun emitter-object-required-string (node key)
  (multiple-value-bind (value present) (node-property node key)
    (unless (and present (stringp value))
      (emitter-object-fail node "property ~S must be a string" key))
    value))

(defun emitter-object-source-string (node key)
  (multiple-value-bind (value present) (node-property node key)
    (cond
      ((and present (stringp value)) value)
      (present (emitter-object-fail node "property ~S must be a string" key))
      ((node-text node)
       (let* ((source (node-text node))
              (post-blank (node-post-blank node))
              (end (- (length source) post-blank)))
         (unless (and (<= 0 end)
                      (every (lambda (character)
                               (member character '(#\Space #\Tab)))
                             (subseq source end)))
           (emitter-object-fail node "source text has an invalid post-blank span"))
         (subseq source 0 end)))
      (t (emitter-object-fail node "property ~S or source text is required" key)))))

(defun emitter-object-markup-source-string (node key delimiter)
  (multiple-value-bind (value present) (node-property node key)
    (cond
      (present
       (unless (stringp value)
         (emitter-object-fail node "property ~S must be a string" key))
       value)
      (t
       (let* ((source (emitter-object-source-string node key))
              (length (length source)))
         (unless (and (>= length 2)
                      (char= (char source 0) delimiter)
                      (char= (char source (1- length)) delimiter))
           (emitter-object-fail node
                                "source text does not contain its markup delimiters"))
         (subseq source 1 (1- length)))))))

(defun emitter-object-normalize-newlines (node value)
  (with-output-to-string (stream)
    (loop for position from 0 below (length value)
          for character = (char value position)
          do (cond
               ((char= character #\Return)
                (unless (and (< (1+ position) (length value))
                             (char= (char value (1+ position)) #\Newline))
                  (emitter-object-fail node "payload contains a bare carriage return"))
                (write-char #\Newline stream)
                (incf position))
               (t (write-char character stream))))))

(defun emitter-object-children-string (node)
  (with-output-to-string (stream)
    (dolist (child (node-children node))
      (unless (eq (node-type-category (node-type child)) :object)
        (emitter-object-fail node "object children must also be objects"))
      (write-string (emitter-object-child-string child) stream))))

(defun emitter-object-no-children (node)
  (when (node-children node)
    (emitter-object-fail node "this object form cannot contain child nodes")))

(defun emitter-object-prefix-p (text prefix)
  (and (<= (length prefix) (length text))
       (string= prefix text :end2 (length prefix))))

(defun emitter-object-element-line-p (line)
  (let* ((trimmed (string-left-trim '(#\Space #\Tab) line))
         (length (length trimmed)))
    (or
     (and (plusp length)
          (char= (char trimmed 0) #\#))
     (and (plusp length)
          (char= (char trimmed 0) #\*)
          (let ((marker-end
                  (position-if-not (lambda (character)
                                     (char= character #\*))
                                   trimmed)))
            (or (null marker-end)
                (member (char trimmed marker-end) '(#\Space #\Tab)))))
     (emitter-object-prefix-p trimmed "CLOCK:")
     (emitter-object-prefix-p trimmed "%%(")
     (and (plusp length) (char= (char trimmed 0) #\|))
     (and (>= length 3)
          (char= (char trimmed 0) #\:)
          (or (member (char trimmed 1) '(#\Space #\Tab))
              (char= (char trimmed (1- length)) #\:)))
     (and (>= length 5)
          (every (lambda (character) (char= character #\-))
                 (subseq trimmed 0 length)))
     (and (>= length 2)
          (member (char trimmed 0) '(#\- #\+))
          (member (char trimmed 1) '(#\Space #\Tab)))
     (and (plusp length)
          (digit-char-p (char trimmed 0))
          (let ((position
                  (loop for cursor from 1 below length
                        while (digit-char-p (char trimmed cursor))
                        finally (return cursor))))
            (and (< (1+ position) length)
                 (find (char trimmed position) ".)")
                 (member (char trimmed (1+ position)) '(#\Space #\Tab))))))))

(defun emitter-object-contains-element-line-p (text)
  (loop with start = 0
        for newline = (position #\Newline text :start start)
        for end = (or newline (length text))
        when (emitter-object-element-line-p (subseq text start end))
          do (return t)
        while newline
        do (setf start (1+ newline))
        finally (return nil)))

(defun emitter-object-recognized-inline-object-p (text)
  (let* ((document (make-document text))
         (end (length text))
         (*object-parse-range-start* 0))
    (prepare-object-parsing document)
    (loop for position from 0 below end
          for match = (%try-parse-object document position end :paragraph)
          thereis (and match
                       (%object-type-allowed-p (object-match-type match)
                                               :paragraph)))))

(defun emitter-object-plain-text (node)
  (multiple-value-bind (value present) (node-property node :value)
    (let ((text (cond
                  ((and present (stringp value)) value)
                  (present
                   (emitter-object-fail node "property :VALUE must be a string"))
                  ((node-text node) (node-text node))
                  (t
                   (emitter-object-fail node
                                         "detached plain text needs a :VALUE string")))))
      (when (node-children node)
        (emitter-object-fail node "plain text cannot contain child nodes"))
      (setf text (emitter-object-normalize-newlines node text))
      (when (and present
                 (or (emitter-object-contains-element-line-p text)
                     (emitter-object-recognized-inline-object-p text)))
        (emitter-object-fail node
                             "plain text contains syntax that Org would parse"))
      (when (zerop (length text))
        (emitter-object-fail node "empty plain text has no node representation"))
      text)))

(defun emitter-object-markup (node delimiter opaque-p)
  (let* ((contents
           (if opaque-p
               (progn
                 (emitter-object-no-children node)
                 (emitter-object-markup-source-string node :value delimiter))
               (emitter-object-children-string node)))
         (length (length contents)))
    (when (or (position delimiter contents)
              (and (not opaque-p)
                   (plusp length)
                   (or (markup-whitespace-character-p (char contents 0))
                       (markup-whitespace-character-p
                        (char contents (1- length))))))
      (emitter-object-fail node "contents cannot be represented by its delimiters"))
    (with-output-to-string (stream)
      (write-char delimiter stream)
      (write-string contents stream)
      (write-char delimiter stream))))

(defun emitter-object-balanced-delimiters-p (text open close)
  (loop with depth = 0
        with escaped = nil
        for character across text
        do (cond
             (escaped (setf escaped nil))
             ((char= character #\\) (setf escaped t))
             ((char= character open) (incf depth))
             ((char= character close)
              (decf depth)
              (when (minusp depth)
                (return-from emitter-object-balanced-delimiters-p nil))))
        finally (return (and (zerop depth) (not escaped)))))

(defun emitter-object-sub-super (node marker)
  (let* ((contents (emitter-object-children-string node))
         (bracketed (node-property node :use-brackets-p)))
    (unless (member bracketed '(nil t))
      (emitter-object-fail node ":USE-BRACKETS-P must be boolean"))
    (unless bracketed
      (unless (and (plusp (length contents))
                   (let ((start (if (find (char contents 0) "+-") 1 0)))
                     (and (< start (length contents))
                          (some #'alphanumericp (subseq contents start))
                          (every (lambda (character)
                                   (or (alphanumericp character)
                                       (find character ".,\\")))
                                 (subseq contents start))
                          (alphanumericp (char contents (1- (length contents)))))))
        (emitter-object-fail node
                             "unbracketed contents do not match Org subscript syntax")))
    (when (and bracketed
               (not (emitter-object-balanced-delimiters-p contents #\{ #\})))
      (emitter-object-fail node "braced contents are unbalanced"))
    (if bracketed
        (format nil "~C{~A}" marker contents)
        (format nil "~C~A" marker contents))))

(defun emitter-object-entity (node)
  (emitter-object-no-children node)
  (let* ((name (emitter-object-required-string node :name))
         (entry (assoc name *org-entity-table* :test #'string=))
         (brackets (node-property node :use-brackets-p)))
    (unless entry
      (emitter-object-fail node "unknown entity name ~S" name))
    (unless (member brackets '(nil t))
      (emitter-object-fail node ":USE-BRACKETS-P must be boolean"))
    (format nil "\\~A~A" name (if brackets "{}" ""))))

(defun emitter-object-export-snippet (node)
  (emitter-object-no-children node)
  (let ((backend (emitter-object-required-string node :back-end))
        (value (emitter-object-required-string node :value)))
    (unless (and (plusp (length backend))
                 (every (lambda (character)
                          (or (alphanumericp character) (char= character #\-)))
                        backend))
      (emitter-object-fail node "export backend is invalid"))
    (when (or (search "@@" value) (position #\Newline value)
              (position #\Return value))
      (emitter-object-fail node "export snippet value contains its closing marker"))
    (format nil "@@~A:~A@@" backend value)))

(defun emitter-object-footnote-reference (node)
  (let ((type (node-property node :type))
        (label (node-property node :label)))
    (unless (member type '(:standard :inline))
      (emitter-object-fail node "unknown footnote reference type ~S" type))
    (when (and label
               (not (and (stringp label)
                         (plusp (length label))
                         (every (lambda (character)
                                  (or (alphanumericp character)
                                      (find character "-_")))
                                label))))
      (emitter-object-fail node "footnote label is invalid"))
    (if (eq type :standard)
        (progn
          (emitter-object-no-children node)
          (unless label
            (emitter-object-fail node "standard footnotes need a label"))
          (format nil "[fn:~A]" label))
        (let ((contents (emitter-object-children-string node)))
          (unless (and (plusp (length contents))
                       (emitter-object-balanced-delimiters-p
                        contents #\[ #\]))
            (emitter-object-fail node "inline footnote contents are invalid"))
          (format nil "[fn:~A:~A]" (or label "") contents)))))

(defun emitter-object-header-value (node value key)
  (when value
    (unless (and (stringp value)
                 (plusp (length value))
                 (not (position #\Newline value))
                 (not (position #\Return value))
                 (not (position #\] value)))
      (emitter-object-fail node "property ~S is not a representable header" key)))
  value)

(defun emitter-object-inline-babel-call (node)
  (emitter-object-no-children node)
  (let ((call (emitter-object-required-string node :call))
        (inside (emitter-object-header-value
                 node (node-property node :inside-header) :inside-header))
        (arguments (node-property node :arguments))
        (end-header (emitter-object-header-value
                     node (node-property node :end-header) :end-header)))
    (unless (and (plusp (length call))
                 (every (lambda (character)
                          (not (or (member character
                                           '(#\Space #\Tab #\Newline #\Return))
                                   (find character "[]()"))))
                        call))
      (emitter-object-fail node "Babel call name is invalid"))
    (when (and arguments
               (not (and (stringp arguments)
                         (plusp (length arguments))
                         (emitter-object-balanced-delimiters-p
                          arguments #\( #\)))))
      (emitter-object-fail node "Babel call arguments are invalid"))
    (format nil "call_~A~A(~A)~A"
            call
            (if inside (format nil "[~A]" inside) "")
            (or arguments "")
            (if end-header (format nil "[~A]" end-header) ""))))

(defun emitter-object-inline-src-block (node)
  (emitter-object-no-children node)
  (let ((language (emitter-object-required-string node :language))
        (parameters (node-property node :parameters))
        (value (emitter-object-required-string node :value)))
    (unless (and (plusp (length language))
                 (not (position-if (lambda (character)
                                    (or (markup-whitespace-character-p character)
                                        (find character "[{")))
                                  language)))
      (emitter-object-fail node "inline source language is invalid"))
    (when (and parameters
               (not (and (stringp parameters)
                         (plusp (length parameters))
                         (not (position #\Newline parameters))
                         (not (position #\Return parameters))
                         (not (position #\] parameters)))))
      (emitter-object-fail node "inline source parameters are invalid"))
    (unless (emitter-object-balanced-delimiters-p value #\{ #\})
      (emitter-object-fail node "inline source body has unbalanced braces"))
    (format nil "src_~A~A{~A}"
            language
            (if parameters (format nil "[~A]" parameters) "")
            value)))

(defun emitter-object-latex-fragment (node)
  (emitter-object-no-children node)
  (let* ((value (emitter-object-required-string node :value))
         (length (length value)))
    (unless (and (plusp length)
                 (not (position #\Newline value))
                 (not (position #\Return value))
                 (or
                  (and (>= length 4)
                       (string= "$$" value :end2 2)
                       (string= "$$" value :start2 (- length 2)))
                  (and (>= length 3)
                       (char= (char value 0) #\$)
                       (char= (char value (1- length)) #\$)
                       (not (char= (char value 1) #\$)))
                  (and (>= length 4)
                       (emitter-object-prefix-p value "\\(")
                       (search "\\)" value :start2 2)
                       (= (search "\\)" value :start2 2) (- length 2)))
                  (and (>= length 4)
                       (emitter-object-prefix-p value "\\[")
                       (search "\\]" value :start2 2)
                       (= (search "\\]" value :start2 2) (- length 2)))
                  (and (>= length 2)
                       (char= (char value 0) #\\)
                       (alpha-char-p (char value 1)))))
      (emitter-object-fail node "LaTeX fragment syntax is invalid"))
    (when (and (char= (char value 0) #\\)
               (let* ((name-end
                        (loop for position from 1 below length
                              while (alpha-char-p (char value position))
                              finally (return position)))
                      (name (subseq value 1 name-end)))
                 (assoc name *org-entity-table* :test #'string=)))
      (emitter-object-fail node "LaTeX command would be parsed as an entity"))
    value))

(defun emitter-object-line-break (node)
  (emitter-object-no-children node)
  (format nil "\\\\~%"))

(defun emitter-object-link-escape-path (path)
  (with-output-to-string (stream)
    (loop for character across path
          do (when (find character "[]\\")
               (write-char #\\ stream))
             (write-char character stream))))

(defun emitter-object-link-known-type-p (type)
  (member type *org-link-object-types* :test #'string=))

(defun emitter-object-link-raw-path (node type path)
  (cond
    ((emitter-object-link-known-type-p type)
     (concatenate 'string type ":" path))
    ((string= type "custom-id")
     (concatenate 'string "#" path))
    ((string= type "coderef")
     (concatenate 'string "(" path ")"))
    ((string= type "fuzzy")
     (when (or (emitter-object-prefix-p path "#")
               (emitter-object-prefix-p path "/")
               (emitter-object-prefix-p path "./")
               (emitter-object-prefix-p path "../")
               (and (search ":" path)
                    (emitter-object-link-known-type-p
                     (string-downcase (subseq path 0 (search ":" path)))))
               (and (emitter-object-prefix-p path "(")
                    (char= (char path (1- (length path))) #\))))
         (emitter-object-fail node "fuzzy link path would change its link type"))
     path)
    (t
     (emitter-object-fail node "link type ~S has no Org representation" type))))

(defun emitter-object-link-plain-path-p (path)
  (and (plusp (length path))
       (not (position-if (lambda (character)
                           (or (markup-whitespace-character-p character)
                               (find character "[]<>")))
                         path))
       (not (find (char path (1- (length path)))
                  ".,;:!?\"'!@#$%^&*_+=|{}"))))

(defun emitter-object-radio-target-exists-p (path)
  (member path (symbol-value '*emitter-radio-targets*) :test #'string=))

(defun emitter-object-link (node)
  (let* ((type (emitter-object-required-string node :type))
         (path (emitter-object-required-string node :path))
         (format (node-property node :format))
         (children (node-children node)))
    (unless (and (plusp (length type))
                 (string= type (string-downcase type))
                 (plusp (length path)))
      (emitter-object-fail node "link type and path must be non-empty canonical strings"))
    (case format
      (:plain
       (when children
         (unless (and (string= type "radio")
                      (string= path (emitter-object-children-string node)))
           (emitter-object-fail node "plain links cannot contain a description")))
       (if (string= type "radio")
           (progn
             (unless (emitter-object-radio-target-exists-p path)
               (emitter-object-fail node "radio link has no matching target"))
             path)
           (progn
             (unless (and (emitter-object-link-known-type-p type)
                          (emitter-object-link-plain-path-p path))
               (emitter-object-fail node "plain link path is invalid"))
             (concatenate 'string type ":" path))))
      (:angle
       (emitter-object-no-children node)
       (unless (and (emitter-object-link-known-type-p type)
                    (not (position #\Newline path))
                    (not (position #\Return path))
                    (not (position #\> path)))
         (emitter-object-fail node "angle link fields are invalid"))
       (format nil "<~A:~A>" type path))
      (:bracket
       (let* ((raw (emitter-object-link-raw-path node type path))
              (escaped (emitter-object-link-escape-path raw)))
         (when (or (position #\Newline raw) (position #\Return raw))
           (emitter-object-fail node "bracket link path cannot contain newlines"))
         (if children
             (let ((description (emitter-object-children-string node)))
               (unless (and (plusp (length description))
                            (emitter-object-balanced-delimiters-p
                             description #\[ #\]))
                 (emitter-object-fail node "bracket link description is invalid"))
               (format nil "[[~A][~A]]" escaped description))
             (format nil "[[~A]]" escaped))))
      (otherwise
       (emitter-object-fail node "unknown link format ~S" format)))))

(defun emitter-object-macro (node)
  (emitter-object-no-children node)
  (let* ((key (emitter-object-required-string node :key))
         (arguments (node-property node :args)))
    (unless (and (plusp (length key))
                 (alpha-char-p (char key 0))
                 (string= key (string-downcase key))
                 (every (lambda (character)
                          (or (alphanumericp character) (find character "-_")))
                        key))
      (emitter-object-fail node "macro key is invalid"))
    (when (and arguments
               (not (and (listp arguments)
                         (every (lambda (argument)
                                  (and (stringp argument)
                                       (not (position #\Newline argument))
                                       (not (position #\Return argument))
                                       (not (position #\, argument))
                                       (not (search "}}}" argument))))
                                arguments))))
      (emitter-object-fail node "macro arguments are invalid"))
    (format nil "{{{~A~A}}}"
            key
            (if arguments
                (format nil "(~{~A~^,~})" arguments)
                ""))))

(defun emitter-object-radio-target (node)
  (let* ((value (emitter-object-required-string node :value))
         (contents (emitter-object-children-string node)))
    (unless (and (plusp (length value))
                 (string= value contents)
                 (not (find (char value 0) "<>"))
                 (not (member (char value 0)
                              '(#\Space #\Tab #\Newline #\Return)))
                 (not (find (char value (1- (length value))) "<>"))
                 (not (member (char value (1- (length value)))
                              '(#\Space #\Tab #\Newline #\Return)))
                 (not (position-if (lambda (character)
                                     (member character
                                             '(#\< #\> #\Newline #\Return)))
                                   value)))
      (emitter-object-fail node "radio target value and children are inconsistent"))
    (format nil "<<<~A>>>" contents)))

(defun emitter-object-statistics-cookie (node)
  (emitter-object-no-children node)
  (let ((value (emitter-object-required-string node :value)))
    (unless (and (eq (node-property node :type) nil)
                 (>= (length value) 4)
                 (char= (char value 0) #\[)
                 (char= (char value (1- (length value))) #\])
                 (let* ((body (subseq value 1 (1- (length value))))
                        (slash (position #\/ body))
                        (percent (position #\% body)))
                   (or (and slash
                            (= (count #\/ body) 1)
                            (every #'digit-char-p (subseq body 0 slash))
                            (every #'digit-char-p (subseq body (1+ slash))))
                       (and percent
                            (= percent (1- (length body)))
                            (= (count #\% body) 1)
                            (every #'digit-char-p (subseq body 0 percent))))))
      (emitter-object-fail node "statistics cookie value is invalid"))
    value))

(defun emitter-object-table-cell (node)
  (let ((value (node-property node :value))
        (contents (emitter-object-children-string node)))
    (unless (or (null value) (stringp value))
      (emitter-object-fail node "table cell :VALUE must be a string when present"))
    (when (or (position #\| contents)
              (position #\Newline contents)
              (position #\Return contents)
              (and (plusp (length contents))
                   (or (member (char contents 0) '(#\Space #\Tab))
                       (member (char contents (1- (length contents)))
                               '(#\Space #\Tab)))))
      (emitter-object-fail node "table cell contents cannot contain separators or edge spaces"))
    contents))

(defun emitter-object-target (node)
  (emitter-object-no-children node)
  (let ((value (emitter-object-required-string node :value)))
    (unless (and (plusp (length value))
                 (not (find (char value 0) "<>"))
                 (not (member (char value 0)
                              '(#\Space #\Tab #\Newline #\Return)))
                 (not (find (char value (1- (length value))) "<>"))
                 (not (member (char value (1- (length value)))
                              '(#\Space #\Tab #\Newline #\Return)))
                 (not (position-if (lambda (character)
                                     (member character
                                             '(#\< #\> #\Newline #\Return)))
                                   value)))
      (emitter-object-fail node "target value cannot be represented"))
    (format nil "<<~A>>" value)))

(defun emitter-object-timestamp-date (node year-key month-key day-key)
  (let ((year (node-property node year-key))
        (month (node-property node month-key))
        (day (node-property node day-key)))
    (unless (and (typep year '(integer 0 9999))
                 (typep month '(integer 0 99))
                 (typep day '(integer 0 99)))
      (emitter-object-fail node "timestamp date fields are incomplete or invalid"))
    (format nil "~4,'0D-~2,'0D-~2,'0D" year month day)))

(defun emitter-object-timestamp-time (node hour-key minute-key)
  (let ((hour (node-property node hour-key))
        (minute (node-property node minute-key)))
    (unless (or (and (null hour) (null minute))
                (and (typep hour '(integer 0 23))
                     (typep minute '(integer 0 59))))
      (emitter-object-fail node "timestamp time fields are incomplete or invalid"))
    (when hour
      (format nil "~2,'0D:~2,'0D" hour minute))))

(defun emitter-object-timestamp-adjustments (node)
  (with-output-to-string (stream)
    (let ((mode (node-property node :repeater-type))
          (amount (node-property node :repeater-value))
          (unit (node-property node :repeater-unit)))
      (if mode
          (let ((prefix (case mode
                          (:cumulate "+")
                          (:catch-up "++")
                          (:restart ".+")))
                (suffix (case unit
                          (:hour #\h)
                          (:day #\d)
                          (:week #\w)
                          (:month #\m)
                          (:year #\y))))
            (unless (and prefix suffix (typep amount '(integer 1)))
              (emitter-object-fail node "timestamp repeater fields are invalid"))
            (format stream " ~A~D~C" prefix amount suffix))
          (when (or amount unit)
            (emitter-object-fail node "timestamp repeater fields are incomplete"))))
    (let ((mode (node-property node :warning-type))
          (amount (node-property node :warning-value))
          (unit (node-property node :warning-unit)))
      (if mode
          (let ((prefix (case mode
                          (:all "-")
                          (:first "--")))
                (suffix (case unit
                          (:day #\d)
                          (:week #\w)
                          (:month #\m)
                          (:year #\y))))
            (unless (and prefix suffix (typep amount '(integer 1)))
              (emitter-object-fail node "timestamp warning fields are invalid"))
            (format stream " ~A~D~C" prefix amount suffix))
          (when (or amount unit)
            (emitter-object-fail node "timestamp warning fields are incomplete"))))))

(defun emitter-object-timestamp-diary (node)
  (let* ((source (emitter-object-source-string node :value))
         (raw (string-right-trim '(#\Space #\Tab #\Newline #\Return) source)))
    (emitter-object-no-children node)
    (when (or (position #\Newline raw) (position #\Return raw))
      (emitter-object-fail node "diary timestamp must be a single line"))
    (cond
      ((and (>= (length raw) 2)
            (member (char raw 0) '(#\< #\[))
            (member (char raw (1- (length raw))) '(#\> #\])))
       raw)
      ((emitter-object-prefix-p raw "%%(")
       (unless (and (null (node-property node :hour-start))
                    (null (node-property node :minute-start))
                    (null (node-property node :hour-end))
                    (null (node-property node :minute-end))
                    (null (node-property node :repeater-type))
                    (null (node-property node :warning-type)))
         (emitter-object-fail node
                              "bare diary expressions cannot carry timestamp suffix fields"))
       (format nil "<~A>" raw))
      (t
       (emitter-object-fail node "diary timestamp needs its original expression")))))

(defun emitter-object-timestamp (node)
  (emitter-object-no-children node)
  (let ((type (node-property node :type)))
    (when (eq type :diary)
      (return-from emitter-object-timestamp
        (emitter-object-timestamp-diary node)))
    (let* ((opening (case type
                      ((:active :active-range) #\<)
                      ((:inactive :inactive-range) #\[)))
           (closing (case type
                      ((:active :active-range) #\>)
                      ((:inactive :inactive-range) #\])))
           (range-p (member type '(:active-range :inactive-range)))
           (start-date (emitter-object-timestamp-date
                        node :year-start :month-start :day-start))
           (end-date (emitter-object-timestamp-date
                      node :year-end :month-end :day-end))
           (start-time (emitter-object-timestamp-time
                        node :hour-start :minute-start))
           (end-time (emitter-object-timestamp-time
                      node :hour-end :minute-end))
           (adjustments (emitter-object-timestamp-adjustments node)))
      (unless opening
        (emitter-object-fail node "unknown timestamp type ~S" type))
      (unless range-p
        (unless (and (string= start-date end-date)
                     (equal start-time end-time))
          (emitter-object-fail node
                               "point timestamp end fields must match its start")))
      (when (and range-p
                 (string= start-date end-date)
                 start-time end-time)
        (return-from emitter-object-timestamp
          (format nil "~C~A ~A-~A~A~C"
                  opening start-date start-time end-time adjustments closing)))
      (if range-p
          (format nil "~C~A~A~C--~C~A~A~C"
                  opening start-date
                  (concatenate 'string
                               (if start-time (format nil " ~A" start-time) "")
                               adjustments)
                  closing
                  opening end-date
                  (if end-time (format nil " ~A" end-time) "")
                  closing)
          (format nil "~C~A~A~C"
                  opening start-date
                  (concatenate 'string
                               (if start-time (format nil " ~A" start-time) "")
                               adjustments)
                  closing)))))

(defun emitter-object-citation-reference (node)
  (unless (and (node-parent node)
               (eq (node-type (node-parent node)) :citation))
    (emitter-object-fail node "citation references must be inside a citation"))
  (emitter-object-no-children node)
  (let ((key (emitter-object-required-string node :key))
        (prefix (node-property node :prefix))
        (suffix (node-property node :suffix))
        (locator (node-property node :locator)))
    (unless (and (plusp (length key))
                 (every (lambda (character)
                          (or (alphanumericp character)
                              (find character "-.:?!`'/*@+|(){}<>&_^$#%~")))
                        key))
      (emitter-object-fail node "citation key is invalid"))
    (when (or prefix locator)
      (emitter-object-fail node "citation prefix and locator fields are not retained"))
    (unless (or (null suffix)
                (and (listp suffix)
                     (every (lambda (value)
                              (and (stringp value)
                                   (not (position #\Newline value))
                                   (not (position #\Return value))
                                   (not (position #\; value))
                                   (not (position #\] value))))
                            suffix)))
      (emitter-object-fail node "citation suffix is invalid"))
    (format nil "@~A~A" key (if suffix (apply #'concatenate 'string suffix) ""))))

(defun emitter-object-citation (node)
  (let ((style (node-property node :style))
        (key (node-property node :key))
        (children (node-children node)))
    (unless (or (null style)
                (and (stringp style)
                     (plusp (length style))
                     (every (lambda (character)
                              (or (alphanumericp character)
                                  (find character "/_-")))
                            style)))
      (emitter-object-fail node "citation style is invalid"))
    (when key
      (emitter-object-fail node "citation key property is not retained"))
    (unless (and children
                 (every (lambda (child)
                          (eq (node-type child) :citation-reference))
                        children))
      (emitter-object-fail node "citations need citation-reference children"))
    (with-output-to-string (stream)
      (format stream "[cite~A: " (if style (format nil "/~A" style) ""))
      (loop for child in children
            for position from 0
            do (write-string (emitter-object-child-string child) stream)
               (when (< position (1- (length children)))
                 (write-char #\; stream)))
      (write-char #\] stream))))

(defun emitter-object-string (node)
  "Return canonical Org text for one object node, without element delimiters."
  (let* ((type (node-type node))
         (value
           (case type
             (:bold (emitter-object-markup node #\* nil))
             (:italic (emitter-object-markup node #\/ nil))
             (:underline (emitter-object-markup node #\_ nil))
             (:strike-through (emitter-object-markup node #\+ nil))
             (:code (emitter-object-markup node #\~ t))
             (:verbatim (emitter-object-markup node #\= t))
             (:citation (emitter-object-citation node))
             (:citation-reference (emitter-object-citation-reference node))
             (:entity (emitter-object-entity node))
             (:export-snippet (emitter-object-export-snippet node))
             (:footnote-reference (emitter-object-footnote-reference node))
             (:inline-babel-call (emitter-object-inline-babel-call node))
             (:inline-src-block (emitter-object-inline-src-block node))
             (:latex-fragment (emitter-object-latex-fragment node))
             (:line-break (emitter-object-line-break node))
             (:link (emitter-object-link node))
             (:macro (emitter-object-macro node))
             (:plain-text (emitter-object-plain-text node))
             (:radio-target (emitter-object-radio-target node))
             (:statistics-cookie (emitter-object-statistics-cookie node))
             (:subscript (emitter-object-sub-super node #\_))
             (:superscript (emitter-object-sub-super node #\^))
             (:table-cell (emitter-object-table-cell node))
             (:target (emitter-object-target node))
             (:timestamp (emitter-object-timestamp node))))
         (post-blank (node-post-blank node)))
    (if (eq type :table-cell)
        (progn
          (unless (zerop post-blank)
            (emitter-object-fail node "table cells cannot carry post-blank"))
          value)
        (concatenate 'string
                     value
                     (make-string post-blank :initial-element #\Space)))))

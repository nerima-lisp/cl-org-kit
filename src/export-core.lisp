;;;; src/export-core.lisp
(in-package #:org-kit)

(defvar *export-requested-backend* nil)
(defvar *export-current-output-check-kind* :native-write)
(defvar *export-current-output-check-index* 0)
(defvar *export-output-check-counts* nil)

(defun export-backend-p (backend)
  (export-backend-registered-p backend))

(defun export-bounded-source (source)
  (let* ((node (and (node-p source) source))
         (document (and node (node-document node)))
         (text (cond
                 (node (and document (node-begin node) (node-end node)
                            (document-text document)))
                 ((stringp source) source)))
         (start (if node (node-begin node) 0))
         (end (if node (node-end node) (and text (length text)))))
    (when text
      (let ((stop (min end (+ start *condition-text-limit*))))
        (with-output-to-string (output)
          (loop for index from start below stop
                for character = (char text index)
                for code = (char-code character)
                do (cond
                     ((char= character #\\) (write-string "\\\\" output))
                     ((char= character #\Newline) (write-string "\\n" output))
                     ((char= character #\Return) (write-string "\\r" output))
                     ((char= character #\Tab) (write-string "\\t" output))
                     ((or (< code 32) (<= 127 code 159))
                      (format output "\\x~2,'0X" code))
                     (t (write-char character output)))
                finally (when (< stop end)
                          (write-string "..." output))))))))

(defun export-source-position (source)
  (if (and (node-p source) (node-begin source))
      (node-begin source)
      0))

(defun export-node-text-position (node text &optional (offset 0))
  (let* ((document (and (node-p node) (node-document node)))
         (source (and document (document-text document)))
         (start (and (node-p node) (node-begin node)))
         (end (and (node-p node) (node-end node)))
         (match (and source start end
                     (search text source :start2 start :end2 end))))
    (if match (+ match offset) (export-source-position node))))

(defun export-error (backend reason source message)
  (error 'org-export-error
         :message message
         :backend (or *export-requested-backend* backend)
         :reason reason
         :source (export-bounded-source source)))

(defun export-unsupported-syntax (backend source position feature message)
  (error 'org-export-unsupported-syntax
         :message message
         :backend (or *export-requested-backend* backend)
         :source (export-bounded-source source)
         :position position
         :feature feature))

(defun export-control-character-p (character)
  (let ((code (char-code character)))
    (or (<= code #x1f) (<= #x7f code #x9f))))

(defun export-whitespace-p (character)
  (find character '(#\Space #\Tab #\Newline #\Return #\Page)))

(defun export-whitespace-character-p (character)
  (let ((code (char-code character)))
    (or (export-whitespace-p character)
        (= code #x0b)
        (member code '(#x85 #xa0 #x1680 #x2028 #x2029 #x202f #x205f #x3000))
        (<= #x2000 code #x200a))))

(defun export-ascii-alpha-p (character)
  (let ((code (char-code character)))
    (or (<= #x41 code #x5a) (<= #x61 code #x7a))))

(defun export-ascii-scheme-character-p (character)
  (or (export-ascii-alpha-p character)
      (let ((code (char-code character)))
        (or (<= #x30 code #x39)
            (find character '(#\+ #\. #\-))))))

(defun export-explicit-url-scheme-end (destination)
  (let ((colon (position #\: destination)))
    (when (and colon
               (plusp colon)
               (export-ascii-alpha-p (char destination 0))
               (loop for position from 1 below colon
                     always (export-ascii-scheme-character-p
                             (char destination position))))
      colon)))

(defun export-explicit-url-scheme (destination)
  (let ((end (export-explicit-url-scheme-end destination)))
    (and end (subseq destination 0 end))))

(defun export-allowed-url-scheme-p (destination end)
  (not (null
        (member-if (lambda (scheme)
                     (and (= (length scheme) end)
                          (string-equal destination scheme :end1 end)))
                   '("http" "https" "mailto")))))

(defun export-safe-url-p (raw-destination expanded-destination)
  "Return true when an expanded destination uses an allowed scheme or is relative."
  (and (notany #'export-control-character-p raw-destination)
       (notany #'export-control-character-p expanded-destination)
       (plusp (length expanded-destination))
       (not (export-whitespace-character-p (char expanded-destination 0)))
       (not (find #\\ expanded-destination))
       (not (and (>= (length expanded-destination) 2)
                 (string= "//" expanded-destination :end2 2)))
       (let ((scheme-end
               (export-explicit-url-scheme-end expanded-destination)))
         (or (null scheme-end)
             (export-allowed-url-scheme-p expanded-destination scheme-end)))))
(defun export-safe-relative-file-path-p (path)
  (and (stringp path)
       (plusp (length path))
       (not (char= (char path 0) #\/))
       (notany #'export-control-character-p path)
       (loop for character across path
             always (or (export-ascii-alpha-p character)
                        (find character "0123456789._/-")))
       (loop with start = 0
             for end = (position #\/ path :start start)
             for component = (subseq path start end)
             do (when (string= component "..")
                  (return nil))
                (unless end
                  (return t))
                (setf start (1+ end)))))
(defun export-image-path-p (path &optional
                                  (extensions '("png" "jpg" "jpeg" "gif"
                                                "svg" "webp")))
  (and (stringp path)
       (let ((extension (position #\. path :from-end t)))
         (and extension
              (loop for suffix in extensions
                    thereis (and (= (length suffix)
                                    (- (length path) (1+ extension)))
                                 (string-equal suffix path
                                               :start2 (1+ extension))))))))

(defun export-utf8-length (text backend)
  (loop with total = 0
        for character across text
        for code = (char-code character)
        for length = (cond ((<= code #x7f) 1)
                           ((<= code #x7ff) 2)
                           ((or (<= code #xd7ff)
                                (<= #xe000 code #xffff))
                            3)
                           ((<= #x10000 code #x10ffff) 4))
        do (unless length
             (export-error backend :encoding text
                           "Unable to measure UTF-8 export output"))
           (incf total length)
        finally (return total)))

(defun export-write-output (text limits output-type &key stream backend)
  "Apply output limits and return a string, UTF-8 octets, or the written stream."
  (let ((octet-output-p
          (case output-type
            (:string
             (when stream
               (export-error backend :invalid-output nil
                             "A stream is only valid with :STREAM output type"))
             nil)
            (:octets
             (when stream
               (export-error backend :invalid-output nil
                             "A stream is only valid with :STREAM output type"))
             t)
            (:stream
             (unless (and (streamp stream) (output-stream-p stream))
               (export-error backend :invalid-output nil
                             "Stream output requires an output stream"))
             (multiple-value-bind (character-p character-known-p)
                 (subtypep (stream-element-type stream) 'character)
               (cond
                 ((and character-known-p character-p) nil)
                 ((equal (stream-element-type stream) '(unsigned-byte 8)) t)
                 (t
                  (export-error backend :invalid-output nil
                                "Unsupported stream element type")))))
            (otherwise
             (export-error backend :invalid-output nil
                           "Unsupported export output type")))))
    (if octet-output-p
        (let ((octet-count (export-utf8-length text backend)))
          (check-limit limits :max-output-octets octet-count)
          (let ((octets (cl-codec-kit:string-to-octets text :encoding :utf-8)))
            (if (eq output-type :stream)
                (let ((*export-current-export-phase* :stream-write))
                  (write-sequence octets stream)
                  stream)
                octets)))
        (if (eq output-type :stream)
            (progn
              (export-utf8-length text backend)
              (let ((*export-current-export-phase* :stream-write))
                (write-string text stream))
              stream)
            text))))

(defun export-check-output-stage (text limits backend phase
                                  &key (base-offset 0) (kind :native-write)
                                    (actual-offset nil actual-offset-p))
  (declare (ignore backend))
  (unless (stringp text)
    (error 'type-error :datum text :expected-type 'string))
  (let* ((counts (or *export-output-check-counts* (make-hash-table)))
         (*export-output-check-counts* counts)
         (actual (if actual-offset-p actual-offset
                     (+ base-offset (length text))))
         (index (1+ (gethash kind counts 0)))
         (*export-current-export-phase* phase)
         (*export-current-output-check-kind* kind)
         (*export-current-output-check-index* index))
    (unless (and (integerp actual) (not (minusp actual)))
      (error 'type-error :datum actual :expected-type '(integer 0 *)))
    (setf (gethash kind counts) index)
    (check-limit limits :max-output-length actual))
  text)

(defun export-check-output-actual (actual limits backend phase
                                  &key (kind :native-write))
  (export-check-output-stage "" limits backend phase
                             :kind kind :actual-offset actual)
  actual)

(defun export-run-renderer-stage
    (renderer context fallback limits backend phase
     &key (base-offset 0) (kind :native-write))
  (let ((*export-current-export-phase* phase)
        (*export-current-output-offset* base-offset))
    (export-check-output-stage
     (export-check-render-string
      (if renderer (funcall renderer context) fallback))
     limits backend phase :base-offset base-offset :kind kind)))

(defun export-run-filter-stage
    (backend stage value context limits phase &key (base-offset 0))
  (let ((*export-current-export-phase* phase)
        (*export-current-output-offset* base-offset))
    (export-check-output-stage
     (export-apply-filter-stage backend stage value context)
     limits backend phase :base-offset base-offset :kind :filter-commit)))

(defun export-trim (string)
  (string-trim '(#\Space #\Tab #\Newline #\Return #\Page) string))

(defun export-split-words (string)
  (let ((words nil)
        (start nil))
    (loop for index from 0 to (length string)
          for boundary-p = (or (= index (length string))
                               (export-whitespace-p (char string index)))
          do (cond
               ((and boundary-p start)
                (push (subseq string start index) words)
                (setf start nil))
               ((and (not boundary-p) (null start))
                (setf start index))))
    (nreverse words)))

(defun export-keyword-records (document)
  (let ((records nil))
    (map-nodes
     (lambda (node)
       (when (eq (node-type node) :keyword)
         (push (list node
                     (string-upcase (or (node-property node :key) ""))
                     (or (node-property node :value) ""))
               records)))
     (document-root document))
    (nreverse records)))

(defun export-records-for-key (records key)
  (remove-if-not (lambda (record) (string= (second record) key)) records))

(defun export-single-keyword-value (records key backend)
  (let ((matches (export-records-for-key records key)))
    (when (> (length matches) 1)
      (export-unsupported-syntax
       backend (first (first matches))
       (export-source-position (first (first matches)))
                                 :duplicate-keyword
                                 "Duplicate export keyword is ambiguous"))
    (if matches
        (values (third (first matches)) t (first (first matches)))
        (values nil nil nil))))

(defun export-parse-tag-list (value source backend feature)
  (let ((tags (export-split-words value)))
    (dolist (tag tags)
      (unless (every (lambda (character)
                            (or (alphanumericp character)
                                (find character "-_@#%")))
                          tag)
        (export-unsupported-syntax backend source
                                   (export-source-position source) feature
                                   "Unsupported tag expression")))
    tags))

(defun export-invalid-option (source backend message)
  (export-unsupported-syntax backend source (export-source-position source)
                             :invalid-option message))

(defun export-option-specification (token source backend ancestry)
  (or (export-resolved-option-specification ancestry token)
      (export-unsupported-syntax backend source (export-source-position source)
                                 :unknown-option
                                 "Unknown export option")))

(defun export-option-boolean (value source backend)
  (cond ((string-equal value "t") t)
        ((string-equal value "nil") nil)
        (t (export-invalid-option source backend "Expected t or nil"))))

(defun export-option-integer (value source backend)
  (let* ((negative-p (and (plusp (length value))
                          (char= (char value 0) #\-)))
         (start (if (and (plusp (length value))
                         (find (char value 0) "+-"))
                    1
                    0))
         (limit (if negative-p (- most-negative-fixnum)
                    most-positive-fixnum)))
    (unless (and (< start (length value))
                 (loop for index from start below (length value)
                       always (digit-char-p (char value index) 10)))
      (export-invalid-option source backend "Expected an integer"))
    (let ((magnitude
            (loop with number = 0
                  for index from start below (length value)
                  for digit = (digit-char-p (char value index) 10)
                  do (when (> number (floor (- limit digit) 10))
                       (export-invalid-option
                        source backend "Option integer exceeds the supported range"))
                     (setf number (+ (* number 10) digit))
                  finally (return number))))
      (if negative-p (- magnitude) magnitude))))

(defun export-option-tokens (value source backend)
  (let ((tokens nil)
        (start nil)
        (depth 0)
        (quoted-p nil))
    (loop for index from 0 to (length value)
          for end-p = (= index (length value))
          for character = (unless end-p (char value index))
          do (cond
               (end-p
                (when (or quoted-p (/= depth 0))
                  (export-invalid-option source backend
                                         "Unclosed option value group"))
                (when start
                  (push (subseq value start index) tokens)))
               ((char= character #\")
                (unless start (setf start index))
                (setf quoted-p (not quoted-p)))
               (quoted-p nil)
               ((char= character #\()
                (unless start (setf start index))
                (incf depth))
               ((char= character #\))
                (decf depth)
                (when (minusp depth)
                  (export-invalid-option source backend
                                         "Unmatched option value group")))
               ((and (zerop depth) (export-whitespace-p character))
                (when start
                  (push (subseq value start index) tokens)
                  (setf start nil)))
               (t (unless start (setf start index)))))
    (nreverse tokens)))

(defun export-option-list (value source backend &key allow-exclusion-p)
  (unless (and (>= (length value) 2)
               (char= (char value 0) #\()
               (char= (char value (1- (length value))) #\)))
    (export-invalid-option source backend "Expected a parenthesized option list"))
  (let ((inner (subseq value 1 (1- (length value))))
        (items nil)
        (position 0)
        (first-quoted-p nil))
    (loop while (< position (length inner))
          do (if (export-whitespace-p (char inner position))
                 (incf position)
                 (let ((start position))
                   (if (char= (char inner position) #\")
                       (progn
                         (when (null items)
                           (setf first-quoted-p t))
                         (incf position)
                         (setf start position)
                         (loop while (not (char= (char inner position) #\"))
                               do (incf position))
                         (push (subseq inner start position) items)
                         (incf position)
                         (unless (or (= position (length inner))
                                     (export-whitespace-p (char inner position)))
                           (export-invalid-option source backend
                                                  "Malformed option list string")))
                       (progn
                         (when (null items)
                           (setf first-quoted-p nil))
                         (loop while (and (< position (length inner))
                                          (not (export-whitespace-p
                                                (char inner position))))
                               do (unless (or (alphanumericp (char inner position))
                                              (find (char inner position) "-_@#%+.:/"))
                                    (export-invalid-option
                                     source backend "Malformed option list item"))
                                  (incf position))
                         (push (subseq inner start position) items))))))
    (setf items (nreverse items))
    (if (and items (not first-quoted-p)
             (string-equal (first items) "not"))
        (if allow-exclusion-p
            (cons :exclude (rest items))
            (export-invalid-option source backend
                                   "Exclusion lists are unsupported here"))
        (cons :include items))))

(defun export-parse-option-value (parser value source backend)
  (let ((kind parser))
    (ecase kind
      (:boolean (export-option-boolean value source backend))
      (:integer (export-option-integer value source backend))
      (:boolean-integer
       (cond ((string-equal value "t") t)
             ((string-equal value "nil") nil)
             (t (export-option-integer value source backend))))
      (:toc (cond ((string-equal value "t") t)
                  ((string-equal value "nil") nil)
                  (t (export-option-integer value source backend))))
      (:creator-mode
       (cond ((string-equal value "t") t)
             ((string-equal value "nil") nil)
             ((string-equal value "comment") :comment)
             (t (export-invalid-option source backend
                                       "Expected t, nil, or comment"))))
      (:sub-superscript
       (cond ((string-equal value "t") t)
             ((string-equal value "nil") nil)
             ((string= value "{}") "{}")
             (t (export-invalid-option source backend
                                       "Expected t, nil, or {}"))))
      (:disabled-only
       (unless (string-equal value "nil")
         (export-invalid-option source backend
                                "This option only supports its fail-closed value"))
       nil)
      (:archive-mode
       (cond ((string-equal value "nil") nil)
             ((string-equal value "t") t)
             ((string-equal value "headline") :headline)
             (t (export-invalid-option source backend "Invalid arch option value"))))
      (:latex-mode
       (cond ((string-equal value "nil") nil)
             ((string-equal value "t") t)
             ((string-equal value "verbatim") :verbatim)
             (t (export-invalid-option source backend "Invalid tex option value"))))
      (:tag-mode
       (cond ((string-equal value "nil") nil)
             ((string-equal value "t") t)
             ((string-equal value "not-in-toc") :not-in-toc)
             (t (export-invalid-option source backend "Invalid tags option value"))))
      (:timestamp-range
       (let ((entry (assoc (string-downcase value)
                           '(("nil" . nil) ("t" . t)
                             ("active" . :active)
                             ("active-exclude-diary" . :active-exclude-diary)
                             ("inactive" . :inactive))
                           :test #'string=)))
         (if entry (cdr entry)
             (export-invalid-option source backend
                                    "Invalid timestamp range option value"))))
      ((:selection-list :drawer-list :tasks-mode)
       (cond ((string-equal value "t") t)
             ((string-equal value "nil") nil)
             ((and (eq kind :tasks-mode) (string-equal value "todo")) :todo)
             ((and (eq kind :tasks-mode) (string-equal value "done")) :done)
             (t (export-option-list value source backend
                                    :allow-exclusion-p
                                    (eq kind :drawer-list))))))))

(defun export-option-token-parts (token source backend)
  (if (and (> (length token) 2)
           (char= (char token 0) #\:)
           (char= (char token 1) #\:))
      (values ":" (subseq token 2))
      (let ((colon (position #\: token)))
        (unless (and colon (plusp colon))
          (export-invalid-option source backend "Malformed export option"))
        (values (string-downcase (subseq token 0 colon))
                (subseq token (1+ colon))))))

(defun export-parse-options (value source backend ancestry)
  (let ((options nil))
    (dolist (token (export-option-tokens value source backend))
      (multiple-value-bind (key option-value)
          (export-option-token-parts token source backend)
        (let* ((specification
                 (export-option-specification key source backend ancestry))
               (option-key (getf specification :key))
               (parser (getf specification :parser))
               (parsed-value
                 (if (functionp parser)
                     (funcall parser option-value source backend)
                     (export-parse-option-value parser option-value source backend)))
               (existing (assoc option-key options)))
          (if existing
              (setf (cdr existing) parsed-value)
              (push (cons option-key parsed-value) options)))))
    (nreverse options)))

(defun export-macro-definition (record backend)
  (destructuring-bind (node key value) record
    (declare (ignore key))
    (let* ((value (export-trim value))
           (separator (position-if #'export-whitespace-p value))
           (name (if separator (subseq value 0 separator) value))
           (template (if separator (export-trim (subseq value separator)) ""))
           (function-p (find #\( name)))
      (unless (and (plusp (length name))
                   (every (lambda (character)
                            (or (alphanumericp character)
                                (find character "-_()")))
                          name))
        (export-unsupported-syntax backend node (export-source-position node)
                                   :invalid-macro
                                   "Invalid macro definition"))
      (values (string-downcase (subseq name 0 (or (position #\( name)
                                                   (length name))))
              template function-p node))))

(defun export-build-macro-table (records backend)
  (let ((macros (make-hash-table :test #'equal)))
    (dolist (record (export-records-for-key records "MACRO"))
      (multiple-value-bind (name template function-p source)
          (export-macro-definition record backend)
        (when (gethash name macros)
          (export-unsupported-syntax backend source
                                     (export-source-position source) :duplicate-macro
                                     "Duplicate macro definition"))
        (setf (gethash name macros)
              (list :template template :function-p function-p :source source))))
    macros))

(defun export-document-settings (document backend context filter-context)
  (let* ((records (export-keyword-records document))
         (context (or context (make-export-context)))
         (metadata nil)
         (options nil)
         (select-tags '("export"))
         (select-present-p nil)
         (exclude-tags '("noexport")))
    (unless (export-context-p context)
      (error 'type-error :datum context :expected-type 'export-context))
    (dolist (specification '(("TITLE" . :title) ("AUTHOR" . :author)
                             ("DATE" . :date) ("LANGUAGE" . :language)))
      (multiple-value-bind (value present-p source)
          (export-single-keyword-value records (car specification) backend)
        (declare (ignore source))
        (when present-p
          (setf (getf metadata (cdr specification)) value))))
    (let* ((ancestry
             (if filter-context
                 (%export-render-context-ancestry filter-context)
                 (nth-value 1 (export-registry-snapshot backend))))
           (parser-backend
             (if filter-context
                 (%export-render-context-backend filter-context)
                 backend)))
      (dolist (record (export-records-for-key records "OPTIONS"))
        (dolist (option
                 (export-parse-options (third record) (first record)
                                       parser-backend ancestry))
          (let ((existing (assoc (car option) options)))
            (if existing
                (setf (cdr existing) (cdr option))
                (setf options (append options (list option))))))))
    (multiple-value-bind (value present-p source)
        (export-single-keyword-value records "SELECT_TAGS" backend)
      (when present-p
        (setf select-present-p t
              select-tags (export-parse-tag-list value source backend
                                                 :select-tag-expression))))
    (multiple-value-bind (value present-p source)
        (export-single-keyword-value records "EXCLUDE_TAGS" backend)
      (when present-p
        (setf exclude-tags (export-parse-tag-list value source backend
                                                  :exclude-tag-expression))))
    (setf (getf metadata :title)
          (or (export-context-title context) (getf metadata :title))
          (getf metadata :author)
          (or (export-context-author context) (getf metadata :author))
          (getf metadata :date)
          (or (export-context-date context) (getf metadata :date))
          (getf metadata :language)
          (or (export-context-language context) (getf metadata :language) "en")
          (getf metadata :time-context) (export-context-time-context context))
    (let* ((select-active-p
             (and select-tags
                  (or select-present-p
                      (export-document-has-tag-p document select-tags))))
           (macros (export-build-macro-table records backend)))
      (when filter-context
        (setf (%export-render-context-metadata filter-context) metadata
              (%export-render-context-options filter-context) options
              (%export-render-context-limits filter-context)
              (document-limits document)
              (%export-render-context-extensions filter-context)
              (export-context-extensions context))
        (let ((preparation
                (%export-render-context-preparation filter-context)))
          (when (export-preparation-p preparation)
            (setf (export-preparation-document preparation) document
                  (export-preparation-backend preparation) backend
                  (export-preparation-metadata preparation) metadata
                  (export-preparation-options preparation) options)))
        (progn
          (setf options
                (export-apply-filter-stage
                 (%export-render-context-backend filter-context)
                 :options options filter-context
                 :validator
                 (lambda (filtered-options)
                   (export-valid-explicit-options-alist-p
                    filtered-options
                    (%export-render-context-ancestry filter-context)))
                 :expected-type '(satisfies export-explicit-options-alist-p))))
        (setf (%export-render-context-options filter-context) options)
        (let ((preparation
                (%export-render-context-preparation filter-context)))
          (when (export-preparation-p preparation)
            (setf (export-preparation-options preparation) options))))
      (values metadata options select-active-p
              select-tags exclude-tags macros))))

(defun export-macro-parameter-index (template position source backend)
  (let ((digit-position (1+ position)))
    (when (and (< digit-position (length template))
               (digit-char-p (char template digit-position)))
      (let ((digit (char template digit-position)))
        (unless (and (member digit '(#\1 #\2))
                     (or (= (1+ digit-position) (length template))
                         (not (digit-char-p
                               (char template (1+ digit-position))))))
          (export-unsupported-syntax
           backend source (export-node-text-position source template position)
           :unsupported-macro-parameter
           "Only $1 and $2 macro parameters are supported"))
        (if (char= digit #\1) 1 2)))))

(defun export-macro-expanded-length (template arguments source backend)
  (loop with position = 0
        with total = 0
        while (< position (length template))
        do (let ((argument-index
                   (and (char= (char template position) #\$)
                        (export-macro-parameter-index
                         template position source backend))))
             (if argument-index
                 (progn
                   (incf total
                         (length (or (nth (1- argument-index) arguments) "")))
                   (incf position 2))
                 (progn
                   (incf total)
                   (incf position))))
        finally (return total)))

(defun export-substitute-macro (template arguments source backend)
  (with-output-to-string (output)
    (loop with position = 0
          while (< position (length template))
          do (let ((argument-index
                     (and (char= (char template position) #\$)
                          (export-macro-parameter-index
                           template position source backend))))
               (if argument-index
                   (progn
                     (write-string
                      (or (nth (1- argument-index) arguments) "") output)
                     (incf position 2))
                   (progn
                     (write-char (char template position) output)
                     (incf position)))))))

(defun export-include-path (value source backend)
  (let ((value (export-trim value)))
    (unless (and (> (length value) 2)
                 (char= (char value 0) #\")
                 (char= (char value (1- (length value))) #\")
                 (not (find #\" value :start 1 :end (1- (length value))))
                 (not (some #'export-control-character-p value)))
      (export-unsupported-syntax backend source (export-source-position source)
                                 :include-form
                                 "Only a quoted literal include path is supported"))
    (subseq value 1 (1- (length value)))))

(defun export-copy-source-name (source-name)
  (unless (or (null source-name) (stringp source-name))
    (error 'type-error :datum source-name :expected-type '(or null string)))
  (if source-name (copy-seq source-name) nil))

(defun export-src-export-mode (node backend)
  (let ((tokens (export-split-words (or (node-property node :switches) "")))
        (mode nil)
        (mode-seen-p nil))
    (loop while tokens
          for token = (pop tokens)
          do (cond
               ((string-equal token ":exports")
                (when mode-seen-p
                  (export-unsupported-syntax
                   backend node (export-source-position node) :src-header-argument
                   "Duplicate :exports source block argument"))
                (unless tokens
                  (export-unsupported-syntax
                   backend node (export-source-position node) :src-header-argument
                   "Missing :exports source block value"))
                (let ((value (string-downcase (pop tokens))))
                  (setf mode
                        (cond ((string= value "code") :code)
                              ((string= value "results") :results)
                              ((string= value "both") :both)
                              ((string= value "none") :none)
                              (t
                               (export-unsupported-syntax
                                backend node (export-source-position node)
                                :src-header-argument
                                "Unsupported :exports mode")))
                        mode-seen-p t)))
               ((and (> (length token) 1)
                     (char= (char token 0) #\:))
                (when (and tokens
                           (not (and (> (length (first tokens)) 1)
                                     (char= (char (first tokens) 0) #\:))))
                  (pop tokens)))))
    (if mode-seen-p mode :code)))

(defun export-results-adjacent-p (source-node results-node)
  (let* ((document (node-document source-node))
         (text (document-text document))
         (start (node-end source-node))
         (end (and results-node (node-begin results-node))))
    (and results-node
         (node-property results-node :RESULTS)
         (<= 0 start end (length text))
         (every #'export-whitespace-character-p (subseq text start end)))))

(defun export-tag-intersection-p (tags selected-tags)
  (some (lambda (tag) (member tag selected-tags :test #'string=)) tags))

(defun export-document-has-tag-p (document selected-tags)
  (let ((found-p nil))
    (map-nodes
     (lambda (node)
       (when (and (member (node-type node) '(:headline :inlinetask) :test #'eq)
                  (export-tag-intersection-p
                   (effective-heading-tags document node) selected-tags))
         (setf found-p t)))
     (document-root document))
    found-p))

(defun export-node-count (root)
  (let ((count 0))
    (map-nodes (lambda (node) (declare (ignore node)) (incf count)) root)
    count))
(defun export-explicit-options-alist-p (options)
  (and (export-proper-list-p options)
       (every (lambda (entry)
                (and (consp entry) (keywordp (car entry))))
              options)
       (= (length options)
          (length (remove-duplicates options :key #'car :test #'eq)))))

(defun export-valid-explicit-options-alist-p (options ancestry)
  (and (export-explicit-options-alist-p options)
       (let ((specifications (export-resolved-option-specifications ancestry)))
         (every (lambda (entry)
                  (find (car entry) specifications
                        :key (lambda (specification)
                               (getf specification :key))
                        :test #'eq))
                options))))

(defun export-acyclic-view-p (view)
  (and (%export-view-p view)
       (let ((active (make-hash-table :test #'eq)))
         (labels ((visit (current)
                    (and (%export-view-p current)
                         (not (gethash current active))
                         (let ((children (%export-view-children current)))
                           (and (export-proper-list-p children)
                                (progn
                                  (setf (gethash current active) t)
                                  (prog1 (every #'visit children)
                                    (remhash current active))))))))
           (visit view)))))

(deftype acyclic-export-view ()
  '(satisfies export-acyclic-view-p))

(defun export-validate-filtered-root-views (views limits)
  (unless (export-proper-list-p views)
    (error 'type-error :datum views :expected-type 'list))
  (let ((active (make-hash-table :test #'eq))
        (count 0))
    (labels ((visit (view depth)
               (unless (%export-view-p view)
                 (error 'type-error :datum view :expected-type 'export-view))
               (when (gethash view active)
                 (error 'type-error :datum view
                        :expected-type 'acyclic-export-view))
               (let ((node (%export-view-node view))
                     (children (%export-view-children view))
                     (source-document
                       (%export-view-source-document view)))
                 (unless (node-p node)
                   (error 'type-error :datum node :expected-type 'node))
                 (unless (export-proper-list-p children)
                   (error 'type-error :datum children :expected-type 'list))
                 (unless (or (null source-document)
                             (document-p source-document))
                   (error 'type-error :datum source-document
                          :expected-type '(or null document)))
                 (check-limit limits :max-element-depth depth)
                 (incf count)
                 (check-limit limits :max-export-expanded-nodes count)
                 (setf (gethash view active) t)
                 (unwind-protect
                     (dolist (child children)
                       (visit child (1+ depth)))
                   (remhash view active)))))
      (dolist (view views)
        (visit view 1)))
    t))

(defun export-limits-with-node-budget (limits node-budget)
  (let ((arguments nil))
    (dolist (slot (sb-mop:class-slots (find-class 'limits)))
      (let* ((slot-name (sb-mop:slot-definition-name slot))
             (name (intern (string-upcase (symbol-name slot-name)) :keyword))
             (accessor (find-symbol
                        (concatenate 'string "%LIMITS-"
                                     (string-upcase (symbol-name slot-name)))
                        (find-package "ORG-KIT")))
             (value (if (eq name :max-node-count)
                        node-budget
                        (funcall (symbol-function accessor) limits))))
        (push (cons name value) arguments)))
    (apply #'make-limits
           (mapcan (lambda (argument)
                     (list (car argument) (cdr argument)))
                   (nreverse arguments)))))

(defun export-global-keyword-p (key)
  (and (not (member key '("INCLUDE" "RESULTS" "NAME" "CAPTION" "HEADER"
                           "LATEX" "ATTR_LATEX")
                         :test #'string=))
       (or (member key '("TITLE" "AUTHOR" "DATE" "LANGUAGE" "OPTIONS"
                         "SELECT_TAGS" "EXCLUDE_TAGS" "MACRO" "ATTR_HTML"
                         "ARCHIVE" "COLUMNS" "PRIORITIES" "CONSTANTS"
                         "EXPORT_FILE_NAME" "DESCRIPTION" "KEYWORDS"
                         "SUBTITLE" "EMAIL" "CREATOR" "CITE_EXPORT"
                         "BIBLIOGRAPHY" "PRINT_BIBLIOGRAPHY"
                         "LATEX_CLASS" "LATEX_CLASS_OPTIONS" "LATEX_HEADER"
                         "LATEX_HEADER_EXTRA")
                       :test #'string=)
           (reader-heading-setting-kind key))))

(defun export-validate-keywords (records backend &key fragment-p ancestry)
  (let ((accepted
          (export-resolved-string-set
           (or ancestry (export-backend-ancestry backend))
           #'%export-backend-descriptor-accepted-keywords)))
  (dolist (record records)
    (let ((key (second record))
          (source (first record)))
      (cond
        ((or (member key accepted :test #'string-equal)
             (reader-heading-setting-kind key))
         (when (and fragment-p (export-global-keyword-p key))
           (export-unsupported-syntax
            backend source (export-source-position source) :fragment-keyword-scope
                                    "Global export keywords in fragments are unsupported")))
        (t
         (export-unsupported-syntax backend source (export-source-position source)
                                    :unknown-keyword
                                    "Unknown export keyword")))))))

(defun export-option-selects-name-p (value name)
  (if (eq (first value) :exclude)
      (not (member name (rest value) :test #'string-equal))
      (member name (rest value) :test #'string-equal)))

(defun export-option-pruned-p (node options)
  (let* ((type (node-type node))
         (properties (assoc :properties options))
         (drawers (assoc :drawers options)))
    (or (and (eq type :clock) (assoc :clock options)
             (not (cdr (assoc :clock options))))
        (and (eq type :planning) (assoc :planning options)
             (not (cdr (assoc :planning options))))
        (and (eq type :statistics-cookie) (assoc :statistics-cookies options)
             (not (cdr (assoc :statistics-cookies options))))
        (and (eq type :inlinetask) (assoc :inline-tasks options)
             (not (cdr (assoc :inline-tasks options))))
        (and (member type '(:latex-environment :latex-fragment) :test #'eq)
             (assoc :latex options)
             (not (cdr (assoc :latex options))))
        (and (member type '(:footnote-reference :footnote-definition) :test #'eq)
             (assoc :footnotes options)
             (not (cdr (assoc :footnotes options))))
        (and (eq type :drawer) drawers
             (not (let ((value (cdr drawers)))
                    (or (eq value t)
                        (and (consp value)
                             (export-option-selects-name-p
                              value (node-property node :drawer-name)))))))
        (and (eq type :property-drawer) properties
             (not (let ((value (cdr properties)))
                    (or (eq value t)
                        (and (consp value)
                             (some (lambda (child)
                                     (member (node-property child :key)
                                             (rest value)
                                             :test #'string-equal))
                                   (node-children node))))))))))

(defun export-heading-pruned-p (node options)
  (let ((archive (assoc :arch options))
        (tasks (assoc :tasks options))
        (todo-keyword (node-property node :todo-keyword))
        (todo-type (node-property node :todo-type)))
    (or (and archive
             (node-property node :archivedp)
             (null (cdr archive)))
        (and tasks todo-keyword
             (let ((selection (cdr tasks)))
               (cond ((null selection) t)
                     ((eq selection :todo) (eq todo-type :done))
                     ((eq selection :done) (eq todo-type :todo))
                     ((consp selection)
                      (not (export-option-selects-name-p selection todo-keyword)))
                     (t nil)))))))

(defun export-prepare (document backend &key include-resolver allow-raw-html allow-raw-latex context filter-context)
  "Prepare a private export view without serializing or modifying DOCUMENT."
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document))
  (unless (export-backend-p backend)
    (export-error backend :unknown-backend nil "Unknown export backend"))
  (unless (or (null include-resolver) (functionp include-resolver))
    (error 'type-error :datum include-resolver :expected-type '(or null function)))
  (progn
    (unless (member allow-raw-html '(nil t))
      (error 'type-error :datum allow-raw-html :expected-type 'boolean))
    (unless (member allow-raw-latex '(nil t))
      (error 'type-error :datum allow-raw-latex :expected-type 'boolean)))
  (let* ((root (document-root document))
         (ancestry (if filter-context
                       (%export-render-context-ancestry filter-context)
                       (nth-value 1 (export-registry-snapshot backend))))
         (limits (document-limits document)))
    (unless root
      (error 'type-error :datum root :expected-type 'node))
    (let ((expanded-node-count (export-node-count root))
          (expanded-character-count 0))
      (check-limit limits :max-export-expanded-nodes expanded-node-count)
        (multiple-value-bind (metadata options select-active-p select-tags exclude-tags
                              macros)
          (export-document-settings document backend context filter-context)
        (labels
            ((account-tree (tree-root)
               (map-nodes
                (lambda (node)
                  (declare (ignore node))
                  (let ((actual (1+ expanded-node-count)))
                    (check-limit limits :max-export-expanded-nodes actual)
                    (setf expanded-node-count actual)))
                tree-root))
             (account-expanded-length (length)
               (let ((actual (+ expanded-character-count length)))
                 (check-limit limits :max-export-expanded-characters actual)
                 (setf expanded-character-count actual)))
             (account-expanded-text (text)
               (account-expanded-length (length text)))
             (parse-fragment (text source-name selected-scope depth element-depth
                                   active-includes active-macros)
               (let* ((remaining-nodes
                        (- (limit-value limits :max-export-expanded-nodes)
                           expanded-node-count))
                      (source-node-limit (limit-value limits :max-node-count))
                      (export-node-limit-p (<= remaining-nodes source-node-limit))
                      (fragment-limits
                        (export-limits-with-node-budget
                         limits (min remaining-nodes source-node-limit)))
                      (fragment
                        (handler-case
                            (parse-org text :source-name source-name
                                            :limits fragment-limits)
                          (org-resource-limit-error (condition)
                            (if (and export-node-limit-p
                                     (eq (org-resource-limit-error-limit-name condition)
                                         :max-node-count))
                                (error 'org-resource-limit-error
                                       :message "Resource limit exceeded"
                                       :limit-name :max-export-expanded-nodes
                                       :limit (limit-value
                                               limits :max-export-expanded-nodes)
                                       :actual (+ expanded-node-count
                                                  (org-resource-limit-error-actual
                                                   condition)))
                                (error condition)))))
                      (fragment-root (document-root fragment)))
                 (account-tree fragment-root)
                 (prepare-siblings (node-children fragment-root) fragment
                                   selected-scope depth element-depth
                                   active-includes active-macros t)))
             (prepare-include (node source-document selected-scope depth element-depth
                                   active-includes active-macros)
               (let* ((path (export-include-path
                             (node-property node :value) node backend))
                      (next-depth (1+ depth)))
                 (when (member path active-includes :test #'string=)
                   (export-error backend :include-cycle node
                                 "Recursive include path"))
                 (unless include-resolver
                   (export-error backend :missing-include-resolver node
                                 "Include requires an explicit resolver"))
                 (export-check-expansion-depth next-depth limits)
                 (let* ((source-name (document-source-name source-document))
                        (text (funcall include-resolver
                                       (copy-seq path)
                                       (export-copy-source-name source-name))))
                   (unless (stringp text)
                     (export-error backend :invalid-include-result node
                                   "Include resolver must return a string"))
                   (account-expanded-text text)
                   (parse-fragment text path selected-scope next-depth element-depth
                                   (cons path active-includes) active-macros))))
             (prepare-macro (node source-document selected-scope depth element-depth
                                 active-includes active-macros)
               (let* ((key (string-downcase (node-property node :key)))
                      (definition (gethash key macros))
                      (definition-source (getf definition :source))
                      (next-depth (1+ depth)))
                 (unless definition
                   (export-unsupported-syntax
                    backend node (export-source-position node) :unknown-macro
                                              "Unknown macro"))
                 (when (getf definition :function-p)
                   (export-unsupported-syntax
                    backend node (export-source-position node) :function-macro
                                              "Function macros are unsupported"))
                 (when (member key active-macros :test #'string=)
                   (export-unsupported-syntax
                    backend node (export-source-position node) :recursive-macro
                                              "Recursive macro expansion"))
                 (export-check-expansion-depth next-depth limits)
                 (let* ((arguments (node-property node :args))
                        (template (getf definition :template))
                        (expanded-length
                          (export-macro-expanded-length
                           template arguments definition-source backend)))
                   (account-expanded-length expanded-length)
                   (let* ((text (export-substitute-macro template arguments
                                                         definition-source backend))
                          (fragment-views
                            (parse-fragment
                             text
                             (document-source-name source-document)
                             selected-scope next-depth (1+ element-depth)
                             active-includes
                             (cons key active-macros))))
                     (values
                      (list (%make-export-view
                             :node node :children fragment-views
                             :payload (list :expanded-text text)
                             :source-document source-document))
                      nil)))))
             (prepare-title-macros (node source-document selected-scope depth
                                         element-depth active-includes active-macros)
               (loop for segment in (node-property node :title)
                     when (and (node-p segment)
                               (eq (node-type segment) :macro))
                       when (eq :default
                                (export-resolved-node-policy
                                 ancestry (node-type segment)))
                       collect
                       (progn
                         (check-limit limits :max-element-depth element-depth)
                         (multiple-value-bind (views descendant-p)
                             (prepare-macro segment source-document selected-scope
                                            depth element-depth active-includes
                                            active-macros)
                           (declare (ignore descendant-p))
                           (cons segment views)))))
             (prepare-node (node source-document selected-scope depth element-depth
                                active-includes active-macros fragment-p)
               (check-limit limits :max-element-depth element-depth)
               (let* ((type (node-type node))
                      (policy (export-resolved-node-policy ancestry type)))
                 (cond
                   ((export-option-pruned-p node options)
                    (values nil nil))
                   ((and (member type '(:headline :inlinetask) :test #'eq)
                         (export-heading-pruned-p node options))
                    (values nil nil))
                   ((member type '(:headline :inlinetask) :test #'eq)
                    (let* ((tags (effective-heading-tags source-document node))
                           (excluded-p (export-tag-intersection-p tags exclude-tags))
                           (archive-headline-only-p
                             (and (node-property node :archivedp)
                                  (eq (cdr (assoc :arch options)) :headline))))
                      (if excluded-p
                          (values nil nil)
                          (let* ((selected-here-p
                                   (and select-active-p
                                        (export-tag-intersection-p tags select-tags)))
                                 (nested-scope (or selected-scope selected-here-p))
                                 (title-macros
                                   (unless (eq policy :prune-children)
                                     (prepare-title-macros
                                      node source-document nested-scope depth
                                      (1+ element-depth) active-includes
                                      active-macros))))
                            (multiple-value-bind
                                  (children selected-descendant-p)
                                (if archive-headline-only-p
                                    (values nil nil)
                                    (if (eq policy :prune-children)
                                        (values nil nil)
                                        (prepare-siblings
                                         (node-children node) source-document
                                         nested-scope depth (1+ element-depth)
                                         active-includes
                                         active-macros fragment-p)))
                              (if (or (not select-active-p) nested-scope
                                      selected-descendant-p)
                                  (values
                                   (list (%make-export-view
                                          :node node :children children
                                          :payload (list :selected selected-here-p
                                                         :title-macros title-macros)
                                          :source-document source-document))
                                   (or selected-here-p selected-descendant-p))
                                  (values nil nil)))))))
                   ((and select-active-p (not selected-scope))
                    (values nil nil))
                   ((and (eq type :keyword)
                         (string= (node-property node :key) "INCLUDE"))
                    (if (eq policy :default)
                        (prepare-include node source-document selected-scope depth
                                         element-depth
                                         active-includes active-macros)
                        (values
                         (list (%make-export-view
                                :node node :children nil
                                :source-document source-document))
                         nil)))
                   ((eq type :keyword)
                    (if (and (consp policy) (eq (car policy) :unsupported))
                        (values
                         (list (%make-export-view
                                :node node :children nil
                                :source-document source-document))
                         nil)
                        (let ((key (string-upcase
                                    (or (node-property node :key) ""))))
                          (export-validate-keywords
                           (list (list node key (or (node-property node :value) "")))
                           backend :fragment-p fragment-p :ancestry ancestry)
                          (if (member
                               key
                               (export-resolved-string-set
                                ancestry
                                #'%export-backend-descriptor-retained-keywords)
                               :test #'string-equal)
                              (values
                               (list (%make-export-view
                                      :node node :children nil
                                      :source-document source-document))
                               nil)
                              (values nil nil)))))
                   ((eq type :macro)
                    (if (eq policy :default)
                        (prepare-macro node source-document selected-scope depth
                                       element-depth
                                       active-includes active-macros)
                        (values
                         (list (%make-export-view
                                :node node :children nil
                                :source-document source-document))
                         nil)))
                   (t
                    (let ((children
                             (unless (eq policy :prune-children)
                               (nth-value
                                0 (prepare-siblings
                                   (node-children node) source-document
                                   selected-scope depth (1+ element-depth)
                                   active-includes
                                   active-macros fragment-p)))))
                      (values
                       (list (%make-export-view
                              :node node :children children
                              :source-document source-document))
                       nil))))))
             (prepare-siblings (nodes source-document selected-scope depth
                                      element-depth active-includes active-macros
                                      fragment-p)
               (let ((remaining nodes)
                     (views nil)
                     (selected-descendant-p nil))
                 (loop while remaining
                       for node = (pop remaining)
                       do (cond
                            ((eq (node-type node) :src-block)
                             (check-limit limits :max-element-depth element-depth)
                             (let* ((policy
                                      (export-resolved-node-policy
                                       ancestry (node-type node)))
                                    (requested-mode
                                      (unless (and (consp policy)
                                                   (eq (car policy) :unsupported))
                                        (export-src-export-mode node backend)))
                                    (results-node (first remaining))
                                    (adjacent-p
                                      (export-results-adjacent-p
                                       node results-node))
                                    (result-views nil))
                                   ;; Org drops adjacent results that the
                                   ;; mode does not export, so they must not
                                   ;; contribute footnotes or selection.
                                   (when (and (not (eq policy :prune-children))
                                              adjacent-p
                                              (member requested-mode
                                                      '(:results :both)
                                                      :test #'eq))
                                     (multiple-value-bind (prepared descendant-p)
                                         (prepare-node results-node source-document
                                                       selected-scope depth
                                                       element-depth
                                                       active-includes active-macros
                                                       fragment-p)
                                       (setf result-views prepared
                                             selected-descendant-p
                                             (or selected-descendant-p
                                                 descendant-p))))
                                   (push (%make-export-view
                                          :node node
                                          :children result-views
                                          :payload (and requested-mode
                                                        (list :exports
                                                              requested-mode
                                                              :adjacent-results
                                                              adjacent-p))
                                          :source-document source-document)
                                         views)
                                   (when adjacent-p
                                     (pop remaining))))
                            (t
                             (multiple-value-bind (prepared descendant-p)
                                 (prepare-node node source-document selected-scope
                                               depth element-depth
                                               active-includes active-macros
                                               fragment-p)
                               (dolist (view prepared)
                                 (push view views))
                               (setf selected-descendant-p
                                     (or selected-descendant-p descendant-p))))))
                 (values (nreverse views) selected-descendant-p))))
          (let* ((source-name (document-source-name document))
                 (active-includes (if (stringp source-name)
                                      (list source-name)
                                      nil))
                 (root-views
                  (prepare-siblings (node-children root) document
                                    (not select-active-p) 0 1
                                    active-includes nil nil)))
            (when filter-context
              (setf (%export-render-context-options filter-context) options
                    (%export-render-context-root-views filter-context) root-views)
              (let ((preparation
                     (%export-render-context-preparation filter-context)))
                (when (export-preparation-p preparation)
                  (setf (export-preparation-document preparation) document
                        (export-preparation-backend preparation) backend
                        (export-preparation-metadata preparation) metadata
                        (export-preparation-options preparation) options
                        (export-preparation-root-views preparation) root-views)))
              (setf root-views
                    (export-apply-filter-stage
                     (%export-render-context-backend filter-context)
                     :parse-tree root-views filter-context
                     :validator
                     (lambda (filtered-views)
                       (export-validate-filtered-root-views filtered-views limits))
                     :expected-type 'list))
              (setf (%export-render-context-root-views filter-context) root-views))
            (let ((preparation
                   (%make-export-preparation
                    :document document :backend backend :metadata metadata :options options
                    :root-views root-views :toc (export-collect-toc root-views options)
                    :footnotes (export-collect-footnotes root-views backend)
                    :allow-raw-html allow-raw-html
                    :allow-raw-latex allow-raw-latex)))
              (when filter-context
                (setf (%export-render-context-root-views filter-context) root-views
                      (%export-render-context-toc filter-context)
                      (export-preparation-toc preparation)
                      (%export-render-context-footnotes filter-context)
                      (export-preparation-footnotes preparation)
                      (%export-render-context-preparation filter-context) preparation)
                (export-initialize-render-states filter-context))
              preparation)))))))

(defun export-collect-views (views)
  (labels ((collect (view)
             (cons view
                   (mapcan #'collect (export-view-children view)))))
    (mapcan #'collect views)))

(defun export-collect-footnotes (views backend)
  (let ((definitions (make-hash-table :test #'equal))
        (numbers (make-hash-table :test #'equal))
        (ordered nil)
        (next-number 0)
        (flat-views (export-collect-views views)))
    (dolist (view flat-views)
      (let ((node (export-view-node view)))
        (when (eq (node-type node) :footnote-definition)
          (let ((label (node-property node :label)))
            (when label
              (when (gethash label definitions)
                (export-error backend :duplicate-footnote-definition node
                              "Duplicate footnote definition"))
              (setf (gethash label definitions) view))))))
    (dolist (view flat-views)
      (let* ((node (export-view-node view))
             (label (and (eq (node-type node) :footnote-reference)
                         (node-property node :label))))
        (when label
          (unless (gethash label definitions)
            (export-error backend :unresolved-footnote node
                          "Footnote reference has no selected definition"))
          (unless (gethash label numbers)
            (incf next-number)
            (setf (gethash label numbers) next-number)
            (push (list :label label :number next-number
                        :definition (gethash label definitions))
                  ordered)))))
    (nreverse ordered)))

(defun export-heading-depth-limit (options)
  (let* ((toc (cdr (assoc :toc options)))
         (explicit-toc-depth (and (integerp toc) toc))
         (heading-depth (or (cdr (assoc :heading-depth options)) 6)))
    (min heading-depth (or explicit-toc-depth 6))))

(defun export-collect-toc (views options)
  (let ((toc (if (assoc :toc options)
                 (cdr (assoc :toc options))
                 t))
        (depth-limit (export-heading-depth-limit options)))
    (when (or (eq toc t) (and (integerp toc) (plusp toc)))
      (loop for view in (export-collect-views views)
            for node = (export-view-node view)
            for level = (and (member (node-type node) '(:headline :inlinetask)
                                     :test #'eq)
                             (node-property node :level))
            when (and level (<= level depth-limit))
              collect (list :view view :level level
                            :title (node-property node :raw-value))))))

(defun export-check-expansion-depth (depth limits)
  (check-limit limits :max-export-expansion-depth depth))

(defmethod export-render-preparation ((preparation export-preparation) backend)
  (declare (ignore preparation))
  (export-error backend :unsupported-backend nil
                "No renderer is available for this export backend"))

(defun export-org (document backend
                            &key include-resolver allow-raw-html allow-raw-latex
                            (output-type :string) stream context)
  "Export DOCUMENT with BACKEND and return its requested output representation."
  (let* ((*export-output-check-counts* (make-hash-table))
         (*export-current-export-phase* :input-validation)
         (registry-snapshot
          (progn
            (unless (document-p document)
              (error 'type-error :datum document :expected-type 'document))
            (unless (keywordp backend)
              (error 'type-error :datum backend :expected-type 'keyword))
            (unless (export-backend-p backend)
              (export-error backend :unknown-backend nil "Unknown export backend"))
            (unless (or (null include-resolver) (functionp include-resolver))
              (error 'type-error :datum include-resolver :expected-type '(or null function)))
            (unless (member allow-raw-html '(nil t))
              (error 'type-error :datum allow-raw-html :expected-type 'boolean))
            (unless (member allow-raw-latex '(nil t))
              (error 'type-error :datum allow-raw-latex :expected-type 'boolean))
            (multiple-value-list (export-registry-snapshot backend))))
         (descriptor (first registry-snapshot))
         (ancestry (second registry-snapshot))
         (root-descriptor (first ancestry))
         (preparation-backend
          (cond
           ((null descriptor) backend)
           ((and root-descriptor
                 (%export-backend-descriptor-builtin-p root-descriptor))
            (%export-backend-descriptor-name root-descriptor))
           ((eq backend :html) backend)
           (t :html)))
         (caller-context (or context (make-export-context)))
         (render-context
          (and descriptor
               (%make-export-render-context
                :backend backend :document document
                :metadata nil :options nil :limits nil
                :root-views nil :toc nil :footnotes nil
                :preparation
                (%make-export-preparation
                 :allow-raw-html allow-raw-html
                 :allow-raw-latex allow-raw-latex)
                :raw-output-options
                (%export-backend-descriptor-raw-output-options descriptor)
                :extensions nil :ancestry ancestry
                :owner-backend backend :state nil :states nil)))
         (preparation
          (let ((*export-requested-backend* backend))
            (export-prepare document preparation-backend
                            :include-resolver include-resolver
                            :allow-raw-html allow-raw-html
                            :allow-raw-latex allow-raw-latex
                            :context caller-context
                            :filter-context render-context)))
         (initialized-render-context render-context)
         (text
          (let ((*export-requested-backend* backend))
            (if (and (%export-backend-descriptor-builtin-p descriptor)
                     (null (%export-backend-descriptor-renderer descriptor))
                     (null (%export-backend-descriptor-preamble-renderer descriptor))
                     (null (%export-backend-descriptor-trailer-renderer descriptor))
                     (null (%export-backend-descriptor-template descriptor)))
                (export-render-preparation preparation backend)
              (let* ((renderer-and-owner
                      (multiple-value-list
                       (export-render-context-renderer
                        initialized-render-context)))
                     (preamble-and-owner
                      (multiple-value-list
                       (export-render-context-preamble-renderer
                        initialized-render-context)))
                     (trailer-and-owner
                      (multiple-value-list
                       (export-render-context-trailer-renderer
                        initialized-render-context)))
                     (preamble-context
                      (and (first preamble-and-owner)
                           (export-context-for-owner
                            initialized-render-context
                            (second preamble-and-owner))))
                     (preamble
                      (export-run-renderer-stage
                       (first preamble-and-owner) preamble-context ""
                       (document-limits document) backend :preamble))
                     (body-context
                      (and (first renderer-and-owner)
                           (export-context-for-owner
                            initialized-render-context
                            (second renderer-and-owner))))
                     (body
                      (let ((*export-current-export-phase* :body)
                            (*export-current-output-offset*
                              (length preamble)))
                        (export-check-output-stage
                         (export-check-render-string
                          (if (first renderer-and-owner)
                              (funcall (first renderer-and-owner) body-context)
                            (export-render-preparation
                             preparation preparation-backend)))
                         (document-limits document) backend :body
                         :base-offset (length preamble))))
                     (body
                      (export-run-filter-stage
                       backend :body body initialized-render-context
                       (document-limits document) :body
                       :base-offset (length preamble)))
                     (trailer-context
                      (and (first trailer-and-owner)
                           (export-context-for-owner
                            initialized-render-context
                            (second trailer-and-owner))))
                     (trailer
                      (export-run-renderer-stage
                       (first trailer-and-owner) trailer-context ""
                       (document-limits document) backend :trailer
                       :base-offset (+ (length preamble) (length body))))
                     (template-and-owner
                      (multiple-value-list
                       (export-render-context-template
                        initialized-render-context)))
                     (template-context
                      (and (first template-and-owner)
                           (export-context-for-owner
                            initialized-render-context
                            (second template-and-owner))))
                     (output
                      (export-run-renderer-stage
                       (and (first template-and-owner)
                            (lambda (ignored-context)
                              (declare (ignore ignored-context))
                              (funcall (first template-and-owner)
                                       template-context preamble body trailer)))
                       initialized-render-context
                       (concatenate 'string preamble body trailer)
                       (document-limits document) backend :template)))
                (export-run-filter-stage
                 backend :final-output output initialized-render-context
                 (document-limits document) :final-output))))))
         (export-write-output text (document-limits document) output-type
                              :stream stream :backend backend)))

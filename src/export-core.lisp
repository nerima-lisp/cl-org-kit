;;;; src/export-core.lisp
(in-package #:org-kit)

(defparameter *export-node-operations*
  '((:org-data :root :root :root)
    (:headline :heading :heading :heading)
    (:inlinetask :heading :heading :heading)
    (:section :section :section :section)
    (:paragraph :paragraph :paragraph :paragraph)
    (:center-block :centered-block :paragraph :paragraph)
    (:quote-block :blockquote :blockquote :quote)
    (:special-block :special-block :children :children)
    (:drawer :children :children :children)
    (:property-drawer :omit :omit :omit)
    (:node-property :omit :omit :omit)
    (:planning :omit :omit :omit)
    (:dynamic-block :literal-block :literal-block :literal-block)
    (:footnote-definition :footnote-definition :footnote-definition
                          :footnote-definition)
    (:plain-list :list :list :list)
    (:item :list-item :list-item :list-item)
    (:table :table :table :table)
    (:table-row :table-row :table-row :table-row)
    (:table-cell :table-cell :table-cell :table-cell)
    (:babel-call :literal :literal :literal)
    (:inline-babel-call :literal :literal :literal)
    (:clock :omit :omit :omit)
    (:comment :omit :omit :omit)
    (:comment-block :omit :omit :omit)
    (:diary-sexp :unsupported :unsupported :unsupported)
    (:example-block :code-block :code-block :code-block)
    (:fixed-width :code-block :code-block :code-block)
    (:src-block :source-block :source-block :source-block)
    (:export-block :export-construct :export-construct :literal)
    (:export-snippet :export-construct :export-construct :literal)
    (:horizontal-rule :horizontal-rule :horizontal-rule :horizontal-rule)
    (:keyword :keyword :keyword :keyword)
    (:latex-environment :literal :literal :literal)
    (:latex-fragment :literal :literal :literal)
    (:verse-block :verse :verse :verse)
    (:bold :bold :bold :bold)
    (:italic :italic :italic :italic)
    (:strike-through :strike-through :strike-through :literal-children)
    (:underline :underline :underline :literal-children)
    (:subscript :subscript :subscript :literal-children)
    (:superscript :superscript :superscript :literal-children)
    (:code :code :code :quoted-literal)
    (:verbatim :code :code :quoted-literal)
    (:citation :literal :literal :literal)
    (:citation-reference :literal :literal :literal)
    (:entity :entity :entity :entity)
    (:footnote-reference :footnote-reference :footnote-reference
                         :footnote-reference)
    (:inline-src-block :inline-code :inline-code :literal)
    (:line-break :line-break :line-break :line-break)
    (:link :link :link :link)
    (:macro :macro :macro :macro)
    (:plain-text :text :text :text)
    (:radio-target :target :target :omit)
    (:target :target :target :omit)
    (:statistics-cookie :literal :literal :literal)
    (:timestamp :timestamp :timestamp :timestamp)))

(defun export-backend-p (backend)
  (member backend '(:html :gfm :ascii) :test #'eq))

(defun export-source-text (source)
  (cond ((node-p source) (node-text source))
        ((stringp source) source)
        (t nil)))

(defun export-bounded-source (source)
  (let ((text (export-source-text source)))
    (when text
      (with-output-to-string (output)
        (loop for character across text
              for index from 0
              when (>= index *condition-text-limit*)
                do (write-string "..." output)
                   (loop-finish)
              do (let ((code (char-code character)))
                   (cond
                     ((char= character #\\) (write-string "\\\\" output))
                     ((char= character #\Newline) (write-string "\\n" output))
                     ((char= character #\Return) (write-string "\\r" output))
                     ((char= character #\Tab) (write-string "\\t" output))
                     ((or (< code 32) (<= 127 code 159))
                      (format output "\\x~2,'0X" code))
                     (t (write-char character output)))))))))

(defun export-error (backend reason source message)
  (error 'org-export-error
         :message message
         :backend backend
         :reason reason
         :source (export-bounded-source source)))

(defun export-unsupported-syntax (backend source position feature message)
  (error 'org-export-unsupported-syntax
         :message message
         :backend backend
         :source (export-bounded-source source)
         :position position
         :feature feature))

(defun export-node-operation (type backend &optional source)
  "Return TYPE's BACKEND action; diary sexps signal unsupported syntax."
  (let ((entry (assoc type *export-node-operations*)))
    (let ((operation
            (ecase backend
              (:html (second entry))
              (:gfm (third entry))
              (:ascii (fourth entry)))))
      (if (eq operation :unsupported)
          (export-unsupported-syntax backend source 0 :diary-sexp
                                     "Diary sexp export is unsupported")
          operation))))

(defun export-html-escape (string attribute-p)
  (with-output-to-string (output)
    (loop for character across string
          do (case character
               (#\& (write-string "&amp;" output))
               (#\< (write-string "&lt;" output))
               (#\> (write-string "&gt;" output))
               (#\" (if attribute-p
                           (write-string "&quot;" output)
                           (write-char character output)))
               (otherwise (write-char character output))))))

(defun export-ascii-punctuation-p (character)
  (let ((code (char-code character)))
    (or (<= 33 code 47) (<= 58 code 64) (<= 91 code 96) (<= 123 code 126))))

(defun export-gfm-escape-literal (string)
  (with-output-to-string (output)
    (loop for character across string
          do (when (export-ascii-punctuation-p character)
               (write-char #\\ output))
             (write-char character output))))

(defun export-gfm-destination (string)
  (with-output-to-string (output)
    (write-char #\< output)
    (loop for character across string
          do (case character
               (#\Space (write-string "%20" output))
               (#\< (write-string "%3C" output))
               (#\> (write-string "%3E" output))
               (otherwise (write-char character output))))
    (write-char #\> output)))

(defun export-max-character-run (string character)
  (loop with run = 0
        with maximum = 0
        for current across string
        do (if (char= current character)
               (setf maximum (max maximum (incf run)))
               (setf run 0))
        finally (return maximum)))

(defun export-gfm-code-span (string)
  (let* ((delimiter (make-string (1+ (export-max-character-run string #\`))
                                 :initial-element #\`))
         (pad-p (and (plusp (length string))
                     (or (char= (char string 0) #\Space)
                         (char= (char string (1- (length string))) #\Space)
                         (char= (char string 0) #\`)
                         (char= (char string (1- (length string))) #\`)))))
    (concatenate 'string delimiter
                 (if pad-p " " "") string (if pad-p " " "") delimiter)))

(defun export-gfm-code-fence (string)
  (make-string (max 3 (1+ (export-max-character-run string #\~)))
               :initial-element #\~))

(defun export-escape-context (backend context string)
  "Escape STRING using the backend rules for its text, label, attribute, destination, or code context."
  (case backend
    (:html (case context
             (:text (export-html-escape string nil))
             (:attribute (export-html-escape string t))))
    (:gfm (case context
            ((:text :label) (export-gfm-escape-literal string))
            (:destination (export-gfm-destination string))
            (:code (export-gfm-code-span string))))
    (:ascii (case context
              ((:text :literal) string)))))

(defun export-control-character-p (character)
  (let ((code (char-code character)))
    (or (<= code #x1f) (<= #x7f code #x9f))))

(defun export-url-has-control-p (string)
  (some #'export-control-character-p string))

(defun export-url-leading-whitespace-p (string)
  (when (plusp (length string))
    (let ((code (char-code (char string 0))))
      (or (<= 9 code 13)
          (= code 32)
          (= code #x85)
          (= code #xa0)
          (= code #x1680)
          (<= #x2000 code #x200a)
          (= code #x2028)
          (= code #x2029)
          (= code #x202f)
          (= code #x205f)
          (= code #x3000)))))

(defun export-url-scheme (string)
  (let ((colon (position #\: string)))
    (when (and colon
               (plusp colon)
               (<= (char-code (char string 0)) 127)
               (alpha-char-p (char string 0))
               (loop for index from 1 below colon
                     for code = (char-code (char string index))
                     always (and (<= code 127)
                                 (let ((character (char string index)))
                                   (or (alphanumericp character)
                                       (find character "+.-"))))))
      (string-downcase (subseq string 0 colon)))))

(defun export-validate-url (document raw-destination)
  "Return whether RAW-DESTINATION is safe and its abbreviation-expanded value."
  (if (export-url-has-control-p raw-destination)
      (values nil raw-destination)
      (let* ((expanded (link-object-expand-abbreviation document raw-destination))
             (scheme (export-url-scheme expanded)))
        (values
         (and (not (export-url-has-control-p expanded))
              (not (export-url-leading-whitespace-p expanded))
              (not (find #\\ expanded))
              (not (and (>= (length expanded) 2)
                        (string= "//" expanded :end2 2)))
              (or (null scheme)
                  (member scheme '("http" "https" "mailto") :test #'string=)))
         expanded))))

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

(defun export-utf8-octets (text backend)
  (handler-case
      (cl-codec-kit:string-to-octets text :encoding :utf-8)
    (cl-codec-kit:unencodable-character ()
      (export-error backend :encoding text "Unable to encode UTF-8 export output"))))

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
             (unless (streamp stream)
               (export-error backend :invalid-output nil
                             "Stream output requires a stream"))
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
    (check-limit limits :max-output-length (length text))
    (if octet-output-p
        (let ((octet-count (export-utf8-length text backend)))
          (check-limit limits :max-output-octets octet-count)
          (let ((octets (export-utf8-octets text backend)))
            (if (eq output-type :stream)
                (progn (write-sequence octets stream) stream)
                octets)))
        (if (eq output-type :stream)
            (progn (write-string text stream) stream)
            text))))

(defun export-whitespace-p (character)
  (find character '(#\Space #\Tab #\Newline #\Return #\Page)))

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
      (export-unsupported-syntax backend (first (first matches)) 0
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
        (export-unsupported-syntax backend source 0 feature
                                   "Unsupported tag expression")))
    tags))

(defparameter *export-option-specifications*
  '(("toc" :toc :toc)
    ("h" :heading-depth :integer)
    ("num" :num :boolean)
    ("^" :^ :sub-superscript)
    ("title" :title :boolean)
    ("author" :author :boolean)
    ("date" :date :boolean)
    ("\\n" :preserve-breaks :boolean)
    ("timestamp" :timestamp :boolean)
    ("arch" :arch :archive-mode)
    ("expand-links" :expand-links :disabled-only)
    ("broken-links" :broken-links :disabled-only)
    ("c" :clock :boolean)
    ("creator" :creator :boolean)
    ("d" :drawers :selection-list)
    ("email" :email :boolean)
    ("*" :emphasis :boolean)
    ("e" :entities :boolean)
    (":" :fixed-width :boolean)
    ("f" :footnotes :boolean)
    ("inline" :inline-tasks :boolean)
    ("tex" :latex :latex-mode)
    ("p" :planning :boolean)
    ("pri" :priority :boolean)
    ("prop" :properties :selection-list)
    ("'" :smart-quotes :boolean)
    ("-" :special-strings :boolean)
    ("stat" :statistics-cookies :boolean)
    ("|" :tables :boolean)
    ("tags" :tags :tag-mode)
    ("tasks" :tasks :tasks-mode)
    ("<" :timestamp-range :timestamp-range)
    ("todo" :todo :boolean)))

(defun export-invalid-option (source backend message)
  (export-unsupported-syntax backend source 0 :invalid-option message))

(defun export-option-specification (key source backend)
  (or (assoc key *export-option-specifications* :test #'string=)
      (export-unsupported-syntax backend source 0 :unknown-option
                                 "Unknown export option")))

(defun export-option-boolean (value source backend)
  (cond ((string-equal value "t") t)
        ((string-equal value "nil") nil)
        (t (export-invalid-option source backend "Expected t or nil"))))

(defun export-option-integer (value source backend)
  (unless (and (plusp (length value))
               (every #'digit-char-p value))
    (export-invalid-option source backend "Expected a nonnegative integer"))
  (loop with number = 0
        for character across value
        for digit = (digit-char-p character)
        do (when (> number (floor (- most-positive-fixnum digit) 10))
             (export-invalid-option source backend
                                    "Option integer exceeds the supported range"))
           (setf number (+ (* number 10) digit))
        finally (return number)))

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

(defun export-option-list (value source backend)
  (unless (and (>= (length value) 2)
               (char= (char value 0) #\()
               (char= (char value (1- (length value))) #\)))
    (export-invalid-option source backend "Expected a parenthesized option list"))
  (let ((inner (subseq value 1 (1- (length value))))
        (items nil)
        (position 0))
    (loop while (< position (length inner))
          do (if (export-whitespace-p (char inner position))
                 (incf position)
                 (let ((start position))
                   (if (char= (char inner position) #\")
                       (progn
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
    (if (and items (string-equal (first items) "not"))
        (cons :exclude (rest items))
        (cons :include items))))

(defun export-option-value (specification value source backend)
  (let ((kind (third specification)))
    (ecase kind
      (:boolean (export-option-boolean value source backend))
      (:integer (export-option-integer value source backend))
      (:toc (cond ((string-equal value "t") t)
                  ((string-equal value "nil") nil)
                  (t (export-option-integer value source backend))))
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
      ((:selection-list :tasks-mode)
       (cond ((string-equal value "t") t)
             ((string-equal value "nil") nil)
             ((and (eq kind :tasks-mode) (string-equal value "todo")) :todo)
             ((and (eq kind :tasks-mode) (string-equal value "done")) :done)
             ((and (eq kind :tasks-mode) (string-equal value "not"))
              (export-invalid-option source backend "Invalid tasks option value"))
             (t (export-option-list value source backend))))
      )))

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

(defun export-parse-options (value source backend)
  (let ((options nil))
    (dolist (token (export-option-tokens value source backend))
      (multiple-value-bind (key option-value)
          (export-option-token-parts token source backend)
        (let* ((specification (export-option-specification key source backend))
               (option-key (second specification))
               (parsed-value
                 (export-option-value specification option-value source backend))
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
        (export-unsupported-syntax backend node 0 :invalid-macro
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
          (export-unsupported-syntax backend source 0 :duplicate-macro
                                     "Duplicate macro definition"))
        (setf (gethash name macros)
              (list :template template :function-p function-p :source source))))
    macros))

(defun export-document-settings (document backend context)
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
    (dolist (record (export-records-for-key records "OPTIONS"))
      (dolist (option (export-parse-options (third record) (first record) backend))
        (let ((existing (assoc (car option) options)))
          (if existing
              (setf (cdr existing) (cdr option))
              (setf options (append options (list option)))))))
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
    (values metadata options
            (and select-tags
                 (or select-present-p
                     (export-document-has-tag-p document select-tags)))
            select-tags exclude-tags (export-build-macro-table records backend))))

(defun export-macro-parameter-index (template position source backend)
  (let ((digit-position (1+ position)))
    (when (and (< digit-position (length template))
               (digit-char-p (char template digit-position)))
      (let ((digit (char template digit-position)))
        (unless (and (member digit '(#\1 #\2))
                     (or (= (1+ digit-position) (length template))
                         (not (digit-char-p
                               (char template (1+ digit-position))))))
          (export-unsupported-syntax backend source position
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
                 (not (export-url-has-control-p value)))
      (export-unsupported-syntax backend source 0 :include-form
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
                   backend node 0 :src-header-argument
                   "Duplicate :exports source block argument"))
                (unless tokens
                  (export-unsupported-syntax
                   backend node 0 :src-header-argument
                   "Missing :exports source block value"))
                (let ((value (string-downcase (pop tokens))))
                  (setf mode
                        (cond ((string= value "code") :code)
                              ((string= value "results") :results)
                              ((string= value "both") :both)
                              ((string= value "none") :none)
                              (t
                               (export-unsupported-syntax
                                backend node 0 :src-header-argument
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
  (and results-node
       (node-property results-node :RESULTS)
       (zerop (node-post-blank source-node))
       (= (node-end source-node) (node-begin results-node))))

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

(defun export-limits-with-node-budget (limits node-budget)
  (apply #'make-limits
         (loop for name in '(:max-input-length :max-heading-depth
                             :max-element-depth :max-object-depth :max-node-count
                             :max-output-length :max-output-octets
                             :max-agenda-range-days :max-agenda-entries
                             :max-future-repeats :max-export-expansion-depth
                             :max-export-expanded-characters
                             :max-export-expanded-nodes :max-formula-tokens
                             :max-formula-ast-depth :max-formula-precision
                             :max-formula-exponent :max-formula-work)
               append (list name
                            (if (eq name :max-node-count)
                                node-budget
                                (limit-value limits name))))))

(defun export-validate-keywords (records backend &key fragment-p)
  (dolist (record records)
    (let ((key (second record))
          (source (first record)))
      (cond
        ((or (member key '("TITLE" "AUTHOR" "DATE" "LANGUAGE" "OPTIONS"
                           "SELECT_TAGS" "EXCLUDE_TAGS" "MACRO" "ATTR_HTML"
                           "ARCHIVE" "COLUMNS" "PRIORITIES" "CONSTANTS"
                           "EXPORT_FILE_NAME" "DESCRIPTION" "KEYWORDS"
                           "SUBTITLE" "EMAIL" "CREATOR")
                 :test #'string=)
             (reader-heading-setting-kind key))
         (when fragment-p
           (export-unsupported-syntax backend source 0 :fragment-keyword-scope
                                      "Global export keywords in fragments are unsupported")))
        ((member key '("INCLUDE" "RESULTS" "NAME" "CAPTION" "HEADER")
                 :test #'string=)
         nil)
        (t
         (export-unsupported-syntax backend source 0 :unknown-keyword
                                    "Unknown export keyword"))))))

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
                             (if (eq (first value) :exclude)
                                 (some (lambda (child)
                                         (not (member (node-property child :key)
                                                      (rest value)
                                                      :test #'string-equal)))
                                       (node-children node))
                                 (some (lambda (child)
                                         (member (node-property child :key)
                                                 (rest value)
                                                 :test #'string-equal))
                                       (node-children node)))))))))))

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

(defun export-prepare (document backend &key include-resolver allow-raw-html context)
  "Prepare a private export view without serializing or modifying DOCUMENT."
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document))
  (unless (export-backend-p backend)
    (export-error backend :unknown-backend nil "Unknown export backend"))
  (unless (or (null include-resolver) (functionp include-resolver))
    (error 'type-error :datum include-resolver :expected-type '(or null function)))
  (unless (member allow-raw-html '(nil t))
    (error 'type-error :datum allow-raw-html :expected-type 'boolean))
  (let* ((root (document-root document))
         (limits (document-limits document)))
    (unless root
      (export-error backend :invalid-output nil "Document has not been parsed"))
    (let ((expanded-node-count (export-node-count root))
          (expanded-character-count 0))
      (check-limit limits :max-export-expanded-nodes expanded-node-count)
        (multiple-value-bind (metadata options select-active-p select-tags exclude-tags
                              macros)
          (export-document-settings document backend context)
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
             (parse-fragment (text source-name selected-scope depth
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
                                   selected-scope depth active-includes
                                   active-macros t)))
             (prepare-include (node source-document selected-scope depth
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
                   (parse-fragment text path selected-scope next-depth
                                   (cons path active-includes) active-macros))))
             (prepare-macro (node source-document selected-scope depth
                                 active-includes active-macros)
               (let* ((key (string-downcase (node-property node :key)))
                      (definition (gethash key macros))
                      (next-depth (1+ depth)))
                 (unless definition
                   (export-unsupported-syntax backend node 0 :unknown-macro
                                              "Unknown macro"))
                 (when (getf definition :function-p)
                   (export-unsupported-syntax backend node 0 :function-macro
                                              "Function macros are unsupported"))
                 (when (member key active-macros :test #'string=)
                   (export-unsupported-syntax backend node 0 :recursive-macro
                                              "Recursive macro expansion"))
                 (export-check-expansion-depth next-depth limits)
                 (let* ((arguments (node-property node :args))
                        (template (getf definition :template))
                        (expanded-length
                          (export-macro-expanded-length template arguments node backend)))
                   (account-expanded-length expanded-length)
                   (let* ((text (export-substitute-macro template arguments
                                                         node backend))
                          (fragment-views
                            (parse-fragment
                             text
                             (document-source-name source-document)
                             selected-scope next-depth active-includes
                             (cons key active-macros))))
                     (values
                      (list (%make-export-view
                             :node node :children fragment-views
                             :payload (list :expanded-text text)
                             :source-document source-document))
                      nil)))))
             (prepare-node (node source-document selected-scope depth
                                active-includes active-macros fragment-p)
               (let ((type (node-type node)))
                 (cond
                   ((and (eq type :keyword)
                         (string= (node-property node :key) "INCLUDE"))
                    (prepare-include node source-document selected-scope depth
                                     active-includes active-macros))
                   ((eq type :keyword)
                    (export-validate-keywords
                     (list (list node
                                 (string-upcase (or (node-property node :key) ""))
                                 (or (node-property node :value) "")))
                     backend :fragment-p fragment-p)
                    (values nil nil))
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
                                 (nested-scope (or selected-scope selected-here-p)))
                            (multiple-value-bind
                                  (children selected-descendant-p)
                                (if archive-headline-only-p
                                    (values nil nil)
                                    (prepare-siblings
                                     (node-children node) source-document
                                     nested-scope depth active-includes
                                     active-macros fragment-p))
                              (if (or (not select-active-p) nested-scope
                                      selected-descendant-p)
                                  (values
                                   (list (%make-export-view
                                          :node node :children children
                                          :payload (list :selected selected-here-p)
                                          :source-document source-document))
                                   (or selected-here-p selected-descendant-p))
                                  (values nil nil)))))))
                   ((and select-active-p (not selected-scope))
                    (values nil nil))
                   ((eq type :macro)
                    (prepare-macro node source-document selected-scope depth
                                   active-includes active-macros))
                   ((eq type :diary-sexp)
                    (export-node-operation type backend node))
                   (t
                    (let* ((operation (export-node-operation type backend node))
                           (children
                             (unless (eq operation :omit)
                               (nth-value
                                0 (prepare-siblings
                                   (node-children node) source-document
                                   selected-scope depth active-includes
                                   active-macros fragment-p)))))
                      (values
                       (list (%make-export-view
                              :node node :children children
                              :payload (list :operation operation)
                              :source-document source-document))
                       nil))))))
             (prepare-siblings (nodes source-document selected-scope depth
                                      active-includes active-macros fragment-p)
               (let ((remaining nodes)
                     (views nil)
                     (selected-descendant-p nil))
                 (loop while remaining
                       for node = (pop remaining)
                       do (cond
                            ((eq (node-type node) :src-block)
                             (let* ((mode (export-src-export-mode node backend))
                                        (results-node (first remaining))
                                        (adjacent-p
                                          (export-results-adjacent-p
                                           node results-node))
                                        (result-views nil))
                                   (when (and adjacent-p
                                              (member mode '(:results :both)
                                                      :test #'eq))
                                     (multiple-value-bind (prepared descendant-p)
                                         (prepare-node results-node source-document
                                                       selected-scope depth
                                                       active-includes active-macros
                                                       fragment-p)
                                       (setf result-views prepared
                                             selected-descendant-p
                                             (or selected-descendant-p
                                                 descendant-p))))
                                   (push (%make-export-view
                                          :node node
                                          :children result-views
                                          :payload (list :exports mode
                                                         :adjacent-results
                                                         adjacent-p)
                                          :source-document source-document)
                                         views)
                                   (when adjacent-p
                                     (pop remaining))))
                            (t
                             (multiple-value-bind (prepared descendant-p)
                                 (prepare-node node source-document selected-scope
                                               depth active-includes active-macros
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
                                     (not select-active-p) 0 active-includes nil nil)))
            (%make-export-preparation
             :document document :backend backend :metadata metadata :options options
             :root-views root-views :toc (export-collect-toc root-views options)
             :footnotes (export-collect-footnotes root-views backend)
             :allow-raw-html allow-raw-html)))))))

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
  (let ((toc (cdr (assoc :toc options)))
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

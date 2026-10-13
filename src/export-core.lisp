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
  "Return the private operation for TYPE and BACKEND, or signal when unsupported."
  (unless (export-backend-p backend)
    (export-error backend :unknown-backend source "Unknown export backend"))
  (unless (node-type-p type)
    (error 'type-error :datum type :expected-type 'node-type))
  (let ((entry (assoc type *export-node-operations*)))
    (unless entry
      (export-unsupported-syntax backend source 0 :unknown-node-type
                                 "Node type has no export mapping"))
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
  "Escape STRING for the named backend context without serializing a node."
  (unless (stringp string)
    (error 'type-error :datum string :expected-type 'string))
  (case backend
    (:html (case context
             (:text (export-html-escape string nil))
             (:attribute (export-html-escape string t))
             (otherwise (error 'type-error :datum context
                               :expected-type '(member :text :attribute)))))
    (:gfm (case context
            ((:text :label) (export-gfm-escape-literal string))
            (:destination (export-gfm-destination string))
            (:code (export-gfm-code-span string))
            (otherwise (error 'type-error :datum context
                              :expected-type '(member :text :label
                                                      :destination :code)))))
    (:ascii (case context
              ((:text :literal) string)
              (otherwise (error 'type-error :datum context
                                :expected-type '(member :text :literal)))))
    (otherwise
     (export-error backend :unknown-backend string "Unknown export backend"))))

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
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document))
  (unless (stringp raw-destination)
    (error 'type-error :datum raw-destination :expected-type 'string))
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
  (handler-case
      (cl-codec-kit:string-size-in-octets text :encoding :utf-8)
    (cl-codec-kit:unsupported-encoding ()
      (export-error backend :encoding text "Unable to measure UTF-8 export output"))))

(defun export-utf8-octets (text backend)
  (handler-case
      (cl-codec-kit:string-to-octets text :encoding :utf-8)
    (cl-codec-kit:unsupported-encoding ()
      (export-error backend :encoding text "Unable to encode UTF-8 export output"))))

(defun export-open-output (limits output-type &key stream backend)
  (unless (limits-p limits)
    (error 'type-error :datum limits :expected-type 'limits))
  (let ((octet-output-p nil)
        (buffer nil))
    (case output-type
      ((:string :octets)
       (when stream
         (export-error backend :invalid-output nil
                       "A stream is only valid with :STREAM output type"))
       (setf octet-output-p (eq output-type :octets)
             buffer (make-string-output-stream)))
      (:stream
       (unless (streamp stream)
         (export-error backend :invalid-output nil
                       "Stream output requires a stream"))
       (multiple-value-bind (character-p character-known-p)
           (subtypep (stream-element-type stream) 'character)
         (cond
           ((and character-known-p character-p))
           ((equal (stream-element-type stream) '(unsigned-byte 8))
            (setf octet-output-p t))
           (t
            (export-error backend :invalid-output nil
                          "Unsupported stream element type")))))
      (otherwise
       (export-error backend :invalid-output nil
                     "Unsupported export output type")))
    (%make-export-output-state :limits limits :backend backend
                               :output-type output-type :stream stream
                               :octet-output-p octet-output-p :buffer buffer)))

(defun export-append-output (state text)
  (unless (export-output-state-p state)
    (error 'type-error :datum state :expected-type 'export-output-state))
  (unless (stringp text)
    (export-error (export-output-state-backend state) :invalid-output text
                  "Export output must be a string"))
  (let* ((limits (export-output-state-limits state))
         (character-count (+ (export-output-state-character-count state)
                             (length text)))
         (octet-count (export-output-state-octet-count state)))
    (check-limit limits :max-output-length character-count)
    (when (export-output-state-octet-output-p state)
      (incf octet-count
            (export-utf8-length text (export-output-state-backend state)))
      (check-limit limits :max-output-octets octet-count))
    (setf (export-output-state-character-count state) character-count
          (export-output-state-octet-count state) octet-count)
    (if (eq (export-output-state-output-type state) :stream)
        (if (export-output-state-octet-output-p state)
            (write-sequence
             (export-utf8-octets text (export-output-state-backend state))
             (export-output-state-stream state))
            (write-string text (export-output-state-stream state)))
        (write-string text (export-output-state-buffer state)))
    state))

(defun export-finish-output (state)
  (unless (export-output-state-p state)
    (error 'type-error :datum state :expected-type 'export-output-state))
  (case (export-output-state-output-type state)
    (:string (get-output-stream-string (export-output-state-buffer state)))
    (:octets
     (export-utf8-octets
      (get-output-stream-string (export-output-state-buffer state))
      (export-output-state-backend state)))
    (:stream (export-output-state-stream state))))

(defun export-write-output (text limits output-type &key stream backend)
  "Apply output limits and return a string, UTF-8 octets, or the written stream."
  (unless (stringp text)
    (export-error backend :invalid-output text "Export output must be a string"))
  (unless (limits-p limits)
    (error 'type-error :datum limits :expected-type 'limits))
  (let ((state (export-open-output limits output-type :stream stream
                                  :backend backend)))
    (export-append-output state text)
    (export-finish-output state)))

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

(defun export-option-boolean (value source backend option-name)
  (cond ((string-equal value "t") t)
        ((string-equal value "nil") nil)
        (t (export-unsupported-syntax backend source 0 :invalid-option
                                      (format nil "Invalid ~A option value"
                                              option-name)))))

(defun export-option-value (key value source backend)
  (cond
    ((string= key "toc")
     (cond ((string-equal value "t") t)
           ((string-equal value "nil") nil)
           ((and (= (length value) 1)
                 (find (char value 0) "123456"))
            (digit-char-p (char value 0)))
           (t (export-unsupported-syntax backend source 0 :invalid-option
                                         "Invalid toc option value"))))
    ((string= key "h")
     (if (and (= (length value) 1)
              (digit-char-p (char value 0))
              (<= 1 (digit-char-p (char value 0)) 6))
         (digit-char-p (char value 0))
         (export-unsupported-syntax backend source 0 :invalid-option
                                    "Heading depth must be from 1 to 6")))
    ((member key '("num" "^" "title" "author" "date") :test #'string=)
     (export-option-boolean value source backend key))
    (t
     (export-unsupported-syntax backend source 0 :unknown-option
                                "Unknown export option"))))

(defun export-option-key (key source backend)
  (let ((entry (assoc key
                      '(("toc" . :toc)
                        ("h" . :heading-depth)
                        ("num" . :num)
                        ("^" . :^)
                        ("title" . :title)
                        ("author" . :author)
                        ("date" . :date))
                      :test #'string=)))
    (if entry
        (cdr entry)
        (export-unsupported-syntax backend source 0 :unknown-option
                                   "Unknown export option"))))

(defun export-parse-options (value source backend)
  (let ((options nil))
    (dolist (token (export-split-words value))
      (let ((colon (position #\: token)))
        (unless (and colon (plusp colon) (< colon (1- (length token))))
          (export-unsupported-syntax backend source 0 :invalid-option
                                     "Malformed export option"))
        (let* ((key (string-downcase (subseq token 0 colon)))
               (option-key (export-option-key key source backend))
               (option-value (export-option-value
                              key (subseq token (1+ colon)) source backend)))
          (when (assoc option-key options)
            (export-unsupported-syntax backend source 0 :duplicate-option
                                       "Duplicate export option"))
          (push (cons option-key option-value) options))))
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
    (multiple-value-bind (value present-p source)
        (export-single-keyword-value records "OPTIONS" backend)
      (when present-p
        (setf options (export-parse-options value source backend))))
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
            select-tags exclude-tags (export-build-macro-table records backend)
            records)))

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

(defun export-copy-source-name (source-name backend source)
  (let ((copies (make-hash-table :test #'eq)))
    (labels ((copy-value (value)
               (cond
                 ((stringp value)
                  (or (gethash value copies)
                      (setf (gethash value copies) (copy-seq value))))
                 ((consp value)
                  (or (gethash value copies)
                      (let ((copy (cons nil nil)))
                        (setf (gethash value copies) copy
                              (car copy) (copy-value (car value))
                              (cdr copy) (copy-value (cdr value)))
                        copy)))
                 ((vectorp value)
                  (or (gethash value copies)
                      (let ((copy
                              (make-array
                               (array-total-size value)
                               :element-type (array-element-type value)
                               :adjustable (adjustable-array-p value)
                               :fill-pointer
                               (when (array-has-fill-pointer-p value)
                                 (fill-pointer value)))))
                        (setf (gethash value copies) copy)
                        (loop for index below (array-total-size value)
                              do (setf (aref copy index)
                                       (copy-value (aref value index))))
                        copy)))
                 ((or (null value) (symbolp value) (numberp value)
                      (characterp value))
                  value)
                 (t
                  (export-unsupported-syntax
                   backend source 0 :include-source-name
                   "Include source name contains an unsupported mutable value")))))
      (copy-value source-name))))

(defun export-src-export-mode (node backend)
  (let ((tokens (export-split-words (or (node-property node :switches) "")))
        (mode nil)
        (mode-seen-p nil))
    (loop while tokens
          for token = (pop tokens)
          do (unless (string-equal token ":exports")
               (export-error backend :invalid-output node
                             "Unsupported source block header argument"))
             (when mode-seen-p
               (export-error backend :invalid-output node
                             "Duplicate :exports source block argument"))
             (unless tokens
               (export-error backend :invalid-output node
                             "Missing :exports source block value"))
             (let ((value (string-downcase (pop tokens))))
               (unless (member value '("code" "results" "both" "none")
                               :test #'string=)
                 (export-unsupported-syntax backend node 0 :invalid-src-exports
                                            "Unsupported :exports mode"))
               (setf mode (intern (string-upcase value) :keyword)
                     mode-seen-p t)))
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
        ((member key '("TITLE" "AUTHOR" "DATE" "LANGUAGE" "OPTIONS"
                       "SELECT_TAGS" "EXCLUDE_TAGS" "MACRO" "ATTR_HTML")
                 :test #'string=)
         (when fragment-p
           (export-unsupported-syntax backend source 0 :fragment-keyword-scope
                                      "Global export keywords in fragments are unsupported")))
        ((reader-heading-setting-kind key)
         (when fragment-p
           (export-unsupported-syntax backend source 0 :fragment-keyword-scope
                                      "Document settings in fragments are unsupported")))
        ((member key '("INCLUDE" "RESULTS") :test #'string=) nil)
        (t
         (export-unsupported-syntax backend source 0 :unknown-keyword
                                    "Unknown export keyword"))))))

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
                            macros records)
          (export-document-settings document backend context)
        (export-validate-keywords records backend)
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
                 (export-validate-keywords
                  (export-keyword-records fragment) backend :fragment-p t)
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
                                       (export-copy-source-name source-name
                                                                backend node))))
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
                    (values nil nil))
                   ((member type '(:headline :inlinetask) :test #'eq)
                    (let* ((tags (effective-heading-tags source-document node))
                           (excluded-p (export-tag-intersection-p tags exclude-tags)))
                      (if excluded-p
                          (values nil nil)
                          (let* ((selected-here-p
                                   (and select-active-p
                                        (export-tag-intersection-p tags select-tags)))
                                 (nested-scope (or selected-scope selected-here-p)))
                            (multiple-value-bind
                                  (children selected-descendant-p)
                                (prepare-siblings (node-children node) source-document
                                                  nested-scope depth active-includes
                                                  active-macros fragment-p)
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
                             (if (and select-active-p (not selected-scope))
                                 (when (export-results-adjacent-p
                                        node (first remaining))
                                   (pop remaining))
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
                                   (setf views
                                         (nconc views
                                                (list (%make-export-view
                                                       :node node
                                                       :children result-views
                                                       :payload (list :exports mode
                                                                      :adjacent-results
                                                                      adjacent-p)
                                                       :source-document
                                                       source-document))))
                                   (when adjacent-p
                                     (pop remaining)))))
                            (t
                             (multiple-value-bind (prepared descendant-p)
                                 (prepare-node node source-document selected-scope
                                               depth active-includes active-macros
                                               fragment-p)
                               (setf views (nconc views prepared)
                                     selected-descendant-p
                                     (or selected-descendant-p descendant-p))))))
                 (values views selected-descendant-p))))
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
                         (node-property node :label)))
             (type (and (eq (node-type node) :footnote-reference)
                        (node-property node :type))))
        (when (and (eq (node-type node) :footnote-reference)
                   (or label (not (eq type :inline))))
          (unless (and label (gethash label definitions))
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

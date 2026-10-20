;;;; src/bibtex.lisp
;;;;
;;;; BibTeX entries and Org headline properties.
(in-package #:org-kit)

(defstruct bibtex-entry
  type
  key
  fields
  position)

(defparameter +bibtex-types+
  '(("article"
     (("author") ("title") ("journal") ("year"))
     (("volume") ("number") ("pages") ("month") ("note") ("doi")))
    ("book"
     (("editor" "author") ("title") ("publisher") ("year"))
     (("volume" "number") ("series") ("address") ("edition") ("month")
      ("note") ("doi")))
    ("booklet"
     (("title"))
     (("author") ("howpublished") ("address") ("month") ("year") ("note")
      ("doi") ("url")))
    ("conference"
     (("author") ("title") ("booktitle") ("year"))
     (("editor") ("pages") ("organization") ("publisher") ("address")
      ("month") ("note") ("doi") ("url")))
    ("inbook"
     (("author" "editor") ("title") ("chapter" "pages") ("publisher")
      ("year"))
     (("crossref") ("volume" "number") ("series") ("type") ("address")
      ("edition") ("month") ("note") ("doi")))
    ("incollection"
     (("author") ("title") ("booktitle") ("publisher") ("year"))
     (("crossref") ("editor") ("volume" "number") ("series") ("type")
      ("chapter") ("pages") ("address") ("edition") ("month") ("note")
      ("doi")))
    ("inproceedings"
     (("author") ("title") ("booktitle") ("year"))
     (("crossref") ("editor") ("volume" "number") ("series") ("pages")
      ("address") ("month") ("organization") ("publisher") ("note")
      ("doi")))
    ("manual"
     (("title"))
     (("author") ("organization") ("address") ("edition") ("month")
      ("year") ("note") ("doi") ("url")))
    ("mastersthesis"
     (("author") ("title") ("school") ("year"))
     (("type") ("address") ("month") ("note") ("doi") ("url")))
    ("misc"
     nil
     (("author") ("title") ("howpublished") ("month") ("year") ("note")
      ("doi") ("url")))
    ("phdthesis"
     (("author") ("title") ("school") ("year"))
     (("type") ("address") ("month") ("note") ("doi") ("url")))
    ("proceedings"
     (("title") ("year"))
     (("editor") ("volume" "number") ("series") ("address") ("month")
      ("organization") ("publisher") ("note") ("doi")))
    ("techreport"
     (("author") ("title") ("institution") ("year"))
     (("type") ("address") ("month") ("note") ("doi") ("url")))
    ("unpublished"
     (("author") ("title") ("note"))
     (("month") ("year") ("doi") ("url")))))

(defun bibtex-whitespace-p (character)
  (find character '(#\Space #\Tab #\Newline #\Return #\Page)))

(defun bibtex-skip-whitespace (input position)
  (loop while (and (< position (length input))
                   (bibtex-whitespace-p (char input position)))
        do (incf position)
        finally (return position)))

(defun bibtex-ascii-alpha-p (character)
  (or (char<= #\A character #\Z)
      (char<= #\a character #\z)))

(defun bibtex-identifier-char-p (character)
  (or (bibtex-ascii-alpha-p character)
      (char<= #\0 character #\9)
      (find character '(#\_ #\-))))

(defun bibtex-read-identifier (input position)
  (when (and (< position (length input))
             (bibtex-ascii-alpha-p (char input position)))
    (let ((end (1+ position)))
      (loop while (and (< end (length input))
                       (bibtex-identifier-char-p (char input end)))
            do (incf end))
      (values (subseq input position end) end))))

(defun bibtex-signal-error (input position reason message)
  (let* ((length (if (stringp input) (length input) 0))
         (position (and (integerp position) (max 0 (min position length))))
         (start (and position (max 0 (- position 24))))
         (end (and position (min length (+ position 40))))
         (context (and position (bounded-text (subseq input start end)))))
    (error 'org-bibtex-error
           :message message
           :reason reason
           :position position
           :context context)))

(defun bibtex-collapse-whitespace (value)
  (with-output-to-string (stream)
    (loop with pending-space-p = nil
          for character across value
          do (if (find character '(#\Space #\Tab #\Newline #\Return))
                 (setf pending-space-p t)
               (progn
                 (when pending-space-p
                   (write-char #\Space stream))
                 (write-char character stream)
                 (setf pending-space-p nil)))
          finally (when pending-space-p
                    (write-char #\Space stream)))))

(defun bibtex-strip-outer-braces (value)
  (if (and (>= (length value) 2)
           (char= (char value 0) #\{))
      (let ((depth 0)
            (closing-position nil))
        (loop for position from 0 below (length value)
              for character = (char value position)
              do (cond ((char= character #\{) (incf depth))
                       ((char= character #\})
                        (decf depth)
                        (when (zerop depth)
                          (setf closing-position position)
                          (return)))))
        (if (eql closing-position (1- (length value)))
            (subseq value 1 closing-position)
            value))
      value))

(defun bibtex-strip-outer-quotes (value)
  (if (and (>= (length value) 2)
           (char= (char value 0) #\"))
      (let ((escaped-p nil)
            (closing-position nil))
        (loop for position from 1 below (length value)
              for character = (char value position)
              do (cond (escaped-p (setf escaped-p nil))
                       ((char= character #\\) (setf escaped-p t))
                       ((char= character #\")
                        (setf closing-position position)
                        (return))))
        (if (eql closing-position (1- (length value)))
            (subseq value 1 closing-position)
            value))
      value))

(defun bibtex-normalize-value (value)
  (bibtex-collapse-whitespace
   (bibtex-strip-outer-quotes
    (bibtex-strip-outer-braces value))))

(defun bibtex-read-value (input position closing-delimiter)
  (let ((start (bibtex-skip-whitespace input position))
        (end nil)
        (depth 0)
        (quoted-p nil)
        (escaped-p nil))
    (loop for current from start below (length input)
          for character = (char input current)
          do (cond
               (escaped-p
                (setf escaped-p nil))
               (quoted-p
                (cond ((char= character #\\) (setf escaped-p t))
                      ((char= character #\") (setf quoted-p nil))))
               ((char= character #\")
                (setf quoted-p t))
               ((char= character #\{)
                (incf depth))
               ((char= character #\})
                (if (plusp depth)
                    (decf depth)
                    (if (char= closing-delimiter #\})
                        (setf end current)
                        (bibtex-signal-error
                         input current :malformed-delimiter
                         "Unexpected closing brace in BibTeX value"))))
               ((and (zerop depth)
                     (or (char= character #\,)
                         (char= character closing-delimiter)))
                (setf end current)))
          while (null end))
    (when (or quoted-p escaped-p (plusp depth))
      (bibtex-signal-error input (length input) :unclosed-value
                           "Unclosed BibTeX field value"))
    (unless end
      (setf end (length input)))
    (values (bibtex-normalize-value (subseq input start end)) end)))

(defun bibtex-parse-fields (input position closing-delimiter)
  (let ((fields nil))
    (loop
      (setf position (bibtex-skip-whitespace input position))
      (when (>= position (length input))
        (bibtex-signal-error input position :unclosed-entry
                             "BibTeX entry has no closing delimiter"))
      (when (char= (char input position) closing-delimiter)
        (return-from bibtex-parse-fields
          (values (nreverse fields) (1+ position))))
      (multiple-value-bind (field field-end)
          (bibtex-read-identifier input position)
        (unless field
          (bibtex-signal-error input position :missing-field
                               "Expected a BibTeX field name"))
        (setf field (string-downcase field)
              position (bibtex-skip-whitespace input field-end))
        (unless (and (< position (length input))
                     (char= (char input position) #\=))
          (bibtex-signal-error input position :malformed-field
                               "Expected = after BibTeX field name"))
        (setf position (bibtex-skip-whitespace input (1+ position)))
        (multiple-value-bind (value value-end)
            (bibtex-read-value input position closing-delimiter)
          (when (assoc field fields :test #'string=)
            (bibtex-signal-error input field-end :duplicate-field
                                 "BibTeX entry contains a duplicate field"))
          (push (cons field value) fields)
          (setf position (bibtex-skip-whitespace input value-end))
          (when (>= position (length input))
            (bibtex-signal-error input position :unclosed-entry
                                 "BibTeX entry has no closing delimiter"))
          (cond
            ((char= (char input position) closing-delimiter)
             (return-from bibtex-parse-fields
               (values (nreverse fields) (1+ position))))
            ((char= (char input position) #\,)
             (incf position)
             (let ((next (bibtex-skip-whitespace input position)))
               (when (and (< next (length input))
                          (char= (char input next) closing-delimiter))
                 (return-from bibtex-parse-fields
                   (values (nreverse fields) (1+ next))))
               (setf position next)))
            (t
             (bibtex-signal-error input position :malformed-field
                                  "Expected a comma or entry delimiter"))))))))

(defun bibtex-parse-entry (input position)
  (let ((entry-position position))
    (incf position)
    (setf position (bibtex-skip-whitespace input position))
    (multiple-value-bind (type type-end)
        (bibtex-read-identifier input position)
      (unless type
        (bibtex-signal-error input position :missing-type
                             "BibTeX entry is missing its type"))
      (setf position (bibtex-skip-whitespace input type-end))
      (unless (< position (length input))
        (bibtex-signal-error input position :malformed-delimiter
                             "BibTeX entry is missing its opening delimiter"))
      (let* ((opening-delimiter (char input position))
             (closing-delimiter
               (case opening-delimiter
                 (#\{ #\})
                 (#\( #\))
                 (otherwise
                  (bibtex-signal-error input position :malformed-delimiter
                                       "Expected { or ( after BibTeX type"))))
             (key-start (1+ position))
             (key-end key-start))
        (loop while (and (< key-end (length input))
                         (not (char= (char input key-end) #\,))
                         (not (char= (char input key-end) closing-delimiter)))
              do (incf key-end))
        (when (>= key-end (length input))
          (bibtex-signal-error input key-end :unclosed-entry
                               "BibTeX entry has no closing delimiter"))
        (let ((key (string-trim '(#\Space #\Tab #\Newline #\Return #\Page)
                                (subseq input key-start key-end))))
          (when (zerop (length key))
            (bibtex-signal-error input key-start :missing-key
                                 "BibTeX entry is missing its key"))
          (when (some #'bibtex-whitespace-p key)
            (bibtex-signal-error input key-start :malformed-key
                                 "BibTeX entry key cannot contain whitespace"))
          (if (char= (char input key-end) closing-delimiter)
              (values (make-bibtex-entry :type (string-downcase type)
                                         :key key
                                         :fields nil
                                         :position entry-position)
                      (1+ key-end))
              (multiple-value-bind (fields next-position)
                  (bibtex-parse-fields input (1+ key-end) closing-delimiter)
                (values (make-bibtex-entry :type (string-downcase type)
                                           :key key
                                           :fields fields
                                           :position entry-position)
                        next-position))))))))

(defun bibtex-skip-outside-comments (input position)
  (loop
    (setf position (bibtex-skip-whitespace input position))
    (if (and (< position (length input))
             (char= (char input position) #\%))
        (let ((newline (position #\Newline input :start position)))
          (setf position (if newline (1+ newline) (length input))))
        (return position))))

(defun bibtex-parse-entries (input)
  (let ((entries nil)
        (position 0))
    (loop
      (setf position (bibtex-skip-outside-comments input position))
      (when (>= position (length input))
        (return))
      (unless (char= (char input position) #\@)
        (bibtex-signal-error input position :unexpected-token
                             "Expected a BibTeX entry"))
      (multiple-value-bind (entry next-position)
          (bibtex-parse-entry input position)
        (push entry entries)
        (setf position next-position)))
    (unless entries
      (bibtex-signal-error input 0 :empty-input
                           "BibTeX input contains no entries"))
    (nreverse entries)))

(defun bibtex-type-definition (type)
  (assoc type +bibtex-types+ :test #'string=))

(defun bibtex-value-present-p (value)
  (and (stringp value)
       (plusp (length (string-trim '(#\Space #\Tab) value)))))

(defun bibtex-entry-field-value (entry field)
  (cdr (assoc field (bibtex-entry-fields entry) :test #'string=)))

(defun bibtex-prefix-string (prefix input position)
  (unless (or (null prefix)
              (and (stringp prefix)
                   (not (find #\: prefix))
                   (not (find #\Newline prefix))
                   (not (find #\Return prefix))))
    (bibtex-signal-error input position :invalid-prefix
                         "BibTeX property prefix must be a single-line string"))
  (or prefix ""))

(defun bibtex-property-key (prefix field)
  (string-upcase (concatenate 'string prefix field)))

(defun bibtex-headline-property (headline key) (nth-value 0 (effective-heading-property-value headline key)))

(defun bibtex-first-headline (document)
  (find :headline (node-children (document-root document)) :key #'node-type))

(defun bibtex-align-generated-properties (document headline)
  (let* ((section (find :section (node-children headline) :key #'node-type))
         (drawer (and section
                      (find :property-drawer (node-children section)
                            :key #'node-type)))
         (properties (and drawer (node-children drawer))))
    (if (null properties)
        document
      (let* ((source (document-text document))
             (patches
              (loop for property in properties
                    for label-length =
                    (+ 2 (length (node-property property :key)))
                    for value-start = (+ (node-begin property) label-length)
                    for line-end =
                    (nth-value 0
                               (edit-properties-line-bounds
                                source (node-begin property)))
                    for value = (node-property property :value)
                    for padding = (max 1 (- 11 label-length))
                    when (stringp value)
                    collect (edit-make-patch
                             value-start line-end
                             (if (zerop (length value))
                                 ""
                               (concatenate
                                'string
                                (make-string padding :initial-element #\Space)
                                value)))))
             (expected (edit-canonical-document document)))
        (if patches
            (edit-apply-patches document patches
                                :target headline :expected expected)
          document)))))

(defun bibtex-build-entry-subtree (document entry prefix)
  (let* ((entry-title (bibtex-entry-field-value entry "title"))
         (newline (edit-properties-line-ending
                   (document-text document)
                   (length (document-text document))))
         (temporary
          (parse-org (concatenate 'string "* " entry-title newline)
                     :limits (document-limits document)
                     :settings (document-settings document))))
    (setf temporary
          (set-property temporary (bibtex-first-headline temporary)
                        (bibtex-property-key prefix "title") entry-title))
    (setf temporary
          (set-property temporary (bibtex-first-headline temporary)
                        (bibtex-property-key prefix "btype")
                        (bibtex-entry-type entry)))
    (setf temporary
          (set-property temporary (bibtex-first-headline temporary)
                        "CUSTOM_ID" (bibtex-entry-key entry)))
    (dolist (field (bibtex-entry-fields entry))
      (unless (string= (car field) "title")
        (setf temporary
              (set-property temporary (bibtex-first-headline temporary)
                            (bibtex-property-key prefix (car field))
                            (cdr field)))))
    (document-text
     (bibtex-align-generated-properties
      temporary (bibtex-first-headline temporary)))))

(defun bibtex-import (document input &key prefix)
  "Import BibTeX INPUT as appended Org headings and return an edit result."
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document))
  (unless (document-root document)
    (bibtex-signal-error nil nil :invalid-document
                         "BibTeX import requires a parsed Org document"))
  (unless (stringp input)
    (bibtex-signal-error nil nil :invalid-input
                         "BibTeX input must be a string"))
  (check-limit (document-limits document) :max-input-length (length input))
  (let* ((prefix (bibtex-prefix-string prefix input 0))
         (entries (bibtex-parse-entries input))
         (subtrees
           (mapcar
            (lambda (entry)
              (unless (bibtex-type-definition (bibtex-entry-type entry))
                (bibtex-signal-error input (bibtex-entry-position entry)
                                     :unknown-type
                                     "Unknown BibTeX entry type"))
              (unless (bibtex-entry-field-value entry "title")
                (bibtex-signal-error input (bibtex-entry-position entry)
                                     :missing-title
                                     "BibTeX entry is missing its title"))
              (bibtex-build-entry-subtree document entry prefix))
            entries))
         (source (document-text document))
         (offset (length source))
         (separator (if (and (plusp offset)
                             (not (document-final-newline-p document)))
                        (edit-properties-line-ending source offset)
                        ""))
         (replacement (concatenate 'string separator
                                   (apply #'concatenate 'string subtrees)))
         (expected (edit-canonical-copy
                    (edit-canonical-document document))))
    (edit-canonical-set-children
     expected nil
     (append
      (getf expected :children)
      (mapcar
       (lambda (subtree)
         (let ((fragment
                 (parse-org subtree
                            :limits (document-limits document)
                            :settings (document-settings document))))
           (edit-canonical-node (bibtex-first-headline fragment))))
       subtrees)))
    (edit-apply-patches-result
     document
     (list (edit-make-patch offset offset replacement))
     :target nil
     :expected expected)))

(defun bibtex-resolve-field-groups (groups value-function)
  (let ((fields nil))
    (dolist (group groups)
      (dolist (field group)
        (let ((value (funcall value-function field)))
          (when (stringp value)
            (push (cons field value) fields)))))
    (nreverse fields)))

(defun bibtex-export-error (reason message &optional context)
  (error 'org-bibtex-error
         :message message
         :reason reason
         :context (and context (bounded-text context))))

(defun bibtex-export-check-output (document output)
  (let ((limits (document-limits document)))
    (check-limit limits :max-output-length (length output))
    (check-limit limits
                 :max-output-octets
                 (length
                  (cl-codec-kit:string-to-octets
                   output :encoding (or (document-encoding document) :utf-8))))
    output))

(defun bibtex-export (document heading &key prefix)
  "Export one Org HEADING's properties as a BibTeX entry string."
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document))
  (unless (document-root document)
    (bibtex-export-error :invalid-document
                         "BibTeX export requires a parsed Org document"))
  (let* ((prefix (bibtex-prefix-string prefix nil nil))
         (headline (edit-resolve-node document heading '(:headline)))
         (type-value (bibtex-headline-property
                      headline (bibtex-property-key prefix "btype")))
         (key (bibtex-headline-property headline "CUSTOM_ID")))
    (unless (bibtex-value-present-p type-value)
      (bibtex-export-error :missing-type
                           "Org heading is missing its BibTeX type"))
    (unless (bibtex-value-present-p key)
      (bibtex-export-error :missing-key
                           "Org heading is missing its BibTeX key"))
    (let* ((type-name (string-downcase (string-trim '(#\Space #\Tab) type-value)))
           (type-definition (bibtex-type-definition type-name)))
      (unless type-definition
        (bibtex-export-error :unknown-type "Unknown BibTeX entry type" type-value))
      (let* ((property-title
               (bibtex-headline-property
                headline (bibtex-property-key prefix "title")))
             (heading-title (node-property headline :raw-value))
             (title (if property-title property-title heading-title))
             (value-function
               (lambda (field)
                 (let ((value
                         (if (string= field "title")
                             title
                             (bibtex-headline-property
                              headline (bibtex-property-key prefix field)))))
                   (if (stringp value)
                       (string-trim '(#\Space #\Tab #\Newline #\Return) value)
                       value))))
             (required-fields
               (bibtex-resolve-field-groups
                (second type-definition) value-function))
             (optional-fields
               (bibtex-resolve-field-groups
                (third type-definition) value-function))
             (fields (append required-fields optional-fields)))
        (bibtex-export-check-output
         document
         (with-output-to-string (stream)
           (format stream "@~A{~A,~%" type-name key)
           (loop for (field . value) in fields
                 for first-p = t then nil
                 do (unless first-p
                      (format stream ",~%"))
                    (format stream "  ~A={~A}" field value))
           (terpri stream)
           (format stream "}~%")))))))

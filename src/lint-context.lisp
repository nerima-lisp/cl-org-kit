(in-package #:org-kit)

(defun %lint-context-node-position (node)
  (or (node-post-affiliated node) (node-begin node)))

(defun %lint-context-node-report (document checker node message &optional position)
  (let ((report (%lint-report document checker
                              (or position (%lint-context-node-position node))
                              message)))
    (when report (list report))))

(defun %lint-context-keyword-p (node key)
  (and (eq (node-type node) :keyword)
       (string= (node-property node :key "") key)))

(defun %lint-context-trim-horizontal (string)
  (string-trim '(#\Space #\Tab #\Return #\Newline) string))

(defun %lint-context-strip-quotes (string)
  (let ((value (%lint-context-trim-horizontal string)))
    (if (and (>= (length value) 2)
             (char= (char value 0) #\")
             (char= (char value (1- (length value))) #\"))
        (subseq value 1 (1- (length value)))
        value)))

(defun %lint-context-remote-path-p (path)
  (let ((colon (position #\: path)))
    (or (and (>= (length path) 2)
             (char= (char path 0) #\/)
             (char= (char path 1) #\/))
        (and colon
             (> colon 0)
             (alpha-char-p (char path 0))
             (loop for index below colon
                   always (or (alphanumericp (char path index))
                              (find (char path index) '(#\+ #\- #\.))))
             (not (and (= colon 1) (alpha-char-p (char path 0))))))))

(defun %lint-context-file-part (path)
  (let ((search (search "::" path)))
    (if search (subseq path 0 search) path)))

(defun %lint-context-babel-name (name)
  (let ((trimmed (%lint-context-trim-horizontal name)))
    (string-downcase (if (and (plusp (length trimmed))
                              (char= (char trimmed 0) #\:))
                         (subseq trimmed 1)
                         trimmed))))

(defun %lint-context-source-property-key (document node)
  (let* ((text (document-text document))
         (start (node-begin node))
         (end (or (position #\Newline text :start start :end (node-end node))
                  (node-end node)))
         (line (subseq text start end))
         (separator (position #\: line :start 1)))
    (string-upcase (if separator (subseq line 1 separator) (node-property node :key "")))))

(defun %lint-deprecated-header-syntax (document context)
  (let ((arguments
          (remove "dir"
                  (mapcar #'%lint-context-babel-name
                          (%lint-context-value context :babel-header-arguments))
                  :test #'string=)))
    (loop for node in (%lint-nodes document)
          append
          (cond
            ((%lint-context-keyword-p node "PROPERTY")
             (let* ((value (node-property node :value ""))
                    (argument
                      (find-if
                       (lambda (name)
                         (and (<= (length name) (length value))
                              (string= name value :end2 (length name))
                              (< (length name) (length value))
                              (find (char value (length name)) '(#\Space #\Tab))))
                       arguments)))
               (when argument
                 (let ((report
                         (%lint-report
                          document "deprecated-header-syntax"
                          (%lint-context-node-position node)
                          (format nil
                                  "Deprecated syntax for \"~A\".  Use header-args instead"
                                  argument))))
                   (when report (list report))))))
            ((and (eq (node-type node) :node-property)
                  (member (%lint-context-babel-name
                           (node-property node :key ""))
                          arguments :test #'string-equal))
             (let* ((key (%lint-context-source-property-key document node))
                    (report (%lint-report
                             document "deprecated-header-syntax"
                             (%lint-context-node-position node)
                             (format nil
                                     "Deprecated syntax for \"~A\".  Use :header-args: instead"
                                     key))))
               (when report (list report))))
            (t nil)))))

(defun %lint-suspicious-language-in-src-block (document context)
  (let ((languages (%lint-context-value context :source-languages)))
    (loop for node in (%lint-nodes document :src-block)
          for language = (node-property node :language)
          when (and language
                    (not (member language languages :test #'string-equal)))
            append (%lint-context-node-report
                    document "suspicious-language-in-src-block" node
                    (format nil "Unknown source block language: '~A'" language)))))

(defun %lint-invalid-id-link (document context)
  (let ((known-ids (%lint-context-value context :known-ids)))
    (loop for node in (%lint-nodes document :link)
          for type = (node-property node :type "")
          for path = (node-property node :path)
          for id = (cond
                     ((string-equal type "id") path)
                     ((and (stringp path)
                           (<= 3 (length path))
                           (string-equal "id:" path :end2 3))
                      (subseq path 3)))
          when (and id (not (member id known-ids :test (function string=))))
            append (%lint-context-node-report
                    document "invalid-id-link" node
                    (format nil "Unknown ID ~S" id)))))

(defun %lint-link-to-local-file (document context)
  (let ((exists-p (%lint-context-value context :file-exists-p))
        (attachment-exists-p
          (%lint-context-value context :attachment-file-exists-p)))
    (loop for node in (%lint-nodes document :link)
          for type = (node-property node :type "")
          for path = (node-property node :path)
          when (and path
                    (member type '("file" "attachment") :test #'string=))
            append
            (let* ((file (%lint-context-file-part path))
                   (remote-p (and (string= type "file")
                                  (%lint-context-remote-path-p file)))
                   (known-p nil)
                   (found-p nil)
                   (reported-file file))
              (unless remote-p
                (cond
                  ((string= type "file")
                   (setf known-p t
                         found-p (funcall exists-p file)))
                  ((functionp attachment-exists-p)
                   (multiple-value-bind (exists resolved-file)
                       (funcall attachment-exists-p file node)
                     (setf known-p t
                           found-p exists)
                     (when (stringp resolved-file)
                       (setf reported-file resolved-file))))))
              (when (and known-p (not found-p))
                (let* ((description-link-p
                         (loop for parent = (node-parent node) then (node-parent parent)
                               while parent
                               thereis (eq (node-type parent) :link)))
                       (message
                         (format nil
                                 (if description-link-p
                                     "Link to non-existent image file ~S in description"
                                     "Link to non-existent local file ~S")
                                 reported-file)))
                  (%lint-context-node-report
                   document "link-to-local-file" node message)))))))

(defun %lint-non-existent-setupfile-parameter (document context)
  (let ((exists-p (%lint-context-value context :file-exists-p)))
    (loop for node in (%lint-nodes document :keyword)
          when (%lint-context-keyword-p node "SETUPFILE")
            append
            (let ((file (%lint-context-strip-quotes
                         (node-property node :value ""))))
              (when (and (plusp (length file))
                         (not (%lint-context-remote-path-p file))
                         (not (funcall exists-p file)))
                (%lint-context-node-report
                 document "non-existent-setupfile-parameter" node
                 (format nil "Non-existent setup file ~S" file)))))))

(defun %lint-include-path (value)
  (let ((trimmed (%lint-context-trim-horizontal value)))
    (when (plusp (length trimmed))
      (if (char= (char trimmed 0) #\")
          (let ((close (position #\" trimmed :start 1)))
            (when (and close (> close 1))
              (subseq trimmed 1 close)))
          (let ((end (position-if (lambda (character)
                                   (find character '(#\Space #\Tab)))
                                 trimmed)))
            (subseq trimmed 0 end))))))

(defun %lint-include-search-part (path)
  (let ((separator (search "::" path)))
    (when separator
      (let ((search-part (%lint-context-trim-horizontal
                          (subseq path (+ separator 2)))))
        (when (plusp (length search-part)) search-part)))))

(defun %lint-wrong-include-link-parameter (document context)
  (let ((exists-p (%lint-context-value context :file-exists-p))
        (target-p (%lint-context-value context :include-target-p))
        (target-check-p (%lint-context-present-p context :include-target-p)))
    (loop for node in (%lint-nodes document :keyword)
          when (%lint-context-keyword-p node "INCLUDE")
            append
            (let* ((path (%lint-include-path (node-property node :value "")))
                   (file (and path (%lint-context-file-part path)))
                   (search-part (and path (%lint-include-search-part path))))
              (cond
                ((null path)
                 (%lint-context-node-report
                  document "wrong-include-link-parameter" node
                  "Missing location argument in INCLUDE keyword"))
                ((or (zerop (length file))
                     (%lint-context-remote-path-p file))
                 nil)
                ((not (funcall exists-p file))
                 (%lint-context-node-report
                  document "wrong-include-link-parameter" node
                  "Non-existent file argument in INCLUDE keyword"))
                ((and search-part target-check-p
                      (not (funcall target-p file search-part)))
                 (%lint-context-node-report
                  document "wrong-include-link-parameter" node
                  (format nil "Invalid search part ~S in INCLUDE keyword"
                          search-part)))
                (t nil))))))

(defun %lint-export-option-name (entry)
  (let ((separator (position #\: entry)))
    (if separator (subseq entry (1+ separator)) entry)))

(defun %lint-options-items (value)
  (let ((items '())
        (start 0))
    (loop while (< start (length value))
          do (let* ((end (position-if (lambda (character)
                                        (find character '(#\Space #\Tab)))
                                      value :start start))
                    (token (subseq value start end))
                    (separator (position #\: token)))
               (when (and separator (plusp separator))
                 (push (cons (subseq token 0 separator)
                             (subseq token (1+ separator)))
                       items))
               (if end
                   (setf start (1+ end))
                   (return))))
    (nreverse items)))

(defun %lint-unknown-options-item (document context)
  (let ((allowed (mapcar #'%lint-export-option-name
                         (%lint-context-value context :export-options))))
    (loop for node in (%lint-nodes document :keyword)
          when (%lint-context-keyword-p node "OPTIONS")
            append
            (loop for (item . value) in
                    (%lint-options-items (node-property node :value ""))
                  append
                  (append
                   (unless (member item allowed :test #'string=)
                     (%lint-context-node-report
                      document "unknown-options-item" node
                      (format nil "Unknown OPTIONS item \"~A\"" item)))
                   (when (zerop (length value))
                     (%lint-context-node-report
                      document "unknown-options-item" node
                      (format nil "Missing value for option item ~S" item))))))))

(defparameter +lint-context-global-export-options+
  '("AUTHOR" "CREATOR" "DATE" "EMAIL" "EXCLUDE_TAGS" "LANGUAGE"
    "SELECT_TAGS" "TITLE" "CITE_EXPORT"))

(defun %lint-export-option-entry (entry)
  (let ((separator (position #\: entry)))
    (if separator
        (values (subseq entry (1+ separator)) (subseq entry 0 separator))
        (values entry nil))))

(defun %lint-misspelled-export-option (document context)
  (let ((options (%lint-context-value context :export-options)))
    (loop for node in (%lint-nodes document :node-property)
          for property = (node-property node :key "")
          append
          (let ((entry
                  (find-if (lambda (item)
                             (multiple-value-bind (name backend)
                                 (%lint-export-option-entry item)
                               (declare (ignore backend))
                               (string-equal name property)))
                           options)))
            (when (and entry
                       (not (member property
                                    '("ARCHIVE" "CATEGORY" "SUMMARY" "DESCRIPTION"
                                      "CUSTOM_ID" "LOCATION" "LOGGING" "COLUMNS"
                                      "VISIBILITY" "TABLE_EXPORT_FORMAT"
                                      "TABLE_EXPORT_FILE" "EXPORT_OPTIONS"
                                      "EXPORT_TEXT" "EXPORT_FILE_NAME"
                                      "EXPORT_TITLE" "EXPORT_AUTHOR" "EXPORT_DATE"
                                      "UNNUMBERED" "ORDERED" "NOBLOCKING"
                                      "COOKIE_DATA" "LOG_INTO_DRAWER"
                                      "REPEAT_TO_STATE" "CLOCK_MODELINE_TOTAL"
                                      "STYLE" "HTML_CONTAINER_CLASS"
                                      "ORG-IMAGE-ACTUAL-WIDTH")
                                    :test #'string-equal)))
              (multiple-value-bind (name backend)
                  (%lint-export-option-entry entry)
                (declare (ignore name))
                (let* ((global-p (member property
                                         +lint-context-global-export-options+
                                         :test #'string-equal))
                       (backend-description
                         (if backend
                             (format nil " in ~A export backend"
                                     (string-downcase backend))
                             ""))
                       (prefix (if global-p "global " ""))
                       (message
                         (format nil
                                 "Potentially misspelled ~Aexport option \"~A\"~A.  Consider \"EXPORT_~A\"."
                                 prefix property backend-description property)))
                  (%lint-context-node-report
                   document "misspelled-export-option" node message))))))))

(defun %lint-placeholder-numbers (template)
  (loop with index = 0
        with numbers = '()
        while (< index (length template))
        do (if (and (char= (char template index) #\$)
                    (< (1+ index) (length template))
                    (digit-char-p (char template (1+ index)))
                    (not (char= (char template (1+ index)) #\0)))
               (let ((start (1+ index)))
                 (setf index start)
                 (loop while (and (< index (length template))
                                  (digit-char-p (char template index)))
                       do (incf index))
                 (push (parse-integer template :start start :end index) numbers))
               (incf index))
        finally (return (sort (remove-duplicates numbers :test #'=) #'<))))

(defun %lint-template-arity (template)
  (let ((numbers (%lint-placeholder-numbers template)))
    (if numbers
        (cons (apply #'max numbers) (apply #'max numbers))
        '(0 . 0))))

(defun %lint-macro-keyword-data (node)
  (let* ((value (%lint-context-trim-horizontal (node-property node :value "")))
         (separator (position-if (lambda (character)
                                   (find character '(#\Space #\Tab)))
                                 value)))
    (if separator
        (values (subseq value 0 separator)
                (%lint-context-trim-horizontal (subseq value separator)))
        (values (and (plusp (length value)) value) ""))))

(defun %lint-macro-entry (entries name)
  (let ((entry (find name entries :key #'car :test #'string-equal)))
    (when entry (cdr entry))))

(defun %lint-macro-definition-template (document context name)
  (or (loop for node in (%lint-nodes document :keyword)
            when (%lint-context-keyword-p node "MACRO")
              do (multiple-value-bind (defined-name template)
                     (%lint-macro-keyword-data node)
                   (when (and defined-name (string-equal defined-name name)
                              (plusp (length template)))
                     (return template))))
      (%lint-macro-entry (%lint-context-value context :macro-definitions) name)
      (%lint-macro-entry (%lint-context-value context :macro-templates) name)))

(defun %lint-macro-name-arity (name template)
  (cond
    ((member name '("keyword" "time") :test #'string-equal) '(1 . 1))
    ((string-equal name "modification-time") '(1 . 2))
    ((member name '("n" "property") :test #'string-equal) '(0 . 2))
    (t (%lint-template-arity template))))

(defun %lint-macro-arity-message (name args arity)
  (let ((actual (length args))
        (minimum (car arity))
        (maximum (cdr arity)))
    (cond
      ((< actual (1- minimum))
       (format nil "Missing arguments in macro ~S" name))
      ((< actual minimum)
       (format nil "Missing argument in macro ~S" name))
      ((> actual (1+ maximum))
       (format nil "Spurious arguments in macro ~S: ~{~A~^, ~}"
               name (subseq args maximum)))
      ((> actual maximum)
       (format nil "Spurious argument in macro ~S: ~A"
               name (car (last args)))))))

(defun %lint-macro-template-reports (document)
  (loop for node in (%lint-nodes document :keyword)
        when (%lint-context-keyword-p node "MACRO")
          append
          (multiple-value-bind (name template) (%lint-macro-keyword-data node)
            (cond
              ((null name)
               (%lint-context-node-report
                document "invalid-macro-argument-and-template" node
                "Missing name in MACRO keyword"))
              ((zerop (length template))
               (%lint-context-node-report
                document "invalid-macro-argument-and-template" node
                "Missing template in macro \"%s\""))
              ((not (equal (%lint-placeholder-numbers template)
                           (loop for number from 1 to
                                 (or (car (last (%lint-placeholder-numbers template))) 0)
                                 collect number)))
               (%lint-context-node-report
                document "invalid-macro-argument-and-template" node
                (format nil "Unused placeholders in macro \"~A\"" name)))))))

(defun %lint-macro-call-reports (document context)
  (let ((built-ins '(("author" . "$1") ("date" . "$1") ("email" . "$1")
                     ("title" . "$1") ("results" . "$1"))))
    (loop for node in (%lint-nodes document :macro)
          for name = (node-property node :key "")
          for template = (or (cdr (assoc name built-ins :test #'string-equal))
                             (%lint-macro-definition-template document context name))
          for args = (node-property node :args)
          append
          (if (null template)
              (%lint-context-node-report
               document "invalid-macro-argument-and-template" node
               (format nil "Undefined macro ~S" name))
              (let ((message (%lint-macro-arity-message
                              name args (%lint-macro-name-arity name template))))
                (when message
                  (%lint-context-node-report
                   document "invalid-macro-argument-and-template" node message)))))))

(defun %lint-invalid-macro-argument-and-template (document context)
  (append (%lint-macro-template-reports document)
          (%lint-macro-call-reports document context)))

(defun %lint-non-existent-bibliography (document context)
  (let ((exists-p (%lint-context-value context :file-exists-p)))
    (loop for node in (%lint-nodes document :keyword)
          when (%lint-context-keyword-p node "BIBLIOGRAPHY")
            append
            (let ((file (%lint-context-strip-quotes
                         (node-property node :value ""))))
              (when (and (plusp (length file))
                         (not (%lint-context-remote-path-p file))
                         (not (funcall exists-p file)))
                (%lint-context-node-report
                 document "non-existent-bibliography" node
                 (format nil "Non-existent bibliography ~S" file)))))))

(defun %lint-cite-declaration-tokens (value)
  (loop with index = 0
        with tokens = '()
        with valid = t
        while (< index (length value))
        do (loop while (and (< index (length value))
                            (find (char value index) '(#\Space #\Tab)))
                 do (incf index))
           (when (< index (length value))
             (if (char= (char value index) #\")
                 (let ((close (position #\" value :start (1+ index))))
                   (if close
                       (progn
                         (push (cons (subseq value (1+ index) close) t) tokens)
                         (setf index (1+ close)))
                       (setf valid nil index (length value))))
                 (let ((start index))
                   (loop while (and (< index (length value))
                                    (not (find (char value index)
                                               '(#\Space #\Tab))))
                         do (incf index))
                   (push (cons (subseq value start index) nil) tokens))))
        finally (return (values (reverse tokens)
                                (and valid (<= (length tokens) 3))))))

(defun %lint-symbol-name-p (name)
  (and (plusp (length name))
       (alpha-char-p (char name 0))
       (every (lambda (character)
                (or (alphanumericp character)
                    (find character '(#\- #\+ #\* #\? #\! #\_))))
              name)))

(defun %lint-invalid-cite-export-declaration (document context)
  (let ((processors (%lint-context-value context :cite-export-processors)))
    (loop for node in (%lint-nodes document :keyword)
          when (%lint-context-keyword-p node "CITE_EXPORT")
            append
            (let ((value (node-property node :value "")))
              (cond
                ((zerop (length value))
                 (%lint-context-node-report
                  document "invalid-cite-export-declaration" node
                  "Missing export processor name"))
                (t
                 (multiple-value-bind (tokens valid-p)
                     (%lint-cite-declaration-tokens value)
                   (let* ((first-token (first tokens))
                          (name (car first-token)))
                     (cond
                       ((or (not valid-p) (null first-token)
                            (cdr first-token)
                            (not (%lint-symbol-name-p name)))
                        (%lint-context-node-report
                         document "invalid-cite-export-declaration" node
                         "Invalid cite export processor declaration"))
                       ((not (member name processors :test #'string=))
                        (%lint-context-node-report
                         document "invalid-cite-export-declaration" node
                         (format nil "Unknown cite export processor ~A" name)))
                       (t nil))))))))))

(defun %lint-beamer-frame (document context)
  (let* ((environment (%lint-context-value context :beamer-frame-environment))
         (text (document-text document))
         (prefixes (list (format nil "~Cbegin{~A}" #\\ environment)
                         (format nil "~Cend{~A}" #\\ environment)))
         (message
           "Beamer frame name may cause error when exporting.  Consider customizing `org-beamer-frame-environment'."))
    (loop for prefix in prefixes
          append
          (loop with start = 0
                for position = (search prefix text :start2 start)
                while position
                collect (%lint-report document "beamer-frame" position message)
                do (setf start (+ position (length prefix)))))))

(defun %lint-context-proper-list-p (value)
  (and (listp value) (ignore-errors (list-length value))))

(defun %lint-context-header-specification-entry-p (entry)
  (and (consp entry)
       (stringp (car entry))
       (let ((allowed-values (cdr entry)))
         (or (null allowed-values)
             (eq allowed-values :any)
             (and (%lint-context-proper-list-p allowed-values)
                  (every
                   (lambda (group)
                     (and (%lint-context-proper-list-p group)
                          (every (lambda (value)
                                   (or (stringp value) (eq value :any)))
                                 group)))
                   allowed-values))))))

(defun %lint-context-header-specification-list-p (specifications)
  (and (%lint-context-proper-list-p specifications)
       (every #'%lint-context-header-specification-entry-p specifications)))

(defun %lint-context-header-specification-data (context)
  (let ((specifications
          (%lint-context-value context :babel-header-specifications)))
    (when (and (%lint-valid-context-p specifications)
               (eq (getf specifications :complete) t)
               (%lint-context-header-specification-list-p
                (getf specifications :common))
               (%lint-context-proper-list-p
                (getf specifications :languages))
               (every
                (lambda (entry)
                  (and (consp entry)
                       (stringp (car entry))
                       (%lint-context-header-specification-list-p
                        (cdr entry))))
                (getf specifications :languages)))
      specifications)))

(defun %lint-context-header-language-specifications (specifications language)
  (cdr (assoc language (getf specifications :languages)
              :test #'string-equal)))

(defun %lint-context-header-allowed-names (specifications language)
  (let ((language-specifications (getf specifications :languages)))
    (append
     (getf specifications :common)
     (if language
         (%lint-context-header-language-specifications
          specifications language)
         (loop for entry in language-specifications append (cdr entry))))))

(defun %lint-context-header-argument-segments (text)
  (let ((position 0)
        (segments '()))
    (dolist (header-start (babel-header-argument-starts text))
      (push (subseq text position (car header-start)) segments)
      (setf position (car header-start)))
    (push (subseq text position) segments)
    (nreverse segments)))

(defun %lint-context-header-argument-entries (text &optional skip-prefix-p)
  (loop for segment in (%lint-context-header-argument-segments text)
        for first-segment-p = t then nil
        for trimmed = (%lint-context-trim-horizontal segment)
        unless (or (zerop (length trimmed))
                   (and first-segment-p skip-prefix-p))
          append
          (if (char= (char trimmed 0) #\:)
              (loop for (argument-name . value)
                      in (babel-parse-header-arguments trimmed)
                    for (start . name-end)
                      in (babel-header-argument-starts trimmed)
                    for original-name =
                      (subseq trimmed (1+ start) name-end)
                    collect (list (concatenate 'string ":" original-name)
                                  argument-name value nil))
              (let* ((separator
                       (position-if #'babel-whitespace-p trimmed))
                     (name (subseq trimmed 0 separator))
                     (value
                       (and separator
                            (%lint-context-trim-horizontal
                             (subseq trimmed separator)))))
                (list (list (string-upcase name)
                            (string-upcase name)
                            (and value (plusp (length value)) value)
                            t))))))

(defun %lint-context-header-property-language (property)
  (let* ((trimmed (%lint-context-trim-horizontal property))
         (end (length trimmed))
         (base (if (and (plusp end)
                        (char= (char trimmed (1- end)) #\+))
                   (subseq trimmed 0 (1- end))
                   trimmed))
         (prefix "header-args:"))
    (cond
      ((string-equal base "header-args")
       (values t nil))
      ((and (> (length base) (length prefix))
            (string-equal base prefix :end1 (length prefix)))
       (values t (subseq base (length prefix))))
      (t (values nil nil)))))

(defun %lint-context-header-property-data (property-value)
  (let* ((value (%lint-context-trim-horizontal property-value))
         (separator (position-if #'babel-whitespace-p value))
         (property (subseq value 0 separator))
         (arguments (if separator
                        (%lint-context-trim-horizontal
                         (subseq value separator))
                        "")))
    (multiple-value-bind (header-args-p language)
        (%lint-context-header-property-language property)
      (when header-args-p
        (list language arguments nil)))))

(defun %lint-context-babel-header-texts (document node)
  (labels ((source-parameter-text (text)
             (when (stringp text)
               (let ((length (length text))
                     (position 0))
                 (labels ((horizontal-whitespace-p (position)
                            (and (< position length)
                                 (babel-whitespace-p (char text position))))
                          (skip-horizontal-whitespace (position)
                            (loop while (horizontal-whitespace-p position)
                                  do (incf position)
                                  finally (return position)))
                          (valid-switch-end-p (position)
                            (or (= position length)
                                (horizontal-whitespace-p position)))
                          (source-switch-end (start)
                            (when (and (< (1+ start) length)
                                       (find (char text start) '(#\- #\+)))
                              (let ((marker (char text start))
                                    (option (char text (1+ start))))
                                (cond
                                  ((char= option #\n)
                                   (let ((digits-start
                                           (skip-horizontal-whitespace
                                            (+ start 2))))
                                     (if (and (< digits-start length)
                                              (digit-char-p
                                               (char text digits-start)))
                                         (let ((end
                                                 (loop for index
                                                         from digits-start below length
                                                       while (digit-char-p
                                                              (char text index))
                                                       finally (return index))))
                                           (and (valid-switch-end-p end) end))
                                         (and (valid-switch-end-p (+ start 2))
                                              (+ start 2)))))
                                  ((and (char= marker #\-)
                                        (find option '(#\i #\k #\r)))
                                   (let ((end (+ start 2)))
                                     (and (valid-switch-end-p end) end)))
                                  ((and (char= marker #\-)
                                        (char= option #\l)
                                        (< (+ start 3) length)
                                        (char= (char text (+ start 2)) #\Space)
                                        (char= (char text (+ start 3))
                                               #\"))
                                   (let ((quote-end
                                           (position #\"
                                                     text
                                                     :start (+ start 4))))
                                     (when (and quote-end
                                                (> quote-end (+ start 4)))
                                       (let ((end (1+ quote-end)))
                                         (and (valid-switch-end-p end)
                                              end))))))))))
                   (loop for start =
                           (skip-horizontal-whitespace position)
                         for end = (source-switch-end start)
                         while end
                         do (setf position end)
                         finally
                           (return
                             (%lint-context-trim-horizontal
                              (subseq text position)))))))))
    (let ((language (node-property node :language)))
      (case (node-type node)
        (:src-block
         (append
          (let ((parameters (node-property node :parameters)))
            (cond
              ((and (stringp language)
                    (plusp (length language))
                    (char= (char language 0) #\:))
               nil)
              ((stringp parameters)
               (list (list language parameters nil)))
              (t
               (let ((text
                       (source-parameter-text
                        (node-property node :switches))))
                 (when text
                   (list (list language text nil)))))))
          (loop for text in (%lint-b-affiliated-header-values document node)
                collect (list language text nil))))
        (:inline-src-block
         (let ((text (node-property node :parameters)))
           (when (stringp text)
             (list (list language text nil)))))
        ((:babel-call :inline-babel-call)
         (loop for key in '(:inside-header :end-header)
               for text = (node-property node key)
               when (stringp text)
                 collect (list nil text nil)))
        (:keyword
         (when (%lint-context-keyword-p node "PROPERTY")
           (let ((data (%lint-context-header-property-data
                        (node-property node :value ""))))
             (when data (list data)))))
        (:node-property
         (multiple-value-bind (property-value present-p)
             (node-property node :key "")
           (declare (ignore present-p))
           (multiple-value-bind (header-args-p property-language)
               (%lint-context-header-property-language property-value)
             (when header-args-p
               (list
                (list property-language
                      (node-property node :value "")
                      nil))))))))))

(defun %lint-wrong-header-argument (document context)
  (let ((specifications (%lint-context-header-specification-data context)))
    (when specifications
      (nreverse
       (loop for node in (%lint-nodes document)
             append
             (loop for (language text skip-prefix-p)
                     in (%lint-context-babel-header-texts document node)
                   for allowed-names =
                     (%lint-context-header-allowed-names
                      specifications language)
                   append
                   (loop for entry
                           in (%lint-context-header-argument-entries
                               text skip-prefix-p)
                         for name = (first entry)
                         for argument-name = (second entry)
                         for missing-colon-p = (fourth entry)
                         unless (or missing-colon-p
                                    (find argument-name allowed-names
                                          :key #'car :test #'string-equal))
                           append
                           (%lint-context-node-report
                            document "wrong-header-argument" node
                            (format nil "Unknown header argument \"~A\"" name))
                         when missing-colon-p
                           append
                           (%lint-context-node-report
                            document "wrong-header-argument" node
                            (format nil
                                    "Missing colon in header argument \"~A\""
                                    name)))))))))

(defun %lint-context-header-specification (specifications name)
  (find name specifications :key #'car :test #'string-equal))

(defun %lint-context-header-value-messages (header-name value allowed-values)
  (unless (or (null allowed-values) (eq allowed-values :any))
    (let ((groups-alist '())
          (messages '())
          (values (if (stringp value)
                      (or (babel-split-header-list value :whitespace)
                          (list nil))
                      (list value))))
      (dolist (header-value values)
        (let ((valid-p nil)
              (display-value
                (if (stringp header-value)
                    header-value
                    (if (null header-value)
                        "nil"
                        (format nil "~A" header-value)))))
          (catch 'header-value-finished
            (dolist (group allowed-values)
              (cond
                ((and (stringp header-value)
                      (some (lambda (allowed-value)
                              (and (stringp allowed-value)
                                   (string= header-value allowed-value)))
                            group))
                 (let ((previous (assoc group groups-alist :test #'equal)))
                   (if previous
                       (progn
                         (push
                          (format nil
                                  "Forbidden combination in header \"~A\": ~A, ~A"
                                  header-name (cdr previous) display-value)
                          messages)
                         (setf valid-p t)
                         (throw 'header-value-finished nil))
                       (progn
                         (push (cons group display-value) groups-alist)
                         (setf valid-p t)))))
                ((member :any group)
                 (push (cons group display-value) groups-alist)
                 (setf valid-p t)))))
          (unless valid-p
            (push
             (format nil "Unknown value \"~A\" for header \"~A\""
                     display-value header-name)
             messages))))
      (nreverse messages))))

(defun %lint-wrong-header-value (document context)
  (let ((specifications (%lint-context-header-specification-data context)))
    (when specifications
      (nreverse
       (loop for node in (%lint-nodes document)
             when (member (node-type node)
                          '(:src-block :inline-src-block :babel-call
                            :inline-babel-call)
                          :test #'eq)
               append
               (loop for (language text skip-prefix-p)
                       in (%lint-context-babel-header-texts document node)
                     for specific =
                       (and language
                            (%lint-context-header-language-specifications
                             specifications language))
                     for allowed-values =
                       (append specific (getf specifications :common))
                     append
                     (loop for entry
                             in (%lint-context-header-argument-entries
                                 text skip-prefix-p)
                           for name = (first entry)
                           for value = (third entry)
                           for argument-key = (subseq name 1)
                           for specification =
                             (%lint-context-header-specification
                              allowed-values argument-key)
                           when specification
                             append
                             (loop for message in
                                     (%lint-context-header-value-messages
                                      name value (cdr specification))
                                   append
                                   (%lint-context-node-report
                                    document "wrong-header-value" node
                                    message)))))))))

(defun %lint-context-named-result-origin (document result-node result-reference)
  (when (stringp result-reference)
    (let ((reference (%lint-context-trim-horizontal result-reference)))
      (if (plusp (length reference))
          (let ((matches
                  (loop for node in (%lint-nodes document :src-block)
                        when (string= (node-property node :name "") reference)
                          collect node)))
            (when (= (length matches) 1)
              (first matches)))
          (let* ((parent (node-parent result-node))
                 (siblings (and parent (node-children parent)))
                 (position (and siblings
                                (position result-node siblings :test #'eq))))
            (when (and position (plusp position))
              (nth (1- position) siblings)))))))

(defun %lint-context-effective-exports (entries source-block)
  (when (and (node-begin source-block)
             (%lint-context-proper-list-p entries))
    (let ((entry
            (find (node-begin source-block) entries
                  :key (lambda (item)
                         (and (consp item) (car item)))
                  :test #'eql)))
      (when (and entry (consp entry) (stringp (cdr entry)))
        (cdr entry)))))

(defun %lint-named-result (document context)
  (let ((effective-exports
          (%lint-context-value context :babel-effective-exports)))
    (loop for result-node in (%lint-nodes document)
          append
          (multiple-value-bind (results-value results-present-p)
              (node-property result-node :results)
            (multiple-value-bind (result-name name-present-p)
                (node-property result-node :name)
              (when (and results-present-p name-present-p
                         (stringp result-name))
                (let* ((source-block
                         (%lint-context-named-result-origin
                          document result-node results-value))
                       (exports
                         (and source-block
                              (eq (node-type source-block) :src-block)
                              (%lint-context-effective-exports
                               effective-exports source-block))))
                  (when (and exports
                             (not (member exports '("results" "both")
                                          :test #'string=)))
                    (let ((report
                            (%lint-report
                             document "named-result" (node-begin result-node)
                             (format nil
                                     "Links to \"~A\" will not be valid during export unless the parent source block has :exports results or both"
                                     result-name))))
                      (when report (list report)))))))))))

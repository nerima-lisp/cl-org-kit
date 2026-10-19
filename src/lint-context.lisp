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
  (let ((exists-p (%lint-context-value context :file-exists-p)))
    (loop for node in (%lint-nodes document :link)
          for type = (node-property node :type "")
          for path = (node-property node :path)
          when (and path
                    (member type '("file" "attachment") :test #'string=))
            append
            (let* ((file (%lint-context-file-part path))
                   (remote-p (%lint-context-remote-path-p file)))
              (when (and (not remote-p) (not (funcall exists-p file)))
                (let* ((description-link-p
                         (loop for parent = (node-parent node) then (node-parent parent)
                               while parent
                               thereis (eq (node-type parent) :link)))
                       (message
                         (format nil
                                 (if description-link-p
                                     "Link to non-existent image file ~S in description"
                                     "Link to non-existent local file ~S")
                                 file)))
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

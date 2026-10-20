;;;; src/link-resolution.lisp
(in-package #:org-kit)

(define-condition org-link-resolution-error (org-kit-error)
  ((reason :initarg :reason :initform nil)
   (context :initarg :context :initform nil))
  (:report (lambda (condition stream)
             (format stream "~A~@[ (~(~A~))~]~@[ near \"~A\"~]"
                     (org-kit-error-message condition)
                     (org-link-resolution-error-reason condition)
                     (org-link-resolution-error-context condition))))
  (:documentation "Signalled when a link resolution request cannot be handled.
REASON identifies the failure and CONTEXT is a bounded, escaped input excerpt."))

(defun org-link-resolution-error-reason (condition)
  "Return the keyword classifying the link-resolution failure."
  (slot-value condition 'reason))

(defun org-link-resolution-error-context (condition)
  "Return the bounded, escaped link or search option that caused the failure."
  (slot-value condition 'context))

(defstruct (link-resolution (:constructor %make-link-resolution-raw
                               (status target candidates))
                            (:conc-name %link-resolution-)
                            (:copier nil)
                            (:predicate %link-resolution-p))
  "A link resolution status, target and ordered candidate list."
  (status :unresolved :read-only t)
  (target nil :read-only t)
  (candidates nil :read-only t))

(defun make-link-resolution (&key (status :unresolved) target candidates)
  "Return a link resolution with STATUS, TARGET and CANDIDATES."
  (%make-link-resolution status target (copy-list candidates)))

(defun %make-link-resolution (status target candidates)
  (unless (member status '(:resolved :unresolved :ambiguous))
    (error 'type-error
           :datum status
           :expected-type '(member :resolved :unresolved :ambiguous)))
  (%make-link-resolution-raw status target candidates))

(defun link-resolution-p (object)
  "Return true when OBJECT is a link resolution."
  (%link-resolution-p object))

(defun link-resolution-status (resolution)
  "Return RESOLUTION's :RESOLVED, :UNRESOLVED or :AMBIGUOUS status."
  (%link-resolution-status resolution))

(defun link-resolution-target (resolution)
  "Return the target object stored in RESOLUTION."
  (%link-resolution-target resolution))

(defun link-resolution-candidates (resolution)
  "Return a fresh list of RESOLUTION's candidates in document order."
  (copy-list (%link-resolution-candidates resolution)))

(defun link-resolution-signal-error (reason context message)
  (error 'org-link-resolution-error
         :message message
         :reason reason
         :context (bounded-text (if (stringp context)
                                    context
                                    (princ-to-string context)))))

(defun link-resolution-candidates-result (candidates)
  (let ((count (length candidates)))
    (cond
      ((zerop count)
       (make-link-resolution :status :unresolved))
      ((= count 1)
       (make-link-resolution :status :resolved
                             :target (first candidates)
                             :candidates candidates))
      (t
       (make-link-resolution :status :ambiguous :candidates candidates)))))

(defun link-resolution-headline-p (node)
  (member (node-type node) '(:headline :inlinetask) :test #'eq))

(defun link-resolution-statistics-cookie-end (string start)
  (when (and (< start (length string))
             (char= (char string start) #\[))
    (let* ((digits-start (1+ start))
           (position digits-start))
      (loop while (and (< position (length string))
                       (digit-char-p (char string position)))
            do (incf position))
      (when (> position digits-start)
        (cond
          ((and (< position (length string))
                (char= (char string position) #\%))
           (incf position))
          ((and (< position (length string))
                (char= (char string position) #\/))
           (incf position)
           (loop while (and (< position (length string))
                            (digit-char-p (char string position)))
                 do (incf position)))
          (t (return-from link-resolution-statistics-cookie-end nil)))
        (when (and (< position (length string))
                   (char= (char string position) #\]))
          (1+ position))))))

(defun link-resolution-search-key (string &key statistics-cookies)
  (when (stringp string)
    (string-downcase
     (with-output-to-string (stream)
       (let ((pending-space nil)
             (written-p nil)
             (position 0))
         (loop while (< position (length string))
               for cookie-end = (and statistics-cookies
                                     (link-resolution-statistics-cookie-end
                                      string position))
               for character = (unless cookie-end (char string position))
               do (cond
                    (cookie-end
                     (setf pending-space t
                           position cookie-end))
                    ((find character '(#\Space #\Tab #\Newline #\Return))
                     (setf pending-space t)
                     (incf position))
                    (t
                     (when (and written-p pending-space)
                       (write-char #\Space stream))
                     (setf pending-space nil
                           written-p t)
                     (write-char character stream)
                     (incf position)))))))))

(defun link-resolution-index-add (table key node)
  (when (and (stringp key)
             (not (member node (gethash key table) :test #'eq)))
    (push node (gethash key table))))

(defun link-resolution-index-finish-table (table)
  (maphash (lambda (key candidates)
             (setf (gethash key table) (nreverse candidates)))
           table)
  table)

(defstruct (link-resolution-index
             (:constructor %make-link-resolution-index
                 (documents id-index custom-ids targets names headings radio-targets))
             (:conc-name %link-resolution-index-)
             (:copier nil)
             (:predicate %link-resolution-index-p))
  (documents nil :read-only t)
  (id-index nil :read-only t)
  (custom-ids nil :read-only t)
  (targets nil :read-only t)
  (names nil :read-only t)
  (headings nil :read-only t)
  (radio-targets nil :read-only t)
  (document-indices (make-hash-table :test #'eq))
  (search-work (make-hash-table :test #'eq)))

(defun make-link-resolution-index (documents)
  (let* ((documents (org-id-normalize-documents documents))
         (custom-ids (make-hash-table :test #'equal))
         (targets (make-hash-table :test #'equal))
         (names (make-hash-table :test #'equal))
         (headings (make-hash-table :test #'equal))
         (radio-targets (make-hash-table :test #'equal)))
    (dolist (document documents)
      (when (document-root document)
        (map-nodes
         (lambda (node)
           (let ((type (node-type node)))
             (when (link-resolution-headline-p node)
               (dolist (property
                        (effective-heading-property-entries node "CUSTOM_ID"))
                 (link-resolution-index-add
                  custom-ids (node-property property :value) node))
               (link-resolution-index-add
                headings
                (link-resolution-search-key (node-property node :raw-value)
                                            :statistics-cookies t)
                node))
             (link-resolution-index-add
              names (link-resolution-search-key (node-property node :name)) node)
             (case type
               (:target
                (link-resolution-index-add
                 targets (link-resolution-search-key (node-property node :value))
                 node))
               (:radio-target
                (link-resolution-index-add
                 radio-targets
                 (link-resolution-search-key (node-property node :value)) node)))))
         (document-root document))))
    (dolist (table (list custom-ids targets names headings radio-targets))
      (link-resolution-index-finish-table table))
    (%make-link-resolution-index
     documents (make-org-id-index documents) custom-ids targets names headings
     radio-targets)))

(defun link-resolution-index-for-document (index document)
  (if (member document (%link-resolution-index-documents index) :test #'eq)
      index
      (or (gethash document (%link-resolution-index-document-indices index))
          (setf (gethash document (%link-resolution-index-document-indices index))
                (make-link-resolution-index document)))))

(defun link-resolution-index-document-candidates (table document key)
  (remove-if-not (lambda (candidate)
                   (eq (node-document candidate) document))
                 (gethash key table)))

(defun link-resolution-custom-id-candidates (index custom-id)
  (and (stringp custom-id)
       (gethash custom-id (%link-resolution-index-custom-ids index))))

(defun link-resolution-fuzzy-candidates (index path)
  (when (stringp path)
    (let* ((starred (and (plusp (length path)) (char= (char path 0) #\*)))
           (heading (link-resolution-search-key
                     (if starred (subseq path 1) path)
                     :statistics-cookies t)))
      (if starred
          (gethash heading (%link-resolution-index-headings index))
          (or (gethash (link-resolution-search-key path)
                       (%link-resolution-index-targets index))
              (gethash (link-resolution-search-key path)
                       (%link-resolution-index-names index))
              (gethash heading (%link-resolution-index-headings index)))))))

(defun link-resolution-radio-candidates (index path)
  (and (stringp path)
       (gethash (link-resolution-search-key path)
                (%link-resolution-index-radio-targets index))))

(defun link-resolution-raw-bracket-path (link)
  (let* ((document (node-document link))
         (text (and document (document-text document)))
         (start (node-begin link))
         (end (node-end link)))
    (when (and text start end
               (eq (node-property link :format) :bracket)
               (<= (+ start 2) end (length text)))
      (let ((close (link-object-bracket-close text (+ start 2) end)))
        (when close
          (subseq text (+ start 2) close))))))

(defun link-resolution-check-function-form (link)
  (let* ((raw-path (link-resolution-raw-bracket-path link))
         (separator (and raw-path (position #\: raw-path)))
         (key (and separator (subseq raw-path 0 separator)))
         (document (node-document link))
         (abbreviations (and document
                             (getf (document-settings document) :link-abbrevs)))
         (entry (and key (listp abbreviations)
                     (assoc key abbreviations :test #'string=)))
         (replacement (and entry (cdr entry))))
    (when (and (stringp replacement) (search "%(" replacement))
      (link-resolution-signal-error
       :unsupported-link-function raw-path
       "Function forms in link abbreviations require an evaluator"))))

(defun link-resolution-file-components (path)
  (let ((separator (search "::" path)))
    (if separator
        (values (subseq path 0 separator) (subseq path (+ separator 2)) t)
        (values path nil nil))))

(defun link-resolution-read-file (link path file-resolver)
  (unless (stringp path)
    (link-resolution-signal-error :invalid-file-path path
                                  "A file link must have a path"))
  (unless (functionp file-resolver)
    (link-resolution-signal-error
     (if file-resolver :invalid-file-resolver :missing-file-resolver)
     path
     (if file-resolver
         "The file resolver must be a function"
         "A file resolver is required to resolve file links")))
  (let* ((parent-document (node-document link))
         (parent-source (and parent-document
                             (document-source-name parent-document)))
         (resolved (funcall file-resolver path parent-source)))
    (cond
      ((null resolved)
       (link-resolution-signal-error :file-not-found path
                                     "The file resolver returned no document"))
      ((document-p resolved) resolved)
      ((stringp resolved) (parse-org resolved :source-name path))
      (t
       (link-resolution-signal-error
        :invalid-file-resolver-result path
        "The file resolver must return a document or Org source string")))))

(defun link-resolution-search-values (node)
  (let ((type (node-type node)))
    (cond
      ((link-resolution-headline-p node)
       (list (node-property node :raw-value)))
      ((eq type :plain-text)
       (list (node-text node)))
      ((member type '(:src-block :example-block :fixed-width :special-block)
               :test #'eq)
       (list (or (node-property node :value) (node-text node))))
      (t nil))))

(defun link-resolution-search-text-matches-p (index document pattern value regex)
  (and (stringp value)
       (if regex
           (let* ((work (%link-resolution-index-search-work index))
                  (actual (+ (gethash document work 0) (length value))))
             (check-limit (document-limits document) :max-search-work actual)
             (setf (gethash document work) actual)
             (cl-regex-kit:is-match-p regex value))
           (search pattern value :test #'char-equal))))

(defun link-resolution-compile-search-regex (document option pattern)
  (let* ((limits (document-limits document))
         (pattern-limit (limit-value limits :max-search-regex-length))
         (depth-limit (limit-value limits :max-element-depth)))
    (check-limit limits :max-search-regex-length (length pattern))
    (handler-case
        (cl-regex-kit:compile-regex
         pattern
         :case-insensitive t
         :multi-line t
         :size-limit (max 1 pattern-limit)
         :nest-limit depth-limit)
      (cl-regex-kit:regex-syntax-error ()
        (link-resolution-signal-error :invalid-search-option option
                                      "Invalid regular expression in file search option")))))

(defun link-resolution-text-candidates (index documents option pattern regex-p)
  (let ((candidates nil))
    (dolist (document documents)
      (let ((root (document-root document))
            (regex nil))
        (when (and regex-p root)
          (setf regex (link-resolution-compile-search-regex document option pattern)))
        (when root
          (map-nodes
           (lambda (node)
             (when (some (lambda (value)
                           (link-resolution-search-text-matches-p
                            index document pattern value regex))
                         (link-resolution-search-values node))
               (push node candidates)))
           root))))
    (nreverse candidates)))

(defun link-resolution-target-search-candidates (index document name)
  (let* ((index (link-resolution-index-for-document index document))
         (key (link-resolution-search-key name)))
    (link-resolution-index-document-candidates
     (%link-resolution-index-targets index) document key)))

(defun link-resolution-file-search-candidates (index document option)
  (cond
    ((and (plusp (length option)) (char= (char option 0) #\*))
     (let* ((index (link-resolution-index-for-document index document))
            (key (link-resolution-search-key (subseq option 1)
                                             :statistics-cookies t)))
       (link-resolution-index-document-candidates
        (%link-resolution-index-headings index) document key)))
    ((and (plusp (length option)) (char= (char option 0) #\#))
     (let* ((index (link-resolution-index-for-document index document))
            (candidates
              (link-resolution-custom-id-candidates index (subseq option 1))))
       (remove-if-not (lambda (candidate)
                        (eq (node-document candidate) document))
                      candidates)))
    ((and (>= (length option) 4)
          (string= "<<" option :end2 2)
          (string= ">>" option :start2 (- (length option) 2)))
     (link-resolution-target-search-candidates
      index document (subseq option 2 (- (length option) 2))))
    ((and (>= (length option) 2)
          (char= (char option 0) #\/)
          (char= (char option (1- (length option))) #\/))
     (link-resolution-text-candidates
      index (list document) option (subseq option 1 (1- (length option))) t))
    (t
     (link-resolution-text-candidates
      index (list document) option option nil))))

(defun link-resolution-file (link path file-resolver index)
  (unless (stringp path)
    (link-resolution-signal-error :invalid-file-path path
                                  "A file link must have a path"))
  (multiple-value-bind (file-path search-option search-p)
      (link-resolution-file-components path)
    (let ((document
            (if (and search-p (zerop (length file-path)))
                (or (node-document link)
                    (link-resolution-signal-error
                     :missing-parent-document path
                     "A current-document file search needs an attached link"))
                (link-resolution-read-file link file-path file-resolver))))
      (if (and search-p (plusp (length search-option)))
          (link-resolution-candidates-result
           (link-resolution-file-search-candidates index document search-option))
          (make-link-resolution :status :resolved :target document
                                :candidates (list document))))))

(defun link-resolution-check-link (link)
  (unless (node-p link)
    (error 'type-error :datum link :expected-type 'node))
  (unless (eq (node-type link) :link)
    (link-resolution-signal-error :invalid-link-node (node-type link)
                                  "LINK must be a parsed :LINK node")))

(defun resolve-link-with-index (link index &key file-resolver)
  (link-resolution-check-link link)
  (unless (%link-resolution-index-p index)
    (error 'type-error :datum index :expected-type 'link-resolution-index))
  (let* ((type (node-property link :type))
         (path (node-property link :path)))
    (link-resolution-check-function-form link)
    (cond
      ((and (stringp type) (string-equal type "id"))
       (if (and (stringp path) (org-id-valid-value-p path))
           (let ((candidates
                   (org-id-index-candidates
                    (%link-resolution-index-id-index index) path)))
             (link-resolution-candidates-result candidates))
           (make-link-resolution :status :unresolved)))
      ((and (stringp type) (string-equal type "custom-id"))
       (link-resolution-candidates-result
        (link-resolution-custom-id-candidates index path)))
      ((and (stringp type) (string-equal type "fuzzy"))
       (link-resolution-candidates-result
        (link-resolution-fuzzy-candidates index path)))
      ((and (stringp type) (string-equal type "radio"))
       (link-resolution-candidates-result
        (link-resolution-radio-candidates index path)))
      ((and (stringp type) (string-equal type "file"))
       (link-resolution-file link path file-resolver index))
      (t (make-link-resolution :status :unresolved)))))

(defun resolve-link (link documents &key file-resolver)
  "Resolve parsed LINK against DOCUMENTS, retaining candidates in source order.
LINK must be a :LINK node. FILE-RESOLVER receives a file path and the source
name of LINK's document, and returns a document or Org source string. A whole-
file link targets that document. Fuzzy searches follow Org's target, name, then
heading order; a `*Heading` search checks headings only."
  (link-resolution-check-link link)
  (resolve-link-with-index link (make-link-resolution-index documents)
                           :file-resolver file-resolver))

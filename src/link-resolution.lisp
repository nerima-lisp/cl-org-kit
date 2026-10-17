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

(defstruct (link-resolution (:constructor %make-link-resolution
                               (status target candidates))
                            (:conc-name %link-resolution-)
                            (:copier nil)
                            (:predicate %link-resolution-p))
  "A link resolution status, target and ordered candidate list."
  (status :unresolved :type (member :resolved :unresolved :ambiguous)
          :read-only t)
  (target nil :read-only t)
  (candidates nil :read-only t))

(defun make-link-resolution (&key (status :unresolved) target candidates)
  "Return a link resolution with STATUS, TARGET and CANDIDATES."
  (check-type status (member :resolved :unresolved :ambiguous))
  (%make-link-resolution status target (copy-list candidates)))

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

(defun link-resolution-proper-list-length (value)
  (handler-case (list-length value)
    (type-error () nil)))

(defun link-resolution-normalize-documents (documents)
  (let ((result (if (document-p documents) (list documents) documents)))
    (unless (link-resolution-proper-list-length result)
      (error 'type-error :datum documents :expected-type '(or document list)))
    (dolist (document result)
      (unless (document-p document)
        (error 'type-error :datum document :expected-type 'document)))
    result))

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

(defun link-resolution-map-candidates (documents predicate)
  (let ((candidates nil))
    (dolist (document documents)
      (let ((root (document-root document)))
        (when root
          (map-nodes (lambda (node)
                       (when (funcall predicate document node)
                         (push node candidates)))
                     root))))
    (nreverse candidates)))

(defun link-resolution-headline-p (node)
  (member (node-type node) '(:headline :inlinetask) :test #'eq))

(defun link-resolution-custom-id (document node)
  (when (link-resolution-headline-p node)
    (effective-property document node "CUSTOM_ID")))

(defun link-resolution-match-string-p (left right)
  (and (stringp left) (stringp right) (string-equal left right)))

(defun link-resolution-custom-id-candidates (documents custom-id)
  (link-resolution-map-candidates
   documents
   (lambda (document node)
     (link-resolution-match-string-p
      custom-id (link-resolution-custom-id document node)))))

(defun link-resolution-fuzzy-candidates (documents path)
  (link-resolution-map-candidates
   documents
   (lambda (document node)
     (declare (ignore document))
     (or (and (link-resolution-headline-p node)
              (link-resolution-match-string-p
               path (node-property node :raw-value)))
         (link-resolution-match-string-p path (node-property node :name))
         (and (member (node-type node) '(:target :radio-target) :test #'eq)
              (link-resolution-match-string-p path (node-property node :value)))))))

(defun link-resolution-radio-candidates (documents path)
  (link-resolution-map-candidates
   documents
   (lambda (document node)
     (declare (ignore document))
     (and (eq (node-type node) :radio-target)
          (link-resolution-match-string-p path (node-property node :value))))))

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

(defun link-resolution-search-text-matches-p (pattern value regex)
  (and (stringp value)
       (if regex
           (cl-regex-kit:is-match-p regex value)
           (search pattern value :test #'char-equal))))

(defun link-resolution-compile-search-regex (document option pattern)
  (let* ((limits (document-limits document))
         (pattern-limit (limit-value limits :max-input-length))
         (depth-limit (limit-value limits :max-element-depth)))
    (check-limit limits :max-input-length (length pattern))
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

(defun link-resolution-text-candidates (documents option pattern regex-p)
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
                            pattern value regex))
                         (link-resolution-search-values node))
               (push node candidates)))
           root))))
    (nreverse candidates)))

(defun link-resolution-target-search-candidates (document name)
  (link-resolution-map-candidates
   (list document)
   (lambda (source node)
     (declare (ignore source))
     (and (member (node-type node) '(:target :radio-target) :test #'eq)
          (link-resolution-match-string-p name (node-property node :value))))))

(defun link-resolution-file-search-candidates (document option)
  (cond
    ((and (plusp (length option)) (char= (char option 0) #\*))
     (link-resolution-map-candidates
      (list document)
      (lambda (source node)
        (declare (ignore source))
        (and (link-resolution-headline-p node)
             (link-resolution-match-string-p
              (subseq option 1) (node-property node :raw-value))))))
    ((and (plusp (length option)) (char= (char option 0) #\#))
     (link-resolution-custom-id-candidates (list document) (subseq option 1)))
    ((and (>= (length option) 4)
          (string= "<<" option :end2 2)
          (string= ">>" option :start2 (- (length option) 2)))
     (link-resolution-target-search-candidates
      document (subseq option 2 (- (length option) 2))))
    ((and (>= (length option) 2)
          (char= (char option 0) #\/)
          (char= (char option (1- (length option))) #\/))
     (link-resolution-text-candidates
      (list document) option (subseq option 1 (1- (length option))) t))
    (t
     (link-resolution-text-candidates (list document) option option nil))))

(defun link-resolution-file (link path file-resolver)
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
           (link-resolution-file-search-candidates document search-option))
          (make-link-resolution :status :resolved :target document
                                :candidates (list document))))))

(defun resolve-link (link documents &key file-resolver)
  "Resolve parsed LINK against DOCUMENTS, retaining candidates in source order.
LINK must be a :LINK node. FILE-RESOLVER receives a file path and the source
name of LINK's document, and returns a document or Org source string. A whole-
file link targets that document. A `*Heading` file search matches the heading's
raw title case-insensitively."
  (unless (node-p link)
    (error 'type-error :datum link :expected-type 'node))
  (unless (eq (node-type link) :link)
    (link-resolution-signal-error :invalid-link-node (node-type link)
                                  "LINK must be a parsed :LINK node"))
  (let* ((documents (link-resolution-normalize-documents documents))
         (type (node-property link :type))
         (path (node-property link :path)))
    (link-resolution-check-function-form link)
    (cond
      ((and (stringp type) (string-equal type "id"))
       (if (and (stringp path) (org-id-valid-value-p path))
           (let* ((index (make-org-id-index documents))
                  (candidates (org-id-index-candidates index path)))
             (link-resolution-candidates-result candidates))
           (make-link-resolution :status :unresolved)))
      ((and (stringp type) (string-equal type "custom-id"))
       (link-resolution-candidates-result
        (if (stringp path)
            (link-resolution-custom-id-candidates documents path)
            nil)))
      ((and (stringp type) (string-equal type "fuzzy"))
       (link-resolution-candidates-result
        (if (stringp path)
            (link-resolution-fuzzy-candidates documents path)
            nil)))
      ((and (stringp type) (string-equal type "radio"))
       (link-resolution-candidates-result
        (if (stringp path)
            (link-resolution-radio-candidates documents path)
            nil)))
      ((and (stringp type) (string-equal type "file"))
       (link-resolution-file link path file-resolver))
      (t (make-link-resolution :status :unresolved)))))

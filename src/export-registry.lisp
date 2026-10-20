;;;; src/export-registry.lisp
(in-package #:org-kit)

(declaim (ftype function %export-backend-derived-p-unlocked
                          %export-backend-ancestry-unlocked
                          export-context-for-owner
                          export-resolved-filters-in-ancestry
                          export-render-preparation
                          export-render-view
                          export-render-node
                          export-render-native-sink-implementation
                          export-check-output-stage
                          export-explicit-url-scheme-end))

(defstruct (%export-backend-descriptor
            (:constructor %make-export-backend-descriptor)
            (:conc-name %export-backend-descriptor-)
            (:copier nil))
  (name nil :read-only t)
  (parent nil :read-only t)
  (renderer nil :read-only t)
  (preamble-renderer nil :read-only t)
  (trailer-renderer nil :read-only t)
  (template nil :read-only t)
  (make-state nil :read-only t)
  (document-class nil :read-only t)
  (transcoders nil :read-only t)
  (native-sink-renderer nil :read-only t)
  (native-sink-scratch nil :read-only t)
  (native-sink-string nil :read-only t)
  (native-sink-write-string nil :read-only t)
  (native-sink-append-node-result nil :read-only t)
  (native-sink-output-offset nil :read-only t)
  (node-policies nil :read-only t)
  (node-wrapper nil :read-only t)
  (option-specifications nil :read-only t)
  (accepted-keywords nil :read-only t)
  (retained-keywords nil :read-only t)
  (raw-output-options nil :read-only t)
  (export-block-names nil :read-only t)
  (export-snippet-names nil :read-only t)
  (link-type-handlers nil :read-only t)
  (filters nil :read-only t)
  (builtin-p nil :read-only t))

(defvar *export-backend-registry* (make-hash-table :test #'eq))
(defvar *export-backend-registry-lock*
  (sb-thread:make-mutex :name "cl-org-kit export backend registry"))
(defvar *export-builtin-link-type-handlers* (make-hash-table :test #'eq))
(defvar *export-registration-backend* nil)
(defvar *export-creating-state* nil)
(defvar *export-current-output-offset* 0)
(defvar *export-current-native-sink* nil)
(defparameter *export-common-accepted-keywords*
  '("TITLE" "AUTHOR" "DATE" "LANGUAGE" "OPTIONS" "SELECT_TAGS"
    "EXCLUDE_TAGS" "MACRO" "ATTR_HTML" "ARCHIVE" "COLUMNS"
    "PRIORITIES" "CONSTANTS" "EXPORT_FILE_NAME" "DESCRIPTION"
    "KEYWORDS" "SUBTITLE" "EMAIL" "CREATOR" "INCLUDE" "RESULTS"
    "NAME" "CAPTION" "HEADER" "CITE_EXPORT" "BIBLIOGRAPHY"
    "PRINT_BIBLIOGRAPHY"))
(defparameter *export-common-retained-keywords*
  '("CITE_EXPORT" "BIBLIOGRAPHY" "PRINT_BIBLIOGRAPHY"))
(defun export-common-node-policies ()
  '((:property-drawer . :prune-children)
    (:node-property . :prune-children)
    (:planning . :prune-children)
    (:clock . :prune-children)
    (:comment . :prune-children)
    (:comment-block . :prune-children)))
(defparameter *export-option-specifications*
  '(("toc" :toc :toc)
    ("h" :heading-depth :integer)
    ("num" :num :boolean-integer)
    ("^" :^ :sub-superscript)
    ("title" :title :boolean)
    ("author" :author :boolean)
    ("date" :date :boolean)
    ("\\n" :preserve-breaks :boolean)
    ("timestamp" :timestamp :boolean)
    ("arch" :arch :archive-mode)
    ("expand-links" :expand-links :boolean)
    ("broken-links" :broken-links :disabled-only)
    ("c" :clock :boolean)
    ("creator" :creator :creator-mode)
    ("d" :drawers :drawer-list)
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

(defstruct (%export-render-frame (:constructor %make-export-render-frame)
                                 (:conc-name %export-render-frame-)
                                 (:copier nil)
                                 (:predicate %export-render-frame-p))
  (mode :block :read-only t)
  (table-cell-p nil :read-only t)
  (preserve-source-breaks-p nil :read-only t)
  (output-offset 0 :read-only t))

(defstruct (%export-render-context (:constructor %make-export-render-context)
                                   (:conc-name %export-render-context-)
                                   (:copier nil)
                                   (:predicate %export-render-context-p))
  backend document metadata options limits root-views toc footnotes
  preparation raw-output-options extensions ancestry owner-backend state states)

(defstruct (export-link-result (:constructor %make-export-link-result)
                               (:conc-name %export-link-result-)
                               (:copier nil)
                               (:predicate %export-link-result-p))
  (kind nil :read-only t)
  (destination nil :read-only t)
  (description nil :read-only t))

(defun make-export-link-result (&key kind destination description)
  "Return a link result for a registered link handler."
  (unless (member kind '(:link :image) :test #'eq)
    (error 'type-error :datum kind :expected-type '(member :link :image)))
  (unless (stringp destination)
    (error 'type-error :datum destination :expected-type 'string))
  (unless (stringp description)
    (error 'type-error :datum description :expected-type 'string))
  (%make-export-link-result :kind kind :destination (copy-seq destination)
                            :description (copy-seq description)))

(defun export-link-result-kind (result)
  "Return the kind of RESULT: :link or :image."
  (unless (%export-link-result-p result)
    (error 'type-error :datum result :expected-type 'export-link-result))
  (%export-link-result-kind result))

(defun export-link-result-destination (result)
  "Return a fresh copy of the destination stored in RESULT."
  (unless (%export-link-result-p result)
    (error 'type-error :datum result :expected-type 'export-link-result))
  (copy-seq (%export-link-result-destination result)))

(defun export-link-result-description (result)
  "Return a fresh copy of the description stored in RESULT."
  (unless (%export-link-result-p result)
    (error 'type-error :datum result :expected-type 'export-link-result))
  (copy-seq (%export-link-result-description result)))

(defun export-registration-error (backend reason message)
  (error 'org-export-registration-error
         :message message :backend (or backend *export-registration-backend*)
         :reason reason))

(defun export-copy-registration-value (value)
  (typecase value
    (string (copy-seq value))
    (vector (let ((copy (copy-seq value)))
              (loop for index below (length copy)
                    do (setf (aref copy index)
                             (export-copy-registration-value
                              (aref copy index))))
              copy))
    (cons (cons (export-copy-registration-value (car value))
                (export-copy-registration-value (cdr value))))
    (t value)))

(defun export-option-default-copyable-p (value)
  (labels ((visit (current active)
             (cond
               ((or (null current) (symbolp current) (numberp current)
                    (characterp current) (stringp current))
                t)
               ((consp current)
                (and (not (member current active :test #'eq))
                     (visit (car current) (cons current active))
                     (visit (cdr current) (cons current active))))
               ((vectorp current)
                (and (not (member current active :test #'eq))
                     (loop for item across current
                           always (visit item (cons current active)))))
               (t nil))))
    (visit value nil)))

(defun export-backend-name-p (name)
  (and (keywordp name)
       (let* ((text (symbol-name name))
              (first (and (plusp (length text)) (char-code (char text 0)))))
         (and first
              (or (<= #x41 first #x5a) (<= #x61 first #x7a))
              (loop for character across text
                    for code = (char-code character)
                    always (or (<= #x41 code #x5a)
                               (<= #x61 code #x7a)
                               (<= #x30 code #x39)
                               (= code #x2d)))))))

(defun export-registration-check-backend-name (name)
  (unless (keywordp name)
    (error 'type-error :datum name :expected-type 'keyword))
  (unless (export-backend-name-p name)
    (export-registration-error name :invalid-backend-name
                               "Invalid export backend name")))

(defun export-registration-check-callback (callback)
  (unless (or (null callback) (functionp callback))
    (error 'type-error :datum callback
           :expected-type '(or null function)))
  callback)

(defun export-plist-key-present-p (plist key)
  (and (export-proper-list-p plist)
       (loop for tail on plist by #'cddr
             thereis (eq (car tail) key))))

(defun export-registration-valid-stage-p (stage)
  (or (member stage '(:options :parse-tree :plain-text :body :final-output)
               :test #'eq)
      (and (consp stage)
           (eq (car stage) :element)
           (consp (cdr stage))
           (null (cddr stage))
           (member (cadr stage) (node-types) :test #'eq))))

(defun export-registration-check-filter-alist (filters)
  (unless (export-proper-list-p filters)
    (error 'type-error :datum filters :expected-type 'list))
  (let ((seen nil))
    (dolist (entry filters)
      (unless (and (consp entry) (export-proper-list-p (cdr entry)))
        (error 'type-error :datum entry :expected-type 'cons))
      (let ((stage (car entry))
            (callbacks (cdr entry)))
        (unless (export-registration-valid-stage-p stage)
          (export-registration-error nil :invalid-filter-stage
                                     "Unknown export filter stage"))
        (when (member stage seen :test #'equal)
          (export-registration-error nil :invalid-filter-stage
                                     "Duplicate export filter stage"))
        (push stage seen)
        (unless (every #'functionp callbacks)
          (let ((bad (find-if-not #'functionp callbacks)))
            (error 'type-error :datum bad :expected-type 'function))))))
  (mapcar (lambda (entry)
            (cons (export-copy-registration-value (car entry))
                  (copy-list (cdr entry))))
          filters))

(defun export-registration-valid-node-policy-p (policy)
  (or (member policy '(:default :prune-children) :test #'eq)
      (and (export-proper-list-p policy)
           (eq (first policy) :unsupported)
           (export-proper-list-p (rest policy))
           (evenp (length (rest policy)))
           (let ((keys (loop for tail on (rest policy) by #'cddr
                             collect (car tail))))
             (and (= (length keys) 2)
                  (member :feature keys :test #'eq)
                  (member :message keys :test #'eq)
                  (keywordp (getf (rest policy) :feature))
                  (stringp (getf (rest policy) :message)))))))

(defun export-registration-check-node-map (mapping field)
  (unless (export-proper-list-p mapping)
    (error 'type-error :datum mapping :expected-type 'list))
  (let ((seen nil)
        (known (node-types)))
    (dolist (entry mapping)
      (unless (consp entry)
        (error 'type-error :datum entry :expected-type 'cons))
      (let ((key (car entry)) (value (cdr entry)))
        (unless (member key known :test #'eq)
          (export-registration-error nil
                                     (if (eq field :transcoders)
                                         :invalid-transcoder
                                         :invalid-node-policy)
                                     "Unknown export node type"))
        (when (member key seen :test #'eq)
          (export-registration-error nil
                                     (if (eq field :transcoders)
                                         :invalid-transcoder
                                         :invalid-node-policy)
                                     "Duplicate export node type"))
        (push key seen)
        (if (eq field :transcoders)
            (unless (functionp value)
              (error 'type-error :datum value :expected-type 'function))
            (unless (export-registration-valid-node-policy-p value)
              (export-registration-error nil :invalid-node-policy
                                         "Invalid export node policy")))))
  (export-copy-registration-value mapping)))

(defun export-registration-check-option-specifications (specifications)
  (unless (export-proper-list-p specifications)
    (error 'type-error :datum specifications :expected-type 'list))
  (labels ((field-present-p (specification key)
             (loop for tail on specification by #'cddr
                   thereis (eq (car tail) key)))
           (parser-p (parser)
             (or (functionp parser)
                 (member parser
                         '(:toc :integer :boolean-integer :sub-superscript
                           :boolean :archive-mode :disabled-only :creator-mode
                           :drawer-list :latex-mode :selection-list :tag-mode
                           :tasks-mode :timestamp-range)
                         :test #'eq)))
           (default-p (parser value)
             (case parser
               (:boolean (member value '(nil t)))
               (:integer (integerp value))
               (:boolean-integer (or (member value '(nil t)) (integerp value)))
               (:toc (or (member value '(nil t)) (integerp value)))
               (:creator-mode (member value '(nil t :comment)))
               (:sub-superscript
                (or (member value '(nil t)) (equal value "{}")))
               (:disabled-only (null value))
               (:archive-mode (member value '(nil t :headline)))
               (:latex-mode (member value '(nil t :verbatim)))
               (:tag-mode (member value '(nil t :not-in-toc)))
               (:timestamp-range
                (member value '(nil t :active :active-exclude-diary :inactive)))
               ((:selection-list :drawer-list :tasks-mode)
                (or (member value '(nil t :todo :done))
                    (and (export-proper-list-p value)
                         (member (first value) '(:include :exclude) :test #'eq)
                         (every #'stringp (rest value)))))
               (otherwise (export-option-default-copyable-p value)))))
    (let ((keys nil)
          (tokens nil))
      (dolist (specification specifications)
        (unless (and (export-proper-list-p specification)
                     (evenp (length specification)))
          (error 'type-error :datum specification :expected-type 'list))
        (let ((seen-keys nil))
          (loop for tail on specification by #'cddr
                for field = (car tail)
                do (unless (member field '(:token :key :parser :default) :test #'eq)
                     (export-registration-error
                      nil :invalid-option-specification
                      "Unknown export option specification field"))
                   (when (member field seen-keys :test #'eq)
                     (export-registration-error
                      nil :invalid-option-specification
                      "Duplicate export option specification field"))
                   (push field seen-keys)))
        (let ((key (getf specification :key))
              (token (getf specification :token))
              (parser (getf specification :parser)))
          (unless (and (keywordp key)
                       (stringp token)
                       (plusp (length token))
                       (parser-p parser)
                       (or (not (field-present-p specification :default))
                           (default-p parser (getf specification :default)))
                       (field-present-p specification :token)
                       (field-present-p specification :key)
                       (field-present-p specification :parser))
            (export-registration-error nil :invalid-option-specification
                                       "Invalid export option specification"))
          (when (member key keys :test #'eq)
            (export-registration-error nil :invalid-option-specification
                                       "Duplicate export option key"))
          (when (member token tokens :test #'string-equal)
            (export-registration-error nil :invalid-option-specification
                                       "Duplicate export option token"))
          (push key keys)
          (push token tokens)))
      (export-copy-registration-value specifications))))

(defun export-registration-check-string-set (items reason)
  (unless (export-proper-list-p items)
    (error 'type-error :datum items :expected-type 'list))
  (unless (every #'stringp items)
    (let ((bad (find-if-not #'stringp items)))
      (error 'type-error :datum bad :expected-type 'string)))
  (when (/= (length items) (length (remove-duplicates items :test #'string-equal)))
    (export-registration-error nil reason "Duplicate export keyword"))
  (mapcar #'copy-seq items))

(defun export-merge-string-sets (&rest sets)
  (let ((result nil))
    (dolist (items sets result)
      (dolist (item items)
        (unless (member item result :test #'string-equal)
          (setf result (append result (list (copy-seq item)))))))))

(defun export-registration-check-raw-options (options)
  (unless (export-proper-list-p options)
    (error 'type-error :datum options :expected-type 'list))
  (let ((seen nil))
    (dolist (entry options)
      (unless (consp entry)
        (error 'type-error :datum entry :expected-type 'cons))
      (unless (member (car entry) '(:html :latex) :test #'eq)
        (export-registration-error nil :invalid-raw-output-policy
                                   "Unknown raw export kind"))
      (when (member (car entry) seen :test #'eq)
        (export-registration-error nil :invalid-raw-output-policy
                                   "Duplicate raw export kind"))
      (unless (keywordp (cdr entry))
        (error 'type-error :datum (cdr entry) :expected-type 'keyword))
      (push (car entry) seen)))
  (export-copy-registration-value options))

(defun export-registration-check-link-handlers (handlers)
  (unless (export-proper-list-p handlers)
    (error 'type-error :datum handlers :expected-type 'list))
  (let ((seen nil))
    (dolist (entry handlers)
      (unless (consp entry)
        (error 'type-error :datum entry :expected-type 'cons))
      (let ((type (car entry)) (callback (cdr entry)))
        (unless (stringp type)
          (error 'type-error :datum type :expected-type 'string))
        (unless (plusp (length type))
          (export-registration-error nil :invalid-link-type-handler
                                     "Link type must not be empty"))
        (unless (functionp callback)
          (error 'type-error :datum callback :expected-type 'function))
        (when (member type seen :test #'string-equal)
          (export-registration-error nil :invalid-link-type-handler
                                     "Duplicate export link type"))
        (push type seen))))
  (mapcar (lambda (entry) (cons (copy-seq (car entry)) (cdr entry))) handlers))

(defun export-registration-check-fields
    (renderer preamble-renderer trailer-renderer template make-state
     document-class node-wrapper transcoders node-policies
     option-specifications accepted-keywords retained-keywords
     raw-output-options export-block-names export-snippet-names
     link-type-handlers filters)
  (dolist (callback (list renderer preamble-renderer trailer-renderer template
                          make-state node-wrapper))
    (export-registration-check-callback callback))
  (unless (or (null document-class) (stringp document-class))
    (error 'type-error :datum document-class :expected-type '(or null string)))
  (list :transcoders (export-registration-check-node-map transcoders :transcoders)
        :document-class (and document-class (copy-seq document-class))
        :node-policies (export-registration-check-node-map node-policies :policies)
        :option-specifications
        (export-registration-check-option-specifications option-specifications)
        :accepted-keywords
        (export-registration-check-string-set accepted-keywords :invalid-keyword-policy)
        :retained-keywords
        (export-registration-check-string-set retained-keywords :invalid-keyword-policy)
        :raw-output-options (export-registration-check-raw-options raw-output-options)
        :export-block-names
        (export-registration-check-string-set export-block-names :invalid-keyword-policy)
        :export-snippet-names
        (export-registration-check-string-set export-snippet-names :invalid-keyword-policy)
        :link-type-handlers
        (export-registration-check-link-handlers link-type-handlers)
        :filters (export-registration-check-filter-alist filters)))

(defun export-make-descriptor (name parent fields builtin-p)
  (%make-export-backend-descriptor
   :name name :parent parent
   :renderer (getf fields :renderer)
   :preamble-renderer (getf fields :preamble-renderer)
   :trailer-renderer (getf fields :trailer-renderer)
   :template (getf fields :template)
   :make-state (getf fields :make-state)
   :document-class (getf fields :document-class)
   :transcoders (getf fields :transcoders)
   :native-sink-renderer (getf fields :native-sink-renderer)
   :native-sink-scratch (getf fields :native-sink-scratch)
   :native-sink-string (getf fields :native-sink-string)
   :native-sink-write-string (getf fields :native-sink-write-string)
   :native-sink-append-node-result
   (getf fields :native-sink-append-node-result)
   :native-sink-output-offset (getf fields :native-sink-output-offset)
   :node-policies (getf fields :node-policies)
   :node-wrapper (getf fields :node-wrapper)
   :option-specifications (getf fields :option-specifications)
   :accepted-keywords
   (export-merge-string-sets *export-common-accepted-keywords*
                             (getf fields :accepted-keywords))
   :retained-keywords
   (export-merge-string-sets *export-common-retained-keywords*
                             (getf fields :retained-keywords))
   :raw-output-options (getf fields :raw-output-options)
   :export-block-names (getf fields :export-block-names)
   :export-snippet-names (getf fields :export-snippet-names)
   :link-type-handlers (getf fields :link-type-handlers)
   :filters (getf fields :filters)
   :builtin-p builtin-p))

(defun export-descriptor-with-link-type-handlers (descriptor handlers)
  (export-make-descriptor
   (%export-backend-descriptor-name descriptor)
   (%export-backend-descriptor-parent descriptor)
   (list :renderer (%export-backend-descriptor-renderer descriptor)
         :preamble-renderer
         (%export-backend-descriptor-preamble-renderer descriptor)
         :trailer-renderer
         (%export-backend-descriptor-trailer-renderer descriptor)
         :template (%export-backend-descriptor-template descriptor)
         :make-state (%export-backend-descriptor-make-state descriptor)
         :document-class (%export-backend-descriptor-document-class descriptor)
         :transcoders (%export-backend-descriptor-transcoders descriptor)
         :native-sink-renderer
         (%export-backend-descriptor-native-sink-renderer descriptor)
         :native-sink-scratch
         (%export-backend-descriptor-native-sink-scratch descriptor)
         :native-sink-string
         (%export-backend-descriptor-native-sink-string descriptor)
         :native-sink-write-string
         (%export-backend-descriptor-native-sink-write-string descriptor)
         :native-sink-append-node-result
         (%export-backend-descriptor-native-sink-append-node-result descriptor)
         :native-sink-output-offset
         (%export-backend-descriptor-native-sink-output-offset descriptor)
         :node-policies (%export-backend-descriptor-node-policies descriptor)
         :node-wrapper (%export-backend-descriptor-node-wrapper descriptor)
         :option-specifications
         (%export-backend-descriptor-option-specifications descriptor)
         :accepted-keywords
         (%export-backend-descriptor-accepted-keywords descriptor)
         :retained-keywords
         (%export-backend-descriptor-retained-keywords descriptor)
         :raw-output-options
         (%export-backend-descriptor-raw-output-options descriptor)
         :export-block-names
         (%export-backend-descriptor-export-block-names descriptor)
         :export-snippet-names
         (%export-backend-descriptor-export-snippet-names descriptor)
         :link-type-handlers handlers
         :filters (%export-backend-descriptor-filters descriptor))
   (%export-backend-descriptor-builtin-p descriptor)))

(defun export-common-option-specifications ()
  (mapcar (lambda (specification)
            (list :token (first specification)
                  :key (second specification)
                  :parser (third specification)))
          *export-option-specifications*))

(defun export-merge-option-specifications (inherited local)
  (let ((result (copy-tree inherited)))
    (dolist (specification local)
      (let* ((key (getf specification :key))
             (position (position key result
                                 :key (lambda (entry) (getf entry :key))
                                 :test #'eq)))
        (if position
            (setf (nth position result) specification)
            (setf result (append result (list specification))))))
    (let ((tokens nil))
      (dolist (specification result)
        (let ((token (getf specification :token)))
          (when (member token tokens :test #'string-equal)
            (export-registration-error nil :invalid-option-specification
                                       "Duplicate export option token"))
          (push token tokens))))
    result))

(defun export-resolved-option-specifications (ancestry)
  (let ((result nil))
    (dolist (descriptor ancestry result)
      (setf result
            (export-merge-option-specifications
             result
             (%export-backend-descriptor-option-specifications descriptor))))))

(defun export-resolved-option-specification (ancestry token)
  (find token (export-resolved-option-specifications ancestry)
        :key (lambda (specification) (getf specification :token))
        :test #'string-equal))

(defun %register-builtin-export-backend
    (name &key renderer preamble-renderer trailer-renderer template make-state
            document-class transcoders native-sink-renderer native-sink-scratch
            native-sink-string native-sink-write-string
            native-sink-append-node-result native-sink-output-offset
            node-policies node-wrapper
            option-specifications accepted-keywords retained-keywords
            raw-output-options export-block-names export-snippet-names
            link-type-handlers filters)
  (let ((*export-registration-backend* name))
    (export-registration-check-backend-name name)
    (export-registration-check-callback renderer)
    (unless renderer
      (export-registration-error name :invalid-renderer
                                 "A built-in export backend requires a renderer"))
    (dolist (callback (list native-sink-renderer native-sink-scratch
                            native-sink-string native-sink-write-string
                            native-sink-append-node-result
                            native-sink-output-offset))
      (export-registration-check-callback callback))
    (when (some #'identity
                (list native-sink-renderer native-sink-scratch
                      native-sink-string native-sink-write-string
                      native-sink-append-node-result
                      native-sink-output-offset))
      (unless (every #'identity
                     (list native-sink-renderer native-sink-scratch
                           native-sink-string native-sink-write-string
                           native-sink-append-node-result
                           native-sink-output-offset))
        (export-registration-error name :invalid-native-sink
                                   "A native sink requires all private callbacks")))
    (let ((fields
            (export-registration-check-fields
             renderer preamble-renderer trailer-renderer template make-state
             document-class node-wrapper transcoders
             (append (export-common-node-policies) node-policies)
             option-specifications accepted-keywords retained-keywords
             raw-output-options export-block-names export-snippet-names
             link-type-handlers filters)))
      (setf (getf fields :option-specifications)
            (export-merge-option-specifications
             (export-common-option-specifications)
             (getf fields :option-specifications)))
      (sb-thread:with-mutex (*export-backend-registry-lock*)
        (let ((existing (gethash name *export-backend-registry*)))
          (when (and existing
                     (not (%export-backend-descriptor-builtin-p existing)))
            (export-registration-error name :duplicate-backend
                                       "Export backend is already registered"))
          (setf (gethash name *export-backend-registry*)
                (let* ((existing-handlers
                         (and existing
                              (%export-backend-descriptor-link-type-handlers
                               existing)))
                       (fields
                         (if existing-handlers
                             (let ((builtin-handlers
                                     (remove-if-not
                                      (lambda (entry)
                                        (member
                                         (car entry)
                                         (gethash name
                                                  *export-builtin-link-type-handlers*)
                                         :test #'string-equal))
                                      existing-handlers))
                                   (custom-handlers
                                     (remove-if
                                      (lambda (entry)
                                        (member
                                         (car entry)
                                         (gethash name
                                                  *export-builtin-link-type-handlers*)
                                         :test #'string-equal))
                                      existing-handlers)))
                               (setf (getf fields :link-type-handlers)
                                     (append (getf fields :link-type-handlers)
                                             builtin-handlers
                                             custom-handlers))
                               fields)
                             fields)))
                  (export-make-descriptor
                   name nil
                   (append fields (list :renderer renderer
                                        :preamble-renderer preamble-renderer
                                        :trailer-renderer trailer-renderer
                                        :template template :make-state make-state
                                        :document-class document-class
                                        :native-sink-renderer native-sink-renderer
                                        :native-sink-scratch native-sink-scratch
                                        :native-sink-string native-sink-string
                                        :native-sink-write-string
                                        native-sink-write-string
                                        :native-sink-append-node-result
                                        native-sink-append-node-result
                                        :native-sink-output-offset
                                        native-sink-output-offset
                                        :node-wrapper node-wrapper))
                   t)))))
      name)))

(defun export-backend-registered-p (backend)
  (and (keywordp backend)
       (sb-thread:with-mutex (*export-backend-registry-lock*)
         (not (null (gethash backend *export-backend-registry*))))))

(defun export-registry-descriptor (backend)
  (sb-thread:with-mutex (*export-backend-registry-lock*)
    (gethash backend *export-backend-registry*)))

(defun export-registry-snapshot (backend)
  (sb-thread:with-mutex (*export-backend-registry-lock*)
    (let ((descriptor (gethash backend *export-backend-registry*)))
      (values descriptor
              (and descriptor (%export-backend-ancestry-unlocked backend))))))

(defun register-export-backend
    (name &key renderer preamble-renderer trailer-renderer template make-state
            document-class transcoders node-policies node-wrapper
            option-specifications accepted-keywords retained-keywords
            raw-output-options export-block-names export-snippet-names
            link-type-handlers filters)
  "Register a root backend with its renderer and export policies."
  (let ((*export-registration-backend* name))
    (export-registration-check-backend-name name)
    (export-registration-check-callback renderer)
    (unless renderer
      (export-registration-error name :invalid-renderer
                                 "A root export backend requires a renderer"))
    (let ((fields
            (export-registration-check-fields
             renderer preamble-renderer trailer-renderer template make-state
             document-class node-wrapper transcoders node-policies
             option-specifications accepted-keywords retained-keywords
             raw-output-options export-block-names export-snippet-names
             link-type-handlers filters)))
      (setf (getf fields :option-specifications)
            (export-merge-option-specifications
             (export-common-option-specifications)
             (getf fields :option-specifications)))
      (sb-thread:with-mutex (*export-backend-registry-lock*)
        (when (gethash name *export-backend-registry*)
          (export-registration-error name :duplicate-backend
                                     "Export backend is already registered"))
        (setf (gethash name *export-backend-registry*)
              (export-make-descriptor
               name nil
               (append fields (list :renderer renderer
                                    :preamble-renderer preamble-renderer
                                    :trailer-renderer trailer-renderer
                                    :template template :make-state make-state
                                    :document-class document-class
                                    :node-wrapper node-wrapper))
               nil)))
      name)))

(defun register-derived-export-backend
    (name parent &key renderer preamble-renderer trailer-renderer template
            make-state document-class transcoders node-policies node-wrapper
            option-specifications accepted-keywords retained-keywords
            raw-output-options export-block-names export-snippet-names
            link-type-handlers filters)
  "Register NAME as a derived backend of PARENT with local overrides."
  (let ((*export-registration-backend* name))
    (export-registration-check-backend-name name)
    (export-registration-check-backend-name parent)
    (let ((fields
            (export-registration-check-fields
             renderer preamble-renderer trailer-renderer template make-state
             document-class node-wrapper transcoders node-policies
             option-specifications accepted-keywords retained-keywords
             raw-output-options export-block-names export-snippet-names
             link-type-handlers filters)))
      (sb-thread:with-mutex (*export-backend-registry-lock*)
        (when (gethash name *export-backend-registry*)
          (export-registration-error name :duplicate-backend
                                     "Export backend is already registered"))
        (unless (gethash parent *export-backend-registry*)
          (export-registration-error name :unknown-parent
                                     "Parent export backend is not registered"))
        (export-merge-option-specifications
         (export-resolved-option-specifications
          (%export-backend-ancestry-unlocked parent))
         (getf fields :option-specifications))
        (setf (gethash name *export-backend-registry*)
              (export-make-descriptor
               name parent
               (append fields (list :renderer renderer
                                    :preamble-renderer preamble-renderer
                                    :trailer-renderer trailer-renderer
                                    :template template :make-state make-state
                                    :document-class document-class
                                    :node-wrapper node-wrapper))
               nil)))
      name)))

(defun export-backend-descendants (backend)
  (let (result)
    (maphash (lambda (name descriptor)
               (declare (ignore descriptor))
               (when (%export-backend-derived-p-unlocked name backend)
                 (push name result)))
             *export-backend-registry*)
    result))

(defun unregister-export-backend (name)
  "Remove a registered non-built-in backend with no registered descendants."
  (unless (keywordp name)
    (error 'type-error :datum name :expected-type 'keyword))
  (sb-thread:with-mutex (*export-backend-registry-lock*)
    (let ((descriptor (gethash name *export-backend-registry*)))
      (unless descriptor
        (export-registration-error name :unknown-backend
                                   "Export backend is not registered"))
      (when (%export-backend-descriptor-builtin-p descriptor)
        (export-registration-error name :builtin-backend
                                   "Built-in export backends cannot be removed"))
      (when (export-backend-descendants name)
        (export-registration-error name :backend-in-use
                                   "Export backend has registered descendants"))
      (remhash name *export-backend-registry*)))
  name)

(defun %export-backend-derived-p-unlocked (backend ancestor)
  (loop with current = (gethash backend *export-backend-registry*)
        for parent = (and current (%export-backend-descriptor-parent current))
        while parent
        when (eq parent ancestor) return t
        do (setf current (gethash parent *export-backend-registry*)
                 parent (and current (%export-backend-descriptor-parent current)))
        finally (return nil)))

(defun export-backend-derived-p (backend ancestor)
  "Return true when BACKEND is a strict descendant of ANCESTOR."
  (unless (keywordp backend)
    (error 'type-error :datum backend :expected-type 'keyword))
  (unless (keywordp ancestor)
    (error 'type-error :datum ancestor :expected-type 'keyword))
  (sb-thread:with-mutex (*export-backend-registry-lock*)
    (%export-backend-derived-p-unlocked backend ancestor)))

(defun %export-backend-ancestry-unlocked (backend)
  (let ((chain nil)
        (current (gethash backend *export-backend-registry*)))
    (loop while current
          do (push current chain)
             (setf current
                   (gethash (%export-backend-descriptor-parent current)
                            *export-backend-registry*)))
    chain))

(defun export-backend-ancestry (backend)
  (sb-thread:with-mutex (*export-backend-registry-lock*)
    (%export-backend-ancestry-unlocked backend)))

(defun export-resolved-filters (backend stage)
  (export-resolved-filters-in-ancestry
   (export-backend-ancestry backend) stage))

(defun export-resolved-filters-in-ancestry (ancestry stage)
  (let ((result nil))
    (dolist (descriptor (reverse ancestry))
      (let ((local (cdr (assoc stage
                               (%export-backend-descriptor-filters descriptor)
                               :test #'equal))))
        (setf result
              (nconc result
                     (mapcar (lambda (callback)
                               (cons (%export-backend-descriptor-name descriptor)
                                     callback))
                             local)))))
    result))

(defun export-resolved-map-value (ancestry accessor key &optional (test #'eq))
  (loop for descriptor in (reverse ancestry)
        for entry = (assoc key (funcall accessor descriptor) :test test)
        when entry return (values (cdr entry)
                                  (%export-backend-descriptor-name descriptor))))

(defun export-resolved-node-policy (ancestry type)
  (or (export-resolved-map-value
       ancestry #'%export-backend-descriptor-node-policies type)
      :default))

(defun export-resolved-string-set (ancestry accessor)
  (let ((result nil))
    (dolist (descriptor ancestry result)
      (dolist (item (funcall accessor descriptor))
        (unless (member item result :test #'string-equal)
          (setf result (append result (list item))))))))

(defun export-resolved-component (ancestry accessor)
  (loop for descriptor in (reverse ancestry)
        for value = (funcall accessor descriptor)
        when value return (values value
                                  (%export-backend-descriptor-name descriptor))))

(defun export-ancestry-through (ancestry backend)
  (loop for descriptor in ancestry
        collect descriptor
        until (eq (%export-backend-descriptor-name descriptor) backend)))

(defun export-apply-filter-stage
    (backend stage value context &key (validator #'stringp)
                                      (expected-type 'string))
  (declare (ignore backend))
  (let ((result value))
    (unless (funcall validator result)
      (error 'type-error :datum result :expected-type expected-type))
    (dolist (entry (export-resolved-filters-in-ancestry
                    (%export-render-context-ancestry context) stage)
                    result)
      (let ((callback-context (export-context-for-owner context (car entry))))
        (case stage
          (:options
           (setf (%export-render-context-options callback-context) result))
          (:parse-tree
           (setf (%export-render-context-root-views callback-context) result)))
        (let ((filtered (funcall (cdr entry) result callback-context)))
          (unless (null filtered)
            (unless (funcall validator filtered)
              (error 'type-error :datum filtered :expected-type expected-type))
            (setf result filtered)
            (when (and (stringp filtered) (zerop (length filtered)))
              (return result)))))))
)

(defun export-check-render-string (value)
  (unless (stringp value)
    (error 'type-error :datum value :expected-type 'string))
  value)

(defvar *export-active-node-dispatch* nil)

(defun export-render-context-check (context)
  (unless (%export-render-context-p context)
    (error 'type-error :datum context :expected-type 'export-render-context))
  context)

(defun export-render-frame-check (frame)
  (unless (%export-render-frame-p frame)
    (error 'type-error :datum frame :expected-type 'export-render-frame))
  frame)

(defun export-render-context-backend (context)
  "Return the backend requested for CONTEXT."
  (%export-render-context-backend (export-render-context-check context)))
(defun export-render-context-document (context)
  "Return the document associated with CONTEXT."
  (%export-render-context-document (export-render-context-check context)))
(defun export-render-context-metadata (context)
  "Return a copy of the metadata stored in CONTEXT."
  (export-copy-snapshot-value
   (%export-render-context-metadata (export-render-context-check context))))
(defun export-render-context-options (context)
  "Return a copy of the options stored in CONTEXT."
  (export-copy-snapshot-value
   (%export-render-context-options (export-render-context-check context))))
(defun export-render-context-limits (context)
  "Return the resource limits applied to this export."
  (%export-render-context-limits (export-render-context-check context)))
(defun export-render-context-root-views (context)
  "Return a fresh list of the prepared root views."
  (copy-list (%export-render-context-root-views
              (export-render-context-check context))))
(defun export-render-context-toc (context)
  "Return a copy of the table-of-contents data."
  (export-copy-snapshot-value
   (%export-render-context-toc (export-render-context-check context))))
(defun export-render-context-footnotes (context)
  "Return a copy of the footnote data."
  (export-copy-snapshot-value
   (%export-render-context-footnotes (export-render-context-check context))))
(defun export-render-context-raw-output-p (context kind)
  "Return true when raw output of KIND is enabled for this export."
  (export-render-context-check context)
  (unless (member kind '(:html :latex) :test #'eq)
    (error 'type-error :datum kind :expected-type '(member :html :latex)))
  (let* ((argument-key (export-resolved-map-value
                        (%export-render-context-ancestry context)
                        #'%export-backend-descriptor-raw-output-options kind))
         (preparation (%export-render-context-preparation context)))
    (and argument-key
         (case argument-key
           (:allow-raw-html
            (export-preparation-allow-raw-html preparation))
           (:allow-raw-latex
            (export-preparation-allow-raw-latex preparation))
           (otherwise nil)))))
(defun export-render-context-extension (context key)
  "Return the extension value registered for KEY."
  (export-render-context-check context)
  (unless (keywordp key)
    (error 'type-error :datum key :expected-type 'keyword))
  (cdr (assoc key (%export-render-context-extensions context) :test #'eq)))
(defun export-render-context-owner-backend (context)
  "Return the backend that owns the active callback state."
  (%export-render-context-owner-backend (export-render-context-check context)))
(defun export-render-context-state (context)
  "Return the state object for the active backend."
  (%export-render-context-state (export-render-context-check context)))

(defun export-render-frame-mode (frame)
  "Return the node-rendering mode stored in FRAME."
  (%export-render-frame-mode (export-render-frame-check frame)))
(defun export-render-frame-table-cell-p (frame)
  "Return whether FRAME is rendering a table cell."
  (%export-render-frame-table-cell-p (export-render-frame-check frame)))
(defun export-render-frame-preserve-source-breaks-p (frame)
  "Return whether FRAME preserves source line breaks."
  (%export-render-frame-preserve-source-breaks-p (export-render-frame-check frame)))
(defun export-render-frame-output-offset (frame)
  "Return the logical output position at the start of FRAME."
  (%export-render-frame-output-offset (export-render-frame-check frame)))

(defun export-context-for-owner (context owner)
  (let ((copy (copy-structure context)))
    (setf (%export-render-context-owner-backend copy) owner
          (%export-render-context-state copy)
          (cdr (assoc owner (%export-render-context-states context) :test #'eq)))
    copy))

(defun export-initialize-render-states (context)
  (export-render-context-check context)
  (let ((states nil))
    (dolist (descriptor (%export-render-context-ancestry context))
      (let ((factory (%export-backend-descriptor-make-state descriptor)))
        (when factory
          (let ((factory-context (copy-structure context)))
            (setf (%export-render-context-owner-backend factory-context)
                  (%export-backend-descriptor-name descriptor)
                  (%export-render-context-state factory-context) nil
                  (%export-render-context-states factory-context) nil)
            (let ((*export-creating-state* t)
                  (*export-active-node-dispatch* nil))
              (push (cons (%export-backend-descriptor-name descriptor)
                          (funcall factory factory-context))
                    states))))))
    (setf states (nreverse states))
    (setf (%export-render-context-states context) states
          (%export-render-context-state context)
          (cdr (assoc (%export-render-context-owner-backend context)
                      states :test #'eq)))
    context))

(defun export-check-render-not-creating-state ()
  (when *export-creating-state*
    (error 'program-error)))

(defun export-render-context-renderer (context)
  (export-resolved-component (%export-render-context-ancestry context)
                             #'%export-backend-descriptor-renderer))

(defun export-render-context-preamble-renderer (context)
  (export-resolved-component (%export-render-context-ancestry context)
                             #'%export-backend-descriptor-preamble-renderer))

(defun export-render-context-trailer-renderer (context)
  (export-resolved-component (%export-render-context-ancestry context)
                             #'%export-backend-descriptor-trailer-renderer))

(defun export-render-context-template (context)
  (export-resolved-component (%export-render-context-ancestry context)
                             #'%export-backend-descriptor-template))

(defun export-render-parent-descriptor (context)
  (export-check-render-not-creating-state)
  (export-render-context-check context)
  (let* ((owner (%export-render-context-owner-backend context))
         (owner-descriptor
           (find owner (%export-render-context-ancestry context)
                 :key #'%export-backend-descriptor-name :test #'eq))
         (parent (and owner-descriptor
                      (%export-backend-descriptor-parent owner-descriptor))))
    (unless parent
      (error 'program-error))
    (find parent (%export-render-context-ancestry context)
          :key #'%export-backend-descriptor-name :test #'eq)))

(defun export-render-parent-renderer (context)
  "Invoke the parent backend renderer for CONTEXT."
  (let* ((parent (export-render-parent-descriptor context))
         (ancestry (export-ancestry-through
                    (%export-render-context-ancestry context)
                    (%export-backend-descriptor-name parent)))
         (callback-and-owner
          (multiple-value-list
           (export-resolved-component ancestry
                                      #'%export-backend-descriptor-renderer))))
    (let ((result
           (funcall (first callback-and-owner)
                    (export-context-for-owner context (second callback-and-owner)))))
      (unless (stringp result)
        (error 'type-error :datum result :expected-type 'string))
      result)))

(defun export-render-parent-preamble (context)
  "Render the parent backend preamble for CONTEXT."
  (let* ((parent (export-render-parent-descriptor context))
         (ancestry (export-ancestry-through
                    (%export-render-context-ancestry context)
                    (%export-backend-descriptor-name parent)))
         (callback-and-owner
           (multiple-value-list
            (export-resolved-component
             ancestry #'%export-backend-descriptor-preamble-renderer))))
    (let ((result
            (if (first callback-and-owner)
                (funcall (first callback-and-owner)
                         (export-context-for-owner context
                                                   (second callback-and-owner)))
                "")))
      (unless (stringp result)
        (error 'type-error :datum result :expected-type 'string))
      result)))

(defun export-render-parent-trailer (context)
  "Render the parent backend trailer for CONTEXT."
  (let* ((parent (export-render-parent-descriptor context))
         (ancestry (export-ancestry-through
                    (%export-render-context-ancestry context)
                    (%export-backend-descriptor-name parent)))
         (callback-and-owner
           (multiple-value-list
            (export-resolved-component
             ancestry #'%export-backend-descriptor-trailer-renderer))))
    (let ((result
            (if (first callback-and-owner)
                (funcall (first callback-and-owner)
                         (export-context-for-owner context
                                                   (second callback-and-owner)))
                "")))
      (unless (stringp result)
        (error 'type-error :datum result :expected-type 'string))
      result)))

(defun export-render-parent-template (context preamble body trailer)
  "Combine PREAMBLE, BODY, and TRAILER with the parent backend template."
  (dolist (component (list preamble body trailer))
    (unless (stringp component)
      (error 'type-error :datum component :expected-type 'string)))
  (let* ((parent (export-render-parent-descriptor context))
         (ancestry (export-ancestry-through
                    (%export-render-context-ancestry context)
                    (%export-backend-descriptor-name parent)))
         (callback-and-owner
           (multiple-value-list
            (export-resolved-component ancestry
                                       #'%export-backend-descriptor-template))))
    (let ((result
            (if (first callback-and-owner)
                (funcall (first callback-and-owner)
                         (export-context-for-owner context
                                                   (second callback-and-owner))
                         preamble body trailer)
                (concatenate 'string preamble body trailer))))
      (unless (stringp result)
        (error 'type-error :datum result :expected-type 'string))
      result)))

(defun export-option-explicit-p (context key)
  "Return true when CONTEXT contains an explicit value for KEY."
  (export-render-context-check context)
  (unless (keywordp key)
    (error 'type-error :datum key :expected-type 'keyword))
  (not (null (assoc key (%export-render-context-options context) :test #'eq))))

(defun export-option-value (context key)
  "Return the explicit or registered default value for KEY."
  (export-render-context-check context)
  (unless (keywordp key)
    (error 'type-error :datum key :expected-type 'keyword))
  (let ((explicit (assoc key (%export-render-context-options context) :test #'eq)))
    (if explicit
        (cdr explicit)
        (let ((specification
                (find key
                      (export-resolved-option-specifications
                       (%export-render-context-ancestry context))
                      :key (lambda (entry) (getf entry :key)) :test #'eq)))
          (when (and specification
                     (export-plist-key-present-p specification :default))
            (export-copy-registration-value
             (getf specification :default)))))))

(defun export-render-native-sink-implementation
    (context node view frame ancestry)
  (multiple-value-bind (renderer owner)
      (export-resolved-component
       ancestry #'%export-backend-descriptor-native-sink-renderer)
    (multiple-value-bind (make-scratch)
        (export-resolved-component
         ancestry #'%export-backend-descriptor-native-sink-scratch)
      (multiple-value-bind (get-string)
          (export-resolved-component
           ancestry #'%export-backend-descriptor-native-sink-string)
        (let* ((callback-context (export-context-for-owner context owner))
               (sink (funcall make-scratch *export-current-native-sink* frame
                              callback-context)))
          (let* ((*export-active-node-dispatch*
                  (list callback-context node view frame ancestry))
                 (*export-current-native-sink* sink)
                 (*export-current-output-offset*
                  (%export-render-frame-output-offset frame))
                 (result (funcall renderer sink node view frame
                                  callback-context)))
            (unless (null result)
              (error 'type-error :datum result :expected-type 'null)))
          (let ((text (funcall get-string sink callback-context)))
            (unless (stringp text)
              (error 'type-error :datum text :expected-type 'string))
            text))))))

(defun export-render-node-implementation (context node view frame ancestry)
  (let* ((type (node-type node))
         (policy (export-resolved-node-policy ancestry type))
         (transcoder (export-resolved-map-value
                      ancestry #'%export-backend-descriptor-transcoders type)))
    (when (and (consp policy) (eq (car policy) :unsupported))
      (export-unsupported-syntax
       (%export-render-context-backend context) node (node-begin node)
       (getf (cdr policy) :feature) (getf (cdr policy) :message)))
    (cond
      (transcoder
       (multiple-value-bind (callback owner)
           (export-resolved-map-value
            ancestry #'%export-backend-descriptor-transcoders type)
         (let* ((callback-context (export-context-for-owner context owner))
                (*export-active-node-dispatch*
                  (list callback-context node view frame ancestry)))
           (funcall callback node view frame
                    callback-context))))
      ((export-resolved-component
        ancestry #'%export-backend-descriptor-native-sink-renderer)
       (export-render-native-sink-implementation
        context node view frame ancestry))
      (t
       (with-output-to-string (output)
         (when view
           (let ((offset (%export-render-frame-output-offset frame)))
             (dolist (child (%export-view-children view))
               (let* ((*export-current-output-offset* offset)
                      (rendered
                        (export-render-view
                         context child
                         :mode (%export-render-frame-mode frame)
                         :table-cell-p
                         (%export-render-frame-table-cell-p frame)
                         :preserve-source-breaks-p
                         (%export-render-frame-preserve-source-breaks-p frame))))
                 (write-string rendered output)
                 (incf offset (length rendered)))))))))))

(defun export-render-view (context view &key (mode :block) table-cell-p
                                           preserve-source-breaks-p)
  "Render VIEW with CONTEXT and return its string."
  (export-render-context-check context)
  (export-check-render-not-creating-state)
  (unless (%export-view-p view)
    (error 'type-error :datum view :expected-type 'export-view))
  (export-render-node context (%export-view-node view) :view view :mode mode
                      :table-cell-p table-cell-p
                      :preserve-source-breaks-p preserve-source-breaks-p))

(defun export-node-post-blank-character (node)
  (if (eq (node-type-category (node-type node)) :object)
      #\Space
      #\Newline))

(defun export-node-post-blank-string (node)
  (make-string (node-post-blank node)
               :initial-element (export-node-post-blank-character node)))

(defun export-node-strip-post-blank (node rendered)
  (let* ((count (node-post-blank node))
         (start (- (length rendered) count))
         (character (export-node-post-blank-character node)))
    (if (and (plusp count) (not (minusp start))
             (loop for index from start below (length rendered)
                   always (char= (char rendered index) character)))
        (subseq rendered 0 start)
        rendered)))

(defun export-render-view-to-sink (context sink view &key (mode :block)
                                                       table-cell-p
                                                       preserve-source-breaks-p)
  (export-render-context-check context)
  (export-check-render-not-creating-state)
  (unless (%export-view-p view)
    (error 'type-error :datum view :expected-type 'export-view))
  (unless (member mode '(:block :inline :object) :test #'eq)
    (error 'type-error :datum mode
           :expected-type '(member :block :inline :object)))
  (unless (member table-cell-p '(nil t))
    (error 'type-error :datum table-cell-p :expected-type 'boolean))
  (unless (member preserve-source-breaks-p '(nil t))
    (error 'type-error :datum preserve-source-breaks-p :expected-type 'boolean))
  (let* ((node (%export-view-node view))
         (type (node-type node))
         (ancestry (%export-render-context-ancestry context))
         (policy (export-resolved-node-policy ancestry type))
         (native-renderer-and-owner
           (multiple-value-list
            (export-resolved-component
             ancestry #'%export-backend-descriptor-native-sink-renderer)))
         (native-scratch-and-owner
           (multiple-value-list
            (export-resolved-component
             ancestry #'%export-backend-descriptor-native-sink-scratch)))
         (native-string-and-owner
           (multiple-value-list
            (export-resolved-component
             ancestry #'%export-backend-descriptor-native-sink-string)))
         (native-write-and-owner
           (multiple-value-list
            (export-resolved-component
             ancestry #'%export-backend-descriptor-native-sink-write-string)))
         (native-append-and-owner
           (multiple-value-list
            (export-resolved-component
             ancestry
             #'%export-backend-descriptor-native-sink-append-node-result)))
         (native-offset-and-owner
           (multiple-value-list
            (export-resolved-component
             ancestry #'%export-backend-descriptor-native-sink-output-offset)))
         (transcoder-and-owner
           (multiple-value-list
            (export-resolved-map-value
             ancestry #'%export-backend-descriptor-transcoders type)))
         (wrapper-and-owner
           (multiple-value-list
            (export-resolved-component ancestry
                                       #'%export-backend-descriptor-node-wrapper)))
         (plain-filters
           (and (eq type :plain-text)
                (export-resolved-filters-in-ancestry ancestry :plain-text)))
         (element-filters
           (export-resolved-filters-in-ancestry
            ancestry (list :element type)))
         (scratch-required-p
           (export-render-node-scratch-required-p
            ancestry type (second transcoder-and-owner)
            (second wrapper-and-owner))))
    (when (and (consp policy) (eq (car policy) :unsupported))
      (export-unsupported-syntax
       (%export-render-context-backend context) node (node-begin node)
       (getf (cdr policy) :feature) (getf (cdr policy) :message)))
    (unless (every #'first
                   (list native-renderer-and-owner native-scratch-and-owner
                         native-string-and-owner native-write-and-owner
                         native-append-and-owner native-offset-and-owner))
      (error 'program-error))
    (let* ((sink-owner (second native-renderer-and-owner))
           (root-owner (%export-backend-descriptor-name (first ancestry)))
           (sink-context (export-context-for-owner context sink-owner))
           (wrapper-added-text-p nil)
           (frame (%make-export-render-frame
                   :mode mode :table-cell-p table-cell-p
                   :preserve-source-breaks-p preserve-source-breaks-p
                   :output-offset
                   (funcall (first native-offset-and-owner) sink sink-context)))
           (backend (%export-render-context-backend context))
           (limits (%export-render-context-limits context)))
      (unless (and (integerp (%export-render-frame-output-offset frame))
                   (not (minusp (%export-render-frame-output-offset frame))))
        (error 'type-error
               :datum (%export-render-frame-output-offset frame)
               :expected-type '(integer 0 *)))
      (labels ((sink-offset (target)
                 (let ((offset
                         (funcall (first native-offset-and-owner)
                                  target sink-context)))
                   (unless (and (integerp offset) (not (minusp offset)))
                     (error 'type-error :datum offset
                            :expected-type '(integer 0 *)))
                   offset))
               (sink-string (target)
                 (let ((text (funcall (first native-string-and-owner)
                                      target sink-context)))
                   (unless (stringp text)
                     (error 'type-error :datum text :expected-type 'string))
                   text))
               (sink-write-string (target text)
                 (let* ((*export-current-output-offset* (sink-offset target))
                        (*export-current-native-sink* target)
                        (result
                          (funcall (first native-write-and-owner)
                                   target text sink-context)))
                   (unless (null result)
                     (error 'type-error :datum result :expected-type 'null))))
               (sink-append-node-result (target text)
                 (let* ((*export-current-output-offset* (sink-offset target))
                        (*export-current-native-sink* target)
                        (result
                          (funcall (first native-append-and-owner)
                                   target text sink-context)))
                   (unless (null result)
                     (error 'type-error :datum result :expected-type 'null))))
               (implementation-frame (selected-frame offset)
                 (%make-export-render-frame
                  :mode (%export-render-frame-mode selected-frame)
                  :table-cell-p (%export-render-frame-table-cell-p selected-frame)
                  :preserve-source-breaks-p
                  (%export-render-frame-preserve-source-breaks-p selected-frame)
                  :output-offset offset))
               (render-selected-raw (target selected-frame)
                 (let* ((offset (sink-offset target))
                        (*export-current-output-offset* offset)
                        (*export-current-native-sink* target)
                        (callback-context
                          (export-context-for-owner
                           context (if (first transcoder-and-owner)
                                       (second transcoder-and-owner)
                                       sink-owner))))
                   (if (first transcoder-and-owner)
                       (let ((text
                               (export-render-node-implementation
                                context node view selected-frame ancestry)))
                         (unless (stringp text)
                           (error 'type-error :datum text
                                  :expected-type 'string))
                         (sink-write-string target text))
                     (let ((*export-active-node-dispatch*
                             (list callback-context node view selected-frame
                                   ancestry))
                           (result
                             (funcall (first native-renderer-and-owner)
                                      target node view selected-frame
                                      callback-context)))
                       (unless (null result)
                         (error 'type-error :datum result
                                :expected-type 'null))))))
               (render-selected (target selected-frame)
                 (if scratch-required-p
                     (let* ((offset (sink-offset target))
                            (selected-frame
                              (implementation-frame selected-frame offset))
                            (scratch
                              (funcall (first native-scratch-and-owner)
                                       target selected-frame sink-context))
                            (*export-current-export-phase*
                              *export-current-export-phase*)
                            (*export-current-output-offset* offset)
                            (*export-current-native-sink* scratch))
                       (render-selected-raw scratch selected-frame)
                       (let* ((native-result (sink-string scratch))
                              (checked-native-result
                                (export-check-output-stage
                                 native-result limits backend
                                 *export-current-export-phase*
                                 :base-offset offset
                                 :actual-offset (sink-offset scratch)
                                 :kind :node-scratch))
                              (plain-result
                                (if plain-filters
                                    (let ((*export-current-output-offset* offset))
                                      (export-apply-filter-stage
                                       backend :plain-text checked-native-result
                                       context))
                                    checked-native-result)))
                         (sink-write-string target plain-result)))
                   (render-selected-raw target selected-frame)))
               (render-with-wrapper (target selected-frame)
                 (if (first wrapper-and-owner)
                     (let* ((calls 0)
                            (wrapper-context
                              (export-context-for-owner
                               context (second wrapper-and-owner)))
                            (*export-active-node-dispatch*
                              (list wrapper-context node view selected-frame
                                    ancestry))
                            (*export-current-native-sink* target)
                            (wrapper-result
                              (funcall
                               (first wrapper-and-owner) node view selected-frame
                               wrapper-context
                               (lambda (prefix suffix)
                                 (unless (and (stringp prefix) (stringp suffix))
                                   (error 'type-error
                                          :datum (list prefix suffix)
                                          :expected-type '(cons string string)))
                                 (incf calls)
                                 (when (or (plusp (length prefix))
                                           (plusp (length suffix)))
                                   (setf wrapper-added-text-p t))
                                 (sink-write-string target prefix)
                                 (render-selected
                                  target
                                  (implementation-frame
                                   selected-frame (sink-offset target)))
                                 (sink-write-string target suffix)
                                 nil))))
                       (unless (null wrapper-result)
                         (error 'type-error :datum wrapper-result
                                :expected-type 'null))
                       (unless (= calls 1)
                         (error 'program-error)))
                   (render-selected target selected-frame)))
               (render-scratch ()
                 (let* ((scratch
                          (funcall (first native-scratch-and-owner)
                                   sink frame sink-context))
                        (base-offset
                          (%export-render-frame-output-offset frame))
                        (*export-current-export-phase*
                          *export-current-export-phase*)
                        (*export-current-output-offset* base-offset)
                        (*export-current-native-sink* scratch))
                   (render-with-wrapper scratch frame)
                   (let* ((native-result (sink-string scratch))
                          (with-post-blank
                            (concatenate 'string native-result
                                         (export-node-post-blank-string node)))
                          (element-result
                            (if element-filters
                                (let ((*export-current-output-offset* base-offset))
                                  (export-apply-filter-stage
                                   backend (list :element type) with-post-blank
                                   context))
                                with-post-blank))
                          (preview
                            (funcall (first native-scratch-and-owner)
                                     sink frame sink-context)))
                     (sink-append-node-result preview element-result)
                     (let ((committed
                             (export-check-output-stage
                              element-result limits backend
                              *export-current-export-phase*
                              :base-offset base-offset
                              :actual-offset (sink-offset preview)
                              :kind :node-commit)))
                       (sink-append-node-result sink committed))))))
        (if scratch-required-p
            (render-scratch)
          (progn
            (render-with-wrapper sink frame)
            (when (plusp (node-post-blank node))
              (sink-write-string sink (export-node-post-blank-string node)))
            (when (and wrapper-added-text-p
                       (first wrapper-and-owner)
                       (eq (second wrapper-and-owner) root-owner))
              (export-check-output-stage
               "" limits backend *export-current-export-phase*
               :actual-offset (sink-offset sink) :kind :native-write))))
        nil))))

(defun export-render-node-scratch-required-p
    (ancestry type transcoder-owner wrapper-owner)
  (let ((root-owner (and ancestry
                         (%export-backend-descriptor-name (first ancestry)))))
    (or (and transcoder-owner (not (eq transcoder-owner root-owner)))
        (and wrapper-owner (not (eq wrapper-owner root-owner)))
        (and (eq type :plain-text)
             (export-resolved-filters-in-ancestry ancestry :plain-text))
        (export-resolved-filters-in-ancestry
         ancestry (list :element type)))))

(defun export-render-node (context node &key view (mode :block) table-cell-p
                                         preserve-source-breaks-p)
  "Render NODE with CONTEXT and return its string."
  (export-render-context-check context)
  (export-check-render-not-creating-state)
  (unless (node-p node)
    (error 'type-error :datum node :expected-type 'node))
  (unless (member mode '(:block :inline :object) :test #'eq)
    (error 'type-error :datum mode :expected-type '(member :block :inline :object)))
  (unless (member table-cell-p '(nil t))
    (error 'type-error :datum table-cell-p :expected-type 'boolean))
  (unless (member preserve-source-breaks-p '(nil t))
    (error 'type-error :datum preserve-source-breaks-p :expected-type 'boolean))
  (when view
    (unless (and (%export-view-p view) (eq node (%export-view-node view)))
      (error 'type-error :datum view :expected-type 'matching-export-view)))
  (let* ((ancestry (%export-render-context-ancestry context))
         (frame (%make-export-render-frame
                 :mode mode :table-cell-p table-cell-p
                 :preserve-source-breaks-p preserve-source-breaks-p
                 :output-offset *export-current-output-offset*))
         (transcoder-and-owner
           (multiple-value-list
            (export-resolved-map-value
             ancestry #'%export-backend-descriptor-transcoders (node-type node))))
         (wrapper-and-owner
           (multiple-value-list
            (export-resolved-component ancestry
                                       #'%export-backend-descriptor-node-wrapper)))
         (scratch-required-p
           (export-render-node-scratch-required-p
            ancestry (node-type node) (second transcoder-and-owner)
            (second wrapper-and-owner)))
         (result nil))
    (labels ((render-implementation (offset)
               (let* ((*export-current-output-offset* offset)
                      (implementation
                        (export-check-render-string
                         (export-render-node-implementation
                          context node view frame ancestry)))
                      (checked-implementation
                        (if scratch-required-p
                            (export-check-output-stage
                             implementation
                             (%export-render-context-limits context)
                             (%export-render-context-backend context)
                             *export-current-export-phase*
                             :base-offset offset :kind :node-scratch)
                            implementation)))
                 (if (eq (node-type node) :plain-text)
                     (export-apply-filter-stage
                      (%export-render-context-backend context) :plain-text
                      checked-implementation context)
                     checked-implementation)))
             (render-next (prefix suffix)
               (unless (and (stringp prefix) (stringp suffix))
                 (error 'type-error :datum (list prefix suffix)
                        :expected-type '(cons string string)))
               (setf result
                     (concatenate
                      'string prefix
                      (render-implementation
                       (+ (%export-render-frame-output-offset frame)
                          (length prefix)))
                      suffix))
               nil))
      (if (first wrapper-and-owner)
          (let* ((calls 0)
                 (wrapper-context
                   (export-context-for-owner context
                                             (second wrapper-and-owner)))
                 (*export-active-node-dispatch*
                   (list wrapper-context node view frame ancestry))
                 (wrapper-result
                   (funcall (first wrapper-and-owner)
                            node view frame wrapper-context
                            (lambda (prefix suffix)
                              (incf calls)
                              (render-next prefix suffix)))))
            (unless (null wrapper-result)
              (error 'type-error :datum wrapper-result :expected-type 'null))
            (unless (= calls 1)
              (error 'program-error)))
          (setf result (render-implementation
                        (%export-render-frame-output-offset frame))))
      (when (plusp (node-post-blank node))
        (setf result
              (concatenate 'string result
                           (export-node-post-blank-string node))))
      (setf result
            (export-apply-filter-stage
             (%export-render-context-backend context)
             (list :element (node-type node)) result context))
      (when scratch-required-p
        (export-check-output-stage
         result (%export-render-context-limits context)
         (%export-render-context-backend context)
         *export-current-export-phase*
         :base-offset (%export-render-frame-output-offset frame)
         :kind :node-commit))
      result)))

(defun export-render-parent-node (context node &key (view nil view-p)
                                            (mode nil mode-p)
                                            (table-cell-p nil table-cell-p-p)
                                            (preserve-source-breaks-p nil
                                                                     preserve-breaks-p))
  "Render NODE using the parent backend for the current callback."
  (export-render-context-check context)
  (export-check-render-not-creating-state)
  (unless (node-p node)
    (error 'type-error :datum node :expected-type 'node))
  (unless *export-active-node-dispatch*
    (error 'program-error))
  (unless (and (eq context (first *export-active-node-dispatch*))
               (eq node (second *export-active-node-dispatch*)))
    (error 'program-error))
  (when (and mode-p (not (member mode '(:block :inline :object) :test #'eq)))
    (error 'type-error :datum mode
           :expected-type '(member :block :inline :object)))
  (when (and table-cell-p-p (not (member table-cell-p '(nil t))))
    (error 'type-error :datum table-cell-p :expected-type 'boolean))
  (when (and preserve-breaks-p
             (not (member preserve-source-breaks-p '(nil t))))
    (error 'type-error :datum preserve-source-breaks-p :expected-type 'boolean))
  (let* ((owner (%export-render-context-owner-backend context))
         (owner-descriptor
           (find owner (%export-render-context-ancestry context)
                 :key #'%export-backend-descriptor-name :test #'eq))
         (parent (and owner-descriptor
                      (%export-backend-descriptor-parent owner-descriptor)))
         (ancestry (and parent
                        (export-ancestry-through
                         (%export-render-context-ancestry context) parent)))
         (active-view (if view-p view (third *export-active-node-dispatch*)))
         (active-frame (fourth *export-active-node-dispatch*)))
    (when (and active-view
               (not (and (%export-view-p active-view)
                         (eq node (%export-view-node active-view)))))
      (error 'type-error :datum active-view :expected-type 'matching-export-view))
    (let ((frame (%make-export-render-frame
                  :mode (if mode-p mode
                            (%export-render-frame-mode active-frame))
                  :table-cell-p (if table-cell-p-p table-cell-p
                                    (%export-render-frame-table-cell-p active-frame))
                  :preserve-source-breaks-p
                  (if preserve-breaks-p preserve-source-breaks-p
                      (%export-render-frame-preserve-source-breaks-p active-frame))
                  :output-offset *export-current-output-offset*)))
      (unless parent
        (error 'program-error))
      (let ((result
              (export-render-node-implementation context node active-view frame
                                                 ancestry)))
        (unless (stringp result)
          (error 'type-error :datum result :expected-type 'string))
        result))))

(defun export-render-parent-view (context view &key (mode :block) table-cell-p
                                             preserve-source-breaks-p)
  "Render VIEW using the parent backend for the current callback."
  (export-render-context-check context)
  (export-check-render-not-creating-state)
  (unless (%export-view-p view)
    (error 'type-error :datum view :expected-type 'export-view))
  (unless (member mode '(:block :inline :object) :test #'eq)
    (error 'type-error :datum mode :expected-type '(member :block :inline :object)))
  (unless (member table-cell-p '(nil t))
    (error 'type-error :datum table-cell-p :expected-type 'boolean))
  (unless (member preserve-source-breaks-p '(nil t))
    (error 'type-error :datum preserve-source-breaks-p :expected-type 'boolean))
  (let* ((owner (%export-render-context-owner-backend context))
         (owner-descriptor
           (find owner (%export-render-context-ancestry context)
                 :key #'%export-backend-descriptor-name :test #'eq))
         (parent (and owner-descriptor
                      (%export-backend-descriptor-parent owner-descriptor)))
         (ancestry (and parent
                        (export-ancestry-through
                         (%export-render-context-ancestry context) parent)))
         (frame (%make-export-render-frame
                 :mode mode :table-cell-p table-cell-p
                 :preserve-source-breaks-p preserve-source-breaks-p
                 :output-offset *export-current-output-offset*)))
    (unless ancestry
      (error 'program-error))
    (let ((result
            (export-render-node-implementation
             context (%export-view-node view) view frame ancestry)))
      (unless (stringp result)
        (error 'type-error :datum result :expected-type 'string))
      result)))

(defun register-export-link-type (backend type function)
  "Register TYPE on BACKEND with a (path description frame context) handler."
  (unless (keywordp backend)
    (error 'type-error :datum backend :expected-type 'keyword))
  (unless (stringp type)
    (error 'type-error :datum type :expected-type 'string))
  (unless (functionp function)
    (error 'type-error :datum function :expected-type 'function))
  (unless (plusp (length type))
    (export-registration-error backend :invalid-link-type-handler
                               "Link type must not be empty"))
  (sb-thread:with-mutex (*export-backend-registry-lock*)
    (let ((descriptor (gethash backend *export-backend-registry*)))
      (unless descriptor
        (export-registration-error backend :unknown-backend
                                   "Export backend is not registered"))
      (when (assoc type (%export-backend-descriptor-link-type-handlers descriptor)
                   :test #'string-equal)
        (export-registration-error backend :duplicate-link-type-handler
                                   "Link type handler is already registered"))
      (setf (gethash backend *export-backend-registry*)
            (export-descriptor-with-link-type-handlers
             descriptor
             (append (%export-backend-descriptor-link-type-handlers descriptor)
                     (list (cons (copy-seq type) function)))))))
  backend)

(defun %register-builtin-export-link-type (backend type function)
  (let ((*export-registration-backend* backend))
    (export-registration-check-backend-name backend)
    (unless (stringp type)
      (error 'type-error :datum type :expected-type 'string))
    (unless (functionp function)
      (error 'type-error :datum function :expected-type 'function))
    (unless (plusp (length type))
      (export-registration-error backend :invalid-link-type-handler
                                 "Link type must not be empty"))
    (sb-thread:with-mutex (*export-backend-registry-lock*)
      (let* ((descriptor (gethash backend *export-backend-registry*))
             (builtin-types (gethash backend *export-builtin-link-type-handlers*)))
        (unless descriptor
          (export-registration-error backend :unknown-backend
                                     "Export backend is not registered"))
        (unless (%export-backend-descriptor-builtin-p descriptor)
          (error 'program-error))
        (let* ((entry (assoc type
                             (%export-backend-descriptor-link-type-handlers descriptor)
                             :test #'string-equal))
               (registered-builtin-p
                 (member type builtin-types :test #'string-equal)))
          (when (and entry (not registered-builtin-p))
            (export-registration-error backend :duplicate-link-type-handler
                                       "Link type handler is already registered"))
          (setf (gethash backend *export-backend-registry*)
                (export-descriptor-with-link-type-handlers
                 descriptor
                 (if entry
                     (mapcar (lambda (existing)
                               (if (eq existing entry)
                                   (cons (car existing) function)
                                   existing))
                             (%export-backend-descriptor-link-type-handlers
                              descriptor))
                     (append (%export-backend-descriptor-link-type-handlers
                              descriptor)
                             (list (cons (copy-seq type) function))))))
          (unless registered-builtin-p
            (setf (gethash backend *export-builtin-link-type-handlers*)
                  (append builtin-types (list (copy-seq type))))))))
  backend))

(defun export-resolved-link-handler (ancestry type)
  (export-resolved-map-value
   ancestry #'%export-backend-descriptor-link-type-handlers type #'string-equal))

(defun export-call-link-type-handler (context type path description frame)
  (export-render-context-check context)
  (unless (stringp type)
    (error 'type-error :datum type :expected-type 'string))
  (unless (stringp path)
    (error 'type-error :datum path :expected-type 'string))
  (unless (stringp description)
    (error 'type-error :datum description :expected-type 'string))
  (export-render-frame-check frame)
  (multiple-value-bind (handler owner)
      (export-resolved-link-handler
       (%export-render-context-ancestry context) type)
    (when handler
      (let* ((callback-context (export-context-for-owner context owner))
             (result (funcall handler path description frame callback-context)))
        (unless (%export-link-result-p result)
          (error 'type-error :datum result :expected-type 'export-link-result))
        (make-export-link-result
         :kind (%export-link-result-kind result)
         :destination (%export-link-result-destination result)
         :description (%export-link-result-description result))))))
(defun export-view-plain-text-description (view)
  (let* ((node (export-view-node view))
         (type (node-type node))
         (children (export-view-children view)))
    (labels ((views-description (views)
                                (with-output-to-string (output)
                                  (dolist (child views)
                                    (write-string (export-view-plain-text-description child)
                                                  output)))))
      (case type
        ((:plain-text :text)
         (or (node-text node) (node-property node :value) ""))
        (:entity
         (or (node-property node :utf-8) (node-property node :unicode)
             (node-text node) ""))
        (:line-break (string #\Newline))
        (:link (if children
                   (views-description children)
                 (or (node-property node :raw-link)
                     (node-property node :path) "")))
        ((:code :verbatim :inline-src-block :latex-fragment)
         (or (node-text node) (node-property node :value) ""))
        (:macro (if children (views-description children) ""))
        (otherwise
         (if children
             (views-description children)
           (or (node-text node) ""))))))
  )
(defun export-call-link-type-handler-for-view (context node view frame)
  (let* ((type (node-property node :type))
         (path (node-property node :path))
         (scheme-end (and (stringp type)
                          (string-equal type "fuzzy")
                          (stringp path)
                          (export-explicit-url-scheme-end path)))
         (handler-type (if scheme-end (subseq path 0 scheme-end) type)))
    (when (and context frame (stringp type) (stringp path))
      (multiple-value-bind (handler)
          (export-resolved-link-handler
           (%export-render-context-ancestry context) handler-type)
        (when handler
          (export-call-link-type-handler
           context handler-type
           (if scheme-end (subseq path (1+ scheme-end)) path)
           (export-view-plain-text-description view) frame))))))

(defun unregister-export-link-type (backend type)
  "Remove the handler registered for TYPE on BACKEND."
  (unless (keywordp backend)
    (error 'type-error :datum backend :expected-type 'keyword))
  (unless (stringp type)
    (error 'type-error :datum type :expected-type 'string))
  (sb-thread:with-mutex (*export-backend-registry-lock*)
    (let* ((descriptor (gethash backend *export-backend-registry*))
           (local (and descriptor
                       (%export-backend-descriptor-link-type-handlers descriptor)))
           (entry (and local (assoc type local :test #'string-equal)))
           (builtin-local-p
             (member type (gethash backend *export-builtin-link-type-handlers*)
                     :test #'string-equal)))
      (unless descriptor
        (export-registration-error backend :unknown-backend
                                   "Export backend is not registered"))
      (when (or builtin-local-p
                (and (null entry)
                     (loop for owner in (%export-backend-ancestry-unlocked backend)
                           thereis
                           (member
                            type
                            (gethash (%export-backend-descriptor-name owner)
                                     *export-builtin-link-type-handlers*)
                            :test #'string-equal))))
        (export-registration-error backend :builtin-link-type-handler
                                   "Built-in link handlers cannot be removed"))
      (unless entry
        (export-registration-error backend :unknown-link-type-handler
                                   "Export link type handler is not registered"))
      (setf (gethash backend *export-backend-registry*)
            (export-descriptor-with-link-type-handlers
             descriptor (remove entry local :test #'eq)))))
  backend)

(eval-when (:load-toplevel :execute)
  (sb-thread:with-mutex (*export-backend-registry-lock*)
    (dolist (name '(:html :gfm :ascii :latex))
      (unless (gethash name *export-backend-registry*)
        (setf (gethash name *export-backend-registry*)
              (%make-export-backend-descriptor
               :name name :builtin-p t
               :option-specifications
               (export-common-option-specifications)
               :accepted-keywords
               (export-merge-string-sets
                *export-common-accepted-keywords*
                (when (eq name :latex)
                  '("LATEX_CLASS" "LATEX_CLASS_OPTIONS" "LATEX_HEADER"
                    "LATEX_HEADER_EXTRA" "LATEX" "ATTR_LATEX")))
               :retained-keywords
               (export-merge-string-sets
                *export-common-retained-keywords*
                (when (eq name :latex) '("LATEX" "ATTR_LATEX")))))))))

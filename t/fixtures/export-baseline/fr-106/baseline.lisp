(in-package #:cl-org-kit/test)

(defparameter *export-baseline-revision*
  "ce017bca425b50eff000d1a6986be7084acfd9c3")
(defparameter *export-baseline-source-sha256*
  '(("cl-org-kit.asd" . "4a992fb371ef5b1dc5ad716e7c8188e00240c20e8575b303eabe1ab81dc79a4c")
    ("src/package.lisp" . "f5152125a9fae854f3968cac21d6063b121d153eb52a13369272c4e966dc552e")
    ("src/export-types.lisp" . "05289e6ad693a997bb98b92ad19366d7ade2414ca0dbde1e5fdab3903f5d3f5f")
    ("src/export-core.lisp" . "02fa101fefa192bcef763ab9a8bd4abb2195f626b88985800afbbd505266fade")
    ("src/export-html.lisp" . "24ab7b41d8c71e60dac3383efccca45f355d2af7f88f6f3f2185bd10b4340e12")
    ("src/export-gfm.lisp" . "c378646bda5f41d2082dee75a4efe88df778605e3def694dd44dbea939a5e44b")
    ("src/export-ascii.lisp" . "991b4dfafc295d7ef7a61317287a0161f04ee955d4eab964a308889407914d55")
    ("src/export-latex.lisp" . "64b7ee84602303279a5c60d734ff05738bac278d7a452f6235a0f53bda37e543")
    ("t/export-core-test.lisp" . "a0db28e125f4c156b37d034fe0720a5ad5bd80bd3815fe71f5b9f30d97ee2979")
    ("t/export-html-test.lisp" . "25cc675d004c07abfab7940117ac806c158cc3a9127070d2972288c5504fd5a9")
    ("t/export-gfm-test.lisp" . "663a78ffad4775521dcb23ea088e071414fa1323107119e629da785643ede9ac")
    ("t/export-ascii-test.lisp" . "c672632a0f7da1556e3f7c80739732284f214ae23b61f12a3601b634c9fe0032")
    ("t/export-latex-test.lisp" . "af9600e002ca35951bed2178418c1ffd6f6dfc8fa17d3c29d9e954be01735a5a")))
(defparameter *export-baseline-source-absent*
  '("src/export-registry.lisp"))
(defparameter *export-baseline-target-files*
  '("t/export-core-test.lisp" "t/export-html-test.lisp"
    "t/export-gfm-test.lisp" "t/export-ascii-test.lisp"
    "t/export-latex-test.lisp"))
(defparameter *export-baseline-directory*
  (asdf:system-relative-pathname
   "cl-org-kit" "t/fixtures/export-baseline/fr-106/"))
(defparameter *export-baseline-manifest-path*
  (merge-pathnames "manifest.sexp" *export-baseline-directory*))
(defvar *export-baseline-original-run-all* (symbol-function 'cl-weave:run-all))
(defvar *export-baseline-current-test* nil)
(defvar *export-baseline-current-ordinal* nil)
(defvar *export-baseline-current-phase-cell* nil)
(defvar *export-baseline-current-check-counts* nil)
(defvar *export-baseline-current-check-kind* nil)
(defvar *export-baseline-current-check-index* nil)
(defvar *export-baseline-current-output-text* nil)
(defvar *export-baseline-current-output-present-p* nil)
(defvar *export-baseline-current-responses* nil)
(defvar *export-baseline-current-output-type* nil)
(defvar *export-baseline-current-stream* nil)
(defvar *export-baseline-current-fixtures* nil)
(defvar *export-baseline-assertion-count* nil)
(defvar *export-baseline-last-run-summary* nil)
(defvar *export-baseline-fixtures* nil)
(defvar *export-baseline-captured* nil)
(defvar *export-baseline-call-instrumentation-active-p* nil)

(defclass export-baseline-character-output-proxy
    (sb-gray:fundamental-character-output-stream)
  ((target :initarg :target :reader export-baseline-output-proxy-target)))

(defclass export-baseline-binary-output-proxy
    (sb-gray:fundamental-binary-output-stream)
  ((target :initarg :target :reader export-baseline-output-proxy-target)))

(defmethod stream-element-type ((stream export-baseline-character-output-proxy))
  (stream-element-type (export-baseline-output-proxy-target stream)))

(defmethod stream-element-type ((stream export-baseline-binary-output-proxy))
  (stream-element-type (export-baseline-output-proxy-target stream)))

(defun export-baseline-stream-write-call (function)
  (if (null *export-baseline-current-phase-cell*)
      (funcall function)
      (let ((previous (car *export-baseline-current-phase-cell*)))
        (setf (car *export-baseline-current-phase-cell*) :stream-write)
        (handler-case
            (let ((values (multiple-value-list (funcall function))))
              (setf (car *export-baseline-current-phase-cell*) previous)
              (values-list values))
          (error (condition)
            (setf (car *export-baseline-current-phase-cell*) :stream-write)
            (error condition))))))

(defmethod sb-gray:stream-write-char
    ((stream export-baseline-character-output-proxy) character)
  (export-baseline-stream-write-call
   (lambda ()
     (write-char character (export-baseline-output-proxy-target stream)))))

(defmethod sb-gray:stream-write-string
    ((stream export-baseline-character-output-proxy) string &optional start end)
  (export-baseline-stream-write-call
   (lambda ()
     (write-string string (export-baseline-output-proxy-target stream)
                   :start (or start 0) :end (or end (length string))))))

(defmethod sb-gray:stream-write-sequence
    ((stream export-baseline-character-output-proxy) sequence
     &optional start end)
  (export-baseline-stream-write-call
   (lambda ()
     (write-sequence sequence (export-baseline-output-proxy-target stream)
                     :start (or start 0) :end (or end (length sequence))))))

(defmethod sb-gray:stream-write-byte
    ((stream export-baseline-binary-output-proxy) byte)
  (export-baseline-stream-write-call
   (lambda ()
     (write-byte byte (export-baseline-output-proxy-target stream)))))

(defmethod sb-gray:stream-write-sequence
    ((stream export-baseline-binary-output-proxy) sequence
     &optional start end)
  (export-baseline-stream-write-call
   (lambda ()
     (write-sequence sequence (export-baseline-output-proxy-target stream)
                     :start (or start 0) :end (or end (length sequence))))))

(defun export-baseline-utf8-encodable-p (string)
  (loop for character across string
        for code = (char-code character)
        always (or (<= code #xd7ff)
                   (<= #xe000 code #xffff)
                   (<= #x10000 code #x10ffff))))

(defun export-baseline-string-codes (string)
  (loop for character across string collect (char-code character)))

(defun export-baseline-context-extensions (context)
  (let ((accessor (find-symbol "EXPORT-CONTEXT-EXTENSIONS" "ORG-KIT")))
    (when (and accessor (fboundp accessor))
      (funcall accessor context))))

(defun export-baseline-safe-value (value)
  (cond
    ((stringp value)
     (if (export-baseline-utf8-encodable-p value)
         value
         (list :character-codes (export-baseline-string-codes value))))
    ((or (null value) (numberp value) (keywordp value) (characterp value)) value)
    ((symbolp value) value)
    ((org-kit:document-p value)
     (list :document
           :encoding (export-baseline-safe-value (org-kit:document-encoding value))
           :bom-p (org-kit:document-bom-p value)
           :source-name (export-baseline-safe-value
                         (org-kit:document-source-name value))
           :settings (export-baseline-safe-value (org-kit:document-settings value))
           :limits (export-baseline-safe-value (org-kit:document-limits value))
           :object-context (export-baseline-safe-value
                            (org-kit::%document-object-context value))
           :root (export-baseline-safe-value (org-kit:document-root value))))
    ((org-kit:node-p value)
     (list :node (org-kit:node-type value)
           :begin (org-kit:node-begin value)
           :end (org-kit:node-end value)
           :contents-begin (org-kit:node-contents-begin value)
           :contents-end (org-kit:node-contents-end value)
           :post-affiliated (org-kit:node-post-affiliated value)
           :post-blank (org-kit:node-post-blank value)
           :properties (export-baseline-safe-value (org-kit:node-properties value))
           :children (mapcar #'export-baseline-safe-value
                             (org-kit:node-children value))))
    ((org-kit:limits-p value)
     (cons :limits
           (loop for name in '(:max-input-length :max-heading-depth
                               :max-element-depth :max-object-depth :max-node-count
                               :max-output-length :max-output-octets
                               :max-agenda-range-days :max-agenda-entries
                               :max-future-repeats :max-export-expansion-depth
                               :max-export-expanded-characters
                               :max-export-expanded-nodes :max-formula-tokens
                               :max-formula-ast-depth :max-formula-precision
                               :max-formula-exponent :max-formula-work
                               :max-write-plan-entries :max-write-plan-bytes
                               :max-write-plan-path-bytes :max-write-plan-path-depth)
                 collect (list name (org-kit:limit-value value name)))))
    ((org-kit:export-context-p value)
     (list :export-context
           :title (export-baseline-safe-value (org-kit:export-context-title value))
           :author (export-baseline-safe-value (org-kit:export-context-author value))
           :date (export-baseline-safe-value (org-kit:export-context-date value))
           :language (export-baseline-safe-value
                      (org-kit:export-context-language value))
           :time-context (export-baseline-safe-value
                          (org-kit:export-context-time-context value))
           :extensions (export-baseline-safe-value
                        (export-baseline-context-extensions value))))
    ((and (fboundp 'org-kit:org-time-context-p)
          (org-kit:org-time-context-p value))
     (list :org-time-context
           :reference-time
           (export-baseline-safe-value
            (org-kit:org-time-context-reference-time value))
           :zone (export-baseline-safe-value
                  (org-kit:org-time-context-zone value))
           :dst-policy (export-baseline-safe-value
                        (org-kit:org-time-context-dst-policy value))))
    ((functionp value)
     (multiple-value-bind (lambda-expression closure-p name)
         (function-lambda-expression value)
       (list :function
             :name (export-baseline-safe-value name)
             :closure-p closure-p
             :lambda-expression
             (export-baseline-safe-value lambda-expression))))
    ((consp value)
     (cons (export-baseline-safe-value (car value))
           (export-baseline-safe-value (cdr value))))
    ((and (vectorp value) (not (stringp value)))
     (list :vector (map 'list #'export-baseline-safe-value value)))
    ((hash-table-p value)
     (list :hash-table
           (sort (loop for key being the hash-keys of value
                         using (hash-value item)
                       collect (list (export-baseline-safe-value key)
                                     (export-baseline-safe-value item)))
                 #'string< :key #'prin1-to-string)))
    (t
     (list :opaque-value
           (princ-to-string (type-of value))
           (handler-case (prin1-to-string value)
             (error () "<unprintable>"))))))

(defun export-baseline-limit-snapshot (limits)
  (cdr (export-baseline-safe-value limits)))

(defun export-baseline-input-snapshot (input)
  (if (org-kit:document-p input)
      (list :input (export-baseline-safe-value (org-kit:document-text input))
            :input-kind :document
            :document-state (export-baseline-safe-value input))
      (list :input (export-baseline-safe-value input)
            :input-kind (if (stringp input) :string (type-of input)))))

(defun export-baseline-context-snapshot (context)
  (export-baseline-safe-value context))

(defun export-baseline-stream-snapshot (stream)
  (when stream
    (list :present t
          :class (export-baseline-safe-value (class-name (class-of stream)))
          :element-type (export-baseline-safe-value (stream-element-type stream))
          :input-p (input-stream-p stream)
          :output-p (output-stream-p stream)
          :initial-content nil)))

(defun export-baseline-stream-proxy (stream output-type)
  (when (and (eq output-type :stream)
             (streamp stream)
             (output-stream-p stream))
    (let ((element-type (stream-element-type stream)))
      (multiple-value-bind (character-p character-known-p)
          (subtypep element-type 'character)
        (cond
          ((and character-known-p character-p)
           (make-instance 'export-baseline-character-output-proxy :target stream))
          ((equal element-type '(unsigned-byte 8))
           (make-instance 'export-baseline-binary-output-proxy :target stream)))))))

(defun export-baseline-condition-readers (condition)
  (let ((readers '()))
    (labels ((record (reader)
               (multiple-value-bind (symbol status)
                   (find-symbol (string-upcase reader) :org-kit)
                 (when (and symbol (eq status :external) (fboundp symbol))
                   (push (list symbol
                               (export-baseline-safe-value
                                (funcall symbol condition)))
                         readers)))))
      (when (typep condition 'org-kit:org-kit-error)
        (record "ORG-KIT-ERROR-MESSAGE"))
      (when (typep condition 'org-kit:org-resource-limit-error)
        (mapc #'record '("ORG-RESOURCE-LIMIT-ERROR-LIMIT-NAME"
                         "ORG-RESOURCE-LIMIT-ERROR-LIMIT"
                         "ORG-RESOURCE-LIMIT-ERROR-ACTUAL")))
      (when (typep condition 'org-kit:org-export-error)
        (mapc #'record '("ORG-EXPORT-ERROR-BACKEND" "ORG-EXPORT-ERROR-REASON"
                         "ORG-EXPORT-ERROR-SOURCE")))
      (when (typep condition 'org-kit:org-export-unsupported-syntax)
        (mapc #'record '("ORG-EXPORT-UNSUPPORTED-SYNTAX-BACKEND"
                         "ORG-EXPORT-UNSUPPORTED-SYNTAX-SOURCE"
                         "ORG-EXPORT-UNSUPPORTED-SYNTAX-POSITION"
                         "ORG-EXPORT-UNSUPPORTED-SYNTAX-FEATURE")))
      (when (typep condition 'org-kit:org-parse-error)
        (mapc #'record '("ORG-PARSE-ERROR-OFFSET" "ORG-PARSE-ERROR-LINE"
                         "ORG-PARSE-ERROR-COLUMN" "ORG-PARSE-ERROR-CONTEXT"
                         "ORG-PARSE-ERROR-SOURCE-NAME")))
      (when (typep condition 'org-kit:org-callback-error)
        (mapc #'record '("ORG-CALLBACK-ERROR-KIND" "ORG-CALLBACK-ERROR-NAME"
                         "ORG-CALLBACK-ERROR-REASON")))
      (when (typep condition 'type-error)
        (push (list 'cl:type-error-datum
                    (export-baseline-safe-value (type-error-datum condition)))
              readers)
        (push (list 'cl:type-error-expected-type
                    (export-baseline-safe-value
                     (type-error-expected-type condition)))
              readers)))
    (nreverse readers)))

(defun export-baseline-condition-snapshot (condition)
  (export-baseline-condition-readers condition))

(defun export-baseline-test-filestring (test-case)
  (let* ((location (cl-weave::test-case-location test-case))
         (file (and (listp location) (getf location :file))))
    (and file (namestring (pathname file)))))

(defun export-baseline-target-file (test-case)
  (let ((path (export-baseline-test-filestring test-case)))
    (and path
         (find-if (lambda (file) (search file path :from-end t))
                  *export-baseline-target-files*))))

(defun export-baseline-test-name (suite test-case)
  (format nil "~{~A~^ > ~}"
          (append (mapcar #'cl-weave::suite-name
                          (rest (cl-weave::suite-lineage suite)))
                  (list (cl-weave::test-case-name test-case)))))

(defun export-baseline-frozen-test-p (manifest file test-name)
  (some (lambda (entry)
          (and (equal file (getf entry :test-file))
               (equal test-name (getf entry :test-name))))
        (getf manifest :entries)))

(defun export-baseline-walk-tests (suite function)
  (dolist (child (cl-weave::suite-children suite))
    (if (cl-weave::suite-p child)
        (export-baseline-walk-tests child function)
        (when (cl-weave::test-case-p child)
          (funcall function suite child)))))

(defun export-baseline-install-test-wrappers
    (root manifest &optional target-only-p)
  (let ((saved '())
        (saved-suite-children '())
        (suite-count 0)
        (test-case-count 0)
        (selected-files nil)
        (selected-test-counts (make-hash-table :test #'equal))
        (complete-p nil))
    (unwind-protect
         (progn
           (when target-only-p
             (labels ((prune-suite (suite root-p)
                        (let ((children (cl-weave::suite-children suite)))
                          (push (cons suite children) saved-suite-children)
                          (setf (cl-weave::suite-children suite)
                                (remove-if-not
                                 (lambda (child)
                                   (cond
                                     ((cl-weave::suite-p child)
                                      (prune-suite child nil))
                                     ((cl-weave::test-case-p child)
                                      (let ((file
                                              (export-baseline-target-file child)))
                                        (when file
                                          (incf test-case-count)
                                          (incf (gethash file selected-test-counts 0))
                                          (push file selected-files))
                                        file))
                                     (t nil)))
                                 children))
                          (when (and (not root-p)
                                     (cl-weave::suite-children suite))
                            (incf suite-count))
                          (or root-p
                              (not (null (cl-weave::suite-children suite)))))))
               (prune-suite root t)
             (let ((files
                     (remove-if-not
                      (lambda (file)
                        (member file selected-files :test #'equal))
                      *export-baseline-target-files*)))
               (export-baseline-check
                (plusp suite-count)
                "FR-106 target suite selection found no suites")
               (export-baseline-check
                (plusp test-case-count)
                "FR-106 target suite selection found no test cases")
               (export-baseline-check
                (equal files *export-baseline-target-files*)
                "FR-106 target suite selection did not include exactly all five files")
               (export-baseline-check
                (every (lambda (file)
                         (plusp (gethash file selected-test-counts 0)))
                       *export-baseline-target-files*)
                "FR-106 target suite selection has an empty export suite")
               (setf selected-files files)
               (progn
                 (format t
                         "FR-106 selected suites: suite-node-count=~D test-case-count=~D target-file-count=~D file-test-counts=~{~A=~D~^, ~} files=~{~A~^, ~}~%"
                         suite-count test-case-count (length selected-files)
                         (loop for file in *export-baseline-target-files*
                               append (list file
                                            (gethash file selected-test-counts 0)))
                         selected-files)
                 (format t
                         "FR-106 resolved source paths: baseline-revision=~A system-definition=~A harness=~A production=~{~A~^, ~} tests=~{~A~^, ~}~%"
                         *export-baseline-revision*
                         (namestring
                          (asdf:system-relative-pathname
                           "cl-org-kit" "cl-org-kit.asd"))
                         (namestring
                          (asdf:system-relative-pathname
                           "cl-org-kit"
                           "t/fixtures/export-baseline/fr-106/baseline.lisp"))
                         (mapcar
                          (lambda (file)
                            (namestring
                             (asdf:system-relative-pathname "cl-org-kit" file)))
                          '("src/export-types.lisp" "src/export-core.lisp"
                            "src/export-html.lisp" "src/export-gfm.lisp"
                            "src/export-ascii.lisp" "src/export-latex.lisp"))
                         (mapcar
                          (lambda (file)
                            (namestring
                             (asdf:system-relative-pathname "cl-org-kit" file)))
                          *export-baseline-target-files*))))))
           (export-baseline-walk-tests
            root
            (lambda (suite test-case)
              (let* ((file (export-baseline-target-file test-case))
                     (test-name (and file
                                     (export-baseline-test-name suite test-case))))
                (when (and file
                           (or target-only-p
                               (null manifest)
                               (export-baseline-frozen-test-p
                                manifest file test-name)))
                  (let ((old-function (cl-weave::test-case-function test-case)))
                    (push (cons test-case old-function) saved)
                    (setf (cl-weave::test-case-function test-case)
                          (lambda (&rest arguments)
                            (let ((*export-baseline-current-test*
                                    (list :test-file file :test-name test-name))
                                  (*export-baseline-current-ordinal* 0))
                              (export-baseline-with-instrumentation
                               (lambda ()
                                 (apply old-function arguments)))))))))))
           (setf complete-p t)
           (list :saved-tests saved
                 :saved-suite-children saved-suite-children
                 :suite-count suite-count
                 :test-case-count test-case-count
                 :files selected-files)
      (unless complete-p
        (dolist (entry saved)
          (setf (cl-weave::test-case-function (car entry)) (cdr entry)))
        (dolist (entry saved-suite-children)
          (setf (cl-weave::suite-children (car entry)) (cdr entry))))))))

(defun export-baseline-stable-hash (string)
  (loop with hash = #x811c9dc5
        for character across string
        do (setf hash
                 (logand #xffffffff
                         (* (logxor hash (char-code character)) #x01000193)))
        finally (return hash)))

(defun export-baseline-invocation-id (file test-name ordinal)
  (let* ((slug (string-downcase
                (subseq file 0 (- (length file) (length ".lisp")))))
         (hash (export-baseline-stable-hash (concatenate 'string file test-name))))
    (format nil "~A-~8,'0X-~4,'0D" slug hash ordinal)))

(defun export-baseline-record-fixture (id role constructor &rest details)
  (push (append
         (list :id id
               :test-file (getf *export-baseline-current-test* :test-file)
               :test-name (getf *export-baseline-current-test* :test-name)
               :invocation-ordinal *export-baseline-current-ordinal*
               :role role
               :constructor constructor)
         details)
        *export-baseline-current-fixtures*)
  id)

(defun export-baseline-make-stream-fixture (description)
  (let* ((class-name (getf description :class))
         (element-type (getf description :element-type)))
    (cond
      ((and (symbolp class-name)
            (string-equal (symbol-name class-name) "STRING-OUTPUT-STREAM"))
       (make-string-output-stream))
      ((and (symbolp class-name)
            (string-equal (symbol-name class-name) "STRING-INPUT-STREAM"))
       (make-string-input-stream ""))
      ((and (symbolp class-name)
            (string-equal (symbol-name class-name)
                          "EXPORT-CORE-TEST-BINARY-OUTPUT-STREAM"))
       (make-instance class-name :element-type element-type))
      (t
       (error "Cannot reconstruct FR-106 stream fixture ~S" class-name)))))

(defun export-baseline-stream-argument (args id)
  (if (not (member :stream args))
      :absent
      (let ((stream (getf args :stream)))
        (cond
          ((null stream) nil)
          ((streamp stream)
           (let* ((fixture-id (format nil "~A-stream" id))
                  (description (export-baseline-stream-snapshot stream)))
             (export-baseline-record-fixture
              fixture-id :stream 'export-baseline-make-stream-fixture
              :description description)
             fixture-id))
          (t (export-baseline-safe-value stream))))))

(defun export-baseline-make-include-resolver-fixture (responses)
  (let ((remaining (copy-tree responses)))
    (lambda (path parent-source)
      (let ((response (pop remaining)))
        (unless (and response
                     (equal path (getf response :path))
                     (equal parent-source (getf response :parent-source)))
          (error "FR-106 include resolver fixture invocation mismatch"))
        (when (getf response :error)
          (error "FR-106 include resolver fixture has a recorded error"))
        (values-list (getf response :values))))))

(defun export-baseline-context-argument (args)
  (if (not (member :context args))
      :absent
      (export-baseline-context-snapshot (getf args :context))))

(defun export-baseline-invocation-snapshot (id arguments)
  (let* ((responses (reverse *export-baseline-current-responses*))
         (resolver (getf arguments :include-resolver))
         (resolver-id
           (when (functionp resolver)
             (let ((fixture-id (format nil "~A-include-resolver" id)))
               (export-baseline-record-fixture
                fixture-id :include-resolver
                'export-baseline-make-include-resolver-fixture
                :responses (export-baseline-safe-value responses))
               fixture-id))))
    (list :output-type
          (if (member :output-type arguments)
              (export-baseline-safe-value (getf arguments :output-type))
              :absent)
          :stream (export-baseline-stream-argument arguments id)
          :include-resolver
          (cond (resolver-id resolver-id)
                ((member :include-resolver arguments)
                 (export-baseline-safe-value resolver))
                (t :absent))
          :include-responses (export-baseline-safe-value responses)
          :context (export-baseline-context-argument arguments))))

(defun export-baseline-stream-written-octets (stream)
  (let ((accessor (find-symbol "EXPORT-CORE-TEST-STREAM-OCTETS"
                               :cl-org-kit/test))
        (class-name (find-symbol "EXPORT-CORE-TEST-BINARY-OUTPUT-STREAM"
                                 :cl-org-kit/test)))
    (when (and accessor (fboundp accessor) class-name
               (typep stream class-name))
      (let ((octets (funcall accessor stream)))
        (when (typep octets '(vector (unsigned-byte 8)))
          (copy-seq octets))))))

(defun export-baseline-rewrite-include-resolver (arguments responses stream-proxy)
  (loop for (key value) on arguments by #'cddr
        append (list key
                     (cond
                       ((eq key :stream) (or stream-proxy value))
                       ((eq key :include-resolver)
                        (if (functionp value)
                            (let ((resolver-function value))
                              (lambda (path parent-source)
                                (handler-case
                                    (let ((values
                                            (multiple-value-list
                                             (funcall resolver-function
                                                      path parent-source))))
                                      (push (list :path
                                                  (export-baseline-safe-value path)
                                                  :parent-source
                                                  (export-baseline-safe-value
                                                   parent-source)
                                                  :values
                                                  (export-baseline-safe-value values))
                                            (symbol-value responses))
                                      (values-list values))
                                  (error (condition)
                                    (push (list :path
                                                (export-baseline-safe-value path)
                                                :parent-source
                                                (export-baseline-safe-value
                                                 parent-source)
                                                :error
                                                (export-baseline-condition-snapshot
                                                 condition))
                                          (symbol-value responses))
                                    (error condition)))))
                            value))
                       (t value)))))

(defun export-baseline-octets (string)
  (when (export-baseline-utf8-encodable-p string)
    (cl-codec-kit:string-to-octets string :encoding :utf-8)))

(defun export-baseline-output-data
    (values output-type output-text output-present-p stream)
  (let ((value (first values)))
    (case output-type
      (:octets
       (unless (typep value '(vector (unsigned-byte 8)))
         (error "FR-106 octet export returned ~S" (type-of value)))
       (values (list :ok :return-kind :octets) (copy-seq value)))
      (:stream
       (unless (and output-present-p (streamp value) (eq value stream))
         (error "FR-106 stream export did not return the requested stream"))
       (values (list :ok :return-kind :stream)
               (or (export-baseline-stream-written-octets stream)
                   (and (stringp output-text)
                        (export-baseline-octets output-text)))))
      (:string
       (unless (and output-present-p (stringp value))
         (error "FR-106 string export returned ~S" (type-of value)))
       (if (export-baseline-utf8-encodable-p value)
           (values (list :ok :return-kind :string)
                   (export-baseline-octets value))
           (values (list :ok :return-kind :string
                         :character-codes
                         (export-baseline-string-codes value))
                   nil)))
      (otherwise
       (error "FR-106 unexpected successful output type ~S" output-type)))))

(defun export-baseline-explicit-value (arguments key)
  (if (member key arguments)
      (export-baseline-safe-value (getf arguments key))
      :absent))

(defun export-baseline-entry-common (id document backend arguments)
  (append
   (list :id id
         :test-file (getf *export-baseline-current-test* :test-file)
         :test-name (getf *export-baseline-current-test* :test-name)
         :invocation-ordinal *export-baseline-current-ordinal*
         :backend (export-baseline-safe-value backend))
   (export-baseline-input-snapshot document)
   (list :source-name
         (and (org-kit:document-p document)
              (export-baseline-safe-value
               (org-kit:document-source-name document)))
         :limits
         (and (org-kit:document-p document)
              (export-baseline-limit-snapshot
               (org-kit:document-limits document)))
         :options nil
         :raw-flags (list :allow-raw-html
                          (export-baseline-explicit-value
                           arguments :allow-raw-html)
                          :allow-raw-latex
                          (export-baseline-explicit-value
                           arguments :allow-raw-latex))
         :invocation (export-baseline-invocation-snapshot id arguments))))

(defun export-baseline-publish-current-fixtures ()
  (setf *export-baseline-fixtures*
        (nconc (reverse *export-baseline-current-fixtures*)
               *export-baseline-fixtures*)))

(defun export-baseline-current-runtime-phase ()
  (let ((symbol (find-symbol "*EXPORT-CURRENT-EXPORT-PHASE*" :org-kit)))
    (if (and symbol (boundp symbol))
        (symbol-value symbol)
        :absent)))

(defun export-baseline-capture-runtime-phase ()
  (when (and *export-baseline-current-phase-cell*
             (eq (car *export-baseline-current-phase-cell*) :input-validation))
    (let ((phase (export-baseline-current-runtime-phase)))
      (unless (or (eq phase :absent)
                  (eq phase :input-validation))
        (setf (car *export-baseline-current-phase-cell*) phase)))))

(defun export-baseline-record-error
    (id document backend arguments condition)
  (let* ((limit-name
          (and (typep condition 'org-kit:org-resource-limit-error)
               (org-kit:org-resource-limit-error-limit-name condition)))
         (output-check-p (eq limit-name :max-output-length))
         (expected
          (append
           (list :error
                 :condition-type (type-of condition)
                 :requested-backend (export-baseline-safe-value backend)
                 :phase (if (and (eq (car *export-baseline-current-phase-cell*) :input-validation)
                                 (typep condition 'org-kit:org-export-error)
                                 (eq (org-kit:org-export-error-reason condition) :unknown-backend))
                            :preparation
                          (car *export-baseline-current-phase-cell*))
                 :output-check-kind
                 (if output-check-p *export-baseline-current-check-kind* :absent)
                 :output-check-index
                 (if output-check-p *export-baseline-current-check-index* :absent)
                 :readers (export-baseline-condition-snapshot condition)))))
    (push (cons (append (export-baseline-entry-common
                         id document backend arguments)
                        (list :expected expected))
                nil)
          *export-baseline-captured*)
    (export-baseline-publish-current-fixtures)
    (error condition)))

(defun export-baseline-record-call (original document backend arguments)
  (unless (and *export-baseline-current-test*
               *export-baseline-current-ordinal*)
    (return-from export-baseline-record-call
      (apply original document backend arguments)))
  (incf *export-baseline-current-ordinal*)
  (let* ((*export-baseline-current-phase-cell* (list :input-validation))
         (*export-baseline-current-check-counts* (make-hash-table))
         (*export-baseline-current-check-kind* :absent)
         (*export-baseline-current-check-index* :absent)
         (*export-baseline-current-output-text* nil)
         (*export-baseline-current-output-present-p* nil)
         (*export-baseline-current-responses* nil)
         (*export-baseline-current-fixtures* nil)
         (*export-baseline-current-stream* (getf arguments :stream))
         (output-type (getf arguments :output-type :string))
         (*export-baseline-current-output-type* output-type)
         (stream-proxy
           (export-baseline-stream-proxy *export-baseline-current-stream* output-type))
         (id (export-baseline-invocation-id
              (getf *export-baseline-current-test* :test-file)
              (getf *export-baseline-current-test* :test-name)
              *export-baseline-current-ordinal*))
         (byte-file (format nil "bytes/~A.bin" id))
         (rewritten (export-baseline-rewrite-include-resolver
                     arguments '*export-baseline-current-responses* stream-proxy))
         (returned nil)
         (condition nil))
    (export-baseline-check
     (and (stringp id) (stringp (getf *export-baseline-current-test* :test-name))
          (plusp *export-baseline-current-ordinal*))
     "FR-106 invocation has incomplete test identity")
    (handler-case
        (setf returned
              (multiple-value-list
               (handler-bind
                   ((error (lambda (caught)
                             (declare (ignore caught))
                             (export-baseline-capture-runtime-phase))))
                 (if *export-baseline-call-instrumentation-active-p*
                     (apply original document backend rewritten)
                     (export-baseline-with-call-instrumentation
                      (lambda ()
                        (apply original document backend rewritten)))))))
      (error (caught) (setf condition caught)))
    (when (and stream-proxy (eq (first returned) stream-proxy))
      (setf (first returned) *export-baseline-current-stream*))
    (when condition
      (export-baseline-record-error
       id document backend arguments condition))
    (multiple-value-bind (expected bytes)
        (export-baseline-output-data
         returned output-type *export-baseline-current-output-text*
         *export-baseline-current-output-present-p* *export-baseline-current-stream*)
      (when (and (eq output-type :stream)
                 (null bytes)
                 (not (stringp *export-baseline-current-output-text*)))
        (error "FR-106 stream export produced no observable output"))
      (let ((entry (append (export-baseline-entry-common
                            id document backend arguments)
                           (list :expected
                                 (if bytes
                                     (append expected (list :bytes-file byte-file))
                                     expected)))))
        (push (cons entry bytes) *export-baseline-captured*)
        (export-baseline-publish-current-fixtures))
      (values-list returned))))

(defun export-baseline-condition-wrapper-call (original phase arguments)
  (if (null *export-baseline-current-phase-cell*)
      (apply original arguments)
      (let ((previous (car *export-baseline-current-phase-cell*))
            (runtime-phase :absent))
        (setf (car *export-baseline-current-phase-cell*) phase)
        (handler-case
            (let ((values
                    (multiple-value-list
                     (handler-bind
                         ((error (lambda (condition)
                                   (declare (ignore condition))
                                   (setf runtime-phase
                                         (export-baseline-current-runtime-phase)))))
                       (apply original arguments)))))
              (setf (car *export-baseline-current-phase-cell*) previous)
              (values-list values))
          (error (condition)
            (setf (car *export-baseline-current-phase-cell*)
                  (cond
                    ((eq runtime-phase :stream-write) :stream-write)
                    ((and (eq phase :preparation)
                          (member runtime-phase
                                  '(:preamble :body :trailer :template)
                                  :test #'eq))
                     runtime-phase)
                    (t phase)))
            (error condition))))))

(defun export-baseline-install-function-wrapper (symbol wrapper)
  (let ((original (symbol-function symbol))
        (documentation (documentation symbol 'function)))
    (setf (symbol-function symbol) (funcall wrapper original))
    (when documentation
      (setf (documentation symbol 'function) documentation))
    (values original documentation)))

(defun export-baseline-with-call-instrumentation (thunk)
  (let ((saved '()))
    (flet ((install (symbol wrapper)
             (multiple-value-bind (original documentation)
                 (export-baseline-install-function-wrapper symbol wrapper)
               (push (list symbol original documentation) saved)))
           (runtime-value (name fallback)
             (let ((symbol (find-symbol name :org-kit)))
               (if (and symbol (boundp symbol))
                   (symbol-value symbol)
                   fallback)))
           (record-output-check (kind index)
             (setf (gethash kind *export-baseline-current-check-counts*) index
                   *export-baseline-current-check-kind* kind
                   *export-baseline-current-check-index* index)))
      (install 'org-kit:parse-org
               (lambda (original)
                 (lambda (&rest arguments)
                   (export-baseline-condition-wrapper-call
                    original :parse arguments))))
      (install 'org-kit::export-prepare
               (lambda (original)
                 (lambda (&rest arguments)
                   (export-baseline-condition-wrapper-call
                    original :preparation arguments))))
      (install 'org-kit::export-render-preparation
               (lambda (original)
                 (lambda (&rest arguments)
                   (export-baseline-condition-wrapper-call original :body arguments))))
      (install 'org-kit::export-write-output
               (lambda (original)
                 (lambda (&rest arguments)
                   (let ((text (first arguments)))
                     (when *export-baseline-current-test*
                       (setf *export-baseline-current-output-text* text
                             *export-baseline-current-output-present-p* t))
                     (export-baseline-condition-wrapper-call
                      original :final-output arguments)))))
      (install 'org-kit::check-limit
         (lambda (original)
           (lambda (limits name actual)
             (when (and *export-baseline-current-phase-cell*
                        (eq name :max-output-length))
               (let* ((reported-kind
                        (runtime-value
                         "*EXPORT-CURRENT-OUTPUT-CHECK-KIND*" :absent))
                      (reported-index
                        (runtime-value
                         "*EXPORT-CURRENT-OUTPUT-CHECK-INDEX*" :absent))
                      (runtime-check-p
                        (and (not (eq reported-kind :absent))
                             (integerp reported-index)
                             (plusp reported-index)))
                      (kind (if runtime-check-p
                                reported-kind
                                :native-write))
                      (index (if runtime-check-p
                                 reported-index
                                 (1+ (gethash :native-write
                                              *export-baseline-current-check-counts*
                                              0)))))
                 (record-output-check kind index)))
             (handler-bind
                 ((error
                   (lambda (condition)
                     (declare (ignore condition))
                     (when *export-baseline-current-phase-cell*
                       (let ((runtime-phase
                               (runtime-value
                                "*EXPORT-CURRENT-EXPORT-PHASE*" :absent)))
                         (unless (or (eq runtime-phase :absent)
                                     (eq runtime-phase :input-validation))
                           (setf (car *export-baseline-current-phase-cell*)
                                 runtime-phase)))))))
               (funcall original limits name actual)))))
      (unwind-protect
           (let ((*export-baseline-call-instrumentation-active-p* t))
             (funcall thunk))
        (dolist (entry saved)
          (setf (symbol-function (first entry)) (second entry))
          (when (third entry)
            (setf (documentation (first entry) 'function) (third entry))))))))
(defun export-baseline-with-instrumentation (thunk)
  (let ((saved nil))
    (unwind-protect
         (progn
           (multiple-value-bind (original documentation)
               (export-baseline-install-function-wrapper
                'org-kit:export-org
                (lambda (original)
                  (lambda (document backend &rest arguments)
                    (export-baseline-record-call
                     original document backend arguments))))
             (push (list 'org-kit:export-org original documentation) saved))
           (funcall thunk))
      (dolist (entry saved)
        (setf (symbol-function (first entry)) (second entry))
        (when (third entry)
          (setf (documentation (first entry) 'function) (third entry)))))))

(defun export-baseline-read-manifest ()
  (with-open-file (stream *export-baseline-manifest-path*
                          :direction :input :external-format :utf-8)
    (let ((*read-eval* nil))
      (read stream))))

(defun export-baseline-read-octets (relative)
  (with-open-file (stream (merge-pathnames relative *export-baseline-directory*)
                          :direction :input :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length stream)
                             :element-type '(unsigned-byte 8))))
      (read-sequence bytes stream)
      bytes)))

(defun export-baseline-entry-match-p (live frozen)
  (let* ((live-entry (car live))
         (live-bytes (cdr live))
         (expected (getf frozen :expected))
         (relative (getf (rest expected) :bytes-file)))
    (and (equal live-entry frozen)
         (if relative
             (and live-bytes (equalp live-bytes
                                     (export-baseline-read-octets relative)))
             (null live-bytes)))))

(defun export-baseline-check (condition control &rest arguments)
  (unless condition
    (apply #'error control arguments))
  (when *export-baseline-assertion-count*
    (incf *export-baseline-assertion-count*))
  t)

(defun export-baseline-entry-files-cover-targets-p (entries)
  (every (lambda (file)
           (some (lambda (entry) (equal file (getf entry :test-file))) entries))
         *export-baseline-target-files*))

(defun export-baseline-unique-plist-ids-p (records)
  (let ((counts (make-hash-table :test (function equal))))
    (dolist (record records)
      (let ((id (getf record :id)))
        (when (stringp id)
          (setf (gethash id counts)
                (1+ (gethash id counts 0))))))
    (let ((problems
            (loop for record in records
                  for id = (getf record :id)
                  when (or (not (stringp id))
                           (> (gethash id counts 0) 1))
                    collect (list :id id
                                  :test-file (getf record :test-file)
                                  :test-name (getf record :test-name)
                                  :invocation-ordinal
                                  (getf record :invocation-ordinal)
                                  :role (getf record :role)))))
      (values (null problems) problems))))

(defun export-baseline-record-order (left right)
  (let* ((left-file (getf left :test-file))
         (right-file (getf right :test-file))
         (left-file-order (position left-file *export-baseline-target-files*
                                    :test #'equal))
         (right-file-order (position right-file *export-baseline-target-files*
                                     :test #'equal))
         (left-name (getf left :test-name))
         (right-name (getf right :test-name))
         (left-ordinal (getf left :invocation-ordinal))
         (right-ordinal (getf right :invocation-ordinal)))
    (or (< left-file-order right-file-order)
        (and (= left-file-order right-file-order)
             (or (string< left-name right-name)
                 (and (equal left-name right-name)
                      (< left-ordinal right-ordinal)))))))

(defun export-baseline-entry-less-p (left right)
  (export-baseline-record-order (car left) (car right)))

(defun export-baseline-fixture-less-p (left right)
  (if (export-baseline-record-order left right)
      t
      (if (export-baseline-record-order right left)
          nil
          (or (string< (symbol-name (getf left :role))
                       (symbol-name (getf right :role)))
              (and (eq (getf left :role) (getf right :role))
                   (string< (getf left :id) (getf right :id)))))))

(defun export-baseline-entry-shape-p (entry)
  (let ((expected (getf entry :expected)))
    (and (stringp (getf entry :id))
         (member (getf entry :test-file) *export-baseline-target-files*
                 :test #'equal)
         (stringp (getf entry :test-name))
         (integerp (getf entry :invocation-ordinal))
         (plusp (getf entry :invocation-ordinal))
         (listp expected)
         (member (first expected) '(:ok :error) :test #'eq))))

(defun export-baseline-fixture-shape-p (fixture)
  (and (stringp (getf fixture :id))
       (member (getf fixture :test-file) *export-baseline-target-files*
               :test #'equal)
       (stringp (getf fixture :test-name))
       (integerp (getf fixture :invocation-ordinal))
       (plusp (getf fixture :invocation-ordinal))
       (member (getf fixture :role)
               '(:stream :include-resolver :context :backend-value :condition-reader)
               :test #'eq)
       (symbolp (getf fixture :constructor))))

(defun export-baseline-compare (captured live-fixtures)
  (let* ((manifest (export-baseline-read-manifest))
         (frozen (getf manifest :entries))
         (fixtures (getf manifest :fixtures))
         (live (sort (copy-list captured) #'export-baseline-entry-less-p))
         (ordered-live-fixtures
           (sort (copy-list live-fixtures) #'export-baseline-fixture-less-p)))
    (export-baseline-check
     (equal (getf manifest :baseline-revision) *export-baseline-revision*)
     "FR-106 baseline revision mismatch")
    (export-baseline-check
     (equal (getf manifest :baseline-source-sha256)
            *export-baseline-source-sha256*)
     "FR-106 baseline source hashes do not match revision ~A"
     *export-baseline-revision*)
    (export-baseline-check
     (equal (getf manifest :baseline-source-absent)
            *export-baseline-source-absent*)
     "FR-106 baseline source absence does not match revision ~A"
     *export-baseline-revision*)
    (export-baseline-check frozen "FR-106 frozen manifest has no entries")
    (export-baseline-check live "FR-106 capture selected no export-org invocations")
    (export-baseline-check fixtures "FR-106 frozen manifest has no fixtures")
    (export-baseline-check ordered-live-fixtures
                           "FR-106 capture selected no test-defined fixtures")
    (export-baseline-check
     (export-baseline-entry-files-cover-targets-p frozen)
     "FR-106 frozen manifest does not cover all five export suites")
    (export-baseline-check
     (export-baseline-unique-plist-ids-p frozen)
     "FR-106 frozen manifest has duplicate entry IDs")
    (export-baseline-check
     (every #'export-baseline-entry-shape-p frozen)
     "FR-106 frozen manifest has an invalid entry")
    (export-baseline-check
     (export-baseline-unique-plist-ids-p fixtures)
     "FR-106 frozen manifest has duplicate fixture IDs")
    (export-baseline-check
     (every #'export-baseline-fixture-shape-p fixtures)
     "FR-106 frozen manifest has an invalid fixture")
    (multiple-value-bind (unique-p problems)
      (export-baseline-unique-plist-ids-p ordered-live-fixtures)
      (export-baseline-check
       unique-p
       "FR-106 live capture has duplicate fixture IDs: ~S"
       problems))
    (export-baseline-check
     (every #'export-baseline-fixture-shape-p ordered-live-fixtures)
     "FR-106 live capture has an invalid fixture")
    (export-baseline-check
     (= (length fixtures) (length ordered-live-fixtures))
     "FR-106 baseline fixture count mismatch: frozen ~D, live ~D"
     (length fixtures) (length ordered-live-fixtures))
    (loop for live-fixture in ordered-live-fixtures
          for frozen-fixture in fixtures
          do (export-baseline-check
              (equal live-fixture frozen-fixture)
              "FR-106 fixture mismatch for ~A ordinal ~D"
              (getf frozen-fixture :test-name)
              (getf frozen-fixture :invocation-ordinal)))
    (export-baseline-check
     (= (length frozen) (length live))
     "FR-106 baseline entry count mismatch: frozen ~D, live ~D"
     (length frozen) (length live))
    (loop for live-entry in live
          for frozen-entry in frozen
          do (export-baseline-check
              (export-baseline-entry-match-p live-entry frozen-entry)
              "FR-106 baseline mismatch for ~A ordinal ~D"
              (getf frozen-entry :test-name)
              (getf frozen-entry :invocation-ordinal)))
    (length frozen)))

(defun export-baseline-run-all (arguments)
  (let* ((frozen-manifest (export-baseline-read-manifest))
         (root (cl-weave::root-suite))
         (target-only-p
           (equal (uiop:getenv "CL_ORG_EXPORT_BASELINE_TARGET_ONLY") "1"))
         (*export-baseline-captured* nil)
         (*export-baseline-fixtures* nil)
         (*export-baseline-assertion-count* 0)
         (registry-state nil)
         (run-result nil)
         (comparison-count nil))
    (unwind-protect
         (progn
           (setf registry-state
                 (export-baseline-install-test-wrappers
                  root frozen-manifest target-only-p))
           (setf run-result
                 (apply *export-baseline-original-run-all* arguments)))
      (when registry-state
        (dolist (saved (getf registry-state :saved-tests))
          (setf (cl-weave::test-case-function (car saved)) (cdr saved)))
        (dolist (saved (getf registry-state :saved-suite-children))
          (setf (cl-weave::suite-children (car saved)) (cdr saved)))))
    (let ((suite-assertion-count *export-baseline-assertion-count*))
      (export-baseline-check
       (plusp suite-assertion-count)
       "FR-106 baseline suite ran no harness assertions"))
    (export-baseline-check run-result "FR-106 baseline suite did not pass")
    (setf comparison-count
          (export-baseline-compare *export-baseline-captured*
                                   *export-baseline-fixtures*))
    (setf *export-baseline-last-run-summary*
          (list :invocation-count (length *export-baseline-captured*)
                :fixture-count (length *export-baseline-fixtures*)
                :assertion-count *export-baseline-assertion-count*
                :selected-suite-count
                (and target-only-p (getf registry-state :suite-count))
                :selected-test-case-count
                (and target-only-p (getf registry-state :test-case-count))
                :selected-files
                (and target-only-p (getf registry-state :files))
                :comparison-count comparison-count))
    (format t "FR-106 baseline: invocation-count=~D fixture-count=~D assertion-count=~D comparison-count=~D~%"
            (getf *export-baseline-last-run-summary* :invocation-count)
            (getf *export-baseline-last-run-summary* :fixture-count)
            (getf *export-baseline-last-run-summary* :assertion-count)
            (getf *export-baseline-last-run-summary* :comparison-count))
    run-result))

(setf (symbol-function 'cl-weave:run-all)
      (lambda (&rest arguments)
        (export-baseline-run-all arguments)))

(it "checks the frozen FR-106 export baseline manifest"
  (let* ((manifest (export-baseline-read-manifest))
         (entries (getf manifest :entries))
         (fixtures (getf manifest :fixtures)))
    (export-baseline-check entries "FR-106 frozen manifest has no entries")
    (export-baseline-check
     (equal (getf manifest :baseline-revision) *export-baseline-revision*)
     "FR-106 frozen manifest revision mismatch")
    (export-baseline-check
     (equal (getf manifest :baseline-source-sha256)
            *export-baseline-source-sha256*)
     "FR-106 baseline source hashes do not match revision ~A"
     *export-baseline-revision*)
    (export-baseline-check
     (equal (getf manifest :baseline-source-absent)
            *export-baseline-source-absent*)
     "FR-106 baseline source absence does not match revision ~A"
     *export-baseline-revision*)
    (export-baseline-check fixtures "FR-106 frozen manifest has no fixtures")))

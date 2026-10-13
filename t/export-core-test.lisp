;;;; t/export-core-test.lisp
(in-package #:cl-org-kit/test)

(defun export-core-test-lines (&rest lines)
  (format nil "~{~A~^~%~}" lines))

(defun export-core-test-views (views)
  (mapcan (lambda (view)
            (cons view (export-core-test-views
                        (org-kit::export-view-children view))))
          views))

(defun export-core-test-all-views (preparation)
  (export-core-test-views
   (org-kit::export-preparation-root-views preparation)))

(defun export-core-test-view-nodes (preparation type)
  (remove-if-not (lambda (view)
                   (eq (node-type (org-kit::export-view-node view)) type))
                 (export-core-test-all-views preparation)))

(defun export-core-test-catch-error (function)
  (handler-case (progn (funcall function) nil)
    (error (condition) condition)))

(defun export-core-test-with-codec-condition (function-name function)
  (let ((original (symbol-function function-name)))
    (unwind-protect
         (progn
           (setf (symbol-function function-name)
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (error 'cl-codec-kit:unsupported-encoding)))
           (funcall function))
      (setf (symbol-function function-name) original))))

(defun export-core-test-preparation (source &rest arguments)
  (apply #'org-kit::export-prepare (parse-org source) :html arguments))

(defun export-core-test-policy-fixture-source (name)
  (uiop:read-file-string
   (asdf:system-relative-pathname
    "cl-org-kit"
    (concatenate 'string "t/fixtures/export/policy/core/" name))))

(defun export-core-test-limits (name value)
  (apply #'make-limits (list name value)))

(defun export-core-test-prepare-with-limit (source name value)
  (org-kit::export-prepare
   (parse-org source :limits (export-core-test-limits name value)) :html))

(defun export-core-test-document-snapshot (document)
  (let ((nodes nil))
    (map-nodes
     (lambda (node)
       (push (list node (node-parent node) (copy-list (node-children node))
                   (node-begin node) (node-end node)
                   (node-contents-begin node) (node-contents-end node)
                   (node-post-blank node) (copy-tree (node-properties node)))
             nodes))
     (document-root document))
    (list (document-text document) (document-source-name document)
          (copy-tree (document-settings document)) (nreverse nodes))))

(defun export-core-test-root-source-selection-document (with-results-p)
  (let* ((source
           (if with-results-p
               (export-core-test-lines
                "#+SELECT_TAGS: keep" "#+BEGIN_SRC text" "root-secret"
                "#+END_SRC" "#+RESULTS:" ": root-result" "* Kept :keep:")
               (export-core-test-lines
                "#+SELECT_TAGS: keep" "#+BEGIN_SRC text" "root-secret"
                "#+END_SRC" "* Kept :keep:")))
         (document (parse-org source))
         (root (document-root document))
         (keyword nil)
         (source-node nil)
         (results-node nil)
         (heading nil))
    (map-nodes
     (lambda (node)
       (case (node-type node)
         (:keyword (setf keyword node))
         (:src-block (setf source-node node))
         (:fixed-width (when (node-property node :RESULTS)
                         (setf results-node node)))
         (:headline (setf heading node))))
     root)
    (setf (node-children root)
          (append (list keyword source-node)
                  (when (and with-results-p results-node)
                    (list results-node))
                  (list heading)))
    document))

(defun export-core-test-source-nodes (preparation type)
  (mapcar #'org-kit::export-view-node
          (export-core-test-view-nodes preparation type)))

(defclass export-core-test-binary-output-stream
    (sb-gray:fundamental-binary-output-stream)
  ((element-type :initarg :element-type :reader export-core-test-element-type)
   (octets :initform (make-array 0 :element-type '(unsigned-byte 8)
                                 :adjustable t :fill-pointer 0)
           :reader export-core-test-stream-octets)))

(defmethod stream-element-type ((stream export-core-test-binary-output-stream))
  (export-core-test-element-type stream))

(defmethod sb-gray:stream-write-byte
    ((stream export-core-test-binary-output-stream) byte)
  (vector-push-extend byte (export-core-test-stream-octets stream)))

(describe "E1 export context and core preparation"
  (it "runs the export policy fixtures through preparation"
    (let ((cases '("footnotes-after-pruning.org"
                   "include-callback.org"
                   "literal-macro.org"
                   "selection-pruning.org"
                   "selection-source-blocks.org"
                   "source-export-modes.org"
                   "recursive-macro.org"
                   "unsupported-diary.org")))
      (with-soft-assertions
        (dolist (name cases)
          (let* ((source (export-core-test-policy-fixture-source name))
                 (condition
                   (export-core-test-catch-error
                    (lambda ()
                      (org-kit::export-prepare
                       (parse-org source) :html
                       :include-resolver (lambda (path parent-source)
                                           (declare (ignore path parent-source))
                                           "Included paragraph"))))))
            (if (member name '("recursive-macro.org" "unsupported-diary.org")
                        :test #'string=)
                (progn
                  (expect (typep condition 'org-export-unsupported-syntax)
                          :to-be t)
                  (when (typep condition 'org-export-unsupported-syntax)
                    (expect (org-export-unsupported-syntax-feature condition)
                            :to-be (if (string= name "recursive-macro.org")
                                       :recursive-macro :diary-sexp))))
                (expect condition :to-be nil)))))))

  (it "keeps caller metadata immutable and applies caller overrides"
    (let* ((context (make-export-context :title "Caller title"
                                         :author "Caller"
                                         :date "2030-01-02"
                                         :language "fr"))
           (document
             (parse-org (export-core-test-lines
                         "#+TITLE: Document title"
                         "#+AUTHOR: Document author"
                         "#+DATE: 2026-10-13"
                         "#+LANGUAGE: ja"
                         "Body")))
           (preparation (org-kit::export-prepare document :html :context context))
           (metadata (org-kit::export-preparation-metadata preparation)))
      (with-soft-assertions
        (expect (export-context-p context) :to-be t)
        (expect (export-context-title context) :to-equal "Caller title")
        (expect (export-context-author context) :to-equal "Caller")
        (expect (export-context-date context) :to-equal "2030-01-02")
        (expect (export-context-language context) :to-equal "fr")
        (expect (export-context-time-context context) :to-be nil)
        (expect (getf metadata :title) :to-equal "Caller title")
        (expect (getf metadata :author) :to-equal "Caller")
        (expect (getf metadata :date) :to-equal "2030-01-02")
        (expect (getf metadata :language) :to-equal "fr")
        (expect (getf metadata :time-context) :to-be nil)
        (expect (export-context-p nil) :to-be nil)
        (expect (typep (export-core-test-catch-error
                        (lambda () (make-export-context :title 42)))
                       'type-error)
                :to-be t))))

  (it "uses stable metadata defaults without consulting ambient state"
    (let* ((preparation (export-core-test-preparation "Body"))
           (metadata (org-kit::export-preparation-metadata preparation)))
      (with-soft-assertions
        (expect (getf metadata :title) :to-be nil)
        (expect (getf metadata :author) :to-be nil)
        (expect (getf metadata :date) :to-be nil)
        (expect (getf metadata :language) :to-equal "en")
        (expect (getf metadata :time-context) :to-be nil))))

  (it "accepts an explicit time context and rejects an invalid one"
    (let* ((time-context
             (make-time-context :reference-time (make-org-time-value "2030-01-02")
                                :zone (make-fixed-time-zone 0)))
           (context (make-export-context :time-context time-context))
           (invalid
             (export-core-test-catch-error
              (lambda () (make-export-context :time-context :local))))
           (invalid-export
             (export-core-test-catch-error
              (lambda ()
                (org-kit::export-prepare (parse-org "Body") :html
                                          :context :invalid)))))
      (with-soft-assertions
        (expect (export-context-time-context context) :to-be time-context)
        (expect (typep invalid 'type-error) :to-be t)
        (expect (typep invalid-export 'type-error) :to-be t))))

  (it "prepares root body content when selection is inactive"
    (let* ((preparation (export-core-test-preparation "Body"))
           (paragraphs (export-core-test-view-nodes preparation :paragraph)))
      (expect (length paragraphs) :to-be 1)))

  (it "omits nodes mapped to the omit operation"
    (let ((preparation
            (export-core-test-preparation
             (export-core-test-lines
              "#+BEGIN_COMMENT" "hidden comment" "#+END_COMMENT" "Visible"))))
      (let ((comment-view
              (first (export-core-test-view-nodes preparation :comment-block))))
        (with-soft-assertions
          (expect (getf (org-kit::export-view-payload comment-view) :operation)
                  :to-be :omit)
          (expect (org-kit::export-view-children comment-view) :to-be nil)
          (expect (export-core-test-view-nodes preparation :paragraph)
                  :to-be-truthy)))))

  (it "applies explicit tag selection and exclusion before building the TOC"
    (let* ((source
             (export-core-test-lines
              "#+SELECT_TAGS: ship noexport"
              "#+EXCLUDE_TAGS: discard"
              "#+OPTIONS: toc:3 h:3"
              "* Container"
              "** Selected :ship:"
              "*** Nested"
              "** SelectedThenExcluded :ship:discard:"
              "** DefaultSelectTagIsReplaced :export:"
              "** ExplicitlySelectedNoexport :noexport:"))
           (preparation (export-core-test-preparation source))
           (headings (mapcar (lambda (view)
                               (node-property (org-kit::export-view-node view)
                                              :raw-value))
                             (export-core-test-view-nodes preparation :headline)))
           (toc (org-kit::export-preparation-toc preparation)))
      (with-soft-assertions
        (expect headings :to-equal
                '("Container" "Selected" "Nested" "ExplicitlySelectedNoexport"))
        (expect (mapcar (lambda (entry) (getf entry :title)) toc)
                :to-equal headings)
        (expect (mapcar (lambda (entry) (getf entry :level)) toc)
                :to-equal '(1 2 3 2)))))

  (it "keeps default selection and excludes noexport headings"
    (let* ((preparation
             (export-core-test-preparation
              (export-core-test-lines
               "* Normal"
               "* Export tag :export:"
               "* Hidden :noexport:")))
           (headings (mapcar (lambda (view)
                               (node-property (org-kit::export-view-node view)
                                              :raw-value))
                             (export-core-test-view-nodes preparation :headline))))
      (expect headings :to-equal '("Export tag"))))

  (it "does not retain source blocks outside a selected heading"
    (let* ((preparation
             (export-core-test-preparation
              (export-core-test-lines
               "#+SELECT_TAGS: keep"
               "#+BEGIN_SRC text"
               "root-secret"
               "#+END_SRC"
               "#+RESULTS:"
               ": root-result"
               "* Hidden :drop:"
               "#+BEGIN_SRC text"
               "hidden-secret"
               "#+END_SRC"
               "#+RESULTS:"
               ": hidden-result"
               "* Kept :keep:"
               "#+BEGIN_SRC text"
               "kept-code"
               "#+END_SRC")))
           (source-views (export-core-test-view-nodes preparation :src-block)))
      (expect (mapcar (lambda (view)
                        (node-property (org-kit::export-view-node view) :value))
                      source-views)
              :to-equal (list (format nil "kept-code~%")))))

  (it "drops root source blocks and adjacent results while selecting headings"
    (let* ((with-results
             (org-kit::export-prepare
              (export-core-test-root-source-selection-document t) :html))
           (without-results
             (org-kit::export-prepare
              (export-core-test-root-source-selection-document nil) :html)))
      (with-soft-assertions
        (expect (export-core-test-view-nodes with-results :src-block) :to-be nil)
        (expect (export-core-test-view-nodes with-results :fixed-width) :to-be nil)
        (expect (export-core-test-view-nodes with-results :headline) :to-be-truthy)
        (expect (export-core-test-view-nodes without-results :src-block) :to-be nil)
        (expect (export-core-test-view-nodes without-results :headline)
                :to-be-truthy))))

  (it "collects footnotes after pruning, deduplicates references, and rejects bad references"
    (let* ((source
             (export-core-test-lines
              "#+SELECT_TAGS: keep"
              "* Kept :keep:"
              "Reference [fn:shared] and again [fn:shared]."
              "[fn:shared] Selected definition."
              "[fn:unused] Unreferenced definition."
              "* Pruned :drop:"
              "[fn:pruned] Definition outside selection."))
           (preparation (export-core-test-preparation source))
           (footnotes (org-kit::export-preparation-footnotes preparation))
           (unresolved
             (export-core-test-catch-error
              (lambda ()
                (export-core-test-preparation
                 (export-core-test-lines
                  "#+SELECT_TAGS: keep"
                  "* Hidden :drop:"
                  "[fn:missing] Definition outside selection."
                  "* Kept :keep:"
                  "Reference [fn:missing].")))))
           (duplicate
             (export-core-test-catch-error
              (lambda ()
                (export-core-test-preparation
                 (export-core-test-lines
                  "* Duplicate definitions"
                  "[fn:twice] First definition."
                  "[fn:twice] Second definition."))))))
      (with-soft-assertions
        (expect (length footnotes) :to-be 1)
        (expect (getf (first footnotes) :label) :to-equal "shared")
        (expect (getf (first footnotes) :number) :to-be 1)
        (expect (typep unresolved 'org-export-error) :to-be t)
        (when (typep unresolved 'org-export-error)
          (expect (org-export-error-reason unresolved) :to-be :unresolved-footnote))
        (expect (typep duplicate 'org-export-error) :to-be t)
        (when (typep duplicate 'org-export-error)
          (expect (org-export-error-reason duplicate)
                  :to-be :duplicate-footnote-definition)))))

  (it "keeps an anonymous inline footnote at its source position"
    (let* ((preparation (export-core-test-preparation "Inline [fn::note text]."))
           (footnote-views (export-core-test-view-nodes preparation
                                                        :footnote-reference)))
      (with-soft-assertions
        (expect (length footnote-views) :to-be 1)
        (expect (org-kit::export-preparation-footnotes preparation) :to-equal nil)
        (expect (node-property (org-kit::export-view-node (first footnote-views))
                               :type)
                :to-be :inline))))

  (it "signals public export conditions with their backend, reason, and source"
    (let ((unknown-backend
            (export-core-test-catch-error
             (lambda ()
               (org-kit::export-prepare (parse-org "Body") :bogus))))
          (unsupported
            (export-core-test-catch-error
             (lambda ()
               (export-core-test-preparation "%%(org-calendar-holiday)")))))
      (with-soft-assertions
        (expect (typep unknown-backend 'org-export-error) :to-be t)
        (when (typep unknown-backend 'org-export-error)
          (expect (org-export-error-backend unknown-backend) :to-be :bogus)
          (expect (org-export-error-reason unknown-backend) :to-be :unknown-backend)
          (expect (org-export-error-source unknown-backend) :to-be nil))
        (expect (typep unsupported 'org-export-unsupported-syntax) :to-be t)
        (when (typep unsupported 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-backend unsupported) :to-be :html)
          (expect (org-export-unsupported-syntax-feature unsupported)
                  :to-be :diary-sexp)
          (expect (org-export-unsupported-syntax-position unsupported) :to-be 0)
          (expect (stringp (org-export-unsupported-syntax-source unsupported))
                  :to-be t)))))

  (it "maps every known node type and rejects unsupported or unknown syntax"
    (let* ((mapped-types (mapcar #'first org-kit::*export-node-operations*))
           (known-types (node-types))
           (unsupported
             (export-core-test-catch-error
              (lambda ()
                (org-kit::export-node-operation
                 :diary-sexp :gfm (make-node :diary-sexp)))))
           (unknown-keyword
             (export-core-test-catch-error
              (lambda ()
                (export-core-test-preparation "#+UNSUPPORTED: value")))))
      (with-soft-assertions
        (expect (sort (copy-list mapped-types) #'string< :key #'symbol-name)
                :to-equal
                (sort (copy-list known-types) #'string< :key #'symbol-name))
        (expect (org-kit::export-node-operation :headline :html) :to-be :heading)
        (expect (org-kit::export-node-operation :clock :gfm) :to-be :omit)
        (expect (org-kit::export-node-operation :export-block :ascii) :to-be :literal)
        (expect (typep unsupported 'org-export-unsupported-syntax) :to-be t)
        (when (typep unsupported 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-feature unsupported)
                  :to-be :diary-sexp))
        (expect (typep unknown-keyword 'org-export-unsupported-syntax) :to-be t)
        (when (typep unknown-keyword 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-feature unknown-keyword)
                  :to-be :unknown-keyword)))))

  (it "covers export condition, mapping, and escaping boundaries"
    (let* ((unknown-backend
             (export-core-test-catch-error
              (lambda () (org-kit::export-node-operation :paragraph :unknown))))
           (invalid-type
             (export-core-test-catch-error
              (lambda () (org-kit::export-node-operation :unknown :html))))
           (missing-mapping
             (let ((org-kit::*export-node-operations*
                     (remove :paragraph org-kit::*export-node-operations*
                             :key #'first)))
               (export-core-test-catch-error
                (lambda ()
                  (org-kit::export-node-operation
                   :paragraph :html (make-node :paragraph))))))
           (invalid-string
             (export-core-test-catch-error
              (lambda () (org-kit::export-escape-context :html :text 42))))
           (invalid-html-context
             (export-core-test-catch-error
              (lambda () (org-kit::export-escape-context :html :code "x"))))
           (invalid-gfm-context
             (export-core-test-catch-error
              (lambda () (org-kit::export-escape-context :gfm :attribute "x"))))
           (invalid-ascii-context
             (export-core-test-catch-error
              (lambda () (org-kit::export-escape-context :ascii :code "x"))))
           (unknown-escape-backend
             (export-core-test-catch-error
              (lambda () (org-kit::export-escape-context :unknown :text "x"))))
           (source
             (coerce (list #\a (code-char 92) #\Newline #\Return #\Tab
                           (code-char 1) (code-char 128) (code-char #xe9))
                     'string))
           (expected-source
             (coerce (list #\a (code-char 92) (code-char 92) (code-char 92)
                           (char "n" 0) (code-char 92) (char "r" 0)
                           (code-char 92) (char "t" 0)
                           (code-char 92) (char "x" 0) #\0 #\1
                           (code-char 92) (char "x" 0) #\8 #\0 (code-char #xe9))
                     'string))
           (long-source (make-string (1+ org-kit::*condition-text-limit*)
                                     :initial-element #\x))
           (bounded (org-kit::export-bounded-source long-source)))
      (with-soft-assertions
        (expect (typep unknown-backend 'org-export-error) :to-be t)
        (expect (typep invalid-type 'type-error) :to-be t)
        (expect (typep missing-mapping 'org-export-unsupported-syntax) :to-be t)
        (expect (typep invalid-string 'type-error) :to-be t)
        (expect (typep invalid-html-context 'type-error) :to-be t)
        (expect (typep invalid-gfm-context 'type-error) :to-be t)
        (expect (typep invalid-ascii-context 'type-error) :to-be t)
        (expect (typep unknown-escape-backend 'org-export-error) :to-be t)
        (expect (org-kit::export-bounded-source source) :to-equal expected-source)
        (expect (length bounded) :to-be (+ org-kit::*condition-text-limit* 3))
        (expect (subseq bounded 0 org-kit::*condition-text-limit*)
                :to-equal (subseq long-source 0 org-kit::*condition-text-limit*))
        (expect (subseq bounded (- (length bounded) 3)) :to-equal "...")
        (expect (org-kit::export-bounded-source nil) :to-be nil)
        (expect (org-kit::export-bounded-source 42) :to-be nil)
        (expect (org-kit::export-escape-context :html :text "plain")
                :to-equal "plain")
        (expect (org-kit::export-gfm-code-fence "plain") :to-equal "~~~")
        (expect (org-kit::export-gfm-code-fence "~~~") :to-equal "~~~~")
        (expect (org-kit::export-gfm-code-span "plain") :to-equal "`plain`")
        (expect (org-kit::export-gfm-code-span " plain ")
                :to-equal "`  plain  `")
        (expect (org-kit::export-gfm-code-span "") :to-equal "``"))))

  (it "validates expanded URLs and escapes backend text contexts"
    (let* ((document
             (let ((document
                     (parse-org
                      (export-core-test-lines
                       "#+LINK: docs https://docs.example/"
                       "#+LINK: bad javascript:" "Body"))))
               (setf (getf (document-settings document) :link-abbrevs)
                     (append (getf (document-settings document) :link-abbrevs)
                             (list (cons "control"
                                         (concatenate
                                          'string "https://docs.example/"
                                          (string (code-char 1))))
                                   (cons "space" " https://docs.example/"))))
               document))
           (safe-abbreviation
             (multiple-value-list
              (org-kit::export-validate-url document "docs:guide")))
           (unsafe-expanded-control
             (multiple-value-list
              (org-kit::export-validate-url document "control:guide")))
           (unsafe-leading-whitespace
             (multiple-value-list
              (org-kit::export-validate-url document "space:guide")))
           (relative-with-invalid-scheme-character
             (multiple-value-list
              (org-kit::export-validate-url document "a_b:relative")))
           (non-ascii-scheme-character
             (org-kit::export-url-scheme "aé:relative"))
           (unicode-scheme (org-kit::export-url-scheme "éxample:relative"))
           (punctuated-scheme (org-kit::export-url-scheme "foo+bar:relative"))
           (short-relative
             (multiple-value-list
              (org-kit::export-validate-url document "x")))
           (empty-leading-whitespace
             (org-kit::export-url-leading-whitespace-p ""))
           (unsafe-abbreviation
             (multiple-value-list
              (org-kit::export-validate-url document "bad:run")))
           (unsafe-scheme
             (multiple-value-list
              (org-kit::export-validate-url document "data:text/html,boom")))
           (safe-scheme
             (multiple-value-list
              (org-kit::export-validate-url document "HtTpS://docs.example/")))
           (safe-relative
             (multiple-value-list
              (org-kit::export-validate-url document "relative/page")))
           (unsafe-protocol-relative
             (multiple-value-list
              (org-kit::export-validate-url document "//outside.example/")))
           (unsafe-backslash
             (multiple-value-list
              (org-kit::export-validate-url document "https:\\outside.example")))
           (unsafe-control
             (multiple-value-list
              (org-kit::export-validate-url document
                                            (format nil "https://docs.example/~%x"))))
           (invalid-document
             (export-core-test-catch-error
              (lambda () (org-kit::export-validate-url nil "https://docs.example/"))))
           (invalid-destination
             (export-core-test-catch-error
              (lambda () (org-kit::export-validate-url document nil)))))
      (with-soft-assertions
        (expect (first safe-abbreviation) :to-be-truthy)
        (expect (second safe-abbreviation) :to-equal "https://docs.example/guide")
        (expect unsafe-expanded-control
                :to-equal (list nil (concatenate 'string "https://docs.example/"
                                                 (string (code-char 1)) "guide")))
        (expect (first unsafe-leading-whitespace) :to-be nil)
        (expect (first relative-with-invalid-scheme-character) :to-be-truthy)
        (expect (second relative-with-invalid-scheme-character)
                :to-equal "a_b:relative")
        (expect non-ascii-scheme-character :to-be nil)
        (expect unicode-scheme :to-be nil)
        (expect punctuated-scheme :to-equal "foo+bar")
        (expect short-relative :to-equal '(t "x"))
        (expect empty-leading-whitespace :to-be nil)
        (expect unsafe-abbreviation :to-equal '(nil "javascript:run"))
        (expect unsafe-scheme :to-equal '(nil "data:text/html,boom"))
        (expect (first safe-scheme) :to-be-truthy)
        (expect (second safe-scheme) :to-equal "HtTpS://docs.example/")
        (expect safe-relative :to-equal '(t "relative/page"))
        (expect unsafe-protocol-relative :to-equal '(nil "//outside.example/"))
        (expect unsafe-backslash :to-equal '(nil "https:\\outside.example"))
        (expect (first unsafe-control) :to-be nil)
        (expect (typep invalid-document 'type-error) :to-be t)
        (expect (typep invalid-destination 'type-error) :to-be t)
        (expect (org-kit::export-escape-context :html :text "<&>\"")
                :to-equal "&lt;&amp;&gt;\"")
        (expect (org-kit::export-escape-context :html :attribute "\"<&>")
                :to-equal "&quot;&lt;&amp;&gt;")
        (expect (org-kit::export-escape-context :gfm :label "[x]")
                :to-equal "\\[x\\]")
        (expect (org-kit::export-escape-context :gfm :destination "a b<c>")
                :to-equal "<a%20b%3Cc%3E>")
        (expect (org-kit::export-escape-context :gfm :code "a``b")
                :to-equal "```a``b```")
        (expect (org-kit::export-gfm-code-span "plain") :to-equal "`plain`")
        (expect (org-kit::export-gfm-code-span " plain ")
                :to-equal "`  plain  `")
        (expect (org-kit::export-escape-context :ascii :text "<&>")
                :to-equal "<&>"))))

  (it "validates export options, tags, and duplicate metadata"
    (let* ((preparation
             (export-core-test-preparation
              "#+OPTIONS: toc:t h:2 num:t ^:nil title:t author:nil date:t"))
           (options (org-kit::export-preparation-options preparation))
           (option-failures
             (mapcar
              (lambda (source)
                (export-core-test-catch-error
                 (lambda () (export-core-test-preparation source))))
              '("#+OPTIONS: num:maybe"
                "#+OPTIONS: toc:bad"
                "#+OPTIONS: toc:7"
                "#+OPTIONS: h:7"
                "#+OPTIONS: h:x"
                "#+OPTIONS: unknown:t"
                "#+OPTIONS: toc"
                "#+OPTIONS: toc:"
                "#+OPTIONS: toc:t toc:nil")))
           (valid-tags
             (org-kit::export-parse-tag-list "ship-it_@#%" nil :html
                                             :select-tag-expression))
           (unknown-option-value
             (export-core-test-catch-error
              (lambda ()
                (org-kit::export-option-value "unknown" "value" nil :html))))
           (unlabeled-non-inline-footnote
             (export-core-test-catch-error
              (lambda ()
                (org-kit::export-collect-footnotes
                 (list (org-kit::%make-export-view
                        :node (make-node :footnote-reference
                                         :properties (list :type :standard))))
                 :html))))
           (invalid-tags
             (export-core-test-catch-error
              (lambda ()
                (org-kit::export-parse-tag-list "bad:tag" nil :html
                                                :select-tag-expression))))
           (empty-macro-name
             (export-core-test-catch-error
              (lambda ()
                (org-kit::export-macro-definition
                 (list (make-node :keyword) "MACRO" "") :html))))
           (duplicate-title
             (export-core-test-catch-error
              (lambda ()
                (export-core-test-preparation
                 (export-core-test-lines "#+TITLE: first" "#+TITLE: second")))))
           (duplicate-macro
             (export-core-test-catch-error
              (lambda ()
                (export-core-test-preparation
                 (export-core-test-lines
                  "#+MACRO: repeat first" "#+MACRO: repeat second"))))))
      (with-soft-assertions
        (expect (cdr (assoc :toc options)) :to-be t)
        (expect (cdr (assoc :heading-depth options)) :to-be 2)
        (expect (cdr (assoc :num options)) :to-be t)
        (expect (cdr (assoc :^ options)) :to-be nil)
        (expect (cdr (assoc :title options)) :to-be t)
        (expect (cdr (assoc :author options)) :to-be nil)
        (expect (cdr (assoc :date options)) :to-be t)
        (expect valid-tags :to-equal '("ship-it_@#%"))
        (expect (typep empty-macro-name 'org-export-unsupported-syntax)
                :to-be t)
        (expect (typep unknown-option-value 'org-export-unsupported-syntax)
                :to-be t)
        (expect (typep unlabeled-non-inline-footnote 'org-export-error)
                :to-be t)
        (when (typep unlabeled-non-inline-footnote 'org-export-error)
          (expect (org-export-error-reason unlabeled-non-inline-footnote)
                  :to-be :unresolved-footnote))
        (when (typep unknown-option-value 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-feature unknown-option-value)
                  :to-be :unknown-option))
        (expect (typep invalid-tags 'org-export-unsupported-syntax) :to-be t)
        (expect (every (lambda (condition)
                         (typep condition 'org-export-unsupported-syntax))
                       option-failures)
                :to-be t)
        (expect (typep duplicate-title 'org-export-unsupported-syntax) :to-be t)
        (when (typep duplicate-title 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-feature duplicate-title)
                  :to-be :duplicate-keyword))
        (expect (typep duplicate-macro 'org-export-unsupported-syntax) :to-be t)
        (when (typep duplicate-macro 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-feature duplicate-macro)
                  :to-be :duplicate-macro)))))

  (it "expands literal macros without evaluation and rejects recursion"
    (let* ((source
             (export-core-test-lines
              "#+MACRO: greeting Hello, $1! $2 / $1"
              "{{{greeting(World,again)}}}"))
           (preparation (export-core-test-preparation source))
           (macro-view (first (export-core-test-view-nodes preparation :macro)))
           (recursive
             (export-core-test-catch-error
              (lambda ()
                (export-core-test-preparation
                 (export-core-test-lines
                  "#+MACRO: loop {{{loop}}}"
                  "{{{loop}}}")))))
           (unknown-macro
             (export-core-test-catch-error
              (lambda () (export-core-test-preparation "{{{missing}}}"))))
           (function-macro
             (export-core-test-catch-error
              (lambda ()
                (export-core-test-preparation
                 (export-core-test-lines "#+MACRO: choose(x) $1"
                                         "{{{choose(value)}}}")))))
           (invalid-macro-name
             (export-core-test-catch-error
              (lambda ()
                (export-core-test-preparation "#+MACRO: bad$ value"))))
           (unsupported-parameter
             (export-core-test-catch-error
              (lambda ()
                (export-core-test-preparation
                 (export-core-test-lines
                  "#+MACRO: third $3"
                  "{{{third(value)}}}")))))
           (short-dollar
             (org-kit::export-macro-parameter-index "$" 0 nil :html))
           (nondigit-parameter
             (org-kit::export-macro-parameter-index "$x" 0 nil :html))
           (multiple-digit-parameter
             (export-core-test-catch-error
              (lambda ()
                (org-kit::export-macro-parameter-index "$12" 0 nil :html)))))
      (with-soft-assertions
        (expect (getf (org-kit::export-view-payload macro-view) :expanded-text)
                :to-equal "Hello, World! again / World")
        (expect (typep recursive 'org-export-unsupported-syntax) :to-be t)
        (when (typep recursive 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-feature recursive)
                  :to-be :recursive-macro))
        (expect (typep unknown-macro 'org-export-unsupported-syntax) :to-be t)
        (when (typep unknown-macro 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-feature unknown-macro)
                  :to-be :unknown-macro))
        (expect (typep function-macro 'org-export-unsupported-syntax) :to-be t)
        (when (typep function-macro 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-feature function-macro)
                  :to-be :function-macro))
        (expect (typep invalid-macro-name 'org-export-unsupported-syntax)
                :to-be t)
        (when (typep invalid-macro-name 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-feature invalid-macro-name)
                  :to-be :invalid-macro))
        (expect (typep unsupported-parameter 'org-export-unsupported-syntax)
                :to-be t)
        (when (typep unsupported-parameter 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-feature unsupported-parameter)
                  :to-be :unsupported-macro-parameter))
        (expect short-dollar :to-be nil)
        (expect nondigit-parameter :to-be nil)
        (expect (typep multiple-digit-parameter 'org-export-unsupported-syntax)
                :to-be t))))

  (it "rejects long numeric options and macro parameters"
    (let* ((digits (make-string 4096 :initial-element #\9))
           (option-error
             (export-core-test-catch-error
              (lambda ()
                (export-core-test-preparation
                 (concatenate 'string "#+OPTIONS: toc:" digits)))))
           (macro-error
             (export-core-test-catch-error
              (lambda ()
                (export-core-test-preparation
                 (export-core-test-lines
                  (concatenate 'string "#+MACRO: many $" digits)
                  "{{{many}}}"))))))
      (with-soft-assertions
        (expect (typep option-error 'org-export-unsupported-syntax) :to-be t)
        (when (typep option-error 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-feature option-error)
                  :to-be :invalid-option))
        (expect (typep macro-error 'org-export-unsupported-syntax) :to-be t)
        (when (typep macro-error 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-feature macro-error)
                  :to-be :unsupported-macro-parameter)))))

  (it "uses only the include callback and rejects missing, invalid, or cyclic includes"
    (let* ((source (parse-org "#+INCLUDE: \"part.org\"" :source-name "main.org"))
           (calls nil)
           (resolver
             (lambda (path source-name)
               (push (list path source-name) calls)
               (when (string= path "part.org")
                 (export-core-test-lines "* Included" "Included text"))))
           (preparation (org-kit::export-prepare source :html
                                                 :include-resolver resolver))
           (included-headings
             (mapcar (lambda (view)
                       (node-property (org-kit::export-view-node view) :raw-value))
                     (export-core-test-view-nodes preparation :headline)))
           (missing (export-core-test-catch-error
                     (lambda () (org-kit::export-prepare source :html))))
           (invalid (export-core-test-catch-error
                     (lambda ()
                       (org-kit::export-prepare source :html
                                                :include-resolver
                                                (lambda (path source-name)
                                                  (declare (ignore path source-name))
                                                  nil)))))
           (cycle-source (parse-org "#+INCLUDE: \"part.org\""
                                    :source-name "main.org"))
           (cycle (export-core-test-catch-error
                   (lambda ()
                     (org-kit::export-prepare
                      cycle-source :html
                      :include-resolver
                      (lambda (path source-name)
                        (declare (ignore source-name))
                        (if (string= path "part.org")
                            "#+INCLUDE: \"part.org\""
                            "unreachable")))))))
      (with-soft-assertions
        (expect calls :to-equal '(("part.org" "main.org")))
        (expect included-headings :to-equal '("Included"))
        (expect (typep missing 'org-export-error) :to-be t)
        (when (typep missing 'org-export-error)
          (expect (org-export-error-reason missing) :to-be :missing-include-resolver))
        (expect (typep invalid 'org-export-error) :to-be t)
        (when (typep invalid 'org-export-error)
          (expect (org-export-error-reason invalid) :to-be :invalid-include-result))
        (expect (typep cycle 'org-export-error) :to-be t)
        (when (typep cycle 'org-export-error)
          (expect (org-export-error-reason cycle) :to-be :include-cycle)))))

  (it "isolates include callback arguments from document and cycle state"
    (let* ((source-name (copy-seq "main.org"))
           (document (parse-org "#+INCLUDE: \"part.org\""
                                :source-name source-name))
           (calls 0)
           (condition
             (export-core-test-catch-error
              (lambda ()
                (org-kit::export-prepare
                 document :html
                 :include-resolver
                 (lambda (path parent-source)
                   (incf calls)
                   (setf (char path 0) #\P
                         (char parent-source 0) #\M)
                   "#+INCLUDE: \"part.org\""))))))
      (with-soft-assertions
        (expect calls :to-be 1)
        (expect source-name :to-equal "main.org")
        (expect (document-source-name document) :to-equal "main.org")
        (expect (typep condition 'org-export-error) :to-be t)
        (when (typep condition 'org-export-error)
          (expect (org-export-error-reason condition) :to-be :include-cycle)))))

  (it "validates include forms and rejects fragment-scoped settings"
    (let* ((invalid-include-forms
             (mapcar
              (lambda (value)
                (export-core-test-catch-error
                 (lambda () (org-kit::export-include-path value nil :html))))
              (list "" "part.org" "\"part.org" "part.org\"" "\"a\"b\""
                    (format nil "\"bad~%path\""))))
           (fragment-errors
             (mapcar
              (lambda (fragment)
                (export-core-test-catch-error
                 (lambda ()
                   (org-kit::export-prepare
                    (parse-org "#+INCLUDE: \"part.org\"") :html
                    :include-resolver (lambda (path parent)
                                        (declare (ignore path parent))
                                        fragment)))))
              '("#+TITLE: included title" "#+TODO: TODO DONE"))))
      (with-soft-assertions
        (expect (every (lambda (condition)
                         (typep condition 'org-export-unsupported-syntax))
                       invalid-include-forms)
                :to-be t)
        (expect (every (lambda (condition)
                         (and (typep condition 'org-export-unsupported-syntax)
                              (eq (org-export-unsupported-syntax-feature condition)
                                  :fragment-keyword-scope)))
                       fragment-errors)
                :to-be t))))

  (it "passes a NIL parent source name to include resolvers"
    (let* ((document (parse-org "#+INCLUDE: \"part.org\""))
           (parent-source :not-called)
           (preparation
             (org-kit::export-prepare
              document :html
              :include-resolver (lambda (path source-name)
                                  (declare (ignore path))
                                  (setf parent-source source-name)
                                  "Included"))))
      (with-soft-assertions
        (expect parent-source :to-be nil)
        (expect (org-kit::export-preparation-p preparation) :to-be t))))

  (it "copies mutable sequence source names for include resolvers"
    (let* ((source-name
             (make-array 2
                         :initial-contents (list (list (copy-seq "main.org"))
                                                 :hidden)
                         :adjustable t :fill-pointer 1))
           (document (parse-org "#+INCLUDE: \"part.org\""
                                :source-name source-name))
           (preparation
             (org-kit::export-prepare
              document :html
              :include-resolver (lambda (path parent-source)
                                  (declare (ignore path))
                                  (setf (char (first (aref parent-source 0)) 0)
                                        #\M)
                                  "Included"))))
      (with-soft-assertions
        (expect (first (aref source-name 0)) :to-equal "main.org")
        (expect (first (aref (document-source-name document) 0))
                :to-equal "main.org")
        (expect (org-kit::export-preparation-p preparation) :to-be t))))

  (it "copies mutable list source names for include resolvers"
    (let* ((source-name (list (copy-seq "main.org")))
           (document (parse-org "#+INCLUDE: \"part.org\""
                                :source-name source-name))
           (preparation
             (org-kit::export-prepare
              document :html
              :include-resolver (lambda (path parent-source)
                                  (declare (ignore path))
                                  (setf (char (car parent-source) 0) #\M)
                                  "Included"))))
      (with-soft-assertions
        (expect source-name :to-equal '("main.org"))
        (expect (document-source-name document) :to-equal '("main.org"))
        (expect (org-kit::export-preparation-p preparation) :to-be t))))

  (it "preserves shared structure in copied include source names"
    (let* ((shared-text (copy-seq "main.org"))
           (shared-cell (list :shared))
           (source-name
             (vector (list shared-text) (list shared-text)
                     shared-cell shared-cell))
           (document (parse-org "#+INCLUDE: \"part.org\""
                                :source-name source-name))
           (parent-source nil)
           (preparation
             (org-kit::export-prepare
              document :html
              :include-resolver (lambda (path source)
                                  (declare (ignore path))
                                  (setf parent-source source
                                        (char (car (aref source 0)) 0) #\M)
                                  "Included"))))
      (with-soft-assertions
        (expect (eq (car (aref parent-source 0))
                    (car (aref parent-source 1)))
                :to-be t)
        (expect (eq (aref parent-source 2) (aref parent-source 3))
                :to-be t)
        (expect shared-text :to-equal "main.org")
        (expect (org-kit::export-preparation-p preparation) :to-be t))))

  (it "copies circular include source names without changing the document"
    (let* ((source-name (cons (copy-seq "main.org") nil))
           (document (parse-org "#+INCLUDE: \"part.org\""
                                :source-name source-name))
           (parent-source nil))
      (setf (cdr source-name) source-name)
      (let ((preparation
              (org-kit::export-prepare
               document :html
               :include-resolver (lambda (path source)
                                   (declare (ignore path))
                                   (setf parent-source source
                                         (char (car source) 0) #\M)
                                   "Included"))))
        (with-soft-assertions
          (expect (eq (cdr parent-source) parent-source) :to-be t)
          (expect (char (car source-name) 0) :to-be #\m)
          (expect (org-kit::export-preparation-p preparation) :to-be t)))))

  (it "rejects mutable non-sequence source names before invoking include resolvers"
    (let* ((source-name (make-hash-table))
           (document (parse-org "#+INCLUDE: \"part.org\""
                                :source-name source-name))
           (calls 0)
           (condition
             (export-core-test-catch-error
              (lambda ()
                (org-kit::export-prepare
                 document :html
                 :include-resolver (lambda (path parent-source)
                                     (declare (ignore path parent-source))
                                     (incf calls)
                                     "Included"))))))
      (with-soft-assertions
        (expect calls :to-be 0)
        (expect (typep condition 'org-export-unsupported-syntax) :to-be t)
        (when (typep condition 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-feature condition)
                  :to-be :include-source-name)))))

  (it "validates source block export argument forms"
    (let* ((node-for (lambda (switches)
                       (make-node :src-block :properties (list :switches switches))))
           (unknown-argument
             (export-core-test-catch-error
              (lambda () (org-kit::export-src-export-mode
                          (funcall node-for ":var x=1") :html))))
           (duplicate-argument
             (export-core-test-catch-error
              (lambda () (org-kit::export-src-export-mode
                          (funcall node-for ":exports code :exports results")
                          :html))))
           (missing-value
             (export-core-test-catch-error
              (lambda () (org-kit::export-src-export-mode
                          (funcall node-for ":exports") :html))))
           (invalid-value
             (export-core-test-catch-error
              (lambda () (org-kit::export-src-export-mode
                          (funcall node-for ":exports never") :html)))))
      (with-soft-assertions
        (expect (typep unknown-argument 'org-export-error) :to-be t)
        (expect (typep duplicate-argument 'org-export-error) :to-be t)
        (expect (typep missing-value 'org-export-error) :to-be t)
        (expect (typep invalid-value 'org-export-unsupported-syntax) :to-be t)
        (expect (org-kit::export-src-export-mode (funcall node-for "") :html)
                :to-be :code))))

  (it "prepares code and adjacent results according to every source export mode"
    (let* ((source
             (export-core-test-lines
              "#+BEGIN_SRC text :exports both"
              "both-code"
              "#+END_SRC"
              "#+RESULTS:"
              ": both-result"
              "#+BEGIN_SRC text :exports code"
              "code-code"
              "#+END_SRC"
              "#+RESULTS:"
              ": code-result"
              "#+BEGIN_SRC text :exports results"
              "results-code"
              "#+END_SRC"
              "#+RESULTS:"
              ": results-result"
              "#+BEGIN_SRC text :exports none"
              "none-code"
              "#+END_SRC"
              "#+RESULTS:"
              ": none-result"
              "#+BEGIN_SRC text"
              "default-code"
              "#+END_SRC"
              "#+BEGIN_SRC text :exports results"
              "separated-code"
              "#+END_SRC"
              ""
              "#+RESULTS:"
              ": separated-result"))
           (preparation (export-core-test-preparation source))
           (source-views (export-core-test-view-nodes preparation :src-block))
           (source-payloads (mapcar #'org-kit::export-view-payload source-views))
           (result-child-counts
             (mapcar (lambda (view)
                       (length (org-kit::export-view-children view)))
                     source-views))
           (separated-result-view
             (find-if (lambda (view)
                        (let ((node (org-kit::export-view-node view)))
                          (and (eq (node-type node) :fixed-width)
                               (node-property node :RESULTS)
                               (search "separated-result"
                                       (node-property node :value)))))
                      (export-core-test-all-views preparation)))
           (separated-result
             (and separated-result-view
                  (org-kit::export-view-node separated-result-view))))
      (with-soft-assertions
        (expect (mapcar (lambda (payload) (getf payload :exports)) source-payloads)
                :to-equal '(:both :code :results :none :code :results))
        (expect (mapcar (lambda (payload) (getf payload :adjacent-results))
                        source-payloads)
                :to-equal '(t t t t nil nil))
        (expect result-child-counts :to-equal '(1 0 1 0 0 0))
        (expect separated-result :to-be-truthy)
        (expect (and separated-result-view
                     (eq (getf (org-kit::export-view-payload separated-result-view)
                               :operation)
                         :code-block))
                :to-be t)
        (expect (and separated-result
                     (string= (node-property separated-result :value)
                              "separated-result"))
                :to-be t))))

  (it "does not mutate documents on successful preparation or failure"
    (let* ((source (parse-org "#+INCLUDE: \"part.org\"" :source-name "main.org"))
           (before (export-core-test-document-snapshot source)))
      (org-kit::export-prepare source :html
                               :include-resolver
                               (lambda (path source-name)
                                 (declare (ignore source-name))
                                 (if (string= path "part.org") "Included" nil)))
      (with-soft-assertions
        (expect (export-core-test-document-snapshot source) :to-equal before)
        (expect (typep (export-core-test-catch-error
                        (lambda () (org-kit::export-prepare source :html)))
                       'org-export-error)
                :to-be t)
        (expect (export-core-test-document-snapshot source) :to-equal before)))))

  (it "validates preparation inputs and requires a parsed document"
    (let* ((document (parse-org "Body"))
           (invalid-document
             (export-core-test-catch-error
              (lambda () (org-kit::export-prepare nil :html))))
           (invalid-backend
             (export-core-test-catch-error
              (lambda () (org-kit::export-prepare document :unknown))))
           (invalid-resolver
             (export-core-test-catch-error
              (lambda ()
                (org-kit::export-prepare document :html :include-resolver :invalid))))
           (invalid-raw-html
             (export-core-test-catch-error
              (lambda ()
                (org-kit::export-prepare document :html :allow-raw-html :invalid))))
           (unparsed
             (export-core-test-catch-error
              (lambda () (org-kit::export-prepare (make-document "Body") :html)))))
      (with-soft-assertions
        (expect (typep invalid-document 'type-error) :to-be t)
        (expect (typep invalid-backend 'org-export-error) :to-be t)
        (when (typep invalid-backend 'org-export-error)
          (expect (org-export-error-reason invalid-backend) :to-be :unknown-backend))
        (expect (typep invalid-resolver 'type-error) :to-be t)
        (expect (typep invalid-raw-html 'type-error) :to-be t)
        (expect (typep unparsed 'org-export-error) :to-be t)
        (when (typep unparsed 'org-export-error)
          (expect (org-export-error-reason unparsed) :to-be :invalid-output)))))

(describe "E1 export output validation"
  (it "validates output types and writes to character and octet streams"
    (let* ((invalid-limits
             (export-core-test-catch-error
              (lambda () (org-kit::export-open-output nil :string))))
           (stream-on-string
             (export-core-test-catch-error
              (lambda ()
                (org-kit::export-open-output
                 (make-limits) :string :stream (make-string-output-stream)))))
           (missing-stream
             (export-core-test-catch-error
              (lambda () (org-kit::export-open-output (make-limits) :stream))))
           (unsupported-stream
             (export-core-test-catch-error
              (lambda ()
                (org-kit::export-open-output
                 (make-limits) :stream
                 :stream (make-instance 'export-core-test-binary-output-stream
                                        :element-type '(unsigned-byte 16))))))
           (unknown-output
             (export-core-test-catch-error
              (lambda () (org-kit::export-open-output (make-limits) :unknown))))
           (invalid-state
             (export-core-test-catch-error
              (lambda () (org-kit::export-append-output nil "text"))))
           (state (org-kit::export-open-output (make-limits) :string :backend :html))
           (invalid-text
             (export-core-test-catch-error
              (lambda () (org-kit::export-append-output state 42))))
           (invalid-finish
             (export-core-test-catch-error
              (lambda () (org-kit::export-finish-output nil))))
           (invalid-write-text
             (export-core-test-catch-error
              (lambda ()
                (org-kit::export-write-output 42 (make-limits) :string
                                              :backend :html))))
           (invalid-write-limits
             (export-core-test-catch-error
              (lambda () (org-kit::export-write-output "x" nil :string))))
           (character-stream (make-string-output-stream))
           (character-result
             (org-kit::export-write-output "text" (make-limits) :stream
                                           :backend :html :stream character-stream))
           (octet-stream
             (make-instance 'export-core-test-binary-output-stream
                            :element-type '(unsigned-byte 8)))
           (octet-result
             (org-kit::export-write-output "é" (make-limits) :stream
                                           :backend :html :stream octet-stream))
           (invalid-utf8-length
             (export-core-test-with-codec-condition
              'cl-codec-kit:string-size-in-octets
              (lambda ()
                (export-core-test-catch-error
                 (lambda () (org-kit::export-utf8-length "text" :html))))))
           (invalid-utf8-encoding
             (export-core-test-with-codec-condition
              'cl-codec-kit:string-to-octets
              (lambda ()
                (export-core-test-catch-error
                 (lambda () (org-kit::export-utf8-octets "text" :html)))))))
      (with-soft-assertions
        (expect (typep invalid-limits 'type-error) :to-be t)
        (expect (typep stream-on-string 'org-export-error) :to-be t)
        (expect (typep missing-stream 'org-export-error) :to-be t)
        (expect (typep unsupported-stream 'org-export-error) :to-be t)
        (expect (typep unknown-output 'org-export-error) :to-be t)
        (expect (typep invalid-state 'type-error) :to-be t)
        (expect (typep invalid-text 'org-export-error) :to-be t)
        (expect (typep invalid-finish 'type-error) :to-be t)
        (expect (typep invalid-write-text 'org-export-error) :to-be t)
        (expect (typep invalid-write-limits 'type-error) :to-be t)
        (expect (eq character-result character-stream) :to-be t)
        (expect (get-output-stream-string character-stream) :to-equal "text")
        (expect (eq octet-result octet-stream) :to-be t)
        (expect (export-core-test-stream-octets octet-stream) :to-equalp #(195 169))
        (expect (typep invalid-utf8-length 'org-export-error) :to-be t)
        (expect (typep invalid-utf8-encoding 'org-export-error) :to-be t)))))

(describe "E1 export resource boundaries"
  (it "checks root nodes before processing export keywords"
    (let* ((document
             (parse-org "#+UNSUPPORTED: value"
                        :limits (make-limits :max-export-expanded-nodes 0)))
           (condition
             (export-core-test-catch-error
              (lambda () (org-kit::export-prepare document :html)))))
      (expect (typep condition 'org-resource-limit-error) :to-be t)
      (when (typep condition 'org-resource-limit-error)
        (expect (org-resource-limit-error-limit-name condition)
                :to-be :max-export-expanded-nodes)
        (expect (org-resource-limit-error-limit condition) :to-be 0)
        (expect (org-resource-limit-error-actual condition)
                :to-be (org-kit::export-node-count (document-root document))))))

  (it "preserves the parser node limit when a fragment exceeds it"
    (let* ((document (parse-org "#+INCLUDE: \"part.org\""
                                :limits (make-limits :max-node-count 4)))
           (condition
             (export-core-test-catch-error
              (lambda ()
                (org-kit::export-prepare
                 document :html
                 :include-resolver
                 (lambda (path parent-source)
                   (declare (ignore path parent-source))
                   (export-core-test-lines
                    "* One" "* Two" "* Three" "* Four" "* Five")))))))
      (expect (typep condition 'org-resource-limit-error) :to-be t)
      (when (typep condition 'org-resource-limit-error)
        (expect (org-resource-limit-error-limit-name condition)
                :to-be :max-node-count))))

  (it "checks expansion depth immediately below, at, and above the limit"
    (let* ((source (export-core-test-lines "#+MACRO: m expansion" "{{{m}}}"))
           (below (export-core-test-catch-error
                   (lambda ()
                     (export-core-test-prepare-with-limit
                      source :max-export-expansion-depth 0))))
           (at (export-core-test-catch-error
                (lambda ()
                  (export-core-test-prepare-with-limit
                   source :max-export-expansion-depth 1))))
           (above (export-core-test-catch-error
                   (lambda ()
                     (export-core-test-prepare-with-limit
                      source :max-export-expansion-depth 2)))))
      (with-soft-assertions
        (expect (typep below 'org-resource-limit-error) :to-be t)
        (when (typep below 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name below)
                  :to-be :max-export-expansion-depth)
          (expect (org-resource-limit-error-limit below) :to-be 0)
          (expect (org-resource-limit-error-actual below) :to-be 1))
        (expect at :to-be nil)
        (expect above :to-be nil))))

  (it "checks expanded characters immediately below, at, and above the limit"
    (let* ((source (export-core-test-lines "#+MACRO: m abc" "{{{m}}}"))
           (below (export-core-test-catch-error
                   (lambda ()
                     (export-core-test-prepare-with-limit
                      source :max-export-expanded-characters 2))))
           (at (export-core-test-catch-error
                (lambda ()
                  (export-core-test-prepare-with-limit
                   source :max-export-expanded-characters 3))))
           (above (export-core-test-catch-error
                   (lambda ()
                     (export-core-test-prepare-with-limit
                      source :max-export-expanded-characters 4)))))
      (with-soft-assertions
        (expect (typep below 'org-resource-limit-error) :to-be t)
        (when (typep below 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name below)
                  :to-be :max-export-expanded-characters)
          (expect (org-resource-limit-error-limit below) :to-be 2)
          (expect (org-resource-limit-error-actual below) :to-be 3))
        (expect at :to-be nil)
        (expect above :to-be nil))))

  (it "checks cumulative expanded nodes immediately below, at, and above the limit"
    (let* ((source (export-core-test-lines "#+MACRO: m expanded" "{{{m}}}"))
           (document (parse-org source))
           (fragment (parse-org "expanded"))
           (actual (+ (org-kit::export-node-count (document-root document))
                      (org-kit::export-node-count (document-root fragment))))
           (below (export-core-test-catch-error
                   (lambda ()
                     (export-core-test-prepare-with-limit
                      source :max-export-expanded-nodes (1- actual)))))
           (at (export-core-test-catch-error
                (lambda ()
                  (export-core-test-prepare-with-limit
                   source :max-export-expanded-nodes actual))))
           (above (export-core-test-catch-error
                   (lambda ()
                     (export-core-test-prepare-with-limit
                      source :max-export-expanded-nodes (1+ actual))))))
      (with-soft-assertions
        (expect (typep below 'org-resource-limit-error) :to-be t)
        (when (typep below 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name below)
                  :to-be :max-export-expanded-nodes)
          (expect (org-resource-limit-error-limit below) :to-be (1- actual))
          (expect (org-resource-limit-error-actual below) :to-be actual))
        (expect at :to-be nil)
        (expect above :to-be nil))))

  (it "checks output characters immediately below, at, and above the limit"
    (let* ((text "abc")
           (below (export-core-test-catch-error
                   (lambda ()
                     (org-kit::export-write-output
                      text (make-limits :max-output-length 2) :string
                      :backend :html))))
           (at (org-kit::export-write-output
                text (make-limits :max-output-length 3) :string
                :backend :html))
           (above (org-kit::export-write-output
                   text (make-limits :max-output-length 4) :string
                   :backend :html)))
      (with-soft-assertions
        (expect (typep below 'org-resource-limit-error) :to-be t)
        (when (typep below 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name below)
                  :to-be :max-output-length)
          (expect (org-resource-limit-error-limit below) :to-be 2)
          (expect (org-resource-limit-error-actual below) :to-be 3))
        (expect at :to-equal text)
        (expect above :to-equal text))))

  (it "checks UTF-8 output octets immediately below, at, and above the limit"
    (let* ((text "é")
           (below (export-core-test-catch-error
                   (lambda ()
                     (org-kit::export-write-output
                      text (make-limits :max-output-octets 1) :octets
                      :backend :html))))
           (at (org-kit::export-write-output
                text (make-limits :max-output-octets 2) :octets
                :backend :html))
           (above (org-kit::export-write-output
                   text (make-limits :max-output-octets 3) :octets
                   :backend :html)))
      (with-soft-assertions
        (expect (typep below 'org-resource-limit-error) :to-be t)
        (when (typep below 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name below)
                  :to-be :max-output-octets)
          (expect (org-resource-limit-error-limit below) :to-be 1)
          (expect (org-resource-limit-error-actual below) :to-be 2))
        (expect at :to-equalp #(195 169))
        (expect above :to-equalp #(195 169))))))

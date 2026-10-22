(in-package #:cl-org-kit/test)

(defun export-man-test-fixture (name)
  (uiop:read-file-string
   (asdf:system-relative-pathname
    "cl-org-kit" (format nil "t/fixtures/export/oracle/corpus/~A.org" name))))

(defun export-man-test-octets (relative-path)
  (with-open-file (input (asdf:system-relative-pathname "cl-org-kit" relative-path)
                         :direction :input
                         :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length input)
                              :element-type '(unsigned-byte 8))))
      (unless (= (read-sequence octets input) (length octets))
        (error "Unable to read complete oracle output: ~A" relative-path))
      octets)))

(defun export-man-test-normalize-target-reference (octets)
  (let* ((ranges
           (loop for start from 0 to (- (length octets) 16)
                 when (and (= (aref octets start) 92)
                           (= (aref octets (+ start 1)) 102)
                           (= (aref octets (+ start 2)) 73)
                           (= (aref octets (+ start 3)) 111)
                           (= (aref octets (+ start 4)) 114)
                           (= (aref octets (+ start 5)) 103)
                           (loop for index from (+ start 6) to (+ start 12)
                                 for byte = (aref octets index)
                                 always (or (<= 48 byte 57)
                                            (<= 97 byte 102)))
                           (= (aref octets (+ start 13)) 92)
                           (= (aref octets (+ start 14)) 102)
                           (= (aref octets (+ start 15)) 80))
                   collect (cons (+ start 3) (+ start 13))))
         (range (first ranges))
         (normalized (copy-seq octets)))
    (unless (= (length ranges) 1)
      (error "Expected one Org-style target reference with seven lowercase hex digits"))
    (loop for index below 10
          do (setf (aref normalized (+ (car range) index))
                   (char-code (char "org0000000" index))))
    normalized))

(defun export-man-test-oracle (case-id &optional include-resolver)
  (let* ((document (parse-org (export-man-test-fixture case-id)))
         (expected (export-man-test-octets
                    (format nil "t/fixtures/export/oracle/man/~A.1" case-id)))
         (actual (export-org document :man
                             :include-resolver include-resolver
                             :output-type :octets)))
    (expect (plusp (length expected)) :to-be t)
    (when (string= case-id "texinfo-elements")
      (setf expected (export-man-test-normalize-target-reference expected)
            actual (export-man-test-normalize-target-reference actual)))
    (expect actual :to-equalp expected)))

(describe "FR-104 man export backend"
  (it "matches the broad man element oracle"
    (export-man-test-oracle "man-elements"))
  (it "matches the Org target and link oracle"
    (export-man-test-oracle "texinfo-elements"))
  (it "matches standard Org man oracle cases"
    (let ((resolver-calls nil)
          (case-ids (list "metadata-selection"
                          "exclude-tags-override"
                          "footnotes-macros-links"
                          "include"
                          "source-blocks"
                          "man-options")))
      (with-soft-assertions
        (dolist (case-id case-ids)
          (export-man-test-oracle
           case-id
           (when (string= case-id "include")
             (lambda (path parent-source)
               (declare (ignore parent-source))
               (push path resolver-calls)
               (when (string= path "included/fragment.org")
                 (export-man-test-fixture "included/fragment"))))))
        (expect (nreverse resolver-calls)
                :to-equal (list "included/fragment.org")))))

  (it "matches the man escaping oracle"
    (export-man-test-oracle "man-escapes"))

  (it "keeps Org 9.8.10 leading dot and apostrophe text unchanged"
    (let* ((document (parse-org (export-man-test-fixture "man-escapes")))
           (output (export-org document :man)))
      (with-soft-assertions
        (expect (search
                 (format nil ".leading-dot remains plain.~%~Cleading-apostrophe remains plain."
                         (code-char 39))
                 output)
                :to-be-truthy)
        (expect (search "Plain escapes: , $\\, \\$, \\{." output)
                :to-be-truthy)
        (expect (search "\\fC. ' \\foo \\\\foo \\$ \\{\\fP" output)
                :to-be-truthy)
        (expect (search "\\fI. ' \\foo \\\\foo \\$ \\{\\fP" output)
                :to-be-truthy)
        (expect (search
                 ".example-dot
'example-apostrophe
\\efoo \\e\\efoo \\e$ \\e{"
                 output)
                :to-be-truthy)
        (expect (search
                 ".source-dot
'source-apostrophe
\\efoo \\e\\efoo \\e$ \\e{"
                 output)
                :to-be-truthy))))

  (it "signals org-resource-limit-error when rendered output exceeds its limit"
    (let* ((document
             (parse-org "A deliberately longer man output."
                        :limits (make-limits :max-output-length 32)))
           (condition
             (handler-case
                 (progn (export-org document :man) nil)
               (org-resource-limit-error (condition) condition))))
      (with-soft-assertions
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when (typep condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name condition)
                  :to-equal :max-output-length)
          (expect (org-resource-limit-error-limit condition) :to-equal 32))))))

(defun export-man-test-context (document)
  (multiple-value-bind (descriptor ancestry)
      (org-kit::export-registry-snapshot :man)
    (let ((context
            (org-kit::%make-export-render-context
             :backend :man :document document
             :metadata nil :options nil :limits nil
             :root-views nil :toc nil :footnotes nil
             :preparation (org-kit::%make-export-preparation)
             :raw-output-options
             (org-kit::%export-backend-descriptor-raw-output-options descriptor)
             :extensions nil :ancestry ancestry :owner-backend :man
             :state nil :states nil)))
      (org-kit::export-prepare document :man :filter-context context)
      context)))

(defun export-man-test-view (node document &optional children)
  (org-kit::%make-export-view :node node :children children
                              :source-document document))

(defun export-man-test-find-view (views type)
  (loop for view in views
        for found =
          (if (eq (node-type (export-view-node view)) type)
              view
            (export-man-test-find-view (export-view-children view) type))
        when found return found))

(defun export-man-test-render-node (context node document &optional children)
  (org-kit::export-render-view
   context (export-man-test-view node document children)))

(describe "FR-104 man backend rendering boundaries"
  (it "renders smart quotes and preserved source breaks"
    (let* ((document (parse-org (format nil "#+LANGUAGE: en~%~%Body")))
           (context (export-man-test-context document))
           (options (org-kit::%export-render-context-options context))
           (quoted
             (progn
               (setf (org-kit::%export-render-context-options context)
                     (acons :smart-quotes t options))
               (org-kit::export-man-plain-text
                "It's 'fine' dogs' \"quoted\"" context 0)))
           (preserved
             (progn
               (setf (org-kit::%export-render-context-options context)
                     (acons :preserve-breaks t
                            (org-kit::%export-render-context-options context)))
               (org-kit::export-man-plain-text
                (format nil "line\\\\  ~%next\\~%") context 0))))
      (with-soft-assertions
        (expect quoted :to-equal "It’s ‘fine’ dogs’ “quoted”")
        (progn
          (expect (search ".br" preserved) :to-be-truthy)
          (expect (org-kit::export-man-plain-text "\\path" context 0)
                  :to-equal "$\\path")))))

  (it "covers raw nodes, links, rows, and fallback renderers"
    (let* ((document (parse-org "Body"))
           (context (export-man-test-context document))
           (entity (make-node :entity :properties '(:utf-8 "é")))
           (html-block
             (make-node :export-block
                        :properties '(:type "HTML" :value "foreign")))
           (html-snippet
             (make-node :export-snippet
                        :properties '(:back-end "html" :value "foreign")))
           (inline-source
             (make-node :inline-src-block
                        :properties '(:parameters ":exports both"
                                      :value "(+ 1 2)")))
           (node-property
             (make-node :node-property
                        :properties '(:key "OWNER" :value "Ada")))
           (first-row (make-node :table-row))
           (rule-row (make-node :table-row :properties '(:type :rule)))
           (table (make-node :table :children (list first-row rule-row)))
           (tag-node (make-node :italic))
           (item (make-node :item :properties
                            (list :tag (list "Term" tag-node nil))))
           (list-node
             (make-node :plain-list :properties '(:type :descriptive)
                        :children (list item)))
           (odd-title
             (make-node :headline :properties '(:title (:unexpected))))
           (unmatched-link (make-node :link))
           (inline-code-output
             (export-org (parse-org "Use ~code~ and =verbatim=.") :man))
           (row-output
             (export-man-test-render-node context first-row document))
           (tag-output
             (org-kit::export-man-transcode-item item "body" context 0)))
      (declare (ignore table list-node))
      (with-soft-assertions
        (expect (export-man-test-render-node context entity document)
                :to-equal "é")
        (expect (export-man-test-render-node context html-block document)
                :to-equal "")
        (expect (export-man-test-render-node context html-snippet document)
                :to-equal "")
        (expect (search "+ 1 2" (export-man-test-render-node
                                  context inline-source document))
                :to-be-truthy)
        (expect (export-man-test-render-node context node-property document)
                :to-equal "OWNER: Ada")
        (expect row-output :to-equal (format nil "~%_"))
        (expect (export-man-test-render-node context rule-row document)
                :to-equal "")
        (expect (search "Term" tag-output) :to-be-truthy)
        (expect (search "\\fI\\fP" tag-output) :to-be-truthy)
        (expect (org-kit::export-man-render-title context odd-title nil)
                :to-equal "")
        (expect (org-kit::export-man-transcode
                 (make-node :org-data)
                 (export-man-test-view (make-node :org-data) document)
                 (org-kit::%make-export-render-frame) context)
                :to-equal "")
        (expect (search "\\fCcode\\fP" inline-code-output) :to-be-truthy)
        (expect (search "\\fIverbatim\\fP" inline-code-output)
                :to-be-truthy)
        (expect (search "\\fI\\fP"
                        (org-kit::export-man-transcode-link
                         unmatched-link "" context 0))
                :to-be-truthy)
        (expect (search "file://server/share"
                        (org-kit::export-man-transcode-link
                         (make-node :link :properties
                                    '(:type "file" :path "//server/share"))
                         "label" context 0))
                :to-be-truthy)
        (progn
          (expect (search "file:///tmp/page"
                          (org-kit::export-man-transcode-link
                           (make-node :link :properties
                                      '(:type "file" :path "/tmp/page"))
                           "" context 0))
                  :to-be-truthy)
          (expect (export-man-test-render-node
                   context (make-node :code :properties '(:value "~literal~"))
                   document)
                  :to-equal "\\fCliteral\\fP")
          (expect (export-man-test-render-node
                   context (make-node :entity :properties '(:utf8 "fallback"))
                   document)
                  :to-equal "fallback")
          (expect (org-kit::export-man-minimum-indentation
                   (format nil "   ~%  ~%"))
                  :to-equal 0)
          (expect (search
                   "result"
                   (org-kit::export-render-view
                    context
                    (org-kit::%make-export-view
                     :node (make-node :src-block :properties '(:value "code"))
                     :children
                     (list (export-man-test-view
                            (make-node :export-block
                                       :properties '(:type "MAN" :value "result"))
                            document))
                     :payload '(:exports :both)
                     :source-document document)))
                  :to-be-truthy)
          (let* ((fallback-document (parse-org "fallback"))
               (fallback-node (make-node :plain-text :begin 0 :end 8))
               (fallback-root (make-node :org-data :begin 0 :end 8
                                         :children (list fallback-node)))
               (empty-body (org-kit::export-man-template context nil "" nil))
               (newline-body (org-kit::export-man-template
                              context nil (format nil "body~%") nil)))
            (setf (document-root fallback-document) fallback-root)
            (expect (export-man-test-render-node context fallback-node nil)
                    :to-equal "fallback")
            (expect (eq (org-kit::export-view-source-document
                         (org-kit::export-man-clone-node-and-view
                          (org-kit::%make-export-view :node fallback-node)
                          document))
                        fallback-document)
                    :to-be t)
            (expect (org-kit::export-man-plain-text "foo\\\\bar" context 0)
                    :to-equal "foo\\\\bar")
            (expect (mapcar (lambda (text)
                              (org-kit::export-man-inline-value
                               (make-node :code :properties (list :value text)) #\~))
                            '("" "~" "x~" "~x"))
                    :to-equal '("" "~" "x~" "~x"))
            (let ((org-kit::*export-current-output-offset* (parse-integer "-1")))
              (expect (handler-case
                          (progn (org-kit::export-man-make-output context) nil)
                        (type-error () t))
                      :to-be t))
            (expect (eq (org-kit::export-man-option-strings "") nil)
                    :to-be t)
            (expect (eq (org-kit::export-man-option-strings ":release") nil)
                    :to-be t)
            (expect (eq (org-kit::export-man-option-strings "release value") nil)
                    :to-be t)
            (expect (org-kit::export-man-option-strings ":release \"v\\n\"")
                    :to-equal (list (cons ":release" (format nil "v~%"))))
            (expect (org-kit::export-man-option-strings ":header \"x\\")
                    :to-equal (list (cons ":header" "x")))
            (expect (search "body" newline-body) :to-be-truthy)
            (expect (> (length empty-body) 0) :to-be t)
            (expect (export-man-test-render-node
                     context (make-node :keyword
                                        :properties '(:key "MAN" :value ".SH NAME"))
                     document)
                    :to-equal (format nil ".SH NAME~%"))
            (expect (search "\\fI\\fP"
                            (org-kit::export-man-transcode-link
                             (make-node :link :properties
                                        '(:type "file" :path ""))
                             "" context 0))
                    :to-be-truthy)
            (expect (search
                     ".RS"
                     (org-kit::export-render-view
                      context
                      (org-kit::%make-export-view
                       :node (make-node :src-block :properties '(:value "code"))
                       :children nil :payload '(:exports :both)
                       :source-document document)))
                    :to-be-truthy))))))

  (it "renders property drawers and separates adjacent blocks"
    (let* ((document
             (parse-org
              (format nil "* Heading~%:PROPERTIES:~%:A: one~%:B: two~%:END:~%")))
           (context (export-man-test-context document))
           (drawer
             (export-man-test-find-view
              (org-kit::%export-render-context-root-views context)
              :property-drawer)))
      (setf (org-kit::%export-render-context-options context)
            (acons :properties t
                   (org-kit::%export-render-context-options context)))
      (let* ((property-output (org-kit::export-render-view context drawer))
             (first (make-node :example-block :properties '(:value "one")))
             (second (make-node :example-block :properties '(:value "two")))
             (views (list (export-man-test-view first document)
                          (export-man-test-view second document))))
        (setf (org-kit::%export-render-context-root-views context) views)
        (with-soft-assertions
          (expect (search "A: one" property-output) :to-be-truthy)
          (expect (search "B: two" property-output) :to-be-truthy)
          (expect (search (format nil ".RE~%.RS")
                          (org-kit::export-man-render-document context))
                  :to-be-truthy)
          (progn
            (expect
             (eq (org-kit::export-view-source-document
                  (org-kit::export-man-clone-node-and-view
                   (org-kit::%make-export-view :node (make-node :paragraph))
                   document))
                 document)
             :to-be t)
            (let* ((property
                     (make-node :node-property
                                :properties '(:key "OWNER" :value "Synthetic")))
                   (drawer-node
                     (make-node :property-drawer :children (list property)))
                   (rendered
                     (export-man-test-render-node
                      context drawer-node nil
                      (list (export-man-test-view property nil)))))
              (expect (search "OWNER: Synthetic" rendered) :to-be-truthy))
            (let* ((ignored-child (make-node :keyword :properties '(:key "OTHER")))
                   (drawer-node
                     (make-node :property-drawer :children (list ignored-child))))
              (expect (export-man-test-render-node context drawer-node nil
                                                   (list (export-man-test-view
                                                          ignored-child nil)))
                      :to-equal ""))
            (expect (export-man-test-render-node
                     context (make-node :property-drawer) nil)
                    :to-equal "")
            (let* ((row (make-node :table-row))
                   (table (make-node :table :children (list row))))
              (declare (ignore table))
              (expect (export-man-test-render-node context row document)
                      :to-equal "")))))))
)

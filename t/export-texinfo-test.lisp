;;;; t/export-texinfo-test.lisp
(in-package #:cl-org-kit/test)

(defun export-texinfo-test-path (relative)
  (asdf:system-relative-pathname
   "cl-org-kit" (concatenate 'string "t/fixtures/export/" relative)))

(defun export-texinfo-test-read-text (relative)
  (uiop:read-file-string (export-texinfo-test-path relative)))

(defun export-texinfo-test-read-octets (relative)
  (with-open-file (stream (export-texinfo-test-path relative)
                          :direction :input :element-type '(unsigned-byte 8))
    (let* ((length (file-length stream))
           (octets (make-array length :element-type '(unsigned-byte 8))))
      (unless (= (read-sequence octets stream) length)
        (error "Unable to read all bytes from Texinfo fixture ~A" relative))
      octets)))

(defun export-texinfo-test-expected (case)
  (export-texinfo-test-read-octets
   (concatenate 'string "oracle/texinfo/" case ".texi")))

(defun export-texinfo-test-actual (case &optional include-resolver)
  (let* ((source (concatenate 'string "oracle/corpus/" case ".org"))
         (document (parse-org (export-texinfo-test-read-text source)
                              :source-name source)))
    (export-org document :texinfo
                :include-resolver include-resolver
                :output-type :octets)))

(defun export-texinfo-test-bytes (text)
  (cl-codec-kit:string-to-octets text :encoding :utf-8))

(describe "X4 Texinfo export"
  (it "matches pinned Org 9.8.10 output for existing and Texinfo-specific fixtures"
    (let ((cases '("metadata-selection"
                   "exclude-tags-override"
                   "footnotes-macros-links"
                   "include"
                   "source-blocks"
                   "texinfo-elements"
                   "texinfo-options"
                   "texinfo-escape-boundaries"
                   "texinfo-extended")))
      (with-soft-assertions
        (dolist (case cases)
          (let* ((resolver-calls nil)
                 (resolver
                   (when (string= case "include")
                     (lambda (path parent-source)
                       (declare (ignore parent-source))
                       (push path resolver-calls)
                       (when (string= path "included/fragment.org")
                         (export-texinfo-test-read-text
                          "oracle/corpus/included/fragment.org")))))
                 (actual (export-texinfo-test-actual case resolver)))
            (expect actual :to-equalp (export-texinfo-test-expected case))
            (expect (nreverse resolver-calls)
                    :to-equal (if (string= case "include")
                                  '("included/fragment.org")
                                  nil))))
        (expect cases
                :to-equal '("metadata-selection"
                            "exclude-tags-override"
                            "footnotes-macros-links"
                            "include"
                            "source-blocks"
                            "texinfo-elements"
                            "texinfo-options"
                            "texinfo-escape-boundaries"
                            "texinfo-extended")))))

  (it "escapes Org text, preserves raw Texinfo output, and filters foreign raw blocks"
    (let* ((output (export-texinfo-test-actual "texinfo-escape-boundaries"))
           (foreign-output
             (export-org
              (parse-org
               (format nil "#+BEGIN_EXPORT html~%foreign block marker~%#+END_EXPORT~%~%@@html:foreign inline marker@@ and @@texinfo:@raw{,}@@"))
              :texinfo)))
      (expect (not (search "foreign block marker" foreign-output))
              :to-be t)
      (expect (not (search "foreign inline marker" foreign-output))
              :to-be t)
      (expect (search "@raw{,}" foreign-output) :to-be-truthy)
      (expect (search (export-texinfo-test-bytes
                       "Text boundary: @@ @{ @} comma@comma{}")
                      output)
              :to-be-truthy)
      (expect (search (export-texinfo-test-bytes
                       "@raw{,} @@literal")
                      output)
              :to-be-truthy)
      (expect (search (export-texinfo-test-bytes
                       "@raw{,} snippet@@@@")
                      output)
              :to-be-truthy)
      (expect (search (export-texinfo-test-bytes
                       (format nil "First hard-break line@*~%Second hard-break line"))
                      output)
              :to-be-truthy)))

  (it "signals the named output resource limit condition"
    (let* ((source (export-texinfo-test-read-text
                    "oracle/corpus/texinfo-elements.org"))
           (document (parse-org source
                                :limits (make-limits :max-output-length 80)))
           (condition
             (handler-case
                 (progn (export-org document :texinfo) nil)
               (org-resource-limit-error (condition) condition))))
      (expect condition :to-be-truthy)
      (expect (org-resource-limit-error-limit-name condition)
              :to-be :max-output-length)
      (expect (> (org-resource-limit-error-actual condition)
                 (org-resource-limit-error-limit condition))
              :to-be-truthy)))
  (it "rejects unsupported Texinfo classes through the public export API"
    (let* ((document
             (parse-org (format nil "#+TEXINFO_CLASS: unsupported~%~%* Section~%")))
           (condition
             (handler-case
                 (progn (export-org document :texinfo) nil)
               (org-export-unsupported-syntax (condition) condition))))
      (expect condition :to-be-truthy)
      (expect (org-export-unsupported-syntax-backend condition)
              :to-be :texinfo)
      (expect (org-export-unsupported-syntax-feature condition)
              :to-be :texinfo-class))))

(defun export-texinfo-test-context (document)
  (multiple-value-bind (descriptor ancestry)
      (org-kit::export-registry-snapshot :texinfo)
    (let ((context
            (org-kit::%make-export-render-context
             :backend :texinfo :document document
             :metadata nil :options nil :limits nil
             :root-views nil :toc nil :footnotes nil
             :preparation (org-kit::%make-export-preparation)
             :raw-output-options
             (org-kit::%export-backend-descriptor-raw-output-options descriptor)
             :extensions nil :ancestry ancestry :owner-backend :texinfo
             :state nil :states nil)))
      (org-kit::export-prepare document :texinfo :filter-context context)
      context)))

(defun export-texinfo-test-view (node document &optional children payload)
  (org-kit::%make-export-view :node node :children children :payload payload
                              :source-document document))

(defun export-texinfo-test-find-view (views type)
  (loop for view in views
        for found =
          (if (eq (node-type (export-view-node view)) type)
              view
            (export-texinfo-test-find-view (export-view-children view) type))
        when found return found))

(defun export-texinfo-test-render-node (context node document
                                       &optional children payload)
  (org-kit::export-render-view
   context (export-texinfo-test-view node document children payload)))

(describe "X4 Texinfo renderer boundaries"
  (it "renders macro titles, entity fallbacks, and item tag forms"
    (let* ((document (parse-org "X"))
           (context (export-texinfo-test-context document))
           (plain-view
             (export-texinfo-test-find-view
              (org-kit::%export-render-context-root-views context) :plain-text))
           (plain-node (export-view-node plain-view))
           (macro-node (make-node :macro))
           (macro-title
             (make-node :headline :properties (list :title (list macro-node))))
           (macro-title-view
             (export-texinfo-test-view
              macro-title document nil
              (list :title-macros
                    (list (cons macro-node (list plain-view))))))
           (fallback-title
             (make-node :headline :properties '(:value "Fallback")))
           (tagged-item
             (make-node :item :properties
                        (list :tag (list plain-node " tail"))))
           (tagged-item-view (export-texinfo-test-view tagged-item document))
           (odd-tag (make-node :item :properties '(:tag :unsupported)))
           (odd-tag-view (export-texinfo-test-view odd-tag document)))
      (with-soft-assertions
        (expect (org-kit::export-texinfo-title-source fallback-title)
                :to-equal "Fallback")
        (expect (org-kit::export-texinfo-transcode-title
                 context macro-title macro-title-view 0)
                :to-equal "X")
        (expect (org-kit::export-texinfo-headline-title
                 context fallback-title 0)
                :to-equal "Fallback")
        (expect (org-kit::export-texinfo-headline-node-name
                 context fallback-title)
                :to-equal "Fallback")
        (expect (search "@item X tail"
                        (org-kit::export-texinfo-render-item
                         context tagged-item tagged-item-view
                         (org-kit::%make-export-render-frame)))
                :to-be-truthy)
        (expect (org-kit::export-texinfo-render-item
                 context odd-tag odd-tag-view
                 (org-kit::%make-export-render-frame))
                :to-equal (format nil "@item~%~%"))
        (expect (org-kit::export-texinfo-entity
                 (make-node :entity :properties
                            '(:name "unknown" :utf-8 "utf" :html "html")))
                :to-equal "utf")
        (expect (org-kit::export-texinfo-entity
                 (make-node :entity :properties '(:name "unknown" :html "html")))
                :to-equal "html")
        (progn
          (expect (org-kit::export-texinfo-entity
                   (make-node :entity
                              :properties '(:name "unknown" :value "value")))
                  :to-equal "value")
          (expect (org-kit::export-texinfo-normalize-lines "") :to-equal "")
          (expect (org-kit::export-texinfo-normalize-lines "body")
                  :to-equal (format nil "body~%"))
          (progn
          (expect (search
                   "String tag"
                   (export-texinfo-test-render-node
                    context (make-node :item :properties '(:tag "String tag"))
                    document))
                  :to-be-truthy)
          (expect (org-kit::export-texinfo-node-name-base "") :to-equal "")
          (expect (org-kit::export-texinfo-node-name-base "(A)")
                  :to-equal "[A)")
          (expect (org-kit::export-texinfo-node-name-base "A@comma{}B")
                  :to-equal "AB")
          (expect (org-kit::export-texinfo-node-name-base "A@commaB")
                  :to-equal "AB")
          (expect (org-kit::export-texinfo-normalize-lines nil)
                  :to-equal "")
          (expect (mapcar (lambda (value)
                            (export-texinfo-test-render-node
                             context (make-node :code
                                                :properties (list :value value))
                             document))
                          '("" "~" "x~" "~x"))
                  :to-equal '("@code{}" "@code{~}" "@code{x~}" "@code{~x}")))))))

  (it "handles file links, math forms, and uncovered transcoders"
    (let* ((document (parse-org "X"))
           (context (export-texinfo-test-context document))
           (text-view
             (export-texinfo-test-find-view
              (org-kit::%export-render-context-root-views context) :plain-text))
           (described-view
             (export-texinfo-test-view
              (make-node :link :properties
                         '(:type "file" :path "//server/share"))
              document (list text-view)))
           (math-values '("\\(x\\)" "\\[x\\]" "$$x$$" "$x$" "x"))
           (math-results
             (mapcar (lambda (value)
                       (export-texinfo-test-render-node
                        context
                        (make-node :latex-fragment :properties (list :value value))
                        document))
                     math-values))
           (property-node
             (make-node :node-property
                        :properties '(:key "OWNER" :value "Ada")))
           (property-drawer (make-node :property-drawer))
           (property-view
             (export-texinfo-test-view
              property-drawer document
              (list (export-texinfo-test-view property-node document))))
           (inline-task
             (make-node :inlinetask
                        :properties '(:todo-keyword "TODO" :priority 65
                                      :tags ("tag") :raw-value "Task")))
           (template (org-kit::%make-node :type :template))
           (footnote (make-node :footnote-reference)))
      (setf (org-kit::%export-render-context-options context)
            (acons :todo t
                   (acons :priority t
                          (acons :tags t
                                 (org-kit::%export-render-context-options context)))))
      (with-soft-assertions
        (expect (search "@uref{file://server/share, X}"
                        (org-kit::export-render-view context described-view))
                :to-be-truthy)
        (expect (search "file:///tmp/x"
                        (org-kit::export-texinfo-render-link
                         context
                         (make-node :link :properties
                                    '(:type "file" :path "/tmp/x"))
                         (export-texinfo-test-view
                          (make-node :link :properties
                                     '(:type "file" :path "/tmp/x")) document)
                         (org-kit::%make-export-render-frame)))
                :to-be-truthy)
        (expect math-results
                :to-equal (list "@math{x}" "@math{x}" "@math{x}"
                                "@math{x}" ""))
        (expect (org-kit::export-render-view context property-view)
                :to-equal (format nil "@verbatim~%@OWNER: Ada@end verbatim"))
        (expect (search "TODO"
                        (export-texinfo-test-render-node
                         context inline-task document))
                :to-be-truthy)
        (expect (search "[A]"
                        (export-texinfo-test-render-node
                         context inline-task document))
                :to-be-truthy)
        (progn
          (expect (search ":tag:"
                          (export-texinfo-test-render-node
                           context inline-task document))
                  :to-be-truthy)
          (expect (export-texinfo-test-render-node
                   context (make-node :property-drawer) document)
                  :to-equal "")
          (expect (export-texinfo-test-render-node
                   context (make-node :link :properties
                                      '(:type "file" :path "relative"))
                   document)
                  :to-equal "@uref{relative}")
          (expect (export-texinfo-test-render-node
                   context (make-node :link :properties
                                      '(:type "file" :path ""))
                   document)
                  :to-equal "@uref{}")
          (expect (export-texinfo-test-render-node
                   context (make-node :link :properties
                                      '(:type "fuzzy" :path "missing"))
                   document)
                  :to-equal "@ref{missing}")
          (expect (export-texinfo-test-render-node
                   context (make-node :example-block :properties '(:value ""))
                   document)
                  :to-equal (format nil "@example~%@end example"))
          (expect (export-texinfo-test-render-node
                   context
                   (make-node :example-block
                              :properties (list :value (format nil "body~%")))
                   document)
                  :to-equal (format nil "@example~%body~%@end example"))
          (progn
            (expect (search
                     "@lisp"
                     (export-texinfo-test-render-node
                      context
                      (make-node :src-block
                                 :properties '(:language "lisp" :value "code"))
                      document nil '(:exports :both)))
                    :to-be-truthy)
            (let ((options (org-kit::%export-render-context-options context)))
              (setf (org-kit::%export-render-context-options context)
                    (acons :todo nil
                           (acons :priority nil
                                  (acons :tags nil options))))
              (let ((output (export-texinfo-test-render-node
                             context inline-task document)))
                (expect (not (search "TODO" output)) :to-be t)
                (expect (not (search ":tag:" output)) :to-be t))
              (setf (org-kit::%export-render-context-options context) options))))
        (expect (org-kit::export-texinfo-transcode-node
                 template (export-texinfo-test-view template document)
                 (org-kit::%make-export-render-frame) context)
                :to-equal "")
        (expect (export-texinfo-test-render-node context footnote document)
                :to-equal "")
        (expect (export-texinfo-test-render-node
                 context (make-node :keyword :properties
                                    '(:key "CINDEX" :value "entry")) document)
                :to-equal "@cindex entry")
        (expect (export-texinfo-test-render-node
                 context (make-node :keyword :properties
                                    '(:key "OTHER" :value "ignored")) document)
                :to-equal "")
        (expect (export-texinfo-test-render-node
                 context (make-node :property-drawer) document)
                :to-equal ""))))

  (it "separates source documents and completes headings with menus"
    (let* ((document (parse-org (format nil "* Parent~%body~%** Child~%child")))
           (other-document (parse-org "Other"))
           (context (export-texinfo-test-context document))
           (section-view
             (export-texinfo-test-find-view
              (org-kit::%export-render-context-root-views context) :section))
           (section-output (org-kit::export-render-view context section-view))
           (first-block
             (make-node :export-block
                        :properties '(:type "TEXINFO" :value "first")))
           (second-block
             (make-node :export-block
                        :properties '(:type "TEXINFO" :value "second")))
           (cross-document-view
             (export-texinfo-test-view
              (make-node :section) document
              (list (export-texinfo-test-view first-block document)
                    (export-texinfo-test-view second-block other-document)
                    (export-texinfo-test-view
                     (make-node :export-block
                                :properties (list :type "TEXINFO"
                                                  :value (string #\Newline)))
                     (parse-org "Blank")))))
           (no-parent-section (make-node :section)))
      (with-soft-assertions
        (expect (search (format nil "body~%~%@menu") section-output)
                :to-be-truthy)
        (expect (search
                 (format nil "first~%~%second")
                 (org-kit::export-texinfo-render-children
                  context cross-document-view
                  (org-kit::%make-export-render-frame) :block))
                :to-be-truthy)
        (expect (org-kit::export-texinfo-render-section
                 context no-parent-section
                 (export-texinfo-test-view no-parent-section document)
                 (org-kit::%make-export-render-frame))
                :to-equal "")
        (progn
          (let* ((foreign-block
                   (make-node :export-block
                              :properties (list :type "TEXINFO" :value "foreign")))
                 (view
                   (export-texinfo-test-view
                    no-parent-section nil
                    (list (export-texinfo-test-view foreign-block nil)))))
            (expect (org-kit::export-texinfo-render-section
                     context no-parent-section view
                     (org-kit::%make-export-render-frame))
                    :to-equal ""))
          (expect (export-texinfo-test-render-node
                   context
                   (make-node :special-block :properties (list :type "note"))
                   document)
                  :to-equal (format nil "@note~%@end note"))
          (let ((clock-output
                  (export-org
                   (parse-org
                    (format nil "* Task~%:LOGBOOK:~%CLOCK: [2024-01-01 Mon 09:00]~%:END:~%"))
                   :texinfo)))
            (expect (search "[2024-01-01 Mon 09:00]" clock-output)
                    :to-be-truthy)
            (expect (not (search "  (" clock-output)) :to-be t))
          (expect (export-texinfo-test-render-node
                   context
                   (make-node :latex-environment
                              :properties (list :value "  "))
                   document)
                  :to-equal ""))
        (expect
         (search "@ref{"
                 (export-org
                  (parse-org
                   (format nil "* Heading~%:PROPERTIES:~%:CUSTOM_ID: Anchor~%:END:~%[[anchor][Target]]"))
                  :texinfo))
         :to-be-truthy)
        (progn
          (expect (search (format nil "@node Parent~%@chapter Parent")
                          (export-org document :texinfo))
                  :to-be-truthy)
          (let* ((parent-view
                   (export-texinfo-test-find-view
                    (org-kit::%export-render-context-root-views context)
                    :headline))
                 (parent-node (export-view-node parent-view)))
            (setf (org-kit::%export-render-context-options context)
                  (acons :toc 0
                         (org-kit::%export-render-context-options context)))
            (expect (search "@menu"
                            (org-kit::export-texinfo-menu
                             context (list parent-node) 0 t))
                    :to-be-truthy)
            (setf (org-kit::%export-render-context-options context)
                  (acons :toc -1
                         (org-kit::%export-render-context-options context)))
            (expect (search "@detailmenu"
                            (org-kit::export-texinfo-menu
                             context (list parent-node) 0 t))
                    :to-be-truthy)
            (let* ((headline
                     (make-node :headline :properties '(:value "Synthetic")))
                   (body (make-node :export-block
                                    :properties '(:type "TEXINFO" :value "Body")))
                   (rendered
                     (export-texinfo-test-render-node
                      context headline document
                      (list (export-texinfo-test-view body document)))))
              (expect (char= (char rendered (1- (length rendered))) #\Newline)
                      :to-be t))
            (expect (search
                     "result"
                     (export-texinfo-test-render-node
                      context
                      (make-node :src-block
                                 :properties '(:language "lisp" :value "code"))
                      document
                      (list (export-texinfo-test-view
                             (make-node :export-block
                                        :properties '(:type "TEXINFO"
                                                      :value "result"))
                             document))
                      '(:exports :both)))
                    :to-be-truthy)
            (expect (export-texinfo-test-render-node
                     context (make-node :center-block) document)
                    :to-equal "")
            (expect (export-texinfo-test-render-node
                     context
                     (make-node :special-block :properties '(:type "note"))
                     document
                     (list (export-texinfo-test-view
                            (make-node :export-block
                                       :properties '(:type "TEXINFO"
                                                     :value "body"))
                            document)))
                    :to-equal (format nil "@note~%body~%@end note"))
            (expect (export-texinfo-test-render-node
                     context
                     (make-node :special-block :properties '(:type "note"))
                     document
                     (list (export-texinfo-test-view
                            (make-node :export-block
                                       :properties (list :type "TEXINFO"
                                                         :value (format nil
                                                                        "body~%")))
                            document)))
                    :to-equal (format nil "@note~%body~%@end note"))
            (expect (export-texinfo-test-render-node
                     context (make-node :table) document)
                    :to-equal "")
            (let* ((section (make-node :section))
                   (headline-parent
                     (make-node :headline :children (list section))))
              (declare (ignore headline-parent))
              (setf (org-kit::%export-render-context-root-views context)
                    (list (export-texinfo-test-view section document)))
              (multiple-value-bind (preamble body original)
                  (org-kit::export-texinfo-top-section context)
                (progn
                  (expect (list preamble body original)
                          :to-equal '(nil nil nil))
                  (expect (search
                           "@menu"
                           (org-kit::export-texinfo-render-section
                            context (export-view-node section-view)
                            (export-texinfo-test-view
                             (export-view-node section-view) document)
                            (org-kit::%make-export-render-frame)))
                          :to-be-truthy)
                  (expect (search
                           "foreign"
                           (org-kit::export-texinfo-render-section
                            context no-parent-section
                            (export-texinfo-test-view
                             no-parent-section other-document
                             (list (export-texinfo-test-view
                                    (make-node :export-block
                                               :properties '(:type "TEXINFO"
                                                             :value "foreign"))
                                    other-document)))
                            (org-kit::%make-export-render-frame)))
                          :to-be-truthy)
                  (expect (search
                           "@center Centered"
                           (export-texinfo-test-render-node
                            context (make-node :center-block) document
                            (list (export-texinfo-test-view
                                   (make-node :export-block
                                              :properties '(:type "TEXINFO"
                                                            :value "Centered"))
                                   document))))
                          :to-be-truthy)
                  (let* ((untitled
                           (make-node :headline :properties '(:value "")))
                         (untitled-view
                           (export-texinfo-test-view untitled document)))
                    (setf (org-kit::%export-render-context-root-views context)
                          (list untitled-view)
                          (org-kit::%export-render-context-state context)
                          (org-kit::export-texinfo-make-state context))
                    (expect (org-kit::export-texinfo-headline-node-name
                             context untitled)
                            :to-equal "Untitled"))
                  (let* ((template-document
                           (parse-org
                            (format nil
                                    "#+TEXINFO_FILENAME: .~%#+TEXINFO_DIR_NAME: Manual~%~%Body")))
                         (template-context
                           (export-texinfo-test-context template-document))
                         (template-output
                           (org-kit::export-texinfo-render-template
                            template-context nil "" nil)))
                    (expect (search "* Manual: (Manual)." template-output)
                            :to-be-truthy))))))))))
)

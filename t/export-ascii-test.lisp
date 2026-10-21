;;;; t/export-ascii-test.lisp
(in-package #:cl-org-kit/test)

(defun export-ascii-test-lines (&rest lines)
  (format nil "~{~A~^~%~}" lines))

(defun export-ascii-test-source (&rest lines)
  (concatenate 'string (apply #'export-ascii-test-lines lines)
               (string #\Newline)))

(defun export-ascii-test-fixture (relative-path)
  (uiop:read-file-string
   (asdf:system-relative-pathname "cl-org-kit" relative-path)))

(defun export-ascii-test-find-node (root type)
  (or (when (eq (node-type root) type) root)
      (some (lambda (child) (export-ascii-test-find-node child type))
            (node-children root))))

(defun export-ascii-test-view (node source-document &optional children)
  (org-kit::%make-export-view :node node :source-document source-document
                              :children children))

(defun export-ascii-test-preparation (document root-views &optional options)
  (org-kit::%make-export-preparation
   :document document :backend :ascii :metadata nil :options options
   :root-views root-views :toc nil :footnotes nil :allow-raw-html nil))

(defun export-ascii-test-output-state
    (preparation &key (base-count 0) (measure-only-p nil)
                     (downstream-indent 0) (visible-controls-p t))
  (org-kit::export-ascii-output-for-preparation
   preparation base-count measure-only-p downstream-indent visible-controls-p))

(defun export-ascii-test-table-cookie-preflight-result ()
  (labels ((find-table-view (views)
             (loop for view in views
                   for found =
                     (if (eq (node-type (org-kit::export-view-node view))
                             :table)
                         view
                         (find-table-view
                          (org-kit::export-view-children view)))
                   when found return found)))
    (let* ((cookie (make-string 32 :initial-element (char "9" 0)))
           (source
             (format nil "| <~A> |~%~{~A~%~}"
                     cookie (make-list 20 :initial-element "| x |")))
           (document
             (parse-org source
                        :limits (make-limits :max-output-length 1000)))
           (preparation (org-kit::export-prepare document :ascii))
           (table-view
             (find-table-view
              (org-kit::export-preparation-root-views preparation)))
           (table-output (export-ascii-test-output-state preparation))
           (buffer (org-kit::export-ascii-output-output table-output))
           (initial-capacity (array-total-size buffer))
           (row-writer-symbol
             (find-symbol "EXPORT-ASCII-OUTPUT-WRITE-TABLE-ROW" "ORG-KIT"))
           (original-row-writer (symbol-function row-writer-symbol))
           (row-writer-called-p nil)
           (table-condition
             (unwind-protect
                  (progn
                    (setf (symbol-function row-writer-symbol)
                          (lambda (&rest arguments)
                            (setf row-writer-called-p t)
                            (apply original-row-writer arguments)))
                    (catch (quote ascii-table-limit)
                      (handler-bind
                          ((org-resource-limit-error
                             (lambda (condition)
                               (throw (quote ascii-table-limit) condition))))
                        (org-kit::export-ascii-output-write-table
                         table-output table-view preparation))))
               (setf (symbol-function row-writer-symbol)
                     original-row-writer))))
      (list :cookie-present-p
            (search (format nil "<~A>" cookie) source)
            :source-length (length source)
            :newline-count (count (code-char 10) source)
            :table-view-p
            (and table-view
                 (eq (node-type (org-kit::export-view-node table-view)) :table))
            :condition-p
            (typep table-condition
                   (find-class
                    (find-symbol "ORG-RESOURCE-LIMIT-ERROR" "ORG-KIT")))
            :limit-name
            (and table-condition
                 (org-resource-limit-error-limit-name table-condition))
            :limit
            (and table-condition
                 (org-resource-limit-error-limit table-condition))
            :row-writer-called-p row-writer-called-p
            :fill-pointer (fill-pointer buffer)
            :capacity (array-total-size buffer)
            :initial-capacity initial-capacity))))

(defun export-ascii-test-output (preparation function)
  (let ((output (export-ascii-test-output-state preparation)))
    (funcall function output)
    (org-kit::export-ascii-output-string output)))

(defun export-ascii-test-render-view (view preparation)
  (export-ascii-test-output
   preparation
   (lambda (output)
     (org-kit::export-ascii-output-render-view output view preparation))))

(defun export-ascii-test-render-views (views preparation)
  (export-ascii-test-output
   preparation
   (lambda (output)
     (org-kit::export-ascii-output-write-views output views preparation))))

(defun export-ascii-test-render-list-item (view preparation ordinal)
  (export-ascii-test-output
   preparation
   (lambda (output)
     (org-kit::export-ascii-output-render-list-item
      output view preparation ordinal))))

(defun export-ascii-test-render-gap (previous current preparation)
  (export-ascii-test-output
   preparation
   (lambda (output)
     (org-kit::export-ascii-output-write-gap output previous current))))

(defun export-ascii-test-render-heading-title
    (view preparation numbers text-width &optional underline toc)
  (export-ascii-test-output
   preparation
   (lambda (output)
     (org-kit::export-ascii-output-write-heading-title
      output view preparation numbers text-width :underline underline :toc toc))))

(defun export-ascii-test-link-nodes (document)
  (let ((links nil))
    (map-nodes (lambda (node)
                 (when (eq (node-type node) :link)
                   (push node links)))
               (document-root document))
    (nreverse links)))

(defun export-ascii-test-catch-error (function)
  (handler-case (progn (funcall function) nil)
    (error (condition) condition)))

(defun export-ascii-test-copy-value (value)
  (cond
    ((stringp value) (copy-seq value))
    ((consp value)
     (cons (export-ascii-test-copy-value (car value))
           (export-ascii-test-copy-value (cdr value))))
    ((vectorp value)
     (map 'vector #'export-ascii-test-copy-value value))
    (t value)))

(defun export-ascii-test-document-snapshot (document)
  (let ((nodes nil))
    (map-nodes
     (lambda (node)
       (push (list node (node-type node) (node-parent node)
                   (copy-list (node-children node))
                   (node-begin node) (node-end node)
                   (node-contents-begin node) (node-contents-end node)
                   (node-post-blank node)
                   (export-ascii-test-copy-value (node-properties node)))
             nodes))
     (document-root document))
    (list (copy-seq (document-text document))
          (export-ascii-test-copy-value (document-source-name document))
          (export-ascii-test-copy-value (document-settings document))
          (nreverse nodes))))

(defun export-ascii-test-oracle-cases ()
  '(("metadata-selection" "metadata-selection.org" nil)
    ("exclude-tags-override" "exclude-tags-override.org" nil)
    ("footnotes-macros-links" "footnotes-macros-links.org" nil)
    ("include" "include.org" "included/fragment.org")
    ("source-blocks" "source-blocks.org" nil)))

(describe "ASCII export"
  (it "matches every checked-in ASCII oracle output through the public API"
    (let ((cases (export-ascii-test-oracle-cases)))
      (with-soft-assertions
        (expect (length cases) :to-be 5)
        (dolist (case cases)
          (let* ((name (first case))
                 (source-name (second case))
                 (include-path (third case))
                 (source
                   (export-ascii-test-fixture
                    (format nil "t/fixtures/export/oracle/corpus/~A"
                            source-name)))
                 (expected
                   (export-ascii-test-fixture
                    (format nil "t/fixtures/export/oracle/ascii/~A.txt" name)))
                 (document (parse-org source :source-name source-name))
                 (calls nil)
                 (resolver
                   (lambda (path parent-source)
                     (push (list path parent-source) calls)
                     (if (string= path include-path)
                         (export-ascii-test-fixture
                          "t/fixtures/export/oracle/corpus/included/fragment.org")
                         (error "Unexpected ASCII export include: ~A" path))))
                 (actual
                   (if include-path
                       (export-org document :ascii :include-resolver resolver)
                       (export-org document :ascii))))
            (expect (list name (plusp (length source)) (plusp (length expected)))
                    :to-equal (list name t t))
            (expect actual :to-equal expected)
            (expect (nreverse calls)
                    :to-equal (if include-path
                                  (list (list include-path source-name))
                                  nil)))))))

  (it "renders ordered and unordered lists deterministically"
    (let* ((source
             (export-ascii-test-source
              "#+OPTIONS: toc:nil num:nil"
              "- bullet"
              "- second bullet"
              "1. ordered"
              "2. second ordered"))
           (expected
             (export-ascii-test-lines
              "  - bullet"
              "  - second bullet"
              "  1. ordered"
              "  2. second ordered"
              ""))
           (document (parse-org source))
           (first-output (export-org document :ascii))
           (second-output (export-org document :ascii)))
      (with-soft-assertions
        (expect first-output :to-equal expected)
        (expect second-output :to-equal first-output))))

  (it "renders list state, nested descriptions, and list-to-paragraph gaps"
    (let* ((source
             (export-ascii-test-source
              "#+OPTIONS: toc:nil num:nil"
              "Before list."
              "- [X] checked"
              "- [ ] unchecked"
              "- [-] partial"
              "Between lists."
              "+ Term :: Definition with"
              "  continuation"
              "  - nested item"
              "+ :: Anonymous description"
              "+ Empty ::"
              "After list."))
           (document (parse-org source))
           (tagged-item
             (let ((item nil))
               (map-nodes
                (lambda (node)
                  (when (and (eq (node-type node) :item)
                             (<= (node-begin node) (search "Term" source))
                             (< (search "Term" source) (node-end node)))
                    (setf item node)))
                (document-root document))
               item))
           ;; Reader object preparation consumes this range into :tag; restore
           ;; the source range to exercise the ASCII renderer's tag extraction.
           (tag-start (search "Term" source))
           (output
             (progn
               (setf (node-property tagged-item :reader-tag-range)
                     (cons tag-start (+ tag-start (length "Term"))))
               (export-org document :ascii)))
           (expected
             (export-ascii-test-lines
              "Before list."
              ""
              "  - [X] checked"
              "  - [ ] unchecked"
              "  - [-] partial"
              "Between lists."
              ""
              "  Term :: "
              "        Definition with continuation"
              ""
              "          - nested item"
              "  :: "
              "        :: Anonymous description"
              "  :: "
              "After list."
              "")))
      (with-soft-assertions
        (expect tagged-item :not :to-be nil)
        (expect tag-start :not :to-be nil)
        (expect output :to-equal expected))))

  (it "renders dynamic blocks and inline object fallbacks"
    (let* ((source
             (export-ascii-test-source
              "#+OPTIONS: toc:nil num:nil"
              "#+BEGIN: clocktable :scope file"
              "dynamic body"
              "#+END:"
              "------"
              "Inline source: src_emacs-lisp[:results raw]{(+ 1 2)}."
              "Target <<anchor>> and stats [2/5]."
              "Timestamp <2026-10-14 Wed>."
              "Citation [cite: see;@key note]."
              "Math \\(x + 1\\) and \\[y=2\\]."))
           (document (parse-org source))
           (output (export-org document :ascii))
           (expected
             (export-ascii-test-lines
              "#+BEGIN: clocktable :scope file"
              "dynamic body"
              "#+END:"
              ""
              ""
              "------------------------------------------------------------------------"
              ""
              "Inline source: (+ 1 2). Target and stats [2/5]. Timestamp <2026-10-14"
              "Wed>. Citation [cite: see;@key note]. Math \\(x + 1\\) and \\[y=2\\]."
              "")))
      (with-soft-assertions
        (expect (export-ascii-test-find-node (document-root document) :dynamic-block)
                :not :to-be nil)
        (expect (export-ascii-test-find-node (document-root document) :inline-src-block)
                :not :to-be nil)
        (expect (export-ascii-test-find-node (document-root document) :target)
                :not :to-be nil)
        (expect (export-ascii-test-find-node (document-root document) :statistics-cookie)
                :not :to-be nil)
        (expect (export-ascii-test-find-node (document-root document) :timestamp)
                :not :to-be nil)
        (expect (export-ascii-test-find-node (document-root document) :latex-fragment)
                :not :to-be nil)
        (expect output :to-equal expected))))

  (it "preserves Babel calls and citations without evaluating them"
    (let* ((source
             (export-ascii-test-source
              "#+OPTIONS: toc:nil num:nil"
              "#+CALL: foo()"
              "Inline call: call_sum(1, 2). Citation [cite/t: see;@smith2024]."))
           (document (parse-org source))
           (output (export-org document :ascii)))
      (with-soft-assertions
        (expect (export-ascii-test-find-node (document-root document) :babel-call)
                :not :to-be nil)
        (expect (export-ascii-test-find-node
                 (document-root document) :inline-babel-call)
                :not :to-be nil)
        (expect (export-ascii-test-find-node (document-root document) :citation)
                :not :to-be nil)
        (expect output
                :to-equal
                (export-ascii-test-source
                 "#+CALL: foo()"
                 ""
                 ""
                 "Inline call: call_sum(1, 2). Citation [cite/t: see;@smith2024].")))))

  (it "renders source-only width, link, code, and entity cases"
    (let* ((text (concatenate 'string (make-string 69 :initial-element #\a) " b"))
           (document (parse-org text))
           (plain-three (make-node :plain-text :begin 0 :end (length text)))
           (plain-four (make-node :plain-text :begin 0 :end (length text)))
           (paragraph-three (make-node :paragraph :children (list plain-three)))
           (paragraph-four (make-node :paragraph :children (list plain-four)))
           (headline-three
             (make-node :headline :properties '(:level 3 :raw-value "Three")
                        :children (list paragraph-three)))
           (headline-four
             (make-node :headline :properties '(:level 4 :raw-value "Four")
                        :children (list paragraph-four)))
           (target
             (make-node :headline :properties '(:level 1 :raw-value "Target")))
           (entity-span (make-node :entity :begin 0 :end 5))
           (root (make-node :org-data
                            :children (list headline-three headline-four target
                                            entity-span)))
           (target-view (export-ascii-test-view target document))
           (preparation
             (export-ascii-test-preparation
              document (list target-view) '((:num . nil))))
           (view-three
             (export-ascii-test-view
              paragraph-three document
              (list (export-ascii-test-view plain-three document))))
           (view-four
             (export-ascii-test-view
              paragraph-four document
              (list (export-ascii-test-view plain-four document))))
           (ascii-links
             (list
              (export-ascii-test-view
               (make-node :link :properties
                          '(:type "https" :path "//fallback.test/path"))
               document)
              (export-ascii-test-view
               (make-node :link :properties '(:path "assets/topic:v2"))
               document)
              (export-ascii-test-view
               (make-node :link :properties '(:path "x")) document)
              (export-ascii-test-view
               (make-node :link :properties
                          '(:type "custom-id" :path "missing-target"))
               document)))
           ;; A leading # here is a deliberate synthetic path-normalization
           ;; case; parsed fuzzy links store the path without the marker.
           (target-link
             (export-ascii-test-view
              (make-node :link :properties '(:type "fuzzy" :path "#Target"))
              document))
           (ordinary-target-link
             (export-ascii-test-view
              (make-node :link :properties '(:type "fuzzy" :path "Target"))
              document))
           (empty-fuzzy-link
             (export-ascii-test-view
              (make-node :link :properties '(:type "fuzzy" :path ""))
              document))
           (missing-fuzzy-path
             (make-node :link :properties '(:type "fuzzy")))
           (external-link
             (make-node :link :properties '(:type "https" :path "//target")))
           (utf8-entity
             (export-ascii-test-view
              (make-node :entity :properties '(:utf8 "UTF8")) document))
           (ascii-entity
             (export-ascii-test-view
              (make-node :entity :properties '(:ascii "ASCII")) document))
           (empty-entity
             (export-ascii-test-view (make-node :entity) document))
           (span-entity (export-ascii-test-view entity-span document))
           (fixed-width
             (export-ascii-test-view
              (make-node :fixed-width :properties '(:value ":  left")) document))
           (fixed-width-rows
             (export-ascii-test-view
              (make-node :fixed-width
                         :properties
                         (list :value (format nil "ordinary~%:  fixed~%")))
              document))
           (empty-fixed-width
             (export-ascii-test-view
              (make-node :fixed-width :properties '(:value "")) document))
           (empty-example
             (export-ascii-test-view
              (make-node :example-block :properties '(:value "")) document))
           (root-view
             (export-ascii-test-view root document (list target-view)))
           (entity-text (make-string 5 :initial-element #\a)))
      ;; Direct paragraph children of a headline are a renderer-supported
      ;; synthetic AST shape; public preparation wraps them in a section.
      (setf (document-root document) root)
      (with-soft-assertions
        (expect (export-ascii-test-render-view view-three preparation)
                :to-equal (format nil "~A~%b" (make-string 69 :initial-element #\a)))
        (expect (export-ascii-test-render-view view-four preparation)
                :to-equal text)
        (expect (mapcar (lambda (view)
                          (export-ascii-test-render-view view preparation))
                        ascii-links)
                :to-equal '("<https://fallback.test/path>"
                            "<assets/topic:v2>"
                            "<x>"
                            "missing-target"))
        (expect (export-ascii-test-render-view target-link preparation)
                :to-equal "Target")
        (expect (export-ascii-test-render-view ordinary-target-link
                                                    preparation)
                :to-equal "Target")
        (expect (export-ascii-test-render-view empty-fuzzy-link preparation)
                :to-equal "")
        (expect (org-kit::export-ascii-local-heading
                 missing-fuzzy-path preparation)
                :to-be nil)
        (expect (org-kit::export-ascii-local-heading external-link preparation)
                :to-be nil)
        (expect (export-ascii-test-render-view root-view preparation)
                :to-equal (format nil "Target~%======"))
        (expect (export-ascii-test-render-view utf8-entity preparation)
                :to-equal "UTF8")
        (expect (export-ascii-test-render-view ascii-entity preparation)
                :to-equal "ASCII")
        (expect (export-ascii-test-render-view empty-entity preparation)
                :to-equal "")
        (expect (export-ascii-test-render-view span-entity preparation)
                :to-equal entity-text)
        (expect (export-ascii-test-render-view fixed-width preparation)
                :to-equal (format nil ",----~%| left~%`----"))
        (expect (export-ascii-test-render-view empty-example preparation)
                :to-equal "")
        (expect (org-kit::export-ascii-destination-safe-p
                 "https://safe.test/path"
                 (format nil "https://safe.test/~C" #\Tab))
                :to-be nil)
        (expect (org-kit::export-ascii-destination-safe-p "raw" nil)
                :to-be nil)
        (expect (org-kit::export-ascii-destination-safe-p "" "")
                :to-be nil)
        (expect (org-kit::export-ascii-destination-safe-p "x" "x")
                :to-be t)
        (expect (org-kit::export-ascii-destination-safe-p
                 "//cdn.test/path" "//cdn.test/path")
                :to-be nil)
        (expect (export-ascii-test-render-view fixed-width-rows preparation)
                :to-equal (format nil ",----~%| ordinary~%| fixed~%`----"))
        (expect (export-ascii-test-render-view empty-fixed-width preparation)
                :to-equal ""))))

  (it "tracks included heading levels and blank sections in prepared views"
    (let* ((source (parse-org "aaaaaaaaaa"))
           (fragment (parse-org "included"))
           (body-text (make-node :plain-text :begin 0 :end 4))
           (tail-text (make-node :plain-text :begin 5 :end 9))
           (body (make-node :paragraph :children (list body-text)))
           (tail (make-node :paragraph :children (list tail-text)))
           (section (make-node :section :children (list body)))
           (deep-heading
             (make-node :headline :properties '(:level 2 :raw-value "Deep")))
           (shallow-heading
             (make-node :headline :properties '(:level 1 :raw-value "Shallow")))
           (root (make-node :org-data
                            :children (list section tail deep-heading shallow-heading)))
           (included-heading
             (make-node :headline :properties '(:level 1 :raw-value "Included")))
           (fragment-root (make-node :org-data :children (list included-heading))))
      (setf (document-root source) root
            (document-root fragment) fragment-root)
      (let* ((included-view (export-ascii-test-view included-heading fragment))
             (body-view
               (export-ascii-test-view
                body source (list (export-ascii-test-view body-text source))))
             (tail-view
               (export-ascii-test-view
                tail source (list (export-ascii-test-view tail-text source))))
             (section-view
               (export-ascii-test-view section source (list body-view)))
             (deep-view (export-ascii-test-view deep-heading source))
             (shallow-view (export-ascii-test-view shallow-heading source))
             (empty-section
               (export-ascii-test-view
                (make-node :section :post-blank 1) source))
             (section-child (make-node :paragraph :post-blank 2))
             (nonempty-section
               (export-ascii-test-view
                (make-node :section :children (list section-child)) source
                (list (export-ascii-test-view section-child source))))
             (preparation (export-ascii-test-preparation source nil))
             (included-preparation
               (export-ascii-test-preparation source
                                              (list included-view deep-view shallow-view)))
             (included-body
               (export-ascii-test-render-views
                (list included-view section-view tail-view) preparation))
             (deeper-after-include
               (export-ascii-test-render-views
                (list included-view deep-view tail-view) included-preparation))
             (same-level-after-include
               (export-ascii-test-render-views
                (list included-view shallow-view tail-view) included-preparation)))
        (with-soft-assertions
          (expect (export-ascii-test-render-gap
                   empty-section tail-view preparation)
                  :to-equal (format nil "~%~%"))
          (expect (export-ascii-test-render-gap
                   nonempty-section tail-view preparation)
                  :to-equal (format nil "~%~%~%"))
          (expect included-body
                  :to-equal (format nil "Included~%========~%~%aaaa~%~%  aaaa"))
          (expect deeper-after-include
                  :to-equal
                  (format nil "1 Included~%==========~%~%~%1.1 Deep~%~A~%~%  aaaa"
                          "~~~~~~~~"))
          (expect same-level-after-include
                  :to-equal
                  (format nil "1 Included~%==========~%~%~%2 Shallow~%=========~%~%aaaa"))))))

  (it "omits heading tags only from TOC titles when requested"
    (let* ((document (parse-org (export-ascii-test-source "* Tagged :note:")))
           (heading (export-ascii-test-find-node
                     (document-root document) :headline))
           (view (export-ascii-test-view heading document))
           (preparation
             (export-ascii-test-preparation
              document (list view) '((:num . nil) (:tags . :not-in-toc))))
           (numbers (org-kit::export-ascii-numbering preparation)))
      (with-soft-assertions
        (expect (export-ascii-test-render-heading-title
                 view preparation numbers 72 nil t)
                :to-equal "Tagged")
        (expect (export-ascii-test-render-heading-title
                 view preparation numbers 72 nil nil)
                :to-equal
                (concatenate 'string "Tagged"
                             (make-string 60 :initial-element #\Space)
                             ":note:")))))

  (it "limits heading numbering and TODO titles at their option boundaries"
    (let* ((document
             (parse-org
              (export-ascii-test-source
               "#+TODO: TODO | DONE"
               "* TODO Top"
               "** Child"
               "*** Deep")))
           (headings
             (let ((found nil))
               (map-nodes (lambda (node)
                            (when (eq (node-type node) :headline)
                              (push node found)))
                          (document-root document))
               (nreverse found)))
           (views (mapcar (lambda (node)
                            (export-ascii-test-view node document))
                          headings))
           (default-preparation
             (export-ascii-test-preparation document views))
           (bounded-preparation
             (export-ascii-test-preparation document views '((:num . 2))))
           (disabled-preparation
             (export-ascii-test-preparation document views '((:num . nil))))
           (default-numbers
             (org-kit::export-ascii-numbering default-preparation))
           (bounded-numbers
             (org-kit::export-ascii-numbering bounded-preparation))
           (disabled-numbers
             (org-kit::export-ascii-numbering disabled-preparation))
           (todo-disabled-preparation
             (export-ascii-test-preparation
              document views '((:num . nil) (:todo . nil))))
           (todo-disabled-numbers
             (org-kit::export-ascii-numbering todo-disabled-preparation)))
      (with-soft-assertions
        (expect (mapcar (lambda (view)
                          (export-ascii-test-render-heading-title
                           view default-preparation default-numbers 72 nil))
                        views)
                :to-equal '("1 TODO Top" "1.1 Child" "1.1.1 Deep"))
        (expect (mapcar (lambda (view)
                          (export-ascii-test-render-heading-title
                           view bounded-preparation bounded-numbers 72 nil))
                        views)
                :to-equal '("1 TODO Top" "1.1 Child" "Deep"))
        (expect (mapcar (lambda (view)
                          (export-ascii-test-render-heading-title
                           view disabled-preparation disabled-numbers 72 nil))
                        views)
                :to-equal '("TODO Top" "Child" "Deep"))
        (expect (export-ascii-test-render-heading-title
                 (first views) todo-disabled-preparation todo-disabled-numbers 72)
                :to-equal "Top"))))

  (it "leaves headings unnumbered for invalid prepared numbering values"
    (let* ((document (parse-org (export-ascii-test-source "* Title")))
           (heading (export-ascii-test-find-node
                     (document-root document) :headline))
           (view (export-ascii-test-view heading document))
           ;; Public option parsing rejects non-integer num values. This
           ;; prepared view isolates the renderer's fallback for malformed
           ;; internal preparation state.
           (preparation
             (export-ascii-test-preparation
              document (list view) '((:num . :invalid))))
           (numbers (org-kit::export-ascii-numbering preparation))
           (invalid-option
             (export-ascii-test-catch-error
              (lambda ()
                (export-org
                 (parse-org
                  (export-ascii-test-source "#+OPTIONS: num:invalid" "* Title"))
                 :ascii)))))
      (with-soft-assertions
        (expect (export-ascii-test-render-heading-title
                 view preparation numbers 72 nil)
                :to-equal "Title")
        (expect (typep invalid-option 'org-export-unsupported-syntax) :to-be t)
        (when (typep invalid-option 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-backend invalid-option)
                  :to-be :ascii)
          (expect (org-export-unsupported-syntax-feature invalid-option)
                  :to-be :invalid-option)))))

  (it "indents non-paragraph list item bodies under their marker"
    (let* ((document (parse-org "value"))
           (text (make-node :plain-text :begin 0 :end 5))
           (unmarked-text (make-node :plain-text :begin 0 :end 5))
           (item (make-node :item :properties '(:bullet "- ")
                            :children (list text)))
           (unmarked-item (make-node :item :children (list unmarked-text)))
           (list (make-node :plain-list :properties '(:type :unordered)
                            :children (list item unmarked-item)))
           (root (make-node :org-data :children (list list)))
           (item-view
             (export-ascii-test-view
              item document (list (export-ascii-test-view text document))))
           (unmarked-item-view
             (export-ascii-test-view
              unmarked-item document
              (list (export-ascii-test-view unmarked-text document))))
           (preparation (export-ascii-test-preparation document nil)))
      (setf (document-root document) root)
      (with-soft-assertions
        (expect (export-ascii-test-render-list-item item-view preparation 1)
                :to-equal (format nil "- ~%  value"))
        (expect (export-ascii-test-render-list-item unmarked-item-view
                                                   preparation 1)
                :to-equal (format nil "- ~%  value")))))

  (it "renders heading depth, metadata flags, inline objects, and ASCII blocks"
    (let* ((source
             (export-ascii-test-source
              "#+OPTIONS: toc:nil num:2 pri:t \\n:t"
              "#+TODO: TODO | DONE"
              "* TODO [#A] Top"
              "** Child"
              "*** Third"
              "**** Deep"
              "***** deeper"
              "Paragraph *bold* /italic/ +strike+ _under_ ^sup _{sub} ~code~ =verbatim= \\alpha [2/5] <2026-10-12 Mon>\\\\"
              "next"
              "#+BEGIN_EXPORT ascii"
              "literal ascii"
              "#+END_EXPORT"
              "#+BEGIN_EXPORT html"
              "hidden html"
              "#+END_EXPORT"
              "@@ascii:snippet@@ @@html:hidden@@"
              "#+BEGIN_CENTER"
              "centered"
              "#+END_CENTER"
              "#+BEGIN_QUOTE"
              "quoted"
              "#+END_QUOTE"
              "#+BEGIN_VERSE"
              "verse"
              "#+END_VERSE"
              ":drawer:"
              "inside"
              ":end:"
              "#+BEGIN_EXAMPLE"
              "example"
              "#+END_EXAMPLE"
              ": fixed width"))
           (document (parse-org source))
           (output (export-org document :ascii))
           (expected
             (export-ascii-test-lines
              "1 TODO (##65) Top"
              "================="
              ""
              "1.1 Child"
              "~~~~~~~~~"
              ""
              "Third"
              "-----"
              ""
              "* Deep"
              ""
              "  + deeper"
              ""
              "    Paragraph *bold* /italic/ strike under ^sup _{sub} `' `' α [2/5]"
              "    <2026-10-12 Mon>"
              "    next"
              ""
              "    literal ascii"
              ""
              ""
              "    snippet"
              ""
              "    centered"
              ""
              "          quoted"
              ""
              "          verse"
              ""
              ""
              "    inside"
              ""
              "    ,----"
              "    | example"
              "    `----"
              ""
              "    ,----"
              "    | fixed width"
              "    `----"
              "")))
      (with-soft-assertions
        (expect (node-property
                 (export-ascii-test-find-node
                  (document-root document) :headline)
                 :priority)
                :to-be 65)
        (expect output :to-equal expected)
        (expect (search "hidden html" output) :to-be nil)
        (expect (search "snippet" output) :not :to-be nil))))

  (it "aligns a Unicode table with the shared width and cell helpers"
    (let* ((source
             (export-ascii-test-source
              "| Name | Value |"
              "|------+-------|"
              "| 日本 | é |"))
           (document (parse-org source))
           (table (export-ascii-test-find-node (document-root document) :table))
           (rows (remove-if-not
                  (lambda (row)
                    (eq (node-property row :type) :standard))
                  (node-children table)))
           (output (export-org document :ascii))
           (expected
             (export-ascii-test-source
              "| Name | Value |"
              "+------+-------+"
              "| 日本 | é     |")))
      (multiple-value-bind (widths alignments)
          (org-kit::table-column-layout
           rows 2 72
           (lambda (row)
             (mapcar (lambda (cell)
                       (or (node-property cell :value) ""))
                     (node-children row))))
        (with-soft-assertions
          (expect (coerce widths 'list) :to-equal '(4 5))
          (expect (coerce alignments 'list) :to-equal '(:left :left))
          (expect (org-kit::table-string-width "日本") :to-be 4)
          (expect (org-kit::table-string-width "é") :to-be 1)
          (expect (org-kit::table-format-cell "日本" (aref widths 0) :left)
                  :to-equal " 日本 ")
          (expect (org-kit::table-format-cell "é" (aref widths 1) :left)
                  :to-equal " é     ")
          (expect output :to-equal expected)))))

  (it "renders blank cells and table.el source tables"
    (let* ((blank-source
             (export-ascii-test-source
              "| alpha | |"
              "|-------+---|"
              "| value | x |"))
           (legacy-source
             (export-ascii-test-source
              "+---+---+"
              "| 1 | 2 |"
              "+---+---+"
              "#+TBLFM: $2=$1+1"))
           (blank-document (parse-org blank-source))
           (legacy-document (parse-org legacy-source))
           (blank-output (export-org blank-document :ascii))
           (legacy-output (export-org legacy-document :ascii))
           (legacy-table
             (export-ascii-test-find-node (document-root legacy-document) :table)))
      (with-soft-assertions
        (expect blank-output
                :to-equal
                (export-ascii-test-source
                 "| alpha |   |"
                 "+-------+---+"
                 "| value | x |"))
        (expect (node-property legacy-table :type) :to-be :table.el)
        (expect legacy-output
                :to-equal
                (export-ascii-test-source
                 "+---+---+"
                 "| 1 | 2 |"
                 "+---+---+")))))

  (it "honors caller metadata without changing the document"
    (let* ((source
             (export-ascii-test-source
              "#+TITLE: Document title"
              "#+AUTHOR: Document author"
              "#+DATE: 2026-10-14"
              "Body"))
           (document (parse-org source))
           (before (export-ascii-test-document-snapshot document))
           (output
             (export-org document :ascii
                         :context (make-export-context
                                   :title "Caller title"
                                   :author "Caller author"
                                   :date "2030-01-02"))))
      (with-soft-assertions
        (expect (search "CALLER TITLE" output) :not :to-be nil)
        (expect (search "DOCUMENT TITLE" output) :to-be nil)
        (expect (search "Caller author" output) :not :to-be nil)
        (expect (search "Document author" output) :to-be nil)
        (expect (search "2030-01-02" output) :not :to-be nil)
        (expect (search "2026-10-14" output) :to-be nil)
        (expect (export-ascii-test-document-snapshot document) :to-equal before))))

  (it "checks the ASCII table output budget before allocating padded rows"
    (let* ((cell-text (make-string 500 :initial-element #\x))
           (source
             (with-output-to-string (stream)
               (format stream "#+OPTIONS: toc:nil num:nil~%")
               (format stream "|<999999999999999999999999>|~A|~%"
                       cell-text)
               (format stream "|---+---|~%")
               (loop repeat 20
                     do (format stream "|~A|~A|~%"
                                 cell-text cell-text))))
           (document
             (parse-org source
                        :limits (make-limits :max-output-length 1000)))
           (preparation (org-kit::export-prepare document :ascii))
           (input-length (length source))
           (condition
             (handler-case
                 (progn (org-kit::export-ascii-render-preparation preparation) nil)
               (org-resource-limit-error (condition) condition))))
      (with-soft-assertions
        (expect (> input-length 20000) :to-be t)
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when (typep condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name condition)
                  :to-be :max-output-length)
          (expect (org-resource-limit-error-limit condition) :to-be 1000)
          (expect (org-resource-limit-error-actual condition) :to-be 1008)))))

  (it-sequential "limits long ASCII table cookies before padded output allocation"
    (let* ((cookie (make-string 32 :initial-element #\9))
           (source
             (format nil "| <~A> |~%~{~A~%~}"
                     cookie (make-list 20 :initial-element "| x |")))
           (document
             (parse-org source
                        :limits (make-limits :max-output-length 1000)))
           (public-condition
             (handler-case
                 (progn (export-org document :ascii) nil)
               (org-resource-limit-error (condition) condition))))
      (with-soft-assertions
        (expect (search (format nil "<~A>" cookie) source)
                :to-be-truthy)
        (expect (> (length source) 100) :to-be t)
        (expect (= (count #\Newline source) 21) :to-be t)
        (expect (typep public-condition 'org-resource-limit-error)
                :to-be t)
        (when (typep public-condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name public-condition)
                  :to-be :max-output-length)
          (expect (org-resource-limit-error-limit public-condition)
                  :to-be 1000)))))

  (it-sequential "preflights long ASCII table cookies before writing padded rows"
    (let ((result
            (export-ascii-test-table-cookie-preflight-result)))
      (with-soft-assertions
        (expect (getf result :cookie-present-p) :to-be-truthy)
        (expect (> (getf result :source-length) 100) :to-be t)
        (expect (getf result :newline-count) :to-be 21)
        (expect (getf result :table-view-p) :to-be t)
        (expect (getf result :condition-p) :to-be t)
        (expect (getf result :limit-name) :to-be :max-output-length)
        (expect (getf result :limit) :to-be 1000)
        (expect (getf result :row-writer-called-p) :to-be nil)
        (expect (getf result :fill-pointer) :to-be 0)
        (expect (<= (getf result :capacity) 1000) :to-be t)
        (expect (= (getf result :capacity)
                   (getf result :initial-capacity))
                :to-be t))))

  (it "preflights all ASCII table rows before writing padded output"
    (let* ((source
             (with-output-to-string (stream)
               (format stream "| <45> |~%")
               (loop repeat 20 do (format stream "| x |~%"))))
           (document
             (parse-org source :limits (make-limits :max-output-length 1000)))
           (condition
             (handler-case
                 (progn (export-org document :ascii) nil)
               (org-resource-limit-error (condition) condition))))
      (with-soft-assertions
        (expect (> (length source) 100) :to-be t)
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when (typep condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name condition)
                  :to-be :max-output-length)
          (expect (org-resource-limit-error-limit condition) :to-be 1000)
          (expect (org-resource-limit-error-actual condition) :to-be 1050)))))

  (it "includes the preceding view gap in the ASCII table preflight"
    (let* ((source
             (with-output-to-string (stream)
               (format stream "0123456789abc~%~%| <42> |~%")
               (loop repeat 20 do (format stream "| x |~%"))))
           (condition
             (handler-case
                 (progn
                   (export-org
                    (parse-org source
                               :limits (make-limits :max-output-length 1000))
                    :ascii)
                   nil)
               (org-resource-limit-error (condition) condition))))
      (with-soft-assertions
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when (typep condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name condition)
                  :to-be :max-output-length)
          (expect (org-resource-limit-error-limit condition) :to-be 1000)
          (expect (org-resource-limit-error-actual condition) :to-be 1002)))))

  (it "includes downstream indentation in the ASCII table preflight"
    (let* ((source
             (format nil "#+begin_quote~%| <20> |~%#+end_quote"))
           (document
             (parse-org source :limits (make-limits :max-output-length 29)))
           (condition
             (handler-case
                 (progn (export-org document :ascii) nil)
               (org-resource-limit-error (condition) condition))))
      (with-soft-assertions
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when (typep condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name condition)
                  :to-be :max-output-length)
          (expect (org-resource-limit-error-limit condition) :to-be 29)
          (expect (org-resource-limit-error-actual condition) :to-be 31)))))

  (it "reserves the final newline before preflighting ASCII table output"
    (labels ((find-table-view (views)
               (loop for view in views
                     for found =
                       (if (eq (node-type (org-kit::export-view-node view))
                               :table)
                           view
                           (find-table-view
                            (org-kit::export-view-children view)))
                     when found return found)))
      (let* ((source "| x |")
             (baseline (export-org (parse-org source) :ascii))
             (final-output-length (length baseline))
             (table-body-length (1- final-output-length))
             (document
               (parse-org source
                          :limits
                          (make-limits :max-output-length table-body-length)))
             (preparation (org-kit::export-prepare document :ascii))
             (table-view
               (find-table-view
                (org-kit::export-preparation-root-views preparation)))
             (public-condition
               (handler-case
                   (progn (export-org document :ascii) nil)
                 (org-resource-limit-error (condition) condition)))
             (output
               (export-ascii-test-output-state preparation :base-count 1))
             (table-output (org-kit::export-ascii-output-scratch output 0))
             (table-condition
               (handler-case
                   (progn
                     (org-kit::export-ascii-output-write-table
                      table-output table-view preparation)
                     nil)
                 (org-resource-limit-error (condition) condition)))
             (buffer (org-kit::export-ascii-output-output table-output)))
        (with-soft-assertions
          (expect (char= (char baseline (1- final-output-length)) #\Newline)
                  :to-be t)
          (expect (eq (node-type (org-kit::export-view-node table-view))
                      :table)
                  :to-be t)
          (expect (typep public-condition 'org-resource-limit-error) :to-be t)
          (expect (typep table-condition 'org-resource-limit-error) :to-be t)
          (when (typep public-condition 'org-resource-limit-error)
            (expect (org-resource-limit-error-limit-name public-condition)
                    :to-be :max-output-length)
            (expect (org-resource-limit-error-limit public-condition)
                    :to-be table-body-length)
            (expect (org-resource-limit-error-actual public-condition)
                    :to-be final-output-length))
          (when (typep table-condition 'org-resource-limit-error)
            (expect (org-resource-limit-error-limit-name table-condition)
                    :to-be :max-output-length)
            (expect (org-resource-limit-error-limit table-condition)
                    :to-be table-body-length)
            (expect (org-resource-limit-error-actual table-condition)
                    :to-be final-output-length))
          (expect (fill-pointer buffer) :to-be 0)
          (expect (array-total-size buffer) :to-be 0)))))

  (it "applies ASCII output limits to the final trimmed text"
    (let* ((source " x ")
           (boundary-document
             (parse-org source
                        :limits (make-limits :max-output-length 2)))
           (boundary-output (export-org boundary-document :ascii))
           (below-document
             (parse-org source
                        :limits (make-limits :max-output-length 1)))
           (below-condition
             (handler-case
                 (progn (export-org below-document :ascii) nil)
               (org-resource-limit-error (condition) condition))))
      (with-soft-assertions
        (expect boundary-output :to-equal (format nil "x~%"))
        (expect (typep below-condition 'org-resource-limit-error) :to-be t)
        (when (typep below-condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name below-condition)
                  :to-be :max-output-length)
          (expect (org-resource-limit-error-limit below-condition) :to-be 1)
          (expect (>= (org-resource-limit-error-actual below-condition) 2)
                  :to-be t)))))

  (it "trims formatted paragraph edges before applying the ASCII output limit"
    (let* ((source "  *bold*  ")
           (baseline (export-org (parse-org source) :ascii))
           (limit (length baseline))
           (at-limit
             (export-org
              (parse-org source :limits (make-limits :max-output-length limit))
              :ascii)))
      (with-soft-assertions
        (expect at-limit :to-equal baseline)
        (expect baseline :to-equal (format nil "*bold*~%")))))

  (it "trims formatted ASCII footnote edges before applying the output limit"
    (let* ((spaces (make-string 2000 :initial-element #\Space))
           (source
             (format nil
                     "#+OPTIONS: toc:nil num:nil~%Before [fn:x]~%~%[fn:x] ~A*note*~A"
                     spaces spaces))
           (baseline (export-org (parse-org source) :ascii))
           (limit (length baseline))
           (at-limit
             (export-org
              (parse-org source :limits (make-limits :max-output-length limit))
              :ascii)))
      (with-soft-assertions
        (expect at-limit :to-equal baseline)
        (expect (search "[1] *note*" baseline) :not :to-be nil))))

  (it "ignores discarded unsafe destinations in the ASCII output budget"
    (let* ((source "[[javascript:alert(1)][label]]")
           (boundary-document
             (parse-org source
                        :limits (make-limits :max-output-length 8)))
           (boundary-output (export-org boundary-document :ascii))
           (below-document
             (parse-org source
                        :limits (make-limits :max-output-length 7)))
           (below-condition
             (handler-case
                 (progn (export-org below-document :ascii) nil)
               (org-resource-limit-error (condition) condition))))
      (with-soft-assertions
        (expect boundary-output :to-equal (format nil "[label]~%"))
        (expect (typep below-condition 'org-resource-limit-error) :to-be t)
        (when (typep below-condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name below-condition)
                  :to-be :max-output-length)
          (expect (org-resource-limit-error-limit below-condition) :to-be 7)
          (expect (> (org-resource-limit-error-actual below-condition) 7)
                  :to-be t)))))

  (it "checks the expanded ASCII abbreviation before budgeting a discarded link"
    (let* ((source
             (format nil
                     "#+OPTIONS: toc:nil num:nil~%#+LINK: joined java%s:~%[[joined:script][l]]"))
           (at-limit
             (export-org
              (parse-org source :limits (make-limits :max-output-length 4))
              :ascii)))
      (expect at-limit :to-equal (format nil "[l]~%"))))

  (it-sequential "rejects an unsafe large ASCII abbreviation before copying its path"
    (let* ((payload (make-string (* 256 1024) :initial-element #\x))
           (source
             (format nil
                     "#+OPTIONS: toc:nil num:nil~%#+LINK: danger javascript:%s~%[[danger:~A][label]]"
                     payload))
           (document
             (parse-org source :limits (make-limits :max-output-length 8)))
           (input-length (length source)))
      (expect (export-org document :ascii) :to-equal (format nil "[label]~%"))
      (let* ((before (progn (sb-ext:gc :full t)
                            (sb-ext:get-bytes-consed)))
             (actual (export-org document :ascii))
             (allocated (- (sb-ext:get-bytes-consed) before)))
        (let* ((baseline-source
                 (format nil
                         "#+OPTIONS: toc:nil num:nil~%#+LINK: danger javascript:%s~%[[danger:~A][label]]"
                         "x"))
               (baseline-document
                 (parse-org baseline-source
                            :limits (make-limits :max-output-length 8)))
               (baseline-warmup
                 (let ((*export-baseline-current-test* nil))
                   (export-org baseline-document :ascii)))
               (baseline-before
                 (progn (sb-ext:gc :full t) (sb-ext:get-bytes-consed)))
               (baseline-actual
                 (let ((*export-baseline-current-test* nil))
                   (export-org baseline-document :ascii)))
               (baseline-allocated
                 (- (sb-ext:get-bytes-consed) baseline-before))
               (baseline-input-length (length baseline-source)))
          (declare (ignore baseline-warmup))
          (with-soft-assertions
            (expect actual :to-equal (format nil "[label]~%"))
            (expect (> input-length (* 256 1024)) :to-be t)
            (expect baseline-actual :to-equal (format nil "[label]~%"))
            (expect (< (- allocated baseline-allocated)
                       (floor (- input-length baseline-input-length) 2))
                    :to-be t))))))

  (it "checks an ASCII abbreviation without a placeholder before discarding it"
    (let* ((source
             (format nil
                     "#+OPTIONS: toc:nil num:nil~%#+LINK: bare https://docs.example/~%[[bare:topic][Bare]]"))
           (output (export-org (parse-org source) :ascii)))
      (expect (search "[Bare] <https://docs.example/topic>" output)
              :to-be-truthy)))

  (it "expands a double-colon ASCII abbreviation without retaining its separator"
    (let* ((source
             (format nil
                     "#+OPTIONS: toc:nil num:nil~%#+LINK: web https://docs.example/~%[[web::manual][Manual]]"))
           (output (export-org (parse-org source) :ascii)))
      (expect (search "[Manual] <https://docs.example/manual>" output)
              :to-be-truthy)))

  (it "renders relative file path forms through export-org"
    (let* ((source
             (format nil
                     "#+OPTIONS: toc:nil num:nil~%[[file:./chapter.org][Dot]]; [[file:../chapter.org][Parent]]; [[file:x][Short]]; [[file:][Empty]]"))
           (output (export-org (parse-org source) :ascii)))
      (expect output :to-equal
              (format nil "[Dot]; [Parent]; [Short]; [Empty]~%"))))

  (it "checks empty and safe ASCII abbreviation prefixes through export-org"
    (let* ((source
             (format nil
                     "#+OPTIONS: toc:nil num:nil~%#+LINK: empty %s~%#+LINK: safe https://docs.example/%s~%[[empty:javascript:alert][Bad]]; [[empty:chapter][Relative]]; [[safe:chapter][Remote]]"))
           (output (export-org (parse-org source) :ascii)))
      (expect output :to-equal
              (format nil
                      "[Bad]; [Relative]; [Remote]~%~%~%[Relative] <chapter>~%~%[Remote] <https://docs.example/chapter>~%"))))

  (it "keeps unsafe raw destinations as labels through export-org"
    (let* ((document (make-document "label"))
           (label (make-node :plain-text :begin 0 :end 5))
           (link
             (make-node :link
                        :properties '(:type "javascript" :path "alert(1)"
                                      :format :bracket
                                      :raw-link "javascript:alert(1)")
                        :children (list label)))
           (paragraph (make-node :paragraph :children (list link)))
           (section (make-node :section :children (list paragraph)))
           (root (make-node :org-data :children (list section))))
      (setf (document-root document) root)
      (expect (export-org document :ascii) :to-equal (format nil "[label]~%"))))

  (it "uses a raw ASCII link source when the programmatic node has no path"
    (let* ((document (make-document "label"))
           (label (make-node :plain-text :begin 0 :end 5))
           (link
             (make-node :link
                        :properties '(:type "https" :format :bracket
                                      :raw-link "https://docs.test/x")
                        :children (list label)))
           (paragraph (make-node :paragraph :children (list link)))
           (section (make-node :section :children (list paragraph)))
           (root (make-node :org-data :children (list section))))
      (setf (document-root document) root)
      (expect (export-org document :ascii)
              :to-equal (format nil "[label]~%~%~%[label] <https://docs.test/x>~%"))))

  (it "rejects an unsafe raw ASCII link source when the node has no path"
    (let* ((document (make-document "label"))
           (label (make-node :plain-text :begin 0 :end 5))
           (link
             (make-node :link
                        :properties '(:type "javascript" :format :bracket
                                      :raw-link "javascript:alert(1)")
                        :children (list label)))
           (paragraph (make-node :paragraph :children (list link)))
           (section (make-node :section :children (list paragraph)))
           (root (make-node :org-data :children (list section))))
      (setf (document-root document) root)
      (expect (export-org document :ascii) :to-equal (format nil "[label]~%"))))

  (it "exports attached plain-text nodes without source offsets"
    (let* ((document (make-document ""))
           (text-node (make-node :plain-text))
           (paragraph-node (make-node :paragraph :children (list text-node)))
           (root (make-node :org-data :children (list paragraph-node))))
      (setf (document-root document) root)
      (expect (export-org document :ascii) :to-equal (format nil "~%"))))

  (it "trims edge whitespace emitted by non-plain ASCII paragraph objects"
    (let* ((document (make-document ""))
           (inline-node
             (make-node :inline-src-block :properties '(:value "  body  ")))
           (paragraph-node
             (make-node :paragraph :children (list inline-node)))
           (root (make-node :org-data :children (list paragraph-node))))
      (setf (document-root document) root)
      (expect (export-org document :ascii) :to-equal (format nil "body~%"))))

  (it "trims an empty non-plain ASCII paragraph object"
    (let* ((document (make-document ""))
           (inline-node (make-node :inline-src-block :properties '(:value "")))
           (paragraph-node
             (make-node :paragraph :children (list inline-node)))
           (root (make-node :org-data :children (list paragraph-node))))
      (setf (document-root document) root)
      (expect (export-org document :ascii) :to-equal (format nil "~%"))))

  (it "substitutes tabs while measuring and writing ASCII table text"
    (let* ((document (parse-org ""))
           (preparation (export-ascii-test-preparation document nil))
           (output (export-ascii-test-output-state preparation))
           (hidden-output
             (export-ascii-test-output-state
              preparation :measure-only-p t :visible-controls-p nil))
           (text (format nil "a~Cb" #\Tab))
           (control (string (code-char 27))))
      (multiple-value-bind (display-width output-length)
          (org-kit::export-ascii-output-measure-text-range
           output text 0 (length text) :substitute-tab-p t)
        (multiple-value-bind (control-width control-length)
            (org-kit::export-ascii-output-measure-text-range
             output control 0 (length control))
          (multiple-value-bind (hidden-width hidden-length)
              (org-kit::export-ascii-output-measure-text-range
               hidden-output control 0 (length control))
            (org-kit::export-ascii-output-write-range
             output text 0 (length text) :substitute-tab-p t)
            (with-soft-assertions
              (expect display-width :to-be 3)
              (expect output-length :to-be 3)
              (expect control-width :to-be 4)
              (expect control-length :to-be 4)
              (expect hidden-length :to-be 1)
              (expect hidden-width :to-be
                      (org-kit::table-character-width (char control 0)))
              (expect (org-kit::export-ascii-output-string output)
                      :to-equal "a b")))))))

  (it "discards unsafe control and whitespace destinations before output budgeting"
    (let* ((esc (string (code-char 27)))
           (crlf (format nil "~C~C" #\Return #\Newline))
           (sources
             (list (format nil "[[https://safe.~Apath][l]]" esc)
                   (format nil "[[https://safe.~Apath][l]]" crlf)
                   "[[ https://safe.test][l]]"
                   (format nil "[[~Chttps://safe.test][l]]" #\Tab)
                   (format nil "[[~%https://safe.test][l]]")))
           (outputs
             (loop for source in sources
                   collect (export-org
                            (parse-org source
                                       :limits
                                       (make-limits :max-output-length 4))
                            :ascii))))
      (with-soft-assertions
        (expect outputs
                :to-equal (make-list (length sources)
                                     :initial-element (format nil "[l]~%"))))))

  (it-sequential "checks ASCII link output before allocating a large destination"
    (let* ((path (make-string (* 256 1024) :initial-element #\x))
           (source
             (concatenate 'string "[[https://budget.test/" path "]]"))
           (document
             (parse-org source
                        :limits (make-limits :max-output-length 128)))
           (preparation (org-kit::export-prepare document :ascii))
           (check-counts (make-hash-table))
           (input-length (length source))
           (before
             (progn
               (let ((org-kit::*export-output-check-counts* check-counts)
                     (org-kit::*export-current-export-phase* :body))
                 (handler-case
                     (progn
                       (org-kit::export-ascii-render-preparation preparation)
                       nil)
                   (org-resource-limit-error (condition) condition)))
               (sb-ext:gc :full t)
               (setf check-counts (make-hash-table))
               (sb-ext:get-bytes-consed)))
           (condition
             (let ((org-kit::*export-output-check-counts* check-counts)
                   (org-kit::*export-current-export-phase* :body))
               (handler-case
                   (progn
                     (org-kit::export-ascii-render-preparation preparation)
                     nil)
                 (org-resource-limit-error (condition) condition))))
           (allocated (- (sb-ext:get-bytes-consed) before)))
      (with-soft-assertions
        (expect (> input-length (* 256 1024)) :to-be t)
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when (typep condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name condition)
                  :to-be :max-output-length)
          (expect (org-resource-limit-error-limit condition) :to-be 128)
          (expect (> (org-resource-limit-error-actual condition) 128)
                  :to-be t))
        (expect (< allocated (floor input-length 2)) :to-be t))))

  (it "uses explicit metadata defaults without consulting ambient values"
    (let* ((empty-output
             (export-org (parse-org (export-ascii-test-source "Body")) :ascii))
           (title-only
             (export-org
              (parse-org (export-ascii-test-source "#+TITLE: Only title" "Body"))
              :ascii))
           (author-only
             (export-org
              (parse-org (export-ascii-test-source "#+AUTHOR: Only author" "Body"))
              :ascii))
           (date-only
             (export-org
              (parse-org (export-ascii-test-source "#+DATE: 2030-01-02" "Body"))
              :ascii))
           (author-and-date
             (export-org
              (parse-org
               (export-ascii-test-source
                "#+AUTHOR: Explicit author"
                "#+DATE: 2030-01-02"
                "Body"))
              :ascii))
           (disabled
             (export-org
              (parse-org
               (export-ascii-test-source
                "#+TITLE: Hidden title"
                "#+AUTHOR: Hidden author"
                "#+DATE: 2026-10-14"
                "#+OPTIONS: title:nil author:nil date:nil"
                "Body"))
              :ascii)))
      (with-soft-assertions
        (expect empty-output :to-equal (export-ascii-test-source "Body"))
        (expect (search "ONLY TITLE" title-only) :not :to-be nil)
        (expect (search "Only author" author-only) :not :to-be nil)
        (expect (search "2030-01-02" date-only) :not :to-be nil)
        (expect (search "Explicit author" author-and-date) :not :to-be nil)
        (expect (search "2030-01-02" author-and-date) :not :to-be nil)
        (expect (search "Hidden title" disabled) :to-be nil)
        (expect (search "Hidden author" disabled) :to-be nil)
        (expect (search "2026-10-14" disabled) :to-be nil)
        (expect (search "Body" disabled) :not :to-be nil))))

  (it "preserves explicit paragraph breaks and wraps at display width"
    (let* ((preserved
             (export-org
              (parse-org
               (export-ascii-test-source
                "#+OPTIONS: \\n:t"
                "First line"
                "Second line"))
              :ascii))
           (long-word (make-string 70 :initial-element #\x))
           (wrapped
             (export-org
              (parse-org
               (export-ascii-test-source
                (format nil "short ~A tail" long-word)))
              :ascii))
           (lines
             (with-input-from-string (stream wrapped)
               (loop for line = (read-line stream nil nil)
                     while line collect line))))
      (with-soft-assertions
        (expect preserved
                :to-equal (export-ascii-test-source "First line" "Second line"))
        (expect (every (lambda (line)
                         (<= (org-kit::table-string-width line) 72))
                       lines)
                :to-be t)
        (expect (search long-word wrapped) :not :to-be nil)
        (expect (search "tail" wrapped) :not :to-be nil))))

  (it "prints link labels, safe destinations, and one note per link"
    (let* ((source
             (export-ascii-test-source
              "#+OPTIONS: toc:nil"
              "* Target"
              ":PROPERTIES:"
              ":CUSTOM_ID: target"
              ":END:"
              "* Links"
              "[[https://repeat.test][Repeat]] and [[https://repeat.test][Repeat]]."
              "[[https://bare.test]] and [[javascript:alert(1)][Unsafe]]."
              "[[#target][Internal]] and [[#target]]."))
           (output (export-org (parse-org source) :ascii))
           (note "[Repeat] <https://repeat.test>")
           (first-note (search note output))
           (second-note (and first-note
                             (search note output :start2 (1+ first-note)))))
      (with-soft-assertions
        (expect first-note :not :to-be nil)
        (expect second-note :to-be nil)
        (expect (search "<https://bare.test>" output) :not :to-be nil)
        (expect (search "[Unsafe]" output) :not :to-be nil)
        (expect (search "[Unsafe] <javascript:" output) :to-be nil)
        (expect (search "[Internal] See section 1" output) :not :to-be nil)
        (expect (search "Target" output) :not :to-be nil))))

  (it "resolves parser-generated fuzzy links through the public API"
    (let* ((document
             (parse-org
              (export-ascii-test-source
               "#+OPTIONS: toc:nil num:nil"
               "* Target"
               "* Links"
               "[[Target][Internal]]")))
           (link (first (export-ascii-test-link-nodes document)))
           (output (export-org document :ascii))
           (expected
             (export-ascii-test-source
              "Target"
              "======"
              ""
              ""
              "Links"
              "====="
              ""
              "  [Internal]"
              ""
              ""
              "[Internal] See section Target")))
      (with-soft-assertions
        (expect (list (node-property link :type) (node-property link :path))
                :to-equal '("fuzzy" "Target"))
        (expect output :to-equal expected))))

  (it "rejects unsafe link destinations and keeps relative colon paths safe"
    (let* ((source
             (export-ascii-test-source
              "#+OPTIONS: toc:nil num:nil"
              "* Links"
              "[[https://safe.test/path\\part][Backslash]]; [[file:private.txt][File]]; [[assets/topic:v2][Colon path]]."))
           (output (export-org (parse-org source) :ascii))
           (expected
             (export-ascii-test-lines
              "Links"
              "====="
              ""
              "  [Backslash]; [File]; [Colon path]."
              ""
              ""
              "[Colon path] <assets/topic:v2>"
              "")))
      (expect output :to-equal expected)))

  (it "applies link policy at whitespace, scheme, and abbreviation boundaries"
    (let* ((source
             (export-ascii-test-source
              "#+OPTIONS: toc:nil num:nil"
              "#+LINK: safe https://docs.example/%s"
              "#+LINK: unsafe javascript:%s"
              "#+LINK: badpath https://docs.example\\%s"
              "[[ https://space.test/path][Leading]]; [[//cdn.test/path][Protocol relative]]; [[ftp://ftp.test/file][FTP]]; [[unknown:resource][Unknown]]; [[HTTPS://upper.test/path][Upper HTTPS]]; [[safe:chapter][Safe expansion]]; [[unsafe:payload][Unsafe expansion]]; [[/single-leading/path][Single slash]]; [[mailto:user@example.test][Mailto]]; [[news:group][News]]; [[badpath:chapter][Backslash expansion]]."))
           (document (parse-org source))
           (links (export-ascii-test-link-nodes document))
           (output (export-org document :ascii))
           (labels '("Leading" "Protocol relative" "FTP" "Unknown"
                     "Upper HTTPS" "Safe expansion" "Unsafe expansion"
                     "Single slash" "Mailto" "News" "Backslash expansion"))
           (unsafe-labels '("Leading" "Protocol relative" "FTP" "Unknown"
                            "Unsafe expansion" "News" "Backslash expansion")))
      (with-soft-assertions
        (expect (length links) :to-be 11)
        (expect (mapcar (lambda (label)
                          (not (null (search (format nil "[~A]" label) output))))
                        labels)
                :to-equal (make-list 11 :initial-element t))
        (expect (search "[Upper HTTPS] <HTTPS://upper.test/path>" output)
                :not :to-be nil)
        (expect (search "[Safe expansion] <https://docs.example/chapter>" output)
                :not :to-be nil)
        (expect (search "[Single slash] </single-leading/path>" output)
                :not :to-be nil)
        (expect (search "[Mailto] <mailto:user@example.test>" output)
                :not :to-be nil)
        (expect (mapcar (lambda (label)
                          (search (format nil "[~A] <" label) output))
                        unsafe-labels)
                :to-equal (make-list 7 :initial-element nil)))))

  (it "deduplicates notes for equal direct and abbreviated destinations"
    (let* ((source
             (export-ascii-test-source
              "#+OPTIONS: toc:nil num:nil"
              "#+LINK: bare https://bare.test/%s"
              "[[bare:path][Bare link]] and [[https://bare.test/path][Bare link]]."))
           (document (parse-org source))
           (links (export-ascii-test-link-nodes document))
           (output (export-org document :ascii))
           (note "[Bare link] <https://bare.test/path>")
           (note-position (search note output))
           (note-second-position
             (and note-position
                  (search note output :start2 (1+ note-position)))))
      (with-soft-assertions
        (expect (length links) :to-be 2)
        (expect (search "[Bare link] and [Bare link]." output)
                :not :to-be nil)
        (expect note-position :not :to-be nil)
        (expect note-second-position :to-be nil))))

  (it "rejects multiline link destinations after bracket normalization"
    (let* ((source
             (export-ascii-test-source
              (format nil "[[https://safe.test/path~%continued][Multiline]]")))
           (document (parse-org source))
           (links (export-ascii-test-link-nodes document))
           (link (first links))
           (link-source
             (and link
                  (subseq source (node-begin link) (node-end link))))
           (output (export-org document :ascii)))
      (with-soft-assertions
        (expect (length links) :to-be 1)
        (expect (and link-source
                     (search (string #\Newline) link-source))
                :not :to-be nil)
        (expect (search "[Multiline]" output) :not :to-be nil)
        (expect (search "[Multiline] <" output) :to-be nil))))

  (it "renders unresolved local links as labels without notes"
    (let* ((source
             (export-ascii-test-source
              "#+OPTIONS: toc:nil num:nil"
              "[[#not-exported][Anchor]]"))
           (document (parse-org source))
           (links (export-ascii-test-link-nodes document))
           (output (export-org document :ascii)))
      (with-soft-assertions
        (expect (length links) :to-be 1)
        (expect output :to-equal (export-ascii-test-source "[Anchor]")))))

  (it "extracts angle and plain link destinations from source spans"
    (let* ((source
             (export-ascii-test-source
              "<https://angle.test/path> https://plain.test/path"))
           (document (parse-org source))
           (links (export-ascii-test-link-nodes document))
           (output (export-org document :ascii)))
      (with-soft-assertions
        (expect (mapcar (lambda (link) (node-property link :format)) links)
                :to-equal '(:angle :plain))
        (expect output
                :to-equal
                (export-ascii-test-source
                 "<https://angle.test/path> <https://plain.test/path>")))))

  (it "renders unresolved custom-id and unsafe links without descriptions"
    (let* ((source
             (export-ascii-test-source
              "[[#not-exported]] [[javascript:alert(1)]]"))
           (output (export-org (parse-org source) :ascii)))
      (expect output
              :to-equal
              (export-ascii-test-source "not-exported javascript:alert(1)"))))

  (it "separates multiple named footnote definitions"
    (let* ((source
             (export-ascii-test-source
              "#+OPTIONS: toc:nil num:nil"
              "* First section"
              "Reference [fn:first]."
              "[fn:first] first note."
              "* Second section"
              "Reference [fn:second]."
              "[fn:second] second note."))
           (output (export-org (parse-org source) :ascii))
           (expected
             (export-ascii-test-lines
              "First section"
              "============="
              ""
              "  Reference [1]."
              ""
              ""
              "Second section"
              "=============="
              ""
              "  Reference [2]."
              ""
              ""
              ""
              "Footnotes"
              "_________"
              ""
              "[1] first note."
              ""
              "[2] second note."
              "")))
      (expect output :to-equal expected)))

  (it "resolves included custom-id links and collects a root-level link note"
    (let* ((source
             (export-ascii-test-source
              "#+OPTIONS: toc:nil num:nil"
              "[[https://root.test/path][Root link]]."
              "#+INCLUDE: \"included/fragment.org\""))
           (document (parse-org source :source-name "root.org"))
           (calls nil)
           (resolver
             (lambda (path parent-source)
               (push (list path parent-source) calls)
               (if (string= path "included/fragment.org")
                   (export-ascii-test-source
                    "* Included target"
                    ":PROPERTIES:"
                    ":CUSTOM_ID: included-target"
                    ":END:"
                    "[[#included-target][Anchor]].")
                   (error "Unexpected ASCII export include: ~A" path))))
           (output
             (export-org document :ascii :include-resolver resolver))
           (root-note "[Root link] <https://root.test/path>")
           (root-note-position (search root-note output))
           (root-note-second-position
             (and root-note-position
                  (search root-note output :start2 (1+ root-note-position))))
           (included-note "[Anchor] See section Included target")
           (included-note-position (search included-note output))
           (included-note-second-position
             (and included-note-position
                  (search included-note output :start2
                          (1+ included-note-position)))))
      (with-soft-assertions
        (expect (nreverse calls)
                :to-equal '(("included/fragment.org" "root.org")))
        (expect root-note-position :not :to-be nil)
        (expect root-note-second-position :to-be nil)
        (expect included-note-position :not :to-be nil)
        (expect included-note-second-position :to-be nil))))

  (it "renders a root-level described URL link and its note"
    (let* ((source
             (export-ascii-test-source "[[https://bare.test][Bare link]]"))
           (output (export-org (parse-org source) :ascii))
           (expected
             (export-ascii-test-source
              "[Bare link]"
              ""
              ""
              "[Bare link] <https://bare.test>")))
      (expect output :to-equal expected)))

  (it "renders radio links without destination notes"
    (let* ((source
             (export-ascii-test-source "<<<radio target>>> radio target."))
           (document (parse-org source))
           (link (export-ascii-test-find-node (document-root document) :link))
           (output (export-org document :ascii)))
      (with-soft-assertions
        (expect (and link (node-property link :type)) :to-equal "radio")
        (expect output :to-equal (export-ascii-test-source "radio target."))
        (expect (search "[radio target] <" output) :to-be nil))))

  (it "uses the shared unsupported-renderer fallback"
    (let* ((preparation
             (org-kit::export-prepare (parse-org "Body") :ascii))
           (condition
             (export-ascii-test-catch-error
              (lambda ()
                (org-kit::export-render-preparation preparation :unregistered)))))
      (with-soft-assertions
        (expect (typep condition 'org-export-error) :to-be t)
        (when (typep condition 'org-export-error)
          (expect (org-export-error-backend condition) :to-be :unregistered)
          (expect (org-export-error-reason condition) :to-be :unsupported-backend)))))

  (it "keeps Babel disabled while exporting source text"
    (let* ((source
             (export-ascii-test-source
              "#+BEGIN_SRC emacs-lisp :eval yes"
              "(error \"Babel must remain disabled\")"
              "#+END_SRC"))
           (output (export-org (parse-org source) :ascii)))
      (with-soft-assertions
        (expect (search "Babel must remain disabled" output)
                :not :to-be nil))))

  (it "renders anonymous inline footnotes at their reference point"
    (let* ((source
             (export-ascii-test-source "Before [fn::inline note] after."))
           (output (export-org (parse-org source) :ascii)))
      (expect output
              :to-equal (export-ascii-test-source "Before inline note after."))))

  (it "signals shared preparation errors for unsupported syntax and footnotes"
    (let* ((diary
             (export-ascii-test-catch-error
              (lambda ()
                (export-org (parse-org "%%(org-calendar-holiday)\n") :ascii))))
           (footnote
             (export-ascii-test-catch-error
              (lambda ()
                (export-org (parse-org "Missing [fn:missing].\n") :ascii)))))
      (with-soft-assertions
        (expect (typep diary 'org-export-unsupported-syntax) :to-be t)
        (when (typep diary 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-backend diary) :to-be :ascii)
          (expect (org-export-unsupported-syntax-feature diary)
                  :to-be :diary-sexp))
        (expect (typep footnote 'org-export-error) :to-be t)
        (when (typep footnote 'org-export-error)
          (expect (org-export-error-backend footnote) :to-be :ascii)
          (expect (org-export-error-reason footnote) :to-be :unresolved-footnote)))))

  (it "spells C0 and C1 controls visibly across ASCII output paths"
    (let* ((source
             (format nil
                     "#+OPTIONS: toc:nil num:nil~%Body~ABody~A~%| x~A | y~A |~%|---+---|~%#+BEGIN_SRC text~%block~A~%#+END_SRC~%"
                     (code-char #x1b) (code-char #x07)
                     (code-char #x7f) (code-char #x80)
                     (code-char #x9f)))
           (document (parse-org source))
           (table (export-ascii-test-find-node (document-root document) :table))
           (context
             (make-export-context
              :title (format nil "metadata~Avalue" (code-char #x00))))
           (output (export-org document :ascii :context context))
           (folded-output (string-downcase output))
           (raw-control-codes
             (loop for character across output
                   for code = (char-code character)
                   when (or (and (< code 32) (/= code 10))
                            (<= 127 code 159))
                     collect code)))
      (with-soft-assertions
        (expect (node-property table :type) :to-be :org)
        (expect (export-ascii-test-find-node (document-root document) :src-block)
                :not :to-be nil)
        (expect raw-control-codes :to-equal nil)
        (expect (mapcar (lambda (spelling)
                          (not (null (search spelling folded-output))))
                        '("\\x00" "\\x07" "\\x1b" "\\x7f"
                          "\\u0080" "\\u009f"))
                :to-equal '(t t t t t t)))))

  (it "preserves tabs in ASCII source blocks"
    (let* ((source
             (format nil
                     "#+OPTIONS: toc:nil num:nil~%#+BEGIN_SRC make~%target:~%~Ccommand~%#+END_SRC~%"
                     #\Tab))
           (document (parse-org source))
           (output (export-org document :ascii)))
      (with-soft-assertions
        (expect (find #\Tab output) :not :to-be nil)
        (expect (search "\\x09" output) :to-be nil))))

  (it "preserves source-block carriage returns during parsing and spells them visibly"
    (let* ((source
             (format nil
                     "#+OPTIONS: toc:nil num:nil~%#+BEGIN_SRC text~%left~Cright~%#+END_SRC~%"
                     #\Return))
           (document (parse-org source))
           (block (export-ascii-test-find-node (document-root document) :src-block))
           (value (node-property block :value))
           (output (export-org document :ascii)))
      (with-soft-assertions
        (expect (find #\Return value) :not :to-be nil)
        (expect (search "\\x0D" output) :not :to-be nil)
        (expect (find #\Return output) :to-be nil))))

  (it "reuses stable output without mutating source nodes"
    (let* ((source
             (export-ascii-test-source
              "#+OPTIONS: toc:nil num:nil"
              "* Heading :tag:"
              "Paragraph with *bold* and /italic/."
              "- item"))
           (document (parse-org source))
           (before (export-ascii-test-document-snapshot document))
           (first-output (export-org document :ascii))
           (second-output (export-org document :ascii)))
      (with-soft-assertions
        (expect first-output :to-equal second-output)
        (expect (export-ascii-test-document-snapshot document) :to-equal before))))

  (it "covers buffered output, visible widths, and bracket path escapes"
    (let* ((document (parse-org "body"))
           (preparation (export-ascii-test-preparation document nil))
           (output (export-ascii-test-output-state preparation))
           (trimmed (export-ascii-test-output-state preparation))
           (empty-trimmed
             (export-ascii-test-output-state preparation))
           (trimmed-range
             (export-ascii-test-output-state preparation))
           (centered (export-ascii-test-output-state preparation))
           (path-output (export-ascii-test-output-state preparation))
           (trailing-space-path-output
             (export-ascii-test-output-state preparation))
           (path (concatenate 'string "  a" (string #\Newline)
                              "   b\\[c\\z  "))
           (trailing-space-path
             (concatenate 'string "a" (string #\Newline) "  "))
           (controls (concatenate 'string (string (code-char #x85))
                                  (string (code-char #x7f)))))
      (setf (org-kit::export-ascii-output-indent output) 2)
      (org-kit::export-ascii-output-write-char output #\A)
      (org-kit::export-ascii-output-write-char output #\Newline)
      (org-kit::export-ascii-output-write-repeat output #\Space 1)
      (org-kit::export-ascii-output-write-string
       trimmed (format nil "x~%~%"))
      (org-kit::export-ascii-output-trim-newlines trimmed)
      (org-kit::export-ascii-output-trim-newlines empty-trimmed)
      (org-kit::export-ascii-output-write-trimmed-newlines
       trimmed-range (format nil "y~%~%"))
      (org-kit::export-ascii-output-write-centered-buffer centered "title" 10)
      (org-kit::export-ascii-output-write-bracket-path-range
       path-output path 0 (length path))
      (org-kit::export-ascii-output-write-bracket-path-range
       trailing-space-path-output trailing-space-path 0
       (length trailing-space-path))
      (with-soft-assertions
        (expect (org-kit::export-ascii-output-string output)
                :to-equal (format nil "  A~%   "))
        (expect (org-kit::export-ascii-output-range-width
                 controls 0 (length controls))
                :to-be 10)
        (expect (org-kit::export-ascii-output-string trimmed) :to-equal "x")
        (expect (org-kit::export-ascii-output-string empty-trimmed) :to-equal "")
        (expect (org-kit::export-ascii-output-string trimmed-range) :to-equal "y")
        (expect (org-kit::export-ascii-output-string centered)
                :to-equal "  title")
        (expect (org-kit::export-ascii-output-string path-output)
                :to-equal "  a b[c\\z  ")
        (expect (org-kit::export-ascii-output-string trailing-space-path-output)
                :to-equal "a "))))

  (it "covers table alignment, raw link paths, abbreviations, and tagged headings"
    (let* ((source-document (make-document "x"))
           (text-node (make-node :plain-text :begin 0 :end 1))
           (plain-cell-node (make-node :table-cell :children (list text-node)))
           (root (make-node :org-data :children (list plain-cell-node)))
           (document (parse-org "body"))
           (preparation (export-ascii-test-preparation document nil))
           (output (export-ascii-test-output-state preparation))
           (plain-cell
             (export-ascii-test-view
              plain-cell-node source-document
              (list (export-ascii-test-view text-node source-document))))
           (empty-cell (export-ascii-test-view (make-node :table-cell) document))
           (nontext-child-cell
             (export-ascii-test-view
              (make-node :table-cell) document
              (list (export-ascii-test-view (make-node :entity) document))))
           (nontext-child-measurement
             (multiple-value-list
              (org-kit::export-ascii-output-measure-table-cell
               output nontext-child-cell preparation)))
           (empty-text-child-cell
             (export-ascii-test-view
              (make-node :table-cell) document
              (list (export-ascii-test-view
                     (make-node :plain-text) document))))
           (empty-text-child-measurement
             (multiple-value-list
              (org-kit::export-ascii-output-measure-table-cell
               output empty-text-child-cell preparation)))
           (right-cell-node (make-node :table-cell :properties '(:value "<r>")))
           (center-cell-node (make-node :table-cell :properties '(:value "<c>")))
           (right-row
             (export-ascii-test-view
              (make-node :table-row :properties '(:type :standard))
              document (list (export-ascii-test-view right-cell-node document))))
           (center-row
             (export-ascii-test-view
              (make-node :table-row :properties '(:type :standard))
              document (list (export-ascii-test-view center-cell-node document))))
           (rule-row
             (export-ascii-test-view
              (make-node :table-row :properties '(:type :rule)) document))
           (cell-node (make-node :table-cell :properties '(:value "x")))
           (cell (export-ascii-test-view cell-node document))
           (center-output
             (export-ascii-test-output-state preparation))
           (right-output
             (export-ascii-test-output-state preparation))
           (link-document (parse-org ""))
           (link-preparation
             (export-ascii-test-preparation link-document nil))
           (link-output
             (export-ascii-test-output-state link-preparation))
           (link-node
             (make-node :link
                        :properties '(:type "fuzzy" :path "web::manual"
                                      :format :bracket :raw-link "web::manual")))
           (bracket-link (export-ascii-test-view link-node link-document))
           (single-colon-link-node
             (make-node :link
                        :properties '(:type "fuzzy" :path "web:manual"
                                      :format :bracket :raw-link "web:manual")))
           (single-colon-link
             (export-ascii-test-view single-colon-link-node link-document))
           (plain-link-node
             (make-node :link
                        :properties '(:type "fuzzy" :path "web::manual"
                                      :format :plain :raw-link "web::manual")))
           (plain-link (export-ascii-test-view plain-link-node link-document))
           (tagged (export-org (parse-org "* Tagged :one:two:") :ascii)))
      (setf (document-root source-document) root
            (getf (document-settings link-document) :link-abbrevs)
            '(("web" . "https://docs.example/")))
      (multiple-value-bind (widths alignments)
          (org-kit::export-ascii-output-table-layout
           output (list rule-row right-row center-row) 1 preparation)
        (declare (ignore widths))
        (org-kit::export-ascii-output-write-table-cell
         center-output cell 3 :center preparation)
        (org-kit::export-ascii-output-write-table-cell
         right-output cell 3 :right preparation)
        (multiple-value-bind (bracket-path ignored-safe)
            (org-kit::export-ascii-output-link-path
             link-output bracket-link link-preparation)
          (declare (ignore ignored-safe))
          (with-soft-assertions
            (expect (org-kit::export-ascii-output-cell-cookie-value plain-cell)
                    :to-equal "x")
            (expect (org-kit::export-ascii-output-cell-cookie-value empty-cell)
                    :to-be nil)
            (expect (org-kit::export-ascii-output-cell-cookie-value
                     nontext-child-cell)
                    :to-be nil)
            (expect nontext-child-measurement :to-equal '(0 0))
            (expect (org-kit::export-ascii-output-cell-cookie-value
                     empty-text-child-cell)
                    :to-be nil)
            (expect empty-text-child-measurement :to-equal '(0 0))
            (expect (org-kit::export-ascii-destination-source-safe-p "path" 0 4)
                    :to-be t)
            (expect (org-kit::export-ascii-destination-source-safe-p
                     "unsafe\\path" 0 11)
                    :to-be nil)
            (expect (org-kit::export-ascii-destination-source-safe-p nil 0 0)
                    :to-be nil)
            (expect (org-kit::export-ascii-destination-safe-p "raw" "expanded")
                    :to-be t)
            (expect (org-kit::export-ascii-destination-safe-p "raw" " leading")
                    :to-be nil)
            (expect (org-kit::export-ascii-destination-safe-p nil "expanded")
                    :to-be nil)
            (expect (org-kit::export-ascii-destination-safe-p "raw" "")
                    :to-be nil)
            (expect (coerce alignments 'list) :to-equal '(:right))
            (expect (org-kit::export-ascii-output-string center-output)
                    :to-equal "  x  ")
            (expect (org-kit::export-ascii-output-string right-output)
                    :to-equal "   x ")
            (expect (org-kit::export-ascii-output-string bracket-path)
                    :to-equal "https://docs.example/manual")
            (expect (multiple-value-bind (single-colon-path ignored-safe)
                        (org-kit::export-ascii-output-link-path
                         link-output single-colon-link link-preparation)
                      (declare (ignore ignored-safe))
                      (org-kit::export-ascii-output-string single-colon-path))
                    :to-equal "https://docs.example/manual")
            (expect (multiple-value-bind (plain-path ignored-safe)
                        (org-kit::export-ascii-output-link-path
                         link-output plain-link link-preparation)
                      (declare (ignore ignored-safe))
                      (org-kit::export-ascii-output-string plain-path))
                    :to-equal "https://docs.example/manual")
            (expect (search ":one:two:" tagged) :not :to-be nil))))))

  (it "covers source export modes and trims padded ASCII footnotes"
    (let* ((document (parse-org "body"))
           (preparation (export-ascii-test-preparation document nil))
           (source-node (make-node :src-block :properties '(:value "code")))
           (code-view
             (org-kit::%make-export-view
              :node source-node :payload '(:exports :code)))
           (results-view
             (org-kit::%make-export-view
              :node source-node :payload '(:exports :results)))
           (code-output (export-ascii-test-output
                         preparation
                         (lambda (output)
                           (org-kit::export-ascii-output-render-source-block
                            output code-view preparation))))
           (results-output (export-ascii-test-output
                            preparation
                            (lambda (output)
                              (org-kit::export-ascii-output-render-source-block
                               output results-view preparation))))
           (footnote-document (make-document "   note   "))
           (footnote-text (make-node :plain-text :begin 0 :end 10))
           (footnote-node
             (make-node :footnote-definition :children (list footnote-text)))
           (footnote-root
             (make-node :org-data :children (list footnote-node)))
           (footnote-view
             (progn
               (setf (document-root footnote-document) footnote-root)
               (export-ascii-test-view
                footnote-node footnote-document
                (list (export-ascii-test-view footnote-text footnote-document)))))
           (footnote-preparation
             (org-kit::%make-export-preparation
              :document footnote-document :backend :ascii :metadata nil
              :options nil :root-views nil :toc nil
              :footnotes (list (list :number 1 :definition footnote-view))
              :allow-raw-html nil))
           (footnote-output
             (export-ascii-test-output
              footnote-preparation
              (lambda (output)
                (org-kit::export-ascii-output-write-footnotes
                 output footnote-preparation)))))
      (with-soft-assertions
        (expect (search "code" code-output) :not :to-be nil)
        (expect results-output :to-equal "")
        (expect (search "[1] note" footnote-output) :not :to-be nil))))

  (it "trims whitespace and a trailing block newline from list item content"
    (let* ((document (make-document "first"))
           (text-node (make-node :plain-text :begin 0 :end 5))
           (paragraph-node (make-node :paragraph :children (list text-node)))
           (export-node
             (make-node :export-block
                        :properties (list :type "ASCII"
                                          :value (format nil "tail~%"))))
           (item-node
             (make-node :item :properties '(:bullet "- ")
                        :children (list paragraph-node export-node)))
           (list-node
             (make-node :plain-list :properties '(:type :unordered)
                        :children (list item-node)))
           (root (make-node :org-data :children (list list-node)))
           (paragraph-view
             (export-ascii-test-view
              paragraph-node document
              (list (export-ascii-test-view text-node document))))
           (export-view (export-ascii-test-view export-node document))
           (item-view
             (export-ascii-test-view
              item-node document (list paragraph-view export-view)))
           (preparation (export-ascii-test-preparation document nil)))
      (setf (document-root document) root)
      (with-soft-assertions
        (expect (export-ascii-test-render-list-item item-view preparation 1)
                :to-equal (format nil "- first~%  ~%  tail")))))

  (it "trims trailing newlines from an ASCII source block"
    (let* ((document (parse-org "body"))
           (preparation (export-ascii-test-preparation document nil))
           (node (make-node :src-block
                            :properties (list :value (format nil "code~%~%"))))
           (view (org-kit::%make-export-view
                  :node node :payload '(:exports :code)))
           (output
             (export-ascii-test-output
              preparation
              (lambda (buffer)
                (org-kit::export-ascii-output-render-source-block
                 buffer view preparation)))))
      (expect output :to-equal (format nil ",----~%| code~%`----"))))

  (it "renders an empty programmatic table.el node through the public API"
    (let* ((document (make-document ""))
           (table (make-node :table :properties '(:type :table.el :value "")))
           (section (make-node :section :children (list table)))
           (root (make-node :org-data :children (list section))))
      (setf (document-root document) root)
      (expect (export-org document :ascii) :to-equal (string #\Newline))))

  (it "expands an abbreviation link whose path ends at its colon"
    (let* ((document
             (parse-org "[[bare:][Empty tail]]"
                        :link-abbrevs '(("bare" . "https://bare.test/%s"))))
           (output (export-org document :ascii)))
      (expect (search "<https://bare.test/>" output)
              :not :to-be nil)))

  (it "trims an all-whitespace paragraph through the public API"
    (let* ((document (make-document "   "))
           (text (make-node :plain-text :begin 0 :end 3))
           (paragraph (make-node :paragraph :children (list text)))
           (section (make-node :section :children (list paragraph)))
           (root (make-node :org-data :children (list section))))
      (setf (document-root document) root)
      (expect (export-org document :ascii) :to-equal (string #\Newline))))

  (it "trims newline-only content from a list item through the public API"
    (let* ((document (make-document " "))
           (text (make-node :plain-text :begin 0 :end 1))
           (paragraph (make-node :paragraph :children (list text)))
           (block (make-node :export-block
                             :properties (list :type "ASCII"
                                               :value (string #\Newline))))
           (item (make-node :item :properties '(:bullet "- ")
                            :children (list paragraph block)))
           (list (make-node :plain-list :properties '(:type :unordered)
                            :children (list item)))
           (section (make-node :section :children (list list)))
           (root (make-node :org-data :children (list section))))
      (setf (document-root document) root)
      (expect (export-org document :ascii) :to-equal (format nil "  - ~%"))))

  (it "trims a newline-only ASCII source block through the public API"
    (let* ((document
             (parse-org (format nil "#+begin_src text~%~%#+end_src~%"))))
      (expect (export-org document :ascii) :to-equal (string #\Newline))))

  (it "covers empty code-box, link-note, and paragraph boundaries"
    (let* ((document (make-document "  trim  "))
           (text-node (make-node :plain-text :begin 0 :end 8))
           (paragraph-node (make-node :paragraph :children (list text-node)))
           (root (make-node :org-data :children (list paragraph-node)))
           (paragraph-view
             (export-ascii-test-view
              paragraph-node document
              (list (export-ascii-test-view text-node document))))
           (preparation (export-ascii-test-preparation document nil))
           (paragraph-output
             (progn
               (setf (document-root document) root)
               (export-ascii-test-render-view paragraph-view preparation)))
           (box-output
             (export-ascii-test-output
              preparation
              (lambda (output)
                (org-kit::export-ascii-output-write-code-box
                 output ":" :fixed-width-p t))))
           (target-view (export-ascii-test-view (make-node :target) document))
           (link-view
             (export-ascii-test-view
              (make-node :link :properties '(:type "radio")) document
              (list target-view)))
           (link-output (export-ascii-test-output-state preparation))
           (link-note
             (org-kit::export-ascii-output-link-note
              link-output link-view preparation)))
      (with-soft-assertions
        (expect paragraph-output :to-equal "trim")
        (expect box-output :to-equal (format nil ",----~%| ~%`----"))
        (expect link-note :to-be nil))))

  (it "trims all whitespace from an ASCII footnote definition"
    (let* ((document (make-document "   "))
           (text-node (make-node :plain-text :begin 0 :end 3))
           (definition (make-node :footnote-definition :children (list text-node)))
           (root (make-node :org-data :children (list definition)))
           (view
             (export-ascii-test-view
              definition document
              (list (export-ascii-test-view text-node document))))
           (preparation
             (org-kit::%make-export-preparation
              :document document :backend :ascii :metadata nil :options nil
              :root-views nil :toc nil
              :footnotes (list (list :number 1 :definition view))
              :allow-raw-html nil))
           (output
             (progn
               (setf (document-root document) root)
               (export-ascii-test-output
                preparation
                (lambda (buffer)
                  (org-kit::export-ascii-output-write-footnotes
                   buffer preparation))))))
      (expect (search "[1]" output) :not :to-be nil)))

  (it "indents continued paragraph lines inside list items"
    (let* ((source
             (export-ascii-test-source
              "#+OPTIONS: toc:nil num:nil \\n:t"
              "- first line"
              "  second line"))
           (output (export-org (parse-org source) :ascii)))
      (with-soft-assertions
        (expect (search (format nil "first line~%    second line") output)
                :not :to-be nil))))
  (it "dispatches nested ASCII overrides through the sink"
      (let ((backend :ascii-test-nested-override)
            (calls 0)
            (registered-p nil))
        (unwind-protect
            (progn
              (register-derived-export-backend
               backend :ascii
               :transcoders
               (list (cons :bold
                           (lambda (node view frame context)
                             (declare (ignore node view frame context))
                             (incf calls)
                             "OVERRIDE"))))
              (setf registered-p t)
              (let ((output
                     (export-org
                      (parse-org
                       (export-ascii-test-source
                        "#+OPTIONS: toc:nil num:nil"
                        ""
                        "before *inside* after"))
                      backend)))
                (with-soft-assertions
                 (expect output :to-equal
                         (format nil "before OVERRIDE after~%"))
                 (expect calls :to-be 1))))
          (when registered-p
            (unregister-export-backend backend)))))
  (it "applies plain-text filters inside wrappers in paragraph and table fast paths"
      (let ((backend :ascii-test-plain-wrapper-order)
            (registered-p nil))
        (unwind-protect
            (progn
              (register-derived-export-backend
               backend :ascii
               :filters
               (list
                (cons :plain-text
                      (list (lambda (value context)
                              (declare (ignore context))
                              (concatenate 'string "F(" value ")")))))
               :node-wrapper
               (lambda (node view frame context render-next)
                 (declare (ignore view frame context))
                 (if (eq (node-type node) :plain-text)
                     (funcall render-next "[" "]")
                   (funcall render-next "" ""))))
              (setf registered-p t)
              (let* ((paragraph-output
                      (export-org
                       (parse-org
                        (export-ascii-test-source
                         "#+OPTIONS: toc:nil num:nil"
                         ""
                         "word"))
                       backend))
                     (table-output
                      (export-org
                       (parse-org
                        (export-ascii-test-source
                         "#+OPTIONS: toc:nil num:nil"
                         ""
                         "| x |"))
                       backend)))
                (with-soft-assertions
                 (expect paragraph-output :to-equal (format nil "[F(word)]~%"))
                 (expect (search "[F(x)]" table-output) :not :to-be nil))))
          (when registered-p
            (unregister-export-backend backend)))))
  (it "passes post-blank to element filters and removes it from block layout"
      (let* ((backend :ascii-test-post-blank-filter)
             (registered-p nil)
             (filtered-value nil)
             (document
              (parse-org
               (export-ascii-test-source
                "#+OPTIONS: toc:nil num:nil"
                ""
                "x"
                ""
                "")))
             (paragraph
              (export-ascii-test-find-node (document-root document) :paragraph))
             (post-blank (node-post-blank paragraph))
             (expected-filter-value
              (concatenate 'string "x"
                           (make-string post-blank
                                        :initial-element #\Newline))))
        (unwind-protect
            (progn
              (register-derived-export-backend
               backend :ascii
               :filters
               (list
                (cons '(:element :paragraph)
                      (list (lambda (value context)
                              (declare (ignore context))
                              (setf filtered-value value)
                              value)))))
              (setf registered-p t)
              (let ((output (export-org document backend)))
                (with-soft-assertions
                 (expect (plusp post-blank) :to-be t)
                 (expect filtered-value :to-equal expected-filter-value)
                 (expect output :to-equal (format nil "x~%")))))
          (when registered-p
            (unregister-export-backend backend)))))
  (it "counts raw element-filter output against the output limit"
    (let ((backend :ascii-test-raw-filter-limit)
          (registered-p nil)
          (document
            (parse-org
             (export-ascii-test-source
              "#+OPTIONS: toc:nil num:nil"
              ""
              "x")
             :limits (make-limits :max-output-length 2))))
      (unwind-protect
           (progn
             (register-derived-export-backend
              backend :ascii
              :filters
              (list
               (cons (list :element :plain-text)
                     (list (lambda (value context)
                             (declare (ignore value context))
                             (string (code-char 27)))))))
             (setf registered-p t)
             (with-soft-assertions
               (expect (export-org document backend)
                       :to-equal (format nil "~C~%" (code-char 27)))))
        (when registered-p
          (unregister-export-backend backend)))))
  (it "writes raw ASCII ranges with indentation, tab substitution, and uppercasing"
    (let* ((preparation (export-ascii-test-preparation (parse-org "") nil))
           (output (export-ascii-test-output-state preparation))
           (tabbed (format nil "a~Cb~%" #\Tab)))
      (setf (org-kit::export-ascii-output-indent output) 2)
      (org-kit::export-ascii-output-write-range
       output tabbed 0 (length tabbed) :substitute-tab-p t :raw-output-p t)
      (org-kit::export-ascii-output-write-range
       output "tail" 0 4 :uppercase-p t :raw-output-p t)
      (with-soft-assertions
        (expect (org-kit::export-ascii-output-string output)
                :to-equal (format nil "  a b~%  TAIL"))
        (expect (org-kit::export-ascii-output-count output)
                :to-be 12))))
  (it "trims whitespace from filtered ASCII plain text at inline boundaries"
    (let ((backend :ascii-test-plain-text-trim)
          (registered-p nil))
      (unwind-protect
          (progn
            (register-derived-export-backend
             backend :ascii
             :filters
             (list
              (cons :plain-text
                    (list (lambda (value context)
                            (declare (ignore context))
                            (format nil " ~C~A ~C~C"
                                    #\Tab value #\Return #\Newline))))))
            (setf registered-p t)
            (let ((output
                    (export-org
                     (parse-org
                      (export-ascii-test-source
                       "#+OPTIONS: toc:nil num:nil"
                       ""
                       "word"))
                     backend)))
              (with-soft-assertions
                (expect output :to-equal (format nil "word~%")))))
        (when registered-p
          (unregister-export-backend backend)))))
  (it "removes post-blank indentation from captured ASCII blocks"
    (labels ((remove-post-blank (rendered post-blank-indent)
               (let* ((preparation
                        (export-ascii-test-preparation (parse-org "") nil))
                      (output (export-ascii-test-output-state preparation))
                      (paragraph (make-node :paragraph :post-blank 2)))
                 (org-kit::export-ascii-output-write-range
                  output rendered 0 (length rendered) :raw-output-p t)
                 (org-kit::export-ascii-output-remove-node-post-blank
                  output paragraph post-blank-indent)
                 output)))
      (let* ((output
               (remove-post-blank (format nil "body~%  ~%~%") 2)))
        (with-soft-assertions
          (expect (org-kit::export-ascii-output-string output)
                  :to-equal (format nil "body~%"))
          (expect (org-kit::export-ascii-output-count output) :to-be 5)
          (expect (org-kit::export-ascii-output-line-start-p output) :to-be t)))
      (with-soft-assertions
        (expect (org-kit::export-ascii-output-string
                 (remove-post-blank (format nil "  ~%~%") 3))
                :to-equal "  ")
        (expect (org-kit::export-ascii-output-string
                 (remove-post-blank (format nil "xy~%~%") 2))
                :to-equal "xy"))))
  (it "rebuilds recursive ASCII views with their original source documents"
    (let* ((document (parse-org "paragraph *bold*"))
           (root (document-root document))
           (view (org-kit::export-ascii-output-view-from-node root)))
      (labels ((check-view (node current-view)
                 (let ((view-children
                         (org-kit::export-view-children current-view))
                       (node-children (node-children node)))
                   (expect (org-kit::export-view-node current-view)
                           :to-be node)
                   (expect (org-kit::export-view-source-document current-view)
                           :to-be document)
                   (expect (mapcar #'org-kit::export-view-node view-children)
                           :to-equal node-children)
                   (mapc #'check-view node-children view-children))))
        (with-soft-assertions
          (check-view root view)))))
  (it "renders a multi-part ASCII table cell through the public API"
    (let* ((source
             (export-ascii-test-source
              "#+OPTIONS: toc:nil num:nil"
              ""
              "| plain *emphasis* |"))
           (document (parse-org source))
           (cell (export-ascii-test-find-node (document-root document) :table-cell))
           (output (export-org document :ascii)))
      (with-soft-assertions
        (expect (> (length (node-children cell)) 1) :to-be t)
        (expect (search "*emphasis*" output) :not :to-be nil))))
  (it "trims all whitespace returned by an ASCII plain-text filter"
    (let ((backend :ascii-test-all-whitespace-filter)
          (registered-p nil))
      (unwind-protect
          (progn
            (register-derived-export-backend
             backend :ascii
             :filters
             (list
              (cons :plain-text
                    (list (lambda (value context)
                            (declare (ignore value context))
                            (format nil "~C~C~C~C"
                                    #\Space #\Tab #\Return #\Newline))))))
            (setf registered-p t)
            (let ((output
                    (export-org
                     (parse-org
                      (export-ascii-test-source
                       "#+OPTIONS: toc:nil num:nil"
                       ""
                       "word"))
                     backend)))
              (expect output :to-equal (string #\Newline))))
        (when registered-p
          (unregister-export-backend backend)))))
  (it "preserves filtered ASCII suffixes around post-blank in heading sections"
    (let* ((backend :ascii-test-post-blank-filter-boundaries)
           (registered-p nil)
           (document
             (parse-org
              (export-ascii-test-source
               "#+OPTIONS: toc:nil num:nil"
               "* Heading"
               ""
               "body"
               ""
               ""
               "")))
           (paragraph
             (export-ascii-test-find-node (document-root document) :paragraph))
           (section (node-parent paragraph))
           (post-blank (node-post-blank paragraph))
           (filtered-value ""))
      (unwind-protect
          (progn
            (register-derived-export-backend
             backend :ascii
             :filters
             (list
              (cons '(:element :paragraph)
                    (list (lambda (value context)
                            (declare (ignore value context))
                            filtered-value)))))
            (setf registered-p t)
            (let* ((empty-output
                     (progn
                       (setf filtered-value "")
                       (export-org document backend)))
                   (blank-only-output
                     (progn
                       (setf filtered-value
                             (make-string post-blank :initial-element #\Newline))
                       (export-org document backend)))
                   (mismatch-output
                     (progn
                       (setf filtered-value "kept-filter-tail")
                       (export-org document backend)))
                   (indented-tail-output
                     (progn
                       (setf filtered-value
                             (concatenate
                              'string "body" (string #\Newline) " X"
                              (make-string post-blank
                                           :initial-element #\Newline)))
                       (export-org document backend))))
              (with-soft-assertions
                (expect (plusp post-blank) :to-be t)
                (expect (node-type section) :to-be :section)
                (expect (node-type (node-parent section)) :to-be :headline)
                (expect blank-only-output :to-equal empty-output)
                (expect (search "kept-filter-tail" mismatch-output)
                        :not :to-be nil)
                (expect (search (format nil "body~%   X") indented-tail-output) :not :to-be nil))))
        (when registered-p
          (unregister-export-backend backend)))))
  (it "rejects raw ASCII element-filter output beyond the output limit"
    (let ((backend :ascii-test-raw-filter-over-limit)
          (registered-p nil)
          (document
            (parse-org
             (export-ascii-test-source
              "#+OPTIONS: toc:nil num:nil"
              ""
              "x")
             :limits (make-limits :max-output-length 2)))
          (condition nil))
      (unwind-protect
          (progn
            (register-derived-export-backend
             backend :ascii
             :filters
             (list
              (cons '(:element :plain-text)
                    (list (lambda (value context)
                            (declare (ignore value context))
                            "too long")))))
            (setf registered-p t
                  condition
                  (export-ascii-test-catch-error
                   (lambda () (export-org document backend))))
            (with-soft-assertions
              (expect (typep condition 'org-resource-limit-error) :to-be t)
              (when (typep condition 'org-resource-limit-error)
                (expect (org-resource-limit-error-limit-name condition)
                        :to-be :max-output-length)
                (expect (> (org-resource-limit-error-actual condition) 2)
                        :to-be t))))
        (when registered-p
          (unregister-export-backend backend)))))
  (it "builds an ASCII view when a renderer omits one"
    (let* ((backend :ascii-test-node-without-view)
           (document
             (parse-org
              (export-ascii-test-source
               "#+OPTIONS: toc:nil num:nil" "" "body")))
           (expected (export-org document :ascii))
           (registered-p nil))
      (unwind-protect
          (progn
            (register-derived-export-backend
             backend :ascii
             :renderer
             (lambda (context)
               (export-render-node
                context
                (document-root (export-render-context-document context)))))
            (setf registered-p t)
            (with-soft-assertions
              (expect expected :to-equal (format nil "body~%"))
              (expect (export-org document backend) :to-equal "body")))
        (when registered-p
          (unregister-export-backend backend))))))

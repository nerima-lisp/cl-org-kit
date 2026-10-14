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
        (expect (org-kit::export-ascii-render-view view-three preparation)
                :to-equal (format nil "~A~%b" (make-string 69 :initial-element #\a)))
        (expect (org-kit::export-ascii-render-view view-four preparation)
                :to-equal text)
        (expect (mapcar (lambda (view)
                          (org-kit::export-ascii-render-view view preparation))
                        ascii-links)
                :to-equal '("<https://fallback.test/path>"
                            "<assets/topic:v2>"
                            "<x>"
                            "missing-target"))
        (expect (org-kit::export-ascii-render-view target-link preparation)
                :to-equal "Target")
        (expect (org-kit::export-ascii-render-view ordinary-target-link
                                                    preparation)
                :to-equal "Target")
        (expect (org-kit::export-ascii-render-view empty-fuzzy-link preparation)
                :to-equal "")
        (expect (org-kit::export-ascii-local-heading
                 missing-fuzzy-path preparation)
                :to-be nil)
        (expect (org-kit::export-ascii-local-heading external-link preparation)
                :to-be nil)
        (expect (org-kit::export-ascii-render-view root-view preparation)
                :to-equal (format nil "Target~%======"))
        (expect (org-kit::export-ascii-render-view utf8-entity preparation)
                :to-equal "UTF8")
        (expect (org-kit::export-ascii-render-view ascii-entity preparation)
                :to-equal "ASCII")
        (expect (org-kit::export-ascii-render-view empty-entity preparation)
                :to-equal "")
        (expect (org-kit::export-ascii-render-view span-entity preparation)
                :to-equal entity-text)
        (expect (org-kit::export-ascii-render-view fixed-width preparation)
                :to-equal (format nil ",----~%| left~%`----"))
        (expect (org-kit::export-ascii-render-view empty-example preparation)
                :to-equal "")
        ;; Left justification leaves both short and over-width source text intact.
        (expect (org-kit::export-ascii-justify-line "Title" 10 :left)
                :to-equal "Title")
        (expect (org-kit::export-ascii-justify-line "Title" 3 :left)
                :to-equal "Title")
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
        ;; Ordinary rows pass through while colon-prefixed rows trim their marker.
        (expect (org-kit::export-ascii-fixed-width-value
                 (format nil "ordinary~%:  fixed~%"))
                :to-equal (format nil "ordinary~%fixed~%"))
        (expect (org-kit::export-ascii-fixed-width-value "")
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
               (org-kit::export-ascii-render-views
                (list included-view section-view tail-view) preparation))
             (deeper-after-include
               (org-kit::export-ascii-render-views
                (list included-view deep-view tail-view) included-preparation))
             (same-level-after-include
               (org-kit::export-ascii-render-views
                (list included-view shallow-view tail-view) included-preparation)))
        (with-soft-assertions
          (expect (org-kit::export-ascii-view-gap empty-section tail-view)
                  :to-equal (org-kit::export-ascii-newlines 2))
          (expect (org-kit::export-ascii-view-gap nonempty-section tail-view)
                  :to-equal (org-kit::export-ascii-newlines 3))
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
        (expect (org-kit::export-ascii-heading-title
                 view preparation numbers 72 nil t)
                :to-equal "Tagged")
        (expect (org-kit::export-ascii-heading-title
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
                          (org-kit::export-ascii-heading-title
                           view default-preparation default-numbers 72 nil))
                        views)
                :to-equal '("1 TODO Top" "1.1 Child" "1.1.1 Deep"))
        (expect (mapcar (lambda (view)
                          (org-kit::export-ascii-heading-title
                           view bounded-preparation bounded-numbers 72 nil))
                        views)
                :to-equal '("1 TODO Top" "1.1 Child" "Deep"))
        (expect (mapcar (lambda (view)
                          (org-kit::export-ascii-heading-title
                           view disabled-preparation disabled-numbers 72 nil))
                        views)
                :to-equal '("TODO Top" "Child" "Deep"))
        (expect (org-kit::export-ascii-heading-first-part
                 (first views) todo-disabled-preparation todo-disabled-numbers)
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
        (expect (org-kit::export-ascii-heading-title
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
        (expect (org-kit::export-ascii-render-item item-view preparation 1)
                :to-equal (format nil "- ~%  value"))
        (expect (org-kit::export-ascii-render-item unmarked-item-view
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
        (expect (export-ascii-test-document-snapshot document) :to-equal before)))))

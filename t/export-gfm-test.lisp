;;;; t/export-gfm-test.lisp
(in-package #:cl-org-kit/test)

(defun export-gfm-test-fixture-path (relative)
  (asdf:system-relative-pathname
   "cl-org-kit" (concatenate 'string "t/fixtures/export/" relative)))

(defun export-gfm-test-read-text (relative)
  (uiop:read-file-string (export-gfm-test-fixture-path relative)))

(defun export-gfm-test-read-octets (relative)
  (with-open-file (stream (export-gfm-test-fixture-path relative)
                          :direction :input :element-type '(unsigned-byte 8))
    (let* ((length (file-length stream))
           (octets (make-array length :element-type '(unsigned-byte 8))))
      (unless (= (read-sequence octets stream) length)
        (error "Unable to read all bytes from GFM fixture ~A" relative))
      octets)))

(defun export-gfm-test-tsv-fields (line)
  (loop with start = 0
        for tab = (position #\Tab line :start start)
        collect (subseq line start tab)
        while tab
          do (setf start (1+ tab))))

(defun export-gfm-test-manifest-rows (relative)
  (mapcar #'export-gfm-test-tsv-fields
          (rest (uiop:read-file-lines (export-gfm-test-fixture-path relative)))))

(defun export-gfm-test-catch-error (function)
  (handler-case (progn (funcall function) nil)
    (error (condition) condition)))

(defun export-gfm-test-nodes (document)
  (let ((nodes nil))
    (map-nodes (lambda (node) (push node nodes)) (document-root document))
    (nreverse nodes)))

(defun export-gfm-test-node-types (document)
  (mapcar #'node-type (export-gfm-test-nodes document)))

(defun export-gfm-test-view (type &key properties children payload
                                  (post-blank 0))
  (org-kit::%make-export-view
   :node (make-node type :properties properties :post-blank post-blank)
   :children children :payload payload))

(defun export-gfm-test-attached-node (type text &key properties children)
  (let* ((document (make-document text))
         (node (make-node type :begin 0 :end (length text)
                          :properties properties :children children))
         (root (if (eq type :org-data) node
                   (make-node :org-data :children (list node)))))
    (setf (document-root document) root)
    node))

(defun export-gfm-test-state (&optional (source "Body"))
  (let* ((preparation (org-kit::export-prepare (parse-org source) :gfm))
         (state (org-kit::export-gfm-state-for-preparation preparation)))
    (org-kit::export-gfm-index-state state)))

(defun export-gfm-test-state-for-preparation (preparation)
  (org-kit::export-gfm-state-for-preparation preparation))

(defun export-gfm-test-policy-source (name)
  (export-gfm-test-read-text (concatenate 'string "policy/gfm/" name)))

(defclass export-gfm-test-octet-input-stream
    (sb-gray:fundamental-binary-input-stream)
  ((octets :initarg :octets :reader export-gfm-test-input-octets)
   (position :initform 0 :accessor export-gfm-test-input-position)))

(defmethod stream-element-type ((stream export-gfm-test-octet-input-stream))
  (declare (ignore stream))
  '(unsigned-byte 8))

(defmethod sb-gray:stream-read-byte ((stream export-gfm-test-octet-input-stream))
  (with-slots (octets position) stream
    (if (< position (length octets))
        (prog1 (aref octets position) (incf position))
        :eof)))

(describe "E3 GFM export"
  (it "matches the GFM-only pinned ox-gfm outputs byte for byte"
    (let ((rows (export-gfm-test-manifest-rows
                 "oracle/gfm/compatibility/manifest.tsv"))
          (case-names nil))
      (with-soft-assertions
        (expect (mapcar #'length rows) :to-equal '(4 4 4 4))
        (dolist (row rows)
          (destructuring-bind (case source include expected) row
            (push case case-names)
            (let* ((document
                     (parse-org (export-gfm-test-read-text
                                 (concatenate 'string "oracle/gfm/" source))
                                :source-name source))
                   (resolver-calls nil)
                   (resolver
                     (unless (string= include "-")
                       (lambda (path parent-source)
                         (declare (ignore parent-source))
                         (push path resolver-calls)
                         (when (string= path "included/fragment.org")
                           (export-gfm-test-read-text
                            "oracle/gfm/compatibility/source/included/fragment.org")))))
                   (actual
                     (export-org document :gfm
                                 :include-resolver resolver
                                 :output-type :octets))
                   (expected-path
                     (concatenate 'string
                                  "oracle/gfm/" expected)))
              (expect actual
                      :to-equalp (export-gfm-test-read-octets expected-path))
              (expect (nreverse resolver-calls)
                      :to-equal (if (string= include "-") nil
                                    '("included/fragment.org")))
              (let ((metadata
                      (export-gfm-test-read-text
                       (concatenate 'string expected-path ".meta"))))
                (expect (search "emacs=31.1" metadata) :to-be-truthy)
                (expect (search "org=9.8.10" metadata) :to-be-truthy)
                (expect (search
                         "ox-gfm-commit=4f774f13d34b3db9ea4ddb0b1edc070b1526ccbb"
                         metadata)
                        :to-be-truthy)
                (expect (search
                         "ox-gfm-source-sha256=84c841f4204447eec317453ebab514741d22d4b65459a652eb87e04b29483771"
                         metadata)
                        :to-be-truthy)))))
        (expect (nreverse case-names)
                :to-equal '("metadata-selection" "exclude-tags-override"
                            "footnotes-macros-links" "include")))))

  (it "pins GFM policy outputs independently from the Ox compatibility subset"
    (let ((rows (export-gfm-test-manifest-rows "policy/gfm/manifest.tsv"))
          (case-names nil))
      (with-soft-assertions
        (expect (mapcar #'length rows) :to-equal '(4 4 4 4 4 4 4))
        (dolist (row rows)
          (destructuring-bind (case source raw expected) row
            (push case case-names)
            (let* ((document
                     (parse-org (export-gfm-test-policy-source source)
                                :source-name source))
                   (actual
                     (export-org document :gfm
                                 :allow-raw-html (string= raw "yes")
                                 :output-type :octets)))
              (expect actual
                      :to-equalp
                      (export-gfm-test-read-octets
                       (concatenate 'string "policy/gfm/" expected)))
              (when (string= case "code")
                (expect (search
                         (cl-codec-kit:string-to-octets
                          "~~struck text~~" :encoding :utf-8)
                         actual)
                        :to-be-truthy)
                (expect (search
                         (cl-codec-kit:string-to-octets
                          "```code with `` two ticks```" :encoding :utf-8)
                         actual)
                        :to-be-truthy)
                (expect (search
                         (cl-codec-kit:string-to-octets
                          "`plain verbatim`" :encoding :utf-8)
                         actual)
                        :to-be-truthy))
              (when (string= case "table-structure")
                (expect (search
                         (cl-codec-kit:string-to-octets
                          (concatenate 'string "First hard break  "
                                       (string #\Newline) "Second line")
                          :encoding :utf-8)
                         actual)
                        :to-be-truthy)))))
        (expect (nreverse case-names)
                :to-equal '("links" "code" "raw-default" "raw-enabled"
                            "table-structure" "source-blocks"
                            "renderer-branches")))))

  (it "renders both-mode source code without an adjacent result block"
    (let* ((newline (string #\Newline))
           (document
             (parse-org
              (format nil "#+begin_src text :exports both~%body~%#+end_src")))
           (output (export-org document :gfm)))
      (expect output
              :to-equal (concatenate 'string "~~~text" newline "body"
                                     newline "~~~"))
      (expect (count #\~ output) :to-equal 6)))

  (it "keeps the policy branch inputs as the expected parsed GFM node types"
    (let* ((link-document (parse-org (export-gfm-test-policy-source "links.org")))
           (code-document (parse-org (export-gfm-test-policy-source "code.org")))
           (raw-document (parse-org (export-gfm-test-policy-source "raw.org")))
           (structure-document
             (parse-org (export-gfm-test-policy-source "table-structure.org")))
           (source-document
             (parse-org (export-gfm-test-policy-source "source-blocks.org")))
           (branch-document
             (parse-org
              (export-gfm-test-policy-source "renderer-branches.org")))
           (branch-table
             (find-if (lambda (node) (eq (node-type node) :table))
                      (export-gfm-test-nodes branch-document)))
           (branch-preparation (org-kit::export-prepare branch-document :gfm))
           (branch-table-rows
             (remove-if (lambda (row)
                          (eq (node-property row :type) :rule))
                        (remove-if-not
                         (lambda (node) (eq (node-type node) :table-row))
                         (node-children branch-table))))
           (branch-table-view
             (find-if (lambda (view)
                        (eq (org-kit::export-view-node view) branch-table))
                      (org-kit::export-collect-views
                       (org-kit::export-preparation-root-views
                        branch-preparation))))
           (branch-table-view-rows
             (remove-if
              (lambda (view)
                (eq (node-property (org-kit::export-view-node view) :type)
                    :rule))
              (org-kit::export-view-children branch-table-view)))
           (table-cells
             (remove-if-not (lambda (node) (eq (node-type node) :table-cell))
                            (export-gfm-test-nodes structure-document))))
      (with-soft-assertions
        (expect (member :link (export-gfm-test-node-types link-document))
                :to-be-truthy)
        (dolist (type '(:code :verbatim :strike-through
                        :example-block :src-block))
          (expect (member type (export-gfm-test-node-types code-document))
                  :to-be-truthy))
        (dolist (type '(:export-block :export-snippet))
          (expect (member type (export-gfm-test-node-types raw-document))
                  :to-be-truthy))
        (dolist (type '(:headline :table :line-break :plain-list :item
                        :quote-block :verse-block :footnote-reference
                        :footnote-definition))
          (expect (member type (export-gfm-test-node-types structure-document))
                  :to-be-truthy))
        (expect (some (lambda (cell)
                        (search "\\|" (node-property cell :value)))
                      table-cells)
                :to-be-truthy)
        (expect (member :src-block (export-gfm-test-node-types source-document))
                :to-be-truthy)
        (dolist (type '(:bold :italic :strike-through :underline
                        :superscript :subscript :entity
                        :inline-src-block :timestamp :citation
                        :citation-reference :dynamic-block :horizontal-rule
                        :quote-block :line-break :center-block :special-block
                        :fixed-width :latex-environment :verse-block
                        :statistics-cookie
                        :plain-list :item :table-cell :footnote-reference
                        :footnote-definition :target :radio-target))
          (expect (member type (export-gfm-test-node-types branch-document))
                  :to-be-truthy))
        (expect (mapcar (lambda (row) (length (node-children row)))
                        branch-table-rows)
                :to-equal '(2 2))
        (expect (mapcar (lambda (view)
                          (length (org-kit::export-view-children view)))
                        branch-table-view-rows)
                :to-equal '(2 2))
        (expect (mapcar (lambda (cell) (node-property cell :value))
                        (node-children (second branch-table-rows)))
                :to-equal '("value" ""))
        (expect (mapcar
                 (lambda (view)
                   (node-property (org-kit::export-view-node view) :value))
                 (org-kit::export-view-children
                  (second branch-table-view-rows)))
                :to-equal '("value" "")))))

  (it "protects table cells when macro-expanded code spans and link destinations contain pipes"
    (let* ((source
             (format nil
                     "#+MACRO: code ~~a|<img src=x onerror=alert(1)>~~~%#+MACRO: link [[https://ok/a|b][x]]~%| {{{code}}} | {{{link}}} |~%"))
           (output (export-org (parse-org source) :gfm))
           (outside-table-output
             (export-org (parse-org "[[https://ok/a|b][x]]") :gfm)))
      (expect outside-table-output :to-equal "[x](<https://ok/a|b>)")
      (expect output
              :to-equal
              (format nil
                      "| `a\\|<img src=x onerror=alert(1)>` | [x](<https://ok/a%7Cb>) |~%| --- | --- |"))))

  (it "escapes table-cell newlines and collapses ordinary soft newlines"
    (let ((table-literal
            (concatenate 'string "left|first" (string #\Newline) "second"))
          (soft-literal
            (concatenate 'string "first" (string #\Newline) "second"
                         (string #\Newline))))
      (with-soft-assertions
        (expect (org-kit::export-gfm-escape-literal table-literal
                                                    :table-cell-p t)
                :to-equal "left\\|first<br>second")
        (expect (org-kit::export-gfm-escape-literal soft-literal
                                                    :soft-newline-p t)
                :to-equal "first second"))))

  (it "normalizes CR and covers hard, soft, trimmed, and table newlines"
    (let ((cr (string #\Return))
          (lf (string #\Newline))
          (tab (string #\Tab)))
      (with-soft-assertions
        (expect (org-kit::export-gfm-normalize-newlines
                 (concatenate 'string "a" (string #\Return) "b"
                              (string #\Return) (string #\Newline) "c"))
                :to-equal (concatenate 'string "a" lf "b" lf "c"))
        (expect (org-kit::export-gfm-escape-literal
                 (concatenate 'string "a" tab "!" cr lf "b" cr "c"))
                :to-equal (concatenate 'string "a \\!" lf "b" lf "c"))
        (expect (org-kit::export-gfm-escape-literal
                 (concatenate 'string "x" cr lf "y" lf)
                 :soft-newline-p t)
                :to-equal "x y")
        (expect (org-kit::export-gfm-escape-literal
                 (concatenate 'string "x" cr lf "y" cr "z"))
                :to-equal (concatenate 'string "x" lf "y" lf "z"))
        (expect (org-kit::export-gfm-escape-literal
                 (concatenate 'string "tail" lf lf)
                 :trim-terminal-newline-p t)
                :to-equal "tail")
        (expect (org-kit::export-gfm-escape-literal
                 (concatenate 'string "a" cr lf "b" lf)
                 :table-cell-p t)
                :to-equal "a<br>b<br>"))))

  (it "normalizes a terminal CR without probing past the input"
    (let ((terminal-cr (concatenate 'string "tail" (string #\Return))))
      (with-soft-assertions
        (expect (org-kit::export-gfm-normalize-newlines terminal-cr)
                :to-equal (concatenate 'string "tail" (string #\Newline)))
        (expect (org-kit::export-gfm-escape-literal terminal-cr)
                :to-equal (concatenate 'string "tail" (string #\Newline))))))

  (it "escapes HTML attributes and builds adaptive inline and block code"
    (let* ((attribute (org-kit::export-gfm-escape-html-attribute
                       (concatenate 'string "&<>\"'")))
           (normal-span (org-kit::export-gfm-code-span "plain"))
           (padded-span (org-kit::export-gfm-code-span " `edge` "))
           (trailing-space-span (org-kit::export-gfm-code-span "edge "))
           (leading-backtick-span (org-kit::export-gfm-code-span "`edge"))
           (trailing-tab-span
             (org-kit::export-gfm-code-span (concatenate 'string "edge" (string #\Tab))))
           (empty-span (org-kit::export-gfm-code-span ""))
           (newline-span
             (org-kit::export-gfm-code-span
              (concatenate 'string "first" (string #\Return) "second")))
           (plain-fence (org-kit::export-gfm-code-fence "plain"))
           (adaptive-fence
             (org-kit::export-gfm-code-fence "~~body~~~"))
           (plain-block (org-kit::export-gfm-fenced-block "body" "lisp"))
           (empty-block (org-kit::export-gfm-fenced-block ""))
           (newline-block
             (org-kit::export-gfm-fenced-block
              (concatenate 'string "body" (string #\Newline)))))
      (with-soft-assertions
        (expect attribute :to-equal "&amp;&lt;&gt;&quot;'")
        (expect normal-span :to-equal "`plain`")
        (expect padded-span :to-equal "``  `edge`  ``")
        (expect trailing-space-span :to-equal "` edge  `")
        (expect leading-backtick-span :to-equal "`` `edge ``")
        (expect trailing-tab-span :to-equal (concatenate 'string "` edge" (string #\Tab) " `"))
        (expect empty-span :to-equal "``")
        (expect newline-span :to-equal "`first second`")
        (expect plain-fence :to-equal "~~~")
        (expect adaptive-fence :to-equal "~~~~")
        (expect plain-block
                :to-equal
                (concatenate 'string "~~~lisp" (string #\Newline)
                             "body" (string #\Newline) "~~~"))
        (expect empty-block
                :to-equal
                (concatenate 'string "~~~" (string #\Newline)
                             (string #\Newline) "~~~"))
        (expect newline-block
                :to-equal
                (concatenate 'string "~~~" (string #\Newline) "body"
                             (string #\Newline) "~~~")))))

  (it "uses default ranges when GFM literal helpers receive no keyword arguments"
    (with-soft-assertions
      (expect (apply #'org-kit::export-gfm-escaped-literal-output-length
                     "plain" nil)
              :to-equal 5)
      (expect (apply #'org-kit::export-gfm-code-span "plain" nil)
              :to-equal "`plain`")))

  (it "flattens inline objects in link labels and follows entity fallbacks"
    (let* ((text-node
             (export-gfm-test-attached-node :plain-text "first|second"))
           (text-view (org-kit::%make-export-view :node text-node))
           (italic-view
             (export-gfm-test-view :italic :children (list text-view)))
           (link-view
             (export-gfm-test-view :link :properties '(:path "destination")
                                   :children (list italic-view)))
           (nested-link-view
             (export-gfm-test-view :link :properties '(:path "nested/path")))
           (outer-link-view
             (export-gfm-test-view :link :children (list nested-link-view)))
           (entity-utf8
             (export-gfm-test-view :entity
                                   :properties '(:utf-8 "utf8" :unicode "unicode"
                                                 :html "html")))
           (entity-unicode
             (export-gfm-test-view :entity :properties '(:unicode "unicode")))
           (entity-html
             (export-gfm-test-view :entity :properties '(:html "&lt;")))
           (entity-empty (export-gfm-test-view :entity))
           (entity-text-node (export-gfm-test-attached-node :entity "<"))
           (entity-text-view
             (org-kit::%make-export-view :node entity-text-node))
           (code-node (make-node :code :properties '(:value "~code~")))
           (padded-code-node
             (make-node :code :post-blank 1 :properties '(:value "~code~ ")))
           (code-view (org-kit::%make-export-view :node code-node))
           (padded-code-view
             (org-kit::%make-export-view :node padded-code-node))
           (code-link-view
             (export-gfm-test-view :link :children (list code-view)))
           (padded-code-link-view
             (export-gfm-test-view :link :children (list padded-code-view))))
      (with-soft-assertions
        (expect (org-kit::export-gfm-link-label link-view :table-cell-p t)
                :to-equal "first\\|second")
        (expect (org-kit::export-gfm-link-label outer-link-view)
                :to-equal "nested\\/path")
        (expect (org-kit::export-gfm-link-label
                 (export-gfm-test-view :link :children (list entity-utf8)))
                :to-equal "utf8")
        (expect (org-kit::export-gfm-link-label
                 (export-gfm-test-view :link :children (list entity-unicode)))
                :to-equal "unicode")
        (expect (org-kit::export-gfm-link-label
                 (export-gfm-test-view :link :children (list entity-html)))
                :to-equal "\\&lt\\;")
        (expect (org-kit::export-gfm-link-label
                 (export-gfm-test-view :link :children (list entity-empty)))
                :to-equal "")
        (expect (org-kit::export-gfm-link-label
                 (export-gfm-test-view :link :children (list entity-text-view)))
                :to-equal "\\<")
        (expect (org-kit::export-gfm-link-label code-link-view)
                :to-equal "code")
        (expect (org-kit::export-gfm-link-label padded-code-link-view)
                :to-equal "code")
        (expect (org-kit::export-gfm-render-inline-view
                 (export-gfm-test-state) entity-text-view)
                :to-equal "\\<"))))

  (it "validates source language tags and literal-link label fallbacks"
    (let* ((valid-source
             (export-gfm-test-view :src-block :properties '(:language "lisp-3+")))
           (invalid-source
             (export-gfm-test-view :src-block :properties '(:language "lisp.bad")))
           (empty-source
             (export-gfm-test-view :src-block :properties '(:language "")))
           (missing-source (export-gfm-test-view :src-block))
           (empty-label-document (parse-org "[[relative/path]]"))
           (empty-label-node
             (find-if (lambda (node) (eq (node-type node) :link))
                      (export-gfm-test-nodes empty-label-document)))
           (empty-label-link (org-kit::%make-export-view :node empty-label-node))
           (state (export-gfm-test-state)))
      (with-soft-assertions
        (expect (org-kit::export-gfm-source-language
                 (org-kit::export-view-node valid-source))
                :to-equal "lisp-3+")
        (expect (org-kit::export-gfm-source-language
                 (org-kit::export-view-node invalid-source))
                :to-be nil)
        (expect (org-kit::export-gfm-source-language
                 (org-kit::export-view-node empty-source))
                :to-be nil)
        (expect (org-kit::export-gfm-source-language
                 (org-kit::export-view-node missing-source))
                :to-be nil)
        (expect (org-kit::export-gfm-link-label empty-label-link)
                :to-equal "relative\\/path")
        (expect (org-kit::export-gfm-render-inline-view state empty-label-link)
                :to-equal "[relative\\/path](<relative/path>)"))))

  (it "indexes custom targets and marks internal-link heading anchors"
    (let* ((heading
             (export-gfm-test-view
              :headline
              :properties '(:level 7 :raw-value "Custom <heading>"
                            :custom-id "Custom&ID" :tags ("tag"))))
           (link
             (export-gfm-test-view
              :link :properties '(:type "custom-id" :path "custom&id")))
           (paragraph
             (export-gfm-test-view :paragraph :children (list link)))
           (empty-heading
             (export-gfm-test-view
              :headline :properties '(:raw-value "!!!")))
           (preparation
             (org-kit::%make-export-preparation
              :backend :gfm :root-views (list heading paragraph empty-heading)
              :toc nil :footnotes nil))
           (state (export-gfm-test-state-for-preparation preparation)))
      (org-kit::export-gfm-index-state state)
      (with-soft-assertions
        (expect (org-kit::export-gfm-heading-reference state
                                                       (org-kit::export-view-node
                                                        heading))
                :to-equal "Custom&ID")
        (expect (gethash (org-kit::export-view-node heading)
                         (org-kit::export-gfm-state-anchored-headings state))
                :to-be-truthy)
        (expect (org-kit::export-gfm-heading-reference state
                                                       (org-kit::export-view-node
                                                        empty-heading))
                :to-equal "org-section")
        (expect (org-kit::export-gfm-render-heading state heading)
                :to-equal
                (format nil
                        "<a id=\"Custom&amp;ID\"></a>~%~%###### Custom \\<heading\\>     :tag:")))))

  (it "covers renderer fallbacks for headings, links, table cells, and targets"
    (let* ((preparation
             (org-kit::%make-export-preparation
              :backend :gfm :root-views nil :toc nil :footnotes nil))
           (state (export-gfm-test-state-for-preparation preparation))
           (custom-heading
             (make-node :headline
                        :properties '(:custom-id "Explicit-ID"
                                      :raw-value "Custom title")))
           (duplicate-heading
             (make-node :headline
                        :properties '(:custom-id "Explicit-ID"
                                      :raw-value "Duplicate title")))
           (empty-custom-id
             (make-node :headline
                        :properties '(:custom-id "" :raw-value "Heading fallback")))
           (missing-title (make-node :headline))
           (path-only-link
             (export-gfm-test-attached-node
              :link "[[https://example.test/fallback]]"
              :properties '(:path "https://example.test/fallback")))
           (unaddressable-link
             (export-gfm-test-attached-node :link "[[no-destination]]"))
           (custom-target-link
             (export-gfm-test-view
              :link :properties '(:type "custom-id" :path "not-indexed")))
           (fuzzy-target-link
             (export-gfm-test-view
              :link :properties '(:type "fuzzy" :path "not-indexed")))
           (radio-target-link
             (export-gfm-test-view
              :link :properties '(:type "radio" :path "not-indexed")))
           (missing-type-link
             (export-gfm-test-view :link :properties '(:path "not-indexed")))
           (missing-path-link
             (export-gfm-test-view :link :properties '(:type "custom-id")))
           (value-cell (export-gfm-test-view
                        :table-cell :properties '(:value "cell value")))
           (text-cell
             (org-kit::%make-export-view
              :node (export-gfm-test-attached-node :table-cell "source cell")))
           (empty-cell (export-gfm-test-view :table-cell))
           (orphan-target
             (export-gfm-test-view :target
                                   :properties '(:value "orphan target")))
           (empty-target (export-gfm-test-view :target)))
      (with-soft-assertions
        (expect (org-kit::export-gfm-heading-reference state custom-heading)
                :to-equal "Explicit-ID")
        (expect (org-kit::export-gfm-heading-reference state duplicate-heading)
                :to-equal "Explicit-ID-2")
        (expect (org-kit::export-gfm-heading-reference state empty-custom-id)
                :to-equal "org-heading-fallback")
        (expect (org-kit::export-gfm-heading-reference state missing-title)
                :to-equal "org-section")
        (expect (org-kit::export-gfm-link-destination state path-only-link)
                :to-equal "<https://example.test/fallback>")
        (expect (org-kit::export-gfm-link-destination state unaddressable-link)
                :to-be nil)
        (expect (org-kit::export-gfm-link-target-id
                 state (org-kit::export-view-node custom-target-link))
                :to-equal "not-indexed")
        (expect (org-kit::export-gfm-link-target-id
                 state (org-kit::export-view-node fuzzy-target-link))
                :to-be nil)
        (expect (org-kit::export-gfm-link-target-id
                 state (org-kit::export-view-node radio-target-link))
                :to-be nil)
        (expect (org-kit::export-gfm-link-target-id
                 state (org-kit::export-view-node missing-type-link))
                :to-be nil)
        (expect (org-kit::export-gfm-link-target-id
                 state (org-kit::export-view-node missing-path-link))
                :to-be nil)
        (expect (org-kit::export-gfm-table-cell state value-cell)
                :to-equal "cell value")
        (expect (org-kit::export-gfm-table-cell state text-cell)
                :to-equal "source cell")
        (expect (org-kit::export-gfm-table-cell state empty-cell)
                :to-equal "")
        (expect (org-kit::export-gfm-render-target state orphan-target)
                :to-equal "<a id=\"org-orphan-target\"></a>")
        (expect (org-kit::export-gfm-render-target state empty-target)
                :to-equal "<a id=\"org-section\"></a>"))))

  (it "renders GFM link labels without destinations through export-org"
    (let* ((source
             (format nil "#+OPTIONS: toc:nil num:nil~%[[javascript:alert]]"))
           (unsafe-output (export-org (parse-org source) :gfm))
           (document (make-document ""))
           (link (make-node :link :properties '(:format :bracket :type "fuzzy")))
           (paragraph (make-node :paragraph :children (list link)))
           (root (make-node :org-data :children (list paragraph))))
      (setf (document-root document) root)
      (with-soft-assertions
        (expect unsafe-output :to-equal "javascript\\:alert")
        (expect (export-org document :gfm) :to-equal ""))))

  (it "rejects malformed bracket sources while extracting link paths"
    (let ((state (export-gfm-test-state))
          (missing-source (make-node :link :properties '(:format :bracket)))
          (short-source
            (export-gfm-test-attached-node
             :link "[[" :properties '(:format :bracket)))
          (wrong-prefix
            (export-gfm-test-attached-node
             :link "plain source"
             :properties '(:format :bracket :path "safe/target")))
          (missing-close
            (export-gfm-test-attached-node
             :link "[[unclosed path" :properties '(:format :bracket)))
          (valid-source
            (export-gfm-test-attached-node
             :link "[[valid/path][label]]" :properties '(:format :bracket))))
      (with-soft-assertions
        (expect (org-kit::export-gfm-raw-bracket-link-path missing-source)
                :to-be nil)
        (expect (org-kit::export-gfm-raw-bracket-link-path short-source)
                :to-be nil)
        (expect (org-kit::export-gfm-raw-bracket-link-path wrong-prefix)
                :to-be nil)
        (expect (org-kit::export-gfm-raw-bracket-link-path missing-close)
                :to-be nil)
        (expect (org-kit::export-gfm-raw-bracket-link-path valid-source)
                :to-equal "valid/path")
        (expect (org-kit::export-gfm-link-destination state wrong-prefix)
                :to-be nil))))

  (it "rejects an entity-obfuscated destination after LINK expansion"
    (let* ((source
             (format nil
                     "#+LINK: danger javascript&colon;%s~%Before [[danger:alert][Danger label]] after"))
           (document (parse-org source))
           (links (remove-if-not
                   (lambda (node) (eq (node-type node) :link))
                   (export-gfm-test-nodes document))))
      (with-soft-assertions
        (expect (length links) :to-be 1)
        (expect (export-org document :gfm)
                :to-equal "Before Danger label after"))))

  (it "handles missing image paths and preserves inline source breaks"
    (let* ((state (export-gfm-test-state))
           (missing-path-node
             (export-gfm-test-attached-node :link "[[unresolved]]"))
           (label-node (export-gfm-test-attached-node :plain-text "Missing path"))
           (missing-path-link
             (org-kit::%make-export-view
              :node missing-path-node
              :children (list (org-kit::%make-export-view :node label-node))))
           (text (format nil "first~%second"))
           (timestamp-node
             (export-gfm-test-attached-node :timestamp text))
           (timestamp-view (org-kit::%make-export-view :node timestamp-node))
           (fallback-node
             (export-gfm-test-attached-node :diary-sexp text))
           (fallback-view (org-kit::%make-export-view :node fallback-node)))
      (with-soft-assertions
        (expect (org-kit::export-gfm-render-link state missing-path-link)
                :to-equal "Missing path")
        (expect (org-kit::export-gfm-render-inline-view state timestamp-view)
                :to-equal "first second")
        (expect (org-kit::export-gfm-render-inline-view
                 state timestamp-view :preserve-source-breaks-p t)
                :to-equal text)
        (expect (org-kit::export-gfm-render-inline-view
                 state timestamp-view :table-cell-p t)
                :to-equal "first<br>second")
        (expect (org-kit::export-gfm-render-inline-view state fallback-view)
                :to-equal "first second")
        (expect (org-kit::export-gfm-render-inline-view
                 state fallback-view :preserve-source-breaks-p t)
                :to-equal text)
        (expect (org-kit::export-gfm-render-inline-view
                 state fallback-view :table-cell-p t)
                :to-equal "first<br>second"))))

  (it "skips empty list children and rejects non-string raw backend tags"
    (let* ((state (export-gfm-test-state))
           (paragraph
             (export-gfm-test-view
              :paragraph :children
              (list (export-gfm-test-view
                     :plain-text :payload nil
                     :children nil))))
           (empty-child (export-gfm-test-view :diary-sexp))
           (item (export-gfm-test-view :item
                                       :children (list paragraph empty-child)))
           (export-block
             (make-node :export-block
                        :properties '(:type :gfm :value "<hr>")))
           (snippet
             (make-node :export-snippet
                        :properties '(:back-end :gfm :value "<br>"))))
      (with-soft-assertions
        (expect (org-kit::export-gfm-render-list-item state item "-" 0)
                :to-equal "- ")
        (expect (org-kit::export-gfm-render-export-block state export-block)
                :to-equal "")
        (expect (org-kit::export-gfm-render-snippet state snippet)
                :to-equal ""))))

  (it "renders footnote reference fallbacks and non-paragraph definitions"
    (let* ((definition
             (export-gfm-test-view
              :footnote-definition
              :children
              (list (export-gfm-test-view
                     :paragraph
                     :children (list (export-gfm-test-view
                                     :entity :properties '(:utf-8 "note"))))
                    (export-gfm-test-view :horizontal-rule)
                    (export-gfm-test-view :diary-sexp))))
           (entry (list :label "complex" :number 1 :definition definition))
           (second-entry
             (list :label "second" :number 2
                   :definition (export-gfm-test-view :footnote-definition)))
           (preparation
             (org-kit::%make-export-preparation
              :backend :gfm :root-views nil :toc nil
              :footnotes (list entry second-entry)))
           (state (export-gfm-test-state-for-preparation preparation))
           (reference
             (export-gfm-test-view
              :footnote-reference
              :properties '(:label "complex" :type :standard)))
           (anonymous
             (export-gfm-test-view
              :footnote-reference :properties '(:type :inline)
              :children (list (export-gfm-test-view
                               :entity :properties '(:utf-8 "inline")))))
           (unresolved
             (export-gfm-test-view
              :footnote-reference
              :properties '(:label "missing" :type :standard))))
      (with-soft-assertions
        (expect (org-kit::export-gfm-render-footnote-reference state reference)
                :to-equal
                "<sup><a id=\"fnr.complex\" class=\"footref\" href=\"#fn.complex\" role=\"doc-backlink\">1</a></sup>")
        (expect (org-kit::export-gfm-render-footnote-reference state reference)
                :to-equal
                "<sup><a id=\"fnr.complex.1\" class=\"footref\" href=\"#fn.complex\" role=\"doc-backlink\">1</a></sup>")
        (expect (org-kit::export-gfm-render-footnote-reference state anonymous)
                :to-equal "inline")
        (expect (org-kit::export-gfm-render-footnote-reference state unresolved)
                :to-equal "missing")
        (expect (org-kit::export-gfm-render-footnote-definition state entry)
                :to-equal
                (format nil
                        "<sup><a id=\"fn.1\" class=\"footnum\" href=\"#fnr.1\">1</a></sup> note~%~%---"))
        (expect (search "## Footnotes" (org-kit::export-gfm-render-footnotes state))
                :to-be-truthy)
        (expect (search
                 (concatenate 'string (string #\Newline) (string #\Newline)
                              "<sup><a id=\"fn.2\"")
                 (org-kit::export-gfm-render-footnotes state))
                :to-be-truthy)
        (expect (org-kit::export-gfm-render-footnotes
                 (export-gfm-test-state))
                :to-be nil))))

  (it "trims split terminal newlines from escaped GFM literal parts"
    (let* ((first-part (format nil "body~%"))
           (last-part (string #\Newline))
           (parts (list (list first-part 0 (length first-part))
                        (list last-part 0 (length last-part))))
           (nonterminal-parts
             (list (list first-part 0 (length first-part))
                   (list "tail" 0 4))))
      (with-soft-assertions
        (expect (org-kit::export-gfm-escape-literal-parts
                 parts :trim-terminal-newline-p t)
                :to-equal "body")
        (expect (org-kit::export-gfm-escape-literal-parts
                 nonterminal-parts :trim-terminal-newline-p t)
                :to-equal (format nil "body~%tail")))))

  (it "validates the complete explicit URL for bracket destinations"
    (let* ((state (export-gfm-test-state))
           (safe (export-gfm-test-attached-node
                  :link "[[https://example.test/path]]"
                  :properties '(:type "https" :path "//example.test/path"
                                :format :bracket)))
           (escaped (export-gfm-test-attached-node
                     :link "[[https://example.test/a\\b]]"
                     :properties
                     '(:type "https" :path "//example.test/a\\b"
                       :format :bracket))))
      (with-soft-assertions
        (expect (org-kit::export-gfm-source-range-matches-parts-p
                 "https://example.test/path" 0 25
                 '("https" ":" "//example.test/path"))
                :to-be t)
        (expect (org-kit::export-gfm-source-range-matches-parts-p
                 "https://example.test/paths" 0 26
                 '("https" ":" "//example.test/path"))
                :to-be nil)
        (expect (org-kit::export-gfm-source-range-matches-parts-p
                 "https://example.test/docs" 0 25
                 '("https" ":" "//example.test/path"))
                :to-be nil)
        (expect (org-kit::export-gfm-source-range-matches-parts-p
                 "" 0 0 nil)
                :to-be t)
        (expect (org-kit::export-gfm-link-destination state safe)
                :to-equal "<https://example.test/path>")
        (expect (org-kit::export-gfm-link-destination state escaped)
                :to-be nil))))

  (it "checks URL, target, and destination boundaries with backslashes and entities"
    (let* ((state (export-gfm-test-state))
           (unsafe-http-path (concatenate 'string "unsafe" (string #\\) "path"))
           (unsafe-target-id (concatenate 'string "unsafe" (string #\\) "target"))
           (control-target-id
             (concatenate 'string "unsafe" (string (code-char 7)) "target"))
           (safe-link
             (export-gfm-test-attached-node
              :link "safe/path" :properties '(:path "safe/path")))
           (entity-link
             (export-gfm-test-attached-node
              :link "obfuscated" :properties '(:path "javascript&colon;alert")))
           (unknown-scheme-link
             (export-gfm-test-attached-node
              :link "mystery:resource"
              :properties '(:type "mystery" :path "resource")))
           (backslash-http-link
             (export-gfm-test-attached-node
              :link "https://example.test/unsafe"
              :properties (list :type "https" :path unsafe-http-path)))
           (missing-http-path-link
             (export-gfm-test-attached-node
              :link "https" :properties '(:type "https")))
           (unsafe-scheme-link
             (export-gfm-test-attached-node
              :link "javascript:alert(1)"
              :properties '(:path "javascript:alert(1)")))
           (expansion-source
             (format nil
                     "#+LINK: safe javascript&colon;%s~%~%[[safe:payload]]"))
           (expansion-document (parse-org expansion-source))
           (expansion-link
             (find-if (lambda (node) (eq (node-type node) :link))
                      (export-gfm-test-nodes expansion-document)))
           (expansion-state (export-gfm-test-state expansion-source)))
      (setf (node-property expansion-link :type) "safe"
            (node-property expansion-link :path) "payload"
            (node-property expansion-link :format) :bracket)
      (with-soft-assertions
        (expect (org-kit::export-gfm-destination-parts-character
                 '("a" "bc") 2)
                :to-be #\c)
        (expect (org-kit::export-gfm-destination-parts-position
                 '("safe" "x&") (lambda (character) (char= character #\&)))
                :to-equal 5)
        (expect (org-kit::export-gfm-destination-parts-explicit-scheme-end
                 '("https" ":" "path"))
                :to-equal 5)
        (expect (org-kit::export-gfm-destination-parts-explicit-scheme-end
                 '(":" "path"))
                :to-be nil)
        (expect (org-kit::export-gfm-destination-parts-explicit-scheme-end
                 '("bad scheme" ":" "path"))
                :to-be nil)
        (expect (org-kit::export-gfm-destination-parts-prefix-equal-p
                 '("//host") "//")
                :to-be t)
        (expect (org-kit::export-gfm-destination-parts-prefix-equal-p
                 '("/") "//")
                :to-be nil)
        (expect (org-kit::export-gfm-destination-parts-safe-url-p
                 '("safe") '("safe"))
                :to-be t)
        (expect (org-kit::export-gfm-destination-parts-safe-url-p
                 (list control-target-id) '("safe"))
                :to-be nil)
        (expect (org-kit::export-gfm-destination-parts-safe-url-p
                 '("safe") (list control-target-id))
                :to-be nil)
        (expect (org-kit::export-gfm-destination-parts-safe-url-p
                 '("safe") '(""))
                :to-be nil)
        (expect (org-kit::export-gfm-destination-parts-safe-url-p
                 '(" path") '(" path"))
                :to-be nil)
        (expect (org-kit::export-gfm-destination-parts-safe-url-p
                 '("//host") '("//host"))
                :to-be nil)
        (expect (org-kit::export-gfm-destination-parts-safe-url-p
                 '("https" ":" "\\path") '("https" ":" "\\path"))
                :to-be nil)
        (expect (org-kit::export-gfm-destination-parts-safe-url-p
                 '("ftp" ":" "//host") '("ftp" ":" "//host"))
                :to-be nil)
        (expect (org-kit::export-gfm-preflight-link-destination
                 "https" "safe/path" nil nil
                 '("safe/path") '("safe/path"))
                :to-be-truthy)
        (expect (org-kit::export-gfm-preflight-link-destination
                 "https" unsafe-http-path nil nil
                 (list unsafe-http-path) (list unsafe-http-path))
                :to-be nil)
        (expect (org-kit::export-gfm-preflight-link-destination
                 "https" "&colon;alert(1)" nil nil
                 '("&colon;alert(1)") '("&colon;alert(1)"))
                :to-be nil)
        (expect (org-kit::export-gfm-preflight-link-destination
                 "https" "a&colon;b" nil nil
                 '("https" ":" "a&colon;b")
                 '("https" ":" "a&colon;b"))
                :to-be-truthy)
        (expect (org-kit::export-gfm-preflight-link-destination
                 "custom-id" nil "section" nil
                 '("#" "section") '("#" "section"))
                :to-be-truthy)
        (expect (org-kit::export-gfm-preflight-link-destination
                 "custom-id" nil unsafe-target-id nil
                 '("#" "unsafe\\target") '("#" "unsafe\\target"))
                :to-be nil)
        (expect (org-kit::export-gfm-preflight-link-destination
                 "fuzzy" nil control-target-id nil
                 (list "#" control-target-id) (list "#" control-target-id))
                :to-be nil)
        (expect (org-kit::export-gfm-preflight-link-destination
                 "fuzzy" nil nil nil '("plain") '("plain"))
                :to-be nil)
        (expect (org-kit::export-gfm-link-destination state safe-link)
                :to-equal "<safe/path>")
        (expect (org-kit::export-gfm-link-destination state entity-link)
                :to-be nil)
        (expect (org-kit::export-gfm-link-destination state unknown-scheme-link)
                :to-be nil)
        (expect (org-kit::export-gfm-link-destination state backslash-http-link)
                :to-be nil)
        (expect (org-kit::export-gfm-link-destination state missing-http-path-link)
                :to-be nil)
        (expect (org-kit::export-gfm-link-destination state unsafe-scheme-link)
                :to-be nil)
        (expect (org-kit::export-gfm-link-destination
                 expansion-state expansion-link)
                :to-be nil)
        (expect (org-kit::export-gfm-link-abbreviation-prefix-unsafe-p
                 expansion-link "safe" nil)
                :to-be t))))

  (it "preserves entity text in a path after an allowed URL scheme"
    (let* ((source
             (format nil
                     "#+OPTIONS: toc:nil num:nil~%[[https:a&colon;b][x]]"))
           (actual (export-org (parse-org source) :gfm)))
      (expect actual :to-equal "[x](<https:a&amp;colon;b>)")))

  (it "preserves dot-relative file links"
    (let* ((source
             (format nil
                     "#+OPTIONS: toc:nil num:nil~%[[./chapter.org][dot]] [[../chapter.org][parent]]"))
           (actual (export-org (parse-org source) :gfm)))
      (expect actual :to-equal
              "[dot](<./chapter.org>) [parent](<../chapter.org>)")))

  (it "keeps programmatic GFM links without a destination as labels"
    (let ((outputs
            (loop for properties in
                  '((:format :bracket :type "fuzzy")
                    (:format :bracket :type "file" :path "" :raw-link "file:"))
                  collect
                  (let* ((source "[[file:][x]]")
                         (document (make-document source))
                         (label (make-node :plain-text :begin 9 :end 10))
                         (link (make-node :link :begin 0 :end 12
                                          :properties properties
                                          :children (list label)))
                         (paragraph (make-node :paragraph
                                               :children (list link)))
                         (section (make-node :section
                                             :children (list paragraph)))
                         (root (make-node :org-data :children (list section))))
                    (setf (document-root document) root)
                    (export-org document :gfm)))))
      (expect outputs :to-equal '("x" "x"))))

  (it "exports an empty programmatic inline code node through export-org"
    (let* ((document (make-document ""))
           (code (make-node :code))
           (paragraph (make-node :paragraph :children (list code)))
           (root (make-node :org-data :children (list paragraph))))
      (setf (document-root document) root)
      (expect (export-org document :gfm) :to-equal "``")))

  (it "dispatches list, table, source, and uncommon view branches"
    (let* ((state (export-gfm-test-state))
           (task
             (export-gfm-test-view
              :item :properties '(:checkbox :trans)
              :children (list (export-gfm-test-view :horizontal-rule))))
           (nested
             (export-gfm-test-view
              :item :children
              (list (export-gfm-test-view :horizontal-rule)
                    (export-gfm-test-view
                     :quote-block :children
                     (list (export-gfm-test-view :horizontal-rule)
                           (export-gfm-test-view :horizontal-rule))))))
           (ordered
             (export-gfm-test-view
              :plain-list :properties '(:type :ordered)
              :children (list nested)))
           (empty-list-item (export-gfm-test-view :item))
           (empty-table (export-gfm-test-view :table))
           (table
             (export-gfm-test-view
              :table
              :children
              (list (export-gfm-test-view
                     :table-row :properties '(:type :standard)
                     :children (list (export-gfm-test-view
                                     :table-cell :properties '(:value "head"))))
                    (export-gfm-test-view :table-row
                                          :properties '(:type :standard)))))
           (table-row
             (export-gfm-test-view
              :table-row :children
              (list (export-gfm-test-view
                     :entity :properties '(:utf-8 "|")))))
           (source-both
             (export-gfm-test-view
              :src-block :properties '(:value "code")
              :payload '(:exports :both)
              :children (list (export-gfm-test-view :horizontal-rule))))
           (source-none
             (export-gfm-test-view :src-block
                                   :properties '(:value "hidden")
                                   :payload '(:exports :none)))
           (source-code
             (export-gfm-test-view :src-block
                                   :properties '(:value "code")
                                   :payload '(:exports :code)))
           (source-results
             (export-gfm-test-view :src-block
                                   :properties '(:value "hidden")
                                   :payload '(:exports :results)
                                   :children (list (export-gfm-test-view
                                                    :horizontal-rule))))
           (section-only-footnote
             (export-gfm-test-view
              :section :children
              (list (export-gfm-test-view :footnote-definition))))
           (cell-line-break (export-gfm-test-view :line-break))
           (unsupported-inline (export-gfm-test-view :diary-sexp)))
      (with-soft-assertions
        (expect (org-kit::export-gfm-render-view state task)
                :to-equal "- [-] ---")
        (expect (org-kit::export-gfm-render-view state empty-list-item)
                :to-equal "- ")
        (expect (org-kit::export-gfm-render-view state ordered)
                :to-equal (format nil "1. ---~%  > ---~%  > ~%  > ---"))
        (expect (org-kit::export-gfm-render-view state empty-table)
                :to-equal "")
        (expect (org-kit::export-gfm-render-view state table)
                :to-equal (format nil "| head |~%| --- |~%|  |"))
        (expect (org-kit::export-gfm-render-view state table-row)
                :to-equal "\\|")
        (expect (org-kit::export-gfm-render-view state source-both)
                :to-equal
                (concatenate 'string "~~~" (string #\Newline) "code"
                             (string #\Newline) "~~~"
                             (string #\Newline) (string #\Newline) "---"))
        (expect (org-kit::export-gfm-render-view state source-none)
                :to-equal "")
        (expect (org-kit::export-gfm-render-view state source-code)
                :to-equal
                (concatenate 'string "~~~" (string #\Newline) "code"
                             (string #\Newline) "~~~"))
        (expect (org-kit::export-gfm-render-view state source-results)
                :to-equal "---")
        (expect (org-kit::export-gfm-view-trailing-post-blank
                 section-only-footnote)
                :to-be 0)
        (expect (org-kit::export-gfm-render-inline-view
                 state cell-line-break :table-cell-p t)
                :to-equal "<br>")
        (expect (org-kit::export-gfm-render-inline-view state unsupported-inline)
                :to-equal "")
        (expect (org-kit::export-gfm-render-view state unsupported-inline)
                :to-equal ""))))

  (it "renders unsafe link paths as labels and rejects control characters"
    (let* ((source (export-gfm-test-policy-source "links.org"))
           (document (parse-org source))
           (links (remove-if-not
                   (lambda (node) (eq (node-type node) :link))
                   (export-gfm-test-nodes document)))
           (paths (mapcar (lambda (node) (node-property node :path)) links))
           (control (code-char 1))
           (unsafe-path
             (concatenate 'string "https://example.test/" (string control)
                          "blocked"))
           (control-source
             (concatenate 'string "Before [[" unsafe-path
                          "][Control label]] after"))
           (control-document (parse-org control-source))
           (control-link
             (find-if (lambda (node) (eq (node-type node) :link))
                      (export-gfm-test-nodes control-document)))
           (control-output (export-org control-document :gfm)))
      (with-soft-assertions
        (expect (typep control-link 'node) :to-be t)
        (expect (some (lambda (path) (search "javascript:" path)) paths)
                :to-be-truthy)
        (expect (some (lambda (path) (and (>= (length path) 2)
                                          (string= "//" path :end2 2)))
                      paths)
                :to-be-truthy)
        (expect (some (lambda (path) (find #\\ path)) paths)
                :to-be-truthy)
        (when (node-p control-link)
          (expect (node-property control-link :type) :to-equal "https")
          (expect (node-property control-link :path)
                  :to-equal (subseq unsafe-path (length "https:")))
          (expect (search (string control) (node-text control-link))
                  :to-be-truthy))
        (expect control-output :to-equal "Before Control label after"))))

  (it "uses deterministic generated heading IDs and the default table of contents"
    (let* ((document (parse-org (format nil "* Stable generated ID~%Body")))
           (first (export-org document :gfm))
           (second (export-org document :gfm)))
      (with-soft-assertions
        (expect first :to-equal second)
        (expect (search "- [Stable generated ID](<#org-stable-generated-id>)"
                        first)
                :to-be-truthy)
        (expect (search "<a id=\"org-stable-generated-id\"></a>" first)
                :to-be-truthy))))

  (it "keeps raw GFM constructs opt-in and literal HTML-like text escaped"
    (let* ((source (export-gfm-test-policy-source "raw.org"))
           (document (parse-org source))
           (disabled (export-org document :gfm))
           (enabled (export-org document :gfm :allow-raw-html t)))
      (with-soft-assertions
        (expect (search "\\<br\\>" disabled) :to-be-truthy)
        (expect (search (concatenate 'string
                                     "~~~" (string #\Newline)
                                     "<aside>raw & content</aside>"
                                     (string #\Newline) "~~~")
                        disabled)
                :to-be-truthy)
        (expect (search "~~~" disabled) :to-be-truthy)
        (expect (search "<aside>raw & content</aside>" enabled)
                :to-be-truthy)
        (expect (search "<u>snippet & text</u>" enabled) :to-be-truthy)
        (expect (search "<strong>other backend</strong>" enabled)
                :to-be nil))))

  (it "supports string, octet, and stream inputs and output representations"
    (let* ((source "日本語 export")
           (source-octets
             (cl-codec-kit:string-to-octets source :encoding :utf-8))
           (string-document (parse-org source))
           (octet-document (parse-org source-octets))
           (character-stream-document
             (parse-org (make-string-input-stream source)))
           (octet-stream-document
             (parse-org
              (make-instance 'export-gfm-test-octet-input-stream
                             :octets source-octets)))
           (string-output (export-org string-document :gfm))
           (octet-output (export-org octet-document :gfm :output-type :octets))
           (character-stream (make-string-output-stream))
           (character-result
             (export-org character-stream-document :gfm
                         :output-type :stream :stream character-stream))
           (binary-stream
             (make-instance 'export-core-test-binary-output-stream
                            :element-type '(unsigned-byte 8)))
           (binary-result
             (export-org octet-stream-document :gfm
                         :output-type :stream :stream binary-stream)))
      (with-soft-assertions
        (expect (document-text octet-document) :to-equal source)
        (expect (document-text character-stream-document) :to-equal source)
        (expect (document-text octet-stream-document) :to-equal source)
        (expect octet-output
                :to-equalp (cl-codec-kit:string-to-octets
                            string-output :encoding :utf-8))
        (expect (eq character-result character-stream) :to-be t)
        (expect (get-output-stream-string character-stream)
                :to-equal string-output)
        (expect (eq binary-result binary-stream) :to-be t)
        (expect (export-core-test-stream-octets binary-stream)
                :to-equalp (cl-codec-kit:string-to-octets
                            string-output :encoding :utf-8)))))

  (it "checks output length and UTF-8 byte limits through the GFM API"
    (let* ((source "é")
           (baseline (export-org (parse-org source) :gfm))
           (baseline-octets
             (cl-codec-kit:string-to-octets baseline :encoding :utf-8))
           (below-length
             (export-gfm-test-catch-error
              (lambda ()
                (export-org
                 (parse-org source
                            :limits (make-limits :max-output-length
                                                 (1- (length baseline))))
                 :gfm))))
           (at-length
             (export-org
              (parse-org source :limits (make-limits :max-output-length
                                                      (length baseline)))
              :gfm))
           (above-length
             (export-org
              (parse-org source :limits (make-limits :max-output-length
                                                      (1+ (length baseline))))
              :gfm))
           (below-octets
             (export-gfm-test-catch-error
              (lambda ()
                (export-org
                 (parse-org source
                            :limits (make-limits :max-output-octets
                                                 (1- (length baseline-octets))))
                 :gfm :output-type :octets))))
           (at-octets
             (export-org
              (parse-org source :limits (make-limits :max-output-octets
                                                      (length baseline-octets)))
              :gfm :output-type :octets))
           (above-octets
             (export-org
              (parse-org source :limits (make-limits :max-output-octets
                                                      (1+ (length baseline-octets))))
              :gfm :output-type :octets))
           (failed-stream (make-string-output-stream))
           (stream-limit-error
             (export-gfm-test-catch-error
              (lambda ()
                (export-org
                 (parse-org source :limits (make-limits :max-output-length 0))
                 :gfm :output-type :stream :stream failed-stream)))))
      (with-soft-assertions
        (expect (typep below-length 'org-resource-limit-error) :to-be t)
        (when (typep below-length 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name below-length)
                  :to-be :max-output-length)
          (expect (org-resource-limit-error-actual below-length)
                  :to-be (length baseline)))
        (expect at-length :to-equal baseline)
        (expect above-length :to-equal baseline)
        (expect (typep below-octets 'org-resource-limit-error) :to-be t)
        (when (typep below-octets 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name below-octets)
                  :to-be :max-output-octets)
          (expect (org-resource-limit-error-actual below-octets)
                  :to-be (length baseline-octets)))
        (expect at-octets :to-equalp baseline-octets)
        (expect above-octets :to-equalp baseline-octets)
        (expect (typep stream-limit-error 'org-resource-limit-error)
                :to-be t)
        (expect (get-output-stream-string failed-stream) :to-equal ""))))

  (it "uses final output length for a GFM link with a rejected destination"
    (let* ((source (format nil
                           "#+OPTIONS: toc:nil num:nil~%[[file:a][x]]"))
           (baseline (export-org (parse-org source) :gfm))
           (at-limit
             (export-org
              (parse-org source
                         :limits (make-limits :max-output-length 1))
              :gfm))
           (below-limit
             (export-gfm-test-catch-error
              (lambda ()
                (export-org
                 (parse-org source
                            :limits (make-limits :max-output-length 0))
                 :gfm)))))
      (with-soft-assertions
        (expect baseline :to-equal "x")
        (expect at-limit :to-equal baseline)
        (expect (typep below-limit 'org-resource-limit-error) :to-be t)
        (when (typep below-limit 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name below-limit)
                  :to-be :max-output-length)
          (expect (org-resource-limit-error-actual below-limit)
                  :to-be 1)))))

  (it "does not budget a rejected expanded GFM abbreviation"
    (let* ((source
             (format nil
                     "#+OPTIONS: toc:nil num:nil~%#+LINK: danger javascript&colon;%s~%[[danger:alert][x]]"))
           (actual
             (export-org
              (parse-org source :limits (make-limits :max-output-length 1))
              :gfm)))
      (expect actual :to-equal "x")))

  (it-sequential "rejects an unsafe large GFM abbreviation before copying its path"
    (let* ((payload (make-string (* 256 1024) :initial-element #\x))
           (source
             (format nil
                     "#+OPTIONS: toc:nil num:nil~%#+LINK: danger javascript:%s~%[[danger:~A][x]]"
                     payload))
           (document
             (parse-org source :limits (make-limits :max-output-length 1)))
           (link
             (find-if (lambda (node) (eq (node-type node) :link))
                      (export-gfm-test-nodes document)))
           (input-length (length source)))
      (expect (export-org document :gfm) :to-equal "x")
      (let* ((before (progn (sb-ext:gc :full t)
                            (sb-ext:get-bytes-consed)))
             (actual (export-org document :gfm))
             (allocated (- (sb-ext:get-bytes-consed) before)))
        (let* ((baseline-source
                 (format nil
                         "#+OPTIONS: toc:nil num:nil~%#+LINK: danger javascript:%s~%[[danger:~A][x]]"
                         "x"))
               (baseline-document
                 (parse-org baseline-source
                            :limits (make-limits :max-output-length 1)))
               (baseline-warmup
                 (let ((*export-baseline-current-test* nil))
                   (export-org baseline-document :gfm)))
               (baseline-before
                 (progn (sb-ext:gc :full t) (sb-ext:get-bytes-consed)))
               (baseline-actual
                 (let ((*export-baseline-current-test* nil))
                   (export-org baseline-document :gfm)))
               (baseline-allocated
                 (- (sb-ext:get-bytes-consed) baseline-before))
               (baseline-input-length (length baseline-source)))
          (declare (ignore baseline-warmup))
          (with-soft-assertions
            (expect actual :to-equal "x")
            (expect (node-property link :type) :to-equal "fuzzy")
            (expect (node-property link :format) :to-be :bracket)
            (expect (>= (length (node-property link :path))
                        (* 256 1024))
                    :to-be t)
            (expect (> input-length (* 256 1024)) :to-be t)
            (expect baseline-actual :to-equal "x")
            (expect (< (- allocated baseline-allocated)
                       (floor (- input-length baseline-input-length) 2))
                    :to-be t))))))

  (it "preserves safe entity fragments and custom-id fuzzy links"
    (let ((fragment-cases
            '(("a&b" "[x](<#a&amp;b>)")
              ("a&colon;b" "[x](<#a&amp;colon;b>)")
              ("a&amp;b" "[x](<#a&amp;amp;b>)"))))
      (with-soft-assertions
        (dolist (case fragment-cases)
          (destructuring-bind (target expected) case
            (let* ((source
                     (format nil
                             "#+OPTIONS: toc:nil num:nil~%[[#~A][x]]"
                             target))
                   (actual (export-org (parse-org source) :gfm)))
              (expect actual :to-equal expected))))))
    (let* ((source
             (format nil
                     "#+OPTIONS: toc:nil num:nil~%* Fuzzy heading~%:PROPERTIES:~%:CUSTOM_ID: a&b~%:END:~%~%[[#a&b][custom]] [[Fuzzy heading][fuzzy]]"))
           (actual (export-org (parse-org source) :gfm)))
      (with-soft-assertions
        (expect (search "[custom](<#a&amp;b>)" actual) :to-be-truthy)
        (expect (search "[fuzzy](<#org-fuzzy-heading>)" actual)
                :to-be-truthy))))

  (it "uses a safe GFM heading ID when a custom ID has unsafe characters"
    (let* ((document (make-document ""))
           (heading
             (make-node :headline
                        :properties '(:custom-id "unsafe\\id"
                                      :raw-value "Unsafe ID" :level 1)))
           (root (make-node :org-data :children (list heading))))
      (setf (document-root document) root)
      (let ((actual (export-org document :gfm)))
        (with-soft-assertions
          (expect (search "- [Unsafe ID](<#org-unsafe-id>)" actual)
                  :to-be-truthy)
          (expect (search "<a id=\"org-unsafe-id\"></a>" actual)
                  :to-be-truthy)
          (expect (find #\\ actual) :to-be nil)))))

  (it "reports invalid output, invalid raw mode, encoding, and unsupported syntax"
    (let* ((invalid-output
             (export-gfm-test-catch-error
              (lambda () (export-org (parse-org "Body") :gfm
                                     :output-type :unknown))))
           (invalid-raw-mode
             (export-gfm-test-catch-error
              (lambda () (export-org (parse-org "Body") :gfm
                                     :allow-raw-html :yes))))
           (surrogate (string (code-char #xd800)))
           (encoding-error
             (export-gfm-test-catch-error
              (lambda () (export-org (parse-org surrogate) :gfm
                                     :output-type :octets))))
           (unsupported
             (export-gfm-test-catch-error
              (lambda () (export-org (parse-org "%%(org-calendar-holiday)")
                                     :gfm)))))
      (with-soft-assertions
        (expect (typep invalid-output 'org-export-error) :to-be t)
        (when (typep invalid-output 'org-export-error)
          (expect (org-export-error-backend invalid-output) :to-be :gfm)
          (expect (org-export-error-reason invalid-output) :to-be :invalid-output))
        (expect (typep invalid-raw-mode 'type-error) :to-be t)
        (expect (typep encoding-error 'org-export-error) :to-be t)
        (when (typep encoding-error 'org-export-error)
          (expect (org-export-error-backend encoding-error) :to-be :gfm)
          (expect (org-export-error-reason encoding-error) :to-be :encoding))
        (expect (typep unsupported 'org-export-unsupported-syntax) :to-be t)
        (when (typep unsupported 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-backend unsupported)
                  :to-be :gfm)
          (expect (org-export-unsupported-syntax-feature unsupported)
                  :to-be :diary-sexp)))))

  (it-sequential "checks GFM output before allocating a large escaped text run"
    (let* ((source (concatenate 'string
                                "Body "
                                (make-string (* 256 1024)
                                             :initial-element #\x)))
           (limits (make-limits :max-output-length 8))
           (document (parse-org source :limits limits))
           (preparation (org-kit::export-prepare document :gfm))
           (input-length (length source))
           (before
             (progn
               (export-gfm-test-catch-error
                (lambda ()
                  (org-kit::export-render-preparation preparation :gfm)))
               (sb-ext:gc :full t)
               (sb-ext:get-bytes-consed)))
           (condition
             (export-gfm-test-catch-error
              (lambda ()
                (org-kit::export-render-preparation preparation :gfm))))
           (allocated (- (sb-ext:get-bytes-consed) before)))
      (with-soft-assertions
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when (typep condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name condition)
                  :to-be :max-output-length)
          (expect (org-resource-limit-error-actual condition)
                  :to-be input-length))
        (expect (< allocated (floor input-length 2)) :to-be t))))

  (it-sequential "checks GFM output before copying a large bracket link destination"
    (let* ((path (make-string (* 256 1024) :initial-element #\x))
           (source
             (concatenate 'string "[[https://budget.test/" path "][label]]"))
           (document
             (parse-org source
                        :limits (make-limits :max-output-length 128)))
           (preparation (org-kit::export-prepare document :gfm))
           (input-length (length source))
           (before
             (progn
               (export-gfm-test-catch-error
                (lambda ()
                  (org-kit::export-render-preparation preparation :gfm)))
               (sb-ext:gc :full t)
               (sb-ext:get-bytes-consed)))
           (condition
             (export-gfm-test-catch-error
              (lambda ()
                (org-kit::export-render-preparation preparation :gfm))))
           (allocated (- (sb-ext:get-bytes-consed) before)))
      (with-soft-assertions
        (expect (> input-length (* 256 1024)) :to-be t)
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when (typep condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name condition)
                  :to-be :max-output-length)
          (expect (> (org-resource-limit-error-actual condition) 128)
                  :to-be t))
        (expect (< allocated (floor input-length 2)) :to-be t))))

  (it-sequential "checks GFM output before copying inline code source text"
    (let* ((body (make-string (* 256 1024) :initial-element #\x))
           (source (concatenate 'string "Before ~" body "~ after"))
           (document
             (parse-org source
                        :limits (make-limits :max-output-length 32)))
           (preparation (org-kit::export-prepare document :gfm))
           (input-length (length source))
           (before
             (progn
               (export-gfm-test-catch-error
                (lambda ()
                  (org-kit::export-render-preparation preparation :gfm)))
               (sb-ext:gc :full t)
               (sb-ext:get-bytes-consed)))
           (condition
             (export-gfm-test-catch-error
              (lambda ()
                (org-kit::export-render-preparation preparation :gfm))))
           (allocated (- (sb-ext:get-bytes-consed) before)))
      (with-soft-assertions
        (expect (> input-length (* 256 1024)) :to-be t)
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when (typep condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name condition)
                  :to-be :max-output-length)
          (expect (> (org-resource-limit-error-actual condition) 32)
                  :to-be t))
        (expect (< allocated (floor input-length 2)) :to-be t))))

  (it-sequential "checks GFM output before copying literal link label objects"
    (let* ((body (make-string (* 256 1024) :initial-element #\x))
           (source
             (concatenate 'string "[[https://budget.test][~" body "~]]"))
           (document
             (parse-org source
                        :limits (make-limits :max-output-length 128)))
           (preparation (org-kit::export-prepare document :gfm))
           (input-length (length source))
           (before
             (progn
               (export-gfm-test-catch-error
                (lambda ()
                  (org-kit::export-render-preparation preparation :gfm)))
               (sb-ext:gc :full t)
               (sb-ext:get-bytes-consed)))
           (condition
             (export-gfm-test-catch-error
              (lambda ()
                (org-kit::export-render-preparation preparation :gfm))))
           (allocated (- (sb-ext:get-bytes-consed) before)))
      (with-soft-assertions
        (expect (> input-length (* 256 1024)) :to-be t)
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when (typep condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name condition)
                  :to-be :max-output-length)
          (expect (> (org-resource-limit-error-actual condition) 128)
                  :to-be t))
        (expect (< allocated (floor input-length 2)) :to-be t))))

  (it-sequential "checks GFM plain link output before concatenating its destination"
    (let* ((path (make-string (* 256 1024) :initial-element #\x))
           (source (concatenate 'string "https://budget.test/" path))
           (limits (make-limits :max-output-length 128))
           (document (make-document source :limits limits))
           (link (make-node :link :begin 0 :end (length source)
                            :properties (list :type "https" :path path
                                              :format :plain)))
           (root (make-node :org-data :children (list link)))
           (attached (setf (document-root document) root))
           (input-length (length source))
           (before
             (progn
               (export-gfm-test-catch-error
                (lambda () (export-org document :gfm)))
               (sb-ext:gc :full t)
               (sb-ext:get-bytes-consed)))
           (condition
             (export-gfm-test-catch-error
              (lambda () (export-org document :gfm))))
           (allocated (- (sb-ext:get-bytes-consed) before)))
      (with-soft-assertions
        (expect attached :to-be root)
        (expect (node-type link) :to-be :link)
        (expect (node-property link :format) :to-be :plain)
        (expect (node-property link :type) :to-equal "https")
        (expect (> input-length (* 256 1024)) :to-be t)
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when (typep condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name condition)
                  :to-be :max-output-length)
          (expect (> (org-resource-limit-error-actual condition) 128)
                  :to-be t))
        (expect (< allocated (floor input-length 2)) :to-be t))))
  (it "dispatches GFM block and inline nodes through derived registry callbacks"
      (let* ((document (parse-org "before *bold* after"))
             (backend :fr106-gfm-node-dispatch)
             (registered-p nil)
             (paragraph-wrapper-calls 0)
             (bold-transcoder-calls 0)
             (bold-frame-mode nil)
             (output nil))
        (unwind-protect
            (progn
              (register-derived-export-backend
               backend :gfm
               :node-wrapper
               (lambda (node view frame context render-next)
                 (declare (ignore view frame context))
                 (if (eq (node-type node) :paragraph)
                     (progn
                       (incf paragraph-wrapper-calls)
                       (funcall render-next "<paragraph>" "</paragraph>"))
                   (funcall render-next "" "")))
               :transcoders
               (list
                (cons :bold
                      (lambda (node view frame context)
                        (declare (ignore node view context))
                        (incf bold-transcoder-calls)
                        (setf bold-frame-mode (export-render-frame-mode frame))
                        "INLINE-OVERRIDE"))))
              (setf registered-p t
                    output (export-org document backend))
              (with-soft-assertions
               (expect output
                       :to-equal
                       "<paragraph>before INLINE-OVERRIDE after</paragraph>")
               (expect paragraph-wrapper-calls :to-be 1)
               (expect bold-transcoder-calls :to-be 1)
               (expect bold-frame-mode :to-be :inline)))
          (when registered-p
            (unregister-export-backend backend)))))
  (it "covers direct GFM cell rendering and two-space inline post-blank output"
    (let* ((state (export-gfm-test-state))
           (child-node (export-gfm-test-attached-node :plain-text "child|value"))
           (child-view (org-kit::%make-export-view :node child-node))
           (child-cell
             (export-gfm-test-view :table-cell :children (list child-view)))
           (value-cell
             (export-gfm-test-view :table-cell
                                   :properties '(:value "value|cell")))
           (inline-cell
             (export-gfm-test-view :table-cell
                                   :properties '(:value "inline|cell")))
           (bold-child-node
             (export-gfm-test-attached-node :plain-text "bold"))
           (bold-view
             (export-gfm-test-view
              :bold :children
              (list (org-kit::%make-export-view :node bold-child-node))
              :post-blank 2))
           (source-node
             (export-gfm-test-attached-node :timestamp "source <text>"))
           (preparation (org-kit::export-gfm-state-preparation state)))
      (with-soft-assertions
        (expect (org-kit::export-gfm-table-cell state child-cell)
                :to-equal "child\\|value")
        (expect (org-kit::export-gfm-table-cell state value-cell)
                :to-equal "value\\|cell")
        (expect (org-kit::export-gfm-render-inline-view
                 state inline-cell :table-cell-p t)
                :to-equal "inline\\|cell")
        (expect (org-kit::export-gfm-render-inline-view state bold-view)
                :to-equal "**bold**  ")
        (expect (org-kit::export-gfm-render-view state child-cell)
                :to-equal "child\\|value")
        (expect (org-kit::export-gfm-render-view-legacy state value-cell)
                :to-equal "value\\|cell")
        (expect (org-kit::export-gfm-dispatch-view
                 state bold-view :mode :inline)
                :to-equal "**bold**  ")
        (expect (org-kit::export-gfm-dispatch-view
                 state child-cell :mode :block)
                :to-equal "child\\|value")
        (expect (org-kit::export-gfm-render-node-legacy
                 state source-node nil nil)
                :to-equal "source \\<text\\>")
        (expect (org-kit::export-gfm-raw-html-enabled-p state) :to-be nil)
        (setf (org-kit::export-preparation-allow-raw-html preparation) t)
        (expect (org-kit::export-gfm-raw-html-enabled-p state) :to-be t))))
  (it "renders GFM images only from safe registered link handlers"
    (let ((backend :gfm-test-image-scheme)
          (backend-registered-p nil)
          (handler-registered-p nil))
      (unwind-protect
          (progn
            (register-derived-export-backend backend :gfm)
            (setf backend-registered-p t)
            (register-export-link-type
             backend "image-scheme"
             (lambda (path description frame context)
               (declare (ignore frame context))
               (make-export-link-result
                :kind :image :destination path :description description)))
            (setf handler-registered-p t)
            (let ((remote
                    (export-org
                     (parse-org
                      "[[image-scheme:https://cdn.example.test/remote.png][Remote image]]")
                     backend))
                  (relative
                    (export-org
                     (parse-org
                      "[[image-scheme:assets/local.png][Local image]]")
                     backend))
                  (mail
                    (export-org
                     (parse-org
                      "[[image-scheme:mailto:images@example.test][Mail image]]")
                     backend)))
              (with-soft-assertions
                (expect remote
                        :to-equal
                        "![Remote image](<https://cdn.example.test/remote.png>)")
                (expect relative
                        :to-equal "![Local image](<assets/local.png>)")
                (expect mail :to-equal "Mail image"))))
        (when handler-registered-p
          (unregister-export-link-type backend "image-scheme"))
        (when backend-registered-p
          (unregister-export-backend backend)))))
  (it "rejects scheme-obfuscating GFM link abbreviations"
    (labels ((view-for-node (node)
               (org-kit::%make-export-view
                :node node
                :children (mapcar #'view-for-node (node-children node))
                :source-document (node-document node)))
             (check-case (source-path replacement)
               (let* ((source (format nil "[[~A][Label]]" source-path))
                      (document (parse-org source))
                      (link
                        (find-if (lambda (node) (eq (node-type node) :link))
                                 (export-gfm-test-nodes document)))
                      (separator (position #\: source-path))
                      (path (if separator
                                (subseq source-path (1+ separator))
                                source-path)))
                 (setf (getf (document-settings document) :link-abbrevs)
                       (list (cons "unsafe" replacement))
                       (node-property link :type) "unsafe"
                       (node-property link :path) path
                       (node-property link :format) :bracket)
                 (let* ((preparation (org-kit::export-prepare document :gfm))
                        (state
                          (export-gfm-test-state-for-preparation preparation))
                        (view (view-for-node link)))
                   (expect
                    (org-kit::export-gfm-link-abbreviation-prefix-unsafe-p
                     link "unsafe" nil)
                    :to-be t)
                   (expect (org-kit::export-gfm-link-destination state link)
                           :to-be nil)
                   (expect (org-kit::export-gfm-render-link state view)
                           :to-equal "Label")))))
      (with-soft-assertions
        (check-case "unsafe:payload" "javascript&colon;")
        (check-case "unsafe" "javascript&colon;%s")
        (check-case "unsafe" "javascript&colon;"))))
  (it "indexes GFM link targets before derived renderer dispatch"
    (let ((backend :gfm-test-render-views-before-index)
          (registered-p nil)
          (document
            (parse-org
             (format nil
                     "#+OPTIONS: toc:nil num:nil~%* Target~%:PROPERTIES:~%:CUSTOM_ID: stable~%:END:~%~%[[#stable][jump]]")))
          (expected nil))
      (unwind-protect
          (progn
            (setf expected (export-org document :gfm))
            (register-derived-export-backend
             backend :gfm
             :renderer
             (lambda (context)
               (format nil "~{~A~^~%~%~}"
                       (remove ""
                               (mapcar (lambda (view)
                                         (export-render-view context view))
                                       (export-render-context-root-views context))
                               :test (function string=)))))
            (setf registered-p t)
            (let ((actual (export-org document backend)))
              (with-soft-assertions
                (expect actual :to-equal expected)
                (expect (search "[jump](<#stable>)" actual)
                        :not :to-be nil))))
        (when registered-p
          (unregister-export-backend backend)))))
)

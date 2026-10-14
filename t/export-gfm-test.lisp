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
         (state (org-kit::%make-export-gfm-state :preparation preparation)))
    (org-kit::export-gfm-index-state state)))

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
                  :to-be-truthy)))))

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
    (expect (org-kit::export-gfm-normalize-newlines
             (concatenate 'string "tail" (string #\Return)))
            :to-equal (concatenate 'string "tail" (string #\Newline))))

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

  (it "flattens inline objects in link labels and follows entity fallbacks"
    (let* ((text-node
             (export-gfm-test-attached-node :plain-text "first|second"))
           (text-view (org-kit::%make-export-view :node text-node))
           (italic-view
             (export-gfm-test-view :italic :children (list text-view)))
           (link-view
             (export-gfm-test-view :link :properties '(:path "destination")
                                   :children (list italic-view)))
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
           (padded-code-view
             (org-kit::%make-export-view :node padded-code-node)))
      (with-soft-assertions
        (expect (org-kit::export-gfm-literal-view-text italic-view)
                :to-equal "first|second")
        (expect (org-kit::export-gfm-link-label link-view :table-cell-p t)
                :to-equal "first\\|second")
        (expect (org-kit::export-gfm-literal-view-text entity-utf8)
                :to-equal "utf8")
        (expect (org-kit::export-gfm-literal-view-text entity-unicode)
                :to-equal "unicode")
        (expect (org-kit::export-gfm-literal-view-text entity-html)
                :to-equal "&lt;")
        (expect (org-kit::export-gfm-literal-view-text entity-empty)
                :to-equal "")
        (expect (org-kit::export-gfm-literal-view-text entity-text-view)
                :to-equal "<")
        (expect (org-kit::export-gfm-inline-code-value code-node)
                :to-equal "code")
        (expect (org-kit::export-gfm-literal-view-text padded-code-view)
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
           (state (org-kit::%make-export-gfm-state :preparation preparation)))
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
           (state (org-kit::%make-export-gfm-state :preparation preparation))
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
           (literal-link
             (export-gfm-test-view
              :link :properties '(:path "fallback/path")))
           (empty-literal-link (export-gfm-test-view :link))
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
        (expect (org-kit::export-gfm-literal-view-text literal-link)
                :to-equal "fallback/path")
        (expect (org-kit::export-gfm-literal-view-text empty-literal-link)
                :to-equal "")
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
                    (export-gfm-test-view :horizontal-rule))))
           (entry (list :label "complex" :number 1 :definition definition))
           (preparation
             (org-kit::%make-export-preparation
              :backend :gfm :root-views nil :toc nil :footnotes (list entry)))
           (state (org-kit::%make-export-gfm-state :preparation preparation))
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
        (expect (org-kit::export-gfm-render-footnotes
                 (export-gfm-test-state))
                :to-be nil))))

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
                  :to-be :diary-sexp))))))

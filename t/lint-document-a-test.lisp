(in-package #:cl-org-kit/test)

(defun lint-document-a-report-signatures (document checker)
  (mapcar (lambda (diagnostic)
            (list (org-lint-diagnostic-checker diagnostic)
                  (org-lint-diagnostic-position diagnostic)
                  (org-lint-diagnostic-message diagnostic)))
          (org-lint document :checkers (list checker))))

(defun lint-document-a-expect-finding (source checker expected)
  (expect (lint-document-a-report-signatures (parse-org source) checker)
          :to-equal expected))

(defun lint-document-a-expect-none (source checker)
  (expect (lint-document-a-report-signatures (parse-org source) checker)
          :to-equal nil))

(describe "document-only Org lint checker group A"
  (it "reports a misplaced heading sequence"
    (lint-document-a-expect-finding
     "Paragraph** Oops heading" "misplaced-heading"
     '(("misplaced-heading" 9 "Possibly misplaced heading line"))))

  (it "ignores ordinary inline emphasis for misplaced headings"
    (lint-document-a-expect-none "Paragraph *emphasis* text" "misplaced-heading"))

  (it "reports every duplicate CUSTOM_ID property"
    (lint-document-a-expect-finding
     "* A
:PROPERTIES:
:CUSTOM_ID: x
:END:
* B
:PROPERTIES:
:CUSTOM_ID: x
:END:"
     "duplicate-custom-id"
     '(("duplicate-custom-id" 18 "Duplicate CUSTOM_ID property \"x\"")
       ("duplicate-custom-id" 55 "Duplicate CUSTOM_ID property \"x\""))))

  (it "accepts a unique CUSTOM_ID property"
    (lint-document-a-expect-none
     "* A
:PROPERTIES:
:CUSTOM_ID: x
:END:" "duplicate-custom-id"))

  (it "reports every duplicate NAME keyword"
    (lint-document-a-expect-finding
     "#+NAME: x
#+BEGIN_SRC emacs-lisp
1
#+END_SRC
#+NAME: x
#+BEGIN_SRC emacs-lisp
2
#+END_SRC"
     "duplicate-name"
     '(("duplicate-name" 1 "Duplicate NAME \"x\"")
       ("duplicate-name" 46 "Duplicate NAME \"x\""))))

  (it "accepts a unique NAME keyword"
    (lint-document-a-expect-none
     "#+NAME: x
#+BEGIN_SRC emacs-lisp
1
#+END_SRC" "duplicate-name"))

  (it "reports each duplicate target"
    (lint-document-a-expect-finding
     "<<x>> and <<x>>" "duplicate-target"
     '(("duplicate-target" 1 "Duplicate target <<x>>")
       ("duplicate-target" 11 "Duplicate target <<x>>"))))

  (it "accepts a unique target"
    (lint-document-a-expect-none "<<x>>" "duplicate-target"))

  (it "reports every duplicate footnote definition"
    (lint-document-a-expect-finding
     "[fn:x] a

[fn:x] b" "duplicate-footnote-definition"
     '(("duplicate-footnote-definition" 1 "Duplicate footnote definition \"x\"")
       ("duplicate-footnote-definition" 11 "Duplicate footnote definition \"x\""))))

  (it "accepts a unique footnote definition"
    (lint-document-a-expect-none "[fn:x] a" "duplicate-footnote-definition"))

  (it "reports an orphaned affiliated keyword"
    (lint-document-a-expect-finding
     "#+CAPTION: orphan" "orphaned-affiliated-keywords"
     '(("orphaned-affiliated-keywords" 1 "Orphaned affiliated keyword: \"CAPTION\""))))

  (it "accepts an affiliated keyword attached to a block"
    (lint-document-a-expect-none
     "#+CAPTION: caption
#+BEGIN_SRC emacs-lisp
1
#+END_SRC"
     "orphaned-affiliated-keywords"))

  (it "reports an independent keyword immediately before affiliated metadata"
    (lint-document-a-expect-finding
     "#+TITLE: title
#+CAPTION: cap
#+BEGIN_SRC emacs-lisp
1
#+END_SRC"
     "combining-keywords-with-affiliated"
     '(("combining-keywords-with-affiliated" 1
       "Independent keyword TITLE may be confused with affiliated keywords below"))))

  (it "accepts an independent keyword separated by a blank line"
    (lint-document-a-expect-none
     "#+TITLE: title

#+CAPTION: cap
#+BEGIN_SRC emacs-lisp
1
#+END_SRC"
     "combining-keywords-with-affiliated"))

  (it "reports an obsolete affiliated keyword on an attached element"
    (lint-document-a-expect-finding
     "#+DATA: x
#+BEGIN_SRC emacs-lisp
1
#+END_SRC"
     "obsolete-affiliated-keywords"
     '(("obsolete-affiliated-keywords" 1
       "Obsolete affiliated keyword: \"DATA\".  Use \"NAME\" instead"))))

  (it "does not report an obsolete keyword separated from the next element"
    (lint-document-a-expect-none
     "#+DATA: x

#+BEGIN_SRC emacs-lisp
1
#+END_SRC"
     "obsolete-affiliated-keywords"))

  (it "reports a deprecated export block"
    (lint-document-a-expect-finding
     "#+BEGIN_HTML
x
#+END_HTML" "deprecated-export-blocks"
     '(("deprecated-export-blocks" 1
       "Deprecated syntax for export block.  Use \"BEGIN_EXPORT HTML\" instead"))))

  (it "accepts the current export block syntax"
    (lint-document-a-expect-none
     "#+BEGIN_EXPORT HTML
x
#+END_EXPORT" "deprecated-export-blocks"))

  (it "reports a source block without a language"
    (lint-document-a-expect-finding
     "#+BEGIN_SRC
x
#+END_SRC" "missing-language-in-src-block"
     '(("missing-language-in-src-block" 1 "Missing language in source block"))))

  (it "accepts a source block with a language"
    (lint-document-a-expect-none
     "#+BEGIN_SRC emacs-lisp
x
#+END_SRC" "missing-language-in-src-block"))

  (it "reports an export block without a backend"
    (lint-document-a-expect-finding
     "#+BEGIN_EXPORT
x
#+END_EXPORT" "missing-backend-in-export-block"
     '(("missing-backend-in-export-block" 1 "Missing backend in export block"))))

  (it "accepts an export block with a backend"
    (lint-document-a-expect-none
     "#+BEGIN_EXPORT HTML
x
#+END_EXPORT" "missing-backend-in-export-block"))

  (it "reports a Babel call block with no call name"
    (lint-document-a-expect-finding
     "#+CALL:" "invalid-babel-call-block"
     '(("invalid-babel-call-block" 1 "Invalid syntax in babel call block"))))

  (it "accepts a Babel call block with a call name"
    (lint-document-a-expect-none "#+CALL: example" "invalid-babel-call-block"))

  (it "reports repeated CATEGORY setup keywords"
    (lint-document-a-expect-finding
     "#+CATEGORY: first
#+CATEGORY: second" "deprecated-category-setup"
     '(("deprecated-category-setup" 19 "Spurious CATEGORY keyword.  Set :CATEGORY: property instead"))))

  (it "accepts a single CATEGORY setup keyword"
    (lint-document-a-expect-none "#+CATEGORY: only" "deprecated-category-setup"))

  (it "reports a coderef link with no matching code label"
    (lint-document-a-expect-finding
     "[[(missing)]]" "invalid-coderef-link"
     '(("invalid-coderef-link" 1 "Unknown coderef \"missing\""))))

  (it "accepts a coderef link found in a source block"
    (lint-document-a-expect-none
     "#+BEGIN_SRC emacs-lisp
1 (ref:here)
#+END_SRC
[[(here)]]"
     "invalid-coderef-link"))

  (it "reports a custom ID link without a local custom ID"
    (lint-document-a-expect-finding
     "[[#missing]]" "invalid-custom-id-link"
     '(("invalid-custom-id-link" 1 "Unknown custom ID \"missing\""))))

  (it "accepts a link to a local custom ID"
    (lint-document-a-expect-none
     "* Heading
:PROPERTIES:
:CUSTOM_ID: here
:END:
[[#here]]"
     "invalid-custom-id-link"))

  (it "reports a fuzzy link without a local destination"
    (lint-document-a-expect-finding
     "[[missing]]" "invalid-fuzzy-link"
     '(("invalid-fuzzy-link" 1 "Unknown fuzzy location \"missing\""))))

  (it "ignores a custom URI scheme that the AST classifies as fuzzy"
    (lint-document-a-expect-none "[[myapp:resource]]" "invalid-fuzzy-link"))

  (it "reports a closing bracket immediately after a link"
    (lint-document-a-expect-finding
     "[[x]]]" "trailing-bracket-after-link"
     '(("trailing-bracket-after-link" 1 "Trailing ']' after link end"))))

  (it "accepts a link without a trailing bracket"
    (lint-document-a-expect-none "[[x]]" "trailing-bracket-after-link"))

  (it "reports an unclosed bracket in a parser-produced link description"
    (lint-document-a-expect-finding
     "[[x][bad \\[desc]]" "unclosed-brackets-in-link-description"
     '(("unclosed-brackets-in-link-description" 1
       "No closing ']' matches '[' in link description: bad \\[desc"))))

  (it "accepts a link description without brackets"
    (lint-document-a-expect-none
     "[[x][balanced text]]" "unclosed-brackets-in-link-description"))

  (it "reports a special property stored in a property drawer"
    (lint-document-a-expect-finding
     "* A
:PROPERTIES:
:TODO: TODO
:END:"
     "special-property-in-properties-drawer"
     '(("special-property-in-properties-drawer" 18
       "Special property \"TODO\" found in a properties drawer"))))

  (it "accepts a regular property in a property drawer"
    (lint-document-a-expect-none
     "* A
:PROPERTIES:
:EFFORT: 1h
:END:"
     "special-property-in-properties-drawer"))

  (it "reports a misplaced PROPERTIES drawer"
    (lint-document-a-expect-finding
     ":PROPERTIES:
:X: y
:END:" "obsolete-properties-drawer"
     '(("obsolete-properties-drawer" 1 "Incorrect contents for PROPERTIES drawer"))))

  (it "accepts a standard headline property drawer"
    (progn
      (lint-document-a-expect-none
       "* A
:PROPERTIES:
:X: y
:END:" "obsolete-properties-drawer")
      (lint-document-a-expect-none
       "* A
SCHEDULED: <2024-01-01 Mon>
:PROPERTIES:
:X: y
:END:"
       "obsolete-properties-drawer")))

  (it "reports an invalid EFFORT duration"
    (lint-document-a-expect-finding
     "* A
:PROPERTIES:
:EFFORT: soon
:END:" "invalid-effort-property"
     '(("invalid-effort-property" 18 "Invalid effort duration format: \"soon\"")))
    (lint-document-a-expect-finding
     "* A
:PROPERTIES:
:EFFORT: 42
:END:" "invalid-effort-property"
     '(("invalid-effort-property" 18 "Invalid effort duration format: \"42\""))))

  (it "checks drawer EFFORT and ignores keyword declarations"
    (lint-document-a-expect-none
     "* A
:PROPERTIES:
:EFFORT: 1d 2:00
:END:" "invalid-effort-property")
    (lint-document-a-expect-none "#+PROPERTY: Effort 42" "invalid-effort-property"))

  (it "reports an ID property containing a double colon"
    (lint-document-a-expect-finding
     "* A
:PROPERTIES:
:ID: a::b
:END:" "invalid-id-property"
     '(("invalid-id-property" 18 "IDs should not include \"::\": \"a::b\""))))

  (it "accepts an ID property without a double colon"
    (lint-document-a-expect-none
     "* A
:PROPERTIES:
:ID: ab
:END:" "invalid-id-property"))

  (it "reports a standard footnote reference with no definition"
    (lint-document-a-expect-finding
     "a[fn:x]" "undefined-footnote-reference"
     '(("undefined-footnote-reference" 2 "Missing definition for footnote [x]"))))

  (it "accepts a standard footnote reference with a definition"
    (lint-document-a-expect-none
     "a[fn:x]

[fn:x] definition" "undefined-footnote-reference"))

  (it "reports an unreferenced footnote definition"
    (lint-document-a-expect-finding
     "[fn:x] definition" "unreferenced-footnote-definition"
     '(("unreferenced-footnote-definition" 1 "No reference for footnote definition [x]"))))

  (it "accepts a footnote definition with a reference"
    (lint-document-a-expect-none
     "a[fn:x]

[fn:x] definition" "unreferenced-footnote-definition"))

  (it "reports extraneous content in the footnote section"
    (lint-document-a-expect-finding
     "* Footnotes
Text" "extraneous-element-in-footnote-section"
     '(("extraneous-element-in-footnote-section" 1
       "Extraneous elements in footnote section are not exported"))))

  (it "accepts only footnote definitions in the footnote section"
    (lint-document-a-expect-none
     "* Footnotes
[fn:x] definition" "extraneous-element-in-footnote-section"))
  (it "matches fuzzy links to document destinations"
    (let* ((source (format nil "#+NAME: named~%#+BEGIN_SRC text~%body~%#+END_SRC~%"))
           (document (parse-org source))
           (node (find :src-block (org-kit::%lint-nodes document)
                       :key (function org-kit::node-type))))
      (expect (org-kit::%lint-a-name-keyword-offset document node "named") :to-be 0)
      (expect (org-kit::%lint-a-name-keyword-offset document node "other") :to-be nil)
      (expect (org-kit::%lint-a-fuzzy-target-present-p document "named") :to-be t))
    (let ((document (parse-org (format nil "* Heading~%<<target>>~%"))))
      (expect (org-kit::%lint-a-fuzzy-target-present-p document "Heading") :to-be t)
      (expect (org-kit::%lint-a-fuzzy-target-present-p document "*Heading") :to-be t)
      (expect (org-kit::%lint-a-fuzzy-target-present-p document "target") :to-be t)
      (expect (org-kit::%lint-a-fuzzy-target-present-p document "missing") :to-be nil))
    (lint-document-a-expect-none
     (format nil "#+RESULTS: result-name~%") "orphaned-affiliated-keywords")
    (lint-document-a-expect-none
     (format nil "* Heading~%[[Heading]]~%") "invalid-fuzzy-link")
    (lint-document-a-expect-none
     (format nil "<<target>>~%[[target]]~%") "invalid-fuzzy-link")
    (lint-document-a-expect-none
     (format nil "#+NAME: named~%#+BEGIN_SRC text~%body~%#+END_SRC~%[[named]]")
     "invalid-fuzzy-link"))
  (it "covers the document checker helper boundaries"
    (let ((text (format nil "first~C~%second" #\Return)))
      (expect (multiple-value-list
               (org-kit::%lint-a-line-bounds text 0))
              :to-equal (list 5 7)))
    (expect (multiple-value-list (org-kit::%lint-a-line-bounds "last" 0))
            :to-equal (list 4 4))
    (expect (multiple-value-list (org-kit::%lint-a-line-keyword "  #+TITLE: Title"))
            :to-equal (list "TITLE" "Title"))
    (expect (org-kit::%lint-a-line-keyword " ordinary text") :to-be nil)
    (expect (org-kit::%lint-a-obsolete-affiliated-name "HEADERS") :to-equal "HEADER")
    (expect (org-kit::%lint-a-obsolete-affiliated-name "RESULT") :to-equal "RESULTS")
    (expect (org-kit::%lint-a-obsolete-affiliated-name "DATA") :to-equal "NAME")
    (expect (not (null (org-kit::%lint-affiliated-keyword-p "NAME"))) :to-be t)
    (expect (not (null (org-kit::%lint-affiliated-keyword-p "ATTR_HTML"))) :to-be t)
    (expect (org-kit::%lint-affiliated-keyword-p "ATTR_") :to-be nil)
    (expect (org-kit::%lint-affiliated-keyword-p "ATTR_HTML+") :to-be nil)
    (expect (org-kit::%lint-outer-bracketed-p "[end]") :to-be t)
    (expect (org-kit::%lint-outer-bracketed-p "[") :to-be nil)
    (expect (org-kit::%lint-outer-bracketed-p "end]") :to-be nil)
    (expect (org-kit::%lint-a-uri-scheme-p "git+ssh:repo") :to-be t)
    (expect (org-kit::%lint-a-uri-scheme-p "plain-path") :to-be nil)
    (expect (org-kit::%lint-a-uri-scheme-p "1scheme:path") :to-be nil)
    (expect (org-kit::%lint-a-uri-scheme-p "bad_scheme:path") :to-be nil)
    (expect (org-kit::%lint-a-search-equal-p "Heading  More" "heading more") :to-be t)
    (expect (org-kit::%lint-a-search-equal-p "Heading" "Other") :to-be nil))
  (it "covers additional document helper boundaries"
  (expect (multiple-value-list (org-kit::%lint-a-line-bounds "one
two" 0))
          :to-equal (list 3 4))
  (expect (org-kit::%lint-a-line-keyword "#+TITLE") :to-be nil)
  (expect (org-kit::%lint-a-line-keyword "  ") :to-be nil)
  (expect (org-kit::%lint-a-offset-p (parse-org "x") 0) :to-be t)
  (expect (org-kit::%lint-a-offset-p (parse-org "x") -1) :to-be nil)
  (expect (org-kit::%lint-a-offset-p (parse-org "x") 2) :to-be nil)
  (expect (org-kit::%lint-affiliated-keyword-p "ATTR") :to-be nil)
  (expect (org-kit::%lint-outer-bracketed-p "[x") :to-be nil)
  (expect (org-kit::%lint-a-search-equal-p nil "heading") :to-be nil)
  (let ((document (parse-org (format nil "* Heading~%:PROPERTIES:~%:CUSTOM_ID: here~%:END:"))))
    (expect (org-kit::%lint-a-fuzzy-target-present-p document "here") :to-be t)
    (expect (org-kit::%lint-a-fuzzy-target-present-p document "*here") :to-be nil))
  (lint-document-a-expect-none
   (format nil "* Footnotes~%** COMMENT Notes~%")
   "extraneous-element-in-footnote-section"))
  (it "covers document checker helper boundaries"
    (let* ((document (parse-org "plain"))
           (node (org-kit::make-node :src-block :begin 0 :end 5)))
      (expect (org-kit::%lint-a-name-keyword-offset document node "name")
              :to-be nil))
    (let* ((document (parse-org "text"))
           (node (org-kit::make-node
                  :babel-call :begin 0 :end 4 :post-affiliated 0
                  :properties '(:call "example()" :end-header "[end]"))))
      (org-kit::append-child (org-kit::document-root document) node)
      (expect (mapcar #'org-kit::org-lint-diagnostic-message
                      (org-kit::%lint-invalid-babel-call-block document nil))
              :to-equal '("Babel call's end header must not be wrapped within brackets")))
    (let ((node (org-kit::make-node
                 :src-block :properties '(:value "plain
  (ref:label)"))))
      (expect (org-kit::%lint-a-coderef-in-block-p node "label") :to-be t))
    (let* ((document (parse-org "plain"))
           (paragraph (first (org-kit::%lint-nodes document :paragraph))))
      (setf (org-kit::node-property paragraph :results) "result-name")
      (expect (org-kit::%lint-a-fuzzy-target-present-p document "result-name")
              :to-be t))
    (lint-document-a-expect-finding
     "[[*missing]]" "invalid-fuzzy-link"
     '(("invalid-fuzzy-link" 1 "Unknown fuzzy location \"missing\"")))
    (let* ((document (parse-org "x[abc"))
           (node (org-kit::make-node
                  :link :begin 0 :end 5 :contents-begin 1 :contents-end 5)))
      (org-kit::append-child (org-kit::document-root document) node)
      (expect (mapcar #'org-kit::org-lint-diagnostic-message
                      (org-kit::%lint-unclosed-brackets-in-link-description
                       document nil))
              :to-equal '("No closing ']' matches '[' in link description: [abc")))
    (let* ((document (parse-org "* Heading"))
           (headline (first (org-kit::%lint-nodes document :headline)))
           (drawer (org-kit::make-node
                    :drawer :begin 0 :end 1 :post-affiliated 0
                    :properties '(:drawer-name "PROPERTIES"))))
      (org-kit::append-child headline drawer)
      (expect (mapcar #'org-kit::org-lint-diagnostic-message
                      (org-kit::%lint-obsolete-properties-drawer document nil))
              :to-equal '("Incorrect contents for PROPERTIES drawer"))))
)
(describe "document-only Org lint checker uncovered branches"
  (it "covers sibling, source offset, and affiliated-keyword boundaries"
    (let ((detached (org-kit::make-node :paragraph)))
      (expect (org-kit::%lint-a-next-sibling detached) :to-be nil)
      (expect (org-kit::%lint-a-previous-siblings detached) :to-equal nil))
    (let* ((before (org-kit::make-node :paragraph))
           (target (org-kit::make-node :paragraph))
           (after (org-kit::make-node :paragraph))
           (parent (org-kit::make-node :paragraph
                                       :children (list before target after))))
      (declare (ignore parent))
      (expect (org-kit::%lint-a-next-sibling target) :to-be after)
      (expect (org-kit::%lint-a-previous-siblings target) :to-equal (list before)))
    (expect (multiple-value-list (org-kit::%lint-a-line-bounds (format nil "~%") 0))
            :to-equal (list 0 1))
    (expect (org-kit::%lint-a-offset-p (parse-org "x") "0") :to-be nil)
    (let* ((source (format nil "#+TITLE: title~%#+NAME: name~%"))
           (document (parse-org source))
           (node (org-kit::make-node :src-block :begin 0 :end (length source)
                                     :post-affiliated (length source))))
      (org-kit::append-child (org-kit::document-root document) node)
      (expect (org-kit::%lint-a-name-keyword-offset document node "name")
              :to-be (search "#+NAME:" source)))
    (expect (org-kit::%lint-affiliated-keyword-p "LONG_KEY") :to-be nil)
    (lint-document-a-expect-none "#+TITLE: title" "orphaned-affiliated-keywords"))
  (it "filters synthetic block, keyword, and coderef values"
    (let* ((document (parse-org "x"))
           (block (org-kit::make-node :special-block :begin 0 :end 1
                                      :post-affiliated 0
                                      :properties '(:type "CUSTOM"))))
      (org-kit::append-child (org-kit::document-root document) block)
      (expect (org-kit::%lint-deprecated-export-blocks document nil) :to-equal '()))
    (let* ((document (parse-org "x"))
           (block (org-kit::make-node :src-block :begin 0 :end 1
                                      :post-affiliated 0
                                      :properties '(:language 1))))
      (setf (org-kit::node-children (org-kit::document-root document)) (list block))
      (expect (mapcar #'org-kit::org-lint-diagnostic-message
                      (org-kit::%lint-missing-language-in-src-block document nil))
              :to-equal '("Missing language in source block")))
    (let* ((document (parse-org "x"))
           (block (org-kit::make-node :src-block :begin 0 :end 1
                                      :properties '(:language nil))))
      (setf (org-kit::node-children (org-kit::document-root document)) (list block))
      (expect (org-kit::%lint-missing-language-in-src-block document nil) :to-equal '()))
    (let* ((document (parse-org "x"))
           (block (org-kit::make-node :export-block :begin 0 :end 1
                                      :post-affiliated 0
                                      :properties '(:type :html))))
      (setf (org-kit::node-children (org-kit::document-root document)) (list block))
      (expect (org-kit::%lint-missing-backend-in-export-block document nil)
              :to-equal '()))
    (let* ((document (parse-org "x"))
           (call (org-kit::make-node :babel-call :begin 0 :end 1
                                     :properties '(:call "example" :end-header "[end]"))))
      (setf (org-kit::node-children (org-kit::document-root document)) (list call))
      (expect (org-kit::%lint-invalid-babel-call-block document nil) :to-equal '()))
    (let* ((document (parse-org "x"))
           (call (org-kit::make-node :babel-call :begin 0 :end 1 :post-affiliated 0
                                     :properties '(:call 1 :end-header "plain"))))
      (setf (org-kit::node-children (org-kit::document-root document)) (list call))
      (expect (org-kit::%lint-invalid-babel-call-block document nil) :to-equal '()))
    (lint-document-a-expect-none "#+TITLE: title" "deprecated-category-setup")
    (let* ((document (parse-org "#+CATEGORY: title"))
           (keyword (first (org-kit::%lint-nodes document :keyword))))
      (setf (org-kit::%node-post-affiliated keyword) nil)
      (expect (org-kit::%lint-deprecated-category-setup document nil) :to-equal '()))
    (let ((block (org-kit::make-node :src-block :properties '(:value 1))))
      (expect (org-kit::%lint-a-coderef-in-block-p block "label") :to-be nil))
    (let ((block (org-kit::make-node :example-block
                                     :properties (list :value (format nil "plain~%second")))))
      (expect (org-kit::%lint-a-coderef-in-block-p block "missing") :to-be nil)))
  (it "covers invalid link targets and balanced descriptions"
    (expect (org-kit::%lint-a-uri-scheme-p nil) :to-be nil)
    (let ((document (parse-org "<<target>>")))
      (expect (org-kit::%lint-a-fuzzy-target-present-p document "") :to-be nil)
      (expect (org-kit::%lint-a-fuzzy-target-present-p document "*target") :to-be nil))
    (lint-document-a-expect-none "[[https://example.test]]" "invalid-fuzzy-link")
    (let* ((document (parse-org "x"))
           (link (org-kit::make-node :link :begin 0 :end 1
                                     :properties '(:type "fuzzy" :path ""))))
      (setf (org-kit::node-children (org-kit::document-root document)) (list link))
      (expect (mapcar #'org-kit::org-lint-diagnostic-message
                      (org-kit::%lint-invalid-fuzzy-link document nil))
              :to-equal '("Unknown fuzzy location \"\"")))
    (let* ((source "x[abc]")
           (document (parse-org source))
           (link (org-kit::make-node :link :begin 0 :end 6
                                     :contents-begin 1 :contents-end 6)))
      (setf (org-kit::node-children (org-kit::document-root document)) (list link))
      (expect (org-kit::%lint-unclosed-brackets-in-link-description document nil)
              :to-equal '())))
  (it "covers drawer, effort, ID, and footnote data boundaries"
    (let* ((document (parse-org "* Heading"))
           (headline (first (org-kit::%lint-nodes document :headline)))
           (other-drawer (org-kit::make-node :drawer :begin 0 :end 1
                                             :post-affiliated 0
                                             :properties '(:drawer-name "LOGBOOK")))
           (invalid-offset-drawer (org-kit::make-node :drawer :begin 0 :end 1
                                                      :properties '(:drawer-name "PROPERTIES"))))
      (org-kit::append-child headline other-drawer)
      (org-kit::append-child headline invalid-offset-drawer)
      (expect (org-kit::%lint-obsolete-properties-drawer document nil)
              :to-equal '()))
    (let* ((document (parse-org "x"))
           (effort (org-kit::make-node :node-property :begin 0 :end 1
                                       :properties '(:key "EFFORT" :value 1)))
           (blank-effort (org-kit::make-node :node-property :begin 0 :end 1
                                             :properties '(:key "EFFORT" :value "  "))))
      (setf (org-kit::node-children (org-kit::document-root document))
            (list effort blank-effort))
      (expect (org-kit::%lint-invalid-effort-property document nil) :to-equal '()))
    (let* ((document (parse-org "x"))
           (id (org-kit::make-node :node-property :begin 0 :end 1
                                   :properties '(:key "ID" :value 1)))
           (empty-id (org-kit::make-node :node-property :begin 0 :end 1
                                         :properties '(:key "ID" :value ""))))
      (setf (org-kit::node-children (org-kit::document-root document)) (list id empty-id))
      (expect (org-kit::%lint-invalid-id-property document nil) :to-equal '()))
    (let* ((document (parse-org "x"))
           (reference (org-kit::make-node
                       :footnote-reference :begin 0 :end 1
                       :properties '(:type "standard" :label "x"))))
      (org-kit::append-child (org-kit::document-root document) reference)
      (setf (org-kit::%node-begin reference) nil)
      (expect (org-kit::%lint-undefined-footnote-reference document nil)
              :to-equal '()))
    (let* ((source "* Footnotes")
           (document (parse-org source))
           (heading (first (org-kit::%lint-nodes document :headline)))
           (link (org-kit::make-node :link :begin 0 :end (length source))))
      (org-kit::append-child heading link)
      (expect (org-kit::%lint-extraneous-element-in-footnote-section document nil)
              :to-equal '())))

)
(describe "document-only Org lint checker remaining offset branches"
  (it "skips misplaced headings when their container offsets are absent"
    (lint-document-a-expect-none "x**" "misplaced-heading")
    (let* ((document (parse-org "x** Heading"))
           (paragraph (first (org-kit::%lint-nodes document :paragraph))))
      (setf (org-kit::%node-begin paragraph) nil)
      (expect (org-kit::%lint-misplaced-heading document nil) :to-equal '()))
    (let* ((document (parse-org "x** Heading"))
           (paragraph (first (org-kit::%lint-nodes document :paragraph))))
      (setf (org-kit::%node-end paragraph) nil)
      (expect (org-kit::%lint-misplaced-heading document nil) :to-equal '())))
  (it "requires valid adjacent keyword offsets"
    (let* ((document (parse-org "x"))
           (root (org-kit::document-root document))
           (keyword (org-kit::make-node :keyword :end 1 :post-affiliated 0))
           (next (org-kit::make-node :paragraph :end 1 :post-affiliated 1)))
      (setf (org-kit::node-children root) nil)
      (org-kit::append-child root keyword)
      (org-kit::append-child root next)
      (expect (org-kit::%lint-combining-keywords-with-affiliated document nil)
              :to-equal '()))
    (let* ((document (parse-org "x"))
           (root (org-kit::document-root document))
           (keyword (org-kit::make-node :keyword :begin 0 :end 1
                                        :post-affiliated 0))
           (next (org-kit::make-node :paragraph :begin 0 :end 1
                                     :post-affiliated 1)))
      (setf (org-kit::node-children root) nil)
      (org-kit::append-child root keyword)
      (org-kit::append-child root next)
      (setf (org-kit::%node-begin keyword) nil)
      (expect (org-kit::%lint-combining-keywords-with-affiliated document nil)
              :to-equal '()))
    (let* ((document (parse-org "x"))
           (root (org-kit::document-root document))
           (keyword (org-kit::make-node :keyword :begin 0 :end 1
                                        :post-affiliated 0))
           (next (org-kit::make-node :paragraph :end 1 :post-affiliated 1)))
      (setf (org-kit::node-children root) nil)
      (org-kit::append-child root keyword)
      (org-kit::append-child root next)
      (expect (org-kit::%lint-combining-keywords-with-affiliated document nil)
              :to-equal '())))
  (it "ignores coderef and custom ID links without source positions"
    (let* ((document (parse-org "x"))
           (root (org-kit::document-root document))
           (coderef (org-kit::make-node :link :end 1
                                        :properties '(:type "coderef" :path "missing")))
           (custom-id (org-kit::make-node :link :end 1
                                          :properties '(:type "custom-id" :path "missing"))))
      (setf (org-kit::node-children root) nil)
      (org-kit::append-child root coderef)
      (org-kit::append-child root custom-id)
      (expect (org-kit::%lint-invalid-coderef-link document nil) :to-equal '())
      (expect (org-kit::%lint-invalid-custom-id-link document nil) :to-equal '())))
  (it "skips link descriptions with invalid offsets or ranges"
    (let* ((document (parse-org "x"))
           (root (org-kit::document-root document))
           (invalid-offset (org-kit::make-node :link :end 1
                                               :contents-begin 0 :contents-end 1))
           (invalid-range (org-kit::make-node :link :begin 0 :end 1
                                              :contents-begin 0 :contents-end 1)))
      (setf (org-kit::node-children root) nil)
      (setf (org-kit::%node-contents-begin invalid-range) 1
            (org-kit::%node-contents-end invalid-range) 0)
      (org-kit::append-child root invalid-offset)
      (org-kit::append-child root invalid-range)
      (expect (org-kit::%lint-unclosed-brackets-in-link-description document nil)
              :to-equal '()))))


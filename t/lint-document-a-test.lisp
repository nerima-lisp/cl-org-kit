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
     '(("invalid-effort-property" 18 "Invalid effort duration format: \"soon\""))))

  (it "accepts a valid EFFORT duration"
    (lint-document-a-expect-none
     "* A
:PROPERTIES:
:EFFORT: 1d 2:00
:END:" "invalid-effort-property"))

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
[fn:x] definition" "extraneous-element-in-footnote-section")))

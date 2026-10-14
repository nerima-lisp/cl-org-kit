;;;; cl-org-kit.asd
(in-package #:asdf-user)

(unless (member :sbcl *features*)
  (error "cl-org-kit supports SBCL only"))

(asdf:defsystem "cl-org-kit"
  :description "Org mode reader, writer, query and format-preserving editor for Common Lisp"
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :homepage "https://github.com/nerima-lisp/cl-org-kit"
  :bug-tracker "https://github.com/nerima-lisp/cl-org-kit/issues"
  :source-control (:git "https://github.com/nerima-lisp/cl-org-kit.git")
  :depends-on ("cl-parser-kit"  ; Combinators and spans for the element and object parsers.
               "cl-regex-kit"   ; Lexical recognition of inline objects.
               "cl-codec-kit"   ; Detects and decodes octet input (BOM, UTF-16/32).
               "cl-date-kit")   ; Calendar values behind Org timestamps.
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "types")
               (:file "conditions")
               (:file "limits")
               (:file "input")
               (:file "document")
               (:file "nodes")
               (:file "org-semantics")
               (:file "outline")
               (:file "timestamp")
               (:file "time-values")
               (:file "objects")
               (:file "object-markup")
               (:file "object-timestamp")
               (:file "object-links")
               (:file "object-other")
               (:file "reader-core")
               (:file "reader-headings")
               (:file "reader-blocks")
               (:file "writer")
               (:file "reader-collections")
               (:file "table-calc-parser")
               (:file "table-calc-eval")
               (:file "edit-patches")
               (:file "table-recalculate")
               (:file "table-width")
               (:file "table-align")
               (:file "table-sort")
               (:file "table-edit")
               (:file "edit-headline")
               (:file "edit-properties")
               (:file "edit-elements")
               (:file "edit-subtree")
               (:file "statistics-cookies")
               ;; Unit E subtree relocation and sort
               (:file "edit-relocate")
               (:file "edit-sort")
               (:file "emitter-objects")
               (:file "emitter")
               (:file "query-types")
               (:file "query-eval")
               (:file "query-list")
               (:file "query-path")
               (:file "query-match")
               (:file "clock")
               (:file "edit-clock")
               (:file "edit-todo-state")
               (:file "clocktable")
               (:file "edit-clocktable")
               ;; F agenda
               ;; Unit E1 export core
               (:file "export-types")
               (:file "export-core")
               ;; Unit E2 HTML export
               (:file "export-html")
               ;; Unit E3 GFM export
               (:file "export-gfm")
               ;; Unit E4 ASCII export
               (:file "export-ascii")
               (:file "agenda"))
  :in-order-to ((test-op (test-op "cl-org-kit/test"))))

(asdf:defsystem "cl-org-kit/test"
  :description "Tests for cl-org-kit"
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :homepage "https://github.com/nerima-lisp/cl-org-kit"
  :bug-tracker "https://github.com/nerima-lisp/cl-org-kit/issues"
  :source-control (:git "https://github.com/nerima-lisp/cl-org-kit.git")
  :depends-on ("cl-org-kit"  ; The system under test.
               "cl-weave")   ; Test framework, generators, per-test timeout.
  :pathname "t"
  :serial t
  :components ((:file "package")
               (:file "public-api-test")
               (:file "types-test")
               (:file "conditions-test")
               (:file "limits-test")
               (:file "input-test")
               (:file "input-stream-limit-test")
               (:file "document-test")
               (:file "nodes-test")
               (:file "org-semantics-test")
               (:file "outline-test")
               (:file "object-parser-test")
               (:file "object-markup-test")
               (:file "timestamp-test")
               (:file "object-timestamp-test")
               (:file "object-links-test")
               (:file "object-other-test")
               (:file "reader-core-test")
               (:file "reader-headings-test")
               (:file "todo-settings-test")
               (:file "reader-blocks-test")
               (:file "reader-collections-test")
               (:file "table-calc-parser-test")
               (:file "table-calc-eval-test")
               (:file "table-recalculate-test")
               (:file "conformance-test")
               (:file "edit-patches-test")
               (:file "table-align-test")
               (:file "table-sort-test")
               (:file "table-edit-test")
               (:file "edit-headline-test")
               (:file "edit-properties-test")
               (:file "edit-elements-test")
               (:file "edit-subtree-test")
               (:file "statistics-cookies-test")
               ;; Unit E subtree relocation and sort
               (:file "edit-relocate-test")
               (:file "edit-sort-test")
               (:file "emitter-test")
               (:file "emitter-objects-test")
               (:file "emitter-property-test")
               (:file "query-types-test")
               (:file "query-eval-test")
               (:file "query-list-test")
               (:file "query-path-test")
               (:file "query-match-test")
               (:file "query-equivalence-test")
               (:file "clock-test")
               (:file "edit-clock-test")
               (:file "edit-clocktable-test")
               (:file "edit-todo-state-test")
               (:file "agenda-test")
               ;; Unit E1 export core
               (:file "clocktable-test")
               (:file "export-core-test")
               ;; Unit E2 HTML export
               (:file "export-html-test")
               ;; Unit E3 GFM export
               (:file "export-gfm-test")
               ;; Unit E4 ASCII export
               (:file "export-ascii-test"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call :cl-org-kit/test :run-tests)
               (error "cl-org-kit test suite failed"))))

(in-package #:cl-org-kit/test)

(defun lint-context-test-lines (&rest lines)
  (format nil "~{~A~^~%~}" lines))

(defun lint-context-test-findings (source checker &optional context)
  (org-kit::org-lint (parse-org source)
                     :context context
                     :checkers (list checker)))

(defun lint-context-test-finding (source checker context position message)
  (let* ((findings (lint-context-test-findings source checker context))
         (finding (first findings)))
    (with-soft-assertions
      (expect (length findings) :to-be 1)
      (expect (org-kit::org-lint-diagnostic-checker finding)
              :to-equal checker)
      (expect (org-kit::org-lint-diagnostic-position finding)
              :to-be position)
      (expect (org-kit::org-lint-diagnostic-message finding)
              :to-equal message))))

(defun lint-context-test-no-findings (source checker &optional context)
  (expect (lint-context-test-findings source checker context)
          :to-equal '()))

(describe "Org lint caller material"
  (it "uses string Babel argument names and (name . template) macro entries"
    (let ((source "#+PROPERTY: var value"))
      (lint-context-test-finding
       source "deprecated-header-syntax"
       '(:babel-header-arguments ("var"))
       1 "Deprecated syntax for \"var\".  Use header-args instead"))
    (let ((source (lint-context-test-lines
                   "#+MACRO: local $1" "{{{local(value)}}}")))
      (lint-context-test-no-findings
       source "invalid-macro-argument-and-template"
       '(:macro-definitions (("caller" . "$1"))
         :macro-templates (("custom" . "$1")))))))

(describe "deprecated-header-syntax caller context"
  (it "reports a header property from the supplied Babel argument names"
    (lint-context-test-finding
     "#+PROPERTY: var value"
     "deprecated-header-syntax"
     '(:babel-header-arguments ("var"))
     1 "Deprecated syntax for \"var\".  Use header-args instead"))
  (it "ignores names that are not deprecated common Babel arguments"
    (lint-context-test-no-findings
     "#+PROPERTY: dir value"
     "deprecated-header-syntax"
     '(:babel-header-arguments ("dir"))))
  (it "reports a deprecated header argument used as a drawer property"
    (let* ((source (lint-context-test-lines "* Code" ":PROPERTIES:"
                                            ":var: value" ":END:"))
           (position (1+ (search ":var:" source))))
      (lint-context-test-finding
       source "deprecated-header-syntax"
       '(:babel-header-arguments ("var"))
       position
       "Deprecated syntax for \"VAR\".  Use :header-args: instead")))
  (it "does not run when Babel arguments are absent"
    (lint-context-test-no-findings
     "#+PROPERTY: var value" "deprecated-header-syntax")))

(describe "suspicious-language-in-src-block caller context"
  (it "reports a source language missing from the caller's language list"
    (let ((source (lint-context-test-lines
                   "#+begin_src mystery" "value" "#+end_src")))
      (lint-context-test-finding
       source "suspicious-language-in-src-block"
       '(:source-languages ("emacs-lisp" "python"))
       1 "Unknown source block language: 'mystery'")))
  (it "accepts a caller-registered source language"
    (lint-context-test-no-findings
     (lint-context-test-lines "#+begin_src python" "print(1)" "#+end_src")
     "suspicious-language-in-src-block"
     '(:source-languages ("python"))))
  (it "does not run when source languages are absent"
    (lint-context-test-no-findings
     (lint-context-test-lines "#+begin_src mystery" "value" "#+end_src")
     "suspicious-language-in-src-block")))

(describe "invalid-id-link caller context"
  (it "reports an ID link missing from the supplied ID list"
    (lint-context-test-finding
     "[[id:missing-id]]" "invalid-id-link" '(:known-ids ("known-id"))
     1 "Unknown ID \"missing-id\""))
  (it "accepts an ID present in the supplied ID list"
    (lint-context-test-no-findings
     "[[id:known-id]]" "invalid-id-link" '(:known-ids ("known-id"))))
  (it "does not run when the ID index is absent"
    (lint-context-test-no-findings "[[id:missing-id]]" "invalid-id-link")))

(describe "link-to-local-file caller context"
  (it "reports a missing local file and passes its path to the predicate"
    (let* ((source "[[file:missing.txt]]")
           (checked-path nil)
           (context (list :file-exists-p
                          (lambda (path)
                            (setf checked-path path)
                            nil)))
           (findings (lint-context-test-findings
                      source "link-to-local-file" context)))
      (with-soft-assertions
        (expect checked-path :to-equal "missing.txt")
        (expect (length findings) :to-be 1)
        (expect (org-kit::org-lint-diagnostic-checker (first findings))
                :to-equal "link-to-local-file")
        (expect (org-kit::org-lint-diagnostic-position (first findings))
                :to-be 1)
        (expect (org-kit::org-lint-diagnostic-message (first findings))
                :to-equal "Link to non-existent local file \"missing.txt\""))))
  (it "does not report a local file confirmed by the caller"
    (lint-context-test-no-findings
     "[[file:present.txt]]" "link-to-local-file"
     (list :file-exists-p (lambda (path) (string= path "present.txt")))))
  (it "does not run when the file predicate is absent"
    (lint-context-test-no-findings
     "[[file:missing.txt]]" "link-to-local-file")))

(describe "non-existent-setupfile-parameter caller context"
  (it "reports a quoted setup file absent from the caller's file system"
    (lint-context-test-finding
     "#+SETUPFILE: \"missing.org\""
     "non-existent-setupfile-parameter"
     (list :file-exists-p (lambda (path) (string= path "present.org")))
     1 "Non-existent setup file \"missing.org\""))
  (it "accepts a setup file confirmed by the caller"
    (lint-context-test-no-findings
     "#+SETUPFILE: present.org" "non-existent-setupfile-parameter"
     (list :file-exists-p (lambda (path) (string= path "present.org")))))
  (it "does not run when the file predicate is absent"
    (lint-context-test-no-findings
     "#+SETUPFILE: missing.org" "non-existent-setupfile-parameter")))

(describe "wrong-include-link-parameter caller context"
  (it "reports an INCLUDE target that the caller says is missing"
    (lint-context-test-finding
     "#+INCLUDE: missing.org src" "wrong-include-link-parameter"
     (list :file-exists-p (lambda (path) (string= path "present.org")))
     1 "Non-existent file argument in INCLUDE keyword"))
  (it "accepts an INCLUDE target confirmed by the caller"
    (lint-context-test-no-findings
     "#+INCLUDE: present.org" "wrong-include-link-parameter"
     (list :file-exists-p (lambda (path) (string= path "present.org")))))
  (it "does not run when the file predicate is absent"
    (lint-context-test-no-findings
     "#+INCLUDE: missing.org" "wrong-include-link-parameter")))

(describe "wrong-include-link-parameter missing target"
  (it "reports an INCLUDE keyword without a location argument"
    (lint-context-test-finding
     "#+INCLUDE:" "wrong-include-link-parameter"
     (list :file-exists-p (lambda (path) (declare (ignore path)) t))
     1 "Missing location argument in INCLUDE keyword")))

(describe "wrong-include-link-parameter search target"
  (it "reports a search part rejected by the optional target predicate"
    (lint-context-test-finding
     "#+INCLUDE: target.org::*Missing" "wrong-include-link-parameter"
     (list :file-exists-p (lambda (path) (string= path "target.org"))
           :include-target-p (lambda (path search-part)
                               (and (string= path "target.org")
                                    (string= search-part "*Existing"))))
     1 "Invalid search part \"*Missing\" in INCLUDE keyword"))
  (it "accepts a search part confirmed by the optional target predicate"
    (lint-context-test-no-findings
     "#+INCLUDE: target.org::*Existing" "wrong-include-link-parameter"
     (list :file-exists-p (lambda (path) (string= path "target.org"))
           :include-target-p (lambda (path search-part)
                               (and (string= path "target.org")
                                    (string= search-part "*Existing"))))))
  (it "skips search validation when the optional target predicate is absent"
    (lint-context-test-no-findings
     "#+INCLUDE: target.org::*Missing" "wrong-include-link-parameter"
     (list :file-exists-p (lambda (path) (string= path "target.org"))))))

(describe "unknown-options-item caller context"
  (it "reports an OPTIONS item missing from the supplied string list"
    (lint-context-test-finding
     "#+OPTIONS: mystery:value" "unknown-options-item"
     '(:export-options ("html" "toc"))
     1 "Unknown OPTIONS item \"mystery\""))
  (it "accepts an item in the supplied string list"
    (lint-context-test-no-findings
     "#+OPTIONS: html:value" "unknown-options-item"
     '(:export-options ("html"))))
  (it "does not run when export options are absent"
    (lint-context-test-no-findings
     "#+OPTIONS: mystery:value" "unknown-options-item")))

(describe "unknown-options-item missing value"
  (it "reports a known OPTIONS item without its value"
    (lint-context-test-finding
     "#+OPTIONS: html:" "unknown-options-item"
     '(:export-options ("html"))
     1 "Missing value for option item \"html\"")))

(describe "misspelled-export-option caller context"
  (it "reports a property matching a supplied global export option"
    (let* ((source (lint-context-test-lines "* Heading" ":PROPERTIES:"
                                            ":TITLE: x" ":END:"))
           (position (1+ (search ":TITLE:" source))))
      (lint-context-test-finding
       source "misspelled-export-option" '(:export-options ("TITLE"))
       position
       "Potentially misspelled global export option \"TITLE\".  Consider \"EXPORT_TITLE\".")))
  (it "does not treat an unrelated property as an export option"
    (lint-context-test-no-findings
     (lint-context-test-lines "* Heading" ":PROPERTIES:"
                              ":UNRELATED: x" ":END:")
     "misspelled-export-option" '(:export-options ("TITLE"))))
  (it "does not run when export options are absent"
    (lint-context-test-no-findings
     (lint-context-test-lines "* Heading" ":PROPERTIES:"
                              ":TITLE: x" ":END:")
     "misspelled-export-option")))

(describe "invalid-macro-argument-and-template caller context"
  (it "reports an undefined macro using caller templates and definitions"
    (let ((source "{{{unknown(value)}}}"))
      (lint-context-test-finding
       source "invalid-macro-argument-and-template"
       '(:macro-definitions (("defined" . "$1"))
         :macro-templates (("custom" . "$1")))
       1 "Undefined macro \"unknown\"")))
  (it "accepts a document macro with the matching argument count"
    (lint-context-test-no-findings
     (lint-context-test-lines "#+MACRO: local $1" "{{{local(value)}}}")
     "invalid-macro-argument-and-template"
     '(:macro-definitions nil :macro-templates nil)))
  (it "does not run when macro material is absent"
    (lint-context-test-no-findings
     "{{{unknown(value)}}}" "invalid-macro-argument-and-template"))
  (it "preserves Org's literal missing-template message"
    (lint-context-test-finding
     "#+MACRO: empty" "invalid-macro-argument-and-template"
     '(:macro-definitions nil :macro-templates nil)
     1 "Missing template in macro \"%s\"")))

(describe "non-existent-bibliography caller context"
  (it "reports a bibliography absent from the caller's file system"
    (lint-context-test-finding
     "#+BIBLIOGRAPHY: \"refs.bib\"" "non-existent-bibliography"
     (list :file-exists-p (lambda (path) (string= path "other.bib")))
     1 "Non-existent bibliography \"refs.bib\""))
  (it "accepts a bibliography confirmed by the caller"
    (lint-context-test-no-findings
     "#+BIBLIOGRAPHY: refs.bib" "non-existent-bibliography"
     (list :file-exists-p (lambda (path) (string= path "refs.bib")))))
  (it "does not run when the file predicate is absent"
    (lint-context-test-no-findings
     "#+BIBLIOGRAPHY: refs.bib" "non-existent-bibliography")))

(describe "invalid-cite-export-declaration caller context"
  (it "reports a processor missing from the caller's string list"
    (lint-context-test-finding
     "#+CITE_EXPORT: missing" "invalid-cite-export-declaration"
     '(:cite-export-processors ("csl"))
     1 "Unknown cite export processor missing"))
  (it "accepts a caller-registered cite export processor"
    (lint-context-test-no-findings
     "#+CITE_EXPORT: csl" "invalid-cite-export-declaration"
     '(:cite-export-processors ("csl"))))
  (it "does not run when cite export processors are absent"
    (lint-context-test-no-findings
     "#+CITE_EXPORT: missing" "invalid-cite-export-declaration"))
  (it "rejects a declaration with too many tokens"
    (lint-context-test-finding
     "#+CITE_EXPORT: csl foo bar baz" "invalid-cite-export-declaration"
     '(:cite-export-processors ("csl"))
     1 "Invalid cite export processor declaration")))

(describe "beamer-frame caller context"
  (it "reports a frame environment with the exact source position"
    (lint-context-test-finding
     "\\begin{frame}" "beamer-frame"
     '(:beamer-frame-environment "frame")
     1
     "Beamer frame name may cause error when exporting.  Consider customizing `org-beamer-frame-environment'."))
  (it "accepts a document without the configured frame environment"
    (lint-context-test-no-findings
     "No frame environment." "beamer-frame"
     '(:beamer-frame-environment "frame")))
  (it "reports the end frame environment too"
    (lint-context-test-finding
     "\\end{frame}" "beamer-frame"
     '(:beamer-frame-environment "frame")
     1
     "Beamer frame name may cause error when exporting.  Consider customizing `org-beamer-frame-environment'."))
  (it "does not run when the frame environment is absent"
    (lint-context-test-no-findings "\\begin{frame}" "beamer-frame")))
(describe "Org lint parsing and material edge cases"
  (it "classifies remote paths and extracts include targets"
    (expect (org-kit::%lint-context-remote-path-p "//server/path") :to-be t)
    (expect (org-kit::%lint-context-remote-path-p "git+ssh:repo/path") :to-be t)
    (expect (org-kit::%lint-context-remote-path-p "A:/local/path") :to-be nil)
    (expect (org-kit::%lint-context-remote-path-p "1scheme:path") :to-be nil)
    (expect (org-kit::%lint-context-remote-path-p "bad_scheme:path") :to-be nil)
    (expect (org-kit::%lint-context-file-part "part.org::*Heading") :to-equal "part.org")
    (expect (org-kit::%lint-context-file-part "part.org") :to-equal "part.org")
    (expect (org-kit::%lint-context-strip-quotes "  \"part.org\"  ") :to-equal "part.org")
    (expect (org-kit::%lint-context-strip-quotes "\"part.org") :to-equal "\"part.org")
    (expect (org-kit::%lint-context-babel-name " :Var ") :to-equal "var")
    (expect (org-kit::%lint-context-babel-name " Name ") :to-equal "name"))
  (it "parses quoted and unquoted include paths and search parts"
    (expect (org-kit::%lint-include-path "  \"part file.org\" src  ") :to-equal "part file.org")
    (expect (org-kit::%lint-include-path "part.org src") :to-equal "part.org")
    (expect (org-kit::%lint-include-path "part.org::Heading") :to-equal "part.org::Heading")
    (expect (org-kit::%lint-include-path "  ") :to-be nil)
    (expect (org-kit::%lint-include-path "\"\"") :to-be nil)
    (expect (org-kit::%lint-include-path "\"unterminated") :to-be nil)
    (expect (org-kit::%lint-include-search-part "part.org::Heading") :to-equal "Heading")
    (expect (org-kit::%lint-include-search-part "part.org::  ") :to-be nil)
    (expect (org-kit::%lint-include-search-part "part.org") :to-be nil))
  (it "tokenizes export options and macro placeholders"
    (expect (org-kit::%lint-export-option-name "html:toc") :to-equal "toc")
    (expect (org-kit::%lint-export-option-name "toc") :to-equal "toc")
    (expect (org-kit::%lint-options-items "toc:yes bare html:")
            :to-equal (list (cons "toc" "yes") (cons "html" "")))
    (expect (multiple-value-list (org-kit::%lint-export-option-entry "html:toc"))
            :to-equal (list "toc" "html"))
    (expect (multiple-value-list (org-kit::%lint-export-option-entry "toc"))
            :to-equal (list "toc" nil))
    (expect (org-kit::%lint-placeholder-numbers "$3 $1 $3 $0 $$2")
            :to-equal (list 1 2 3))
    (expect (org-kit::%lint-template-arity "$1 and $3") :to-equal (cons 3 3))
    (expect (org-kit::%lint-template-arity "no placeholders") :to-equal (cons 0 0)))
  (it "parses macro definitions and all arity message forms"
    (let* ((document (parse-org "#+MACRO: local $1"))
           (node (first (org-kit::%lint-nodes document :keyword))))
      (expect (multiple-value-list (org-kit::%lint-macro-keyword-data node))
              :to-equal (list "local" "$1")))
    (expect (org-kit::%lint-macro-entry (list (cons "local" "$1")) "local")
            :to-equal "$1")
    (expect (org-kit::%lint-macro-entry nil "local") :to-be nil)
    (expect (org-kit::%lint-macro-name-arity "keyword" "$1") :to-equal (cons 1 1))
    (expect (org-kit::%lint-macro-name-arity "modification-time" "$1")
            :to-equal (cons 1 2))
    (expect (org-kit::%lint-macro-name-arity "n" "$1") :to-equal (cons 0 2))
    (expect (org-kit::%lint-macro-name-arity "custom" "$1") :to-equal (cons 1 1))
    (expect (org-kit::%lint-macro-arity-message "m" nil (cons 2 2))
            :to-equal "Missing arguments in macro \"m\"")
    (expect (org-kit::%lint-macro-arity-message "m" (list "a") (cons 2 2))
            :to-equal "Missing argument in macro \"m\"")
    (expect (org-kit::%lint-macro-arity-message "m" (list "a" "b") (cons 2 2))
            :to-be nil)
    (expect (org-kit::%lint-macro-arity-message "m" (list "a" "b" "c") (cons 2 2))
            :to-equal "Spurious argument in macro \"m\": c")
    (expect (org-kit::%lint-macro-arity-message "m" (list "a" "b" "c" "d") (cons 2 2))
            :to-equal "Spurious arguments in macro \"m\": c, d"))
  (it "covers caller material parser and backend paths"
  (expect (org-kit::%lint-context-remote-path-p ":local") :to-be nil)
  (expect (org-kit::%lint-context-remote-path-p "a:b") :to-be nil)
  (expect (multiple-value-list (org-kit::%lint-macro-keyword-data
                                (first (org-kit::%lint-nodes
                                        (parse-org "#+MACRO: local") :keyword))))
          :to-equal (list "local" ""))
  (expect (multiple-value-list (org-kit::%lint-cite-declaration-tokens
                                "\"csl processor\" plain"))
          :to-equal (list (list (cons "csl processor" t) (cons "plain" nil)) t))
  (expect (multiple-value-list (org-kit::%lint-cite-declaration-tokens "\"unclosed"))
          :to-equal (list nil nil))
  (expect (org-kit::%lint-symbol-name-p "proc_2?") :to-be t)
  (expect (org-kit::%lint-symbol-name-p "2proc") :to-be nil)
  (let ((document (parse-org "plain")))
    (expect (org-kit::%lint-macro-definition-template
             document '(:macro-definitions (("caller" . "$1"))) "caller")
            :to-equal "$1")
    (expect (org-kit::%lint-macro-definition-template
             document '(:macro-templates (("custom" . "$2"))) "custom")
            :to-equal "$2"))
  (lint-context-test-no-findings
   "[[file://server/share]]" "link-to-local-file"
   (list :file-exists-p (lambda (path) (declare (ignore path)) nil)))
  (lint-context-test-no-findings
   "#+SETUPFILE: https://example.org/setup.org"
   "non-existent-setupfile-parameter"
   (list :file-exists-p (lambda (path) (declare (ignore path)) nil)))
  (lint-context-test-no-findings
   "#+INCLUDE: https://example.org/part.org" "wrong-include-link-parameter"
   (list :file-exists-p (lambda (path) (declare (ignore path)) nil)))
  (let ((source (lint-context-test-lines "* Heading" ":PROPERTIES:"
                                         ":HTML_FOO: x" ":END:")))
    (lint-context-test-finding
     source "misspelled-export-option"
     '(:export-options ("html:HTML_FOO"))
     (1+ (search ":HTML_FOO:" source))
     "Potentially misspelled export option \"HTML_FOO\" in html export backend.  Consider \"EXPORT_HTML_FOO\".")))
  (it "covers caller-material validation boundaries"
    (expect (org-kit::%lint-context-source-property-key
             (parse-org "plain")
             (org-kit::make-node :node-property :begin 0 :end 5
                                 :properties '(:key "fallback")))
            :to-equal "FALLBACK")
    (let* ((document (parse-org "[[id:missing]]"))
           (link (first (org-kit::%lint-nodes document :link))))
      (setf (org-kit::node-property link :type) "id"
            (org-kit::node-property link :path) "missing")
      (expect (length (org-kit::%lint-invalid-id-link
                       document '(:known-ids nil)))
              :to-be 1))
    (let* ((document (parse-org
                      (lint-context-test-lines
                       "#+MACRO:" "#+MACRO: local" "#+MACRO: sparse $2")))
           (context '(:macro-definitions nil :macro-templates nil)))
      (expect (mapcar #'org-kit::org-lint-diagnostic-message
                      (org-kit::%lint-invalid-macro-argument-and-template
                       document context))
              :to-equal '("Missing name in MACRO keyword"
                          "Missing template in macro \"%s\""
                          "Unused placeholders in macro \"sparse\"")))
    (let* ((document (parse-org "{{{unknown(x)}}} {{{title}}}"))
           (context '(:macro-definitions nil :macro-templates nil)))
      (expect (mapcar #'org-kit::org-lint-diagnostic-message
                      (org-kit::%lint-invalid-macro-argument-and-template
                       document context))
              :to-equal '("Undefined macro \"unknown\""
                          "Missing argument in macro \"title\"")))
    (expect (mapcar #'org-kit::org-lint-diagnostic-message
                    (lint-context-test-findings
                     "#+CITE_EXPORT:" "invalid-cite-export-declaration"
                     '(:cite-export-processors nil)))
            :to-equal '("Missing export processor name")))
)
(describe "Org lint caller material branch cases"
  (it "handles empty Babel names and deprecated property boundaries"
    (expect (org-kit::%lint-context-babel-name "  ") :to-equal "")
    (lint-context-test-no-findings
     (lint-context-test-lines
      "#+PROPERTY: x" "#+PROPERTY: not-var value" "#+PROPERTY: var"
      "#+PROPERTY: varX value")
     "deprecated-header-syntax"
     '(:babel-header-arguments ("long" "var"))))
  (it "ignores short, ordinary, and pathless links"
    (lint-context-test-no-findings
     "[[x]] [[file:plain.txt]]" "invalid-id-link" '(:known-ids nil))
    (let* ((document (parse-org "x"))
           (link (org-kit::make-node
                  :link :begin 0 :end 1 :properties '(:type "file"))))
      (setf (org-kit::node-children (org-kit::document-root document))
            (list link))
      (expect (org-kit::%lint-invalid-id-link document '(:known-ids nil))
              :to-equal '())
      (expect (org-kit::%lint-link-to-local-file
               document (list :file-exists-p (lambda (path)
                                               (declare (ignore path))
                                               nil)))
              :to-equal '()))
    (lint-context-test-no-findings
     "[[https://example.org/page]]" "link-to-local-file"
     (list :file-exists-p (lambda (path) (declare (ignore path)) nil))))
  (it "recognizes a missing file nested in a link description"
    (let* ((source "[[file:missing.png]]")
           (document (parse-org source))
           (inner (first (org-kit::%lint-nodes document :link)))
           (outer (org-kit::make-node
                   :link :begin 0 :end (length source)
                   :properties '(:type "file" :path "outer.txt"))))
      (setf (org-kit::node-children (org-kit::document-root document))
            (list outer))
      (org-kit::append-child outer inner)
      (expect (mapcar #'org-kit::org-lint-diagnostic-message
                      (org-kit::%lint-link-to-local-file
                       document
                       (list :file-exists-p
                             (lambda (path) (string= path "outer.txt")))))
              :to-equal '("Link to non-existent image file \"missing.png\" in description"))))
  (it "skips unrelated keywords and an empty OPTIONS value"
    (let ((context (list :file-exists-p (lambda (path)
                                          (declare (ignore path))
                                          nil))))
      (lint-context-test-no-findings "#+TITLE: x" "non-existent-setupfile-parameter" context)
      (lint-context-test-no-findings "#+TITLE: x" "wrong-include-link-parameter" context))
    (lint-context-test-no-findings
     (lint-context-test-lines "#+TITLE: x" "#+OPTIONS:")
     "unknown-options-item" '(:export-options nil)))
  (it "accepts allow-listed export properties and resolves local macros"
    (lint-context-test-no-findings
     (lint-context-test-lines "* Heading" ":PROPERTIES:" ":SUMMARY: x" ":END:")
     "misspelled-export-option" '(:export-options ("SUMMARY")))
    (let* ((document (parse-org
                      (lint-context-test-lines
                       "#+TITLE: x" "#+MACRO:" "#+MACRO: local $1"
                       "#+MACRO: empty" "{{{unknown(x)}}} {{{empty}}}")))
           (context '(:macro-definitions nil :macro-templates nil)))
      (expect (mapcar #'org-kit::org-lint-diagnostic-message
                      (org-kit::%lint-invalid-macro-argument-and-template
                       document context))
              :to-equal '("Missing name in MACRO keyword"
                          "Missing template in macro \"%s\""
                          "Undefined macro \"unknown\""
                          "Undefined macro \"empty\""))))
  (it "skips empty and remote bibliographies and rejects invalid cite names"
    (lint-context-test-no-findings
     (lint-context-test-lines
      "#+TITLE: x" "#+BIBLIOGRAPHY:" "#+BIBLIOGRAPHY: https://example.org/refs.bib")
     "non-existent-bibliography"
     (list :file-exists-p (lambda (path) (declare (ignore path)) nil)))
    (expect (multiple-value-list
             (org-kit::%lint-cite-declaration-tokens "csl "))
            :to-equal (list (list (cons "csl" nil)) t))
    (expect (mapcar #'org-kit::org-lint-diagnostic-message
                    (lint-context-test-findings
                     (lint-context-test-lines "#+TITLE: x" "#+CITE_EXPORT: bad/name")
                     "invalid-cite-export-declaration"
                     '(:cite-export-processors ("csl"))))
            :to-equal '("Invalid cite export processor declaration"))))


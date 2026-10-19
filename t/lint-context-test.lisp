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

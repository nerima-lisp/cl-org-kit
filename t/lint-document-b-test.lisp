(in-package #:cl-org-kit/test)

(defun expect-document-b-diagnostic (source checker position message)
  (let* ((diagnostics
           (org-lint (parse-org source) :checkers (list checker)))
         (diagnostic (first diagnostics)))
    (expect (length diagnostics) :to-be 1)
    (expect (org-lint-diagnostic-checker diagnostic) :to-equal checker)
    (expect (org-lint-diagnostic-position diagnostic) :to-be position)
    (expect (org-lint-diagnostic-message diagnostic) :to-equal message)))

(defun expect-document-b-clean (source checker)
  (expect (org-lint (parse-org source) :checkers (list checker)) :to-equal '()))

(describe "document-only Org lint checkers, group B"
  (it "detects obsolete include markup and accepts export syntax"
    (expect-document-b-diagnostic
     (format nil "#+INCLUDE: \"part.org\" HTML~%")
     "obsolete-include-markup" 1
     "Obsolete markup \"HTML\" in INCLUDE keyword.  Use \"export HTML\" instead")
    (expect-document-b-clean
     (format nil "#+INCLUDE: \"part.org\" export HTML~%")
     "obsolete-include-markup"))

  (it "detects a missing keyword colon and accepts normal keyword syntax"
    (expect-document-b-diagnostic
     (format nil "#+TITLE Missing colon~%")
     "invalid-keyword-syntax" 1
     "Possible missing colon in keyword \"TITLE\"")
    (expect-document-b-clean
     (format nil "#+TITLE: Present~%")
     "invalid-keyword-syntax"))

  (it "detects unsupported image alignment and accepts supported alignment"
    (expect-document-b-diagnostic
     (format nil "#+ATTR_ORG: :align top~%[[file:picture.png]]~%")
     "invalid-image-alignment" 1
     "\"top\" not a supported value for #+ATTR_ORG keyword attribute \":align\".")
    (expect-document-b-clean
     (format nil "#+ATTR_ORG: :align center~%[[file:picture.png]]~%")
     "invalid-image-alignment"))

  (it "detects a malformed block closer and accepts a complete source block"
    (expect-document-b-diagnostic
     (format nil "#+END_SRC trailing~%")
     "invalid-block" 1
     "Invalid block closing line \"#+END_SRC trailing\"")
    (expect-document-b-clean
     (format nil "#+BEGIN_SRC text~%body~%#+END_SRC~%")
     "invalid-block"))

  (it "detects mismatched planning repeaters and accepts matching repeaters"
    (expect-document-b-diagnostic
     (format nil "* TODO Task~%SCHEDULED: <2026-10-19 Mon +1w> DEADLINE: <2026-10-20 Tue +2w>~%")
     "mismatched-planning-repeaters" 13
     "Different repeaters in SCHEDULED and DEADLINE timestamps.")
    (expect-document-b-clean
     (format nil "* TODO Task~%SCHEDULED: <2026-10-19 Mon +1w> DEADLINE: <2026-10-20 Tue +1w>~%")
     "mismatched-planning-repeaters"))

  (it "detects misplaced planning information and accepts heading planning"
    (expect-document-b-diagnostic
     (format nil "SCHEDULED: <2026-10-19 Mon>~%")
     "misplaced-planning-info" 1
     "Misplaced planning info line")
    (expect-document-b-clean
     (format nil "* TODO Task~%SCHEDULED: <2026-10-19 Mon>~%")
     "misplaced-planning-info"))

  (it "detects an incomplete drawer and accepts a closed property drawer"
    (expect-document-b-diagnostic
     (format nil ":LOGBOOK:~%")
     "incomplete-drawer" 1
     "Possible incomplete drawer \":LOGBOOK:\"")
    (expect-document-b-clean
     (format nil "* Task~%:PROPERTIES:~%:ID: task-id~%:END:~%")
     "incomplete-drawer"))

  (it "detects indented diary syntax and accepts a non-indented diary line"
    (expect-document-b-diagnostic
     (format nil " %%(date)~%")
     "indented-diary-sexp" 1
     "Possible indented diary-sexp")
    (expect-document-b-clean
     (format nil "%%(date)~%")
     "indented-diary-sexp"))

  (it "detects deprecated quote headlines and inlinetasks with exact capitalization"
    (expect-document-b-diagnostic
     (format nil "* QUOTE Old text~%")
     "quote-section" 1
     "Deprecated QUOTE section")
    (expect-document-b-diagnostic
     (format nil "*************** QUOTE Old text~%body~%*************** END~%")
     "quote-section" 1
     "Deprecated QUOTE section")
    (expect-document-b-diagnostic
     (format nil "* COMMENT QUOTE Old text~%")
     "quote-section" 1
     "Deprecated QUOTE section")
    (expect-document-b-clean
     (format nil "* Quote Old text~%")
     "quote-section")
    (expect-document-b-clean
     (format nil "*************** Quote Old text~%body~%*************** END~%")
     "quote-section"))

  (it "detects file application links and accepts regular file links"
    (expect-document-b-diagnostic
     "[[file+emacs:document.org]]"
     "file-application" 1
     "Deprecated \"file+emacs\" link type")
    (expect-document-b-clean "[[file:document.org]]" "file-application"))

  (it "detects obsolete percent escapes and accepts standard escapes"
    (expect-document-b-diagnostic
     "[[https://example.org/a%2Ffile][link]]"
     "percent-encoding-link-escape" 1
     "Link escaped with obsolete percent-encoding syntax")
    (expect-document-b-clean
     "[[https://example.org/a%20b%25][link]]"
     "percent-encoding-link-escape"))

  (it "detects trailing spurious tag colons and accepts a named tag"
    (expect-document-b-diagnostic
     (format nil "* Heading ::~%")
     "spurious-colons" 1
     "Tags contain a spurious colon")
    (expect-document-b-clean
     (format nil "* Heading :named:~%")
     "spurious-colons"))

  (it "requires a print bibliography for citations and accepts its keyword"
    (expect-document-b-diagnostic
     (format nil "[cite:@reference]~%")
     "missing-print-bibliography" 19
     "Possibly missing \"PRINT_BIBLIOGRAPHY\" keyword")
    (expect-document-b-clean
     (format nil "#+PRINT_BIBLIOGRAPHY: t~%[cite:@reference]~%")
     "missing-print-bibliography"))

  (it "detects incomplete citation markup and accepts a complete citation"
    (expect-document-b-diagnostic
     (format nil "Text [cite:~%")
     "incomplete-citation" 1
     "Possibly incomplete citation markup")
    (expect-document-b-clean
     (format nil "Text [cite:@reference]~%")
     "incomplete-citation"))

  (it "detects incorrect ordered item numbering and accepts sequential items"
    (expect-document-b-diagnostic
     (format nil "3. first~%4. second~%")
     "item-number" 1
     "Bullet counter \"3. \" is not the same with item position 1.  Consider adding manual [@3] counter.")
    (expect-document-b-clean
     (format nil "1. first~%2. second~%")
     "item-number"))

  (it "detects out-of-bounds priority and accepts an in-range priority"
    (expect-document-b-diagnostic
     (format nil "* [#Z] Urgent~%")
     "priority" 1
     "Out-of-bounds priority 'Z'")
    (expect-document-b-clean
     (format nil "* [#B] Normal~%")
     "priority"))

  (it "detects an ambiguous dollar boundary and accepts an unambiguous fragment"
    (expect-document-b-diagnostic
     "$x1$" "LaTeX-$" 4
     "$ symbol potentially matching LaTeX fragment boundary.  Consider using \\dollar entity.")
    (expect-document-b-clean "$xyz$" "LaTeX-$"))

  (it "detects a malformed timestamp weekday and accepts its canonical spelling"
    (expect-document-b-diagnostic
     "<2026-10-19 Tue>" "timestamp-syntax" 1
     "Potentially malformed timestamp <2026-10-19 Tue>.  Parsed as: <2026-10-19 Mon>")
    (expect-document-b-diagnostic
     "<2026-10-19 Tue>--<2026-10-20 Tue>" "timestamp-syntax" 1
     "Potentially malformed timestamp <2026-10-19 Tue>--<2026-10-20 Tue>.  Parsed as: <2026-10-19 Mon>--<2026-10-20 Tue>")
    (expect-document-b-clean "<2026-10-19 Mon>" "timestamp-syntax")
    (expect-document-b-clean
     "<2026-10-19 Mon>--<2026-10-20 Tue>" "timestamp-syntax"))

  (it "detects malformed clock spelling and accepts a canonical clock"
    (expect-document-b-diagnostic
     (format nil "CLOCK: [2026-10-19 Tue]~%")
     "clock-syntax" 1
     (format nil "Potentially malformed CLOCK: line~%           CLOCK: [2026-10-19 Tue]~%Parsed as: CLOCK: [2026-10-19 Mon]"))
    (expect-document-b-diagnostic
     (format nil "CLOCK: [2026-10-19 Tue 09:00]--[2026-10-19 Tue 10:00] => 1:00~%")
     "clock-syntax" 1
     (format nil "Potentially malformed CLOCK: line~%           CLOCK: [2026-10-19 Tue 09:00]--[2026-10-19 Tue 10:00] => 1:00~%Parsed as: CLOCK: [2026-10-19 Mon 09:00-10:00] =>  1:00"))
    (expect-document-b-clean
     (format nil "CLOCK: [2026-10-19 Mon]~%")
     "clock-syntax")
    (expect-document-b-clean
     (format nil "CLOCK: [2026-10-19 Mon 09:00-10:00] =>  1:00~%")
     "clock-syntax"))

  (it "detects inactive scheduled timestamps and accepts active planning"
    (expect-document-b-diagnostic
     (format nil "* TODO Task~%SCHEDULED: [2026-10-19 Mon]~%")
     "planning-inactive" 13
     "Inactive timestamp in SCHEDULED will not appear in agenda.")
    (expect-document-b-clean
     (format nil "* TODO Task~%SCHEDULED: <2026-10-19 Mon>~%")
     "planning-inactive")))

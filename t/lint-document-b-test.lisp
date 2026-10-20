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
     "planning-inactive"))
  (it "covers source-line and scalar helper boundaries"
  (expect (not (null (org-kit::%lint-b-horizontal-space-p #\Tab))) :to-be t)
  (expect (org-kit::%lint-b-horizontal-space-p #\Newline) :to-be nil)
  (expect (org-kit::%lint-b-prefix-p "  #+BEGIN_SRC" "#+" 2) :to-be t)
  (expect (org-kit::%lint-b-prefix-p "#+A" "LONG") :to-be nil)
  (expect (org-kit::%lint-b-leading-space-count (format nil " ~CText" #\Tab)) :to-be 2)
  (expect (org-kit::%lint-b-leading-space-count "   ") :to-be 3)
  (expect (org-kit::%lint-b-drawer-line-p " :LOGBOOK:  ") :to-equal ":LOGBOOK:")
  (expect (org-kit::%lint-b-drawer-line-p ":BAD NAME:") :to-be nil)
  (expect (org-kit::%lint-b-citation-prefix-end "[cite:@item]" 0) :to-be 6)
  (expect (org-kit::%lint-b-citation-prefix-end "[cite/foo:@item]" 0) :to-be 10)
  (expect (org-kit::%lint-b-citation-prefix-end "[cite/:@item]" 0) :to-be nil)
  (expect (not (null (org-kit::%lint-b-citation-prefix-p "text [cite:@item]"))) :to-be t)
  (expect (org-kit::%lint-b-citation-prefix-p "text [cited:@item]") :to-be nil)
  (expect (org-kit::%lint-b-weekday 2000 2 29) :to-equal "Tue")
  (expect (org-kit::%lint-b-weekday 2024 2 29) :to-equal "Thu")
  (expect (org-kit::%lint-b-weekday 1900 2 29) :to-be nil)
  (expect (org-kit::%lint-b-weekday 2024 13 1) :to-be nil)
  (expect (org-kit::%lint-b-weekday 2024 1 32) :to-be nil)
  (expect (org-kit::%lint-b-unit-char :year) :to-be #\y)
  (expect (org-kit::%lint-b-unit-char :unknown) :to-be nil)
  (expect (org-kit::%lint-b-bullet-number "b)") :to-be 2)
  (expect (org-kit::%lint-b-bullet-number "4.") :to-be 4)
  (expect (org-kit::%lint-b-bullet-number "-") :to-be nil)
  (expect (org-kit::%lint-b-bullet-number 4) :to-be nil)
  (expect (org-kit::%lint-b-priority-string 90) :to-equal "Z")
  (expect (org-kit::%lint-b-priority-string 64) :to-equal "64")
  (expect (org-kit::%lint-b-attribute-value "x :align top y" ":align") :to-equal "top")
  (expect (org-kit::%lint-b-attribute-value "x :align" ":align") :to-be nil))
  (it "covers additional document checker paths"
  (expect-document-b-diagnostic
   (format nil "#+END:bad~%") "invalid-block" 1
   "Invalid block closing line \"#+END:bad\"")
  (expect-document-b-diagnostic
   (format nil "#+BEGIN~%") "invalid-block" 1
   "Possible incomplete block \"#+BEGIN\"")
  (expect-document-b-diagnostic
   (format nil "* TODO Task~%DEADLINE: [2026-10-19 Mon]~%")
   "planning-inactive" 13
   "Inactive timestamp in DEADLINE will not appear in agenda.")
  (expect-document-b-clean
   (format nil "3. [@3] third~%4. fourth~%") "item-number")
  (let* ((document (parse-org (format nil "<2024-01-01 Mon +1d -2h>")))
         (timestamp (first (org-kit::%lint-nodes document :timestamp))))
    (expect (org-kit::%lint-b-timestamp-adjustments timestamp)
            :to-equal " +1d -2h")
    (expect (org-kit::%lint-b-timestamp-canonical timestamp)
            :to-equal "<2024-01-01 Mon +1d -2h>"))
  (expect-document-b-clean "[2026-10-19 Mon 09:00-10:00]"
                           "timestamp-syntax")
  (expect (org-kit::%lint-b-citation-prefix-end "text [cite: @item]" 5)
          :to-be 12)
  (expect (org-kit::%lint-b-weekday 0 1 1) :to-be nil)
  (expect (org-kit::%lint-b-weekday 2024 4 31) :to-be nil)
  (expect (org-kit::%lint-b-attribute-value "x :align   " ":align") :to-be nil))
  (it "covers document checker helper boundaries"
    (let* ((document (parse-org (format nil "first~C~%second" #\Return)))
           (lines nil))
      (org-kit::%lint-b-lines
       document
       (lambda (start end line) (push (list start end line) lines)))
      (expect (nreverse lines)
              :to-equal '((0 5 "first") (7 13 "second"))))
    (expect-document-b-diagnostic
     (format nil "#+INCLUDE: part.org HTML~%")
     "obsolete-include-markup" 1
     "Obsolete markup \"HTML\" in INCLUDE keyword.  Use \"export HTML\" instead")
    (let* ((document (parse-org (format nil "#+INCLUDE: part.org HTML~%")))
           (keyword (first (org-kit::%lint-nodes document :keyword))))
      (setf (org-kit::%node-post-affiliated keyword) nil)
      (expect (mapcar #'org-kit::org-lint-diagnostic-position
                      (org-kit::org-lint document :checkers '("obsolete-include-markup")))
              :to-equal '(1)))
    (let* ((document (parse-org ":END:"))
           (drawer (org-kit::make-node
                    :drawer :begin 0 :end 5 :post-affiliated 0)))
      (setf (org-kit::node-children (org-kit::document-root document))
            (list drawer))
      (expect (org-kit::org-lint document :checkers '("incomplete-drawer"))
              :to-equal '()))
    (let* ((document (parse-org
                      (format nil ":LOGBOOK:~%:OTHER:~%:END:~%")))
           (diagnostics (org-kit::org-lint
                         document :checkers '("incomplete-drawer")))
           (nested-entry (remove-if-not
                          (lambda (diagnostic)
                            (= (org-kit::org-lint-diagnostic-position diagnostic) 11))
                          diagnostics)))
      (expect (mapcar #'org-kit::org-lint-diagnostic-message nested-entry)
              :to-equal '("Possible misleading drawer entry \":OTHER:\"")))
    (expect-document-b-diagnostic
     "* Heading ::" "spurious-colons" 1 "Tags contain a spurious colon")
    (expect-document-b-diagnostic
     (format nil "#+BEGIN text~%") "invalid-block" 1
     "Possible incomplete block \"#+BEGIN text\"")
    (dolist (day '((2024 10 4 "Fri") (2024 10 5 "Sat") (2024 10 6 "Sun")))
      (expect (org-kit::%lint-b-weekday (first day) (second day) (third day))
              :to-equal (fourth day)))
    (let ((timestamp (org-kit::make-node
                      :timestamp
                      :properties '(:repeater-type :catch-up :repeater-value 2
                                    :repeater-unit :week :warning-type :all
                                    :warning-value 3 :warning-unit :month))))
      (expect (org-kit::%lint-b-timestamp-adjustments timestamp)
              :to-equal " ++2w -3m")
      (setf (org-kit::node-property timestamp :repeater-type) :restart)
      (expect (org-kit::%lint-b-timestamp-adjustments timestamp)
              :to-equal " .+2w -3m"))
    (let ((timestamp (org-kit::make-node
                      :timestamp
                      :properties '(:type :active
                                    :year-start 2024 :month-start 1 :day-start 1
                                    :hour-start 9 :minute-start 0
                                    :year-end 2024 :month-end 1 :day-end 1
                                    :hour-end 10 :minute-end 30))))
      (expect (org-kit::%lint-b-timestamp-canonical timestamp)
              :to-equal "<2024-01-01 Mon 09:00-10:30>")
      (setf (org-kit::node-property timestamp :hour-end) 9
            (org-kit::node-property timestamp :minute-end) 0)
      (expect (org-kit::%lint-b-timestamp-canonical timestamp)
              :to-equal "<2024-01-01 Mon 09:00>"))
    (let* ((document (parse-org
                      (format nil "CLOCK: [2024-01-01 Mon] => 9:5~%")))
           (clock (first (org-kit::%lint-nodes document :clock))))
      (expect (org-kit::%lint-b-clock-canonical clock)
              :to-equal "CLOCK: [2024-01-01 Mon] =>  9:05"))
    (let* ((document (parse-org "[cite:"))
           (node (org-kit::make-node :plain-text :begin 0 :end 6)))
      (setf (org-kit::node-children (org-kit::document-root document))
            (list node))
      (expect (mapcar #'org-kit::org-lint-diagnostic-position
                      (org-kit::%lint-incomplete-citation document nil))
              :to-equal '(1)))
    (dolist (raw-value '("[#Q]" "[#Q"))
      (let* ((document (parse-org "* Heading"))
             (headline (first (org-kit::%lint-nodes document :headline))))
        (setf (org-kit::node-property headline :priority) nil
              (org-kit::node-property headline :raw-value) raw-value)
        (expect (mapcar #'org-kit::org-lint-diagnostic-message
                        (org-kit::%lint-priority document nil))
                :to-equal (list (if (char= (char raw-value (1- (length raw-value))) #\])
                                    "Invalid priority 'Q'"
                                    "Malformed priority '[#Q'")))))
    (expect (org-kit::%lint-b-unit-char :week) :to-be #\w)
    (expect (org-kit::%lint-b-unit-char :month) :to-be #\m)
    (expect-document-b-diagnostic
     (format nil "#+INCLUDE: \"part.org\" HTML~%")
     "obsolete-include-markup" 1
     "Obsolete markup \"HTML\" in INCLUDE keyword.  Use \"export HTML\" instead"))
)
(describe "document-only Org lint checker branch cases"
  (it "covers citation, include, and keyword syntax boundaries"
    (expect (org-kit::%lint-b-citation-prefix-end "[cit" 0) :to-be nil)
    (expect (org-kit::%lint-b-citation-prefix-end "[xite:@item]" 0) :to-be nil)
    (expect (org-kit::%lint-b-citation-prefix-end "[cite" 0) :to-be nil)
    (expect (org-kit::%lint-b-citation-prefix-end "[cite/foo" 0) :to-be nil)
    (expect-document-b-clean "#+TITLE: x" "obsolete-include-markup")
    (dolist (source '("#+INCLUDE:" "#+INCLUDE: \"\" HTML"
                      "#+INCLUDE: \"part.org HTML"))
      (expect-document-b-clean source "obsolete-include-markup"))
    (let* ((document (parse-org "x"))
           (keyword (org-kit::make-node :keyword :begin 0 :end 1
                                        :post-affiliated 0
                                        :properties '(:key "INCLUDE" :value nil))))
      (setf (org-kit::node-children (org-kit::document-root document)) (list keyword))
      (expect (org-kit::%lint-obsolete-include-markup document nil) :to-equal '()))
    (expect-document-b-clean
     (format nil "#+BEGIN_SRC text~%#+END_SRC~%") "invalid-keyword-syntax")
    (expect-document-b-clean "#+MIDDLE text" "invalid-block")
    (expect-document-b-clean "#+BEGINX" "invalid-block")
    (expect-document-b-clean "#+END_SRC   " "invalid-block"))
  (it "filters synthetic LaTeX, links, and plain text nodes"
    (let* ((document (parse-org "x"))
           (fragment (org-kit::make-node :latex-fragment :begin 0 :end 1
                                         :properties '(:value nil))))
      (setf (org-kit::node-children (org-kit::document-root document)) (list fragment))
      (expect (org-kit::%lint-latex-dollar-ambiguous document nil) :to-equal '()))
    (let* ((document (parse-org "x"))
           (fragment (org-kit::make-node :latex-fragment :end 1
                                         :properties '(:value "$ab2"))))
      (setf (org-kit::node-children (org-kit::document-root document)) (list fragment))
      (expect (org-kit::%lint-latex-dollar-ambiguous document nil) :to-equal '()))
    (let* ((document (parse-org "x"))
           (link (org-kit::make-node :link :begin 0 :end 1
                                     :properties '(:path "part%2Fpath"))))
      (setf (org-kit::node-children (org-kit::document-root document)) (list link))
      (expect (org-kit::%lint-file-application document nil) :to-equal '())
      (expect (org-kit::%lint-percent-encoding-link-escape document nil)
              :to-equal '()))
    (let* ((document (parse-org "x"))
           (plain-text (org-kit::make-node :plain-text)))
      (org-kit::append-child (org-kit::document-root document) plain-text)
      (expect (org-kit::%lint-incomplete-citation document nil) :to-equal '())))
  (it "validates timestamp fields and date ranges"
    (dolist (properties '((:year-start nil :month-start 1 :day-start 1)
                          (:year-start 2024 :month-start nil :day-start 1)
                          (:year-start 2024 :month-start 1 :day-start nil)
                          (:year-start 2024 :month-start 4 :day-start 31)))
      (expect (org-kit::%lint-b-timestamp-endpoint
               (org-kit::make-node :timestamp :properties properties) nil)
              :to-be nil))
    (expect (org-kit::%lint-b-timestamp-endpoint
             (org-kit::make-node
              :timestamp
              :properties '(:year-start 2024 :month-start 1 :day-start 1
                            :hour-start 9 :minute-start nil))
             nil)
            :to-equal "2024-01-01 Mon")
    (let ((timestamp (org-kit::make-node
                      :timestamp
                      :properties '(:type :active-range
                                    :year-start 2024 :month-start 1 :day-start 1
                                    :hour-start 9 :minute-start 0
                                    :year-end 2025 :month-end 1 :day-end 1
                                    :hour-end 10 :minute-end 30))))
      (expect (org-kit::%lint-b-timestamp-canonical timestamp)
              :to-equal "<2024-01-01 Mon 09:00>--<2025-01-01 Wed 10:30>")
      (setf (org-kit::node-property timestamp :year-end) 2024
            (org-kit::node-property timestamp :month-end) 2)
      (expect (org-kit::%lint-b-timestamp-canonical timestamp)
              :to-equal "<2024-01-01 Mon 09:00>--<2024-02-01 Thu 10:30>")
      (setf (org-kit::node-property timestamp :month-end) 1
            (org-kit::node-property timestamp :minute-start) nil)
      (expect (org-kit::%lint-b-timestamp-canonical timestamp)
              :to-equal "<2024-01-01 Mon>--<2024-01-01 Mon 10:30>"))
    (expect (org-kit::%lint-b-timestamp-canonical
             (org-kit::make-node :timestamp))
            :to-be nil)
    (expect (org-kit::%lint-b-timestamp-canonical
             (org-kit::make-node
              :timestamp
              :properties '(:type :active
                            :year-start 2024 :month-start 1 :day-start 1
                            :hour-start 9 :minute-start 0
                            :year-end 2024 :month-end 1 :day-end 1
                            :hour-end nil :minute-end 30)))
            :to-equal "<2024-01-01 Mon 09:00>")
    (let* ((document (parse-org (format nil "CLOCK: [2024-01-01 Mon] => 10:5~%")))
           (clock (first (org-kit::%lint-nodes document :clock))))
      (expect (org-kit::%lint-b-clock-canonical clock)
              :to-equal "CLOCK: [2024-01-01 Mon] => 10:05")))
  (it "checks planning repeaters and protected source regions"
    (let* ((document (parse-org "x"))
           (restart (org-kit::make-node
                     :planning :begin 0 :end 1
                     :properties
                     (list :scheduled (org-kit::make-node
                                      :timestamp :properties
                                      '(:repeater-type :restart :repeater-value 1
                                        :repeater-unit :week))
                           :deadline (org-kit::make-node
                                      :timestamp :properties
                                      '(:repeater-type :cumulate :repeater-value 2
                                        :repeater-unit :week)))))
           (zero (org-kit::make-node
                  :planning :begin 0 :end 1
                  :properties
                  (list :scheduled (org-kit::make-node
                                   :timestamp :properties
                                   '(:repeater-type :cumulate :repeater-value 0
                                     :repeater-unit :week))
                        :deadline (org-kit::make-node
                                   :timestamp :properties
                                   '(:repeater-type :cumulate :repeater-value 1
                                     :repeater-unit :week)))))
           (different-modes (org-kit::make-node
                             :planning :begin 0 :end 1
                             :properties
                             (list :scheduled (org-kit::make-node
                                              :timestamp :properties
                                              '(:repeater-type :cumulate :repeater-value 1
                                                :repeater-unit :week))
                                   :deadline (org-kit::make-node
                                              :timestamp :properties
                                              '(:repeater-type :catch-up :repeater-value 1
                                                :repeater-unit :week)))))
           (root (org-kit::document-root document)))
      (setf (org-kit::node-children root) (list restart zero different-modes))
      (expect (mapcar #'org-kit::org-lint-diagnostic-message
                      (org-kit::%lint-mismatched-planning-repeaters document nil))
              :to-equal '("Different repeaters in SCHEDULED and DEADLINE timestamps.")))
    (let* ((source ":LOGBOOK:")
           (document (parse-org source))
           (paragraph (org-kit::make-node :paragraph :begin 0 :end (length source))))
      (setf (org-kit::node-children (org-kit::document-root document)) (list paragraph))
      (expect (mapcar #'org-kit::org-lint-diagnostic-message
                      (org-kit::%lint-incomplete-drawer document nil))
              :to-equal '("Possible incomplete drawer \":LOGBOOK:\"")))
    (let* ((source (format nil " %%(date)"))
           (document (parse-org source))
           (paragraph (org-kit::make-node :paragraph :begin 0 :end (length source)))
           (example (org-kit::make-node :example-block :begin 0 :end (length source)
                                        :children (list paragraph))))
      (setf (org-kit::node-children (org-kit::document-root document)) (list example))
      (expect (org-kit::%lint-indented-diary-sexp document nil) :to-equal '())))
  (it "handles short quote titles, malformed headlines, and bibliography checks"
    (let* ((document (parse-org "* Heading"))
           (headline (first (org-kit::%lint-nodes document :headline))))
      (setf (org-kit::node-property headline :raw-value) "QUOTE")
      (expect (org-kit::%lint-quote-section document nil) :to-equal '())
      (setf (org-kit::node-property headline :raw-value) "COMMENT QUOTE")
      (expect (org-kit::%lint-quote-section document nil) :to-equal '())
      (setf (org-kit::node-property headline :raw-value) 1
            (org-kit::node-property headline :priority) nil)
      (expect (org-kit::%lint-priority document nil) :to-equal '()))
    (let* ((document (parse-org "* Heading"))
           (headline (first (org-kit::%lint-nodes document :headline))))
      (setf (org-kit::node-property headline :tags) "tag")
      (expect (org-kit::%lint-spurious-colons document nil) :to-equal '()))
    (expect (mapcar #'org-kit::org-lint-diagnostic-message
                    (org-kit::%lint-missing-print-bibliography
                     (parse-org (format nil "#+TITLE: x~%[cite:@reference]")) nil))
            :to-equal '("Possibly missing \"PRINT_BIBLIOGRAPHY\" keyword")))
)

(describe "empty Babel header argument checker"
  (it "matches Org's valueless source-block header tokens"
    (expect-document-b-diagnostic
     (format nil "#+BEGIN_SRC emacs-lisp :results :exports code~%body~%#+END_SRC~%")
     "empty-header-argument" 1
     "Empty value in header argument \":results\"")
    (expect-document-b-diagnostic
     (format nil "#+BEGIN_SRC emacs-lisp :results=~%body~%#+END_SRC~%")
     "empty-header-argument" 1
     "Empty value in header argument \":results=\"")
    (expect-document-b-diagnostic
     (format nil "#+BEGIN_SRC emacs-lisp :results=foo~%body~%#+END_SRC~%")
     "empty-header-argument" 1
     "Empty value in header argument \":results=foo\"")
    (expect-document-b-clean
     (format nil "#+BEGIN_SRC emacs-lisp~%body~%#+END_SRC~%")
     "empty-header-argument")
    (expect-document-b-clean
     (format nil "#+BEGIN_SRC emacs-lisp :results output~%body~%#+END_SRC~%")
     "empty-header-argument")
    (expect-document-b-clean
     (format nil "#+BEGIN_SRC emacs-lisp :results= output~%body~%#+END_SRC~%")
     "empty-header-argument"))

  (it "checks affiliated HEADER and HEADERS lines"
    (let ((source
            (format nil
                    "#+HEADERS: :cache~%#+HEADER: :results output~%#+BEGIN_SRC emacs-lisp~%body~%#+END_SRC~%")))
      (expect-document-b-diagnostic
       source "empty-header-argument" (1+ (search "#+BEGIN_SRC" source))
       "Empty value in header argument \":cache\""))
    (expect-document-b-clean
     (format nil "#+HEADER: :results output~%#+BEGIN_SRC emacs-lisp~%body~%#+END_SRC~%")
     "empty-header-argument")
    (expect-document-b-clean
     (format nil "#+HEADERS: :results output~%#+BEGIN_SRC emacs-lisp~%body~%#+END_SRC~%")
     "empty-header-argument")
    (expect-document-b-clean
     (format nil "#+BEGIN_SRC emacs-lisp~%body~%#+END_SRC~%")
     "empty-header-argument"))

  (it "detects empty inline-source headers and ignores absent or populated headers"
    (expect-document-b-diagnostic
     "Text src_emacs-lisp[:exports]{(+ 1 1)}."
     "empty-header-argument" 6
     "Empty value in header argument \":exports\"")
    (expect-document-b-clean
     "Text src_emacs-lisp{(+ 1 1)}."
     "empty-header-argument")
    (expect-document-b-clean
     "Text src_emacs-lisp[:exports code]{(+ 1 1)}."
     "empty-header-argument"))

  (it "detects empty inline Babel call headers before and after arguments"
    (expect-document-b-diagnostic
     "Text call_demo[:results](value)."
     "empty-header-argument" 6
     "Empty value in header argument \":results\"")
    (expect-document-b-diagnostic
     "Text call_demo(value)[:exports]."
     "empty-header-argument" 6
     "Empty value in header argument \":exports\"")
    (expect-document-b-clean
     "Text call_demo(value)."
     "empty-header-argument")
    (expect-document-b-clean
     "Text call_demo[:results output](value)."
     "empty-header-argument")
    (expect-document-b-clean
     "Text call_demo(value)[:exports results]."
     "empty-header-argument"))

  (it "detects empty block Babel-call headers and ignores absent or populated values"
    (expect-document-b-diagnostic
     "#+CALL: demo[:results](value)"
     "empty-header-argument" 1
     "Empty value in header argument \":results\"")
    (expect-document-b-diagnostic
     "#+CALL: demo(value) :exports"
     "empty-header-argument" 1
     "Empty value in header argument \":exports\"")
    (expect-document-b-clean
     "#+CALL: demo(value)"
     "empty-header-argument")
    (expect-document-b-clean
     "#+CALL: demo[:results output](value) :exports code"
     "empty-header-argument")))

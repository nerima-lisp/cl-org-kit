(in-package #:cl-org-kit/test)

(defun reader-blocks-test-elements (document type)
  (let ((elements '()))
    (map-nodes
     (lambda (node)
       (when (eq (node-type node) type)
         (push node elements)))
     (document-root document))
    (nreverse elements)))

(defun reader-blocks-test-lines (&rest strings)
  (format nil "~{~A~^~%~}" strings))

(describe "block and line element parsing"
          (it "parses container blocks, special blocks, drawers, and footnotes"
              (let* ((source
                      (reader-blocks-test-lines "* Blocks"
                                                "#+begin_quote"
                                                "quoted text"
                                                "#+end_quote"
                                                "#+begin_center"
                                                "centered text"
                                                "#+end_center"
                                                "#+begin_note details"
                                                "special text"
                                                "#+end_note"
                                                "#+begin_comment"
                                                "comment text"
                                                "#+end_comment"
                                                "#+begin_example"
                                                "example text"
                                                "#+end_example"
                                                "#+begin_export html"
                                                "<p>export text</p>"
                                                "#+end_export"
                                                "#+begin_src emacs-lisp -n 1"
                                                "(+ 1 2)"
                                                "#+end_src"
                                                "#+begin_verse"
                                                "line one"
                                                "  line two"
                                                "#+end_verse"
                                                "#+begin: report :scope file"
                                                "dynamic text"
                                                "#+end:"
                                                ":NOTES:"
                                                "drawer text"
                                                "#+begin_example"
                                                "nested literal"
                                                "#+end_example"
                                                ":END:"
                                                "[fn:short] footnote text."))
                     (document (parse-org source))
                     (quote-block
                      (first
                       (reader-blocks-test-elements document :quote-block)))
                     (center-block
                      (first
                       (reader-blocks-test-elements document :center-block)))
                     (special-block
                      (first
                       (reader-blocks-test-elements document :special-block)))
                     (comment-block
                      (first
                       (reader-blocks-test-elements document :comment-block)))
                     (example-block
                      (first
                       (reader-blocks-test-elements document :example-block)))
                     (export-block
                      (first
                       (reader-blocks-test-elements document :export-block)))
                     (src-block
                      (first (reader-blocks-test-elements document :src-block)))
                     (verse-block
                      (first
                       (reader-blocks-test-elements document :verse-block)))
                     (dynamic-block
                      (first
                       (reader-blocks-test-elements document :dynamic-block)))
                     (drawer
                      (first (reader-blocks-test-elements document :drawer)))
                     (footnote
                      (first
                       (reader-blocks-test-elements document
                                                    :footnote-definition))))
                (with-soft-assertions
                 (expect (node-property quote-block :type) :to-be nil)
                 (expect (node-type (first (node-children quote-block)))
                         :to-be
                         :paragraph)
                 (expect (node-property center-block :type) :to-be nil)
                 (expect (node-property special-block :type) :to-equal "note")
                 (expect (node-property special-block :parameters)
                         :to-equal
                         "details")
                 (expect (node-property comment-block :value)
                         :to-equal
                         (format nil "comment text~%"))
                 (expect (node-property example-block :value)
                         :to-equal
                         (format nil "example text~%"))
                 (expect (node-property export-block :type) :to-equal "HTML")
                 (expect (node-property export-block :value)
                         :to-equal
                         (format nil "<p>export text</p>~%"))
                 (expect (node-property src-block :language)
                         :to-equal
                         "emacs-lisp")
                 (expect (node-property src-block :value)
                         :to-equal
                         (format nil "(+ 1 2)~%"))
                 (expect (node-property src-block :switches) :to-equal "-n 1")
                 (expect (node-property verse-block :type) :to-be nil)
                 (expect (node-contents-begin verse-block)
                         :to-be
                         (+ (search "#+begin_verse" source)
                            (length "#+begin_verse")
                            1))
                 (expect (node-property dynamic-block :block-name)
                         :to-equal
                         "report")
                 (expect (node-property dynamic-block :arguments)
                         :to-equal
                         ":scope file")
                 (expect (node-property drawer :drawer-name) :to-equal "NOTES")
                 (expect
                  (find :example-block
                        (node-children drawer)
                        :key
                        #'node-type
                        :test
                        #'eq)
                  :to-be-truthy)
                 (expect (node-property footnote :label) :to-equal "short")
                 (expect (node-type (first (node-children footnote)))
                         :to-be
                         :paragraph))))
          (it
           "keeps empty spans absent and routes NOTE blocks to special-block"
           (let* ((source
                   (reader-blocks-test-lines "* Empty"
                                             "#+begin_quote"
                                             "#+end_quote"
                                             "#+begin_center"
                                             "#+end_center"
                                             "#+begin_note"
                                             "#+end_note"
                                             ":EMPTY:"
                                             ":END:"
                                             "#+begin: empty-dynamic"
                                             "#+end:"
                                             "[fn:empty]"
                                             "[fn:next] following body."))
                  (document (parse-org source))
                  (quote-block
                   (first (reader-blocks-test-elements document :quote-block)))
                  (center-block
                   (first (reader-blocks-test-elements document :center-block)))
                  (special-block
                   (first (reader-blocks-test-elements document :special-block)))
                  (drawer
                   (first (reader-blocks-test-elements document :drawer)))
                  (dynamic-block
                   (first (reader-blocks-test-elements document :dynamic-block)))
                  (footnotes
                   (reader-blocks-test-elements document :footnote-definition)))
             (with-soft-assertions
              (expect (node-contents-begin quote-block) :to-be nil)
              (expect (node-contents-end quote-block) :to-be nil)
              (expect (node-contents-begin center-block) :to-be nil)
              (expect (node-contents-begin special-block) :to-be nil)
              (expect (node-property special-block :type) :to-equal "note")
              (expect (node-contents-begin drawer) :to-be nil)
              (expect (node-property drawer :drawer-name) :to-equal "EMPTY")
              (expect (node-contents-begin dynamic-block) :to-be nil)
              (expect (length footnotes) :to-be 2)
              (expect (node-contents-begin (first footnotes)) :to-be nil)
              (expect (node-contents-end (first footnotes)) :to-be nil)
              (expect (node-property (first footnotes) :label)
                      :to-equal
                      "empty")
              (expect (node-property (second footnotes) :label)
                      :to-equal
                      "next"))))
          (it "parses heading property drawers and treats drawers after CLOSED as ordinary"
              (let* ((source
                      (reader-blocks-test-lines "* Has properties"
                                                ":PROPERTIES:"
                                                ":CUSTOM_ID: property-id"
                                                ":EMPTY:"
                                                ":OWNER+: value"
                                                ":END:"
                                                "After properties."
                                                "* Ordinary drawer"
                                                "CLOSED: [2026-10-10 Sat]"
                                                ":PROPERTIES:"
                                                ":ID: ordinary-id"
                                                ":EMPTY:"
                                                ":END:"))
                     (document (parse-org source))
                     (property-drawers
                      (reader-blocks-test-elements document :property-drawer))
                     (drawers (reader-blocks-test-elements document :drawer))
                     (properties (node-children (first property-drawers)))
                     (ordinary-drawer (first drawers))
                     (ordinary-contents (node-children ordinary-drawer)))
                (with-soft-assertions
                 (expect (length property-drawers) :to-be 1)
                 (expect (length drawers) :to-be 1)
                 (expect (mapcar #'node-type properties)
                         :to-equal
                         '(:node-property :node-property :node-property))
                 (expect
                  (mapcar
                   (lambda (node)
                     (node-property node :key))
                   properties)
                  :to-equal
                  '("CUSTOM_ID" "EMPTY" "OWNER+"))
                 (expect
                  (mapcar
                   (lambda (node)
                     (node-property node :value))
                   properties)
                  :to-equal
                  '("property-id" "" "value"))
                 (expect (node-property ordinary-drawer :drawer-name)
                         :to-equal
                         "PROPERTIES")
                 (expect (length ordinary-contents) :to-be 1)
                 (expect (node-type (first ordinary-contents))
                         :to-be
                         :paragraph)
                 (expect (node-text (first ordinary-contents))
                         :to-equal
                         (format nil ":ID: ordinary-id~%:EMPTY:~%")))))
          (it
           "parses line elements and preserves affiliated paragraph boundaries"
           (let* ((source
                   (reader-blocks-test-lines "#+author: Org parser fixture"
                                             "#+name: attached-name"
                                             "#+caption: An affiliated caption."
                                             "[[https:affiliated-path][affiliated target]]"
                                             "#+CALL: foo()"
                                             "CLOCK: [2026-10-12 Mon 09:00]--[2026-10-12 Mon 09:42] => 0:42"
                                             "clock: [2026-10-11 Sun]"
                                             "CLOCK: => 12:30"
                                             "%%(org-calendar-holiday)"
                                             "-----"
                                             ": A fixed-width line."
                                             ": Another fixed-width line."
                                             "#"
                                             "----"
                                             "#+EMPTY:"
                                             "#+MISSING-COLON"))
                  (document (parse-org source))
                  (paragraphs (reader-blocks-test-elements document :paragraph))
                  (paragraph (first paragraphs))
                  (babel-call
                   (first (reader-blocks-test-elements document :babel-call)))
                  (clocks (reader-blocks-test-elements document :clock))
                  (diary
                   (first (reader-blocks-test-elements document :diary-sexp)))
                  (rule
                   (first
                    (reader-blocks-test-elements document :horizontal-rule)))
                  (fixed-width
                   (first (reader-blocks-test-elements document :fixed-width)))
                  (comment
                   (first (reader-blocks-test-elements document :comment)))
                  (keywords (reader-blocks-test-elements document :keyword))
                  (link-start (search "[[https:" source)))
             (with-soft-assertions
              (expect (node-begin paragraph) :to-be (search "#+name:" source))
              (expect (node-post-affiliated paragraph) :to-be link-start)
              (expect (node-contents-begin paragraph) :to-be link-start)
              (expect (node-property paragraph :name) :to-equal "attached-name")
              (expect (node-property paragraph :caption)
                      :to-equal
                      "An affiliated caption.")
              (expect (node-property babel-call :call) :to-equal "foo")
              (expect (node-property babel-call :arguments) :to-be nil)
              (expect (length clocks) :to-be 3)
              (expect (node-property (first clocks) :status) :to-be :closed)
              (expect (node-type (node-property (first clocks) :value))
                      :to-be
                      :timestamp)
              (expect
               (list (node-end (node-property (first clocks) :value))
                     (node-post-blank (node-property (first clocks) :value))
                     (node-text (node-property (first clocks) :value)))
               :to-equal
               (list (1+ (search " =>" source))
                     1
                     (concatenate 'string
                                  "[2026-10-12 Mon 09:00]--[2026-10-12 Mon 09:42]"
                                  " ")))
              (expect (node-property (second clocks) :status) :to-be :running)
              (expect
               (list (node-type (node-property (second clocks) :value))
                     (node-text (node-property (second clocks) :value))
                     (node-post-blank (node-property (second clocks) :value)))
               :to-equal
               '(:timestamp "[2026-10-11 Sun]" 0))
              (expect (node-property (third clocks) :value) :to-be nil)
              (multiple-value-bind (range present)
                  (node-property (first clocks) :reader-clock-value-range)
                (declare (ignore range))
                (expect present :to-be nil))
              (expect (node-property diary :value)
                      :to-equal
                      "%%(org-calendar-holiday)")
              (expect (node-begin rule) :to-be (search "-----" source))
              (expect (node-property fixed-width :value)
                      :to-equal
                      (format nil
                              "A fixed-width line.~%Another fixed-width line."))
              (expect (node-property comment :value) :to-equal "")
              (expect (length keywords) :to-be 2)
              (expect (node-property (second keywords) :value) :to-equal "")
              (expect (length paragraphs) :to-be 3)
              (expect (search "----" (node-text (second paragraphs)))
                      :to-be-truthy)
              (expect (search "#+MISSING-COLON" (node-text (third paragraphs)))
                      :to-be-truthy))))
  (it "parses LaTeX environments and preserves an unclosed Org quote at EOF"
    (let* ((latex-source
            (reader-blocks-test-lines "\\begin{align}" "x" "\\end{align}"))
           (latex-document (parse-org latex-source))
           (latex (first (reader-blocks-test-elements latex-document
                                                      :latex-environment)))
           (unclosed-source (reader-blocks-test-lines "#+begin_quote" "unfinished"))
           (unclosed-document (parse-org unclosed-source))
           (unclosed (first (reader-blocks-test-elements unclosed-document
                                                        :quote-block))))
      (with-soft-assertions
        (expect (node-type latex) :to-be :latex-environment)
        (expect (node-text latex) :to-equal latex-source)
        (expect (node-property latex :value) :to-equal latex-source)
        (expect (list (node-begin latex) (node-end latex))
                :to-equal
                (list 0 (length latex-source)))
        (expect (node-text unclosed) :to-equal unclosed-source)
        (expect (node-end unclosed) :to-be (length unclosed-source))
        (dolist (source (list (format nil "\\begin{}~%body~%")
                              (format nil "\\begin{broken~%body~%")))
          (expect (reader-blocks-test-elements (parse-org source)
                                               :latex-environment)
                  :to-equal
                  '())))))

  (it "stops multiline footnotes at headlines and preserves comment and call values"
    (let* ((footnote-source
            (reader-blocks-test-lines "* Before"
                                      "[fn:multi] first line"
                                      ""
                                      "second line"
                                      "* After"
                                      "after."))
           (footnote-document (parse-org footnote-source))
           (footnote (first (reader-blocks-test-elements
                             footnote-document :footnote-definition)))
           (next-heading (search "* After" footnote-source))
           (comments-and-call
            (parse-org (reader-blocks-test-lines "# spaced comment"
                                                 "#plain comment"
                                                 "#+CALL: bare-call()")))
           (comments (reader-blocks-test-elements comments-and-call :comment))
           (call (first (reader-blocks-test-elements comments-and-call
                                                     :babel-call))))
      (with-soft-assertions
        (expect (node-property footnote :label) :to-equal "multi")
        (expect (node-end footnote) :to-be next-heading)
        (expect (node-text footnote)
                :to-equal
                (subseq footnote-source
                        (search "[fn:multi]" footnote-source)
                        next-heading))
        (expect (length (node-children footnote)) :to-be 2)
        (expect (mapcar (lambda (node) (node-property node :value)) comments)
                :to-equal
                '("spaced comment" "plain comment"))
        (expect (node-property call :call) :to-equal "bare-call")
        (expect (node-property call :arguments) :to-be nil))))

  (it "preserves empty block openers and drawer-like paragraph text"
    (let* ((source
            (reader-blocks-test-lines "#+BEGIN:"
                                      "#+BEGIN_"
                                      ":A:B:"
                                      ":END:"))
           (document (parse-org source))
           (keywords (reader-blocks-test-elements document :keyword))
           (paragraphs (reader-blocks-test-elements document :paragraph)))
      (with-soft-assertions
        (expect (length keywords) :to-be 1)
        (expect (node-property (first keywords) :key) :to-equal "BEGIN")
        (expect (node-property (first keywords) :value) :to-equal "")
        (expect (length paragraphs) :to-be 1)
        (expect (node-text (first paragraphs))
                :to-equal
                (reader-blocks-test-lines "#+BEGIN_" ":A:B:" ":END:")))))

  (it "parses drawers after planning metadata and preserves their contents"
    (let* ((planned-source
            (reader-blocks-test-lines "* Planned"
                                      "SCHEDULED: <2026-10-12 Mon>"
                                      ":PROPERTIES:"
                                      ":OWNER: planner"
                                      ":END:"))
           (planned-document (parse-org planned-source))
           (planned-drawer
            (first
             (reader-blocks-test-elements planned-document :property-drawer)))
           (empty-source
            (reader-blocks-test-lines "* Empty properties"
                                      ":PROPERTIES:"
                                      ":END:"))
           (empty-document (parse-org empty-source))
           (empty-drawer
            (first
             (reader-blocks-test-elements empty-document :property-drawer)))
           (plain-source
            (reader-blocks-test-lines "* Plain drawer content"
                                      ":NOTES:"
                                      "plain text"
                                      ":END:"))
           (plain-document (parse-org plain-source))
           (plain-drawer
            (first (reader-blocks-test-elements plain-document :drawer)))
           (plain-property-source
            (reader-blocks-test-lines "* Non-property line"
                                      ":PROPERTIES:"
                                      ":DESCRIPTION"
                                      ":END:"))
           (plain-property-document (parse-org plain-property-source))
           (plain-property-drawer
            (first
             (reader-blocks-test-elements plain-property-document
                                          :property-drawer)))
           (short-property-source
            (reader-blocks-test-lines "* Short property field"
                                      ":PROPERTIES:"
                                      "x"
                                      ":END:"))
           (short-property-document (parse-org short-property-source))
           (short-property-drawer
            (first
             (reader-blocks-test-elements short-property-document
                                          :property-drawer)))
           (fixed-property-source
            (reader-blocks-test-lines "* Fixed-width property content"
                                      ":PROPERTIES:"
                                      ":"
                                      ":END:"))
           (fixed-property-document (parse-org fixed-property-source))
           (fixed-property-drawer
            (first
             (reader-blocks-test-elements fixed-property-document
                                          :property-drawer))))
      (with-soft-assertions
        (expect (node-type planned-drawer) :to-be :property-drawer)
        (expect (node-property (first (node-children planned-drawer)) :key)
                :to-equal
                "OWNER")
        (expect (node-type empty-drawer) :to-be :property-drawer)
        (expect (node-children empty-drawer) :to-equal '())
        (expect (node-property plain-drawer :drawer-name)
                :to-equal
                "NOTES")
        (expect (node-type (first (node-children plain-drawer)))
                :to-be
                :paragraph)
        (expect (node-text (first (node-children plain-drawer)))
                :to-equal
                (format nil "plain text~%"))
        (expect (node-type plain-property-drawer) :to-be :property-drawer)
        (expect (node-type (first (node-children plain-property-drawer)))
                :to-be
                :paragraph)
        (expect (node-text (first (node-children plain-property-drawer)))
                :to-equal
                (format nil ":DESCRIPTION~%"))
        (expect (node-type short-property-drawer) :to-be :property-drawer)
        (expect (node-type (first (node-children short-property-drawer)))
                :to-be
                :paragraph)
        (expect (node-text (first (node-children short-property-drawer)))
                :to-equal
                (format nil "x~%"))
        (expect (node-type fixed-property-drawer) :to-be :property-drawer)
        (expect (node-type (first (node-children fixed-property-drawer)))
                :to-be
                :fixed-width)
        (expect (node-property (first (node-children fixed-property-drawer))
                               :value)
                :to-equal
                ""))))

  (it "checks empty fixed-width and example escapes, preserves #+HEADERS spelling"
    (let* ((fixed-document (parse-org ":"))
           (fixed-width
            (first
             (reader-blocks-test-elements fixed-document :fixed-width)))
           (headers-source
            (format nil
                    "#+HEADERS: :var value~%#+BEGIN_SRC emacs-lisp~%body~%#+END_SRC~%"))
           (headers-document (parse-org headers-source))
           (source-block
            (first
             (reader-blocks-test-elements headers-document :src-block)))
           (example-document
            (parse-org
             (reader-blocks-test-lines "#+BEGIN_EXAMPLE" ",#" "#+END_EXAMPLE")))
           (example
            (first (reader-blocks-test-elements example-document :example-block))))
      (with-soft-assertions
        (expect (node-property fixed-width :value) :to-equal "")
        (expect (node-property source-block :header) :to-equal ":var value")
        (expect (emit-org (document-root headers-document)) :to-equal headers-source)
        (expect (node-property example :value)
                :to-equal
                (format nil ",#~%")))))

  (it "parses standard #+CALL fields, empty end headers, and malformed delimiters"
    (let* ((source
             "#+CALL: combine[:var first=1](2, (list 3 4), \"literal (pair)\") [:results output]")
           (document (parse-org source))
           (call (first (reader-blocks-test-elements document :babel-call)))
           (outside-header-source
            (format nil "#+CALL: foo[:inside](bar) :outside~%"))
           (outside-header-document (parse-org outside-header-source))
           (outside-header-call
            (first
             (reader-blocks-test-elements outside-header-document :babel-call)))
           (spaced-delimiter-source
            (format nil "#+CALL: combine [:var value] (2)~%"))
           (spaced-delimiter-emitted-source
            (format nil "#+CALL: combine[:var value] (2)~%"))
           (spaced-delimiter-document (parse-org spaced-delimiter-source))
           (spaced-delimiter-call
            (first
             (reader-blocks-test-elements spaced-delimiter-document :babel-call)))
           (close-delimiter-source
            (format nil "#+CALL: combine )~%"))
           (close-delimiter-document (parse-org close-delimiter-source))
           (close-delimiter-call
            (first
             (reader-blocks-test-elements close-delimiter-document :babel-call))))
      (with-soft-assertions
        (expect (node-property call :call) :to-equal "combine")
        (expect (node-property call :inside-header)
                :to-equal
                ":var first=1")
        (expect (node-property call :arguments)
                :to-equal
                "2, (list 3 4), \"literal (pair)\"")
        (expect (node-property call :end-header)
                :to-equal
                "[:results output]")
        (expect
         (multiple-value-list
          (org-kit::reader-blocks-call-fields
           "combine[:var value=1](2)[]"))
         :to-equal
         (list "combine" ":var value=1" "2" "[]"))
        (expect
         (multiple-value-list
          (org-kit::reader-blocks-call-fields "foo[:inside"))
         :to-equal
         (list "foo" nil nil "[:inside"))
        (expect
         (multiple-value-list
          (org-kit::reader-blocks-call-fields "broken (2, (list 3 4)"))
         :to-equal
         (list "broken" nil nil "(2, (list 3 4)"))
        (expect
         (multiple-value-list
          (org-kit::reader-blocks-call-fields
           "broken[:var first=1](2)[unfinished"))
         :to-equal
         (list "broken" ":var first=1" "2" "[unfinished"))
        (expect (emit-org (document-root document))
                :to-equal
                source)
        (expect (node-property outside-header-call :call) :to-equal "foo")
        (expect (node-property outside-header-call :inside-header)
                :to-equal
                ":inside")
        (expect (node-property outside-header-call :arguments)
                :to-equal
                "bar")
        (expect (node-property outside-header-call :end-header)
                :to-equal
                ":outside")
        (expect (emit-org (document-root outside-header-document))
                :to-equal
                outside-header-source)
        (expect (node-property spaced-delimiter-call :call) :to-equal "combine")
        (expect (node-property spaced-delimiter-call :inside-header)
                :to-equal
                ":var value")
        (expect (node-property spaced-delimiter-call :arguments) :to-equal nil)
        (expect (node-property spaced-delimiter-call :end-header)
                :to-equal
                "(2)")
        (expect (emit-org (document-root spaced-delimiter-document))
                :to-equal
                spaced-delimiter-emitted-source)
        (expect (node-property close-delimiter-call :call) :to-equal "combine")
        (expect (node-property close-delimiter-call :inside-header) :to-equal nil)
        (expect (node-property close-delimiter-call :arguments) :to-equal nil)
        (expect (node-property close-delimiter-call :end-header)
                :to-equal
                ")")
        (expect (emit-org (document-root close-delimiter-document))
                :to-equal
                close-delimiter-source)))))
(describe "Babel CALL field boundaries"
  (it "keeps empty names and appends trailing text to the end header"
    (with-soft-assertions
      (expect
       (multiple-value-list
        (org-kit::reader-blocks-call-fields "[]"))
       :to-equal
       (list "" nil nil nil))
      (expect
       (multiple-value-list
        (org-kit::reader-blocks-call-fields
         "combine[:var value=1](2)[:results output] trailing-token"))
       :to-equal
       (list "combine" ":var value=1" "2"
             "[:results output] trailing-token")))))
(describe "Babel CALL field empty cases"
  (it "distinguishes empty input, bare names, and empty bracketed fields"
    (with-soft-assertions
      (expect
       (multiple-value-list
        (org-kit::reader-blocks-call-fields ""))
       :to-equal
       (list "" nil nil nil))
      (expect
       (multiple-value-list
        (org-kit::reader-blocks-call-fields "combine"))
       :to-equal
       (list "combine" nil nil nil))
      (expect
       (multiple-value-list
        (org-kit::reader-blocks-call-fields "combine[]"))
       :to-equal
       (list "combine" nil nil nil))
      (expect
       (multiple-value-list
        (org-kit::reader-blocks-call-fields "combine[:var value"))
       :to-equal
       (list "combine" nil nil "[:var value"))
      (expect
       (multiple-value-list
        (org-kit::reader-blocks-call-fields "combine[:var value=1]"))
       :to-equal
       (list "combine" ":var value=1" nil nil))
      (expect
       (multiple-value-list
        (org-kit::reader-blocks-call-fields "combine( )"))
       :to-equal
       (list "combine" nil nil nil))
      (expect
       (multiple-value-list
        (org-kit::reader-blocks-call-fields "foo()  "))
       :to-equal
       (list "foo" nil nil nil))
      (expect
       (multiple-value-list
        (org-kit::reader-blocks-call-fields "combine[:results output]"))
       :to-equal
       (list "combine" ":results output" nil nil)))))

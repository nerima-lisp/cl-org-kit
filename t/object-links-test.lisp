(in-package #:cl-org-kit/test)

(defun link-test-match (source &optional
                               start
                               end
                               range-start
                               settings
                               prepare-p)
  (let ((document (make-document source)))
    (when settings
      (setf (document-settings document) settings))
    (when prepare-p
      (org-kit::prepare-object-parsing document))
    (let ((org-kit::*object-parse-range-start* range-start))
      (values
       (org-kit::try-parse-link-object document
                                       (or start 0)
                                       (or end (length source))
                                       :paragraph)
       document))))

(defun link-test-property (match key)
  (getf (org-kit::object-match-properties match) key))

(describe "link-related object recognition"
          (it
           "recognizes plain, angle, and bracket links with Org property values"
           (let* ((plain-source "Text https:plain-path.")
                  (plain-start (search "https:" plain-source))
                  (plain-match (link-test-match plain-source plain-start))
                  (angle-source "<https:angle-path>")
                  (angle-match (link-test-match angle-source))
                  (bracket-source
                   "[[https:bracket-path][a *bold* description]]")
                  (bracket-match (link-test-match bracket-source))
                  (bracket-description-start
                   (+ (position #\] bracket-source) 2))
                  (bracket-description-end
                   (search "]]"
                           bracket-source
                           :start2
                           bracket-description-start)))
             (with-soft-assertions
              (expect (org-kit::object-match-type plain-match) :to-be :link)
              (expect (org-kit::object-match-end plain-match)
                      :to-be
                      (position #\. plain-source :from-end t))
              (expect (org-kit::object-match-properties plain-match)
                      :to-equal
                      '(:type "https" :path "plain-path" :format :plain))
              (expect (org-kit::object-match-type angle-match) :to-be :link)
              (expect (org-kit::object-match-end angle-match)
                      :to-be
                      (length angle-source))
              (expect (org-kit::object-match-properties angle-match)
                      :to-equal
                      '(:type "https" :path "angle-path" :format :angle))
              (expect (org-kit::object-match-type bracket-match) :to-be :link)
              (expect (org-kit::object-match-end bracket-match)
                      :to-be
                      (length bracket-source))
              (expect (org-kit::object-match-contents-begin bracket-match)
                      :to-be
                      bracket-description-start)
              (expect (org-kit::object-match-contents-end bracket-match)
                      :to-be
                      bracket-description-end)
              (expect (org-kit::object-match-properties bracket-match)
                      :to-equal
                      '(:type "https" :path "bracket-path" :format :bracket)))))
          (it
           "recognizes fuzzy and custom-id bracket links and expands abbreviations"
           (let* ((internal-source "[[#local-anchor][anchor link]]")
                  (internal-match (link-test-match internal-source))
                  (fuzzy-match (link-test-match "[[docs/chapter]]"))
                  (abbrev-source "[[web:manual]]")
                  (abbrev-match
                   (link-test-match abbrev-source
                                    0
                                    (length abbrev-source)
                                    0
                                    '(:link-abbrevs
                                      (("web" . "https://example.test/%s"))))))
             (with-soft-assertions
              (expect (org-kit::object-match-properties internal-match)
                      :to-equal
                      '(:type "custom-id" :path "local-anchor" :format :bracket))
              (expect (org-kit::object-match-contents-begin internal-match)
                      :to-be
                      17)
              (expect (org-kit::object-match-contents-end internal-match)
                      :to-be
                      28)
              (expect (org-kit::object-match-properties fuzzy-match)
                      :to-equal
                      '(:type "fuzzy" :path "docs/chapter" :format :bracket))
              (expect (org-kit::object-match-properties abbrev-match)
                      :to-equal
                      '(:type "https"
                              :path
                              "//example.test/manual"
                              :format
                              :bracket)))))
          (it "recognizes radio targets and links from document-local targets"
              (let* ((source
                      "<<<radio *target* phrase>>> radio *target* phrase.")
                     (document (make-document source)))
                (org-kit::prepare-object-parsing document)
                (let* ((target-match
                        (let ((org-kit::*object-parse-range-start* 0))
                          (org-kit::try-parse-link-object document
                                                          0
                                                          (length source)
                                                          :paragraph)))
                       (link-start
                        (search "radio *target* phrase" source :start2 4))
                       (link-match
                        (let ((org-kit::*object-parse-range-start* 0))
                          (org-kit::try-parse-link-object document
                                                          link-start
                                                          (length source)
                                                          :paragraph))))
                  (with-soft-assertions
                   (expect (org-kit::object-match-type target-match)
                           :to-be
                           :radio-target)
                   (expect (link-test-property target-match :value)
                           :to-equal
                           "radio *target* phrase")
                   (expect (org-kit::object-match-contents-begin target-match)
                           :to-be
                           3)
                   (expect (org-kit::object-match-contents-end target-match)
                           :to-be
                           24)
                   (expect (org-kit::object-match-type link-match) :to-be :link)
                   (expect (org-kit::object-match-contents-begin link-match)
                           :to-be
                           link-start)
                   (expect (org-kit::object-match-contents-end link-match)
                           :to-be
                           (+ link-start 21))
                   (expect (org-kit::object-match-properties link-match)
                           :to-equal
                           '(:type "radio"
                                   :path
                                   "radio *target* phrase"
                                   :format
                                   :plain))))))
          (it "recognizes targets, macros, and export snippets"
              (let* ((target-source "x <<local-anchor>>")
                     (target-match (link-test-match target-source 2))
                     (macro-source "{{{greet(日本語)}}}  tail")
                     (macro-match (link-test-match macro-source))
                     (export-source "@@html:<mark>inline export</mark>@@")
                     (export-match (link-test-match export-source)))
                (with-soft-assertions
                 (expect (org-kit::object-match-type target-match)
                         :to-be
                         :target)
                 (expect (link-test-property target-match :value)
                         :to-equal
                         "local-anchor")
                 (expect (org-kit::object-match-type macro-match) :to-be :macro)
                 (expect (org-kit::object-match-properties macro-match)
                         :to-equal
                         '(:key "greet"
                                :args
                                ("日本語")
                                :value
                                "{{{greet(日本語)}}}"))
                 (expect (org-kit::object-match-end macro-match)
                         :to-be
                         (+ (search "  tail" macro-source) 2))
                 (expect (org-kit::object-match-post-blank macro-match)
                         :to-be
                         2)
                 (expect (org-kit::object-match-type export-match)
                         :to-be
                         :export-snippet)
                 (expect (org-kit::object-match-properties export-match)
                         :to-equal
                         '(:back-end "html" :value "<mark>inline export</mark>")))))
          (it "recognizes standard and inline footnote references"
              (let* ((source "[fn:short] and [fn::inline note]")
                     (standard (link-test-match source))
                     (inline-start (search "[fn::" source))
                     (inline (link-test-match source inline-start))
                     (inline-contents-start (+ inline-start 5))
                     (inline-contents-end
                      (position #\] source :start inline-contents-start)))
                (with-soft-assertions
                 (expect (org-kit::object-match-type standard)
                         :to-be
                         :footnote-reference)
                 (expect (org-kit::object-match-properties standard)
                         :to-equal
                         '(:label "short" :type :standard))
                 (expect (org-kit::object-match-type inline)
                         :to-be
                         :footnote-reference)
                 (expect (org-kit::object-match-properties inline)
                         :to-equal
                         '(:label nil :type :inline))
                 (expect (org-kit::object-match-contents-begin inline)
                         :to-be
                         inline-contents-start)
                 (expect (org-kit::object-match-contents-end inline)
                         :to-be
                         inline-contents-end))))
          (it "recognizes citations and their reference children"
              (let* ((source "[cite/t: see;@smith2024;@doe-2025 p. 4;as noted]")
                     (citation (link-test-match source))
                     (first-key (search "@smith2024" source))
                     (second-key (search "@doe-2025" source))
                     (contents-end (1+ (search ";as noted" source)))
                     (first-reference
                      (link-test-match source first-key second-key))
                     (second-reference
                      (link-test-match source second-key contents-end)))
                (with-soft-assertions
                 (expect (org-kit::object-match-type citation) :to-be :citation)
                 (expect (org-kit::object-match-properties citation)
                         :to-equal
                         '(:style "t" :key nil))
                 (expect (org-kit::object-match-contents-begin citation)
                         :to-be
                         first-key)
                 (expect (org-kit::object-match-contents-end citation)
                         :to-be
                         contents-end)
                 (expect (org-kit::object-match-type first-reference)
                         :to-be
                         :citation-reference)
                 (expect (org-kit::object-match-end first-reference)
                         :to-be
                         second-key)
                 (expect (org-kit::object-match-properties first-reference)
                         :to-equal
                         '(:key "smith2024"
                                :prefix
                                nil
                                :suffix
                                nil
                                :locator
                                nil))
                 (expect (org-kit::object-match-type second-reference)
                         :to-be
                         :citation-reference)
                 (expect (org-kit::object-match-end second-reference)
                         :to-be
                         contents-end)
                 (expect (org-kit::object-match-properties second-reference)
                         :to-equal
                         '(:key "doe-2025"
                                :prefix
                                nil
                                :suffix
                                (" p. 4")
                                :locator
                                nil)))))
          (it
           "preserves the target interpretation after an unmatched radio opener"
           (let* ((source
                   (format nil
                           "Radio-link boundary: <<<unmatched radio marker.~%Plain-link boundary: https: spaced-path.~%Regular-link boundary: [[bracket-path][label].~%Macro boundary: {{{unterminated macro.~%Target boundary: <<unterminated target.~%Radio-target boundary: <<<unterminated radio target>>>."))
                  (document (make-document source))
                  (radio-start (search "<<<unterminated radio target" source))
                  (target-start (1+ radio-start)))
             (let ((org-kit::*object-parse-range-start* 0))
               (with-soft-assertions
                (expect
                 (org-kit::try-parse-link-object document
                                                 radio-start
                                                 (length source)
                                                 :paragraph)
                 :to-be
                 nil)
                (let ((match
                       (org-kit::try-parse-link-object document
                                                       target-start
                                                       (length source)
                                                       :paragraph)))
                  (expect (org-kit::object-match-type match) :to-be :target)
                  (expect (link-test-property match :value)
                          :to-equal
                          "unterminated radio target")
                  (expect (org-kit::object-match-end match)
                          :to-be
                          (+ target-start
                             (length "<<unterminated radio target>>"))))))))
          (it
           "rejects malformed forms and keeps scanning inside the supplied range"
           (dolist
               (source
                '("https: spaced-path" "[[bracket-path][label]"
                                       "{{{unterminated macro"
                                       "<<unterminated target"
                                       "@@html:<b>unclosed"
                                       "[fn:]"
                                       "[fn:has space]"
                                       "[cite @missing-colon]"))
             (expect (link-test-match source) :to-be nil))
           (let* ((source "https:path   tail")
                  (object-end (search "   " source))
                  (within-range (link-test-match source 0 (+ object-end 2))))
             (with-soft-assertions
              (expect (org-kit::object-match-end within-range)
                      :to-be
                      (+ object-end 2))
              (expect (org-kit::object-match-post-blank within-range) :to-be 2)
              (expect (link-test-match "https:path" 0 5) :to-be nil))))
          (it "allows markup when radio links are forbidden"
              (let* ((source "<<<*needle* x>>> [[dest][*needle* x]]")
                     (document (make-document source))
                     (start (search "*needle*" source :from-end t))
                     (match nil))
                (org-kit::prepare-object-parsing document)
                (let ((org-kit::*object-parse-range-start* start))
                  (setf match (org-kit::%try-parse-object document
                                                          start
                                                          (length source)
                                                          :link)))
                (with-soft-assertions
                 (expect (org-kit::object-match-type match) :to-be :bold)
                 (expect (org-kit::object-match-end match)
                         :to-be
                         (+ start (length "*needle* "))))))
          (it "prefers the longest radio target candidate"
              (let* ((source "<<<a b>>> <<<a>>> a b")
                     (document (make-document source))
                     (start (search "a b" source :from-end t))
                     (match nil))
                (org-kit::prepare-object-parsing document)
                (let ((org-kit::*object-parse-range-start* 0))
                  (setf match (org-kit::try-parse-link-object document
                                                              start
                                                              (length source)
                                                              :paragraph)))
                (with-soft-assertions
                 (expect (org-kit::object-match-type match) :to-be :link)
                 (expect (link-test-property match :path) :to-equal "a b")
                 (expect (org-kit::object-match-end match)
                         :to-be
                         (+ start (length "a b"))))))
          (it "rejects shorter radio candidates and identifier substrings"
              (let* ((longest-source "<<<a b>>> <<<a>>> <<<a b c>>> a b c.")
                     (longest-document (make-document longest-source))
                     (longest-start (search "a b c." longest-source))
                     (before-source "<<<target>>>x_target")
                     (before-document (make-document before-source))
                     (before-start (1+ (search "_target" before-source)))
                     (after-source "<<<target>>> targets")
                     (after-document (make-document after-source))
                     (after-start (search "target" after-source :from-end t)))
                (org-kit::prepare-object-parsing longest-document)
                (org-kit::prepare-object-parsing before-document)
                (org-kit::prepare-object-parsing after-document)
                (let ((longest-match
                       (let ((org-kit::*object-parse-range-start* 0))
                         (org-kit::try-parse-link-object longest-document
                                                         longest-start
                                                         (length longest-source)
                                                         :paragraph)))
                      (before-match
                       (let ((org-kit::*object-parse-range-start* 0))
                         (org-kit::try-parse-link-object before-document
                                                         before-start
                                                         (length before-source)
                                                         :paragraph)))
                      (after-match
                       (let ((org-kit::*object-parse-range-start* 0))
                         (org-kit::try-parse-link-object after-document
                                                         after-start
                                                         (length after-source)
                                                         :paragraph))))
                  (with-soft-assertions
                   (expect (link-test-property longest-match :path)
                           :to-equal
                           "a b c")
                   (expect (org-kit::object-match-end longest-match)
                           :to-be
                           (+ longest-start (length "a b c")))
                   (expect before-match :to-be nil)
                   (expect after-match :to-be nil)))))
          (it "classifies bracket paths and expands literal link abbreviations"
              (let* ((file-match (link-test-match "[[../chapter.org]]"))
                     (coderef-match (link-test-match "[[(symbol)]]"))
                     (unknown-scheme-match
                      (link-test-match "[[my+scheme:target]]"))
                     (abbrev-source "[[web:manual]]")
                     (abbrev-match
                      (link-test-match abbrev-source
                                       0
                                       (length abbrev-source)
                                       0
                                       '(:link-abbrevs
                                         (("web" . "https://docs.example/"))))))
                (with-soft-assertions
                 (expect (org-kit::object-match-properties file-match)
                         :to-equal
                         '(:type "file" :path "../chapter.org" :format :bracket))
                 (expect (org-kit::object-match-properties coderef-match)
                         :to-equal
                         '(:type "coderef" :path "symbol" :format :bracket))
                 (expect (org-kit::object-match-properties unknown-scheme-match)
                         :to-equal
                         '(:type "fuzzy"
                                 :path
                                 "my+scheme:target"
                                 :format
                                 :bracket))
                 (expect (org-kit::object-match-properties abbrev-match)
                         :to-equal
                         '(:type "https"
                                 :path
                                 "//docs.example/manual"
                                 :format
                                 :bracket)))))
          (it
           "handles escaped brackets, bracket path whitespace, and nested descriptions"
           (let* ((escaped-source "[[foo\\]bar]]")
                  (escaped-match (link-test-match escaped-source))
                  (space-match (link-test-match "[[two words]]"))
                  (newline-source (format nil "[[first~%  second]]"))
                  (newline-match (link-test-match newline-source))
                  (description-source "[[dest][nested [label] text]]")
                  (description-start (search "nested" description-source))
                  (description-end
                   (search "]]" description-source :start2 description-start))
                  (description-match (link-test-match description-source))
                  (malformed-match (link-test-match "[[dest]x")))
             (with-soft-assertions
              (expect (link-test-property escaped-match :path)
                      :to-equal
                      "foo]bar")
              (expect (link-test-property space-match :path)
                      :to-equal
                      "two words")
              (expect (link-test-property newline-match :path)
                      :to-equal
                      "first second")
              (expect (org-kit::object-match-type description-match)
                      :to-be
                      :link)
              (expect (org-kit::object-match-contents-begin description-match)
                      :to-be
                      description-start)
              (expect (org-kit::object-match-contents-end description-match)
                      :to-be
                      description-end)
              (expect (org-kit::link-object-normalize-bracket-path "trailing ")
                      :to-equal
                      "trailing ")
              (expect malformed-match :to-be nil))))
          (it "keeps balanced parentheses in plain link paths"
              (let* ((source "https:path(inner))")
                     (match (link-test-match source)))
                (with-soft-assertions
                 (expect (org-kit::object-match-properties match)
                         :to-equal
                         '(:type "https" :path "path(inner)" :format :plain))
                 (expect (org-kit::object-match-end match)
                         :to-be
                         (1- (length source))))))
          (it
           "normalizes macro arguments and accepts export backend digits and hyphens"
           (let* ((macro-source "{{{greet(alpha   beta, gamma)}}}")
                  (macro-match (link-test-match macro-source))
                  (empty-args-match (link-test-match "{{{greet(   )}}}"))
                  (export-match (link-test-match "@@html5-custom:<b>x</b>@@"))
                  (malformed-match (link-test-match "{{{greet(alpha}}}")))
             (with-soft-assertions
              (expect (org-kit::object-match-type macro-match) :to-be :macro)
              (expect (link-test-property macro-match :args)
                      :to-equal
                      '("alpha beta" "gamma"))
              (expect (link-test-property empty-args-match :args) :to-be nil)
              (expect (org-kit::object-match-properties export-match)
                      :to-equal
                      '(:back-end "html5-custom" :value "<b>x</b>"))
              (expect malformed-match :to-be nil))))
          (it "handles escaped closing brackets in footnotes and citations"
              (let* ((footnote-source "[fn::note \\] tail]")
                     (footnote-match (link-test-match footnote-source))
                     (citation-source "[cite: see;@key note \\] tail]")
                     (citation-match (link-test-match citation-source))
                     (invalid-style-match (link-test-match "[cite/: @key]")))
                (with-soft-assertions
                 (expect (org-kit::object-match-type footnote-match)
                         :to-be
                         :footnote-reference)
                 (expect (link-test-property footnote-match :type)
                         :to-be
                         :inline)
                 (expect (org-kit::object-match-contents-end footnote-match)
                         :to-be
                         (1- (length footnote-source)))
                 (expect (org-kit::object-match-type citation-match)
                         :to-be
                         :citation)
                 (expect (org-kit::object-match-properties citation-match)
                         :to-equal
                         '(:style nil :key nil))
                 (expect (org-kit::object-match-end citation-match)
                         :to-be
                         (length citation-source))
                 (expect invalid-style-match :to-be nil))))
          (it "covers link boundary and delimiter fallbacks"
  (let* ((radio-space (link-test-match "<<<bad >>>"))
         (radio-newline (link-test-match (format nil "<<<a~%b>>>")))
         (empty-plain (link-test-match "https:"))
         (empty-angle (link-test-match "<https:>"))
         (plain-source "https:path]tail")
         (plain-match (link-test-match plain-source))
         (angle-lf (link-test-match (format nil "<https:line~%break>")))
         (angle-cr
           (link-test-match
            (concatenate 'string "<https:line" (string #\Return) "break>")))
         (abbrev-source "[[web::manual]]")
         (abbrev-match
           (link-test-match abbrev-source
                            0
                            (length abbrev-source)
                            0
                            '(:link-abbrevs (("web" . "https://docs.example/")))))
         (citation-source "[cite: @key note   ]")
         (citation-match (link-test-match citation-source))
         (escaped-description-source "[[dest][label \\] tail]]")
         (escaped-description (link-test-match escaped-description-source))
         (nested-footnote-source "[fn::outer [inner] tail]")
         (nested-footnote (link-test-match nested-footnote-source))
         (nested-citation-source "[cite: see;@key [note]]")
         (nested-citation (link-test-match nested-citation-source)))
    (with-soft-assertions
      (expect radio-space :to-be nil)
      (expect radio-newline :to-be nil)
      (expect empty-plain :to-be nil)
      (expect empty-angle :to-be nil)
      (expect (org-kit::object-match-properties plain-match)
              :to-equal
              '(:type "https" :path "path" :format :plain))
      (expect (org-kit::object-match-end plain-match)
              :to-be
              (length "https:path"))
      (expect angle-lf :to-be nil)
      (expect angle-cr :to-be nil)
      (expect (org-kit::object-match-properties abbrev-match)
              :to-equal
              '(:type "https"
                      :path "//docs.example/manual"
                      :format :bracket))
      (expect (org-kit::object-match-contents-end citation-match)
              :to-be
              (search "   ]" citation-source))
      (expect (org-kit::object-match-type escaped-description)
              :to-be
              :link)
      (expect (org-kit::object-match-contents-end escaped-description)
              :to-be
              (search "]]" escaped-description-source))
      (expect (org-kit::object-match-end escaped-description)
              :to-be
              (length escaped-description-source))
      (expect (org-kit::object-match-type nested-footnote)
              :to-be
              :footnote-reference)
      (expect (org-kit::object-match-end nested-footnote)
              :to-be
              (length nested-footnote-source))
      (expect (org-kit::object-match-type nested-citation)
              :to-be
              :citation)
      (expect (org-kit::object-match-end nested-citation)
              :to-be
              (length nested-citation-source))))))

(describe "link helper boundaries"
  (it "checks ranges, prefixes, whitespace, and delimiters"
    (let ((document (make-document "x")))
      (with-soft-assertions
        (expect (org-kit::link-object-valid-range-p document 0 1) :to-be t)
        (expect (org-kit::link-object-valid-range-p document 1 1) :to-be nil)
        (expect (org-kit::link-object-valid-range-p document -1 1) :to-be nil)
        (expect (org-kit::link-object-prefix-at-p "abc" 0 3 "abc") :to-be t)
        (expect (org-kit::link-object-prefix-at-p "abc" 0 2 "abc") :to-be nil)
        (expect (org-kit::link-object-whitespace-p #\Space) :to-be-truthy)
        (expect (org-kit::link-object-whitespace-p #\Return) :to-be-truthy)
        (expect (org-kit::link-object-whitespace-p #\x) :to-be nil)
        (expect (org-kit::link-object-post-blank-end (format nil "a ~cX" #\Tab)
                                                     1
                                                     4)
                :to-be
                3)
        (expect (org-kit::link-object-post-blank-end "abc" 1 3) :to-be 1)
        (expect (org-kit::link-object-boundary-before-p "abc" 0) :to-be t)
        (expect (org-kit::link-object-boundary-before-p "x y" 2) :to-be t)
        (expect (org-kit::link-object-boundary-before-p "x_y" 2) :to-be nil)
        (expect (org-kit::link-object-boundary-before-p "xay" 2) :to-be nil)
        (expect (org-kit::link-object-boundary-after-p "x" 1 1) :to-be t)
        (expect (org-kit::link-object-boundary-after-p "x!" 1 2) :to-be t)
        (expect (org-kit::link-object-boundary-after-p "x_" 1 2) :to-be nil)
        (expect (org-kit::link-object-boundary-after-p "xa" 1 2) :to-be nil)
        (expect
         (org-kit::link-object-target-boundary-p "<<a>>" 0 5 "<<" ">>")
         :to-be
         t)
        (expect
         (org-kit::link-object-target-boundary-p "<<>>" 0 4 "<<" ">>")
         :to-be
         nil)
        (expect
         (org-kit::link-object-target-boundary-p "<< a>>" 0 6 "<<" ">>")
         :to-be
         nil)
        (expect
         (org-kit::link-object-target-boundary-p "<<a >>" 0 6 "<<" ">>")
         :to-be
         nil)
        (expect
         (org-kit::link-object-target-boundary-p "<<a>b>>" 0 7 "<<" ">>")
         :to-be
         nil)
        (expect
         (org-kit::link-object-target-boundary-p (format nil "<<a~%b>>")
                                                 0
                                                 7
                                                 "<<"
                                                 ">>")
         :to-be
         nil))))
  (it "expands only registered link abbreviations"
    (let ((document (make-document "")))
      (with-soft-assertions
        (expect (org-kit::link-object-expand-abbreviation document "plain")
                :to-equal
                "plain")
        (setf (document-settings document)
              '(:link-abbrevs (("web" . "https://example.test/%s"))))
        (expect (org-kit::link-object-expand-abbreviation document "web::manual")
                :to-equal
                "https://example.test/manual")
        (expect (org-kit::link-object-expand-abbreviation document "web:")
                :to-equal
                "https://example.test/")
        (setf (document-settings document)
              '(:link-abbrevs (("web" . "https://example.test/"))))
        (expect (org-kit::link-object-expand-abbreviation document "web:manual")
                :to-equal
                "https://example.test/manual")
        (setf (document-settings document) '(:link-abbrevs "invalid"))
        (expect (org-kit::link-object-expand-abbreviation document "web:manual")
                :to-equal
                "web:manual")
        (setf (document-settings document)
              '(:link-abbrevs (("web" . 42))))
        (expect (org-kit::link-object-expand-abbreviation document "web:manual")
                :to-equal
                "web:manual")
        (expect (org-kit::object-match-post-blank
                 (org-kit::link-object-match :link 1))
                :to-be
                0)))))

(describe "link parser edge branches"
  (it "covers path classification, invalid ranges, and delimiter fallbacks"
    (let ((document (make-document "")))
      (with-soft-assertions
        (expect (org-kit::link-object-valid-range-p document 1 0)
                :to-be
                nil)
        (expect (org-kit::link-object-known-scheme-end "https:path")
                :to-equal
                "https")
        (expect (org-kit::link-object-known-scheme-end "bad_:path")
                :to-be
                nil)
        (expect (org-kit::link-object-classify-bracket-path document "/absolute")
                :to-equal
                "file")
        (expect (org-kit::link-object-classify-bracket-path document "./relative")
                :to-equal
                "file")
        (expect (org-kit::link-object-classify-bracket-path document "../relative")
                :to-equal
                "file")
        (expect (org-kit::link-object-classify-bracket-path document "(symbol)")
                :to-equal
                "coderef")
        (expect (org-kit::link-object-classify-bracket-path document "#anchor")
                :to-equal
                "custom-id")
        (expect (org-kit::link-object-classify-bracket-path document "relative")
                :to-equal
                "fuzzy")
        (expect (org-kit::link-object-normalize-bracket-path
                 (format nil "a ~c~% ~cb" #\Tab #\Tab))
                :to-equal
                "a b")
        (expect (org-kit::link-object-bracket-close "a\\]b]" 0 5) :to-be 4)
        (expect (org-kit::link-object-bracket-description-close "a[b]c]]" 0 7)
                :to-be
                5)
        (expect (org-kit::link-object-bracket (make-document "[]") 0 2)
                :to-be
                nil)
        (expect (org-kit::link-object-plain-path-end "abc." 0 4) :to-be 3)
        (expect (org-kit::link-object-plain-path-end "..." 0 3) :to-be nil)
        (expect (org-kit::link-object-angle-link (make-document "<http:>")
                                                0
                                                7)
                :to-be
                nil)
        (expect (org-kit::link-object-angle-link (make-document "<unknown:x>")
                                                0
                                                11)
                :to-be
                nil)
        (expect (org-kit::link-object-macro (make-document "{{{name}}}") 0 10)
                :to-be-truthy)
        (expect (org-kit::link-object-macro (make-document "{{{name(a)}}}")
                                            0
                                            13)
                :to-be-truthy)
        (expect (org-kit::link-object-macro (make-document "{{{name(a}}}")
                                            0
                                            12)
                :to-be
                nil)
        (expect (org-kit::link-object-export-snippet (make-document "@@html:@@")
                                                    0
                                                    9)
                :to-be-truthy)
        (expect (org-kit::link-object-export-snippet (make-document "@@html:value")
                                                    0
                                                    12)
                :to-be
                nil)
        (expect (org-kit::link-object-export-snippet
                 (make-document "@@h_tml:value@@") 0 15)
                :to-be
                nil)
        (expect (org-kit::link-object-find-close-bracket "\\]" 0 2)
                :to-be
                nil)
        (expect (org-kit::link-object-footnote (make-document "[fn:]") 0 5)
                :to-be
                nil)
        (expect (org-kit::link-object-footnote (make-document "[fn:label]")
                                               0
                                               10)
                :to-be-truthy)
        (expect (org-kit::link-object-footnote (make-document "[fn:label:]")
                                               0
                                               11)
                :to-be
                nil)
        (expect (org-kit::link-object-citation (make-document "[cite/?:key]")
                                               0
                                               12)
                :to-be
                nil)
        (expect (org-kit::link-object-citation (make-document "[cite: ]") 0 8)
                :to-be
                nil)
        (expect (org-kit::try-parse-link-object document -1 0 :paragraph)
                :to-be
                nil)))))

(describe "link parser remaining branch outcomes"
  (it "covers short paths, malformed delimiters, and citation variants"
    (let ((document (make-document "")))
      (with-soft-assertions
        (expect (org-kit::link-object-classify-bracket-path document "")
                :to-equal
                "fuzzy")
        (expect (org-kit::link-object-classify-bracket-path document ".")
                :to-equal
                "fuzzy")
        (expect (org-kit::link-object-classify-bracket-path document "..")
                :to-equal
                "fuzzy")
        (expect (org-kit::link-object-normalize-bracket-path
                 (format nil "a~%b"))
                :to-equal
                "a b")
        (expect (org-kit::link-object-bracket-close "]" 0 1) :to-be 0)
        (expect (org-kit::link-object-bracket-close "\\]" 0 2) :to-be nil)
        (expect (org-kit::link-object-bracket-description-close "a\\]b]]" 0 6)
                :to-be
                4)
        (expect (org-kit::link-object-bracket (make-document "[[path]]") 0 8)
                :to-be-truthy)
        (expect (org-kit::link-object-bracket (make-document "[[path]") 0 7)
                :to-be
                nil)
        (expect (org-kit::link-object-angle-link (make-document "x") 0 1)
                :to-be
                nil)
        (expect (org-kit::link-object-angle-link (make-document "<") 0 1)
                :to-be
                nil)
        (expect (org-kit::link-object-angle-link (make-document "<http") 0 5)
                :to-be
                nil)
        (expect (org-kit::link-object-macro (make-document "{{{}}}") 0 6)
                :to-be
                nil)
        (expect (org-kit::link-object-macro (make-document "{{{name}}") 0 8)
                :to-be
                nil)
        (expect (org-kit::link-object-export-snippet (make-document "@@:value@@")
                                                    0
                                                    10)
                :to-be
                nil)
        (expect (org-kit::link-object-export-snippet (make-document "@@html:")
                                                    0
                                                    7)
                :to-be
                nil)
        (expect (org-kit::link-object-footnote
                 (make-document "[fn:l:a\\]b]") 0 11)
                :to-be-truthy)
        (expect (org-kit::link-object-citation
                 (make-document "[cite/author:@ref]") 0 18)
                :to-be-truthy)
        (expect (org-kit::link-object-citation
                 (make-document "[cite:@ref]") 0 11)
                :to-be-truthy)
        (expect (org-kit::link-object-citation (make-document "[cite:   ]") 0 10)
                :to-be
                nil)
        (expect (org-kit::link-object-citation
                 (make-document "[cite:@key;]") 0 12)
                :to-be-truthy)
        (expect (org-kit::link-object-citation (make-document "[cite/x? :ref]")
                                               0
                                               14)
                :to-be
                nil))))
  (it "covers incomplete citation and bracket scanning states"
    (with-soft-assertions
      (expect (org-kit::link-object-normalize-bracket-path
               (format nil "a~%"))
              :to-equal
              "a ")
      (expect (org-kit::link-object-bracket-description-close "a]x" 0 3)
              :to-be
              nil)
      (expect (org-kit::link-object-bracket (make-document "[[path") 0 6)
              :to-be
              nil)
      (expect (org-kit::link-object-macro (make-document "{{name}}") 0 8)
              :to-be
              nil)
      (expect (org-kit::link-object-footnote
               (make-document "[fn:a-b_c]") 0 10)
              :to-be-truthy)
      (expect (org-kit::link-object-citation (make-document "[cite @key]") 0 11)
              :to-be
              nil)
      (expect (org-kit::link-object-citation (make-document "x") 0 1)
              :to-be
              nil)
      (expect (org-kit::link-object-citation (make-document "[cite:@key") 0 10)
              :to-be
              nil))))

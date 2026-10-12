(in-package #:cl-org-kit/test)

(defun link-test-match (source &optional
                               start
                               end
                               range-start
                               settings
                               prepare-p
                               (container-type :paragraph))
  (let ((document (make-document source)))
    (when settings
      (setf (document-settings document) settings))
    (when prepare-p
      (org-kit::prepare-object-parsing document))
    (let* ((start (or start 0))
           (end (or end (length source)))
           (scan-start (or range-start start))
           (match
             (find-if (lambda (object)
                        (and (= (node-begin object) start)
                             (not (eq (node-type object) :plain-text))))
                      (org-kit::parse-objects document
                                              scan-start
                                              end
                                              container-type))))
      (values match document))))

(defun link-test-property (object key)
  (node-property object key))

(defun link-test-org-objects (source &rest parse-org-arguments)
  (let ((document (apply #'parse-org source parse-org-arguments)))
    (labels ((collect (node)
               (append
                (when (member (node-type node)
                              '(:bold
                                :citation
                                :citation-reference
                                :code
                                :export-snippet
                                :footnote-reference
                                :italic
                                :link
                                :macro
                                :radio-target
                                :strike-through
                                :target
                                :underline
                                :verbatim))
                  (list node))
                (mapcan #'collect (node-children node)))))
      (mapcan #'collect (node-children (document-root document))))))

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
              (expect (node-type plain-match) :to-be :link)
              (expect (node-end plain-match)
                      :to-be
                      (position #\. plain-source :from-end t))
              (expect (node-properties plain-match)
                      :to-equal
                      '(:type "https" :path "plain-path" :format :plain))
              (expect (node-type angle-match) :to-be :link)
              (expect (node-end angle-match)
                      :to-be
                      (length angle-source))
              (expect (node-properties angle-match)
                      :to-equal
                      '(:type "https" :path "angle-path" :format :angle))
              (expect (node-type bracket-match) :to-be :link)
              (expect (node-end bracket-match)
                      :to-be
                      (length bracket-source))
              (expect (node-contents-begin bracket-match)
                      :to-be
                      bracket-description-start)
              (expect (node-contents-end bracket-match)
                      :to-be
                      bracket-description-end)
              (expect (node-properties bracket-match)
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
              (expect (node-properties internal-match)
                      :to-equal
                      '(:type "custom-id" :path "local-anchor" :format :bracket))
              (expect (node-contents-begin internal-match)
                      :to-be
                      17)
              (expect (node-contents-end internal-match)
                      :to-be
                      28)
              (expect (node-properties fuzzy-match)
                      :to-equal
                      '(:type "fuzzy" :path "docs/chapter" :format :bracket))
              (expect (node-properties abbrev-match)
                      :to-equal
                      '(:type "https"
                              :path
                              "//example.test/manual"
                              :format
                              :bracket)))))
          (it "recognizes radio targets and links from document-local targets"
              (let* ((source
                      "<<<radio *target* phrase>>> radio *target* phrase.")
                     (target-match (link-test-match source 0 nil 0 nil t))
                       (link-start
                        (search "radio *target* phrase" source :start2 4))
                       (link-match (link-test-match source link-start nil 0 nil t)))
                  (with-soft-assertions
                   (expect (node-type target-match)
                           :to-be
                           :radio-target)
                   (expect (link-test-property target-match :value)
                           :to-equal
                           "radio *target* phrase")
                   (expect (node-contents-begin target-match)
                           :to-be
                           3)
                   (expect (node-contents-end target-match)
                           :to-be
                           24)
                   (expect (node-type link-match) :to-be :link)
                   (expect (node-contents-begin link-match)
                           :to-be
                           link-start)
                   (expect (node-contents-end link-match)
                           :to-be
                           (+ link-start 21))
                   (expect (node-properties link-match)
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
                 (expect (node-type target-match)
                         :to-be
                         :target)
                 (expect (link-test-property target-match :value)
                         :to-equal
                         "local-anchor")
                 (expect (node-type macro-match) :to-be :macro)
                 (expect (node-properties macro-match)
                         :to-equal
                         '(:key "greet"
                                :args
                                ("日本語")
                                :value
                                "{{{greet(日本語)}}}"))
                 (expect (node-end macro-match)
                         :to-be
                         (+ (search "  tail" macro-source) 2))
                 (expect (node-post-blank macro-match)
                         :to-be
                         2)
                 (expect (node-type export-match)
                         :to-be
                         :export-snippet)
                 (expect (node-properties export-match)
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
                 (expect (node-type standard)
                         :to-be
                         :footnote-reference)
                 (expect (node-properties standard)
                         :to-equal
                         '(:label "short" :type :standard))
                 (expect (node-type inline)
                         :to-be
                         :footnote-reference)
                 (expect (node-properties inline)
                         :to-equal
                         '(:label nil :type :inline))
                 (expect (node-contents-begin inline)
                         :to-be
                         inline-contents-start)
                 (expect (node-contents-end inline)
                         :to-be
                         inline-contents-end))))
          (it "recognizes citations and their reference children"
              (let* ((source "[cite/t: see;@smith2024;@doe-2025 p. 4;as noted]")
                     (citation (link-test-match source))
                     (first-key (search "@smith2024" source))
                     (second-key (search "@doe-2025" source))
                     (contents-end (1+ (search ";as noted" source)))
                     (references
                      (remove-if-not
                       (lambda (child)
                         (eq (node-type child) :citation-reference))
                       (node-children citation)))
                     (first-reference (first references))
                     (second-reference (second references)))
                (with-soft-assertions
                 (expect (node-type citation) :to-be :citation)
                 (expect (node-properties citation)
                         :to-equal
                         '(:style "t" :key nil))
                 (expect (node-contents-begin citation)
                         :to-be
                         first-key)
                 (expect (node-contents-end citation)
                         :to-be
                         contents-end)
                 (expect (node-type first-reference)
                         :to-be
                         :citation-reference)
                 (expect (node-end first-reference)
                         :to-be
                         second-key)
                 (expect (node-properties first-reference)
                         :to-equal
                         '(:key "smith2024"
                                :prefix
                                nil
                                :suffix
                                nil
                                :locator
                                nil))
                 (expect (node-type second-reference)
                         :to-be
                         :citation-reference)
                 (expect (node-end second-reference)
                         :to-be
                         contents-end)
                 (expect (node-properties second-reference)
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
                  (radio-start (search "<<<unterminated radio target" source))
                  (target-start (1+ radio-start)))
             (with-soft-assertions
              (expect (link-test-match source radio-start nil 0) :to-be nil)
              (let ((match (link-test-match source target-start nil 0)))
                (expect (node-type match) :to-be :target)
                (expect (link-test-property match :value)
                        :to-equal
                        "unterminated radio target")
                (expect (node-end match)
                        :to-be
                        (+ target-start
                           (length "<<unterminated radio target>>")))))
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
              (expect (node-end within-range)
                      :to-be
                      (+ object-end 2))
              (expect (node-post-blank within-range) :to-be 2)
              (expect (link-test-match "https:path" 0 5) :to-be nil))))
          (it "allows markup when radio links are forbidden"
              (let* ((source "<<<*needle* x>>> [[dest][*needle* x]]")
                     (start (search "*needle*" source :from-end t))
                     (match (link-test-match source start nil start nil t :link)))
                (with-soft-assertions
                 (expect (node-type match) :to-be :bold)
                 (expect (node-end match)
                         :to-be
                         (+ start (length "*needle* "))))))
          (it "prefers the longest radio target candidate"
              (let* ((source "<<<a b>>> <<<a>>> a b")
                     (start (search "a b" source :from-end t))
                     (match (link-test-match source start nil 0 nil t)))
                (with-soft-assertions
                 (expect (node-type match) :to-be :link)
                 (expect (link-test-property match :path) :to-equal "a b")
                 (expect (node-end match)
                         :to-be
                         (+ start (length "a b"))))))
          (it "rejects shorter radio candidates and identifier substrings"
              (let* ((longest-source "<<<a b>>> <<<a>>> <<<a b c>>> a b c.")
                     (longest-start (search "a b c." longest-source))
                     (before-source "<<<target>>>x_target")
                     (before-start (1+ (search "_target" before-source)))
                     (after-source "<<<target>>> targets")
                     (after-start (search "target" after-source :from-end t))
                     (longest-match
                       (link-test-match longest-source longest-start nil 0 nil t))
                     (before-match
                       (link-test-match before-source before-start nil 0 nil t))
                     (after-match
                       (link-test-match after-source after-start nil 0 nil t)))
                  (with-soft-assertions
                   (expect (link-test-property longest-match :path)
                           :to-equal
                           "a b c")
                   (expect (node-end longest-match)
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
                 (expect (node-properties file-match)
                         :to-equal
                         '(:type "file" :path "../chapter.org" :format :bracket))
                 (expect (node-properties coderef-match)
                         :to-equal
                         '(:type "coderef" :path "symbol" :format :bracket))
                 (expect (node-properties unknown-scheme-match)
                         :to-equal
                         '(:type "fuzzy"
                                 :path
                                 "my+scheme:target"
                                 :format
                                 :bracket))
                 (expect (node-properties abbrev-match)
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
              (expect (node-type description-match)
                      :to-be
                      :link)
              (expect (node-contents-begin description-match)
                      :to-be
                      description-start)
              (expect (node-contents-end description-match)
                      :to-be
                      description-end)
              (expect malformed-match :to-be nil))))
          (it "keeps balanced parentheses in plain link paths"
              (let* ((source "https:path(inner))")
                     (match (link-test-match source)))
                (with-soft-assertions
                 (expect (node-properties match)
                         :to-equal
                         '(:type "https" :path "path(inner)" :format :plain))
                 (expect (node-end match)
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
              (expect (node-type macro-match) :to-be :macro)
              (expect (link-test-property macro-match :args)
                      :to-equal
                      '("alpha beta" "gamma"))
              (expect (link-test-property empty-args-match :args) :to-be nil)
              (expect (node-properties export-match)
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
                 (expect (node-type footnote-match)
                         :to-be
                         :footnote-reference)
                 (expect (link-test-property footnote-match :type)
                         :to-be
                         :inline)
                 (expect (node-contents-end footnote-match)
                         :to-be
                         (1- (length footnote-source)))
                 (expect (node-type citation-match)
                         :to-be
                         :citation)
                 (expect (node-properties citation-match)
                         :to-equal
                         '(:style nil :key nil))
                 (expect (node-end citation-match)
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
      (expect (node-properties plain-match)
              :to-equal
              '(:type "https" :path "path" :format :plain))
      (expect (node-end plain-match)
              :to-be
              (length "https:path"))
      (expect angle-lf :to-be nil)
      (expect angle-cr :to-be nil)
      (expect (node-properties abbrev-match)
              :to-equal
              '(:type "https"
                      :path "//docs.example/manual"
                      :format :bracket))
      (expect (node-contents-end citation-match)
              :to-be
              (search "   ]" citation-source))
      (expect (node-type escaped-description)
              :to-be
              :link)
      (expect (node-contents-end escaped-description)
              :to-be
              (search "]]" escaped-description-source))
      (expect (node-end escaped-description)
              :to-be
              (length escaped-description-source))
      (expect (node-type nested-footnote)
              :to-be
              :footnote-reference)
      (expect (node-end nested-footnote)
              :to-be
              (length nested-footnote-source))
      (expect (node-type nested-citation)
              :to-be
              :citation)
      (expect (node-end nested-citation)
              :to-be
              (length nested-citation-source))))))

(describe "link object text boundaries"
  (it "classifies bracket paths and parses link-related Org objects"
    (let* ((absolute (link-test-match "[[/absolute]]"))
           (relative (link-test-match "[[../relative]]"))
           (anchor (link-test-match "[[#anchor]]"))
           (fuzzy (link-test-match "[[relative]]"))
           (macro (link-test-match "{{{name(a)}}}"))
           (export (link-test-match "@@html:value@@"))
           (footnote (link-test-match "[fn:label]"))
           (citation (link-test-match "[cite/author:@ref]")))
      (with-soft-assertions
        (expect (node-type absolute) :to-be :link)
        (expect (link-test-property absolute :type) :to-equal "file")
        (expect (node-type relative) :to-be :link)
        (expect (link-test-property anchor :type) :to-equal "custom-id")
        (expect (link-test-property fuzzy :type) :to-equal "fuzzy")
        (expect (node-type macro) :to-be :macro)
        (expect (node-type export) :to-be :export-snippet)
        (expect (node-type footnote) :to-be :footnote-reference)
        (expect (node-type citation) :to-be :citation)
        (expect (link-test-match "{{{name(a}}}") :to-be nil)
        (expect (link-test-match "@@html:value") :to-be nil)
        (expect (link-test-match "[fn:]") :to-be nil)
        (expect (link-test-match "[cite: ]") :to-be nil))))
  (it "applies link abbreviations through bracket-link parsing"
    (let ((match
            (link-test-match "[[web:manual]]"
                             nil nil nil
                             '(:link-abbrevs
                               (("web" . "https://example.test/%s"))))))
      (expect (link-test-property match :type)
              :to-equal
              "https")
      (expect (link-test-property match :path)
              :to-equal
              "//example.test/manual")))
  (it "leaves bracket links unchanged when abbreviations are not a list"
    (let ((link (find :link
                      (link-test-org-objects "[[web:manual]]"
                                              :link-abbrevs :invalid)
                      :key #'node-type)))
      (expect (node-properties link)
              :to-equal
              '(:type "fuzzy" :path "web:manual" :format :bracket))))
  (it "expands empty abbreviation suffixes and preserves unmatched paths"
    (let* ((objects
             (link-test-org-objects
              "[[web:]] [[other:manual]] [[empty:]]"
              :link-abbrevs
              '(("web" . "https://example.test/%s")
                ("empty" . "%s"))))
           (links (remove-if-not (lambda (object)
                                   (eq (node-type object) :link))
                                 objects)))
      (with-soft-assertions
        (expect (mapcar (lambda (object)
                          (list (node-property object :type)
                                (node-property object :path)))
                        links)
                :to-equal
                '(("https" "//example.test/")
                  ("fuzzy" "other:manual")
                  ("fuzzy" ""))))))
  (it "parses escaped paths, radio precedence, and link variants through parse-org"
    (let* ((radio-objects
             (link-test-org-objects "<<<a b c>>> <<<a b>>> a b c"))
           (radio-link
             (find :link radio-objects :key #'node-type))
           (path-cases
             '(("[[./chapter]]" "file" "./chapter")
               ("[[#]]" "custom-id" "")
               ("[[my_scheme:target]]" "fuzzy" "my_scheme:target")
               ("[[(symbol]]" "fuzzy" "(symbol")
               ("[[(symbol)]]" "coderef" "symbol")
               ("[[foo\\[bar\\]]]" "fuzzy" "foo[bar]")
               ("[[foo\\\\bar]]" "fuzzy" "foo\\bar")
               ("[[foo\\q]]" "fuzzy" "foo\\q")
               ("[[tail\\\\]]" "fuzzy" "tail\\")
               ("[[trail  ]]" "fuzzy" "trail  ")
               ("[[two  words]]" "fuzzy" "two  words")))
           (newline-object
             (find :link
                   (link-test-org-objects
                    (format nil "[[first~%  second]]"))
                   :key
                   #'node-type))
           (trailing-newline-object
             (find :link
                   (link-test-org-objects (format nil "[[first~%]]"))
                   :key
                   #'node-type))
           (shadowed-objects
             (link-test-org-objects "<<<outer <<<inner>>> tail>>>"))
           (identifier-source "<<<target>>> target_name x-target")
           (identifier-boundary-objects
             (link-test-org-objects identifier-source))
           (identifier-links
             (remove-if-not (lambda (object)
                              (eq (node-type object) :link))
                            identifier-boundary-objects))
           (citation-objects
             (link-test-org-objects
              "{{{join(alpha, beta,)}}} [cite: @key note]"))
           (public-objects
             (link-test-org-objects
              "@@html5-custom:value@@ [fn:short] <https:angle-path>"))
           (macro
             (find :macro citation-objects :key #'node-type))
           (citation
             (find :citation citation-objects :key #'node-type))
           (citation-reference
             (find :citation-reference citation-objects :key #'node-type))
           (export-object
             (find :export-snippet public-objects :key #'node-type))
           (footnote
             (find :footnote-reference public-objects :key #'node-type))
           (angle-link
             (find :link public-objects :key #'node-type))
           (invalid-style-objects
             (link-test-org-objects "[cite/: @key]"))
           (abbrev-link
             (find :link
                   (link-test-org-objects
                    "[[web:manual]]"
                    :link-abbrevs
                    '(("web" . "https://docs.example/")))
                   :key
                   #'node-type)))
      (with-soft-assertions
        (expect (node-type radio-link) :to-be :link)
        (expect (link-test-property radio-link :path)
                :to-equal
                "a b c")
        (dolist (case path-cases)
          (destructuring-bind (source type path) case
            (let* ((object (find :link
                                 (link-test-org-objects source)
                                 :key
                                 #'node-type)))
              (expect (link-test-property object :type) :to-equal type)
              (expect (link-test-property object :path) :to-equal path))))
        (expect (link-test-property newline-object :path)
                :to-equal
                "first second")
        (expect (link-test-property trailing-newline-object :path)
                :to-equal
                "first ")
        (expect (find :radio-target shadowed-objects :key #'node-type)
                :to-be
                nil)
        (expect (mapcar #'node-contents-begin identifier-links)
                :to-equal
                (list (search "target" identifier-source :from-end t)))
        (expect (node-property macro :args) :to-equal '("alpha" "beta" ""))
        (expect (node-type citation) :to-be :citation)
        (expect (node-property citation-reference :key) :to-equal "key")
        (expect (node-property citation-reference :suffix)
                :to-equal
                '(" note"))
        (expect (node-properties export-object)
                :to-equal
                '(:back-end "html5-custom" :value "value"))
        (expect (node-properties footnote)
                :to-equal
                '(:label "short" :type :standard))
        (expect (node-properties angle-link)
                :to-equal
                '(:type "https" :path "angle-path" :format :angle))
        (expect (find :citation invalid-style-objects :key #'node-type)
                :to-be
                nil)
        (expect (node-properties abbrev-link)
                :to-equal
                '(:type "https"
                        :path "//docs.example/manual"
                        :format :bracket)))))
  (it "recognizes a plain link at a parsed heading-title boundary"
    (let* ((document (parse-org "* https:heading-link"))
           (headline (first (node-children (document-root document))))
           (link (find-if (lambda (segment)
                            (and (node-p segment)
                                 (eq (node-type segment) :link)))
                          (node-property headline :title))))
      (with-soft-assertions
        (expect (node-type link) :to-be :link)
        (expect (node-properties link)
                :to-equal
                '(:type "https" :path "heading-link" :format :plain)))))
  (it "leaves incomplete and unsupported link spellings as text through parse-org"
    (dolist (source '("<<<>>>" "<<< bad>>>" "<<<bad >>>" "<<>>" "<<unclosed"
                      "[[dest]" "[[dest]x" "[[dest][label]" "[[unclosed"
                      "[fn::]" "[fn:unclosed" "[fn::unclosed" "[cite"
                      "[cite/" "[cite:" "https:"
                      "<" "<https:>" "<unknown:target>"
                      "@@html:value" "@@bad_backend:value@@"))
      (expect (link-test-org-objects source) :to-be nil)))
  (it "chooses the longest overlapping radio target in either registration order"
    (labels ((final-link (source)
               (let* ((start (search "alpha beta" source :from-end t))
                      (objects (link-test-org-objects source)))
                 (find-if (lambda (object)
                            (and (eq (node-type object) :link)
                                 (= (node-begin object) start)))
                          objects))))
      (let ((short-first
              (final-link "<<<alpha>>> <<<alpha beta>>> alpha beta"))
            (long-first
              (final-link "<<<alpha beta>>> <<<alpha>>> alpha beta")))
        (with-soft-assertions
          (expect (node-property short-first :path) :to-equal "alpha beta")
          (expect (node-property long-first :path) :to-equal "alpha beta")))))
  (it "covers nested delimiters and path boundaries through parse-org"
    (let* ((nested-description-source "[[dest][outer [inner] text]]")
           (nested-description-link
             (find :link
                   (link-test-org-objects nested-description-source)
                   :key
                   #'node-type))
           (escaped-description-source "[[dest][outer \\] literal]]")
           (escaped-description-link
             (find :link
                   (link-test-org-objects escaped-description-source)
                   :key
                   #'node-type))
           (nested-plain-link
             (find :link
                   (link-test-org-objects "https:path(a(b)c)")
                   :key
                   #'node-type))
           (closing-paren-link
             (find :link
                   (link-test-org-objects "https:closing)")
                   :key
                   #'node-type))
           (double-colon-link
             (find :link
                   (link-test-org-objects
                    "[[web::manual]]"
                    :link-abbrevs
                    '(("web" . "https://docs.example/%s")))
                   :key
                   #'node-type))
           (footnote-source "intro [fn::outer [inner\\] note] tail]")
           (footnote
             (find :footnote-reference
                   (link-test-org-objects footnote-source)
                   :key
                   #'node-type))
           (citation-source
             "[cite: intro [nested\\] note;@key suffix;@other   ]]")
           (citation-objects (link-test-org-objects citation-source))
           (citation
             (find :citation
                   citation-objects
                   :key
                   #'node-type))
           (citation-references
             (remove-if-not
              (lambda (object)
                (eq (node-type object) :citation-reference))
              citation-objects))
           (simple-citation-source "[cite: @key   ]")
           (simple-citation
             (find :citation
                   (link-test-org-objects simple-citation-source)
                   :key
                   #'node-type))
           (empty-macro
             (find :macro
                   (link-test-org-objects "{{{empty()}}}")
                   :key
                   #'node-type))
           (angle-cr-links
             (remove-if-not
              (lambda (object) (eq (node-type object) :link))
              (link-test-org-objects
               (concatenate 'string
                            "<https:line"
                            (string #\Return)
                            "break>")))))
      (with-soft-assertions
        (expect (link-test-property nested-description-link :path)
                :to-equal
                "dest")
        (expect (link-test-property escaped-description-link :path)
                :to-equal
                "dest")
        (expect (link-test-property nested-plain-link :path)
                :to-equal
                "path(a(b)c)")
        (expect (link-test-property closing-paren-link :path)
                :to-equal
                "closing")
        (expect (node-properties double-colon-link)
                :to-equal
                '(:type "https"
                        :path "//docs.example/manual"
                        :format :bracket))
        (expect (node-properties footnote)
                :to-equal
                '(:label nil :type :inline))
        (expect (node-type citation) :to-be :citation)
        (expect (mapcar (lambda (object) (node-property object :key))
                        citation-references)
                :to-equal
                '("key" "other"))
        (expect (node-type simple-citation) :to-be :citation)
        (expect (node-contents-end simple-citation)
                :to-be
                (search "   ]" simple-citation-source))
        (expect (node-properties empty-macro)
                :to-equal
                '(:key "empty" :args nil :value "{{{empty()}}}"))
        (expect (length angle-cr-links) :to-be 1)
        (expect (node-properties (first angle-cr-links))
                :to-equal
                '(:type "https" :path "line" :format :plain))))))

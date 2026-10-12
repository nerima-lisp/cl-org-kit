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
              (expect (link-test-match "https:path" 0 5) :to-be nil)))))

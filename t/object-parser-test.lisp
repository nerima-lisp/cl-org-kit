;;;; t/object-parser-test.lisp
(in-package #:cl-org-kit/test)

(describe "object parsing interface"
          (it "returns no objects for an empty range"
              (let ((document (make-document "plain")))
                (with-soft-assertions
                 (expect (org-kit::prepare-object-parsing document) :to-be nil)
                 (expect (org-kit::parse-objects document 2 2 :paragraph)
                         :to-equal
                         nil))))
          (it "covers a non-empty range with one plain-text node"
              (let* ((document (make-document "plain text"))
                     (objects (org-kit::parse-objects document 0 10 :paragraph))
                     (object (first objects)))
                (with-soft-assertions (expect (length objects) :to-be 1)
                                      (expect (node-type object)
                                              :to-be
                                              :plain-text)
                                      (expect
                                       (list (node-begin object)
                                             (node-end object))
                                       :to-equal
                                       '(0 10))
                                      (expect (node-parent object) :to-be nil))))
          (it "parses markup into nested object and plain-text nodes"
              (let* ((source "*outer /inner/ tail*")
                     (document (make-document source))
                     (objects
                      (org-kit::parse-objects document
                                              0
                                              (length source)
                                              :paragraph))
                     (bold (first objects))
                     (children (node-children bold)))
                (with-soft-assertions
                 (expect (mapcar #'node-type objects) :to-equal '(:bold))
                 (expect (mapcar #'node-type children)
                         :to-equal
                         '(:plain-text :italic :plain-text))
                 (expect (node-parent (second children)) :to-be bold)
                 (expect
                  (subseq (document-text document)
                          (node-begin bold)
                          (node-end bold))
                  :to-equal
                  source)
                 (expect
                  (subseq (document-text document)
                          (node-begin (second children))
                          (node-end (second children)))
                  :to-equal
                  "/inner/ "))))
          (it "keeps recognition within the requested range"
              (let* ((document (make-document "x *bold* y"))
                     (objects (org-kit::parse-objects document 2 8 :paragraph))
                     (bold (first objects)))
                (with-soft-assertions (expect (length objects) :to-be 1)
                                      (expect (node-type bold) :to-be :bold)
                                      (expect
                                       (list (node-begin bold) (node-end bold))
                                       :to-equal
                                       '(2 8))
                                      (expect
                                       (subseq (document-text document)
                                               (node-begin bold)
                                               (node-end bold))
                                       :to-equal
                                       "*bold*"))))
          (it "leaves disallowed object syntax as plain text"
              (let* ((document (make-document "*bold*"))
                     (objects (org-kit::parse-objects document 0 6 :citation)))
                (with-soft-assertions
                 (expect (mapcar #'node-type objects) :to-equal '(:plain-text))
                 (expect
                  (subseq (document-text document)
                          (node-begin (first objects))
                          (node-end (first objects)))
                  :to-equal
                  "*bold*"))))
          (it "enforces syntax-object depth and document node-count limits"
              (let* ((depth-neutral-document
                      (make-document "*x*"
                                     :limits
                                     (make-limits :max-object-depth 1)))
                     (depth-neutral-objects
                      (org-kit::parse-objects depth-neutral-document
                                              0
                                              3
                                              :paragraph))
                     (nested-depth-document
                      (make-document "*a /b/*"
                                     :limits
                                     (make-limits :max-object-depth 1)))
                     (count-document
                      (make-document "*x*"
                                     :limits
                                     (make-limits :max-node-count 1))))
                (with-soft-assertions
                 (expect (length depth-neutral-objects) :to-be 1)
                 (expect (node-type (first depth-neutral-objects)) :to-be :bold)
                 (signals org-resource-limit-error
                          (org-kit::parse-objects nested-depth-document
                                                  0
                                                  7
                                                  :paragraph))
                 (signals org-resource-limit-error
                          (org-kit::parse-objects count-document 0 3 :paragraph)))))
          (it "collects syntax-recognized radio targets in document-local state"
              (let* ((document (make-document "<<<target>>>"))
                     (opaque-document
                      (make-document
                       "<<<target>>> =<<<code>>>= ~<<<verbatim>>>~"))
                     (tree-text
                      "* H <<<inside>>>
#+begin_src text
<<<hidden>>>
#+end_src
")
                     (tree-document (make-document tree-text))
                     (title-end (position #\Newline tree-text))
                     (target-start (search "<<<inside>>>" tree-text))
                     (block-start (search "#+begin_src" tree-text))
                     (hidden-start (search "<<<hidden>>>" tree-text))
                     (source-block
                      (make-node :src-block
                                 :begin
                                 block-start
                                 :end
                                 (length tree-text)
                                 :contents-begin
                                 hidden-start
                                 :contents-end
                                 (+ hidden-start 12)))
                     (headline
                      (make-node :headline
                                 :begin
                                 0
                                 :end
                                 (length tree-text)
                                 :properties
                                 (list :reader-title-range (cons 2 title-end))
                                 :children
                                 (list source-block)))
                     (root
                      (make-node :org-data
                                 :begin
                                 0
                                 :end
                                 (length tree-text)
                                 :children
                                 (list headline))))
                (setf (document-root tree-document) root)
                (with-soft-assertions
                 (expect (org-kit::prepare-object-parsing document) :to-be nil)
                 (expect
                  (org-kit::%object-context-radio-targets
                   (org-kit::%document-object-context document))
                  :to-equal
                  (list "target"))
                 (expect (org-kit::prepare-object-parsing opaque-document)
                         :to-be
                         nil)
                 (expect
                  (org-kit::%object-context-radio-targets
                   (org-kit::%document-object-context opaque-document))
                  :to-equal
                  (list "target"))
                 (expect (org-kit::prepare-object-parsing tree-document)
                         :to-be
                         nil)
                 (expect
                  (org-kit::%object-context-radio-targets
                   (org-kit::%document-object-context tree-document))
                  :to-equal
                  (list "inside"))
                 (expect
                  (node-type
                   (first
                    (org-kit::parse-objects tree-document
                                            target-start
                                            (+ target-start 12)
                                            :headline)))
                  :to-be
                  :radio-target))))
          (it "rejects invalid containers and malformed ranges"
              (let ((document (make-document "text")))
                (with-soft-assertions
                 (signals type-error
                          (org-kit::parse-objects document 0 4 :unknown))
                 (signals org-kit-error
                          (org-kit::parse-objects document 3 2 :paragraph))
                 (signals type-error
                          (org-kit::parse-objects document -1 2 :paragraph)))))
          (it
           "matches fixture-derived spans and properties for all object types"
           (labels ((fixture-node (begin source
                                         type
                                         container
                                         &optional
                                         preceding-character
                                         include-preceding-p)
                      (let* ((context-length
                              (if include-preceding-p
                                  1
                                  0))
                             (padding
                              (- begin
                                 (if preceding-character
                                     1
                                     0)))
                             (text
                              (concatenate 'string
                                           (make-string padding
                                                        :initial-element
                                                        #\Space)
                                           (if preceding-character
                                               (string preceding-character)
                                               "")
                                           source))
                             (document (make-document text)))
                        (org-kit::prepare-object-parsing document)
                        (let ((nodes
                               (org-kit::parse-objects document
                                                       (- begin context-length)
                                                       (+ begin (length source))
                                                       container)))
                          (find type nodes :key #'node-type))))
                    (check-fixture (begin source
                                          type
                                          end
                                          contents-begin
                                          contents-end
                                          properties
                                          container
                                          &optional
                                          preceding-character
                                          include-preceding-p)
                      (let ((node
                             (fixture-node begin
                                           source
                                           type
                                           container
                                           preceding-character
                                           include-preceding-p)))
                        (expect (node-type node) :to-be type)
                        (expect
                         (list (node-begin node)
                               (node-end node)
                               (node-contents-begin node)
                               (node-contents-end node))
                         :to-equal
                         (list begin end contents-begin contents-end))
                        (expect (node-properties node) :to-equal properties)
                        node)))
             (let ((seen-types '()))
               (with-soft-assertions
                (flet ((check (begin source
                                     type
                                     end
                                     contents-begin
                                     contents-end
                                     properties
                                     container
                                     &optional
                                     preceding-character
                                     include-preceding-p)
                         (let ((node
                                (check-fixture begin
                                               source
                                               type
                                               end
                                               contents-begin
                                               contents-end
                                               properties
                                               container
                                               preceding-character
                                               include-preceding-p)))
                           (push type seen-types)
                           node)))
                  (check 23
                         (format nil
                                 "Plain text remains a plain-text object unless a more specific form matches.~%")
                         :plain-text
                         99
                         nil
                         nil
                         nil
                         :paragraph)
                  (check 114
                         "\\alpha"
                         :entity
                         120
                         nil
                         nil
                         '(:name "alpha"
                                 :latex
                                 "\\alpha"
                                 :latex-math-p
                                 t
                                 :html
                                 "&alpha;"
                                 :ascii
                                 "alpha"
                                 :utf-8
                                 "α"
                                 :use-brackets-p
                                 nil)
                         :paragraph)
                  (check 254
                         "\\(a + b\\)"
                         :latex-fragment
                         263
                         nil
                         nil
                         '(:value "\\(a + b\\)")
                         :paragraph)
                  (check 296
                         "_{i}"
                         :subscript
                         300
                         298
                         299
                         '(:use-brackets-p t)
                         :paragraph
                         #\a
                         t)
                  (check 365 "*bold*" :bold 371 366 370 nil :paragraph)
                  (check 373 "/italic/" :italic 381 374 380 nil :paragraph)
                  (check 383
                         "_underline_"
                         :underline
                         394
                         384
                         393
                         nil
                         :paragraph)
                  (check 396
                         "+strike-through+"
                         :strike-through
                         412
                         397
                         411
                         nil
                         :paragraph)
                  (check 414
                         "=verbatim text="
                         :verbatim
                         429
                         nil
                         nil
                         nil
                         :paragraph)
                  (check 435 "~code text~" :code 446 nil nil nil :paragraph)
                  (check 559 "~*not bold*~ " :code 572 nil nil nil :paragraph)
                  (check 576
                         "=/not italic/="
                         :verbatim
                         590
                         nil
                         nil
                         nil
                         :paragraph)
                  (check 620
                         "_2O"
                         :subscript
                         623
                         621
                         623
                         '(:use-brackets-p nil)
                         :paragraph
                         #\H
                         t)
                  (check 626
                         "^3"
                         :superscript
                         628
                         627
                         628
                         '(:use-brackets-p nil)
                         :paragraph
                         #\x
                         t)
                  (check 644
                         "^{y^{z}}"
                         :superscript
                         652
                         646
                         651
                         '(:use-brackets-p t)
                         :paragraph
                         #\x
                         t)
                  (check 61
                         "\\notAnEntity"
                         :latex-fragment
                         73
                         nil
                         nil
                         '(:value "\\notAnEntity")
                         :paragraph)
                  (check 72
                         "call_sum(1, 2) "
                         :inline-babel-call
                         87
                         nil
                         nil
                         '(:call "sum"
                                 :inside-header
                                 nil
                                 :arguments
                                 "1, 2"
                                 :end-header
                                 nil)
                         :paragraph)
                  (check 91
                         "call_greet[lang=ja](\"こんにちは\")"
                         :inline-babel-call
                         119
                         nil
                         nil
                         '(:call "greet"
                                 :inside-header
                                 "lang=ja"
                                 :arguments
                                 "\"こんにちは\""
                                 :end-header
                                 nil)
                         :paragraph)
                  (check 136
                         "src_emacs-lisp[:results raw]{(+ 1 2)}"
                         :inline-src-block
                         173
                         nil
                         nil
                         '(:language "emacs-lisp"
                                     :value
                                     "(+ 1 2)"
                                     :parameters
                                     ":results raw")
                         :paragraph)
                  (check 248
                         (format nil "\\\\~%")
                         :line-break
                         251
                         nil
                         nil
                         nil
                         :paragraph)
                  (check 361
                         "[2/5] "
                         :statistics-cookie
                         367
                         nil
                         nil
                         '(:value "[2/5]" :type nil)
                         :paragraph)
                  (check 169
                         "https:plain-path"
                         :link
                         185
                         nil
                         nil
                         '(:type "https" :path "plain-path" :format :plain)
                         :paragraph)
                  (check 199
                         "<https:angle-path>"
                         :link
                         217
                         nil
                         nil
                         '(:type "https" :path "angle-path" :format :angle)
                         :paragraph)
                  (let* ((link-text
                          "[[https:bracket-path][a *bold* description]]")
                         (link
                          (check 257
                                 link-text
                                 :link
                                 301
                                 279
                                 299
                                 '(:type "https"
                                         :path
                                         "bracket-path"
                                         :format
                                         :bracket)
                                 :paragraph)))
                    (expect (mapcar #'node-type (node-children link))
                            :to-equal
                            '(:plain-text :bold :plain-text))
                    (expect
                     (list (node-begin (second (node-children link)))
                           (node-end (second (node-children link))))
                     :to-equal
                     '(281 288)))
                  (check 326
                         "[[#local-anchor][anchor link]]"
                         :link
                         356
                         343
                         354
                         '(:type "custom-id"
                                 :path
                                 "local-anchor"
                                 :format
                                 :bracket)
                         :paragraph)
                  (check 377
                         "<<local-anchor>>"
                         :target
                         393
                         nil
                         nil
                         '(:value "local-anchor")
                         :paragraph)
                  (check 409
                         "{{{title}}} "
                         :macro
                         421
                         nil
                         nil
                         '(:key "title" :args nil :value "{{{title}}}")
                         :paragraph)
                  (check 425
                         "{{{greet(日本語)}}}"
                         :macro
                         441
                         nil
                         nil
                         '(:key "greet"
                                :args
                                ("日本語")
                                :value
                                "{{{greet(日本語)}}}")
                         :paragraph)
                  (check 459
                         "@@html:<mark>inline export</mark>@@"
                         :export-snippet
                         494
                         nil
                         nil
                         '(:back-end "html" :value "<mark>inline export</mark>")
                         :paragraph)
                  (check 567
                         "[fn:short] "
                         :footnote-reference
                         578
                         nil
                         nil
                         '(:label "short" :type :standard)
                         :paragraph)
                  (check 582
                         "[fn::inline note]"
                         :footnote-reference
                         599
                         587
                         598
                         '(:label nil :type :inline)
                         :paragraph)
                  (let* ((citation-text
                          "[cite/t: see;@smith2024;@doe-2025 p. 4;as noted]")
                         (citation
                          (check 631
                                 citation-text
                                 :citation
                                 679
                                 644
                                 670
                                 '(:style "t" :key nil)
                                 :paragraph))
                         (references (node-children citation)))
                    (push :citation-reference seen-types)
                    (expect (mapcar #'node-type references)
                            :to-equal
                            '(:citation-reference :citation-reference))
                    (expect
                     (mapcar
                      (lambda (reference)
                        (list (node-begin reference)
                              (node-end reference)
                              (node-properties reference)))
                      references)
                     :to-equal
                     '((644 655
                            (:key "smith2024"
                                  :prefix
                                  nil
                                  :suffix
                                  nil
                                  :locator
                                  nil))
                       (655 670
                            (:key "doe-2025"
                                  :prefix
                                  nil
                                  :suffix
                                  (" p. 4")
                                  :locator
                                  nil))))
                    (check 75
                           "<<<radio *target* phrase>>>"
                           :radio-target
                           102
                           78
                           99
                           '(:value "radio *target* phrase")
                           :paragraph)
                    (let* ((target "<<<radio *target* phrase>>>")
                           (occurrence "radio *target* phrase")
                           (document-text
                            (concatenate 'string
                                         (make-string 75
                                                      :initial-element
                                                      #\Space)
                                         target
                                         (make-string (- 133 75 (length target))
                                                      :initial-element
                                                      #\Space)
                                         occurrence))
                           (document (make-document document-text)))
                      (org-kit::prepare-object-parsing document)
                      (let* ((nodes
                              (org-kit::parse-objects document
                                                      133
                                                      154
                                                      :paragraph))
                             (link (first nodes))
                             (bold (second (node-children link))))
                        (push :link seen-types)
                        (expect (length nodes) :to-be 1)
                        (expect (node-type link) :to-be :link)
                        (expect
                         (list (node-begin link)
                               (node-end link)
                               (node-contents-begin link)
                               (node-contents-end link))
                         :to-equal
                         '(133 154 133 154))
                        (expect (node-properties link)
                                :to-equal
                                '(:type "radio"
                                        :path
                                        "radio *target* phrase"
                                        :format
                                        :plain))
                        (expect (mapcar #'node-type (node-children link))
                                :to-equal
                                '(:plain-text :bold :plain-text))
                        (expect (list (node-begin bold) (node-end bold))
                                :to-equal
                                '(139 148))))
                    (check 670
                           " Cell with *bold* |"
                           :table-cell
                           689
                           671
                           687
                           '(:value "Cell with *bold*")
                           :table-row
                           #\|)
                    (check 422
                           "<2026-10-12 Mon 09:30 +1w -2d> "
                           :timestamp
                           453
                           nil
                           nil
                           '(:type :active
                                   :year-start
                                   2026
                                   :month-start
                                   10
                                   :day-start
                                   12
                                   :hour-start
                                   9
                                   :minute-start
                                   30
                                   :year-end
                                   2026
                                   :month-end
                                   10
                                   :day-end
                                   12
                                   :hour-end
                                   9
                                   :minute-end
                                   30
                                   :repeater-type
                                   :cumulate
                                   :repeater-value
                                   1
                                   :repeater-unit
                                   :week
                                   :warning-type
                                   :all
                                   :warning-value
                                   2
                                   :warning-unit
                                   :day)
                           :paragraph)
                    (check 466
                           "[2026-10-11 Sun]"
                           :timestamp
                           482
                           nil
                           nil
                           '(:type :inactive
                                   :year-start
                                   2026
                                   :month-start
                                   10
                                   :day-start
                                   11
                                   :hour-start
                                   nil
                                   :minute-start
                                   nil
                                   :year-end
                                   2026
                                   :month-end
                                   10
                                   :day-end
                                   11
                                   :hour-end
                                   nil
                                   :minute-end
                                   nil
                                   :repeater-type
                                   nil
                                   :repeater-value
                                   nil
                                   :repeater-unit
                                   nil
                                   :warning-type
                                   nil
                                   :warning-value
                                   nil
                                   :warning-unit
                                   nil)
                           :paragraph)
                    (check 497
                           "<2026-10-12 Mon>--<2026-10-14 Wed>"
                           :timestamp
                           531
                           nil
                           nil
                           '(:type :active-range
                                   :year-start
                                   2026
                                   :month-start
                                   10
                                   :day-start
                                   12
                                   :hour-start
                                   nil
                                   :minute-start
                                   nil
                                   :year-end
                                   2026
                                   :month-end
                                   10
                                   :day-end
                                   14
                                   :hour-end
                                   nil
                                   :minute-end
                                   nil
                                   :repeater-type
                                   nil
                                   :repeater-value
                                   nil
                                   :repeater-unit
                                   nil
                                   :warning-type
                                   nil
                                   :warning-value
                                   nil
                                   :warning-unit
                                   nil)
                           :paragraph)
                    (check 544
                           "<2026-10-12 Mon 09:00-10:30>"
                           :timestamp
                           572
                           nil
                           nil
                           '(:type :active-range
                                   :year-start
                                   2026
                                   :month-start
                                   10
                                   :day-start
                                   12
                                   :hour-start
                                   9
                                   :minute-start
                                   0
                                   :year-end
                                   2026
                                   :month-end
                                   10
                                   :day-end
                                   12
                                   :hour-end
                                   10
                                   :minute-end
                                   30
                                   :repeater-type
                                   nil
                                   :repeater-value
                                   nil
                                   :repeater-unit
                                   nil
                                   :warning-type
                                   nil
                                   :warning-value
                                   nil
                                   :warning-unit
                                   nil)
                           :paragraph)
                    (check 590
                           "<%%(diary-float t 4 2) 12:00-14:00>"
                           :timestamp
                           625
                           nil
                           nil
                           '(:type :diary
                                   :year-start
                                   nil
                                   :month-start
                                   nil
                                   :day-start
                                   nil
                                   :hour-start
                                   12
                                   :minute-start
                                   0
                                   :year-end
                                   nil
                                   :month-end
                                   nil
                                   :day-end
                                   nil
                                   :hour-end
                                   14
                                   :minute-end
                                   0
                                   :repeater-type
                                   nil
                                   :repeater-value
                                   nil
                                   :repeater-unit
                                   nil
                                   :warning-type
                                   nil
                                   :warning-value
                                   nil
                                   :warning-unit
                                   nil)
                           :paragraph)
                    (check 646
                           "<2026-13-99 NotADay>"
                           :timestamp
                           666
                           nil
                           nil
                           '(:type :active
                                   :year-start
                                   2026
                                   :month-start
                                   13
                                   :day-start
                                   99
                                   :hour-start
                                   nil
                                   :minute-start
                                   nil
                                   :year-end
                                   2026
                                   :month-end
                                   13
                                   :day-end
                                   99
                                   :hour-end
                                   nil
                                   :minute-end
                                   nil
                                   :repeater-type
                                   nil
                                   :repeater-value
                                   nil
                                   :repeater-unit
                                   nil
                                   :warning-type
                                   nil
                                   :warning-value
                                   nil
                                   :warning-unit
                                   nil)
                           :paragraph)
                    (check 814
                           "<2026-10-12 Mon>"
                           :timestamp
                           830
                           nil
                           nil
                           '(:type :active
                                   :year-start
                                   2026
                                   :month-start
                                   10
                                   :day-start
                                   12
                                   :hour-start
                                   nil
                                   :minute-start
                                   nil
                                   :year-end
                                   2026
                                   :month-end
                                   10
                                   :day-end
                                   12
                                   :hour-end
                                   nil
                                   :minute-end
                                   nil
                                   :repeater-type
                                   nil
                                   :repeater-value
                                   nil
                                   :repeater-unit
                                   nil
                                   :warning-type
                                   nil
                                   :warning-value
                                   nil
                                   :warning-unit
                                   nil)
                           :table-cell)
                    (check 371
                           "[40%]"
                           :statistics-cookie
                           376
                           nil
                           nil
                           '(:value "[40%]" :type nil)
                           :paragraph)
                    (expect
                     (sort (remove-duplicates seen-types)
                           #'string<
                           :key
                           #'symbol-name)
                     :to-equal
                     '(:bold :citation
                             :citation-reference
                             :code
                             :entity
                             :export-snippet
                             :footnote-reference
                             :inline-babel-call
                             :inline-src-block
                             :italic
                             :latex-fragment
                             :line-break
                             :link
                             :macro
                             :plain-text
                             :radio-target
                             :statistics-cookie
                             :strike-through
                             :subscript
                             :superscript
                             :table-cell
                             :target
                             :timestamp
                             :underline
                             :verbatim)))))))))

(describe "object parser state edge branches"
  (it "scans valid and malformed ranges from every structural node type"
    (let* ((source
             "plain <<<para>>> <<<cell>>> <<<verse>>> <<<title>>> <<<task>>> <<<tag>>>\n#+KEY: <<<keyword>>>")
           (document (make-document source))
           (paragraph-start (search "<<<para>>>" source))
           (cell-start (search "<<<cell>>>" source))
           (verse-start (search "<<<verse>>>" source))
           (title-start (search "<<<title>>>" source))
           (task-start (search "<<<task>>>" source))
           (tag-start (search "<<<tag>>>" source))
           (keyword-start (search "#+KEY:" source))
           (keyword-value "<<<keyword>>>")
           (keyword-value-start (search keyword-value source))
           (paragraph-end (+ paragraph-start (length "<<<para>>>")))
           (cell-end (+ cell-start (length "<<<cell>>>")))
           (verse-end (+ verse-start (length "<<<verse>>>")))
           (title-end (+ title-start (length "<<<title>>>")))
           (task-end (+ task-start (length "<<<task>>>")))
           (tag-end (+ tag-start (length "<<<tag>>>")))
           (nodes
             (list
              (make-node :radio-target :properties '(:value "manual"))
              (make-node :radio-target :properties '(:value ""))
              (make-node :radio-target :properties '(:value 42))
              (make-node :paragraph :begin paragraph-start :end paragraph-end
                         :contents-begin paragraph-start :contents-end paragraph-end)
              (make-node :paragraph :begin 0 :end 1 :contents-begin 0)
              (make-node :table-cell :begin cell-start :end cell-end
                         :contents-begin cell-start :contents-end cell-end)
              (make-node :verse-block :begin verse-start :end verse-end
                         :contents-begin verse-start :contents-end verse-end)
              (make-node :headline :properties
                         (list :reader-title-range (cons title-start title-end)))
              (make-node :headline)
              (make-node :inlinetask :properties
                         (list :reader-title-range (cons "bad" title-end)))
              (make-node :headline :properties
                         '(:reader-title-range (0 . "bad")))
              (make-node :headline :properties
                         '(:reader-title-range (-1 . 1)))
              (make-node :headline :properties
                         (list :reader-title-range
                               (cons 0 (1+ (length source)))))
              (make-node :inlinetask :properties
                         (list :reader-title-range (cons task-start task-end)))
              (make-node :item :properties
                         (list :reader-tag-range (cons tag-start tag-end)))
              (make-node :item :properties '(:reader-tag-range invalid))
              (make-node :keyword :begin 0 :end 5 :properties '(:value "plain"))
              (make-node :keyword :begin keyword-start :end (length source)
                         :properties '(:value 42))
              (make-node :keyword :begin keyword-start :end (length source)
                         :properties '(:value "not-present"))
              (make-node :keyword :begin keyword-start :end (length source)
                         :properties (list :value keyword-value))))
           (root (make-node :org-data :begin 0 :end (length source)
                            :children nodes)))
      (setf (document-root document) root)
      (with-soft-assertions
        (expect (org-kit::prepare-object-parsing document) :to-be nil)
        (let ((targets
                (org-kit::%object-context-radio-targets
                 (org-kit::%document-object-context document))))
          (expect (member "manual" targets :test #'string=) :to-be-truthy)
          (expect (member "para" targets :test #'string=) :to-be-truthy)
          (expect (member "keyword" targets :test #'string=) :to-be-truthy))))))

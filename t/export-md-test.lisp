;;;; t/export-md-test.lisp
(in-package #:cl-org-kit/test)

(defun export-md-test-fixture-path (relative)
  (asdf:system-relative-pathname
   "cl-org-kit" (concatenate 'string "t/fixtures/export/" relative)))

(defun export-md-test-read-text (relative)
  (uiop:read-file-string (export-md-test-fixture-path relative)))

(defun export-md-test-read-octets (relative)
  (with-open-file (stream (export-md-test-fixture-path relative)
                          :direction :input :element-type '(unsigned-byte 8))
    (let* ((length (file-length stream))
           (octets (make-array length :element-type '(unsigned-byte 8))))
      (unless (= (read-sequence octets stream) length)
        (error "Unable to read all Markdown fixture bytes from ~A" relative))
      octets)))

(progn
  (progn
  (defun export-md-test-catch-resource-limit (function)
    (handler-case (progn (funcall function) nil)
      (org-resource-limit-error (condition) condition)))
  (defun export-md-test-render-context (document)
    (multiple-value-bind (descriptor ancestry)
        (org-kit::export-registry-snapshot :md)
      (let ((render-context
              (org-kit::%make-export-render-context
               :backend :md
               :document document
               :metadata nil
               :options nil
               :limits (document-limits document)
               :root-views nil
               :toc nil
               :footnotes nil
               :preparation
               (org-kit::%make-export-preparation
                :allow-raw-html nil
                :allow-raw-latex nil)
               :raw-output-options
               (org-kit::%export-backend-descriptor-raw-output-options
                descriptor)
               :extensions nil
               :ancestry ancestry
               :owner-backend :md
               :state nil
               :states nil)))
        (let ((org-kit::*export-requested-backend* :md))
          (org-kit::export-prepare
           document :gfm
           :allow-raw-html nil
           :allow-raw-latex nil
           :context (org-kit:make-export-context)
           :filter-context render-context))
        render-context))))
  (describe "Markdown export edge behavior"
    (it "renders headline objects, options, and deep-level fallback like ox-md"
      (let* ((source
               (format nil
                       "#+OPTIONS: toc:nil num:2 pri:t tags:nil todo:t~%* TODO [#A] Parent *bold* /italic/ :visible:~%******* DONE [#B] Deep _title_~%Body line.~%"))
             (output (export-org (parse-org source) :md)))
        (with-soft-assertions
          (expect (search "# TODO [#A] Parent **bold** *italic*" output)
                  :to-be-truthy)
          (expect (search "-   DONE [#B] Deep <span class=\"underline\">title</span>"
                          output)
                  :to-be-truthy)
          (expect (search "    Body line." output) :to-be-truthy)
          (expect (search "visible" output) :to-be nil))))
    (it "preserves code-span link labels and leaves unrelated destinations unchanged"
        (let* ((label (concatenate (quote string) (string (code-char 96))
                                   "label ](<x>)" (string (code-char 96))))
               (destination "<guide.md>")
               (rendered (format nil "[~A](~A)" label destination))
               (expected (format nil "[~A](guide.md)" label))
               (mismatched (format nil "[label](~A)" "<other.md>"))
               (unfinished (format nil "[label](~A" destination))
               (short-destination "<>")
               (short-text "[label](<>)")
               (plain-destination "guide.md")
               (plain-text "[label](guide.md)"))
          (with-soft-assertions
           (expect (org-kit::export-md-strip-link-destination-angles rendered destination)
                   :to-equal expected)
           (expect (org-kit::export-md-strip-link-destination-angles mismatched destination)
                   :to-equal mismatched)
           (expect (org-kit::export-md-strip-link-destination-angles unfinished destination)
                   :to-equal unfinished)
           (expect (org-kit::export-md-strip-link-destination-angles short-text short-destination)
                   :to-equal short-text)
           (expect (org-kit::export-md-strip-link-destination-angles plain-text plain-destination)
                   :to-equal plain-text))))
    (it "renders heading TODO, priority, and tags according to options"
      (let* ((source
               (format nil
                       "#+OPTIONS: toc:nil num:nil pri:nil tags:t todo:nil~%* TODO [#A] Tagged :visible:~%"))
             (output (export-org (parse-org source) :md)))
        (with-soft-assertions
          (expect (search "# Tagged     :visible:" output) :to-be-truthy)
          (expect (search "TODO" output) :to-be nil)
          (expect (search "[#A]" output) :to-be nil))))
    (it "merges a special block class with ATTR_HTML classes"
      (let* ((source
               (format nil
                       "#+ATTR_HTML: :id stable-special :class custom~%#+begin_note~%Special class content.~%#+end_note~%"))
             (output (export-org (parse-org source) :md)))
        (expect (search "<div id=\"stable-special\" class=\"custom note\">"
                        output)
                :to-be-truthy)))
    (it "renders descriptive terms and respects num:nil for ordered lists"
      (let* ((source
               (format nil "- Term :: Definition body.~%"))
             (output (export-org (parse-org source) :md))
             (num-nil-source
               (format nil
                       "#+OPTIONS: num:nil~%1. First item~%2. Second item~%"))
             (num-nil-output
               (export-org (parse-org num-nil-source) :md))
             (numbered-source
               (format nil
                       "1. First numbered item~%2. Second numbered item~%"))
             (numbered-output
               (export-org (parse-org numbered-source) :md)))
        (with-soft-assertions
          (expect (search "-   **Term:** Definition body." output)
                  :to-be-truthy)
          (expect (search
                   (format nil
                           "-   First item~%-   Second item")
                   num-nil-output)
                  :to-be-truthy)
          (expect (search
                   (format nil
                           "1.   First numbered item~%2.   Second numbered item")
                   numbered-output)
                  :to-be-truthy))))
    (it "escapes a paragraph-leading literal hash once"
      (let* ((source (format nil "~C#literal paragraph~%" #\\))
             (output (export-org (parse-org source) :md))
             (once (format nil "~C~C#literal paragraph" #\\ #\\))
             (thrice (format nil "~C~C~C#literal paragraph"
                             #\\ #\\ #\\)))
        (with-soft-assertions
          (expect (search once output) :to-be-truthy)
          (expect (search thrice output) :to-be nil))))
    (it "honors the raw HTML policy while matching ox-md's explicit raw default"
      (let* ((source
               (format nil
                       "#+begin_export html~%<aside>policy marker</aside>~%#+end_export~%@@html:<u>snippet marker</u>@@~%"))
             (document (parse-org source))
             (denied (export-org document :md))
             (allowed (export-org document :md :allow-raw-html t)))
        (with-soft-assertions
          (expect (search "<aside>policy marker</aside>" denied) :to-be nil)
          (expect (search "&lt;aside&gt;policy marker&lt;/aside&gt;" denied)
                  :to-be-truthy)
          (expect (search "<aside>policy marker</aside>" allowed)
                  :to-be-truthy)
          (expect (search "<u>snippet marker</u>" allowed)
                  :to-be-truthy))))
    (it "validates inline :exports while preserving default, code, and results behavior"
      (let* ((default-source
               (format nil "src_emacs-lisp{(+ 1 2)}~%"))
             (results-source
               (format nil "src_emacs-lisp[:results raw]{(+ 1 2)}~%"))
             (code-source
               (format nil "src_emacs-lisp[:results raw :exports code]{(+ 1 2)}~%"))
             (results-then-code-source
               (format nil
                       "src_emacs-lisp[:exports results :exports code]{(+ 1 2)}~%"))
             (code-then-results-source
               (format nil
                       "src_emacs-lisp[:exports code :exports results]{(+ 1 2)}~%"))
             (invalid-source
               (format nil "src_emacs-lisp[:exports foo]{(+ 1 2)}~%"))
             (default-output
               (export-org (parse-org default-source) :md))
             (results-output
               (export-org (parse-org results-source) :md))
             (code-output
               (export-org (parse-org code-source) :md))
             (results-then-code-output
               (export-org (parse-org results-then-code-source) :md))
             (code-then-results-output
               (export-org (parse-org code-then-results-source) :md))
             (invalid-error
               (handler-case
                   (progn (export-org (parse-org invalid-source) :md) nil)
                 (org-export-unsupported-syntax (condition)
                   (list (org-export-unsupported-syntax-backend condition)
                         (org-export-unsupported-syntax-feature condition))))))
        (with-soft-assertions
          (expect (search "`(+ 1 2)`" default-output) :to-be-truthy)
          (expect (search "(+ 1 2)" results-output) :to-be nil)
          (expect (search "`(+ 1 2)`" code-output) :to-be-truthy)
          (expect (search "`(+ 1 2)`" results-then-code-output)
                  :to-be-truthy)
          (expect (search "`(+ 1 2)`" code-then-results-output) :to-be nil)
          (expect (and (consp invalid-error)
                       (eq (first invalid-error) :md)
                       (eq (second invalid-error) :src-header-argument))
                  :to-be-truthy))))))

(describe "Markdown export"
  (it "matches all eight pinned Org 9.8.10 Markdown oracle cases byte for byte"
    (let* ((rows
             (remove-if-not
              (lambda (row)
                (member (first row)
                        (list "metadata-selection" "exclude-tags-override"
                              "footnotes-macros-links" "include"
                              "source-blocks" "md-elements"
                              "org-elements" "backend-extended-elements")
                        :test (function string=)))
              (mapcar
               (lambda (line)
                 (loop with start = 0
                       for tab = (position #\Tab line :start start)
                       collect (subseq line start tab)
                       while tab
                         do (setf start (1+ tab))))
               (nthcdr 2 (uiop:read-file-lines
                           (export-md-test-fixture-path "oracle/manifest.tsv"))))))
           (case-names nil))
      (with-soft-assertions
        (expect (length rows) :to-equal 8)
        (expect (mapcar #'length rows) :to-equal '(3 3 3 3 3 3 3 3))
        (expect (export-backend-derived-p :md :gfm) :to-be t)
        (dolist (row rows)
          (destructuring-bind (case source include) row
            (push case case-names)
            (let* ((source-path (concatenate 'string "oracle/corpus/" source))
                   (expected-path (concatenate 'string "oracle/md/" case ".md"))
                   (metadata-path (concatenate 'string "oracle/md/" case ".meta"))
                   (source-text (export-md-test-read-text source-path))
                   (metadata (export-md-test-read-text metadata-path))
                   (expected (export-md-test-read-octets expected-path))
                   (include-calls nil)
                   (resolver
                    (unless (string= include "-")
                      (lambda (path parent-source)
                        (declare (ignore parent-source))
                        (push path include-calls)
                        (if (string= path include)
                            (export-md-test-read-text
                             (concatenate 'string "oracle/corpus/" include))
                            (error "Unexpected Markdown oracle include path ~A" path))))))
              (expect (plusp (length source-text)) :to-be t)
              (expect (plusp (length metadata)) :to-be t)
              (expect (plusp (length expected)) :to-be t)
              (expect (search "org=9.8.10" metadata) :to-be-truthy)
              (expect (search "backend=md" metadata) :to-be-truthy)
              (when
                  (and (plusp (length source-text))
                       (plusp (length metadata))
                       (plusp (length expected))
                       (search "org=9.8.10" metadata)
                       (search "backend=md" metadata))
                (let* ((document (parse-org source-text :source-name source))
                       (actual
                        (export-org document
                                    :md
                                    :allow-raw-html
                                    t
                                    :include-resolver
                                    resolver
                                    :output-type
                                    :octets)))
                  (expect actual :to-equalp expected)
                  (expect (reverse include-calls)
                          :to-equal
                          (if (string= include "-")
                              nil
                              (list include))))))))
        (expect (reverse case-names)
                :to-equal '("metadata-selection" "exclude-tags-override"
                       "footnotes-macros-links"
                       "include"
                       "source-blocks"
                       "md-elements"
                       "org-elements"
                       "backend-extended-elements")))))

  (it "enforces output character and UTF-8 octet limits"
    (let* ((source "é")
           (baseline (export-org (parse-org source) :md))
           (baseline-octets
             (cl-codec-kit:string-to-octets baseline :encoding :utf-8))
           (length-error
             (export-md-test-catch-resource-limit
              (lambda ()
                (export-org
                 (parse-org source
                            :limits
                            (make-limits :max-output-length
                                         (1- (length baseline))))
                 :md))))
           (octet-error
             (export-md-test-catch-resource-limit
              (lambda ()
                (export-org
                 (parse-org source
                            :limits
                            (make-limits :max-output-octets
                                         (1- (length baseline-octets))))
                 :md :output-type :octets)))))
      (with-soft-assertions
        (expect (typep length-error 'org-resource-limit-error) :to-be t)
        (when (typep length-error 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name length-error)
                  :to-be :max-output-length)
          (expect (org-resource-limit-error-actual length-error)
                  :to-be (length baseline)))
        (expect (typep octet-error 'org-resource-limit-error) :to-be t)
        (when (typep octet-error 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name octet-error)
                  :to-be :max-output-octets)
          (expect (org-resource-limit-error-actual octet-error)
                  :to-be (length baseline-octets))))))
  (it "checks code-span output limits before materializing the span"
    (let ((failure
            (export-md-test-catch-resource-limit
             (lambda ()
               (export-org
                (parse-org "=x="
                           :limits (make-limits :max-output-length 2))
                :md)))))
      (with-soft-assertions
        (expect (typep failure (quote org-resource-limit-error)) :to-be t)
        (when (typep failure (quote org-resource-limit-error))
          (expect (org-resource-limit-error-limit-name failure)
                  :to-be :max-output-length)
          (expect (org-resource-limit-error-actual failure) :to-be 3)))))
  (it "preflights each footnote body against cumulative output length"
    (let* ((source
             (format nil
                     "Body text[fn:limit].~%~%[fn:limit] =cumulative body=~%"))
           (baseline (export-org (parse-org source) :md))
           (limit (- (length baseline) 2))
           (failure
             (export-md-test-catch-resource-limit
              (lambda ()
                (export-org
                 (parse-org source
                            :limits
                            (make-limits :max-output-length limit))
                 :md)))))
      (with-soft-assertions
        (expect (> (length baseline) 2) :to-be t)
        (expect (typep failure 'org-resource-limit-error) :to-be t)
        (when (typep failure 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name failure)
                  :to-be :max-output-length)
          (expect (org-resource-limit-error-actual failure)
                  :to-be (1- (length baseline)))))))
  (it "renders Markdown references, inline and display math, tasks, and footnotes"
    (let* ((tick (code-char 96))
           (slash (code-char 92))
           (source
             (format nil
                     "#+OPTIONS: toc:t num:2 tags:t todo:t pri:t~%* TODO [#A] Guide :public:~%:PROPERTIES:~%:CUSTOM_ID: guide-id~%:END:~%First hard break  ~%Second line.~%Claim[fn:note].~%Math ~C[x+y~C].~%Inline \\(a+b\\).~%Display ~C[x-y~C].~%Ticks =left~Cright=.~%- [ ] Open~%- [X] Done~%- [-] In progress~%- *Term* :: Definition body.~%10. Tenth~%11. Eleventh~%[fn:note] note body  ~%"
                     slash slash slash slash tick))
           (output (export-org (parse-org source) :md)))
      (with-soft-assertions
        (expect (search "# Table of Contents" output) :to-be-truthy)
        (expect (search "#guide-id" output) :to-be-truthy)
        (expect (search ":public:" output) :to-be-truthy)
        (expect (search "<a id=\"guide-id\"></a>" output) :to-be-truthy)
        (expect (search "First hard break  " output) :to-be-truthy)
        (expect (search "Second line." output) :to-be-truthy)
        (expect (search "$$x+y$$" output) :to-be-truthy)
        (expect (and (search "Display $$x-y$$." output)
             (search "Inline $a+b$." output))
        :to-be-truthy)
        (expect
         (search (concatenate 'string (string tick) (string tick)
                              "left" (string tick) "right"
                              (string tick) (string tick))
                 output)
         :to-be-truthy)
        (expect (search "-   [ ] Open" output) :to-be-truthy)
        (expect (search "-   [X] Done" output) :to-be-truthy)
        (expect (search "-   [-] In progress" output) :to-be-truthy)
        (expect (search "Definition body." output) :to-be-truthy)
        (expect (search "10.   Tenth" output) :to-be-truthy)
        (expect (search "# Footnotes" output) :to-be-truthy)
        (expect (search "note body" output) :to-be-truthy)
        (expect (search "note body  " output) :to-be nil))))
  (it "renders safe and rejected Markdown link handler results"
    (let ((link-registered-p nil)
          (image-registered-p nil))
      (unwind-protect
           (progn
             (register-export-link-type
              :md "md-test"
              (lambda (path description frame context)
                (declare (ignore frame context))
                (make-export-link-result
                 :kind :link
                 :destination
                 (if (string= path "unsafe")
                     "javascript:alert(1)"
                     "https://example.test/safe")
                 :description description)))
             (setf link-registered-p t)
             (register-export-link-type
              :md "md-image"
              (lambda (path description frame context)
                (declare (ignore path frame context))
                (make-export-link-result
                 :kind :image
                 :destination "assets/logo.png"
                 :description description)))
             (setf image-registered-p t)
             (let ((output
                     (export-org
                      (parse-org
                       (format nil
                               "[[md-test:safe][Safe]]~%[[md-test:unsafe][Blocked]]~%[[md-image:asset.png][Logo]]~%[[file:guide.org][Guide]]~%[[file:x][Short path]]~%"))
                      :md)))
               (with-soft-assertions
                 (expect
                  (search "[Safe](https://example.test/safe)" output)
                  :to-be-truthy)
                 (expect (search "Blocked" output) :to-be-truthy)
                 (expect (search "javascript:alert(1)" output) :to-be nil)
                 (expect (search "![Logo](assets/logo.png)" output)
                         :to-be-truthy)
                 (expect (search "[Guide](guide.md)" output)
                         :to-be-truthy)
                 (expect (search "Short path" output) :to-be-truthy)
                 (expect (search "[Short path](x)" output) :to-be nil))))
        (when image-registered-p
          (unregister-export-link-type :md "md-image"))
        (when link-registered-p
          (unregister-export-link-type :md "md-test")))))
  (it "renders generated special block attributes and inline tasks"
    (let* ((source
             (format nil
                     "#+begin_note~%Note body.~%#+end_note~%*************** TODO Inline task~%Task body.~%*************** END~%"))
           (output (export-org (parse-org source) :md)))
      (with-soft-assertions
        (expect (search "id=\"" output) :to-be-truthy)
        (expect (search "class=\"note\"" output) :to-be-truthy)
        (expect (search "Note body." output) :to-be-truthy)
        (expect (search "<div class=\"inlinetask\">" output) :to-be-truthy)
        (expect (search "Inline task" output) :to-be-truthy)
        (expect (search "Task body." output) :to-be-truthy))))
  (it "covers Markdown escaping and less common block forms"
    (let* ((source
             (format nil
                     "#+OPTIONS: toc:t prop:(CUSTOM_ID) pri:t tags:t todo:t~%#+begin_export md~%**kept**~%#+end_export~%#+begin_export latex~%hidden-latex-export~%#+end_export~%@@latex:hidden-latex-snippet@@~%#+HTML: <aside>suppressed</aside>~%* TODO [#B] Parent :one:two:~%:PROPERTIES:~%:CUSTOM_ID: selected-id~%:END:~%#+begin_quote~%quoted first~%quoted second~%#+end_quote~%- Parent item~%  - Nested item~%1. First~%2. Second~%[[https://example.test]]~%Math \\(x+y\\).~%"))
           (output (export-org (parse-org source) :md))
           (tick (string (code-char 96)))
           (padded-code
             (export-org
              (parse-org (format nil "=~Avalue~A=" tick tick)) :md))
           (escaped-output
             (let* ((document
                      (parse-org (format nil "#+OPTIONS: \\n:t~%")))
                    (text
                      (make-node
                       :plain-text
                       :properties
                       (list :value (format nil "a~%#b ![x] *c*"))))
                    (paragraph (make-node :paragraph :children (list text)))
                    (section (make-node :section :children (list paragraph))))
               (append-child (document-root document) section)
               (export-org document :md)))
           (entity-link-output
             (export-org
              (parse-org "[[https://example.test/a&amp;b][Entity link]]")
              :md)))
      (with-soft-assertions
        (expect (search "**kept**" output) :to-be-truthy)
        (expect (or (search "suppressed" output) (search "hidden-latex-export" output) (search "hidden-latex-snippet" output)) :to-be nil)
        (expect (search "CUSTOM_ID: selected-id" output) :to-be-truthy)
        (expect (search "> quoted first" output) :to-be-truthy)
        (expect (search "> quoted second" output) :to-be-truthy)
        (expect (search "Nested item" output) :to-be-truthy)
        (expect (search "1.   First" output) :to-be-truthy)
        (expect (search "2.   Second" output) :to-be-truthy)
        (expect (search "https://example.test" output) :to-be-truthy)
        (expect (search "$x+y$" output) :to-be-truthy)
        (expect
         (search
          (format nil "~A~A ~Avalue~A ~A~A"
                  (code-char 96) (code-char 96) tick tick
                  (code-char 96) (code-char 96))
          padded-code)
         :to-be-truthy)
        (expect
         (search (format nil "a  ~%\\#b \\![x] \\*c\\*") escaped-output)
         :to-be-truthy)
        (expect
         (search "[Entity link](https://example.test/a&amp;b)"
                 entity-link-output)
         :to-be-truthy))))
  (it "formats Markdown list markers and computes nested-list spacing"
    (let* ((paragraph (make-node :paragraph))
           (nested-list (make-node :plain-list))
           (parent
             (make-node :item :children (list paragraph nested-list)))
           (paragraph-view
             (org-kit::%make-export-view :node paragraph :children nil))
           (item
             (make-node :item :post-blank 2))
           (item-view
             (org-kit::%make-export-view :node item :children nil)))
      (with-soft-assertions
        (expect (org-kit::export-md-list-marker-output-length #\+)
                :to-equal 1)
        (expect (org-kit::export-md-list-marker-output-length :bullet)
                :to-equal 6)
        (expect (org-kit::export-md-list-marker-output-length -12)
                :to-equal 4)
        (expect (org-kit::export-md-list-marker-text #\+)
                :to-equal "+")
        (expect (org-kit::export-md-list-marker-text :bullet)
                :to-equal "BULLET")
        (expect (org-kit::export-md-view-post-blank item-view)
                :to-equal 2)
        (expect (org-kit::export-md-view-post-blank paragraph-view)
                :to-equal 0)
        (append-child parent (make-node :drawer))
        (expect (org-kit::export-md-view-post-blank paragraph-view)
                :to-equal 1))))
  (it "renders macro-expanded headings, snippets, and nested-only lists"
    (let* ((source
             (format nil
                     "#+OPTIONS: pri:t~%#+MACRO: emph *$1*~%* [#12] Macro {{{emph(expanded)}}} tail~%@@md:retained snippet@@~%@@text:ignored snippet@@~%- ~%  - Nested only~%"))
           (output (export-org (parse-org source) :md))
           (nested-only-output
             (export-org
              (parse-org (format nil "- ~%  - Nested only~%"))
              :md)))
      (with-soft-assertions
        (expect (search "[#12] Macro" output) :to-be-truthy)
        (expect (search "expanded" output) :to-be-truthy)
        (expect (search "tail" output) :to-be-truthy)
        (expect (search "retained snippet" output) :to-be-truthy)
        (expect (search "ignored snippet" output) :to-be nil)
        (expect (search "Nested only" output) :to-be-truthy)
        (expect nested-only-output
                :to-equal (format nil "-   -   Nested only~%~%")))))
  (it "renders saved descriptive tags and infers missing ordered markers"
    (labels ((collect-items (node)
               (append (if (eq (org-kit::node-type node) :item)
                           (list node)
                           nil)
                       (loop for child in (org-kit::node-children node)
                             append (collect-items child)))))
      (let* ((document
               (parse-org
                (format nil
                        "- Raw term :: Definition body.~%~%Intervening paragraph.~%~%1. Numbered item~%2. Empty bullet item~%3. Nonnumeric bullet item~%")))
             (items (collect-items (org-kit::document-root document))))
        (unless (= (length items) 4)
          (error "Expected a descriptive item and three ordered items"))
        (setf (org-kit::node-property (first items) :tag) nil
              (org-kit::node-property (first items) :reader-tag-range)
              (cons 2 10)
              (org-kit::node-property (second items) :bullet) nil
              (org-kit::node-property (second items) :counter) nil
              (org-kit::node-property (third items) :bullet) ""
              (org-kit::node-property (third items) :counter) nil
              (org-kit::node-property (fourth items) :bullet) "x"
              (org-kit::node-property (fourth items) :counter) nil
              (org-kit::node-property
               (org-kit::node-parent (second items)) :type)
              :ordered)
        (let ((output (export-org document :md)))
          (with-soft-assertions
            (expect (search "**Raw term:** Definition body." output)
                    :to-be-truthy)
            (expect (search "1.   Numbered item" output)
                    :to-be-truthy)
            (expect (search "2.   Empty bullet item" output)
                    :to-be-truthy)
            (expect (search "-   Nonnumeric bullet item" output)
                    :to-be-truthy))))))
  (it "rejects ATTR_HTML on unsupported Markdown views"
    (let* ((source
             (format nil
                     "#+ATTR_HTML: :id unsupported~%#+begin_example~%body~%#+end_example~%"))
           (failure
             (handler-case
                 (progn (export-org (parse-org source) :md) nil)
               (org-export-unsupported-syntax (condition) condition))))
      (with-soft-assertions
        (expect (typep failure (quote org-export-unsupported-syntax)) :to-be t)
        (when (typep failure (quote org-export-unsupported-syntax))
          (expect (org-export-unsupported-syntax-backend failure)
                  :to-be :md)
          (expect (org-export-unsupported-syntax-feature failure)
                  :to-be :html-attribute)
          (expect
           (search
            "ATTR_HTML is supported only on HTML links, images, and special blocks"
            (org-kit-error-message failure))
           :to-be-truthy)))))
  (it "preserves unwrapped LaTeX commands and omits foreign export snippets"
    (let* ((source
             (format nil
                     "Math ~Censuremath{alpha}.~%@@text:ignored snippet@@~%"
                     (code-char 92)))
           (output (export-org (parse-org source) :md)))
      (with-soft-assertions
        (expect (search (format nil "Math ~Censuremath{alpha}." (code-char 92)) output)
                :to-be-truthy)
        (expect (search "ignored snippet" output) :to-be nil))))
  (it "allows HTTPS destinations from Markdown image handlers"
    (let ((registered-p nil))
      (unwind-protect
           (progn
             (register-export-link-type
              :md "md-coverage-image"
              (lambda (path description frame context)
                (declare (ignore path frame context))
                (make-export-link-result
                 :kind :image
                 :destination "https://example.test/remote.png"
                 :description description)))
             (setf registered-p t)
             (let ((output
                     (export-org
                      (parse-org
                       "[[md-coverage-image:logo.png][Remote]]")
                      :md)))
               (with-soft-assertions
                 (expect (search "![Remote](https://example.test/remote.png)"
                                 output)
                         :to-be-truthy))))
        (when registered-p
          (unregister-export-link-type :md "md-coverage-image")))))
  (it "falls back to raw titles and indents deep heading content"
    (labels ((collect-headlines (node)
               (append (if (eq (org-kit::node-type node) :headline)
                           (list node)
                           nil)
                       (loop for child in (org-kit::node-children node)
                             append (collect-headlines child)))))
      (let* ((document
               (parse-org
                (format nil
                        "******* Deep heading~%first paragraph~%~%second paragraph~%")))
             (headlines
               (collect-headlines (org-kit::document-root document))))
        (unless (= (length headlines) 1)
          (error "Expected one deep Markdown heading"))
        (setf (org-kit::node-property (first headlines) :title) nil)
        (let ((output (export-org document :md)))
          (with-soft-assertions
            (expect (search "-   Deep heading" output) :to-be-truthy)
            (expect (search "    first paragraph" output) :to-be-truthy)
            (expect (search "    second paragraph" output) :to-be-truthy))))))
  (it "preserves a terminal hard break in a Markdown quote"
      (let* ((slash (code-char 92))
             (source
              (format nil
                      "#+begin_quote~%quoted line~C~C~%#+end_quote~%"
                      slash slash))
             (document (parse-org source))
             (line-break nil)
             (output nil))
        (map-nodes
         (lambda (node)
           (when (eq (node-type node) :line-break)
             (setf line-break node)))
         (document-root document))
        (unless line-break
          (error "Expected the quote paragraph to contain a terminal line break"))
        (setf output (export-org document :md))
        (with-soft-assertions
         (expect (search (format nil "> quoted line  ~%") output)
                 :to-be-truthy)
         (expect (search (format nil "> quoted line~%> ~%") output)
                 :to-be nil))))
  (it "normalizes alternate Org list bullets in Markdown output"
    (let* ((source (format nil "+ Alternate item~%"))
           (output (export-org (parse-org source) :md)))
      (expect output
              :to-equal (format nil "-   Alternate item~%~%")))))

(describe "Markdown backend helper paths"
  (it "exports a source-backed example block when its value is absent"
    (let* ((source "example body")
           (document (make-document source))
           (example (make-node :example-block :begin 0 :end (length source)))
           (section (make-node :section :children (list example)))
           (root (make-node :org-data :children (list section))))
      (setf (document-root document) root)
      (with-soft-assertions
        (expect (node-property example :value) :to-be nil)
        (expect (export-org document :md)
                :to-equal (format nil "    example body~%~%")))))
  (it "exports an empty Markdown document through the public API"
  (expect (export-org (parse-org "") :md)
          :to-equal (format nil "~%")))
  (it "preserves Markdown-specific keyword values in Markdown export"
    (let* ((source
             (format nil
                     "Before body.~%~%#+MD: **first marker**~%~%Middle body.~%~%#+MARKDOWN: *second marker*~%~%After body.~%"))
           (output (export-org (parse-org source) :md))
           (before-position (search "Before body." output))
           (first-position (search "**first marker**" output))
           (middle-position (search "Middle body." output))
           (second-position (search "*second marker*" output))
           (after-position (search "After body." output)))
      (with-soft-assertions
        (expect before-position :to-be-truthy)
        (expect first-position :to-be-truthy)
        (expect middle-position :to-be-truthy)
        (expect second-position :to-be-truthy)
        (expect after-position :to-be-truthy)
        (when (and before-position first-position middle-position
                   second-position after-position)
          (expect (< before-position first-position) :to-be t)
          (expect (< first-position middle-position) :to-be t)
          (expect (< middle-position second-position) :to-be t)
          (expect (< second-position after-position) :to-be t)))))
  (it "renders a Markdown footnote reference through the public API"
  (let* ((source
           (format nil "Claim[fn:note].~%~%[fn:note] = note body =~%"))
         (output (export-org (parse-org source) :md))
         (reference
           "<sup><a id=\"fnr.note\" class=\"footref\" href=\"#fn.note\" role=\"doc-backlink\">1</a></sup>"))
    (with-soft-assertions
      (expect (search reference output) :to-be-truthy)
      (expect (search "# Footnotes" output) :to-be-truthy)
      (expect (search "note body" output) :to-be-truthy))))
  (it "preserves blank lines in indented Markdown example blocks"
    (let* ((source
             (format nil
                     "#+begin_example~%first~%~%second~%#+end_example~%"))
           (output (export-org (parse-org source) :md)))
      (expect (search (format nil "    first~%~%    second") output)
              :to-be-truthy)))
  (it "keeps a continuation paragraph after a nested Markdown list"
  (let* ((source
           (format nil
                   "- Parent paragraph~%  - Nested item~%~%  Parent continuation.~%"))
         (output (export-org (parse-org source) :md))
         (parent-position (search "-   Parent paragraph" output))
         (nested-position (search "    -   Nested item" output))
         (continuation-position
           (search "    Parent continuation." output)))
    (with-soft-assertions
      (expect parent-position :to-be-truthy)
      (expect nested-position :to-be-truthy)
      (expect continuation-position :to-be-truthy)
      (when (and parent-position nested-position)
        (expect (< parent-position nested-position) :to-be t))
      (when (and nested-position continuation-position)
        (expect (< nested-position continuation-position) :to-be t)))))
  (it "does not indent an empty body under a deep Markdown heading"
    (let* ((source
             (format nil "#+OPTIONS: num:nil~%******* Empty heading~%"))
           (output (export-org (parse-org source) :md)))
      (with-soft-assertions
        (expect (search "-   Empty heading" output) :to-be-truthy)
        (expect (search "    " output) :to-be nil))))
  (it "separates Markdown source code from results when the code has no final newline"
      (let* ((source
              (format nil
                      "#+begin_src emacs-lisp :exports both~%(+ 1 2)~%#+end_src~%#+begin_src emacs-lisp :exports both~%(+ 3 4)~%#+end_src~%#+RESULTS:~%7~%"))
             (document (parse-org source))
             (source-blocks nil))
        (map-nodes
         (lambda (node)
           (when (eq (node-type node) :src-block)
             (push node source-blocks)))
         (document-root document))
        (setf source-blocks (nreverse source-blocks))
        (unless (= (length source-blocks) 2)
          (error "Expected two source blocks"))
        (setf (node-property (second source-blocks) :value)
              "no-final-newline")
        (expect (export-org document :md)
                :to-equal
                (format nil "    (+ 1 2)~%~%    no-final-newline~%~%7~%~%"))))
  (it "renders an empty Markdown quote through the public API"
    (let* ((source (format nil "#+begin_quote~%#+end_quote~%"))
           (output (export-org (parse-org source) :md)))
      (expect output :to-equal (format nil "> ~%~%"))))
  (it "omits heading tags from the Markdown table of contents when configured"
    (let* ((doc
             (parse-org
              (format nil "#+OPTIONS: toc:t tags:not-in-toc~%* Tagged :private:~%")))
           (output (export-org doc :md))
           (toc-position (search "# Table of Contents" output))
           (heading-position (search "# Tagged" output))
           (tag-position (search ":private:" output)))
      (with-soft-assertions
        (expect toc-position :to-be-truthy)
        (expect heading-position :to-be-truthy)
        (expect tag-position :to-be-truthy)
        (when (and heading-position toc-position)
          (expect (> heading-position toc-position) :to-be t))
        (when (and heading-position tag-position)
          (expect (> tag-position heading-position) :to-be t)))))
  (it "trims rendered indentation around Markdown footnote bodies"
  (let* ((source
           (format nil
                   "Claim[fn:note].~%~%[fn:note]~%#+begin_example~%note code~%#+end_example~%"))
         (output (export-org (parse-org source) :md)))
    (with-soft-assertions
      (expect (search "1</a></sup> note code" output) :to-be-truthy)
      (expect (search "1</a></sup>     note code" output) :to-be nil))))
  (it "skips empty render output inside filtered Markdown list views"
    (let ((backend :md-coverage-empty-list-children)
          (document (parse-org (format nil "- Visible~%")))
          (registered-p nil)
          (output nil))
      (labels ((empty-latex-view (source-document)
                 (org-kit::%make-export-view
                  :node
                  (make-node :export-block
                             :properties (list :type "latex" :value "hidden"))
                  :children nil
                  :source-document source-document))
               (clone-view (view)
                 (let* ((node (export-view-node view))
                        (source-document (export-view-source-document view))
                        (children
                          (mapcar #'clone-view (export-view-children view))))
                   (when (eq (node-type node) :item)
                     (setf children
                           (append children
                                   (list (empty-latex-view source-document)))))
                   (when (eq (node-type node) :plain-list)
                     (setf children
                           (cons (empty-latex-view source-document) children)))
                   (org-kit::%make-export-view
                    :node node
                    :children children
                    :payload (org-kit::%export-view-payload view)
                    :source-document source-document)))
               (rewrite-views (views context)
                 (declare (ignore context))
                 (mapcar #'clone-view views)))
        (unwind-protect
             (progn
               (register-derived-export-backend
                backend :md
                :filters (list (cons :parse-tree (list #'rewrite-views))))
               (setf registered-p t
                     output (export-org document backend))
               (expect output :to-equal (format nil "-   Visible~%~%")))
          (when registered-p
            (unregister-export-backend backend))))))
  (it "trims a filter-produced newline from a Markdown quote"
    (let* ((backend :md-coverage-quote-newline-filter)
           (registered-p nil)
           (slash (code-char 92))
           (source
             (format nil
                     "#+begin_quote~%quoted line~C~C~%#+end_quote~%"
                     slash slash))
           (document (parse-org source))
           (output nil))
      (unwind-protect
           (progn
             (register-derived-export-backend
              backend :md
              :filters
              (list
               (cons '(:element :paragraph)
                     (list (lambda (text context)
                             (declare (ignore text context))
                             (format nil "~%"))))))
             (setf registered-p t
                   output (export-org document backend))
             (expect output :to-equal (format nil "> ~%~%")))
        (when registered-p
          (unregister-export-backend backend)))))
  (it "indexes Markdown render state only once across repeated body renders"
    (let* ((document
             (parse-org
              (format nil "#+OPTIONS: toc:t~%* Indexed heading~%")))
           (context (export-md-test-render-context document))
           (state (export-render-context-state context))
           (gfm-state (org-kit::export-md-state-gfm-state state))
           (first-output nil)
           (second-output nil))
      (with-soft-assertions
        (expect (org-kit::export-gfm-state-indexed-p gfm-state) :to-be nil)
        (setf first-output (org-kit::export-md-render-body context))
        (expect (org-kit::export-gfm-state-indexed-p gfm-state) :to-be t)
        (setf second-output (org-kit::export-md-render-body context))
        (expect second-output :to-equal first-output))))
  (it "covers Markdown compatibility renderer fallbacks"
    (let* ((document (parse-org "Legacy paragraph"))
           (context (export-md-test-render-context document))
           (paragraph nil)
           (frame (org-kit::%make-export-render-frame :mode :block))
           (parent-output nil)
           (entity-context
             (export-md-test-render-context (parse-org "Entity context")))
           (entity (make-node :entity :properties (list :html "&alpha;")))
           (entity-output nil)
           (normalized
             (org-kit::export-md-normalize-link-destination
              "<manual&amp;.md>"))
           (plain
             (org-kit::export-md-normalize-link-destination "manual.md"))
           (unclosed
             (org-kit::export-md-strip-link-destination-angles
              "[label](<manual.md)" "<manual.md")))
      (map-nodes
       (lambda (node)
         (when (eq (node-type node) :paragraph)
           (setf paragraph node)))
       (document-root document))
      (setf parent-output
            (let ((org-kit::*export-active-node-dispatch*
                    (list context paragraph nil frame
                          (org-kit::%export-render-context-ancestry context))))
              (org-kit::export-md-render-node
               (export-render-context-state context)
               paragraph nil frame context))
            entity-output
            (org-kit::export-md-render-html-fallback
             (export-render-context-state entity-context) entity nil))
      (with-soft-assertions
        (expect (search "Legacy paragraph" parent-output) :to-be-truthy)
        (expect entity-output :to-equal "&alpha;")
        (expect normalized :to-equal "manual&.md")
        (expect plain :to-equal "manual.md")
        (expect unclosed :to-equal "[label](<manual.md)"))))
  (it "uses the fallback marker for unmarked Markdown list items"
    (let* ((document (parse-org "- Fallback marker"))
           (context (export-md-test-render-context document))
           (preparation (org-kit::%export-render-context-preparation context))
           (root-views (org-kit::export-preparation-root-views preparation))
           (item-view nil))
      (labels ((find-item-view (views)
                 (loop for view in views
                       for node = (export-view-node view)
                       for found =
                         (if (eq (node-type node) :item)
                             view
                             (find-item-view (export-view-children view)))
                       when found return found)))
        (setf item-view (find-item-view root-views)))
      (unless item-view
        (error "Expected the Markdown list fixture to contain an item view"))
      (setf (node-property (export-view-node item-view) :bullet) nil)
      (let ((output
              (let ((org-kit::*export-gfm-list-marker* nil)
                    (org-kit::*export-gfm-output-limits*
                      (export-render-context-limits context))
                    (org-kit::*export-gfm-output-prefix-length* 0)
                    (org-kit::*export-gfm-list-depth* 0))
                (org-kit::export-md-render-list-item
                 (export-render-context-state context) item-view context))))
        (expect output :to-equal "-   Fallback marker"))))
  (it "covers Markdown fallback branches for compatibility nodes"
  (let* ((document (parse-org "Context"))
         (context (export-md-test-render-context document))
         (state (export-render-context-state context))
         (frame (org-kit::%make-export-render-frame :mode :block)))
    (labels ((make-view (node &key children payload)
               (org-kit::%make-export-view
                :node node :children children :payload payload
                :source-document document))
             (render-node (node &key children payload)
               (org-kit::export-md-render-node
                state node
                (make-view node :children children :payload payload)
                frame context))
             (render-source-block (code)
               (let* ((source
                        (format nil
                                "#+begin_src emacs-lisp :exports both~%placeholder~%#+end_src~%#+RESULTS:~%7~%"))
                      (document (parse-org source))
                      (source-block nil))
                 (map-nodes
                  (lambda (node)
                    (when (eq (node-type node) :src-block)
                      (setf source-block node)))
                  (document-root document))
                 (unless source-block
                   (error "Expected a source block for the coverage case"))
                 (setf (node-property source-block :value) code)
                 (export-org document :md)))
             (render-post-blank (parent paragraph nested-list)
               (append-child parent paragraph)
               (append-child parent nested-list)
               (org-kit::export-md-view-post-blank (make-view paragraph))))
      (let* ((special
               (make-node :special-block :properties (list :type nil)))
             (export-block
               (make-node :export-block
                          :properties (list :type nil :value "ignored")))
             (export-snippet
               (make-node :export-snippet
                          :properties (list :back-end nil :value "ignored")))
             (keyword
               (make-node :keyword
                          :properties (list :key nil :value "ignored")))
             (link
               (make-node :link
                          :properties (list :type "file" :path nil)))
             (link-handler-node
               (make-node :link
                          :properties (list :type "custom" :path nil)))
             (file-node
               (make-node :link
                          :properties (list :type "file" :path "guide.org")))
             (position-zero-parent (make-node :item))
             (position-zero-paragraph (make-node :paragraph))
             (position-zero-list (make-node :plain-list))
             (position-one-parent (make-node :item))
             (preceding-node
               (make-node :plain-text :properties (list :value "before")))
             (position-one-paragraph (make-node :paragraph))
             (position-one-list (make-node :plain-list)))
        (append-child position-one-parent preceding-node)
        (append-child position-one-parent position-one-paragraph)
        (with-soft-assertions
          (expect (org-kit::export-md-normalize-link-destination "")
                  :to-equal "")
          (expect (search "class=\"special-block\"" (render-node special))
                  :to-be-truthy)
          (expect (render-node export-block) :to-equal "")
          (expect (render-node export-snippet) :to-equal "")
          (expect (render-node keyword) :to-equal "")
          (expect
           (render-node
            (make-node :latex-fragment
                       :properties
                       (list :value (format nil "~C(" (code-char 92)))))
           :to-equal (format nil "~C(" (code-char 92)))
          (let ((output (render-source-block "")))
            (expect (search "7" output) :to-be-truthy)
            (expect (search "placeholder" output) :to-be nil))
          (let ((output (render-source-block "code")))
            (expect (search "    code" output) :to-be-truthy)
            (expect (search "7" output) :to-be-truthy))
          (let ((output (render-source-block (format nil "code~%"))))
            (expect (search "    code" output) :to-be-truthy)
            (expect (search "7" output) :to-be-truthy))
          (expect
           (org-kit::export-md-render-link-handler
            link-handler-node
            (make-export-link-result
             :kind :link :destination "https://example.test" :description "Link")
            nil)
           :to-be nil)
          (expect (render-node link) :to-equal "")
          (expect
           (org-kit::export-md-render-file-link-destination
            file-node "guide.org" nil)
           :to-be nil)
          (expect
           (render-post-blank position-zero-parent
                              position-zero-paragraph position-zero-list)
           :to-equal 0)
          (expect
           (render-post-blank position-one-parent
                              position-one-paragraph position-one-list)
           :to-equal 1)))
      (let* ((priority-document
               (parse-org
                (format nil "#+OPTIONS: pri:t~%* [#A] Heading~%")))
             (priority-node nil))
        (map-nodes
         (lambda (node)
           (when (eq (node-type node) :headline)
             (setf priority-node node)))
         (document-root priority-document))
        (setf (node-property priority-node :priority) "custom")
        (expect (search "[#custom] Heading"
                        (export-org priority-document :md))
                :to-be-truthy)))))
  (it "exports Markdown and HTML keywords and blocks unsafe file destinations"
  (let* ((source
           (format nil
                   "#+MD: **markdown keyword**~%#+HTML: <b>raw keyword</b>~%[[file:guide.org][Guide]]~%[[file:javascript:guide.org][Unsafe]]~%"))
         (document (parse-org source))
         (denied (export-org document :md))
         (allowed (export-org document :md :allow-raw-html t)))
    (with-soft-assertions
      (expect (search "**markdown keyword**" denied) :to-be-truthy)
      (expect (search "<b>raw keyword</b>" denied) :to-be nil)
      (expect (search "<b>raw keyword</b>" allowed) :to-be-truthy)
      (expect (search "[Guide](guide.md)" denied) :to-be-truthy)
      (expect (search "Unsafe" denied) :to-be-truthy)
      (expect (search "[Unsafe](" denied) :to-be nil)))))

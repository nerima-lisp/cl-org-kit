;;;; t/export-org-test.lisp
(in-package #:cl-org-kit/test)

(defun export-org-test-fixture-path (relative)
  (asdf:system-relative-pathname
   "cl-org-kit" (concatenate 'string "t/fixtures/export/" relative)))

(defun export-org-test-read-text (relative)
  (uiop:read-file-string (export-org-test-fixture-path relative)))

(defun export-org-test-read-octets (relative)
  (with-open-file (stream (export-org-test-fixture-path relative)
                          :direction :input :element-type '(unsigned-byte 8))
    (let* ((length (file-length stream))
           (octets (make-array length :element-type '(unsigned-byte 8))))
      (unless (= (read-sequence octets stream) length)
        (error "Unable to read all bytes from Org fixture ~A" relative))
      octets)))

(defun export-org-test-catch-error (function)
  (handler-case (progn (funcall function) nil)
    (error (condition) condition)))
(defun export-org-test-render-context (document)
  (multiple-value-bind (descriptor ancestry)
      (org-kit::export-registry-snapshot :org)
    (unless descriptor
      (error "The Org export backend is not registered"))
    (org-kit::export-initialize-render-states
     (org-kit::%make-export-render-context
      :backend :org
      :document document
      :metadata nil
      :options nil
      :limits (document-limits document)
      :root-views nil
      :toc nil
      :footnotes nil
      :preparation nil
      :raw-output-options nil
      :extensions nil
      :ancestry ancestry
      :owner-backend :org
      :state nil
      :states nil))))

(describe "E4 Org backend export"
  (it "matches all eight Org 9.8.10 oracle outputs byte for byte"
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
               (nthcdr 2
                (uiop:read-file-lines
                 (export-org-test-fixture-path "oracle/manifest.tsv"))))))
           (case-names nil))
      (with-soft-assertions
        (expect (length rows) :to-equal 8)
        (expect (mapcar #'length rows) :to-equal '(3 3 3 3 3 3 3 3))
        (expect (org-kit::export-backend-registered-p :org) :to-be t)
        (dolist (row rows)
          (destructuring-bind (case source include) row
            (push case case-names)
            (let* ((source-path
                     (concatenate 'string "oracle/corpus/" source))
                   (expected-path
                     (concatenate 'string "oracle/org/" case ".org"))
                   (source-text
                     (export-org-test-read-text source-path))
                   (expected
                     (export-org-test-read-octets expected-path))
                   (metadata
                     (export-org-test-read-text
                      (concatenate 'string "oracle/org/" case ".meta")))
                   (document
                     (parse-org source-text :source-name source))
                   (resolver-calls nil)
                   (resolver
                     (unless (string= include "-")
                       (lambda (path parent-source)
                         (push (list path parent-source) resolver-calls)
                         (unless (and (string= path include)
                                      (string= parent-source source))
                           (error "Unexpected include resolver request: ~S from ~S"
                                  path parent-source))
                         (export-org-test-read-text
                          (concatenate 'string "oracle/corpus/" path)))))
                   (actual
                     (export-org document :org
                                 :include-resolver resolver
                                 :output-type :octets)))
              (expect (plusp (length source-text)) :to-be t)
              (expect (plusp (length expected)) :to-be t)
              (expect (plusp (length metadata)) :to-be t)
              (expect (search "emacs=31.1" metadata) :to-be-truthy)
              (expect (search "org=9.8.10" metadata) :to-be-truthy)
              (expect (search "backend=org" metadata) :to-be-truthy)
              (expect (search (format nil "source=~A" source) metadata)
                      :to-be-truthy)
              (expect
               (search
                (format nil "include-source=~A"
                        (if (string= include "-") "none" include))
                metadata)
               :to-be-truthy)
              (expect (reverse resolver-calls)
                      :to-equal
                      (if (string= include "-")
                          nil
                          (list (list include source))))
              (expect actual :to-equalp expected))))
        (expect
         (reverse case-names)
         :to-equal
         '("metadata-selection" "exclude-tags-override"
           "footnotes-macros-links" "include" "source-blocks"
           "md-elements" "org-elements" "backend-extended-elements")))))

  (it "renders expanded macros in heading titles"
    (let* ((source (format nil "#+MACRO: foo Expanded~%* {{{foo}}}"))
           (output (export-org (parse-org source) :org)))
      (with-soft-assertions
        (expect (search "#+macro: foo Expanded" output) :to-be-truthy)
        (expect (search "* Expanded" output) :to-be-truthy)
        (expect (search "* {{{foo}}}" output) :to-be nil))))

  (it "retains only selected properties in a property drawer"
    (let* ((source
             (format nil
                     "#+OPTIONS: prop:(CUSTOM_ID)~%* Property selection~%:PROPERTIES:~%:CUSTOM_ID: selected-value~%:PRIVATE: hidden-value~%:END:~%"))
           (output (export-org (parse-org source) :org)))
      (with-soft-assertions
        (expect (search ":PROPERTIES:" output) :to-be-truthy)
        (expect (search ":CUSTOM_ID: selected-value" output)
                :to-be-truthy)
        (expect (search ":PRIVATE: hidden-value" output) :to-be nil)
        (expect (search ":END:" output) :to-be-truthy))))

  (it "distinguishes same-day time ranges with warning adjustments from date ranges"
    (let* ((time-range
             (export-org
              (parse-org "<2026-10-22 10:00-11:00 --2d>")
              :org))
           (date-range
             (export-org
              (parse-org "<2026-10-22>--<2026-10-24>")
              :org))
           (warning-date-range
             (export-org
              (parse-org "<2026-10-22 --2d>--<2026-10-24>")
              :org)))
      (with-soft-assertions
        (expect time-range :to-equal
                (format nil "<2026-10-22 Thu 10:00-11:00 --2d>~%"))
        (expect date-range :to-equal
                (format nil "<2026-10-22 Thu>--<2026-10-24 Sat>~%"))
        (expect warning-date-range :to-equal
                (format nil "<2026-10-22 Thu --2d>--<2026-10-24 Sat --2d>~%")))))

  (it "enforces output character and UTF-8 octet limits"
    (let* ((source "é output")
           (baseline (export-org (parse-org source) :org))
           (baseline-octets
             (cl-codec-kit:string-to-octets baseline :encoding :utf-8))
           (below-length
             (export-org-test-catch-error
              (lambda ()
                (export-org
                 (parse-org
                  source
                  :limits (make-limits :max-output-length 1))
                 :org))))
           (below-octets
             (export-org-test-catch-error
              (lambda ()
                (export-org
                 (parse-org
                  source
                  :limits (make-limits :max-output-octets 1))
                 :org :output-type :octets)))))
      (with-soft-assertions
        (expect (plusp (length baseline)) :to-be t)
        (expect (plusp (length baseline-octets)) :to-be t)
        (expect (typep below-length 'org-resource-limit-error) :to-be t)
        (when (typep below-length 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name below-length)
                  :to-be :max-output-length)
          (expect (org-resource-limit-error-limit below-length) :to-be 1)
          (expect (> (org-resource-limit-error-actual below-length) 1) :to-be t))
        (expect (typep below-octets 'org-resource-limit-error) :to-be t)
        (when (typep below-octets 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name below-octets)
                  :to-be :max-output-octets)
          (expect (org-resource-limit-error-limit below-octets) :to-be 1)
          (expect (> (org-resource-limit-error-actual below-octets) 1) :to-be t)))))
  (it "renders registered Org links and preserves unmatched source links"
    (let ((handler-registered-p nil))
      (unwind-protect
           (progn
             (register-export-link-type
              :org "export-test"
              (lambda (path description frame context)
                (declare (ignore path frame context))
                (make-export-link-result
                 :kind :link
                 :destination "https://example.test/target"
                 :description description)))
             (setf handler-registered-p t)
             (let ((output
                     (export-org
                      (parse-org
                       (format nil
                               "[[export-test:target][*Styled*]]~%[[export-test:bare]]~%[[https://example.test/fallback][Fallback]]"))
                      :org)))
               (with-soft-assertions
                 (expect
                  (search "[[https://example.test/target][*Styled*]]" output)
                  :to-be-truthy)
                 (expect
                  (search "[[https://example.test/target]]" output)
                  :to-be-truthy)
                 (expect
                  (search "[[https://example.test/fallback][Fallback]]" output)
                  :to-be-truthy))))
        (when handler-registered-p
          (unregister-export-link-type :org "export-test")))))
  (it "applies Org metadata, planning, clock, priority, tag, and property options"
    (let* ((content
             (format nil
                     "#+TITLE: Export guide~%#+AUTHOR: Ada Example~%#+EMAIL: ada@example.test~%#+CREATOR: Toolchain~%* TODO [#B] Task :private:~%SCHEDULED: <2026-10-20 Tue>~%:PROPERTIES:~%:CUSTOM_ID: selected-id~%:PRIVATE: hidden-value~%:END:~%CLOCK: [2026-10-20 Tue 09:00]--[2026-10-20 Tue 09:30]~%Body~%"))
           (enabled
             (export-org
              (parse-org
               (format nil "#+OPTIONS: author:t email:t creator:comment p:t c:t pri:t prop:(CUSTOM_ID) tags:nil todo:nil~%~A" content))
              :org))
           (defaults (export-org (parse-org content) :org)))
      (with-soft-assertions
        (expect (search "#+author: Ada Example" enabled) :to-be-truthy)
        (expect (search "#+email: ada@example.test" enabled) :to-be-truthy)
        (expect (search "#+creator: Toolchain" enabled) :to-be-truthy)
        (expect (search "SCHEDULED: <2026-10-20 Tue>" enabled) :to-be-truthy)
        (expect (search "CLOCK: [2026-10-20 Tue 09:00]" enabled) :to-be-truthy)
        (expect (search ":CUSTOM_ID: selected-id" enabled) :to-be-truthy)
        (expect (search "hidden-value" enabled) :to-be nil)
        (expect (search "[#B]" enabled) :to-be-truthy)
        (expect (search "TODO" enabled) :to-be nil)
        (expect (search ":private:" enabled) :to-be nil)
        (expect (search "TODO" defaults) :to-be-truthy)
        (expect (search ":private:" defaults) :to-be-truthy)
        (expect (search "SCHEDULED:" defaults) :to-be nil)
        (expect (search "CLOCK:" defaults) :to-be nil)
        (expect (search ":CUSTOM_ID:" defaults) :to-be nil))))
  (it "preserves include boundaries, source code whitespace, keyword cases, and timestamps"
    (let* ((source
             (format nil
                     "* Parent~%Before include.~%#+INCLUDE: \"child.org\"~%After include.~%#+begin_src text~%    first~%      second~%#+end_src"))
           (included
             (export-org
              (parse-org source :source-name "root.org")
              :org
              :include-resolver
              (lambda (path parent-source)
                (declare (ignore parent-source))
                (if (string= path "child.org")
                    (format nil "* Included~%Child body.~%")
                    (error "Unexpected include path ~A" path)))))
           (keywords
             (export-org
              (parse-org
               (format nil "#+RESULTS:~%~%#+RESULTS: Value~%~%#+CUSTOM_KEY: Visible~%"))
              :org))
           (timestamps
             (export-org
              (parse-org
               "<2026-10-20 Tue> [2026-10-21 Wed] <2026-10-25 Sun> <%%(diary-float t 4 2)>")
              :org)))
      (with-soft-assertions
        (expect (search "Before include." included) :to-be-truthy)
        (expect (search "Child body." included) :to-be-truthy)
        (expect (search "After include." included) :to-be-truthy)
        (expect (search "  first" included) :to-be-truthy)
        (expect (search "    second" included) :to-be-truthy)
        (expect (search "#+results: " keywords) :to-be-truthy)
        (expect (search "#+results: Value" keywords) :to-be-truthy)
        (expect (search "#+custom_key: Visible" keywords) :to-be-truthy)
        (expect (search "<2026-10-20 Tue>" timestamps) :to-be-truthy)
        (expect (search "[2026-10-21 Wed]" timestamps) :to-be-truthy)
        (expect (search "<2026-10-25 Sun>" timestamps) :to-be-truthy)
        (expect
         (search "<%%(diary-float t 4 2)>" timestamps)
         :to-be-truthy))))
  (it "covers Org keyword filtering, headline labels, and source normalization"
    (let* ((source
             (format nil
                     "#+OPTIONS: pri:t~%* COMMENT Parent~%~%** Child[fn:note]~%Child body.~%[fn:note] Definition.~%"))
           (output (export-org (parse-org source) :org))
           (document (parse-org "Text"))
           (node (first (node-children (document-root document))))
           (view (org-kit::%make-export-view
                  :node node :children nil :source-document nil))
           (attribute-line (format nil "  #+ATTR_HTML: :width 10~%"))
           (reference
             (make-node :footnote-reference
                        :properties (list :type :standard :label "note")))
           (title-segment (make-node :paragraph :children (list reference)))
           (headline
             (make-node :headline :properties (list :title (list title-segment))))
           (append-error
             (handler-case
                 (progn (org-kit::export-org-backend-append nil :invalid) nil)
               (error (condition) condition))))
      (with-soft-assertions
        (expect
         (org-kit::export-org-backend-normalize-element
          (format nil "value  ~C~%" #\Tab))
         :to-equal (format nil "value~%"))
        (expect (org-kit::export-org-backend-priority-string 12)
                :to-equal "12")
        (progn
  (expect
   (org-kit::export-org-backend-attribute-line-p
    attribute-line 0 (length attribute-line) t)
   :to-be t)
  (dolist (text
           (list (format nil "#+ATTR_: value~%")
                 (format nil "#+ATTR_HTML value~%")
                 (format nil "#+ATTR_HTML:bad~%")))
    (expect
     (org-kit::export-org-backend-attribute-line-p
      text 0 (length text) t)
     :to-be nil))
  (expect
   (org-kit::export-org-backend-attribute-line-p
    "#+ATTR_HTML: value" 0 (length "#+ATTR_HTML: value") nil)
   :to-be nil)
  (let ((task (make-node :inlinetask)))
    (dolist (text (list "END" "*** END trailing" "*** ENDx"))
      (expect
       (org-kit::export-org-backend-normalize-identity-text task text)
       :to-equal text)))
  (let* ((outside-item
           (make-node :item :properties (list :bullet "+")))
         (outside-parent
           (make-node :paragraph :children (list outside-item)))
         (list-item
           (make-node :item :properties (list :bullet nil)))
         (list-parent
           (make-node :plain-list :children (list list-item))))
    (declare (ignore outside-parent list-parent))
    (expect
     (org-kit::export-org-backend-normalize-identity-text
      outside-item "  + Item")
     :to-equal "  + Item")
    (expect
     (org-kit::export-org-backend-normalize-identity-text
      list-item "  Item")
     :to-equal "  Item")))
        (expect (org-kit::export-org-backend-view-node view) :to-be node)
        (expect (org-kit::export-org-backend-view-children view) :to-equal nil)
        (expect (org-kit::export-org-backend-source-document view)
                :to-be document)
        (expect append-error :to-be-truthy)
        (expect
         (org-kit::export-org-backend-headline-footnote-labels headline)
         :to-equal (list "note"))
        (expect (org-kit::export-org-backend-headline-footnote-labels nil)
                :to-equal nil)
        (expect (search "COMMENT Parent" output) :to-be-truthy)
        (progn
          (expect (search "Child body." output) :to-be-truthy)
          (let ((registered-p nil))
            (unwind-protect
                 (progn
                   (register-derived-export-backend
                    :coverage-org-keyword :org
                    :accepted-keywords (list "CUSTOM_KEY"))
                   (setf registered-p t)
                   (expect
                    (export-org
                     (parse-org "#+CUSTOM_KEY: Suppressed")
                     :coverage-org-keyword)
                    :to-equal ""))
              (when registered-p
                (unregister-export-backend :coverage-org-keyword))))))))
  (it "covers Org child keyword, drawer, and attribute boundary paths"
  (let* ((document (make-document "visible"))
         (context (export-org-test-render-context document))
         (keyword
           (make-node :keyword :properties (list :key nil :value "Value")))
         (keyword-view nil)
         (preamble-property
           (make-node :keyword
                      :properties
                      (list :key "PROPERTY" :value "Preamble" :post-blank 1)))
         (preamble-section
           (make-node :section :children (list preamble-property)))
         (preamble-root (make-node :org-data :children (list preamble-section)))
         (body-property
           (make-node :keyword
                      :properties (list :key "PROPERTY" :value "Body")))
         (body-parent (make-node :paragraph :children (list body-property)))
         (drawer-document
           (parse-org
            (format nil
                    ":NOTES:~%visible drawer~%:END:~%:LOGBOOK:~%hidden drawer~%:END:~%")))
         (drawer-context (export-org-test-render-context drawer-document))
         (drawer-views nil)
         (notes-view nil)
         (logbook-view nil)
         (attribute-without-colon (format nil "#+ATTR_HTML~%"))
         (attribute-final-colon "#+ATTR_HTML:")
         (task (make-node :inlinetask)))
    (labels ((make-view (node source-document &optional children payload)
               (org-kit::%make-export-view
                :node node :children children :source-document source-document
                :payload payload))
             (render-view (render-context view)
               (org-kit::export-org-backend-capture
                render-context
                (lambda ()
                  (org-kit::export-org-backend-render-child
                   render-context view))))
             (node-view (node)
               (make-view
                node drawer-document
                (mapcar (function node-view) (node-children node))))
             (render-drawer (view)
               (render-view drawer-context view)))
      (setf (document-root document) preamble-root
            keyword-view (make-view keyword document))
      (map-nodes
       (lambda (node)
         (when (eq (node-type node) :drawer)
           (push (cons (node-property node :drawer-name) (node-view node))
                 drawer-views)))
       (document-root drawer-document))
      (setf notes-view (cdr (assoc "NOTES" drawer-views :test #'string=))
            logbook-view
            (cdr (assoc "LOGBOOK" drawer-views :test #'string=)))
      (with-soft-assertions
        (expect (org-kit::node-parent body-property) :to-be body-parent)
        (expect (render-view context keyword-view)
                :to-equal (format nil "#+: Value~%"))
        (expect
         (render-view context (make-view preamble-property document))
         :to-equal (format nil "#+property: Preamble~%"))
        (expect
         (render-view context (make-view body-property document))
         :to-equal (format nil "#+property: Body~%"))
        (expect notes-view :to-be-truthy)
        (expect logbook-view :to-be-truthy)
        (setf (org-kit::%export-render-context-options drawer-context)
              (list (cons :drawers (list :include "NOTES"))))
        (expect (search "visible drawer" (render-drawer notes-view))
                :to-be-truthy)
        (expect (render-drawer logbook-view) :to-equal "")
        (setf (org-kit::%export-render-context-options drawer-context)
              (list (cons :drawers t)))
        (expect (search "hidden drawer" (render-drawer logbook-view))
                :to-be-truthy)
        (expect
         (org-kit::export-org-backend-attribute-line-p
          attribute-without-colon 0 (1- (length attribute-without-colon)) t)
         :to-be nil)
        (expect
         (org-kit::export-org-backend-attribute-line-p
          attribute-final-colon 0 (length attribute-final-colon) t)
         :to-be t)
        (dolist (text (list (format nil "x~%") " END"))
          (expect
           (org-kit::export-org-backend-normalize-identity-text task text)
           :to-equal text))))))

  (it "separates included Org documents with blank lines"
  (let* ((parent-document (make-document ""))
         (child-document (make-document ""))
         (context (export-org-test-render-context parent-document))
         (parent-before
           (make-node :keyword
                      :properties
                      (list :key "RESULTS" :value (format nil "before~%"))))
         (child-first
           (make-node :keyword
                      :properties
                      (list :key "RESULTS" :value (format nil "child~%"))))
         (parent-middle
           (make-node :keyword :properties (list :key "RESULTS" :value "middle")))
         (child-last
           (make-node :keyword :properties (list :key "RESULTS" :value "after")))
         (parent-last
           (make-node :keyword :properties (list :key "RESULTS" :value "last"))))
    (labels ((make-view (node source-document)
               (org-kit::%make-export-view
                :node node :children nil :source-document source-document))
             (render-children ()
               (org-kit::export-org-backend-capture
                context
                (lambda ()
                  (org-kit::export-org-backend-render-children
                   context
                   (list (make-view parent-before parent-document)
                         (make-view child-first child-document)
                         (make-view parent-middle parent-document)
                         (make-view child-last child-document)
                         (make-view parent-last parent-document))
                   parent-document)))))
      (with-soft-assertions
        (expect (render-children)
                :to-equal
                (format nil
                        "#+results: before~%#+results: child~%~%#+results: middle~%#+results: after~%~%#+results: last~%"))))))



  (it "renders source-backed and spanless programmatic children"
    (let* ((source-document (parse-org "Original."))
           (source-paragraph nil)
           (injected-keyword
             (make-node :keyword
                        :properties (list :key "CUSTOM_KEY" :value "Injected")))
           (generated-document (make-document ""))
           (generated-keyword
             (make-node :keyword
                        :properties (list :key "CUSTOM_KEY" :value "Generated")))
           (generated-paragraph
             (make-node :paragraph :children (list generated-keyword)))
           (generated-section
             (make-node :section :children (list generated-paragraph)))
           (generated-root
             (make-node :org-data :children (list generated-section))))
      (map-nodes
       (lambda (node)
         (when (and (null source-paragraph)
                    (eq (node-type node) :paragraph))
           (setf source-paragraph node)))
       (document-root source-document))
      (unless source-paragraph
        (error "Expected the parsed source to contain a paragraph"))
      (append-child source-paragraph injected-keyword)
      (setf (document-root generated-document) generated-root)
      (let ((source-output (export-org source-document :org))
            (generated-output (export-org generated-document :org)))
        (with-soft-assertions
          (expect (search "Original." source-output) :to-be-truthy)
          (expect (search "#+custom_key: Injected" source-output)
                  :to-be-truthy)
          (expect (search "#+custom_key: Generated" generated-output)
                  :to-be-truthy)))))
  (it "preserves source-block metadata, plain fixed-width text, and final newlines"
      (let* ((document
              (parse-org
               (format nil "#+begin_src text~%original~%#+end_src")))
             (source-block nil)
             (preserved-document
              (parse-org
               (format nil "#+begin_src text~%    first~%      second~%#+end_src~%")))
             (preserved-block nil)
             (fixed-width-output
              (export-org (parse-org ": x") :org)))
        (map-nodes
         (lambda (node)
           (when (and (null source-block) (eq (node-type node) :src-block))
             (setf source-block node)))
         (document-root document))
        (map-nodes
         (lambda (node)
           (when (and (null preserved-block) (eq (node-type node) :src-block))
             (setf preserved-block node)))
         (document-root preserved-document))
        (unless (and source-block preserved-block)
          (error "Expected both parsed source blocks"))
        (setf (node-property source-block :value) "terminal"
              (node-property preserved-block :preserve-indent) t)
        (let ((output (export-org document :org))
              (preserved-output (export-org preserved-document :org)))
          (with-soft-assertions
            (expect
             (search (format nil "#+begin_src text~%  terminal~%#+end_src")
                     output)
             :to-be-truthy)
            (expect
             (search
              (format nil "#+begin_src text~%    first~%      second~%#+end_src")
              preserved-output)
             :to-be-truthy)
            (expect fixed-width-output
                    :to-equal (format nil ": x~%"))))))
  (it "preserves blank lines before nested Org headlines"
    (let* ((document
             (parse-org (format nil "* Parent~%~%** Child~%")))
           (output (export-org document :org)))
      (with-soft-assertions
        (expect
         (search (format nil "* Parent~%~%** Child") output)
         :to-be-truthy))))
  (it "counts blank source lines after parsed headline starts and at EOF"
    (let* ((document
             (parse-org (format nil "* Parent~%~%** Child~%")))
           (unterminated-document (parse-org "* Lone"))
           (trailing-blank-document
             (parse-org (format nil "* Parent~%  ")))
           (headlines nil)
           (unterminated-headline nil)
           (trailing-blank-headline nil))
      (map-nodes
       (lambda (node)
         (when (eq (node-type node) :headline)
           (push node headlines)))
       (document-root document))
      (map-nodes
       (lambda (node)
         (when (and (null unterminated-headline)
                    (eq (node-type node) :headline))
           (setf unterminated-headline node)))
       (document-root unterminated-document))
      (map-nodes
       (lambda (node)
         (when (and (null trailing-blank-headline)
                    (eq (node-type node) :headline))
           (setf trailing-blank-headline node)))
       (document-root trailing-blank-document))
      (setf headlines (nreverse headlines))
      (let* ((parent (first headlines))
             (child (second headlines))
             (parent-view
               (org-kit::%make-export-view
                :node parent :children nil :source-document document))
             (child-view
               (org-kit::%make-export-view
                :node child :children nil :source-document document))
             (unterminated-view
               (org-kit::%make-export-view
                :node unterminated-headline :children nil
                :source-document unterminated-document))
             (trailing-blank-view
               (org-kit::%make-export-view
                :node trailing-blank-headline :children nil
                :source-document trailing-blank-document)))
        (with-soft-assertions
          (expect (length headlines) :to-be 2)
          (expect (node-begin parent) :to-be 0)
          (expect
           (org-kit::export-org-backend-headline-pre-blank parent parent-view)
           :to-be 1)
          (expect
           (org-kit::export-org-backend-headline-pre-blank child child-view)
           :to-be 0)
          (expect unterminated-headline :to-be-truthy)
          (expect (node-begin unterminated-headline) :to-be 0)
          (expect
           (org-kit::export-org-backend-headline-pre-blank
            unterminated-headline unterminated-view)
           :to-be 0)
          (expect trailing-blank-headline :to-be-truthy)
          (expect (node-begin trailing-blank-headline) :to-be 0)
          (expect
           (org-kit::export-org-backend-headline-pre-blank
            trailing-blank-headline trailing-blank-view)
           :to-be 1)))))
  (it "preserves unterminated blocks and normalizes uppercase source terminators"
    (let* ((unterminated-source (format nil "#+begin_src text~%body~%"))
           (unterminated-output
             (export-org (parse-org unterminated-source) :org))
           (uppercase-source
             (format nil "#+begin_src text~%body~%#+END_SRC"))
           (uppercase-output
             (export-org (parse-org uppercase-source) :org)))
      (with-soft-assertions
        (expect unterminated-output :to-equal unterminated-source)
        (expect uppercase-output
                :to-equal
                (format nil "#+begin_src text~%  body~%#+end_src~%")))))
    (it "renders source when an internal source-block mode is unrecognized"
    (let* ((source (format nil "#+begin_src text~%body~%#+end_src"))
           (expected (format nil "#+begin_src text~%  body~%#+end_src~%"))
           (document (parse-org source))
           (source-block nil))
      (map-nodes
       (lambda (node)
         (when (eq (node-type node) :src-block)
           (setf source-block node)))
       (document-root document))
      (unless source-block
        (error "Expected the parsed source to contain a source block"))
      (let* ((context (export-org-test-render-context document))
             (default-view
               (org-kit::%make-export-view
                :node source-block :children nil :payload nil
                :source-document document))
             (fallback-view
               (org-kit::%make-export-view
                :node source-block :children nil
                :payload (list :exports :future)
                :source-document document))
             (default-output
               (org-kit::export-org-backend-render-source-contents
                source-block default-view context))
             (fallback-output
               (org-kit::export-org-backend-render-source-contents
                source-block fallback-view context)))
        (with-soft-assertions
          (expect default-output :to-equal expected)
          (expect fallback-output :to-equal expected)
          (expect fallback-output :to-equal default-output)))))
  (it "preserves source for a source-backed table with no rows"
    (let* ((source (format nil "| A |~%"))
           (document (parse-org source))
           (table nil))
      (map-nodes
       (lambda (node)
         (when (eq (node-type node) :table)
           (setf table node)))
       (document-root document))
      (unless table
        (error "Expected the parsed source to contain a table"))
      (setf (node-children table) nil)
      (let* ((view
               (org-kit::%make-export-view
                :node table :children nil :source-document document))
             (context (export-org-test-render-context document))
             (output
               (org-kit::export-org-backend-capture
                context
                (lambda ()
                  (org-kit::export-org-backend-render-table
                   table view context)))))
        (with-soft-assertions
          (expect (node-begin table) :to-be 0)
          (expect (node-end table) :to-be (length source))
          (expect output :to-equal source)))))
    (it "renders child views for a raw INCLUDE keyword"
    (let* ((document
             (parse-org (format nil "#+INCLUDE: \"child.org\"~%")))
           (child-document
             (parse-org (format nil "#+CHILD_TEXT: Included child.~%")))
           (include-node nil)
           (child-keyword nil))
      (map-nodes
       (lambda (node)
         (when (and (eq (node-type node) :keyword)
                    (string= (node-property node :key) "INCLUDE"))
           (setf include-node node)))
       (document-root document))
      (map-nodes
       (lambda (node)
         (when (and (null child-keyword)
                    (eq (node-type node) :keyword)
                    (string= (node-property node :key) "CHILD_TEXT"))
           (setf child-keyword node)))
       (document-root child-document))
      (unless include-node
        (error "Expected the parsed source to contain an INCLUDE keyword"))
      (unless child-keyword
        (error "Expected the child source to contain a CHILD_TEXT keyword"))
      (let* ((child-view
               (org-kit::%make-export-view
                :node child-keyword :children nil
                :source-document child-document))
             (include-view
               (org-kit::%make-export-view
                :node include-node :children (list child-view)
                :source-document document))
             (context (export-org-test-render-context document))
             (output
               (org-kit::export-org-backend-render-keyword
                include-node include-view context)))
        (with-soft-assertions
          (expect output :to-equal
                  (format nil "#+child_text: Included child.~%"))))))
      (it
  "selects named drawers and all drawers and properties"
  (let* ((source
           (format nil
                   "#+OPTIONS: d:(NOTES) prop:t~%* Drawer selection~%:PROPERTIES:~%:CUSTOM_ID: selected~%:PRIVATE: retained~%:END:~%:NOTES:~%note value~%:END:~%:LOGBOOK:~%log value~%:END:~%"))
         (all-drawers-source
           (format nil
                   "#+OPTIONS: d:t~%* Drawer selection~%:NOTES:~%note value~%:END:~%:LOGBOOK:~%log value~%:END:~%"))
         (no-drawers-source
           (format nil
                   "#+OPTIONS: d:nil~%* Drawer selection~%:NOTES:~%note value~%:END:~%"))
         (output (export-org (parse-org source) :org))
         (all-drawers-output
           (export-org (parse-org all-drawers-source) :org))
         (no-drawers-output
           (export-org (parse-org no-drawers-source) :org)))
    (with-soft-assertions
      (expect (search ":NOTES:" output) :to-be-truthy)
      (expect (search "note value" output) :to-be-truthy)
      (expect (search ":LOGBOOK:" output) :to-be nil)
      (expect (search "log value" output) :to-be nil)
      (expect (search ":CUSTOM_ID: selected" output) :to-be-truthy)
      (expect (search ":PRIVATE: retained" output) :to-be-truthy)
      (expect (search ":NOTES:" all-drawers-output) :to-be-truthy)
      (expect (search ":LOGBOOK:" all-drawers-output) :to-be-truthy)
      (expect (search "log value" all-drawers-output) :to-be-truthy)
      (expect (search ":NOTES:" no-drawers-output) :to-be nil)
      (expect (search "note value" no-drawers-output) :to-be nil))))
        (it "drops affiliated attributes while preserving blank source-code lines"
    (let* ((source
             (format nil
                     "#+ATTR_HTML: :width 10~%#+ATTR_HTML_2-ALT: :width 20~%#+ATTR_HTML:~%#+begin_src text~%first~%~%second~%#+end_src~%"))
           (output (export-org (parse-org source) :org)))
      (with-soft-assertions
        (expect (search "#+ATTR_HTML:" output) :to-be nil)
        (expect (search "attr_html_2-alt" output) :to-be nil)
        (expect (search "attr_html:" output) :to-be nil)
        (expect (search (format nil "  first~%~%  second") output)
                :to-be-truthy))))
          (it "renders source blocks, associated results, and empty code bodies"
    (let* ((source
             (format nil
                     "#+begin_src text :exports both~%first~%~%second~%#+end_src~%#+RESULTS:~%: result value~%~%#+begin_src text :exports both~%code only~%#+end_src"))
           (output (export-org (parse-org source) :org))
           (first-code
             (format nil
                     "#+begin_src text :exports both~%  first~%~%  second~%#+end_src"))
           (result
             (format nil "#+results: ~%: result value"))
           (code-only
             (format nil
                     "#+begin_src text :exports both~%  code only~%#+end_src"))
           (first-code-start (search first-code output))
           (result-start
             (and first-code-start
                  (search result output
                          :start2 (+ first-code-start (length first-code)))))
           (code-only-start
             (and result-start
                  (search code-only output
                          :start2 (+ result-start (length result)))))
           (extra-result
             (and result-start
                  (search "#+results: " output :start2 (1+ result-start))))
           (empty-output
             (export-org
              (parse-org (format nil "#+begin_src text~%#+end_src~%"))
              :org)))
      (with-soft-assertions
        (expect first-code-start :to-be-truthy)
        (expect result-start :to-be-truthy)
        (expect code-only-start :to-be-truthy)
        (when (and first-code-start result-start code-only-start)
          (expect (< first-code-start result-start code-only-start)
                  :to-be t))
        (expect extra-result :to-be nil)
        (expect empty-output
                :to-equal
                (format nil "#+begin_src text~%#+end_src~%")))))
            (it
  "normalizes nondefault list bullets and inline-task terminators"
  (let* ((stars (make-string 15 :initial-element #\*))
         (source
           (format nil
                   "+ Alternate item~%  + Nested alternate item~%- Standard item~%~A TODO Inline task~%Body~%~A END~%"
                   stars stars))
         (output (export-org (parse-org source) :org)))
    (with-soft-assertions
      (expect (search "- Alternate item" output) :to-be-truthy)
      (expect (search "- Nested alternate item" output) :to-be-truthy)
      (expect (search "+ Nested alternate item" output) :to-be nil)
      (expect (search "- Standard item" output) :to-be-truthy)
      (expect (search "+ Standard item" output) :to-be nil)
      (expect (search (format nil "~A end" stars) output) :to-be-truthy)
      (expect (search (format nil "~A END" stars) output) :to-be nil))))
              (it "honors disabled title and date options and omits blank metadata"
    (let* ((disabled
             (export-org
              (parse-org
               (format nil
                       "#+OPTIONS: title:nil date:nil~%#+TITLE: Hidden title~%#+DATE: Hidden date~%"))
              :org))
           (blank
             (export-org
              (parse-org
               (format nil "#+TITLE:   ~%#+DATE:   ~%"))
              :org))
           (unicode-blank
             (export-org
              (parse-org
               (format nil "#+TITLE: ~C~%#+DATE: ~C~%"
                       (code-char #xa0) (code-char #x2003)))
              :org)))
      (with-soft-assertions
        (expect (search "#+title: Hidden title" disabled) :to-be nil)
        (expect (search "#+date: Hidden date" disabled) :to-be nil)
        (expect (search "#+title:" blank) :to-be nil)
        (expect (search "#+date:" blank) :to-be nil)
        (expect (search "#+title:" unicode-blank) :to-be nil)
        (expect (search "#+date:" unicode-blank) :to-be nil))))
                (it "renders supported and empty programmatic headline title segments"
                    (let* ((segment-source "Linked")
                           (segment-document (make-document segment-source))
                           (title-segment
                            (make-node :plain-text :begin 0 :end (length segment-source)))
                           (empty-title-segment (make-node :plain-text))
                           (segment-paragraph
                            (make-node :paragraph :begin 0 :end (length segment-source)
                                       :children (list title-segment)))
                           (segment-section (make-node :section :children (list segment-paragraph)))
                           (segment-root (make-node :org-data :children (list segment-section)))
                           (document (make-document ""))
                           (headline
                            (make-node :headline
                                       :properties
                                       (list :level 1
                                             :title
                                             (list "Generated " 42 empty-title-segment title-segment))))
                           (section (make-node :section))
                           (root (make-node :org-data :children (list headline))))
                      (setf (document-root segment-document) segment-root)
                      (append-child headline section)
                      (setf (document-root document) root)
                      (let ((output (export-org document :org)))
                        (with-soft-assertions
                         (expect (search "* Generated Linked" output) :to-be-truthy)
                         (expect (search "42" output) :to-be nil)
                         (expect (search "NIL" output) :to-be nil)))))
                  (it "uses a registered link description when it differs from source text"
    (let ((registered-p nil))
      (unwind-protect
           (progn
             (register-export-link-type
              :org "description-test"
              (lambda (path description frame context)
                (declare (ignore path description frame context))
                (make-export-link-result
                 :kind :link
                 :destination "https://example.test/target"
                 :description "Handler label")))
             (setf registered-p t)
             (let ((output
                     (export-org
                      (parse-org
                       "[[description-test:target][Source label]]")
                      :org)))
               (with-soft-assertions
                 (expect
                  (search
                   "[[https://example.test/target][Handler label]]"
                   output)
                  :to-be-truthy))))
        (when registered-p
          (unregister-export-link-type :org "description-test")))))
                  (it
  "normalizes repeated blank lines around included document boundaries"
  (let* ((source
           (format nil
                   "Before.~%~%~%#+INCLUDE: \"child.org\"~%After.~%"))
         (output
           (export-org
            (parse-org source :source-name "root.org")
            :org
            :include-resolver
            (lambda (path parent-source)
              (declare (ignore parent-source))
              (if (string= path "child.org")
                  (format nil "Included.~%~%")
                  (error "Unexpected include path ~A" path))))))
    (with-soft-assertions
      (expect (search (format nil "Before.~%Included.") output)
              :to-be-truthy)
      (expect (search (format nil "Included.~%~%After.") output)
              :to-be-truthy))))
                  (it
  "preserves Org link syntax in headline titles"
  (let* ((source
           "* Heading [[https://example.test/path][Linked title]]")
         (output (export-org (parse-org source) :org)))
    (expect (search source output) :to-be-truthy)))
                  (it "keeps adjacent included documents and omitted drawers distinct"
    (let* ((source-document (make-document ""))
           (first-child-document (make-document ""))
           (second-child-document (make-document ""))
           (context (export-org-test-render-context source-document))
           (first-keyword
             (make-node :keyword
                        :properties (list :key "RESULTS" :value "first")))
           (second-keyword
             (make-node :keyword
                        :properties (list :key "RESULTS" :value "second")))
           (drawer-node
             (make-node :drawer :properties (list :drawer-name "NOTES")))
           (first-view nil)
           (second-view nil)
           (drawer-view nil)
           (adjacent-output nil)
           (drawer-output nil)
           (children-output nil)
           (task (make-node :inlinetask)))
      (labels ((make-view (node source)
                 (org-kit::%make-export-view
                  :node node :children nil :source-document source)))
        (setf first-view (make-view first-keyword first-child-document)
              second-view (make-view second-keyword second-child-document)
              drawer-view (make-view drawer-node source-document))
        (setf adjacent-output
              (org-kit::export-org-backend-capture
               context
               (lambda ()
                 (org-kit::export-org-backend-render-children
                  context (list first-view second-view) source-document)))
              drawer-output
              (org-kit::export-org-backend-capture
               context
               (lambda ()
                 (org-kit::export-org-backend-render-child
                  context drawer-view)))
              children-output
              (org-kit::export-org-backend-capture
               context
               (lambda ()
                 (org-kit::export-org-backend-render-children
                  context (list drawer-view) source-document)))))
      (with-soft-assertions
        (expect adjacent-output
                :to-equal
                (format nil "#+results: first~%#+results: second~%"))
        (expect drawer-output :to-equal "")
        (expect children-output :to-equal "")
        (expect
         (org-kit::export-org-backend-normalize-identity-text task "x END")
         :to-equal "x END"))))
                  (it "covers empty source transitions and disabled drawer rendering"
  (let* ((source-document (make-document ""))
         (child-document (make-document ""))
         (context (export-org-test-render-context source-document))
         (parent-node
           (make-node :src-block :properties (list :value "hidden")))
         (child-node
           (make-node :keyword
                      :properties
                      (list :key "CUSTOM_KEY" :value "visible")))
         (drawer-document (make-document ""))
         (drawer-context (export-org-test-render-context drawer-document))
         (drawer-node
           (make-node :drawer :properties (list :drawer-name "NOTES")))
         (parent-output nil)
         (drawer-output nil))
    (labels ((make-view (node source &optional payload)
               (org-kit::%make-export-view
                :node node :children nil :payload payload
                :source-document source))
             (capture-render (render-context thunk)
               (org-kit::export-org-backend-capture
                render-context thunk)))
      (setf (org-kit::%export-render-context-options drawer-context)
            (list (cons :drawers nil))
            parent-output
            (capture-render
             context
             (lambda ()
               (org-kit::export-org-backend-render-children
                context
                (list (make-view parent-node source-document
                                 (list :exports :none))
                      (make-view child-node child-document))
                source-document)))
            drawer-output
            (capture-render
             drawer-context
             (lambda ()
               (org-kit::export-org-backend-render-child
                drawer-context
                (make-view drawer-node drawer-document)))))
      (with-soft-assertions
        (expect parent-output
                :to-equal (format nil "#+custom_key: visible~%"))
        (expect drawer-output :to-equal ""))))))

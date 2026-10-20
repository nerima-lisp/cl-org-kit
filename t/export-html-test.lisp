;;;; t/export-html-test.lisp
(in-package #:cl-org-kit/test)

(defstruct export-html-test-node type value attributes children)

(defun export-html-test-read-file (relative-path)
  (uiop:read-file-string
   (asdf:system-relative-pathname "cl-org-kit" relative-path)))

(defun export-html-test-normalize-newlines (text)
  (with-output-to-string (output)
    (loop with position = 0
          while (< position (length text))
          for character = (char text position)
          do (if (and (char= character #\Return)
                      (< (1+ position) (length text))
                      (char= (char text (1+ position)) #\Newline))
                 (progn
                   (write-char #\Newline output)
                   (incf position 2))
                 (progn
                   (write-char character output)
                   (incf position))))))

(defun export-html-test-space-p (character)
  (member character '(#\Space #\Tab #\Newline #\Return #\Page)
          :test #'char=))

(defun export-html-test-whitespace-only-p (text)
  (and (stringp text)
       (every #'export-html-test-space-p text)))

(defun export-html-test-find-tag-end (text start)
  (loop with quote = nil
        for position from start below (length text)
        for character = (char text position)
        do (cond
             (quote
              (when (char= character quote)
                (setf quote nil)))
             ((member character (list (code-char 34) (code-char 39)) :test #'char=)
              (setf quote character))
             ((char= character #\>) (return position)))
        finally (return nil)))

(defun export-html-test-parse-start-tag (text start end)
  (let* ((content (subseq text start end))
         (trimmed (string-right-trim
                   '(#\Space #\Tab #\Newline #\Return) content))
         (self-closing-p
           (and (plusp (length trimmed))
                (char= (char trimmed (1- (length trimmed))) #\/)))
         (content (if self-closing-p
                      (subseq trimmed 0 (1- (length trimmed)))
                      trimmed))
         (position 0)
         (length (length content)))
    (labels ((skip-space ()
               (loop while (and (< position length)
                                (export-html-test-space-p
                                 (char content position)))
                     do (incf position)))
             (scan-name ()
               (let ((start position))
                 (loop while (and (< position length)
                                  (not (export-html-test-space-p
                                        (char content position)))
                                  (not (member (char content position)
                                               '(#\= #\/)
                                               :test #'char=)))
                       do (incf position))
                 (subseq content start position)))
             (scan-attribute-value ()
               (if (and (< position length)
                        (member (char content position)
                                (list (code-char 34) (code-char 39))
                                :test #'char=))
                   (let* ((quote (char content position))
                          (start (incf position)))
                     (loop while (and (< position length)
                                      (not (char= (char content position) quote)))
                           do (incf position))
                     (when (>= position length)
                       (error "Unterminated HTML attribute value"))
                     (prog1 (subseq content start position)
                       (incf position)))
                   (let ((start position))
                     (loop while (and (< position length)
                                      (not (export-html-test-space-p
                                            (char content position))))
                           do (incf position))
                     (subseq content start position)))))
      (skip-space)
      (let ((tag (string-downcase (scan-name)))
            (attributes nil))
        (unless (plusp (length tag))
          (error "Empty HTML tag"))
        (loop
          (skip-space)
          (when (>= position length)
            (return))
          (let ((name (string-downcase (scan-name))))
            (unless (plusp (length name))
              (error "Malformed HTML attribute list in ~S at offset ~D"
                     content position))
            (skip-space)
            (push (cons name
                        (if (and (< position length)
                                 (char= (char content position) #\=))
                            (progn
                              (incf position)
                              (skip-space)
                              (scan-attribute-value))
                            ""))
                  attributes)))
        (values tag (nreverse attributes) self-closing-p)))))

(defun export-html-test-add-child (parent child)
  (push child (export-html-test-node-children parent)))

(defun export-html-test-parse-document (html)
  (let* ((text (export-html-test-normalize-newlines html))
         (root (make-export-html-test-node :type :document))
         (stack (list root))
         (position 0)
         (length (length text)))
    (labels ((add-text (start end)
               (when (< start end)
                 (export-html-test-add-child
                  (car stack)
                  (make-export-html-test-node
                   :type :text :value (subseq text start end)))))
             (add-markup (kind start end)
               (export-html-test-add-child
                (car stack)
                (make-export-html-test-node
                 :type kind :value (subseq text start end))))
             (close-tag (tag-start tag-end)
               (let* ((name (string-downcase
                             (string-trim
                              '(#\Space #\Tab #\Newline #\Return)
                              (subseq text (+ tag-start 2) tag-end))))
                      (open (car stack)))
                 (unless (and open
                              (eq (export-html-test-node-type open) :element)
                              (string= name (export-html-test-node-value open)))
                   (error "Unexpected HTML closing tag: ~A" name))
                 (pop stack)))
             (finish (node)
               (setf (export-html-test-node-children node)
                     (nreverse (export-html-test-node-children node)))
               (dolist (child (export-html-test-node-children node))
                 (when (member (export-html-test-node-type child)
                               '(:document :element) :test #'eq)
                   (finish child)))))
      (loop while (< position length)
            for open = (position #\< text :start position)
            do (if (null open)
                   (progn
                     (add-text position length)
                     (setf position length))
                   (progn
                     (add-text position open)
                     (cond
                       ((and (<= (+ open 4) length)
                             (string= "<!--" text :start2 open :end2 (+ open 4)))
                        (let ((end (search "-->" text :start2 (+ open 4))))
                          (unless end (error "Unterminated HTML comment"))
                          (add-markup :comment open (+ end 3))
                          (setf position (+ end 3))))
                       ((and (< (1+ open) length)
                             (char= (char text (1+ open)) #\?))
                        (let ((end (search "?>" text :start2 (+ open 2))))
                          (unless end (error "Unterminated HTML declaration"))
                          (add-markup :markup open (+ end 2))
                          (setf position (+ end 2))))
                       (t
                        (let ((end (export-html-test-find-tag-end text (1+ open))))
                          (unless end (error "Unterminated HTML tag"))
                          (cond
                            ((and (< (+ open 1) end)
                                  (char= (char text (1+ open)) #\/))
                             (close-tag open end))
                            ((and (< (+ open 1) end)
                                  (char= (char text (1+ open)) #\!))
                             (add-markup :markup open (1+ end)))
                            (t
                             (multiple-value-bind (tag attributes self-closing-p)
                                 (export-html-test-parse-start-tag
                                  text (1+ open) end)
                               (let ((node
                                       (make-export-html-test-node
                                        :type :element :value tag
                                        :attributes attributes)))
                                 (export-html-test-add-child (car stack) node)
                                 (unless self-closing-p
                                   (push node stack))))))
                          (setf position (1+ end))))))))
      (unless (and (= (length stack) 1) (eq (car stack) root))
        (error "Unclosed HTML element"))
      (finish root)
      root)))

(defun export-html-test-generated-reference-p (value)
  (and (stringp value)
       (= (length value) 10)
       (string-equal value "org" :end1 3 :end2 3)
       (every (lambda (character) (digit-char-p character 16))
              (subseq value 3))))

(defun export-html-test-explicit-ids (document)
  (let ((ids (make-hash-table :test #'equal)))
    (labels ((remember (value)
               (when (stringp value)
                 (setf (gethash value ids) t))))
      (map-nodes
       (lambda (node)
         (when (member (node-type node) '(:headline :inlinetask) :test #'eq)
           (multiple-value-bind (value found-p)
               (effective-property document node "CUSTOM_ID")
             (when found-p (remember value))))
         (remember (node-property node :NAME)))
       (document-root document)))
    ids))

(defun export-html-test-generated-references (tree explicit-ids)
  (let ((references (make-hash-table :test #'equal)))
    (labels ((visit (node)
               (when (eq (export-html-test-node-type node) :element)
                 (let ((id (cdr (assoc "id"
                                       (export-html-test-node-attributes node)
                                       :test #'string=))))
                   (when (and id
                              (export-html-test-generated-reference-p id)
                              (not (gethash id explicit-ids))
                              (not (gethash id references)))
                     (setf (gethash id references)
                           (format nil "generated-reference-~D"
                                   (1+ (hash-table-count references)))))))
               (dolist (child (export-html-test-node-children node))
                 (visit child))))
      (visit tree))
    references))

(defun export-html-test-reference-value (value references)
  (gethash value references))

(defun export-html-test-rewrite-id (value references)
  (or (export-html-test-reference-value value references)
      (loop for prefix in '("outline-container-" "text-")
            for suffix = (and (> (length value) (length prefix))
                              (string= prefix value :end2 (length prefix))
                              (subseq value (length prefix)))
            when suffix
              do (let ((replacement
                         (export-html-test-reference-value suffix references)))
                   (when replacement
                     (return (concatenate 'string prefix replacement)))))))

(defun export-html-test-normalize-tree (tree explicit-ids)
  (let ((references
          (export-html-test-generated-references tree explicit-ids)))
    (labels ((rewrite-attribute (name value)
               (cond
                 ((string= name "id")
                  (or (export-html-test-rewrite-id value references) value))
                 ((string= name "name")
                  (or (export-html-test-rewrite-id value references) value))
                 ((and (string= name "href")
                       (> (length value) 1)
                       (char= (char value 0) #\#))
                  (let ((replacement
                          (export-html-test-rewrite-id
                           (subseq value 1) references)))
                    (if replacement
                        (concatenate 'string "#" replacement)
                        value)))
                 (t value)))
             (normalize-children (node)
               (let* ((children (export-html-test-node-children node))
                      (parent-tag (export-html-test-node-value node))
                      (preserve-whitespace-p
                        (and (stringp parent-tag)
                             (member parent-tag '("pre" "code")
                                     :test #'string=))))
                 (loop with previous = nil
                       for remaining on children
                       for child = (car remaining)
                       for next = (cadr remaining)
                       unless (and (not preserve-whitespace-p)
                                   (eq (export-html-test-node-type child) :text)
                                   (export-html-test-whitespace-only-p
                                    (export-html-test-node-value child))
                                   previous next
                                   (eq (export-html-test-node-type previous) :element)
                                   (eq (export-html-test-node-type next) :element))
                         collect (normalize child)
                       do (setf previous child))))
             (normalize (node)
               (case (export-html-test-node-type node)
                 (:element
                  (list :element
                        (export-html-test-node-value node)
                        (stable-sort
                         (mapcar (lambda (attribute)
                                   (cons (car attribute)
                                         (rewrite-attribute
                                          (car attribute) (cdr attribute))))
                                 (copy-list
                                  (export-html-test-node-attributes node)))
                         #'string< :key #'car)
                        (normalize-children node)))
                 (:document (list :document (normalize-children node)))
                 (:text (list :text (export-html-test-node-value node)))
                 (:markup (list :markup (export-html-test-node-value node)))
                 (:comment (list :comment (export-html-test-node-value node)))
                 (otherwise (error "Unknown HTML test node type")))))
      (normalize tree))))

(defun export-html-test-normalize (document html)
  (export-html-test-normalize-tree
   (export-html-test-parse-document html)
   (export-html-test-explicit-ids document)))

(defun export-html-test-catch-error (function)
  (handler-case (progn (funcall function) nil)
    (error (condition) condition)))

(defun export-html-test-fixture-source (relative-path)
  (export-html-test-read-file relative-path))

(defun export-html-test-expect-fixture (relative-source relative-expected
                                        &key allow-raw-html)
  (let* ((source (parse-org (export-html-test-fixture-source relative-source)))
         (actual (export-org source :html :allow-raw-html allow-raw-html
                             :include-resolver
                             (lambda (path parent-source)
                               (declare (ignore parent-source))
                               (when (string= path "included/fragment.org")
                                 (export-html-test-read-file
                                  "t/fixtures/export/oracle/corpus/included/fragment.org")))))
         (expected (export-html-test-read-file relative-expected)))
    (expect (export-html-test-normalize source actual)
            :to-equal (export-html-test-normalize source expected))))

(defun export-html-test-attributed-node (document)
  (let ((result nil))
    (map-nodes
     (lambda (node)
       (when (node-property node :ATTR_HTML)
         (setf result node)))
     (document-root document))
    result))

(defun export-html-test-node-of-type (document type)
  (let ((result nil))
    (map-nodes
     (lambda (node)
       (when (eq (node-type node) type)
         (setf result node)))
     (document-root document))
    result))

(defun export-html-test-assert-attribute-condition (document condition)
  (let ((node (export-html-test-attributed-node document)))
    (with-soft-assertions
      (expect (typep condition 'org-export-unsupported-syntax) :to-be t)
      (when (typep condition 'org-export-unsupported-syntax)
        (expect (org-kit-error-message condition) :to-be-truthy)
        (expect (org-export-unsupported-syntax-backend condition) :to-be :html)
        (expect (org-export-unsupported-syntax-feature condition)
                :to-be :html-attribute)
        (expect (org-export-unsupported-syntax-position condition)
                :to-be (or (node-begin node) 0))
        (expect (org-export-unsupported-syntax-source condition)
                :to-equal (org-kit::export-bounded-source node))))))

(describe "E2 HTML export"
  (it "matches every standard Org HTML compatibility fixture"
    (dolist (name '("metadata-selection" "exclude-tags-override"
                    "footnotes-macros-links" "include" "source-blocks"
                    "markup-lists-blocks-tables-links" "html-renderer-branches"
                    "html-numbering-enabled"))
      (export-html-test-expect-fixture
       (format nil "t/fixtures/export/oracle/corpus/~A.org" name)
       (format nil "t/fixtures/export/oracle/html/~A.html" name))))

  (it "matches XSS, URL, and allowed image attribute policy fixtures"
    (dolist (name '("xss-text" "unsafe-urls" "allowed-attributes"))
      (export-html-test-expect-fixture
       (format nil "t/fixtures/export/policy/html/~A.org" name)
       (format nil "t/fixtures/export/policy/html/~A.html" name))))

  (it "escapes raw HTML by default and allows only HTML-matched constructs"
    (export-html-test-expect-fixture
     "t/fixtures/export/policy/html/raw-html.org"
     "t/fixtures/export/policy/html/raw-html.html")
    (let* ((document
             (parse-org
              (export-html-test-fixture-source
               "t/fixtures/export/policy/html/raw-html.org")))
           (actual (export-org document :html :allow-raw-html t))
           (expected
             (export-html-test-read-file
              "t/fixtures/export/policy/html/raw-html.raw.html")))
      (expect actual :to-equal expected))
    (let* ((source
             (parse-org
              (format nil "#+BEGIN_EXPORT html~%<b>matched block</b>~%#+END_EXPORT~%~%#+BEGIN_EXPORT latex~%<script>mismatch</script>~%#+END_EXPORT~%~%@@html:<i>matched snippet</i>@@ @@latex:mismatch@@")))
           (output (export-org source :html :allow-raw-html t)))
      (with-soft-assertions
        (expect (search "<b>matched block</b>" output) :not :to-be nil)
        (expect (search "<i>matched snippet</i>" output) :not :to-be nil)
        (expect (search "<script>mismatch</script>" output) :to-be nil)
        (expect (search "@@latex:mismatch@@" output) :to-be nil))))

  (it "rejects event, style, unknown, and ambiguous image attributes with full conditions"
    (dolist (name '("disallowed-event" "disallowed-style" "disallowed-unknown"))
      (let* ((relative (format nil "t/fixtures/export/policy/html/~A.org" name))
             (document (parse-org (export-html-test-fixture-source relative)))
             (condition
               (export-html-test-catch-error
                (lambda () (export-org document :html))))
             (error-fixture
               (export-html-test-read-file
                (format nil "t/fixtures/export/policy/html/~A.error" name))))
        (with-soft-assertions
          (expect (search "feature=:html-attribute" error-fixture)
                  :not :to-be nil))
        (export-html-test-assert-attribute-condition document condition)))
    (let* ((source
             (parse-org
              (format nil "#+ATTR_HTML: :alt unsafe~%[[file:first.png]] and [[file:second.png]]")))
           (condition
             (export-html-test-catch-error
              (lambda () (export-org source :html)))))
      (export-html-test-assert-attribute-condition source condition))
    (let* ((source
             (parse-org
              (format nil "#+ATTR_HTML: :alt unsafe~%[[file:first.png]][[file:second.png]]")))
           (condition
             (export-html-test-catch-error
              (lambda () (export-org source :html)))))
      (export-html-test-assert-attribute-condition source condition)))

  (it "escapes allowed image attributes and rejects malformed values"
    (let* ((source
             (parse-org
              (format nil "#+ATTR_HTML: :title \"<tag>&\"~%[[file:image.png]]")))
           (output (export-org source :html)))
      (expect (search "title=\"&lt;tag&gt;&amp;\"" output) :not :to-be nil))
    (dolist (source-text
             (list (format nil "#+ATTR_HTML: :width 12px~%[[file:image.png]]")
                   (format nil "#+ATTR_HTML: :title \"unfinished~%[[file:image.png]]")
                   (format nil "#+ATTR_HTML: :width \"\"~%[[file:image.png]]")
                   (format nil "#+ATTR_HTML: : image~%[[file:image.png]]")
                   (format nil "#+ATTR_HTML: :class one :class two~%[[file:image.png]]")
                   (format nil "#+ATTR_HTML: class value~%[[file:image.png]]")
                   (format nil "#+ATTR_HTML: :class~%[[file:image.png]]")))
      (let* ((document (parse-org source-text))
             (condition
               (export-html-test-catch-error
                (lambda () (export-org document :html)))))
        (export-html-test-assert-attribute-condition document condition))))
    (let* ((document
             (parse-org
              (format nil "#+ATTR_HTML: :alt safe~%[[file:image.png]]")))
           (node (export-html-test-attributed-node document)))
      (setf (node-property node :ATTR_HTML) 42)
      (let ((condition
              (export-html-test-catch-error
               (lambda () (export-org document :html)))))
        (export-html-test-assert-attribute-condition document condition)))

  (it "rejects diary sexps with an unsupported syntax condition"
    (let* ((document (parse-org (format nil "%%(org-calendar-holiday)~%")))
           (condition
             (export-html-test-catch-error
              (lambda () (export-org document :html)))))
      (with-soft-assertions
        (expect (typep condition 'org-export-unsupported-syntax) :to-be t)
        (when (typep condition 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-backend condition)
                  :to-be :html)
          (expect (org-export-unsupported-syntax-feature condition)
                  :to-be :diary-sexp)
          (expect (org-export-unsupported-syntax-position condition)
                  :to-be-truthy)))))

  (it "renders anonymous inline footnotes at their source position"
    (let* ((document (parse-org "Before [fn::inline <&>] after."))
           (output (export-org document :html))
           (before (search "Before " output))
           (content (search "inline &lt;&amp;&gt;" output))
           (after (search " after." output)))
      (with-soft-assertions
        (expect (and before content after
                     (< before content)
                     (< content after))
                :to-be t)
        (expect (search "[fn::" output) :to-be nil)
        (expect (search "id=\"footnotes\"" output) :to-be nil))))

  (it "signals unresolved errors for unnumbered footnote references"
    (let* ((document
             (parse-org
              (format nil "Reference [fn:note].~%~%[fn:note] Definition.")))
           (preparation (org-kit::export-prepare document :html))
           (state (org-kit::%make-export-html-state
                   :preparation preparation
                   :limits (document-limits document)))
           (reference
             (export-html-test-node-of-type document :footnote-reference))
           (condition
             (export-html-test-catch-error
              (lambda ()
                (org-kit::export-html-footnote-reference
                 state reference nil nil)))))
      (with-soft-assertions
        (expect (typep condition 'org-export-error) :to-be t)
        (when (typep condition 'org-export-error)
          (expect (org-export-error-reason condition)
                  :to-be :unresolved-footnote)))))

  (it "signals an unresolved error for a missing footnote definition"
    (let* ((document (parse-org "Text[fn:missing]."))
           (condition
             (export-html-test-catch-error
              (lambda () (export-org document :html)))))
      (with-soft-assertions
        (expect (typep condition 'org-export-error) :to-be t)
        (when (typep condition 'org-export-error)
          (expect (org-export-error-backend condition) :to-be :html)
          (expect (org-export-error-reason condition)
                  :to-be :unresolved-footnote)))))

  (it "collision-resolves generated footnote IDs against explicit IDs"
    (let* ((document
             (parse-org
              (format nil
                      "#+ATTR_HTML: :id fn.note~%[[file:image.png]]~%~%* Explicit reference~%:PROPERTIES:~%:CUSTOM_ID: fnr.note~%:END:~%A reference[fn:note].~%~%[fn:note] Definition.")))
           (output (export-org document :html)))
      (with-soft-assertions
        (expect (search "id=\"fn.note\"" output) :not :to-be nil)
        (expect (search "id=\"fn.note.1\"" output) :not :to-be nil)
        (expect (search "href=\"#fn.note.1\"" output) :not :to-be nil)
        (expect (search "id=\"fnr.note\"" output) :not :to-be nil)
        (expect (search "id=\"fnr.note.1\"" output) :not :to-be nil)
        (expect (search "href=\"#fnr.note.1\"" output) :not :to-be nil))))

  (it "escapes literal LaTeX fragments and environments"
    (let* ((document
             (parse-org
              (format nil "$x < y$~%~%\\begin{equation}~%x < y~%\\end{equation}")))
           (environment
             (export-html-test-node-of-type document :latex-environment)))
      (setf (node-property environment :value) nil)
      (let ((output (export-org document :html)))
        (with-soft-assertions
          (expect (search "x &lt; y" output) :not :to-be nil)
          (expect (search "x < y" output) :to-be nil)))))

  (it "renders timestamps as time elements with ISO dates"
    (let* ((document (parse-org "Date <2026-10-14 Wed>"))
           (output (export-org document :html)))
      (with-soft-assertions
        (expect (search
                 "<time class=\"timestamp\" datetime=\"2026-10-14\">&lt;2026-10-14 Wed&gt;</time>"
                 output)
                :not :to-be nil)
        (expect (search "timestamp-wrapper" output) :to-be nil))))

  (it "keeps explicit IDs exact while normalizing generated Org references"
    (let* ((document
             (parse-org
              (format nil "* Explicit~%:PROPERTIES:~%:CUSTOM_ID: org00000aa~%:END:~%#+NAME: org00000bb~%#+BEGIN_EXAMPLE~%named example~%#+END_EXAMPLE~%<<org00000cc>>")))
           (output (export-org document :html))
           (normalized (export-html-test-normalize document output)))
      (with-soft-assertions
        (expect (search "id=\"org00000aa\"" output) :not :to-be nil)
        (expect (search "id=\"org00000bb\"" output) :not :to-be nil)
        (expect (search "id=\"org00000cc\"" output) :to-be nil)
        (expect (search "org00000aa" (prin1-to-string normalized))
                :not :to-be nil)
        (expect (search "org00000bb" (prin1-to-string normalized))
                :not :to-be nil)
        (expect (search "generated-reference-" (prin1-to-string normalized))
                :not :to-be nil))))

  (it "reserves repeated explicit IDs using the next available suffix"
    (let ((state (org-kit::%make-export-html-state)))
      (setf (gethash "topic.1"
                     (org-kit::export-html-state-used-references state))
            t)
      (expect (loop repeat 3
                    collect (org-kit::export-html-reserve-reference state "topic"))
              :to-equal '("topic" "topic.2" "topic.3"))))

  (it "validates links through export-org after public node updates"
    (let* ((raw-control (format nil "https://example.test~C" (code-char 1)))
           (expanded-control-type (format nil "https~C" (code-char 1)))
           (leading-nbsp
             (concatenate 'string (string (code-char #xa0))
                          "https://example.test"))
           (cases
             `(("fuzzy" "relative/path" "relative/path" t)
               ("fuzzy" "x" "x" t)
               ("fuzzy" "#section" "#section" t)
               ("fuzzy" "/root/path" "/root/path" t)
               ("https" "//example.test" "https://example.test" t)
               ("HTTPS" "//example.test" "HTTPS://example.test" t)
               ("MaIlTo" "user@example.test" "MaIlTo:user@example.test" t)
               ("fuzzy" "javascript:alert(1)" "javascript:alert(1)" nil)
               ("fuzzy" "data:text/html,x" "data:text/html,x" nil)
               ("fuzzy" "file:secret" "file:secret" nil)
               ("fuzzy" "ftp://example.test" "ftp://example.test" nil)
               ("fuzzy" "proto2+test.x-y:payload"
                "proto2+test.x-y:payload" nil)
               ("fuzzy" "bad_scheme:payload" "bad_scheme:payload" t)
               ("fuzzy" ":payload" ":payload" t)
               ("fuzzy" "1scheme:payload" "1scheme:payload" t)
               ("fuzzy" ,(concatenate 'string " " "https://example.test")
                ,(concatenate 'string " " "https://example.test") nil)
               ("fuzzy" ,leading-nbsp ,leading-nbsp nil)
               ("fuzzy" "" "" nil)
               ("fuzzy" ,raw-control ,raw-control nil)
               (,expanded-control-type "example.test"
                ,(concatenate 'string expanded-control-type ":example.test") nil)
               ("fuzzy" "https:\\evil.example" "https:\\evil.example" nil)
               ("fuzzy" "//evil.example" "//evil.example" nil)
               ("fuzzy" nil "" nil)
               ("file" nil "" nil)
               (nil "relative/path" "relative/path" t))))
      (with-soft-assertions
        (dolist (case cases)
          (let* ((document (parse-org "[[relative/path]]"))
                 (link (export-html-test-node-of-type document :link)))
            (setf (node-property link :type) (first case)
                  (node-property link :path) (second case))
            (let ((href (format nil "href=\"~A\"" (third case)))
                  (output (export-org document :html)))
              (expect (not (null (search href output)))
                      :to-be (fourth case))))))))

  (it "rejects backslashes in external and custom-id URLs"
    (let* ((state (org-kit::%make-export-html-state))
           (unsafe-path (concatenate 'string "unsafe" (string #\\) "path"))
           (control-path
             (concatenate 'string "unsafe" (string (code-char 1)) "path"))
           (external-link
             (make-node :link :properties
                        (list :type "https" :path unsafe-path)))
           (custom-id-link
             (make-node :link :properties
                        (list :type "custom-id" :path unsafe-path)))
           (missing-custom-id-link
             (make-node :link :properties '(:type "custom-id")))
           (control-custom-id-link
             (make-node :link :properties
                        (list :type "custom-id" :path control-path)))
           (fuzzy-link
             (make-node :link :properties '(:type "fuzzy" :path "target"))))
      (setf (gethash "target"
                     (org-kit::export-html-state-reference-targets state))
            unsafe-path)
      (with-soft-assertions
        (expect (org-kit::export-html-link-destination state external-link)
                :to-be nil)
        (expect (org-kit::export-html-link-destination state custom-id-link)
                :to-be nil)
        (expect (org-kit::export-html-link-destination state missing-custom-id-link)
                :to-be nil)
        (expect (org-kit::export-html-link-destination state control-custom-id-link)
                :to-be nil)
        (expect (org-kit::export-html-link-destination state fuzzy-link)
                :to-equal "target"))))

  (it "validates destinations after LINK abbreviation expansion"
    (let* ((document
             (parse-org
              (format nil "#+LINK: x javascript:%s~%~%[[x:alert(1)]]~%")))
           (link (export-html-test-node-of-type document :link))
           (output (export-org document :html)))
      (with-soft-assertions
        (expect (node-property link :path) :to-equal "javascript:alert(1)")
        (expect (search "href=\"javascript:alert(1)\"" output) :to-be nil)
        (expect (search "javascript:alert(1)" output) :not :to-be nil))))

  (it "preserves anchors across multiline link descriptions"
    (let* ((document
             (parse-org
              (format nil "[[https://a.test][two~%lines]]")))
           (output (export-org document :html)))
      (expect (search (format nil "<a href=\"https://a.test\">two~%lines</a>")
                      output)
              :not :to-be nil)))

  (it "uses the complete raw link as a label when description is absent"
    (let* ((document (parse-org "[[https://a.test]]"))
           (output (export-org document :html)))
      (with-soft-assertions
        (expect (search "<a href=\"https://a.test\">" output)
                :not :to-be nil)
        (expect (search ">https://a.test</a>" output)
                :not :to-be nil))))

  (it "fails closed for unknown backends and escaping contexts"
    (let* ((backend-error
             (export-html-test-catch-error
              (lambda () (export-org (parse-org "Body") :unknown))))
           (context-error
             (export-html-test-catch-error
              (lambda ()
                (org-kit::export-html-escape-to-output nil :url "")))))
      (with-soft-assertions
        (expect (typep backend-error 'org-export-error) :to-be t)
        (when (typep backend-error 'org-export-error)
          (expect (org-export-error-backend backend-error) :to-be :unknown)
          (expect (org-export-error-reason backend-error) :to-be :unknown-backend)
          (expect (org-export-error-source backend-error) :to-be nil)
          (expect (org-kit-error-message backend-error) :to-be-truthy))
        (expect (typep context-error 'org-export-error) :to-be t)
        (when (typep context-error 'org-export-error)
          (expect (org-export-error-backend context-error) :to-be :html)
          (expect (org-export-error-reason context-error)
                  :to-be :invalid-escape-context)
          (expect (org-export-error-source context-error) :to-be nil)
          (expect (org-kit-error-message context-error) :to-be-truthy)))))

  (it "rejects unrecognized schemes with valid RFC scheme characters"
    (let* ((document
             (parse-org "[[proto2+test.x-y:payload][safe label]]"))
           (output (export-org document :html)))
      (with-soft-assertions
        (expect (search "safe label" output) :not :to-be nil)
        (expect (search "href=\"proto2+test.x-y:payload\"" output)
                :to-be nil))))

  (it "does not serialize coderef links with JavaScript event attributes"
    (let* ((document (parse-org "[[(answer)][safe reference]]"))
           (output (export-org document :html)))
      (with-soft-assertions
        (expect (search "safe reference" output) :not :to-be nil)
        (expect (search "href=\"coderef:answer\"" output) :to-be nil)
        (expect (search "onmouseover=" output) :to-be nil)
        (expect (search "onmouseout=" output) :to-be nil))))

  (it "treats creator:comment like enabled creator output for HTML"
    (let* ((comment-output
             (export-org (parse-org
                          (format nil "#+OPTIONS: creator:comment~%Body"))
                         :html))
           (disabled-output
             (export-org (parse-org
                          (format nil "#+OPTIONS: creator:nil~%Body"))
                         :html)))
      (with-soft-assertions
        (expect (search "<meta name=\"generator\" content=\"Org Mode\" />"
                         comment-output)
                :not :to-be nil)
        (expect (search "<meta name=\"generator\"" disabled-output)
                :to-be nil))))

  (it "renders empty headings, heading options, and inline title objects"
    (let* ((empty-heading
             (export-org (parse-org (format nil "*~%")) :html))
           (raw-title (parse-org (format nil "* Raw title fallback~%")))
           (raw-heading
             (export-html-test-node-of-type raw-title :headline))
           (options
             (export-org
              (parse-org
               (format nil
                       "#+OPTIONS: todo:nil pri:t~%* TODO [#10] Numeric priority~%"))
              :html))
           (enabled-todo
             (export-org
              (parse-org (format nil "#+OPTIONS: todo:t~%* TODO Visible~%"))
              :html))
           (title-objects
             (export-org
              (parse-org
               (format nil
                       "#+MACRO: greet Hello $1~%* {{{greet(world)}}}~%* [[https://example.test][linked title]]~%* Footnote [fn::aside]~%"))
              :html)))
      (setf (node-property raw-heading :title) nil)
      (with-soft-assertions
        (expect (search "<h2" empty-heading) :not :to-be nil)
        (expect (search "Raw title fallback" (export-org raw-title :html))
                :not :to-be nil)
        (expect (search "class=\"priority\">[10]" options)
                :not :to-be nil)
        (expect (search "class=\"todo TODO\"" options) :to-be nil)
        (expect (search "class=\"todo TODO\"" enabled-todo)
                :not :to-be nil)
        (expect (search "Hello world" title-objects) :not :to-be nil)
        (expect (search "href=\"https://example.test\"" title-objects)
                :not :to-be nil)
        (expect (search "Footnote aside" title-objects) :not :to-be nil))))
  (it "suppresses the HTML document heading when title is disabled"
    (let ((output
           (export-org
            (parse-org
             (format nil
                     "#+TITLE: Hidden title~%#+OPTIONS: title:nil~%Body remains.~%"))
            :html)))
      (with-soft-assertions
        (expect (search "<h1 class=\"title\">Hidden title</h1>" output)
                :to-be nil)
        (expect (search "Body remains." output) :not :to-be nil))))

  (it "uses the public export API's default string output"
    (let ((output (export-org (parse-org "default output") :html)))
      (with-soft-assertions
        (expect (typep output 'string) :to-be t)
        (expect (search "default output" output) :not :to-be nil))))

  (it "keeps the first reference alias for duplicate Org targets"
    (let* ((document
             (parse-org
              (format nil
                      "#+OPTIONS: toc:nil num:nil~%<<repeat>> first target~%<<repeat>> second target~%[[repeat]]~%")))
           (output (export-org document :html))
           (first-anchor (search "<a id=\"" output))
           (id-start (and first-anchor (+ first-anchor (length "<a id=\""))))
           (id-end (and id-start (position #\" output :start id-start)))
           (first-id (and id-start id-end (subseq output id-start id-end))))
      (expect (and first-id (search (format nil "href=\"#~A\"" first-id) output))
              :not :to-be nil)))

  (it "resolves ID, custom-ID, fuzzy, and current-file links through the public resolver"
    (let* ((document
             (parse-org
              (format nil
                      "#+OPTIONS: toc:nil num:nil~%* Heading~%:PROPERTIES:~%:ID: stored-id~%:CUSTOM_ID: visible-id~%:END:~%[[id:stored-id]] [[#visible-id]] [[Heading]] [[file:::*Heading]]~%")))
           (output (export-org document :html))
           (href-count 0)
           (search-start 0))
      (loop for position = (search "href=\"#visible-id\"" output
                                   :start2 search-start)
            while position
            do (incf href-count)
               (setf search-start (+ position (length "href=\"#visible-id\""))))
      (expect href-count :to-be 4)))

  (it "builds a link resolution index once for an HTML export"
    (let* ((document
             (parse-org
              (format nil
                      "#+OPTIONS: toc:nil num:nil~%* Heading~%[[Heading]] [[*Heading]] [[file:::*Heading]]")))
           (symbol (find-symbol "MAKE-LINK-RESOLUTION-INDEX" "ORG-KIT"))
           (original (symbol-function symbol))
           (calls 0)
           (output nil))
      (unwind-protect
           (progn
             (setf (symbol-function symbol)
                   (lambda (documents)
                     (incf calls)
                     (funcall original documents)))
             (setf output (export-org document :html)))
        (setf (symbol-function symbol) original))
      (with-soft-assertions
        (expect (typep output 'string) :to-be t)
        (expect calls :to-be 1))))
  (it "omits an HTML reference when its ID index is invalid"
  (let* ((document (parse-org "[[id:target]]"))
         (link (export-html-test-node-of-type document :link))
         (index (org-kit::%make-link-resolution-index nil nil nil nil nil nil nil))
         (state (org-kit::%make-export-html-state
                 :limits (make-limits)
                 :resolution-index index))
         (reference (org-kit::export-html-link-resolution-reference state link)))
    (expect reference :to-be nil)))
  (it "includes the link source document in a lazy HTML resolution index"
  (let* ((document
           (parse-org
            (format nil "* Target~%:PROPERTIES:~%:ID: stored-id~%:END:~%~%[[id:stored-id]]")))
         (link (export-html-test-node-of-type document :link))
         (state (org-kit::%make-export-html-state :limits (make-limits)))
         (index (org-kit::export-html-link-resolution-index state link))
         (resolution (org-kit::resolve-link-with-index link index)))
    (with-soft-assertions
      (expect (org-kit::%link-resolution-index-documents index)
              :to-equal (list document))
      (expect (link-resolution-status resolution) :to-be :resolved)
      (expect (link-resolution-target resolution)
              :to-be (export-html-test-node-of-type document :headline)))))

  (it "exports links whose resolution is unsupported, malformed, or over budget"
    (let ((documents
            (list
             (parse-org
              (format nil "#+LINK: f %(g)~%~%[[f:x]]"))
             (parse-org "[[file:::/[/]]")
             (parse-org
              (format nil
                      "* Empty ID~%:PROPERTIES:~%:ID:~%:END:~%~%[[id:missing]]"))
             (parse-org
              (format nil
                      "#+OPTIONS: toc:nil num:nil~%* Heading~%[[file:::/./]] [[file:::/./]]")
              :limits (make-limits :max-search-work 7)))))
      (dolist (document documents)
        (let ((output nil)
              (condition nil))
          (handler-case
              (setf output (export-org document :html))
            (error (caught)
              (setf condition caught)))
          (with-soft-assertions
            (expect condition :to-be nil)
            (expect (typep output 'string) :to-be t))))))

  (it "renders nested tables of contents and block footnote children"
    (let* ((toc
             (export-org
              (parse-org
               (format nil
                       "#+OPTIONS: toc:3~%* One~%*** Three~%** Two~%"))
              :html))
           (footnotes
             (export-org
              (parse-org
               (format nil
                       "Reference[fn:note].~%~%[fn:note] First paragraph.~%~%  - nested item~%"))
              :html)))
      (with-soft-assertions
        (expect (search "id=\"table-of-contents\"" toc) :not :to-be nil)
        (expect (search "Three" toc) :not :to-be nil)
        (expect (search "<ul" footnotes) :not :to-be nil)
        (expect (search "nested item" footnotes) :not :to-be nil))))

  (it "renders Org block values and timestamps through the public export API"
    (let* ((document
             (parse-org
              (format nil
                      "#+BEGIN_EXAMPLE~%example body~%#+END_EXAMPLE~%~%: fixed body~%~%#+BEGIN_SRC emacs-lisp~%(+ 1 1)~%#+END_SRC~%~%Date <2026-10-14 Wed>~%")))
           (example (export-html-test-node-of-type document :example-block))
           (fixed-width (export-html-test-node-of-type document :fixed-width))
           (source (export-html-test-node-of-type document :src-block))
           (timestamp (export-html-test-node-of-type document :timestamp)))
      (setf (node-property example :value) nil
            (node-property fixed-width :value) nil
            (node-property source :value) nil
            (node-property timestamp :day-start) 32)
      (let ((output (export-org document :html)))
        (with-soft-assertions
          (expect (search "example body" output) :not :to-be nil)
          (expect (search "fixed body" output) :not :to-be nil)
          (expect (search "(+ 1 1)" output) :not :to-be nil)
          (expect (search "&lt;2026-10-14 Wed&gt;" output)
                  :not :to-be nil)))))

  (it "omits source code and results when the HTML serializer mode is none"
    (let* ((document
             (parse-org
              (format nil "#+BEGIN_SRC text~%hidden~%#+END_SRC~%")))
           (source (export-html-test-node-of-type document :src-block))
           (view (org-kit::%make-export-view
                  :node source :payload '(:exports :none)))
           (state (org-kit::%make-export-html-state)))
      (org-kit::export-html-render-source-block state view)
      (expect (coerce (org-kit::export-html-state-output state) 'string)
              :to-equal "")))

  (it "handles empty and unterminated Org block values"
    (let* ((document
             (parse-org
              (format nil
                      "#+BEGIN_EXAMPLE~%#+END_EXAMPLE~%~%: fixed body~%~%#+BEGIN: empty~%#+END:~%~%#+BEGIN: populated~%dynamic body~%#+END:~%~%#+BEGIN_EXPORT html~%#+END_EXPORT~%")))
           (example (export-html-test-node-of-type document :example-block))
           (fixed-width (export-html-test-node-of-type document :fixed-width))
           (output nil))
      (setf (node-property example :value) "example tail"
            (node-property fixed-width :value) "fixed tail")
      (setf output (export-org document :html))
      (with-soft-assertions
        (expect (search "example tail" output) :not :to-be nil)
        (expect (search "fixed tail" output) :not :to-be nil)
        (expect (search "<pre class=\"example\">" output)
                :not :to-be nil)
        (expect (search "dynamic body" output) :not :to-be nil))))

  (it "renders descriptive terms, empty table cells, inline code, entities, and rules"
    (let* ((descriptive (parse-org (format nil "- /Term/ :: Description~%")))
           (table (parse-org (format nil "| |~%")))
           (table-cell (export-html-test-node-of-type table :table-cell))
           (inline (parse-org
                    (format nil "src_emacs-lisp{(+ 1 1)}~%~%-----~%~%\\alpha~%")))
           (entity (export-html-test-node-of-type inline :entity)))
      (setf (node-property table-cell :value) "fallback <&>"
            (node-property entity :html) "<unsafe>")
      (let ((descriptive-output (export-org descriptive :html))
            (table-output (export-org table :html))
            (inline-output (export-org inline :html)))
        (with-soft-assertions
          (expect (search "<dt><i>Term</i></dt>" descriptive-output)
                  :not :to-be nil)
          (expect (search "fallback &lt;&amp;&gt;" table-output)
                  :not :to-be nil)
          (expect (search "(+ 1 1)" inline-output) :not :to-be nil)
          (expect (search "<hr />" inline-output) :not :to-be nil)
          (expect (search "&lt;unsafe&gt;" inline-output) :not :to-be nil)))))

  (it "rejects empty attributes and ATTR_HTML on unsupported nodes"
    (let* ((empty-attributes
             (parse-org
              (format nil
                      "#+ATTR_HTML: :alt safe~%[[file:image.png]]~%")))
           (image (export-html-test-node-of-type empty-attributes :link))
           (unsupported (parse-org (format nil "* Heading~%")))
           (heading (export-html-test-node-of-type unsupported :headline)))
      (setf (node-property image :ATTR_HTML) " "
            (node-property heading :ATTR_HTML) ":id heading")
      (let ((empty-error
              (export-html-test-catch-error
               (lambda () (export-org empty-attributes :html))))
            (unsupported-error
              (export-html-test-catch-error
               (lambda () (export-org unsupported :html)))))
        (with-soft-assertions
          (expect (typep empty-error 'org-export-unsupported-syntax) :to-be t)
          (expect (typep unsupported-error 'org-export-unsupported-syntax)
                  :to-be t)))))

  (it "keeps malformed entity text escaped after a public node update"
    (let* ((document (parse-org (format nil "\\alpha")))
           (entity (export-html-test-node-of-type document :entity)))
      (setf (node-property entity :html) "&<script>x</script>;")
      (let ((output (export-org document :html)))
        (with-soft-assertions
          (expect (search "&amp;&lt;script&gt;x&lt;/script&gt;;" output)
                  :not :to-be nil)
          (expect (search "<script>" output) :to-be nil)))))

  (it "renders a non-paragraph first list child"
    (let* ((document
             (parse-org
              (format nil
                      "- ~%  - nested child~%~%  + another nested child~%")))
           (output (export-org document :html)))
      (with-soft-assertions
        (expect (search "nested child" output) :not :to-be nil)
        (expect (search "another nested child" output) :not :to-be nil))))

  (it "trims list indentation and wraps paragraphs after tables"
    (let* ((indented-list
             (export-org (parse-org (format nil "-   *bold*~%")) :html))
           (unterminated-list
             (export-org (parse-org "- final") :html))
           (table-list
             (export-org
              (parse-org (format nil "- | cell |~%  paragraph~%")) :html)))
      (with-soft-assertions
        (expect (search "<li><b>bold</b></li>" indented-list)
                :not :to-be nil)
        (expect (search "<li>final</li>" unterminated-list)
                :not :to-be nil)
        (expect (search "<table" table-list) :not :to-be nil)
        (expect (search "<p>
paragraph" table-list) :not :to-be nil))))

  (it "separates paragraph text from a nested list child"
    (let* ((document
             (parse-org (format nil "- parent~%  - nested child~%")))
           (output (export-org document :html)))
      (expect (search "<li>parent
<ul class=\"org-ul\">" output) :not :to-be nil)))

  (it "separates a hidden list comment from a nested list child"
    (let* ((document
             (parse-org
              (format nil
                      "- #+BEGIN_COMMENT~%  ignored~%  #+END_COMMENT~%  - nested child~%")))
           (output (export-org document :html)))
      (with-soft-assertions
        (expect (search "<li>
<ul class=\"org-ul\">" output) :not :to-be nil)
        (expect (search "nested child" output) :not :to-be nil))))

  (it "groups resolved include siblings under imported headings"
    (let* ((document
             (parse-org
              (format nil
                      "* Host~%#+INCLUDE: \"heading.org\"~%#+INCLUDE: \"tail.org\"~%")))
           (output
             (export-org
              document :html
              :include-resolver
              (lambda (path parent-source)
                (declare (ignore parent-source))
                (cond ((string= path "heading.org")
                       (format nil
                               "* Imported~%:PROPERTIES:~%:ID: imported~%:END:~%~%Body~%** Nested~%Nested body~%"))
                      ((string= path "tail.org")
                       (format nil "Included tail~%")))))))
      (with-soft-assertions
        (expect (search "Imported" output) :not :to-be nil)
        (expect (search "Nested body" output) :not :to-be nil)
        (expect (search "Included tail" output) :not :to-be nil))))

  (it "renders empty literal objects, rule-only tables, and entities"
    (let* ((literal (export-org (parse-org "==") :html))
           (table (export-org (parse-org (format nil "|---|~%")) :html))
           (entity (export-org (parse-org (format nil "\\vbar~%")) :html)))
      (with-soft-assertions
        (expect (search "<code></code>" literal) :not :to-be nil)
        (expect (search "<table" table) :not :to-be nil)
        (expect (search "<colgroup>" table) :to-be nil)
        (expect (search "|" entity) :not :to-be nil))))

  (it "respects author nil when the document has an author"
    (let ((output
            (export-org
             (parse-org (format nil "#+AUTHOR: A~%#+OPTIONS: author:nil~%Body~%"))
             :html)))
      (expect (search "<meta name=\"author\"" output) :to-be nil)))

  (it "skips empty and invalid target aliases after public node updates"
    (dolist (alias (list "" nil))
      (let* ((document (parse-org (format nil "<<target>>~%[[target]]~%")))
             (target (export-html-test-node-of-type document :target)))
        (setf (node-property target :value) alias)
        (expect (typep (export-org document :html) 'string) :to-be t))))

  (it "does not mutate documents on successful or failed HTML export"
    (let* ((valid (parse-org
                   (format nil "#+ATTR_HTML: :alt safe~%[[file:image.png]]")))
           (invalid (parse-org
                     (format nil "#+ATTR_HTML: :onclick alert(1)~%[[file:image.png]]")))
           (valid-before (export-core-test-document-snapshot valid))
           (invalid-before (export-core-test-document-snapshot invalid))
           (condition
             (export-html-test-catch-error
              (lambda () (export-org invalid :html)))))
      (export-org valid :html)
      (with-soft-assertions
        (expect (export-core-test-document-snapshot valid) :to-equal valid-before)
        (expect (typep condition 'org-export-unsupported-syntax) :to-be t)
        (expect (export-core-test-document-snapshot invalid) :to-equal invalid-before)))))

  (it-sequential "checks HTML escaping before allocating a large unescaped run"
    (let* ((text (make-string (* 256 1024) :initial-element #\x))
           (warmup-state (org-kit::%make-export-html-state
                          :limits (make-limits :max-output-length 0)))
           (state (org-kit::%make-export-html-state
                   :limits (make-limits :max-output-length 0)))
           (before
             (progn
               (export-html-test-catch-error
                (lambda ()
                  (org-kit::export-html-escape-to-output
                   warmup-state :text text)))
               (sb-ext:gc :full t)
               (sb-ext:get-bytes-consed)))
           (condition
             (export-html-test-catch-error
              (lambda ()
                (org-kit::export-html-escape-to-output state :text text))))
           (allocated (- (sb-ext:get-bytes-consed) before)))
      (with-soft-assertions
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when (typep condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name condition)
                  :to-be :max-output-length)
          (expect (org-resource-limit-error-actual condition)
                  :to-be (length text)))
        (expect (< allocated (floor (length text) 2)) :to-be t))))

  (it-sequential "checks dynamic block output before allocating a source slice"
    (let* ((body (make-string (* 256 1024) :initial-element #\x))
           (prefix (format nil "#+OPTIONS: toc:nil num:nil~%#+BEGIN: populated~%"))
           (suffix (format nil "~%#+END:~%"))
           (source (concatenate 'string prefix body suffix))
           (document (make-document source))
           (dynamic-block
             (make-node :dynamic-block
                        :begin 0 :end (length source)
                        :contents-begin (length prefix)
                        :contents-end (+ (length prefix) (length body))))
           (root (make-node :org-data :children (list dynamic-block)))
           (state
             (org-kit::%make-export-html-state
              :limits (make-limits :max-output-length 2048)))
           (attached (setf (document-root document) root))
           (before
             (progn
               (export-html-test-catch-error
                (lambda ()
                  (org-kit::export-html-render-code-block
                   (org-kit::%make-export-html-state
                    :limits (make-limits :max-output-length 2048))
                   dynamic-block)))
               (sb-ext:gc :full t)
               (sb-ext:get-bytes-consed)))
           (condition
             (export-html-test-catch-error
              (lambda ()
                (org-kit::export-html-render-code-block state dynamic-block))))
           (allocated (- (sb-ext:get-bytes-consed) before)))
      (with-soft-assertions
        (expect attached :to-be root)
        (expect (typep dynamic-block 'node) :to-be t)
        (expect (node-contents-begin dynamic-block) :not :to-be nil)
        (expect (node-contents-end dynamic-block) :not :to-be nil)
        (when (and (node-contents-begin dynamic-block)
                   (node-contents-end dynamic-block))
          (expect (>= (- (node-contents-end dynamic-block)
                         (node-contents-begin dynamic-block))
                      (length body))
                  :to-be t))
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when (typep condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name condition)
                  :to-be :max-output-length))
        (expect (< allocated (floor (length body) 2)) :to-be t))))

  (it-sequential "checks HTML link destinations before concatenating them"
    (let* ((path (make-string (* 256 1024) :initial-element #\x))
           (source (concatenate 'string "https://budget.test/" path))
           (limits (make-limits :max-output-length 128))
           (document (make-document source :limits limits))
           (link (make-node :link :begin 0 :end (length source)
                            :properties (list :type "https" :path path)))
           (root (make-node :org-data :children (list link)))
           (attached (setf (document-root document) root))
           (state (org-kit::%make-export-html-state :limits limits))
           (before
             (progn
               (export-html-test-catch-error
                (lambda () (export-org document :html)))
               (export-html-test-catch-error
                (lambda ()
                  (org-kit::export-html-link-destination state link)))
               (sb-ext:gc :full t)
               (sb-ext:get-bytes-consed)))
           (condition
             (export-html-test-catch-error
              (lambda ()
                (org-kit::export-html-link-destination state link))))
           (allocated (- (sb-ext:get-bytes-consed) before)))
      (with-soft-assertions
        (expect attached :to-be root)
        (expect (node-type link) :to-be :link)
        (expect (node-property link :type) :to-equal "https")
        (expect (node-property link :path) :to-equal path)
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when (typep condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name condition)
                  :to-be :max-output-length))
        (expect (> (length source) (* 256 1024)) :to-be t)
        (expect (< allocated (floor (length source) 2)) :to-be t))))

  (it-sequential "checks HTML attribute values before copying them"
    (let* ((body (make-string (* 256 1024) :initial-element #\x))
           (value (concatenate 'string ":id \"" body "\""))
           (node (make-node :table :properties (list :ATTR_HTML value)))
           (state (org-kit::%make-export-html-state
                   :limits (make-limits :max-output-length 128)))
           (before
             (progn
               (export-html-test-catch-error
                (lambda ()
                  (org-kit::export-html-parse-attributes
                   node
                   (org-kit::%make-export-html-state
                    :limits (make-limits :max-output-length 128)))))
               (sb-ext:gc :full t)
               (sb-ext:get-bytes-consed)))
           (condition
             (export-html-test-catch-error
              (lambda () (org-kit::export-html-parse-attributes node state))))
           (allocated (- (sb-ext:get-bytes-consed) before)))
      (with-soft-assertions
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when (typep condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name condition)
                  :to-be :max-output-length))
        (expect (> (length value) (* 256 1024)) :to-be t)
        (expect (< allocated (floor (length value) 2)) :to-be t))))

  (it "handles remaining text and empty HTML escape ranges"
    (let ((state (org-kit::%make-export-html-state :limits (make-limits))))
      (with-soft-assertions
        (expect (org-kit::export-html-escaped-output-length
                 :text (string #\") 0 1)
                :to-equal 1)
        (expect (org-kit::export-html-escaped-output-length
                 :attribute (string #\") 0 1)
                :to-equal 6)
        (expect (org-kit::export-html-escape-to-output-range
                 state :text "" 0 0)
                :to-be state)
        (expect (coerce (org-kit::export-html-state-output state) 'string)
                :to-equal ""))))

  (it "checks escaped HTML parts at their exact output boundary"
    (let* ((text (string #\"))
           (parts (list (list text 0)))
           (state (org-kit::%make-export-html-state
                   :limits (make-limits :max-output-length 6)))
           (below-state (org-kit::%make-export-html-state
                         :limits (make-limits :max-output-length 5)))
           (accepted-length
             (prog1 (org-kit::export-html-check-escaped-parts
                     state :attribute parts)
               (org-kit::export-html-escape-to-output-range
                state :attribute text 0 1)))
           (output (coerce (org-kit::export-html-state-output state) 'string))
           (below
             (export-html-test-catch-error
              (lambda ()
                (org-kit::export-html-check-escaped-parts
                 below-state :attribute parts)))))
      (with-soft-assertions
        (expect accepted-length :to-equal 6)
        (expect output :to-equal "&quot;")
        (expect (typep below 'org-resource-limit-error) :to-be t)
        (when (typep below 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name below)
                  :to-be :max-output-length)
          (expect (org-resource-limit-error-limit below) :to-be 5)
          (expect (org-resource-limit-error-actual below) :to-be 6)))))
  (it "covers legacy HTML attribute validation and compatibility document rendering"
      (let* ((paragraph-document (parse-org (format nil "Paragraph text.~%")))
             (paragraph (export-html-test-node-of-type paragraph-document :paragraph))
             (heading-document (parse-org (format nil "* Heading~%")))
             (heading (export-html-test-node-of-type heading-document :headline))
             (render-document (parse-org (format nil "#+TITLE: Prepared title~%Body text.~%")))
             (paragraph-state nil)
             (heading-state nil)
             (output nil))
        (setf (node-property paragraph :ATTR_HTML) ":class figure"
              (node-property heading :ATTR_HTML) ":id heading"
              paragraph-state
              (org-kit::%make-export-html-state
               :preparation (org-kit::export-prepare paragraph-document :html)
               :limits (document-limits paragraph-document))
              heading-state
              (org-kit::%make-export-html-state
               :preparation (org-kit::export-prepare heading-document :html)
               :limits (document-limits heading-document))
              output
              (org-kit::export-render-preparation
               (org-kit::export-prepare render-document :html) :html))
        (let ((paragraph-error
               (export-html-test-catch-error
                (lambda ()
                  (org-kit::export-html-render-paragraph
                   paragraph-state paragraph nil))))
              (heading-error
               (export-html-test-catch-error
                (lambda ()
                  (org-kit::export-html-render-node-legacy
                   heading-state heading nil nil)))))
          (with-soft-assertions
           (expect
            (typep paragraph-error
                   (find-symbol "ORG-EXPORT-UNSUPPORTED-SYNTAX" "ORG-KIT"))
            :to-be t)
           (expect
            (and paragraph-error
                 (eq (org-export-unsupported-syntax-feature paragraph-error)
                     :html-attribute))
            :to-be t)
           (expect
            (typep heading-error
                   (find-symbol "ORG-EXPORT-UNSUPPORTED-SYNTAX" "ORG-KIT"))
            :to-be t)
           (expect
            (and heading-error
                 (eq (org-export-unsupported-syntax-feature heading-error)
                     :html-attribute))
            :to-be t)
           (expect (search "Prepared title" output) :to-be-truthy)
           (expect (search "Body text." output) :to-be-truthy)
           (expect (search "</html>" output) :to-be-truthy)))))
  (it "suppresses the legacy HTML title when title is disabled"
      (let* ((document
               (parse-org
                (format nil
                        "#+TITLE: Hidden title~%#+OPTIONS: title:nil~%Body remains.~%")))
             (output
               (org-kit::export-render-preparation
                (org-kit::export-prepare document :html) :html)))
        (with-soft-assertions
          (expect (search "<h1 class=\"title\">Hidden title</h1>" output)
                  :to-be nil)
          (expect (search "Body remains." output) :to-be-truthy))))

(describe "HTML export budget regressions"
  (it "enforces exact character output limits through export-org"
    (let* ((source "Body")
           (expected (export-org (parse-org source) :html))
           (length (length expected))
           (below
             (export-html-test-catch-error
              (lambda ()
                (export-org
                 (parse-org source :limits
                            (make-limits :max-output-length (1- length)))
                 :html))))
           (at (export-org (parse-org source :limits
                                      (make-limits :max-output-length length))
                           :html)))
      (with-soft-assertions
        (expect (typep below 'org-resource-limit-error) :to-be t)
        (when (typep below 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name below)
                  :to-be :max-output-length)
          (expect (org-resource-limit-error-limit below) :to-be (1- length))
          (expect (> (org-resource-limit-error-actual below) (1- length))
                  :to-be t))
        (expect at :to-equal expected))))
  (it "uses the filtered body length as the trailer output offset"
    (let* ((source
             (concatenate 'string "Body "
                          (make-string 16384 :initial-element (code-char 120))))
           (backend :fr106-html-filtered-body-offset)
           (registered-p nil)
           (body-filter-input-length 0)
           (expected nil)
           (limit nil)
           (at-limit nil))
      (unwind-protect
           (progn
             (register-derived-export-backend
              backend :html
              :filters
              (list
               (cons :body
                     (list (lambda (value context)
                             (declare (ignore context))
                             (setf body-filter-input-length (length value))
                             "")))))
             (setf registered-p t
                   expected (export-org (parse-org source) backend)
                   limit (- (+ (length expected) body-filter-input-length) 1))
             (setf at-limit
                   (export-org
                    (parse-org
                     source :limits
                     (make-limits
                      :max-output-length limit))
                    backend))
             (with-soft-assertions
               (expect (> body-filter-input-length 4096) :to-be t)
               (expect (plusp (length expected)) :to-be t)
               (expect (search "Body " expected) :to-be nil)
               (expect at-limit :to-equal expected)
               (expect (length at-limit) :to-equal (length expected))))
        (when registered-p
          (unregister-export-backend backend)))))

  (it "exports a programmatic inline code node without source offsets"
    (let* ((document (make-document ""))
           (code (make-node :code))
           (paragraph (make-node :paragraph :children (list code)))
           (root (make-node :org-data :children (list paragraph))))
      (setf (document-root document) root)
      (expect (search "<code></code>" (export-org document :html))
              :to-be-truthy)))

  (it "renders empty and unterminated dynamic source ranges"
    (let* ((nonempty-document (make-document "x"))
           (nonempty-node
             (make-node :dynamic-block :begin 0 :end 1
                        :contents-begin 0 :contents-end 1))
           (nonempty-root (make-node :org-data :children (list nonempty-node)))
           (empty-document (make-document ""))
           (empty-node
             (make-node :dynamic-block :begin 0 :end 0
                        :contents-begin 0 :contents-end 0))
           (empty-root (make-node :org-data :children (list empty-node)))
           (nonempty-state
             (org-kit::%make-export-html-state
              :limits (make-limits :max-output-length 100)))
           (empty-state
             (org-kit::%make-export-html-state
              :limits (make-limits :max-output-length 100)))
           (nonempty-output
             (progn
               (setf (document-root nonempty-document) nonempty-root)
               (org-kit::export-html-render-code-block
                nonempty-state nonempty-node)
               (coerce (org-kit::export-html-state-output nonempty-state)
                       'string)))
           (empty-output
             (progn
               (setf (document-root empty-document) empty-root)
               (org-kit::export-html-render-code-block empty-state empty-node)
               (coerce (org-kit::export-html-state-output empty-state)
                       'string))))
      (with-soft-assertions
        (expect nonempty-output
                :to-equal (format nil "<pre class=\"example\">~%x~%</pre>~%"))
        (expect empty-output
                :to-equal (format nil "<pre class=\"example\">~%~%</pre>~%")))))

  (it-sequential "checks HTML output before copying inline code source text"
    (let* ((body (make-string (* 256 1024) :initial-element #\x))
           (source (concatenate 'string "Before ~" body "~ after"))
           (document
             (parse-org source
                        :limits (make-limits :max-output-length 32)))
           (node (export-html-test-node-of-type document :code))
           (view (org-kit::%make-export-view :node node))
           (warmup-state (org-kit::%make-export-html-state
                          :limits (make-limits :max-output-length 32)))
           (state (org-kit::%make-export-html-state
                   :limits (make-limits :max-output-length 32)))
           (input-length (length source))
           (before
             (progn
               (export-html-test-catch-error
                (lambda ()
                  (org-kit::export-html-render-view warmup-state view)))
               (sb-ext:gc :full t)
               (sb-ext:get-bytes-consed)))
           (condition
             (export-html-test-catch-error
              (lambda ()
                (org-kit::export-html-render-view state view))))
           (allocated (- (sb-ext:get-bytes-consed) before)))
      (with-soft-assertions
        (expect (typep node 'node) :to-be t)
        (when (typep node 'node)
          (expect (>= (- (node-end node) (node-begin node))
                      (length body))
                  :to-be t))
        (expect (> input-length (* 256 1024)) :to-be t)
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when (typep condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name condition)
                  :to-be :max-output-length)
          (expect (> (org-resource-limit-error-actual condition) 32)
                  :to-be t))
        (expect (< allocated (floor input-length 2)) :to-be t))))
  (it "dispatches heading titles and descriptive terms through registered transcoders"
      (let ((document
             (parse-org
              (format nil
                      "#+OPTIONS: toc:nil~%* Title *bold*~%~%- /Term/ :: description~%")))
            (backend :fr106-html-title-term)
            (registered-p nil)
            (bold-calls 0)
            (italic-calls 0)
            (output nil))
        (unwind-protect
            (progn
              (register-derived-export-backend
               backend :html
               :transcoders
               (list
                (cons :bold
                      (lambda (node view frame context)
                        (declare (ignore node view frame context))
                        (incf bold-calls)
                        "TITLE-OVERRIDE"))
                (cons :italic
                      (lambda (node view frame context)
                        (declare (ignore node view frame context))
                        (incf italic-calls)
                        "TERM-OVERRIDE"))))
              (setf registered-p t
                    output (export-org document backend))
              (with-soft-assertions
               (expect (search "TITLE-OVERRIDE" output) :not :to-be nil)
               (expect (search "<dt>TERM-OVERRIDE</dt>" output)
                       :not :to-be nil)
               (expect bold-calls :to-be 1)
               (expect italic-calls :to-be 1)))
          (when registered-p
            (unregister-export-backend backend)))))
  (it "dispatches view-less children through registered transcoders"
      (let* ((document (parse-org "*child*"))
             (bold (export-html-test-node-of-type document :bold))
             (child (first (node-children bold)))
             (backend :fr106-html-viewless-child)
             (registered-p nil)
             (output nil))
        (unwind-protect
            (progn
              (register-derived-export-backend
               backend :html
               :template
               (lambda (context preamble body trailer)
                 (declare (ignore context preamble trailer))
                 body)
               :renderer
               (lambda (context)
                 (export-render-node
                  context
                  (export-html-test-node-of-type
                   (export-render-context-document context) :bold)))
               :transcoders
               (list
                (cons (node-type child)
                      (lambda (node view frame context)
                        (declare (ignore node view frame context))
                        "VIEWLESS-CHILD"))))
              (setf registered-p t
                    output (export-org document backend))
              (expect output :to-equal "<b>VIEWLESS-CHILD</b>"))
          (when registered-p
            (unregister-export-backend backend)))))
  (it "dispatches anonymous inline footnote children through the registry"
      (let* ((document (parse-org "Before [fn::*inline*]after."))
             (backend :fr106-html-inline-footnote)
             (registered-p nil)
             (output nil))
        (unwind-protect
            (progn
              (register-derived-export-backend
               backend :html
               :template
               (lambda (context preamble body trailer)
                 (declare (ignore context preamble trailer))
                 body)
               :renderer
               (lambda (context)
                 (export-render-node
                  context
                  (export-html-test-node-of-type
                   (export-render-context-document context) :footnote-reference)
                  :mode :inline))
               :transcoders
               (list
                (cons :bold
                      (lambda (node view frame context)
                        (declare (ignore node view frame context))
                        "INLINE-FOOTNOTE-CHILD"))))
              (setf registered-p t
                    output (export-org document backend))
              (expect output :to-equal "INLINE-FOOTNOTE-CHILD"))
          (when registered-p
            (unregister-export-backend backend)))))
  (it "validates ATTR_HTML before invoking the node wrapper"
      (let* ((invalid-document (parse-org (format nil "* Invalid~%")))
             (invalid-heading
              (export-html-test-node-of-type invalid-document :headline))
             (valid-document (parse-org (format nil "* Valid~%")))
             (backend :fr106-html-wrapper-order)
             (registered-p nil)
             (headline-wrapper-calls 0)
             (invalid-condition nil)
             (valid-output nil))
        (setf (node-property invalid-heading :ATTR_HTML) ":id \"invalid\"")
        (unwind-protect
            (progn
              (register-derived-export-backend
               backend :html
               :node-wrapper
               (lambda (node view frame context render-next)
                 (declare (ignore view frame context))
                 (if (eq (node-type node) :headline)
                     (progn
                       (incf headline-wrapper-calls)
                       (funcall render-next
                                "<heading-wrapper>" "</heading-wrapper>"))
                   (funcall render-next "" ""))))
              (setf registered-p t
                    invalid-condition
                    (export-html-test-catch-error
                     (lambda () (export-org invalid-document backend))))
              (with-soft-assertions
               (expect (typep invalid-condition 'org-export-unsupported-syntax)
                       :to-be t)
               (when (typep invalid-condition 'org-export-unsupported-syntax)
                 (expect (org-export-unsupported-syntax-feature invalid-condition)
                         :to-be :html-attribute))
               (expect headline-wrapper-calls :to-be 0))
              (setf valid-output (export-org valid-document backend))
              (with-soft-assertions
               (expect (search "<heading-wrapper>" valid-output) :not :to-be nil)
               (expect (search "</heading-wrapper>" valid-output) :not :to-be nil)
               (expect headline-wrapper-calls :to-be 1)))
          (when registered-p
            (unregister-export-backend backend)))))
  (it "dispatches wrapped list paragraphs through the registry"
      (let* ((document
               (parse-org (format nil "- | cell |~%  paragraph~%")))
             (backend :fr106-html-list-paragraph-wrapper)
             (registered-p nil)
             (paragraph-wrapper-calls 0)
             (output nil))
        (unwind-protect
            (progn
              (register-derived-export-backend
               backend :html
               :node-wrapper
               (lambda (node view frame context render-next)
                 (declare (ignore view frame context))
                 (if (eq (node-type node) :paragraph)
                     (progn
                       (incf paragraph-wrapper-calls)
                       (funcall render-next
                                "<list-paragraph-wrapper>"
                                "</list-paragraph-wrapper>"))
                   (funcall render-next "" ""))))
              (setf registered-p t
                    output (export-org document backend))
              (with-soft-assertions
               (expect
                (search
                 (format nil
                         "</table>~%<list-paragraph-wrapper><p>~%paragraph")
                 output)
                :not :to-be nil)
               (expect
                (search
                 (format nil "</p>~%</list-paragraph-wrapper></li>")
                 output)
                :not :to-be nil)
               (expect paragraph-wrapper-calls :to-be 1)))
          (when registered-p
            (unregister-export-backend backend)))))
  (it "dispatches standalone image paragraphs through the registry"
      (let* ((document
               (parse-org
                (format nil "#+ATTR_HTML: :title \"<tag>&\"~%[[file:image.png]]")))
             (backend :fr106-html-image-wrapper)
             (registered-p nil)
             (link-wrapper-calls 0)
             (output nil))
        (unwind-protect
            (progn
              (register-derived-export-backend
               backend :html
               :node-wrapper
               (lambda (node view frame context render-next)
                 (declare (ignore view frame context))
                 (if (eq (node-type node) :link)
                     (progn
                       (incf link-wrapper-calls)
                       (funcall render-next
                                "<image-wrapper>" "</image-wrapper>"))
                   (funcall render-next "" ""))))
              (setf registered-p t
                    output (export-org document backend))
              (with-soft-assertions
               (expect (search
                        (format nil "class=\"figure\">~%<p><image-wrapper>")
                        output)
                       :not :to-be nil)
               (expect (search "title=\"&lt;tag&gt;&amp;\"" output)
                       :not :to-be nil)
               (expect (search
                        (format nil "</image-wrapper>~%</p>~%</div>")
                        output)
                       :not :to-be nil)
               (expect link-wrapper-calls :to-be 1)))
          (when registered-p
            (unregister-export-backend backend)))))
  (it "renders registered HTML links and images only for safe destinations"
    (let ((backend :html-test-link-results)
          (backend-registered-p nil)
          (image-registered-p nil)
          (link-registered-p nil)
          (output nil))
      (unwind-protect
          (progn
            (register-derived-export-backend backend :html)
            (setf backend-registered-p t)
            (register-export-link-type
             backend "image-scheme"
             (lambda (path description frame context)
               (declare (ignore frame context))
               (make-export-link-result
                :kind :image :destination path :description description)))
            (setf image-registered-p t)
            (register-export-link-type
             backend "link-scheme"
             (lambda (path description frame context)
               (declare (ignore frame context))
               (make-export-link-result
                :kind :link
                :destination (format nil "https://links.example.test/~A" path)
                :description description)))
            (setf link-registered-p t
                  output
                  (export-org
                   (parse-org
                    (format nil
                            "[[image-scheme:https://cdn.example.test/remote.png][Remote image]]~%[[image-scheme:assets/local.png][Local image]]~%[[image-scheme:mailto:images@example.test][Mail image]]~%[[link-scheme:article][Article]]~%"))
                   backend))
            (with-soft-assertions
              (expect (search
                       "<img src=\"https://cdn.example.test/remote.png\" alt=\"Remote image\" />"
                       output)
                      :not :to-be nil)
              (expect (search
                       "<img src=\"assets/local.png\" alt=\"Local image\" />"
                       output)
                      :not :to-be nil)
              (expect (search "src=\"mailto:images@example.test\"" output)
                      :to-be nil)
              (expect (search "Mail image" output) :not :to-be nil)
              (expect (search
                       "href=\"https://links.example.test/article\">Article</a>"
                       output)
                      :not :to-be nil)))
        (when link-registered-p
          (unregister-export-link-type backend "link-scheme"))
        (when image-registered-p
          (unregister-export-link-type backend "image-scheme"))
        (when backend-registered-p
          (unregister-export-backend backend))))))

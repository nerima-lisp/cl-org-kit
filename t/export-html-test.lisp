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
              (error "Malformed HTML attribute list"))
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
         (remember (node-property node :NAME))
         (when (member (node-type node) '(:target :radio-target) :test #'eq)
           (remember (node-property node :value))))
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
                    "markup-lists-blocks-tables-links"))
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
      (export-html-test-assert-attribute-condition source condition)))

  (it "keeps explicit IDs exact while normalizing generated Org references"
    (let* ((document
             (parse-org
              (format nil "* Explicit~%:PROPERTIES:~%:CUSTOM_ID: org00000aa~%:END:~%#+NAME: org00000bb~%#+BEGIN_EXAMPLE~%named example~%#+END_EXAMPLE~%<<org00000cc>>")))
           (output (export-org document :html))
           (normalized (export-html-test-normalize document output)))
      (with-soft-assertions
        (expect (search "id=\"org00000aa\"" output) :not :to-be nil)
        (expect (search "id=\"org00000bb\"" output) :not :to-be nil)
        (expect (search "id=\"org00000cc\"" output) :not :to-be nil)
        (expect (search "org00000aa" (prin1-to-string normalized))
                :not :to-be nil)
        (expect (search "org00000bb" (prin1-to-string normalized))
                :not :to-be nil)
        (expect (search "org00000cc" (prin1-to-string normalized))
                :not :to-be nil))))

  (it "rejects unsafe schemes and accepts allowed explicit schemes case-insensitively"
    (let ((cases
            `(("relative/path" "relative/path" t)
              ("#section" "#section" t)
              ("/root/path" "/root/path" t)
              ("HTTPS://example.test" "HTTPS://example.test" t)
              ("MaIlTo:user@example.test" "MaIlTo:user@example.test" t)
              ("javascript:alert(1)" "javascript:alert(1)" nil)
              ("data:text/html,x" "data:text/html,x" nil)
              ("file:secret" "file:secret" nil)
              ("ftp://example.test" "ftp://example.test" nil)
              (,(concatenate 'string " " "https://example.test")
               ,(concatenate 'string " " "https://example.test") nil)
              (,(concatenate 'string (string (code-char #xa0)) "https://example.test")
               ,(concatenate 'string (string (code-char #xa0)) "https://example.test") nil)
              ("http://example.test" "http://example.test" t))))
      (with-soft-assertions
        (dolist (case cases)
          (expect (org-kit::export-safe-url-p (first case) (second case))
                  :to-be (third case)))
        (expect (org-kit::export-safe-url-p
                 "https://example.test" (format nil "https://example.test~C"
                                                  (code-char 1)))
                :to-be nil)
        (expect (org-kit::export-safe-url-p
                 "https://example.test" "https:\\evil.example")
                :to-be nil)
        (expect (org-kit::export-safe-url-p
                 "//evil.example" "//evil.example")
                :to-be nil))))

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

  (it "preserves all limits slots while replacing the node budget"
    (let* ((limits (make-limits :max-input-length 11 :max-heading-depth 12
                                :max-element-depth 13 :max-object-depth 14
                                :max-node-count 15 :max-output-length 16
                                :max-output-octets 17 :max-agenda-range-days 18
                                :max-agenda-entries 19 :max-future-repeats 20
                                :max-export-expansion-depth 21
                                :max-export-expanded-characters 22
                                :max-export-expanded-nodes 23
                                :max-formula-tokens 24 :max-formula-ast-depth 25
                                :max-formula-precision 26 :max-formula-exponent 27
                                :max-formula-work 28))
           (budget 99)
           (copy (org-kit::export-limits-with-node-budget limits budget)))
      (with-soft-assertions
        (dolist (slot (sb-mop:class-slots (find-class 'limits)))
          (let* ((slot-name (sb-mop:slot-definition-name slot))
                 (accessor
                   (find-symbol
                    (concatenate 'string "%LIMITS-"
                                 (string-upcase (symbol-name slot-name)))
                    "ORG-KIT")))
            (expect (not (null (and accessor (fboundp accessor)))) :to-be t)
            (when (and accessor (fboundp accessor))
              (expect (funcall (symbol-function accessor) copy)
                      :to-be (if (string= (symbol-name slot-name)
                                          "MAX-NODE-COUNT")
                                 budget
                                 (funcall (symbol-function accessor) limits)))))))))

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

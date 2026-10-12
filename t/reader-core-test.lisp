;;;; t/reader-core-test.lisp
(in-package #:cl-org-kit/test)

;;;; t/reader-core-test.lisp
(describe "Org parsing and lossless writing" (it "returns a document with a root section and linked nodes"
              (let* ((source (format nil "First line~%Second line~%"))
                     (document (parse-org source))
                     (root (document-root document))
                     (section (first (node-children root)))
                     (paragraph (first (node-children section))))
                (with-soft-assertions (expect (node-type root) :to-be :org-data)
                                      (expect (node-type section)
                                              :to-be
                                              :section)
                                      (expect (node-type paragraph)
                                              :to-be
                                              :paragraph)
                                      (expect (node-parent section) :to-be root)
                                      (expect (node-parent paragraph)
                                              :to-be
                                              section)
                                      (expect (node-document paragraph)
                                              :to-be
                                              document)
                                      (expect (node-text paragraph)
                                              :to-equal
                                              source))))
          (it
           "preserves CRLF and the missing final newline for string and stream output"
           (let* ((source (format nil "* Heading~C~CBody" #\Return #\Newline))
                  (document (parse-org source)))
             (with-soft-assertions
              (expect (write-org document) :to-equal source)
              (expect
               (with-output-to-string (stream)
                 (write-org document :stream stream :output-type :stream))
               :to-equal
               source))))
          (it
           "parses string, character stream, octet vector, octet stream, and pathname input"
           (let* ((source "A")
                  (source-octets (octets 65)))
             (with-soft-assertions
              (expect (write-org (parse-org source)) :to-equal source)
              (with-input-from-string (stream source)
                (expect (write-org (parse-org stream)) :to-equal source))
              (expect (write-org (parse-org source-octets)) :to-equal source)
              (call-with-octet-file source-octets
                                    (lambda (pathname)
                                      (with-open-file
                                          (stream pathname
                                                  :element-type
                                                  (array-element-type
                                                   source-octets))
                                        (expect (write-org (parse-org stream))
                                                :to-equal
                                                source))
                                      (expect (write-org (parse-org pathname))
                                              :to-equal
                                              source))))))
          (it "restores a UTF-8 BOM when writing decoded octet input"
              (let* ((source (octets 239 187 191 97 13 10 98))
                     (document (parse-org source))
                     (text
                      (format nil
                              "~Ca~C~Cb"
                              (code-char #xFEFF)
                              #\Return
                              #\Newline)))
                (with-soft-assertions
                 (expect (document-bom-p document) :to-be t)
                 (expect (write-org document) :to-equal text)
                 (expect (equalp (write-org document :output-type :octets) source) :to-be t))))
          (it "writes octets to a binary output stream"
              (let* ((source-octets (octets 65))
                     (document (parse-org "A")))
                (uiop:with-temporary-file
                 (:pathname pathname
                            :stream
                            stream
                            :element-type
                            (array-element-type source-octets)
                            :direction
                            :io)
                 (declare (ignore pathname))
                 (with-soft-assertions
                  (expect
                   (write-org document :stream stream :output-type :stream)
                   :to-be
                   stream)
                  (finish-output stream)
                  (file-position stream 0)
                  (let ((output
                         (make-array 1
                                     :element-type
                                     (array-element-type source-octets))))
                    (expect (read-sequence output stream) :to-be 1)
                    (expect (equalp output source-octets) :to-be t))))))
          (it "enforces node and output limits"
              (let ((document (parse-org "plain text")))
                (with-soft-assertions
                 (signals org-resource-limit-error
                          (parse-org "plain text"
                                     :limits
                                     (make-limits :max-node-count 1)))
                 (signals org-resource-limit-error
                          (parse-org "plain text"
                                     :limits
                                     (make-limits :max-element-depth 1)))
                 (signals org-resource-limit-error
                          (write-org document
                                     :limits
                                     (make-limits :max-output-length 2))))))
          (it "attaches ATTR properties and preserves blank-line boundaries"
              (let* ((source
                      (format nil
                              "~%#+NAME: sample~%#+ATTR_HTML: :width 80~%#+begin_example~%literal~%#+end_example~%~%~%"))
                     (document (parse-org source))
                     (section (first (node-children (document-root document))))
                     (block (first (node-children section))))
                (with-soft-assertions
                 (expect (node-type block) :to-be :example-block)
                 (expect (node-property block :name) :to-equal "sample")
                 (expect (node-property block :ATTR_HTML)
                         :to-equal
                         ":width 80")
                 (expect (node-post-blank block) :to-be 2)
                 (expect (node-end block) :to-be (length source))
                 (expect (write-org document) :to-equal source))))
          (it "separates paragraphs at affiliated keywords and blank lines"
              (let* ((affiliated-source
                      (format nil
                              "First paragraph.~%#+NAME: second paragraph~%Second paragraph."))
                     (affiliated-document (parse-org affiliated-source))
                     (affiliated-section
                      (first
                       (node-children
                        (document-root affiliated-document))))
                     (affiliated-paragraphs
                      (node-children affiliated-section))
                     (blank-source
                      (format nil "First paragraph.~%~%Second paragraph."))
                     (blank-document (parse-org blank-source))
                     (blank-section
                      (first (node-children (document-root blank-document))))
                     (blank-paragraphs (node-children blank-section))
                     (orphan-source
                      (format nil "#+NAME: orphan~%~%* Heading"))
                     (orphan-document (parse-org orphan-source))
                     (orphan-children
                      (node-children (document-root orphan-document)))
                     (orphan-section (first orphan-children))
                     (orphan-keyword (first (node-children orphan-section)))
                     (orphan-heading (second orphan-children)))
                (with-soft-assertions
                 (expect (mapcar (lambda (node) (node-type node))
                                 affiliated-paragraphs)
                         :to-equal
                         (list :paragraph :paragraph))
                 (expect (node-property (second affiliated-paragraphs) :name)
                         :to-equal
                         "second paragraph")
                 (expect (mapcar (lambda (node) (node-type node))
                                 blank-paragraphs)
                         :to-equal
                         (list :paragraph :paragraph))
                 (expect (node-end (first blank-paragraphs))
                         :to-be
                         (search "Second paragraph." blank-source))
                 (expect (node-post-blank (first blank-paragraphs))
                         :to-be
                         1)
                 (expect (write-org blank-document) :to-equal blank-source)
                 (expect (node-type orphan-keyword) :to-be :keyword)
                 (expect
                  (list (node-property orphan-keyword :key)
                        (node-property orphan-keyword :value))
                  :to-equal
                  (list "NAME" "orphan"))
                 (expect (node-type orphan-heading) :to-be :headline)
                 (expect (node-property orphan-heading :name) :to-be nil)
                 (expect (write-org orphan-document) :to-equal orphan-source))))
          (it "round-trips UTF-16 and UTF-32 byte order marks"
              (dolist (source
                       (list (octets #xFF #xFE #x41 #x00)
                             (octets #xFE #xFF #x00 #x41)
                             (octets #xFF #xFE #x00 #x00 #x41 #x00 #x00 #x00)
                             (octets #x00 #x00 #xFE #xFF #x00 #x00 #x00 #x41)))
                (expect
                 (equalp (write-org (parse-org source) :output-type :octets)
                         source)
                 :to-be
                 t)))
          (it "signals type errors for invalid limits and a missing stream"
              (let ((document (parse-org "source")))
                (with-soft-assertions
                 (signals type-error (parse-org "source" :limits nil))
                 (signals type-error
                          (write-org document :output-type :stream)))))
          (it "returns no elements for input containing only blank lines"
              (let ((document (parse-org (format nil "~%~%"))))
                (expect (node-children (document-root document))
                        :to-equal
                        nil)))
          (it "rejects internal element spans that violate parser contracts"
              (let* ((document (make-document "x"))
                     (context
                      (org-kit::%make-reader-context
                       :document document
                       :text "x"
                       :limits (make-limits)
                       :settings nil))
                     (node (make-node :paragraph))
                     (original-parser
                      (symbol-function (quote org-kit::reader-call-parser))))
                (unwind-protect
                     (progn
                       (setf (symbol-function
                              (quote org-kit::reader-call-parser))
                             (lambda (&rest arguments)
                               (declare (ignore arguments))
                               (values node 0)))
                       (signals org-kit-error
                         (org-kit::reader-parse-recognized-at context 0 1 1)))
                  (setf (symbol-function
                         (quote org-kit::reader-call-parser))
                        original-parser))))
          (it "rejects invalid object ranges and reports unknown BOM names"
              (let ((document (make-document "x")))
                (with-soft-assertions
                 (signals org-kit-error
                   (org-kit::reader-objects-in-range document :invalid :paragraph))
                 (signals org-kit-error
                   (org-kit::reader-objects-in-range document (cons 0 2) :paragraph))
                 (expect (length (org-kit::reader-output-bom :unknown))
                         :to-be
                         0))))
          (it "advances after a blank line when its line reader does not progress"
              (let* ((source (format nil " ~%x"))
                     (document (make-document source))
                     (context
                      (org-kit::%make-reader-context
                       :document document
                       :text source
                       :limits (make-limits)
                       :settings nil))
                     (original-blank-p
                      (symbol-function (quote org-kit::reader-blank-line-p)))
                     (original-next-line
                      (symbol-function (quote org-kit::reader-next-line)))
                     (blank-calls 0))
                (unwind-protect
                     (progn
                       (setf (symbol-function
                              (quote org-kit::reader-blank-line-p))
                             (lambda (context offset)
                               (declare (ignore context offset))
                               (= (incf blank-calls) 1)))
                       (setf (symbol-function
                              (quote org-kit::reader-next-line))
                             (lambda (context offset)
                               (declare (ignore context))
                               offset))
                       (expect
                        (org-kit::reader-parse-elements
                         context 0 (length source) 1)
                        :to-equal
                        nil))
                  (setf (symbol-function
                         (quote org-kit::reader-blank-line-p))
                        original-blank-p)
                  (setf (symbol-function
                         (quote org-kit::reader-next-line))
                        original-next-line)))))

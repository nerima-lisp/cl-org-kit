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
                      (org-kit::parse-objects document 0 (length source)
                                              :paragraph))
                     (bold (first objects))
                     (children (node-children bold)))
                (with-soft-assertions
                 (expect (mapcar #'node-type objects) :to-equal '(:bold))
                 (expect (mapcar #'node-type children)
                         :to-equal
                         '(:plain-text :italic :plain-text))
                 (expect (node-parent (second children)) :to-be bold)
                 (expect (subseq (document-text document)
                                 (node-begin bold) (node-end bold))
                         :to-equal
                         source)
                 (expect (subseq (document-text document)
                                 (node-begin (second children))
                                 (node-end (second children)))
                         :to-equal
                         "/inner/ "))))
          (it "keeps recognition within the requested range"
              (let* ((document (make-document "x *bold* y"))
                     (objects (org-kit::parse-objects document 2 8 :paragraph))
                     (bold (first objects)))
                (with-soft-assertions
                 (expect (length objects) :to-be 1)
                 (expect (node-type bold) :to-be :bold)
                 (expect (list (node-begin bold) (node-end bold))
                         :to-equal
                         '(2 8))
                 (expect (subseq (document-text document)
                                 (node-begin bold) (node-end bold))
                         :to-equal
                         "*bold*"))))
          (it "leaves disallowed object syntax as plain text"
              (let* ((document (make-document "*bold*"))
                     (objects (org-kit::parse-objects document 0 6 :citation)))
                (with-soft-assertions
                 (expect (mapcar #'node-type objects) :to-equal '(:plain-text))
                 (expect (subseq (document-text document)
                                 (node-begin (first objects))
                                 (node-end (first objects)))
                         :to-equal
                         "*bold*"))))
          (it "enforces object depth and document node-count limits"
              (let* ((depth-document
                       (make-document "*x*"
                                      :limits (make-limits :max-object-depth 1)))
                     (count-document
                       (make-document "*x*"
                                      :limits (make-limits :max-node-count 1))))
                (with-soft-assertions
                 (signals org-resource-limit-error
                   (org-kit::parse-objects depth-document 0 3 :paragraph))
                 (signals org-resource-limit-error
                   (org-kit::parse-objects count-document 0 3 :paragraph)))))
          (it "collects radio targets in document-local state"
              (let ((document (make-document "<<<target>>>")))
                (with-soft-assertions
                 (expect (org-kit::prepare-object-parsing document) :to-be nil)
                 (expect
                  (org-kit::%object-context-radio-targets
                   (org-kit::%document-object-context document))
                  :to-equal
                  '("target")))))
          (it "rejects invalid containers and malformed ranges"
              (let ((document (make-document "text")))
                (with-soft-assertions
                 (signals type-error
                   (org-kit::parse-objects document 0 4 :unknown))
                 (signals org-kit-error
                   (org-kit::parse-objects document 3 2 :paragraph))
                 (signals type-error
                   (org-kit::parse-objects document -1 2 :paragraph)))))
          )

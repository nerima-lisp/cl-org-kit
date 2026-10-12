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
                                      (expect (node-parent object) :to-be nil)))))

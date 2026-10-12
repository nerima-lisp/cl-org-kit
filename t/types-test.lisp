;;;; t/types-test.lisp
(in-package #:cl-org-kit/test)

(describe "node types"
  (it "lists every Org Syntax v2 node type under its category"
    (with-soft-assertions
      (expect (node-types :document) :to-equal '(:org-data))
      (expect (length (node-types :greater-element)) :to-be 13)
      (expect (length (node-types :lesser-element)) :to-be 17)
      (expect (length (node-types :object)) :to-be 25)
      (expect (length (node-types)) :to-be 56)))

  (it "returns a fresh list each time"
    (let ((types (node-types)))
      (setf (first types) :changed)
      (expect (first (node-types)) :to-be :org-data)))

  (it "returns no types for an unknown category"
    (expect (node-types :no-such-category) :to-equal '()))

  (it "classifies known and unknown keywords"
    (with-soft-assertions
      (expect (node-type-category :headline) :to-be :greater-element)
      (expect (node-type-category :src-block) :to-be :lesser-element)
      (expect (node-type-category :timestamp) :to-be :object)
      (expect (node-type-category :no-such-type) :to-be nil)
      (expect (node-type-p :paragraph) :to-be t)
      (expect (node-type-p :no-such-type) :to-be nil)
      (expect (node-type-p "headline") :to-be nil)
      (expect (typep :table-cell 'node-type) :to-be-truthy))))

;;;; t/nodes-test.lisp
(in-package #:cl-org-kit/test)

(defun sample-document ()
  "A document with a hand-built tree: one headline over a section paragraph."
  (let* ((document (make-document (lines "* Tasks" "Body" "")))
         (paragraph (make-node :paragraph :begin 8 :end 13 :contents-begin 8 :contents-end 13))
         (section (make-node :section :begin 8 :end 13 :contents-begin 8 :contents-end 13
                                      :children (list paragraph)))
         (headline (make-node :headline :begin 0 :end 13 :contents-begin 8 :contents-end 13
                                        :properties '(:level 1 :raw-value "Tasks")
                                        :children (list section)))
         (root (make-node :org-data :begin 0 :end 13 :contents-begin 0 :contents-end 13
                                    :children (list headline))))
    (setf (document-root document) root)
    (values document root headline section paragraph)))

(describe "node construction"
  (it "builds a node with offsets, properties and children"
    (let* ((child (make-node :plain-text))
           (node (make-node :bold :begin 1 :end 9 :contents-begin 2 :contents-end 8
                                  :post-affiliated 1 :post-blank 2
                                  :properties '(:k 1) :children (list child))))
      (with-soft-assertions
        (expect (node-p node) :to-be t)
        (expect (node-p :bold) :to-be nil)
        (expect (node-type node) :to-be :bold)
        (expect (list (node-begin node) (node-end node) (node-contents-begin node)
                      (node-contents-end node) (node-post-affiliated node) (node-post-blank node))
                :to-equal '(1 9 2 8 1 2))
        (expect (node-children node) :to-equal (list child))
        (expect (node-parent child) :to-be node)
        (expect (node-parent node) :to-be nil))))

  (it "defaults a program-built node to no offsets and no children"
    (let ((node (make-node :paragraph)))
      (expect (list (node-begin node) (node-end node) (node-contents-begin node)
                    (node-contents-end node) (node-post-affiliated node)
                    (node-post-blank node) (node-children node) (node-properties node))
              :to-equal '(nil nil nil nil nil 0 nil nil))))

  (it "rejects an unknown type and unordered or invalid offsets"
    (with-soft-assertions
      (signals type-error (make-node :no-such-type))
      (signals org-kit-error (make-node :paragraph :begin 5 :end 4))
      (signals org-kit-error (make-node :paragraph :begin 0 :end 9 :contents-begin 6
                                                   :contents-end 5))
      (signals org-kit-error (make-node :paragraph :begin -1))
      (signals org-kit-error (make-node :paragraph :begin "0"))
      (signals type-error (make-node :paragraph :post-blank -1)))))

(describe "node tree editing"
  (it "replaces children and re-parents them"
    (let ((parent (make-node :paragraph))
          (a (make-node :plain-text))
          (b (make-node :bold)))
      (setf (node-children parent) (list a b))
      (with-soft-assertions
        (expect (node-children parent) :to-equal (list a b))
        (expect (node-parent b) :to-be parent))))

  (it "appends a child at the end"
    (let ((parent (make-node :paragraph :children (list (make-node :plain-text))))
          (child (make-node :italic)))
      (with-soft-assertions
        (expect (append-child parent child) :to-be child)
        (expect (car (last (node-children parent))) :to-be child)
        (expect (node-parent child) :to-be parent)))))

(describe "node properties"
  (it "reads present and absent properties with a presence flag"
    (let ((node (make-node :headline :properties '(:level 2 :tags nil))))
      (with-soft-assertions
        (expect (multiple-value-list (node-property node :level)) :to-equal '(2 t))
        (expect (multiple-value-list (node-property node :tags)) :to-equal '(nil t))
        (expect (multiple-value-list (node-property node :priority)) :to-equal '(nil nil))
        (expect (node-property node :priority 65) :to-be 65))))

  (it "does not mistake a value for a key"
    (let ((node (make-node :keyword :properties '(:key :value))))
      (expect (multiple-value-list (node-property node :value)) :to-equal '(nil nil))))

  (it "sets properties and returns fresh plists"
    (let ((node (make-node :headline)))
      (setf (node-property node :level) 3)
      (let ((plist (node-properties node)))
        (setf (second plist) 99)
        (expect (node-property node :level) :to-be 3))))

  (it "copies the PROPERTIES argument"
    (let* ((plist (list :level 1))
           (node (make-node :headline :properties plist)))
      (setf (second plist) 7)
      (expect (node-property node :level) :to-be 1))))

(describe "nodes attached to a document"
  (it "links the root to its document and every node to the root"
    (multiple-value-bind (document root headline section paragraph) (sample-document)
      (declare (ignore section))
      (with-soft-assertions
        (expect (document-root document) :to-be root)
        (expect (node-root paragraph) :to-be root)
        (expect (node-root root) :to-be root)
        (expect (node-document paragraph) :to-be document)
        (expect (node-text headline) :to-equal (lines "* Tasks" "Body" ""))
        (expect (node-text paragraph) :to-equal (lines "Body" "")))))

  (it "derives positions and inclusive line ranges from offsets"
    (multiple-value-bind (document root headline section paragraph) (sample-document)
      (declare (ignore document root section))
      (with-soft-assertions
        (expect (multiple-value-list (node-start-position paragraph)) :to-equal '(2 1))
        (expect (multiple-value-list (node-end-position paragraph)) :to-equal '(3 1))
        (expect (multiple-value-list (node-line-range headline)) :to-equal '(1 2))
        (expect (multiple-value-list (node-line-range paragraph)) :to-equal '(2 2)))))

  (it "gives an empty node a one-line range"
    (let* ((document (make-document "ab"))
           (node (make-node :org-data :begin 0 :end 0)))
      (setf (document-root document) node)
      (expect (multiple-value-list (node-line-range node)) :to-equal '(1 1))))

  (it "returns NIL positions and text for detached or offset-less nodes"
    (let ((detached (make-node :paragraph :begin 0 :end 1))
          (offsetless (make-node :paragraph)))
      (multiple-value-bind (document root) (sample-document)
        (declare (ignore document))
        (append-child root offsetless))
      (with-soft-assertions
        (dolist (node (list detached offsetless))
          (expect (node-text node) :to-be nil)
          (expect (node-start-position node) :to-be nil)
          (expect (node-end-position node) :to-be nil)
          (expect (node-line-range node) :to-be nil))
        (expect (node-document detached) :to-be nil))))

  (it "unlinks a replaced root and accepts NIL"
    (multiple-value-bind (document root) (sample-document)
      (let ((replacement (make-node :org-data)))
        (setf (document-root document) replacement)
        (with-soft-assertions
          (expect (node-document root) :to-be nil)
          (expect (node-document replacement) :to-be document)
          (expect (setf (document-root document) nil) :to-be nil)
          (expect (node-document replacement) :to-be nil))))))

(describe "tree traversal"
  (it "visits nodes in document order"
    (multiple-value-bind (document root) (sample-document)
      (declare (ignore document))
      (let ((types '()))
        (expect (map-nodes (lambda (node) (push (node-type node) types)) root) :to-be nil)
        (expect (nreverse types) :to-equal '(:org-data :headline :section :paragraph))))))

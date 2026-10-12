;;;; src/nodes.lisp
;;;;
;;;; One generic node structure for every element and object. A type keyword
;;;; plus an open property plist lets parsers for different node types be
;;;; written independently without redefining a shared structure.
(in-package #:org-kit)

(defstruct (node (:constructor %make-node) (:conc-name %node-) (:copier nil)
                 (:predicate %node-p))
  "An element or object of an Org document. See docs/src/reference/node-model.md."
  (type :org-data :type keyword :read-only t)
  (parent nil)
  (children '() :type list)
  (begin nil)
  (end nil)
  (contents-begin nil)
  (contents-end nil)
  (post-affiliated nil)
  (post-blank 0 :type (integer 0))
  (properties '() :type list)
  ;; Set on a root node only, by (SETF DOCUMENT-ROOT).
  (document nil))

(defun check-span (begin post-affiliated contents-begin contents-end end)
  "Signal an error unless the non-NIL offsets are ordered
BEGIN <= POST-AFFILIATED <= CONTENTS-BEGIN <= CONTENTS-END <= END."
  (let ((offsets (remove nil (list begin post-affiliated contents-begin contents-end end))))
    (unless (and (every (lambda (offset) (typep offset '(integer 0))) offsets)
                 (apply #'<= 0 offsets))
      (error 'org-kit-error
             :message (format nil "Node offsets are not ordered: begin ~S, post-affiliated ~S, contents ~S..~S, end ~S"
                              begin post-affiliated contents-begin contents-end end)))))

(defun make-node (type &key begin end contents-begin contents-end post-affiliated
                         (post-blank 0) properties children)
  "Return a node of TYPE, one of NODE-TYPES. Offsets are 0-based character
indexes into the document text, with END and CONTENTS-END exclusive; leave them
NIL for a node built by a program rather than parsed. PROPERTIES is a plist of
type-specific values. Each of CHILDREN gets the new node as its parent.
POST-BLANK is a non-negative integer. Signals TYPE-ERROR for an unknown TYPE or
invalid POST-BLANK and ORG-KIT-ERROR for unordered offsets."
  (unless (node-type-p type)
    (error 'type-error :datum type :expected-type 'node-type))
  (unless (typep post-blank '(integer 0))
    (error 'type-error :datum post-blank :expected-type '(integer 0)))
  (check-span begin post-affiliated contents-begin contents-end end)
  (let ((node (%make-node :type type :begin begin :end end
                          :contents-begin contents-begin :contents-end contents-end
                          :post-affiliated post-affiliated :post-blank post-blank
                          :properties (copy-list properties))))
    (setf (node-children node) children)
    node))

(defun node-p (object)
  "Return true when OBJECT is a node."
  (%node-p object))

(defun node-type (node)
  "Return the type keyword of NODE."
  (%node-type node))

(defun node-parent (node)
  "Return the parent of NODE, or NIL for a root."
  (%node-parent node))

(defun node-children (node)
  "Return the children of NODE in document order. Do not modify the list."
  (%node-children node))

(defun (setf node-children) (children node)
  "Make CHILDREN, in order, the children of NODE and set each one's parent."
  (dolist (child children)
    (setf (%node-parent child) node))
  (setf (%node-children node) (copy-list children)))

(defun append-child (parent child)
  "Add CHILD as the last child of PARENT and return CHILD."
  (setf (%node-parent child) parent
        (%node-children parent) (append (%node-children parent) (list child)))
  child)

(defun node-begin (node)
  "Return the 0-based offset where NODE starts, including affiliated keywords, or NIL."
  (%node-begin node))

(defun node-end (node)
  "Return the exclusive offset where NODE ends, including trailing blank lines, or NIL."
  (%node-end node))

(defun node-contents-begin (node)
  "Return the offset where the contents of NODE start, or NIL when it has none."
  (%node-contents-begin node))

(defun node-contents-end (node)
  "Return the exclusive offset where the contents of NODE end, or NIL."
  (%node-contents-end node))

(defun node-post-affiliated (node)
  "Return the offset just after the affiliated keywords of NODE, or NIL."
  (%node-post-affiliated node))

(defun node-post-blank (node)
  "Return the number of blank lines (elements) or spaces (objects) after NODE."
  (%node-post-blank node))

(defun node-property (node key &optional default)
  "Return the type-specific property KEY of NODE, or DEFAULT when it is absent.
A second value is true when the property is present."
  (let ((value (getf (%node-properties node) key '%absent)))
    (if (eq value '%absent)
        (values default nil)
        (values value t))))

(defun (setf node-property) (value node key)
  "Set the type-specific property KEY of NODE to VALUE."
  (setf (getf (%node-properties node) key) value))

(defun node-properties (node)
  "Return a fresh plist of every type-specific property of NODE."
  (copy-list (%node-properties node)))

(defun node-root (node)
  "Return the root of the tree containing NODE."
  (loop for current = node then parent
        for parent = (%node-parent current)
        while parent
        finally (return current)))

(defun node-document (node)
  "Return the DOCUMENT whose root tree contains NODE, or NIL when detached."
  (%node-document (node-root node)))

(defun document-root (document)
  "Return the root node of DOCUMENT, or NIL before parsing."
  (%document-root document))

(defun (setf document-root) (root document)
  "Install ROOT as the parsed tree of DOCUMENT, linking ROOT back to DOCUMENT."
  (let ((previous (%document-root document)))
    (when previous
      (setf (%node-document previous) nil)))
  (when root
    (setf (%node-document root) document))
  (setf (%document-root document) root))

(defun node-text (node)
  "Return the source text NODE spans, or NIL when it is detached or has no offsets."
  (let ((document (node-document node))
        (begin (%node-begin node))
        (end (%node-end node)))
    (when (and document begin end)
      (subseq (%document-text document) begin end))))

(defun node-start-position (node)
  "Return the 1-based line and column where NODE begins, or NIL when unknown."
  (let ((document (node-document node))
        (begin (%node-begin node)))
    (when (and document begin)
      (offset-line-column document begin))))

(defun node-end-position (node)
  "Return the 1-based line and column of the exclusive end of NODE, or NIL."
  (let ((document (node-document node))
        (end (%node-end node)))
    (when (and document end)
      (offset-line-column document end))))

(defun node-line-range (node)
  "Return the first and last 1-based lines NODE occupies, both inclusive, or NIL.
A trailing newline belongs to the line it ends."
  (let ((document (node-document node))
        (begin (%node-begin node))
        (end (%node-end node)))
    (when (and document begin end)
      (values (offset-line-column document begin)
              (offset-line-column document (max begin (1- end)))))))

(defun map-nodes (function node)
  "Call FUNCTION on NODE and then on every descendant, in document order
(pre-order). Return NIL."
  (funcall function node)
  (dolist (child (%node-children node))
    (map-nodes function child))
  nil)

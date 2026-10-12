;;;; src/types.lisp
;;;;
;;;; The closed set of node type keywords. Names follow Emacs org-element so the
;;;; conformance fixtures it generates compare without a renaming table.
(in-package #:org-kit)

(defparameter *node-type-table*
  '((:org-data . :document)
    ;; Greater elements, including headlines and sections.
    (:headline . :greater-element)
    (:section . :greater-element)
    (:inlinetask . :greater-element)
    (:center-block . :greater-element)
    (:quote-block . :greater-element)
    (:special-block . :greater-element)
    (:drawer . :greater-element)
    (:property-drawer . :greater-element)
    (:dynamic-block . :greater-element)
    (:footnote-definition . :greater-element)
    (:plain-list . :greater-element)
    (:item . :greater-element)
    (:table . :greater-element)
    ;; Lesser elements.
    (:babel-call . :lesser-element)
    (:clock . :lesser-element)
    (:comment . :lesser-element)
    (:comment-block . :lesser-element)
    (:diary-sexp . :lesser-element)
    (:example-block . :lesser-element)
    (:export-block . :lesser-element)
    (:fixed-width . :lesser-element)
    (:horizontal-rule . :lesser-element)
    (:keyword . :lesser-element)
    (:latex-environment . :lesser-element)
    (:node-property . :lesser-element)
    (:paragraph . :lesser-element)
    (:planning . :lesser-element)
    (:src-block . :lesser-element)
    (:table-row . :lesser-element)
    (:verse-block . :lesser-element)
    ;; Objects.
    (:bold . :object)
    (:citation . :object)
    (:citation-reference . :object)
    (:code . :object)
    (:entity . :object)
    (:export-snippet . :object)
    (:footnote-reference . :object)
    (:inline-babel-call . :object)
    (:inline-src-block . :object)
    (:italic . :object)
    (:latex-fragment . :object)
    (:line-break . :object)
    (:link . :object)
    (:macro . :object)
    (:plain-text . :object)
    (:radio-target . :object)
    (:statistics-cookie . :object)
    (:strike-through . :object)
    (:subscript . :object)
    (:superscript . :object)
    (:table-cell . :object)
    (:target . :object)
    (:timestamp . :object)
    (:underline . :object)
    (:verbatim . :object))
  "Alist from each node type keyword to its category, in documentation order.")

(defun node-type-category (type)
  "Return the category of node TYPE: :DOCUMENT, :GREATER-ELEMENT,
:LESSER-ELEMENT or :OBJECT. Return NIL when TYPE is not a node type."
  (cdr (assoc type *node-type-table*)))

(defun node-type-p (object)
  "Return true when OBJECT is one of the node type keywords."
  (and (node-type-category object) t))

(deftype node-type ()
  "A keyword naming one of the node types listed by NODE-TYPES."
  '(satisfies node-type-p))

(defun node-types (&optional category)
  "Return a fresh list of node type keywords in documentation order.
With CATEGORY (:DOCUMENT, :GREATER-ELEMENT, :LESSER-ELEMENT or :OBJECT), return
only the types in that category."
  (loop for (type . type-category) in *node-type-table*
        when (or (null category) (eq category type-category))
          collect type))

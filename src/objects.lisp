;;;; src/objects.lisp
;;;;
;;;; Internal object parsing interface shared with the element parser.
(in-package #:org-kit)

(defvar *object-parse-range-start*
  nil)

(defstruct
    (object-match
     (:constructor %make-object-match
                   (&key type
                         end
                         contents-begin
                         contents-end
                         (post-blank 0)
                         properties
                         opaque-p))
     (:copier nil))
  "Recognized object span and properties, before children are constructed."
  type
  end
  contents-begin
  contents-end
  (post-blank 0 :type (integer 0))
  properties
  opaque-p)

(defun prepare-object-parsing (document)
  "Prepare per-document state needed by object parsing."
  (declare (ignore document))
  nil)

(defun parse-objects (document start end container-type)
  "Parse DOCUMENT-TEXT in [START, END) as objects allowed in CONTAINER-TYPE.
Return nodes in source order, with this minimal implementation representing
non-empty input as one :PLAIN-TEXT node."
  (unless (node-type-p container-type)
    (error 'type-error :datum container-type :expected-type 'node-type))
  (check-offset document start)
  (check-offset document end)
  (when (> start end)
    (error 'org-kit-error :message "Object parsing range starts after it ends"))
  (if (= start end)
      nil
      (list (make-node :plain-text :begin start :end end))))

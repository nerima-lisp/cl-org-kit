;;;; src/objects.lisp
;;;;
;;;; Shared object recognition, nesting and resource accounting.
(in-package #:org-kit)

(defvar *object-parse-range-start* nil)
(defvar *object-parse-depth* 0)

(defstruct
    (object-match
     (:constructor %make-object-match
                   (&key type end contents-begin contents-end (post-blank 0)
                         properties opaque-p))
     (:copier nil))
  "Recognized object span and properties, before children are constructed."
  type
  end
  contents-begin
  contents-end
  (post-blank 0 :type (integer 0))
  properties
  opaque-p)

(defstruct (%object-context (:constructor %make-object-context))
  (radio-targets nil :type list)
  (node-count 0 :type (integer 0)))

(defparameter *standard-object-types*
  '(:bold :citation :code :entity :export-snippet :footnote-reference
    :inline-babel-call :inline-src-block :italic :latex-fragment :line-break
    :link :macro :radio-target :statistics-cookie :strike-through :subscript
    :superscript :target :timestamp :underline :verbatim))

(defparameter *object-container-restrictions*
  `((:bold . ,*standard-object-types*)
    (:citation . (:citation-reference))
    (:citation-reference . (:bold :code :entity :export-snippet
                             :inline-babel-call :inline-src-block :italic
                             :latex-fragment :macro :radio-target
                             :statistics-cookie :strike-through :subscript
                             :superscript :target :timestamp :underline
                             :verbatim))
    (:footnote-reference . ,*standard-object-types*)
    (:headline . (:bold :citation :code :entity :export-snippet
                   :footnote-reference :inline-babel-call :inline-src-block
                   :italic :latex-fragment :link :macro :radio-target
                   :statistics-cookie :strike-through :subscript :superscript
                   :target :timestamp :underline :verbatim))
    (:inlinetask . (:bold :citation :code :entity :export-snippet
                    :footnote-reference :inline-babel-call :inline-src-block
                    :italic :latex-fragment :link :macro :radio-target
                    :statistics-cookie :strike-through :subscript :superscript
                    :target :timestamp :underline :verbatim))
    (:item . (:bold :citation :code :entity :export-snippet
             :footnote-reference :inline-babel-call :inline-src-block :italic
             :latex-fragment :link :macro :radio-target :statistics-cookie
             :strike-through :subscript :superscript :target :timestamp
             :underline :verbatim))
    (:keyword . (:bold :citation :code :entity :export-snippet
                 :inline-babel-call :inline-src-block :italic :latex-fragment
                 :link :macro :radio-target :statistics-cookie
                 :strike-through :subscript :superscript :target :timestamp
                 :underline :verbatim))
    (:link . (:bold :code :entity :export-snippet :inline-babel-call
               :inline-src-block :italic :latex-fragment :macro
               :statistics-cookie :strike-through :subscript
               :superscript :underline :verbatim))
    (:paragraph . ,*standard-object-types*)
    (:radio-target . (:bold :code :entity :italic :latex-fragment
                      :strike-through :subscript :superscript :underline
                      :verbatim))
    (:strike-through . ,*standard-object-types*)
    (:subscript . ,*standard-object-types*)
    (:superscript . ,*standard-object-types*)
    (:table-cell . (:bold :citation :code :entity :export-snippet
                    :footnote-reference :italic :latex-fragment :link :macro
                    :radio-target :strike-through :subscript :superscript
                    :target :timestamp :underline :verbatim))
    (:table-row . (:table-cell))
    (:underline . ,*standard-object-types*)
    (:verse-block . ,*standard-object-types*)))

(defun %object-types-for-container (container-type)
  (or (cdr (assoc container-type *object-container-restrictions*))
      *standard-object-types*))

(defun %object-type-allowed-p (type container-type)
  (member type (%object-types-for-container container-type)))

(defun %add-radio-target (target targets)
  (if (and (stringp target) (plusp (length target)))
      (adjoin target targets :test #'string=)
      targets))

(defun %radio-targets-in-text (text targets)
  (loop with position = 0
        for begin = (search "<<<" text :start2 position)
        while begin
        for value-start = (+ begin 3)
        for end = (search ">>>" text :start2 value-start)
        while end
        do (setf targets (%add-radio-target
                          (subseq text value-start end) targets)
                 position (+ end 3))
        finally (return targets)))

(defun prepare-object-parsing (document)
  "Prepare document-local node accounting and radio-target lookup data."
  (check-type document document)
  (let ((context (%make-object-context))
        (root (%document-root document)))
    (when root
      (map-nodes
       (lambda (node)
         (incf (%object-context-node-count context))
         (when (eq (node-type node) :radio-target)
           (setf (%object-context-radio-targets context)
                 (%add-radio-target (node-property node :value)
                                    (%object-context-radio-targets context)))))
       root))
    (setf (%object-context-radio-targets context)
          (%radio-targets-in-text (%document-text document)
                                  (%object-context-radio-targets context)))
    (setf (%document-object-context document) context)
    nil))

(defun %ensure-object-context (document)
  (unless (%document-object-context document)
    (prepare-object-parsing document))
  (%document-object-context document))

(defun %object-parser-functions ()
  '(try-parse-timestamp-object
    try-parse-link-object
    try-parse-other-object
    try-parse-markup-object))

(defun %try-parse-object (document start end container-type)
  (loop for parser-name in (%object-parser-functions)
        for parser = (and (fboundp parser-name) (symbol-function parser-name))
        for match = (when parser
                      (funcall parser document start end container-type))
          when match
          return match))

(defun %valid-object-match-p (match start range-end)
  (and (object-match-p match)
       (node-type-p (object-match-type match))
       (eq (node-type-category (object-match-type match)) :object)
       (not (eq (object-match-type match) :plain-text))
       (integerp (object-match-end match))
       (< start (object-match-end match) (1+ range-end))
       (<= (1+ start) (object-match-end match))
       (integerp (object-match-post-blank match))
       (<= 0 (object-match-post-blank match)
           (- (object-match-end match) start))
       (let ((contents-begin (object-match-contents-begin match))
             (contents-end (object-match-contents-end match)))
         (or (and (null contents-begin) (null contents-end))
             (and (integerp contents-begin)
                  (integerp contents-end)
                  (<= start contents-begin contents-end
                      (object-match-end match)))))))

(defun %account-object-node (document)
  (let* ((context (%ensure-object-context document))
         (limits (%document-limits document))
         (depth (1+ *object-parse-depth*))
         (node-count (1+ (%object-context-node-count context))))
    (check-limit limits :max-object-depth depth)
    (check-limit limits :max-node-count node-count)
    (setf (%object-context-node-count context) node-count)))

(defun %make-plain-text-node (document start end)
  (%account-object-node document)
  (make-node :plain-text :begin start :end end))

(defun %object-match-children (document match)
  (let ((contents-begin (object-match-contents-begin match))
        (contents-end (object-match-contents-end match)))
    (when (and (not (object-match-opaque-p match))
               contents-begin contents-end
               (< contents-begin contents-end))
      (let ((*object-parse-depth* (1+ *object-parse-depth*)))
        (parse-objects document contents-begin contents-end
                       (object-match-type match))))))

(defun %make-object-node (document start match)
  (%account-object-node document)
  (let ((children (%object-match-children document match)))
    (make-node (object-match-type match)
               :begin start
               :end (object-match-end match)
               :contents-begin (object-match-contents-begin match)
               :contents-end (object-match-contents-end match)
               :post-blank (object-match-post-blank match)
               :properties (object-match-properties match)
               :children children)))

(defun parse-objects (document start end container-type)
  "Parse DOCUMENT text in [START, END) as objects allowed in CONTAINER-TYPE.
Return source-ordered object and plain-text nodes with nested children."
  (check-type document document)
  (unless (node-type-p container-type)
    (error 'type-error :datum container-type :expected-type 'node-type))
  (check-offset document start)
  (check-offset document end)
  (when (> start end)
    (error 'org-kit-error :message "Object parsing range starts after it ends"))
  (%ensure-object-context document)
  (let ((result '())
        (position start)
        (plain-start start)
        (*object-parse-range-start* start))
    (labels ((flush-plain (plain-end)
               (when (< plain-start plain-end)
                 (push (%make-plain-text-node document plain-start plain-end)
                       result))))
      (loop while (< position end)
            for match = (%try-parse-object document position end container-type)
            do (if (and match
                        (%valid-object-match-p match position end)
                        (%object-type-allowed-p (object-match-type match)
                                                container-type))
                   (progn
                     (flush-plain position)
                     (push (%make-object-node document position match)
                           result)
                     (setf position (object-match-end match)
                           plain-start position))
                   (incf position)))
      (flush-plain end))
    (nreverse result)))

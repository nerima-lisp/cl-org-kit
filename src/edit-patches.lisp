;;;; src/edit-patches.lisp
;;;;
;;;; Shared source splicing and semantic validation for format-preserving edits.
(in-package #:org-kit)

(defstruct (edit-patch (:constructor %make-edit-patch (start end replacement))
                       (:copier nil))
  "A half-open source range and the text that replaces it."
  start
  end
  replacement)

(defun edit-target-description (target)
  "Return a bounded, escaped description suitable for an edit condition."
  (bounded-text
   (cond ((stringp target) target)
         ((node-p target) (string-downcase (symbol-name (node-type target))))
         ((null target) "target")
         (t (with-output-to-string (stream)
              (let ((*print-circle* t)
                    (*print-length* 16)
                    (*print-level* 4))
                (prin1 target stream)))))))

(defun edit-signal-error (target reason message)
  "Signal an ORG-EDIT-ERROR for TARGET with REASON and MESSAGE."
  (error 'org-edit-error
         :message message
         :target (edit-target-description target)
         :reason reason))

(defun edit-make-patch (start end replacement &optional target)
  "Return a patch replacing [START, END) with REPLACEMENT."
  (unless (and (integerp start) (integerp end) (<= 0 start end)
               (stringp replacement))
    (edit-signal-error target :invalid-value "Invalid source patch"))
  (%make-edit-patch start end replacement))

(defun edit-proper-list-length (value)
  "Return the length of proper VALUE, or NIL when VALUE is dotted or circular."
  (handler-case (list-length value)
    (type-error () nil)))

(defun edit-normalize-patches (patches source-length target)
  "Validate PATCHES against SOURCE-LENGTH and return them sorted by range."
  (unless (edit-proper-list-length patches)
    (edit-signal-error target :invalid-value "Patches must be a proper list"))
  (dolist (patch patches)
    (unless (edit-patch-p patch)
      (edit-signal-error target :invalid-value "Invalid source patch"))
    (unless (and (integerp (edit-patch-start patch))
                 (integerp (edit-patch-end patch))
                 (<= 0 (edit-patch-start patch) (edit-patch-end patch)
                     source-length)
                 (stringp (edit-patch-replacement patch)))
      (edit-signal-error target :invalid-value "Patch range is outside the source")))
  (let ((sorted (sort (copy-list patches)
                      (lambda (left right)
                        (or (< (edit-patch-start left) (edit-patch-start right))
                            (and (= (edit-patch-start left)
                                    (edit-patch-start right))
                                 (< (edit-patch-end left)
                                    (edit-patch-end right))))))))
    (loop for previous = nil then patch
          for patch in sorted
          when (and previous
                    (or (= (edit-patch-start previous)
                           (edit-patch-start patch))
                        (< (edit-patch-start patch)
                               (edit-patch-end previous))))
            do (edit-signal-error target :overlapping-patches
                                  "Source patches overlap"))
    sorted))

(defun edit-splice-source (source patches)
  "Apply already sorted, non-overlapping PATCHES to SOURCE."
  (with-output-to-string (stream)
    (loop with cursor = 0
          for patch in patches
          do (write-string source stream :start cursor
                                       :end (edit-patch-start patch))
             (write-string (edit-patch-replacement patch) stream)
             (setf cursor (edit-patch-end patch))
          finally (write-string source stream :start cursor))))

(defun edit-source-property-p (key)
  "Return true when KEY is a reader-only source-range property."
  (and (keywordp key)
       (let ((name (symbol-name key)))
         (and (>= (length name) 7)
              (string= name "READER-" :end1 7 :end2 7)))))

(defun edit-canonical-value (value)
  "Copy VALUE into a source-offset-free representation."
  (cond ((node-p value) (edit-canonical-node value))
        ((consp value)
         (cons (edit-canonical-value (car value))
               (edit-canonical-value (cdr value))))
        ((and (vectorp value) (not (stringp value)))
         (map 'vector #'edit-canonical-value value))
        (t value)))

(defun edit-canonical-node (node)
  "Return NODE's type, semantic properties, children and trailing blank count."
  (list :type (node-type node)
        :post-blank (node-post-blank node)
        :properties
        (loop for (key value) on (node-properties node) by #'cddr
              unless (edit-source-property-p key)
                append (list key (edit-canonical-value value)))
        :children (mapcar #'edit-canonical-node (node-children node))))

(defun edit-canonical-document (document)
  "Return DOCUMENT's parsed tree without source offsets or reader-only ranges."
  (let ((root (document-root document)))
    (unless root
      (edit-signal-error document :uneditable-structure
                         "The document has no parsed tree"))
    (edit-canonical-node root)))

(defun edit-canonical-copy (canonical-tree)
  "Return a tree copy that an editor can adjust to describe the expected result."
  (copy-tree canonical-tree))

(defun edit-node-path (document node &optional target)
  "Return NODE's zero-based child indexes from DOCUMENT's root."
  (unless (node-p node)
    (edit-signal-error target :wrong-node-type "Target is not an Org node"))
  (unless (eq (node-document node) document)
    (edit-signal-error (or target node) :foreign-node
                       "Node does not belong to the input document"))
  (let ((root (document-root document))
        (current node)
        (path '()))
    (unless root
      (edit-signal-error (or target node) :uneditable-structure
                         "The document has no parsed tree"))
    (loop until (eq current root)
          for parent = (node-parent current)
          do (unless parent
               (edit-signal-error (or target node) :uneditable-structure
                                  "Node is detached from the document root"))
             (let ((index (position current (node-children parent) :test #'eq)))
               (unless index
                 (edit-signal-error (or target node) :uneditable-structure
                                    "Node is absent from its parent's children"))
               (push index path))
             (setf current parent))
    path))

(defun edit-resolve-node (document target &optional allowed-types)
  "Resolve a node or a one-element node list and check ownership and type."
  (let ((node
          (cond ((node-p target) target)
                ((listp target)
                 (let ((count (edit-proper-list-length target)))
                   (unless count
                     (edit-signal-error target :invalid-value
                                        "Node target must be a proper list"))
                   (cond ((zerop count)
                          (edit-signal-error target :target-not-found
                                             "Edit target was not found"))
                         ((= count 1)
                          (unless (node-p (first target))
                            (edit-signal-error target :wrong-node-type
                                               "Target list does not contain a node"))
                          (first target))
                         (t (edit-signal-error target :ambiguous-target
                                              "Edit target resolves to multiple nodes")))))
                (t (edit-signal-error target :target-not-found
                                      "Edit target was not found")))))
    (unless (eq (node-document node) document)
      (edit-signal-error target :foreign-node
                         "Node does not belong to the input document"))
    (when (and allowed-types (not (member (node-type node) allowed-types)))
      (edit-signal-error target :wrong-node-type
                         "Node type is not valid for this edit"))
    node))

(defun edit-canonical-node-at-path (canonical-tree path)
  "Return the canonical node at zero-based child PATH."
  (let ((current canonical-tree))
    (dolist (index path current)
      (let ((children (getf current :children)))
        (unless (and (integerp index) (<= 0 index) (listp children)
                     (< index (length children)))
          (edit-signal-error path :invalid-value
                             "Canonical node path is outside the tree"))
        (setf current (nth index children))))))

(defun edit-canonical-set-property (canonical-tree path key value)
  "Set KEY to VALUE on the node at PATH in CANONICAL-TREE."
  (let* ((node (edit-canonical-node-at-path canonical-tree path))
         (properties (getf node :properties)))
    (setf (getf properties key) (edit-canonical-value value)
          (getf node :properties) properties)
    canonical-tree))

(defun edit-canonical-remove-property (canonical-tree path key)
  "Remove KEY from the properties of the node at PATH."
  (let* ((node (edit-canonical-node-at-path canonical-tree path))
         (properties (getf node :properties)))
    (remf properties key)
    (setf (getf node :properties) properties)
    canonical-tree))

(defun edit-canonical-set-children (canonical-tree path children)
  "Set the canonical child list of the node at PATH."
  (let ((node (edit-canonical-node-at-path canonical-tree path)))
    (setf (getf node :children) children))
  canonical-tree)

(defun edit-prepend-bom (octets encoding)
  "Add the encoding-specific BOM to OCTETS when ENCODING does not add one."
  (let ((bom (if (reader-generic-bom-encoding-p encoding)
                 #()
                 (reader-output-bom encoding))))
    (if (zerop (length bom))
        octets
        (let ((result (make-array (+ (length bom) (length octets))
                                  :element-type '(unsigned-byte 8))))
          (replace result bom)
          (replace result octets :start1 (length bom))
          result))))

(defun edit-encoded-source (text encoding bom-p)
  "Encode TEXT in ENCODING and restore its original BOM state."
  (handler-case
      (let ((octets (cl-codec-kit:string-to-octets text :encoding encoding)))
        (if bom-p
            (edit-prepend-bom octets encoding)
            octets))
    (cl-codec-kit:cl-codec-kit-error ()
      (error 'org-kit-error
             :message (format nil "Edited text cannot be represented in ~A"
                              (bounded-text (princ-to-string encoding)))))))

(defun edit-reparse-source (document text target)
  "Parse TEXT as a new document while preserving DOCUMENT's input metadata."
  (let* ((encoding (document-encoding document))
         (bom-p (document-bom-p document))
         (limits (document-limits document))
         (output-length (+ (length text) (if bom-p 1 0))))
    (check-limit limits :max-output-length output-length)
    (handler-case
        (parse-org
         (if encoding
             (edit-encoded-source text encoding bom-p)
             (if bom-p
                 (concatenate 'string (string (code-char #xFEFF)) text)
                 text))
         :source-name (document-source-name document)
         :encoding encoding
         :limits limits)
      (org-parse-error ()
        (edit-signal-error target :validation-parse-failed
                           "Edited text did not parse as Org")))))

(defun edit-canonical-equal-p (left right)
  "Compare canonical values while preserving exact string and sequence contents."
  (cond ((stringp left)
         (and (stringp right) (string= left right)))
        ((stringp right) nil)
        ((consp left)
         (and (consp right)
              (edit-canonical-equal-p (car left) (car right))
              (edit-canonical-equal-p (cdr left) (cdr right))))
        ((consp right) nil)
        ((vectorp left)
         (and (vectorp right)
              (not (stringp right))
              (= (length left) (length right))
              (loop for left-item across left
                    for right-item across right
                    always (edit-canonical-equal-p left-item right-item))))
        ((vectorp right) nil)
        (t (equalp left right))))

(defun edit-apply-patches (document patches &key target expected)
  "Splice PATCHES into DOCUMENT, reparse, and require EXPECTED canonical data."
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document))
  (unless (document-root document)
    (edit-signal-error target :uneditable-structure
                       "The document has no parsed tree"))
  (let* ((source (document-text document))
         (normalized (edit-normalize-patches patches (length source) target))
         (new-source (edit-splice-source source normalized))
         (new-document (edit-reparse-source document new-source target)))
    (unless (edit-canonical-equal-p expected (edit-canonical-document new-document))
      (edit-signal-error target :unexpected-diff
                         "Edited document contains an unexpected semantic change"))
    new-document))

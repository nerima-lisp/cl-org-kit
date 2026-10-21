;;;; src/edit-patches.lisp
;;;;
;;;; Shared source splicing and semantic validation for format-preserving edits.
(in-package #:org-kit)

(defstruct (edit-span (:constructor %make-edit-span (start end))
                      (:conc-name %edit-span-)
                      (:copier nil)
                      (:predicate %edit-span-p))
  "A half-open character range in the source before an edit."
  (start 0 :read-only t)
  (end 0 :read-only t))

(defun make-edit-span (start end)
  "Return a half-open edit span with START and END source character offsets."
  (%make-edit-span start end))

(defun edit-span-p (object)
  "Return true when OBJECT is an edit span."
  (%edit-span-p object))

(defun edit-span-start (span)
  "Return SPAN's inclusive start offset in the original source."
  (%edit-span-start span))

(defun edit-span-end (span)
  "Return SPAN's exclusive end offset in the original source."
  (%edit-span-end span))

(defstruct (edit-result (:constructor %make-edit-result (updated-document spans))
                        (:conc-name %edit-result-)
                        (:copier nil)
                        (:predicate %edit-result-p))
  "A successfully reparsed document and its original-source edit spans."
  (updated-document nil :read-only t)
  (spans nil :read-only t))

(defun make-edit-result (&key updated-document spans)
  "Return an edit result containing UPDATED-DOCUMENT and SPANS."
  (%make-edit-result updated-document spans))

(defun edit-result-p (object)
  "Return true when OBJECT is an edit result."
  (%edit-result-p object))

(defun edit-result-updated-document (result)
  "Return RESULT's reparsed document."
  (%edit-result-updated-document result))

(defun edit-result-spans (result)
  "Return RESULT's edit spans in ascending original-source order."
  (%edit-result-spans result))

(defstruct (edit-patch (:constructor %make-edit-patch (start end replacement))
                       (:copier nil))
  "A half-open source range and the text that replaces it."
  start
  end
  replacement)

(defstruct (edit-prepared-patches
            (:constructor %make-edit-prepared-patches
                (document source patches target expected))
            (:copier nil))
  document
  source
  patches
  target
  expected)

(defun edit-target-description (target)
  "Return a bounded, escaped description suitable for an edit condition."
  (let* ((description
           (cond ((stringp target) target)
                 ((node-p target)
                  (string-downcase (symbol-name (node-type target))))
                 ((null target) "target")
                 (t (with-output-to-string (stream)
                      (let ((*print-circle* t)
                            (*print-length* 16)
                            (*print-level* 4))
                        (prin1 target stream))))))
         (limit *condition-text-limit*))
    (loop for end downfrom (min (length description) limit) to 0
          for truncated-p = (< end (length description))
          for escaped = (bounded-text (subseq description 0 end))
          when (<= (+ (length escaped) (if truncated-p 3 0)) limit)
            return (if truncated-p
                       (concatenate 'string escaped "...")
                       escaped))))

(defun edit-signal-error (target reason message)
  "Signal an ORG-EDIT-ERROR for TARGET with REASON and MESSAGE."
  (error 'org-edit-error
         :message message
         :target (edit-target-description target)
         :reason reason))

(defun edit-make-patch (start end replacement &optional target)
  "Return a patch replacing [START, END) with REPLACEMENT."
  (declare (ignore target))
  (%make-edit-patch start end replacement))

(defun edit-proper-list-length (value)
  "Return the length of proper VALUE, or NIL when VALUE is dotted or circular."
  (handler-case (list-length value)
    (type-error () nil)))

(defun edit-normalize-patches (patches)
  "Return internally generated PATCHES sorted by source position."
  (stable-sort
   (copy-list patches)
   (lambda (left right)
     (let ((left-start (edit-patch-start left))
           (right-start (edit-patch-start right)))
       (or (< left-start right-start)
           (and (= left-start right-start)
                (= left-start (edit-patch-end left))
                (/= right-start (edit-patch-end right))))))))

(defun edit-validate-patches (source patches target)
  "Return sorted PATCHES after validating their source ranges and overlap."
  (unless (edit-proper-list-length patches)
    (edit-signal-error target :invalid-value
                       "Edit patches must be a proper list"))
  (dolist (patch patches)
    (unless (and (edit-patch-p patch)
                 (integerp (edit-patch-start patch))
                 (integerp (edit-patch-end patch))
                 (stringp (edit-patch-replacement patch))
                 (<= 0 (edit-patch-start patch)
                     (edit-patch-end patch)
                     (length source)))
      (edit-signal-error target :invalid-value
                         "Edit patch has an invalid range or replacement")))
  (let ((normalized (edit-normalize-patches patches)))
    (loop for previous in normalized
          for current in (cdr normalized)
          for previous-start = (edit-patch-start previous)
          for previous-end = (edit-patch-end previous)
          for current-start = (edit-patch-start current)
          for current-end = (edit-patch-end current)
          when (or (> previous-end current-start)
                   (and (= previous-start previous-end)
                        (= current-start current-end)
                        (= previous-start current-start)))
            do (edit-signal-error target :overlapping-patches
                                  "Edit patches overlap"))
    normalized))

(defun edit-output-bom-p (document)
  (or (document-bom-p document)
      (and (document-encoding document)
           (reader-generic-bom-encoding-p (document-encoding document)))))

(defun edit-prospective-output-length (document source patches)
  "Return the output character count implied by PATCHES, including its BOM."
  (+ (loop with length = (length source)
           for patch in patches
           do (incf length
                    (- (length (edit-patch-replacement patch))
                       (- (edit-patch-end patch) (edit-patch-start patch))))
           finally (return length))
     (if (edit-output-bom-p document) 1 0)))

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
  (edit-canonical-node (document-root document)))

(defun edit-canonical-copy (canonical-tree)
  "Return a tree copy that an editor can adjust to describe the expected result."
  (copy-tree canonical-tree))

(defun edit-node-path (document node &optional target)
  "Return NODE's zero-based child indexes from DOCUMENT's root."
  (let ((root (document-root document))
        (current node)
        (path '()))
    (loop until (eq current root)
          for parent = (node-parent current)
          do (let ((index (position current (node-children parent) :test #'eq)))
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
      (setf current (nth index (getf current :children))))))

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

(defun edit-with-encoding-error-mapping (encoding thunk)
  "Call THUNK and map codec failures to the established edit encoding error."
  (handler-case (funcall thunk)
    (cl-codec-kit:cl-codec-kit-error ()
      (error 'org-kit-error
             :message (format nil "Edited text cannot be represented in ~A"
                              (bounded-text (princ-to-string encoding)))))))

(defun edit-encoded-range-octets (string start end encoding)
  "Count the octets for STRING[START,END) in ENCODING."
  (edit-with-encoding-error-mapping
   encoding
   (lambda ()
     (cl-codec-kit:string-size-in-octets string :start start :end end
                                         :encoding encoding))))

(defun edit-prospective-output-octets (document source patches)
  "Count the encoded output implied by PATCHES without splicing SOURCE."
  (let* ((encoding (or (document-encoding document) :utf-8))
         (generic-preamble-p (reader-generic-bom-encoding-p encoding))
         (preamble-length (if generic-preamble-p
                              (edit-encoded-range-octets "" 0 0 encoding)
                              0))
         (body-octets 0))
    (loop with cursor = 0
          for patch in patches
          for start = (edit-patch-start patch)
          for end = (edit-patch-end patch)
          for replacement = (edit-patch-replacement patch)
          do (incf body-octets
                   (- (edit-encoded-range-octets source cursor start encoding)
                      preamble-length))
             (incf body-octets
                   (- (edit-encoded-range-octets replacement 0
                                                  (length replacement) encoding)
                      preamble-length))
             (setf cursor end)
          finally
             (incf body-octets
                   (- (edit-encoded-range-octets source cursor (length source)
                                                  encoding)
                      preamble-length)))
    (+ body-octets
       preamble-length
       (if (and (document-bom-p document) (not generic-preamble-p))
           (length (reader-output-bom encoding))
           0))))

(defun edit-encoded-source (text encoding bom-p)
  "Encode TEXT in ENCODING and restore its original BOM state."
  (let ((octets
          (edit-with-encoding-error-mapping
           encoding
           (lambda ()
             (cl-codec-kit:string-to-octets text :encoding encoding)))))
    (if bom-p
        (edit-prepend-bom octets encoding)
        octets)))

(defun edit-reparse-source (document text target)
  "Parse TEXT as a new document while preserving DOCUMENT's input metadata."
  (let* ((encoding (document-encoding document))
         (bom-p (document-bom-p document))
         (limits (document-limits document))
         (output-length (+ (length text) (if (edit-output-bom-p document) 1 0))))
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
                           "Edited document failed validation parsing")))))

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
              (= (length left) (length right))
              (loop for left-item across left
                    for right-item across right
                    always (edit-canonical-equal-p left-item right-item))))
        (t (equalp left right))))

(defun edit-prepare-patches (document patches &key target expected)
  "Validate PATCHES and check per-document size limits before source splicing."
  (let* ((source (document-text document))
         (normalized (edit-validate-patches source patches target))
         (limits (document-limits document))
         (output-length (edit-prospective-output-length document source normalized)))
    (check-limit limits :max-output-length output-length)
    (unless (document-encoding document)
      (check-limit limits :max-input-length output-length))
    (let ((output-octets
            (edit-prospective-output-octets document source normalized)))
      (when (document-encoding document)
        (check-limit limits :max-input-length output-octets))
      (check-limit limits :max-output-octets output-octets)
      (%make-edit-prepared-patches document source normalized target expected))))

(defun edit-finish-prepared-patches (prepared)
  "Splice, reparse, and validate a prepared patch operation."
  (let* ((document (edit-prepared-patches-document prepared))
         (patches (edit-prepared-patches-patches prepared))
         (target (edit-prepared-patches-target prepared))
         (new-source (edit-splice-source
                      (edit-prepared-patches-source prepared) patches))
         (new-document (edit-reparse-source document new-source target)))
    (unless (edit-canonical-equal-p
             (edit-prepared-patches-expected prepared)
             (edit-canonical-document new-document))
      (edit-signal-error target :unexpected-diff
                         "Edited document contains an unexpected semantic change"))
    (%make-edit-result
     new-document
     (mapcar (lambda (patch)
               (make-edit-span (edit-patch-start patch)
                               (edit-patch-end patch)))
             patches))))

(defun edit-apply-patches-result (document patches &key target expected)
  "Apply PATCHES and return the reparsed document with original-source spans."
  (edit-finish-prepared-patches
   (edit-prepare-patches document patches :target target :expected expected)))

(defun edit-apply-patches-transaction (operations)
  "Return edit results only after every independent document edit succeeds.
Each operation checks output limits for its own document."
  (mapcar #'edit-finish-prepared-patches
          (mapcar (lambda (arguments)
                    (apply #'edit-prepare-patches arguments))
                  operations)))

(defun edit-apply-patches (document patches &key target expected)
  "Splice PATCHES into DOCUMENT, reparse, and require EXPECTED canonical data.

Return the reparsed document for compatibility with existing edit APIs."
  (edit-result-updated-document
   (edit-apply-patches-result document patches
                              :target target :expected expected)))

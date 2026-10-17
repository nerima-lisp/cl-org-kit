;;;; src/edit-footnotes.lisp
;;;; Format-preserving footnote insertion, renumbering and normalization.
(in-package #:org-kit)

(defun edit-footnote-nodes (document type)
  (let ((nodes '()))
    (map-nodes (lambda (node)
                 (when (eq (node-type node) type)
                   (push node nodes)))
               (document-root document))
    (nreverse nodes)))

(defun edit-footnote-labels (document type)
  (remove nil (mapcar (lambda (node) (node-property node :label))
                      (edit-footnote-nodes document type))))

(defun edit-footnote-unique-labels (labels)
  (remove-duplicates labels :from-end t :test #'string=))

(defun edit-footnote-diagnostics (document)
  (let* ((references (edit-footnote-unique-labels
                      (edit-footnote-labels document :footnote-reference)))
         (definitions (edit-footnote-unique-labels
                       (edit-footnote-labels document :footnote-definition))))
    (values (remove-if (lambda (label) (member label definitions :test #'string=))
                       references)
            (remove-if (lambda (label) (member label references :test #'string=))
                       definitions))))

(defun edit-footnote-expected-after-patches (document patches target)
  (edit-prepare-patches document patches :target target)
  (let* ((source (document-text document))
         (normalized (edit-validate-patches source patches target))
         (edited-source (edit-splice-source source normalized)))
    (edit-canonical-document (edit-reparse-source document edited-source target))))

(defun edit-footnote-line-ending (source)
  (let ((newline (position #\Newline source)))
    (if (and newline (plusp newline)
             (char= (char source (1- newline)) #\Return))
        (format nil "~C~C" #\Return #\Newline)
        (string #\Newline))))

(defun edit-footnote-line-start-p (source offset)
  (or (zerop offset)
      (char= (char source (1- offset)) #\Newline)))

(defun edit-footnote-numeric-label-p (label)
  (and (stringp label) (plusp (length label))
       (every #'digit-char-p label)))

(defun edit-footnote-next-label (document)
  (let ((maximum 0))
    (dolist (label (append (edit-footnote-labels document :footnote-reference)
                           (edit-footnote-labels document :footnote-definition)))
      (when (edit-footnote-numeric-label-p label)
        (setf maximum (max maximum (parse-integer label)))))
    (write-to-string (1+ maximum))))

(defun edit-footnote-label-patches (nodes labels target)
  (loop for node in nodes
        for label = (node-property node :label)
        for replacement = (and label (gethash label labels))
        when (and replacement (not (string= label replacement)))
          collect (edit-make-patch (+ (node-begin node) 4)
                                   (+ (node-begin node) 4 (length label))
                                   replacement target)))

(defun edit-footnote-report (result)
  (multiple-value-bind (undefined unreferenced)
      (edit-footnote-diagnostics (edit-result-updated-document result))
    (values result undefined unreferenced)))

(defun insert-footnote (document offset body)
  "Insert a numbered footnote reference at OFFSET and append BODY as its definition.

Return an EDIT-RESULT, the assigned label, undefined reference labels, and
unreferenced definition labels, in that order. BODY is raw Org content without
a trailing line ending. OFFSET is a character offset in DOCUMENT-TEXT."
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document))
  (unless (and (integerp offset) (<= 0 offset (length (document-text document))))
    (edit-signal-error offset :invalid-value "Footnote insertion offset is invalid"))
  (when (and (plusp offset)
             (< offset (length (document-text document)))
             (char= (char (document-text document) (1- offset)) #\Return)
             (char= (char (document-text document) offset) #\Newline))
    (edit-signal-error offset :invalid-value "Footnote insertion splits a CRLF line ending"))
  (unless (and (stringp body) (plusp (length body))
               (not (or (char= (char body (1- (length body))) #\Newline)
                        (char= (char body (1- (length body))) #\Return))))
    (edit-signal-error "footnote body" :invalid-value
                       "Footnote body must be a non-empty string without a trailing line ending"))
  (let* ((source (document-text document))
         (label (edit-footnote-next-label document))
         (newline (edit-footnote-line-ending source))
         (reference (format nil "[fn:~A]" label))
         (reference-at-line-start-p (edit-footnote-line-start-p source offset))
         (reference-text
           (concatenate 'string
                        (if reference-at-line-start-p " " "")
                        reference
                        (if reference-at-line-start-p newline "")))
         (definition (concatenate 'string "[fn:" label "] " body))
         (definition-text
           (concatenate 'string
                        (if (= offset (length source))
                            (if reference-at-line-start-p "" newline)
                            (if (and (plusp (length source))
                                     (not (char= (char source (1- (length source)))
                                                 #\Newline)))
                                newline ""))
                        definition
                        (if (document-final-newline-p document) newline "")))
         (patches
           (if (= offset (length source))
               (list (edit-make-patch offset offset
                                      (concatenate 'string reference-text definition-text)
                                      offset))
               (list (edit-make-patch offset offset reference-text offset)
                     (edit-make-patch (length source) (length source)
                                      definition-text offset))))
         (expected (edit-footnote-expected-after-patches document patches offset))
         (result (edit-apply-patches-result document patches
                                             :target offset :expected expected)))
    (multiple-value-bind (undefined unreferenced)
        (edit-footnote-diagnostics (edit-result-updated-document result))
      (values result label undefined unreferenced))))

(defun renumber-footnotes (document)
  "Renumber numeric footnote labels by first reference, then unused definition order.

Return an EDIT-RESULT followed by undefined reference labels and unreferenced
definition labels from the updated document. Named labels remain unchanged."
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document))
  (let* ((references (edit-footnote-nodes document :footnote-reference))
         (definitions (edit-footnote-nodes document :footnote-definition))
         (ordered-labels '())
         (labels (make-hash-table :test #'equal)))
    (dolist (node references)
      (let ((label (node-property node :label)))
        (when (and (edit-footnote-numeric-label-p label)
                   (not (member label ordered-labels :test #'string=)))
          (setf ordered-labels (append ordered-labels (list label))))))
    (dolist (node definitions)
      (let ((label (node-property node :label)))
        (when (and (edit-footnote-numeric-label-p label)
                   (not (member label ordered-labels :test #'string=)))
          (setf ordered-labels (append ordered-labels (list label))))))
    (loop for label in ordered-labels
          for number from 1
          do (setf (gethash label labels) (write-to-string number)))
    (let* ((patches (append (edit-footnote-label-patches
                             references labels "footnotes")
                            (edit-footnote-label-patches
                             definitions labels "footnotes")))
           (expected (edit-footnote-expected-after-patches
                      document patches "footnotes"))
           (result (edit-apply-patches-result document patches
                                              :target "footnotes" :expected expected)))
      (edit-footnote-report result))))

(defun edit-footnote-normalization-order (document definitions)
  (let ((reference-order (edit-footnote-unique-labels
                          (edit-footnote-labels document :footnote-reference))))
    (stable-sort
     (copy-list definitions)
     (lambda (left right)
       (let* ((left-position (position (node-property left :label)
                                       reference-order :test #'string=))
              (right-position (position (node-property right :label)
                                        reference-order :test #'string=)))
         (cond ((and left-position right-position) (< left-position right-position))
               (left-position t)
               (right-position nil)
               (t (< (node-begin left) (node-begin right)))))))))

(defun normalize-footnotes (document)
  "Move all footnote definitions to the document end in first-reference order.

Definitions without references follow in their original order. Return an
EDIT-RESULT followed by undefined reference labels and unreferenced definition
labels from the updated document. Definition source text is moved verbatim."
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document))
  (let* ((source (document-text document))
         (definitions (edit-footnote-nodes document :footnote-definition))
         (ordered (edit-footnote-normalization-order document definitions)))
    (if (null definitions)
        (edit-footnote-report
         (edit-apply-patches-result
          document nil :target "footnotes"
          :expected (edit-canonical-document document)))
        (let* ((moved-text
                 (with-output-to-string (stream)
                   (loop for node in ordered
                         for start = (node-begin node)
                         for end = (node-end node)
                         for chunk = (subseq source start end)
                         do (write-string chunk stream)
                            (when (and (plusp (length chunk))
                                       (not (char= (char chunk (1- (length chunk)))
                                                   #\Newline))
                                       (not (eq node (car (last ordered)))))
                              (write-string (edit-footnote-line-ending source) stream)))))
               (deletions
                 (mapcar (lambda (node)
                           (edit-make-patch (node-begin node) (node-end node) ""
                                            "footnotes"))
                         definitions))
               (prepared-deletions
                 (edit-prepare-patches document deletions :target "footnotes"))
               (retained-source
                 (edit-splice-source
                  source (edit-prepared-patches-patches prepared-deletions)))
               (prefix
                 (if (and (plusp (length retained-source))
                          (not (char= (char retained-source
                                           (1- (length retained-source)))
                                      #\Newline)))
                     (edit-footnote-line-ending source) ""))
               (patches
                 (append deletions
                         (list (edit-make-patch
                                (length source) (length source)
                                (concatenate 'string prefix moved-text)
                                "footnotes"))))
               (prepared-patches
                 (edit-prepare-patches document patches :target "footnotes"))
               (new-source (edit-splice-source source
                                               (edit-prepared-patches-patches
                                                prepared-patches)))
               (effective-patches (if (string= source new-source) nil patches))
               (expected (if effective-patches
                             (edit-footnote-expected-after-patches
                              document effective-patches "footnotes")
                             (edit-canonical-document document)))
               (result (edit-apply-patches-result document effective-patches
                                                  :target "footnotes"
                                                  :expected expected)))
          (edit-footnote-report result)))))

;;;; src/edit-elements.lisp
;;;;
;;;; Format-preserving edits for raw block bodies and list checkboxes.
(in-package #:org-kit)

(defun edit-element-block-body-span (document node body target)
  "Return the source interval occupied by NODE's parsed raw block BODY."
  (let* ((source (document-text document))
         (contents-begin (node-contents-begin node))
         (contents-end (node-contents-end node))
         (node-begin (node-post-affiliated node))
         (node-end (node-end node)))
    (unless (and (stringp body)
                 (integerp node-begin)
                 (integerp node-end)
                 (<= 0 node-begin node-end (length source)))
      (edit-signal-error target :uneditable-structure
                         "Block source boundaries are unavailable"))
    (multiple-value-bind (start end)
        (cond
          ((and (integerp contents-begin) (integerp contents-end))
           (values contents-begin contents-end))
          ((and (null contents-begin) (null contents-end))
           (let ((opening-newline
                   (position #\Newline source :start node-begin :end node-end)))
             (unless opening-newline
               (edit-signal-error target :uneditable-structure
                                  "Block opener has no body boundary"))
             (let ((body-start (1+ opening-newline)))
               (values body-start (+ body-start (length body))))))
          (t
           (edit-signal-error target :uneditable-structure
                              "Block content boundaries are incomplete")))
      (unless (and (integerp start) (integerp end)
                   (<= node-begin start end node-end (length source))
                   (= (- end start) (length body))
                   (string= source body :start1 start :end1 end))
        (edit-signal-error target :uneditable-structure
                           "Parsed block body does not match its source span"))
      (values start end))))

(defun replace-block-body (document target raw-body)
  "Replace the raw body of one source or example block and reparse the result."
  (let ((node (edit-resolve-node document target '(:src-block :example-block))))
    (unless (stringp raw-body)
      (edit-signal-error target :invalid-value "Block body must be a string"))
    (multiple-value-bind (body present-p) (node-property node :value)
      (unless (and present-p (stringp body))
        (edit-signal-error target :uneditable-structure
                           "Block has no parsed body value"))
      (multiple-value-bind (start end)
          (edit-element-block-body-span document node body target)
        (let* ((path (edit-node-path document node target))
               (expected (edit-canonical-copy
                          (edit-canonical-document document))))
          (edit-canonical-set-property expected path :value raw-body)
          (edit-apply-patches
           document
           (list (edit-make-patch start end raw-body target))
           :target node
           :expected expected))))))

(defun edit-element-checkbox-position (document node target checkbox)
  "Return the source offset of NODE's checkbox state character."
  (let* ((source (document-text document))
         (start (node-post-affiliated node))
         (end (node-end node))
         (bullet (node-property node :bullet)))
    (unless (and (integerp start) (integerp end)
                 (<= 0 start end (length source))
                 (stringp bullet))
      (edit-signal-error target :uneditable-structure
                         "List item source boundaries are unavailable"))
    (let* ((line-end (or (position #\Newline source :start start :end end) end))
           (line (subseq source start line-end))
           (bullet-start
             (position-if-not (lambda (character)
                                (find character '(#\Space #\Tab)))
                              line))
           (bullet-end (and bullet-start (+ bullet-start (length bullet))))
           (checkbox-start
             (and bullet-end
                  (loop with position = bullet-end
                        while (and (< position (length line))
                                   (find (char line position) '(#\Space #\Tab)))
                        do (incf position)
                        finally (return position)))))
      (unless (and bullet-start
                   bullet-end
                   (<= bullet-end (length line))
                   (string= line bullet :start1 bullet-start
                                          :end1 bullet-end)
                   checkbox-start
                   (<= (+ checkbox-start 2) (length line))
                   (char= (char line checkbox-start) #\[)
                   (char= (char line (+ checkbox-start 2)) #\])
                   (case checkbox
                     (:off (char= (char line (1+ checkbox-start)) #\Space))
                     (:on (find (char line (1+ checkbox-start)) '(#\X #\x)))
                     (:trans (char= (char line (1+ checkbox-start)) #\-))))
        (edit-signal-error target :uneditable-structure
                           "Parsed checkbox does not match its source marker"))
      (+ start checkbox-start 1))))

(defun set-checkbox-state (document target state)
  "Set one list item's checkbox to :CHECKED, :UNCHECKED, or :PARTIAL."
  (let ((node (edit-resolve-node document target '(:item))))
    (unless (member state '(:checked :unchecked :partial))
      (edit-signal-error target :invalid-value "Unknown checkbox state"))
    (multiple-value-bind (checkbox present-p) (node-property node :checkbox)
      (unless (and present-p (member checkbox '(:on :off :trans)))
        (edit-signal-error target :invalid-value
                           "List item does not have a checkbox"))
      (let* ((path (edit-node-path document node target))
             (expected (edit-canonical-copy
                        (edit-canonical-document document)))
             (replacement (ecase state
                           (:checked "X")
                           (:unchecked " ")
                           (:partial "-")))
             (expected-checkbox (ecase state
                                 (:checked :on)
                                 (:unchecked :off)
                                 (:partial :trans)))
             (position (edit-element-checkbox-position
                        document node target checkbox)))
        (edit-canonical-set-property expected path :checkbox expected-checkbox)
        (edit-apply-patches
         document
         (list (edit-make-patch position (1+ position) replacement target))
         :target node
         :expected expected)))))

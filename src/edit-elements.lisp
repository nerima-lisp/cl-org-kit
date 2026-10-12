;;;; src/edit-elements.lisp
;;;;
;;;; Format-preserving edits for raw block bodies and list checkboxes.
(in-package #:org-kit)

(defun edit-element-block-body-span (document node body target)
  "Return the source interval occupied by NODE's parsed raw block BODY."
  (let* ((source (document-text document))
         (node-begin (node-post-affiliated node))
         (node-end (node-end node))
         (opening-newline
           (position #\Newline source :start node-begin :end node-end)))
    (unless opening-newline
      (edit-signal-error target :uneditable-structure
                         "Block opener has no body boundary"))
    (let ((body-start (1+ opening-newline)))
      (values body-start (+ body-start (length body))))))

(defun replace-block-body (document target raw-body)
  "Replace the raw body of one source or example block and reparse the result."
  (let ((node (edit-resolve-node document target '(:src-block :example-block))))
    (unless (stringp raw-body)
      (edit-signal-error target :invalid-value "Block body must be a string"))
    (let ((body (node-property node :value)))
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

(defun edit-element-checkbox-position (document node)
  "Return the source offset of NODE's checkbox state character."
  (let* ((source (document-text document))
         (start (node-post-affiliated node))
         (end (node-end node))
         (bullet (node-property node :bullet))
         (line-end (or (position #\Newline source :start start :end end) end))
         (line (subseq source start line-end))
         (bullet-start
           (position-if-not (lambda (character)
                              (find character '(#\Space #\Tab)))
                            line)))
    (+ start bullet-start (length bullet) 1)))

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
                        document node)))
        (edit-canonical-set-property expected path :checkbox expected-checkbox)
        (edit-apply-patches
         document
         (list (edit-make-patch position (1+ position) replacement target))
         :target node
         :expected expected)))))

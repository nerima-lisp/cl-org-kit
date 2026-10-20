(in-package #:cl-org-kit/test)
(defun edit-toggle-test-error (thunk)
  (handler-case
      (progn (funcall thunk) nil)
    (org-edit-error (condition) condition)))

(defun edit-toggle-test-preserves-outside-span-p (source result)
  (let* ((spans (edit-result-spans result))
         (span (first spans))
         (start (and span (edit-span-start span)))
         (end (and span (edit-span-end span)))
         (updated (document-text (edit-result-updated-document result)))
         (updated-suffix-start (and span (- (length updated) (- (length source) end)))))
    (and (= (length spans) 1)
         (<= 0 start end (length source))
         (string= source updated :start1 0 :end1 start :start2 0 :end2 start)
         (string= source updated
                  :start1 end :end1 (length source)
                  :start2 updated-suffix-start :end2 (length updated)))))

(describe "format-preserving line toggles"
  (it "toggles COMMENT at the headline prefix and preserves its surrounding source"
    (let* ((source (format nil "* TODO [#A] Heading :tag:~%tail~%"))
           (document (parse-org source))
           (point (search "Heading" source))
           (span (make-edit-span point point))
           (result (org-kit::toggle-heading-comment document span))
           (updated (edit-result-updated-document result))
           (updated-source (document-text updated))
           (remove-result (org-kit::toggle-heading-comment
                           updated (make-edit-span 0 0)))
           (removed (edit-result-updated-document remove-result)))
      (with-soft-assertions
        (expect (edit-result-p result) :to-be t)
        (expect updated-source
                :to-equal (format nil "* TODO [#A] COMMENT Heading :tag:~%tail~%"))
        (expect (document-text removed) :to-equal source)
        (expect (edit-span-start (first (edit-result-spans result)))
                :to-be point)
        (expect (edit-span-end (first (edit-result-spans result)))
                :to-be point)
        (expect (edit-toggle-test-preserves-outside-span-p source result)
                :to-be t)
        (expect (edit-toggle-test-preserves-outside-span-p
                 updated-source remove-result)
                :to-be t)
        (expect (document-text document) :to-equal source))))

  (it "rejects COMMENT toggles when the span does not locate a headline"
    (let* ((document (parse-org (format nil "ordinary text~%")))
           (condition
             (edit-toggle-test-error
              (lambda ()
                (org-kit::toggle-heading-comment
                 document (make-edit-span 2 2))))))
      (with-soft-assertions
        (expect (typep condition 'org-edit-error) :to-be t)
        (when (typep condition 'org-edit-error)
          (expect (org-edit-error-reason condition) :to-be :wrong-node-type)
          (expect (org-edit-error-target condition) :to-equal "headline")))))

  (it "toggles a normal line to a heading and back"
    (let* ((source (format nil "plain text~%kept line~%"))
           (document (parse-org source))
           (span (make-edit-span 3 3))
           (result (org-kit::toggle-heading document span))
           (updated (edit-result-updated-document result))
           (remove-result
             (org-kit::toggle-heading updated (make-edit-span 3 3))))
      (with-soft-assertions
        (expect (document-text updated)
                :to-equal (format nil "* plain text~%kept line~%"))
        (expect (document-text (edit-result-updated-document remove-result))
                :to-equal source)
        (expect (edit-toggle-test-preserves-outside-span-p source result)
                :to-be t)
        (expect (edit-toggle-test-preserves-outside-span-p
                 (document-text updated) remove-result)
                :to-be t)
        (expect (document-text document) :to-equal source))))

  (it "toggles a normal line to an item and back"
    (let* ((source (format nil "before~%  plain text~%after~%"))
           (document (parse-org source))
           (point (search "plain" source))
           (span (make-edit-span point point))
           (result (org-kit::toggle-item document span))
           (updated (edit-result-updated-document result))
           (remove-result (org-kit::toggle-item updated span)))
      (with-soft-assertions
        (expect (document-text updated)
                :to-equal (format nil "before~%  - plain text~%after~%"))
        (expect (document-text (edit-result-updated-document remove-result))
                :to-equal source)
        (expect (edit-span-start (first (edit-result-spans result)))
                :to-be point)
        (expect (edit-span-end (first (edit-result-spans result)))
                :to-be point)
        (expect (edit-toggle-test-preserves-outside-span-p source result)
                :to-be t)
        (expect (edit-toggle-test-preserves-outside-span-p
                 (document-text updated) remove-result)
                :to-be t)
        (expect (document-text document) :to-equal source))))

  (it "converts TODO and DONE headings to unchecked and checked items"
    (dolist (fixture (list (list "TODO" "[ ]")
                           (list "DONE" "[X]")))
      (let* ((source (format nil "* ~A Task :tag:~%" (first fixture)))
             (document (parse-org source))
             (result
               (org-kit::toggle-item document (make-edit-span 0 0))))
        (with-soft-assertions
          (expect (document-text (edit-result-updated-document result))
                  :to-equal
                  (format nil "- ~A Task~%" (second fixture)))
          (expect (edit-toggle-test-preserves-outside-span-p source result)
                  :to-be t)
          (expect (document-text document) :to-equal source)))))

  (it "adds and removes the fixed-width marker on a line"
    (let* ((source (format nil "before~%plain text~%after~%"))
           (document (parse-org source))
           (point (search "plain" source))
           (span (make-edit-span point point))
           (result (org-kit::toggle-fixed-width document span))
           (updated (edit-result-updated-document result))
           (remove-result (org-kit::toggle-fixed-width updated span)))
      (with-soft-assertions
        (expect (document-text updated)
                :to-equal (format nil "before~%: plain text~%after~%"))
        (expect (document-text (edit-result-updated-document remove-result))
                :to-equal source)
        (expect (edit-toggle-test-preserves-outside-span-p source result)
                :to-be t)
        (expect (edit-toggle-test-preserves-outside-span-p
                 (document-text updated) remove-result)
                :to-be t)
        (expect (document-text document) :to-equal source))))

  (it "rejects fixed-width insertion in a source block and invalid spans"
    (let* ((source (format nil "#+BEGIN_SRC text~%body~%#+END_SRC~%"))
           (document (parse-org source))
           (blocked
             (edit-toggle-test-error
              (lambda ()
                (org-kit::toggle-fixed-width
                 document
                 (make-edit-span (search "body" source)
                                 (search "body" source))))))
           (invalid
             (edit-toggle-test-error
              (lambda ()
                (org-kit::toggle-heading document (make-edit-span -1 0)))))
           (invalid-document (parse-org (format nil "plain~%"))))
      (with-soft-assertions
        (expect (typep blocked 'org-edit-error) :to-be t)
        (when (typep blocked 'org-edit-error)
          (expect (org-edit-error-reason blocked)
                  :to-be :uneditable-structure)
          (expect (org-edit-error-target blocked) :to-equal "fixed-width"))
        (expect (typep invalid 'org-edit-error) :to-be t)
        (when (typep invalid 'org-edit-error)
          (expect (org-edit-error-reason invalid) :to-be :invalid-value)
          (expect (org-edit-error-target invalid) :to-equal "heading"))
        (expect (document-text invalid-document) :to-equal (format nil "plain~%"))))))


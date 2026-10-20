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
        (expect (document-text invalid-document) :to-equal (format nil "plain~%")))))
  (it "validates documents and spans and keeps blank heading and item selections unchanged"
      (let* ((source (format nil " ~C~%" #\Tab))
             (document (parse-org source))
             (wrong-document
              (handler-case
                  (progn
                    (org-kit::toggle-heading nil (make-edit-span 0 0))
                    nil)
                (type-error (condition) condition)))
             (invalid-spans
              (mapcar (lambda (span)
                        (edit-toggle-test-error
                         (lambda () (org-kit::toggle-heading document span))))
                      (list nil
                            (make-edit-span 0.5 0)
                            (make-edit-span 0 0.5)
                            (make-edit-span 2 1)
                            (make-edit-span 99 99))))
             (heading-result
              (org-kit::toggle-heading document (make-edit-span 0 0)))
             (item-result
              (org-kit::toggle-item document (make-edit-span 0 0)))
             (fixed-result
              (org-kit::toggle-fixed-width document (make-edit-span 0 0)))
             (comment-error
              (edit-toggle-test-error
               (lambda ()
                 (org-kit::toggle-heading-comment document (make-edit-span 0 0))))))
        (with-soft-assertions
         (expect (typep wrong-document 'type-error) :to-be t)
         (expect (every (lambda (condition)
                          (and (typep condition 'org-edit-error)
                               (eq (org-edit-error-reason condition) :invalid-value)))
                        invalid-spans)
                 :to-be t)
         (expect (document-text (edit-result-updated-document heading-result))
                 :to-equal source)
         (expect (document-text (edit-result-updated-document item-result))
                 :to-equal source)
         (expect (document-text (edit-result-updated-document fixed-result))
                 :to-equal (format nil " ~C: ~%" #\Tab))
         (expect (typep comment-error 'org-edit-error) :to-be t)
         (when (typep comment-error 'org-edit-error)
           (expect (org-edit-error-reason comment-error) :to-be :target-not-found)
           (expect (org-edit-error-target comment-error) :to-equal "headline")))))
  (it "preserves CRLF spans and skips protected or non-normal lines"
      (let* ((crlf-source (format nil "first~C~%second" #\Return))
             (crlf-document (parse-org crlf-source))
             (crlf-result
              (org-kit::toggle-heading
               crlf-document (make-edit-span 1 (length crlf-source))))
             (mixed-source
              (format nil "plain~%~%: fixed~%#+NAME: value~%* headline~%"))
             (mixed-document (parse-org mixed-source))
             (mixed-result
              (org-kit::toggle-heading
               mixed-document (make-edit-span 0 (length mixed-source))))
             (block-source
              (format nil "#+BEGIN_SRC text~%body~%#+END_SRC~%"))
             (block-document (parse-org block-source))
             (body-point (search "body" block-source))
             (block-heading-result
              (org-kit::toggle-heading
               block-document (make-edit-span body-point body-point)))
             (block-item-result
              (org-kit::toggle-item
               block-document (make-edit-span body-point body-point))))
        (with-soft-assertions
         (expect (document-text (edit-result-updated-document crlf-result))
                 :to-equal (format nil "* first~C~%* second" #\Return))
         (expect (document-text (edit-result-updated-document mixed-result))
                 :to-equal
                 (format nil "* plain~%~%: fixed~%#+NAME: value~%* headline~%"))
         (expect (document-text (edit-result-updated-document block-heading-result))
                 :to-equal block-source)
         (expect (document-text (edit-result-updated-document block-item-result))
                 :to-equal block-source))))
  (it "converts ordered and checkbox items by their enclosing heading"
      (let* ((item-source
              (format nil
                      "* Parent~%12. numbered~%13) alternate~%- [ ] open~%- [X] checked~%- [x] lower~%- [-] partial~%- [a] literal~%- [~%"))
             (item-document (parse-org item-source))
             (item-start (search "12." item-source))
             (heading-result
              (org-kit::toggle-heading
               item-document (make-edit-span item-start (length item-source))))
             (nested-source (format nil "* Parent~%** TODO Child~%*** DONE Deep~%"))
             (nested-document (parse-org nested-source))
             (item-result
              (org-kit::toggle-item
               nested-document (make-edit-span 0 (length nested-source))))
             (plain-source (format nil "* Top~%** Child~%"))
             (plain-document (parse-org plain-source))
             (child-point (search "Child" plain-source))
             (plain-result
              (org-kit::toggle-heading
               plain-document (make-edit-span child-point child-point))))
        (with-soft-assertions
         (expect (document-text (edit-result-updated-document heading-result))
                 :to-equal
                 (format nil
                         "* Parent~%** numbered~%** alternate~%** TODO open~%** DONE checked~%** DONE lower~%** TODO partial~%** [a] literal~%** [~%"))
         (expect (document-text (edit-result-updated-document item-result))
                 :to-equal
                 (format nil "- Parent~%  - [ ] Child~%    - [X] Deep~%"))
         (expect (document-text (edit-result-updated-document plain-result))
                 :to-equal (format nil "* Top~%Child~%")))))
  (it "checks COMMENT metadata and resolves headline source positions"
      (let* ((comment-source (format nil "* COMMENT~%"))
             (comment-document (parse-org comment-source))
             (comment-result
              (org-kit::toggle-heading-comment
               comment-document (make-edit-span 0 0)))
             (fallback-source (format nil "* Fallback~%"))
             (fallback-document (parse-org fallback-source))
             (fallback-headline
              (let ((found nil))
                (map-nodes
                 (lambda (node)
                   (when (eq (node-type node) :headline)
                     (setf found node)))
                 (document-root fallback-document))
                found))
             (mismatch-source (format nil "* Heading~%"))
             (mismatch-document (parse-org mismatch-source))
             (mismatch-headline
              (let ((found nil))
                (map-nodes
                 (lambda (node)
                   (when (eq (node-type node) :headline)
                     (setf found node)))
                 (document-root mismatch-document))
                found)))
        (setf (org-kit::%node-post-affiliated fallback-headline) nil
              (node-property mismatch-headline :commentedp) t)
        (let* ((fallback-result
                (org-kit::toggle-heading-comment
                 fallback-document (make-edit-span 0 0)))
               (mismatch-error
                (edit-toggle-test-error
                 (lambda ()
                   (org-kit::toggle-heading-comment
                    mismatch-document (make-edit-span 0 0)))))
               (stale-document (parse-org (format nil "* Stale~%")))
               (stale-headline
                (let ((found nil))
                  (map-nodes
                   (lambda (node)
                     (when (eq (node-type node) :headline)
                       (setf found node)))
                   (document-root stale-document))
                  found)))
          (setf (org-kit::%node-begin stale-headline) 1
                (org-kit::%node-post-affiliated stale-headline) 1)
          (let ((stale-error
                 (edit-toggle-test-error
                  (lambda ()
                    (org-kit::toggle-item stale-document (make-edit-span 0 0))))))
            (with-soft-assertions
             (expect (document-text (edit-result-updated-document comment-result))
                     :to-equal (format nil "* ~%"))
             (expect (document-text (edit-result-updated-document fallback-result))
                     :to-equal (format nil "* COMMENT Fallback~%"))
             (expect (typep mismatch-error 'org-edit-error) :to-be t)
             (when (typep mismatch-error 'org-edit-error)
               (expect (org-edit-error-reason mismatch-error)
                       :to-be :uneditable-structure)
               (expect (org-edit-error-target mismatch-error)
                       :to-equal "headline"))
             (expect (typep stale-error 'org-edit-error) :to-be t)
             (when (typep stale-error 'org-edit-error)
               (expect (org-edit-error-reason stale-error)
                       :to-be :uneditable-structure)
               (expect (org-edit-error-target stale-error)
                       :to-equal "item")))))))
  (it "preserves indentation and handles marker-only and blank fixed-width lines"
      (let* ((tab-source (format nil "~Cplain~%" #\Tab))
             (tab-document (parse-org tab-source))
             (tab-point (search "plain" tab-source))
             (tab-result
              (org-kit::toggle-fixed-width
               tab-document (make-edit-span tab-point tab-point)))
             (tab-updated (edit-result-updated-document tab-result))
             (tab-remove-result
              (org-kit::toggle-fixed-width
               tab-updated (make-edit-span tab-point tab-point)))
             (marker-source (format nil ":~%"))
             (marker-document (parse-org marker-source))
             (marker-result
              (org-kit::toggle-fixed-width
               marker-document (make-edit-span 0 0)))
             (blank-source (format nil " ~C~%" #\Tab))
             (blank-document (parse-org blank-source))
             (blank-result
              (org-kit::toggle-fixed-width
               blank-document (make-edit-span 0 0))))
        (with-soft-assertions
         (expect (document-text tab-updated)
                 :to-equal (format nil "~C: plain~%" #\Tab))
         (expect (document-text (edit-result-updated-document tab-remove-result))
                 :to-equal tab-source)
         (expect (document-text (edit-result-updated-document marker-result))
                 :to-equal (format nil "~%"))
         (expect (document-text (edit-result-updated-document blank-result))
                 :to-equal (format nil " ~C: ~%" #\Tab))))))


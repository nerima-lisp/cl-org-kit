;;;; t/store-link-test.lisp
(in-package #:cl-org-kit/test)

(defun store-link-test-headings (document)
  (let ((headings '()))
    (map-nodes (lambda (node)
                 (when (member (node-type node) '(:headline :inlinetask)
                               :test #'eq)
                   (push node headings)))
               (document-root document))
    (nreverse headings)))

(defun store-link-test-links (document)
  (let ((links nil))
    (map-nodes (lambda (node)
                 (when (eq (node-type node) :link)
                   (push node links)))
               (document-root document))
    (nreverse links)))

(defun store-link-test-expect-error (thunk reason)
  (let ((condition nil))
    (handler-case (funcall thunk)
      (org-kit::org-store-link-error (caught)
        (setf condition caught)))
    (with-soft-assertions
      (expect (not (null condition)) :to-be t)
      (when condition
        (expect (org-kit::org-store-link-error-reason condition) :to-be reason)
        (expect (stringp (org-kit::org-store-link-error-context condition))
                :to-be t)))))

(describe "store-link"
  (it "stores local ID, CUSTOM_ID, and heading links in exact Org syntax"
    (let* ((document
             (parse-org
              (format nil
                      "* Heading~%:PROPERTIES:~%:ID: stable-id~%:CUSTOM_ID: local-anchor~%:END:~%")))
           (heading (first (store-link-test-headings document))))
      (with-soft-assertions
        (expect (org-kit:store-link heading) :to-equal "[[id:stable-id]]")
        (expect (org-kit:store-link heading :style :custom-id)
                :to-equal "[[#local-anchor]]")
        (expect (org-kit:store-link heading :style :heading)
                :to-equal "[[*Heading]]"))))

  (it "escapes bracket and backslash characters in generated paths"
    (let* ((document
             (parse-org
              (format nil
                      "* A [target]~%:PROPERTIES:~%:ID: id[part]\\tail~%:CUSTOM_ID: anchor[part]\\tail~%:END:~%")))
           (heading (first (store-link-test-headings document))))
      (with-soft-assertions
        (expect (org-kit:store-link heading)
                :to-equal "[[id:id\\[part\\]\\\\tail]]")
        (expect (org-kit:store-link heading :style :custom-id)
                :to-equal "[[#anchor\\[part\\]\\\\tail]]")
        (expect (org-kit:store-link heading :style :heading)
                :to-equal "[[*A \\[target\\]]]"))))

  (it "does not fall back when the selected property is absent"
    (let* ((document
             (parse-org
              (format nil
                      "* Heading~%:PROPERTIES:~%:CUSTOM_ID: local-anchor~%:END:~%")))
           (heading (first (store-link-test-headings document))))
      (store-link-test-expect-error
       (lambda () (org-kit:store-link heading :style :id))
       :missing-property)))

  (it "reports duplicate or empty local properties"
    (let* ((duplicate-document
             (parse-org
              (format nil
                      "* Heading~%:PROPERTIES:~%:ID: first~%:ID: second~%:END:~%")))
           (empty-document
             (parse-org
              (format nil "* Heading~%:PROPERTIES:~%:ID:~%:END:~%")))
           (duplicate-heading
             (first (store-link-test-headings duplicate-document)))
           (empty-heading (first (store-link-test-headings empty-document))))
      (store-link-test-expect-error
       (lambda () (org-kit:store-link duplicate-heading))
       :ambiguous-property)
      (store-link-test-expect-error
       (lambda () (org-kit:store-link empty-heading))
       :invalid-property-value)))

  (it "reports invalid styles, targets, and empty heading titles"
    (let* ((document (parse-org (format nil "* Heading~%")))
           (heading (first (store-link-test-headings document)))
           (empty-title
             (first (store-link-test-headings
                     (parse-org (format nil "* ~%"))))))
      (store-link-test-expect-error
       (lambda () (org-kit:store-link heading :style :file))
       :invalid-style)
      (store-link-test-expect-error
       (lambda () (org-kit:store-link (make-node :paragraph)))
       :invalid-heading)
      (store-link-test-expect-error
       (lambda () (org-kit:store-link empty-title :style :heading))
       :missing-heading-title)))

  (it "emits the same fuzzy target syntax for headings with duplicate titles"
    (let* ((document (parse-org (format nil "* Same~%* Same~%")))
           (headings (store-link-test-headings document)))
      (with-soft-assertions
        (expect (org-kit:store-link (first headings) :style :heading)
                :to-equal "[[*Same]]")
        (expect (org-kit:store-link (second headings) :style :heading)
                :to-equal "[[*Same]]"))))

  (it "resolves a stored heading link back to its heading"
    (let* ((document (parse-org (format nil "* Round trip~%")))
           (heading (first (store-link-test-headings document)))
           (stored-link (org-kit:store-link heading :style :heading))
           (parsed-link (first (store-link-test-links (parse-org stored-link))))
           (resolution (resolve-link parsed-link document)))
      (with-soft-assertions
        (expect stored-link :to-equal "[[*Round trip]]")
        (expect (link-resolution-status resolution) :to-be :resolved)
        (expect (link-resolution-target resolution) :to-be heading)))))

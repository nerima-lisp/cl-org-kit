;;;; t/query-types-test.lisp
(in-package #:cl-org-kit/test)

(defun query-types-test-headlines (document)
  (let ((headlines '()))
    (map-nodes (lambda (node)
                 (when (eq (node-type node) :headline)
                   (push node headlines)))
               (document-root document))
    (nreverse headlines)))

(describe "query contexts and heading paths"
  (it "copies caller tag groups and returns fresh copies"
    (let* ((groups (list (list :group "State"
                               :members (list "open" "closed"))))
           (context (org-kit::make-query-context :tag-groups groups))
           (returned (org-kit::query-context-tag-groups context)))
      (setf (getf (first groups) :group) "caller change"
            (getf (first returned) :group) "returned change")
      (with-soft-assertions
        (expect (org-kit::query-context-p context) :to-be t)
        (expect (org-kit::query-context-tag-groups context)
                :to-equal
                '((:group "State" :members ("open" "closed"))))
        (expect (org-kit::query-context-property-inheritance-p context)
                :to-be nil)
        (expect (org-kit::query-context-tag-inheritance-p context)
                :to-be t))))

  (it "rejects dotted, circular, and over-deep caller tag groups"
    (let ((cycle (list :group "Loop" :members '("x"))))
      (setf (cdr (last cycle)) cycle)
      (with-soft-assertions
        (signals type-error
          (org-kit::make-query-context :tag-groups (list (cons :group :dotted))))
        (signals type-error
          (org-kit::make-query-context :tag-groups (list cycle)))
        (let ((deep 'leaf))
          (loop repeat 520
                do
            (setf deep (list deep)))
          (signals org-resource-limit-error
            (org-kit::make-query-context :tag-groups (list deep)))))))

  (it "returns root-to-leaf titles with duplicate sibling ordinals"
    (let* ((document
            (parse-org
             (format nil "* Root~%** Duplicate~%** Duplicate~%* Other~%** Duplicate")))
           (headlines (query-types-test-headlines document))
           (path (org-kit::node-heading-path (third headlines))))
      (with-soft-assertions
        (expect path
                :to-equal
                '((:title "Root" :index 1)
                  (:title "Duplicate" :index 2)))
        (expect (org-kit::node-heading-path (fifth headlines))
                :to-equal
                '((:title "Other" :index 1)
                  (:title "Duplicate" :index 1)))))))

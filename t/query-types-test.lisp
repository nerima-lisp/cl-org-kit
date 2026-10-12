;;;; t/query-types-test.lisp
(in-package #:cl-org-kit/test)

(defun query-types-test-headlines (document)
  (let ((headlines '()))
    (map-nodes (lambda (node)
                 (when (eq (node-type node) :headline)
                   (push node headlines)))
               (document-root document))
    (nreverse headlines)))

(defun query-types-test-node-of-type (document type)
  (let ((found nil))
    (map-nodes (lambda (node)
                 (when (and (null found) (eq (node-type node) type))
                   (setf found node)))
               (document-root document))
    found))

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
        (expect (org-kit::query-context-p nil) :to-be nil)
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
          (org-kit::make-query-context :tag-groups (cons :group :dotted)))
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

  (it "returns every explicitly configured context value"
    (let* ((blocked-provider (lambda (headline)
                               (declare (ignore headline))
                               :blocked))
           (clock-sum-provider (lambda (headline)
                                 (declare (ignore headline))
                                 90))
           (clock-sum-today-provider (lambda (headline)
                                       (declare (ignore headline))
                                       30))
           (context
             (org-kit::make-query-context
              :tag-groups nil
              :property-inheritance-p t
              :tag-inheritance-p nil
              :file "tasks.org"
              :category "Tasks"
              :blocked-provider blocked-provider
              :clock-sum-provider clock-sum-provider
              :clock-sum-today-provider clock-sum-today-provider
              :case-fold-p t)))
      (with-soft-assertions
        (expect (org-kit::query-context-tag-groups context) :to-equal nil)
        (expect (org-kit::query-context-property-inheritance-p context)
                :to-be t)
        (expect (org-kit::query-context-tag-inheritance-p context) :to-be nil)
        (expect (org-kit::query-context-file context) :to-equal "tasks.org")
        (expect (org-kit::query-context-category context) :to-equal "Tasks")
        (expect (eq (org-kit::query-context-blocked-provider context)
                    blocked-provider)
                :to-be t)
        (expect (eq (org-kit::query-context-clock-sum-provider context)
                    clock-sum-provider)
                :to-be t)
        (expect (eq (org-kit::query-context-clock-sum-today-provider context)
                    clock-sum-today-provider)
                :to-be t)
        (expect (org-kit::query-context-case-fold-p context) :to-be t))))

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
                  (:title "Duplicate" :index 1)))
        (expect (org-kit::node-heading-path (fourth headlines))
                :to-equal
                '((:title "Other" :index 1))))))

  (it "finds a parsed node's nearest headline and paths from non-headlines"
    (let* ((document (parse-org (format nil "* Parent~%Paragraph")))
           (paragraph (query-types-test-node-of-type document :paragraph))
           (headlines (query-types-test-headlines document)))
      (with-soft-assertions
        (expect (eq (org-kit::query-nearest-headline paragraph)
                    (first headlines))
                :to-be t)
        (expect (org-kit::query-nearest-headline (document-root document))
                :to-be nil)
        (expect (org-kit::node-heading-path paragraph)
                :to-equal
                '((:title "Parent" :index 1)))))))

;;;; t/agenda-category-test.lisp
(in-package #:cl-org-kit/test)

(defun agenda-category-test-heading (document title)
  (let ((found nil))
    (map-nodes (lambda (node)
                 (when (and (null found)
                            (eq (node-type node) :headline)
                            (string= title (node-property node :raw-value "")))
                   (setf found node)))
               (document-root document))
    found))

(describe "Agenda heading category"
  (it "prefers the nearest inherited CATEGORY property"
    (let* ((document
             (parse-org
              (format nil
                      "#+CATEGORY: DocumentCategory~%* Parent~%:PROPERTIES:~%:CATEGORY: ParentCategory~%:END:~%** Inherited~%** Local~%:PROPERTIES:~%:CATEGORY: LocalCategory~%:END:~%* DocumentFallback")
              :source-name "source.org"))
           (parent (agenda-category-test-heading document "Parent"))
           (inherited (agenda-category-test-heading document "Inherited"))
           (local (agenda-category-test-heading document "Local"))
           (document-fallback
             (agenda-category-test-heading document "DocumentFallback")))
      (with-soft-assertions
        (expect (org-kit::agenda-heading-category document parent)
                :to-equal "ParentCategory")
        (expect (org-kit::agenda-heading-category document inherited)
                :to-equal "ParentCategory")
        (expect (org-kit::agenda-heading-category document local)
                :to-equal "LocalCategory")
        (expect (org-kit::agenda-heading-category document document-fallback)
                :to-equal "DocumentCategory"))))

  (it "uses the source basename when no CATEGORY setting applies"
    (let* ((document (parse-org "* Entry" :source-name "archive.org"))
           (heading (agenda-category-test-heading document "Entry")))
      (expect (org-kit::agenda-heading-category document heading)
              :to-equal "archive")))

  (it "uses the source basename without an extension"
    (let* ((document (parse-org "* Entry" :source-name "archive"))
           (heading (agenda-category-test-heading document "Entry")))
      (expect (org-kit::agenda-heading-category document heading)
              :to-equal "archive")))

  (it "returns Org's unknown category when no fallback is available"
    (let* ((document (parse-org "* Entry" :source-name "folder/"))
           (heading (agenda-category-test-heading document "Entry")))
      (expect (org-kit::agenda-heading-category document heading)
              :to-equal "???"))

    (let* ((document (parse-org "* Entry"))
           (heading (agenda-category-test-heading document "Entry")))
      (expect (org-kit::agenda-heading-category document heading)
              :to-equal "???")))
  (it "copies effective category to agenda entries"
    (let* ((document
             (parse-org
              (format nil
                      "#+CATEGORY: Project~%* TODO Task~%DEADLINE: <2025-01-01 Wed>~%")))
           (date (make-org-time-value "2025-01-01"))
           (context
             (make-time-context
              :reference-time (make-org-time-value "2025-01-01T12:00:00")
              :zone (resolve-time-zone "UTC")))
           (entries
             (agenda-result-entries
              (agenda (list document) date date :time-context context))))
      (expect (mapcar (function agenda-entry-category) entries)
              :to-equal (list "Project"))))
  (it "uses programmatic heading categories and honors explicit overrides"
  (let* ((document
           (parse-org
            (format nil "#+CATEGORY: DocumentCategory~%* Parent~%** Child")))
         (parent (agenda-category-test-heading document "Parent"))
         (child (agenda-category-test-heading document "Child")))
    (setf (node-property parent :CATEGORY) "ProgrammaticParent")
    (with-soft-assertions
      (expect (org-kit::agenda-heading-category document parent)
              :to-equal "ProgrammaticParent")
      (expect (org-kit::agenda-heading-category document child)
              :to-equal "ProgrammaticParent")
      (setf (node-property child :CATEGORY) "ProgrammaticChild")
      (expect (org-kit::agenda-heading-category document child)
              :to-equal "ProgrammaticChild")
      (expect (org-kit::org-effective-category
               document child :category-override "ContextCategory")
              :to-equal "ContextCategory")))))

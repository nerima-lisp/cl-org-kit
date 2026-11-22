;;;; t/edit-planning-oracle-test.lisp
(in-package #:cl-org-kit/test)

(defun edit-planning-oracle-test-cases ()
  (let ((path (asdf:system-relative-pathname
               "cl-org-kit" "t/fixtures/e/oracle/planning/cases.sexp"))
        (*read-eval* nil))
    (with-open-file (stream path :direction :input :external-format :utf-8)
      (getf (read stream) :cases))))

(defun edit-planning-oracle-test-heading (document)
  (let ((found nil))
    (map-nodes
     (lambda (node)
       (when (and (eq (node-type node) :headline)
                  (string= "one" (node-property node :raw-value)))
         (setf found node)))
     (document-root document))
    found))

(defun edit-planning-oracle-test-replay (case)
  "Run the kit operation that CASE records for Emacs and return the new text."
  (let* ((document (parse-org (getf case :source-input)))
         (heading (edit-planning-oracle-test-heading document))
         (operation (getf case :operation))
         (at "2026-11-22T14:20:00"))
    (document-text
     (ecase (first operation)
       (:todo
        (edit-result-updated-document
         (transition-heading-todo
          document heading (second operation)
          :at (make-org-time-value at)
          ;; Org keeps SCHEDULED on completion; the kit drops it unless asked.
          :preserve-scheduled-p t
          :time-context (make-time-context
                         :reference-time (make-org-time-value at)
                         :zone (resolve-time-zone "UTC")))))
       (:set (set-planning document heading (second operation)
                           (getf case :timestamp)))
       (:remove (remove-planning document heading (second operation)))))))

(describe "planning line edits against the Emacs oracle"
  (it "writes and removes planning keywords the way Org does"
    (let ((cases (edit-planning-oracle-test-cases)))
      (with-soft-assertions
        (expect (>= (length cases) 42) :to-be t)
        (dolist (case cases)
          (let ((name (getf case :name)))
            (expect (list name
                          (handler-case (edit-planning-oracle-test-replay case)
                            (org-edit-error (condition)
                              (list :edit-error
                                    (org-edit-error-reason condition)))))
                    :to-equal (list name (getf case :expected)))))))))

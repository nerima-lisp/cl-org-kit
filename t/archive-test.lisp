(in-package #:cl-org-kit/test)

(defun archive-test-heading (document title)
  (let ((found nil))
    (map-nodes
     (lambda (node)
       (when (and (eq (node-type node) :headline)
                  (string= (node-property node :raw-value) title))
         (setf found node)))
     (document-root document))
    found))

(defun archive-test-error (thunk)
  (handler-case (progn (funcall thunk) nil)
    (org-archive-error (condition) condition)))

(defun archive-test-time-context (reference)
  (make-time-context :reference-time (make-org-time-value reference)
                     :zone (resolve-time-zone "UTC")))

(describe "archive location and sibling edits"
  (it "uses inherited ARCHIVE before the document keyword and default"
    (let* ((document
             (parse-org
              (format nil
                      "#+ARCHIVE: document-%s.org::Document~%* Parent~%:PROPERTIES:~%:ARCHIVE: parent-%s.org::Parent~%:END:~%** Child~%")
              :source-name "notes/work.org"))
           (child (archive-test-heading document "Child")))
      (expect (resolve-archive-location document child :default-location
                                        "fallback-%s.org::Fallback")
              :to-equal '("notes/parent-work.org.org" . "Parent"))))

  (it "uses a local ARCHIVE property and expands %s in both location parts"
    (let* ((document
             (parse-org
              (format nil "#+ARCHIVE: document-%s.org::Document~%* Document~%")
              :source-name "notes/work.org"))
           (heading (archive-test-heading document "Document")))
      (setf document
            (set-property document heading "ARCHIVE" "local-%s.org::Box-%s::Section"))
      (expect
       (resolve-archive-location document
                                 (archive-test-heading document "Document"))
       :to-equal '("notes/local-work.org.org" . "Box-work.org::Section"))))

  (it "matches the default %s_archive:: location and first :: split"
    (let* ((document (parse-org (format nil "* Task~%")
                               :source-name #p"notes/work.org"))
           (heading (archive-test-heading document "Task")))
      (expect (resolve-archive-location document heading)
              :to-equal '("notes/work.org_archive" . ""))
      (expect (resolve-archive-location
               document heading :default-location "vault-%s.org::Top::Tasks")
              :to-equal '("notes/vault-work.org.org" . "Top::Tasks"))))
  (it "bounds pathname namestring conversion before allocation"
    (let* ((limits (make-limits :max-output-length 0))
           (document
             (parse-org (format nil "* Task~%")
                        :source-name (make-pathname :name :wild)
                        :limits limits))
           (condition
             (handler-case
                 (progn
                   (org-kit::archive-source-basename document limits)
                   nil)
               (org-resource-limit-error (condition) condition))))
      (expect (not (null condition)) :to-be t)
      (when condition
        (expect (org-resource-limit-error-limit-name condition)
                :to-equal :max-output-length)
        (expect (org-resource-limit-error-actual condition) :to-equal 1)))
    (let* ((source-name
             (make-pathname :directory (list :relative "notes")
                            :name "work" :type :unspecific))
           (limits (make-limits :max-output-length 9))
           (document
             (parse-org (format nil "* Task~%")
                        :source-name source-name
                        :limits limits))
           (condition
             (handler-case
                 (progn
                   (resolve-archive-location
                    document (archive-test-heading document "Task")
                    :default-location "::")
                   nil)
               (org-resource-limit-error (condition) condition))))
      (expect (not (null condition)) :to-be t)
      (when condition
        (expect (org-resource-limit-error-limit-name condition)
                :to-equal :max-output-length)
        (expect (org-resource-limit-error-actual condition) :to-equal 10)))
    (let* ((source-name
             (make-pathname :directory (list :relative "notes")
                            :name "work" :type :unspecific))
           (limits (make-limits :max-output-length 10))
           (document
             (parse-org (format nil "* Task~%")
                        :source-name source-name
                        :limits limits)))
      (expect (resolve-archive-location
               document (archive-test-heading document "Task")
               :default-location "::")
              :to-equal (cons "notes/work" ""))))

  (it "signals reasons for invalid locations and missing source names"
    (let* ((invalid-document (parse-org (format nil "#+ARCHIVE: nowhere~%* Task~%")))
           (invalid (archive-test-error
                     (lambda ()
                       (resolve-archive-location
                        invalid-document
                        (archive-test-heading invalid-document "Task")))))
           (missing-document (parse-org (format nil "* Task~%")))
           (missing (archive-test-error
                     (lambda ()
                       (resolve-archive-location
                        missing-document
                        (archive-test-heading missing-document "Task"))))))
      (with-soft-assertions
        (expect (typep invalid 'org-archive-error) :to-be t)
        (when (typep invalid 'org-archive-error)
          (expect (org-archive-error-reason invalid) :to-equal :invalid-location))
        (expect (typep missing 'org-archive-error) :to-be t)
        (when (typep missing 'org-archive-error)
          (expect (org-archive-error-reason missing)
                  :to-equal :source-name-required)))))

  (it "enforces the output limit across archive location expansion"
    (let* ((document
             (parse-org (format nil "* Task~%")
                        :source-name "x.org"
                        :limits (make-limits :max-output-length 8)))
           (condition
             (handler-case
                 (progn
                   (resolve-archive-location
                    document (archive-test-heading document "Task")
                    :default-location "%s::%s")
                   nil)
               (org-resource-limit-error (condition) condition))))
      (expect (typep condition 'org-resource-limit-error) :to-be t)
      (when (typep condition 'org-resource-limit-error)
        (expect (org-resource-limit-error-limit-name condition)
                :to-equal :max-output-length))))
  (it "bounds a relative archive path before merging"
    (let* ((document
             (parse-org (format nil "* Task~%")
                        :source-name "notes/work.org"
                        :limits (make-limits :max-output-length 15)))
           (condition
             (handler-case
                 (progn
                   (resolve-archive-location
                    document (archive-test-heading document "Task")
                    :default-location "archive.org::")
                   nil)
               (org-resource-limit-error (condition) condition))))
      (expect (typep condition 'org-resource-limit-error) :to-be t)
      (when (typep condition 'org-resource-limit-error)
        (expect (org-resource-limit-error-limit-name condition)
                :to-equal :max-output-length)
        (expect (org-resource-limit-error-actual condition)
                :to-equal 17))))

  (it "creates a tagged sibling and records the explicit archive time"
    (let* ((source-text
             (format nil "* Parent~%** Task~%body~%** Later~%"))
           (document (parse-org source-text))
           (heading (archive-test-heading document "Task"))
           (result (archive-to-sibling
                    document heading
                    :time-context (archive-test-time-context
                                   "2024-02-19T12:34:00")))
           (updated (edit-result-updated-document result)))
      (with-soft-assertions
        (expect (edit-result-p result) :to-be t)
        (expect (document-text document) :to-equal source-text)
        (expect (document-text updated)
                :to-equal
                (format nil
                        "* Parent~%** Later~%** Archive :ARCHIVE:~%*** Task~%:PROPERTIES:~%:ARCHIVE_TIME: 2024-02-19 Mon 12:34~%:END:~%body~%")))))

  (it "creates a root Archive sibling when one is missing"
    (let* ((source-text (format nil "* Task~%body~%"))
           (document (parse-org source-text))
           (result
             (archive-to-sibling
              document (archive-test-heading document "Task")
              :time-context (archive-test-time-context "2024-02-19T12:34:00")))
           (updated (edit-result-updated-document result)))
      (with-soft-assertions
        (expect (document-text document) :to-equal source-text)
        (expect (document-text updated)
                :to-equal
                (format nil
                        "* Archive :ARCHIVE:~%** Task~%:PROPERTIES:~%:ARCHIVE_TIME: 2024-02-19 Mon 12:34~%:END:~%body~%")))))

  (it "archives below an existing Archive sibling without tagging the task"
    (let* ((source-text
             (format nil "* Task~%body~%* Archive :ARCHIVE:~%old~%"))
           (document (parse-org source-text))
           (result
             (archive-to-sibling
              document (archive-test-heading document "Task")
              :time-context (archive-test-time-context "2024-02-19T12:34:00")))
           (updated (edit-result-updated-document result))
           (archived (archive-test-heading updated "Task")))
      (with-soft-assertions
        (expect (document-text updated)
                :to-equal
                (format nil
                        "* Archive :ARCHIVE:~%old~%** Task~%:PROPERTIES:~%:ARCHIVE_TIME: 2024-02-19 Mon 12:34~%:END:~%body~%"))
        (expect (find "ARCHIVE" (node-property archived :tags)
                       :test #'string=)
                :to-be nil)
        (expect (search ":ARCHIVE_TIME: 2024-02-19 Mon 12:34"
                        (document-text updated))
                :to-be-truthy))))

  (it "requires a time context instead of reading the current clock"
    (let* ((document (parse-org (format nil "* Task~%")))
           (condition
             (archive-test-error
              (lambda ()
                (archive-to-sibling document
                                    (archive-test-heading document "Task"))))))
      (expect (org-archive-error-reason condition)
              :to-equal :time-context-required))))

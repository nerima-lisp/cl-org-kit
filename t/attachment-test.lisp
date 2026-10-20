(in-package #:cl-org-kit/test)

(defun attachment-test-heading (document title)
  (let ((found nil))
    (map-nodes
     (lambda (node)
       (when (and (eq (node-type node) :headline)
                  (string= (node-property node :raw-value) title))
         (setf found node)))
     (document-root document))
    found))

(defun attachment-test-error (thunk)
  (handler-case (progn (funcall thunk) nil)
    (org-attachment-error (condition) condition)))

(defun attachment-test-id-document (&key (id "abcdef123") (limits (make-limits)))
  (parse-org
   (format nil "* Task~%:PROPERTIES:~%:ID: ~A~%:END:~%" id)
   :limits limits))

(describe "attachment directory resolution and plans"
  (it "uses DIR before ID and inherits DIR only with ATTACH_DIR_INHERIT"
    (let* ((document
             (parse-org
              (format nil
                      "* Parent~%:PROPERTIES:~%:DIR: parent-files~%:ATTACH_DIR_INHERIT: t~%:END:~%** Child~%:PROPERTIES:~%:ID: child-id~%:END:~%")))
           (child (attachment-test-heading document "Child")))
      (expect (resolve-attachment-directory document child)
              :to-equal "parent-files")
      (let* ((updated
               (set-property document child "DIR" "child-files"))
             (updated-child (attachment-test-heading updated "Child")))
        (expect (resolve-attachment-directory updated updated-child)
                :to-equal "child-files"))))

  (it "matches Org's default ID data path and short-ID fallback"
    (let* ((document (attachment-test-id-document))
           (heading (attachment-test-heading document "Task"))
           (short-document (attachment-test-id-document :id "x"))
           (short-heading (attachment-test-heading short-document "Task")))
      (with-soft-assertions
        (expect (resolve-attachment-directory document heading)
                :to-equal "data/ab/cdef123")
        (expect (resolve-attachment-directory short-document short-heading)
                :to-equal "data/__/x/x"))))

  (it "resolves an absolute DIR lexically below the source directory"
    (let* ((document
             (parse-org
              (format nil "* Task~%:PROPERTIES:~%:DIR: /vault/attachments~%:END:~%")
              :source-name "/vault/notes.org"))
           (heading (attachment-test-heading document "Task")))
      (expect (resolve-attachment-directory document heading)
              :to-equal "attachments")))

  (it "rejects absolute DIR paths outside the source directory"
    (let* ((document
             (parse-org
              (format nil "* Task~%:PROPERTIES:~%:DIR: /outside/attachments~%:END:~%")
              :source-name "/vault/notes.org"))
           (condition
             (attachment-test-error
              (lambda ()
                (resolve-attachment-directory
                 document (attachment-test-heading document "Task"))))))
      (expect (typep condition 'org-attachment-error) :to-be t)
      (when (typep condition 'org-attachment-error)
        (expect (org-attachment-error-reason condition)
                :to-equal :path-outside-source-directory))))

  (it "checks absolute path and source-root byte limits before splitting paths"
    (let* ((limits (make-limits :max-write-plan-path-bytes 8))
           (input-document
             (parse-org
              (format nil
                      "* Task~%:PROPERTIES:~%:DIR: /vault/../outside~%:END:~%")
              :source-name "/vault/notes.org" :limits limits))
           (input-error
             (handler-case
                 (progn
                   (resolve-attachment-directory
                    input-document
                    (attachment-test-heading input-document "Task"))
                   nil)
               (condition (condition) condition)))
           (root-document
             (parse-org
              (format nil
                      "* Task~%:PROPERTIES:~%:DIR: /root/x~%:END:~%")
              :limits limits))
           (root-error
             (handler-case
                 (progn
                   (resolve-attachment-directory
                    root-document
                    (attachment-test-heading root-document "Task")
                    :source-directory "/../root/long-root")
                   nil)
               (condition (condition) condition))))
      (with-soft-assertions
        (expect (typep input-error 'org-resource-limit-error) :to-be t)
        (when (typep input-error 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name input-error)
                  :to-equal :max-write-plan-path-bytes))
        (expect (typep root-error 'org-resource-limit-error) :to-be t)
        (when (typep root-error 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name root-error)
                  :to-equal :max-write-plan-path-bytes)))))

  (it "signals when no attachment path exists and rejects traversal"
    (let* ((missing-document (parse-org (format nil "* Task~%")))
           (missing
             (attachment-test-error
              (lambda ()
                (resolve-attachment-directory
                 missing-document
                 (attachment-test-heading missing-document "Task")))))
           (invalid-document
             (parse-org
              (format nil "* Task~%:PROPERTIES:~%:DIR: ../escape~%:END:~%")))
           (invalid
             (attachment-test-error
              (lambda ()
                (resolve-attachment-directory
                 invalid-document
                 (attachment-test-heading invalid-document "Task"))))))
      (with-soft-assertions
        (expect (typep missing 'org-attachment-error) :to-be t)
        (when (typep missing 'org-attachment-error)
          (expect (org-attachment-error-reason missing)
                  :to-equal :missing-attachment-location))
        (expect (typep invalid 'org-attachment-error) :to-be t)
        (when (typep invalid 'org-attachment-error)
          (expect (org-attachment-error-reason invalid)
                  :to-equal :invalid-path)))))

  (it "returns write and one-file delete plans without applying them"
    (let* ((document (attachment-test-id-document))
           (heading (attachment-test-heading document "Task"))
           (write-plan
             (add-attachment document heading "readme.txt" "attachment body"))
           (write-entry (first (coerce (write-plan-entries write-plan) 'list)))
           (delete-plan
             (delete-attachment document heading "readme.txt"))
           (delete-entry (first (coerce (write-plan-entries delete-plan) 'list))))
      (with-soft-assertions
        (expect (write-plan-p write-plan) :to-be t)
        (expect (write-plan-entry-operation write-entry) :to-be :write)
        (expect (write-plan-entry-path write-entry)
                :to-equal "data/ab/cdef123/readme.txt")
        (expect (write-plan-entry-content write-entry)
                :to-equal "attachment body")
        (expect (write-plan-entry-create-directories-p write-entry)
                :to-be t)
        (expect (write-plan-entry-operation delete-entry) :to-be :delete)
        (expect (write-plan-entry-path delete-entry)
                :to-equal "data/ab/cdef123/readme.txt")
        (expect (write-plan-entry-content delete-entry) :to-be nil))))

  (it "rejects invalid filenames and enforces path and content limits"
    (let* ((document (attachment-test-id-document))
           (heading (attachment-test-heading document "Task"))
           (filename-error
             (attachment-test-error
              (lambda () (add-attachment document heading "../escape" "x"))))
           (path-document
             (attachment-test-id-document
              :limits (make-limits :max-write-plan-path-depth 3)))
           (path-error
             (handler-case
                 (progn
                   (add-attachment
                    path-document (attachment-test-heading path-document "Task")
                    "file.txt" "x")
                   nil)
               (org-resource-limit-error (condition) condition)))
           (content-document
             (attachment-test-id-document
              :limits (make-limits :max-write-plan-bytes 2)))
           (content-error
             (handler-case
                 (progn
                   (add-attachment
                    content-document
                    (attachment-test-heading content-document "Task")
                    "file.txt" "long")
                   nil)
               (org-resource-limit-error (condition) condition))))
      (with-soft-assertions
        (expect (typep filename-error 'org-attachment-error) :to-be t)
        (when (typep filename-error 'org-attachment-error)
          (expect (org-attachment-error-reason filename-error)
                  :to-equal :invalid-filename))
        (expect (typep path-error 'org-resource-limit-error) :to-be t)
        (when (typep path-error 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name path-error)
                  :to-equal :max-write-plan-path-depth))
        (expect (typep content-error 'org-resource-limit-error) :to-be t)
        (when (typep content-error 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name content-error)
                  :to-equal :max-write-plan-bytes)))))
)

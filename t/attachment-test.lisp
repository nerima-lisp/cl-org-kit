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
  (it "uses ATTACH_DIR before ID and honors inheritance settings"
      (let* ((inherited-document
              (parse-org
               (format nil
                       "* Parent~%:PROPERTIES:~%:ATTACH_DIR: parent-files~%:ATTACH_DIR_INHERIT: yes~%:END:~%** Child~%:PROPERTIES:~%:ID: child-id~%:END:~%")))
             (inherited-child (attachment-test-heading inherited-document "Child"))
             (local-document
              (parse-org
               (format nil
                       "* Task~%:PROPERTIES:~%:ID: abcdef123~%:ATTACH_DIR: local-files~%:END:~%")))
             (local-heading (attachment-test-heading local-document "Task"))
             (disabled-document
              (parse-org
               (format nil
                       "* Parent~%:PROPERTIES:~%:ATTACH_DIR: ignored-files~%:ATTACH_DIR_INHERIT: false~%:END:~%** Child~%:PROPERTIES:~%:ID: child-id~%:END:~%")))
             (disabled-child (attachment-test-heading disabled-document "Child")))
        (with-soft-assertions
         (expect (resolve-attachment-directory inherited-document inherited-child)
                 :to-equal "parent-files")
         (expect (resolve-attachment-directory local-document local-heading)
                 :to-equal "local-files")
         (expect (resolve-attachment-directory disabled-document disabled-child)
                 :to-equal "data/ch/ild-id"))))
  (it "normalizes absolute attachment paths and rejects unsafe roots"
      (let* ((normalized-document
              (parse-org
               (format nil
                       "* Task~%:PROPERTIES:~%:DIR: /vault/./notes/../attachments/~%:END:~%")
               :source-name "/vault/notes.org"))
             (normalized-heading (attachment-test-heading normalized-document "Task"))
             (root-document
              (parse-org
               (format nil "* Task~%:PROPERTIES:~%:DIR: /vault~%:END:~%")
               :source-name "/vault/notes.org"))
             (root-heading (attachment-test-heading root-document "Task"))
             (provided-root-document
              (parse-org
               (format nil "* Task~%:PROPERTIES:~%:DIR: /vault/files~%:END:~%")))
             (provided-root-heading
              (attachment-test-heading provided-root-document "Task"))
             (missing-root-document
              (parse-org
               (format nil "* Task~%:PROPERTIES:~%:DIR: /vault/files~%:END:~%")))
             (missing-root
              (attachment-test-error
               (lambda ()
                 (resolve-attachment-directory
                  missing-root-document
                  (attachment-test-heading missing-root-document "Task")))))
             (escape-document
              (parse-org
               (format nil "* Task~%:PROPERTIES:~%:DIR: /../outside~%:END:~%")
               :source-name "/vault/notes.org"))
             (escape
              (attachment-test-error
               (lambda ()
                 (resolve-attachment-directory
                  escape-document (attachment-test-heading escape-document "Task")))))
             (backslash-document
              (parse-org
               (format nil "* Task~%:PROPERTIES:~%:DIR: /vault\\files~%:END:~%")
               :source-name "/vault/notes.org"))
             (backslash
              (attachment-test-error
               (lambda ()
                 (resolve-attachment-directory
                  backslash-document
                  (attachment-test-heading backslash-document "Task")))))
             (relative-backslash-document
              (parse-org
               (format nil "* Task~%:PROPERTIES:~%:DIR: bad\\path~%:END:~%")))
             (relative-backslash
              (attachment-test-error
               (lambda ()
                 (resolve-attachment-directory
                  relative-backslash-document
                  (attachment-test-heading relative-backslash-document "Task"))))))
        (with-soft-assertions
         (expect (resolve-attachment-directory normalized-document normalized-heading)
                 :to-equal "attachments")
         (expect (resolve-attachment-directory root-document root-heading)
                 :to-equal "")
         (expect (resolve-attachment-directory
                  provided-root-document provided-root-heading
                  :source-directory #p"/vault")
                 :to-equal "files")
         (expect (org-attachment-error-reason missing-root)
                 :to-equal :absolute-path-requires-source-directory)
         (expect (org-attachment-error-reason escape)
                 :to-equal :invalid-path)
         (expect (org-attachment-error-reason backslash)
                 :to-equal :invalid-path)
         (expect (org-attachment-error-reason relative-backslash)
                 :to-equal :invalid-path))))
  (it "counts UTF-8 path bytes and validates the source directory"
      (let* ((limits (make-limits :max-write-plan-path-bytes 15))
             (unicode-document
              (parse-org
               (format nil
                       "* Task~%:PROPERTIES:~%:DIR: /vault/é☕🧪~%:END:~%")
               :source-name "/vault/notes.org" :limits limits))
             (unicode-error
              (handler-case
                  (progn
                    (resolve-attachment-directory
                     unicode-document
                     (attachment-test-heading unicode-document "Task"))
                    nil)
                (org-resource-limit-error (condition) condition)))
             (source-document (attachment-test-id-document))
             (invalid-source-directory
              (attachment-test-error
               (lambda ()
                 (resolve-attachment-directory
                  source-document (attachment-test-heading source-document "Task")
                  :source-directory 42)))))
        (with-soft-assertions
         (expect (typep unicode-error 'org-resource-limit-error) :to-be t)
         (when (typep unicode-error 'org-resource-limit-error)
           (expect (org-resource-limit-error-actual unicode-error) :to-equal 16))
         (expect (org-attachment-error-reason invalid-source-directory)
                 :to-equal :invalid-source-directory))))
  (it "returns binary plans and validates ID-based roots"
      (let* ((directory-document
              (parse-org
               (format nil "* Task~%:PROPERTIES:~%:DIR: .~%:END:~%")))
             (directory-heading (attachment-test-heading directory-document "Task"))
             (bytes (make-array 3 :element-type '(unsigned-byte 8)
                                :initial-contents '(0 127 255)))
             (write-plan
              (add-attachment directory-document directory-heading "binary.dat" bytes))
             (entry (first (coerce (write-plan-entries write-plan) 'list)))
             (id-document (attachment-test-id-document))
             (id-heading (attachment-test-heading id-document "Task"))
             (unsafe-id-document (attachment-test-id-document :id "../escape"))
             (unsafe-id
              (attachment-test-error
               (lambda ()
                 (resolve-attachment-directory
                  unsafe-id-document
                  (attachment-test-heading unsafe-id-document "Task")))))
             (empty-root
              (resolve-attachment-directory id-document id-heading :attachment-root ""))
             (pathname-root
              (resolve-attachment-directory id-document id-heading
                                            :attachment-root #p"attachments"))
             (invalid-root
              (attachment-test-error
               (lambda ()
                 (resolve-attachment-directory id-document id-heading
                                               :attachment-root 42))))
             (empty-directory-document
              (parse-org
               (format nil "* Task~%:PROPERTIES:~%:DIR: ~%:END:~%")))
             (empty-directory
              (attachment-test-error
               (lambda ()
                 (resolve-attachment-directory
                  empty-directory-document
                  (attachment-test-heading empty-directory-document "Task")))))
             (invalid-document
              (attachment-test-error
               (lambda () (resolve-attachment-directory nil nil))))
             (invalid-add-document
              (attachment-test-error
               (lambda () (add-attachment nil nil "file.txt" "content"))))
             (invalid-delete-document
              (attachment-test-error
               (lambda () (delete-attachment nil nil "file.txt"))))
             (invalid-content
              (handler-case
                  (progn (add-attachment directory-document directory-heading
                                         "bad.dat" 42)
                    nil)
                (type-error (condition) condition))))
        (with-soft-assertions
         (expect (write-plan-entry-path entry) :to-equal "binary.dat")
         (expect (coerce (write-plan-entry-content entry) 'list)
                 :to-equal '(0 127 255))
         (expect empty-root :to-equal "ab/cdef123")
         (expect pathname-root :to-equal "attachments/ab/cdef123")
         (expect (org-attachment-error-reason unsafe-id) :to-equal :invalid-id)
         (expect (org-attachment-error-reason invalid-root)
                 :to-equal :invalid-attachment-root)
         (expect (org-attachment-error-reason empty-directory)
                 :to-equal :invalid-path)
         (expect (org-attachment-error-reason invalid-document)
                 :to-equal :invalid-document)
         (expect (org-attachment-error-reason invalid-add-document)
                 :to-equal :invalid-document)
         (expect (org-attachment-error-reason invalid-delete-document)
                 :to-equal :invalid-document)
         (expect (typep invalid-content 'type-error) :to-be t))))
  (it "rejects invalid source roots, control characters, and empty IDs"
      (let* ((document
              (parse-org
               (format nil "* Task~%:PROPERTIES:~%:DIR: /vault/files~%:END:~%")))
             (heading (attachment-test-heading document "Task"))
             (relative-root-error
              (attachment-test-error
               (lambda ()
                 (resolve-attachment-directory
                  document heading :source-directory "vault"))))
             (null-root-error
              (attachment-test-error
               (lambda ()
                 (resolve-attachment-directory
                  document heading
                  :source-directory (format nil "/vault/bad~Croot" #\Null)))))
             (newline-root-error
              (attachment-test-error
               (lambda ()
                 (resolve-attachment-directory
                  document heading
                  :source-directory (format nil "/vault/bad~Croot" #\Newline)))))
             (return-root-error
              (attachment-test-error
               (lambda ()
                 (resolve-attachment-directory
                  document heading
                  :source-directory (format nil "/vault/bad~Croot" #\Return)))))
             (empty-id-document (attachment-test-id-document :id ""))
             (empty-id-error
              (attachment-test-error
               (lambda ()
                 (resolve-attachment-directory
                  empty-id-document
                  (attachment-test-heading empty-id-document "Task"))))))
        (with-soft-assertions
         (expect (org-attachment-error-reason relative-root-error)
                 :to-equal :invalid-path)
         (expect (org-attachment-error-reason null-root-error)
                 :to-equal :invalid-path)
         (expect (org-attachment-error-reason newline-root-error)
                 :to-equal :invalid-path)
         (expect (org-attachment-error-reason return-root-error)
                 :to-equal :invalid-path)
         (expect (org-attachment-error-reason empty-id-error)
                 :to-equal :invalid-id)))))

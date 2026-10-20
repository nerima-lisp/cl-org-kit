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
  (it "uses a parent heading ARCHIVE before the document keyword and default"
    (let* ((document
             (parse-org
              (format nil
                      "#+ARCHIVE: document-%s.org::Document~%* Parent~%:PROPERTIES:~%:ARCHIVE: parent-%s.org::Parent~%:END:~%** Child~%")
              :source-name "notes/work.org"))
           (child (archive-test-heading document "Child")))
      (expect (resolve-archive-location document child :default-location
                                        "fallback-%s.org::Fallback")
              :to-equal '("notes/parent-work.org.org" . "Parent"))))

  (it "expands each %s in local ARCHIVE values to the source basename and extension"
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

  (it "handles source-path conversion types and empty-file errors"
    (let* ((string-document
             (parse-org (format nil "* Task~%") :source-name "notes/work.org"))
           (path-document
             (parse-org (format nil "* Task~%") :source-name #p"notes/work.org"))
           (missing-document (parse-org (format nil "* Task~%")))
           (invalid-document
             (parse-org (format nil "* Task~%") :source-name 42))
           (string-path (org-kit::archive-document-source-path string-document))
           (pathname-path (org-kit::archive-document-source-path path-document))
           (missing-path (org-kit::archive-document-source-path missing-document))
           (invalid-path
             (archive-test-error
              (lambda () (org-kit::archive-document-source-path invalid-document))))
           (empty-file-error
             (archive-test-error
              (lambda ()
                (resolve-archive-location
                 invalid-document (archive-test-heading invalid-document "Task")
                 :default-location "::")))))
      (with-soft-assertions
        (expect string-path :to-equal "notes/work.org")
        (expect pathname-path :to-equal "notes/work.org")
        (expect missing-path :to-be nil)
        (expect (org-archive-error-reason invalid-path)
                :to-equal :invalid-source-name)
        (expect (org-archive-error-reason empty-file-error)
                :to-equal :invalid-source-name))))

  (it "bounds wildcard and unspecific pathname components before conversion"
    (let* ((limits (make-limits :max-output-length 2))
           (wild-document
             (parse-org
              (format nil "* Task~%")
              :source-name
              (make-pathname :directory '(:relative :wild)
                             :name "work" :type "org")
              :limits limits))
           (unspecific-document
             (parse-org
              (format nil "* Task~%")
              :source-name (make-pathname :name :unspecific :type "org")
              :limits limits))
           (wild-condition
             (handler-case
                 (progn
                   (resolve-archive-location
                    wild-document (archive-test-heading wild-document "Task")
                    :default-location "::")
                   nil)
               (org-resource-limit-error (condition) condition)))
           (unspecific-condition
             (handler-case
                 (progn
                   (resolve-archive-location
                    unspecific-document
                    (archive-test-heading unspecific-document "Task")
                    :default-location "::")
                   nil)
               (org-resource-limit-error (condition) condition)))
           (basename-limits (make-limits :max-output-length 0))
           (basename-document
             (parse-org
              (format nil "* Task~%")
              :source-name (make-pathname :name :unspecific :type "org")
              :limits basename-limits))
           (basename-condition
             (handler-case
                 (progn
                   (org-kit::archive-source-basename
                    basename-document basename-limits)
                   nil)
               (org-resource-limit-error (condition) condition))))
      (with-soft-assertions
        (expect (typep wild-condition 'org-resource-limit-error) :to-be t)
        (expect (typep unspecific-condition 'org-resource-limit-error) :to-be t)
        (expect (typep basename-condition 'org-resource-limit-error) :to-be t))))

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
                :to-be nil))))

  (it "requires a time context instead of reading the current clock"
    (let* ((document (parse-org (format nil "* Task~%")))
           (condition
             (archive-test-error
              (lambda ()
                (archive-to-sibling document
                                    (archive-test-heading document "Task"))))))
      (expect (org-archive-error-reason condition)
              :to-equal :time-context-required)))
  (it "expands escaped archive markers and rejects malformed formats"
      (let* ((document
              (parse-org (format nil "* Task~%") :source-name "notes/work.org"))
             (heading (archive-test-heading document "Task"))
             (expanded
              (resolve-archive-location
               document heading
               :default-location "archive%%-%s.org::Section%%-%s"))
             (trailing
              (archive-test-error
               (lambda ()
                 (resolve-archive-location
                  document heading :default-location "archive%::"))))
             (unsupported
              (archive-test-error
               (lambda ()
                 (resolve-archive-location
                  document heading :default-location "archive%q::")))))
        (with-soft-assertions
         (expect expanded
                 :to-equal '("notes/archive%-work.org.org"
                             . "Section%-work.org"))
         (expect (org-archive-error-reason trailing)
                 :to-equal :invalid-location-format)
         (expect (org-archive-error-reason unsupported)
                 :to-equal :invalid-location-format))))
  (it "resolves empty, relative, and absolute archive destinations"
      (let* ((source-document
              (parse-org (format nil "* Task~%") :source-name "notes/work.org"))
             (source-heading (archive-test-heading source-document "Task"))
             (empty-file
              (resolve-archive-location
               source-document source-heading :default-location "::"))
             (no-source (parse-org (format nil "* Task~%")))
             (no-source-heading (archive-test-heading no-source "Task"))
             (empty-without-source
              (resolve-archive-location
               no-source no-source-heading :default-location "::"))
             (relative
              (resolve-archive-location
               no-source no-source-heading
               :default-location "archive.org::Top"))
             (absolute
              (resolve-archive-location
               no-source no-source-heading
               :default-location "/vault/archive.org::Top"))
             (without-source-format
              (archive-test-error
               (lambda ()
                 (resolve-archive-location
                  no-source no-source-heading :default-location "%s::"))))
             (invalid-source (parse-org (format nil "* Task~%") :source-name 42))
             (invalid-source-heading (archive-test-heading invalid-source "Task"))
             (invalid-format-source
              (archive-test-error
               (lambda ()
                 (resolve-archive-location
                  invalid-source invalid-source-heading :default-location "%s::"))))
             (invalid-relative-source
              (archive-test-error
               (lambda ()
                 (resolve-archive-location
                  invalid-source invalid-source-heading
                  :default-location "archive.org::")))))
        (with-soft-assertions
         (expect empty-file :to-equal '("notes/work.org" . ""))
         (expect empty-without-source :to-equal '(nil . ""))
         (expect relative :to-equal '("archive.org" . "Top"))
         (expect absolute :to-equal '("/vault/archive.org" . "Top"))
         (expect (org-archive-error-reason without-source-format)
                 :to-equal :source-name-required)
         (expect (org-archive-error-reason invalid-format-source)
                 :to-equal :invalid-source-name)
         (expect (org-archive-error-reason invalid-relative-source)
                 :to-equal :invalid-source-name))))
  (it "rejects invalid archive documents, locations, and time contexts"
      (let* ((document (parse-org (format nil "* Task~%")))
             (heading (archive-test-heading document "Task"))
             (invalid-document
              (archive-test-error
               (lambda () (resolve-archive-location nil nil))))
             (invalid-location
              (archive-test-error
               (lambda ()
                 (resolve-archive-location document heading :default-location 42))))
             (invalid-time-context
              (handler-case
                  (progn
                    (archive-to-sibling document heading :time-context 42)
                    nil)
                (type-error (condition) condition))))
        (with-soft-assertions
         (expect (org-archive-error-reason invalid-document)
                 :to-equal :invalid-document)
         (expect (org-archive-error-reason invalid-location)
                 :to-equal :invalid-location)
         (expect (typep invalid-time-context 'type-error) :to-be t))))
  (it "rejects invalid edit documents and logical source paths"
      (let* ((invalid-edit-document
              (archive-test-error
               (lambda ()
                 (archive-to-sibling
                  nil nil
                  :time-context (archive-test-time-context "2024-02-19T12:34:00")))))
             (logical-source
              (progn
                (setf (logical-pathname-translations "ORG-KIT-TEST") nil)
                (make-pathname :host "ORG-KIT-TEST"
                               :directory (list :absolute "vault")
                               :name "work" :type "org")))
             (logical-path-document
              (parse-org (format nil "* Task~%") :source-name logical-source))
             (logical-string-document
              (parse-org (format nil "* Task~%")
                         :source-name (namestring logical-source)))
             (logical-heading
              (archive-test-heading logical-path-document "Task"))
             (logical-string-heading
              (archive-test-heading logical-string-document "Task"))
             (logical-basename-error
              (archive-test-error
               (lambda ()
                 (resolve-archive-location
                  logical-path-document logical-heading :default-location "%s::"))))
             (logical-path-error
              (archive-test-error
               (lambda ()
                 (resolve-archive-location
                  logical-path-document logical-heading
                  :default-location "archive.org::"))))
             (logical-string-error
              (archive-test-error
               (lambda ()
                 (resolve-archive-location
                  logical-string-document logical-string-heading
                  :default-location "archive.org::")))))
        (with-soft-assertions
         (expect (org-archive-error-reason invalid-edit-document)
                 :to-equal :invalid-document)
         (expect (org-archive-error-reason logical-basename-error)
                 :to-equal :invalid-source-name)
         (expect (org-archive-error-reason logical-path-error)
                 :to-equal :invalid-source-name)
         (expect (org-archive-error-reason logical-string-error)
                 :to-equal :invalid-source-name))))
  (it "resolves directory and parent-relative source paths"
      (let* ((directory-source
              (make-pathname :directory '(:relative "notes")))
             (directory-document
              (parse-org (format nil "* Task~%") :source-name directory-source))
             (directory-heading (archive-test-heading directory-document "Task"))
             (directory-basename-error
              (archive-test-error
               (lambda ()
                 (resolve-archive-location
                  directory-document directory-heading :default-location "%s::"))))
             (directory-location
              (resolve-archive-location
               directory-document directory-heading :default-location "archive.org::"))
             (parent-source
              (make-pathname :directory '(:relative :up "notes")
                             :name "work" :type "org"))
             (parent-document
              (parse-org (format nil "* Task~%") :source-name parent-source
                         :limits (make-limits :max-output-length 16)))
             (parent-path-error
              (handler-case
                  (progn
                    (resolve-archive-location
                     parent-document (archive-test-heading parent-document "Task")
                     :default-location "archive.org::")
                    nil)
                (org-resource-limit-error (condition) condition))))
        (with-soft-assertions
         (expect (org-archive-error-reason directory-basename-error)
                 :to-equal :source-name-required)
         (expect directory-location :to-equal '("notes/archive.org" . ""))
         (expect (typep parent-path-error 'org-resource-limit-error) :to-be t)
         (when (typep parent-path-error 'org-resource-limit-error)
           (expect (org-resource-limit-error-actual parent-path-error)
                   :to-equal 20))))))

(describe "archive path edge coverage"
  (it "measures pathname components and rejects unsupported components"
    (with-soft-assertions
      (expect (org-kit::archive-path-component-length nil) :to-be 0)
      (expect (org-kit::archive-path-component-length "notes") :to-be 5)
      (expect (org-kit::archive-path-component-length :wild) :to-be 1)
      (dolist (component '(:up :back :wild-inferiors))
        (expect (org-kit::archive-path-component-length component) :to-be 2))
      (dolist (component '(:unspecific :newest))
        (expect (org-kit::archive-path-component-length component) :to-be 0))
      (dolist (component '(:unsupported 42))
        (let ((condition
                (archive-test-error
                 (lambda ()
                   (org-kit::archive-path-component-length component)))))
          (expect (org-archive-error-reason condition)
                  :to-be :invalid-source-name)))))

  (it "handles empty basenames, unspecific pathname types, and absolute files"
    (let* ((empty-name (parse-org "* Task" :source-name "notes/"))
           (empty-error
             (archive-test-error
              (lambda ()
                (resolve-archive-location
                 empty-name (archive-test-heading empty-name "Task")
                 :default-location "%s::"))))
           (unspecific-type
             (parse-org "* Task"
                        :source-name
                        (make-pathname :directory '(:relative "notes")
                                       :name "work" :type :unspecific)))
           (absolute-document (parse-org "* Task"))
           (absolute-source-document
             (parse-org "* Task" :source-name "/vault/work.org")))
      (with-soft-assertions
        (expect (org-archive-error-reason empty-error)
                :to-be :source-name-required)
        (expect (org-kit::archive-source-basename
                 unspecific-type (make-limits))
                :to-equal "work")
        (expect (org-kit::archive-expand-relative-file
                 absolute-document "/vault/archive.org" (make-limits))
                :to-equal "/vault/archive.org")
        (expect (org-kit::archive-expand-relative-file
                 absolute-source-document "archive.org" (make-limits))
                :to-equal "/vault/archive.org"))))

  (it "skips malformed document keyword keys when resolving locations"
    (let* ((document (parse-org "* Task"))
           (heading (archive-test-heading document "Task"))
           (keyword (make-node :keyword :properties '(:key nil :value "bad")))
           (symbol 'org-kit::edit-properties-document-keywords)
           (original (symbol-function symbol)))
      (unwind-protect
           (progn
             (setf (symbol-function symbol)
                   (lambda (ignored-document &optional ignored-key)
                     (declare (ignore ignored-document ignored-key))
                     (list keyword)))
             (expect (org-kit::archive-location-value document heading nil)
                     :to-equal "%s_archive::"))
        (setf (symbol-function symbol) original))))

  (it "handles empty source insertion and reports a missing sibling after insertion"
    (let* ((empty (parse-org ""))
           (empty-result
             (org-kit::archive-root-insert-sibling empty nil 1))
           (non-final-newline (parse-org "* Task"))
           (non-final-result
             (org-kit::archive-root-insert-sibling
              non-final-newline
              (archive-test-heading non-final-newline "Task")
              1))
           (document (parse-org "* Task"))
           (heading (archive-test-heading document "Task"))
           (symbol 'org-kit::archive-root-insert-sibling)
           (original (symbol-function symbol))
           (condition nil))
      (setf (symbol-function symbol)
            (lambda (ignored-document ignored-heading ignored-stars)
              (declare (ignore ignored-document ignored-heading ignored-stars))
              (make-edit-result :updated-document document :spans nil)))
      (unwind-protect
           (setf condition
                 (archive-test-error
                  (lambda ()
                    (archive-to-sibling
                     document heading
                     :time-context
                     (archive-test-time-context "2024-02-19T12:34:00")))))
        (setf (symbol-function symbol) original))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document empty-result))
                :to-equal (format nil "* Archive :ARCHIVE:~%"))
        (expect (document-text
                 (edit-result-updated-document non-final-result))
                :to-equal (format nil "* Task~%* Archive :ARCHIVE:~%"))
        (expect (org-archive-error-reason condition)
                :to-be :archive-sibling-not-found)))))

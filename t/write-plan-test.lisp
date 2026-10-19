;;;; t/write-plan-test.lisp
(in-package #:cl-org-kit/test)

(defun write-plan-test-call-with-directory (function)
  (uiop:with-temporary-file (:pathname seed-file :stream seed-stream
                             :direction :output)
    (close seed-stream)
    (delete-file seed-file)
    (let ((directory
            (make-pathname :directory
                           (append (pathname-directory seed-file)
                                   (list (pathname-name seed-file)))
                           :name nil :type nil :defaults seed-file)))
      (ensure-directories-exist (merge-pathnames "marker" directory))
      (unwind-protect
           (funcall function directory)
        (uiop:delete-directory-tree directory :validate t)))))

(defun write-plan-test-child-directory (parent name)
  (let ((directory
          (make-pathname :directory
                         (append (pathname-directory parent) (list name))
                         :name nil :type nil :defaults parent)))
    (ensure-directories-exist (merge-pathnames "marker" directory))
    directory))

(defun write-plan-test-path (directory relative)
  (merge-pathnames relative directory))

(defun write-plan-test-write-text (path text)
  (with-open-file (stream path :direction :output :if-exists :supersede
                                :external-format :utf-8)
    (write-string text stream)))

(defun write-plan-test-read-text (path)
  (with-open-file (stream path :direction :input :external-format :utf-8)
    (with-output-to-string (output)
      (loop for character = (read-char stream nil nil)
            while character
            do (write-char character output)))))

(defun write-plan-test-read-octets (path)
  (with-open-file (stream path :direction :input
                                :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length stream)
                              :element-type '(unsigned-byte 8))))
      (expect (read-sequence octets stream) :to-be (length octets))
      octets)))

(defun write-plan-test-directory-exists-p (path)
  (uiop:directory-exists-p path))

(defun write-plan-test-capture-error (plan root &key limits)
  (handler-case
      (progn (apply-write-plan plan root :limits (or limits (make-limits))) nil)
    (org-write-plan-error (condition) condition)
    (org-resource-limit-error (condition) condition)))

(defun write-plan-test-call-with-failing-renames (failure-index function)
  (let* ((symbol 'org-kit::write-plan-rename-temporary-file)
         (original (symbol-function symbol))
         (cause (make-condition 'simple-error
                                 :format-control "injected rename failure"
                                 :format-arguments nil))
         (calls 0)
         (failed-temporary-name nil))
    (unwind-protect
         (progn
           (setf (symbol-function symbol)
                 (lambda (&rest arguments)
                   (if (= (incf calls) failure-index)
                       (progn
                         (setf failed-temporary-name (second arguments))
                         (error cause))
                       (apply original arguments))))
           (funcall function cause (lambda () calls)
                    (lambda () failed-temporary-name)))
      (setf (symbol-function symbol) original))))

(defun write-plan-test-call-with-full-encoder-counter (function)
  (let* ((symbol 'org-kit::write-plan-encode-string)
         (original (symbol-function symbol))
         (calls 0))
    (unwind-protect
         (progn
           (setf (symbol-function symbol)
                 (lambda (&rest arguments)
                   (incf calls)
                   (apply original arguments)))
           (funcall function (lambda () calls)))
      (setf (symbol-function symbol) original))))
(defun write-plan-test-call-after-preflight (mutation thunk)
  (let* ((symbol 'org-kit::write-plan-prepare)
         (original (symbol-function symbol)))
    (unwind-protect
         (progn
           (setf (symbol-function symbol)
                 (lambda (plan root limits)
                   (multiple-value-bind (canonical-root records)
                       (funcall original plan root limits)
                     (funcall mutation canonical-root)
                     (values canonical-root records))))
           (funcall thunk))
      (setf (symbol-function symbol) original))))

(describe "write plans"
  (it "writes exact UTF-8 content with the requested mode and creates parents"
    (write-plan-test-call-with-directory
     (lambda (root)
       (let* ((entry (make-write-plan-entry "nested/output.txt"
                                            (format nil "hello~%")
                                            :mode #o600
                                            :create-directories-p t))
              (plan (make-write-plan (list entry)))
              (target (write-plan-test-path root "nested/output.txt")))
         (with-soft-assertions
           (expect (write-plan-p plan) :to-be t)
           (expect (apply-write-plan plan root) :to-equal '("nested/output.txt"))
           (expect (write-plan-test-read-text target)
                   :to-equal (format nil "hello~%"))
           (expect (logand (sb-posix:stat-mode (sb-posix:stat (namestring target))) #o777)
                   :to-be #o600))))))

  (it "uses mode defaults, preserves existing modes, and ignores the umask"
    (write-plan-test-call-with-directory
     (lambda (root)
       (let* ((entry (make-write-plan-entry "accent.txt" "é"))
              (plan (make-write-plan (list entry)))
              (target (write-plan-test-path root "accent.txt"))
              (existing-target (write-plan-test-path root "existing.txt"))
              (new-target (write-plan-test-path root "new.txt"))
              (explicit-target (write-plan-test-path root "explicit.txt"))
              (existing-plan
                (make-write-plan
                 (list (make-write-plan-entry "existing.txt" "replacement"))))
              (new-plan
                (make-write-plan
                 (list (make-write-plan-entry "new.txt" "new"))))
              (explicit-plan
                (make-write-plan
                 (list (make-write-plan-entry "explicit.txt" "explicit"
                                              :mode #o777))))
              (old-umask (sb-posix:umask #o077)))
         (unwind-protect
              (progn
                (write-plan-test-write-text existing-target "old")
                (sb-posix:chmod (namestring existing-target) #o600)
                (apply-write-plan
                 plan root
                 :limits (make-limits :max-write-plan-entries 1
                                      :max-write-plan-bytes 2))
                (apply-write-plan existing-plan root)
                (apply-write-plan new-plan root)
                (apply-write-plan explicit-plan root)
                (with-soft-assertions
                  (expect (write-plan-entry-mode entry) :to-be #o644)
                  (expect (write-plan-test-read-octets target)
                          :to-equalp (vector #xC3 #xA9))
                  (expect (logand (sb-posix:stat-mode
                                   (sb-posix:stat (namestring target)))
                                  #o777)
                          :to-be #o644)
                  (expect (logand (sb-posix:stat-mode
                                   (sb-posix:stat (namestring existing-target)))
                                  #o777)
                          :to-be #o600)
                  (expect (logand (sb-posix:stat-mode
                                   (sb-posix:stat (namestring new-target)))
                                  #o777)
                          :to-be #o644)
                  (expect (logand (sb-posix:stat-mode
                                   (sb-posix:stat (namestring explicit-target)))
                                  #o777)
                          :to-be #o777)))
           (sb-posix:umask old-umask))))))

  (it "writes octet vectors unchanged and counts their exact byte length"
    (write-plan-test-call-with-directory
     (lambda (root)
       (let* ((source (make-array 4 :element-type '(unsigned-byte 8)
                                  :initial-contents '(0 #xFF #x80 10)))
              (entry (make-write-plan-entry "attachment.bin" source))
              (plan (make-write-plan (list entry)))
              (content-copy (write-plan-entry-content entry))
              (target (write-plan-test-path root "attachment.bin"))
              (condition
                (write-plan-test-capture-error
                 plan root :limits (make-limits :max-write-plan-bytes 3))))
         (setf (aref source 0) #x55
               (aref content-copy 1) 0)
         (with-soft-assertions
           (expect (typep condition 'org-resource-limit-error) :to-be t)
           (when condition
             (expect (org-resource-limit-error-limit-name condition)
                     :to-be :max-write-plan-bytes)
             (expect (org-resource-limit-error-actual condition) :to-be 4))
           (expect (probe-file target) :to-be nil)
           (expect (apply-write-plan
                    plan root :limits (make-limits :max-write-plan-bytes 4))
                   :to-equal '("attachment.bin"))
           (expect (write-plan-test-read-octets target)
                   :to-equalp (make-array 4 :element-type '(unsigned-byte 8)
                                          :initial-contents '(0 #xFF #x80 10))))))))

  (it "checks an oversized string budget before full encoding or file writes"
    (write-plan-test-call-with-directory
     (lambda (root)
       (let* ((content (make-string 4097 :initial-element #\a))
              (plan (make-write-plan
                     (list (make-write-plan-entry "oversized.txt" content))))
              (target (write-plan-test-path root "oversized.txt")))
         (write-plan-test-call-with-full-encoder-counter
          (lambda (calls)
            (let ((condition
                    (write-plan-test-capture-error
                     plan root :limits (make-limits :max-write-plan-bytes 4096))))
              (with-soft-assertions
                (expect (typep condition 'org-resource-limit-error) :to-be t)
                (when condition
                  (expect (org-resource-limit-error-actual condition) :to-be 4097))
                (expect (funcall calls) :to-be 0)
                (expect (probe-file target) :to-be nil)))))))))

  (it "writes strings whose size spans multiple counting chunks"
    (write-plan-test-call-with-directory
     (lambda (root)
       (let* ((content (make-string 9000 :initial-element #\a))
              (plan (make-write-plan
                     (list (make-write-plan-entry "chunked.txt" content))))
              (target (write-plan-test-path root "chunked.txt")))
         (with-soft-assertions
           (expect (apply-write-plan
                    plan root
                    :limits (make-limits :max-write-plan-bytes (length content)))
                   :to-equal '("chunked.txt"))
           (expect (write-plan-test-read-text target) :to-equal content))))))

  (it "rejects absolute and parent-component paths before writing"
    (write-plan-test-call-with-directory
     (lambda (root)
       (let* ((root-name (car (last (pathname-directory root))))
              (outside-name (format nil "~A-outside.txt" root-name))
              (outside-path (write-plan-test-path root (format nil "../~A" outside-name)))
              (absolute-path (namestring outside-path)))
         (unwind-protect
              (dolist (case (list (list absolute-path :absolute-path)
                                  (list (format nil "../~A" outside-name) :parent-component)
                                  (list (format nil "nested/../../~A" outside-name)
                                        :parent-component)))
                (let* ((path (first case))
                       (condition
                         (write-plan-test-capture-error
                          (make-write-plan
                           (list (make-write-plan-entry "nested/first.txt" "first"
                                                        :create-directories-p t)
                                 (make-write-plan-entry path "rejected")))
                          root)))
                  (with-soft-assertions
                    (expect (typep condition 'org-write-plan-error) :to-be t)
                    (when condition
                      (expect (org-write-plan-error-reason condition)
                              :to-be (second case)))
                    (expect (probe-file outside-path) :to-be nil)
                    (expect (probe-file
                             (write-plan-test-path root "nested/first.txt"))
                            :to-be nil)
                    (expect (write-plan-test-directory-exists-p
                             (write-plan-test-path root "nested"))
                            :to-be nil))))
           (when (probe-file outside-path)
             (delete-file outside-path)))))))

  (it "rejects normalized duplicates and file-parent conflicts before writes"
    (write-plan-test-call-with-directory
     (lambda (root)
       (dolist (case '((("a/./b.txt" "a//b.txt") :duplicate-path)
                       (("Readme.txt" "README.TXT") :duplicate-path)
                       (("a" "a/child.txt") :path-conflict)))
         (let* ((paths (first case))
                (plan (make-write-plan
                       (mapcar (lambda (path)
                                 (make-write-plan-entry path "value"
                                                        :create-directories-p t))
                               paths)))
                (condition (write-plan-test-capture-error plan root)))
           (with-soft-assertions
             (expect (typep condition 'org-write-plan-error) :to-be t)
             (when condition
               (expect (org-write-plan-error-reason condition) :to-be (second case)))
             (dolist (path paths)
               (expect (probe-file (write-plan-test-path root path)) :to-be nil))))))))

  (it "rejects empty, trailing-separator, NUL, and dot-only paths"
    (write-plan-test-call-with-directory
     (lambda (root)
       (dolist (path (list "" "trailing/" (format nil "nul~Cpath" #\Null) "."))
         (let ((condition
                 (write-plan-test-capture-error
                  (make-write-plan
                   (list (make-write-plan-entry "safe.txt" "first"
                                                :create-directories-p t)
                         (make-write-plan-entry path "invalid")))
                  root)))
           (with-soft-assertions
             (expect (typep condition 'org-write-plan-error) :to-be t)
             (when condition
               (expect (org-write-plan-error-reason condition)
                       :to-be :invalid-path))
             (expect (probe-file (write-plan-test-path root "safe.txt"))
                     :to-be nil)))))))

  (it "rejects an existing directory as the target"
    (write-plan-test-call-with-directory
     (lambda (root)
       (let* ((target (write-plan-test-child-directory root "target"))
              (condition
                (write-plan-test-capture-error
                 (make-write-plan
                  (list (make-write-plan-entry "target" "replacement")))
                 root)))
         (with-soft-assertions
           (expect (typep condition 'org-write-plan-error) :to-be t)
           (when condition
             (expect (org-write-plan-error-reason condition)
                     :to-be :target-not-regular-file))
           (expect (write-plan-test-directory-exists-p target)
                   :to-be-truthy))))))

  (it "requires an existing parent when directory creation is disabled"
    (write-plan-test-call-with-directory
     (lambda (root)
       (let ((condition
               (write-plan-test-capture-error
                (make-write-plan
                 (list (make-write-plan-entry "missing/target.txt" "value"
                                              :create-directories-p nil)))
                root)))
         (with-soft-assertions
           (expect (typep condition 'org-write-plan-error) :to-be t)
           (when condition
             (expect (org-write-plan-error-reason condition)
                     :to-be :missing-parent-directory))
           (expect (write-plan-test-directory-exists-p
                    (write-plan-test-path root "missing"))
                   :to-be nil)
           (expect (probe-file (write-plan-test-path root "missing/target.txt"))
                   :to-be nil))))))

  (it "resolves a symlink used as the root directory"
    (write-plan-test-call-with-directory
     (lambda (base)
       (let* ((root (write-plan-test-child-directory base "root"))
              (root-link (write-plan-test-path base "root-link"))
              (target (write-plan-test-path root "resolved.txt"))
              (plan (make-write-plan
                     (list (make-write-plan-entry "resolved.txt" "through link")))))
         (sb-posix:symlink (namestring root) (namestring root-link))
         (with-soft-assertions
           (expect (apply-write-plan plan root-link)
                   :to-equal '("resolved.txt"))
           (expect (write-plan-test-read-text target) :to-equal "through link")
           (expect (logand (sb-posix:stat-mode (sb-posix:lstat (namestring root-link)))
                           #o170000)
                   :to-be #o120000))))))

  (it "replaces an existing regular file"
    (write-plan-test-call-with-directory
     (lambda (root)
       (let* ((target (write-plan-test-path root "replace.txt"))
              (plan (make-write-plan
                     (list (make-write-plan-entry "replace.txt" "new content")))))
         (write-plan-test-write-text target "old content")
         (with-soft-assertions
           (expect (apply-write-plan plan root) :to-equal '("replace.txt"))
           (expect (write-plan-test-read-text target) :to-equal "new content"))))))

  (it "wraps rename I/O failures and removes the temporary file"
    (write-plan-test-call-with-directory
     (lambda (root)
       (let ((plan (make-write-plan
                    (list (make-write-plan-entry "failed.txt" "value")))))
         (write-plan-test-call-with-failing-renames
          1
          (lambda (cause calls temporary-name)
            (let ((condition (write-plan-test-capture-error plan root)))
              (with-soft-assertions
                (expect (typep condition 'org-write-plan-error) :to-be t)
                (when condition
                  (expect (org-write-plan-error-reason condition) :to-be :io-failed)
                  (expect (eq (org-write-plan-error-cause condition) cause) :to-be t))
                (expect (funcall calls) :to-be 1)
                (expect (probe-file (write-plan-test-path root "failed.txt"))
                        :to-be nil)
                (expect (funcall temporary-name) :to-be-truthy)
                (when (funcall temporary-name)
                  (expect (probe-file (write-plan-test-path
                                       root (funcall temporary-name)))
                          :to-be nil))))))))))

  (it "keeps earlier files applied when a later rename fails"
    (write-plan-test-call-with-directory
     (lambda (root)
       (let ((plan (make-write-plan
                    (list (make-write-plan-entry "first.txt" "first")
                          (make-write-plan-entry "second.txt" "second")))))
         (write-plan-test-call-with-failing-renames
          2
          (lambda (cause calls temporary-name)
            (let ((condition (write-plan-test-capture-error plan root)))
              (with-soft-assertions
                (expect (typep condition 'org-write-plan-error) :to-be t)
                (when condition
                  (expect (org-write-plan-error-reason condition) :to-be :io-failed)
                  (expect (eq (org-write-plan-error-cause condition) cause) :to-be t))
                (expect (funcall calls) :to-be 2)
                (expect (write-plan-test-read-text
                         (write-plan-test-path root "first.txt"))
                        :to-equal "first")
                (expect (probe-file (write-plan-test-path root "second.txt"))
                        :to-be nil)
                (expect (funcall temporary-name) :to-be-truthy)
                (when (funcall temporary-name)
                  (expect (probe-file (write-plan-test-path
                                       root (funcall temporary-name)))
                          :to-be nil))))))))))

  (it "rejects existing parent, dangling-parent, and final symlinks"
    (write-plan-test-call-with-directory
     (lambda (base)
       (let* ((root (write-plan-test-child-directory base "root"))
              (outside (write-plan-test-child-directory base "outside"))
              (sentinel (write-plan-test-path outside "sentinel.txt"))
              (parent-link (write-plan-test-path root "linked-parent"))
              (dangling-link (write-plan-test-path root "dangling-parent"))
              (target-link (write-plan-test-path root "linked-target")))
         (write-plan-test-write-text sentinel "unchanged")
         (sb-posix:symlink (namestring outside) (namestring parent-link))
         (sb-posix:symlink (namestring (write-plan-test-path base "missing"))
                           (namestring dangling-link))
         (sb-posix:symlink (namestring sentinel) (namestring target-link))
         (dolist (case '(("linked-parent/new.txt" :symlink-component)
                         ("dangling-parent/new.txt" :symlink-component)
                         ("linked-target" :symlink-component)))
           (let* ((condition
                    (write-plan-test-capture-error
                     (make-write-plan
                      (list (make-write-plan-entry (first case) "changed"
                                                   :create-directories-p t)))
                     root)))
             (with-soft-assertions
               (expect (typep condition 'org-write-plan-error) :to-be t)
               (when condition
                 (expect (org-write-plan-error-reason condition) :to-be (second case)))
               (expect (write-plan-test-read-text sentinel) :to-equal "unchanged")
               (expect (probe-file (write-plan-test-path outside "new.txt")) :to-be nil))))))))

  (it "checks every entry and configured limits before creating any target"
    (write-plan-test-call-with-directory
     (lambda (root)
       (let* ((count-plan
                (make-write-plan
                 (list (make-write-plan-entry "first.txt" "first")
                       (make-write-plan-entry "second.txt" "second"))))
              (count-error
                (write-plan-test-capture-error
                 count-plan root :limits (make-limits :max-write-plan-entries 1)))
              (byte-plan
                (make-write-plan
                 (list (make-write-plan-entry "large.txt" "é"))))
              (byte-error
                (write-plan-test-capture-error
                 byte-plan root :limits (make-limits :max-write-plan-bytes 1))))
         (with-soft-assertions
           (expect (typep count-error 'org-resource-limit-error) :to-be t)
           (when count-error
             (expect (org-resource-limit-error-limit-name count-error)
                     :to-be :max-write-plan-entries)
             (expect (org-resource-limit-error-limit count-error) :to-be 1)
             (expect (org-resource-limit-error-actual count-error) :to-be 2))
           (expect (typep byte-error 'org-resource-limit-error) :to-be t)
           (when byte-error
             (expect (org-resource-limit-error-limit-name byte-error)
                     :to-be :max-write-plan-bytes)
             (expect (org-resource-limit-error-limit byte-error) :to-be 1)
             (expect (org-resource-limit-error-actual byte-error) :to-be 2))
           (expect (probe-file (write-plan-test-path root "first.txt")) :to-be nil)
           (expect (probe-file (write-plan-test-path root "second.txt")) :to-be nil)
           (expect (probe-file (write-plan-test-path root "large.txt")) :to-be nil))))))

  (it "rejects symlink swaps during application"
      (write-plan-test-call-with-directory
       (lambda (base)
         (let* ((root (write-plan-test-child-directory base "root"))
                (outside (write-plan-test-child-directory base "outside"))
                (parent (write-plan-test-child-directory root "existing"))
                (existing-condition
                 (write-plan-test-call-after-preflight
                  (lambda (canonical-root)
                    (declare (ignore canonical-root))
                    (uiop:delete-directory-tree parent :validate t)
                    (sb-posix:symlink (namestring outside) (namestring (write-plan-test-path root "existing"))))
                  (lambda ()
                    (write-plan-test-capture-error
                     (make-write-plan
                      (list (make-write-plan-entry "existing/new.txt" "value"
                                                   :create-directories-p t)))
                     root))))
                (planned-parent (write-plan-test-path root "planned"))
                (planned-condition
                 (write-plan-test-call-after-preflight
                  (lambda (canonical-root)
                    (declare (ignore canonical-root))
                    (sb-posix:symlink (namestring outside)
                                      (namestring planned-parent)))
                  (lambda ()
                    (write-plan-test-capture-error
                     (make-write-plan
                      (list (make-write-plan-entry "planned/new.txt" "value"
                                                   :create-directories-p t)))
                     root)))))
           (with-soft-assertions
            (dolist (condition (list existing-condition planned-condition))
              (expect (typep condition 'org-write-plan-error) :to-be t)
              (when condition
                (expect (org-write-plan-error-reason condition) :to-be :io-failed)
                (expect (typep (org-write-plan-error-cause condition)
                               'sb-posix:syscall-error)
                        :to-be t)
                (when (typep (org-write-plan-error-cause condition)
                             'sb-posix:syscall-error)
                  (expect (member
                           (sb-posix:syscall-errno
                            (org-write-plan-error-cause condition))
                           (list sb-posix:enotdir sb-posix:eloop))
                          :to-be-truthy)))
              (expect (uiop:directory-files outside) :to-equal nil))))))
      (write-plan-test-call-with-directory
       (lambda (base)
         (let* ((root (write-plan-test-child-directory base "root"))
                (outside (write-plan-test-child-directory base "outside"))
                (target (write-plan-test-path root "target.txt"))
                (sentinel (write-plan-test-path outside "sentinel.txt"))
                (plan (make-write-plan
                       (list (make-write-plan-entry "target.txt" "replacement"))))
                (applied nil))
           (write-plan-test-write-text target "old")
           (write-plan-test-write-text sentinel "unchanged")
           (setf applied
                 (write-plan-test-call-after-preflight
                  (lambda (canonical-root)
                    (declare (ignore canonical-root))
                    (delete-file target)
                    (sb-posix:symlink (namestring sentinel) (namestring target)))
                  (lambda () (apply-write-plan plan root))))
           (with-soft-assertions
            (expect applied :to-equal '("target.txt"))
            (expect (write-plan-test-read-text target) :to-equal "replacement")
            (expect (write-plan-test-read-text sentinel) :to-equal "unchanged")
            (expect (logand (sb-posix:stat-mode (sb-posix:lstat (namestring target)))
                            #o170000)
                    :to-be #o100000)))))
      )

  (it "copies and validates write-plan data"
    (let* ((source-path (copy-seq "entry.txt"))
           (source-content (copy-seq "payload"))
           (entry (make-write-plan-entry source-path source-content
                                         :mode #o640 :create-directories-p t))
           (path-copy (write-plan-entry-path entry))
           (content-copy (write-plan-entry-content entry))
           (entries (vector entry))
           (plan (make-write-plan entries))
           (plan-entries (write-plan-entries plan))
           (pathname (make-pathname :directory '(:relative "nested")
                                    :name "path" :type "txt"))
           (pathname-entry (make-write-plan-entry pathname "path"))
           (empty-plan (make-write-plan nil)))
      (setf (char source-path 0) #\X
            (char source-content 0) #\X
            (char path-copy 0) #\Y
            (char content-copy 0) #\Y
            (aref entries 0) nil
            (aref plan-entries 0) nil)
      (with-soft-assertions
        (expect (write-plan-entry-p entry) :to-be t)
        (expect (write-plan-entry-p nil) :to-be nil)
        (expect (write-plan-entry-path entry) :to-equal "entry.txt")
        (expect (write-plan-entry-content entry) :to-equal "payload")
        (expect (write-plan-entry-mode entry) :to-be #o640)
        (expect (write-plan-entry-create-directories-p entry) :to-be t)
        (expect (write-plan-entry-path pathname-entry)
                :to-equal (sb-ext:native-namestring pathname))
        (expect (write-plan-p plan) :to-be t)
        (expect (write-plan-p nil) :to-be nil)
        (expect (eq (aref (write-plan-entries plan) 0) entry) :to-be t)
        (expect (length (write-plan-entries empty-plan)) :to-be 0)
        (expect (write-plan-entries (make-write-plan (list entry)))
                :to-equalp (vector entry))
        (expect (handler-case
                    (progn (make-write-plan-entry 12 "content") nil)
                  (type-error () t))
                :to-be t)
        (expect (handler-case
                    (progn (make-write-plan-entry "path" 12) nil)
                  (type-error () t))
                :to-be t)
        (expect (handler-case
                    (progn (make-write-plan-entry "path" "content" :mode #o1000) nil)
                  (type-error () t))
                :to-be t)
        (expect (handler-case
                    (progn (make-write-plan-entry "path" "content"
                                                  :create-directories-p :yes)
                           nil)
                  (type-error () t))
                :to-be t)
        (expect (handler-case (progn (make-write-plan 12) nil)
                  (type-error () t))
                :to-be t)
        (expect (handler-case (progn (make-write-plan (vector nil)) nil)
                  (type-error () t))
                :to-be t)
        (expect (handler-case
                    (progn (make-write-plan (cons entry :tail)) nil)
                  (type-error () t))
                :to-be t)
        (let ((cycle (list entry)))
          (setf (cdr cycle) cycle)
          (expect (handler-case (progn (make-write-plan cycle) nil)
                    (type-error () t))
                  :to-be t)))))

  (it "validates roots and both file-parent conflict orders"
    (write-plan-test-call-with-directory
     (lambda (root)
       (let* ((empty-plan (make-write-plan nil))
              (file-root (write-plan-test-path root "root-file"))
              (missing-root (write-plan-test-path root "missing-root"))
              (parent-file (write-plan-test-path root "parent"))
              (parent-plan
                (make-write-plan
                 (list (make-write-plan-entry "a" "file")
                       (make-write-plan-entry "a/child.txt" "child"
                                              :create-directories-p t))))
              (child-plan
                (make-write-plan
                 (list (make-write-plan-entry "a/child.txt" "child"
                                              :create-directories-p t)
                       (make-write-plan-entry "a" "file"))))
              (invalid-root-error nil)
              (file-root-error nil)
              (parent-error nil)
              (child-error nil))
         (write-plan-test-write-text file-root "root")
         (write-plan-test-write-text parent-file "parent")
         (setf invalid-root-error
               (write-plan-test-capture-error empty-plan missing-root)
               file-root-error
               (write-plan-test-capture-error empty-plan file-root)
               parent-error
               (write-plan-test-capture-error parent-plan root)
               child-error
               (write-plan-test-capture-error child-plan root))
         (with-soft-assertions
           (expect (handler-case (progn (apply-write-plan nil root) nil)
                     (type-error () t))
                   :to-be t)
           (expect (handler-case
                       (progn (apply-write-plan empty-plan root :limits nil) nil)
                     (type-error () t))
                   :to-be t)
           (expect (handler-case (progn (apply-write-plan empty-plan nil) nil)
                     (type-error () t))
                   :to-be t)
           (expect (typep invalid-root-error 'org-write-plan-error) :to-be t)
           (when invalid-root-error
             (expect (org-write-plan-error-reason invalid-root-error)
                     :to-be :invalid-root))
           (expect (typep file-root-error 'org-write-plan-error) :to-be t)
           (when file-root-error
             (expect (org-write-plan-error-reason file-root-error)
                     :to-be :root-not-directory))
           (expect (typep parent-error 'org-write-plan-error) :to-be t)
           (when parent-error
             (expect (org-write-plan-error-reason parent-error)
                     :to-be :path-conflict))
           (expect (typep child-error 'org-write-plan-error) :to-be t)
           (when child-error
             (expect (org-write-plan-error-reason child-error)
                     :to-be :path-conflict))
           (expect (apply-write-plan empty-plan (pathname root)) :to-be nil)
           (multiple-value-bind (normalized components)
               (org-kit::write-plan-normalized-path
                (make-pathname :directory '(:relative "nested")
                               :name "file" :type "txt"))
             (expect normalized :to-equal "nested/file.txt")
             (expect components :to-equal '("nested" "file.txt"))))))))

  (it "rejects an existing regular file as a parent directory"
    (write-plan-test-call-with-directory
     (lambda (root)
       (let ((parent (write-plan-test-path root "parent"))
             (condition nil))
         (write-plan-test-write-text parent "file")
         (setf condition
               (write-plan-test-capture-error
                (make-write-plan
                 (list (make-write-plan-entry "parent/child.txt" "value"
                                              :create-directories-p t)))
                root))
         (with-soft-assertions
           (expect (typep condition 'org-write-plan-error) :to-be t)
           (when condition
             (expect (org-write-plan-error-reason condition)
                     :to-be :parent-not-directory))
           (expect (write-plan-test-read-text parent) :to-equal "file"))))))
  (it "writes files under an existing parent directory"
    (write-plan-test-call-with-directory
     (lambda (root)
       (write-plan-test-child-directory root "parent")
       (let ((plan
               (make-write-plan
                (list (make-write-plan-entry "parent/child.txt" "value")))))
         (with-soft-assertions
           (expect (apply-write-plan plan root)
                   :to-equal (list "parent/child.txt"))
           (expect (write-plan-test-read-text
                    (write-plan-test-path root "parent/child.txt"))
                   :to-equal "value"))))))

  (it "covers path, UTF-8, and directory I/O failure paths"
  (progn
    (write-plan-test-call-with-directory
     (lambda (root)
       (flet ((call-with-function (symbol replacement thunk)
                (let ((original (symbol-function symbol)))
                  (unwind-protect
                       (progn
                         (setf (symbol-function symbol) replacement)
                         (funcall thunk))
                    (setf (symbol-function symbol) original)))))
         (let* ((plan (make-write-plan
                       (list (make-write-plan-entry "encoded.txt" "value"))))
                (denied (make-condition 'sb-posix:syscall-error
                                        :errno sb-posix:eacces :name "lstat"))
                (size-symbol (find-symbol "STRING-SIZE-IN-OCTETS" "CL-CODEC-KIT"))
                (encoder-symbol (find-symbol "STRING-TO-OCTETS" "CL-CODEC-KIT"))
                (lstat-error
                  (call-with-function
                   'sb-posix:lstat
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (error denied))
                   (lambda () (write-plan-test-capture-error plan root))))
                (wrapped-lstat-error
                  (call-with-function
                   'sb-posix:lstat
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (error denied))
                   (lambda ()
                     (handler-case (org-kit::write-plan-lstat "missing")
                       (org-write-plan-error (condition) condition)))))
                (count-error
                  (call-with-function
                   size-symbol
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (error "injected size encoding failure"))
                   (lambda () (write-plan-test-capture-error plan root))))
                (encode-error
                  (call-with-function
                   encoder-symbol
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (error "injected UTF-8 encoder failure"))
                   (lambda ()
                     (handler-case
                         (org-kit::write-plan-encode-string
                          "value" "encoded.txt")
                       (org-write-plan-error (condition) condition)))))
                (size-mismatch
                  (call-with-function
                   'org-kit::write-plan-encode-string
                   (lambda (string path)
                     (declare (ignore string path))
                     #())
                   (lambda () (write-plan-test-capture-error plan root)))))
           (with-soft-assertions
             (expect (org-write-plan-error-reason lstat-error) :to-be :io-failed)
             (expect (eq (org-write-plan-error-cause lstat-error) denied) :to-be t)
             (expect (org-write-plan-error-reason wrapped-lstat-error)
                     :to-be :io-failed)
             (expect (eq (org-write-plan-error-cause wrapped-lstat-error) denied)
                     :to-be t)
             (expect (org-write-plan-error-reason count-error) :to-be :invalid-utf-8)
             (expect (org-write-plan-error-reason encode-error) :to-be :invalid-utf-8)
             (expect (org-write-plan-error-reason size-mismatch)
                     :to-be :utf-8-size-mismatch)
             (expect (probe-file (write-plan-test-path root "encoded.txt"))
                     :to-be nil)
             (let* ((root-without-trailing-slash
                    (string-right-trim "/" (namestring (truename root))))
                    (expected (concatenate 'string root-without-trailing-slash "/"))
                    (normalized
                     (org-kit::write-plan-canonical-root root-without-trailing-slash)))
               (expect normalized :to-equal expected)))))))
    (write-plan-test-call-with-directory
   (lambda (root)
     (flet ((call-with-function (symbol replacement thunk)
              (let ((original (symbol-function symbol)))
                (unwind-protect
                     (progn
                       (setf (symbol-function symbol) replacement)
                       (funcall thunk))
                  (setf (symbol-function symbol) original)))))
       (let* ((openat-symbol 'org-kit::write-plan-openat)
              (original-openat (symbol-function openat-symbol))
              (original-result
                (symbol-function 'org-kit::write-plan-syscall-result))
              (missing (make-condition 'sb-posix:syscall-error
                                       :errno sb-posix:enoent :name "openat"))
              (exists (make-condition 'sb-posix:syscall-error
                                      :errno sb-posix:eexist :name "openat"))
              (denied (make-condition 'sb-posix:syscall-error
                                      :errno sb-posix:eacces :name "openat"))
              (root-fd
                (sb-posix:open (namestring root)
                               (logior sb-posix:o-rdonly
                                       sb-posix:o-directory
                                       sb-posix:o-nofollow))))
         (unwind-protect
              (with-soft-assertions
                (let ((caught
                        (call-with-function
                         openat-symbol
                         (lambda (&rest arguments)
                           (declare (ignore arguments))
                           (error missing))
                         (lambda ()
                           (handler-case
                               (org-kit::write-plan-open-child-directory
                                root-fd "absent" "absent/file" nil)
                             (org-write-plan-error (condition) condition))))))
                  (expect (org-write-plan-error-reason caught)
                          :to-be :missing-parent-directory))
                (let ((caught
                        (call-with-function
                         openat-symbol
                         (lambda (&rest arguments)
                           (declare (ignore arguments))
                           (error denied))
                         (lambda ()
                           (handler-case
                               (org-kit::write-plan-open-child-directory
                                root-fd "denied" "denied/file" nil)
                             (sb-posix:syscall-error (condition) condition))))))
                  (expect (eq caught denied) :to-be t))
                (write-plan-test-child-directory root "raced")
                (let ((calls 0)
                      (opened nil))
                  (call-with-function
                   'org-kit::%write-plan-mkdirat
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     -1)
                   (lambda ()
                     (call-with-function
                      'org-kit::write-plan-syscall-result
                      (lambda (result name)
                        (if (string= name "mkdirat")
                            (error exists)
                            (funcall original-result result name)))
                      (lambda ()
                        (call-with-function
                         openat-symbol
                         (lambda (directory-fd component flags &optional (mode 0))
                           (if (= (incf calls) 1)
                               (error missing)
                               (funcall original-openat directory-fd component
                                        flags mode)))
                         (lambda ()
                           (setf opened
                                 (org-kit::write-plan-open-child-directory
                                  root-fd "raced" "raced/file" t)))))))) (expect (integerp opened) :to-be t) (when opened (sb-posix:close opened)) (expect calls :to-be 2)
                (let ((caught
                        (call-with-function
                         'org-kit::%write-plan-mkdirat
                         (lambda (&rest arguments)
                           (declare (ignore arguments))
                           -1)
                         (lambda ()
                           (call-with-function
                            'org-kit::write-plan-syscall-result
                            (lambda (result name)
                              (if (string= name "mkdirat")
                                  (error denied)
                                  (funcall original-result result name)))
                            (lambda ()
                              (call-with-function
                               openat-symbol
                               (lambda (&rest arguments)
                                 (declare (ignore arguments))
                                 (error missing))
                               (lambda ()
                                 (handler-case
                                     (org-kit::write-plan-open-child-directory
                                      root-fd "denied" "denied/file" t)
                                   (sb-posix:syscall-error (condition)
                                     condition))))))))))
                  (expect (eq caught denied) :to-be t)
                (let ((caught
                        (call-with-function
                         openat-symbol
                         (lambda (&rest arguments)
                           (declare (ignore arguments))
                           (error denied))
                         (lambda ()
                           (handler-case
                               (org-kit::write-plan-open-root-directory
                                (namestring root))
                             (org-write-plan-error (condition) condition))))))
                  (expect (org-write-plan-error-reason caught)
                          :to-be :io-failed)
                  (expect (eq (org-write-plan-error-cause caught) denied)
                          :to-be t))
                (let ((opened-root
                        (org-kit::write-plan-open-root-directory
                         (namestring (truename root)))))
                  (expect (integerp opened-root) :to-be t)
                  (sb-posix:close opened-root))
                (write-plan-test-child-directory root "first")
                (let* ((entry
                         (make-write-plan-entry
                          "first/second/file" "value" :create-directories-p t))
                       (record
                         (list entry "first/second/file"
                               '("first" "second" "file")
                               #(118 97 108 117 101)
                               "first/second/file"
                               '("first" "second" "file")))
                       (child-symbol 'org-kit::write-plan-open-child-directory)
                       (original-child (symbol-function child-symbol))
                       (opened nil)
                       (caught nil))
                  (flet ((fd-closed-p (fd)
                           (handler-case
                               (progn (sb-posix:fstat fd) nil)
                             (sb-posix:syscall-error (condition)
                               (= (sb-posix:syscall-errno condition) sb-posix:ebadf)))))
                    (unwind-protect
                         (progn
                           (setf (symbol-function child-symbol)
                                 (lambda (directory-fd component path create-p)
                                   (if (string= component "first")
                                       (let ((fd
                                               (funcall original-child directory-fd
                                                        component path create-p)))
                                         (push fd opened)
                                         fd)
                                       (error denied))))
                           (setf caught
                                 (handler-case
                                     (org-kit::write-plan-open-parent-directory root-fd record)
                                   (sb-posix:syscall-error (condition) condition)))
                           (expect (eq caught denied) :to-be t)
                           (expect (integerp (first opened)) :to-be t)
                           (when (integerp (first opened))
                             (expect (fd-closed-p (first opened)) :to-be t)))
                      (setf (symbol-function child-symbol) original-child))))
                (write-plan-test-child-directory
                 (write-plan-test-child-directory root "first")
                 "second")
                (let* ((entry
                         (make-write-plan-entry
                          "first/second/file" "value" :create-directories-p t))
                       (record
                         (list entry "first/second/file"
                               '("first" "second" "file")
                               #(118 97 108 117 101)
                               "first/second/file"
                               '("first" "second" "file")))
                       (parent-fd
                         (org-kit::write-plan-open-parent-directory root-fd record)))
                  (expect (integerp parent-fd) :to-be t)
                  (sb-posix:close parent-fd))
                (let ((caught
                        (call-with-function
                         openat-symbol
                         (lambda (&rest arguments)
                           (declare (ignore arguments))
                           (error exists))
                         (lambda ()
                           (handler-case
                               (org-kit::write-plan-open-temporary-file
                                root-fd "target.txt" (make-random-state t))
                             (org-write-plan-error (condition) condition))))))
                  (expect (org-write-plan-error-reason caught)
                          :to-be :temporary-name-exhausted))
                (let ((caught
                        (call-with-function
                         openat-symbol
                         (lambda (&rest arguments)
                           (declare (ignore arguments))
                           (error denied))
                         (lambda ()
                           (handler-case
                               (org-kit::write-plan-open-temporary-file
                                root-fd "target.txt" (make-random-state t))
                             (sb-posix:syscall-error (condition) condition))))))
                  (expect (eq caught denied) :to-be t))
                (let* ((entry
                         (make-write-plan-entry "record-failure.txt" "value"))
                       (record
                         (list entry "record-failure.txt"
                               '("record-failure.txt")
                               #(118 97 108 117 101)
                               "record-failure.txt"
                               '("record-failure.txt")))
                       (open-parent-symbol
                         'org-kit::write-plan-open-parent-directory)
                       (original-open-parent
                         (symbol-function open-parent-symbol))
                       (parent-fd nil)
                       (caught nil))
                  (flet ((fd-closed-p (fd)
                           (handler-case
                               (progn (sb-posix:fstat fd) nil)
                             (sb-posix:syscall-error (condition)
                               (= (sb-posix:syscall-errno condition)
                                  sb-posix:ebadf)))))
                    (progn
                      (setf caught
                            (call-with-function
                             open-parent-symbol
                             (lambda (root-fd record)
                               (setf parent-fd
                                     (funcall original-open-parent
                                              root-fd record)))
                             (lambda ()
                               (call-with-function
                                'org-kit::write-plan-open-temporary-file
                                (lambda (&rest arguments)
                                  (declare (ignore arguments))
                                  (error denied))
                                (lambda ()
                                  (handler-case
                                      (org-kit::write-plan-apply-record
                                       root-fd record (make-random-state t))
                                    (org-write-plan-error (condition)
                                      condition)))))))
                      (expect (org-write-plan-error-reason caught)
                              :to-be :io-failed)
                      (expect (eq (org-write-plan-error-cause caught) denied)
                              :to-be t)
                      (expect (integerp parent-fd) :to-be t)
                      (expect (fd-closed-p parent-fd) :to-be t)
                      (expect (fd-closed-p root-fd) :to-be nil))))
                )
           (sb-posix:close root-fd))))))))))
  (it "cleans up temporary files after write failure"
    (progn
      (write-plan-test-call-with-directory
       (lambda (root)
         (let* ((plan
                  (make-write-plan
                   (list (make-write-plan-entry "target.txt" "value"))))
                (source (write-plan-test-path root "source.txt"))
                (open-symbol 'org-kit::write-plan-open-temporary-file)
                (original-open (symbol-function open-symbol))
                (temp-name nil)
                (read-only-fd nil)
                (condition nil))
           (write-plan-test-write-text source "source")
           (flet ((call-with-function (symbol replacement thunk)
                    (let ((original (symbol-function symbol)))
                      (unwind-protect
                           (progn
                             (setf (symbol-function symbol) replacement)
                             (funcall thunk))
                        (setf (symbol-function symbol) original))))
                  (fd-closed-p (fd)
                    (handler-case
                        (progn (sb-posix:fstat fd) nil)
                      (sb-posix:syscall-error (condition)
                        (= (sb-posix:syscall-errno condition) sb-posix:ebadf))))) (with-soft-assertions
               (setf condition
                     (call-with-function
                      open-symbol
                      (lambda (parent-fd path random-state)
                        (multiple-value-bind (name fd)
                            (funcall original-open parent-fd path random-state)
                          (setf temp-name name)
                          (sb-posix:close fd)
                          (setf read-only-fd
                                (sb-posix:open (namestring source)
                                               sb-posix:o-rdonly))
                          (values name read-only-fd)))
                      (lambda ()
                        (write-plan-test-capture-error plan root))))
               (expect (typep condition 'org-write-plan-error) :to-be t)
               (when condition
                 (expect (org-write-plan-error-reason condition)
                         :to-be :io-failed))
               (expect temp-name :to-be-truthy)
               (when temp-name
                 (expect (probe-file (write-plan-test-path root temp-name))
                         :to-be nil))
               (expect (probe-file (write-plan-test-path root "target.txt"))
                       :to-be nil)
               (expect (integerp read-only-fd) :to-be t)
               (when (integerp read-only-fd)
                 (expect (fd-closed-p read-only-fd) :to-be t))
               (expect (write-plan-test-read-text source) :to-equal "source"))))))))
  (it "accepts string roots and preserves apply failures"
    (write-plan-test-call-with-directory
     (lambda (root)
       (let* ((string-plan
                (make-write-plan
                 (list (make-write-plan-entry "string-root.txt" "via string"))))
              (generic-plan
                (make-write-plan
                 (list (make-write-plan-entry "generic-failure.txt" "value"))))
              (specific-plan
                (make-write-plan
                 (list (make-write-plan-entry "specific-failure.txt" "value"))))
              (root-open-symbol
                (find-symbol "WRITE-PLAN-OPEN-ROOT-DIRECTORY" "ORG-KIT"))
              (temporary-open-symbol
                (find-symbol "WRITE-PLAN-OPEN-TEMPORARY-FILE" "ORG-KIT"))
              (generic-failure
                (make-condition (find-symbol "SIMPLE-ERROR" "COMMON-LISP")
                                :format-control "injected root-open failure"
                                :format-arguments nil))
              (specific-failure
                (make-condition (find-symbol "ORG-WRITE-PLAN-ERROR" "ORG-KIT")
                                :path "specific-failure.txt"
                                :reason :io-failed
                                :message "injected write-plan failure"))
              (generic-condition nil)
              (specific-condition nil))
         (flet ((call-with-function (symbol replacement thunk)
                  (let ((original (symbol-function symbol)))
                    (unwind-protect
                         (progn
                           (setf (symbol-function symbol) replacement)
                           (funcall thunk))
                      (setf (symbol-function symbol) original)))))
           (with-soft-assertions
             (expect (apply-write-plan
                     string-plan
                     (string-right-trim "/" (namestring (truename root))))
                     :to-equal (list "string-root.txt"))
             (expect (write-plan-test-read-text
                      (write-plan-test-path root "string-root.txt"))
                     :to-equal "via string")
             (setf generic-condition
                   (call-with-function
                    root-open-symbol
                    (lambda (&rest arguments)
                      (declare (ignore arguments))
                      (error generic-failure))
                    (lambda ()
                      (write-plan-test-capture-error generic-plan root))))
             (expect (typep generic-condition
                            (find-symbol "ORG-WRITE-PLAN-ERROR" "ORG-KIT"))
                     :to-be t)
             (when (typep generic-condition
                          (find-symbol "ORG-WRITE-PLAN-ERROR" "ORG-KIT"))
               (expect (org-write-plan-error-reason generic-condition)
                       :to-be :io-failed)
               (expect (eq (org-write-plan-error-cause generic-condition)
                           generic-failure)
                       :to-be t))
             (expect (probe-file
                      (write-plan-test-path root "generic-failure.txt"))
                     :to-be nil)
             (setf specific-condition
                   (call-with-function
                    temporary-open-symbol
                    (lambda (&rest arguments)
                      (declare (ignore arguments))
                      (error specific-failure))
                    (lambda ()
                      (write-plan-test-capture-error specific-plan root))))
             (expect (eq specific-condition specific-failure) :to-be t)
             (expect (org-write-plan-error-reason specific-condition)
                     :to-be :io-failed)
             (expect (probe-file
                      (write-plan-test-path root "specific-failure.txt"))
                     :to-be nil)))))))
  (it "closes temporary descriptors when stream creation fails"
    (write-plan-test-call-with-directory
     (lambda (root)
       (let* ((plan
                (make-write-plan
                 (list (make-write-plan-entry "stream-failure.txt" "value"))))
              (temporary-open-symbol
                (find-symbol "WRITE-PLAN-OPEN-TEMPORARY-FILE" "ORG-KIT"))
              (stream-maker-symbol
                (find-symbol "MAKE-FD-STREAM" "SB-SYS"))
              (original-temporary-open
                (symbol-function temporary-open-symbol))
              (temporary-name nil)
              (temporary-fd nil)
              (condition nil))
         (flet ((call-with-function (symbol replacement thunk)
                  (let ((original (symbol-function symbol)))
                    (unwind-protect
                         (progn
                           (setf (symbol-function symbol) replacement)
                           (funcall thunk))
                      (setf (symbol-function symbol) original))))
                (call-with-stream-maker (replacement thunk)
                  (let ((original (symbol-function stream-maker-symbol)))
                    (unwind-protect
                         (progn
                           (sb-ext:without-package-locks
                             (setf (symbol-function stream-maker-symbol)
                                   replacement))
                           (funcall thunk))
                      (sb-ext:without-package-locks
                        (setf (symbol-function stream-maker-symbol)
                              original)))))
                (fd-closed-p (fd)
                  (handler-case
                      (progn (sb-posix:fstat fd) nil)
                    (sb-posix:syscall-error (condition)
                      (= (sb-posix:syscall-errno condition) sb-posix:ebadf)))))
           (with-soft-assertions
             (setf condition
                   (call-with-function
                    temporary-open-symbol
                    (lambda (parent-fd path random-state)
                      (multiple-value-bind (name fd)
                          (funcall original-temporary-open
                                   parent-fd path random-state)
                        (setf temporary-name name
                              temporary-fd fd)
                        (values name fd)))
                    (lambda ()
                      (call-with-stream-maker
                       (lambda (&rest arguments)
                         (declare (ignore arguments))
                         (error "injected output stream creation failure"))
                       (lambda ()
                         (write-plan-test-capture-error plan root))))))
             (expect (typep condition
                            (find-symbol "ORG-WRITE-PLAN-ERROR" "ORG-KIT"))
                     :to-be t)
             (when (typep condition
                          (find-symbol "ORG-WRITE-PLAN-ERROR" "ORG-KIT"))
               (expect (org-write-plan-error-reason condition)
                       :to-be :io-failed))
             (expect (integerp temporary-fd) :to-be t)
             (when (integerp temporary-fd)
               (expect (fd-closed-p temporary-fd) :to-be t))
             (expect temporary-name :to-be-truthy)
             (when temporary-name
               (expect (probe-file
                        (write-plan-test-path root temporary-name))
                       :to-be nil))
             (expect (probe-file
                      (write-plan-test-path root "stream-failure.txt"))
                     :to-be nil))))))))

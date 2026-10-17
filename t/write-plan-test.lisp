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

  (it "uses the 0644 mode default and enforces exact UTF-8 byte limits"
    (write-plan-test-call-with-directory
     (lambda (root)
       (let* ((plan (make-write-plan
                     (list (make-write-plan-entry "accent.txt" "é"))))
              (target (write-plan-test-path root "accent.txt")))
         (with-soft-assertions
           (expect (apply-write-plan
                    plan root
                    :limits (make-limits :max-write-plan-entries 1
                                         :max-write-plan-bytes 2))
                   :to-equal '("accent.txt"))
           (expect (write-plan-test-read-octets target)
                   :to-equalp (vector #xC3 #xA9))
           (expect (logand (sb-posix:stat-mode (sb-posix:stat (namestring target))) #o777)
                   :to-be #o644))))))

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
       (dolist (case '(("/outside.txt" :absolute-path)
                       ("../outside.txt" :parent-component)
                       ("nested/../../outside.txt" :parent-component)))
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
               (expect (org-write-plan-error-reason condition) :to-be (second case)))
             (expect (probe-file (write-plan-test-path root "nested/first.txt"))
                     :to-be nil)
             (expect (write-plan-test-directory-exists-p
                      (write-plan-test-path root "nested"))
                     :to-be nil)))))))

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

  (it "rejects a symlink parent without creating a target outside the root"
    (write-plan-test-call-with-directory
     (lambda (base)
       (let* ((root (write-plan-test-child-directory base "root"))
              (outside (write-plan-test-child-directory base "outside"))
              (sentinel (write-plan-test-path outside "sentinel.txt"))
              (link (write-plan-test-path root "parent")))
         (write-plan-test-write-text sentinel "unchanged")
         (sb-posix:symlink (namestring outside) (namestring link))
         (let ((condition
                 (write-plan-test-capture-error
                  (make-write-plan
                   (list (make-write-plan-entry "parent/new.txt" "changed")))
                  root)))
           (with-soft-assertions
             (expect (typep condition 'org-write-plan-error) :to-be t)
             (when condition
               (expect (org-write-plan-error-reason condition)
                       :to-be :symlink-component))
             (expect (write-plan-test-read-text sentinel) :to-equal "unchanged")
             (expect (probe-file (write-plan-test-path outside "new.txt"))
                     :to-be nil))))))))

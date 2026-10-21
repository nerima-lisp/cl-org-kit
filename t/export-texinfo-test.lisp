;;;; t/export-texinfo-test.lisp
(in-package #:cl-org-kit/test)

(defun export-texinfo-test-path (relative)
  (asdf:system-relative-pathname
   "cl-org-kit" (concatenate 'string "t/fixtures/export/" relative)))

(defun export-texinfo-test-read-text (relative)
  (uiop:read-file-string (export-texinfo-test-path relative)))

(defun export-texinfo-test-read-octets (relative)
  (with-open-file (stream (export-texinfo-test-path relative)
                          :direction :input :element-type '(unsigned-byte 8))
    (let* ((length (file-length stream))
           (octets (make-array length :element-type '(unsigned-byte 8))))
      (unless (= (read-sequence octets stream) length)
        (error "Unable to read all bytes from Texinfo fixture ~A" relative))
      octets)))

(defun export-texinfo-test-expected (case)
  (export-texinfo-test-read-octets
   (concatenate 'string "oracle/texinfo/" case ".texi")))

(defun export-texinfo-test-actual (case &optional include-resolver)
  (let* ((source (concatenate 'string "oracle/corpus/" case ".org"))
         (document (parse-org (export-texinfo-test-read-text source)
                              :source-name source)))
    (export-org document :texinfo
                :include-resolver include-resolver
                :output-type :octets)))

(defun export-texinfo-test-bytes (text)
  (cl-codec-kit:string-to-octets text :encoding :utf-8))

(describe "X4 Texinfo export"
  (it "matches pinned Org 9.8.10 output for existing and Texinfo-specific fixtures"
    (let ((cases '("metadata-selection"
                   "exclude-tags-override"
                   "footnotes-macros-links"
                   "include"
                   "source-blocks"
                   "texinfo-elements"
                   "texinfo-options"
                   "texinfo-escape-boundaries")))
      (with-soft-assertions
        (dolist (case cases)
          (let* ((resolver-calls nil)
                 (resolver
                   (when (string= case "include")
                     (lambda (path parent-source)
                       (declare (ignore parent-source))
                       (push path resolver-calls)
                       (when (string= path "included/fragment.org")
                         (export-texinfo-test-read-text
                          "oracle/corpus/included/fragment.org")))))
                 (actual (export-texinfo-test-actual case resolver)))
            (expect actual :to-equalp (export-texinfo-test-expected case))
            (expect (nreverse resolver-calls)
                    :to-equal (if (string= case "include")
                                  '("included/fragment.org")
                                  nil))))
        (expect cases
                :to-equal '("metadata-selection"
                            "exclude-tags-override"
                            "footnotes-macros-links"
                            "include"
                            "source-blocks"
                            "texinfo-elements"
                            "texinfo-options"
                            "texinfo-escape-boundaries")))))

  (it "escapes Org text, preserves raw Texinfo output, and filters foreign raw blocks"
    (let* ((output (export-texinfo-test-actual "texinfo-escape-boundaries"))
           (foreign-output
             (export-org
              (parse-org
               (format nil "#+BEGIN_EXPORT html~%foreign block marker~%#+END_EXPORT~%~%@@html:foreign inline marker@@ and @@texinfo:@raw{,}@@"))
              :texinfo)))
      (expect (not (search "foreign block marker" foreign-output))
              :to-be t)
      (expect (not (search "foreign inline marker" foreign-output))
              :to-be t)
      (expect (search "@raw{,}" foreign-output) :to-be-truthy)
      (expect (search (export-texinfo-test-bytes
                       "Text boundary: @@ @{ @} comma@comma{}")
                      output)
              :to-be-truthy)
      (expect (search (export-texinfo-test-bytes
                       "@raw{,} @@literal")
                      output)
              :to-be-truthy)
      (expect (search (export-texinfo-test-bytes
                       "@raw{,} snippet@@@@")
                      output)
              :to-be-truthy)
      (expect (search (export-texinfo-test-bytes
                       (format nil "First hard-break line@*~%Second hard-break line"))
                      output)
              :to-be-truthy)))

  (it "signals the named output resource limit condition"
    (let* ((source (export-texinfo-test-read-text
                    "oracle/corpus/texinfo-elements.org"))
           (document (parse-org source
                                :limits (make-limits :max-output-length 80)))
           (condition
             (handler-case
                 (progn (export-org document :texinfo) nil)
               (org-resource-limit-error (condition) condition))))
      (expect condition :to-be-truthy)
      (expect (org-resource-limit-error-limit-name condition)
              :to-be :max-output-length)
      (expect (> (org-resource-limit-error-actual condition)
                 (org-resource-limit-error-limit condition))
              :to-be-truthy))))

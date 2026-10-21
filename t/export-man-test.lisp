(in-package #:cl-org-kit/test)

(defun export-man-test-fixture (name)
  (uiop:read-file-string
   (asdf:system-relative-pathname
    "cl-org-kit" (format nil "t/fixtures/export/oracle/corpus/~A.org" name))))

(defun export-man-test-octets (relative-path)
  (with-open-file (input (asdf:system-relative-pathname "cl-org-kit" relative-path)
                         :direction :input
                         :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length input)
                              :element-type '(unsigned-byte 8))))
      (unless (= (read-sequence octets input) (length octets))
        (error "Unable to read complete oracle output: ~A" relative-path))
      octets)))

(defun export-man-test-oracle (case-id &optional include-resolver)
  (let* ((document (parse-org (export-man-test-fixture case-id)))
         (expected (export-man-test-octets
                    (format nil "t/fixtures/export/oracle/man/~A.1" case-id)))
         (actual (export-org document :man
                             :include-resolver include-resolver
                             :output-type :octets)))
    (expect (plusp (length expected)) :to-be t)
    (expect actual :to-equalp expected)))

(describe "FR-104 man export backend"
  (it "matches the broad man element oracle"
    (export-man-test-oracle "man-elements"))
  (it "matches all five standard Org man oracle cases"
    (let ((resolver-calls nil)
          (case-ids (list "metadata-selection"
                          "exclude-tags-override"
                          "footnotes-macros-links"
                          "include"
                          "source-blocks")))
      (with-soft-assertions
        (dolist (case-id case-ids)
          (export-man-test-oracle
           case-id
           (when (string= case-id "include")
             (lambda (path parent-source)
               (declare (ignore parent-source))
               (push path resolver-calls)
               (when (string= path "included/fragment.org")
                 (export-man-test-fixture "included/fragment"))))))
        (expect (nreverse resolver-calls)
                :to-equal (list "included/fragment.org")))))

  (it "matches the man escaping oracle"
    (export-man-test-oracle "man-escapes"))

  (it "keeps Org 9.8.10 leading dot and apostrophe text unchanged"
    (let* ((document (parse-org (export-man-test-fixture "man-escapes")))
           (output (export-org document :man)))
      (with-soft-assertions
        (expect (search
                 ".PP
.leading-dot remains plain.
'leading-apostrophe remains plain."
                 output)
                :to-be-truthy)
        (expect (search "Plain escapes: , $\\, \\$, \\{." output)
                :to-be-truthy)
        (expect (search "\\fC. ' \\foo \\\\foo \\$ \\{\\fP" output)
                :to-be-truthy)
        (expect (search "\\fI. ' \\foo \\\\foo \\$ \\{\\fP" output)
                :to-be-truthy)
        (expect (search
                 ".example-dot
'example-apostrophe
\\efoo \\e\\efoo \\e$ \\e{"
                 output)
                :to-be-truthy)
        (expect (search
                 ".source-dot
'source-apostrophe
\\efoo \\e\\efoo \\e$ \\e{"
                 output)
                :to-be-truthy))))

  (it "signals org-resource-limit-error when rendered output exceeds its limit"
    (let* ((document
             (parse-org "A deliberately longer man output."
                        :limits (make-limits :max-output-length 32)))
           (condition
             (handler-case
                 (progn (export-org document :man) nil)
               (org-resource-limit-error (condition) condition))))
      (with-soft-assertions
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when (typep condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name condition)
                  :to-equal :max-output-length)
          (expect (org-resource-limit-error-limit condition) :to-equal 32))))))

;;;; t/package.lisp
(defpackage #:cl-org-kit/test
  (:use #:cl #:org-kit)
  ;; DESCRIBE clashes with CL:DESCRIBE, so shadow-import cl-weave's.
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave
   #:it #:it-each #:it-property #:expect #:signals #:run-all
   #:with-soft-assertions
   #:gen-integer #:gen-string #:gen-member #:gen-list)
  (:export #:run-tests))

(in-package #:cl-org-kit/test)

(defun test-timeout-ms ()
  "Per-spec wall-clock budget; CL_ORG_TEST_TIMEOUT_MS overrides the default."
  (let ((value (uiop:getenv "CL_ORG_TEST_TIMEOUT_MS")))
    (if value
        (parse-integer value)
        10000)))

(defun run-tests ()
  "Run every registered spec and return true only when all of them pass.
An empty suite counts as a failure so a broken registration cannot pass."
  (let ((passed (run-all :reporter :spec :pass-with-no-tests nil
                         :timeout-ms (test-timeout-ms))))
    (when passed
      (format t "~&cl-org-kit/test: successful completion with 0 failures~%"))
    passed))

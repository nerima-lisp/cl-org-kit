;;;; t/package.lisp
(defpackage #:cl-org-kit/test
  (:use #:cl #:org-kit)
  ;; DESCRIBE clashes with CL:DESCRIBE, so shadow-import cl-weave's.
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave
   #:it #:it-sequential #:it-each #:it-property #:expect #:signals #:run-all
   #:with-soft-assertions
   #:gen-integer #:gen-string #:gen-member #:gen-list)
  (:export #:run-tests))

(in-package #:cl-org-kit/test)

(defvar *coverage-run-p* nil)

(defstruct allocation-budget-sample
  bytes
  values
  condition
  at-signal)

(defun measure-allocation-budget (prepare)
  (labels ((measure (thunk)
             (let ((before (sb-ext:get-bytes-consed))
                   (at-signal nil))
               (let* ((outcome
                        (handler-case
                            (list :values
                                  (multiple-value-list
                                   (handler-bind
                                       ((org-resource-limit-error
                                         (lambda (condition)
                                           (declare (ignore condition))
                                           (unless at-signal
                                             (setf at-signal
                                                   (- (sb-ext:get-bytes-consed)
                                                      before))))))
                                     (funcall thunk))))
                          (org-resource-limit-error (condition)
                            (list :condition condition))))
                      (bytes (- (sb-ext:get-bytes-consed) before)))
                 (make-allocation-budget-sample
                  :bytes bytes
                  :values (getf outcome :values)
                  :condition (getf outcome :condition)
                  :at-signal at-signal)))))
    (handler-case
        (funcall (funcall prepare))
      (org-resource-limit-error () nil))
    (let ((samples
            (loop repeat 3
                  collect (let ((thunk (funcall prepare)))
                            (sb-ext:gc :full t)
                            (measure thunk)))))
      (values (reduce #'min samples
                      :key #'allocation-budget-sample-bytes)
              samples))))

(defun test-timeout-ms ()
  "Per-spec wall-clock budget; CL_ORG_TEST_TIMEOUT_MS overrides the default."
  (let ((value (uiop:getenv "CL_ORG_TEST_TIMEOUT_MS")))
    (if value
        (parse-integer value)
        10000)))

(defun run-tests ()
  "Run every registered spec and return true only when all of them pass.
An empty suite counts as a failure so a broken registration cannot pass."
  ;; Keep specs serial because SBCL allocation and coverage counters are process-wide.
  (let ((passed (run-all
                 :reporter :spec :pass-with-no-tests nil
                 :timeout-ms (test-timeout-ms)
                 :max-workers 1)))
    (when passed
      (format t "~&cl-org-kit/test: successful completion with 0 failures~%"))
    passed))

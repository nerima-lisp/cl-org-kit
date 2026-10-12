;;;; t/limits-test.lisp
(in-package #:cl-org-kit/test)

(describe "resource limits"
  (it "has documented defaults"
    (let ((limits (make-limits)))
      (with-soft-assertions
        (expect (limits-p limits) :to-be t)
        (expect (limits-p 3) :to-be nil)
        (expect (limit-value limits :max-input-length) :to-be (* 64 1024 1024))
        (expect (limit-value limits :max-heading-depth) :to-be 256)
        (expect (limit-value limits :max-element-depth) :to-be 512)
        (expect (limit-value limits :max-object-depth) :to-be 128)
        (expect (limit-value limits :max-node-count) :to-be (* 4 1024 1024))
        (expect (limit-value limits :max-output-length) :to-be (* 64 1024 1024)))))

  (it "accepts overrides"
    (let ((limits (make-limits :max-input-length 1 :max-heading-depth 2
                               :max-element-depth 3 :max-object-depth 4
                               :max-node-count 5 :max-output-length 6)))
      (expect (mapcar (lambda (name) (limit-value limits name))
                      '(:max-input-length :max-heading-depth :max-element-depth
                        :max-object-depth :max-node-count :max-output-length))
              :to-equal '(1 2 3 4 5 6))))

  (it "rejects a negative limit and an unknown limit name"
    (with-soft-assertions
      (signals type-error (make-limits :max-node-count -1))
      (signals error (limit-value (make-limits) :max-nothing))))

  (it "passes values at the limit and signals above it"
    (let ((limits (make-limits :max-node-count 3)))
      (with-soft-assertions
        (expect (org-kit::check-limit limits :max-node-count 3) :to-be 3)
        (let ((condition (handler-case (org-kit::check-limit limits :max-node-count 4)
                           (org-resource-limit-error (c) c))))
          (expect (org-resource-limit-error-limit-name condition) :to-be :max-node-count)
          (expect (org-resource-limit-error-limit condition) :to-be 3)
          (expect (org-resource-limit-error-actual condition) :to-be 4))))))

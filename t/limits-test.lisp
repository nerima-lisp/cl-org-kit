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
      (dolist (name '(:max-input-length :max-heading-depth :max-element-depth
                      :max-object-depth :max-node-count :max-output-length))
        (signals type-error (apply #'make-limits (list name -1))))
      (signals error (limit-value (make-limits) :max-nothing))))

  (it "passes a limit and signals when public parsing exceeds it"
    (let* ((document (parse-org "" :limits (make-limits :max-node-count 1)))
           (node-limit-error
             (handler-case
                 (progn (parse-org "" :limits (make-limits :max-node-count 0)) nil)
               (org-resource-limit-error (condition) condition))))
      (with-soft-assertions
        (expect (document-p document) :to-be t)
        (expect (org-resource-limit-error-limit-name node-limit-error)
                :to-be :max-node-count)
        (expect (org-resource-limit-error-limit node-limit-error) :to-be 0)
        (expect (org-resource-limit-error-actual node-limit-error) :to-be 1)
        (signals org-resource-limit-error
          (parse-org "*a /b/*" :limits (make-limits :max-object-depth 1)))))))

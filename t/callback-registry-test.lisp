;;;; t/callback-registry-test.lisp
(in-package #:cl-org-kit/test)

(defvar *callback-registry-test-probe* nil)

(describe "explicit callback registry"
  (it "passes an Emacs Lisp expression unchanged to the registered evaluator"
    (let* ((registry (make-callback-registry))
           (expression '(setf *callback-registry-test-probe* t))
           (received nil))
      (register-callback registry :evaluator :emacs-lisp
                         (lambda (form)
                           (setf received form)
                           :evaluated))
      (with-soft-assertions
        (expect (callback-registry-p registry) :to-be t)
        (expect (evaluate-elisp registry expression) :to-be :evaluated)
        (expect (eq received expression) :to-be t)
        (expect *callback-registry-test-probe* :to-be nil))))

  (it "signals the named evaluator condition without evaluating an expression"
    (let* ((registry (make-callback-registry))
           (expression '(setf *callback-registry-test-probe* t))
           (condition
             (handler-case
                 (progn (evaluate-elisp registry expression) nil)
               (org-elisp-evaluator-error (condition) condition))))
      (with-soft-assertions
        (expect (typep condition 'org-elisp-evaluator-error) :to-be t)
        (when condition
          (expect (org-callback-error-kind condition) :to-equal "EVALUATOR")
          (expect (org-callback-error-name condition) :to-equal "emacs-lisp")
          (expect (org-callback-error-reason condition) :to-be :unregistered))
        (expect *callback-registry-test-probe* :to-be nil))))

  (it "reuses kind and name registration for language and crypt callbacks"
    (let ((registry (make-callback-registry)))
      (register-callback registry :babel-executor "python"
                         (lambda (source) (concatenate 'string "ran:" source)))
      (register-callback registry :crypt-encrypt :org-crypt
                         (lambda (text) (concatenate 'string "sealed:" text)))
      (with-soft-assertions
        (expect (invoke-callback registry :babel-executor "python" "print(1)")
                :to-equal "ran:print(1)")
        (expect (invoke-callback registry :crypt-encrypt "org-crypt" "secret")
                :to-equal "sealed:secret"))))

  (it "propagates a registered callback condition by identity"
    (let* ((registry (make-callback-registry))
           (condition (make-condition 'simple-error
                                      :format-control "callback failure"))
           (caught nil))
      (register-callback registry :evaluator :emacs-lisp
                         (lambda (expression)
                           (declare (ignore expression))
                           (error condition)))
      (setf caught
            (handler-case
                (progn (evaluate-elisp registry "(+ 1 2)") nil)
              (simple-error (condition) condition)))
      (expect (eq caught condition) :to-be t)))

  (it "classifies missing generic callbacks"
    (let* ((registry (make-callback-registry))
           (condition
             (handler-case
                 (progn (invoke-callback registry :babel-executor "unknown") nil)
               (org-callback-error (condition) condition))))
      (with-soft-assertions
        (expect (typep condition 'org-callback-error) :to-be t)
        (when condition
          (expect (org-callback-error-kind condition) :to-equal "BABEL-EXECUTOR")
          (expect (org-callback-error-name condition) :to-equal "unknown")
          (expect (org-callback-error-reason condition) :to-be :unregistered))))))

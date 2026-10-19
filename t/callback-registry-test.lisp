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
          (expect (org-callback-error-reason condition) :to-be :unregistered)))))

  (it "normalizes callback keys and rejects invalid registrations"
    (let ((registry (make-callback-registry)))
      (with-soft-assertions
        (expect (callback-registry-p nil) :to-be nil)
        (expect (org-kit::callback-key-part "MiXeD" :upper) :to-equal "MIXED")
        (expect (org-kit::callback-key-part 'MiXeD :lower) :to-equal "mixed")
        (expect (handler-case
                    (progn (org-kit::callback-key-part 12 :lower) nil)
                  (type-error () t))
                :to-be t)
        (expect (handler-case
                    (progn (register-callback nil :kind :name #'identity) nil)
                  (type-error () t))
                :to-be t)
        (expect (handler-case
                    (progn (register-callback registry :kind :name 12) nil)
                  (type-error () t))
                :to-be t)
        (expect (handler-case
                    (progn (invoke-callback nil :kind :name) nil)
                  (type-error () t))
                :to-be t)
        (register-callback registry "KiNd" "NaMe" #'identity)
        (expect (invoke-callback registry :kind :name "value")
                :to-equal "value")))))

(describe "callback timeout reporting"
  (it "propagates a caller-reported timeout with callback identity"
    (let* ((registry (make-callback-registry))
           (timeout (make-condition 'org-callback-timeout-error
                                    :kind "BABEL-EXECUTOR"
                                    :name "PYTHON"))
           (caught nil))
      (register-callback registry :babel-executor "python"
                         (lambda (source)
                           (declare (ignore source))
                           (error timeout)))
      (setf caught
            (handler-case
                (progn (invoke-callback registry :babel-executor "python" "work") nil)
              (org-callback-timeout-error (condition) condition)))
      (with-soft-assertions
        (expect (typep caught 'org-callback-timeout-error) :to-be t)
        (when caught
          (expect (eq caught timeout) :to-be t)
          (expect (org-callback-error-kind caught) :to-equal "BABEL-EXECUTOR")
          (expect (org-callback-error-name caught) :to-equal "PYTHON")
          (expect (org-callback-error-reason caught) :to-be :timeout))))))

(describe "callback identity condition reports"
  (it "escapes ASCII and C1 controls and Unicode line separators in callback identities"
    (let* ((registry (make-callback-registry))
           (control-codes
             (append (loop for code below 32 collect code)
                     (loop for code from #x80 to #x9F collect code)
                     (list #x7F #x2028 #x2029)))
           (control-text (map 'string #'code-char control-codes))
           (kind (concatenate 'string "KiNd" control-text))
           (name (concatenate 'string "NaMe" control-text))
           (condition (handler-case
                          (progn (invoke-callback registry kind name) nil)
                        (org-callback-error (condition) condition)))
           (report (format nil "~A" condition))
           (expected-control
             (format nil "~{~A~}"
                     (loop for code in control-codes
                           collect (case code
                                     (#x0A "\\n")
                                     (#x0D "\\r")
                                     (#x09 "\\t")
                                     (otherwise (format nil "\\x~2,'0X" code))))))
           (expected-kind
             (concatenate 'string "kind" (string-downcase expected-control)))
           (expected-name (concatenate 'string "name" expected-control))
           (expected
             (format nil "No callback is registered (~A/~A, unregistered)"
                     expected-kind expected-name)))
      (expect report :to-equal expected)))

  (it "truncates callback identity inputs at the condition text limit"
    (let* ((registry (make-callback-registry))
           (limit org-kit::*condition-text-limit*)
           (kind (concatenate 'string (string #\Newline)
                              (make-string limit :initial-element (code-char 65))))
           (name (concatenate 'string (string (code-char 0))
                              (make-string limit :initial-element (code-char 66))))
           (condition (handler-case
                          (progn (invoke-callback registry kind name) nil)
                        (org-callback-error (condition) condition)))
           (report (format nil "~A" condition))
           (expected-kind
             (concatenate 'string "\\n"
                          (make-string (1- limit) :initial-element (code-char 97)) "..."))
           (expected-name
             (concatenate 'string "\\x00"
                          (make-string (1- limit) :initial-element (code-char 98)) "...")))
      (expect report
              :to-equal (format nil "No callback is registered (~A/~A, unregistered)"
                                expected-kind expected-name)))))

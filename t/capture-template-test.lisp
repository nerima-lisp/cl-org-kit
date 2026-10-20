(in-package #:cl-org-kit/test)
(defun capture-template-test-time-context ()
  (make-time-context :reference-time (make-org-time-value "2024-02-19T12:34:00")
                     :zone (resolve-time-zone "UTC")))
(defun capture-template-test-condition (thunk)
  (handler-case (progn (funcall thunk) nil)
    (error (condition) condition)))
(describe "capture template expansion"
  (it "expands a prompt and all reference-time forms with the first cursor"
    (let* ((result
             (expand-capture-template
              "* %^{Title}%? %t %T %u %U"
              :time-context (capture-template-test-time-context)
              :prompt-answers '(("Title" . "Draft")))))
      (with-soft-assertions
        (expect (capture-template-result-p result) :to-be t)
        (expect (capture-template-result-expanded-text result)
                :to-equal
                "* Draft <2024-02-19 Mon> <2024-02-19 Mon 12:34> [2024-02-19 Mon] [2024-02-19 Mon 12:34]")
        (expect (capture-template-result-cursor-offset result) :to-be 7))))
  (it "uses caller-supplied initial text, annotation, tags, time and file contents"
    (let ((result
            (expand-capture-template
             (concatenate 'string "Intro" (string #\Newline) "  %i"
                          (string #\Newline)
                          "%a %^g %^t %[memo.org]")
             :time-context (capture-template-test-time-context)
             :initial (format nil "one~%two")
             :annotation "https://example.test/"
             :tag-answer '("work" "urgent")
             :prompted-time "2024-02-20"
             :file-contents '(("memo.org" . "memo body")))))
      (with-soft-assertions
        (expect (capture-template-result-expanded-text result)
                :to-equal
                (format nil
                        "Intro~%  one~%  two~%https://example.test/ :work:urgent: <2024-02-20 Tue> memo body"))
        (expect (capture-template-result-cursor-offset result) :to-be 0))))
  (it "passes an Emacs Lisp expression to the registered evaluator"
    (let ((callbacks (make-callback-registry))
          (received nil))
      (register-callback
       callbacks :evaluator :emacs-lisp
       (lambda (expression)
         (setf received expression)
         "evaluated"))
      (let ((result
              (expand-capture-template
               "before %(concat \"a\" (identity \"b\")) after"
               :time-context (capture-template-test-time-context)
               :callbacks callbacks)))
        (with-soft-assertions
          (expect received :to-equal "(concat \"a\" (identity \"b\"))")
          (expect (capture-template-result-expanded-text result)
                  :to-equal "before evaluated after")))))
  (it "signals the named evaluator condition when no evaluator is registered"
    (let ((condition
            (capture-template-test-condition
             (lambda ()
               (expand-capture-template
                "%(identity \"value\")"
                :time-context (capture-template-test-time-context)
                :callbacks (make-callback-registry))))))
      (with-soft-assertions
        (expect (typep condition 'org-elisp-evaluator-error) :to-be t)
        (when (typep condition 'org-elisp-evaluator-error)
          (expect (org-callback-error-reason condition) :to-be :unregistered)))))
  (it "signals missing-value for unanswered prompts and absent supplied files"
    (let* ((time-context (capture-template-test-time-context))
           (prompt-condition
             (capture-template-test-condition
              (lambda ()
                (expand-capture-template "%^{Title}"
                                         :time-context time-context))))
           (file-condition
             (capture-template-test-condition
              (lambda ()
                (expand-capture-template "%[missing.org]"
                                         :time-context time-context)))))
      (with-soft-assertions
        (expect (typep prompt-condition 'org-capture-error) :to-be t)
        (when (typep prompt-condition 'org-capture-error)
          (expect (org-capture-error-reason prompt-condition)
                  :to-be :missing-value))
        (expect (typep file-condition 'org-capture-error) :to-be t)
        (when (typep file-condition 'org-capture-error)
          (expect (org-capture-error-reason file-condition)
                  :to-be :missing-value)))))
  (it "enforces character and UTF-8 output limits"
    (let* ((time-context (capture-template-test-time-context))
           (character-condition
             (capture-template-test-condition
              (lambda ()
                (expand-capture-template "long"
                                         :time-context time-context
                                         :limits (make-limits
                                                  :max-output-length 3)))))
           (octet-condition
             (capture-template-test-condition
              (lambda ()
                (expand-capture-template "éé"
                                         :time-context time-context
                                         :limits (make-limits
                                                  :max-output-octets 3))))))
      (with-soft-assertions
        (expect (typep character-condition 'org-resource-limit-error) :to-be t)
        (when (typep character-condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name character-condition)
                  :to-be :max-output-length))
        (expect (typep octet-condition 'org-resource-limit-error) :to-be t)
        (when (typep octet-condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name octet-condition)
                  :to-be :max-output-octets))))))

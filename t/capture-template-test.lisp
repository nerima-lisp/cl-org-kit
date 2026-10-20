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
  (it "preserves non-ASCII current lines when indenting initial text"
    (let* ((time-context (capture-template-test-time-context))
           (result
             (expand-capture-template
              (format nil "見出し~%かな %i")
              :initial (format nil "日本語~%語")
              :time-context time-context)))
      (with-soft-assertions
        (expect (capture-template-result-expanded-text result)
                :to-equal
                (format nil "見出し~%かな 日本語~%かな 語")))))
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
                  :to-be :max-output-octets)))))
  (it "rejects invalid template values and incomplete placeholders"
    (let ((time-context (capture-template-test-time-context))
          (callbacks (make-callback-registry)))
      (labels ((condition-for (template &rest options)
                 (capture-template-test-condition
                  (lambda ()
                    (apply #'expand-capture-template template options))))
               (rejects (reason template &rest options)
                 (let ((condition (apply #'condition-for template options)))
                   (expect (typep condition 'org-capture-error) :to-be t)
                   (when (typep condition 'org-capture-error)
                     (expect (org-capture-error-reason condition) :to-be reason))
                   condition)))
        (register-callback
         callbacks :evaluator :emacs-lisp
         (lambda (expression)
           (declare (ignore expression))
           17))
        (with-soft-assertions
          (rejects :invalid-template 7)
          (rejects :invalid-limits "" :limits "invalid"
                   :time-context time-context)
          (rejects :invalid-time-context "")
          (rejects :missing-value "%i" :time-context time-context)
          (rejects :invalid-template-value "%i" :initial 7
                   :time-context time-context)
          (rejects :missing-value "%a" :time-context time-context)
          (rejects :invalid-template-value "%a" :annotation 7
                   :time-context time-context)
          (rejects :missing-value "%^g" :time-context time-context)
          (rejects :invalid-template-value "%^g" :tag-answer 7
                   :time-context time-context)
          (rejects :missing-value "%^t" :time-context time-context)
          (let ((condition
                  (rejects :invalid-time-value "%^t" :prompted-time 42
                           :time-context time-context)))
            (expect (org-capture-error-context condition) :to-be nil))
          (rejects :invalid-time-value "%^t" :prompted-time "not a date"
                   :time-context time-context)
          (let ((condition
                  (rejects :invalid-template-value "%^{Title}"
                           :prompt-answers '(("Title" . 7))
                           :time-context time-context)))
            (expect (org-capture-error-context condition) :to-equal "Title"))
          (rejects :invalid-template-value "%[memo]"
                   :file-contents '(("memo" . 7))
                   :time-context time-context)
          (rejects :invalid-template "%^{Title" :time-context time-context)
          (rejects :invalid-template "%[memo" :time-context time-context)
          (rejects :invalid-template "%(identity" :time-context time-context)
          (rejects :invalid-callback-result "%(identity)" :callbacks callbacks
                   :time-context time-context)))))
  (it "formats prompted time variants and preserves unsupported markers"
    (let ((time-context (capture-template-test-time-context)))
      (labels ((expanded-time (value)
                 (capture-template-result-expanded-text
                  (expand-capture-template "%^t"
                                           :prompted-time value
                                           :time-context time-context)))
               (expanded-tags (value)
                 (capture-template-result-expanded-text
                  (expand-capture-template "%^g"
                                           :tag-answer value
                                           :time-context time-context))))
        (let* ((cursor-result
                 (expand-capture-template "a%?b%?"
                                          :time-context time-context))
               (initial-result
                 (expand-capture-template "%i"
                                          :initial (format nil "one~%")
                                          :time-context time-context)))
          (with-soft-assertions
            (expect (mapcar #'expanded-time
                            (list "[kept]"
                                  "2024-02-20T13:05:00"
                                  (make-org-time-value "2024-02-20T13:05:00")
                                  (cl-date-kit:make-local-date 2024 2 20)
                                  (cl-date-kit:make-local-date-time
                                   (cl-date-kit:make-local-date 2024 2 20)
                                   (cl-date-kit:make-local-time 13 5 0))))
                    :to-equal
                    (list "[kept]"
                          "<2024-02-20 Tue 13:05>"
                          "<2024-02-20 Tue 13:05>"
                          "<2024-02-20 Tue>"
                          "<2024-02-20 Tue 13:05>"))
            (expect (expanded-tags '(":work:" " urgent "))
                    :to-equal ":work:urgent:")
            (expect (expanded-tags " :solo: ") :to-equal ":solo:")
            (expect (expanded-tags nil) :to-equal "")
            (expect (capture-template-result-expanded-text cursor-result)
                    :to-equal "ab")
            (expect (capture-template-result-cursor-offset cursor-result)
                    :to-be 1)
            (expect (capture-template-result-expanded-text initial-result)
                    :to-equal (format nil "one~%"))
            (expect
             (capture-template-result-expanded-text
              (expand-capture-template "%z %^x %"
                                       :time-context time-context))
             :to-equal "%z %^x %"))))))
  (it "scans escaped strings and comments and omits NIL results"
    (let ((callbacks (make-callback-registry))
          (received nil)
          (time-context (capture-template-test-time-context))
          (template
            (concatenate 'string
                         "%(call \"escaped "
                         "\\\" ) text\" ; ) ignored"
                         (string #\Newline)
                         " (nested)) tail")))
      (register-callback
       callbacks :evaluator :emacs-lisp
       (lambda (expression)
         (setf received expression)
         "done"))
      (let ((result
              (expand-capture-template template
                                       :callbacks callbacks
                                       :time-context time-context))
            (nil-callbacks (make-callback-registry)))
        (register-callback nil-callbacks :evaluator :emacs-lisp
                           (lambda (expression)
                             (declare (ignore expression))
                             nil))
        (let ((nil-result
                (expand-capture-template
                 "before %(identity) after"
                 :callbacks nil-callbacks
                 :time-context time-context)))
          (with-soft-assertions
            (expect received
                    :to-equal
                    (concatenate 'string
                                 "(call \"escaped \\\" ) text\" ; ) ignored"
                                 (string #\Newline)
                                 " (nested))"))
            (expect (capture-template-result-expanded-text result)
                    :to-equal "done tail")
            (expect (capture-template-result-expanded-text nil-result)
                    :to-equal "before  after"))))))
  (it "rejects short prompted times and non-list prompt answers"
    (let* ((time-context (capture-template-test-time-context))
           (prompt-condition
             (capture-template-test-condition
              (lambda ()
                (expand-capture-template "%^{Title}"
                                         :time-context time-context
                                         :prompt-answers #()))))
           (time-condition
             (capture-template-test-condition
              (lambda ()
                (expand-capture-template "%^t"
                                         :time-context time-context
                                         :prompted-time ""))))
           (input-limit-condition
             (capture-template-test-condition
              (lambda ()
                (expand-capture-template "long"
                                         :time-context time-context
                                         :limits (make-limits
                                                  :max-input-length 3))))))
      (with-soft-assertions
        (expect (typep prompt-condition 'org-capture-error) :to-be t)
        (when (typep prompt-condition 'org-capture-error)
          (expect (org-capture-error-reason prompt-condition)
                  :to-be :missing-value))
        (expect (typep time-condition 'org-capture-error) :to-be t)
        (when (typep time-condition 'org-capture-error)
          (expect (org-capture-error-reason time-condition)
                  :to-be :invalid-time-value))
        (expect (typep input-limit-condition 'org-resource-limit-error)
                :to-be t)
        (when (typep input-limit-condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name input-limit-condition)
                  :to-be :max-input-length)))))
  (it-sequential "keeps CPU-time growth near linear for long single-line templates"
    (let* ((time-context (capture-template-test-time-context))
           (short-template (make-string 4096 :initial-element #\x))
           (long-template (make-string 8192 :initial-element #\x))
           (warmup-pairs 2)
           (sample-pairs 5)
           (repetitions 8))
      (labels ((measure (template)
                 (sb-ext:gc :full t)
                 (let ((before-bytes (sb-ext:get-bytes-consed))
                       (start-time (get-internal-run-time))
                       (result nil))
                   (loop repeat repetitions
                         do (setf result
                                  (expand-capture-template
                                   template :time-context time-context)))
                   (let ((elapsed (- (get-internal-run-time) start-time))
                         (allocated (- (sb-ext:get-bytes-consed)
                                       before-bytes)))
                     (values elapsed allocated
                             (capture-template-result-expanded-text
                              result)))))
               (median (numbers)
                 (nth (floor (length numbers) 2)
                      (sort (copy-list numbers) #'<))))
        (with-soft-assertions
          (let ((samples nil))
            (loop for pair from 0 below (+ warmup-pairs sample-pairs)
                  for short-first = (evenp pair)
                  for first-template = (if short-first
                                           short-template long-template)
                  for second-template = (if short-first
                                            long-template short-template)
                  do (multiple-value-bind
                         (first-time first-allocation first-output)
                         (measure first-template)
                       (multiple-value-bind
                           (second-time second-allocation second-output)
                           (measure second-template)
                         (expect first-output :to-equal first-template)
                         (expect second-output :to-equal second-template)
                         (when (>= pair warmup-pairs)
                           (push (if short-first
                                     (list first-time second-time
                                           first-allocation second-allocation)
                                     (list second-time first-time
                                           second-allocation first-allocation))
                                 samples)))))
            (let* ((time-ratios
                     (mapcar (lambda (sample)
                               (/ (float (second sample))
                                  (max 1 (first sample))))
                             samples))
                   (allocation-ratios
                     (mapcar (lambda (sample)
                               (/ (float (fourth sample))
                                  (max 1 (third sample))))
                             samples)))
              (expect (= (length samples) sample-pairs) :to-be t)
              (expect (every (lambda (sample)
                               (and (> (first sample) 0)
                                    (> (second sample) 0)))
                             samples)
                      :to-be t)
              (expect (< (median time-ratios) 3.25) :to-be t)
              (expect (< (median allocation-ratios) 3) :to-be t)))))))
  (it-sequential "checks initial expansion limits before copying large text"
    (let* ((initial (make-string 262144 :initial-element #\x))
           (time-context (capture-template-test-time-context))
           (before (progn (sb-ext:gc :full t)
                          (sb-ext:get-bytes-consed)))
           (condition
             (capture-template-test-condition
              (lambda ()
                (expand-capture-template
                 "%i" :initial initial
                 :time-context time-context
                 :limits (make-limits :max-output-length 128)))))
           (allocated (- (sb-ext:get-bytes-consed) before)))
      (with-soft-assertions
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when (typep condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name condition)
                  :to-be :max-output-length)
          (expect (org-resource-limit-error-actual condition)
                  :to-be (length initial)))
        (expect (< allocated (floor (length initial) 2)) :to-be t)))))

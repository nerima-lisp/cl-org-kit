(in-package #:cl-org-kit/test)

(defun diary-sexp-test-source (text &optional limits)
  (org-kit::diary-sexp-source-for-node
   (make-node :diary-sexp :properties (list :value text))
   limits))

(defun diary-sexp-test-match (text date)
  (org-kit::diary-sexp-source-match-p (diary-sexp-test-source text)
                             date
                             (make-callback-registry)))

(defun diary-sexp-test-condition (function)
  (handler-case (progn
                  (funcall function)
                  nil)
    (org-agenda-error (condition)
      condition)
    (org-resource-limit-error (condition)
      condition)))

(describe "diary sexp expressions"
          (it "matches built-in date, inclusive block, and cyclic boundaries"
              (let ((christmas (cl-date-kit:make-local-date 2026 12 25))
                    (eve (cl-date-kit:make-local-date 2026 12 24))
                    (block-end (cl-date-kit:make-local-date 2026 12 26))
                    (after-block (cl-date-kit:make-local-date 2026 12 27))
                    (cyclic-day (cl-date-kit:make-local-date 2026 10 23))
                    (between-cycles (cl-date-kit:make-local-date 2026 10 22)))
                (with-soft-assertions
                 (expect
                  (diary-sexp-test-match "%%(diary-date 12 25 2026)" christmas)
                  :to-be
                  t)
                 (expect (diary-sexp-test-match "%%(diary-date 12 25 2026)" eve)
                         :to-be
                         nil)
                 (expect
                  (diary-sexp-test-match "%%(diary-block 12 24 2026 12 26 2026)"
                                         eve)
                  :to-be
                  t)
                 (expect
                  (diary-sexp-test-match "%%(diary-block 12 24 2026 12 26 2026)"
                                         block-end)
                  :to-be
                  t)
                 (expect
                  (diary-sexp-test-match "%%(diary-block 12 24 2026 12 26 2026)"
                                         after-block)
                  :to-be
                  nil)
                 (expect
                  (diary-sexp-test-match "%%(diary-cyclic 3 10 20 2026)"
                                         cyclic-day)
                  :to-be
                  t)
                 (expect
                  (diary-sexp-test-match "%%(diary-cyclic 3 10 20 2026)"
                                         between-cycles)
                  :to-be
                  nil))))
          (it
           "matches anniversaries and positive or negative weekday occurrences"
           (let ((anniversary (cl-date-kit:make-local-date 2026 7 4))
                 (day-after (cl-date-kit:make-local-date 2026 7 5))
                 (leap-day (cl-date-kit:make-local-date 2024 2 29))
                 (shifted-leap-day (cl-date-kit:make-local-date 2025 3 1))
                 (second-monday (cl-date-kit:make-local-date 2026 5 11))
                 (first-monday (cl-date-kit:make-local-date 2026 5 4))
                 (last-friday (cl-date-kit:make-local-date 2026 5 29)))
             (with-soft-assertions
              (expect
               (diary-sexp-test-match "%%(diary-anniversary 7 4 2020)"
                                      anniversary)
               :to-be
               t)
              (expect
               (diary-sexp-test-match "%%(diary-anniversary 7 4 2020)"
                                      day-after)
               :to-be
               nil)
              (expect
               (diary-sexp-test-match "%%(diary-anniversary 2 29 2020)"
                                      leap-day)
               :to-be
               t)
              (expect
               (diary-sexp-test-match "%%(diary-anniversary 2 29 2020)"
                                      shifted-leap-day)
               :to-be
               t)
              (expect
               (diary-sexp-test-match "%%(diary-float 5 1 2)" second-monday)
               :to-be
               t)
              (expect
               (diary-sexp-test-match "%%(diary-float 5 1 2)" first-monday)
               :to-be
               nil)
              (expect
               (diary-sexp-test-match "%%(diary-float 5 5 -1)" last-friday)
               :to-be
               t)
              (expect
               (diary-sexp-test-match "%%(diary-float 12 1 1 30)"
                                      (cl-date-kit:make-local-date 2027 1 4))
               :to-be
               t)
              (expect
               (diary-sexp-test-match "%%(diary-float 12 1 1 30)"
                                      (cl-date-kit:make-local-date 2027 1 11))
               :to-be
               nil)
              (expect
               (diary-sexp-test-match "%%(diary-float 12 1 -1 1)"
                                      (cl-date-kit:make-local-date 2026 11 30))
               :to-be
               t)
              (expect
               (diary-sexp-test-match "%%(diary-float 12 1 -1 1)"
                                      (cl-date-kit:make-local-date 2026 12 7))
               :to-be
               nil))))
          (it
           "passes unknown forms unchanged and ignores reader-like text in strings and comments"
           (let* ((text
                   (format nil
                           "%%(custom-fn \"paren ) #. #S(x) pkg::sym\" ; #. #S pkg::sym~% ?\\( ?\\) ?\\^()"))
                  (expected
                   (format nil
                           "(custom-fn \"paren ) #. #S(x) pkg::sym\" ; #. #S pkg::sym~% ?\\( ?\\) ?\\^()"))
                  (registry (make-callback-registry))
                  (calls 0)
                  (callback-expression nil)
                  (callback-context nil))
             (register-callback registry
                                :evaluator
                                :emacs-lisp
                                (lambda (expression context)
                                  (incf calls)
                                  (setf callback-expression expression
                                        callback-context context)
                                  t))
             (multiple-value-bind (expression entry found-p)
                 (org-kit::diary-sexp-source-parts text)
               (with-soft-assertions (expect found-p :to-be t)
                                     (expect expression :to-equal expected)
                                     (expect entry :to-equal "")))
             (let* ((source (diary-sexp-test-source text))
                    (date (cl-date-kit:make-local-date 2026 10 20)))
               (with-soft-assertions
                (expect (org-kit::diary-sexp-source-match-p source date registry)
                        :to-be
                        t)
                (expect calls :to-be 1)
                (expect callback-expression :to-equal expected)
                (expect callback-context
                        :to-equal
                        '(:entry "" :date (10 20 2026)))))))
          (it
           "rejects reader evaluation, structures, and package-qualified symbols before callbacks"
           (dolist
               (text
                '("%%(custom-fn #.(progn 1))" "%%(custom-fn #S(foo))"
                                              "%%(custom-fn pkg::sym)"))
             (let ((registry (make-callback-registry))
                   (calls 0))
               (register-callback registry
                                  :evaluator
                                  :emacs-lisp
                                  (lambda (&rest arguments)
                                    (declare (ignore arguments))
                                    (incf calls)
                                    t))
               (let ((condition
                      (diary-sexp-test-condition
                       (lambda ()
                         (org-kit::diary-sexp-source-match-p
                          (diary-sexp-test-source text)
                          (cl-date-kit:make-local-date 2026 10 20)
                          registry)))))
                 (with-soft-assertions
                  (expect (typep condition 'org-agenda-error) :to-be t)
                  (when condition
                    (expect (org-agenda-error-reason condition)
                            :to-be
                            :invalid-diary-expression))
                  (expect calls :to-be 0)))))) (it
            "rejects malformed, deeply nested, and oversized expressions with named limits"
            (let* ((malformed
                    (diary-sexp-test-condition
                     (lambda ()
                       (diary-sexp-test-source
                        "%%(diary-date 2 29 \"unterminated)"))))
                   (deep-text
                    (format nil
                            "%%(unknown ~A0~A"
                            (make-string 8 :initial-element #\()
                            (make-string 9 :initial-element #\))))
                   (deep-condition
                    (diary-sexp-test-condition
                     (lambda ()
                       (diary-sexp-test-source deep-text
                                               (make-limits
                                                :max-diary-expression-depth
                                                4)))))
                   (long-text
                    (format nil
                            "%%(unknown \"~A\")"
                            (make-string 2000 :initial-element #\x)))
                   (long-condition
                    (handler-case (progn
                                    (diary-sexp-test-source long-text
                                                            (make-limits
                                                             :max-diary-expression-length
                                                             64))
                                    nil)
                      (org-resource-limit-error (condition)
                        condition))))
              (with-soft-assertions
               (expect (typep malformed 'org-agenda-error) :to-be t)
               (when malformed
                 (expect (org-agenda-error-reason malformed)
                         :to-be
                         :invalid-diary-expression))
               (expect (typep deep-condition 'org-resource-limit-error)
                       :to-be
                       t)
               (when deep-condition
                 (expect (org-resource-limit-error-limit-name deep-condition)
                         :to-be
                         :max-diary-expression-depth)
                 (expect (org-resource-limit-error-limit deep-condition)
                         :to-be
                         4)
                 (expect (org-resource-limit-error-actual deep-condition)
                         :to-be
                         5))
               (expect (typep long-condition 'org-resource-limit-error)
                       :to-be
                       t)
               (when long-condition
                 (expect (org-resource-limit-error-limit-name long-condition)
                         :to-be
                         :max-diary-expression-length)
                 (expect (org-resource-limit-error-limit long-condition)
                         :to-be
                         64)
                 (expect (org-resource-limit-error-actual long-condition)
                         :to-be
                         65))))))
(describe "diary sexp parser and built-in edge cases"
  (it "parses built-in comments, escaped strings, quote marks, and signed integers"
    (let* ((date (cl-date-kit:make-local-date 2026 12 25))
           (commented (format nil
                              "%%(diary-date +12 +25 +2026 ; note~% \"a\\n\\r\\t\\z\")"))
           (quote-mark (format nil "%%(diary-date 12 25 2026 ~Cmark)"
                               (code-char 39))))
      (with-soft-assertions
        (expect (diary-sexp-test-match commented date) :to-be t)
        (expect (diary-sexp-test-match quote-mark date) :to-be t)
        (expect (diary-sexp-test-match
                 "%%(diary-date t 25 2026)"
                 (cl-date-kit:make-local-date 2026 1 25))
                :to-be t)
        (expect (diary-sexp-test-match "%%(diary-date (1 12) 25 2026)" date)
                :to-be t)
        (expect (diary-sexp-test-match "%%(diary-date nil 25 2026)" date)
                :to-be nil)
        (expect (diary-sexp-test-match "%%(diary-date month 25 2026)" date)
                :to-be nil))))
  (it "rejects invalid integer tokens and unclosed expressions beyond the limit"
    (let* ((invalid-token
             (diary-sexp-test-condition
              (lambda ()
                (diary-sexp-test-source "%%(diary-date 12x 25 2026)"))))
           (overflow
             (diary-sexp-test-condition
              (lambda ()
                (diary-sexp-test-source
                 (format nil "%%(diary-date ~A 25 2026)"
                         (make-string 100 :initial-element #\9))))))
           (long-unclosed
             (handler-case
                 (progn
                   (diary-sexp-test-source
                    (format nil "%%(unknown ~A"
                            (make-string 128 :initial-element #\x))
                    (make-limits :max-diary-expression-length 64))
                   nil)
               (org-resource-limit-error (condition)
                 condition))))
      (with-soft-assertions
        (expect (typep invalid-token
                       (find-class (find-symbol "ORG-AGENDA-ERROR" "ORG-KIT")))
                :to-be t)
        (expect (typep overflow
                       (find-class (find-symbol "ORG-AGENDA-ERROR" "ORG-KIT")))
                :to-be t)
        (expect (typep long-unclosed
                       (find-class
                        (find-symbol "ORG-RESOURCE-LIMIT-ERROR" "ORG-KIT")))
                :to-be t)
        (when long-unclosed
          (expect (org-resource-limit-error-limit-name long-unclosed)
                  :to-be :max-diary-expression-length)
          (expect (org-resource-limit-error-actual long-unclosed)
                  :to-be 65)))))
  (it "handles optional and invalid date, block, cyclic, and anniversary arguments"
    (let ((date (cl-date-kit:make-local-date 2026 12 25)))
      (with-soft-assertions
        (expect (diary-sexp-test-match "%%(diary-date 12 25)" date) :to-be nil)
        (expect
         (diary-sexp-test-match "%%(diary-block 12 24 2026 12 26 2026 t)" date)
         :to-be t)
        (expect
         (diary-sexp-test-match "%%(diary-block 2 30 2026 12 26 2026)" date)
         :to-be nil)
        (expect (diary-sexp-test-match "%%(diary-block 12 25 2026)" date)
                :to-be nil)
        (expect (diary-sexp-test-match "%%(diary-cyclic 1 12 25 2026 t)" date)
                :to-be t)
        (expect (diary-sexp-test-match "%%(diary-cyclic 0 12 25 2026)" date)
                :to-be nil)
        (expect (diary-sexp-test-match "%%(diary-cyclic 1 12 26 2026)" date)
                :to-be nil)
        (expect (diary-sexp-test-match "%%(diary-cyclic 1 12 25)" date)
                :to-be nil)
        (expect (diary-sexp-test-match "%%(diary-anniversary 12 25)" date)
                :to-be t)
        (expect (diary-sexp-test-match "%%(diary-anniversary 13 25)" date)
                :to-be nil))))
  (it "matches floating month boundaries and rejects invalid selectors"
    (with-soft-assertions
      (expect
       (diary-sexp-test-match "%%(diary-float 12 4 -1)"
                              (cl-date-kit:make-local-date 2026 12 31))
       :to-be t)
      (expect
       (diary-sexp-test-match "%%(diary-float t 1 1)"
                              (cl-date-kit:make-local-date 2027 1 4))
       :to-be t)
      (expect
       (diary-sexp-test-match "%%(diary-float (5 12) 1 1)"
                              (cl-date-kit:make-local-date 2026 12 7))
       :to-be t)
      (expect
       (diary-sexp-test-match "%%(diary-float 5 1 0)"
                              (cl-date-kit:make-local-date 2026 5 4))
       :to-be nil)
      (expect
       (diary-sexp-test-match "%%(diary-float 5 7 1)"
                              (cl-date-kit:make-local-date 2026 5 4))
       :to-be nil)
      (expect
       (diary-sexp-test-match "%%(diary-float 5 1)"
                              (cl-date-kit:make-local-date 2026 5 4))
       :to-be nil)
      (expect
       (diary-sexp-test-match "%%(diary-float 5 1 1 0)"
                              (cl-date-kit:make-local-date 2026 5 4))
       :to-be nil)
      (expect
       (diary-sexp-test-match "%%(diary-float month 1 1)"
                              (cl-date-kit:make-local-date 2026 5 4))
       :to-be nil))))

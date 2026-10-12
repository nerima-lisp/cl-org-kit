;;;; t/conditions-test.lisp
(in-package #:cl-org-kit/test)

(defun report (condition)
  (princ-to-string condition))

(describe "bounded condition text"
  (it "escapes control characters and backslashes"
    (expect (org-kit::bounded-text
             (coerce (list #\a #\\ #\Newline #\Return #\Tab (code-char 1) (code-char 127) #\b)
                     'string))
            :to-equal "a\\\\\\n\\r\\t\\x01\\x7Fb"))

  (it "truncates long text after the limit"
    (let ((text (org-kit::bounded-text (make-string 200 :initial-element #\x))))
      (with-soft-assertions
        (expect (length text) :to-be (+ org-kit::*condition-text-limit* 3))
        (expect (subseq text (- (length text) 3)) :to-equal "..."))))

  (it "keeps text at the limit intact and passes NIL through"
    (with-soft-assertions
      (expect (org-kit::bounded-text (make-string 80 :initial-element #\y))
              :to-equal (make-string 80 :initial-element #\y))
      (expect (org-kit::bounded-text nil) :to-be nil))))

(describe "condition hierarchy and reports"
  (it "roots every condition at ORG-KIT-ERROR"
    (with-soft-assertions
      (dolist (type '(org-parse-error org-query-error org-edit-error
                      org-resource-limit-error))
        (expect (subtypep type 'org-kit-error) :to-be t))
      (expect (subtypep 'org-kit-error 'error) :to-be t)))

  (it "reports the base condition message"
    (with-soft-assertions
      (expect (report (make-condition 'org-kit-error)) :to-equal "cl-org-kit error")
      (expect (org-kit-error-message (make-condition 'org-kit-error :message "m"))
              :to-equal "m")))

  (it "reports a parse error with and without its position"
    (let ((full (make-condition 'org-parse-error :message "Bad" :offset 4 :line 2
                                                 :column 3 :context "x" :source-name "f.org"))
          (bare (make-condition 'org-parse-error :message "Bad")))
      (with-soft-assertions
        (expect (report full) :to-equal "f.org: Bad at line 2, column 3 (offset 4) near \"x\"")
        (expect (report bare) :to-equal "Bad")
        (expect (list (org-parse-error-offset full) (org-parse-error-line full)
                      (org-parse-error-column full) (org-parse-error-context full)
                      (org-parse-error-source-name full))
                :to-equal '(4 2 3 "x" "f.org")))))

  (it "reports a query error"
    (let ((condition (make-condition 'org-query-error :message "Unexpected token"
                                                      :query "a/b" :position 2)))
      (with-soft-assertions
        (expect (report condition) :to-equal "Unexpected token at position 2 in query \"a/b\"")
        (expect (org-query-error-query condition) :to-equal "a/b")
        (expect (org-query-error-position condition) :to-be 2)
        (expect (report (make-condition 'org-query-error :message "Empty"))
                :to-equal "Empty"))))

  (it "reports an edit error"
    (let ((condition (make-condition 'org-edit-error :message "Refused"
                                                     :target "Tasks/One" :reason :ambiguous)))
      (with-soft-assertions
        (expect (report condition) :to-equal "Refused (ambiguous) for target Tasks/One")
        (expect (org-edit-error-target condition) :to-equal "Tasks/One")
        (expect (org-edit-error-reason condition) :to-be :ambiguous)
        (expect (report (make-condition 'org-edit-error :message "Refused"))
                :to-equal "Refused"))))

  (it "reports a resource limit error"
    (let ((condition (make-condition 'org-resource-limit-error
                                     :message "Resource limit exceeded"
                                     :limit-name :max-node-count :limit 10 :actual 11)))
      (with-soft-assertions
        (expect (report condition)
                :to-equal "Resource limit exceeded: max-node-count is 11, above the limit of 10")
        (expect (list (org-resource-limit-error-limit-name condition)
                      (org-resource-limit-error-limit condition)
                      (org-resource-limit-error-actual condition))
                :to-equal '(:max-node-count 10 11))))))

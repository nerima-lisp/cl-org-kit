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
                      org-table-error org-resource-limit-error org-babel-error))
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

  (it "reports a table calculation error"
    (let ((condition (make-condition 'org-table-error :message "Invalid result"
                                                       :target "$2" :reason :empty-range)))
      (with-soft-assertions
        (expect (report condition) :to-equal "Invalid result (empty-range) for target $2")
        (expect (org-table-error-target condition) :to-equal "$2")
        (expect (org-table-error-reason condition) :to-be :empty-range)
        (expect (report (make-condition 'org-table-error :message "Invalid result"))
                :to-equal "Invalid result"))))

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

(progn
(describe "time error reports"
  (it "reports reason and context when present and omits absent slots"
    (let ((full (make-condition 'org-time-error :message "Bad time"
                                                 :reason :invalid-time :context "date"))
          (bare (make-condition 'org-time-error :message "Bad time")))
      (with-soft-assertions
        (expect (subtypep 'org-time-error 'org-kit-error) :to-be t)
        (expect (org-time-error-reason full) :to-be :invalid-time)
        (expect (org-time-error-context full) :to-equal "date")
        (expect (report full) :to-equal "Bad time (invalid-time) near \"date\"")
        (expect (org-time-error-reason bare) :to-be nil)
        (expect (org-time-error-context bare) :to-be nil)
        (expect (report bare) :to-equal "Bad time")))))


(describe "callback and write-plan error reports"
  (it "reports callback and write-plan fields"
    (let ((callback (make-condition 'org-callback-error
                                    :message "Callback unavailable"
                                    :kind :evaluator :name "emacs-lisp"
                                    :reason :unregistered))
          (evaluator (make-condition 'org-elisp-evaluator-error
                                     :message "Evaluator unavailable"
                                     :kind :evaluator :name "emacs-lisp"
                                     :reason :unregistered))
          (write-plan (make-condition 'org-write-plan-error
                                      :message "Write plan rejected"
                                      :path "nested/output.txt"
                                      :reason :invalid-path))
          (bare (make-condition 'org-write-plan-error :message "Write failed")))
      (with-soft-assertions
        (expect (subtypep 'org-callback-error 'org-kit-error) :to-be t)
        (expect (subtypep 'org-elisp-evaluator-error 'org-callback-error) :to-be t)
        (expect (report callback)
                :to-equal "Callback unavailable (evaluator/emacs-lisp, unregistered)")
        (expect (list (org-callback-error-kind callback)
                      (org-callback-error-name callback)
                      (org-callback-error-reason callback))
                :to-equal '(:evaluator "emacs-lisp" :unregistered))
        (expect (report evaluator)
                :to-equal "Evaluator unavailable (evaluator/emacs-lisp, unregistered)")
        (expect (report write-plan)
                :to-equal "Write plan rejected (invalid-path) for nested/output.txt")
        (expect (list (org-write-plan-error-path write-plan)
                      (org-write-plan-error-reason write-plan)
                      (org-write-plan-error-cause write-plan))
                :to-equal '("nested/output.txt" :invalid-path nil))
        (expect (report bare) :to-equal "Write failed (nil)")
        (expect (list (org-write-plan-error-path bare)
                      (org-write-plan-error-reason bare)
                      (org-write-plan-error-cause bare))
                :to-equal '(nil nil nil))))))

(describe "agenda and export error reports"
  (it "reports agenda and export context and exposes their fields"
    (let ((agenda (make-condition 'org-agenda-error
                                  :message "Invalid agenda"
                                  :reason :invalid-option :context "calendar"))
          (bare-agenda (make-condition 'org-agenda-error :message "Invalid agenda"))
          (export (make-condition 'org-export-error
                                  :message "Export failed" :backend :html
                                  :reason :invalid-source :source "bad"))
          (bare-export (make-condition 'org-export-error :message "Export failed"))
          (unsupported (make-condition 'org-export-unsupported-syntax
                                       :message "Unsupported" :backend :html
                                       :source "citeproc" :position 7
                                       :feature :citation))
          (bare-unsupported
            (make-condition 'org-export-unsupported-syntax
                            :message "Unsupported")))
      (with-soft-assertions
        (expect (report agenda) :to-equal "Invalid agenda (invalid-option) near \"calendar\"")
        (expect (list (org-agenda-error-reason agenda)
                      (org-agenda-error-context agenda))
                :to-equal '(:invalid-option "calendar"))
        (expect (report bare-agenda) :to-equal "Invalid agenda")
        (expect (report export) :to-equal "Export failed (invalid-source) for HTML near \"bad\"")
        (expect (list (org-export-error-backend export)
                      (org-export-error-reason export)
                      (org-export-error-source export))
                :to-equal '(:html :invalid-source "bad"))
        (expect (report bare-export) :to-equal "Export failed")
        (expect (report unsupported)
                :to-equal "Unsupported (citation) at position 7 near \"citeproc\"")
        (expect (list (org-export-unsupported-syntax-backend unsupported)
                      (org-export-unsupported-syntax-source unsupported)
                      (org-export-unsupported-syntax-position unsupported)
                      (org-export-unsupported-syntax-feature unsupported))
                :to-equal '(:html "citeproc" 7 :citation))
        (expect (report bare-unsupported)
                :to-equal "Unsupported at position 0")))))
)

(describe "Babel error reports"
  (it "reports reason and context when present and omits absent slots"
    (let ((full (make-condition 'org-babel-error :message "Cannot resolve Babel input"
                                                 :reason :missing-result-resolver
                                                 :context "input"))
          (bare (make-condition 'org-babel-error :message "Cannot resolve Babel input")))
      (with-soft-assertions
        (expect (org-babel-error-reason full) :to-be :missing-result-resolver)
        (expect (org-babel-error-context full) :to-equal "input")
        (expect (report full)
                :to-equal "Cannot resolve Babel input (missing-result-resolver) near \"input\"")
        (expect (org-babel-error-reason bare) :to-be nil)
        (expect (org-babel-error-context bare) :to-be nil)
        (expect (report bare) :to-equal "Cannot resolve Babel input")))))

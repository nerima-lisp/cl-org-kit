;;;; t/query-match-test.lisp
(in-package #:cl-org-kit/test)

(defparameter +query-match-test-reference-time+
  (cl-date-kit:make-local-date-time
   (cl-date-kit:make-local-date 2026 10 13)
   (cl-date-kit:make-local-time 9 30 0)))

(defun query-match-test-predicate (source &optional reference-time limits context)
  (org-kit::query-parse-org-match source context (or limits (make-limits))
                                 reference-time))

(defun query-match-test-error (source &optional reference-time limits)
  (handler-case
      (progn
        (query-match-test-predicate source reference-time limits)
        nil)
    (org-query-error (condition) condition)))

(describe "Org match query parsing"
  (it "preserves boolean precedence, signs, and property aliases"
    (with-soft-assertions
      (expect (query-match-test-predicate "work|laptop+night")
              :to-equal
              '(:or (:tag := "work")
                    (:and (:tag := "laptop") (:tag := "night"))))
      (expect (query-match-test-predicate "+LEVEL=3+boss-TODO=\"DONE\"")
              :to-equal
              '(:and (:level := 3)
                     (:tag := "boss")
                     (:not (:todo := "DONE"))))
      (expect (mapcar (lambda (query)
                        (getf (cdr (query-match-test-predicate query)) :operator))
                      '("P==2" "P!=2" "P/=2" "P<>2" "P<=2" "P>=2" "P<2" "P>2"))
              :to-equal '(:= :!= :!= :!= :<= :>= :< :>))))

  (it "parses TODO clauses, tag regexes, and existence forms"
    (with-soft-assertions
      (expect (query-match-test-predicate "work/!-WAITING-NEXT")
              :to-equal
              '(:and (:tag := "work") (:todo-present)
                     (:not (:todo := "WAITING"))
                     (:not (:todo := "NEXT"))))
      (expect (query-match-test-predicate "/!+WAITING|+NEXT")
              :to-equal
              '(:and (:todo-present)
                     (:or (:todo := "WAITING") (:todo := "NEXT"))))
      (expect (query-match-test-predicate "SCHEDULED=*{.*}")
              :to-equal '(:exists :planning :scheduled))
      (expect (query-match-test-predicate "-SCHEDULED=*{.*}")
              :to-equal '(:missing :planning :scheduled))
      (expect (query-match-test-predicate "-CUSTOM=*{.*}")
              :to-equal '(:missing :property "CUSTOM"))))

  (it "types numeric and fixed-reference Org date values"
    (let* ((numeric (query-match-test-predicate "EFFORT<*2"))
           (date-query
             (query-match-test-predicate "DEADLINE<=\"<today>\""
                                         +query-match-test-reference-time+))
           (date-value (fourth date-query)))
      (with-soft-assertions
        (expect numeric :to-equal
                '(:property :name "EFFORT" :operator :< :value 2
                  :missing-as :zero :require-present-p t))
        (expect (first date-query) :to-be :planning)
        (expect (second date-query) :to-be :deadline)
        (expect (third date-query) :to-be :<=)
        (expect (cl-date-kit:local-date-p date-value) :to-be t)
        (expect (cl-date-kit:format-local-date date-value) :to-equal "2026-10-13"))))

  (it "normalizes TODO properties and compiles Emacs tag regexes"
    (let* ((predicate (query-match-test-predicate "TODO!=\"DONE\""))
           (regex-predicate (query-match-test-predicate "{^work$}"))
           (regex (third regex-predicate)))
      (with-soft-assertions
        (expect predicate :to-equal '(:todo :!= "DONE"))
        (expect (first regex-predicate) :to-be :tag)
        (expect (second regex-predicate) :to-be :matches)
        (expect (org-kit::query-regex-p regex) :to-be t))))

  (it "evaluates inherited match sections and property presence"
    (let* ((document
             (parse-org
              (format nil "#+TODO: TODO | DONE~%* TODO Task :work:~%:PROPERTIES:~%:EFFORT: 2~%:END:~%* DONE Complete :work:~%* Plain")))
           (active (query-match-at document "work/!"
                                   +query-match-test-reference-time+))
           (effort (query-match-at document "EFFORT<*3"
                                   +query-match-test-reference-time+))
           (regex (query-match-at document "{^work$}"
                                  +query-match-test-reference-time+)))
      (with-soft-assertions
        (expect (mapcar (lambda (node) (node-property node :raw-value)) active)
                :to-equal '("Task"))
        (expect (mapcar (lambda (node) (node-property node :raw-value)) effort)
                :to-equal '("Task"))
        (expect (mapcar (lambda (node) (node-property node :raw-value)) regex)
                :to-equal '("Task" "Complete")))))

  (it "requires a date-time reference for match evaluation"
    (let ((document (parse-org "* TODO Task")))
      (signals type-error
        (query-match-at document "TODO" nil))))

  (it "reports malformed syntax at its source position"
    (with-soft-assertions
      (expect (org-query-error-position (query-match-test-error "work&(laptop|night)"))
              :to-be 5)
      (expect (org-query-error-position (query-match-test-error "work/"))
              :to-be 5)
      (expect (org-query-error-position (query-match-test-error "tag/=X"))
              :to-be 5)
      (expect (org-query-error-position (query-match-test-error "{\\w}"))
              :to-be 1)
      (signals org-resource-limit-error
        (query-match-test-predicate "long" nil
                                    (make-limits :max-input-length 3))))))

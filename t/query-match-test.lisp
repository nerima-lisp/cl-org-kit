;;;; t/query-match-test.lisp
(in-package #:cl-org-kit/test)

(defparameter +query-match-test-reference-time+
  (cl-date-kit:make-local-date-time
   (cl-date-kit:make-local-date 2026 10 13)
   (cl-date-kit:make-local-time 9 30 0)))

(defun query-match-test-predicate (source &optional reference-time limits context)
  (org-kit::query-parse-org-match source context (or limits (make-limits))
                                 reference-time))

(defun query-match-test-public-error (document source)
  (handler-case
      (progn
        (query-match-at document source +query-match-test-reference-time+)
        nil)
    (org-query-error (condition) condition)))

(defun query-match-test-titles (nodes)
  (mapcar (lambda (node) (node-property node :raw-value)) nodes))

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

  (it "parses escaped tag and property syntax and decimal comparisons"
    (let ((decimal (query-match-test-predicate "EFFORT=-1.5")))
      (with-soft-assertions
        (expect (query-match-test-predicate "work\\+group")
                :to-equal '(:tag := "work+group"))
        (expect (getf (cdr (query-match-test-predicate "BUILD\\-TYPE=\"optimized\""))
                      :name)
                :to-equal "BUILD-TYPE")
        (expect (getf (cdr decimal) :value) :to-equal -3/2)
        (expect (query-match-test-predicate "CLOSED=*{.*}")
                :to-equal '(:exists :planning :closed)))))

  (it "keeps non-graphic escaped characters out of property names"
    (let* ((document (parse-org "* Plain"))
           (match (concatenate 'string "NAME\\" (string (code-char #x80))
                               "=value")))
      (expect (query-match-test-titles
               (query-match-at document match +query-match-test-reference-time+))
              :to-equal nil)))

  (it "decodes supported quoted-value escapes"
    (with-soft-assertions
      (expect (getf (cdr (query-match-test-predicate "LABEL=\"a\\nb\"")) :value)
              :to-equal (format nil "a~%b"))
      (expect (getf (cdr (query-match-test-predicate "LABEL=\"a\\rb\"")) :value)
              :to-equal (format nil "a~cb" #\Return))
      (expect (getf (cdr (query-match-test-predicate "LABEL=\"a\\tb\"")) :value)
              :to-equal (format nil "a~cb" #\Tab))
      (expect (getf (cdr (query-match-test-predicate "LABEL=\"a\\\\b\"")) :value)
              :to-equal "a\\b")
      (expect (getf (cdr (query-match-test-predicate "LABEL=\"a\\\"b\"")) :value)
              :to-equal "a\"b")))

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

  (it "matches tags, TODO states, levels, and properties through the public API"
    (let* ((document
             (parse-org
              (format nil
                      "#+TODO: TODO | DONE~%* TODO Build :work:~%:PROPERTIES:~%:EFFORT: 2~%:LABEL: fast-build~%:BUILD-TYPE: optimized~%:END:~%** TODO Check~%* DONE Ship :work:~%:PROPERTIES:~%:EFFORT: 3~%:LABEL: slow-build~%:END:~%* TODO Rest :home:~%")))
           (match (lambda (source)
                    (query-match-test-titles
                     (query-match-at document source
                                     +query-match-test-reference-time+))))
           (local-context (make-query-context :tag-inheritance-p nil)))
      (with-soft-assertions
        (expect (funcall match "work/!") :to-equal '("Build" "Check"))
        (expect (funcall match "/TODO") :to-equal '("Build" "Check" "Rest"))
        (expect (funcall match "/DONE") :to-equal '("Ship"))
        (expect (funcall match "work|home")
                :to-equal '("Build" "Check" "Ship" "Rest"))
        (expect (funcall match "LEVEL=2") :to-equal '("Check"))
        (expect (funcall match "EFFORT<*3") :to-equal '("Build"))
        (expect (funcall match "EFF\\ORT<*3") :to-equal '("Build"))
        (expect (funcall match "LABEL=*{^fast}") :to-equal '("Build"))
        (expect (funcall match "LABEL!=*{^slow}") :to-equal '("Build"))
        (expect (funcall match "TODO=\"DONE\"") :to-equal '("Ship"))
        (expect (funcall match "TODO=*\"TODO\"")
                :to-equal '("Build" "Check" "Rest"))
        (expect (funcall match "EFFORT=1.5") :to-equal nil)
        (expect (funcall match "EFFORT=-1.5") :to-equal nil)
        (expect (funcall match "DEADLINE<=\"<today>\"") :to-equal nil)
        (expect (funcall match "SCHEDULED=*{.*}") :to-equal nil)
        (expect (funcall match "CLOSED=*{.*}") :to-equal nil)
        (expect (funcall match "SCHEDULED=*{^2026}") :to-equal nil)
        (expect (funcall match "CUSTOM=*{.*}") :to-equal nil)
        (expect (funcall match "work\\+group") :to-equal nil)
        (dolist (source '("LABEL=\"a\\nb\"" "LABEL=\"a\\rb\""
                          "LABEL=\"a\\tb\"" "LABEL=\"a\\\\b\""
                          "LABEL=\"a\\\"b\""))
          (expect (funcall match source) :to-equal nil))
        (expect (funcall match "/{^DONE$}") :to-equal '("Ship"))
        (expect (funcall match "TODO!={^DONE$}")
                :to-equal '("Build" "Check" "Rest"))
        (expect (funcall match "BUILD\\-TYPE=\"optimized\"")
                :to-equal '("Build"))
        (expect (funcall match "NAME\\ =value") :to-equal nil)
        (expect (funcall match
                         (concatenate 'string "NAME\\"
                                      (string (code-char #xe9)) "=value"))
                :to-equal nil)
        (expect (typep
                 (query-match-test-public-error
                  document
                  (concatenate 'string "NAME\\"
                               (string (code-char #x378)) "=value"))
                 'org-query-error)
                :to-be t)
        (expect (funcall match "=value") :to-equal nil)
        (expect (funcall match "-work") :to-equal '("Rest"))
        (expect (query-match-test-titles
                 (query-match-at document "work/!"
                                 +query-match-test-reference-time+
                                 :context local-context))
                :to-equal '("Build")))))

  (it "parses nested regex delimiters through the public API"
    (let ((document
            (parse-org
             (format nil "* Brace~%:PROPERTIES:~%:LABEL: {x}~%:END:~%"))))
      (expect (query-match-test-titles
               (query-match-at document "LABEL=*{{x}}"
                               +query-match-test-reference-time+))
              :to-equal '("Brace"))))

  (it "validates malformed Org matches through the public API"
    (let ((document (parse-org "* TODO Task")))
      (with-soft-assertions
        (dolist (source (list "" "work/" "work/bar/baz" "work&(laptop|night)"
                              "TODO>\"DONE\"" "LABEL<*{fast}"
                              "LABEL=\"bad\\q\"" "LABEL=\"bad\\"
                              "LABEL=\"bad" "LABEL=1." "LABEL=.1"
                              "LABEL=word" "LABEL=" "LABEL=*"
                              "+" "-" "/" "work&" "work(" "work{fast}"
                              "work\\" "{\\}" "{\\"
                              (concatenate 'string "work" (string (code-char 1)))
                              (concatenate 'string "work\\" (string (code-char 1)))
                              (concatenate 'string "{x" (string #\Newline) "}")
                              (concatenate 'string "LABEL=\"x"
                                           (string #\Newline) "y\"")))
          (expect (typep (query-match-test-public-error document source)
                         'org-query-error)
                  :to-be t))
        (expect (org-query-error-position
                 (query-match-test-public-error document "work&(laptop|night)"))
                :to-be 5)
        (expect (org-query-error-position
                 (query-match-test-public-error document "work/bar/baz"))
                :to-be 8)
        (expect (query-match-test-titles
                 (query-match-at document "-CUSTOM=*{.*}"
                                 +query-match-test-reference-time+))
                :to-equal '("Task"))
        (expect (query-match-test-titles (query-match-at document "/!"
                                                        +query-match-test-reference-time+))
                :to-equal '("Task")))))

  (it "preserves match input and regex depth limits from the document"
    (let* ((input-limited-document
             (parse-org "* x" :limits (make-limits :max-input-length 3)))
           (depth-limited-document
             (parse-org "* x" :limits (make-limits :max-element-depth 8)))
           (nested-regex
             (concatenate 'string "{" (make-string 8 :initial-element #\{)
                          "x" (make-string 9 :initial-element #\}))))
      (signals org-resource-limit-error
        (query-match-at input-limited-document "long"
                        +query-match-test-reference-time+))
      (signals org-resource-limit-error
        (query-match-at depth-limited-document nested-regex
                        +query-match-test-reference-time+))))

  (it "matches relative dates using the supplied time context's zone"
    (let* ((document
             (parse-org
              (format nil
                      "* TODO Due today~%DEADLINE: <2026-10-13>~%* TODO Due tomorrow~%DEADLINE: <2026-10-14>")))
           (time-context
             (make-time-context
              :reference-time (make-org-time-value "2026-10-13T22:30:00+00:00")
              :zone (make-fixed-time-zone 0))))
      (expect (query-match-test-titles
               (query-match-now document "DEADLINE<=\"<today>\""
                                :time-context time-context))
              :to-equal '("Due today"))))

  (it "reports an invalid time context with a named reason"
    (let* ((document (parse-org "* TODO Task"))
           (invalid-context
             (handler-case
                 (progn
                   (query-match-now document "TODO" :time-context :invalid)
                   nil)
               (org-time-error (condition) condition))))
      (with-soft-assertions
        (expect (typep invalid-context 'org-time-error) :to-be t)
        (expect (org-time-error-reason invalid-context) :to-be :invalid-time))))

  (it "requires a date-time reference for match evaluation"
    (let ((document (parse-org "* TODO Task")))
      (signals type-error
        (query-match-at document "TODO" nil))
      (signals type-error
        (query-match-at :not-a-document "TODO"
                        +query-match-test-reference-time+))
      (signals type-error
        (query-match-at document nil +query-match-test-reference-time+))
      (signals type-error
        (query-match-at document "TODO" +query-match-test-reference-time+
                        :context :not-a-query-context))))

  (it "reports malformed syntax at its source position"
    (let ((document (parse-org "* TODO Task")))
      (with-soft-assertions
        (expect (org-query-error-position
                 (query-match-test-public-error document "work&(laptop|night)"))
                :to-be 5)
        (expect (org-query-error-position
                 (query-match-test-public-error document "work/"))
                :to-be 5)
        (expect (org-query-error-position
                 (query-match-test-public-error document "tag//X"))
                :to-be 4)
        (expect (org-query-error-position
                 (query-match-test-public-error document "{\\w}"))
                :to-be 1)
        (signals org-resource-limit-error
          (query-match-test-predicate "long" nil
                                      (make-limits :max-input-length 3)))))))

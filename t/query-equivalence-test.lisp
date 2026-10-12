;;;; t/query-equivalence-test.lisp
(in-package #:cl-org-kit/test)

(defun query-equivalence-test-titles (nodes)
  (mapcar (lambda (node) (node-property node :raw-value)) nodes))

(defun query-equivalence-test-same-nodes-p (left right)
  (and (= (length left) (length right))
       (every #'eq left right)))

(defun query-equivalence-test-reference-time ()
  (cl-date-kit:make-local-date-time
   (cl-date-kit:make-local-date 2024 7 2)
   (cl-date-kit:make-local-time 9 30 0)))

(describe "equivalent list, path, and Org match queries"
  (it "returns identical headline nodes in document order for the FR-010 triples"
    (let ((document (query-path-test-document))
          (reference-time (query-equivalence-test-reference-time)))
      (dolist (test-case
               '((:todo
                  "work/!-WAITING-NEXT"
                  (:and (:tag := "work")
                        (:todo-present)
                        (:not (:todo := "WAITING"))
                        (:not (:todo := "NEXT")))
                  "**?{tag=\"work\"&todo-present&todo!=\"WAITING\"&todo!=\"NEXT\"}"
                  ("Root" "Deep" "Child"))
                 (:scheduled-absolute
                  "SCHEDULED>=\"<2008-10-11>\""
                  (:planning :scheduled :>= (:date "<2008-10-11>"))
                  "**?{planning.SCHEDULED>=date(\"<2008-10-11>\")}"
                  ("Root" "Child"))
                 (:effort-default-zero
                  "EFFORT<2"
                  (:property "EFFORT" :< 2)
                  "**?{property.EFFORT<2}"
                  ("Root" "Deep" "Child" "Plain"
                   "slash/title" "star*title" "bracket[title]" "back\\slash"))
                 (:effort-present-only
                  "EFFORT<*2"
                  (:and (:property "EFFORT" :< 2 :missing-as :zero)
                        (:property-exists "EFFORT"))
                  "**?{property.EFFORT<2@missing=zero&exists.property.EFFORT}"
                  ("Root"))
                 (:deadline-relative
                  "DEADLINE<=\"<today>\""
                  (:planning :deadline :<= (:date "<today>"))
                  "**?{planning.DEADLINE<=date(\"<today>\")}"
                  ("Root"))
                 (:scheduled-relative
                  "SCHEDULED>=\"<+5d>\""
                  (:planning :scheduled :>= (:date "<+5d>"))
                  "**?{planning.SCHEDULED>=date(\"<+5d>\")}"
                  ("Child"))
                 (:scheduled-present
                  "SCHEDULED=*{.*}"
                  (:planning-exists :scheduled)
                  "**?{exists.planning.SCHEDULED}"
                  ("Root" "Child"))
                 (:scheduled-missing
                  "-SCHEDULED=*{.*}"
                  (:planning-missing :scheduled)
                  "**?{missing.planning.SCHEDULED}"
                  ("Deep" "Child" "Plain"
                   "slash/title" "star*title" "bracket[title]" "back\\slash"))))
        (destructuring-bind (name match form path expected-titles) test-case
          (let ((list-nodes
                  (org-kit::query-list document form
                                       :reference-time reference-time))
                (path-nodes
                  (org-kit::query-path document path
                                       :reference-time reference-time))
                (match-nodes
                  (org-kit::query-match-at document match reference-time)))
            (with-soft-assertions
              (expect (list name
                            (query-equivalence-test-same-nodes-p
                             list-nodes path-nodes))
                      :to-equal (list name t))
              (expect (list name
                            (query-equivalence-test-same-nodes-p
                             list-nodes match-nodes))
                      :to-equal (list name t))
              (expect (list name
                            (query-equivalence-test-same-nodes-p
                             path-nodes match-nodes))
                      :to-equal (list name t))
              (expect (list name (query-equivalence-test-titles list-nodes))
                      :to-equal (list name expected-titles))))))))

  (it "parses the LEVEL triple to canonical predicates without evaluating it"
    (let* ((document (query-path-test-document))
           (context (org-kit::make-query-context))
           (limits (document-limits document))
           (reference-time (query-equivalence-test-reference-time))
           (list-form
             '(:and (:level := 3)
                    (:tag := "boss")
                    (:not (:todo := "DONE"))))
           (expected
             '(:and (:level := 3)
                    (:tag := "boss")
                    (:not (:todo := "DONE")))))
      (let ((list-predicate
              (org-kit::query-list-parse-form
               list-form context limits reference-time
               "(:and (:level := 3) (:tag := \"boss\") (:not (:todo := \"DONE\")))"))
            (match-predicate
              (org-kit::query-parse-org-match
               "+LEVEL=3+boss-TODO=\"DONE\""
               context limits reference-time)))
        (multiple-value-bind (selector path-predicate)
            (org-kit::query-path-parse-source
             "**?{level=3&tag=\"boss\"&todo!=\"DONE\"}"
             context limits reference-time)
          (with-soft-assertions
            (expect list-predicate :to-equal expected)
            (expect match-predicate :to-equal expected)
            (expect selector :to-equal '(:**))
            (expect path-predicate
                    :to-equal
                    '(:and (:level := 3)
                           (:tag := "boss")
                           (:todo :!= "DONE")))))))))

;;;; t/query-path-test.lisp
(in-package #:cl-org-kit/test)

(defun query-path-test-lines (&rest lines)
  (format nil "~{~A~^~%~}" lines))

(defun query-path-test-document ()
  (parse-org
   (query-path-test-lines
    "#+TODO: TODO WAIT | DONE"
    "* TODO Root :boss:work:"
    "SCHEDULED: <2008-10-11 Sat> DEADLINE: <2024-07-02 Tue>"
    ":PROPERTIES:"
    ":EFFORT: 1"
    ":LABEL: ready"
    ":END:"
    "Root body."
    "** DONE Child :work:"
    "SCHEDULED: <2024-07-07 Sun>"
    ":PROPERTIES:"
    ":EFFORT: 3"
    ":LABEL: built"
    ":END:"
    "Child body."
    "*** TODO Deep :night:"
    "#+begin_src emacs-lisp"
    "(message \"body\")"
    "#+end_src"
    "** TODO Child :fast:"
    ":PROPERTIES:"
    ":LABEL: second"
    ":END:"
    "Second child body."
    "* Plain"
    "Plain body."
    "* slash/title"
    "* star*title"
    "* bracket[title]"
    "* back\\slash")))

(defun query-path-test-titles (nodes)
  (mapcar (lambda (node) (node-property node :raw-value)) nodes))

(defun query-path-test-query-error (document source)
  (handler-case
      (progn (org-kit:query-path document source) nil)
    (org-query-error (condition) condition)))

(defun query-path-test-same-nodes-p (left right)
  (and (= (length left) (length right))
       (every #'eq left right)))

(defun query-path-test-list-equivalent-p (document path form reference-time)
  (query-path-test-same-nodes-p
   (org-kit:query-path document path :reference-time reference-time)
   (org-kit::query-list document form :reference-time reference-time)))

(describe "path and filter queries"
  (it "matches root paths, wildcards, duplicate indexes, and escaped titles"
    (let ((document (query-path-test-document)))
      (with-soft-assertions
        (expect (query-path-test-titles
                 (org-kit:query-path document "/Root"))
                :to-equal '("Root"))
        (expect (query-path-test-titles
                 (org-kit:query-path document "Root/Child[2]"))
                :to-equal '("Child"))
        (expect (query-path-test-titles
                 (org-kit:query-path document "Root/*"))
                :to-equal '("Child" "Child"))
        (expect (query-path-test-titles
                 (org-kit:query-path document "Root/**"))
                :to-equal '("Root" "Child" "Deep" "Child"))
        (expect (query-path-test-titles
                 (org-kit:query-path document "**/Deep"))
                :to-equal '("Deep"))
        (expect (query-path-test-titles
                 (org-kit:query-path document "slash\\/title"))
                :to-equal '("slash/title"))
        (expect (query-path-test-titles
                 (org-kit:query-path document "star\\*title"))
                :to-equal '("star*title"))
        (expect (query-path-test-titles
                 (org-kit:query-path document "bracket\\[title\\]"))
                :to-equal '("bracket[title]"))
        (expect (query-path-test-titles
                 (org-kit:query-path document "back\\\\slash"))
                :to-equal '("back\\slash"))
        (expect (org-kit:query-path document "Root/Child[99]") :to-equal nil))))

  (it "parses filter precedence, parentheses, and path predicates"
    (let* ((document (query-path-test-document))
           (context (org-kit::make-query-context))
           (limits (document-limits document)))
      (multiple-value-bind (selector predicate)
          (org-kit::query-path-parse-source
           "**?{tag=\"boss\"|todo=\"DONE\"&todo-present}"
           context limits nil)
        (expect selector :to-equal '(:**))
        (expect predicate
                :to-equal '(:or (:tag := "boss")
                                (:and (:todo := "DONE")
                                      (:todo-present)))))
      (multiple-value-bind (selector predicate)
          (org-kit::query-path-parse-source
           "**?{!(tag=\"boss\"|todo=\"DONE\")}"
           context limits nil)
        (expect selector :to-equal '(:**))
        (expect predicate
                :to-equal '(:not (:or (:tag := "boss")
                                      (:todo := "DONE")))))
      (let ((source
              (concatenate 'string
                           "**?{property.LABEL=\"a"
                           "\\\\"
                           "b"
                           "\\\""
                           "c"
                           "\\n\\r\\t"
                           "\"}")))
        (multiple-value-bind (selector predicate)
            (org-kit::query-path-parse-source source context limits nil)
          (declare (ignore selector))
          (expect (getf (cdr predicate) :value)
                  :to-equal
                  (concatenate 'string "a\\b" "\"" "c"
                               (string #\Newline) (string #\Return)
                               (string #\Tab)))))
      (with-soft-assertions
        (expect (query-path-test-titles
                 (org-kit:query-path
                  document "**?{tag=\"work\"}"
                  :context (org-kit::make-query-context
                            :tag-inheritance-p nil)))
                :to-equal '("Root" "Child"))
        (expect (query-path-test-titles
                 (org-kit:query-path
                  document "Root/**?{tag!=\"work\"}"
                  :context (org-kit::make-query-context
                            :tag-inheritance-p nil)))
                :to-equal '("Deep" "Child"))
        (expect (query-path-test-titles
                 (org-kit:query-path document "Root/**?{todo-present}"))
                :to-equal '("Root" "Deep" "Child"))
        (expect (query-path-test-titles
                 (org-kit:query-path document "Root/**?{todo-missing}"))
                :to-equal nil)
        (expect (query-path-test-titles
                 (org-kit:query-path document "**?{todo=\"DONE\"}"))
                :to-equal '("Child"))
        (expect (query-path-test-titles
                 (org-kit:query-path document "**?{todo=regex(\"TO\")}"))
                :to-equal '("Root" "Deep" "Child"))
        (expect (query-path-test-titles
                 (org-kit:query-path
                  document "Root/**?{todo!=regex(\"DONE\")}"))
                :to-equal '("Root" "Deep" "Child"))
        (expect (query-path-test-titles
                 (org-kit:query-path document
                                      "**?{type=:headline&todo-present}"))
                :to-equal '("Root" "Deep" "Child"))
        (expect (query-path-test-titles
                 (org-kit:query-path document "**?{type!=:headline}"))
                :to-equal nil)
        (expect (query-path-test-titles
                 (org-kit:query-path document "Root/**?{!todo=\"DONE\"}"))
                :to-equal '("Root" "Deep" "Child"))
        (let ((blocks (org-kit:query-path
                       document "Root/**?{type=:src-block}" :scope :nodes)))
          (expect (length blocks) :to-equal 1)
          (when blocks
            (expect (node-type (first blocks)) :to-be :src-block))))))

  (it "parses property, planning, and existence filters"
    (let ((document (query-path-test-document))
          (reference-time (cl-date-kit:make-local-date 2024 7 2)))
      (with-soft-assertions
        (expect (query-path-test-titles
                 (org-kit:query-path document "Root/**?{property.EFFORT<2}"))
                :to-equal '("Root" "Deep" "Child"))
        (expect (query-path-test-titles
                 (org-kit:query-path
                  document "Root/**?{property.\"EFFORT\"<2}"))
                :to-equal '("Root" "Deep" "Child"))
        (expect (query-path-test-titles
                 (org-kit:query-path
                  document "Root/**?{property.EFFORT<2@missing=nonmatch}"))
                :to-equal '("Root"))
        (expect (query-path-test-titles
                 (org-kit:query-path
                  document
                  "Root/**?{property.EFFORT<2@missing=zero&exists.property.EFFORT}"))
                :to-equal '("Root"))
        (expect (query-path-test-titles
                 (org-kit:query-path
                  document "Root/**?{missing.property.EFFORT}"))
                :to-equal '("Deep" "Child"))
        (expect (query-path-test-titles
                 (org-kit:query-path
                  document "Root/**?{property.LABEL=\"second\"}"))
                :to-equal '("Child"))
        (expect (query-path-test-titles
                 (org-kit:query-path
                  document "Root/**?{property.LABEL=regex(\"^s\")}"))
                :to-equal '("Child"))
        (expect (query-path-test-titles
                 (org-kit:query-path
                  document "Root?{property.LABEL!=regex(\"^s\")}"))
                :to-equal '("Root"))
        (expect (org-kit:query-path
                 document
                 "Root?{property.MISSING=date(\"<2024-07-02>\")}")
                :to-equal nil)
        (expect (query-path-test-titles
                 (org-kit:query-path
                  document
                  "Root/**?{planning.SCHEDULED>=date(\"<+5d>\")}"
                  :reference-time reference-time))
                :to-equal '("Child"))
        (expect (query-path-test-titles
                 (org-kit:query-path
                  document
                  "Root/**?{planning.DEADLINE<=date(\"<today>\")}"
                  :reference-time reference-time))
                :to-equal '("Root"))
        (expect (query-path-test-titles
                 (org-kit:query-path document
                                      "Root/**?{exists.planning.SCHEDULED}"))
                :to-equal '("Root" "Child"))
        (expect (query-path-test-titles
                 (org-kit:query-path document
                                      "Root/**?{missing.planning.DEADLINE}"))
                :to-equal '("Child" "Deep" "Child"))
        (expect (query-path-test-titles
                 (org-kit:query-path document
                                      "Root/**?{missing.planning.CLOSED}"))
                :to-equal '("Root" "Child" "Deep" "Child")))))

  (it "matches list-query results for equivalent path filters"
    (let ((document (query-path-test-document))
          (reference-time (cl-date-kit:make-local-date 2024 7 2)))
      (with-soft-assertions
        (dolist (pair
                 '(("**?{tag=\"work\"&todo-present&todo!=\"WAIT\"&todo!=\"NEXT\"}"
                    (:and (:tag := "work")
                          (:todo-present)
                          (:todo :!= "WAIT")
                          (:todo :!= "NEXT")))
                   ("**?{planning.SCHEDULED>=date(\"<2008-10-11>\")}"
                    (:planning :scheduled :>= (:date "<2008-10-11>")))
                   ("**?{property.EFFORT<2}"
                    (:property "EFFORT" :< 2))
                   ("**?{property.EFFORT<2@missing=zero&exists.property.EFFORT}"
                    (:and (:property "EFFORT" :< 2 :missing-as :zero)
                          (:property-exists "EFFORT")))
                   ("**?{planning.DEADLINE<=date(\"<today>\")}"
                    (:planning :deadline :<= (:date "<today>")))
                   ("**?{planning.SCHEDULED>=date(\"<+5d>\")}"
                    (:planning :scheduled :>= (:date "<+5d>")))
                   ("**?{exists.planning.SCHEDULED}"
                    (:planning-exists :scheduled))
                   ("**?{missing.planning.DEADLINE}"
                    (:planning-missing :deadline))))
          (destructuring-bind (path form) pair
            (expect (query-path-test-list-equivalent-p
                     document path form reference-time)
                    :to-be t))))))

  (it "evaluates level filters"
    (let* ((document (query-path-test-document))
           (context (org-kit::make-query-context))
           (limits (document-limits document)))
      (multiple-value-bind (selector predicate)
          (org-kit::query-path-parse-source "**?{level>=2}"
                                             context limits nil)
        (expect selector :to-equal '(:**))
        (expect predicate :to-equal '(:level :>= 2)))
      (expect (query-path-test-titles
               (org-kit:query-path document "**?{level>=2}"))
              :to-equal '("Child" "Deep" "Child"))
      (expect (first (query-path-test-titles
                      (org-kit:query-path document "**?{level>=-1}")))
              :to-equal "Root")))

  (it "rejects malformed paths and filters with source positions"
    (let* ((document (query-path-test-document))
           (bad-paths
             '("" "/" "Root/" "Root//Child" "Root/a*" "Root/*a"
               "Root/a**" "Root/\\**" "Root/*\\*" "Root/***"
               "Root/a[0]" "Root/a[01]" "Root/a[1]x" "Root/a[2"
               "Root/a]" "[1]" "Root\\"
               "Root/\\q" "Root/[\\2]" "Root?x" "Root?" "Root?{tag=\"boss\""
               "Root?{tag=\"boss}" "Root?{tag=\"x\\}"
               "Root?{}" "Root?{(todo-present}"
               "Root?{tag=\"boss\"&&todo-present}"
               "Root?{tag=\"x\\q\"}" "Root?{!!todo-present}"
               "Root{suffix}" "Root?{todo-present{}}"
               "Root?{unknown=x}" "Root?{type=:unknown}"
               "Root?{type=src-block}" "Root?{type=:src-block}extra"
               "Root?{level>=x}" "Root?{todo=1}"
               "Root?{exists.property.1LABEL}"
               "Root?{exists.property.\"\"}" "Root?{property.LABEL}"
               "Root?{property.EFFORT<2@missing=unknown}"
               "Root?{planning.SCHEDULED>=1}"
               "Root?{missing.planning.UNKNOWN}"
               "Root?{property.LABEL<regex(\"x\")}"
               "Root?{property.LABEL=\"x\"@missing=zero}"
               "Root?{planning.SCHEDULED>=date(\"not-a-date\")}"
               "Root?{tag=regex(\"[\")}")))
      (with-soft-assertions
        (dolist (source bad-paths)
          (let ((condition (query-path-test-query-error document source)))
            (expect condition :to-be-truthy)
            (when condition
              (expect (org-query-error-query condition)
                      :to-equal (org-kit::bounded-text source))
              (expect (typep (org-query-error-position condition)
                             '(integer 0 *))
                      :to-be t))))
        (let ((condition
                (query-path-test-query-error
                 document "Root?{property.EFFORT<2@missing=zero@missing=zero}")))
          (expect condition :to-be-truthy)
          (when condition
            (expect (typep (org-query-error-position condition)
                           '(integer 0 *))
                    :to-be t)))
        (let* ((source "Root?{tag=regex(\"[\")}")
               (condition (query-path-test-query-error document source)))
          (expect condition :to-be-truthy)
          (when condition
            (expect (org-query-error-position condition)
                    :to-equal (position #\[ source))))
        (let* ((source "A?{tag=regex(\"\\\\(a\\\\{1000\\\\}\\\\)\\\\{1000\\\\}\")}")
               (limits (make-limits :max-input-length (length source)))
               (limited-document (make-document "" :limits limits))
               (condition (query-path-test-query-error limited-document source)))
          (expect condition :to-be-truthy)
          (when condition
            (expect (org-query-error-position condition)
                    :to-equal (position #\" source :from-end t))))
        (dolist (source
                 (list (concatenate 'string "Root?{tag=\"x"
                                    (string #\Newline) "\"}")
                       (concatenate 'string "Root/" (string #\Tab))))
          (let ((condition (query-path-test-query-error document source)))
            (expect condition :to-be-truthy)
            (when condition
              (expect (typep (org-query-error-position condition)
                             '(integer 0 *))
                      :to-be t)))))))

  (it "validates scope and context arguments"
    (let ((document (query-path-test-document)))
      (signals type-error
        (org-kit:query-path document "Root" :scope :invalid))
      (signals type-error
        (org-kit:query-path document "Root" :context :invalid))))

  (it "enforces path input, segment, and filter-depth limits"
    (let ((input-limited
            (make-document "" :limits (make-limits :max-input-length 3)))
          (node-limited
            (make-document "" :limits (make-limits :max-node-count 1)))
          (path-depth-limited
            (make-document "" :limits (make-limits :max-element-depth 1)))
          (depth-limited
            (make-document "" :limits (make-limits :max-element-depth 2))))
      (signals org-resource-limit-error
        (org-kit:query-path input-limited "Root"))
      (signals org-resource-limit-error
        (org-kit:query-path node-limited "A/B"))
      (signals org-resource-limit-error
        (org-kit:query-path path-depth-limited "A/B"))
      (signals org-resource-limit-error
        (org-kit:query-path
         depth-limited "Root?{((((todo-present))))}"))))

  (it "counts boolean predicate depth after adding the path predicate"
    (let ((too-shallow
            (parse-org "* A"
                       :limits (make-limits :max-element-depth 2)))
          (exact-depth
            (parse-org "* A"
                       :limits (make-limits :max-element-depth 3))))
      (signals org-resource-limit-error
        (org-kit:query-path
         too-shallow "A?{todo-present&todo-missing}"))
      (expect (org-kit:query-path
               exact-depth "A?{todo-present&todo-missing}")
              :to-equal nil)))

  (it "requires reference-time for relative date filters"
    (let ((document (query-path-test-document)))
      (signals org-query-error
        (org-kit:query-path
         document "Root/**?{planning.DEADLINE<=date(\"<today>\")}")))))

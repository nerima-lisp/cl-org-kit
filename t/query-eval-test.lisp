;;;; t/query-eval-test.lisp
(in-package #:cl-org-kit/test)

(defun query-eval-test-run (document predicate &key (scope :headlines) context
                                                reference-time)
  (org-kit::query-evaluate-ir
   document (org-kit::%make-query-ir scope predicate context)
   :reference-time reference-time))

(defun query-eval-test-headlines (document)
  (let ((headlines '()))
    (map-nodes (lambda (node)
                 (when (eq (node-type node) :headline)
                   (push node headlines)))
               (document-root document))
    (nreverse headlines)))

(defun query-eval-test-titles (nodes)
  (mapcar (lambda (node) (node-property node :raw-value)) nodes))

(defun query-eval-test-public-list (text form &rest options)
  (apply #'org-kit:query-list (parse-org text) form options))

(defun query-eval-test-programmatic-document (properties &key settings source-name)
  (let* ((document (org-kit:make-document "" :source-name source-name))
         (headline (org-kit:make-node :headline :properties properties))
         (root (org-kit:make-node :org-data :children (list headline))))
    (setf (org-kit:document-root document) root
          (org-kit:document-settings document) settings)
    document))

(describe "query IR evaluation"
  (it "traverses configured candidates and matches full heading paths"
    (let* ((document
            (parse-org
             (format nil "* Root~%** Repeat~%** Repeat~%* Other~%** Repeat")))
           (headlines (query-eval-test-headlines document)))
      (with-soft-assertions
        (expect (query-eval-test-titles
                 (query-eval-test-run
                  document '(:path ("Root" (:name "Repeat" 2)))))
                :to-equal
                '("Repeat"))
        (expect (query-eval-test-titles
                 (query-eval-test-run
                  document '(:path ("Other" :**))))
                :to-equal
                '("Other" "Repeat"))
        (expect (length (query-eval-test-run
                         document '(:type :headline) :scope :nodes))
                :to-be
                (length headlines))
        (expect (query-eval-test-titles
                 (query-eval-test-run
                  document '(:not (:path ("Other" :**)))))
                :to-equal
                '("Root" "Repeat" "Repeat")))))

  (it "matches named path segments through the public list query"
    (let ((document (parse-org
                     (format nil "* Root~%** Repeat~%** Different~%** Repeat"))))
      (expect (query-eval-test-titles
               (org-kit:query-list
                document '(:path ("Root" (:name "Repeat" 2)))))
              :to-equal '("Repeat"))))

  (it "rejects malformed and over-deep untrusted query structures"
    (let ((document (parse-org "* One")))
      (with-soft-assertions
        (signals org-query-error
          (org-kit:query-list document (cons :and :dotted-tail)))
        (let ((cycle (list :and)))
          (setf (cdr cycle) cycle)
          (signals org-query-error (org-kit:query-list document cycle)))
        (let* ((cycle (list :not nil)))
          (setf (second cycle) cycle)
          (signals org-query-error (org-kit:query-list document cycle)))
        (let ((deep '(:type :headline)))
          (loop repeat 520 do (setf deep (list :not deep)))
          (signals org-resource-limit-error
            (org-kit:query-list document deep))))))

  (it "compares logical heading levels and validates level predicates"
    (let ((document
            (parse-org
             (format nil "~{~A~^~%~}"
                     '("#+STARTUP: odd" "* Root" "** Section"
                       "*** Also Section" "**** Logical Three"
                       "***** TODO Deep :boss:")))))
      (with-soft-assertions
        (expect (query-eval-test-titles
                 (query-eval-test-run document '(:level := 3)))
                :to-equal '("Logical Three" "Deep"))
        (expect (query-eval-test-titles
                 (query-eval-test-run document '(:level :< 3)))
                :to-equal '("Root" "Section" "Also Section"))
        (expect (query-eval-test-titles
                 (query-eval-test-run document '(:level :>= 3)))
                :to-equal '("Logical Three" "Deep"))
        (expect (query-eval-test-titles
                 (query-eval-test-run document '(:level :!= 2)))
                :to-equal '("Root" "Logical Three" "Deep"))
        (expect (query-eval-test-titles
                 (query-eval-test-run document '(:level :<= 2)))
                :to-equal '("Root" "Section" "Also Section"))
        (expect (query-eval-test-titles
                 (query-eval-test-run document '(:level :> 2)))
                :to-equal '("Logical Three" "Deep"))
        (expect (query-eval-test-titles
                 (query-eval-test-run
                  document '(:property :name "LEVEL" :operator :=
                             :value 3 :missing-as :zero)))
                :to-equal '("Logical Three" "Deep"))
        (signals org-query-error
          (org-kit:query-list document '(:level :matches 3)))
        (signals org-query-error
          (org-kit:query-list document '(:level := "3")))
        (signals org-query-error
          (org-kit:query-list document '(:level := 3 :extra t))))))

  (it "resolves TODO keywords and inherited tag groups from document settings"
    (let* ((document
            (parse-org
             (format nil
                     "#+TODO: TODO(t) | DONE(d!) CANCELLED(c@)~%#+FILETAGS: :global:~%#+TAGS: { State : open closed }~%* TODO Open :open:~%** Child~%* DONE Closed :closed:~%* CANCELLED Cancelled~%* Plain")))
           (local-context (org-kit::make-query-context :tag-inheritance-p nil)))
      (with-soft-assertions
        (expect (query-eval-test-titles
                 (query-eval-test-run document '(:todo-present)))
                :to-equal
                '("Open"))
        (expect (query-eval-test-titles
                 (query-eval-test-run document '(:todo-missing)))
                :to-equal
                '("Child" "Plain"))
        (expect (query-eval-test-titles
                 (query-eval-test-run document '(:todo := "CANCELLED")))
                :to-equal
                '("Cancelled"))
        (expect (query-eval-test-titles
                 (query-eval-test-run document '(:tag := "State")))
                :to-equal
                '("Open" "Child" "Closed"))
        (expect (query-eval-test-titles
                 (query-eval-test-run document '(:tag := "global")))
                :to-equal
                '("Open" "Child" "Closed" "Cancelled" "Plain"))
        (expect (query-eval-test-titles
                 (query-eval-test-run document '(:tag := "global")
                                      :context local-context))
                :to-equal
                '()))))

  (it "uses property inheritance, numeric missing values, and planning dates"
    (let* ((document
            (parse-org
             (format nil
                     "* Parent~%SCHEDULED: <2025-01-02 Thu>~%:PROPERTIES:~%:COUNT: 3~%:OWNER: Ada~%:END:~%** Child~%* Plain")))
           (inherited-context
            (org-kit::make-query-context :property-inheritance-p t))
           (date (org-kit::query-parse-org-date-literal "<2025-01-02 Thu>" nil)))
      (with-soft-assertions
        (expect (query-eval-test-titles
                 (query-eval-test-run
                  document '(:property :name "COUNT" :operator := :value 3
                             :missing-as :zero)))
                :to-equal
                '("Parent"))
        (expect (query-eval-test-titles
                 (query-eval-test-run
                  document '(:property :name "MISSING" :operator := :value 0
                             :missing-as :zero)))
                :to-equal
                '("Parent" "Child" "Plain"))
        (expect (query-eval-test-titles
                 (query-eval-test-run
                  document '(:property :name "MISSING" :operator := :value 0
                             :missing-as :zero :require-present-p t)))
                :to-equal
                '())
        (expect (query-eval-test-titles
                 (query-eval-test-run
                  document '(:property :name "OWNER" :operator := :value "Ada"
                             :missing-as :nonmatch)
                  :context inherited-context))
                :to-equal
                '("Parent" "Child"))
        (expect (query-eval-test-titles
                 (query-eval-test-run
                  document `(:planning :scheduled := ,date)))
                :to-equal
                '("Parent"))
        (expect (query-eval-test-titles
                 (query-eval-test-run
                  document '(:exists :planning :scheduled)))
                :to-equal
                '("Parent")))))

  (it "resolves special properties through context settings and providers"
    (let* ((document (parse-org (format nil "#+CATEGORY: Buffer~%* Entry")
                                :source-name "notes.org"))
           (context
            (org-kit::make-query-context
             :category "Explicit"
             :file "override.org"
             :blocked-provider (lambda (headline)
                                 (and (string= (node-property headline :raw-value)
                                               "Entry")
                                      "blocked"))
             :clock-sum-provider (lambda (headline)
                                   (declare (ignore headline))
                                   "1:30")
             :clock-sum-today-provider (lambda (headline)
                                         (declare (ignore headline))
                                         "0:30"))))
      (with-soft-assertions
        (expect (query-eval-test-titles
                 (query-eval-test-run
                  document
                  '(:and
                    (:property :name "CATEGORY" :operator := :value "Explicit"
                     :missing-as :nonmatch)
                    (:property :name "FILE" :operator := :value "override.org"
                     :missing-as :nonmatch)
                    (:property :name "BLOCKED" :operator := :value "blocked"
                     :missing-as :nonmatch)
                    (:property :name "CLOCKSUM" :operator := :value "1:30"
                     :missing-as :nonmatch)
                    (:property :name "CLOCKSUM_T" :operator := :value "0:30"
                     :missing-as :nonmatch))
                  :context context))
                :to-equal
                '("Entry")))))

  (it "compiles a bounded Emacs regex subset with case folding and intervals"
    (let* ((limits (make-limits))
           (grouped
            (org-kit::query-compile-emacs-regex
             "\\(?:cat\\|dog\\)" nil limits "\\(?:cat\\|dog\\)"))
           (folded
            (org-kit::query-compile-emacs-regex
             "^open$" (org-kit::make-query-context :case-fold-p t)
             limits "^open$"))
           (interval
            (org-kit::query-compile-emacs-regex
             "^a\\{,3\\}$" nil limits "^a\\{,3\\}")))
      (with-soft-assertions
        (expect (cl-regex-kit:is-match-p (org-kit::query-regex-compiled grouped)
                                         "dog")
                :to-be t)
        (expect (cl-regex-kit:is-match-p (org-kit::query-regex-compiled folded)
                                         "OPEN")
                :to-be t)
        (expect (cl-regex-kit:is-match-p (org-kit::query-regex-compiled interval)
                                         "aaa")
                :to-be t)
        (expect (cl-regex-kit:is-match-p (org-kit::query-regex-compiled interval)
                                         "aaaa")
                :to-be nil)
        (signals org-query-error
          (org-kit::query-compile-emacs-regex "\\w" nil limits "\\w"))
        (signals org-query-error
          (org-kit::query-compile-emacs-regex "(?:x)" nil limits "(?:x)"))
        (signals org-query-error
          (org-kit::query-compile-emacs-regex "a\\{1001\\}" nil limits "a")))))

  (it "parses strict date literals and relative values without reading the clock"
    (let* ((reference
            (org-kit::query-parse-org-date-literal "<2025-01-02 Thu>" nil))
           (tomorrow
            (org-kit::query-parse-org-date-literal "<tomorrow>" reference))
           (expected
            (org-kit::query-parse-org-date-literal "<2025-01-03 Fri>" nil)))
      (with-soft-assertions
        (expect (org-kit::query-date-compare tomorrow expected) :to-be 0)
        (signals org-query-error
          (org-kit::query-parse-org-date-literal "<2025-02-30>" reference))
        (signals org-query-error
          (org-kit::query-parse-org-date-literal "<2025-01-02 nonsense>" reference))
        (signals org-query-error
          (org-kit::query-parse-org-date-literal "<today>" nil))))))

(describe "public query evaluation edge coverage"
  (it "evaluates boolean, wildcard, node-scope, and existence queries"
    (let* ((document
             (parse-org
              (format nil
                      "#+TODO: TODO | DONE~%#+FILETAGS: :global:~%* TODO Root :work:~%:PROPERTIES:~%:VALUE: 2~%:END:~%** DONE Child~%Body text.~%* Plain")))
           (roots
             (org-kit:query-list document '(:path (:*))))
           (nodes
             (org-kit:query-list document '(:type :paragraph) :scope :nodes)))
      (with-soft-assertions
        (expect (query-eval-test-titles
                 (org-kit:query-list
                  document '(:or (:todo := "TODO") (:tag := "global"))))
                :to-equal '("Root" "Child" "Plain"))
        (expect (query-eval-test-titles roots) :to-equal '("Root" "Plain"))
        (expect (length nodes) :to-be 1)
        (expect (query-eval-test-titles
                 (org-kit:query-list document '(:property-exists "VALUE")))
                :to-equal '("Root"))
        (expect (query-eval-test-titles
                 (org-kit:query-list document '(:property-missing "VALUE")))
                :to-equal '("Child" "Plain"))
        (expect (query-eval-test-titles
                 (org-kit:query-list document '(:planning-exists :scheduled)))
                :to-equal '())
        (expect (query-eval-test-titles
                 (org-kit:query-list document '(:planning-missing :closed)))
                :to-equal '("Root" "Child" "Plain")))))

  (it "coerces signed decimal and date properties through list queries"
    (let* ((document
             (parse-org
              (format nil
                      "* Decimal~%:PROPERTIES:~%:POSITIVE: +3.5~%:NEGATIVE: -.25~%:END:~%* Integer~%:PROPERTIES:~%:NEGATIVE: -3~%:DATE: <2025-01-02 Thu>~%:END:~%* Invalid date~%:PROPERTIES:~%:DATE: someday~%:END:~%* Missing")))
           (reference
             (cl-date-kit:make-local-date-time
              (cl-date-kit:make-local-date 2025 1 2)
              (cl-date-kit:make-local-time 9 30 0))))
      (with-soft-assertions
        (expect (query-eval-test-titles
                 (org-kit:query-list
                  document '(:property "POSITIVE" :< 4 :missing-as :nonmatch)))
                :to-equal '("Decimal"))
        (expect (query-eval-test-titles
                 (org-kit:query-list document '(:property "NEGATIVE" := 0)))
                :to-equal '("Invalid date" "Missing"))
        (expect (query-eval-test-titles
                 (org-kit:query-list document '(:property "NEGATIVE" :< 0)))
                :to-equal '("Decimal" "Integer"))
        (expect (query-eval-test-titles
                 (org-kit:query-list
                  document '(:property "DATE" := (:date "[2025-01-02 Thu]")
                             :missing-as :nonmatch)
                  :reference-time reference))
                :to-equal '("Integer"))
        (expect (query-eval-test-titles
                 (org-kit:query-list
                  document '(:property "DATE" := (:date "<today>")
                             :missing-as :nonmatch)
                  :reference-time reference))
                :to-equal '("Integer"))
        (expect (query-eval-test-titles
                 (org-kit:query-list
                  document '(:property "DATE" := (:date "<today>")
                             :missing-as :nonmatch)
                  :reference-time
                  (cl-date-kit:make-local-date 2025 1 2)))
                :to-equal '("Integer"))
        (let ((relative-document
                (parse-org
                 (format nil
                         "* Month~%:PROPERTIES:~%:WHEN: <2025-02-02 Sun>~%:END:~%* Year~%:PROPERTIES:~%:WHEN: <2026-01-02 Fri>~%:END:"))))
          (expect (query-eval-test-titles
                   (org-kit:query-list
                    relative-document
                    '(:property "WHEN" := (:date "<+1m>") :missing-as :nonmatch)
                    :reference-time reference))
                  :to-equal '("Month"))
          (expect (query-eval-test-titles
                   (org-kit:query-list
                    relative-document
                    '(:property "WHEN" := (:date "<+1y>") :missing-as :nonmatch)
                    :reference-time reference))
                  :to-equal '("Year"))))))

  (it "covers date and number coercion boundaries through list queries"
    (let* ((document
             (parse-org
              (format nil
                      "* Entry~%:PROPERTIES:~%:WHEN: <2025-01-02>~%:NOW: <now>~%:BAD-RANGE: <2025-01-02 Thu 09:30x10:00>~%:BAD-SEPARATOR: <2025-01x02>~%:BAD-YEAR: <abcd-01-02>~%:EXTRA-TIME: <2025-01-02 Thu 09:30 extra>~%:PLAIN-NUMBER: 3~%:BAD-NUMBER: 3x~%:EMPTY-FRACTION: 1.~%:EMPTY-NUMBER:~%:END:")))
           (date (cl-date-kit:make-local-date 2025 1 2))
           (date-time
             (cl-date-kit:make-local-date-time
              date (cl-date-kit:make-local-time 9 30 0))))
      (with-soft-assertions
        (expect (query-eval-test-titles
                 (org-kit:query-list
                  document
                  '(:property "WHEN" := (:date "<2025-01-02   >"))))
                :to-equal '("Entry"))
        (expect (query-eval-test-titles
                 (org-kit:query-list document
                                     '(:property "NOW" := (:date "<now>"))
                                     :reference-time date))
                :to-equal '("Entry"))
        (expect (query-eval-test-titles
                 (org-kit:query-list document
                                     '(:property "NOW" := (:date "<now>"))
                                     :reference-time date-time))
                :to-equal '("Entry"))
        (expect (query-eval-test-titles
                 (org-kit:query-list
                  document
                  '(:property "NOW" := (:date "<2025-01-02>"))))
                :to-equal nil)
        (dolist (name '("BAD-RANGE" "BAD-SEPARATOR" "BAD-YEAR" "EXTRA-TIME"))
          (expect (query-eval-test-titles
                   (org-kit:query-list
                    document `(:property ,name := (:date "<2025-01-02>"))))
                  :to-equal nil))
        (expect (query-eval-test-titles
                 (org-kit:query-list document
                                     '(:property "PLAIN-NUMBER" := 3)))
                :to-equal '("Entry"))
        (expect (query-eval-test-titles
                 (org-kit:query-list document
                                     '(:property "BAD-NUMBER" := 3)))
                :to-equal nil)
        (expect (query-eval-test-titles
                 (org-kit:query-list document
                                     '(:property "EMPTY-FRACTION" := 1)))
                :to-equal nil)
        (expect (query-eval-test-titles
                 (org-kit:query-list document
                                     '(:property "EMPTY-NUMBER" := 1)))
                :to-equal nil))))

  (it "evaluates Org date-times, relative units, and timestamp properties"
    (let* ((document
             (parse-org
              (format nil
                      "* Event~%SCHEDULED: <2025-01-03 Fri 09:30-10:00> DEADLINE: <2024-12-26 Thu> CLOSED: [2026-01-02 Fri 09:30]~%Active <2025-01-05 Sun 08:15>~%Inactive [2025-01-06 Mon 11:00]")))
           (reference
             (cl-date-kit:make-local-date-time
              (cl-date-kit:make-local-date 2025 1 2)
              (cl-date-kit:make-local-time 9 30 0))))
      (with-soft-assertions
        (dolist (literal '("<tomorrow>" "<+1d>"))
          (expect (query-eval-test-titles
                   (org-kit:query-list
                    document `(:planning :scheduled :>= (:date ,literal))
                    :reference-time reference))
                  :to-equal '("Event")))
        (expect (query-eval-test-titles
                 (org-kit:query-list
                  document '(:planning :deadline := (:date "<-1w>")
                             )
                  :reference-time reference))
                :to-equal '("Event"))
        (expect (query-eval-test-titles
                 (org-kit:query-list
                  document '(:planning :closed := (:date "<now>"))
                  :reference-time
                  (cl-date-kit:make-local-date-time
                   (cl-date-kit:make-local-date 2026 1 2)
                   (cl-date-kit:make-local-time 9 30 0))))
                :to-equal '("Event"))
        (expect (query-eval-test-titles
                 (org-kit:query-list
                  document '(:planning :scheduled := (:date "<2025-01-03 Fri 09:30>"))))
                :to-equal '("Event"))
        (expect (query-eval-test-titles
                 (org-kit:query-list
                  document
                  '(:planning :scheduled := (:date "<2025-01-03 Fri 09:30-10:00>"))))
                :to-equal '("Event"))
        (expect (query-eval-test-titles
                 (org-kit:query-list
                  document '(:property "TIMESTAMP" := (:date "<2025-01-05 Sun 08:15>"))))
                :to-equal '("Event"))
        (expect (query-eval-test-titles
                 (org-kit:query-list
                  document
                  '(:property "SCHEDULED" := (:date "<2025-01-03 Fri 09:30>"))))
                :to-equal '("Event"))
        (expect (query-eval-test-titles
                 (org-kit:query-list
                  document
                  '(:property "CLOSED" := (:date "[2026-01-02 Fri 09:30]"))))
                :to-equal '("Event"))
        (expect (query-eval-test-titles
                 (org-kit:query-list
                  document
                  `(:property "SCHEDULED" :=
                    ,(cl-date-kit:format-local-date-time
                      (cl-date-kit:make-local-date-time
                       (cl-date-kit:make-local-date 2025 1 3)
                       (cl-date-kit:make-local-time 9 30 0))))))
                :to-equal '("Event"))
        (signals org-query-error
          (org-kit:query-list document '(:planning :scheduled := (:date "<today>"))))
        (signals org-query-error
          (org-kit:query-list
           document '(:planning :scheduled := (:date "<2025-01-02 nonsense>"))))
      (let ((timestamp-document
              (parse-org
               (format nil
                      "* Entry~%Active <2025-01-02 Thu 09:30>~%Inactive [2025-01-03 Fri 11:00]"))))
        (with-soft-assertions
          (expect (query-eval-test-titles
                   (org-kit:query-list
                    timestamp-document
                    '(:property "TIMESTAMP" := (:date "<2025-01-02 Thu 09:30>"))))
                  :to-equal '("Entry"))
          (expect (query-eval-test-titles
                   (org-kit:query-list
                    timestamp-document
                    '(:property "TIMESTAMP_IA" := (:date "[2025-01-03 Fri 11:00]"))))
                  :to-equal '("Entry")))))))

  (it "resolves public special properties and provider fallbacks"
    (let* ((text
             (format nil
                     "#+CATEGORY: FileCategory~%#+FILETAGS: :global:~%#+TODO: TODO | DONE~%#+TAGS: { State : open closed } { Pattern : {^w.*} }~%* TODO [#A] Local :open:work:~%SCHEDULED: <2025-01-02 Thu>~%DEADLINE: <2025-01-03 Fri>~%CLOSED: [2025-01-04 Sat]~%:PROPERTIES:~%:CATEGORY: LocalCategory~%:END:~%Active <2025-01-05 Sun> inactive [2025-01-06 Mon]~%** DONE Child :closed:work:~%* Plain"))
           (document (parse-org text :source-name "notes.org"))
           (category-document
             (parse-org
              (format nil
                      "#+CATEGORY: FileCategory~%* Local~%:PROPERTIES:~%:CATEGORY: LocalCategory~%:END:~%** Child~%* Plain")))
           (context
             (org-kit:make-query-context
              :file "override.org"
              :category "OverrideCategory"
              :tag-groups '((:group "State" :members ("from-context"))
                            (:group "Extra" :members ("closed")))
              :blocked-provider (lambda (headline)
                                  (declare (ignore headline)) "blocked")
              :clock-sum-provider (lambda (headline)
                                    (declare (ignore headline)) "1:30")
              :clock-sum-today-provider (lambda (headline)
                                          (declare (ignore headline)) "0:30"))))
      (with-soft-assertions
        (expect (query-eval-test-titles
                 (org-kit:query-list document '(:property "TODO" := "TODO")))
                :to-equal '("Local"))
        (expect (query-eval-test-titles
                 (org-kit:query-list document '(:property "PRIORITY" := "A")))
                :to-equal '("Local"))
        (expect (query-eval-test-titles
                 (org-kit:query-list document '(:property "ITEM" := "Local")))
                :to-equal '("Local"))
        (expect (query-eval-test-titles
                 (org-kit:query-list category-document
                                     '(:property "CATEGORY" := "LocalCategory")))
                :to-equal '("Local" "Child"))
        (expect (query-eval-test-titles
                 (org-kit:query-list category-document
                                     '(:property "CATEGORY" := "FileCategory")))
                :to-equal '("Plain"))
        (expect (query-eval-test-titles
                 (org-kit:query-list document '(:property "FILE" := "notes.org")))
                :to-equal '("Local" "Child" "Plain"))
        (expect (query-eval-test-titles
                 (org-kit:query-list document '(:property "TAGS" := ":open:work:")))
                :to-equal '("Local"))
        (expect (query-eval-test-titles
                 (org-kit:query-list document '(:property "ALLTAGS" := ":open:work:global:")))
                :to-equal '("Local"))
        (expect (query-eval-test-titles
                 (org-kit:query-list document '(:tag := "State")))
                :to-equal '("Local" "Child"))
        (expect (query-eval-test-titles
                 (org-kit:query-list document '(:tag := "Pattern")))
                :to-equal '("Local" "Child"))
        (expect (query-eval-test-titles
                 (org-kit:query-list document '(:tag := "Extra") :context context))
                :to-equal '("Child"))
        (expect (query-eval-test-titles
                 (org-kit:query-list document '(:property "CATEGORY" := "OverrideCategory")
                                     :context context))
                :to-equal '("Local" "Child" "Plain"))
        (expect (query-eval-test-titles
                 (org-kit:query-list document '(:property "FILE" := "override.org")
                                     :context context))
                :to-equal '("Local" "Child" "Plain"))
        (expect (query-eval-test-titles
                 (org-kit:query-list document '(:property "BLOCKED" := "blocked")
                                     :context context))
                :to-equal '("Local" "Child" "Plain"))
        (expect (query-eval-test-titles
                 (org-kit:query-list document '(:property "CLOCKSUM" := "1:30")
                                     :context context))
                :to-equal '("Local" "Child" "Plain"))
        (expect (query-eval-test-titles
                 (org-kit:query-list document '(:property "CLOCKSUM_T" := "0:30")
                                     :context context))
                :to-equal '("Local" "Child" "Plain"))
        (expect (query-eval-test-titles
                 (org-kit:query-list document '(:property "BLOCKED" := "blocked")))
                :to-equal nil)
        (expect (query-eval-test-titles
                 (org-kit:query-list document '(:property "CLOCKSUM" := "1:30")))
                :to-equal nil)
        (expect (query-eval-test-titles
                 (org-kit:query-list document '(:property "CLOCKSUM_T" := "0:30")))
                :to-equal nil)
        (let ((local-tags (org-kit:make-query-context :tag-inheritance-p nil)))
          (expect (query-eval-test-titles
                   (org-kit:query-list
                    document '(:property "ALLTAGS" := ":open:work:")
                    :context local-tags))
                  :to-equal '("Local"))
          (expect (query-eval-test-titles
                   (org-kit:query-list
                    document '(:property "ALLTAGS" := "")
                    :context local-tags))
                  :to-equal '("Plain")))
        (let ((fallback (parse-org "* Entry" :source-name "archive.org")))
          (expect (query-eval-test-titles
                   (org-kit:query-list fallback '(:property "CATEGORY" := "archive")))
                  :to-equal '("Entry")))
        (let ((fallback (parse-org "* Entry" :source-name "archive")))
          (expect (query-eval-test-titles
                   (org-kit:query-list fallback '(:property "CATEGORY" := "archive")))
                  :to-equal '("Entry")))
        (let ((directory-only (parse-org "* Entry" :source-name "folder/")))
          (expect (query-eval-test-titles
                   (org-kit:query-list directory-only
                                       '(:property "CATEGORY" := "")))
                  :to-equal '("Entry")))
        (let ((deadline-document
                (parse-org
                 (format nil "* Event~%SCHEDULED: <2025-01-02 Thu> DEADLINE: <2025-01-03 Fri> CLOSED: [2025-01-04 Sat]"))))
          (expect (query-eval-test-titles
                   (org-kit:query-list
                    deadline-document
                    '(:property "DEADLINE" := (:date "<2025-01-03 Fri>"))))
                  :to-equal '("Event")))
        (let ((no-file (parse-org "* Entry")))
          (expect (query-eval-test-titles
                   (org-kit:query-list no-file '(:property-missing "FILE")))
                  :to-equal '("Entry")))
        (let ((nested-timestamp
                (parse-org
                 (format nil "* Parent~%** Child~%<2025-02-03 Mon>"))))
          (expect (query-eval-test-titles
                   (org-kit:query-list
                    nested-timestamp
                    '(:property "TIMESTAMP" := (:date "<2025-02-03 Mon>"))))
                  :to-equal '("Child")))
        (let ((string-priority
                (query-eval-test-programmatic-document
                 '(:raw-value "String priority" :priority "A"))))
          (expect (query-eval-test-titles
                   (org-kit:query-list
                    string-priority '(:property "PRIORITY" := "A")))
                  :to-equal '("String priority")))
        (let ((character-priority
                (query-eval-test-programmatic-document
                 '(:raw-value "Character priority" :priority #\A))))
          (expect (query-eval-test-titles
                   (org-kit:query-list
                    character-priority '(:property "PRIORITY" := "A")))
                  :to-equal '("Character priority")))
        (let ((no-category (parse-org "* Entry")))
          (expect (query-eval-test-titles
                   (org-kit:query-list no-category '(:property-missing "CATEGORY")))
                  :to-equal '("Entry"))))))

  (it "stops drawer property lookup at the next headline"
    (let ((document
            (parse-org
             (format nil "* Parent~%** Child~%:PROPERTIES:~%:CHILD-ONLY: yes~%:END:~%"))))
      (expect (query-eval-test-titles
               (org-kit:query-list document '(:property-missing "CHILD-ONLY")))
              :to-equal '("Parent"))))

  (it "skips property drawers when searching entry timestamps"
    (let ((document
            (parse-org
             (format nil "* Event~%:PROPERTIES:~%:NOTE: value~%:END:~%<2025-02-03 Mon>"))))
      (expect (query-eval-test-titles
               (org-kit:query-list
                document '(:property "TIMESTAMP" := (:date "<2025-02-03 Mon>"))))
              :to-equal '("Event"))))

  (it "treats absent special properties on caller-created headline nodes as missing"
    (let ((document (query-eval-test-programmatic-document nil)))
      (expect (query-eval-test-titles
               (org-kit:query-list document '(:level := 1)))
              :to-equal nil)
      (expect (length (org-kit:query-list document '(:property-missing "ITEM")))
              :to-be 1)))

  (it "stringifies values returned by special-property providers"
    (let* ((document
             (parse-org
              (format nil
                      "* Character~%* Number~%* Symbol~%* Date~%* DateTime~%* Timestamp~%Active <2025-01-02 Thu 09:30>~%* Fallback")))
           (timestamp
             (first (org-kit:query-list document '(:type :timestamp) :scope :nodes)))
           (date (cl-date-kit:make-local-date 2025 1 2))
           (date-time
             (cl-date-kit:make-local-date-time
              date (cl-date-kit:make-local-time 9 30 0)))
           (context
             (org-kit:make-query-context
              :blocked-provider
              (lambda (headline)
                (let ((title (node-property headline :raw-value)))
                  (cond
                    ((string= title "Character") #\X)
                    ((string= title "Number") 42)
                    ((string= title "Symbol") :busy)
                    ((string= title "Date") date)
                    ((string= title "DateTime") date-time)
                    ((string= title "Timestamp") timestamp)
                    (t #())))))))
      (with-soft-assertions
        (expect (query-eval-test-titles
                 (org-kit:query-list document '(:property "BLOCKED" := "X")
                                     :context context))
                :to-equal '("Character"))
        (expect (query-eval-test-titles
                 (org-kit:query-list document '(:property "BLOCKED" := "42")
                                     :context context))
                :to-equal '("Number"))
        (expect (query-eval-test-titles
                 (org-kit:query-list document '(:property "BLOCKED" := "BUSY")
                                     :context context))
                :to-equal '("Symbol"))
        (expect (query-eval-test-titles
                 (org-kit:query-list document
                                     `(:property "BLOCKED" := ,(cl-date-kit:format-local-date date))
                                     :context context))
                :to-equal '("Date"))
        (expect (query-eval-test-titles
                 (org-kit:query-list
                  document
                  `(:property "BLOCKED" := ,(cl-date-kit:format-local-date-time date-time))
                  :context context))
                :to-equal '("DateTime"))
        (expect (query-eval-test-titles
                 (org-kit:query-list document `(:property "BLOCKED" := ,(node-text timestamp))
                                     :context context))
                :to-equal '("Timestamp"))
        (expect (query-eval-test-titles
                 (org-kit:query-list
                  document
                  '(:property "BLOCKED" := (:date "<2025-01-02 Thu 09:30>"))
                  :context context))
                :to-equal '("DateTime" "Timestamp"))
        (expect (query-eval-test-titles
                 (org-kit:query-list document '(:property "BLOCKED" := "")
                                     :context context))
                :to-equal '("Fallback")))))

  (it "treats NIL special-property provider values as absent"
    (let* ((document (parse-org "* Entry"))
           (context
             (org-kit:make-query-context
              :blocked-provider (lambda (node) (declare (ignore node)) nil)
              :clock-sum-provider (lambda (node) (declare (ignore node)) nil)
              :clock-sum-today-provider (lambda (node) (declare (ignore node)) nil))))
      (expect (length
               (org-kit:query-list
                document
                '(:and (:property-missing "BLOCKED")
                       (:property-missing "CLOCKSUM")
                       (:property-missing "CLOCKSUM_T"))
                :context context))
              :to-equal 1)))

  (it "matches translated Emacs regular expressions and reports invalid patterns"
    (let ((text (format nil "* aab~%* abbb~%* [~%* ]~%* x^Root~%* x$Root~%* Root~%* dog~%* y")))
      (with-soft-assertions
        (expect (query-eval-test-titles
                 (query-eval-test-public-list text '(:property "ITEM" :matches (:regex "^a\\{2,3\\}b$"))))
                :to-equal '("aab"))
        (expect (query-eval-test-titles
                 (query-eval-test-public-list text '(:property "ITEM" :matches (:regex "^a\\{2,\\}b$"))))
                :to-equal '("aab"))
        (expect (query-eval-test-titles
                 (query-eval-test-public-list text '(:property "ITEM" :matches (:regex "^a\\{2\\}b$"))))
                :to-equal '("aab"))
        (expect (query-eval-test-titles
                 (query-eval-test-public-list text '(:property "ITEM" :matches (:regex "^a\\{,2\\}b+$"))))
                :to-equal '("aab" "abbb"))
        (expect (query-eval-test-titles
                 (query-eval-test-public-list text '(:property "ITEM" :matches (:regex "[a-c]"))))
                :to-equal '("aab" "abbb"))
        (expect (query-eval-test-titles
                 (query-eval-test-public-list text '(:property "ITEM" :matches (:regex "[a\\-c]"))))
                :to-equal '("aab" "abbb"))
        (expect (query-eval-test-titles
                 (query-eval-test-public-list text '(:property "ITEM" :matches (:regex "[a\\]]"))))
                :to-equal '("aab" "abbb" "]"))
        (expect (query-eval-test-titles
                 (query-eval-test-public-list text '(:property "ITEM" :matches (:regex "[]a]"))))
                :to-equal '("aab" "abbb" "]"))
        (expect (query-eval-test-titles
                 (query-eval-test-public-list text '(:property "ITEM" :matches (:regex "\\(dog\\|cat\\)"))))
                :to-equal '("dog"))
        (expect (query-eval-test-titles
                 (query-eval-test-public-list text '(:property "ITEM" :matches (:regex "\\(^Root\\)"))))
                :to-equal '("Root"))
        (expect (query-eval-test-titles
                 (query-eval-test-public-list text '(:property "ITEM" :matches (:regex "\\(^Root$\\)"))))
                :to-equal '("Root"))
        (expect (query-eval-test-titles
                 (query-eval-test-public-list text '(:property "ITEM" :matches (:regex "\\(?:^Root\\)"))))
                :to-equal '("Root"))
        (expect (query-eval-test-titles
                 (query-eval-test-public-list
                  (format nil "* plain(group)|bar")
                  '(:property "ITEM" :matches (:regex "plain(group)|bar"))))
                :to-equal '("plain(group)|bar"))
        (expect (query-eval-test-titles
                 (query-eval-test-public-list text '(:property "ITEM" :matches (:regex "x\\^Root"))))
                :to-equal '("x^Root"))
        (expect (query-eval-test-titles
                 (query-eval-test-public-list text '(:property "ITEM" :matches (:regex "x\\$Root"))))
                :to-equal '("x$Root"))
        (expect (query-eval-test-titles
                 (query-eval-test-public-list text '(:property "ITEM" :matches (:regex "x^Root"))))
                :to-equal '("x^Root"))
        (expect (query-eval-test-titles
                 (query-eval-test-public-list text '(:property "ITEM" :matches (:regex "x$Root"))))
                :to-equal '("x$Root"))
        (let ((anchor-text (format nil "* abcx^~%* ab^~%* z")))
          (expect (query-eval-test-titles
                   (query-eval-test-public-list anchor-text '(:property "ITEM" :matches (:regex "abcx^"))))
                  :to-equal '("abcx^"))
          (expect (query-eval-test-titles
                   (query-eval-test-public-list anchor-text '(:property "ITEM" :matches (:regex "z\\|ab^"))))
                  :to-equal '("ab^" "z")))
        (let ((special-text
                (format nil
                        "* x~%* xy~%* y~%* plus+value~%* question?value~%* star*value~%* dot.value~%* bracket[one]~%* slash\\value~%* dog~%* brace{2}")))
          (expect (query-eval-test-titles
                   (query-eval-test-public-list special-text '(:property "ITEM" :matches (:regex "^x+y$"))))
                  :to-equal '("xy"))
          (expect (query-eval-test-titles
                   (query-eval-test-public-list special-text '(:property "ITEM" :matches (:regex "^x?y$"))))
                  :to-equal '("xy" "y"))
          (expect (query-eval-test-titles
                   (query-eval-test-public-list special-text '(:property "ITEM" :matches (:regex "^plus\\+value$"))))
                  :to-equal '("plus+value"))
          (expect (query-eval-test-titles
                   (query-eval-test-public-list special-text '(:property "ITEM" :matches (:regex "^question\\?value$"))))
                  :to-equal '("question?value"))
          (expect (query-eval-test-titles
                   (query-eval-test-public-list special-text '(:property "ITEM" :matches (:regex "^star\\*value$"))))
                  :to-equal '("star*value"))
          (expect (query-eval-test-titles
                   (query-eval-test-public-list special-text '(:property "ITEM" :matches (:regex "^dot\\.value$"))))
                  :to-equal '("dot.value"))
          (expect (query-eval-test-titles
                   (query-eval-test-public-list special-text '(:property "ITEM" :matches (:regex "^bracket\\[one\\]$"))))
                  :to-equal '("bracket[one]"))
          (expect (query-eval-test-titles
                   (query-eval-test-public-list special-text '(:property "ITEM" :matches (:regex "^slash\\\\value$"))))
                  :to-equal '("slash\\value"))
          (expect (query-eval-test-titles
                   (query-eval-test-public-list special-text '(:property "ITEM" :matches (:regex "^\\(dog\\|cat\\)$"))))
                  :to-equal '("dog"))
          (expect (query-eval-test-titles
                   (query-eval-test-public-list special-text '(:property "ITEM" :matches (:regex "^brace{2}$"))))
                  :to-equal '("brace{2}")))
        (signals org-query-error
          (query-eval-test-public-list text '(:property "ITEM" :matches (:regex "\\w"))))
        (signals org-query-error
          (query-eval-test-public-list text '(:property "ITEM" :matches (:regex "(?:dog)"))))
        (signals org-query-error
          (query-eval-test-public-list text '(:property "ITEM" :matches (:regex "\\(?=dog\\)"))))
        (signals org-query-error
          (query-eval-test-public-list text '(:property "ITEM" :matches (:regex "[abc"))))
        (signals org-query-error
          (query-eval-test-public-list text '(:property "ITEM" :matches (:regex "[a\\q]"))))
        (signals org-query-error
          (query-eval-test-public-list text '(:property "ITEM" :matches (:regex "[a\\"))))
        (signals org-query-error
          (query-eval-test-public-list text '(:property "ITEM" :matches (:regex "\\"))))
        (signals org-query-error
          (query-eval-test-public-list text '(:property "ITEM" :matches (:regex "\\)"))))
        (signals org-query-error
          (query-eval-test-public-list text '(:property "ITEM" :matches (:regex "\\("))))
        (signals org-query-error
          (query-eval-test-public-list text '(:property "ITEM" :matches (:regex "\\{2,,3\\}"))))
        (signals org-query-error
          (query-eval-test-public-list text '(:property "ITEM" :matches (:regex "\\{,\\}"))))
        (signals org-query-error
          (query-eval-test-public-list text '(:property "ITEM" :matches (:regex "\\{2"))))
        (signals org-query-error
          (query-eval-test-public-list text '(:property "ITEM" :matches (:regex "\\}"))))
        (signals org-query-error
          (query-eval-test-public-list text '(:property "ITEM" :matches (:regex "\\{3,2\\}"))))
        (signals org-query-error
          (query-eval-test-public-list text '(:property "ITEM" :matches (:regex "[z-a]"))))
        (let ((shallow-document
                (parse-org "" :limits (make-limits :max-element-depth 2))))
          (signals org-query-error
            (org-kit:query-list
             shallow-document
             '(:property "ITEM" :matches (:regex "\\(\\(\\(x\\)\\)\\)"))))))))

  (it "evaluates public path and Org match queries against parsed documents"
    (let* ((document
             (parse-org
              (format nil "#+TODO: TODO | DONE~%* TODO Root :work:~%** TODO Child :night:~%* DONE Other :work:")))
           (reference
             (cl-date-kit:make-local-date-time
              (cl-date-kit:make-local-date 2025 1 2)
              (cl-date-kit:make-local-time 9 30 0))))
      (with-soft-assertions
        (expect (query-eval-test-titles
                 (org-kit:query-path document "Root/*?{tag=\"night\"}"))
                :to-equal '("Child"))
        (expect (query-eval-test-titles
                 (org-kit:query-match-at document "work/!" reference))
                :to-equal '("Root" "Child"))
        (expect (query-eval-test-titles
                 (org-kit:query-match-at document "TODO!=\"DONE\"" reference))
                :to-equal '("Root" "Child")))))

  (it "merges configured TODO and tag groups and detects cyclic definitions"
    (let* ((document
             (parse-org
              (format nil
                      "#+TODO: TODO | DONE~%#+TAGS: { State : open closed } { Outer : Inner } { Inner : work }~%* FINISHED Wrapped :work:~%* TODO Open :open:~%* DONE Closed :closed:")
              :settings '(:todo-keywords (("FINISHED") ("TODO" . :todo)
                                          ("DONE" . :done))
                          :todo-sequences ((:done-keywords ("FINISHED"))))))
           (context
             (org-kit:make-query-context
              :tag-groups '((:group "State" :members ("from-context"))
                            (:group "ContextOnly" :members ("work"))))))
      (with-soft-assertions
        (expect (query-eval-test-titles
                 (org-kit:query-list document '(:todo-missing)))
                :to-equal '())
        (expect (query-eval-test-titles
                 (org-kit:query-list document '(:todo-present)))
                :to-equal '("Open"))
        (expect (query-eval-test-titles
                 (org-kit:query-list document '(:tag := "State")))
                :to-equal '("Open" "Closed"))
        (expect (query-eval-test-titles
                 (org-kit:query-list document '(:tag := "Outer")))
                :to-equal '("Wrapped"))
        (expect (query-eval-test-titles
                 (org-kit:query-list document '(:tag := "ContextOnly") :context context))
                :to-equal '("Wrapped"))
        (expect (query-eval-test-titles
                 (org-kit:query-list document '(:tag :!= "absent")))
                :to-equal '("Wrapped" "Open" "Closed"))
        (let* ((keyword-done
                 (parse-org
                  "* FINISHED Entry"
                  :settings '(:todo-keywords (("FINISHED"))
                              :todo-sequences ((:done-keywords ("FINISHED"))))))
               (headline (first (org-kit:query-list
                                 keyword-done '(:type :headline)))))
          (expect (org-kit:node-property headline :todo-type) :to-be nil)
          (expect (query-eval-test-titles
                   (org-kit:query-list keyword-done '(:todo-present)))
                  :to-equal '()))
        (let ((keyword-table-done
                (parse-org "* FINISHED Entry"
                           :settings '(:todo-keywords (("FINISHED"))))))
          (setf (org-kit:document-settings keyword-table-done)
                '(:todo-keywords (("FINISHED" . :done))))
          (expect (query-eval-test-titles
                   (org-kit:query-list keyword-table-done '(:todo-present)))
                  :to-equal '()))
        (let* ((todo-document
                 (parse-org "* WAIT Entry"
                            :settings '(:todo-keywords (("WAIT")))))
               (headline (first (org-kit:query-list
                                 todo-document '(:type :headline)))))
          (expect (org-kit:node-property headline :todo-type) :to-be nil)
          (expect (query-eval-test-titles
                   (org-kit:query-list todo-document '(:todo-present)))
                  :to-equal '("Entry")))
        (let* ((sequence-document
                 (parse-org "* WAIT Entry"
                            :settings '(:todo-keywords (("WAIT"))
                                        :todo-sequences
                                        ((:done-keywords ("DONE"))))))
               (headline (first (org-kit:query-list
                                 sequence-document '(:type :headline)))))
          (expect (org-kit:node-property headline :todo-type) :to-be nil)
          (expect (query-eval-test-titles
                   (org-kit:query-list sequence-document '(:todo-present)))
                  :to-equal '("Entry")))
        (let ((malformed-keywords
                (parse-org "* WAIT Entry"
                           :settings '(:todo-keywords (("WAIT"))))))
          (setf (org-kit:document-settings malformed-keywords)
                '(:todo-keywords (42 (:WAIT))))
          (expect (query-eval-test-titles
                   (org-kit:query-list malformed-keywords '(:todo-present)))
                  :to-equal '("Entry")))
        (let ((legacy-document
                (parse-org "* DONE Entry"
                           :settings '(:todo-keywords (("DONE"))))))
          (setf (org-kit:document-settings legacy-document) nil)
          (expect (query-eval-test-titles
                   (org-kit:query-list legacy-document '(:todo-present)))
                  :to-equal nil))
        (let* ((cyclic
                 (parse-org
                  (format nil "#+TAGS: { First : Second } { Second : First }~%* Entry :x:"))))
          (signals org-query-error
            (org-kit:query-list cyclic '(:tag := "First"))))
        (let* ((document (parse-org "* Entry"))
               (context
                 (org-kit:make-query-context
                  :tag-groups '((:group "Invalid" :members (42))))))
          (signals org-query-error
            (org-kit:query-list document '(:tag := "Invalid") :context context))))))

  (it "caches tag-group regexes through public list queries"
    (let* ((document (parse-org "* Tagged :work:"))
           (context
             (org-kit:make-query-context
              :tag-groups '((:group "State" :members ((:regex "^work$")))))))
      (expect (query-eval-test-titles
               (org-kit:query-list document '(:tag := "State") :context context))
              :to-equal '("Tagged"))))

  (it "follows and validates context tag-group chains through list queries"
    (let* ((document (parse-org "* Tagged :work:"))
           (nested
             (org-kit:make-query-context
              :tag-groups '((:group "Outer" :members ("Inner"))
                            (:group "Inner" :members ("work")))))
           (cyclic
             (org-kit:make-query-context
              :tag-groups '((:group "First" :members ("Second"))
                            (:group "Second" :members ("First")))))
           (invalid
             (org-kit:make-query-context
              :tag-groups '((:group "Invalid" :members (42)))))
           (invalid-list-member
             (org-kit:make-query-context
              :tag-groups '((:group "InvalidListMember"
                             :members ((:word "work")))))))
      (expect (query-eval-test-titles
               (org-kit:query-list document '(:tag := "Outer") :context nested))
              :to-equal '("Tagged"))
      (signals org-query-error
        (org-kit:query-list document '(:tag := "First") :context cyclic))
      (signals org-query-error
        (org-kit:query-list document '(:tag := "Invalid") :context invalid))
      (signals org-query-error
        (org-kit:query-list document '(:tag := "InvalidListMember")
                            :context invalid-list-member))))

  (it "ignores non-string file tags through the public settings setter"
    (let ((document (parse-org "* Tagged :work:")))
      (setf (org-kit:document-settings document)
            (list* :file-tags '(42 "global")
                   (org-kit:document-settings document)))
      (expect (query-eval-test-titles
               (org-kit:query-list
                document '(:property "ALLTAGS" := ":work:global:")))
              :to-equal '("Tagged"))))

  (it "ignores non-symbol property keys set on a parsed headline"
    (let* ((document (parse-org "* Entry"))
           (headline (first (org-kit:query-list document '(:type :headline)))))
      (setf (org-kit:node-property headline "OWNER") "Ada")
      (expect (query-eval-test-titles
               (org-kit:query-list document '(:property-missing "OWNER")))
              :to-equal '("Entry"))))

  (it "continues drawer property lookup after a keyword child"
    (let ((document
            (parse-org
             (format nil "* Entry~%:PROPERTIES:~%#+FOO: bar~%:NOTE: value~%:END:"))))
      (expect (query-eval-test-titles
               (org-kit:query-list document '(:property "NOTE" := "value")))
              :to-equal '("Entry"))))

  (it "rejects over-depth tag-group chains before matching"
    (let* ((document
             (parse-org "* Entry :leaf:" :limits (make-limits :max-element-depth 4)))
           (groups
             (loop for index from 1 to 5
                   collect (list :group (format nil "Group-~D" index)
                                 :members
                                 (list (if (= index 5)
                                           "leaf"
                                           (format nil "Group-~D" (1+ index)))))))
           (context (org-kit:make-query-context :tag-groups groups)))
      (signals org-resource-limit-error
        (org-kit:query-list document '(:tag := "Group-1") :context context)))))

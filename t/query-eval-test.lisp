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

  (it "rejects malformed and over-deep untrusted query structures"
    (let ((document (parse-org "* One")))
      (with-soft-assertions
        (signals org-query-error
          (query-eval-test-run document (cons :and :dotted-tail)))
        (let ((cycle (list :and)))
          (setf (cdr cycle) cycle)
          (signals org-query-error (query-eval-test-run document cycle)))
        (let* ((cycle (list :not nil)))
          (setf (second cycle) cycle)
          (signals org-query-error (query-eval-test-run document cycle)))
        (let ((deep '(:type :headline)))
          (loop repeat 520 do (setf deep (list :not deep)))
          (signals org-resource-limit-error
            (query-eval-test-run document deep)))
        (signals org-query-error
          (query-eval-test-run
           document '(:path ((:name "One" 1 . :tail))))))))

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

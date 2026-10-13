;;;; t/query-list-test.lisp
(in-package #:cl-org-kit/test)

(defun query-list-test-lines (&rest lines)
  (format nil "~{~A~^~%~}" lines))

(defun query-list-test-document ()
  (parse-org
   (query-list-test-lines
    "#+TODO: TODO WAIT | DONE"
    "* TODO Alpha :work:"
    "SCHEDULED: <2024-07-02 Tue>"
    ":PROPERTIES:"
    ":EFFORT: 1"
    ":LABEL: ready"
    ":END:"
    "Alpha body."
    "** DONE Build :urgent:"
    ":PROPERTIES:"
    ":EFFORT: 3"
    ":LABEL: built"
    ":END:"
    "First build body."
    "** TODO Build :fast:"
    ":PROPERTIES:"
    ":EFFORT: 2"
    ":LABEL: fast"
    ":END:"
    "Second build body."
    "* WAIT Beta :home:"
    "DEADLINE: <2024-07-03 Wed>"
    ":PROPERTIES:"
    ":EFFORT: 0"
    ":LABEL: blocked"
    ":END:"
    "Beta body."
    "* Plain"
    "Plain body.")))

(defun query-list-test-titles (nodes)
  (mapcar (lambda (node) (node-property node :raw-value)) nodes))

(defun query-list-test-query-error (document form)
  (handler-case
      (progn (org-kit::query-list document form) nil)
    (org-query-error (condition) condition)))

(defclass query-list-unprintable-test-value () ())

(defmethod print-object ((value query-list-unprintable-test-value) stream)
  (declare (ignore value stream))
  (error "This test value cannot be printed"))

(describe "structured list queries"
  (it "evaluates logic, type, path, and node scope forms"
    (let ((document (query-list-test-document)))
      (with-soft-assertions
        (expect
         (query-list-test-titles
          (org-kit::query-list
           document '(:and (:todo := "TODO") (:tag := "work"))))
         :to-equal '("Alpha" "Build"))
        (expect
         (query-list-test-titles
          (org-kit::query-list
           document '(:or (:todo := "WAIT") (:todo := "DONE")
                          (:todo := "UNKNOWN"))))
         :to-equal '("Build" "Beta"))
        (expect
         (query-list-test-titles
          (org-kit::query-list
           document '(:not (:tag := "work"))))
         :to-equal '("Beta" "Plain"))
        (expect
         (query-list-test-titles
          (org-kit::query-list
           document '(:path ("Alpha" (:name "Build" 2)))))
         :to-equal '("Build"))
        (expect
         (query-list-test-titles
          (org-kit::query-list document '(:path (:** "Build"))))
         :to-equal '("Build" "Build"))
        (expect
         (org-kit::query-list document '(:type :table))
         :to-equal nil)
        (let ((paragraphs
                (org-kit::query-list document '(:type :paragraph)
                                     :scope :nodes)))
          (expect (plusp (length paragraphs)) :to-be t)
          (expect (every (lambda (node) (eq (node-type node) :paragraph))
                         paragraphs)
                  :to-be t)
          (expect
           (mapcar
            (lambda (node)
              (node-property (org-kit::query-nearest-headline node) :raw-value))
            (org-kit::query-list
             document '(:and (:path ("Alpha")) (:type :paragraph))
             :scope :nodes))
           :to-equal '("Alpha"))))))

  (it "evaluates TODO, tag, property, and missing-value operators"
    (let ((document (query-list-test-document)))
      (with-soft-assertions
        (expect (query-list-test-titles
                 (org-kit::query-list document '(:todo-present)))
                :to-equal '("Alpha" "Build" "Beta"))
        (expect (query-list-test-titles
                 (org-kit::query-list document '(:todo-missing)))
                :to-equal '("Plain"))
        (expect (query-list-test-titles
                 (org-kit::query-list document '(:todo :!= "DONE")))
                :to-equal '("Alpha" "Build" "Beta" "Plain"))
        (expect (query-list-test-titles
                 (org-kit::query-list
                  document '(:todo :matches (:regex "TO"))))
                :to-equal '("Alpha" "Build"))
        (expect (query-list-test-titles
                 (org-kit::query-list
                  document '(:todo :not-matches (:regex "TO"))))
                :to-equal '("Build" "Beta" "Plain"))
        (expect (query-list-test-titles
                 (org-kit::query-list document '(:tag := "work")))
                :to-equal '("Alpha" "Build" "Build"))
        (expect (query-list-test-titles
                 (org-kit::query-list document '(:tag :!= "work")))
                :to-equal '("Beta" "Plain"))
        (expect (query-list-test-titles
                 (org-kit::query-list
                  document '(:tag :matches (:regex "^work$"))))
                :to-equal '("Alpha" "Build" "Build"))
        (expect (query-list-test-titles
                 (org-kit::query-list
                  document '(:tag :not-matches (:regex "^work$"))))
                :to-equal '("Beta" "Plain"))
        (expect (query-list-test-titles
                 (org-kit::query-list
                  document '(:property "EFFORT" :< 2)))
                :to-equal '("Alpha" "Beta" "Plain"))
        (dolist (test-case
                 '((:= 2 ("Build"))
                   (:!= 2 ("Alpha" "Build" "Beta" "Plain"))
                   (:<= 2 ("Alpha" "Build" "Beta" "Plain"))
                   (:> 2 ("Build"))
                   (:>= 2 ("Build" "Build"))))
          (destructuring-bind (operator value expected) test-case
            (expect
             (query-list-test-titles
              (org-kit::query-list
               document (list :property "EFFORT" operator value)))
             :to-equal expected)))
        (expect (query-list-test-titles
                 (org-kit::query-list
                  document
                  '(:and (:property "EFFORT" :< 2 :missing-as :zero)
                         (:property-exists "EFFORT"))))
                :to-equal '("Alpha" "Beta"))
        (expect (query-list-test-titles
                 (org-kit::query-list
                  document '(:property "EFFORT" :< 2 :missing-as :nonmatch)))
                :to-equal '("Alpha" "Beta"))
        (expect (query-list-test-titles
                 (org-kit::query-list
                  document '(:property "LABEL" := "ready")))
                :to-equal '("Alpha"))
        (expect (query-list-test-titles
                 (org-kit::query-list
                  document '(:property "LABEL" :!= "ready")))
                :to-equal '("Build" "Build" "Beta"))
        (expect (query-list-test-titles
                 (org-kit::query-list
                  document '(:property "LABEL" :< "c")))
                :to-equal '("Build" "Beta"))
        (expect (query-list-test-titles
                 (org-kit::query-list
                  document '(:property "LABEL" :matches (:regex "^b"))))
                :to-equal '("Build" "Beta"))
        (expect (query-list-test-titles
                 (org-kit::query-list
                  document '(:property "LABEL" :not-matches (:regex "^b"))))
                :to-equal '("Alpha" "Build"))
        (expect (query-list-test-titles
                 (org-kit::query-list document '(:property-exists "LABEL")))
                :to-equal '("Alpha" "Build" "Build" "Beta"))
        (expect (query-list-test-titles
                 (org-kit::query-list document '(:property-missing "LABEL")))
                :to-equal '("Plain")))))

  (it "uses a supplied reference date for planning comparisons"
    (let ((document (query-list-test-document))
          (reference-time (cl-date-kit:make-local-date 2024 7 2)))
      (with-soft-assertions
        (expect (query-list-test-titles
                 (org-kit::query-list
                  document
                  '(:planning :scheduled :>= (:date "<today>"))
                  :reference-time reference-time))
                :to-equal '("Alpha"))
        (expect (query-list-test-titles
                 (org-kit::query-list
                  document
                  '(:planning :deadline := (:date "<+1d>"))
                  :reference-time reference-time))
                :to-equal '("Beta"))
        (expect (query-list-test-titles
                 (org-kit::query-list document '(:planning-exists :deadline)))
                :to-equal '("Beta"))
        (expect (query-list-test-titles
                 (org-kit::query-list document '(:planning-missing :deadline)))
                :to-equal '("Alpha" "Build" "Build" "Plain")))))

  (it "evaluates level predicates"
    (let* ((document (query-list-test-document))
           (context (org-kit::make-query-context))
           (limits (document-limits document))
           (form '(:level :>= 2))
           (predicate
             (org-kit::query-list-parse-form
              form context limits nil "(:level :>= 2)")))
      (expect predicate :to-equal '(:level :>= 2))
      (expect (query-list-test-titles
               (org-kit::query-list document form))
              :to-equal '("Build" "Build"))))

  (it "accepts a supplied query context and validates public options"
    (let ((document (query-list-test-document)))
      (expect (query-list-test-titles
               (org-kit::query-list
                document '(:type :headline)
                :context (org-kit::make-query-context)))
              :to-equal '("Alpha" "Build" "Build" "Beta" "Plain"))
      (signals type-error
        (org-kit::query-list document '(:todo-present) :scope :everything))
      (signals type-error
        (org-kit::query-list document '(:todo-present) :context :invalid))))

  (it "uses a fallback query label when printing user input fails"
    (let* ((document (query-list-test-document))
           (form (list :unknown (make-instance 'query-list-unprintable-test-value)))
           (condition (query-list-test-query-error document form)))
      (expect (org-query-error-query condition) :to-equal "<list query>")))

  (it "rejects malformed forms with org-query-error and no source position"
    (let* ((document (query-list-test-document))
           (bad-forms
             '(()
               (not-a-keyword)
               (:and)
               (:or (:tag := "work"))
               (:not (:tag := "work") (:todo-present))
               (:not (:tag := "work" . :tail))
               (:unknown)
               (:type "headline")
               (:path ())
               (:path "Alpha")
               (:path ("Alpha") "extra")
               (:path ("Alpha" :unknown))
               (:path ("Alpha" (:name "Build")))
               (:path ("Alpha" (:other "Build" 2)))
               (:path ("Alpha" (:name "Build" 0)))
               (:path ("Alpha" (:name "Build" 2 . :tail)))
               (:todo-present :unexpected)
               (:todo-missing :unexpected)
               (:todo)
               (:tag :=)
               (:todo :matches "TO")
               (:tag :matches (:regex "work" :extra))
               (:tag := 42)
               (:tag :unknown "work")
               (:property "EFFORT")
               (:property 42 := "value")
               (:property "EFFORT" :unknown "value")
               (:property "EFFORT" := nil)
               (:property "EFFORT" :< 2 :wrong :zero)
               (:property "EFFORT" := 1 :missing-as :unknown)
               (:property "EFFORT" :< "2" :missing-as :zero)
               (:property "EFFORT" :matches (:regex "b.*") :missing-as :zero)
               (:property "EFFORT" :matches "b.*")
               (:planning :unknown := (:date "2024-07-02"))
               (:planning :scheduled :unknown (:date "2024-07-02"))
               (:planning :scheduled :=)
               (:planning :scheduled :>= "<today>")
               (:planning :scheduled :>= (:date "<today>" :extra))
               (:property-exists 42)
               (:property-exists "LABEL" :extra)
               (:property-missing)
               (:planning-exists :unknown)
               (:planning-exists :scheduled :extra)
               (:planning-missing :unknown)
               (:planning-missing :deadline :extra)
               (:level :contains 1)
               (:type :headline :extra)
               (:level := "1")
               (:property "EFFORT" := (:date "not-a-date"))
               (:tag :matches (:regex "\\("))
               (:planning :scheduled :>= (:date "<today>")))))
      (with-soft-assertions
        (dolist (form bad-forms)
          (let ((condition (query-list-test-query-error document form)))
            (expect condition :to-be-truthy)
            (when condition
              (expect (org-query-error-query condition) :to-be-truthy)
              (expect (org-query-error-position condition) :to-be nil))))
        (let* ((dotted (cons :and :tail))
               (condition (query-list-test-query-error document dotted)))
          (expect condition :to-be-truthy)
          (when condition
            (expect (org-query-error-position condition) :to-be nil)))
        (let ((cycle (list :not nil)))
          (setf (second cycle) cycle)
          (expect (query-list-test-query-error document cycle) :to-be-truthy)))))

  (it "enforces element-depth and selector-string limits"
    (let ((document (make-document "" :limits (make-limits :max-element-depth 1))))
      (signals org-resource-limit-error
        (org-kit::query-list document '(:not (:not (:todo-present))))))
    (let ((document (make-document "" :limits (make-limits :max-input-length 4))))
      (signals org-resource-limit-error
        (org-kit::query-list document '(:tag := "longer")))))

  (it "requires reference-time for relative dates"
    (let ((document (query-list-test-document)))
      (signals org-query-error
        (org-kit::query-list
         document '(:planning :scheduled :>= (:date "<today>")))))))

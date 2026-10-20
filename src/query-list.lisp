;;;; src/query-list.lisp
(in-package #:org-kit)

(defun query-list-query-text (form)
  (handler-case
      (let ((*print-circle* t)
            (*print-level* 8)
            (*print-length* 32)
            (*print-pretty* nil)
            (*print-readably* nil))
        (write-to-string form))
    (error () "<list query>")))

(defun query-list-form-error (query message)
  (query-signal-error query nil message))

(defun query-list-copy-form (form limits query)
  (unless (%query-proper-list-p form)
    (query-list-form-error query "List query must be a proper list"))
  (handler-case
      (%query-copy-proper-tree form limits)
    (type-error ()
      (query-list-form-error query "List query must contain only proper lists"))))

(defun query-list-check-string (value limits)
  (check-limit limits :max-input-length (length value))
  value)

(defun query-list-exact-length-p (form length)
  (= (length form) length))

(defun query-list-regex-literal (form context limits query)
  (unless (and (%query-proper-list-p form)
               (query-list-exact-length-p form 2)
               (eq (first form) :regex)
               (stringp (second form)))
    (query-list-form-error query "Expected a (:regex string) literal"))
  (let ((pattern (query-list-check-string (second form) limits)))
    (handler-case
        (query-compile-emacs-regex pattern context limits query)
      (org-query-error (condition)
        (query-list-form-error query (org-kit-error-message condition))))))

(defun query-list-date-literal (form reference-time limits query)
  (unless (and (%query-proper-list-p form)
               (query-list-exact-length-p form 2)
               (eq (first form) :date)
               (stringp (second form)))
    (query-list-form-error query "Expected a (:date string) literal"))
  (let ((text (query-list-check-string (second form) limits)))
    (query-parse-org-date-literal text reference-time query nil)))

(defun query-list-ordered-value (value reference-time limits query)
  (cond
    ((integerp value) value)
    ((stringp value) (query-list-check-string value limits))
    ((consp value) (query-list-date-literal value reference-time limits query))
    (t
     (query-list-form-error query
                            "Expected a string, integer, or (:date string) value"))))

(defun query-list-check-path-selector (selector limits query)
  (unless (and (%query-proper-list-p selector) selector)
    (query-list-form-error query "Path selector must be a non-empty proper list"))
  (dolist (segment selector)
    (cond
      ((stringp segment)
       (query-list-check-string segment limits))
      ((member segment '(:* :**) :test #'eq) nil)
      ((and (%query-proper-list-p segment)
            (query-list-exact-length-p segment 3)
            (eq (first segment) :name)
            (stringp (second segment))
            (typep (third segment) '(integer 1 *)))
       (query-list-check-string (second segment) limits))
      (t
       (query-list-form-error query "Invalid path selector segment"))))
  selector)

(defun query-list-match-arguments (form context limits query)
  (unless (query-list-exact-length-p form 3)
    (query-list-form-error query "Match predicate has incorrect arity"))
  (let ((operator (second form))
        (value (third form)))
    (cond
      ((member operator '(:= :!=) :test #'eq)
       (unless (stringp value)
         (query-list-form-error query "Equality predicates require a string"))
       (list operator (query-list-check-string value limits)))
      ((member operator '(:matches :not-matches) :test #'eq)
       (list operator (query-list-regex-literal value context limits query)))
      (t
       (query-list-form-error query "Unknown match predicate operator")))))

(defun query-list-parse-property (form context limits reference-time query)
  (unless (and (or (= (length form) 4) (= (length form) 6))
               (stringp (second form)))
    (query-list-form-error query "Property predicate has incorrect arity or name"))
  (let* ((name (query-list-check-string (second form) limits))
         (operator (third form)))
    (cond
      ((member operator '(:matches :not-matches) :test #'eq)
       (unless (= (length form) 4)
         (query-list-form-error query
                                "Regex property predicates do not accept :missing-as"))
       (list :property
             :name name
             :operator operator
             :value (query-list-regex-literal (fourth form) context limits query)
             :missing-as :nonmatch))
      ((member operator '(:= :!= :< :<= :> :>=) :test #'eq)
       (let* ((value (query-list-ordered-value
                      (fourth form) reference-time limits query))
              (missing-as
                (if (= (length form) 6)
                    (progn
                      (unless (eq (fifth form) :missing-as)
                        (query-list-form-error query
                                               "Expected :missing-as before its mode"))
                      (sixth form))
                    (if (integerp value) :zero :nonmatch))))
         (unless (member missing-as '(:zero :nonmatch) :test #'eq)
           (query-list-form-error query "Unknown property missing mode"))
         (when (and (eq missing-as :zero) (not (integerp value)))
           (query-list-form-error query
                                  ":missing-as :zero requires an integer value"))
         (list :property
               :name name
               :operator operator
               :value value
               :missing-as missing-as)))
      (t
       (query-list-form-error query "Unknown property predicate operator")))))

(defun query-list-parse-predicate (form context limits reference-time query)
  (unless (and (%query-proper-list-p form) (consp form) (keywordp (first form)))
    (query-list-form-error query "Each query form must be a proper list with a keyword operator"))
  (let ((operator (first form)))
    (case operator
      (:and
       (unless (>= (length form) 3)
         (query-list-form-error query "AND requires at least two child forms"))
       (cons :and
             (mapcar (lambda (child)
                       (query-list-parse-predicate
                        child context limits reference-time query))
                     (rest form))))
      (:or
       (unless (>= (length form) 3)
         (query-list-form-error query "OR requires at least two child forms"))
       (cons :or
             (mapcar (lambda (child)
                       (query-list-parse-predicate
                        child context limits reference-time query))
                     (rest form))))
      (:not
       (unless (query-list-exact-length-p form 2)
         (query-list-form-error query "NOT requires exactly one child form"))
       (list :not (query-list-parse-predicate
                   (second form) context limits reference-time query)))
      (:path
       (unless (query-list-exact-length-p form 2)
         (query-list-form-error query "Path predicate has incorrect arity"))
       (list :path
             (query-list-check-path-selector (second form) limits query)))
      (:type
       (unless (and (query-list-exact-length-p form 2)
                    (node-type-p (second form)))
         (query-list-form-error query "Type predicate requires a known node type"))
       (list :type (second form)))
      (:diary-match
       (unless (query-list-exact-length-p form 1)
         (query-list-form-error query "DIARY-MATCH takes no arguments"))
       (list :diary-match))
      (:todo-present
       (unless (query-list-exact-length-p form 1)
         (query-list-form-error query "TODO-PRESENT takes no arguments"))
       (list :todo-present))
      (:todo-missing
       (unless (query-list-exact-length-p form 1)
         (query-list-form-error query "TODO-MISSING takes no arguments"))
       (list :todo-missing))
      (:todo
       (let ((arguments (query-list-match-arguments form context limits query)))
         (cons :todo arguments)))
      (:tag
       (let ((arguments (query-list-match-arguments form context limits query)))
         (cons :tag arguments)))
      (:property
       (query-list-parse-property form context limits reference-time query))
      (:level
       (unless (and (query-list-exact-length-p form 3)
                    (member (second form) '(:= :!= :< :<= :> :>=) :test #'eq)
                    (integerp (third form)))
         (query-list-form-error query
                                "Level predicate requires a numeric comparator and integer"))
       (list :level (second form) (third form)))
      (:planning
       (unless (and (query-list-exact-length-p form 4)
                    (member (second form) '(:scheduled :deadline :closed) :test #'eq)
                    (member (third form) '(:= :!= :< :<= :> :>=) :test #'eq))
         (query-list-form-error query "Planning predicate has invalid fields or arity"))
       (list :planning
             (second form)
             (third form)
             (query-list-date-literal
              (fourth form) reference-time limits query)))
      (:property-exists
       (unless (and (query-list-exact-length-p form 2)
                    (stringp (second form)))
         (query-list-form-error query "Property-exists requires one property name"))
       (list :exists :property
             (query-list-check-string (second form) limits)))
      (:property-missing
       (unless (and (query-list-exact-length-p form 2)
                    (stringp (second form)))
         (query-list-form-error query "Property-missing requires one property name"))
       (list :missing :property
             (query-list-check-string (second form) limits)))
      (:planning-exists
       (unless (and (query-list-exact-length-p form 2)
                    (member (second form) '(:scheduled :deadline :closed) :test #'eq))
         (query-list-form-error query "Planning-exists requires a planning field"))
       (list :exists :planning (second form)))
      (:planning-missing
       (unless (and (query-list-exact-length-p form 2)
                    (member (second form) '(:scheduled :deadline :closed) :test #'eq))
         (query-list-form-error query "Planning-missing requires a planning field"))
       (list :missing :planning (second form)))
      (otherwise
       (query-list-form-error query "Unknown query operator")))))

(defun query-list-parse-form (form context limits reference-time query)
  (let ((copy (query-list-copy-form form limits query)))
    (query-list-parse-predicate copy context limits reference-time query)))

(defun query-list (document form
                    &key (scope :headlines) context reference-time
                      callback-registry)
  "Evaluate structured FORM against DOCUMENT and return matching nodes.
FORM is a proper, unevaluated list expression. SCOPE is :HEADLINES or
:NODES. CONTEXT configures property, tag, provider, and regex behavior.
REFERENCE-TIME may be NIL, a local date, or a local date-time; relative
values require one. :DIARY-MATCH requires :SCOPE :NODES and an explicit
REFERENCE-TIME. CALLBACK-REGISTRY supplies unknown diary forms and defaults
to an empty registry."
  (check-type document document)
  (check-type reference-time (or null cl-date-kit:local-date
                                 cl-date-kit:local-date-time))
  (unless (member scope '(:headlines :nodes) :test #'eq)
    (error 'type-error :datum scope :expected-type '(member :headlines :nodes)))
  (when (and context (not (query-context-p context)))
    (error 'type-error :datum context :expected-type 'query-context))
  (when (and callback-registry (not (callback-registry-p callback-registry)))
    (error 'type-error :datum callback-registry
           :expected-type 'callback-registry))
  (let* ((query (query-list-query-text form))
         (context (or context (make-query-context)))
         (limits (document-limits document))
         (predicate
           (query-list-parse-form form context limits reference-time query))
         (callback-registry (or callback-registry (make-callback-registry))))
    (labels ((contains-diary-match-p (predicate)
               (case (first predicate)
                 (:diary-match t)
                 ((:and :or)
                  (some #'contains-diary-match-p (rest predicate)))
                 (:not
                  (contains-diary-match-p (second predicate)))
                 (otherwise nil))))
      (let ((diary-match-p (contains-diary-match-p predicate)))
        (when (and diary-match-p (not (eq scope :nodes)))
          (query-list-form-error query
                                 "DIARY-MATCH requires :SCOPE :NODES"))
        (when (and diary-match-p (null reference-time))
          (query-list-form-error query
                                 "DIARY-MATCH requires an explicit :REFERENCE-TIME"))
        (query-evaluate-ir
         document (%make-query-ir scope predicate context)
         :reference-time reference-time
         :callback-registry callback-registry
         :diary-match-p diary-match-p)))))

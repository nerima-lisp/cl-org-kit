;;;; t/edit-sort-test.lisp
(in-package #:cl-org-kit/test)

(defparameter +edit-sort-oracle-fixtures+
  '("alpha" "numeric" "time" "created" "scheduled" "deadline"
    "priority" "priority-directive" "todo" "property"))

(defun edit-sort-test-fixture (relative-path)
  (let ((path (asdf:system-relative-pathname
               "cl-org-kit" relative-path))
        (eof (gensym "EOF")))
    (with-open-file (stream path :direction :input)
      (let ((*read-eval* nil)
            (fixture (read stream nil eof)))
        (when (eq fixture eof)
          (error "Sort fixture ~A is empty." relative-path))
        (unless (eq (read stream nil eof) eof)
          (error "Sort fixture ~A has more than one form." relative-path))
        fixture))))

(defun edit-sort-test-headlines (document)
  (let ((headlines '()))
    (map-nodes (lambda (node)
                 (when (eq (node-type node) :headline)
                   (push node headlines)))
               (document-root document))
    (nreverse headlines)))

(defun edit-sort-test-parent (document)
  (find :headline (node-children (document-root document)) :key #'node-type))

(defun edit-sort-test-direct-child-values (heading)
  (mapcar (lambda (child) (node-property child :raw-value))
          (remove-if-not (lambda (child) (eq (node-type child) :headline))
                         (node-children heading))))

(defun edit-sort-test-direct-child-headline-lines (document heading)
  (let ((source (document-text document)))
    (mapcar
     (lambda (child)
       (let* ((start (node-post-affiliated child))
              (line-end (or (position #\Newline source :start start)
                            (length source)))
              (line (string-right-trim '(#\Return)
                                       (subseq source start line-end)))
              (title-start (position #\Space line)))
         (subseq line (1+ title-start))))
     (remove-if-not (lambda (child) (eq (node-type child) :headline))
                    (node-children heading)))))

(defun edit-sort-test-time-context ()
  (make-time-context :reference-time (make-org-time-value "2025-01-01")
                     :zone (make-fixed-time-zone 0)))

(defun edit-sort-test-call (document heading key property-name direction)
  (let ((options (append (list :direction direction)
                         (when (eq key :property)
                           (list :property-name property-name))
                         (when (member key '(:time :scheduled :deadline)
                                       :test #'eq)
                           (list :time-context (edit-sort-test-time-context))))))
    (apply #'sort-subtree-children document heading key options)))

(defun edit-sort-test-reorder-fixture (fixture direction)
  (let* ((source (getf fixture :input))
         (document (parse-org source))
         (parent (edit-sort-test-parent document))
         (key (getf fixture :key))
         (property-name (getf fixture :property-name))
         (result (edit-sort-test-call document parent key property-name direction))
         (updated (edit-result-updated-document result)))
    (values document parent result updated source)))

(defun edit-sort-test-patch-excludes-outside-p
    (source output start end replacement)
  (let ((replacement-end (+ start (length replacement))))
    (and (string= source output :start1 0 :end1 start :start2 0 :end2 start)
         (string= source output
                  :start1 end :end1 (length source)
                  :start2 replacement-end :end2 (length output)))))

(defun edit-sort-test-utf8-bom-octets (source)
  (cl-codec-kit:string-to-octets
   (concatenate 'string (string (code-char #xfeff)) source)
   :encoding :utf-8))

(describe "subtree child sorting"
  (it "matches the Org 9.8.10 result for every built-in key"
    (dolist (name +edit-sort-oracle-fixtures+)
      (let* ((relative-path
               (format nil "t/fixtures/e/oracle/sort/~A.sexp" name))
             (fixture (edit-sort-test-fixture relative-path)))
        (multiple-value-bind (document parent result updated source)
            (edit-sort-test-reorder-fixture fixture :ascending)
          (with-soft-assertions
            (expect (getf fixture :operation) :to-be :sort)
            (expect (getf fixture :org-version) :to-equal "9.8.10")
            (unless (eq (getf fixture :key) :alpha)
              (expect (document-text updated) :to-equal (getf fixture :expected))
              (expect (edit-sort-test-direct-child-headline-lines
                       updated (edit-sort-test-parent updated))
                      :to-equal (getf fixture :expected-order)))
            (expect (document-text document) :to-equal source)
            (expect (node-property parent :raw-value) :to-equal "Parent")
            (expect (edit-result-p result) :to-be t))))))

  (it "uses the policy fixtures for code-point alpha and missing-last behavior"
    (dolist (name '("alpha-code-point-case-sensitive"
                    "created-missing-last"
                    "property-missing-last" "todo-missing-last"))
      (let* ((relative-path
               (format nil "t/fixtures/e/policy/sort/~A.sexp" name))
             (fixture (edit-sort-test-fixture relative-path)))
        (multiple-value-bind (document parent result updated source)
            (edit-sort-test-reorder-fixture fixture :ascending)
          (declare (ignore parent))
          (with-soft-assertions
            (expect (getf fixture :operation) :to-be :sort)
            (expect (edit-sort-test-direct-child-headline-lines
                     updated (edit-sort-test-parent updated))
                    :to-equal (getf fixture :expected-order))
            (expect (document-text document) :to-equal source)
            (expect (edit-result-p result) :to-be t)
            (when (string= name "alpha-code-point-case-sensitive")
              (expect (getf fixture :divergence)
                      :to-be :org-alpha-is-case-insensitive)
              (expect (equal
                       (edit-sort-test-direct-child-headline-lines
                        updated (edit-sort-test-parent updated))
                       (getf fixture :oracle-expected-order))
                      :to-be nil)))))))

  (it "keeps missing values last for ascending and descending property order"
    (let* ((source
             (format nil
                     "* Parent~%** C~%:PROPERTIES:~%:RANK: C~%:END:~%** A~%:PROPERTIES:~%:RANK: A~%:END:~%** Missing~%"))
           (ascending (parse-org source))
           (descending (parse-org source))
           (ascending-result
             (sort-subtree-children ascending (edit-sort-test-parent ascending)
                                    :property :property-name "RANK"))
           (descending-result
             (sort-subtree-children descending (edit-sort-test-parent descending)
                                    :property :property-name "RANK"
                                    :direction :descending)))
      (with-soft-assertions
        (expect (edit-sort-test-direct-child-values
                 (edit-sort-test-parent
                  (edit-result-updated-document ascending-result)))
                :to-equal '("A" "C" "Missing"))
        (expect (edit-sort-test-direct-child-values
                 (edit-sort-test-parent
                  (edit-result-updated-document descending-result)))
                :to-equal '("C" "A" "Missing"))
        (expect (document-text ascending) :to-equal source)
        (expect (document-text descending) :to-equal source))))

  (it "keeps equal keys in source order"
    (let* ((source (format nil "* Parent~%** Z~%last~%** Same~%first~%** Same~%second~%** A~%first alphabetically~%"))
           (expected (format nil "* Parent~%** A~%first alphabetically~%** Same~%first~%** Same~%second~%** Z~%last~%"))
           (document (parse-org source))
           (parent (edit-sort-test-parent document))
           (result (sort-subtree-children document parent :alpha))
           (updated (edit-result-updated-document result)))
      (with-soft-assertions
        (expect (document-text updated) :to-equal expected)
        (expect (edit-sort-test-direct-child-values
                 (edit-sort-test-parent updated))
                :to-equal '("A" "Same" "Same" "Z"))
        (expect (document-text document) :to-equal source))))

  (it "sorts a final child without adding a final newline"
    (let* ((source (format nil "* P~%** B~%** A"))
           (document (parse-org source))
           (result (sort-subtree-children document
                                          (edit-sort-test-parent document)
                                          :alpha))
           (updated (edit-result-updated-document result)))
      (with-soft-assertions
        (expect (document-text updated)
                :to-equal (format nil "* P~%** A~%** B"))
        (expect (document-text document) :to-equal source))))

  (it "keeps parent section content while sorting its headline children"
    (let* ((source
             (format nil
                     "* Parent~%parent body~%** Z~%z body~%** A~%a body~%"))
           (expected
             (format nil
                     "* Parent~%parent body~%** A~%a body~%** Z~%z body~%"))
           (document (parse-org source))
           (result
             (sort-subtree-children document
                                    (edit-sort-test-parent document) :alpha))
           (updated (edit-result-updated-document result)))
      (with-soft-assertions
        (expect (document-text updated) :to-equal expected)
        (expect (edit-sort-test-direct-child-values
                 (edit-sort-test-parent updated))
                :to-equal '("A" "Z"))
        (expect (document-text document) :to-equal source))))

  (it "reports the exact source span and leaves text outside sorted children unchanged"
    (let* ((source (format nil "* Before~%outside prefix~%* Parent~%** Z~%z body~%** A~%a body~%* After~%outside suffix~%"))
           (expected (format nil "* Before~%outside prefix~%* Parent~%** A~%a body~%** Z~%z body~%* After~%outside suffix~%"))
           (document (parse-org source))
           (parent (find "Parent" (edit-sort-test-headlines document)
                          :key (lambda (node) (node-property node :raw-value))
                          :test #'string=))
           (children (remove-if-not (lambda (node) (eq (node-type node) :headline))
                                    (node-children parent)))
           (start (node-begin (first children)))
           (end (search "* After" source))
           (result (sort-subtree-children document parent :alpha))
           (updated (edit-result-updated-document result)))
      (with-soft-assertions
        (expect (document-text updated) :to-equal expected)
        (expect (mapcar (lambda (span)
                          (list (edit-span-start span) (edit-span-end span)))
                        (edit-result-spans result))
                :to-equal (list (list start end)))
        (expect (edit-sort-test-patch-excludes-outside-p
                 source expected start end (subseq expected start end))
                :to-be t)
        (expect (document-text document) :to-equal source))))

  (it "preserves UTF-8 BOM octets and CRLF while sorting"
    (let* ((crlf (concatenate 'string (string #\Return) (string #\Newline)))
           (source (concatenate 'string
                                "* 外" crlf "prefix 日本" crlf
                                "** Parent" crlf
                                "*** Z" crlf "z 本文" crlf
                                "*** A" crlf "a 本文" crlf
                                "* 後" crlf))
           (expected (concatenate 'string
                                  "* 外" crlf "prefix 日本" crlf
                                  "** Parent" crlf
                                  "*** A" crlf "a 本文" crlf
                                  "*** Z" crlf "z 本文" crlf
                                  "* 後" crlf))
           (document (parse-org (edit-sort-test-utf8-bom-octets source)
                                :encoding :utf-8))
           (parent (find "Parent" (edit-sort-test-headlines document)
                          :key (lambda (node)
                                 (string-right-trim
                                  '(#\Return)
                                  (node-property node :raw-value)))
                          :test #'string=))
           (result (sort-subtree-children document parent :alpha))
           (updated (edit-result-updated-document result)))
      (with-soft-assertions
        (expect (document-bom-p updated) :to-be t)
        (expect (document-text updated) :to-equal expected)
        (expect (write-org updated :output-type :octets)
                :to-equalp (edit-sort-test-utf8-bom-octets expected))
        (expect (document-text document) :to-equal source))))

  (it "uses a document priority default and honors an explicit missing default"
    (let* ((source (format nil "#+PRIORITIES: B D C~%* Parent~%** [#B] Explicit~%** Implicit~%** [#D] Low~%"))
           (document (parse-org source))
           (parent (edit-sort-test-parent document))
           (result (sort-subtree-children document parent :priority))
           (missing-source (format nil "* Parent~%** [#A] A~%** Unset~%** [#C] C~%"))
           (missing-document (parse-org missing-source
                                        :settings '(:priority-default nil)))
           (missing-parent (edit-sort-test-parent missing-document))
           (missing-result
             (sort-subtree-children missing-document missing-parent :priority)))
      (with-soft-assertions
        (expect (edit-sort-test-direct-child-values
                 (edit-sort-test-parent (edit-result-updated-document result)))
                :to-equal '("Explicit" "Implicit" "Low"))
        (expect (edit-sort-test-direct-child-values
                 (edit-sort-test-parent
                  (edit-result-updated-document missing-result)))
                :to-equal '("A" "C" "Unset"))
        (expect (document-text document) :to-equal source)
        (expect (document-text missing-document) :to-equal missing-source))))

  (it "requires an explicit context for time keys and resolves DST errors"
    (let* ((source (format nil "* Parent~%** <2025-03-09 Sun 02:30> Gap~%** <2025-03-09 Sun 03:30> Valid~%"))
           (document (parse-org source))
           (parent (edit-sort-test-parent document))
           (missing-context
             (handler-case (sort-subtree-children document parent :time)
               (org-time-error (condition) condition)))
           (strict-context
             (make-time-context :reference-time (make-org-time-value "2025-03-10")
                                :zone (resolve-time-zone "America/New_York")
                                :dst-policy :strict))
           (dst-error
             (handler-case
                 (sort-subtree-children document parent :time
                                        :time-context strict-context)
               (org-time-error (condition) condition))))
      (with-soft-assertions
        (expect (typep missing-context 'org-time-error) :to-be t)
        (expect (org-time-error-reason missing-context) :to-be :invalid-time)
        (expect (typep dst-error 'org-time-error) :to-be t)
        (expect (org-time-error-reason dst-error) :to-be :dst-gap)
        (expect (document-text document) :to-equal source))))

  (it "rejects invalid numeric values, invalid CREATED dates, and duplicate properties"
    (let* ((numeric-source (format nil "* Parent~%** 10~%** not-a-number~%"))
           (numeric-document (parse-org numeric-source))
           (numeric-error
             (handler-case
                 (sort-subtree-children numeric-document
                                        (edit-sort-test-parent numeric-document)
                                        :numeric)
               (org-edit-error (condition) condition)))
           (created-source
             (format nil "* Parent~%** First~%:PROPERTIES:~%:CREATED: [2024-02-30 Fri]~%:END:~%** Second~%"))
           (created-document (parse-org created-source))
           (created-error
             (handler-case
                 (sort-subtree-children created-document
                                        (edit-sort-test-parent created-document)
                                        :created)
               (org-edit-error (condition) condition)))
           (property-source
             (format nil "* Parent~%** First~%:PROPERTIES:~%:RANK: A~%:RANK: B~%:END:~%** Second~%:PROPERTIES:~%:RANK: C~%:END:~%"))
           (property-document (parse-org property-source))
           (property-error
             (handler-case
                 (sort-subtree-children property-document
                                        (edit-sort-test-parent property-document)
                                        :property :property-name "RANK")
               (org-edit-error (condition) condition))))
      (with-soft-assertions
        (expect (org-edit-error-reason numeric-error) :to-be :invalid-value)
        (expect (org-edit-error-target numeric-error) :to-equal "headline")
        (expect (org-kit-error-message numeric-error)
                :to-equal "Headline value is not a decimal number")
        (expect (org-edit-error-reason created-error) :to-be :invalid-value)
        (expect (org-edit-error-target created-error) :to-equal "headline")
        (expect (org-kit-error-message created-error)
                :to-equal "CREATED property is not a valid timestamp")
        (expect (org-edit-error-reason property-error) :to-be :ambiguous-target)
        (expect (org-edit-error-target property-error) :to-equal "headline")
        (expect (org-kit-error-message property-error)
                :to-equal "Requested property is ambiguous")
        (expect (document-text numeric-document) :to-equal numeric-source)
        (expect (document-text created-document) :to-equal created-source)
        (expect (document-text property-document) :to-equal property-source))))

  (it "validates key, direction, and property-name options"
    (let* ((source (format nil "* Parent~%** A~%** B~%"))
           (document (parse-org source))
           (parent (edit-sort-test-parent document))
           (errors
             (loop for thunk in
                     (list (lambda () (sort-subtree-children document parent :unknown))
                           (lambda () (sort-subtree-children document parent :alpha
                                                             :direction :sideways))
                           (lambda () (sort-subtree-children document parent :property))
                           (lambda () (sort-subtree-children document parent :alpha
                                                             :property-name nil))
                           (lambda () (sort-subtree-children document parent :property
                                                             :property-name nil))
                           (lambda () (sort-subtree-children document parent :property
                                                             :property-name ""))
                           (lambda () (sort-subtree-children document parent :property
                                                             :property-name 1)))
                   collect (handler-case (funcall thunk)
                             (org-edit-error (condition) condition)))))
      (with-soft-assertions
        (expect (mapcar #'org-edit-error-reason errors)
                :to-equal '(:invalid-value :invalid-value :invalid-value
                            :invalid-value :invalid-value :invalid-value
                            :invalid-value))
        (expect (every (lambda (condition)
                         (and (stringp (org-edit-error-target condition))
                              (stringp (org-kit-error-message condition))))
                       errors)
                :to-be t)
        (expect (document-text document) :to-equal source))))

  (it "orders prefix values by their code-point length"
    (let* ((short-first-source (format nil "* Parent~%** A~%** AA~%"))
           (long-first-source (format nil "* Parent~%** AA~%** A~%"))
           (short-first (parse-org short-first-source))
           (long-first (parse-org long-first-source))
           (short-result
             (sort-subtree-children short-first
                                    (edit-sort-test-parent short-first) :alpha))
           (long-result
             (sort-subtree-children long-first
                                    (edit-sort-test-parent long-first) :alpha)))
      (with-soft-assertions
        (expect (edit-sort-test-direct-child-values
                 (edit-sort-test-parent (edit-result-updated-document short-result)))
                :to-equal '("A" "AA"))
        (expect (edit-sort-test-direct-child-values
                 (edit-sort-test-parent (edit-result-updated-document long-result)))
                :to-equal '("A" "AA")))))

  (it "resolves priority defaults from settings and a short priorities directive"
    (let ((source (format nil "* Parent~%** [#C] Explicit~%** Implicit~%")))
      (dolist (case '((66 ("Implicit" "Explicit"))
                      (#\B ("Implicit" "Explicit"))
                      ("66" ("Implicit" "Explicit"))
                      ("B" ("Implicit" "Explicit"))
                      ("" ("Explicit" "Implicit"))
                      ("9X" ("Explicit" "Implicit"))
                      (:unsupported ("Explicit" "Implicit"))))
        (let* ((document (parse-org source
                                    :settings (list :priority-default (first case))))
               (result (sort-subtree-children
                        document (edit-sort-test-parent document) :priority)))
          (expect (edit-sort-test-direct-child-values
                   (edit-sort-test-parent (edit-result-updated-document result)))
                  :to-equal (second case))))
      (let* ((directive-source
               (format nil "#+PRIORITIES: A B~%~A" source))
             (document (parse-org directive-source))
             (result (sort-subtree-children
                      document (edit-sort-test-parent document) :priority)))
        (expect (edit-sort-test-direct-child-values
                 (edit-sort-test-parent (edit-result-updated-document result)))
                :to-equal '("Implicit" "Explicit")))
      (let* ((scanned-source
               (format nil
                       "Preamble text~%#+PRIORITIES: A C D~%* Parent~%** [#D] Explicit~%** Implicit~%** [#C] Medium~%"))
             (document (parse-org scanned-source))
             (result (sort-subtree-children
                      document (edit-sort-test-parent document) :priority)))
        (expect (edit-sort-test-direct-child-values
                 (edit-sort-test-parent (edit-result-updated-document result)))
                :to-equal '("Medium" "Explicit" "Implicit")))))

  (it "keeps two missing sort keys in source order and accepts a symbol property name"
    (let* ((missing-source (format nil "* Parent~%** Z~%** A~%"))
           (missing-document (parse-org missing-source))
           (missing-result
             (sort-subtree-children missing-document
                                    (edit-sort-test-parent missing-document) :todo))
           (property-source
             (format nil "* Parent~%** Z~%:PROPERTIES:~%:RANK: Z~%:END:~%** A~%:PROPERTIES:~%:RANK: A~%:END:~%"))
           (property-document (parse-org property-source))
           (property-result
             (sort-subtree-children property-document
                                    (edit-sort-test-parent property-document)
                                    :property :property-name 'rank)))
      (with-soft-assertions
        (expect (edit-sort-test-direct-child-values
                 (edit-sort-test-parent
                  (edit-result-updated-document missing-result)))
                :to-equal '("Z" "A"))
        (expect (edit-sort-test-direct-child-values
                 (edit-sort-test-parent
                  (edit-result-updated-document property-result)))
                :to-equal '("A" "Z")))))

  (it "rejects duplicate CREATED values and non-timestamp CREATED text"
    (let* ((duplicate-source
             (format nil "* Parent~%** First~%:PROPERTIES:~%:CREATED: [2024-01-01 Mon]~%:CREATED: [2024-01-02 Tue]~%:END:~%** Second~%"))
           (duplicate-document (parse-org duplicate-source))
           (duplicate-error
             (handler-case
                 (sort-subtree-children duplicate-document
                                        (edit-sort-test-parent duplicate-document)
                                        :created)
               (org-edit-error (condition) condition)))
           (text-source
             (format nil "* Parent~%** First~%:PROPERTIES:~%:CREATED: not a timestamp~%:END:~%** Second~%"))
           (text-document (parse-org text-source))
           (text-error
             (handler-case
                 (sort-subtree-children text-document
                                        (edit-sort-test-parent text-document)
                                        :created)
               (org-edit-error (condition) condition))))
      (with-soft-assertions
        (expect (org-edit-error-reason duplicate-error) :to-be :ambiguous-target)
        (expect (org-edit-error-reason text-error) :to-be :invalid-value)
        (expect (org-kit-error-message text-error)
                :to-equal "CREATED property is not a valid timestamp")
        (expect (document-text duplicate-document) :to-equal duplicate-source)
        (expect (document-text text-document) :to-equal text-source))))

  (it "rejects a non-string CREATED value from a parsed public node"
    (let* ((source
             (format nil
                     "* Parent~%** First~%:PROPERTIES:~%:CREATED: [2024-01-01 Mon]~%:END:~%** Second~%"))
           (document (parse-org source))
           (property nil))
      (map-nodes
       (lambda (node)
         (when (and (eq (node-type node) :node-property)
                    (string= (node-property node :key) "CREATED"))
           (setf property node)))
       (document-root document))
      (setf (node-property property :value) 1)
      (let ((condition
              (handler-case
                  (sort-subtree-children document
                                         (edit-sort-test-parent document)
                                         :created)
                (org-edit-error (error) error))))
        (with-soft-assertions
          (expect (org-edit-error-reason condition) :to-be :invalid-value)
          (expect (org-kit-error-message condition)
                  :to-equal "CREATED property is not a valid timestamp")
          (expect (document-text document) :to-equal source)))))

  (it "rejects a non-string property value from a parsed public node"
    (let* ((source
             (format nil "* Parent~%** A~%:PROPERTIES:~%:RANK: one~%:END:~%** B~%:PROPERTIES:~%:RANK: two~%:END:~%"))
           (document (parse-org source))
           (property nil))
      (map-nodes
       (lambda (node)
         (when (and (eq (node-type node) :node-property)
                    (string= (node-property node :key) "RANK"))
           (setf property node)))
       (document-root document))
      (setf (node-property property :value) 1)
      (let ((condition
              (handler-case
                  (sort-subtree-children document
                                         (edit-sort-test-parent document)
                                         :property :property-name "RANK")
                (org-edit-error (error) error))))
        (with-soft-assertions
          (expect (org-edit-error-reason condition) :to-be :invalid-value)
          (expect (org-kit-error-message condition)
                  :to-equal "Requested property value is not a string")
          (expect (document-text document) :to-equal source))))))

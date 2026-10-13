;;;; t/clocktable-test.lisp
(in-package #:cl-org-kit/test)

(defun clocktable-test-fixture (tree)
  (read-conformance-tree
   (asdf:system-relative-pathname
    "cl-org-kit"
    (format nil "t/fixtures/behavior/~A/g2-clocktable.sexp" tree))))

(defun clocktable-test-source-octets (source-name)
  (read-conformance-octets
   (asdf:system-relative-pathname
    "cl-org-kit"
    (format nil "t/fixtures/behavior/corpus/~A" source-name))))

(defun clocktable-test-capture-condition (thunk)
  (handler-case
      (progn (funcall thunk) nil)
    (condition (condition) condition)))

(defun clocktable-test-expect-clock-error (condition reason context message)
  (expect (typep condition 'org-clock-error) :to-be t)
  (when (typep condition 'org-clock-error)
    (expect (org-kit-error-message condition) :to-equal message)
    (expect (org-clock-error-reason condition) :to-equal reason)
    (expect (org-clock-error-context condition) :to-equal context)))

(defun clocktable-test-expect-time-error (condition reason context message)
  (expect (typep condition 'org-time-error) :to-be t)
  (when (typep condition 'org-time-error)
    (expect (org-kit-error-message condition) :to-equal message)
    (expect (org-time-error-reason condition) :to-equal reason)
    (expect (org-time-error-context condition) :to-equal context)))

(defun clocktable-test-expect-type-error (condition datum expected-type)
  (expect (typep condition 'type-error) :to-be t)
  (when (typep condition 'type-error)
    (expect (type-error-datum condition) :to-equal datum)
    (expect (type-error-expected-type condition) :to-equal expected-type)))

(defun clocktable-test-node-of-type (document type)
  (let ((found nil))
    (map-nodes (lambda (node)
                 (when (and (null found) (eq (node-type node) type))
                   (setf found node)))
               (document-root document))
    found))

(defun clocktable-test-node-count (document)
  (let ((count 0))
    (map-nodes (lambda (node)
                 (declare (ignore node))
                 (incf count))
               (document-root document))
    count))

(defun clocktable-test-heading-path (document path)
  (let ((found nil))
    (map-nodes
     (lambda (node)
       (when (eq (node-type node) :headline)
         (let ((actual-path
                 (loop for ancestor = node then (node-parent ancestor)
                       while ancestor
                       when (eq (node-type ancestor) :headline)
                         collect (node-property ancestor :raw-value))))
           (when (equal (nreverse actual-path) path)
             (setf found node)))))
     (document-root document))
    found))

(defun clocktable-test-rows (result)
  (mapcar (lambda (row)
            (list :level (clocktable-row-level row)
                  :title (clocktable-row-title row)
                  :tags (clocktable-row-tags row)
                  :minutes (clocktable-row-minutes row)))
          (clocktable-result-rows result)))

(defun clocktable-test-run-case (document case)
  (let* ((from (make-org-time-value (getf case :from)))
         (to (make-org-time-value (getf case :to)))
         (range (make-time-range from to))
         (context (make-time-context
                   :reference-time from
                   :zone (resolve-time-zone (getf case :zone))
                   :dst-policy :compatible))
         (heading (clocktable-test-heading-path
                   document (getf case :heading-path)))
         (options (append (list :range range :time-context context)
                          (when (member :match case)
                            (list :match (getf case :match)))
                          (when (member :max-depth case)
                            (list :max-depth (getf case :max-depth))))))
    (when heading
      (values (apply #'clocktable-data document heading options)
              range))))

(describe "Org clocktable data"
  (it "matches the generated Org oracle cases and preserves source octets"
    (let* ((cases (getf (clocktable-test-fixture "cases") :cases))
           (expected (clocktable-test-fixture "expected")))
      (with-soft-assertions
        (expect (length cases) :to-be (length expected))
        (dolist (case cases)
          (let* ((id (getf case :id))
                 (match-query (getf case :match))
                 (match-query-copy (and (stringp match-query)
                                        (copy-seq match-query)))
                 (source-octets
                   (clocktable-test-source-octets (getf case :source)))
                 (source-copy (copy-seq source-octets))
                 (document
                   (parse-org source-octets
                              :inlinetask-min-level
                              (getf case :inlinetask-min-level 15)))
                 (oracle (find id expected :key (lambda (row) (getf row :id))
                               :test #'string=)))
            (expect (list id (not (null oracle))) :to-equal (list id t))
            (multiple-value-bind (result range)
                (clocktable-test-run-case document case)
              (expect (list id (clocktable-result-p result))
                      :to-equal (list id t))
              (when (and result oracle)
                (expect (list id (clocktable-result-total-minutes result))
                        :to-equal
                        (list id (getf oracle :org-clock-total-minutes)))
                (expect (list id (clocktable-test-rows result))
                        :to-equal (list id (getf oracle :org-clock-rows)))
                (expect (list id (eq (clocktable-result-range result) range))
                        :to-equal (list id t))
                (dolist (row (clocktable-result-rows result))
                  (expect (list id (eq (clocktable-row-range row) range))
                          :to-equal (list id t)))))
            (expect (equal match-query-copy (getf case :match)) :to-be t)
            (expect (equalp source-octets source-copy) :to-be t)
            (expect (equalp source-copy
                            (write-org document :output-type :octets))
                    :to-be t)))))))

(describe "Org clocktable data validation"
  (it "rejects malformed and duplicate option lists"
    (let* ((document (parse-org "* Scope"))
           (heading (clocktable-test-heading-path document '("Scope")))
           (from (make-org-time-value "2026-10-12T09:00:00"))
           (to (make-org-time-value "2026-10-12T10:00:00"))
           (range (make-time-range from to))
           (context (make-time-context :reference-time from
                                       :zone (make-fixed-time-zone 0)
                                       :dst-policy :compatible))
           (valid-options (list :range range :time-context context))
           (unsupported-option
             (clocktable-test-capture-condition
              (lambda ()
                (apply #'clocktable-data document heading
                       (append valid-options '(:scope :file))))))
           (non-keyword-option
             (clocktable-test-capture-condition
              (lambda ()
                (apply #'clocktable-data document heading
                       (append valid-options (list t t))))))
           (duplicate-option
             (clocktable-test-capture-condition
              (lambda ()
                (apply #'clocktable-data document heading
                       (append valid-options (list :range range))))))
           (odd-options
             (clocktable-test-capture-condition
              (lambda ()
                (apply #'clocktable-data document heading
                       (append valid-options '(:match)))))))
      (with-soft-assertions
        (clocktable-test-expect-clock-error unsupported-option
                                            :unsupported-option "SCOPE"
                                            "Unsupported clock option")
        (clocktable-test-expect-clock-error non-keyword-option
                                            :unsupported-option "T"
                                            "Unsupported clock option")
        (clocktable-test-expect-clock-error duplicate-option
                                            :unsupported-option "RANGE"
                                            "Unsupported clock option")
        (clocktable-test-expect-clock-error odd-options
                                            :unsupported-option
                                            "missing option value"
                                            "Unsupported clock option"))))

  (it "rejects invalid argument and constructor values through the public API"
    (let* ((document (parse-org "* Scope"))
           (heading (clocktable-test-heading-path document '("Scope")))
           (paragraph-document (parse-org "* Scope\n\nText"))
           (non-headline
             (clocktable-test-node-of-type paragraph-document :paragraph))
           (foreign-document (parse-org "* Foreign"))
           (foreign-heading
             (clocktable-test-heading-path foreign-document '("Foreign")))
           (from (make-org-time-value "2026-10-12T09:00:00"))
           (to (make-org-time-value "2026-10-12T10:00:00"))
           (range (make-time-range from to))
           (context (make-time-context :reference-time from
                                       :zone (make-fixed-time-zone 0)
                                       :dst-policy :compatible))
           (options (list :range range :time-context context))
           (row (make-clocktable-row :level 1 :title "Scope" :tags nil
                                     :range range))
           (invalid-document
             (clocktable-test-capture-condition
              (lambda ()
                (apply #'clocktable-data "not a document" heading options))))
           (invalid-heading
             (clocktable-test-capture-condition
              (lambda ()
                (apply #'clocktable-data document "not a node" options))))
           (non-headline-error
             (clocktable-test-capture-condition
              (lambda ()
                (apply #'clocktable-data paragraph-document non-headline options))))
           (foreign-heading-error
             (clocktable-test-capture-condition
              (lambda ()
                (apply #'clocktable-data document foreign-heading options))))
           (invalid-range
             (clocktable-test-capture-condition
              (lambda ()
                (clocktable-data document heading :range "not a range"
                                 :time-context context))))
           (invalid-match
             (clocktable-test-capture-condition
              (lambda ()
                (apply #'clocktable-data document heading
                       (append options '(:match 42))))))
           (invalid-depth
             (clocktable-test-capture-condition
              (lambda ()
                (apply #'clocktable-data document heading
                       (append options '(:max-depth -1))))))
           (invalid-context
             (clocktable-test-capture-condition
              (lambda ()
                (clocktable-data document heading :range range
                                 :time-context nil))))
           (invalid-row-level
             (clocktable-test-capture-condition
              (lambda ()
                (make-clocktable-row :level 0 :title "Scope"
                                     :tags nil :range range))))
           (invalid-row-title
             (clocktable-test-capture-condition
              (lambda ()
                (make-clocktable-row :level 1 :title 42
                                     :tags nil :range range))))
           (invalid-row-tags
             (clocktable-test-capture-condition
              (lambda ()
                (make-clocktable-row :level 1 :title "Scope"
                                     :tags '("tag" 42) :range range))))
           (improper-row-tags
             (clocktable-test-capture-condition
              (lambda ()
                (make-clocktable-row :level 1 :title "Scope"
                                     :tags (cons "tag" :improper)
                                     :range range))))
           (invalid-row-range
             (clocktable-test-capture-condition
              (lambda ()
                (make-clocktable-row :level 1 :title "Scope"
                                     :tags nil :range nil))))
           (invalid-row-minutes
             (clocktable-test-capture-condition
              (lambda ()
                (make-clocktable-row :level 1 :title "Scope"
                                     :tags nil :range range :minutes -1))))
           (invalid-result-minutes
             (clocktable-test-capture-condition
              (lambda ()
                (make-clocktable-result :total-minutes -1 :rows nil
                                        :range range))))
           (invalid-result-rows
             (clocktable-test-capture-condition
              (lambda ()
                (make-clocktable-result :total-minutes 0
                                        :rows (list :not-a-row) :range range))))
           (improper-result-rows
             (clocktable-test-capture-condition
              (lambda ()
                (make-clocktable-result :total-minutes 0
                                        :rows (cons row :improper)
                                        :range range))))
           (invalid-result-range
             (clocktable-test-capture-condition
              (lambda ()
                (make-clocktable-result :total-minutes 0 :rows nil
                                        :range nil)))))
      (with-soft-assertions
        (clocktable-test-expect-type-error invalid-document "not a document"
                                           'document)
        (clocktable-test-expect-type-error invalid-heading "not a node" 'node)
        (clocktable-test-expect-type-error non-headline-error non-headline 'node)
        (expect (typep foreign-heading-error 'org-kit-error) :to-be t)
        (when (typep foreign-heading-error 'org-kit-error)
          (expect (org-kit-error-message foreign-heading-error)
                  :to-equal "Heading does not belong to DOCUMENT"))
        (clocktable-test-expect-type-error invalid-range "not a range"
                                           'org-time-range)
        (clocktable-test-expect-type-error invalid-match 42 '(or string null))
        (clocktable-test-expect-type-error invalid-depth -1 'unsigned-byte)
        (clocktable-test-expect-type-error invalid-context nil
                                           'org-time-context)
        (clocktable-test-expect-type-error invalid-row-level 0 '(integer 1))
        (clocktable-test-expect-type-error invalid-row-title 42 'string)
        (clocktable-test-expect-type-error invalid-row-tags '("tag" 42) 'list)
        (clocktable-test-expect-type-error improper-row-tags
                                           (cons "tag" :improper) 'list)
        (clocktable-test-expect-type-error invalid-row-range nil 'org-time-range)
        (clocktable-test-expect-type-error invalid-row-minutes -1 'unsigned-byte)
        (clocktable-test-expect-type-error invalid-result-minutes -1
                                           'unsigned-byte)
        (clocktable-test-expect-type-error invalid-result-rows
                                           '(:not-a-row) 'list)
        (clocktable-test-expect-type-error improper-result-rows
                                           (cons row :improper) 'list)
        (clocktable-test-expect-type-error invalid-result-range nil
                                           'org-time-range))))

  (it "reports malformed match and parsed CLOCK conditions with their details"
    (let* ((document (parse-org "* Scope"))
           (heading (clocktable-test-heading-path document '("Scope")))
           (from (make-org-time-value "2026-10-12T09:00:00"))
           (to (make-org-time-value "2026-10-12T10:00:00"))
           (range (make-time-range from to))
           (context (make-time-context :reference-time from
                                       :zone (make-fixed-time-zone 0)
                                       :dst-policy :compatible))
           (match-query "+")
           (match-error
             (clocktable-test-capture-condition
              (lambda ()
                (clocktable-data document heading :range range :match match-query
                                 :time-context context))))
           (invalid-clock-line "CLOCK: [2026-13-99 Mon 09:00]")
           (clock-document
             (parse-org (format nil "* Scope~%~A" invalid-clock-line)))
           (clock-heading
             (clocktable-test-heading-path clock-document '("Scope")))
           (clock-error
             (clocktable-test-capture-condition
              (lambda ()
                (clocktable-data clock-document clock-heading :range range
                                 :time-context context))))
           (malformed-clock-line "CLOCK: => x:10")
           (malformed-clock-document
             (parse-org (format nil "* Scope~%~A" malformed-clock-line)))
           (malformed-clock-heading
             (clocktable-test-heading-path malformed-clock-document '("Scope")))
           (malformed-clock-error
             (clocktable-test-capture-condition
              (lambda ()
                (clocktable-data malformed-clock-document
                                 malformed-clock-heading :range range
                                 :time-context context)))))
      (with-soft-assertions
        (expect (typep match-error 'org-query-error) :to-be t)
        (when (typep match-error 'org-query-error)
          (expect (org-kit-error-message match-error)
                  :to-equal "Sign must be followed by a term")
          (expect (org-query-error-query match-error) :to-equal match-query)
          (expect (org-query-error-position match-error) :to-be 1))
        (clocktable-test-expect-time-error clock-error :invalid-time
                                           "timestamp has no valid start"
                                           "Invalid date or time")
        (clocktable-test-expect-clock-error malformed-clock-error
                                            :malformed-clock
                                            malformed-clock-line
                                            "Malformed CLOCK entry"))))

  (it "checks the document node limit below, at, and above its boundary"
    (let* ((source (format nil "* ~%CLOCK: => 0:02"))
           (baseline (parse-org source))
           (node-count (clocktable-test-node-count baseline))
           (limits (make-limits :max-node-count node-count))
           (below (parse-org "* " :limits limits))
           (at (parse-org source :limits limits))
           (over (parse-org source :limits limits))
           (from (make-org-time-value "2026-10-12T09:00:00"))
           (to (make-org-time-value "2026-10-12T10:00:00"))
           (range (make-time-range from to))
           (context (make-time-context :reference-time from
                                       :zone (make-fixed-time-zone 0)
                                       :dst-policy :compatible))
           (below-result
             (clocktable-data below
                              (clocktable-test-heading-path below '(""))
                              :range range :time-context context))
           (at-result
             (clocktable-data at
                              (clocktable-test-heading-path at '(""))
                              :range range :time-context context))
           (over-condition
             (progn
               (append-child (document-root over) (make-node :paragraph))
               (clocktable-test-capture-condition
                (lambda ()
                  (clocktable-data
                   over (clocktable-test-heading-path over '(""))
                   :range range :time-context context))))))
      (with-soft-assertions
        (expect (< (clocktable-test-node-count below) node-count) :to-be t)
        (expect (clocktable-test-node-count at) :to-be node-count)
        (expect (clocktable-test-node-count over) :to-be (1+ node-count))
        (expect (clocktable-result-p below-result) :to-be t)
        (expect (clocktable-result-p at-result) :to-be t)
        (expect (typep over-condition 'org-resource-limit-error) :to-be t)
        (when (typep over-condition 'org-resource-limit-error)
          (expect (org-kit-error-message over-condition)
                  :to-equal "Resource limit exceeded")
          (expect (org-resource-limit-error-limit-name over-condition)
                  :to-be :max-node-count)
          (expect (org-resource-limit-error-limit over-condition)
                  :to-be node-count)
          (expect (org-resource-limit-error-actual over-condition)
                  :to-be (1+ node-count)))))))

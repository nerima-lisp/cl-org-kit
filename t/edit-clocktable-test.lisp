;;;; t/edit-clocktable-test.lisp
;;;;
;;;; H2's deterministic clocktable formatter and body-only patch contract.
(in-package #:cl-org-kit/test)

(defun edit-clocktable-test-fixture (tree)
  (read-conformance-tree
   (asdf:system-relative-pathname
    "cl-org-kit"
    (format nil "t/fixtures/behavior/~A/h2-clocktable-write-policy.sexp"
            tree))))

(defun edit-clocktable-test-source-octets (name)
  (read-conformance-octets
   (asdf:system-relative-pathname
    "cl-org-kit"
    (format nil "t/fixtures/behavior/corpus/~A" name))))

(defun edit-clocktable-test-range ()
  (make-time-range (make-org-time-value "2026-01-01")
                   (make-org-time-value "2026-01-02")))

(defun edit-clocktable-test-result (case)
  (let ((range (edit-clocktable-test-range)))
    (make-clocktable-result
     :total-minutes (getf case :total-minutes)
     :rows (mapcar (lambda (row)
                     (make-clocktable-row
                      :level (getf row :level)
                      :title (getf row :title)
                      :tags (getf row :tags)
                      :range range
                      :minutes (getf row :minutes)))
                   (getf case :rows))
     :range range)))

(defun edit-clocktable-test-result-snapshot (result)
  (list (clocktable-result-total-minutes result)
        (clocktable-result-range result)
        (mapcar (lambda (row)
                  (list (clocktable-row-level row)
                        (clocktable-row-title row)
                        (clocktable-row-tags row)
                        (clocktable-row-range row)
                        (clocktable-row-minutes row)))
                (clocktable-result-rows result))))

(defun edit-clocktable-test-dynamic-block (document &optional block-name)
  (let ((found nil))
    (map-nodes
     (lambda (node)
       (when (and (null found)
                  (eq (node-type node) :dynamic-block)
                  (or (null block-name)
                      (string= (node-property node :block-name) block-name)))
         (setf found node)))
     (document-root document))
    found))

(defun edit-clocktable-test-node-count (document)
  (let ((count 0))
    (map-nodes (lambda (node)
                 (declare (ignore node))
                 (incf count))
               (document-root document))
    count))

(defun edit-clocktable-test-write (source-octets case &optional limits)
  (let* ((document (if limits
                       (parse-org source-octets :limits limits)
                       (parse-org source-octets)))
         (result (edit-clocktable-test-result case))
         (target (edit-clocktable-test-dynamic-block document "clocktable")))
    (values (write-clocktable document target result) document result)))

(defun edit-clocktable-test-capture-condition (thunk)
  (handler-case
      (funcall thunk)
    (condition (condition)
      condition)))

(defun edit-clocktable-test-newlines (text newline)
  (with-output-to-string (stream)
    (loop for character across text
          do (if (char= character #\Newline)
                 (write-string newline stream)
                 (write-char character stream)))))

(defun edit-clocktable-test-utf16-octets (text)
  (cl-codec-kit:string-to-octets text :encoding :utf-16))

(defun edit-clocktable-test-case (cases id)
  (find id cases :key (lambda (case) (getf case :id)) :test #'string=))

(defun edit-clocktable-test-expect-limit-error
    (condition limit-name limit actual)
  (expect (typep condition 'org-resource-limit-error) :to-be t)
  (when (typep condition 'org-resource-limit-error)
    (expect (org-kit-error-message condition)
            :to-equal "Resource limit exceeded")
    (expect (org-resource-limit-error-limit-name condition)
            :to-be limit-name)
    (expect (org-resource-limit-error-limit condition) :to-be limit)
    (expect (org-resource-limit-error-actual condition) :to-be actual)))

(defun edit-clocktable-test-dynamic-update
    (source &key document-resolver time-context)
  (let* ((document (parse-org source))
         (target (edit-clocktable-test-dynamic-block document "clocktable"))
         (edit (org-kit::update-dynamic-block
                document target :document-resolver document-resolver
                :time-context time-context)))
    (values (document-text (edit-result-updated-document edit)) edit document)))

(defun edit-clocktable-test-expect-parameter-error
    (condition reason parameter value)
  (expect (typep condition 'org-kit::org-clocktable-parameter-error)
          :to-be t)
  (when (typep condition 'org-kit::org-clocktable-parameter-error)
    (expect (org-kit::org-kit-error-message condition)
            :to-equal (ecase reason
                        (:unsupported-parameter
                         "Unsupported clocktable parameter")
                        (:invalid-value "Invalid clocktable parameter value")))
    (expect (org-kit::org-clocktable-parameter-error-reason condition)
            :to-be reason)
    (expect (org-kit::org-clocktable-parameter-error-parameter condition)
            :to-equal parameter)
    (expect (org-kit::org-clocktable-parameter-error-value condition)
            :to-equal value)))

(defun edit-clocktable-test-dynamic-source-octets (source)
  (cl-codec-kit:string-to-octets source :encoding :utf-8))

(describe "clocktable body edits"
  (it "matches the H2 policy oracle and patches only the dynamic-block body"
    (let* ((cases (getf (edit-clocktable-test-fixture "cases") :cases))
           (expected (getf (edit-clocktable-test-fixture "expected") :cases)))
      (with-soft-assertions
        (expect (length cases) :to-be 10)
        (expect (length expected) :to-be (length cases))
        (dolist (case cases)
          (let* ((id (getf case :id))
                 (oracle (edit-clocktable-test-case expected id))
                 (source-octets
                   (edit-clocktable-test-source-octets (getf case :source)))
                 (source-copy (copy-seq source-octets))
                 (document (parse-org source-octets))
                 (source (document-text document))
                 (target (edit-clocktable-test-dynamic-block
                          document "clocktable"))
                 (result (edit-clocktable-test-result case))
                 (result-before (edit-clocktable-test-result-snapshot result))
                 (edit (write-clocktable document target result))
                 (updated (edit-result-updated-document edit))
                 (updated-source (document-text updated))
                 (body (getf oracle :org-clocktable-body))
                 (span (first (edit-result-spans edit)))
                 (start (edit-span-start span))
                 (end (edit-span-end span))
                 (body-end (+ start (length body))))
            (expect (list id (not (null oracle))) :to-equal (list id t))
            (expect (list id (node-p target)) :to-equal (list id t))
            (expect (list id (edit-result-p edit)) :to-equal (list id t))
            (expect (list id updated-source)
                    :to-equal (list id (getf oracle :org-updated-document)))
            (expect (list id (length (edit-result-spans edit)))
                    :to-equal (list id 1))
            (expect (list id (<= body-end (length updated-source)))
                    :to-equal (list id t))
            (when (<= body-end (length updated-source))
              (expect (subseq updated-source start body-end)
                      :to-equal body))
            (expect (subseq source 0 start)
                    :to-equal (subseq updated-source 0 start))
            (when (<= body-end (length updated-source))
              (expect (subseq source end)
                      :to-equal (subseq updated-source body-end)))
            (expect (document-text document) :to-equal source)
            (expect (write-org document :output-type :octets)
                    :to-equalp source-copy)
            (expect source-octets :to-equalp source-copy)
            (expect (edit-clocktable-test-result-snapshot result)
                    :to-equal result-before)
            (expect (write-org updated :output-type :octets)
                    :to-equalp
                    (cl-codec-kit:string-to-octets
                     (getf oracle :org-updated-document) :encoding :utf-8)))))))

  (it "preserves CRLF and reversible UTF-16 bytes outside the patched body"
    (let* ((cases (getf (edit-clocktable-test-fixture "cases") :cases))
           (expected (getf (edit-clocktable-test-fixture "expected") :cases))
           (case (edit-clocktable-test-case
                  cases "nested-levels-default-columns"))
           (oracle (edit-clocktable-test-case
                    expected "nested-levels-default-columns"))
           (base-source-octets
             (edit-clocktable-test-source-octets (getf case :source)))
           (base-source (document-text (parse-org base-source-octets)))
           (source (edit-clocktable-test-newlines base-source (format nil "~C~C" #\Return #\Newline)))
           (source-octets (edit-clocktable-test-utf16-octets source))
           (document (parse-org source-octets :encoding :utf-16))
           (target (edit-clocktable-test-dynamic-block document "clocktable"))
           (result (edit-clocktable-test-result case))
           (edit (write-clocktable document target result))
           (updated (edit-result-updated-document edit))
           (updated-source (document-text updated))
           (expected-source
             (edit-clocktable-test-newlines
              (getf oracle :org-updated-document)
              (format nil "~C~C" #\Return #\Newline)))
           (body (edit-clocktable-test-newlines
                  (getf oracle :org-clocktable-body)
                  (format nil "~C~C" #\Return #\Newline)))
           (span (first (edit-result-spans edit)))
           (start (edit-span-start span))
           (end (edit-span-end span))
           (output-octets (write-org updated :output-type :octets))
           (expected-octets (edit-clocktable-test-utf16-octets expected-source)))
      (with-soft-assertions
        (expect (document-encoding document) :to-be :utf-16)
        (expect (document-bom-p document) :to-be t)
        (expect (write-org document :output-type :octets)
                :to-equalp source-octets)
        (expect updated-source :to-equal expected-source)
        (expect (subseq updated-source start (+ start (length body)))
                :to-equal body)
        (expect output-octets :to-equalp expected-octets)
        (expect (subseq output-octets 0 (+ 2 (* 2 start)))
                :to-equalp (subseq source-octets 0 (+ 2 (* 2 start))))
        (expect (subseq output-octets (+ 2 (* 2 start) (* 2 (length body))))
                :to-equalp (subseq source-octets (+ 2 (* 2 end))))
        (expect (document-text document) :to-equal source))))

  (it "accepts exact output character and octet budgets and rejects one below"
    (let* ((cases (getf (edit-clocktable-test-fixture "cases") :cases))
           (expected (getf (edit-clocktable-test-fixture "expected") :cases))
           (case (edit-clocktable-test-case cases "nested-levels-default-columns"))
           (oracle (edit-clocktable-test-case
                    expected "nested-levels-default-columns"))
           (source-octets
             (edit-clocktable-test-source-octets (getf case :source)))
           (expected-source (getf oracle :org-updated-document))
           (output-length (length expected-source))
           (output-octets
             (length (cl-codec-kit:string-to-octets
                      expected-source :encoding :utf-8))))
      (with-soft-assertions
        (multiple-value-bind (edit document)
            (edit-clocktable-test-write
             source-octets case (make-limits :max-output-length output-length))
          (expect (document-text (edit-result-updated-document edit))
                  :to-equal expected-source)
          (expect (write-org document :output-type :octets)
                  :to-equalp source-octets))
        (let* ((document
                 (parse-org source-octets
                            :limits (make-limits
                                     :max-output-length (1- output-length))))
               (result (edit-clocktable-test-result case))
               (target (edit-clocktable-test-dynamic-block document "clocktable"))
               (condition
                 (edit-clocktable-test-capture-condition
                  (lambda () (write-clocktable document target result)))))
          (edit-clocktable-test-expect-limit-error
           condition :max-output-length (1- output-length) output-length)
          (expect (write-org document :output-type :octets)
                  :to-equalp source-octets))
        (multiple-value-bind (edit document)
            (edit-clocktable-test-write
             source-octets case (make-limits :max-output-octets output-octets))
          (expect (document-text (edit-result-updated-document edit))
                  :to-equal expected-source)
          (expect (write-org document :output-type :octets)
                  :to-equalp source-octets))
        (let* ((document
                 (parse-org source-octets
                            :limits (make-limits
                                     :max-output-octets (1- output-octets))))
               (result (edit-clocktable-test-result case))
               (target (edit-clocktable-test-dynamic-block document "clocktable"))
               (condition
                 (edit-clocktable-test-capture-condition
                  (lambda () (write-clocktable document target result)))))
          (edit-clocktable-test-expect-limit-error
           condition :max-output-octets (1- output-octets) output-octets)
          (expect (write-org document :output-type :octets)
                  :to-equalp source-octets)))))

  (it "preflights generated table nodes against the document node budget"
    (let* ((cases (getf (edit-clocktable-test-fixture "cases") :cases))
           (case (edit-clocktable-test-case cases "empty-default-table"))
           (source-octets
             (edit-clocktable-test-source-octets (getf case :source)))
           (baseline (parse-org source-octets))
           (baseline-node-count (edit-clocktable-test-node-count baseline))
           (minimum-generated-node-count 8)
           (limit (+ baseline-node-count
                     (1- minimum-generated-node-count)))
           (document (parse-org source-octets
                                :limits (make-limits :max-node-count limit)))
           (source (document-text document))
           (target (edit-clocktable-test-dynamic-block document "clocktable"))
           (result (edit-clocktable-test-result case))
           (condition
             (edit-clocktable-test-capture-condition
              (lambda () (write-clocktable document target result)))))
      (with-soft-assertions
        (edit-clocktable-test-expect-limit-error
         condition :max-node-count limit
         (+ baseline-node-count minimum-generated-node-count))
        (expect (document-text document) :to-equal source)
        (expect (write-org document :output-type :octets)
                :to-equalp source-octets))))

  (it "matches the full Org body for unencoded, padded, wide and sparse rows"
    (let* ((cases (getf (edit-clocktable-test-fixture "cases") :cases))
           (expected (getf (edit-clocktable-test-fixture "expected") :cases))
           (case (edit-clocktable-test-case cases "unencoded-padded-wide-sparse"))
           (oracle (edit-clocktable-test-case
                    expected "unencoded-padded-wide-sparse"))
           (source-octets
             (edit-clocktable-test-source-octets (getf case :source)))
           (source (cl-codec-kit:octets-to-string
                    source-octets :encoding :utf-8))
           (document (parse-org source))
           (target (edit-clocktable-test-dynamic-block document "clocktable"))
           (result (edit-clocktable-test-result case))
           (edit (write-clocktable document target result))
           (updated-source
             (document-text (edit-result-updated-document edit)))
           (body (getf oracle :org-clocktable-body))
           (span (first (edit-result-spans edit)))
           (start (edit-span-start span)))
      (with-soft-assertions
        (expect (document-encoding document) :to-be nil)
        (expect updated-source :to-equal (getf oracle :org-updated-document))
        (expect (subseq updated-source start (+ start (length body)))
                :to-equal body))))

  (it "matches Org's alignment cookies and indented clocktable-body oracle"
    (let* ((cases (getf (edit-clocktable-test-fixture "cases") :cases))
           (expected (getf (edit-clocktable-test-fixture "expected") :cases)))
      (dolist (id '("alignment-right-cookie-headline"
                    "width-cookie-headline"
                    "indented-clocktable-body"))
        (let* ((case (edit-clocktable-test-case cases id))
               (oracle (edit-clocktable-test-case expected id))
               (source-octets
                 (edit-clocktable-test-source-octets (getf case :source)))
               (document (parse-org source-octets))
               (source (document-text document))
               (target
                 (edit-clocktable-test-dynamic-block document "clocktable"))
               (result (edit-clocktable-test-result case))
               (edit (write-clocktable document target result))
               (updated-source
                 (document-text (edit-result-updated-document edit)))
               (body (getf oracle :org-clocktable-body))
               (start (edit-span-start (first (edit-result-spans edit)))))
          (with-soft-assertions
            (expect (list id updated-source)
                    :to-equal (list id (getf oracle :org-updated-document)))
            (expect (subseq updated-source start (+ start (length body)))
                    :to-equal body)
            (expect (document-text document) :to-equal source))))))

  (it "rejects clock cells and row indentation above their output budgets"
    (let* ((cases (getf (edit-clocktable-test-fixture "cases") :cases))
           (expected (getf (edit-clocktable-test-fixture "expected") :cases))
           (time-case (edit-clocktable-test-case cases "time-cell-output-limit"))
           (time-oracle
             (edit-clocktable-test-case expected "time-cell-output-limit"))
           (time-body (getf time-oracle :org-clocktable-body))
           (time-cell "*41d 16:00*")
           (time-start (search time-cell time-body))
           (time-limit (1- (length time-cell)))
           (time-source-octets
             (edit-clocktable-test-source-octets (getf time-case :source)))
           (time-document
             (parse-org time-source-octets
                        :limits (make-limits :max-output-length time-limit)))
           (time-target
             (edit-clocktable-test-dynamic-block time-document "clocktable"))
           (time-result (edit-clocktable-test-result time-case))
           (time-source (document-text time-document))
           (time-condition
             (edit-clocktable-test-capture-condition
              (lambda ()
                (write-clocktable time-document time-target time-result))))
           (indent-case (edit-clocktable-test-case cases "indent-output-limit"))
           (indent-oracle
             (edit-clocktable-test-case expected "indent-output-limit"))
           (indent-body (getf indent-oracle :org-clocktable-body))
           (indent-text "\\_      ")
           (indent-start (search indent-text indent-body))
           (total-cell "*0:01*")
           (total-start (search total-cell indent-body))
           (indent-limit (1- (length indent-text)))
           (indent-source-octets
             (edit-clocktable-test-source-octets (getf indent-case :source)))
           (indent-document
             (parse-org indent-source-octets
                        :limits (make-limits
                                 :max-output-length indent-limit)))
           (indent-target
             (edit-clocktable-test-dynamic-block
              indent-document "clocktable"))
           (indent-result (edit-clocktable-test-result indent-case))
           (indent-source (document-text indent-document))
           (indent-condition
             (edit-clocktable-test-capture-condition
              (lambda ()
                (write-clocktable
                 indent-document indent-target indent-result)))))
      (with-soft-assertions
        (expect time-start :to-be-truthy)
        (expect (and time-start
                     (subseq time-body time-start
                             (+ time-start (length time-cell))))
                :to-equal time-cell)
        (edit-clocktable-test-expect-limit-error
         time-condition :max-output-length time-limit (length time-cell))
        (expect (document-text time-document) :to-equal time-source)
        (expect (write-org time-document :output-type :octets)
                :to-equalp time-source-octets)
        (expect indent-start :to-be-truthy)
        (expect total-start :to-be-truthy)
        (expect (<= (length total-cell) indent-limit) :to-be t)
        (edit-clocktable-test-expect-limit-error
         indent-condition :max-output-length indent-limit
         (length indent-text))
        (expect (document-text indent-document) :to-equal indent-source)
        (expect (write-org indent-document :output-type :octets)
                :to-equalp indent-source-octets))))

  (it "preserves a UTF-8 BOM through a clocktable body edit"
    (let* ((source (format nil "#+BEGIN: clocktable~%stale~%#+END:~%"))
           (source-octets
             (concatenate '(vector (unsigned-byte 8))
                          #(239 187 191)
                          (cl-codec-kit:string-to-octets
                           source :encoding :utf-8)))
           (document (parse-org source-octets))
           (target (edit-clocktable-test-dynamic-block document "clocktable"))
           (case
             (edit-clocktable-test-case
              (getf (edit-clocktable-test-fixture "cases") :cases)
              "empty-default-table"))
           (result (edit-clocktable-test-result case))
           (edit (write-clocktable document target result))
           (output-octets
             (write-org (edit-result-updated-document edit)
                        :output-type :octets)))
      (with-soft-assertions
        (expect (document-encoding document) :to-be :utf-8)
        (expect (document-bom-p document) :to-be t)
        (expect (subseq output-octets 0 3) :to-equalp #(239 187 191))
        (expect (write-org document :output-type :octets)
                :to-equalp source-octets))))

  (it "signals bounded edit errors for a missing or non-clocktable target"
    (let* ((cases (getf (edit-clocktable-test-fixture "cases") :cases))
           (case (edit-clocktable-test-case
                  cases "empty-default-table"))
           (source-octets
             (edit-clocktable-test-source-octets (getf case :source)))
           (document (parse-org source-octets))
           (source (document-text document))
           (result (edit-clocktable-test-result case))
           (missing
             (edit-clocktable-test-capture-condition
              (lambda () (write-clocktable document nil result)))))
      (with-soft-assertions
        (expect (typep missing 'org-edit-error) :to-be t)
        (when (typep missing 'org-edit-error)
          (expect (org-kit-error-message missing)
                  :to-equal "Edit target was not found")
          (expect (org-edit-error-target missing) :to-equal "target")
          (expect (org-edit-error-reason missing) :to-be :target-not-found))
        (expect (document-text document) :to-equal source)
        (expect (write-org document :output-type :octets)
                :to-equalp source-octets))
      (let* ((wrong-source (format nil "#+BEGIN: example~%stale~%#+END:~%"))
             (wrong-document (parse-org wrong-source))
             (wrong-target
               (edit-clocktable-test-dynamic-block wrong-document "example"))
             (wrong-octets (write-org wrong-document :output-type :octets))
             (wrong-condition
               (edit-clocktable-test-capture-condition
                (lambda ()
                  (write-clocktable wrong-document wrong-target result)))))
        (with-soft-assertions
          (expect (typep wrong-condition 'org-edit-error) :to-be t)
          (when (typep wrong-condition 'org-edit-error)
            (expect (org-kit-error-message wrong-condition)
                    :to-equal "Dynamic block is not a clocktable")
            (expect (org-edit-error-target wrong-condition)
                    :to-equal "dynamic-block")
            (expect (org-edit-error-reason wrong-condition)
                    :to-be :wrong-node-type))
          (expect (document-text wrong-document) :to-equal wrong-source)
          (expect (write-org wrong-document :output-type :octets)
                  :to-equalp wrong-octets)))))

  (it "rejects invalid results and multiline clocktable titles"
    (let* ((cases (getf (edit-clocktable-test-fixture "cases") :cases))
           (case (edit-clocktable-test-case cases "empty-default-table"))
           (source-octets
             (edit-clocktable-test-source-octets (getf case :source)))
           (document (parse-org source-octets))
           (source (document-text document))
           (target (edit-clocktable-test-dynamic-block document "clocktable"))
           (invalid-result
             (edit-clocktable-test-capture-condition
              (lambda () (write-clocktable document target nil)))))
      (with-soft-assertions
        (expect (typep invalid-result 'type-error) :to-be t)
        (when (typep invalid-result 'type-error)
          (expect (type-error-datum invalid-result) :to-be nil)
          (expect (type-error-expected-type invalid-result)
                  :to-equal 'clocktable-result))
        (expect (document-text document) :to-equal source)
        (expect (write-org document :output-type :octets)
                :to-equalp source-octets))
      (let* ((range (edit-clocktable-test-range))
             (result
               (make-clocktable-result
                :total-minutes 1 :range range
                :rows (list (make-clocktable-row
                             :level 1 :title (format nil "bad~%title")
                             :range range :minutes 1))))
             (condition
               (edit-clocktable-test-capture-condition
                (lambda () (write-clocktable document target result)))))
        (with-soft-assertions
          (expect (typep condition 'org-edit-error) :to-be t)
          (when (typep condition 'org-edit-error)
            (expect (org-kit-error-message condition)
                    :to-equal "Clocktable headline contains a line break")
            (expect (org-edit-error-target condition)
                    :to-equal "dynamic-block")
            (expect (org-edit-error-reason condition) :to-be :invalid-value))
          (expect (document-text document) :to-equal source)
          (expect (write-org document :output-type :octets)
                  :to-equalp source-octets)))))

  (it "rejects clocktable blocks without a complete closing line"
    (let* ((cases (getf (edit-clocktable-test-fixture "cases") :cases))
           (case (edit-clocktable-test-case cases "empty-default-table"))
           (result (edit-clocktable-test-result case)))
      (dolist (source (list "#+BEGIN: clocktable"
                            (format nil "#+BEGIN: clocktable~%")))
        (let* ((source-octets
                 (cl-codec-kit:string-to-octets source :encoding :utf-8))
               (document (parse-org source-octets))
               (target (edit-clocktable-test-dynamic-block document "clocktable"))
               (condition
                 (edit-clocktable-test-capture-condition
                  (lambda () (write-clocktable document target result)))))
          (with-soft-assertions
            (expect (node-p target) :to-be t)
            (expect (typep condition 'org-edit-error) :to-be t)
            (when (typep condition 'org-edit-error)
              (expect (org-edit-error-target condition)
                      :to-equal "dynamic-block")
              (expect (org-edit-error-reason condition)
                      :to-be :uneditable-structure))
            (expect (document-text document) :to-equal source)
            (expect (write-org document :output-type :octets)
                    :to-equalp source-octets)))))))

(describe "clocktable dynamic block parameters"
  (it "uses scope, block, maxlevel, tags, and emphasis with exact output"
    (let* ((source
             (format nil
                     "* Parent :parent:~%CLOCK: [2026-10-12 Mon 09:00]--[2026-10-12 Mon 09:15] => 0:15~%#+BEGIN: clocktable :scope subtree :block 2026-10-12 :maxlevel 2 :tags t :emphasize t~%stale~%#+END:~%** Child :child:~%CLOCK: [2026-10-12 Mon 09:15]--[2026-10-12 Mon 09:30] => 0:15~%*** Deep :deep:~%CLOCK: [2026-10-12 Mon 09:30]--[2026-10-12 Mon 10:00] => 0:30~%"))
           (expected
             (format nil
                     "* Parent :parent:~%CLOCK: [2026-10-12 Mon 09:00]--[2026-10-12 Mon 09:15] => 0:15~%#+BEGIN: clocktable :scope subtree :block 2026-10-12 :maxlevel 2 :tags t :emphasize t~%| Tags          | Headline     | Time   |        |~%|---------------+--------------+--------+--------|~%|               | *Total time* | *1:00* |        |~%|---------------+--------------+--------+--------|~%| parent        | *Parent*     | *1:00* |        |~%| parent, child | \\_  /Child/  |        | /0:45/ |~%#+END:~%** Child :child:~%CLOCK: [2026-10-12 Mon 09:15]--[2026-10-12 Mon 09:30] => 0:15~%*** Deep :deep:~%CLOCK: [2026-10-12 Mon 09:30]--[2026-10-12 Mon 10:00] => 0:30~%")))
      (multiple-value-bind (updated edit document)
          (edit-clocktable-test-dynamic-update source)
        (with-soft-assertions
          (expect (edit-result-p edit) :to-be t)
          (expect updated :to-equal expected)
          (expect (document-text document) :to-equal source)))))

  (it "uses a block-only date range with exact output"
    (let* ((source
             (format nil
                     "* Kept~%CLOCK: [2026-10-12 Mon 09:00]--[2026-10-12 Mon 10:00] => 1:00~%* Excluded~%CLOCK: [2026-10-13 Tue 09:00]--[2026-10-13 Tue 10:00] => 1:00~%#+BEGIN: clocktable :block 2026-10-12~%stale~%#+END:~%"))
           (expected
             (format nil
                     "* Kept~%CLOCK: [2026-10-12 Mon 09:00]--[2026-10-12 Mon 10:00] => 1:00~%* Excluded~%CLOCK: [2026-10-13 Tue 09:00]--[2026-10-13 Tue 10:00] => 1:00~%#+BEGIN: clocktable :block 2026-10-12~%| Headline     | Time   |~%|--------------+--------|~%| *Total time* | *1:00* |~%|--------------+--------|~%| Kept         | 1:00   |~%#+END:~%"))
           (source-octets (edit-clocktable-test-dynamic-source-octets source))
           (document (parse-org source-octets))
           (block (edit-clocktable-test-dynamic-block document "clocktable"))
           (edit (org-kit::update-dynamic-block document block))
           (updated (edit-result-updated-document edit)))
      (with-soft-assertions
        (expect (document-text updated) :to-equal expected)
        (expect (document-text document) :to-equal source)
        (expect (write-org document :output-type :octets)
                :to-equalp source-octets))))

  (it "applies explicit start and end dates, step buckets, and match filtering"
    (let* ((source
             (format nil
                     "* Kept :keep:~%CLOCK: [2026-10-12 Mon 09:00]--[2026-10-12 Mon 10:00] => 1:00~%CLOCK: [2026-10-13 Tue 09:00]--[2026-10-13 Tue 10:00] => 1:00~%* Skipped :skip:~%CLOCK: [2026-10-12 Mon 10:00]--[2026-10-12 Mon 11:00] => 1:00~%#+BEGIN: clocktable :scope file :tstart \"[2026-10-12 09:30]\" :tend 2026-10-14 :step day :match \"+keep\"~%stale~%#+END:~%"))
           (source-octets (edit-clocktable-test-dynamic-source-octets source))
           (document (parse-org source-octets))
           (block (edit-clocktable-test-dynamic-block document "clocktable"))
           (edit (org-kit::update-dynamic-block document block))
           (updated (document-text (edit-result-updated-document edit))))
      (with-soft-assertions
        (expect (search "Daily report: [2026-10-12 Mon]" updated)
                :to-be-truthy)
        (expect (search "Daily report: [2026-10-13 Tue]" updated)
                :to-be-truthy)
        (expect (search "| Kept" updated) :to-be-truthy)
        (expect (search "0:30" updated) :to-be-truthy)
        (expect (search "1:00" updated) :to-be-truthy)
        (expect (search "| Skipped" updated) :to-be nil)
        (expect (document-text document) :to-equal source)
        (expect (write-org document :output-type :octets)
                :to-equalp source-octets))))

  (it "emits Org inline percentages for the percent formula"
    (let* ((source
             (format nil
                     "* Parent :parent:~%CLOCK: [2026-10-12 Mon 09:00]--[2026-10-12 Mon 09:15] => 0:15~%#+BEGIN: clocktable :scope subtree :block 2026-10-12 :formula %~%stale~%#+END:~%** Child :child:~%CLOCK: [2026-10-12 Mon 09:15]--[2026-10-12 Mon 09:30] => 0:15~%*** Deep :deep:~%CLOCK: [2026-10-12 Mon 09:30]--[2026-10-12 Mon 10:00] => 0:30~%"))
           (document (parse-org source))
           (block (edit-clocktable-test-dynamic-block document "clocktable"))
           (edit (org-kit::update-dynamic-block document block))
           (updated-document (edit-result-updated-document edit))
           (table (org-kit::edit-clocktable-find-table updated-document))
           (updated (document-text updated-document)))
      (with-soft-assertions
        (expect (search "%" (node-text table)) :to-be-truthy)
        (expect (search "100.0" updated) :to-be-truthy)
        (expect (search "75.0" updated) :to-be-truthy)
        (expect (search "50.0" updated) :to-be-truthy)
        (expect (search "#+TBLFM:" updated) :to-be nil)
        (expect (document-text document) :to-equal source))))

  (it "labels each step with the beginning date of its reporting period"
    (let* ((range (make-time-range
                   (make-org-time-value "2026-10-13T09:30:00")
                   (make-org-time-value "2026-10-14T00:00:00")))
           (steps '("day" "week" "semimonth" "month" "quarter" "year")))
      (expect (mapcar (lambda (step)
                        (org-kit::clocktable-step-name range step))
                      steps)
              :to-equal
              '("Daily report: [2026-10-13 Tue]"
                "Weekly report starting on: [2026-10-12 Mon]"
                "Semimonthly report starting on: [2026-10-01 Thu]"
                "Monthly report starting on: [2026-10-01 Thu]"
                "Quarterly report starting on: [2026-10-01 Thu]"
                "Annual report starting on: [2026-01-01 Thu]"))))

  (it "uses only the supplied resolver and skips zero-contribution files"
    (let* ((source
             (format nil "#+BEGIN: clocktable :scope (keep empty) :block 2026-10-12 :fileskip0 t~%stale~%#+END:~%"))
           (target-document (parse-org source))
           (target (edit-clocktable-test-dynamic-block
                    target-document "clocktable"))
           (kept-document
             (parse-org
              (format nil
                      "* Tracked :keep:~%CLOCK: [2026-10-12 Mon 09:00]--[2026-10-12 Mon 09:30] => 0:30~%")
              :source-name "keep.org"))
           (empty-document (parse-org "* Empty" :source-name "empty.org"))
           (selectors nil)
           (resolver (lambda (selector)
                       (push selector selectors)
                       (cond ((string= selector "keep") kept-document)
                             ((string= selector "empty") empty-document))))
           (source-octets (write-org target-document :output-type :octets))
           (edit (org-kit::update-dynamic-block
                  target-document target :document-resolver resolver))
           (updated (document-text (edit-result-updated-document edit))))
      (with-soft-assertions
        (expect (nreverse selectors) :to-equal '("keep" "empty"))
        (expect (search "File: keep.org" updated) :to-be-truthy)
        (expect (search "File: empty.org" updated) :to-be nil)
        (expect (search "*0:30*" updated) :to-be-truthy)
        (expect (document-text target-document) :to-equal source)
        (expect (write-org target-document :output-type :octets)
                :to-equalp source-octets))))

  (it "reports exact unsupported, invalid, and missing-time-context reasons"
    (dolist (case '((":unknown 3" :unsupported-parameter "unknown" "3")
                    (":maxlevel -1" :invalid-value "maxlevel" "-1")
                    (":step day" :invalid-value "step"
                     "requires :block or :tstart and :tend")
                    (":block today" :invalid-value "block"
                     "relative block requires :time-context")))
      (destructuring-bind (arguments reason parameter value) case
        (let* ((source (format nil "#+BEGIN: clocktable ~A~%stale~%#+END:~%"
                               arguments))
               (document (parse-org source))
               (target (edit-clocktable-test-dynamic-block
                        document "clocktable"))
               (source-octets (write-org document :output-type :octets))
               (condition
                 (edit-clocktable-test-capture-condition
                  (lambda () (org-kit::update-dynamic-block document target)))))
          (edit-clocktable-test-expect-parameter-error
           condition reason parameter value)
          (expect (document-text document) :to-equal source)
          (expect (write-org document :output-type :octets)
                  :to-equalp source-octets)))))

  (it "names malformed clocktable syntax as an invalid parameter"
    (let* ((source (format nil "#+BEGIN: clocktable :block~%stale~%#+END:~%"))
           (document (parse-org source))
           (target (edit-clocktable-test-dynamic-block document "clocktable"))
           (condition
             (edit-clocktable-test-capture-condition
              (lambda () (org-kit::update-dynamic-block document target)))))
      (with-soft-assertions
        (edit-clocktable-test-expect-parameter-error
         condition :invalid-value "block" ":block")
        (expect (document-text document) :to-equal source))))

  (it "reports the name and value of a duplicate clocktable parameter"
    (let* ((source
             (format nil
                     "#+BEGIN: clocktable :block 2025-01 :block today~%stale~%#+END:~%"))
           (document (parse-org source))
           (target (edit-clocktable-test-dynamic-block document "clocktable"))
           (condition
             (edit-clocktable-test-capture-condition
              (lambda () (org-kit::update-dynamic-block document target)))))
      (with-soft-assertions
        (edit-clocktable-test-expect-parameter-error
         condition :invalid-value "block" "today")
        (expect (document-text document) :to-equal source)))))

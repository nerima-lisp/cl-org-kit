;;;; t/statistics-cookies-test.lisp
(in-package #:cl-org-kit/test)

(defparameter +statistics-cookie-oracle-fixtures+
  '("todo-direct" "todo-recursive" "checkbox-direct" "checkbox-recursive"
    "checkbox-recursive-only" "checkbox-noninherit" "todo-zero" "percent-floor"
    "multiple-cookies" "todo-plus-recursive"
    "checkbox-plus-recursive-local-heading" "checkbox-continuation-cookie"
    "default-checkbox" "todo-item-cookie" "checkbox-plus-inherited-recursion"
    "inline-task-checkbox" "todo-inherited-recursion"
    "no-cookie-item-checkbox"))

(defun statistics-cookies-test-nodes-in-tree (document types)
  (let ((nodes nil))
    (map-nodes (lambda (node)
                 (when (member (node-type node) types :test #'eq)
                   (push node nodes)))
               (document-root document))
    (nreverse nodes)))

(defun statistics-cookies-test-nodes (document type)
  (let ((nodes nil))
    (map-nodes (lambda (node)
                 (when (eq (node-type node) type)
                   (push node nodes)))
               (document-root document))
    (dolist (heading (statistics-cookies-test-nodes-in-tree document
                                                            '(:headline :inlinetask)))
      (dolist (segment (node-property heading :title))
        (when (node-p segment)
          (map-nodes (lambda (node)
                       (when (eq (node-type node) type)
                         (push node nodes)))
                     segment))))
    (nreverse (remove-duplicates nodes :test #'eq))))

(defun statistics-cookies-test-owner-heading (node)
  (loop for parent = (node-parent node) then (node-parent parent)
        while parent
        when (member (node-type parent) '(:headline :inlinetask) :test #'eq)
          return parent))

(defun statistics-cookies-test-heading (document)
  (find-if
   (lambda (heading)
     (some (lambda (cookie)
             (eq (statistics-cookies-test-owner-heading cookie) heading))
           (statistics-cookies-test-nodes document :statistics-cookie)))
   (statistics-cookies-test-nodes document :headline)))

(defun statistics-cookies-test-fixture-path (tree name)
  (asdf:system-relative-pathname
   "cl-org-kit/test"
   (format nil "t/fixtures/statistics-cookies/~A/~A.org" tree name)))

(defun statistics-cookies-test-fixture-text (tree name)
  (uiop:read-file-string (statistics-cookies-test-fixture-path tree name)))

(defun statistics-cookies-test-patches (updates spans)
  (mapcar (lambda (update span)
            (org-kit::edit-make-patch
             (edit-span-start span) (edit-span-end span)
             (statistics-cookie-update-new-value update)))
          updates spans))

(defun statistics-cookies-test-unpatched-ranges-match-p (source output patches)
  (let ((source-cursor 0)
        (output-cursor 0))
    (dolist (patch patches)
      (let* ((start (org-kit::edit-patch-start patch))
             (end (org-kit::edit-patch-end patch))
             (length (- start source-cursor)))
        (unless (string= source output
                         :start1 source-cursor :end1 start
                         :start2 output-cursor :end2 (+ output-cursor length))
          (return-from statistics-cookies-test-unpatched-ranges-match-p nil))
        (incf output-cursor
              (+ length (length (org-kit::edit-patch-replacement patch))))
        (setf source-cursor end)))
    (string= source output
             :start1 source-cursor :end1 (length source)
             :start2 output-cursor :end2 (length output))))

(defun statistics-cookies-test-bom-octets (text)
  (let* ((encoded (cl-codec-kit:string-to-octets text :encoding :utf-8))
         (result (make-array (+ 3 (length encoded))
                             :element-type '(unsigned-byte 8))))
    (replace result #(239 187 191))
    (replace result encoded :start1 3)
    result))

(defun statistics-cookies-test-edit-error (thunk)
  (handler-case
      (progn (funcall thunk) nil)
    (org-edit-error (condition)
      (list (org-edit-error-reason condition)
            (org-edit-error-target condition)))))

(defun statistics-cookies-test-with-traversal-counts (function)
  (let* ((map-symbol 'org-kit:map-nodes)
         (children-symbol 'org-kit:node-children)
         (original-map (symbol-function map-symbol))
         (original-children (symbol-function children-symbol))
         (map-visits 0)
         (children-reads 0))
    (unwind-protect
         (progn
           (setf (symbol-function map-symbol)
                 (lambda (callback node)
                   (incf map-visits)
                   (funcall original-map callback node))
                 (symbol-function children-symbol)
                 (lambda (node)
                   (incf children-reads)
                   (funcall original-children node)))
           (multiple-value-bind (result updates) (funcall function)
             (values result updates map-visits children-reads)))
      (setf (symbol-function map-symbol) original-map
            (symbol-function children-symbol) original-children))))

(describe "statistics cookie updates"
  (it "matches each pinned Org update fixture and preserves unpatched source"
    (dolist (name +statistics-cookie-oracle-fixtures+)
      (let* ((source (statistics-cookies-test-fixture-text "source" name))
             (expected (statistics-cookies-test-fixture-text "expected" name))
             (document (parse-org source))
             (heading (statistics-cookies-test-heading document))
             (source-tree (org-kit::edit-canonical-document document))
             (result-and-updates (multiple-value-list
                                  (update-statistics-cookies document heading)))
             (result (first result-and-updates))
             (updates (second result-and-updates))
             (spans (edit-result-spans result))
             (edited (edit-result-updated-document result))
             (patches (statistics-cookies-test-patches updates spans)))
        (with-soft-assertions
          (expect (document-p edited) :to-be t)
          (expect (document-text edited) :to-equal expected)
          (expect (length spans) :to-equal (length updates))
          (expect (mapcar #'edit-span-start spans)
                  :to-equal (sort (mapcar #'edit-span-start spans) #'<))
          (expect (statistics-cookies-test-unpatched-ranges-match-p
                   source (document-text edited) patches)
                  :to-be t)
          (expect (document-text document) :to-equal source)
          (expect (org-kit::edit-canonical-document document)
                  :to-equalp source-tree)
          (loop for update in updates
                for span in spans
                do (expect (edit-span-start span)
                           :to-be (node-begin (statistics-cookie-update-node update)))
                   (expect (edit-span-end span)
                           :to-be (- (node-end (statistics-cookie-update-node update))
                                     (node-post-blank
                                      (statistics-cookie-update-node update))))
                   (expect (subseq source (edit-span-start span) (edit-span-end span))
                           :to-equal (statistics-cookie-update-old-value update))
                   (expect (statistics-cookie-update-p update) :to-be t)
                   (expect (eq (node-document (statistics-cookie-update-node update))
                               document)
                           :to-be t)
                   (expect (stringp (statistics-cookie-update-old-value update)) :to-be t)
                   (expect (stringp (statistics-cookie-update-new-value update)) :to-be t))))))

  (it "uses tab-separated COOKIE_DATA tokens and defaults for an empty value"
    (let* ((tab-source
             (format nil
                     "* Parent [0/0]~%:PROPERTIES:~%:COOKIE_DATA: todo~Crecursive~%:END:~%** TODO Direct~%*** DONE Nested~%"
                     #\Tab))
           (tab-expected
             (format nil
                     "* Parent [1/2]~%:PROPERTIES:~%:COOKIE_DATA: todo~Crecursive~%:END:~%** TODO Direct~%*** DONE Nested~%"
                     #\Tab))
           (empty-source
             (format nil
                     "* Parent [0/0]~%:PROPERTIES:~%:COOKIE_DATA: ~%:END:~%** TODO Direct~%*** DONE Nested~%"))
           (empty-expected
             (format nil
                     "* Parent [0/1]~%:PROPERTIES:~%:COOKIE_DATA: ~%:END:~%** TODO Direct~%*** DONE Nested~%"))
           (tab-document (parse-org tab-source))
           (empty-document (parse-org empty-source))
           (tab-result
             (update-statistics-cookies
              tab-document (statistics-cookies-test-heading tab-document)))
           (empty-result
             (update-statistics-cookies
              empty-document (statistics-cookies-test-heading empty-document))))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document tab-result))
                :to-equal tab-expected)
        (expect (document-text (edit-result-updated-document empty-result))
                :to-equal empty-expected)
        (expect (document-text tab-document) :to-equal tab-source)
        (expect (document-text empty-document) :to-equal empty-source))))

  (it "defaults to direct TODO statistics, uses COOKIE_DATA for recursion, and ignores the legacy setting"
    (let* ((source (format nil "* Parent [0/0]~%** TODO Direct~%*** DONE Nested~%"))
           (expected-direct
             (format nil "* Parent [0/1]~%** TODO Direct~%*** DONE Nested~%"))
           (recursive-source
             (format nil
                     "* Parent [0/0]~%:PROPERTIES:~%:COOKIE_DATA: todo recursive~%:END:~%** TODO Direct~%*** DONE Nested~%"))
           (expected-recursive
             (format nil
                     "* Parent [1/2]~%:PROPERTIES:~%:COOKIE_DATA: todo recursive~%:END:~%** TODO Direct~%*** DONE Nested~%"))
           (default-document (parse-org source))
           (legacy-enabled-document
             (parse-org source :settings '(:org-hierarchical-todo-statistics t)))
           (legacy-disabled-document
             (parse-org source :settings '(:org-hierarchical-todo-statistics nil)))
           (recursive-document (parse-org recursive-source))
           (direct-results
             (mapcar (lambda (document)
                       (update-statistics-cookies
                        document (statistics-cookies-test-heading document)))
                     (list default-document legacy-enabled-document
                           legacy-disabled-document)))
           (recursive-result
             (update-statistics-cookies
              recursive-document
              (statistics-cookies-test-heading recursive-document))))
      (with-soft-assertions
        (dolist (result direct-results)
          (expect (document-text (edit-result-updated-document result))
                  :to-equal expected-direct))
        (expect (document-text (edit-result-updated-document recursive-result))
                :to-equal expected-recursive)
        (dolist (document (list default-document legacy-enabled-document
                                legacy-disabled-document))
          (expect (document-text document) :to-equal source))
        (expect (document-text recursive-document) :to-equal recursive-source))))

  (it "updates checkbox item cookies when COOKIE_DATA is absent"
    (let* ((source (statistics-cookies-test-fixture-text
                    "source" "no-cookie-item-checkbox"))
           (expected (statistics-cookies-test-fixture-text
                      "expected" "no-cookie-item-checkbox"))
           (document (parse-org source))
           (heading (first (statistics-cookies-test-nodes document :headline)))
           (result-and-updates (multiple-value-list
                                (update-statistics-cookies document heading)))
           (result (first result-and-updates)))
      (with-soft-assertions
        (expect (mapcar #'statistics-cookie-update-new-value
                        (second result-and-updates))
                :to-equal '("[1/1]"))
        (expect (document-text (edit-result-updated-document result))
                :to-equal expected)
        (expect (document-text document) :to-equal source))))

  (it "inherits TODO COOKIE_DATA for child headings but scopes checkbox recursion locally"
    (let* ((todo-source
             (format nil
                     "* Parent~%:PROPERTIES:~%:COOKIE_DATA: todo recursive~%:END:~%** TODO Child [9/9]~%*** DONE Direct~%*** TODO Direct~%**** DONE Nested~%"))
           (todo-document (parse-org todo-source))
           (todo-heading (second (statistics-cookies-test-nodes todo-document :headline)))
           (todo-result (update-statistics-cookies todo-document todo-heading))
           (override-source
             (format nil
                     "* Parent~%:PROPERTIES:~%:COOKIE_DATA: todo recursive~%:END:~%** TODO Child [9/9]~%:PROPERTIES:~%:COOKIE_DATA: todo~%:END:~%*** DONE Direct~%*** TODO Direct~%**** DONE Nested~%"))
           (override-document (parse-org override-source))
           (override-heading
             (second (statistics-cookies-test-nodes override-document :headline)))
           (override-result
             (update-statistics-cookies override-document override-heading))
           (checkbox-source
             (format nil
                     "* Parent~%:PROPERTIES:~%:COOKIE_DATA: checkbox recursive~%:END:~%** Child [9/9]~%- [X] Direct complete~%- [ ] Direct open~%  - [X] Nested complete~%"))
           (checkbox-document (parse-org checkbox-source))
           (checkbox-heading
             (second (statistics-cookies-test-nodes checkbox-document :headline)))
           (checkbox-result
             (update-statistics-cookies checkbox-document checkbox-heading)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document todo-result))
                :to-equal
                (format nil "* Parent~%:PROPERTIES:~%:COOKIE_DATA: todo recursive~%:END:~%** TODO Child [2/3]~%*** DONE Direct~%*** TODO Direct~%**** DONE Nested~%"))
        (expect (document-text (edit-result-updated-document override-result))
                :to-equal
                (format nil "* Parent~%:PROPERTIES:~%:COOKIE_DATA: todo recursive~%:END:~%** TODO Child [1/2]~%:PROPERTIES:~%:COOKIE_DATA: todo~%:END:~%*** DONE Direct~%*** TODO Direct~%**** DONE Nested~%"))
        (expect (document-text (edit-result-updated-document checkbox-result))
                :to-equal
                (format nil "* Parent~%:PROPERTIES:~%:COOKIE_DATA: checkbox recursive~%:END:~%** Child [1/2]~%- [X] Direct complete~%- [ ] Direct open~%  - [X] Nested complete~%")))))

  (it "renders zero-total percentages as zero and floors non-integral percentages"
    (let* ((zero-source (format nil "* Parent [77%]~%"))
           (zero-document (parse-org zero-source))
           (zero-heading (statistics-cookies-test-heading zero-document))
           (zero-result (update-statistics-cookies zero-document zero-heading))
           (zero-fraction-source (format nil "* Parent [3/4]~%"))
           (zero-fraction-document (parse-org zero-fraction-source))
           (zero-fraction-heading
             (statistics-cookies-test-heading zero-fraction-document))
           (zero-fraction-result
             (update-statistics-cookies zero-fraction-document zero-fraction-heading))
           (floor-source (format nil "* Parent [0%]~%** DONE A~%** TODO B~%** TODO C~%"))
           (floor-document (parse-org floor-source))
           (floor-heading (statistics-cookies-test-heading floor-document))
           (floor-result (update-statistics-cookies floor-document floor-heading)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document zero-result))
                :to-equal (format nil "* Parent [0%]~%"))
        (expect (document-text (edit-result-updated-document zero-fraction-result))
                :to-equal (format nil "* Parent [0/0]~%"))
        (expect (document-text (edit-result-updated-document floor-result))
                :to-equal (format nil "* Parent [33%]~%** DONE A~%** TODO B~%** TODO C~%")))))

  (it "updates heading and list cookies in source order without changing UTF-8 bytes elsewhere"
    (let* ((crlf (concatenate 'string (string #\Return) (string #\Newline)))
           (source (concatenate 'string
                                "* Parent 日本語 [0/0]" crlf
                                ":PROPERTIES:" crlf
                                ":COOKIE_DATA: checkbox" crlf
                                ":END:" crlf
                                "- [ ] group [0/0]" crlf
                                "  - [X] checked 日本語" crlf
                                "  - [ ] open" crlf
                                "- [X] peer checkbox" crlf
                                "- plain item without checkbox" crlf
                                "** Child [0/0]" crlf
                                "- [X] child checkbox" crlf
                                "* Peer [8/9]" crlf))
           (expected (concatenate 'string
                                  "* Parent 日本語 [1/2]" crlf
                                  ":PROPERTIES:" crlf
                                  ":COOKIE_DATA: checkbox" crlf
                                  ":END:" crlf
                                  "- [ ] group [1/2]" crlf
                                  "  - [X] checked 日本語" crlf
                                  "  - [ ] open" crlf
                                  "- [X] peer checkbox" crlf
                                  "- plain item without checkbox" crlf
                                  "** Child [0/0]" crlf
                                  "- [X] child checkbox" crlf
                                  "* Peer [8/9]" crlf))
           (document (parse-org (statistics-cookies-test-bom-octets source)
                                :encoding :utf-8))
           (heading (statistics-cookies-test-heading document))
           (result-and-updates (multiple-value-list
                                (update-statistics-cookies document heading)))
           (result (first result-and-updates))
           (updates (second result-and-updates)))
      (with-soft-assertions
        (expect (mapcar #'statistics-cookie-update-old-value updates)
                :to-equal '("[0/0]" "[0/0]"))
        (expect (mapcar #'statistics-cookie-update-new-value updates)
                :to-equal '("[1/2]" "[1/2]"))
        (expect (write-org (edit-result-updated-document result) :output-type :octets)
                :to-equalp (statistics-cookies-test-bom-octets expected))
        (expect (document-text document) :to-equal source))))

  (it "returns an empty result when all cookies already have current values"
    (let* ((source (format nil "* Parent [1/2]~%** DONE Direct~%** TODO Direct~%"))
           (document (parse-org source))
           (heading (statistics-cookies-test-heading document))
           (result-and-updates (multiple-value-list
                                (update-statistics-cookies document heading)))
           (result (first result-and-updates)))
      (with-soft-assertions
        (expect (edit-result-spans result) :to-equal nil)
        (expect (second result-and-updates) :to-equal nil)
        (expect (document-text (edit-result-updated-document result))
                :to-equal source)
        (expect (eq (edit-result-updated-document result) document) :to-be nil)
        (expect (document-text document) :to-equal source))))

  (it "ignores statistics cookies outside the target heading title and body items"
    (let* ((source (format nil "* Parent~%Body [7/8]~%* Child [9/9]~%- [X] child [0/0]~%"))
           (document (parse-org source))
           (heading (first (statistics-cookies-test-nodes document :headline)))
           (result-and-updates (multiple-value-list
                                (update-statistics-cookies document heading)))
           (result (first result-and-updates)))
      (with-soft-assertions
        (expect (edit-result-spans result) :to-equal nil)
        (expect (second result-and-updates) :to-equal nil)
        (expect (document-text (edit-result-updated-document result))
                :to-equal source)
        (expect (document-text document) :to-equal source))))

  (it "counts inline-task checkboxes in the parent scope and updates inline item cookies"
    (let* ((source (format nil
                           "* Parent [0/0]~%- [ ] Parent item [0/0]~%*************** TODO Inline~%- [X] Inline item [9/9]~%*************** END~%"))
           (expected (format nil
                             "* Parent [1/2]~%- [ ] Parent item [0/0]~%*************** TODO Inline~%- [X] Inline item [0/0]~%*************** END~%"))
           (document (parse-org source))
           (heading (first (statistics-cookies-test-nodes document :headline)))
           (result-and-updates (multiple-value-list
                                (update-statistics-cookies document heading)))
           (result (first result-and-updates)))
      (with-soft-assertions
        (expect (mapcar #'statistics-cookie-update-new-value (second result-and-updates))
                :to-equal '("[1/2]" "[0/0]"))
        (expect (document-text (edit-result-updated-document result))
                :to-equal expected)
        (expect (document-text document) :to-equal source))))

  (it "keeps cookie aggregation and canonical indexing linear through the public API"
    (let* ((item-count 2000)
           (source
             (with-output-to-string (stream)
               (write-string "* Parent" stream)
               (terpri stream)
               (dotimes (index item-count)
                 (format stream "- [ ] Item~D [9/9]~%" index))))
           (expected
             (with-output-to-string (stream)
               (write-string "* Parent" stream)
               (terpri stream)
               (dotimes (index item-count)
                 (format stream "- [ ] Item~D [0/0]~%" index))))
           (document (parse-org source))
           (heading (first (statistics-cookies-test-nodes document :headline)))
           (node-count 0))
      (map-nodes (lambda (node) (declare (ignore node)) (incf node-count))
                 (document-root document))
      (multiple-value-bind (result updates map-visits children-reads)
          (statistics-cookies-test-with-traversal-counts
           (lambda () (update-statistics-cookies document heading)))
        (with-soft-assertions
          (expect (length updates) :to-equal item-count)
          (expect (every (lambda (update)
                           (string= (statistics-cookie-update-new-value update)
                                    "[0/0]"))
                         updates)
                  :to-be t)
          (expect (document-text (edit-result-updated-document result))
                  :to-equal expected)
          (expect (<= map-visits (* 5 node-count)) :to-be t)
          (expect (<= children-reads (* 8 node-count)) :to-be t)
          (expect (document-text document) :to-equal source)))))

  (it "updates title cookies alongside other inline title objects"
    (let* ((source (format nil "* *Parent* [9/9]~%** TODO Child~%"))
           (expected (format nil "* *Parent* [0/1]~%** TODO Child~%"))
           (document (parse-org source))
           (heading (first (statistics-cookies-test-nodes document :headline)))
           (result (update-statistics-cookies document heading)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result))
                :to-equal expected)
        (expect (document-text document) :to-equal source))))

  (it "respects exact output limits and signals before returning a partial result"
    (let* ((source (format nil "* Parent [9/9]~%** TODO Child~%"))
           (expected (format nil "* Parent [0/1]~%** TODO Child~%"))
           (exact-document
             (parse-org source
                        :limits (make-limits :max-output-length (length expected))))
           (exact-heading (statistics-cookies-test-heading exact-document))
           (exact-result (update-statistics-cookies exact-document exact-heading))
           (limited-document
             (parse-org source
                        :limits (make-limits :max-output-length (1- (length expected)))))
           (limited-heading (statistics-cookies-test-heading limited-document))
           (failure (handler-case
                        (progn (update-statistics-cookies limited-document limited-heading)
                               nil)
                      (org-resource-limit-error (condition)
                        (list (org-resource-limit-error-limit-name condition)
                              (org-resource-limit-error-limit condition)
                              (org-resource-limit-error-actual condition))))))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document exact-result))
                :to-equal expected)
        (expect failure :to-equal (list :max-output-length
                                        (1- (length expected))
                                        (length expected)))
        (expect (document-text limited-document) :to-equal source))))

  (it "reports invalid, foreign, and wrong-type heading targets"
    (let* ((source (format nil "* Parent [0/0]~%"))
           (document (parse-org source))
           (heading (statistics-cookies-test-heading document))
           (foreign-document (parse-org source))
           (foreign-heading (statistics-cookies-test-heading foreign-document))
           (item-document (parse-org (format nil "* Parent~%- [ ] item~%")))
           (item (first (statistics-cookies-test-nodes item-document :item))))
      (with-soft-assertions
        (expect (statistics-cookies-test-edit-error
                 (lambda () (update-statistics-cookies document nil)))
                :to-equal '(:target-not-found "target"))
        (expect (statistics-cookies-test-edit-error
                 (lambda () (update-statistics-cookies document foreign-heading)))
                :to-equal '(:foreign-node "headline"))
        (expect (statistics-cookies-test-edit-error
                 (lambda () (update-statistics-cookies item-document item)))
                :to-equal '(:wrong-node-type "item"))
        (expect (document-text document) :to-equal source)
        (expect (document-text foreign-document) :to-equal source)
        (expect (document-text item-document) :to-equal (format nil "* Parent~%- [ ] item~%")))))

  (it "rejects a non-document input and accepts a one-element heading list"
    (let* ((source (format nil "* Parent [0/0]~%"))
           (document (parse-org source))
           (heading (statistics-cookies-test-heading document))
           (result (update-statistics-cookies document (list heading)))
           (failure (handler-case
                        (progn (update-statistics-cookies nil heading) nil)
                      (type-error (condition)
                        (type-error-expected-type condition)))))
      (with-soft-assertions
        (expect failure :to-be 'document)
        (expect (document-text (edit-result-updated-document result))
                :to-equal (format nil "* Parent [0/0]~%"))
        (expect (document-text document) :to-equal source))))

  (it "keeps the statistics update API and all of its exported symbols documented"
    (let ((update (make-statistics-cookie-update :node nil :old-value "[1/2]"
                                                 :new-value "[2/2]"))
          (default-update (make-statistics-cookie-update)))
      (with-soft-assertions
        (expect (statistics-cookie-update-p update) :to-be t)
        (expect (statistics-cookie-update-node update) :to-be nil)
        (expect (statistics-cookie-update-old-value update) :to-equal "[1/2]")
        (expect (statistics-cookie-update-new-value update) :to-equal "[2/2]")
        (expect (list (statistics-cookie-update-node default-update)
                      (statistics-cookie-update-old-value default-update)
                      (statistics-cookie-update-new-value default-update))
                :to-equal '(nil nil nil))
        (dolist (name '(statistics-cookie-update make-statistics-cookie-update
                        statistics-cookie-update-p statistics-cookie-update-node
                        statistics-cookie-update-old-value
                        statistics-cookie-update-new-value update-statistics-cookies))
          (expect (documented-p (find-symbol (symbol-name name) "ORG-KIT"))
                  :to-be t))))))

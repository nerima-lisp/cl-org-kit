;;;; t/agenda-bulk-test.lisp
(in-package #:cl-org-kit/test)

(defun agenda-bulk-test-utf8-bom-octets (text)
  (cl-codec-kit:string-to-octets
   (concatenate 'string (string (code-char #xfeff)) text)
   :encoding :utf-8))

(defun agenda-bulk-test-fixture (name)
  (let ((path (asdf:system-relative-pathname
               "cl-org-kit"
               (format nil "t/fixtures/e/oracle/agenda-bulk/~A.sexp" name)))
        (eof (gensym "EOF")))
    (with-open-file (stream path :direction :input)
      (let ((*read-eval* nil)
            (fixture (read stream nil eof)))
        (when (eq fixture eof)
          (error "Agenda bulk fixture ~A is empty." name))
        (unless (eq (read stream nil eof) eof)
          (error "Agenda bulk fixture ~A has more than one form." name))
        fixture))))

(defun agenda-bulk-test-headline (document title)
  (let ((found nil))
    (map-nodes
     (lambda (node)
       (when (and (eq (node-type node) :headline)
                  (string= title (node-property node :raw-value)))
         (setf found node)))
     (document-root document))
    found))

(defun agenda-bulk-test-entries (document titles)
  (mapcar (lambda (title)
            (make-agenda-entry
             :headline title
             :source-node (agenda-bulk-test-headline document title)))
          titles))

(defun agenda-bulk-test-time-context ()
  (make-time-context
   :reference-time (make-org-time-value "2024-02-19T12:34:00+00:00")
   :zone (resolve-time-zone "UTC")))

(defun agenda-bulk-test-apply-oracle (fixture source target entries)
  (let ((context (agenda-bulk-test-time-context)))
    (case (getf fixture :action)
      (:todo
       (agenda-bulk-action
        entries :todo :state (getf fixture :completion-choice)
        :at (make-org-time-value "2024-02-19T12:34:00+00:00")
        :time-context context))
      (:tag-add
       (agenda-bulk-action
        entries :add-tag :tag (getf fixture :completion-choice)))
      ((:schedule-set :schedule-delete)
       (agenda-bulk-action
        entries :schedule
        :schedule-date (getf fixture :date-choice)))
      (:refile
       (agenda-bulk-action
        entries :refile :target-document target
        :target-heading
        (agenda-bulk-test-headline target (getf fixture :target-heading))))
      (:archive
       (agenda-bulk-action
        entries :archive :target-document target :time-context context)))))

(defun agenda-bulk-test-error (thunk)
  (handler-case
      (progn (funcall thunk) nil)
    (error (condition) condition)))

(describe "agenda bulk actions"
  (it "matches Org 9.8.10 multi-entry action fixtures"
    (dolist (name '("todo-multi" "tag-add-multi" "schedule-set-multi"
                    "schedule-delete-multi" "refile-parent-child"
                    "archive-parent-child"))
      (let* ((fixture (agenda-bulk-test-fixture name))
             (source-text (getf fixture :source-input))
             (target-text (getf fixture :target-input))
             (source (parse-org source-text
                                :source-name (getf fixture :source-file)))
             (target
               (when (member (getf fixture :action) '(:refile :archive))
                 (parse-org
                  (or target-text "")
                  :source-name
                  (if (eq (getf fixture :action) :archive)
                      (concatenate 'string (getf fixture :source-file) "_archive")
                      (format nil "t/fixtures/e/oracle/agenda-bulk/~A-target.org"
                              name)))))
             (entries
               (agenda-bulk-test-entries
                source (getf fixture :selected-headlines)))
             (results
               (agenda-bulk-test-apply-oracle fixture source target entries))
             (source-edit (cdr (assoc source results :test #'eq)))
             (target-edit (and target (cdr (assoc target results :test #'eq)))))
        (with-soft-assertions
          (expect (getf fixture :org-version) :to-equal "9.8.10")
          (expect (document-text source) :to-equal source-text)
          (expect (edit-result-p source-edit) :to-be t)
          (expect (document-text (edit-result-updated-document source-edit))
                  :to-equal (getf fixture :source-expected))
          (if target
              (progn
                (expect (document-text target) :to-equal (or target-text ""))
                (expect (edit-result-p target-edit) :to-be t)
                (expect (document-text (edit-result-updated-document target-edit))
                        :to-equal (getf fixture :target-expected))
                (expect (length results) :to-be 2))
              (expect (length results) :to-be 1))))))
  (it "deduplicates rows and preserves duplicate-title order"
    (let* ((first-source "* Move~%First~%* Move~%Second~%")
           (second-source "* Also~%")
           (first-document (parse-org (format nil first-source)))
           (second-document (parse-org (format nil second-source)))
           (first-headings
             (remove-if-not
              (lambda (node) (eq (node-type node) :headline))
              (node-children (document-root first-document))))
           (first-heading (first first-headings))
           (second-first-heading (second first-headings))
           (second-heading (agenda-bulk-test-headline second-document "Also"))
           (target (parse-org ""))
           (entries
             (list (make-agenda-entry :source-node first-heading)
                   (make-agenda-entry :source-node first-heading)
                   (make-agenda-entry :source-node second-first-heading)
                   (make-agenda-match-entry :document second-document
                                            :node second-heading)))
           (results (agenda-bulk-action entries :refile :target-document target))
           (first-edit (cdr (assoc first-document results :test #'eq)))
           (second-edit (cdr (assoc second-document results :test #'eq)))
           (target-edit (cdr (assoc target results :test #'eq))))
      (with-soft-assertions
        (expect (length results) :to-be 3)
        (expect (document-text first-document) :to-equal (format nil first-source))
        (expect (document-text second-document) :to-equal (format nil second-source))
        (expect (document-text (edit-result-updated-document first-edit))
                :to-equal "")
        (expect (document-text (edit-result-updated-document second-edit))
                :to-equal "")
        (expect (document-text (edit-result-updated-document target-edit))
                :to-equal
                (format nil "* Move~%First~%* Move~%Second~%* Also~%")))))
  (it "matches agenda bulk TODO blocker inhibition while preserving direct checks"
    (let* ((source-text
             (format nil "#+TODO: TODO | DONE~%* TODO Parent~%** TODO Child~%"))
           (document (parse-org source-text))
           (parent (agenda-bulk-test-headline document "Parent"))
           (entry (make-agenda-entry :source-node parent))
           (at (make-org-time-value "2025-03-07T09:00:00+00:00"))
           (time-context (agenda-bulk-test-time-context))
           (blocked
             (agenda-bulk-test-error
              (lambda ()
                (transition-heading-todo document parent "DONE"
                                         :at at :time-context time-context))))
           (results
             (agenda-bulk-action (list entry) :todo :state "DONE"
                                 :at at :time-context time-context))
           (edit (cdr (assoc document results :test #'eq))))
      (with-soft-assertions
        (expect (typep blocked 'org-edit-error) :to-be t)
        (when (typep blocked 'org-edit-error)
          (expect (org-edit-error-reason blocked) :to-be :blocked))
        (expect (document-text document) :to-equal source-text)
        (expect (document-text (edit-result-updated-document edit))
                :to-equal
                (format nil "#+TODO: TODO | DONE~%* DONE Parent~%** TODO Child~%")))))
  (it "enforces the lowest document bulk-action limit before editing"
    (let* ((source-text (format nil "* First~%* Second~%"))
           (document
             (parse-org source-text
                        :limits (make-limits :max-agenda-bulk-actions 1)))
           (entries (agenda-bulk-test-entries document '("First" "Second")))
           (condition
             (agenda-bulk-test-error
              (lambda () (agenda-bulk-action entries :add-tag :tag "bulk")))))
      (with-soft-assertions
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when (typep condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name condition)
                  :to-be :max-agenda-bulk-actions)
          (expect (org-resource-limit-error-limit condition) :to-be 1)
          (expect (org-resource-limit-error-actual condition) :to-be 2))
        (expect (document-text document) :to-equal source-text))))
  (it "counts duplicate agenda rows against the bulk-action limit before deduplication"
    (let* ((source-text (format nil "* First~%"))
           (document
             (parse-org source-text
                        :limits (make-limits :max-agenda-bulk-actions 1)))
           (entry (first (agenda-bulk-test-entries document '("First"))))
           (condition
             (let ((original (symbol-function 'org-kit::node-heading-path)))
               (unwind-protect
                   (progn
                     (setf (symbol-function 'org-kit::node-heading-path)
                           (lambda (&rest _arguments)
                             (error "Bulk limit must precede heading-path construction")))
                     (agenda-bulk-test-error
                      (lambda ()
                        (agenda-bulk-action (list entry entry)
                                            :add-tag :tag "bulk"))))
                 (setf (symbol-function 'org-kit::node-heading-path) original)))))
      (with-soft-assertions
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when (typep condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name condition)
                  :to-be :max-agenda-bulk-actions)
          (expect (org-resource-limit-error-limit condition) :to-be 1)
          (expect (org-resource-limit-error-actual condition) :to-be 2))
        (expect (document-text document) :to-equal source-text))))
  (it "checks max-output-octets across all staged documents"
    (let* ((source-text (format nil "#+TODO: TODO | DONE~%* TODO Item~%"))
           (expected-output (format nil "#+TODO: TODO | DONE~%* DONE Item~%"))
           (input-octets (agenda-bulk-test-utf8-bom-octets source-text))
           (expected-document
             (parse-org (agenda-bulk-test-utf8-bom-octets expected-output)
                        :encoding :utf-8))
           (output-octets
             (length (write-org expected-document :output-type :octets)))
           (output-limit (1- (* 2 output-octets)))
           (first-document
             (parse-org input-octets :encoding :utf-8
                        :limits (make-limits :max-output-octets output-limit)))
           (second-document
             (parse-org input-octets :encoding :utf-8
                        :limits (make-limits :max-output-octets output-limit)))
           (entries
             (append (agenda-bulk-test-entries first-document '("Item"))
                     (agenda-bulk-test-entries second-document '("Item"))))
           (at (make-org-time-value "2024-02-19T12:34:00+00:00"))
           (time-context (agenda-bulk-test-time-context))
           (condition
             (agenda-bulk-test-error
              (lambda ()
                (agenda-bulk-action entries :todo :state "DONE"
                                    :at at :time-context time-context)))))
      (with-soft-assertions
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when (typep condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name condition)
                  :to-be :max-output-octets)
          (expect (org-resource-limit-error-limit condition)
                  :to-be output-limit)
          (expect (org-resource-limit-error-actual condition)
                  :to-be (* 2 output-octets)))
        (expect (document-text first-document) :to-equal source-text)
        (expect (document-text second-document) :to-equal source-text)
        (expect (document-bom-p first-document) :to-be t)
        (expect (document-bom-p second-document) :to-be t))))
)

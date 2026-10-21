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
  (it "deduplicates rows and adjusts nested paths after earlier moves"
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
                (format nil "* Move~%First~%* Move~%Second~%* Also~%"))))
    (let* ((nested-source
             (format nil
                     "* A~%** B~%first subtree marker~%* A~%** B~%selected child marker~%* A~%** B~%unselected child marker~%"))
           (nested-document (parse-org nested-source))
           (parent-headings
             (remove-if-not
              (lambda (node) (eq (node-type node) :headline))
              (node-children (document-root nested-document))))
           (first-parent (first parent-headings))
           (second-parent (second parent-headings))
           (selected-child
             (find-if (lambda (node)
                        (and (eq (node-type node) :headline)
                             (string= "B" (node-property node :raw-value))))
                      (node-children second-parent)))
           (nested-target (parse-org "* Destination~%"))
           (target-heading
             (agenda-bulk-test-headline nested-target "Destination"))
           (nested-results
             (agenda-bulk-action
              (list (make-agenda-entry :source-node first-parent)
                    (make-agenda-entry :source-node selected-child))
              :refile :target-document nested-target
              :target-heading target-heading))
           (source-edit
             (cdr (assoc nested-document nested-results :test #'eq)))
           (target-edit
             (cdr (assoc nested-target nested-results :test #'eq)))
           (updated-source
             (document-text (edit-result-updated-document source-edit)))
           (updated-target
             (document-text (edit-result-updated-document target-edit))))
      (with-soft-assertions
        (expect updated-source
                :to-equal
                (format nil "* A~%* A~%** B~%unselected child marker~%"))
        (expect (search "first subtree marker" updated-target) :to-be-truthy)
        (expect (search "selected child marker" updated-target) :to-be-truthy)
        (expect (search "unselected child marker" updated-target) :to-be nil))))
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
  (it "reports document and selection context when a bulk document fails"
    (let* ((source-text (format nil "#+TODO: TODO | DONE~%* TODO Item~%"))
           (expected-output (format nil "#+TODO: TODO | DONE~%* DONE Item~%"))
           (input-octets (agenda-bulk-test-utf8-bom-octets source-text))
           (expected-document
             (parse-org (agenda-bulk-test-utf8-bom-octets expected-output)
                        :encoding :utf-8))
           (output-octets
             (length (write-org expected-document :output-type :octets)))
           (first-document
             (parse-org input-octets :encoding :utf-8
                        :limits (make-limits :max-output-octets output-octets)))
           (second-document
             (parse-org input-octets :encoding :utf-8
                        :limits (make-limits :max-output-octets 0)))
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
        (expect (typep condition 'org-agenda-error) :to-be t)
        (when (typep condition 'org-agenda-error)
          (expect (org-agenda-error-reason condition)
                  :to-be :bulk-action-failed)
          (expect (search "document 2" (org-agenda-error-context condition))
                  :to-be-truthy)
          (expect (search "Item" (org-agenda-error-context condition))
                  :to-be-truthy)
          (expect (search "max-output-octets"
                          (org-kit-error-message condition))
                  :to-be-truthy))
        (expect (document-text first-document) :to-equal source-text)
        (expect (document-text second-document) :to-equal source-text)
        (expect (document-bom-p first-document) :to-be t)
        (expect (document-bom-p second-document) :to-be t))))
  (it "applies max-output-octets per changed document"
    (let* ((source-text (format nil "#+TODO: TODO | DONE~%* TODO Item~%"))
           (expected-output (format nil "#+TODO: TODO | DONE~%* DONE Item~%"))
           (unchanged-source (format nil "* Unchanged~%"))
           (input-octets (agenda-bulk-test-utf8-bom-octets source-text))
           (expected-document
             (parse-org (agenda-bulk-test-utf8-bom-octets expected-output)
                        :encoding :utf-8))
           (output-octets
             (length (write-org expected-document :output-type :octets)))
           (first-document
             (parse-org input-octets :encoding :utf-8
                        :limits (make-limits :max-output-octets output-octets)))
           (second-document
             (parse-org input-octets :encoding :utf-8
                        :limits (make-limits :max-output-octets output-octets)))
           (unchanged-document
             (parse-org unchanged-source
                        :limits (make-limits :max-output-octets 0)))
           (entries
             (append (agenda-bulk-test-entries first-document '("Item"))
                     (agenda-bulk-test-entries second-document '("Item"))))
           (at (make-org-time-value "2024-02-19T12:34:00+00:00"))
           (time-context (agenda-bulk-test-time-context))
           (results
             (agenda-bulk-action entries :todo :state "DONE"
                                 :at at :time-context time-context
                                 :target-document unchanged-document))
           (first-edit (cdr (assoc first-document results :test #'eq)))
           (second-edit (cdr (assoc second-document results :test #'eq)))
           (unchanged-edit (cdr (assoc unchanged-document results :test #'eq)))
           (first-updated (edit-result-updated-document first-edit))
           (second-updated (edit-result-updated-document second-edit))
           (unchanged-updated (edit-result-updated-document unchanged-edit)))
      (with-soft-assertions
        (expect (length (write-org first-updated :output-type :octets))
                :to-be output-octets)
        (expect (length (write-org second-updated :output-type :octets))
                :to-be output-octets)
        (expect (document-text unchanged-updated) :to-equal unchanged-source)
        (expect (edit-result-spans unchanged-edit) :to-be nil)
        (expect (document-text first-document) :to-equal source-text)
        (expect (document-text second-document) :to-equal source-text))))
  (it "applies output limits per document during refile"
    (let* ((source-text (format nil "* Keep~%* Move~%"))
           (target-text (format nil "* Destination~%"))
           (source-updated-text (format nil "* Keep~%"))
           (target-updated-text (format nil "* Destination~%* Move~%"))
           (source-limit
             (length (write-org (parse-org source-updated-text)
                                :output-type :octets)))
           (target-limit
             (length (write-org (parse-org target-updated-text)
                                :output-type :octets)))
           (source
             (parse-org source-text
                        :limits (make-limits :max-output-octets source-limit)))
           (target
             (parse-org target-text
                        :limits (make-limits :max-output-octets target-limit)))
           (entry (make-agenda-entry
                   :source-node (agenda-bulk-test-headline source "Move")))
           (results (agenda-bulk-action (list entry) :refile
                                        :target-document target))
           (source-edit (cdr (assoc source results :test #'eq)))
           (target-edit (cdr (assoc target results :test #'eq)))
           (source-updated (edit-result-updated-document source-edit))
           (target-updated (edit-result-updated-document target-edit)))
      (with-soft-assertions
        (expect (document-text source-updated) :to-equal source-updated-text)
        (expect (document-text target-updated) :to-equal target-updated-text)
        (expect (length (write-org source-updated :output-type :octets))
                :to-be source-limit)
        (expect (length (write-org target-updated :output-type :octets))
                :to-be target-limit)
        (expect (document-text source) :to-equal source-text)
        (expect (document-text target) :to-equal target-text))))
  (it "validates action options, agenda sources, and staged heading paths"
    (let* ((document (parse-org (format nil "* Task~%")))
           (entry (first (agenda-bulk-test-entries document '("Task"))))
           (heading (agenda-bulk-test-headline document "Task"))
           (at (make-org-time-value "2024-02-19T12:34:00+00:00"))
           (time-context (agenda-bulk-test-time-context))
           (invalid-action
             (agenda-bulk-test-error
              (lambda () (agenda-bulk-action nil :unknown))))
           (missing-todo-state
             (agenda-bulk-test-error
              (lambda () (agenda-bulk-action nil :todo :at at))))
           (missing-todo-at
             (agenda-bulk-test-error
              (lambda () (agenda-bulk-action nil :todo :state "DONE"))))
           (invalid-todo-state
             (agenda-bulk-test-error
              (lambda () (agenda-bulk-action nil :todo :state 7 :at at))))
           (invalid-todo-target-heading
             (agenda-bulk-test-error
              (lambda ()
                (agenda-bulk-action (list entry) :todo
                                    :state "DONE" :at at
                                    :target-heading heading))))
           (missing-tag
             (agenda-bulk-test-error
              (lambda () (agenda-bulk-action nil :add-tag))))
           (invalid-tag
             (agenda-bulk-test-error
              (lambda () (agenda-bulk-action nil :add-tag :tag 7))))
           (invalid-tag-target-heading
             (agenda-bulk-test-error
              (lambda ()
                (agenda-bulk-action (list entry) :add-tag
                                    :tag "bulk" :target-heading heading))))
           (missing-schedule-date
             (agenda-bulk-test-error
              (lambda () (agenda-bulk-action nil :schedule))))
           (invalid-schedule-date
             (agenda-bulk-test-error
              (lambda ()
                (agenda-bulk-action (list entry) :schedule
                                    :schedule-date "not-a-date"))))
           (invalid-schedule-target-heading
             (agenda-bulk-test-error
              (lambda ()
                (agenda-bulk-action
                 (list entry) :schedule :schedule-date "2024-02-20"
                 :target-heading heading))))
           (time-schedule-date
             (agenda-bulk-test-error
              (lambda ()
                (agenda-bulk-action
                 (list entry) :schedule
                 :schedule-date (make-org-time-value
                                 "2024-02-20T09:00:00+00:00")))))
           (integer-schedule-date
             (agenda-bulk-test-error
              (lambda ()
                (agenda-bulk-action (list entry) :schedule :schedule-date 42))))
           (missing-refile-target
             (agenda-bulk-test-error
              (lambda () (agenda-bulk-action nil :refile))))
           (missing-archive-context
             (agenda-bulk-test-error
              (lambda () (agenda-bulk-action nil :archive))))
           (invalid-archive-target
             (agenda-bulk-test-error
              (lambda ()
                (agenda-bulk-action nil :archive :target-document 7
                                    :time-context time-context))))
           (missing-heading-document
             (agenda-bulk-test-error
              (lambda ()
                (agenda-bulk-action (list entry) :add-tag :tag "bulk"
                                    :target-heading heading))))
           (invalid-entry
             (agenda-bulk-test-error
              (lambda ()
                (agenda-bulk-action (list (make-agenda-entry))
                                    :add-tag :tag "bulk"))))
           (stale-path
             (let ((path (copy-tree (org-kit::node-heading-path heading))))
               (setf (getf (first path) :title) "Missing")
               (agenda-bulk-test-error
                (lambda ()
                  (org-kit::agenda-bulk-resolve-heading-path document path)))))
           (missing-current-document
             (agenda-bulk-test-error
              (lambda ()
                (org-kit::agenda-bulk-current-document nil document))))
           (missing-stage-document
             (agenda-bulk-test-error
              (lambda ()
                (org-kit::agenda-bulk-stage-document nil document document))))
           (invalid-source-document (parse-org "Body text"))
           (invalid-source-entry
             (make-agenda-entry
              :source-node (first (node-children
                                   (document-root invalid-source-document)))))
           (missing-source-headline
             (agenda-bulk-test-error
              (lambda ()
                (agenda-bulk-action (list invalid-source-entry) :add-tag
                                    :tag "bulk"))))
           (invalid-index-path
             (let ((path (copy-tree (org-kit::node-heading-path heading))))
               (setf (getf (first path) :index) "first")
               (agenda-bulk-test-error
                (lambda ()
                  (org-kit::agenda-bulk-resolve-heading-path document path)))))
           (unrecognized-entry-heading
             (org-kit::agenda-bulk-entry-heading nil)))
      (with-soft-assertions
        (dolist (condition
                 (list invalid-action missing-todo-state missing-todo-at
                       invalid-todo-state invalid-todo-target-heading
                       missing-tag invalid-tag invalid-tag-target-heading
                       missing-schedule-date invalid-schedule-date
                       invalid-schedule-target-heading
                       time-schedule-date integer-schedule-date
                       missing-refile-target missing-archive-context
                       invalid-archive-target missing-heading-document
                       invalid-entry missing-source-headline))
          (expect (typep condition 'org-agenda-error) :to-be t)
          (when (typep condition 'org-agenda-error)
            (expect (org-agenda-error-reason condition) :to-be :invalid-option)))
        (dolist (condition
                 (list stale-path missing-current-document missing-stage-document
                       invalid-index-path))
          (expect (typep condition 'org-agenda-error) :to-be t)
          (when (typep condition 'org-agenda-error)
            (expect (org-agenda-error-reason condition) :to-be :target-not-found)))
        (expect (agenda-bulk-action nil :add-tag :tag "bulk") :to-be nil)
        (expect unrecognized-entry-heading :to-be nil))))
  (it "accepts agenda results whose source nodes are planning entries"
    (let* ((source-text
             (format nil "* TODO Task~%SCHEDULED: <2024-02-20 Tue>~%"))
           (document (parse-org source-text))
           (date (make-org-time-value "2024-02-20"))
           (view (agenda (list document) date date
                         :time-context (agenda-bulk-test-time-context)))
           (results (agenda-bulk-action view :add-tag :tag "bulk"))
           (updated (edit-result-updated-document
                     (cdr (assoc document results :test #'eq)))))
      (with-soft-assertions
        (expect (plusp (length (agenda-result-entries view))) :to-be t)
        (expect (not (null (search ":bulk:" (document-text updated))))
               :to-be t))))
  (it "schedules date variants and handles unscheduled entries"
    (flet ((scheduled-text (date)
             (let* ((document (parse-org (format nil "* Task~%")))
                    (results
                      (agenda-bulk-action
                       (agenda-bulk-test-entries document '("Task"))
                       :schedule :schedule-date date)))
               (document-text
                (edit-result-updated-document
                 (cdr (assoc document results :test #'eq)))))))
      (with-soft-assertions
        (dolist (date
                 (list (cl-date-kit:make-local-date 2024 2 20)
                       (make-org-time-value "2024-02-20")
                       "2024-02-20"))
          (expect (not (null (search "SCHEDULED: <2024-02-20 Tue>"
                                     (scheduled-text date))))
                  :to-be t))
        (let* ((source-text
                 (format nil
                         "* Task~%:PROPERTIES:~%:ID: range-task~%:END:~%SCHEDULED: <2025-03-01 Sat +1w>--<2025-03-08 Sat>~%"))
               (document (parse-org source-text))
               (entries (agenda-bulk-test-entries document '("Task")))
               (results
                 (agenda-bulk-action entries :schedule
                                     :schedule-date "2025-03-02"))
               (updated
                 (document-text
                  (edit-result-updated-document
                   (cdr (assoc document results :test #'eq))))))
          (expect updated
                  :to-equal
                  (format nil
                          "* Task~%:PROPERTIES:~%:ID: range-task~%:END:~%SCHEDULED: <2025-03-02 Sun +1w>--<2025-03-09 Sun>~%")))
        (let* ((source-text
                 (format nil
                         "* Task~%SCHEDULED:  <2025-03-01 Sat +1w>--<2025-03-08 Sat>~%Keep this text.~%"))
               (document (parse-org source-text))
               (entries (agenda-bulk-test-entries document '("Task")))
               (results
                 (agenda-bulk-action entries :schedule :schedule-date nil))
               (updated
                 (document-text
                  (edit-result-updated-document
                   (cdr (assoc document results :test #'eq))))))
          (progn
            (expect updated :to-equal (format nil "* Task~%Keep this text.~%"))
            (let* ((source-text
                     (format nil
                             "* Task~%SCHEDULED: <2025-03-01 Sat +1w>--<2025-03-08 Sat> Keep this too.~%"))
                   (document (parse-org source-text))
                   (entries (agenda-bulk-test-entries document '("Task")))
                   (results
                     (agenda-bulk-action entries :schedule :schedule-date nil))
                   (updated
                     (document-text
                      (edit-result-updated-document
                       (cdr (assoc document results :test (function eq)))))))
              (expect updated :to-equal (format nil "* Task~%Keep this too.~%")))
            (let* ((source-text
                     (format nil
                             "* Task~%SCHEDULED: <2025-03-01 Sat +1w>--<2025-03-08 Sat>"))
                   (document (parse-org source-text))
                   (entries (agenda-bulk-test-entries document '("Task")))
                   (results
                     (agenda-bulk-action entries :schedule :schedule-date nil))
                   (updated
                     (document-text
                      (edit-result-updated-document
                       (cdr (assoc document results :test (function eq)))))))
              (expect updated :to-equal (format nil "* Task~%")))))
        (dolist (source-text
                 (list (format nil "* Task~%")
                       (format nil
                               "* Task~%SCHEDULED:<2025-03-01 Sat> Keep this text.~%")
                       (format nil
                               "* Task~%SCHEDULED: extra <2025-03-01 Sat> Keep this text.~%")
                       (format nil
                               "* Task~%<2025-03-01 Sat> SCHEDULED: Keep this text.~%")
                       (format nil
                               "* Task~%DEADLINE: <2025-03-01 Sat> Keep this text.~%")))
          (let* ((document (parse-org source-text))
                 (results
                   (agenda-bulk-action
                    (agenda-bulk-test-entries document (list "Task"))
                    :schedule :schedule-date nil))
                 (edit (cdr (assoc document results :test (function eq)))))
            (expect (document-text (edit-result-updated-document edit))
                    :to-equal source-text)
            (expect (edit-result-spans edit) :to-be nil))))))
  (it "checks archive headers, local archive targets, and refile cycles"
    (let* ((source (parse-org (format nil "* Task~%")
                              :source-name "source.org"))
           (entry (first (agenda-bulk-test-entries source '("Task"))))
           (context (agenda-bulk-test-time-context))
           (archive-target (parse-org "* Existing"))
           (archive-results
             (agenda-bulk-action (list entry) :archive
                                 :target-document archive-target
                                 :time-context context))
           (updated-target
             (document-text
              (edit-result-updated-document
               (cdr (assoc archive-target archive-results :test #'eq)))))
           (new-target (parse-org ""))
           (new-results
             (agenda-bulk-action (list entry) :archive
                                 :target-document new-target
                                 :time-context context))
           (updated-new-target
             (document-text
              (edit-result-updated-document
               (cdr (assoc new-target new-results :test #'eq)))))
           (local-source (parse-org (format nil "* Task~%")))
           (local-results
             (agenda-bulk-action
              (agenda-bulk-test-entries local-source '("Task"))
              :archive :time-context context))
           (local-updated
             (document-text
              (edit-result-updated-document
               (cdr (assoc local-source local-results :test #'eq)))))
           (same-document-results
             (agenda-bulk-action
              (agenda-bulk-test-entries local-source '("Task")) :archive
              :target-document local-source :time-context context))
           (same-document-updated
             (document-text
              (edit-result-updated-document
               (cdr (assoc local-source same-document-results :test #'eq)))))
           (source-with-name (parse-org "* Task" :source-name "source.org"))
           (unnamed-source (parse-org "* Task"))
           (existing-target (parse-org "* Existing"))
           (line-ending-target
             (parse-org (format nil "* Existing~C~%" #\Return)))
           (line-ending-updated
             (document-text
              (org-kit::agenda-bulk-archive-target-document
               source-with-name line-ending-target (parse-org ""))))
           (cycle-document
             (parse-org (format nil "* Parent~%** Child~%* Elsewhere~%")))
           (parent (agenda-bulk-test-headline cycle-document "Parent"))
           (child (agenda-bulk-test-headline cycle-document "Child"))
           (cycle-entry (make-agenda-entry :source-node parent))
           (self-cycle
             (agenda-bulk-test-error
              (lambda ()
                (agenda-bulk-action (list cycle-entry) :refile
                                    :target-document cycle-document
                                    :target-heading parent))))
           (descendant-cycle
             (agenda-bulk-test-error
              (lambda ()
                (agenda-bulk-action (list cycle-entry) :refile
                                    :target-document cycle-document
                                    :target-heading child))))
           (tabbed-document
             (parse-org (format nil "* TODO Tab~CTitle :old:~%" #\Tab)))
           (tabbed-results
             (agenda-bulk-action
              (agenda-bulk-test-entries
               tabbed-document (list (format nil "Tab~CTitle" #\Tab)))
              :add-tag :tag "bulk")))
      (with-soft-assertions
        (expect (search "Archived entries from file source.org" updated-target)
                :to-be nil)
        (expect (search "* Task" updated-target) :to-be-truthy)
        (expect (search "Archived entries from file source.org"
                        updated-new-target)
                :to-be-truthy)
        (expect (search (format nil "Archived entries from file source.org~C~C"
                                #\Return #\Newline)
                        line-ending-updated)
                :to-be-truthy)
        (expect (not (null (search "Archive" local-updated))) :to-be t)
        (expect (not (null (search "ARCHIVE_TIME" same-document-updated)))
                :to-be t)
        (expect (org-kit::agenda-bulk-archive-target-document
                 source-with-name source-with-name source-with-name)
                :to-be source-with-name)
        (expect (org-kit::agenda-bulk-archive-target-document
                 unnamed-source existing-target existing-target)
                :to-be existing-target)
        (dolist (condition (list self-cycle descendant-cycle))
          (expect (typep condition 'org-agenda-error) :to-be t)
          (when (typep condition 'org-agenda-error)
            (expect (org-agenda-error-reason condition) :to-be :invalid-option)))
        (expect (not
                 (null
                  (search ":bulk:"
                          (document-text
                           (edit-result-updated-document
                            (cdr (assoc tabbed-document tabbed-results
                                        :test #'eq)))))))
                :to-be t))))
  (it "rejects non-boolean TODO blocker inhibition"
    (let* ((document
             (parse-org (format nil "#+TODO: TODO | DONE~%* TODO Task~%")))
           (heading (agenda-bulk-test-headline document "Task"))
           (condition
             (agenda-bulk-test-error
              (lambda ()
                (transition-heading-todo
                 document heading "DONE"
                 :at (make-org-time-value "2024-02-19T12:34:00+00:00")
                 :time-context (agenda-bulk-test-time-context)
                 :inhibit-blocking :yes)))))
      (with-soft-assertions
        (expect (typep condition 'org-edit-error) :to-be t)
        (when (typep condition 'org-edit-error)
          (expect (org-edit-error-reason condition) :to-be :invalid-value)))))
)

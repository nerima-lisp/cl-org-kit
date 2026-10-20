;;;; t/agenda-write-test.lisp
(in-package #:cl-org-kit/test)

(defun agenda-write-test-error (function)
  (handler-case (progn
                  (funcall function)
                  nil)
    (error (condition)
      condition)))

(defun agenda-write-test-context ()
  (make-time-context :reference-time
                     (make-org-time-value "2025-01-01T12:00:00+00:00")
                     :zone
                     (resolve-time-zone "UTC")))

(defun agenda-write-test-document (&optional source limits)
  (parse-org (or source (format nil "#+CATEGORY: Work~%* TODO Alpha :work:~%"))
             :limits
             (or limits (make-limits))))

(defun agenda-write-test-headings (document)
  (let ((headings nil))
    (map-nodes
     (lambda (node)
       (when (eq (node-type node) :headline)
         (push node headings)))
     (document-root document))
    (nreverse headings)))

(describe "Agenda output and source-range writes"
          (it "matches Org 9.8.10 default agenda date and entry layout"
  (let* ((first-date (make-org-time-value "2025-01-01"))
         (second-date (make-org-time-value "2025-01-02"))
         (monday-date (make-org-time-value "2025-01-06"))
         (result
           (make-agenda-result
            :entries
            (list
             (make-agenda-entry
              :date first-date :kind :deadline :headline "Prepare"
              :todo-state "TODO" :effective-tags '("work" "home")
              :category "Planning" :warning-state :due
              :timestamp
              (make-org-timestamp-view
               :kind :active-range
               :start (make-org-time-value "2025-01-01T09:15:00+00:00")
               :end (make-org-time-value "2025-01-01T10:00:00+00:00")
               :start-inclusive-p t :end-inclusive-p nil))
             (make-agenda-entry
              :date first-date :kind :scheduled :headline "Call"
              :todo-state "NEXT" :effective-tags '("phone")
              :category "Calls"
              :timestamp
              (make-org-timestamp-view
               :kind :active
               :start (make-org-time-value "2025-01-01T14:05:00+00:00")))
             (make-agenda-entry
              :date first-date :kind :deadline :headline "Soon"
              :category "Work" :warning-state :prewarning
              :timestamp
              (make-org-timestamp-view
               :kind :active :start (make-org-time-value "2025-01-04")))
             (make-agenda-entry
              :date first-date :kind :deadline :headline "Late"
              :category "Work" :warning-state :overdue
              :timestamp
              (make-org-timestamp-view
               :kind :active :start (make-org-time-value "2024-12-30")))
             (make-agenda-entry
              :date first-date :kind :scheduled :headline "Retry"
              :category "Work" :scheduled-delay-state :delayed
              :timestamp
              (make-org-timestamp-view
               :kind :active :start (make-org-time-value "2024-12-31")))
             (make-agenda-entry
              :date first-date :kind :scheduled :headline "Habit"
              :category "Work" :habit-p t :scheduled-delay-state :delayed
              :timestamp
              (make-org-timestamp-view
               :kind :active
               :start (make-org-time-value "2024-12-31T09:30:00+00:00")))
             (make-agenda-entry
              :date first-date :kind :timestamp :headline "All-day"
              :category "Work"
              :timestamp
              (make-org-timestamp-view
               :kind :active :start (make-org-time-value "2025-01-01")))
             (make-agenda-entry :date first-date :headline "Symbol"
                                :category :misc)
             (make-agenda-entry :date second-date :headline "Review")
             (make-agenda-entry :date monday-date
                                :headline "Start" :category "Work")))))
    (expect (agenda-format-text result)
            :to-equal
            (concatenate
             'string
             "Wednesday   1 January 2025" (string #\Newline)
             "  Planning:    9:15-10:00 Deadline:   TODO Prepare"
             (make-string 38 :initial-element #\Space)
             " :work:home:" (string #\Newline)
             "  Calls:      14:05...... Scheduled:  NEXT Call"
             (make-string 41 :initial-element #\Space)
             " :phone:" (string #\Newline)
             "  Work:       In   3 d.:  Soon" (string #\Newline)
             "  Work:        2 d. ago:  Late" (string #\Newline)
             "  Work:       Sched. 1x:  Retry" (string #\Newline)
             "  Work:        9:30...... Sched. 1x:  Habit" (string #\Newline)
             "  Work:       All-day" (string #\Newline)
             "  MISC:       Symbol" (string #\Newline)
             "Thursday    2 January 2025" (string #\Newline)
             "              Review" (string #\Newline)
             "Monday      6 January 2025 W02" (string #\Newline)
             "  Work:       Start" (string #\Newline)))))
          (it "formats match entries and accepts a custom view wrapper"
  (let* ((document (agenda-write-test-document))
         (context (agenda-write-test-context))
         (matches
           (agenda-match (list document) "TODO!=\"DONE\""
                         :time-context
                         context))
         (view
           (first
            (agenda-custom (list document)
                           (list (list :key "todo" :type :todo))
                           :time-context
                           context)))
         (expected
           (concatenate 'string
                        "  Work:       TODO Alpha"
                        (make-string 40 :initial-element #\Space)
                        " :work:"
                        (string #\Newline))))
    (with-soft-assertions
     (expect (agenda-format-text matches) :to-equal expected)
     (expect (agenda-format-text view) :to-equal expected)
     (expect (agenda-format-text nil) :to-equal ""))))
          (it "rejects unsupported entries and enforces output limits"
              (let* ((invalid-view
                      (agenda-write-test-error
                       (lambda ()
                         (agenda-format-text :unknown))))
                     (invalid-result
                      (agenda-write-test-error
                       (lambda ()
                         (agenda-format-text
                          (make-agenda-result :entries '(:unknown))))))
                     (length-limited
                      (agenda-write-test-document (format nil "* TODO Long~%")
                                                  (make-limits
                                                   :max-output-length
                                                   4)))
                     (length-error
                      (agenda-write-test-error
                       (lambda ()
                         (agenda-format-text
                          (agenda-match (list length-limited) "TODO=\"TODO\""
                                        :time-context
                                        (agenda-write-test-context))))))
                     (octet-limited
                      (agenda-write-test-document (format nil "* TODO A~%")
                                                  (make-limits
                                                   :max-output-length
                                                   100
                                                   :max-output-octets
                                                   1)))
                     (octet-error
                      (agenda-write-test-error
                       (lambda ()
                         (agenda-format-text
                          (agenda-match (list octet-limited) "TODO=\"TODO\""
                                        :time-context
                                        (agenda-write-test-context)))))))
                (with-soft-assertions
                 (expect (typep invalid-view 'org-agenda-error) :to-be t)
                 (expect (org-agenda-error-reason invalid-view)
                         :to-be
                         :unsupported-entry)
                 (expect (typep invalid-result 'org-agenda-error) :to-be t)
                 (expect (org-agenda-error-reason invalid-result)
                         :to-be
                         :unsupported-entry)
                 (expect (typep length-error 'org-resource-limit-error)
                         :to-be
                         t)
                 (expect (org-resource-limit-error-limit-name length-error)
                         :to-be
                         :max-output-length)
                 (expect (typep octet-error 'org-resource-limit-error) :to-be t)
                 (expect (org-resource-limit-error-limit-name octet-error)
                         :to-be
                         :max-output-octets))))
          (it "limits output entries and rejects invalid dates or timestamps"
              (let* ((limited-document
                      (agenda-write-test-document
                       (format nil "* TODO First~%* TODO Second~%")
                       (make-limits :max-agenda-entries 1)))
                     (headings (agenda-write-test-headings limited-document))
                     (entry-limit-error
                      (agenda-write-test-error
                       (lambda ()
                         (agenda-format-text
                          (make-agenda-result :entries
                                              (mapcar
                                               (lambda (heading)
                                                 (make-agenda-entry :headline
                                                                    (node-property
                                                                     heading
                                                                     :raw-value)
                                                                    :source-node
                                                                    heading))
                                               headings))))))
                     (time-value
                      (make-org-time-value "2025-01-01T12:00:00+00:00"))
                     (date-error
                      (agenda-write-test-error
                       (lambda ()
                         (agenda-format-text
                          (make-agenda-result :entries
                                              (list
                                               (make-agenda-entry :date
                                                                  time-value)))))))
                     (timestamp-error
                      (agenda-write-test-error
                       (lambda ()
                         (agenda-format-text
                          (make-agenda-result :entries
                                              (list
                                               (make-agenda-entry :headline
                                                                  "Invalid"
                                                                  :timestamp
                                                                  :invalid))))))))
                (with-soft-assertions
                 (expect (typep entry-limit-error 'org-resource-limit-error)
                         :to-be
                         t)
                 (expect (org-resource-limit-error-limit-name entry-limit-error)
                         :to-be
                         :max-agenda-entries)
                 (expect (typep date-error 'org-agenda-error) :to-be t)
                 (expect (org-agenda-error-reason date-error)
                         :to-be
                         :invalid-option)
                 (expect (typep timestamp-error 'org-agenda-error) :to-be t)
                 (expect (org-agenda-error-reason timestamp-error)
                         :to-be
                         :invalid-option))))
          (it "retains selected source text, deduplicates source subtrees, normalizes levels, and aligns promoted tags"
  (let* ((document
          (agenda-write-test-document
           (format nil
                   "* Parent~%parent~%** TODO Child :work:~%child~%*** Deep~%deep~%** TODO Child :work:~%child~%*** Deep~%deep~%* TODO Other~%other~%")))
         (matches
           (agenda-match (list document) "TODO=\"TODO\""
                         :time-context
                         (agenda-write-test-context)))
         (org-document
           (agenda-to-org (append matches (list (first matches)))))
         (level-document
          (agenda-write-test-document
           (format nil
                   "* Parent~%parent~%** TODO Duplicate :work:~%body~%* TODO Duplicate :work:~%body~%")))
         (level-org
           (agenda-to-org
            (agenda-match (list level-document) "TODO=\"TODO\""
                          :time-context
                          (agenda-write-test-context))))
         (crlf-document
           (agenda-write-test-document
            (format nil
                    "* TODO First~C~C* TODO Second~C~C"
                    #\Return #\Newline #\Return #\Newline)))
         (crlf-org
           (agenda-to-org
            (agenda-match (list crlf-document) "TODO=\"TODO\""
                          :time-context
                          (agenda-write-test-context)))))
    (with-soft-assertions
     (expect (document-p org-document) :to-be t)
     (expect (write-org org-document :output-type :string)
             :to-equal
             (concatenate 'string
                      "* TODO Child"
                      (make-string 59 :initial-element #\Space)
                      ":work:" (string #\Newline)
                      "child" (string #\Newline)
                      "* TODO Other" (string #\Newline)
                      "other" (string #\Newline)))
     (expect (document-text (agenda-to-org nil)) :to-equal "")
     (expect (document-text level-org)
             :to-equal
             (concatenate 'string
                      "* TODO Duplicate"
                      (make-string 55 :initial-element #\Space)
                      ":work:" (string #\Newline)
                      "body" (string #\Newline)
                      "* TODO Duplicate :work:" (string #\Newline)
                      "body" (string #\Newline)))
     (expect (document-text crlf-org)
             :to-equal
             (format nil
                     "* TODO First~C~C* TODO Second~C~C"
                     #\Return #\Newline #\Return #\Newline)))))
          (it
           "signals a named error when a selected entry has no source headline"
           (let* ((document (agenda-write-test-document))
                  (root (document-root document))
                  (entry
                   (make-agenda-entry :headline "Detached" :source-node root))
                  (invalid-match
                   (make-agenda-match-entry :document
                                            document
                                            :node
                                            (make-node :headline)))
                  (entry-error
                   (agenda-write-test-error
                    (lambda ()
                      (agenda-to-org (make-agenda-result :entries (list entry))))))
                  (match-error
                   (agenda-write-test-error
                    (lambda ()
                      (agenda-to-org (list invalid-match))))))
             (with-soft-assertions
              (expect (typep entry-error 'org-agenda-error) :to-be t)
              (expect (org-agenda-error-reason entry-error)
                      :to-be
                      :unsupported-entry)
              (expect (typep match-error 'org-agenda-error) :to-be t)
              (expect (org-agenda-error-reason match-error)
                      :to-be
                      :unsupported-entry))))
  (it "replaces only the explicit text range and reports original offsets"
  (let* ((document
          (agenda-write-test-document (format nil "prefix~%OLD~%suffix~%")))
         (result
           (make-agenda-result :entries
                               (list (make-agenda-entry
                                      :headline "Task"
                                      :todo-state "TODO"))))
         (replacement (agenda-format-text result))
         (start (line-offset document 2))
         (end (line-offset document 3))
         (edit (agenda-write-into document result start end))
         (updated (edit-result-updated-document edit))
         (source-bytes
           (cl-codec-kit:string-to-octets (document-text document)
                                          :encoding
                                          :utf-8))
         (updated-bytes
           (cl-codec-kit:string-to-octets (document-text updated)
                                          :encoding
                                          :utf-8))
         (span (first (edit-result-spans edit))))
    (with-soft-assertions
     (expect (document-text updated)
             :to-equal
             (concatenate 'string
                          "prefix" (string #\Newline)
                          (make-string 14 :initial-element #\Space)
                          "TODO Task" (string #\Newline)
                          "suffix" (string #\Newline)))
     (expect (list (edit-span-start span) (edit-span-end span))
             :to-equal
             (list start end))
     (expect (subseq updated-bytes 0 start)
             :to-equalp
             (subseq source-bytes 0 start))
     (expect (subseq updated-bytes (+ start (length replacement)))
             :to-equalp
             (subseq source-bytes end)))))
          (it
           "writes normalized Org output and signals invalid format or range errors"
           (let* ((source-document
                   (agenda-write-test-document (format nil "before~%OLD~%after~%")))
                  (source-headings
                   (agenda-write-test-headings
                    (agenda-write-test-document (format nil "** TODO Child~%body~%"))))
                  (source-entry
                   (make-agenda-entry :headline
                                      "Child"
                                      :source-node
                                      (first source-headings)))
                  (start (line-offset source-document 2))
                  (end (line-offset source-document 3))
                  (org-edit
                   (agenda-write-into source-document
                                      (make-agenda-result :entries
                                                          (list source-entry))
                                      start
                                      end
                                      :format
                                      :org))
                  (bad-format
                   (agenda-write-test-error
                    (lambda ()
                      (agenda-write-into source-document
                                         nil
                                         start
                                         end
                                         :format
                                         :html))))
                  (bad-range
                   (agenda-write-test-error
                    (lambda ()
                      (agenda-write-into source-document
                                         nil
                                         0
                                         (1+
                                          (length
                                           (document-text source-document))))))))
             (with-soft-assertions
              (expect (document-text (edit-result-updated-document org-edit))
                      :to-equal
                      (format nil "before~%* TODO Child~%body~%after~%"))
              (expect (typep bad-format 'org-agenda-error) :to-be t)
              (expect (org-agenda-error-reason bad-format)
                      :to-be
                      :invalid-option)
              (expect (typep bad-range 'org-edit-error) :to-be t)
              (expect (org-edit-error-reason bad-range) :to-be :invalid-value)
              (expect
               (typep
                (agenda-write-test-error
                 (lambda ()
                   (agenda-write-into :not-a-document nil 0 0)))
                'type-error)
               :to-be
               t))))
          (it "aligns promoted tagged headings by display width and rejects invalid sources and ranges"
  (let* ((document
           (parse-org
            (format nil "* Parent~%** TODO~cChild :work:" #\Tab)))
         (child (second (agenda-write-test-headings document)))
         (org-document
           (agenda-to-org
            (agenda-match (list document) "TODO=\"TODO\""
                          :time-context (agenda-write-test-context))))
         (detached-heading
           (make-node :headline
                      :properties (list :level 1 :raw-value "Detached")))
         (invalid-views
           (list
            (make-agenda-result
             :entries (list (make-agenda-entry :headline "No source")))
            (make-agenda-result
             :entries (list (make-agenda-entry :source-node detached-heading)))
            (list (make-agenda-match-entry :document document :node :invalid))
            (list (make-agenda-match-entry :document :invalid :node child))
            (list (make-agenda-match-entry
                   :document document :node (document-root document)))))
         (invalid-entry-errors
           (mapcar (lambda (view)
                     (agenda-write-test-error
                      (lambda () (agenda-to-org view))))
                   invalid-views))
         (range-document
           (parse-org
            (format nil
                    "* TODO MissingBegin~%* TODO MissingEnd~%* TODO OutOfRange~%* TODO MarkerOutside~%* TODO NoMarker~%")))
         (range-errors
           (let ((headings (agenda-write-test-headings range-document)))
             (setf (org-kit::%node-begin (first headings)) nil
                   (org-kit::%node-end (second headings)) nil
                   (org-kit::%node-end (third headings))
                   (1+ (length (document-text range-document)))
                   (org-kit::%node-begin (fourth headings))
                   (1+ (node-begin (fourth headings)))
                   (org-kit::%node-post-affiliated (fifth headings))
                   (1+ (node-post-affiliated (fifth headings))))
             (mapcar
              (lambda (heading)
                (agenda-write-test-error
                 (lambda ()
                   (agenda-to-org
                    (make-agenda-result
                     :entries (list (make-agenda-entry :source-node heading)))))))
              headings))))
    (with-soft-assertions
      (expect (document-text org-document)
              :to-equal
              (concatenate (intern "STRING" :cl)
                           "* TODO" (string #\Tab) "Child"
                           (make-string 58 :initial-element #\Space)
                           ":work:" (string #\Newline)))
      (expect
       (mapcar (lambda (condition)
                 (and (typep condition (intern "ORG-AGENDA-ERROR" :org-kit))
                      (eq (org-agenda-error-reason condition)
                          :unsupported-entry)))
               invalid-entry-errors)
       :to-equal (make-list 5 :initial-element t))
      (expect
       (mapcar (lambda (condition)
                 (and (typep condition (intern "ORG-AGENDA-ERROR" :org-kit))
                      (eq (org-agenda-error-reason condition)
                          :unsupported-entry)))
               range-errors)
       :to-equal (make-list 5 :initial-element t)))))
          (it "preserves affiliated source lines while aligning promoted tagged headlines"
  (let* ((document
           (agenda-write-test-document
            (format nil
                    "#+NAME: Child~%** TODO~cChild :work:~%"
                    #\Tab)))
         (view
           (agenda-match (list document) "TODO=\"TODO\""
                         :time-context (agenda-write-test-context)))
         (output (agenda-to-org view)))
    (expect (document-text output)
            :to-equal
            (concatenate (intern "STRING" :cl)
                         "#+NAME: Child" (string #\Newline)
                         "* TODO" (string #\Tab) "Child"
                         (make-string 58 :initial-element #\Space)
                         ":work:" (string #\Newline)))))
          (it "renders selected source headlines from multiple documents"
              (let* ((first-document (parse-org (format nil "* TODO First~%")))
                     (second-document (parse-org (format nil "* TODO Second~%")))
                     (first-heading (first (agenda-write-test-headings first-document)))
                     (second-heading (first (agenda-write-test-headings second-document)))
                     (output
                       (agenda-to-org
                        (list (make-agenda-match-entry :document first-document
                                                       :node first-heading)
                              (make-agenda-match-entry :document second-document
                                                       :node second-heading)))))
                (expect (document-text output)
                        :to-equal (format nil "* TODO First~%* TODO Second~%"))))
          (it "rejects improper agenda entry lists"
              (let ((entry-error
                      (agenda-write-test-error
                       (lambda ()
                         (agenda-to-org
                          (make-agenda-result
                           :entries
                           (cons (make-agenda-entry :headline "Detached") :tail)))))))
                (with-soft-assertions
                  (expect (typep entry-error 'org-agenda-error) :to-be t)
                  (when (typep entry-error 'org-agenda-error)
                    (expect (org-agenda-error-reason entry-error)
                            :to-be :unsupported-entry)))))
          (it "rejects invalid dates when writing a date heading"
              (let ((date-error
                      (agenda-write-test-error
                       (lambda ()
                         (org-kit::agenda-write-date-heading "not a date")))))
                (with-soft-assertions
                  (expect (typep date-error 'org-agenda-error) :to-be t)
                  (when (typep date-error 'org-agenda-error)
                    (expect (org-agenda-error-reason date-error)
                            :to-be :invalid-option)))))
          (it "rejects unsupported categories with a named agenda error"
              (let* ((entry (make-agenda-entry :headline "Invalid category"
                                               :category '("invalid")))
                     (result (make-agenda-result :entries (list entry)))
                     (category-error
                       (agenda-write-test-error
                        (lambda () (agenda-format-text result)))))
                (with-soft-assertions
                  (expect (typep category-error 'org-agenda-error) :to-be t)
                  (when (typep category-error 'org-agenda-error)
                    (expect (org-agenda-error-reason category-error)
                            :to-be :invalid-option))))))

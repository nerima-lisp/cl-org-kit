;;;; t/reader-headings-test.lisp
(in-package #:cl-org-kit/test)

(defun reader-headings-test-elements (document type)
  (let ((elements '()))
    (map-nodes
     (lambda (node)
       (when (eq (node-type node) type)
         (push node elements)))
     (document-root document))
    (nreverse elements)))

(defun reader-headings-test-lines (&rest strings)
  (format nil "~{~A~^~%~}" strings))

(describe "headline parsing" (it "builds a heading hierarchy with linked parents"
              (let* ((source
                      (reader-headings-test-lines "* Parent"
                                                  "Parent prose."
                                                  "** Child"
                                                  "Child prose."
                                                  "* Sibling"))
                     (document (parse-org source))
                     (headlines
                      (reader-headings-test-elements document :headline))
                     (parent (first headlines))
                     (child (second headlines))
                     (sibling (third headlines))
                     (section
                      (find :section (node-children parent) :key #'node-type))
                     (paragraph (first (node-children section))))
                (with-soft-assertions (expect (length headlines) :to-be 3)
                                      (expect
                                       (mapcar
                                        (lambda (node)
                                          (node-property node :level))
                                        headlines)
                                       :to-equal
                                       '(1 2 1))
                                      (expect (node-parent parent)
                                              :to-be
                                              (document-root document))
                                      (expect (node-parent child) :to-be parent)
                                      (expect (node-parent sibling)
                                              :to-be
                                              (document-root document))
                                      (expect (node-parent section)
                                              :to-be
                                              parent)
                                      (expect (node-type paragraph)
                                              :to-be
                                              :paragraph)
                                      (expect (node-parent paragraph)
                                              :to-be
                                              section))))
          (it
           "parses default TODO states, priority, comment marker, tags, archive, and title"
           (let* ((source
                   (reader-headings-test-lines
                    "* TODO [#A] COMMENT Tagged title :work:ARCHIVE:"
                    "* DONE Finished"))
                  (document (parse-org source))
                  (headlines (reader-headings-test-elements document :headline))
                  (todo (first headlines))
                  (done (second headlines)))
             (with-soft-assertions
              (expect (node-property todo :todo-keyword) :to-equal "TODO")
              (expect (node-property todo :todo-type) :to-be :todo)
              (expect (node-property done :todo-keyword) :to-equal "DONE")
              (expect (node-property done :todo-type) :to-be :done)
              (expect (node-property todo :priority) :to-be 65)
              (expect (node-property todo :raw-value) :to-equal "Tagged title")
              (expect (node-property todo :title) :to-equal '("Tagged title"))
              (expect (node-property todo :tags) :to-equal '("work" "ARCHIVE"))
              (expect (node-property todo :commentedp) :to-be t)
              (expect (node-property todo :archivedp) :to-be t))))
          (it "trims headline titles and keeps empty titles"
              (let* ((document
                      (parse-org
                       (reader-headings-test-lines "* Trimmed   " "* ")))
                     (headlines
                       (reader-headings-test-elements document :headline))
                     (trimmed (first headlines))
                     (empty (second headlines)))
                (expect (node-property trimmed :raw-value) :to-equal "Trimmed")
                (expect (node-property trimmed :title) :to-equal '("Trimmed"))
                (expect (node-property empty :raw-value) :to-equal "")))
          (it
           "collects custom TODO sequences and lets explicit settings override them"
           (let* ((source
                   (reader-headings-test-lines
                    "#+TODO: TODO(t) WAIT(w@/!) | DONE(d!) CANCELLED(c@)"
                    "#+SEQ_TODO: NEXT(n) HOLD(h) | FINISHED(f!)"
                    "#+TYP_TODO: IDEA(i) | REJECTED(r!)"
                    "* TODO First"
                    "* WAIT Waiting"
                    "* DONE Complete"
                    "* CANCELLED Cancelled"
                    "* NEXT Next"
                    "* HOLD Hold"
                    "* FINISHED Finished"
                    "* IDEA Idea"
                    "* REJECTED Rejected"))
                  (document (parse-org source))
                  (headlines (reader-headings-test-elements document :headline))
                  (states
                   (mapcar
                    (lambda (node)
                      (list (node-property node :todo-keyword)
                            (node-property node :todo-type)))
                    headlines))
                  (override-document
                   (parse-org
                    (reader-headings-test-lines "#+TODO: TODO | DONE"
                                                "* REVIEW Review"
                                                "* CLOSED Closed")
                    :todo-keywords
                    '(("REVIEW" . :todo) ("CLOSED" . :done))))
                  (override-headlines
                   (reader-headings-test-elements override-document :headline)))
             (with-soft-assertions
              (expect (document-settings document)
                      :to-equal
                      '(:todo-keywords
                        (("TODO" . :todo) ("WAIT" . :todo)
                         ("DONE" . :done)
                         ("CANCELLED" . :done)
                         ("NEXT" . :todo)
                         ("HOLD" . :todo)
                         ("FINISHED" . :done)
                         ("IDEA" . :todo)
                         ("REJECTED" . :done))
                        :startup
                        nil
                        :inlinetask-min-level
                        15
                        :link-abbrevs
                        nil))
              (expect states
                      :to-equal
                      '(("TODO" :todo) ("WAIT" :todo)
                        ("DONE" :done)
                        ("CANCELLED" :done)
                        ("NEXT" :todo)
                        ("HOLD" :todo)
                        ("FINISHED" :done)
                        ("IDEA" :todo)
                        ("REJECTED" :done)))
              (expect
               (getf (document-settings override-document) :todo-keywords)
               :to-equal
               '(("REVIEW" . :todo) ("CLOSED" . :done)))
              (expect
               (mapcar
                (lambda (node)
                  (list (node-property node :todo-keyword)
                        (node-property node :todo-type)))
                override-headlines)
               :to-equal
               '(("REVIEW" :todo) ("CLOSED" :done))))))
          (it "applies STARTUP odd levels to heading levels and ancestry"
              (let* ((document
                      (parse-org
                       (reader-headings-test-lines "#+STARTUP: odd"
                                                   "* One"
                                                   "*** Two"
                                                   "***** Three")))
                     (headlines
                      (reader-headings-test-elements document :headline))
                     (one (first headlines))
                     (two (second headlines))
                     (three (third headlines)))
                (with-soft-assertions
                 (expect (getf (document-settings document) :startup)
                         :to-equal
                         '("odd"))
                 (expect
                  (mapcar
                   (lambda (node)
                     (node-property node :level))
                   headlines)
                  :to-equal
                  '(1 2 3))
                 (expect (node-parent two) :to-be one)
                 (expect (node-parent three) :to-be two))))
          (it "collects LINK abbreviations and expands them in bracket links"
              (let* ((document
                      (parse-org
                       (reader-headings-test-lines
                        "#+LINK: issue https://tracker.example/%s"
                        "* Links"
                        "[[issue:42]]")))
                     (link
                      (first (reader-headings-test-elements document :link))))
                (with-soft-assertions
                 (expect (getf (document-settings document) :link-abbrevs)
                         :to-equal
                         '(("issue" . "https://tracker.example/%s")))
                 (expect (node-property link :type) :to-equal "https")
                 (expect (node-property link :path)
                         :to-equal
                         "//tracker.example/42"))))
          (it
           "attaches planning timestamps to both the planning node and headline"
           (let* ((document
                   (parse-org
                    (reader-headings-test-lines "* Scheduled task"
                                                "SCHEDULED: <2026-10-12 Mon> DEADLINE: <2026-10-13 Tue> CLOSED: [2026-10-11 Sun]"
                                                "Task body.")))
                  (headline
                   (first (reader-headings-test-elements document :headline)))
                  (section
                   (find :section (node-children headline) :key #'node-type))
                  (planning
                   (find :planning (node-children section) :key #'node-type))
                  (properties '(:scheduled :deadline :closed)))
             (with-soft-assertions
              (expect (node-type planning) :to-be :planning)
              (dolist (property properties)
                (let ((headline-timestamp (node-property headline property))
                      (planning-timestamp (node-property planning property)))
                  (expect (node-type headline-timestamp) :to-be :timestamp)
                  (expect (node-type planning-timestamp) :to-be :timestamp)
                  (expect (node-parent headline-timestamp) :to-be headline)
                  (expect (node-parent planning-timestamp) :to-be planning))))))
          (it "does not treat prose or an invalid timestamp as planning"
              (let* ((prose-document
                      (parse-org
                       (reader-headings-test-lines "* Prose"
                                                   "Narrative mentioning SCHEDULED: <2026-10-12 Mon> in passing.")))
                     (invalid-document
                      (parse-org
                       (reader-headings-test-lines "* Invalid"
                                                   "SCHEDULED: <not a date>")))
                     (prose-section
                      (find :section
                            (node-children
                             (first
                              (reader-headings-test-elements prose-document
                                                             :headline)))
                            :key
                            #'node-type))
                     (invalid-section
                      (find :section
                            (node-children
                             (first
                              (reader-headings-test-elements invalid-document
                                                             :headline)))
                            :key
                            #'node-type))
                     (prose-headline
                      (first
                       (reader-headings-test-elements prose-document :headline)))
                     (planning-properties '(:scheduled :deadline :closed)))
                (with-soft-assertions
                 (expect
                  (reader-headings-test-elements prose-document :planning)
                  :to-equal
                  '())
                 (expect (mapcar #'node-type (node-children prose-section))
                         :to-equal
                         '(:paragraph))
                 (dolist (property planning-properties)
                   (expect
                    (getf (node-properties prose-headline) property :missing)
                    :to-be
                    nil))
                 (expect
                  (reader-headings-test-elements invalid-document :planning)
                  :to-equal
                  '())
                 (expect (mapcar #'node-type (node-children invalid-section))
                         :to-equal
                         '(:paragraph)))))
          (it "uses the default and explicit inlinetask thresholds"
              (let* ((default-document
                      (parse-org
                       (reader-headings-test-lines
                        (concatenate 'string
                                     (make-string 14 :initial-element #\*)
                                     " Regular")
                        (concatenate 'string
                                     (make-string 15 :initial-element #\*)
                                     " TODO Inline")
                        "Inline body."
                        (concatenate 'string
                                     (make-string 15 :initial-element #\*)
                                     " END"))))
                     (default-headline
                      (first
                       (reader-headings-test-elements default-document
                                                      :headline)))
                     (default-inline
                      (first
                       (reader-headings-test-elements default-document
                                                      :inlinetask)))
                     (explicit-document
                      (parse-org
                       (reader-headings-test-lines "** TODO Inline"
                                                   "Inline body."
                                                   "** END")
                       :inlinetask-min-level
                       2))
                     (explicit-inline
                      (first
                       (reader-headings-test-elements explicit-document
                                                      :inlinetask))))
                (with-soft-assertions
                 (expect (node-property default-headline :level) :to-be 14)
                 (expect (node-type default-inline) :to-be :inlinetask)
                 (expect (node-property default-inline :level) :to-be 15)
                 (expect (node-property default-inline :todo-keyword)
                         :to-equal
                         "TODO")
                 (expect (node-property explicit-inline :level) :to-be 2)
                 (expect (node-type explicit-inline) :to-be :inlinetask))))
          (it "enforces the maximum heading depth"
              (with-soft-assertions
               (signals org-resource-limit-error
                        (parse-org (reader-headings-test-lines "* One" "** Two")
                                   :limits
                                   (make-limits :max-heading-depth 1)))))
          (it "uses implicit DONE states and bracketed TODO sequence keys"
              (let* ((source
                      (reader-headings-test-lines
                       "#+TODO: OPEN WAIT CLOSED"
                       "#+SEQ_TODO[flow]: NEXT | FINISHED"
                       "* OPEN One"
                       "* WAIT Two"
                       "* CLOSED Three"
                       "* NEXT Four"
                       "* FINISHED Five"))
                     (document (parse-org source))
                     (headlines
                      (reader-headings-test-elements document :headline)))
                (with-soft-assertions
                 (expect (getf (document-settings document) :todo-keywords)
                         :to-equal
                         '(("OPEN" . :todo) ("WAIT" . :todo)
                           ("CLOSED" . :done) ("NEXT" . :todo)
                           ("FINISHED" . :done)))
                 (expect
                  (mapcar
                   (lambda (node)
                     (list (node-property node :todo-keyword)
                           (node-property node :todo-type)))
                   headlines)
                  :to-equal
                  '(("OPEN" :todo) ("WAIT" :todo) ("CLOSED" :done)
                    ("NEXT" :todo) ("FINISHED" :done))))))

          (it "lets explicit STARTUP and LINK settings override buffer values"
              (let* ((source
                      (reader-headings-test-lines
                       "#+STARTUP: showall"
                       "#+LINK: issue https://buffer.example/%s"
                       "*** Parent"
                       "***** TODO Inline"
                       "Inline body."
                       "***** END"
                       "* Links"
                       "[[issue:42]]"))
                     (document
                      (parse-org
                       source
                       :startup
                       '("odd")
                       :inlinetask-min-level
                       3
                       :link-abbrevs
                       '(("issue" . "https://explicit.example/%s"))))
                     (parent
                      (find "Parent"
                            (reader-headings-test-elements document :headline)
                            :key
                            (lambda (node) (node-property node :raw-value))
                            :test
                            #'string=))
                     (inline
                      (first
                       (reader-headings-test-elements document :inlinetask)))
                     (link (first (reader-headings-test-elements document :link))))
                (with-soft-assertions
                 (expect (getf (document-settings document) :startup)
                         :to-equal
                         '("odd"))
                 (expect (getf (document-settings document) :link-abbrevs)
                         :to-equal
                         '(("issue" . "https://explicit.example/%s")))
                 (expect (node-property parent :level) :to-be 2)
                 (expect (node-type inline) :to-be :inlinetask)
                 (expect (node-property inline :level) :to-be 3)
                 (expect (node-parent (node-parent inline)) :to-be parent)
                 (expect (node-property link :path)
                         :to-equal
                         "//explicit.example/42"))))

          (it "ignores headings and settings inside a static block"
              (let* ((document
                      (parse-org
                       (reader-headings-test-lines
                        "* Parent"
                        ""
                        "#+begin_example"
                        "** Ghost heading"
                        "#+TODO: HIDDEN | FINISHED"
                        "#+end_example"
                        "** Child")))
                     (headlines
                      (reader-headings-test-elements document :headline))
                     (parent (first headlines))
                     (child (second headlines))
                     (section
                      (find :section (node-children parent) :key #'node-type)))
                (with-soft-assertions
                 (expect (mapcar (lambda (node) (node-property node :raw-value))
                                 headlines)
                         :to-equal
                         '("Parent" "Child"))
                 (expect (node-parent child) :to-be parent)
                 (expect (find :example-block (node-children section)
                               :key #'node-type)
                         :to-be-truthy)
                 (expect (assoc "HIDDEN"
                                (getf (document-settings document)
                                      :todo-keywords)
                                :test
                                #'string=)
                         :to-be
                         nil))))

          (it "parses numeric priority and special tags, preserving title objects"
              (let* ((document
                      (parse-org
                       (reader-headings-test-lines
                        "* [#42] Numeric :_team:@private:%draft:"
                        "* [#65] Invalid :bad/tag:"
                        "* Object /styled/")))
                     (headlines
                      (reader-headings-test-elements document :headline))
                     (numeric (first headlines))
                     (invalid (second headlines))
                     (styled (third headlines))
                     (italic
                      (find-if #'node-p (node-property styled :title))))
                (with-soft-assertions
                 (expect (node-property numeric :priority) :to-be 42)
                 (expect (node-property numeric :tags)
                         :to-equal
                         '("_team" "@private" "%draft"))
                 (expect (node-property invalid :priority) :to-be nil)
                 (expect (node-property invalid :tags) :to-be nil)
                 (expect (node-property invalid :raw-value)
                         :to-equal
                         "[#65] Invalid :bad/tag:")
                 (expect (node-type italic) :to-be :italic)
                 (expect (node-parent italic) :to-be styled))))

          (it "rejects duplicate planning keywords on one line"
              (let* ((document
                      (parse-org
                       (reader-headings-test-lines
                        "* Duplicate"
                        "SCHEDULED: <2026-10-12 Mon> SCHEDULED: <2026-10-13 Tue>")))
                     (headline
                      (first
                       (reader-headings-test-elements document :headline)))
                     (section
                      (find :section (node-children headline) :key #'node-type)))
                (with-soft-assertions
                 (expect (find :planning (node-children section)
                               :key #'node-type)
                         :to-be
                         nil)
                 (expect (getf (node-properties headline) :scheduled :missing)
                         :to-be
                         nil))))

          (it "ignores block-local inline END and leaves an unclosed task's body outside"
    (let* ((stars (make-string 15 :initial-element #\*))
           (closed-source
            (reader-headings-test-lines
             (concatenate 'string stars " TODO Closed")
             "#+begin_example"
             (concatenate 'string stars " END")
             "#+end_example"
             "Body."
             (concatenate 'string stars " END")))
           (closed-document (parse-org closed-source))
           (closed-task
            (first
             (reader-headings-test-elements closed-document :inlinetask)))
           (open-source
            (reader-headings-test-lines
             (concatenate 'string stars " TODO Open")
             "Body."
             (concatenate 'string stars " Other")))
           (open-document (parse-org open-source))
           (open-tasks
            (reader-headings-test-elements open-document :inlinetask)))
      (with-soft-assertions
       (expect (length
                (reader-headings-test-elements closed-document :inlinetask))
               :to-be
               1)
       (expect (mapcar #'node-type (node-children closed-task))
               :to-equal
               '(:example-block :paragraph))
       (expect
        (reader-headings-test-elements closed-document :headline)
        :to-equal
        '())
       (expect (length open-tasks) :to-be 2)
       (expect (node-end (first open-tasks))
               :to-be
               (1+ (position #\Newline open-source)))
       (expect (node-children (first open-tasks)) :to-equal nil)
       (expect (length
                (reader-headings-test-elements open-document :paragraph))
               :to-be
               1)
       (expect (node-property (second open-tasks) :raw-value)
               :to-equal
               "Other")
       (expect (node-end (second open-tasks))
               :to-be
               (length open-source))
       (expect
        (reader-headings-test-elements open-document :headline)
        :to-equal
        '()))))
          (it "covers bounded heading children and supplied planning nodes"
              (let* ((source "* Sibling")
                     (document (make-document source))
                     (context
                      (org-kit::%make-reader-context
                       :document document
                       :text source
                       :limits (make-limits)
                       :settings nil))
                     (empty-document (make-document ""))
                     (empty-context
                      (org-kit::%make-reader-context
                       :document empty-document
                       :text ""
                       :limits (make-limits)
                       :settings nil))
                     (planning-node
                      (org-kit::make-reader-node empty-context
                                                 :planning
                                                 :begin
                                                 0
                                                 :end
                                                 0
                                                 :depth
                                                 2))
                     (headline-properties
                      (list :reader-closed-range :old
                            :reader-deadline-range :old
                            :reader-scheduled-range :old))
                     (children
                      (org-kit::reader-heading-children
                       empty-context
                       0
                       0
                       1
                       1
                       planning-node
                       headline-properties)))
                (with-soft-assertions
                 (expect
                  (org-kit::reader-heading-first-child-start
                   context 0 (length source) 1)
                  :to-be
                  nil)
                 (expect
                  (org-kit::reader-heading-children empty-context 0 0 1 1)
                  :to-equal
                  nil)
                 (expect (node-type (first children)) :to-be :section)
                 (expect
                  (node-type (first (node-children (first children))))
                  :to-be
                  :planning)
                 (expect headline-properties
                         :to-equal
                         (list :reader-closed-range nil
                               :reader-deadline-range nil
                               :reader-scheduled-range nil))))))

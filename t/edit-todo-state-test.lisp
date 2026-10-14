;;;; t/edit-todo-state-test.lisp
(in-package #:cl-org-kit/test)

(defun edit-todo-state-test-fixture (relative)
  (read-conformance-tree
   (conformance-fixture-path "behavior" relative)))

(defun edit-todo-state-test-heading-path (document path)
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

(defun edit-todo-state-test-first-heading (document)
  (let ((found nil))
    (map-nodes
     (lambda (node)
       (when (and (null found) (eq (node-type node) :headline))
         (setf found node)))
     (document-root document))
    found))

(defun edit-todo-state-test-context (at)
  (make-time-context :reference-time (make-org-time-value at)
                     :zone (resolve-time-zone "UTC")))

(defun edit-todo-state-test-workflow-options (case)
  (let ((keys '(:log-done :log-repeat :repeat-to-state :log-into-drawer)))
    (when (some (lambda (key) (member key case)) keys)
      (apply #'make-org-workflow-options
             (loop for key in keys
                   when (member key case)
                     append (list key (getf case key)))))))

(defun edit-todo-state-test-case-call (document heading case)
  (let* ((workflow-options (edit-todo-state-test-workflow-options case))
         (arguments
           (append
            (list :at (make-org-time-value (getf case :at))
                  :time-context (edit-todo-state-test-context (getf case :at)))
            (when (member :sequence-id case)
              (list :sequence-id (getf case :sequence-id)))
            (when workflow-options
              (list :workflow-options workflow-options))
            (when (member :note case)
              (list :note (getf case :note))))))
    (ecase (getf case :action)
      (:cycle
       (apply #'cycle-heading-todo
              document heading :direction (getf case :direction) arguments))
      (:transition
       (apply #'transition-heading-todo
              document heading (getf case :state) arguments)))))

(defun edit-todo-state-test-span-ranges (spans)
  (mapcar (lambda (span)
            (list (edit-span-start span) (edit-span-end span)))
          spans))

(defun edit-todo-state-test-unpatched-ranges-match-p (source output spans)
  (if (null spans)
      (string= source output)
      (let ((source-cursor 0)
            (output-cursor 0)
            (previous-end 0))
        (dolist (span spans)
          (let* ((start (edit-span-start span))
                 (end (edit-span-end span))
                 (gap (subseq source source-cursor start))
                 (gap-length (length gap)))
            (unless (and (<= previous-end start)
                         (<= start end (length source)))
              (return-from edit-todo-state-test-unpatched-ranges-match-p nil))
            (when (plusp gap-length)
              (let ((position (search gap output :start2 output-cursor)))
                (unless (and position
                             (or (plusp source-cursor)
                                 (= position output-cursor)))
                  (return-from edit-todo-state-test-unpatched-ranges-match-p nil))
                (setf output-cursor (+ position gap-length))))
            (setf source-cursor end
                  previous-end end)))
        (let ((suffix (subseq source source-cursor)))
          (or (zerop (length suffix))
              (let ((position (search suffix output :start2 output-cursor)))
                (and position
                     (= (+ position (length suffix)) (length output)))))))))

(defun edit-todo-state-test-error (thunk)
  (handler-case
      (progn (funcall thunk) nil)
    (error (condition) condition)))

(defun edit-todo-state-test-assert-error (condition reason)
  (with-soft-assertions
    (expect (typep condition 'org-edit-error) :to-be t)
    (when (typep condition 'org-edit-error)
      (expect (org-edit-error-target condition) :to-equal "headline")
      (expect (org-edit-error-reason condition) :to-be reason)
      (expect (stringp (org-kit-error-message condition)) :to-be t)
      (expect (plusp (length (org-kit-error-message condition))) :to-be t))))

(defun edit-todo-state-test-bom-octets (text)
  (let* ((encoded (cl-codec-kit:string-to-octets text :encoding :utf-8))
         (result (make-array (+ 3 (length encoded))
                             :element-type '(unsigned-byte 8))))
    (replace result #(239 187 191))
    (replace result encoded :start1 3)
    result))

(defun edit-todo-state-test-run-transition
    (document heading state at &key workflow-options log-drawer
                                   log-drawer-supplied-p)
  (let ((arguments
          (append (list :at (make-org-time-value at)
                        :time-context (edit-todo-state-test-context at))
                  (when workflow-options
                    (list :workflow-options workflow-options))
                  (when log-drawer-supplied-p
                    (list :log-drawer log-drawer)))))
    (apply #'transition-heading-todo document heading state arguments)))

(describe "TODO state transitions"
  (it "constructs workflow options and transition results with public readers"
    (let* ((defaults (make-org-workflow-options))
           (workflow
             (make-org-workflow-options :log-done :time :log-repeat :note
                                        :repeat-to-state "NEXT"
                                        :log-into-drawer nil
                                        :clock-into-drawer "CLOCKS"))
           (empty-result (make-transition-result))
           (result
             (make-transition-result
              :previous-state "TODO" :new-state "DONE"
              :log-events '("- State \"DONE\"")
              :rescheduled-timestamps '("<2025-03-08 Sat +1w>")
              :closed-timestamp "[2025-03-07 Fri 09:00]")))
      (with-soft-assertions
        (expect (org-workflow-options-p defaults) :to-be t)
        (expect (org-workflow-options-log-done defaults) :to-be nil)
        (expect (org-workflow-options-log-repeat defaults) :to-be nil)
        (expect (org-workflow-options-repeat-to-state defaults) :to-be nil)
        (expect (org-workflow-options-log-into-drawer defaults) :to-equal "LOGBOOK")
        (expect (org-workflow-options-clock-into-drawer defaults) :to-equal "LOGBOOK")
        (expect (list (org-workflow-options-log-done workflow)
                      (org-workflow-options-log-repeat workflow)
                      (org-workflow-options-repeat-to-state workflow)
                      (org-workflow-options-log-into-drawer workflow)
                      (org-workflow-options-clock-into-drawer workflow))
                :to-equal '(:time :note "NEXT" nil "CLOCKS"))
        (expect (transition-result-p empty-result) :to-be t)
        (expect (list (transition-result-previous-state empty-result)
                      (transition-result-new-state empty-result)
                      (transition-result-log-events empty-result)
                      (transition-result-rescheduled-timestamps empty-result)
                      (transition-result-closed-timestamp empty-result))
                :to-equal '(nil nil nil nil nil))
        (expect (transition-result-p result) :to-be t)
        (expect (list (transition-result-previous-state result)
                      (transition-result-new-state result)
                      (transition-result-log-events result)
                      (transition-result-rescheduled-timestamps result)
                      (transition-result-closed-timestamp result))
                :to-equal
                '("TODO" "DONE" ("- State \"DONE\"")
                  ("<2025-03-08 Sat +1w>") "[2025-03-07 Fri 09:00]")))))

  (it "matches every pinned Org 9.8.10 transition result and source edit"
    (let* ((manifest
             (getf (edit-todo-state-test-fixture "cases/d-todo-state.sexp")
                   :cases))
           (oracle (edit-todo-state-test-fixture "expected/d-todo-state.sexp"))
           (oracle-ids (mapcar (lambda (case) (getf case :id)) oracle)))
      (with-soft-assertions
        (expect (length manifest) :to-be 41)
        (expect (length oracle) :to-be 41)
        (expect (mapcar (lambda (case) (getf case :id)) manifest)
                :to-equal oracle-ids)
        (dolist (case manifest)
          (let* ((id (getf case :id))
                 (expected (find id oracle :key (lambda (entry) (getf entry :id))
                                 :test #'string=))
                 (source-octets
                   (read-conformance-octets
                    (conformance-fixture-path
                     "behavior"
                     (concatenate 'string "corpus/" (getf case :source)))))
                 (document (parse-org source-octets))
                 (source (document-text document))
                 (heading (edit-todo-state-test-heading-path
                           document (getf case :heading-path))))
            (expect (plusp (length source-octets)) :to-be t)
            (expect expected :to-be-truthy)
            (expect (node-p heading) :to-be t)
            (when (and expected (node-p heading))
              (multiple-value-bind (edit transition)
                  (edit-todo-state-test-case-call document heading case)
                (let* ((updated (edit-result-updated-document edit))
                       (updated-source (document-text updated))
                       (updated-heading
                         (and (null (getf expected :document-after))
                              (edit-todo-state-test-heading-path
                               updated (getf case :heading-path))))
                       (actual-source-after
                         (if (getf expected :document-after)
                             updated-source
                             (and updated-heading
                                  (subseq updated-source
                                          (node-begin updated-heading)
                                          (node-end updated-heading)))))
                       (expected-source-after
                         (or (getf expected :document-after)
                             (getf expected :source-after)))
                       (spans (edit-result-spans edit)))
                  (expect (edit-result-p edit) :to-be t)
                  (expect (transition-result-p transition) :to-be t)
                  (expect (transition-result-previous-state transition)
                          :to-equal (getf expected :previous-state))
                  (expect (transition-result-new-state transition)
                          :to-equal (getf expected :new-state))
                  (expect (transition-result-log-events transition)
                          :to-equal (getf expected :log-events))
                  (when (string= id "existing-body-bullet-with-state-log")
                    (expect (transition-result-log-events transition)
                            :to-equal
                            '("- State \"NEXT\"       from \"TODO\"       [2025-03-07 Fri 09:00]"))
                    (expect (search "- Existing body bullet" updated-source)
                            :to-be-truthy))
                  (when (string= id "drawerless-state-log-before-indented-body")
                    (expect (transition-result-log-events transition)
                            :to-equal
                            '("- State \"NEXT\"       from \"TODO\"       [2025-03-07 Fri 09:00]"))
                    (expect (search "  Existing indented paragraph text"
                                    updated-source)
                            :to-be-truthy))
                  (when (string= id "done-note-multiline-blank-line")
                    (expect (transition-result-log-events transition)
                            :to-equal
                            (list (format nil "- CLOSING NOTE [2025-03-07 Fri 09:00] ~C~C" (code-char 92) (code-char 92))
                              "  first line"
                              ""
                              "  third line")))
                  (expect (transition-result-rescheduled-timestamps transition)
                          :to-equal (getf expected :rescheduled-timestamps))
                  (expect (transition-result-closed-timestamp transition)
                          :to-equal (getf expected :closed-timestamp))
                  (expect actual-source-after :to-equal expected-source-after)
                  (expect (document-text document) :to-equal source)
                  (expect (write-org document :output-type :octets)
                          :to-equalp source-octets)
                  (expect (mapcar #'edit-span-start spans)
                          :to-equal (sort (mapcar #'edit-span-start spans) #'<))
                  (dolist (span spans)
                    (expect (edit-span-p span) :to-be t)
                    (expect (<= 0 (edit-span-start span)
                                (edit-span-end span) (length source))
                            :to-be t))
                  (loop for tail on spans
                        while (cdr tail)
                        for left = (first tail)
                        for right = (second tail)
                        do (expect (<= (edit-span-end left)
                                       (edit-span-start right))
                                   :to-be t))
                  (expect (edit-todo-state-test-unpatched-ranges-match-p
                           source updated-source spans)
                          :to-be t)
                  (when (string= id "cookie-child-completes")
                    (let* ((todo-start (search "TODO" source
                                               :start2 (node-begin heading)))
                           (cookie-start (search "[0/1]" source))
                           (ranges (edit-todo-state-test-span-ranges spans)))
                      (expect (member
                               (list todo-start (+ todo-start (length "TODO")))
                               ranges :test #'equal)
                              :to-be-truthy)
                      (expect (member
                               (list cookie-start (+ cookie-start (length "[0/1]")))
                               ranges :test #'equal)
                              :to-be-truthy)))))))))))

  (it "keeps Org's native no-drawer behavior separate from the kit LOGBOOK policy"
    (let* ((oracle (edit-todo-state-test-fixture "expected/d-todo-state.sexp"))
           (native (find "marker-enter-time-native-default-drawer" oracle
                         :key (lambda (case) (getf case :id)) :test #'string=))
           (policy-manifest
             (getf (edit-todo-state-test-fixture "cases/d-todo-policy.sexp")
                   :cases))
           (policy (find "built-in-logbook-policy" policy-manifest
                         :key (lambda (case) (getf case :id)) :test #'string=))
           (policy-expected
             (find "built-in-logbook-policy"
                   (edit-todo-state-test-fixture "expected/d-todo-policy.sexp")
                   :key (lambda (case) (getf case :id)) :test #'string=))
           (source-octets
             (read-conformance-octets
              (conformance-fixture-path
               "behavior" (getf policy :source))))
           (document (parse-org source-octets))
           (source (document-text document))
           (heading (edit-todo-state-test-heading-path
                     document (getf policy :heading-path))))
      (with-soft-assertions
        (expect native :to-be-truthy)
        (when native
          (expect (search ":LOGBOOK:" (getf native :source-after)) :to-be nil)
          (expect (search "- State \"TODO\""
                          (getf native :source-after))
                  :to-be-truthy))
        (expect (length policy-manifest) :to-be 2)
        (expect (string= (getf policy-expected :id) (getf policy :id)) :to-be t)
        (multiple-value-bind (edit transition)
            (edit-todo-state-test-case-call document heading policy)
          (let* ((updated (edit-result-updated-document edit))
                 (updated-heading
                   (edit-todo-state-test-heading-path
                    updated (getf policy :heading-path))))
            (expect (subseq (document-text updated)
                            (node-begin updated-heading)
                            (node-end updated-heading))
                    :to-equal (getf policy-expected :source-after))
            (expect (transition-result-new-state transition) :to-equal "TODO")
            (expect (transition-result-log-events transition)
                    :to-equal (getf policy-expected :log-events))
            (expect (search ":LOGBOOK:" (document-text updated)) :to-be-truthy)
            (expect (document-text document) :to-equal source))))))

  (it "removes a nonrepeating schedule on completion by kit policy"
    (let* ((policy-manifest
             (getf (edit-todo-state-test-fixture "cases/d-todo-policy.sexp")
                   :cases))
           (policy
             (find "completion-removes-nonrepeat-scheduled-policy"
                   policy-manifest
                   :key (lambda (case) (getf case :id)) :test #'string=))
           (policy-expected
             (find "completion-removes-nonrepeat-scheduled-policy"
                   (edit-todo-state-test-fixture "expected/d-todo-policy.sexp")
                   :key (lambda (case) (getf case :id)) :test #'string=))
           (source (conformance-fixture-path
                    "behavior" (getf policy :source)))
           (document (parse-org (read-conformance-octets source)))
           (source-text (document-text document))
           (heading (edit-todo-state-test-heading-path
                     document (getf policy :heading-path))))
      (with-soft-assertions
        (expect policy :to-be-truthy)
        (expect policy-expected :to-be-truthy)
        (multiple-value-bind (edit transition)
            (edit-todo-state-test-case-call document heading policy)
          (let* ((updated (edit-result-updated-document edit))
                 (updated-heading
                   (edit-todo-state-test-heading-path
                    updated (getf policy :heading-path))))
            (expect (transition-result-previous-state transition)
                    :to-equal (getf policy-expected :previous-state))
            (expect (transition-result-log-events transition)
                    :to-equal (getf policy-expected :log-events))
            (expect (transition-result-rescheduled-timestamps transition)
                    :to-equal (getf policy-expected :rescheduled-timestamps))
            (expect (transition-result-closed-timestamp transition)
                    :to-equal (getf policy-expected :closed-timestamp))
            (expect (subseq (document-text updated)
                            (node-begin updated-heading)
                            (node-end updated-heading))
                    :to-equal (getf policy-expected :source-after))
            (expect (transition-result-new-state transition) :to-equal "DONE")
            (expect (search "SCHEDULED:" (document-text updated)) :to-be nil)
            (expect (document-text document)
                    :to-equal source-text))))))

  (it "validates error class, reason, target, and message for invalid transitions"
    (let* ((at "2025-03-07T09:00:00")
           (context (edit-todo-state-test-context at))
           (missing-time-source (format nil "#+TODO: TODO | DONE~%* TODO Target~%"))
           (missing-time-document (parse-org missing-time-source))
           (missing-time-heading
             (edit-todo-state-test-heading-path
              missing-time-document '("Target")))
           (missing-time
             (edit-todo-state-test-error
              (lambda ()
                (transition-heading-todo missing-time-document
                                         missing-time-heading nil))))
           (missing-note-source
             (format nil "#+TODO: TODO | DONE(d@)~%* TODO Target~%"))
           (missing-note-document (parse-org missing-note-source))
           (missing-note-heading
             (edit-todo-state-test-heading-path
              missing-note-document '("Target")))
           (missing-note
             (edit-todo-state-test-error
              (lambda ()
                (transition-heading-todo
                 missing-note-document missing-note-heading "DONE"
                 :at (make-org-time-value at) :time-context context))))
           (wrong-sequence-source
             (format nil "#+SEQ_TODO: TODO | DONE~%#+SEQ_TODO: WAITING | CANCELLED~%* TODO Target~%"))
           (wrong-sequence-document (parse-org wrong-sequence-source))
           (wrong-sequence-heading
             (edit-todo-state-test-heading-path
              wrong-sequence-document '("Target")))
           (wrong-sequence
             (edit-todo-state-test-error
              (lambda ()
                (transition-heading-todo
                 wrong-sequence-document wrong-sequence-heading "CANCELLED"
                 :at (make-org-time-value at) :time-context context))))
           (ambiguous-source
             (format nil "#+SEQ_TODO: TODO | DONE~%#+SEQ_TODO: WAITING | CANCELLED~%* Target~%"))
           (ambiguous-document (parse-org ambiguous-source))
           (ambiguous-heading
             (edit-todo-state-test-heading-path ambiguous-document '("Target")))
           (ambiguous
             (edit-todo-state-test-error
              (lambda ()
                (cycle-heading-todo ambiguous-document ambiguous-heading
                                    :direction :next
                                    :at (make-org-time-value at)
                                    :time-context context)))))
      (edit-todo-state-test-assert-error missing-time :invalid-value)
      (edit-todo-state-test-assert-error missing-note :invalid-value)
      (edit-todo-state-test-assert-error wrong-sequence :invalid-value)
      (edit-todo-state-test-assert-error ambiguous :ambiguous-target)
      (with-soft-assertions
        (expect (document-text missing-time-document) :to-equal missing-time-source)
        (expect (document-text missing-note-document) :to-equal missing-note-source)
        (expect (document-text wrong-sequence-document) :to-equal wrong-sequence-source)
        (expect (document-text ambiguous-document) :to-equal ambiguous-source))))

  (it "keeps raw set-heading-todo limited to the TODO token"
    (let* ((crlf (concatenate 'string (string #\Return) (string #\Newline)))
           (source (concatenate 'string
                                "#+TODO: TODO | DONE" crlf
                                "* TODO [#B] Keep title :tag:" crlf
                                "CLOSED: [2025-03-06 Thu]" crlf
                                "Body" crlf))
           (document (parse-org source))
           (heading (edit-todo-state-test-heading-path document
                                                        '("Keep title")))
           (start (search "TODO" source :start2 (node-begin heading)))
           (end (+ start (length "TODO")))
           (expected (concatenate 'string (subseq source 0 start) "DONE"
                                  (subseq source end)))
           (edited (set-heading-todo document heading "DONE")))
      (with-soft-assertions
        (expect (document-text edited) :to-equal expected)
        (expect (document-text document) :to-equal source))))

  (it "uses source-coordinate spans for both the state token and parent cookie"
    (let* ((source-octets
             (read-conformance-octets
              (conformance-fixture-path
               "behavior" "corpus/d-todo-cookie.org")))
           (document (parse-org source-octets))
           (source (document-text document))
           (heading (edit-todo-state-test-heading-path
                     document '("Parent [0/1]" "Child"))))
      (multiple-value-bind (edit transition)
          (transition-heading-todo
           document heading "DONE"
           :at (make-org-time-value "2025-03-07T09:00:00")
           :time-context (edit-todo-state-test-context "2025-03-07T09:00:00")
           :workflow-options
           (make-org-workflow-options :log-done :note
                                      :log-into-drawer "LOGBOOK")
           :note "cookie transition note")
        (let* ((updated (edit-result-updated-document edit))
               (spans (edit-result-spans edit))
               (ranges (edit-todo-state-test-span-ranges spans))
               (todo-start (search "TODO" source :start2 (node-begin heading)))
               (cookie-start (search "[0/1]" source)))
          (with-soft-assertions
            (expect (transition-result-new-state transition) :to-equal "DONE")
            (expect (member
                     (list todo-start (+ todo-start (length "TODO")))
                     ranges :test #'equal)
                    :to-be-truthy)
            (expect (member
                     (list cookie-start (+ cookie-start (length "[0/1]")))
                     ranges :test #'equal)
                    :to-be-truthy)
            (expect (search "* Parent [1/1]" (document-text updated)) :to-be-truthy)
            (expect (search "CLOSED: [2025-03-07 Fri 09:00]"
                            (document-text updated))
                    :to-be-truthy)
            (expect (search ":LOGBOOK:" (document-text updated)) :to-be-truthy)
            (expect (search "- CLOSING NOTE [2025-03-07 Fri 09:00]"
                            (document-text updated))
                    :to-be-truthy)
            (expect (edit-todo-state-test-unpatched-ranges-match-p
                     source (document-text updated) spans)
                    :to-be t)
            (expect (document-text document) :to-equal source))))))

  (it "applies drawer precedence and supplied workflow defaults"
    (let* ((at "2025-03-07T09:00:00")
           (time-context (edit-todo-state-test-context at))
           (global-source
             (format nil "#+TODO: TODO(t!) | DONE~%#+PROPERTY: LOG_INTO_DRAWER DOCUMENT~%* Target~%"))
           (global-document
             (parse-org global-source
                        :settings '(:log-into-drawer "DOCUMENT")))
           (global-heading
             (edit-todo-state-test-heading-path global-document '("Target")))
           (global-result
             (multiple-value-list
              (transition-heading-todo
               global-document global-heading "TODO"
               :at (make-org-time-value at) :time-context time-context)))
           (workflow-source
             (format nil "#+TODO: TODO(t!) | DONE~%#+PROPERTY: LOG_INTO_DRAWER DOCUMENT~%* Target~%"))
           (workflow-document
             (parse-org workflow-source
                        :settings '(:log-into-drawer "DOCUMENT")))
           (workflow-heading
             (edit-todo-state-test-heading-path workflow-document '("Target")))
           (workflow-result
             (multiple-value-list
              (transition-heading-todo
               workflow-document workflow-heading "TODO"
               :at (make-org-time-value at) :time-context time-context
               :workflow-options
               (make-org-workflow-options :log-into-drawer "WORKFLOW"))))
           (direct-source
             (format nil "#+TODO: TODO | DONE(d!)~%* TODO Target~%:PROPERTIES:~%:LOG_INTO_DRAWER: LOCAL~%:END:~%"))
           (direct-document (parse-org direct-source))
           (direct-heading
             (edit-todo-state-test-heading-path direct-document '("Target")))
           (direct-result
             (multiple-value-list
              (edit-todo-state-test-run-transition
               direct-document direct-heading "DONE" at :log-drawer "DIRECT"
               :log-drawer-supplied-p t)))
           (default-options (make-org-workflow-options))
           (override-source
             (format nil
                     "#+TODO: TODO | DONE(d!)~%#+PROPERTY: LOG_INTO_DRAWER DOCUMENT~%#+PROPERTY: LOG_DONE time~%#+PROPERTY: LOG_REPEAT time~%* TODO Closed~%* TODO Repeating~%SCHEDULED: <2025-03-01 Sat +1w>~%"))
           (override-document
             (parse-org override-source
                        :settings '(:log-into-drawer "DOCUMENT"
                                    :log-done time :log-repeat time)))
           (override-headings
             (list (edit-todo-state-test-heading-path override-document '("Closed"))
                   (edit-todo-state-test-heading-path override-document '("Repeating"))))
           (override-results
             (loop for heading in override-headings
                   collect
                   (multiple-value-list
                    (transition-heading-todo
                     override-document heading "DONE"
                     :at (make-org-time-value at) :time-context time-context
                     :workflow-options default-options)))))
      (with-soft-assertions
        (expect (search ":DOCUMENT:" (document-text
                                     (edit-result-updated-document
                                      (first global-result))))
                :to-be-truthy)
        (expect (search ":WORKFLOW:" (document-text
                                      (edit-result-updated-document
                                       (first workflow-result))))
                :to-be-truthy)
        (expect (search ":DOCUMENT:" (document-text
                                      (edit-result-updated-document
                                       (first workflow-result))))
                :to-be nil)
        (expect (search ":DIRECT:" (document-text
                                    (edit-result-updated-document
                                     (first direct-result))))
                :to-be-truthy)
        (expect (search ":LOCAL:" (document-text
                                   (edit-result-updated-document
                                    (first direct-result))))
                :to-be nil)
        (expect (search ":LOGBOOK:" (document-text
                                     (edit-result-updated-document
                                      (first (first override-results)))))
                :to-be-truthy)
        (expect (search ":DOCUMENT:" (document-text
                                      (edit-result-updated-document
                                       (first (first override-results)))))
                :to-be nil)
        (expect (search "CLOSED:" (document-text
                                   (edit-result-updated-document
                                    (first (first override-results)))))
                :to-be nil)
        (expect (search "LAST_REPEAT:" (document-text
                                        (edit-result-updated-document
                                         (first (second override-results)))))
                :to-be nil)
        (expect (org-workflow-options-log-done default-options) :to-be nil)
        (expect (org-workflow-options-log-repeat default-options) :to-be nil)
        (expect (org-workflow-options-log-into-drawer default-options)
                :to-equal "LOGBOOK")
        (expect (document-text global-document) :to-equal global-source)
        (expect (document-text workflow-document) :to-equal workflow-source)
        (expect (document-text direct-document) :to-equal direct-source)
        (expect (document-text override-document) :to-equal override-source))))

  (it "applies DST policies when TODO repeaters land in a gap"
    (let* ((zone (resolve-time-zone "America/New_York"))
           (at (make-org-time-value "2025-03-02T09:00:00"))
           (workflow-options
             (make-org-workflow-options :log-done nil :log-repeat nil
                                        :repeat-to-state "TODO"
                                        :log-into-drawer nil))
           (source
             (format nil
                     "#+TODO: TODO | DONE~%* TODO DST week repeater~%SCHEDULED: <2025-03-02 Sun 02:30 +1w>~%")))
      (dolist (policy '((:compatible . "03:30")
                        (:later . "03:30")
                        (:earlier . "01:30")))
        (let* ((document (parse-org source))
               (original (document-text document))
               (heading
                 (edit-todo-state-test-heading-path
                  document '("DST week repeater")))
               (time-context
                 (make-time-context :reference-time at :zone zone
                                    :dst-policy (car policy))))
          (multiple-value-bind (edit transition)
              (transition-heading-todo
               document heading "DONE" :at at :time-context time-context
               :workflow-options workflow-options)
            (let* ((updated (edit-result-updated-document edit))
                   (timestamp
                     (format nil "<2025-03-09 Sun ~A +1w>" (cdr policy))))
              (with-soft-assertions
                (expect (transition-result-new-state transition)
                        :to-equal "TODO")
                (expect (transition-result-rescheduled-timestamps transition)
                        :to-equal (list timestamp))
                (expect (transition-result-log-events transition) :to-be nil)
                (expect (search (concatenate 'string "SCHEDULED: " timestamp)
                                (document-text updated))
                        :to-be-truthy)
                (expect (document-text document) :to-equal original))))))
      (let* ((document (parse-org source))
             (original (document-text document))
             (heading
               (edit-todo-state-test-heading-path
                document '("DST week repeater")))
             (time-context
               (make-time-context :reference-time at :zone zone
                                  :dst-policy :strict))
             (condition
               (edit-todo-state-test-error
                (lambda ()
                  (transition-heading-todo
                   document heading "DONE" :at at :time-context time-context
                   :workflow-options workflow-options)))))
        (with-soft-assertions
          (expect (typep condition 'org-time-error) :to-be t)
          (when (typep condition 'org-time-error)
            (expect (org-time-error-reason condition) :to-be :dst-gap))
          (expect (document-text document) :to-equal original)))
      (let* ((hour-at (make-org-time-value "2025-03-09T09:00:00"))
             (hour-source
               (format nil
                       "#+TODO: TODO | DONE~%* TODO DST hour repeater~%SCHEDULED: <2025-03-09 Sun 01:30 +1h>~%"))
             (document (parse-org hour-source))
             (original (document-text document))
             (heading
               (edit-todo-state-test-heading-path
                document '("DST hour repeater")))
             (time-context
               (make-time-context :reference-time hour-at :zone zone
                                  :dst-policy :compatible)))
        (multiple-value-bind (edit transition)
            (transition-heading-todo
             document heading "DONE" :at hour-at :time-context time-context
             :workflow-options workflow-options)
          (let* ((updated (edit-result-updated-document edit))
                 (timestamp "<2025-03-09 Sun 03:30 +1h>"))
            (with-soft-assertions
              (expect (transition-result-rescheduled-timestamps transition)
                      :to-equal (list timestamp))
              (expect (search (concatenate 'string "SCHEDULED: " timestamp)
                              (document-text updated))
                      :to-be-truthy)
              (expect (document-text document) :to-equal original)))))))

  (it "requires a zone for date-time repeaters and catches up across a DST overlap by instant"
    (let* ((workflow-options
             (make-org-workflow-options :log-done nil :log-repeat nil
                                        :repeat-to-state "TODO"
                                        :log-into-drawer nil))
           (missing-context-source
             (format nil
                     "#+TODO: TODO | DONE~%* TODO Missing zone repeater~%SCHEDULED: <2025-03-02 Sun 01:30 +1w>~%"))
           (missing-context-document (parse-org missing-context-source))
           (missing-context-heading
             (edit-todo-state-test-heading-path
              missing-context-document '("Missing zone repeater")))
           (missing-context-error
             (edit-todo-state-test-error
              (lambda ()
                (transition-heading-todo
                 missing-context-document missing-context-heading "DONE"
                 :at (make-org-time-value "2025-03-02T09:00:00")
                 :workflow-options workflow-options)))))
      (with-soft-assertions
        (expect (typep missing-context-error 'org-time-error) :to-be t)
        (when (typep missing-context-error 'org-time-error)
          (expect (org-time-error-reason missing-context-error)
                  :to-be :invalid-time))
        (expect (document-text missing-context-document)
                :to-equal missing-context-source))
      (let* ((date-source
               (format nil
                       "#+TODO: TODO | DONE~%* TODO Date-only repeater~%SCHEDULED: <2025-03-02 Sun +1w>~%"))
             (date-document (parse-org date-source))
             (date-original (document-text date-document))
             (date-heading
               (edit-todo-state-test-heading-path
                date-document '("Date-only repeater"))))
        (multiple-value-bind (edit transition)
            (transition-heading-todo
             date-document date-heading "DONE"
             :at (make-org-time-value "2025-03-02")
             :workflow-options workflow-options)
          (let* ((updated (edit-result-updated-document edit))
                 (timestamp "<2025-03-09 Sun +1w>"))
            (with-soft-assertions
              (expect (transition-result-new-state transition) :to-equal "TODO")
              (expect (transition-result-rescheduled-timestamps transition)
                      :to-equal (list timestamp))
              (expect (search (concatenate 'string "SCHEDULED: " timestamp)
                              (document-text updated))
                      :to-be-truthy)
              (expect (document-text date-document) :to-equal date-original)))))
      (let* ((zone (resolve-time-zone "America/New_York"))
             (at (make-org-time-value "2025-11-02T01:45:00-04:00"))
             (time-context
               (make-time-context :reference-time at :zone zone
                                  :dst-policy :later))
             (source
               (format nil
                       "#+TODO: TODO | DONE~%* TODO Fall-back catch-up~%SCHEDULED: <2025-10-26 Sun 01:30 ++1w>~%"))
             (document (parse-org source))
             (original (document-text document))
             (heading
               (edit-todo-state-test-heading-path
                document '("Fall-back catch-up"))))
        (multiple-value-bind (edit transition)
            (transition-heading-todo
             document heading "DONE" :at at :time-context time-context
             :workflow-options workflow-options)
          (let* ((updated (edit-result-updated-document edit))
                 (timestamp "<2025-11-02 Sun 01:30 ++1w>"))
            (with-soft-assertions
              (expect (transition-result-new-state transition) :to-equal "TODO")
              (expect (transition-result-rescheduled-timestamps transition)
                      :to-equal (list timestamp))
              (expect (search (concatenate 'string "SCHEDULED: " timestamp)
                              (document-text updated))
                      :to-be-truthy)
              (expect (transition-result-log-events transition) :to-be nil)
              (expect (document-text document) :to-equal original)))))))

  (it "lets local LOG_DONE and LOG_REPEAT properties override workflow values"
    (let* ((at "2025-03-07T09:00:00")
           (time-context (edit-todo-state-test-context at))
           (workflow-options
             (make-org-workflow-options :log-done nil :log-repeat nil
                                        :log-into-drawer nil))
           (source
             (format nil
                     "#+TODO: TODO | DONE~%* TODO Closed~%:PROPERTIES:~%:LOG_DONE: time~%:END:~%* TODO Repeating~%:PROPERTIES:~%:LOG_REPEAT: time~%:END:~%SCHEDULED: <2025-03-01 Sat +1w>~%"))
           (document (parse-org source))
           (headings
             (list (edit-todo-state-test-heading-path document '("Closed"))
                   (edit-todo-state-test-heading-path document '("Repeating"))))
           (results
             (loop for heading in headings
                   collect
                   (multiple-value-list
                    (transition-heading-todo
                     document heading "DONE"
                     :at (make-org-time-value at) :time-context time-context
                     :workflow-options workflow-options)))))
      (with-soft-assertions
        (expect (search "CLOSED: [2025-03-07 Fri 09:00]"
                        (document-text
                         (edit-result-updated-document
                          (first (first results)))))
                :to-be-truthy)
        (expect (search "LAST_REPEAT: [2025-03-07 Fri 09:00]"
                        (document-text
                         (edit-result-updated-document
                          (first (second results)))))
                :to-be-truthy)
        (expect (transition-result-log-events (second (second results)))
                :to-equal
                '("- State \"DONE\"       from \"TODO\"       [2025-03-07 Fri 09:00]"))
        (expect (document-text document) :to-equal source))))

  (it "keeps an indented body with a drawerless closing note"
    (let* ((at "2025-03-07T09:00:00")
           (source
             (format nil
                     "#+TODO: TODO | DONE~%** TODO Task~%~%  Existing indented paragraph text~%"))
           (expected
             (format nil
                     "#+TODO: TODO | DONE~%** DONE Task~%CLOSED: [2025-03-07 Fri 09:00]~%~%- CLOSING NOTE [2025-03-07 Fri 09:00] ~C~C~%  closing note~%  Existing indented paragraph text~%"
                     (code-char 92) (code-char 92)))
           (document (parse-org source))
           (original (document-text document))
           (heading (edit-todo-state-test-first-heading document))
           (workflow-options
             (make-org-workflow-options :log-done :note :log-into-drawer nil))
           (edit nil)
           (transition nil)
           (condition
             (edit-todo-state-test-error
              (lambda ()
                (multiple-value-setq (edit transition)
                  (transition-heading-todo
                   document heading "DONE"
                   :at (make-org-time-value at)
                   :time-context (edit-todo-state-test-context at)
                   :workflow-options workflow-options
                   :note "closing note"))))))
      (with-soft-assertions
        (expect condition :to-be nil)
        (when condition
          (expect (typep condition 'org-edit-error) :to-be t)
          (when (typep condition 'org-edit-error)
            (expect (org-edit-error-reason condition) :to-be :unexpected-diff)))
        (unless condition
          (expect (transition-result-new-state transition) :to-equal "DONE")
          (expect (transition-result-log-events transition)
                  :to-equal
                  (list (format nil "- CLOSING NOTE [2025-03-07 Fri 09:00] ~C~C"
                                (code-char 92) (code-char 92))
                        "  closing note"))
          (expect (document-text (edit-result-updated-document edit))
                  :to-equal expected))
        (expect (document-text document) :to-equal original))))

  (it "covers public TODO sequence selection and argument validation edges"
    (let* ((at (make-org-time-value "2025-03-07T09:00:00"))
           (no-sequence-document
             (parse-org (format nil "* Target~%")
                        :settings '(:todo-keywords nil)))
           (no-sequence-heading
             (edit-todo-state-test-first-heading no-sequence-document))
           (no-sequence-error
             (edit-todo-state-test-error
              (lambda ()
                (transition-heading-todo
                 no-sequence-document no-sequence-heading "DONE" :at at))))
           (unknown-state-document
             (parse-org (format nil "#+TODO: TODO | DONE~%* Target~%")))
           (unknown-state-heading
             (edit-todo-state-test-first-heading unknown-state-document))
           (unknown-state-error
             (edit-todo-state-test-error
              (lambda ()
                (transition-heading-todo
                 unknown-state-document unknown-state-heading "MISSING" :at at))))
           (sequence-document
             (parse-org (format nil
                                "#+SEQ_TODO[first]: TODO | DONE~%#+SEQ_TODO[second]: WAITING | CANCELLED~%* TODO Target~%")))
           (sequence-heading
             (edit-todo-state-test-first-heading sequence-document))
           (unknown-sequence-error
             (edit-todo-state-test-error
              (lambda ()
                (cycle-heading-todo sequence-document sequence-heading
                                    :direction :next :sequence-id "missing"
                                    :at at))))
           (mismatched-sequence-error
             (edit-todo-state-test-error
              (lambda ()
                (cycle-heading-todo sequence-document sequence-heading
                                    :direction :next :sequence-id "second"
                                    :at at))))
           (invalid-direction-error
             (edit-todo-state-test-error
              (lambda ()
                (cycle-heading-todo sequence-document sequence-heading
                                    :direction :sideways :at at))))
           (missing-cycle-time-error
             (edit-todo-state-test-error
              (lambda ()
                (cycle-heading-todo sequence-document sequence-heading
                                    :direction :next))))
           (invalid-state-type-error
             (edit-todo-state-test-error
              (lambda ()
                (transition-heading-todo sequence-document sequence-heading 7
                                         :at at))))
           (invalid-options-type-error
             (edit-todo-state-test-error
              (lambda ()
                (transition-heading-todo sequence-document sequence-heading "DONE"
                                         :at at :workflow-options :invalid))))
           (invalid-at-type-error
             (edit-todo-state-test-error
              (lambda ()
                (transition-heading-todo sequence-document sequence-heading "DONE"
                                         :at 7)))))
      (edit-todo-state-test-assert-error no-sequence-error :invalid-value)
      (edit-todo-state-test-assert-error unknown-state-error :invalid-value)
      (edit-todo-state-test-assert-error unknown-sequence-error :invalid-value)
      (edit-todo-state-test-assert-error mismatched-sequence-error :invalid-value)
      (edit-todo-state-test-assert-error invalid-direction-error :invalid-value)
      (edit-todo-state-test-assert-error missing-cycle-time-error :invalid-value)
      (edit-todo-state-test-assert-error invalid-state-type-error :invalid-value)
      (edit-todo-state-test-assert-error invalid-options-type-error :invalid-value)
      (with-soft-assertions
        (expect (typep invalid-at-type-error 'org-time-error) :to-be t)))
    (labels ((run (at)
               (let* ((document
                        (parse-org
                         (format nil "#+TODO: TODO | DONE~%* Target~%")))
                      (heading (edit-todo-state-test-first-heading document)))
                 (multiple-value-bind (edit transition)
                     (transition-heading-todo document heading "DONE" :at at)
                   (declare (ignore edit))
                   transition))))
      (with-soft-assertions
        (expect (transition-result-new-state
                 (run "2025-03-07T09:00:00"))
                :to-equal "DONE")
        (expect (transition-result-new-state
                 (run (cl-date-kit:make-local-date 2025 3 7)))
                :to-equal "DONE")
        (expect (transition-result-new-state
                 (run (cl-date-kit:make-local-date-time
                       (cl-date-kit:make-local-date 2025 3 7)
                       (cl-date-kit:make-local-time 9 0 0))))
                :to-equal "DONE")))
    (dolist (source (list (format nil "#+TODO: | DONE~%* Target~%")
                          (format nil "#+TODO: TODO |~%* Target~%")))
      (let* ((document (parse-org source))
             (heading (edit-todo-state-test-first-heading document))
             (direction (if (search "| DONE" source) :next :previous))
             (condition
               (edit-todo-state-test-error
                (lambda ()
                  (cycle-heading-todo document heading :direction direction
                                      :at "2025-03-07")))))
        (edit-todo-state-test-assert-error condition :invalid-value))))

  (it "covers repeat calendar units, range endpoints, restart modes, and limits"
    (labels ((run (source at &key time-context workflow-options limits)
               (let* ((document (if limits
                                    (parse-org source :limits limits)
                                    (parse-org source)))
                      (heading (edit-todo-state-test-first-heading document))
                      (arguments
                        (append (list :at at)
                                (when time-context
                                  (list :time-context time-context))
                                (when workflow-options
                                  (list :workflow-options workflow-options)))))
                 (apply #'transition-heading-todo document heading "DONE"
                        arguments))))
      (let ((monthly-source
              (format nil
                      "#+TODO: TODO | DONE~%* TODO Month~%SCHEDULED: <2025-03-01 Sat +1m>~%"))
            (yearly-source
              (format nil
                      "#+TODO: TODO | DONE~%* TODO Year~%SCHEDULED: <2025-03-01 Sat +1y>~%")))
        (multiple-value-bind (edit transition)
            (run monthly-source (make-org-time-value "2025-03-07"))
          (declare (ignore transition))
          (with-soft-assertions
            (expect (search "<2025-04-01" (document-text
                                            (edit-result-updated-document edit)))
                    :to-be-truthy)))
        (multiple-value-bind (edit transition)
            (run yearly-source (make-org-time-value "2025-03-07"))
          (declare (ignore transition))
          (with-soft-assertions
            (expect (search "<2026-03-01" (document-text
                                            (edit-result-updated-document edit)))
                    :to-be-truthy))))
      (let* ((range-source
               (format nil
                       "#+TODO: TODO | DONE~%* TODO Range~%SCHEDULED: <2025-03-01 Sat 08:00-09:00 +1w>~%"))
             (context (edit-todo-state-test-context "2025-03-07T09:00:00")))
        (multiple-value-bind (edit transition)
            (run range-source (make-org-time-value "2025-03-07T09:00:00")
                 :time-context context)
          (with-soft-assertions
            (expect (transition-result-rescheduled-timestamps transition)
                    :to-equal
                    '("<2025-03-08 Sat 08:00-09:00 +1w>"))
            (expect (search "SCHEDULED: <2025-03-08 Sat 08:00-09:00 +1w>"
                            (document-text
                             (edit-result-updated-document edit)))
                    :to-be-truthy))))
      (let* ((restart-date-source
               (format nil
                       "#+TODO: TODO | DONE~%* TODO Restart date~%SCHEDULED: <2025-03-01 Sat .+1w>~%"))
             (restart-time-source
               (format nil
                       "#+TODO: TODO | DONE~%* TODO Restart time~%SCHEDULED: <2025-03-01 Sat 08:00 .+1w>~%"))
             (context (edit-todo-state-test-context "2025-03-07T09:00:00")))
        (multiple-value-bind (edit transition)
            (run restart-date-source (make-org-time-value "2025-03-07T09:00:00"))
          (declare (ignore transition))
          (with-soft-assertions
            (expect (search "<2025-03-14" (document-text
                                            (edit-result-updated-document edit)))
                    :to-be-truthy)))
        (multiple-value-bind (edit transition)
            (run restart-time-source (make-org-time-value "2025-03-07")
                 :time-context context)
          (declare (ignore transition))
          (with-soft-assertions
            (expect (search "<2025-03-14 Fri 08:00" (document-text
                                                       (edit-result-updated-document edit)))
                    :to-be-truthy))))
      (let* ((offset-source
               (format nil
                       "#+TODO: TODO | DONE~%* TODO Offset restart~%SCHEDULED: <2025-03-01 Sat 08:00 .+1w>~%"))
             (offset-at (make-org-time-value "2025-03-07T09:00:00-05:00"))
             (zone (resolve-time-zone "America/New_York"))
             (context (make-time-context :reference-time offset-at :zone zone)))
        (multiple-value-bind (edit transition)
            (run offset-source offset-at :time-context context)
          (declare (ignore transition))
          (with-soft-assertions
            (expect (search "<2025-03-14 Fri 09:00" (document-text
                                                       (edit-result-updated-document edit)))
                    :to-be-truthy))))
      (let* ((hour-source
               (format nil
                       "#+TODO: TODO | DONE~%* TODO Hour~%SCHEDULED: <2025-03-01 Sat 08:00 +1h>~%"))
             (condition
               (edit-todo-state-test-error
                (lambda ()
                  (run hour-source (make-org-time-value "2025-03-07T09:00:00"))))))
        (with-soft-assertions
          (expect (typep condition 'org-time-error) :to-be t)))
      (let* ((limited-source
               (format nil
                       "#+TODO: TODO | DONE~%* TODO Limited catch-up~%SCHEDULED: <2025-01-01 Wed ++1d>~%"))
             (limits (make-limits :max-future-repeats 1))
             (condition
               (edit-todo-state-test-error
                (lambda ()
                  (run limited-source (make-org-time-value "2025-03-07T09:00:00")
                       :time-context
                       (edit-todo-state-test-context "2025-03-07T09:00:00")
                       :limits limits)))))
        (with-soft-assertions
          (expect (typep condition 'org-resource-limit-error) :to-be t)))
      (let* ((invalid-repeat-source
               (format nil
                       "#+TODO: TODO | DONE~%* TODO Invalid repeat state~%SCHEDULED: <2025-03-01 Sat +1w>~%"))
             (condition
               (edit-todo-state-test-error
                (lambda ()
                  (run invalid-repeat-source (make-org-time-value "2025-03-07")
                       :workflow-options
                       (make-org-workflow-options :repeat-to-state "MISSING"))))))
        (with-soft-assertions
          (expect (typep condition 'org-edit-error) :to-be t)
          (when (typep condition 'org-edit-error)
            (expect (org-edit-error-reason condition) :to-be :invalid-value))))))

  (it "covers date-time repeat units, warnings, and external timestamp ranges"
    (labels ((run (source at context)
               (let* ((document (parse-org source))
                      (heading (edit-todo-state-test-first-heading document)))
                 (apply #'transition-heading-todo document heading "DONE"
                        :at at
                        (when context (list :time-context context))))))
      (let* ((at (make-org-time-value "2025-03-07T09:00:00"))
             (context (edit-todo-state-test-context "2025-03-07T09:00:00")))
        (with-soft-assertions
          (dolist (case '(("+1d" "<2025-03-02 Sun 08:00 +1d>")
                          ("+1m" "<2025-04-01 Tue 08:00 +1m>")
                          ("+1y" "<2026-03-01 Sun 08:00 +1y>")))
            (let* ((source
                     (format nil
                             "#+TODO: TODO | DONE~%* TODO Unit~%SCHEDULED: <2025-03-01 Sat 08:00 ~A>~%"
                             (first case)))
                   (expected (second case)))
              (multiple-value-bind (edit transition) (run source at context)
                (expect (transition-result-rescheduled-timestamps transition)
                        :to-equal (list expected))
                (expect (search expected
                                (document-text (edit-result-updated-document edit)))
                        :to-be-truthy))))
          (dolist (warning '("--2h" "-2d" "--2w" "-2m" "--2y"))
            (let* ((timestamp
                     (format nil "<2025-03-01 Sat 08:00 +1d ~A>" warning))
                   (expected
                     (format nil "<2025-03-02 Sun 08:00 +1d ~A>" warning))
                   (source
                     (format nil
                             "#+TODO: TODO | DONE~%* TODO Warning~%SCHEDULED: ~A~%"
                             timestamp)))
              (multiple-value-bind (edit transition) (run source at context)
                (expect (transition-result-rescheduled-timestamps transition)
                        :to-equal (list expected))
                (expect (search expected
                                (document-text (edit-result-updated-document edit)))
                        :to-be-truthy))))
          (dolist (case
                    (list
                     (list
                      (format nil
                              "#+TODO: TODO | DONE~%* TODO Active range~%SCHEDULED: <2025-03-01 Sat +1w>--<2025-03-08 Sat>~%")
                      "<2025-03-08 Sat +1w>--<2025-03-15 Sat>")
                     (list
                      (format nil
                              "#+TODO: TODO | DONE~%* TODO Inactive range~%History [2025-03-01 Sat +1w]--[2025-03-08 Sat]~%")
                      "[2025-03-08 Sat +1w]--[2025-03-15 Sat]")))
            (multiple-value-bind (edit transition)
                (run (first case) (make-org-time-value "2025-03-07") nil)
              (expect (transition-result-rescheduled-timestamps transition)
                      :to-equal (list (second case)))
              (expect (search (second case)
                              (document-text (edit-result-updated-document edit)))
                      :to-be-truthy)))))))

  (it "normalizes boolean and string logging modes and removes completed schedules"
    (let* ((at (make-org-time-value "2025-03-07T09:00:00"))
           (source (format nil "#+TODO: TODO | DONE~%* TODO Target~%"))
           (document (parse-org source))
           (heading (edit-todo-state-test-first-heading document))
           (time-result
             (multiple-value-list
              (transition-heading-todo
               document heading "DONE" :at at
               :workflow-options (make-org-workflow-options :log-done t))))
           (note-result
             (multiple-value-list
              (transition-heading-todo
               document heading "DONE" :at at
               :workflow-options (make-org-workflow-options :log-done "NOTE")
               :note "finished")))
           (off-result
             (multiple-value-list
              (transition-heading-todo
               document heading "DONE" :at at
               :workflow-options (make-org-workflow-options :log-done "OFF"))))
           (invalid-mode
             (edit-todo-state-test-error
              (lambda ()
                (transition-heading-todo
                 document heading "DONE" :at at
                 :workflow-options (make-org-workflow-options :log-done 7)))))
           (missing-note
             (edit-todo-state-test-error
              (lambda ()
                (transition-heading-todo
                 document heading "DONE" :at at
                 :workflow-options (make-org-workflow-options :log-done "NOTE"))))))
      (with-soft-assertions
        (expect (transition-result-closed-timestamp (second time-result))
                :to-equal "[2025-03-07 Fri 09:00]")
        (expect (transition-result-log-events (second note-result)) :to-be-truthy)
        (expect (transition-result-closed-timestamp (second note-result))
                :to-equal "[2025-03-07 Fri 09:00]")
        (expect (transition-result-closed-timestamp (second off-result)) :to-be nil)
        (expect (transition-result-log-events (second off-result)) :to-be nil))
      (edit-todo-state-test-assert-error invalid-mode :invalid-value)
      (edit-todo-state-test-assert-error missing-note :invalid-value))
    (let* ((source
             (format nil
                     "#+TODO: TODO | DONE~%* TODO Scheduled~%SCHEDULED: <2025-03-01 Sat>~%"))
           (document (parse-org source))
           (heading (edit-todo-state-test-first-heading document)))
      (multiple-value-bind (edit transition)
          (transition-heading-todo document heading "DONE"
                                   :at (make-org-time-value "2025-03-07"))
        (let ((updated (document-text (edit-result-updated-document edit))))
          (with-soft-assertions
            (expect (transition-result-new-state transition) :to-equal "DONE")
            (expect (search "SCHEDULED:" updated) :to-be nil)))))
    (let* ((source
             (format nil
                     "#+TODO: TODO | DONE~%* TODO Symbol repeat state~%SCHEDULED: <2025-03-01 Sat +1w>~%"))
           (document (parse-org source))
           (heading (edit-todo-state-test-first-heading document)))
      (multiple-value-bind (edit transition)
          (transition-heading-todo
           document heading "DONE" :at (make-org-time-value "2025-03-07")
           :workflow-options
           (make-org-workflow-options :repeat-to-state :TODO))
        (with-soft-assertions
          (expect (transition-result-new-state transition) :to-equal "TODO")
          (expect (search "SCHEDULED: <2025-03-08 Sat +1w>"
                          (document-text (edit-result-updated-document edit)))
                  :to-be-truthy)))))

  (it "uses the first TODO state when repeat-to-state is nil"
    (let* ((source
             (format nil
                     "#+TODO: TODO | DONE~%* TODO Nil repeat state~%SCHEDULED: <2025-03-01 Sat +1w>~%"))
           (document (parse-org source))
           (heading (edit-todo-state-test-first-heading document)))
      (multiple-value-bind (edit transition)
          (transition-heading-todo
           document heading "DONE" :at (make-org-time-value "2025-03-07")
           :workflow-options (make-org-workflow-options :repeat-to-state "nil"))
        (with-soft-assertions
          (expect (transition-result-new-state transition) :to-equal "TODO")
          (expect (search "SCHEDULED: <2025-03-08 Sat +1w>"
                          (document-text (edit-result-updated-document edit)))
                  :to-be-truthy)))))

  (it "updates stale body and headline cookies alongside repeated title timestamps"
    (let* ((source
             (format nil
                     "#+TODO: TODO | DONE~%* TODO [1/1] <2025-03-01 Sat +1w> Parent~%** TODO Child~%"))
           (document (parse-org source))
           (heading (edit-todo-state-test-first-heading document)))
      (multiple-value-bind (edit transition)
          (transition-heading-todo document heading "DONE"
                                   :at (make-org-time-value "2025-03-07"))
        (declare (ignore transition))
        (with-soft-assertions
          (expect (search "* TODO [0/1] <2025-03-08 Sat +1w> Parent"
                          (document-text (edit-result-updated-document edit)))
                  :to-be-truthy))))
    (let* ((source
             (format nil
                     "#+TODO: TODO | DONE~%* TODO Checkbox cookie~%  - Parent [1/1]~%    - [ ] unchecked~%"))
           (document (parse-org source))
           (heading (edit-todo-state-test-first-heading document)))
      (multiple-value-bind (edit transition)
          (transition-heading-todo document heading "DONE"
                                   :at (make-org-time-value "2025-03-07"))
        (declare (ignore transition))
        (with-soft-assertions
          (expect (search "Parent [0/1]"
                          (document-text (edit-result-updated-document edit)))
                  :to-be-truthy))))
    (let* ((source
             (format nil
                     "#+TODO: TODO | DONE~%* TODO Shifted cookie~%  - Parent [1/1]~%    - [ ] unchecked~%"))
           (document (parse-org source))
           (heading (edit-todo-state-test-first-heading document)))
      (multiple-value-bind (edit transition)
          (transition-heading-todo
           document heading "DONE"
           :at (make-org-time-value "2025-03-07T09:00:00")
           :workflow-options (make-org-workflow-options :log-done :time))
        (let ((updated (document-text (edit-result-updated-document edit))))
          (with-soft-assertions
            (expect (transition-result-closed-timestamp transition)
                    :to-equal "[2025-03-07 Fri 09:00]")
            (expect (search "Parent [0/1]" updated) :to-be-truthy)))))
    (let* ((source (format nil "#+TODO: TODO | DONE~%* TODO Note cookie~%"))
           (document (parse-org source))
           (heading (edit-todo-state-test-first-heading document)))
      (multiple-value-bind (edit transition)
          (transition-heading-todo
           document heading "DONE" :at (make-org-time-value "2025-03-07")
           :workflow-options (make-org-workflow-options
                              :log-done :note :log-into-drawer nil)
           :note "Keep this spelling [1/1]")
        (declare (ignore transition))
        (with-soft-assertions
          (expect (search "Keep this spelling [1/1]"
                          (document-text (edit-result-updated-document edit)))
                  :to-be-truthy)))))

  (it "handles offset restarts and rejects a repeated timestamp year beyond its format"
    (let* ((source
             (format nil
                     "#+TODO: TODO | DONE~%* TODO Offset restart~%SCHEDULED: <2025-03-01 Sat .+1w>~%"))
           (document (parse-org source))
           (heading (edit-todo-state-test-first-heading document))
           (at (make-org-time-value "2025-03-07T09:00:00-05:00"))
           (context (make-time-context
                     :reference-time at
                     :zone (resolve-time-zone "America/New_York"))))
      (multiple-value-bind (edit transition)
          (transition-heading-todo document heading "DONE" :at at
                                   :time-context context)
        (declare (ignore transition))
        (with-soft-assertions
          (expect (search "SCHEDULED: <2025-03-14 Fri .+1w>"
                          (document-text (edit-result-updated-document edit)))
                  :to-be-truthy))))
    (let* ((source
             (format nil
                     "#+TODO: TODO | DONE~%* TODO Beyond formatter year~%SCHEDULED: <9999-12-31 Fri +1y>~%"))
           (document (parse-org source))
           (heading (edit-todo-state-test-first-heading document))
           (condition
             (edit-todo-state-test-error
              (lambda ()
                (transition-heading-todo
                 document heading "DONE"
                 :at (make-org-time-value "2025-03-07"))))))
      (with-soft-assertions
        (expect (typep condition 'org-time-error) :to-be t))))

  (it "covers transition defaults and timestamp patch boundaries"
    (let* ((patch (org-kit::edit-make-patch 10 20 "abcde" "headline"))
           (offset-at (make-org-time-value "2025-03-07T09:00:00-05:00"))
           (restarted
             (org-kit::edit-todo-restart-value
              (make-org-time-value "2025-03-01") offset-at))
           (restarted-time
             (org-kit::edit-todo-restart-value
              (make-org-time-value "2025-03-01T08:00:00")
              (make-org-time-value "2025-03-07")))
           (kept-date-time
             (org-kit::edit-todo-restart-value
              (make-org-time-value "2025-03-01T08:00:00")
              (make-org-time-value "2025-03-07T09:00:00")))
           (kept-date
             (org-kit::edit-todo-restart-value
              (make-org-time-value "2025-03-01")
              (make-org-time-value "2025-03-07")))
           (unformattable
             (org-kit::%make-org-time-value
              :kind :date :iso8601 "10000-01-01"
              :year 10000 :month 1 :day 1))
           (format-error
             (edit-todo-state-test-error
              (lambda ()
                (org-kit::edit-todo-format-point-timestamp unformattable))))
           (missing-at-document
             (parse-org (format nil "#+TODO: TODO | DONE~%* TODO Missing time~%")))
           (missing-at-heading
             (edit-todo-state-test-first-heading missing-at-document))
           (missing-at-error
             (edit-todo-state-test-error
              (lambda ()
                (transition-heading-todo
                 missing-at-document missing-at-heading "DONE"))))
           (remove-present-p
             (org-kit::edit-todo-final-planning-present-p
              nil '((:scheduled :action :remove)) :scheduled)))
      (with-soft-assertions
        (expect (org-time-value-iso8601 restarted) :to-equal "2025-03-07")
        (expect (org-time-value-iso8601 restarted-time)
                :to-equal "2025-03-07T08:00:00")
        (expect (org-time-value-iso8601 kept-date-time)
                :to-equal "2025-03-07T09:00:00")
        (expect (org-time-value-iso8601 kept-date)
                :to-equal "2025-03-07")
        (expect (typep format-error 'org-time-error) :to-be t)
        (expect (typep missing-at-error 'org-edit-error) :to-be t)
        (when (typep missing-at-error 'org-edit-error)
          (expect (org-edit-error-reason missing-at-error) :to-be :invalid-value))
        (expect remove-present-p :to-be nil)
        (expect (multiple-value-list
                 (org-kit::edit-todo-staged-offset-to-original
                  12 (list patch) :start))
                :to-equal '(nil nil))
        (expect (multiple-value-list
                 (org-kit::edit-todo-staged-offset-to-original
                  10 (list patch) :end))
                :to-equal '(10 t))
        (expect (multiple-value-list
                 (org-kit::edit-todo-staged-offset-to-original
                  12 (list patch) :end))
                :to-equal '(nil nil)))))

  (it "uses the default transition keywords when omitted"
    (let* ((source (format nil "#+TODO: TODO | DONE(d!)~%* TODO Defaults~%"))
           (document (parse-org source))
           (heading (edit-todo-state-test-first-heading document))
           (missing-at
             (edit-todo-state-test-error
              (lambda ()
                (transition-heading-todo document heading "DONE")))))
      (edit-todo-state-test-assert-error missing-at :invalid-value)
      (multiple-value-bind (edit transition)
          (transition-heading-todo
           document heading "DONE" :at (make-org-time-value "2025-03-07T09:00:00")
           :workflow-options (make-org-workflow-options :log-done :time))
        (let ((updated (document-text (edit-result-updated-document edit))))
          (with-soft-assertions
            (expect (transition-result-log-events transition) :to-be-truthy)
            (expect (search ":LOGBOOK:" updated) :to-be-truthy))))))

  (it "restarts a date-time repeater from a date-only completion time"
    (let* ((source
             (format nil
                     "#+TODO: TODO | DONE~%* TODO Restart date-time~%SCHEDULED: <2025-03-01 Sat 08:00 .+1w>~%"))
           (document (parse-org source))
           (heading (edit-todo-state-test-first-heading document)))
      (multiple-value-bind (edit transition)
          (transition-heading-todo
           document heading "DONE" :at (make-org-time-value "2025-03-07")
           :time-context
           (edit-todo-state-test-context "2025-03-07T09:00:00"))
        (let ((updated (document-text (edit-result-updated-document edit))))
          (with-soft-assertions
            (expect (transition-result-rescheduled-timestamps transition)
                    :to-equal '("<2025-03-14 Fri 08:00 .+1w>"))
            (expect (search "SCHEDULED: <2025-03-14 Fri 08:00 .+1w>" updated)
                    :to-be-truthy))))))

  (it "restarts a date-only repeater from a date-only completion time"
    (let* ((source
             (format nil
                     "#+TODO: TODO | DONE~%* TODO Restart date~%SCHEDULED: <2025-03-01 Sat .+1w>~%"))
           (document (parse-org source))
           (heading (edit-todo-state-test-first-heading document)))
      (multiple-value-bind (edit transition)
          (transition-heading-todo
           document heading "DONE" :at (make-org-time-value "2025-03-07"))
        (let ((updated (document-text (edit-result-updated-document edit))))
          (with-soft-assertions
            (expect (transition-result-new-state transition) :to-equal "TODO")
            (expect (transition-result-rescheduled-timestamps transition)
                    :to-equal '("<2025-03-14 Fri .+1w>"))
            (expect (search "SCHEDULED: <2025-03-14 Fri .+1w>" updated)
                    :to-be-truthy))))))

  (it "formats a parsed timestamp without a repeater suffix"
    (let* ((source
             (format nil
                     "#+TODO: TODO | DONE~%* TODO Timestamp view~%SCHEDULED: <2025-03-01 Sat>~%"))
           (document (parse-org source))
           (heading (edit-todo-state-test-first-heading document))
           (view (timestamp-view (node-property heading :scheduled)))
           (start (org-timestamp-view-start view)))
      (with-soft-assertions
        (expect (org-kit::edit-todo-repeater-suffix view) :to-equal "")
        (expect (org-kit::edit-todo-format-timestamp view start nil nil)
                :to-equal "<2025-03-01 Sat>"))))

  (it "removes CLOSED and a nonrepeating schedule when a body repeater fires"
    (let* ((source
             (format nil
                     "#+TODO: TODO | DONE~%* DONE Keep schedule~%SCHEDULED: <2025-03-02 Sun> CLOSED: [2025-03-01 Sat]~%Body repeater <2025-03-03 Mon +1w>~%"))
           (document (parse-org source))
           (heading (edit-todo-state-test-first-heading document)))
      (multiple-value-bind (edit transition)
          (transition-heading-todo
           document heading "DONE" :at (make-org-time-value "2025-03-07")
           :workflow-options (make-org-workflow-options :repeat-to-state "TODO"))
        (let ((updated (document-text (edit-result-updated-document edit))))
          (with-soft-assertions
            (expect (transition-result-new-state transition) :to-equal "TODO")
            (expect (transition-result-rescheduled-timestamps transition)
                    :to-equal '("<2025-03-10 Mon +1w>"))
            (expect (search "CLOSED:" updated) :to-be nil)
            (expect (search "SCHEDULED:" updated) :to-be nil)
            (expect (search "Body repeater <2025-03-10 Mon +1w>" updated)
                    :to-be-truthy))))))

  (it "inserts a drawerless done log at EOF without adding a final newline"
    (let* ((source (format nil "#+TODO: TODO | DONE(d!)~%* TODO Log at EOF"))
           (document (parse-org source))
           (heading (edit-todo-state-test-first-heading document))
           (options (make-org-workflow-options :log-into-drawer nil)))
      (multiple-value-bind (edit transition)
          (transition-heading-todo
           document heading "DONE" :at (make-org-time-value "2025-03-07T09:00:00")
           :workflow-options options)
        (let* ((updated (document-text (edit-result-updated-document edit)))
               (last-character (char updated (1- (length updated)))))
          (with-soft-assertions
            (expect (transition-result-log-events transition)
                    :to-equal
                    '("- State \"DONE\"       from \"TODO\"       [2025-03-07 Fri 09:00]"))
            (expect (search "- State \"DONE\"" updated) :to-be-truthy)
            (expect (search ":LOGBOOK:" updated) :to-be nil)
            (expect updated
                    :to-equal
                    (format nil
                            "#+TODO: TODO | DONE(d!)~%* DONE Log at EOF~%- State \"DONE\"       from \"TODO\"       [2025-03-07 Fri 09:00]"))
            (expect (char= last-character #\Newline) :to-be nil))))))

  (it "transitions a state-less heading to no TODO state"
    (let* ((source (format nil "#+TODO: TODO | DONE~%* Plain heading~%"))
           (document (parse-org source))
           (heading (edit-todo-state-test-first-heading document)))
      (multiple-value-bind (edit transition)
          (transition-heading-todo
           document heading nil :at (make-org-time-value "2025-03-07"))
        (with-soft-assertions
          (expect (transition-result-previous-state transition) :to-be nil)
          (expect (transition-result-new-state transition) :to-be nil)
          (expect (edit-result-spans edit) :to-be nil)
          (expect (document-text (edit-result-updated-document edit))
                  :to-equal source)))))

  (it "does not log an unchanged TODO state with a logging marker"
    (let* ((source
             (format nil "#+TODO: TODO(t!) | DONE(d!)~%* TODO Unchanged~%"))
           (document (parse-org source))
           (heading (edit-todo-state-test-first-heading document)))
      (multiple-value-bind (edit transition)
          (transition-heading-todo
           document heading "TODO" :at (make-org-time-value "2025-03-07"))
        (with-soft-assertions
          (expect (transition-result-new-state transition) :to-equal "TODO")
          (expect (transition-result-log-events transition) :to-be nil)
          (expect (document-text (edit-result-updated-document edit))
                  :to-equal source)))))

  (it "updates scheduled planning without closing a property drawer"
    (let* ((source (format nil "#+TODO: TODO | DONE~%* TODO Planning helper~%"))
           (document (parse-org source))
           (heading (edit-todo-state-test-first-heading document))
           (value
             (org-kit::edit-properties-planning-value
              document :scheduled "<2025-03-08 Sat>" heading))
           (expected (org-kit::edit-canonical-document document))
           (changes
             (list (cons :scheduled
                         (list :action :set :canonical-value value))))
           (actual
             (org-kit::edit-todo-canonical-planning-changes
              expected document heading changes heading))
           (updated-document
             (parse-org (format nil
                                "#+TODO: TODO | DONE~%* TODO Planning helper~%SCHEDULED: <2025-03-08 Sat>~%"))))
      (with-soft-assertions
        (expect actual
                :to-equal (org-kit::edit-canonical-document updated-document)))))

  (it "keeps inline logs before generic drawers"
    (let* ((source
             (format nil
                     "#+TODO: TODO | DONE(d!)~%* TODO Generic drawer~%:NOTES:~%Archive text~%:END:~%"))
           (document (parse-org source))
           (heading (edit-todo-state-test-first-heading document)))
      (multiple-value-bind (edit transition)
          (transition-heading-todo
           document heading "DONE" :at (make-org-time-value "2025-03-07T09:00:00")
           :workflow-options (make-org-workflow-options
                              :log-done :time :log-into-drawer nil))
        (let* ((updated (document-text (edit-result-updated-document edit)))
               (log-position (search "- State" updated))
               (drawer-position (search ":NOTES:" updated))
               (body-position (search "Archive text" updated)))
          (with-soft-assertions
            (expect (transition-result-log-events transition) :to-be-truthy)
            (expect (< log-position drawer-position) :to-be t)
            (expect (< drawer-position body-position) :to-be t))))))

  (it "matches planned repeater nodes by their source range"
    (let* ((source
             (format nil
                     "#+TODO: TODO | DONE~%* TODO Repeating~%SCHEDULED: <2025-03-01 Sat +1w>~%"))
           (document (parse-org source))
           (heading (edit-todo-state-test-first-heading document))
           (section (find :section (node-children heading) :key #'node-type))
           (planning (find :planning (node-children section) :key #'node-type))
           (heading-timestamp (node-property heading :scheduled))
           (planning-timestamp (node-property planning :scheduled)))
      (multiple-value-bind (edit transition)
          (transition-heading-todo document heading "DONE"
                                   :at (make-org-time-value "2025-03-07"))
        (let ((updated (document-text (edit-result-updated-document edit))))
          (with-soft-assertions
            (expect (node-p heading-timestamp) :to-be t)
            (expect (node-p planning-timestamp) :to-be t)
            (expect (eq heading-timestamp planning-timestamp) :to-be nil)
            (expect (node-begin heading-timestamp)
                    :to-equal (node-begin planning-timestamp))
            (expect (node-end heading-timestamp)
                    :to-equal (node-end planning-timestamp))
            (expect (transition-result-rescheduled-timestamps transition)
                    :to-equal '("<2025-03-08 Sat +1w>"))
            (expect (search "SCHEDULED: <2025-03-08 Sat +1w>" updated)
                    :to-be-truthy)
            (expect (document-text document) :to-equal source))))))

  (it "covers workflow modes, drawer values, and CLOSED planning edits"
    (let* ((at (make-org-time-value "2025-03-07T09:00:00"))
           (context (edit-todo-state-test-context "2025-03-07T09:00:00")))
      (labels ((run (source state options &key log-drawer log-drawer-supplied-p)
                 (let* ((document (parse-org source))
                        (heading (edit-todo-state-test-first-heading document))
                        (arguments
                          (append (list :at at :time-context context
                                        :workflow-options options)
                                  (when log-drawer-supplied-p
                                    (list :log-drawer log-drawer)))))
                   (apply #'transition-heading-todo document heading state
                          arguments))))
        (let* ((source (format nil "#+TODO: TODO | DONE(d!)~%* TODO Target~%"))
               (invalid-mode
                 (edit-todo-state-test-error
                  (lambda ()
                    (run source "DONE"
                         (make-org-workflow-options :log-done "unknown")))))
               (invalid-drawer
                 (edit-todo-state-test-error
                  (lambda ()
                    (run source "DONE" (make-org-workflow-options :log-done :time)
                         :log-drawer 7 :log-drawer-supplied-p t))))
               (time-mode (make-org-workflow-options :log-done :time
                                                     :log-into-drawer "t"))
               (inline-mode (make-org-workflow-options :log-done :time
                                                      :log-into-drawer "nil")))
          (edit-todo-state-test-assert-error invalid-mode :invalid-value)
          (edit-todo-state-test-assert-error invalid-drawer :invalid-value)
          (multiple-value-bind (edit transition)
              (run source "DONE" time-mode)
            (with-soft-assertions
              (expect (transition-result-closed-timestamp transition)
                      :to-equal "[2025-03-07 Fri 09:00]")
              (expect (search ":LOGBOOK:" (document-text
                                             (edit-result-updated-document edit)))
                      :to-be-truthy)))
          (multiple-value-bind (edit transition)
              (run source "DONE" inline-mode)
            (with-soft-assertions
              (expect (transition-result-log-events transition) :to-be-truthy)
              (expect (search "- State" (document-text
                                           (edit-result-updated-document edit)))
                      :to-be-truthy)))))
      (dolist (source
               (list (format nil "#+TODO: TODO | DONE~%* TODO Existing CLOSED~%CLOSED: [2025-03-01 Sat]~%")
                     (format nil "#+TODO: TODO | DONE~%* TODO Existing planning~%SCHEDULED: <2025-03-01 Sat>~%")
                     (format nil "#+TODO: TODO | DONE~%* TODO Property drawer~%:PROPERTIES:~%:ID: task~%:END:~%")))
        (let* ((document (parse-org source))
               (heading (edit-todo-state-test-first-heading document)))
          (multiple-value-bind (edit transition)
              (transition-heading-todo
               document heading "DONE" :at at :time-context context
               :workflow-options (make-org-workflow-options :log-done :time))
            (let ((updated (document-text (edit-result-updated-document edit))))
              (with-soft-assertions
                (expect (transition-result-closed-timestamp transition)
                        :to-equal "[2025-03-07 Fri 09:00]")
                (expect (search "CLOSED: [2025-03-07 Fri 09:00]" updated)
                        :to-be-truthy))))))
      (let* ((source
               (format nil
                       "#+TODO: TODO | DONE~%* TODO Repeating with unclosed drawer~%SCHEDULED: <2025-03-01 Sat +1w>~%:PROPERTIES:~%:ID: task~%"))
             (document (parse-org source))
             (heading (edit-todo-state-test-first-heading document))
             (condition
               (edit-todo-state-test-error
                (lambda ()
                  (transition-heading-todo
                   document heading "DONE" :at at :time-context context
                   :workflow-options (make-org-workflow-options
                                      :log-repeat :time))))))
        (edit-todo-state-test-assert-error condition :uneditable-structure))))

  (it "keeps inline state logs before an indented body paragraph"
    (let* ((at (make-org-time-value "2025-03-07T09:00:00"))
           (source
             (format nil
                     "#+TODO: TODO | DONE(d!)~%* TODO Inline log~%~%  Existing body paragraph~%"))
           (document (parse-org source))
           (heading (edit-todo-state-test-first-heading document))
           (options (make-org-workflow-options :log-done :time
                                               :log-into-drawer nil)))
      (multiple-value-bind (edit transition)
          (transition-heading-todo document heading "DONE" :at at
                                   :workflow-options options)
        (let ((updated (document-text (edit-result-updated-document edit))))
          (with-soft-assertions
            (expect (transition-result-log-events transition) :to-be-truthy)
            (expect (search "- State \"DONE\"" updated) :to-be-truthy)
            (expect (search "Existing body paragraph" updated) :to-be-truthy)
            (expect (document-text document) :to-equal source))))))

  (it "preserves UTF-8 BOM bytes, Japanese text, and CRLF during transition"
    (let* ((crlf (concatenate 'string (string #\Return) (string #\Newline)))
           (source (concatenate 'string
                                "#+TODO: TODO | DONE" crlf
                                "* TODO 日本語" crlf
                                "本文" crlf))
           (source-octets (edit-todo-state-test-bom-octets source))
           (document (parse-org source-octets :encoding :utf-8))
           (heading (edit-todo-state-test-first-heading document))
           (expected-text (concatenate 'string
                                       "#+TODO: TODO | DONE" crlf
                                       "* DONE 日本語" crlf
                                       "本文" crlf))
           (workflow-options
             (make-org-workflow-options :log-done nil :log-repeat nil
                                        :log-into-drawer nil)))
      (multiple-value-bind (edit transition)
          (transition-heading-todo
           document heading "DONE"
           :at (make-org-time-value "2025-03-07T09:00:00")
           :time-context (edit-todo-state-test-context "2025-03-07T09:00:00")
           :workflow-options workflow-options)
        (let* ((updated (edit-result-updated-document edit))
               (todo-start (search "TODO" source :start2 (node-begin heading)))
               (span (first (edit-result-spans edit))))
          (with-soft-assertions
            (expect (transition-result-new-state transition) :to-equal "DONE")
            (expect (write-org updated :output-type :octets)
                    :to-equalp (edit-todo-state-test-bom-octets expected-text))
            (expect (document-encoding updated) :to-be :utf-8)
            (expect (document-bom-p updated) :to-be t)
            (expect (edit-span-start span) :to-be todo-start)
            (expect (edit-span-end span) :to-be (+ todo-start (length "TODO")))
            (expect (document-text document) :to-equal source)))))))

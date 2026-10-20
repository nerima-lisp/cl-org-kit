;;;; t/edit-subtree-clone-test.lisp
(in-package #:cl-org-kit/test)

(defun edit-subtree-clone-test-headlines (document)
  (let ((headlines nil))
    (map-nodes (lambda (node)
                 (when (eq (node-type node) :headline)
                   (push node headlines)))
               (document-root document))
    (nreverse headlines)))

(defun edit-subtree-clone-test-headline (document title)
  (find title (edit-subtree-clone-test-headlines document)
        :key (lambda (node) (node-property node :raw-value))
        :test #'string=))

(defun edit-subtree-clone-test-random-source (&optional (start 0))
  (let ((value start))
    (lambda ()
      (prog1 (mod value 256)
        (incf value)))))

(defun edit-subtree-clone-test-condition (function)
  (handler-case
      (progn (funcall function) nil)
    (error (condition) condition)))

(defun edit-subtree-clone-test-id (headline)
  (let ((property (first (org-kit::org-id-target-id-properties headline))))
    (and property (node-property property :value))))

(describe "subtree cloning with shifted timestamps"
  (it "matches Org output while preserving the original and peer source"
    (let* ((source
             (format nil
                     "#+TITLE: untouched λ~%* Task~%SCHEDULED: <2026-03-02>~%DEADLINE: [2026-03-04]~%body <2026-03-05> [2026-03-07]~%:LOGBOOK:~%CLOCK: [2026-03-02 09:00]--[2026-03-02 10:00] =>  1:00~%:END:~%** Child~%<2026-03-08>~%* Peer~%untouched~%"))
           (prefix (format nil "#+TITLE: untouched λ~%"))
           (suffix (format nil "* Peer~%untouched~%"))
           (expected
             (concatenate
              'string prefix
              (format nil
                      "* Task~%SCHEDULED: <2026-03-02>~%DEADLINE: [2026-03-04]~%body <2026-03-05> [2026-03-07]~%:LOGBOOK:~%CLOCK: [2026-03-02 09:00]--[2026-03-02 10:00] =>  1:00~%:END:~%** Child~%<2026-03-08>~%* Task~%SCHEDULED: <2026-03-09 Mon>~%DEADLINE: [2026-03-11 Wed]~%body <2026-03-12 Thu> [2026-03-14 Sat]~%** Child~%<2026-03-15 Sun>~%* Task~%SCHEDULED: <2026-03-16 Mon>~%DEADLINE: [2026-03-18 Wed]~%body <2026-03-19 Thu> [2026-03-21 Sat]~%** Child~%<2026-03-22 Sun>~%* Peer~%untouched~%")))
           (document (parse-org source))
           (target (edit-subtree-clone-test-headline document "Task"))
           (result (clone-subtree-with-time-shift document target 2 1 :week))
           (updated (edit-result-updated-document result))
           (output (document-text updated)))
      (with-soft-assertions
        (expect (edit-result-p result) :to-be t)
        (expect output :to-equal expected)
        (expect (subseq output 0 (length prefix)) :to-equal prefix)
        (expect (subseq output (- (length output) (length suffix)))
                :to-equal suffix)
        (expect (document-text document) :to-equal source))))
  (it "matches Org timestamp rewriting inside blocks and verbatim text"
      (let* ((source
              (format nil
                      "* Task~%#+begin_src text~%<2026-03-02 Mon> [2026-03-02 Mon]~%#+end_src~%#+begin_example~%<2026-03-02 Mon> [2026-03-02 Mon]~%#+end_example~%Verbatim ~~<2026-03-02 Mon>~~ and =<2026-03-02 Mon>=~%* Peer~%"))
             (expected
              (format nil
                      "* Task~%#+begin_src text~%<2026-03-02 Mon> [2026-03-02 Mon]~%#+end_src~%#+begin_example~%<2026-03-02 Mon> [2026-03-02 Mon]~%#+end_example~%Verbatim ~~<2026-03-02 Mon>~~ and =<2026-03-02 Mon>=~%* Task~%#+begin_src text~%<2026-03-09 Mon> [2026-03-09 Mon]~%#+end_src~%#+begin_example~%<2026-03-09 Mon> [2026-03-09 Mon]~%#+end_example~%Verbatim ~~<2026-03-09 Mon>~~ and =<2026-03-09 Mon>=~%* Peer~%"))
             (document (parse-org source))
             (target (edit-subtree-clone-test-headline document "Task"))
             (result (clone-subtree-with-time-shift document target 1 1 :week)))
        (with-soft-assertions
         (expect (document-text (edit-result-updated-document result))
                 :to-equal expected)
         (expect (document-text document) :to-equal source))))

  (it "regenerates only cloned root IDs deterministically"
    (let* ((source
             (format nil
                     "* Task~%:PROPERTIES:~%:ID: source-id~%:END:~%** Child~%:PROPERTIES:~%:ID: child-id~%:END:~%Body~%* Peer~%"))
           (document (parse-org source))
           (target (edit-subtree-clone-test-headline document "Task"))
           (result
             (clone-subtree-with-time-shift
              document target 2 nil nil
              :random-source (edit-subtree-clone-test-random-source)))
           (updated (edit-result-updated-document result))
           (headlines (edit-subtree-clone-test-headlines updated))
           (task-headlines
             (remove-if-not
              (lambda (headline)
                (string= (node-property headline :raw-value) "Task"))
              headlines))
           (child-headlines
             (remove-if-not
              (lambda (headline)
                (string= (node-property headline :raw-value) "Child"))
              headlines)))
      (with-soft-assertions
        (expect (mapcar #'edit-subtree-clone-test-id task-headlines)
                :to-equal
                '("source-id"
                  "00010203-0405-4607-8809-0a0b0c0d0e0f"
                  "10111213-1415-4617-9819-1a1b1c1d1e1f"))
        (expect (mapcar #'edit-subtree-clone-test-id child-headlines)
                :to-equal '("child-id" "child-id" "child-id"))
        (expect (document-text document) :to-equal source))))

  (it "matches Org repeater output and retains CLOCK only in the unshifted instance"
    (let* ((source
             (format nil
                     "* Repeat~%SCHEDULED: <2026-03-02 +1w>~%:LOGBOOK:~%CLOCK: [2026-03-02 09:00]--[2026-03-02 10:00] =>  1:00~%:END:~%Body~%* Peer~%untouched~%"))
           (expected
             (format nil
                     "* Repeat~%SCHEDULED: <2026-03-02 Mon>~%:LOGBOOK:~%CLOCK: [2026-03-02 Mon 09:00]--[2026-03-02 Mon 10:00] =>  1:00~%:END:~%Body~%* Repeat~%SCHEDULED: <2026-03-09 Mon>~%Body~%* Repeat~%SCHEDULED: <2026-03-16 Mon +1w>~%Body~%* Peer~%untouched~%"))
           (document (parse-org source))
           (target (edit-subtree-clone-test-headline document "Repeat"))
           (result (clone-subtree-with-time-shift document target 1 1 :week)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result))
                :to-equal expected)
        (expect (document-text document) :to-equal source))))

  (it "treats an explicit zero shift as an active repeater expansion"
    (let* ((source
             (format nil
                     "* Repeat~%SCHEDULED: <2026-03-02 +1w>~%Body~%* Peer~%untouched~%"))
           (expected
             (format nil
                     "* Repeat~%SCHEDULED: <2026-03-02 Mon>~%Body~%* Repeat~%SCHEDULED: <2026-03-02 Mon +1w>~%Body~%* Peer~%untouched~%"))
           (document (parse-org source))
           (target (edit-subtree-clone-test-headline document "Repeat"))
           (result (clone-subtree-with-time-shift document target 0 0 :week)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result))
                :to-equal expected)
        (expect (document-text document) :to-equal source))))

  (it "returns a byte-identical no-op for zero clones at EOF without a final newline"
    (let* ((source "* Task")
           (document (parse-org source))
           (target (edit-subtree-clone-test-headline document "Task"))
           (result (clone-subtree-with-time-shift document target 0 nil nil))
           (updated (edit-result-updated-document result)))
      (with-soft-assertions
        (expect (document-text updated) :to-equal source)
        (expect (edit-result-spans result) :to-equal nil)
        (expect (eq updated document) :to-be t))))

  (it "reports invalid values, targets, IDs, and generated clone counts"
    (let* ((source (format nil "* Task~%Body~%* Peer~%"))
           (limited (parse-org source :limits (make-limits :max-subtree-clones 1)))
           (target (edit-subtree-clone-test-headline limited "Task"))
           (invalid-count
             (edit-subtree-clone-test-condition
              (lambda () (clone-subtree-with-time-shift limited target -1 nil nil))))
           (invalid-shift
             (edit-subtree-clone-test-condition
              (lambda () (clone-subtree-with-time-shift limited target 1 nil :week))))
           (invalid-unit
             (edit-subtree-clone-test-condition
              (lambda () (clone-subtree-with-time-shift limited target 1 1 :decade))))
           (missing-target
             (edit-subtree-clone-test-condition
              (lambda () (clone-subtree-with-time-shift limited "missing" 1 nil nil))))
           (clone-limit
             (edit-subtree-clone-test-condition
              (lambda () (clone-subtree-with-time-shift limited target 2 nil nil))))
           (repeater-source
             (format nil "* Repeat~%SCHEDULED: <2026-03-02 +1w>~%* Peer~%"))
           (repeater-document
             (parse-org repeater-source
                        :limits (make-limits :max-subtree-clones 2)))
           (repeater-target
             (edit-subtree-clone-test-headline repeater-document "Repeat"))
           (repeater-limit
             (edit-subtree-clone-test-condition
              (lambda ()
                (clone-subtree-with-time-shift
                 repeater-document repeater-target 1 1 :week))))
           (duplicate-document
             (parse-org
              (format nil
                      "* Task~%:PROPERTIES:~%:ID: duplicate~%:END:~%* Peer~%:PROPERTIES:~%:ID: duplicate~%:END:~%")))
           (duplicate-target
             (edit-subtree-clone-test-headline duplicate-document "Task"))
           (id-error
             (edit-subtree-clone-test-condition
              (lambda ()
                (clone-subtree-with-time-shift
                 duplicate-document duplicate-target 1 nil nil)))))
      (with-soft-assertions
        (dolist (condition (list invalid-count invalid-shift invalid-unit))
          (expect (typep condition 'org-edit-error) :to-be t)
          (when (typep condition 'org-edit-error)
            (expect (org-edit-error-reason condition) :to-be :invalid-value)))
        (expect (typep missing-target 'org-edit-error) :to-be t)
        (when (typep missing-target 'org-edit-error)
          (expect (org-edit-error-reason missing-target) :to-be :target-not-found))
        (dolist (entry (list (list clone-limit 1 2)
                             (list repeater-limit 2 3)))
          (destructuring-bind (condition limit actual) entry
          (expect (typep condition 'org-resource-limit-error) :to-be t)
          (when (typep condition 'org-resource-limit-error)
            (expect (org-resource-limit-error-limit-name condition)
                    :to-be :max-subtree-clones)
              (expect (org-resource-limit-error-limit condition) :to-be limit)
              (expect (org-resource-limit-error-actual condition) :to-be actual))))
        (expect (typep id-error 'org-id-error) :to-be t)
        (when (typep id-error 'org-id-error)
          (expect (org-id-error-reason id-error) :to-be :duplicate-id))
        (expect (document-text limited) :to-equal source)
        (expect (document-text repeater-document) :to-equal repeater-source))))

  (it "continues enforcing the existing output and node count limits"
    (let* ((source (format nil "* Task~%Body~%* Peer~%"))
           (document
             (parse-org source :limits (make-limits :max-output-length
                                                    (length source))))
           (target (edit-subtree-clone-test-headline document "Task"))
           (output-limit
             (edit-subtree-clone-test-condition
              (lambda ()
                (org-kit::clone-subtree-with-time-shift
                 document target 1 nil nil))))
           (unlimited-document (parse-org source))
           (node-count
             (let ((count 0))
               (map-nodes (lambda (node) (declare (ignore node)) (incf count))
                          (document-root unlimited-document))
               count))
           (node-limit-count (+ node-count 2))
           (node-limited-document
             (parse-org source
                        :limits (make-limits :max-node-count node-limit-count)))
           (node-target
             (edit-subtree-clone-test-headline node-limited-document "Task"))
           (node-limit
             (edit-subtree-clone-test-condition
              (lambda ()
                (org-kit::clone-subtree-with-time-shift
                 node-limited-document node-target 1 nil nil)))))
      (with-soft-assertions
        (expect (typep output-limit 'org-resource-limit-error) :to-be t)
        (when (typep output-limit 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name output-limit)
                  :to-be :max-output-length)
          (expect (org-resource-limit-error-limit output-limit)
                  :to-be (length source))
          (expect (> (org-resource-limit-error-actual output-limit)
                     (length source))
                  :to-be t))
        (expect (typep node-limit 'org-resource-limit-error) :to-be t)
        (when (typep node-limit 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name node-limit)
                  :to-be :max-node-count)
          (expect (org-resource-limit-error-limit node-limit)
                  :to-be node-limit-count)
          (expect (> (org-resource-limit-error-actual node-limit)
                     node-limit-count)
                  :to-be t))
        (expect (document-text document) :to-equal source)
        (expect (document-text node-limited-document) :to-equal source)))))
(describe "subtree clone edge behavior"
  (it "shifts month and year dates through the public API"
    (dolist (case '((1 :month "<2026-04-02 Thu>")
                   (1 :year "<2027-03-02 Tue>")))
      (destructuring-bind (amount unit expected-timestamp) case
        (let* ((source (format nil "* Task~%<2026-03-02>~%* Peer~%"))
               (document (parse-org source))
               (target (edit-subtree-clone-test-headline document "Task"))
               (result
                 (clone-subtree-with-time-shift
                  document target 1 amount unit)))
          (expect (document-text (edit-result-updated-document result))
                  :to-equal
                  (format nil
                          "* Task~%<2026-03-02>~%* Task~%~A~%* Peer~%"
                          expected-timestamp))))))

  (it "shifts timestamp times across midnight and shifts every time"
    (let* ((source
             (format nil
                     "* Task~%<2026-03-02 23:30-22:45>~%[2026-03-04 08:15]~%* Peer~%"))
           (document (parse-org source))
           (target (edit-subtree-clone-test-headline document "Task"))
           (result (clone-subtree-with-time-shift document target 1 1 :hour)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result))
                :to-equal
                (format nil
                        "* Task~%<2026-03-02 23:30-22:45>~%[2026-03-04 08:15]~%* Task~%<2026-03-03 Tue 00:30-23:45>~%[2026-03-04 Wed 09:15]~%* Peer~%"))
        (expect (document-text document) :to-equal source))))

  (it "shifts date-only timestamps by whole days for hour units"
    (let* ((source (format nil "* Task~%<2026-03-02>~%* Peer~%"))
           (document (parse-org source))
           (target (edit-subtree-clone-test-headline document "Task"))
           (result (clone-subtree-with-time-shift document target 1 24 :hour)))
      (expect (document-text (edit-result-updated-document result))
              :to-equal
              (format nil
                      "* Task~%<2026-03-02>~%* Task~%<2026-03-03 Tue>~%* Peer~%"))))

  (it "reports invalid calendar dates and out-of-range date shifts"
    (let* ((invalid-source (format nil "* Task~%<2026-02-30>~%* Peer~%"))
           (invalid-document (parse-org invalid-source))
           (invalid-target
             (edit-subtree-clone-test-headline invalid-document "Task"))
           (invalid-date
             (edit-subtree-clone-test-condition
              (lambda ()
                (clone-subtree-with-time-shift
                 invalid-document invalid-target 1 1 :day))))
           (overflow-source (format nil "* Task~%<9999-12-31>~%* Peer~%"))
           (overflow-document (parse-org overflow-source))
           (overflow-target
             (edit-subtree-clone-test-headline overflow-document "Task"))
           (overflow
             (edit-subtree-clone-test-condition
              (lambda ()
                (clone-subtree-with-time-shift
                 overflow-document overflow-target 1 1 :year)))))
      (with-soft-assertions
        (dolist (condition (list invalid-date overflow))
          (expect (typep condition 'org-edit-error) :to-be t)
          (when (typep condition 'org-edit-error)
            (expect (org-edit-error-reason condition) :to-be :invalid-value)))
        (expect (document-text invalid-document) :to-equal invalid-source)
        (expect (document-text overflow-document) :to-equal overflow-source))))

  (it "keeps malformed token fallbacks and parses both timestamp delimiters"
    (let* ((document (parse-org (format nil "* Task~%* Peer~%")))
           (target (edit-subtree-clone-test-headline document "Task"))
           (date (cl-date-kit:make-local-date 2026 3 2))
           (unsupported
             (org-kit::edit-subtree-clone-date-shift
              date 1 :unsupported target))
           (invalid-token
             (org-kit::edit-subtree-clone-shift-timestamp-token
              "<not-a-date>" 1 :day target))
           (unclosed
             (org-kit::edit-subtree-clone-shift-timestamp-token
              "<2026-03-02" 1 :day target))
           (inactive-repeater
             (org-kit::edit-subtree-clone-remove-repeater-token
              "[2026-03-02 +1w]"))
           (unclosed-repeater
             (org-kit::edit-subtree-clone-remove-repeater-token
              "<2026-03-02 +1w"))
           (trailing-whitespace-layout
             (org-kit::edit-subtree-clone-adjustment-token-layouts
              "<2026-03-02 +1w >")))
      (with-soft-assertions
        (expect (equalp unsupported date) :to-be t)
        (expect invalid-token :to-equal "<not-a-date>")
        (expect unclosed :to-equal "<2026-03-03 Tue")
        (expect inactive-repeater :to-equal "[2026-03-02]")
        (expect unclosed-repeater :to-equal "<2026-03-02")
        (expect (first (first trailing-whitespace-layout))
                :to-be :repeater))))

  (it "skips an unclosed timestamp and still shifts a later valid one"
    (let* ((source
             (format nil
                     "* Task~%unclosed <2026-03-02 and [2026-03-04]~%* Peer~%"))
           (document (parse-org source))
           (target (edit-subtree-clone-test-headline document "Task"))
           (result (clone-subtree-with-time-shift document target 1 1 :day)))
      (expect (document-text (edit-result-updated-document result))
              :to-equal
              (format nil
                      "* Task~%unclosed <2026-03-02 and [2026-03-04]~%* Task~%unclosed <2026-03-02 and [2026-03-05 Thu]~%* Peer~%"))))

  (it "removes indented clock lines and whitespace-only drawers from clones"
    (let* ((source
             (format nil
                     "* Task~%:LOGBOOK:~%  ~%:END:~%  CLOCK: raw~%* Peer~%"))
           (document (parse-org source))
           (target (edit-subtree-clone-test-headline document "Task"))
           (result (clone-subtree-with-time-shift document target 1 nil nil)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result))
                :to-equal
                (format nil
                        "* Task~%:LOGBOOK:~%  ~%:END:~%  CLOCK: raw~%* Task~%* Peer~%"))
        (expect (document-text document) :to-equal source))))

  (it "removes a final clock line without a newline"
    (let* ((source (format nil "* Task~%  CLOCK: raw"))
           (document (parse-org source))
           (target (edit-subtree-clone-test-headline document "Task"))
           (result (clone-subtree-with-time-shift document target 1 nil nil)))
      (expect (document-text (edit-result-updated-document result))
              :to-equal
              (format nil "* Task~%  CLOCK: raw~%* Task~%"))))

  (it "generates a root ID without a supplied random source"
    (let* ((source
             (format nil
                     "* Task~%:PROPERTIES:~%:ID: source-id~%:END:~%* Peer~%"))
           (document (parse-org source))
           (target (edit-subtree-clone-test-headline document "Task"))
           (result (clone-subtree-with-time-shift document target 1 nil nil))
           (headlines
             (remove-if-not
              (lambda (headline)
                (string= (node-property headline :raw-value) "Task"))
              (edit-subtree-clone-test-headlines
               (edit-result-updated-document result))))
           (ids (mapcar #'edit-subtree-clone-test-id headlines)))
      (with-soft-assertions
        (expect (first ids) :to-equal "source-id")
        (expect (stringp (second ids)) :to-be t)
        (expect (not (string= (first ids) (second ids))) :to-be t)
        (expect (length (second ids)) :to-equal 36))))

  (it "rejects a generated root ID that collides with an existing ID"
    (let* ((source
             (format nil
                     "* Task~%:PROPERTIES:~%:ID: 00010203-0405-4607-8809-0a0b0c0d0e0f~%:END:~%* Peer~%"))
           (document (parse-org source))
           (target (edit-subtree-clone-test-headline document "Task"))
           (condition
             (edit-subtree-clone-test-condition
              (lambda ()
                (clone-subtree-with-time-shift
                 document target 1 nil nil
                 :random-source (edit-subtree-clone-test-random-source))))))
      (with-soft-assertions
        (expect (typep condition 'org-id-error) :to-be t)
        (when (typep condition 'org-id-error)
          (expect (org-id-error-reason condition) :to-be :duplicate-id))
        (expect (document-text document) :to-equal source))))

  (it "adds and preserves final newlines when cloning at end of file"
    (dolist (source (list "* Task" (format nil "* Task~%")))
      (let* ((document (parse-org source))
             (target (edit-subtree-clone-test-headline document "Task"))
             (result (clone-subtree-with-time-shift document target 1 nil nil)))
        (expect (document-text (edit-result-updated-document result))
                :to-equal
                (format nil "* Task~%* Task~%")))))

  (it "preserves final newline state when expanding repeaters at end of file"
    (dolist (source
             (list
              (format nil "* Repeat~%SCHEDULED: <2026-03-02 +1w>")
              (format nil "* Repeat~%SCHEDULED: <2026-03-02 +1w>~%")))
      (let* ((document (parse-org source))
             (target (edit-subtree-clone-test-headline document "Repeat"))
             (result (clone-subtree-with-time-shift document target 0 1 :week)))
        (expect (document-text (edit-result-updated-document result))
                :to-equal
                (format nil
                        "* Repeat~%SCHEDULED: <2026-03-02 Mon>~%* Repeat~%SCHEDULED: <2026-03-09 Mon +1w>~%")))))

  (it "signals a type error for non-document inputs"
    (let ((condition
            (edit-subtree-clone-test-condition
             (lambda () (clone-subtree-with-time-shift nil "Task" 1 nil nil)))))
      (expect (typep condition 'type-error) :to-be t))))

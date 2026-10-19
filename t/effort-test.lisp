;;;; t/effort-test.lisp
(in-package #:cl-org-kit/test)

(defun effort-test-heading (document title)
  (let ((found nil))
    (map-nodes (lambda (node)
                 (when (and (eq (node-type node) :headline)
                            (string= (node-property node :raw-value) title))
                   (setf found node)))
               (document-root document))
    found))

(defun effort-test-inlinetask (document)
  (let ((found nil))
    (map-nodes (lambda (node)
                 (when (eq (node-type node) :inlinetask)
                   (setf found node)))
               (document-root document))
    found))

(defun effort-test-condition (function)
  (handler-case
      (progn (funcall function) nil)
    (error (condition) condition)))

(describe "effort estimates"
  (it "parses Org durations into exact minutes"
    (expect (list (org-kit:effort-duration-minutes "1:30")
                  (org-kit:effort-duration-minutes "2d")
                  (org-kit:effort-duration-minutes "1h 30min")
                  (org-kit:effort-duration-minutes "1d3h5min")
                  (org-kit:effort-duration-minutes "1:23:45")
                  (org-kit:effort-duration-minutes "1:90")
                  (org-kit:effort-duration-minutes "1:90:90")
                  (org-kit:effort-duration-minutes "3d 13:35")
                  (org-kit:effort-duration-minutes "2.5h")
                  (org-kit:effort-duration-minutes "1.5min")
                  (org-kit:effort-duration-minutes "1w")
                  (org-kit:effort-duration-minutes "1m")
                  (org-kit:effort-duration-minutes "1y")
                  (org-kit:effort-duration-minutes " 1h 30min ")
                  (org-kit:effort-duration-minutes
                   (format nil "~C1h~C30min~C" #\Tab #\Tab #\Tab))
                  (org-kit:effort-duration-minutes "1.")
                  (org-kit:effort-duration-minutes "2.5")
                  (org-kit:effort-duration-minutes "123")
                  (org-kit:effort-duration-minutes ""))
            :to-equal '(90 2880 90 1625 335/4 150 303/2 5135 150 3/2
                        10080 43200 525960 90 90 1 5/2 123 0)))

  (it "sums each direct EFFORT property in the selected subtree"
    (let* ((document
             (parse-org
              (format nil
                      "* Project~%:PROPERTIES:~%:EFFORT: 1:30~%:END:~%** First~%:PROPERTIES:~%:effort: 1h~%:END:~%*** Detail~%:PROPERTIES:~%:EFFORT: 2d~%:END:~%**** No estimate~%** Second~%:PROPERTIES:~%:EFFORT: 10min~%:END:~%* Outside~%:PROPERTIES:~%:EFFORT: 4h~%:END:~%")))
           (project (effort-test-heading document "Project"))
           (first (effort-test-heading document "First"))
           (no-estimate (effort-test-heading document "No estimate")))
      (expect (list (org-kit:subtree-effort-minutes document project)
                    (org-kit:subtree-effort-minutes document first)
                    (org-kit:subtree-effort-minutes document no-estimate))
              :to-equal '(3040 2940 0))))

  (it "sums inline-task effort within a subtree"
    (let* ((document
             (parse-org
              (format nil
                      "* Project~%:PROPERTIES:~%:EFFORT: 1h~%:END:~%*************** Inline :inline_tag:~%:PROPERTIES:~%:EFFORT: 30min~%:END:~%Inline body.~%*************** END~%")))
           (project (effort-test-heading document "Project"))
           (inline (effort-test-inlinetask document)))
      (with-soft-assertions
        (expect (node-type inline) :to-be :inlinetask)
        (expect (list (org-kit:subtree-effort-minutes document project)
                      (org-kit:subtree-effort-minutes document inline))
                :to-equal '(90 30)))))

  (it "signals a named condition with a reason for invalid input and targets"
    (let* ((duration-condition
             (effort-test-condition
              (lambda () (org-kit:effort-duration-minutes "1h nope"))))
           (empty-condition
             (effort-test-condition
              (lambda () (org-kit:effort-duration-minutes "   "))))
           (clock-condition
             (effort-test-condition
              (lambda ()
                (org-kit:effort-duration-minutes "1:30:00:00"))))
           (document (parse-org (format nil "* Task~%Text~%* Other~%")))
           (other-document (parse-org (format nil "* Foreign~%")))
           (task (effort-test-heading document "Task"))
           (foreign (effort-test-heading other-document "Foreign"))
           (wrong-node (first (node-children task)))
           (foreign-condition
             (effort-test-condition
              (lambda ()
                (org-kit:subtree-effort-minutes document foreign))))
           (wrong-type-condition
             (effort-test-condition
              (lambda ()
                (org-kit:subtree-effort-minutes document wrong-node)))))
      (with-soft-assertions
        (expect (typep duration-condition 'org-kit:org-effort-error) :to-be t)
        (expect (typep empty-condition 'org-kit:org-effort-error) :to-be t)
        (expect (typep clock-condition 'org-kit:org-effort-error) :to-be t)
        (expect (typep foreign-condition 'org-kit:org-effort-error) :to-be t)
        (expect (typep wrong-type-condition 'org-kit:org-effort-error) :to-be t)
        (when (typep duration-condition 'org-kit:org-effort-error)
          (expect (org-kit:org-effort-error-reason duration-condition)
                  :to-be :invalid-duration))
        (when (typep empty-condition 'org-kit:org-effort-error)
          (expect (org-kit:org-effort-error-reason empty-condition)
                  :to-be :invalid-duration))
        (when (typep clock-condition 'org-kit:org-effort-error)
          (expect (org-kit:org-effort-error-reason clock-condition)
                  :to-be :invalid-duration))
        (when (typep foreign-condition 'org-kit:org-effort-error)
          (expect (org-kit:org-effort-error-reason foreign-condition)
                  :to-be :foreign-node))
        (when (typep wrong-type-condition 'org-kit:org-effort-error)
          (expect (org-kit:org-effort-error-reason wrong-type-condition)
                  :to-be :wrong-node-type)
          (expect (princ-to-string wrong-type-condition)
                  :to-equal "Effort target is not a heading (wrong-node-type)")))))

  (it "rejects malformed and signed Org durations with the named reason"
    (let ((conditions
            (mapcar (lambda (duration)
                      (effort-test-condition
                       (lambda () (org-kit:effort-duration-minutes duration))))
                    '("2q" "bogus" ":30" "1:3" "1:aa" "1:23:4"
                      "1:23:4x" "1:23:456" "2q 1:30" "1h -30min"
                      "-1h" "+1h"))))
      (expect (mapcar (lambda (condition)
                        (and (typep condition 'org-kit:org-effort-error)
                             (org-kit:org-effort-error-reason condition)))
                      conditions)
              :to-equal '(:invalid-duration :invalid-duration :invalid-duration
                          :invalid-duration :invalid-duration :invalid-duration
                          :invalid-duration :invalid-duration :invalid-duration
                          :invalid-duration :invalid-duration :invalid-duration))))

  (it "rejects invalid public argument types and prints a reasonless condition"
    (let* ((document (parse-org (format nil "* Task~%")))
           (heading (effort-test-heading document "Task"))
           (bad-limits
             (effort-test-condition
              (lambda ()
                (org-kit:effort-duration-minutes "1h" :limits nil))))
           (bad-duration
             (effort-test-condition
              (lambda () (org-kit:effort-duration-minutes 1))))
           (bad-document
             (effort-test-condition
              (lambda () (org-kit:subtree-effort-minutes nil heading))))
           (bad-heading
             (effort-test-condition
              (lambda () (org-kit:subtree-effort-minutes document nil))))
           (reasonless (make-condition 'org-kit:org-effort-error
                                       :message "Unclassified effort error")))
      (with-soft-assertions
        (expect (typep bad-limits 'type-error) :to-be t)
        (expect (typep bad-duration 'type-error) :to-be t)
        (expect (typep bad-document 'type-error) :to-be t)
        (expect (typep bad-heading 'type-error) :to-be t)
        (expect (org-kit:org-effort-error-reason reasonless) :to-be nil)
        (expect (princ-to-string reasonless) :to-equal "Unclassified effort error"))))

  (it "enforces the configured duration input limit"
    (let ((condition
            (effort-test-condition
             (lambda ()
               (org-kit:effort-duration-minutes
                "1h" :limits (make-limits :max-input-length 1))))))
      (expect (typep condition 'org-resource-limit-error) :to-be t)
      (when (typep condition 'org-resource-limit-error)
        (expect (org-resource-limit-error-limit-name condition)
                :to-be :max-input-length)
        (expect (org-resource-limit-error-limit condition) :to-be 1)
        (expect (org-resource-limit-error-actual condition) :to-be 2)))))

  (it "enforces the document node limit when summing a program-built subtree"
    (let* ((document (make-document "" :limits (make-limits :max-node-count 1)))
           (heading (make-node :headline :properties '(:level 1 :raw-value "Task")))
           (root (make-node :org-data :children (list heading))))
      (setf (document-root document) root)
      (let ((condition
              (effort-test-condition
               (lambda () (org-kit:subtree-effort-minutes document heading)))))
        (with-soft-assertions
          (expect (typep condition 'org-resource-limit-error) :to-be t)
          (when (typep condition 'org-resource-limit-error)
            (expect (org-resource-limit-error-limit-name condition)
                    :to-be :max-node-count)
            (expect (org-resource-limit-error-limit condition) :to-be 1)
            (expect (org-resource-limit-error-actual condition) :to-be 2))))))

(describe "effort unit parser boundary cases"
  (it "accepts an empty unit slice without treating it as a sign"
    (expect (multiple-value-list (org-kit::effort-unit-duration "" 0 0))
            :to-equal '(0 nil))))

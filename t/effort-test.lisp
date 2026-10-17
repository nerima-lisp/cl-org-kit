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

(defun effort-test-condition (function)
  (handler-case
      (progn (funcall function) nil)
    (error (condition) condition)))

(describe "effort estimates"
  (it "parses Org durations into exact minutes"
    (expect (list (org-kit::effort-duration-minutes "1:30")
                  (org-kit::effort-duration-minutes "2d")
                  (org-kit::effort-duration-minutes "1h 30min")
                  (org-kit::effort-duration-minutes "1d3h5min")
                  (org-kit::effort-duration-minutes "1:23:45")
                  (org-kit::effort-duration-minutes "1:90")
                  (org-kit::effort-duration-minutes "1:90:90")
                  (org-kit::effort-duration-minutes "3d 13:35")
                  (org-kit::effort-duration-minutes "2.5h")
                  (org-kit::effort-duration-minutes "1.5min")
                  (org-kit::effort-duration-minutes "1w")
                  (org-kit::effort-duration-minutes "1m")
                  (org-kit::effort-duration-minutes "1y")
                  (org-kit::effort-duration-minutes " 1h 30min ")
                  (org-kit::effort-duration-minutes "2.5")
                  (org-kit::effort-duration-minutes "123")
                  (org-kit::effort-duration-minutes ""))
            :to-equal '(90 2880 90 1625 335/4 150 303/2 5135 150 3/2
                        10080 43200 525960 90 5/2 123 0)))

  (it "sums each direct EFFORT property in the selected subtree"
    (let* ((document
             (parse-org
              (format nil
                      "* Project~%:PROPERTIES:~%:EFFORT: 1:30~%:END:~%** First~%:PROPERTIES:~%:effort: 1h~%:END:~%*** Detail~%:PROPERTIES:~%:EFFORT: 2d~%:END:~%**** No estimate~%** Second~%:PROPERTIES:~%:EFFORT: 10min~%:END:~%* Outside~%:PROPERTIES:~%:EFFORT: 4h~%:END:~%")))
           (project (effort-test-heading document "Project"))
           (first (effort-test-heading document "First")))
      (expect (list (org-kit::subtree-effort-minutes document project)
                    (org-kit::subtree-effort-minutes document first))
              :to-equal '(3040 2940))))

  (it "signals a named condition with a reason for invalid input and targets"
    (let* ((duration-condition
             (effort-test-condition
              (lambda () (org-kit::effort-duration-minutes "1h nope"))))
           (empty-condition
             (effort-test-condition
              (lambda () (org-kit::effort-duration-minutes "   "))))
           (clock-condition
             (effort-test-condition
              (lambda ()
                (org-kit::effort-duration-minutes "1:30:00:00"))))
           (document (parse-org (format nil "* Task~%Text~%* Other~%")))
           (other-document (parse-org (format nil "* Foreign~%")))
           (task (effort-test-heading document "Task"))
           (foreign (effort-test-heading other-document "Foreign"))
           (wrong-node (first (node-children task)))
           (foreign-condition
             (effort-test-condition
              (lambda ()
                (org-kit::subtree-effort-minutes document foreign))))
           (wrong-type-condition
             (effort-test-condition
              (lambda ()
                (org-kit::subtree-effort-minutes document wrong-node)))))
      (with-soft-assertions
        (expect (typep duration-condition 'org-kit::org-effort-error) :to-be t)
        (expect (typep empty-condition 'org-kit::org-effort-error) :to-be t)
        (expect (typep clock-condition 'org-kit::org-effort-error) :to-be t)
        (expect (typep foreign-condition 'org-kit::org-effort-error) :to-be t)
        (expect (typep wrong-type-condition 'org-kit::org-effort-error) :to-be t)
        (when (typep duration-condition 'org-kit::org-effort-error)
          (expect (org-kit::org-effort-error-reason duration-condition)
                  :to-be :invalid-duration))
        (when (typep empty-condition 'org-kit::org-effort-error)
          (expect (org-kit::org-effort-error-reason empty-condition)
                  :to-be :invalid-duration))
        (when (typep clock-condition 'org-kit::org-effort-error)
          (expect (org-kit::org-effort-error-reason clock-condition)
                  :to-be :invalid-duration))
        (when (typep foreign-condition 'org-kit::org-effort-error)
          (expect (org-kit::org-effort-error-reason foreign-condition)
                  :to-be :foreign-node))
        (when (typep wrong-type-condition 'org-kit::org-effort-error)
          (expect (org-kit::org-effort-error-reason wrong-type-condition)
                  :to-be :wrong-node-type)))))

  (it "enforces the configured duration input limit"
    (let ((condition
            (effort-test-condition
             (lambda ()
               (org-kit::effort-duration-minutes
                "1h" :limits (make-limits :max-input-length 1))))))
      (expect (typep condition 'org-resource-limit-error) :to-be t)
      (when (typep condition 'org-resource-limit-error)
        (expect (org-resource-limit-error-limit-name condition)
                :to-be :max-input-length)
        (expect (org-resource-limit-error-limit condition) :to-be 1)
        (expect (org-resource-limit-error-actual condition) :to-be 2)))))

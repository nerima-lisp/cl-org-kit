;;;; t/agenda-stuck-projects-test.lisp
(in-package #:cl-org-kit/test)

(defun agenda-stuck-projects-test-context ()
  (make-time-context :reference-time (make-org-time-value "2025-01-01T12:00:00+00:00")
                     :zone (resolve-time-zone "UTC")))

(defun agenda-stuck-projects-test-titles (entries)
  (mapcar (lambda (entry)
            (node-property (agenda-match-entry-node entry) :raw-value))
          entries))

(defun agenda-stuck-projects-test-error (function)
  (handler-case (progn (funcall function) nil)
    (error (condition) condition)))

(describe "Agenda stuck projects"
  (it "vetoes unfinished TODOs on the candidate or in its descendants"
    (let* ((document
             (parse-org
              (format nil "* Active project~%** TODO Active task~%* Stuck project~%** DONE Finished task~%* TODO Own TODO~%")))
           (entries
             (agenda-stuck-projects
              (list document) '("LEVEL=1" ("TODO") nil nil)
              :time-context (agenda-stuck-projects-test-context))))
      (with-soft-assertions
        (expect (agenda-stuck-projects-test-titles entries)
                :to-equal '("Stuck project"))
        (expect (agenda-match-entry-line (first entries)) :to-be 3))))
  (it "vetoes explicit candidate and descendant tags without inheritance"
    (let* ((own-tagged
             (parse-org
              (format nil "* Candidate :BLOCKED:~%** TODO Child~%")))
           (descendant-tagged
             (parse-org
              (format nil "* Parent~%** TODO Tagged child :BLOCKED:~%")))
           (inherited-tagged
             (parse-org
              (format nil "* Parent :BLOCKED:~%** Inherited child~%")))
           (own-tagged-entries
             (agenda-stuck-projects
              (list own-tagged) '("LEVEL=1" nil ("BLOCKED") nil)
              :time-context (agenda-stuck-projects-test-context)))
           (descendant-tagged-entries
             (agenda-stuck-projects
              (list descendant-tagged) '("LEVEL=1" nil ("BLOCKED") nil)
              :time-context (agenda-stuck-projects-test-context)))
           (inherited-tagged-entries
             (agenda-stuck-projects
              (list inherited-tagged) '("LEVEL=2" nil ("BLOCKED") nil)
              :time-context (agenda-stuck-projects-test-context))))
      (with-soft-assertions
        (expect (agenda-stuck-projects-test-titles own-tagged-entries)
                :to-equal nil)
        (expect (agenda-stuck-projects-test-titles descendant-tagged-entries)
                :to-equal nil)
        (expect (agenda-stuck-projects-test-titles inherited-tagged-entries)
                :to-equal '("Inherited child")))))
  (it "supports tag and TODO wildcard vetoes without inheriting tags"
    (let* ((own-tagged
             (parse-org
              (format nil "* Candidate :BLOCKED:~%** Child~%")))
           (descendant-tagged
             (parse-org
              (format nil "* Parent~%** TODO Tagged child :OTHER:~%")))
           (inherited-tagged
             (parse-org
              (format nil "* Parent :BLOCKED:~%** Inherited child~%")))
           (todo-wildcard
             (parse-org
              (format nil "#+TODO: TODO NEXT | DONE~%* Parent~%** NEXT Action~%")))
           (own-tagged-entries
             (agenda-stuck-projects
              (list own-tagged) '("LEVEL=1" nil ("*") nil)
              :time-context (agenda-stuck-projects-test-context)))
           (descendant-tagged-entries
             (agenda-stuck-projects
              (list descendant-tagged) '("LEVEL=1" nil ("*") nil)
              :time-context (agenda-stuck-projects-test-context)))
           (inherited-tagged-entries
             (agenda-stuck-projects
              (list inherited-tagged) '("LEVEL=2" nil ("*") nil)
              :time-context (agenda-stuck-projects-test-context)))
           (todo-wildcard-entries
             (agenda-stuck-projects
              (list todo-wildcard) '("LEVEL=1" ("*") nil nil)
              :time-context (agenda-stuck-projects-test-context))))
      (with-soft-assertions
        (expect (agenda-stuck-projects-test-titles own-tagged-entries)
                :to-equal nil)
        (expect (agenda-stuck-projects-test-titles descendant-tagged-entries)
                :to-equal nil)
        (expect (agenda-stuck-projects-test-titles inherited-tagged-entries)
                :to-equal '("Inherited child"))
        (expect (agenda-stuck-projects-test-titles todo-wildcard-entries)
                :to-equal nil))))
  (it "evaluates nested project candidates independently"
    (let* ((document
             (parse-org
              (format nil "* Parent~%** TODO Sibling task~%** Nested project~%")))
           (entries
             (agenda-stuck-projects
              (list document) '("LEVEL<3" ("TODO") nil nil)
              :time-context (agenda-stuck-projects-test-context))))
      (expect (agenda-stuck-projects-test-titles entries)
              :to-equal '("Nested project"))))
  (it "applies case-sensitive subtree regexp vetoes and validates criteria"
    (let* ((document
             (parse-org
              (format nil "* TODO Blocked~%** TODO Task~%vendor reply pending~%* TODO Mixed case~%** TODO Task~%Vendor reply pending~%* TODO Clear~%** TODO Task~%")))
           (entries
             (agenda-stuck-projects
              (list document) '("LEVEL=1" nil nil "vendor")
              :time-context (agenda-stuck-projects-test-context)))
           (missing-criteria
             (agenda-stuck-projects-test-error
              (lambda ()
                (agenda-stuck-projects
                 (list document) '("LEVEL=1" nil nil "")
                 :time-context (agenda-stuck-projects-test-context)))))
           (invalid-options
             (agenda-stuck-projects-test-error
              (lambda ()
                (agenda-stuck-projects
                 (list document) '("LEVEL=1" nil)
                 :time-context (agenda-stuck-projects-test-context)))))
           (invalid-value
             (agenda-stuck-projects-test-error
              (lambda ()
                (agenda-stuck-projects
                 (list document) '("LEVEL=1" ("TODO" 7) nil nil)
                 :time-context (agenda-stuck-projects-test-context))))))
      (with-soft-assertions
        (expect (agenda-stuck-projects-test-titles entries)
                :to-equal '("Mixed case" "Clear"))
        (expect (typep missing-criteria 'org-agenda-error) :to-be t)
        (when (typep missing-criteria 'org-agenda-error)
          (expect (org-agenda-error-reason missing-criteria)
                  :to-be :missing-unstuck-criteria))
        (expect (typep invalid-options 'org-agenda-error) :to-be t)
        (when (typep invalid-options 'org-agenda-error)
          (expect (org-agenda-error-reason invalid-options)
                  :to-be :invalid-option))
        (expect (typep invalid-value 'org-agenda-error) :to-be t)
        (when (typep invalid-value 'org-agenda-error)
          (expect (org-agenda-error-reason invalid-value)
                  :to-be :invalid-option))))))

(in-package #:cl-org-kit/test)
(describe "Org refile target candidate enumeration"
  (it "matches the Org 9.8.10 selectors and stable union order"
    (let* ((source (format nil "~A~%~A~%~A~%~A~%~A~%~A~%~A~%~A"
                           "#+TODO: TODO WAIT | DONE"
                           "* Top :local:"
                           "** TODO Direct"
                           "*** Deep :local:"
                           "* TODO TopTask"
                           "** Child"
                           "* WAIT WaitTask :local:"
                           "* DONE DoneTask :local:"))
           (document (parse-org source))
           (all (refile-target-candidates document (list (list nil t))))
           (level-one
             (refile-target-candidates document
                                       (list (list nil (cons :level 1)))))
           (level-two
             (refile-target-candidates document
                                       (list (list nil (cons :level 2)))))
           (max-level-two
             (refile-target-candidates document
                                       (list (list nil (cons :maxlevel 2)))))
           (local-tag
             (refile-target-candidates document
                                       (list (list nil (cons :tag "local")))))
           (todo
             (refile-target-candidates document
                                       (list (list nil (cons :todo "TODO")))))
           (union
             (refile-target-candidates
              document
              (list (list nil (cons :tag "local"))
                    (list nil t))))
           (default-targets (refile-target-candidates document nil)))
      (labels ((names (entries)
                 (mapcar (lambda (entry) (outline-entry-name entry)) entries)))
        (with-soft-assertions
          (expect (names all)
                  :to-equal (list "Top" "Direct" "Deep" "TopTask"
                                  "Child" "WaitTask" "DoneTask"))
          (expect (names level-one)
                  :to-equal (list "Top" "TopTask" "WaitTask" "DoneTask"))
          (expect (names level-two) :to-equal (list "Direct" "Child"))
          (expect (names max-level-two)
                  :to-equal (list "Top" "Direct" "TopTask" "Child"
                                  "WaitTask" "DoneTask"))
          (expect (names local-tag)
                  :to-equal (list "Top" "Deep" "WaitTask" "DoneTask"))
          (expect (names todo) :to-equal (list "Direct" "TopTask"))
          (expect (names union)
                  :to-equal (list "Top" "Deep" "WaitTask" "DoneTask"
                                  "Direct" "TopTask" "Child"))
          (expect (names default-targets)
                  :to-equal (list "Top" "TopTask" "WaitTask" "DoneTask"))))))

  (it "uses logical levels in odd-level documents"
    (let* ((document
             (parse-org (format nil "~A~%~A~%~A~%~A"
                                "#+STARTUP: odd"
                                "* One"
                                "*** Three"
                                "***** Five")))
           (level-two
             (refile-target-candidates document
                                       (list (list nil (cons :level 2)))))
           (max-level-two
             (refile-target-candidates document
                                       (list (list nil (cons :maxlevel 2))))))
      (with-soft-assertions
        (expect (mapcar (lambda (entry) (outline-entry-name entry)) level-two)
                :to-equal (list "Three"))
        (expect (mapcar (lambda (entry) (outline-entry-name entry)) max-level-two)
                :to-equal (list "One" "Three")))))

  (it "scans document lists in order and deduplicates repeated nodes"
    (let* ((first-document (parse-org "* First"))
           (second-document (parse-org "* Second"))
           (entries
             (refile-target-candidates
              first-document
              (list (list nil t)
                    (list (list second-document first-document) t)
                    (list first-document t))))
           (names (mapcar (lambda (entry) (outline-entry-name entry)) entries)))
      (with-soft-assertions
        (expect names :to-equal (list "First" "Second"))
        (expect (mapcar (lambda (entry)
                          (eq (node-document (outline-entry-node entry))
                              first-document))
                        entries)
                :to-equal (list t nil))
        (expect (document-text first-document) :to-equal "* First")
        (expect (document-text second-document) :to-equal "* Second"))))

  (it "rejects invalid target specifications with the invalid-value reason"
    (let* ((document (parse-org "* Entry"))
           (malformed
             (handler-case
                 (progn
                   (refile-target-candidates document (list (list nil t nil)))
                   nil)
               (org-edit-error (condition) condition)))
           (unsupported
             (handler-case
                 (progn
                   (refile-target-candidates
                    document (list (list nil (cons :regexp "Entry"))))
                   nil)
               (org-edit-error (condition) condition)))
           (invalid-level
             (handler-case
                 (progn
                   (refile-target-candidates
                    document (list (list nil (cons :level 0))))
                   nil)
               (org-edit-error (condition) condition)))
           (invalid-document
             (handler-case
                 (progn
                   (refile-target-candidates document
                                             (list (list "entry.org" t)))
                   nil)
               (org-edit-error (condition) condition))))
      (with-soft-assertions
        (expect (org-edit-error-reason malformed) :to-be :invalid-value)
        (expect (org-edit-error-reason unsupported) :to-be :invalid-value)
        (expect (org-edit-error-reason invalid-level) :to-be :invalid-value)
        (expect (org-edit-error-reason invalid-document) :to-be :invalid-value)
        (signals type-error
          (refile-target-candidates document nil :limits "invalid")))))

  (it "returns no candidates for an empty document and preserves input nodes"
    (let* ((empty (parse-org ""))
           (source (format nil "~A~%~A~%~A"
                           "* Parent :local:"
                           "** Child"
                           "body"))
           (document (parse-org source))
           (root (document-root document))
           (before
             (let ((properties nil))
               (map-nodes
                (lambda (node)
                  (push (list (node-type node) (node-properties node))
                        properties))
                root)
               (nreverse properties)))
           (entries
             (refile-target-candidates
              document
              (list (list nil t) (list nil (cons :tag "local")))))
           (after
             (let ((properties nil))
               (map-nodes
                (lambda (node)
                  (push (list (node-type node) (node-properties node))
                        properties))
                root)
               (nreverse properties))))
      (with-soft-assertions
        (expect (refile-target-candidates empty nil) :to-equal nil)
        (expect (document-root document) :to-be root)
        (expect (document-text document) :to-equal source)
        (expect before :to-equal after)
        (expect (mapcar (lambda (entry) (outline-entry-name entry)) entries)
                :to-equal (list "Parent" "Child")))))

  (it "enforces node traversal limits"
    (let* ((document (parse-org "* Entry"))
           (condition
             (handler-case
                 (progn
                   (refile-target-candidates
                    document (list (list nil t))
                    :limits (make-limits :max-node-count 0))
                   nil)
               (org-resource-limit-error (condition) condition))))
      (with-soft-assertions
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when condition
          (expect (org-resource-limit-error-limit-name condition)
                  :to-be :max-node-count)
          (expect (org-resource-limit-error-limit condition) :to-be 0)
          (expect (org-resource-limit-error-actual condition) :to-be 1))))))

;;;; t/agenda-search-view-test.lisp
(in-package #:cl-org-kit/test)

(defun agenda-search-view-test-titles (entries)
  (mapcar (lambda (entry)
            (node-property (agenda-match-entry-node entry) :raw-value))
          entries))

(defun agenda-search-view-test-error (function)
  (handler-case (progn (funcall function) nil)
    (error (condition) condition)))

(describe "Agenda search view"
  (it "returns exact matching headings in source order with phrase and veto terms"
    (let* ((document
             (parse-org
              (format nil "* Alpha~%a quiet amber phrase~%* Beta~%a cobalt phrase~%* Gamma~%a quiet amber phrase blocked~%")))
           (entries
             (agenda-search-view (list document) "+\"quiet amber phrase\" -blocked")))
      (with-soft-assertions
        (expect (agenda-search-view-test-titles entries) :to-equal '("Alpha"))
        (expect (agenda-match-entry-line (first entries)) :to-be 1)
        (expect (agenda-match-entry-heading-path (first entries))
                :to-equal '((:title "Alpha" :index 1))))))
  (it "uses phrases without a Boolean prefix and ANDs Boolean snippets"
    (let* ((document
             (parse-org
              (format nil
                      (concatenate
                       (quote string)
                       "* Phrase~%amber~%cobalt~%* Noncontiguous~%amber gap cobalt~%"
                       "* Reversed~%cobalt then amber~%* Amber only~%amber~%"
                       "* Cobalt only~%cobalt~%* Beta~%COBALT SIGNAL~%"
                       "* Gamma~%violet signal~%"))))
           (phrase-entries
             (agenda-search-view (list document) "amber cobalt"))
           (and-entries
             (agenda-search-view (list document) "+amber cobalt"))
           (regex-entries
             (agenda-search-view (list document) "+{cobalt.*signal}")))
      (with-soft-assertions
        (expect (agenda-search-view-test-titles phrase-entries)
                :to-equal (list "Phrase"))
        (expect (agenda-search-view-test-titles and-entries)
                :to-equal (list "Phrase" "Noncontiguous" "Reversed"))
        (expect (agenda-search-view-test-titles regex-entries)
                :to-equal (list "Beta")))))
  (it "treats quotes as literal text outside Boolean mode"
    (let* ((document
             (parse-org
              (format nil
                      "* Quoted~%the phrase \"amber cobalt\" appears~%* Unquoted~%the phrase amber cobalt appears~%")))
           (entries (agenda-search-view (list document) "\"amber cobalt\"")))
      (expect (agenda-search-view-test-titles entries) :to-equal (list "Quoted"))))
  (it "searches subtrees and supports Boolean queries with only veto terms"
    (let* ((document
             (parse-org
              (format nil
                      "* Root~%** Child~%needle in child~%* Blocked~%blocked text~%* Other~%ordinary text~%")))
           (subtree-entries (agenda-search-view (list document) "needle"))
           (veto-only-entries (agenda-search-view (list document) "-blocked")))
      (with-soft-assertions
        (expect (agenda-search-view-test-titles subtree-entries)
                :to-equal (list "Root" "Child"))
        (expect (agenda-search-view-test-titles veto-only-entries)
                :to-equal (list "Root" "Child" "Other")))))
  (it "signals query and agenda errors with their reasons"
    (let* ((document (parse-org ""))
           (bad-query
             (agenda-search-view-test-error
              (lambda () (agenda-search-view (list document) "+ "))))
           (bad-document
             (agenda-search-view-test-error
              (lambda () (agenda-search-view '("not a document") "term")))))
      (with-soft-assertions
        (expect (typep bad-query 'org-query-error) :to-be t)
        (when (typep bad-query 'org-query-error)
          (expect (org-query-error-position bad-query) :to-be 0))
        (expect (typep bad-document 'org-agenda-error) :to-be t)
        (when (typep bad-document 'org-agenda-error)
          (expect (org-agenda-error-reason bad-document)
                  :to-be :unsupported-entry)))))
  (it "enforces search input, regex-length, and regex-work limits"
    (let* ((input-limited
             (parse-org "" :limits (make-limits :max-input-length 3)))
           (regex-limited
             (parse-org "* Root" :limits
                        (make-limits :max-input-length 16
                                     :max-search-regex-length 2)))
           (work-limited
             (parse-org (format nil "* Root~%a long needle~%") :limits
                        (make-limits :max-search-work 4)))
           (input-error
             (agenda-search-view-test-error
              (lambda () (agenda-search-view (list input-limited) "long"))))
           (regex-error
             (agenda-search-view-test-error
              (lambda () (agenda-search-view (list regex-limited) "{abc}"))))
           (work-error
             (agenda-search-view-test-error
              (lambda () (agenda-search-view (list work-limited) "{needle}")))))
      (with-soft-assertions
        (expect (typep input-error 'org-resource-limit-error) :to-be t)
        (when (typep input-error 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name input-error)
                  :to-be :max-input-length))
        (expect (typep regex-error 'org-resource-limit-error) :to-be t)
        (when (typep regex-error 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name regex-error)
                  :to-be :max-search-regex-length))
        (expect (typep work-error 'org-resource-limit-error) :to-be t)
        (when (typep work-error 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name work-error)
                  :to-be :max-search-work))))))

;;;; t/outline-test.lisp
(in-package #:cl-org-kit/test)

(defun outline-test-lines (&rest lines)
  (format nil "~{~A~^~%~}" lines))

(describe "Org outline"
  (it "returns headline entries with document order, logical levels, and subtree ranges"
    (let* ((source (outline-test-lines "#+NAME: Parent ref"
                                       "* Parent"
                                       "intro"
                                       "** Child"
                                       "body"
                                       "**** Gap"
                                       "last"
                                       "* Sibling"
                                       "tail"))
           (document (parse-org source :source-name "outline.org"))
           (result (outline document))
           (entries (outline-result-symbols result)))
      (with-soft-assertions
        (expect (outline-result-p result) :to-be t)
        (expect (outline-result-path result) :to-equal "outline.org")
        (expect (outline-result-lang result) :to-be :org)
        (expect (outline-result-total result) :to-be 4)
        (expect (outline-result-truncated result) :to-be nil)
        (expect (mapcar (lambda (entry)
                          (list (outline-entry-kind entry)
                                (outline-entry-name entry)
                                (outline-entry-level entry)
                                (outline-entry-line entry)
                                (outline-entry-end-line entry)))
                        entries)
                :to-equal '((:headline "Parent" 1 2 7)
                            (:headline "Child" 2 4 7)
                            (:headline "Gap" 4 6 7)
                            (:headline "Sibling" 1 8 9)))
        (expect (outline-entry-p (first entries)) :to-be t)
        (expect (node-type (outline-entry-node (first entries)))
                :to-be :headline)
        (expect (outline-entry-p :not-an-entry) :to-be nil))))

  (it "uses the parser's resolved odd headline levels"
    (let* ((document
             (parse-org (outline-test-lines "#+STARTUP: odd"
                                            "* One"
                                            "*** Two"
                                            "***** Three")))
           (entries (outline-result-symbols (outline document))))
      (expect (mapcar #'outline-entry-level entries) :to-equal '(1 2 3))))

  (it "handles empty input, an unparsed document, and a trailing newline"
    (let* ((empty (outline (parse-org "")))
           (unparsed (outline (make-document "")))
           (trailing
             (outline (parse-org (concatenate 'string "* End" (string #\Newline)))))
           (entry (first (outline-result-symbols trailing))))
      (with-soft-assertions
        (expect (outline-result-symbols empty) :to-equal '())
        (expect (outline-result-total empty) :to-be 0)
        (expect (outline-result-truncated empty) :to-be nil)
        (expect (outline-result-path empty) :to-be nil)
        (expect (outline-result-symbols unparsed) :to-equal '())
        (expect (outline-entry-line entry) :to-be 1)
        (expect (outline-entry-end-line entry) :to-be 1))))

  (it "uses node ranges for CRLF and mixed line endings"
    (let* ((crlf (concatenate 'string
                              "* First" (string #\Return) (string #\Newline)
                              "body" (string #\Return) (string #\Newline)
                              "* Second"))
           (mixed (concatenate 'string
                               "* First" (string #\Newline)
                               "body" (string #\Return) (string #\Newline)
                               "** Child"))
           (crlf-entries (outline-result-symbols (outline (parse-org crlf))))
           (mixed-entries (outline-result-symbols (outline (parse-org mixed)))))
      (with-soft-assertions
        (expect (mapcar (lambda (entry)
                          (list (outline-entry-line entry)
                                (outline-entry-end-line entry)))
                        crlf-entries)
                :to-equal '((1 2) (3 3)))
        (expect (mapcar (lambda (entry)
                          (list (outline-entry-line entry)
                                (outline-entry-end-line entry)))
                        mixed-entries)
                :to-equal '((1 3) (3 3))))))

  (it "lists named and unnamed src/example blocks in source order"
    (let* ((source (outline-test-lines "* Code"
                                       "#+NAME: run-lisp"
                                       "#+begin_src lisp"
                                       "* literal source"
                                       "#+end_src"
                                       "#+begin_example"
                                       "* literal example"
                                       "#+end_example"
                                       "#+begin_src python"
                                       "print(1)"
                                       "#+end_src"
                                       "#+NAME: example-name"
                                       "#+begin_example"
                                       "* literal named example"
                                       "#+end_example"
                                       "* After"
                                       "done"))
           (entries (outline-result-symbols (outline (parse-org source)))))
      (with-soft-assertions
        (expect (mapcar #'outline-entry-kind entries)
                :to-equal '(:headline :src-block :example-block :src-block
                            :example-block :headline))
        (expect (mapcar #'outline-entry-name entries)
                :to-equal '("Code" "run-lisp" "" "python" "example-name" "After"))
        (expect (mapcar #'outline-entry-level entries)
                :to-equal '(1 nil nil nil nil 1))
        (expect (mapcar (lambda (entry)
                          (list (outline-entry-line entry)
                                (outline-entry-end-line entry)))
                        entries)
                :to-equal '((1 15) (2 5) (6 8) (9 11) (12 15) (16 17)))
        (expect (outline-result-total (outline (parse-org source))) :to-be 6))))

  (it "reports total and truncation at zero and non-truncating limits"
    (let* ((document (parse-org (outline-test-lines "* First" "* Second")))
           (all (outline document))
           (zero (outline document :limit 0))
           (one (outline document :limit 1))
           (boundary (outline document :limit 2))
           (above (outline document :limit 3))
           (empty-zero (outline (parse-org "") :limit 0)))
      (with-soft-assertions
        (expect (outline-result-total all) :to-be 2)
        (expect (length (outline-result-symbols zero)) :to-be 0)
        (expect (outline-result-total zero) :to-be 2)
        (expect (outline-result-truncated zero) :to-be t)
        (expect (length (outline-result-symbols one)) :to-be 1)
        (expect (outline-result-truncated one) :to-be t)
        (expect (length (outline-result-symbols boundary)) :to-be 2)
        (expect (outline-result-truncated boundary) :to-be nil)
        (expect (length (outline-result-symbols above)) :to-be 2)
        (expect (outline-result-truncated above) :to-be nil)
        (expect (outline-result-total empty-zero) :to-be 0)
        (expect (outline-result-truncated empty-zero) :to-be nil)
        (expect (outline-result-p (make-outline-result)) :to-be t)
        (signals type-error (outline document :limit -1))))))

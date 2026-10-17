;;;; t/link-resolution-test.lisp
(in-package #:cl-org-kit/test)

(defun link-resolution-test-link (document &optional type)
  (let ((result nil))
    (map-nodes
     (lambda (node)
       (when (and (null result)
                  (eq (node-type node) :link)
                  (or (null type)
                      (string-equal type (node-property node :type))))
         (setf result node)))
     (document-root document))
    result))

(defun link-resolution-test-parse-link (source &rest parse-arguments)
  (let* ((document (apply #'parse-org source parse-arguments))
         (link (link-resolution-test-link document)))
    (values link document)))

(defun link-resolution-test-catch-error (function)
  (handler-case (progn (funcall function) nil)
    (error (condition) condition)))

(defun link-resolution-test-heading (document title)
  (let ((result nil))
    (map-nodes
     (lambda (node)
       (when (and (null result)
                  (member (node-type node) '(:headline :inlinetask) :test #'eq)
                  (string= title (node-property node :raw-value)))
         (setf result node)))
     (document-root document))
    result))

(describe "link resolution"
  (it "resolves IDs, custom IDs, fuzzy headings, named elements, targets and radio targets"
    (let* ((document
             (parse-org
              (format nil
                      "* Heading~%:PROPERTIES:~%:ID: entry-1~%:CUSTOM_ID: custom-1~%:END:~%#+NAME: named-example~%#+BEGIN_EXAMPLE~%Example body~%#+END_EXAMPLE~%~%<<named-target>>~%~%<<<radio phrase>>> radio phrase.")))
           (heading (link-resolution-test-heading document "Heading"))
           (id-link (nth-value 0 (link-resolution-test-parse-link "[[id:entry-1]]")))
           (custom-link (nth-value 0 (link-resolution-test-parse-link "[[#custom-1]]")))
           (fuzzy-link (nth-value 0 (link-resolution-test-parse-link "[[Heading]]")))
           (name-link (nth-value 0 (link-resolution-test-parse-link "[[named-example]]")))
           (target-link (nth-value 0 (link-resolution-test-parse-link "[[named-target]]")))
           (radio-document
             (parse-org "<<<radio phrase>>> radio phrase."))
           (radio-link (link-resolution-test-link radio-document "radio"))
           (id-result (resolve-link id-link document))
           (custom-result (resolve-link custom-link document))
           (fuzzy-result (resolve-link fuzzy-link document))
           (name-result (resolve-link name-link document))
           (target-result (resolve-link target-link document))
           (radio-result (resolve-link radio-link radio-document)))
      (with-soft-assertions
        (expect (link-resolution-status id-result) :to-be :resolved)
        (expect (link-resolution-target id-result) :to-be heading)
        (expect (link-resolution-candidates id-result) :to-equal (list heading))
        (expect (link-resolution-status custom-result) :to-be :resolved)
        (expect (link-resolution-target custom-result) :to-be heading)
        (expect (link-resolution-status fuzzy-result) :to-be :resolved)
        (expect (link-resolution-target fuzzy-result) :to-be heading)
        (expect (link-resolution-status name-result) :to-be :resolved)
        (expect (node-property (link-resolution-target name-result) :name)
                :to-equal "named-example")
        (expect (link-resolution-status target-result) :to-be :resolved)
        (expect (node-type (link-resolution-target target-result)) :to-be :target)
        (expect (link-resolution-status radio-result) :to-be :resolved)
        (expect (node-type (link-resolution-target radio-result))
                :to-be :radio-target))))

  (it "returns unresolved results and preserves every ambiguous candidate in document order"
    (let* ((first-document (parse-org "* Duplicate"))
           (second-document (parse-org "* Duplicate"))
           (link (nth-value 0 (link-resolution-test-parse-link "[[Duplicate]]")))
           (ambiguous (resolve-link link (list first-document second-document)))
           (missing-link (nth-value 0 (link-resolution-test-parse-link "[[Missing]]")))
           (unresolved (resolve-link missing-link first-document))
           (empty-documents (resolve-link missing-link nil)))
      (with-soft-assertions
        (expect (link-resolution-status ambiguous) :to-be :ambiguous)
        (expect (link-resolution-target ambiguous) :to-be nil)
        (expect (mapcar #'node-document (link-resolution-candidates ambiguous))
                :to-equal (list first-document second-document))
        (expect (link-resolution-status unresolved) :to-be :unresolved)
        (expect (link-resolution-target unresolved) :to-be nil)
        (expect (link-resolution-candidates unresolved) :to-equal nil)
        (expect (link-resolution-status empty-documents) :to-be :unresolved))))

  (it "uses the file resolver and handles all supported file search options"
    (let* ((file-document
             (parse-org
              (format nil
                      "* Chapter~%:PROPERTIES:~%:CUSTOM_ID: chapter-id~%:END:~%Text with Needle here.~%~%<<spot>>")))
           (main-document (parse-org "[[file:manual.org::*Chapter]]"
                                     :source-name "main.org"))
           (main-link (link-resolution-test-link main-document))
           (whole-file-link
             (nth-value 0 (link-resolution-test-parse-link
                           "[[file:manual.org]]" :source-name "main.org")))
           (calls nil)
           (resolver (lambda (path parent-source)
                       (push (list path parent-source) calls)
                       file-document))
           (searches '( ("*Chapter" :headline)
                        ("#chapter-id" :headline)
                        ("<<spot>>" :target)
                        ("/needle/" :plain-text)
                        ("Needle" :plain-text))))
      (with-soft-assertions
        (dolist (entry searches)
          (let* ((source (format nil "[[file:manual.org::~A]]" (first entry)))
                 (link (nth-value 0
                                  (link-resolution-test-parse-link
                                   source :source-name "main.org")))
                 (result (resolve-link link main-document :file-resolver resolver)))
            (expect (link-resolution-status result) :to-be :resolved)
            (expect (node-type (link-resolution-target result)) :to-be (second entry))))
        (expect (nreverse calls)
                :to-equal
                '(("manual.org" "main.org")
                  ("manual.org" "main.org")
                  ("manual.org" "main.org")
                  ("manual.org" "main.org")
                  ("manual.org" "main.org")))
        (expect (resolve-link main-link main-document :file-resolver resolver)
                :to-be-truthy)
        (let ((source-result
                (resolve-link
                 whole-file-link main-document
                 :file-resolver
                 (lambda (path parent-source)
                   (declare (ignore parent-source))
                   (format nil "#+TITLE: ~A" path)))))
          (expect (link-resolution-status source-result) :to-be :resolved)
          (expect (document-source-name (link-resolution-target source-result))
                  :to-equal "manual.org")))))

  (it "searches the current document for file links with an empty path"
    (let* ((document
             (parse-org
              (format nil "* Chapter~%~%[[file:::*Chapter]]")))
           (link (link-resolution-test-link document))
           (result (resolve-link link document)))
      (expect (link-resolution-status result) :to-be :resolved)
      (expect (link-resolution-target result)
              :to-be (link-resolution-test-heading document "Chapter"))))

  (it "reports invalid file search regular expressions and invalid link nodes"
    (let* ((document (parse-org "* Target"))
           (bad-regex-link
             (make-node :link
                        :properties '(:type "file" :path "manual.org::/[/")))
           (bad-regex
             (link-resolution-test-catch-error
              (lambda ()
                (resolve-link bad-regex-link document
                              :file-resolver
                              (lambda (path parent-source)
                                (declare (ignore path parent-source))
                                document)))))
           (invalid-link
             (link-resolution-test-catch-error
              (lambda ()
                (resolve-link (make-node :plain-text) document)))))
      (with-soft-assertions
        (expect (typep bad-regex 'org-link-resolution-error) :to-be t)
        (when (typep bad-regex 'org-link-resolution-error)
          (expect (org-link-resolution-error-reason bad-regex)
                  :to-be :invalid-search-option))
        (expect (typep invalid-link 'org-link-resolution-error) :to-be t)
        (when (typep invalid-link 'org-link-resolution-error)
          (expect (org-link-resolution-error-reason invalid-link)
                  :to-be :invalid-link-node)))))

  (it "reports missing, absent and invalid file resolver results explicitly"
    (let* ((document (parse-org "[[file:missing.org]]" :source-name "main.org"))
           (link (link-resolution-test-link document))
           (missing-resolver
             (link-resolution-test-catch-error
              (lambda () (resolve-link link document))))
           (invalid-resolver
             (link-resolution-test-catch-error
              (lambda ()
                (resolve-link link document :file-resolver :invalid))))
           (missing-file
             (link-resolution-test-catch-error
              (lambda ()
                (resolve-link link document :file-resolver
                              (lambda (path parent-source)
                                (declare (ignore path parent-source))
                                nil)))))
           (invalid-result
             (link-resolution-test-catch-error
              (lambda ()
                (resolve-link link document :file-resolver
                              (lambda (path parent-source)
                                (declare (ignore path parent-source))
                                42))))))
      (with-soft-assertions
        (expect (typep missing-resolver 'org-link-resolution-error) :to-be t)
        (when (typep missing-resolver 'org-link-resolution-error)
          (expect (org-link-resolution-error-reason missing-resolver)
                  :to-be :missing-file-resolver))
        (expect (typep invalid-resolver 'org-link-resolution-error) :to-be t)
        (when (typep invalid-resolver 'org-link-resolution-error)
          (expect (org-link-resolution-error-reason invalid-resolver)
                  :to-be :invalid-file-resolver))
        (expect (typep missing-file 'org-link-resolution-error) :to-be t)
        (when (typep missing-file 'org-link-resolution-error)
          (expect (org-link-resolution-error-reason missing-file)
                  :to-be :file-not-found))
        (expect (typep invalid-result 'org-link-resolution-error) :to-be t)
        (when (typep invalid-result 'org-link-resolution-error)
          (expect (org-link-resolution-error-reason invalid-result)
                  :to-be :invalid-file-resolver-result)))))

  (it "passes raw and percent-encoded UTF-8 LINK suffixes to the resolver"
    (let* ((source
             (format nil
                     "#+LINK: raw file:%s~%#+LINK: hex file:%h~%~%[[raw:manual.org]] [[hex:a b/é]]"))
           (document (parse-org source :source-name "caller.org"))
           (links (remove-if-not
                   (lambda (node) (eq (node-type node) :link))
                   (let ((nodes nil))
                     (map-nodes (lambda (node)
                                  (when (eq (node-type node) :link)
                                    (push node nodes)))
                                (document-root document))
                     (nreverse nodes))))
           (calls nil)
           (resolver (lambda (path parent-source)
                       (push (list path parent-source) calls)
                       (parse-org "* Resolved"))))
      (with-soft-assertions
        (dolist (link links)
          (expect (link-resolution-status
                   (resolve-link link document :file-resolver resolver))
                  :to-be :resolved))
        (expect (nreverse calls)
                :to-equal
                '(("manual.org" "caller.org")
                  ("a%20b%2F%C3%A9" "caller.org"))))))

  (it "signals the named condition for function-form LINK abbreviations"
    (let* ((document
             (parse-org
              (format nil "#+LINK: calc %(identity %s)~%~%[[calc:input]]")))
           (link (link-resolution-test-link document))
           (condition
             (link-resolution-test-catch-error
              (lambda () (resolve-link link document)))))
      (with-soft-assertions
        (expect (typep condition 'org-link-resolution-error) :to-be t)
        (when (typep condition 'org-link-resolution-error)
          (expect (org-link-resolution-error-reason condition)
                  :to-be :unsupported-link-function)
          (expect (org-link-resolution-error-context condition)
                  :to-equal "calc:input"))))))

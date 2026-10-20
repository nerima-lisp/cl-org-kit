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
  (it "copies candidates at resolution construction and access"
  (let* ((source-candidates (list :first :second))
         (resolution
           (org-kit::make-link-resolution
            :status :ambiguous :candidates source-candidates))
         (returned-candidates (link-resolution-candidates resolution))
         (invalid-status
           (link-resolution-test-catch-error
            (lambda ()
              (org-kit::make-link-resolution :status :unknown)))))
    (setf (first source-candidates) :changed
          (first returned-candidates) :changed)
    (with-soft-assertions
      (expect (link-resolution-p resolution) :to-be t)
      (expect (link-resolution-p nil) :to-be nil)
      (expect (link-resolution-status resolution) :to-be :ambiguous)
      (expect (link-resolution-target resolution) :to-be nil)
      (expect (link-resolution-candidates resolution) :to-equal '(:first :second))
      (expect (typep invalid-status 'type-error) :to-be t))))

  (it "matches targets and names before headings and normalizes heading text like Org"
    (let* ((document
             (parse-org
              (format nil
                      "* Target Match~%<<Target Match>>~%* Named Match~%#+NAME: Named Match~%#+BEGIN_EXAMPLE~%named~%#+END_EXAMPLE~%* Section   Title [2/4]~%* Progress Report [75%]~%")))
           (target-link (nth-value 0
                                   (link-resolution-test-parse-link
                                    "[[target   match]]")))
           (name-link (nth-value 0
                                 (link-resolution-test-parse-link
                                  "[[named  match]]")))
           (heading-link (nth-value 0
                                    (link-resolution-test-parse-link
                                     "[[section title]]")))
           (starred-link (nth-value 0
                                    (link-resolution-test-parse-link
                                     "[[*Section title]]")))
           (progress-link (nth-value 0
                                     (link-resolution-test-parse-link
                                      "[[progress report]]")))
           (target-result (resolve-link target-link document))
           (name-result (resolve-link name-link document))
           (heading-result (resolve-link heading-link document))
           (starred-result (resolve-link starred-link document))
           (progress-result (resolve-link progress-link document)))
      (with-soft-assertions
        (expect (node-type (link-resolution-target target-result))
                :to-be :target)
        (expect (node-property (link-resolution-target name-result) :name)
                :to-equal "Named Match")
        (expect (node-type (link-resolution-target heading-result))
                :to-be :headline)
        (expect (node-type (link-resolution-target starred-result))
                :to-be :headline)
        (expect (node-property (link-resolution-target starred-result) :raw-value)
                :to-equal "Section   Title [2/4]")
        (expect (node-property (link-resolution-target progress-result) :raw-value)
                :to-equal "Progress Report [75%]"))))

  (it "matches CUSTOM_ID values with exact case and uses one index for repeated resolution"
    (let* ((document
             (parse-org
              (format nil
                      "* Target~%:PROPERTIES:~%:CUSTOM_ID: Case-Sensitive~%:END:~%[[#case-sensitive]]")))
           (wrong-case-link (link-resolution-test-link document "custom-id"))
           (index (org-kit::make-link-resolution-index document))
           (wrong-case-result (org-kit::resolve-link-with-index
                               wrong-case-link index))
           (repeated-result (org-kit::resolve-link-with-index
                             wrong-case-link index)))
      (with-soft-assertions
        (expect (link-resolution-status wrong-case-result) :to-be :unresolved)
        (expect (link-resolution-status repeated-result) :to-be :unresolved))))

  (it "reports consistent document errors from link and ID indexing"
    (let* ((link (nth-value 0 (link-resolution-test-parse-link "[[id:missing]]")))
           (documents (list (parse-org "* Target") 42))
           (link-error (link-resolution-test-catch-error
                        (lambda () (resolve-link link documents))))
           (id-error (link-resolution-test-catch-error
                      (lambda () (make-org-id-index documents)))))
      (with-soft-assertions
        (expect (typep link-error 'org-id-error) :to-be t)
        (expect (typep id-error 'org-id-error) :to-be t)
        (when (and (typep link-error 'org-id-error)
                   (typep id-error 'org-id-error))
          (expect (org-id-error-reason link-error)
                  :to-be (org-id-error-reason id-error))))))

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

  (it "reuses a per-document search index for repeated file links"
    (let* ((main-document (parse-org "[[file:manual.org::<<spot>>]]"))
           (link (link-resolution-test-link main-document))
           (file-document (parse-org "<<spot>>"))
           (index (org-kit::make-link-resolution-index main-document))
           (resolver (lambda (path parent-source)
                       (declare (ignore path parent-source))
                       file-document))
           (first-result (org-kit::resolve-link-with-index
                          link index :file-resolver resolver))
           (cached-index
             (gethash file-document
                      (org-kit::%link-resolution-index-document-indices index)))
           (second-result (org-kit::resolve-link-with-index
                           link index :file-resolver resolver)))
      (with-soft-assertions
        (expect (link-resolution-status first-result) :to-be :resolved)
        (expect (node-type (link-resolution-target first-result)) :to-be :target)
        (expect cached-index :to-be-truthy)
        (expect (gethash file-document
                         (org-kit::%link-resolution-index-document-indices index))
                :to-be cached-index)
        (expect (link-resolution-status second-result) :to-be :resolved))))

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

  (it "enforces the dedicated file search regex length limit"
    (let* ((document
             (parse-org "* Target"
                        :limits (make-limits :max-search-regex-length 3)))
           (link (nth-value 0
                            (link-resolution-test-parse-link
                             "[[file:manual.org::/needle/]]")))
           (condition
             (link-resolution-test-catch-error
              (lambda ()
                (resolve-link link document
                              :file-resolver
                              (lambda (path parent-source)
                                (declare (ignore path parent-source))
                                document))))))
      (with-soft-assertions
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when (typep condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name condition)
                  :to-be :max-search-regex-length)
          (expect (org-resource-limit-error-limit condition) :to-be 3)
          (expect (org-resource-limit-error-actual condition) :to-be 6)))))

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
                  :to-equal "calc:input")))))
  (it "keeps malformed search keys and reports invalid link resolution inputs"
  (let* ((malformed-document (parse-org "* Report [12/x]"))
         (malformed-link
           (make-node :link :properties (list :type "fuzzy" :path "Report [12/x]")))
         (malformed-result (resolve-link malformed-link malformed-document))
         (source-document
           (parse-org
            (format nil
                    "#+BEGIN_SRC text~%needle in the source block~%#+END_SRC~%~%[[file:::needle]]")))
         (source-block
           (let ((found nil))
             (map-nodes (lambda (node)
                          (when (and (null found) (eq (node-type node) :src-block))
                            (setf found node)))
                        (document-root source-document))
             found))
         (source-link (link-resolution-test-link source-document))
         (missing-path (make-node :link :properties '(:type "file")))
         (missing-path-condition
           (link-resolution-test-catch-error
            (lambda () (resolve-link missing-path malformed-document))))
         (missing-parent-link
           (make-node :link :properties '(:type "file" :path "::*Heading")))
         (missing-parent-condition
           (link-resolution-test-catch-error
            (lambda () (resolve-link missing-parent-link nil))))
         (invalid-id-link (make-node :link :properties '(:type "id" :path " ")))
         (invalid-id-result (resolve-link invalid-id-link malformed-document))
         (unknown-link
           (make-node :link :properties '(:type "unknown" :path "value")))
         (unknown-result (resolve-link unknown-link malformed-document))
         (non-node-condition
           (link-resolution-test-catch-error
            (lambda () (resolve-link 42 malformed-document))))
         (invalid-index-condition
           (link-resolution-test-catch-error
            (lambda ()
              (org-kit::resolve-link-with-index malformed-link nil)))))
    (setf (node-property source-block :value) nil)
    (let ((source-result (resolve-link source-link source-document))
          (report (format nil "~A" missing-path-condition)))
      (with-soft-assertions
        (expect (link-resolution-status malformed-result) :to-be :resolved)
        (expect (node-property (link-resolution-target malformed-result) :raw-value)
                :to-equal "Report [12/x]")
        (expect (link-resolution-status source-result) :to-be :resolved)
        (expect (node-type (link-resolution-target source-result)) :to-be :src-block)
        (expect (typep missing-path-condition 'org-link-resolution-error) :to-be t)
        (when (typep missing-path-condition 'org-link-resolution-error)
          (expect (org-link-resolution-error-reason missing-path-condition)
                  :to-be :invalid-file-path)
          (expect (search "invalid-file-path" report) :not :to-be nil))
        (expect (typep missing-parent-condition 'org-link-resolution-error) :to-be t)
        (when (typep missing-parent-condition 'org-link-resolution-error)
          (expect (org-link-resolution-error-reason missing-parent-condition)
                  :to-be :missing-parent-document))
        (expect (link-resolution-status invalid-id-result) :to-be :unresolved)
        (expect (link-resolution-status unknown-result) :to-be :unresolved)
        (expect (typep non-node-condition 'type-error) :to-be t)
        (expect (typep invalid-index-condition 'type-error) :to-be t)))))
  (it "enforces cumulative regexp search work per indexed document"
    (let* ((main-document (parse-org "[[file:manual.org::/.*/]]"))
           (link (link-resolution-test-link main-document))
           (file-document
             (parse-org "* Alpha"
                        :limits (make-limits :max-search-work 5)))
           (index (org-kit::make-link-resolution-index main-document))
           (resolver (lambda (path parent-source)
                       (declare (ignore path parent-source))
                       file-document))
           (first-result (org-kit::resolve-link-with-index
                          link index :file-resolver resolver))
           (condition
             (link-resolution-test-catch-error
              (lambda ()
                (org-kit::resolve-link-with-index
                 link index :file-resolver resolver)))))
      (with-soft-assertions
        (expect (link-resolution-status first-result) :to-be :resolved)
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when (typep condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name condition)
                  :to-be :max-search-work)
          (expect (org-resource-limit-error-limit condition) :to-be 5)
          (expect (org-resource-limit-error-actual condition) :to-be 10)))))
  (it "covers link-resolution boundary and malformed input branches"
      (let* ((document (parse-org "* Target"))
             (index (org-kit::make-link-resolution-index document))
             (default-resolution (org-kit::make-link-resolution))
             (raw-resolution (org-kit::%make-link-resolution :resolved nil nil))
             (cookie-results
              (mapcar
               (lambda (arguments)
                 (apply #'org-kit::link-resolution-statistics-cookie-end arguments))
               '(( "[" 1) ("[" 0) ("[x]" 0) ("[12" 0) ("[12x]" 0)
                 ("[12/34]" 0) ("[12/" 0) ("[12%" 0))))
             (invalid-status
              (link-resolution-test-catch-error
               (lambda ()
                 (let ((status (gensym "INVALID-STATUS-")))
                   (org-kit::%make-link-resolution status nil nil)))))
             (duplicate-document
              (parse-org
               (format nil
                       "* Repeated~%:PROPERTIES:~%:CUSTOM_ID: repeated~%:CUSTOM_ID: repeated~%:END:~%")))
             (duplicate-index (org-kit::make-link-resolution-index duplicate-document))
             (unparsed-document (make-document ""))
             (unparsed-index (org-kit::make-link-resolution-index unparsed-document))
             (unparsed-result
              (org-kit::resolve-link-with-index
               (make-node :link :properties '(:type "fuzzy" :path "Missing"))
               unparsed-index))
             (malformed-results
              (mapcar
               (lambda (properties)
                 (link-resolution-status
                  (org-kit::resolve-link-with-index
                   (make-node :link :properties properties)
                   index)))
               '((:type 42 :path "Target")
                 (:type "id" :path 42)
                 (:type "custom-id" :path 42)
                 (:type "fuzzy" :path 42)
                 (:type "radio" :path 42))))
             (private-path-error
              (link-resolution-test-catch-error
               (lambda () (org-kit::link-resolution-read-file nil nil nil))))
             (empty-option-candidates
              (org-kit::link-resolution-file-search-candidates index document ""))
             (non-string-search-match
              (org-kit::link-resolution-search-text-matches-p
               index document "target" 42 nil)))
        (with-soft-assertions
         (expect (link-resolution-status default-resolution) :to-be :unresolved)
         (expect (link-resolution-status raw-resolution) :to-be :resolved)
         (expect (link-resolution-target raw-resolution) :to-be nil)
         (expect (link-resolution-candidates raw-resolution) :to-equal nil)
         (expect cookie-results :to-equal '(nil nil nil nil nil 7 nil nil))
         (expect (typep invalid-status 'type-error) :to-be t)
         (expect (length (gethash "repeated"
                                  (org-kit::%link-resolution-index-custom-ids
                                   duplicate-index)))
                 :to-be 1)
         (expect (document-root unparsed-document) :to-be nil)
         (expect (link-resolution-status unparsed-result) :to-be :unresolved)
         (expect malformed-results
                 :to-equal '(:unresolved :unresolved :unresolved :unresolved
                                         :unresolved))
         (expect (typep private-path-error 'org-link-resolution-error) :to-be t)
         (when (typep private-path-error 'org-link-resolution-error)
           (expect (org-link-resolution-error-reason private-path-error)
                   :to-be :invalid-file-path))
         (expect (length empty-option-candidates) :to-be 1)
         (expect non-string-search-match :to-be nil)))))

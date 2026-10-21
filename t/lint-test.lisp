(in-package #:cl-org-kit/test)

(defun lint-test-component-paths ()
  (loop for component in (asdf:component-children
                          (asdf:find-system "cl-org-kit/test"))
        for path = (asdf:component-pathname component)
        when (and path (uiop:string-suffix-p "-test.lisp" (namestring path)))
          collect (namestring path)))

(defun lint-test-files-on-disk ()
  (loop for path in (uiop:directory-files
                     (asdf:system-relative-pathname "cl-org-kit" "t/"))
        when (uiop:string-suffix-p "-test.lisp" (namestring path))
          collect (namestring path)))
(defun lint-test-selected-messages (document checker &optional context)
  (mapcar #'org-kit::org-lint-diagnostic-message
          (org-lint document
                    :context context
                    :checkers (list checker))))
(defun lint-test-babel-header-context (complete-p)
  (list :babel-header-specifications
        (list :complete complete-p
              :common
              (list (cons "results" :any)
                    (cons "exports"
                          (list (list "code" "results" "both" "none"))))
              :languages
              (list
               (cons "emacs-lisp"
                     (list (cons "elisp-only" :any)
                           (cons "exports"
                                 (list (list "code" "results" "both" "none")))))))))

(describe "Org lint registration and dispatch"
  (it "registers every test source in the canonical ASDF suite"
    (let ((registered (lint-test-component-paths))
          (on-disk (lint-test-files-on-disk)))
      (expect (set-difference registered on-disk :test #'string=) :to-equal '())
      (expect (set-difference on-disk registered :test #'string=) :to-equal '())))

  (it "registers all 61 Org checker names as dispatchable"
    (let* ((table org-kit::*org-lint-checkers*)
           (active (remove-if (lambda (checker)
                                (eq (third checker) :dependency-wait))
                              table)))
      (expect (length table) :to-be 61)
      (expect (count :document-only table :key #'third) :to-be 46)
      (expect (count :caller-material table :key #'third) :to-be 15)
      (expect (count :dependency-wait table :key #'third) :to-be 0)
      (expect (length active) :to-be 61)
      (expect (length (remove-duplicates table :key #'first :test #'string-equal))
              :to-be 61)))

  (it "returns no findings for a clean parsed document"
    (expect (org-lint (parse-org "Plain text.")) :to-equal '()))

  (it "accepts an explicit empty checker selection"
    (expect (org-lint (parse-org "#+BEGIN_SRC
body
#+END_SRC
")
                      :checkers '())
            :to-equal '()))

  (it "does not run context-dependent checks without required material"
    (expect (org-lint (parse-org "#+BEGIN_SRC not-a-language
body
#+END_SRC
")
                      :checkers '("suspicious-language-in-src-block"))
            :to-equal '()))
  (it "does not dispatch checkers marked :dependency-wait"
    (let* ((document (parse-org "[[id:missing-id]]"))
           (org-kit::*org-lint-checkers*
             (list (list "invalid-id-link"
                         (find-symbol "%LINT-INVALID-ID-LINK" :org-kit)
                         :dependency-wait nil))))
      (expect (org-lint document :context '(:known-ids nil))
              :to-equal nil)))

  (it "rejects an unknown checker name"
    (signals org-kit-error
      (org-lint (parse-org "Text.") :checkers '("not-a-checker"))))

  (it "requires an already parsed document"
    (signals org-kit-error (org-lint (make-document "Text.")))))
(describe "Org lint validation edge cases"
  (it "uses diagnostic defaults and validates explicit positions"
    (let ((diagnostic (org-kit::%make-org-lint-diagnostic)))
      (expect (org-kit::org-lint-diagnostic-checker diagnostic) :to-equal "")
      (expect (org-kit::org-lint-diagnostic-position diagnostic) :to-be 1)
      (expect (org-kit::org-lint-diagnostic-message diagnostic) :to-equal ""))
    (let ((diagnostic (org-kit::%make-org-lint-diagnostic
                       :checker "checker" :position 3 :message "message")))
      (expect (org-kit::org-lint-diagnostic-position diagnostic) :to-be 3)))
  (it "rejects invalid diagnostic values"
    (signals type-error (org-lint nil))
    (signals type-error
      (org-kit::%make-org-lint-diagnostic :checker 42))
    (signals type-error
      (org-kit::%make-org-lint-diagnostic :position 0))
    (signals type-error
      (org-kit::%make-org-lint-diagnostic :position "3"))
    (signals type-error
      (org-kit::%make-org-lint-diagnostic :message 42)))
  (it "rejects malformed context plist shapes"
    (let ((document (parse-org "Text.")))
      (signals org-kit-error (org-lint document :context :invalid))
      (signals org-kit-error
        (org-lint document :context (quote (:known-ids nil . malformed))))
      (signals org-kit-error
        (org-lint document :context (quote (known-ids nil))))
      (signals org-kit-error
        (org-lint document :context (quote (:known-ids nil :dangling))))
      (let ((cycle (list :known-ids nil)))
        (setf (cdr (last cycle)) cycle)
        (signals org-kit-error (org-lint document :context cycle)))))
  (it "rejects an unmatched keyword in the context plist"
    (signals org-kit-error
      (org-lint (parse-org "Text.") :context '(:not-a-pair))))
  (it "rejects malformed checker selections"
    (let ((document (parse-org "Text.")))
      (signals type-error
        (org-lint document :checkers (quote ("known" . malformed))))
      (signals type-error
        (org-lint document :checkers (quote (42))))
      (let ((cycle (list "known")))
        (setf (cdr (last cycle)) cycle)
        (expect (org-kit::%lint-string-list-p cycle) :to-be nil))))
  (it "rejects diagnostic offsets outside the document or with invalid types"
    (let ((document (parse-org "Text.")))
      (expect (org-kit::%lint-report document "test" -1 "message") :to-be nil)
      (expect (org-kit::%lint-report document "test" 6 "message") :to-be nil)
      (expect (org-kit::%lint-report document "test" "1" "message") :to-be nil)))
)
(describe "Org Babel lint caller material"
  (it "reports unknown source-block header arguments with the exact Org message"
    (let* ((document
             (parse-org "#+BEGIN_SRC emacs-lisp :bogus yes
body
#+END_SRC
"))
           (messages
             (lint-test-selected-messages
              document "wrong-header-argument"
              (lint-test-babel-header-context t))))
      (expect messages :to-equal
              (list "Unknown header argument \":bogus\""))))
  (it "reports a missing colon in a source-block header argument with the exact Org message"
    (let* ((document
             (parse-org "#+BEGIN_SRC emacs-lisp results output
body
#+END_SRC
"))
           (messages
             (lint-test-selected-messages
              document "wrong-header-argument"
              (lint-test-babel-header-context t))))
      (expect messages :to-equal
              (list "Missing colon in header argument \"RESULTS\""))))
  (it "accepts common and language-specific source-block header names"
    (let* ((document
             (parse-org "#+BEGIN_SRC emacs-lisp :exports code :elisp-only yes
body
#+END_SRC
"))
           (messages
             (lint-test-selected-messages
              document "wrong-header-argument"
              (lint-test-babel-header-context t))))
      (expect messages :to-equal nil)))
  (it "does not treat a colon-prefixed token parsed as the language as a header argument"
    (let* ((document
             (parse-org "#+BEGIN_SRC :elisp-only yes
body
#+END_SRC
"))
           (messages
             (lint-test-selected-messages
              document "wrong-header-argument"
              (lint-test-babel-header-context t))))
      (expect messages :to-equal nil)))
  (it "skips source-block header-name checks when specifications are absent or incomplete"
    (let ((document
            (parse-org "#+BEGIN_SRC emacs-lisp :bogus yes
body
#+END_SRC
")))
      (expect (lint-test-selected-messages document "wrong-header-argument")
              :to-equal nil)
      (expect (lint-test-selected-messages
               document "wrong-header-argument"
               (lint-test-babel-header-context nil))
              :to-equal nil)))
  (it "reports unknown source-block header values with the exact Org message"
    (let* ((document
             (parse-org "#+BEGIN_SRC emacs-lisp :exports invalid
body
#+END_SRC
"))
           (messages
             (lint-test-selected-messages
              document "wrong-header-value"
              (lint-test-babel-header-context t))))
      (expect messages :to-equal
              (list "Unknown value \"invalid\" for header \":exports\""))))
  (it "accepts language-specific values and rejects common-only values for the same header"
    (let ((context
            (list :babel-header-specifications
                  (list :complete t
                        :common
                        (list (cons "exports" (list (list "code"))))
                        :languages
                        (list (cons "emacs-lisp"
                                    (list (cons "exports"
                                                (list (list "both"))))))))))
      (expect (lint-test-selected-messages
               (parse-org "#+BEGIN_SRC emacs-lisp :exports both
body
#+END_SRC
")
               "wrong-header-value" context)
              :to-equal nil)
      (expect (lint-test-selected-messages
               (parse-org "#+BEGIN_SRC emacs-lisp :exports code
body
#+END_SRC
")
               "wrong-header-value" context)
              :to-equal
              (list "Unknown value \"code\" for header \":exports\""))))
  (it "skips source-block header-value checks when specifications are absent or incomplete"
    (let ((document
            (parse-org "#+BEGIN_SRC emacs-lisp :exports invalid
body
#+END_SRC
")))
      (expect (lint-test-selected-messages document "wrong-header-value")
              :to-equal nil)
      (expect (lint-test-selected-messages
               document "wrong-header-value"
               (lint-test-babel-header-context nil))
              :to-equal nil)))
  (it "reports named results when effective exports omit results"
    (let* ((document
             (parse-org "#+NAME: block
#+BEGIN_SRC emacs-lisp :exports code
(+ 1 2)
#+END_SRC
#+NAME: result
#+RESULTS: block
: 3
"))
           (block (first (org-kit::%lint-nodes document :src-block)))
           (begin (org-kit::node-begin block))
           (messages
             (lint-test-selected-messages
              document "named-result"
              (list :babel-effective-exports
                    (list (cons begin "code"))))))
      (expect messages :to-equal
              (list "Links to \"result\" will not be valid during export unless the parent source block has :exports results or both"))))
  (it "accepts named results with effective exports results or both"
    (let* ((document
             (parse-org "#+NAME: block
#+BEGIN_SRC emacs-lisp :exports code
(+ 1 2)
#+END_SRC
#+NAME: result
#+RESULTS: block
: 3
"))
           (block (first (org-kit::%lint-nodes document :src-block)))
           (begin (org-kit::node-begin block)))
      (dolist (exports (list "results" "both"))
        (expect (lint-test-selected-messages
                 document "named-result"
                 (list :babel-effective-exports
                       (list (cons begin exports))))
                :to-equal nil))))
  (it "skips named-result checks without a matching valid effective-export entry"
    (let* ((document
             (parse-org "#+NAME: block
#+BEGIN_SRC emacs-lisp :exports code
(+ 1 2)
#+END_SRC
#+NAME: result
#+RESULTS: block
: 3
"))
           (block (first (org-kit::%lint-nodes document :src-block)))
           (begin (org-kit::node-begin block)))
      (expect (lint-test-selected-messages document "named-result")
              :to-equal nil)
      (expect (lint-test-selected-messages
               document "named-result"
               (list :babel-effective-exports
                     (list (cons (1+ begin) "code"))))
              :to-equal nil)
      (expect (lint-test-selected-messages
               document "named-result"
               (list :babel-effective-exports
                     (list (cons begin 42))))
              :to-equal nil)))
  (it "skips attachment links when only the generic file predicate is supplied"
    (let* ((file-predicate-called nil)
           (messages
             (lint-test-selected-messages
              (parse-org "[[attachment:photo.png]]")
              "link-to-local-file"
              (list :file-exists-p
                    (lambda (file)
                      (declare (ignore file))
                      (setf file-predicate-called t)
                      t)))))
      (expect messages :to-equal nil)
      (expect file-predicate-called :to-be nil)))
  (it "reports a missing attachment through the attachment-aware predicate"
    (let* ((generic-predicate-called nil)
           (checked-file nil)
           (checked-node-type nil)
           (messages
             (lint-test-selected-messages
              (parse-org "[[attachment:photo.png]]")
              "link-to-local-file"
              (list :file-exists-p
                    (lambda (file)
                      (declare (ignore file))
                      (setf generic-predicate-called t)
                      t)
                    :attachment-file-exists-p
                    (lambda (file node)
                      (setf checked-file file
                            checked-node-type (org-kit::node-type node))
                      nil)))))
      (expect generic-predicate-called :to-be nil)
      (expect checked-file :to-equal "photo.png")
      (expect checked-node-type :to-equal :link)
      (expect messages :to-equal
              (list "Link to non-existent local file \"photo.png\""))))
  (it "accepts an attachment confirmed by the attachment-aware predicate"
    (let* ((generic-predicate-called nil)
           (checked-file nil)
           (checked-node-type nil)
           (messages
             (lint-test-selected-messages
              (parse-org "[[attachment:photo.png]]")
              "link-to-local-file"
              (list :file-exists-p
                    (lambda (file)
                      (declare (ignore file))
                      (setf generic-predicate-called t)
                      t)
                    :attachment-file-exists-p
                    (lambda (file node)
                      (setf checked-file file
                            checked-node-type (org-kit::node-type node))
                      t)))))
      (expect generic-predicate-called :to-be nil)
      (expect checked-file :to-equal "photo.png")
      (expect checked-node-type :to-equal :link)
      (expect messages :to-equal nil))))
(progn
(describe "Org Babel header interpretation"
  (it "reports a missing colon in an affiliated header argument"
    (let* ((document
             (parse-org "#+HEADER: results output
#+BEGIN_SRC emacs-lisp
body
#+END_SRC
"))
           (messages
             (lint-test-selected-messages
              document "wrong-header-argument"
              (lint-test-babel-header-context t))))
      (expect messages :to-equal
              (list "Missing colon in header argument \"RESULTS\""))))
  (it "reports a forbidden combination of exclusive header values"
    (let* ((document
             (parse-org "#+BEGIN_SRC emacs-lisp :exports code both
body
#+END_SRC
"))
           (context
             (list :babel-header-specifications
                   (list :complete t
                         :common
                         (list
                          (cons "exports"
                                (list (list "code" "results" "both" "none"))))
                         :languages nil)))
           (messages
             (lint-test-selected-messages
              document "wrong-header-value" context)))
      (expect messages :to-equal
              (list "Forbidden combination in header \":exports\": code, both"))))
  (it "strips source-block switches and checks following header arguments"
    (let ((context (lint-test-babel-header-context t)))
      (dolist (switches '("-i" "-k" "-r" "-l \"label\"" "-n" "+n" "-n 4" "+n 4"
                          "-i -k -r -l \"label\" -n 4 +n 5"))
        (let* ((switch-only
                 (parse-org
                  (format nil "#+BEGIN_SRC emacs-lisp ~a~%body~%#+END_SRC~%"
                          switches)))
               (with-header
                 (parse-org
                  (format nil "#+BEGIN_SRC emacs-lisp ~a :bogus yes~%body~%#+END_SRC~%"
                          switches))))
          (expect (lint-test-selected-messages
                   switch-only "wrong-header-argument" context)
                  :to-equal nil)
          (expect (lint-test-selected-messages
                   with-header "wrong-header-argument" context)
                  :to-equal
                  (list "Unknown header argument \":bogus\"")))))))

(describe "Org Babel caller-material edge cases"
  (it "checks complete header argument properties at document and drawer scope"
    (let ((context (lint-test-babel-header-context t)))
      (dolist (source
                (list "#+PROPERTY: unrelated value
#+PROPERTY: header-args :bogus yes
"
                      "#+PROPERTY: header-args+ :bogus yes
"
                      "#+PROPERTY: header-args:emacs-lisp+ :bogus yes
"
                      "* Heading
:PROPERTIES:
:header-args+: :bogus yes
:END:
#+BEGIN_SRC emacs-lisp
body
#+END_SRC
"
                      "* Heading
:PROPERTIES:
:header-args:emacs-lisp+: :bogus yes
:END:
#+BEGIN_SRC emacs-lisp
body
#+END_SRC
"))
        (expect (lint-test-selected-messages
                 (parse-org source) "wrong-header-argument" context)
                :to-equal
                (list "Unknown header argument \":bogus\"")))
      (expect (lint-test-selected-messages
               (parse-org "#+PROPERTY: header-args
")
               "wrong-header-argument" context)
              :to-equal nil)))
  (it "checks inline source and Babel call header arguments"
    (let ((context (lint-test-babel-header-context t)))
      (expect (lint-test-selected-messages
               (parse-org "src_emacs-lisp[:bogus yes]{(+ 1 2)}")
               "wrong-header-argument" context)
              :to-equal
              (list "Unknown header argument \":bogus\""))
      (expect (lint-test-selected-messages
               (parse-org "#+CALL: target[:bogus yes]() :other no
")
               "wrong-header-argument" context)
              :to-equal
              (list "Unknown header argument \":other\""
                    "Unknown header argument \":bogus\""))
      (expect (lint-test-selected-messages
               (parse-org "call_target[:bogus yes](1)[:other no]")
               "wrong-header-argument" context)
              :to-equal
              (list "Unknown header argument \":other\""
                    "Unknown header argument \":bogus\""))))
  (it "checks string source-block parameters from the parsed node"
    (let* ((document
             (parse-org "#+BEGIN_SRC emacs-lisp
body
#+END_SRC
"))
           (block (first (org-kit::%lint-nodes document :src-block))))
      (setf (org-kit::node-property block :parameters) ":bogus yes")
      (expect (lint-test-selected-messages
               document "wrong-header-argument"
               (lint-test-babel-header-context t))
              :to-equal
              (list "Unknown header argument \":bogus\""))))
  (it "accepts any values within an allowed group and reports empty values"
    (let ((context
            (list :babel-header-specifications
                  (list :complete t
                        :common (list (cons "exports" (list (list :any))))
                        :languages nil))))
      (expect (lint-test-selected-messages
               (parse-org "#+BEGIN_SRC emacs-lisp :exports unusual
body
#+END_SRC
")
               "wrong-header-value" context)
              :to-equal nil))
    (expect (org-kit::%lint-context-header-value-messages
             ":exports" "" (list (list "code")))
            :to-equal
            (list "Unknown value \"nil\" for header \":exports\""))
    (expect (org-kit::%lint-context-header-value-messages
             ":exports" 42 (list (list "code")))
            :to-equal
            (list "Unknown value \"42\" for header \":exports\""))
    (expect (lint-test-selected-messages
             (parse-org "#+BEGIN_SRC emacs-lisp :exports
body
#+END_SRC
")
             "wrong-header-value"
             (lint-test-babel-header-context t))
            :to-equal
            (list "Unknown value \"nil\" for header \":exports\"")))
  (it "uses the preceding source block for an unreferenced named result"
    (let* ((document
             (parse-org "#+BEGIN_SRC emacs-lisp :exports code
(+ 1 2)
#+END_SRC
#+NAME: result
#+RESULTS:
: 3
"))
           (block (first (org-kit::%lint-nodes document :src-block)))
           (begin (org-kit::node-begin block)))
      (expect (lint-test-selected-messages
               document "named-result"
               (list :babel-effective-exports (list (cons begin "code"))))
              :to-equal
              (list "Links to \"result\" will not be valid during export unless the parent source block has :exports results or both")))
    (expect (lint-test-selected-messages
             (parse-org "#+NAME: result
#+RESULTS:
: 3
")
             "named-result"
             (list :babel-effective-exports nil))
            :to-equal nil))
  (it "uses a resolved attachment filename in a missing-file finding"
    (expect (lint-test-selected-messages
             (parse-org "[[attachment:photo.png]]")
             "link-to-local-file"
             (list :file-exists-p (lambda (file)
                                   (declare (ignore file))
                                   t)
                   :attachment-file-exists-p
                   (lambda (file node)
                     (declare (ignore file node))
                     (values nil "resolved/photo.png"))))
            :to-equal
            (list "Link to non-existent local file \"resolved/photo.png\""))))

(describe "Org lint checker dispatch without implementations"
  (it "skips a checker entry without an implementation function"
    (let* ((document (parse-org "Plain text."))
           (org-kit::*org-lint-checkers*
             (list (list "empty-checker" nil :document-only nil))))
      (expect (org-lint document :checkers '("empty-checker"))
              :to-equal nil))))
)

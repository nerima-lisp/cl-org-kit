;;;; t/babel-headers-test.lisp
(in-package #:cl-org-kit/test)

(defun babel-headers-test-lines (&rest lines)
  (format nil "~{~A~^~%~}" lines))

(defun babel-headers-test-src-blocks (document)
  (let ((blocks '()))
    (map-nodes
     (lambda (node)
       (when (eq (node-type node) :src-block)
         (push node blocks)))
     (document-root document))
    (nreverse blocks)))

(defun babel-headers-test-values (parameters key)
  (loop for (parameter-key . value) in parameters
        when (string= parameter-key key)
          collect value))

(describe "Babel header-argument resolution"
  (it "applies header priority and inherits subtree properties"
    (let* ((document
             (parse-org
              (babel-headers-test-lines
               "#+PROPERTY: header-args :session file :exports code :eval never :noweb no :var file=1"
               "#+PROPERTY: header-args:emacs-lisp :session language :results value :exports both :var language=2"
               "#+begin_src emacs-lisp"
               "file scope"
               "#+end_src"
               "* Parent"
               ":PROPERTIES:"
               ":header-args: :session parent :results table :exports results :cache yes :var parent=3"
               ":END:"
               "#+begin_src emacs-lisp"
               "parent scope"
               "#+end_src"
               "** Child"
               ":PROPERTIES:"
               ":header-args: :session child :results verbatim :exports none :var child=4"
               ":END:"
               "#+begin_src emacs-lisp"
               "child scope"
               "#+end_src"
               "#+HEADERS: :session affiliated :results raw :var header=5 :header-flag"
               "#+begin_src emacs-lisp"
               "affiliated header"
               "#+end_src"
               "#+HEADER: :session affiliated :results raw :var header=5 :header-flag"
               "#+begin_src emacs-lisp :session local :results html :var local=6 :local-flag"
               "local header"
               "#+end_src")))
           (blocks (babel-headers-test-src-blocks document))
           (file-parameters
             (resolve-babel-parameters document (nth 0 blocks)))
           (parent-parameters
             (resolve-babel-parameters document (nth 1 blocks)))
           (child-parameters
             (resolve-babel-parameters document (nth 2 blocks)))
           (affiliated-parameters
             (resolve-babel-parameters document (nth 3 blocks)))
           (local-parameters
             (resolve-babel-parameters document (nth 4 blocks)))
           (language-document
             (parse-org
              (babel-headers-test-lines
               "* Parent"
               ":PROPERTIES:"
               ":header-args: :session parent-generic"
               ":header-args:emacs-lisp: :session parent-language"
               ":END:"
               "** Child"
               ":PROPERTIES:"
               ":header-args:emacs-lisp+: :var child=1"
               ":END:"
               "#+begin_src emacs-lisp"
               "body"
               "#+end_src")))
           (language-block
             (first (babel-headers-test-src-blocks language-document)))
           (language-parameters
             (resolve-babel-parameters language-document language-block)))
      (with-soft-assertions
        (expect (length blocks) :to-be 5)

        (expect (babel-headers-test-values file-parameters "SESSION")
                :to-equal '("language"))
        (expect (babel-headers-test-values file-parameters "RESULTS")
                :to-equal '("value replace"))
        (expect (babel-headers-test-values file-parameters "EXPORTS")
                :to-equal '("both"))
        (expect (babel-headers-test-values file-parameters "EVAL")
                :to-equal '("never"))
        (expect (babel-headers-test-values file-parameters "VAR")
                :to-equal '("file=1" "language=2"))

        (expect (babel-headers-test-values parent-parameters "SESSION")
                :to-equal (quote ("parent")))
        (expect (babel-headers-test-values parent-parameters "RESULTS")
                :to-equal '("table value replace"))
        (expect (babel-headers-test-values parent-parameters "EXPORTS")
                :to-equal (quote ("results")))
        (expect (babel-headers-test-values parent-parameters "CACHE")
                :to-equal '("yes"))
        (expect (babel-headers-test-values parent-parameters "VAR")
                :to-equal (quote ("file=1" "language=2" "parent=3")))

        (expect (babel-headers-test-values child-parameters "SESSION")
                :to-equal (quote ("child")))
        (expect (babel-headers-test-values child-parameters "RESULTS")
                :to-equal '("verbatim value replace"))
        (expect (babel-headers-test-values child-parameters "EXPORTS")
                :to-equal (quote ("none")))
        (expect (babel-headers-test-values child-parameters "CACHE")
                :to-equal '("no"))
        (expect (babel-headers-test-values child-parameters "VAR")
                :to-equal (quote ("file=1" "language=2" "child=4")))

        (expect (babel-headers-test-values affiliated-parameters "SESSION")
                :to-equal '("affiliated"))
        (expect (babel-headers-test-values affiliated-parameters "RESULTS")
                :to-equal '("raw verbatim value replace"))
        (expect (babel-headers-test-values affiliated-parameters "VAR")
                :to-equal (quote ("file=1" "language=2" "child=4" "header=5")))
        (expect (babel-headers-test-values affiliated-parameters "HEADER-FLAG")
                :to-equal '(nil))

        (expect (babel-headers-test-values local-parameters "SESSION")
                :to-equal '("local"))
        (expect (babel-headers-test-values local-parameters "RESULTS")
                :to-equal '("html verbatim value replace"))
        (expect (babel-headers-test-values local-parameters "VAR")
                :to-equal (quote ("file=1" "language=2" "child=4" "header=5" "local=6")))
        (expect (babel-headers-test-values local-parameters "HEADER-FLAG")
                :to-equal '(nil))
        (expect (babel-headers-test-values local-parameters "LOCAL-FLAG")
                :to-equal '(nil))
        (expect (babel-headers-test-values language-parameters "SESSION")
                :to-equal '("parent-language"))
        (expect (babel-headers-test-values language-parameters "VAR")
                :to-equal '("child=1")))))

  (it "appends PROPERTY+ values at document and heading scope"
    (let* ((document
             (parse-org
              (babel-headers-test-lines
               "#+PROPERTY: header-args :session file :var base=1"
               "#+PROPERTY: header-args+ :eval never :var file-plus=2"
               "* Parent"
               ":PROPERTIES:"
               ":header-args+: :cache yes :var parent-plus=3"
               ":END:"
               "** Child"
               ":PROPERTIES:"
               ":header-args+: :noweb yes :var child-plus=4"
               ":END:"
               "#+begin_src emacs-lisp"
               "child with accumulated header arguments"
               "#+end_src")))
           (block (first (babel-headers-test-src-blocks document)))
           (parameters (resolve-babel-parameters document block)))
      (with-soft-assertions
        (expect (babel-headers-test-values parameters "SESSION")
                :to-equal '("file"))
        (expect (babel-headers-test-values parameters "EVAL")
                :to-equal '("never"))
        (expect (babel-headers-test-values parameters "CACHE")
                :to-equal '("yes"))
        (expect (babel-headers-test-values parameters "NOWEB")
                :to-equal '("yes"))
        (expect (babel-headers-test-values parameters "VAR")
                :to-equal '("base=1" "file-plus=2" "parent-plus=3" "child-plus=4")))))

  (it "preserves balanced values and moves a replaced named variable"
    (let* ((document
             (parse-org
              (babel-headers-test-lines
               "#+begin_src emacs-lisp :file-desc \"quoted: label, with spaces\" :prologue (list prefix:suffix) :var first=one :var repeated=old :var last=three :var repeated=new"
               "body"
               "#+end_src")))
           (block (first (babel-headers-test-src-blocks document)))
           (parameters (resolve-babel-parameters document block)))
      (with-soft-assertions
        (expect (babel-headers-test-values parameters "FILE-DESC")
                :to-equal '("\"quoted: label, with spaces\""))
        (expect (babel-headers-test-values parameters "PROLOGUE")
                :to-equal '("(list prefix:suffix)"))
        (expect (babel-headers-test-values parameters "VAR")
                :to-equal '("first=one" "last=three" "repeated=new")))))

  (it "rejects a non-source-block target with a typed reason"
    (let* ((document (parse-org "* Heading"))
           (heading nil)
           (condition nil))
      (map-nodes (lambda (node)
                   (when (eq (node-type node) :headline)
                     (setf heading node)))
                 (document-root document))
      (setf condition
            (handler-case
                (progn (resolve-babel-parameters document heading) nil)
              (org-babel-error (error) error)))
      (with-soft-assertions
        (expect (typep condition 'org-babel-error) :to-be t)
        (when condition
          (expect (org-babel-error-reason condition) :to-be :invalid-target)))))
  (it "parses quoted and nested header values"
    (let ((arguments
            (org-kit::babel-parse-header-arguments
             ":outer (list :inside) :brace {a :hidden} :bracket [b :hidden] :quote \"escaped \\\" :inside\" :empty :flag_2 value"))
          (comma-parts
            (org-kit::babel-split-header-list
             "first={a,b}, second=[c,d], third=(e,f), fourth=\"g,h\"" #\,))
          (escaped-parts
            (org-kit::babel-split-header-list
             "value=\"a, \\\"b,c\",tail=1" #\,))
          (space-parts
            (org-kit::babel-split-header-list
             "alpha (two words) \"three four\" {five six} [seven eight]"
             :whitespace)))
      (with-soft-assertions
        (expect (mapcar (function car) arguments)
                :to-equal
                (list "OUTER" "BRACE" "BRACKET" "QUOTE" "EMPTY" "FLAG_2"))
        (expect (cdr (first arguments))
                :to-equal "(list :inside)")
        (expect (cdr (second arguments))
                :to-equal "{a :hidden}")
        (expect (cdr (third arguments))
                :to-equal "[b :hidden]")
        (expect (cdr (fourth arguments))
                :to-equal "\"escaped \\\" :inside\"")
        (expect (cdr (fifth arguments))
                :to-equal nil)
        (expect (babel-headers-test-values arguments "FLAG_2")
                :to-equal (list "value"))
        (expect (org-kit::babel-parse-header-arguments nil)
                :to-equal nil)
        (expect (org-kit::babel-parse-header-arguments "")
                :to-equal nil)
        (expect (org-kit::babel-parse-header-arguments "plain:adjacent")
                :to-equal nil)
        (expect comma-parts
                :to-equal
                (list "first={a,b}" "second=[c,d]" "third=(e,f)"
                      "fourth=\"g,h\""))
        (expect escaped-parts
                :to-equal (list "value=\"a, \\\"b,c\"" "tail=1"))
        (expect space-parts
                :to-equal
                (list "alpha" "(two words)" "\"three four\""
                      "{five six}" "[seven eight]"))
        (expect (org-kit::babel-split-header-list ")a,}b,]c" #\,)
                :to-equal (list ")a" "}b" "]c"))
        (expect (org-kit::babel-split-header-list nil #\,)
                :to-equal nil)
        (expect (org-kit::babel-split-header-list "" :whitespace)
                :to-equal nil)
        (expect (org-kit::babel-whitespace-p #\Space) :to-be t)
        (expect (org-kit::babel-whitespace-p #\Tab) :to-be t)
        (expect (org-kit::babel-whitespace-p #\Newline) :to-be t)
        (expect (org-kit::babel-whitespace-p #\Return) :to-be t)
        (expect (org-kit::babel-whitespace-p #\!)
                :to-be nil)
        (expect (org-kit::babel-header-name-character-p #\A) :to-be t)
        (expect (org-kit::babel-header-name-character-p #\z) :to-be t)
        (expect (org-kit::babel-header-name-character-p #\7) :to-be t)
        (expect (org-kit::babel-header-name-character-p #\-)
                :to-be t)
        (expect (org-kit::babel-header-name-character-p #\_)
                :to-be t)
        (expect (org-kit::babel-header-name-character-p #\!)
                :to-be nil))))
  (it "covers variable and exclusive-parameter merge paths"
    (let ((variables
            (org-kit::babel-merge-var-parameter
             (list (cons "VAR" "amount=1")
                   (cons "VAR" "other=2")
                   (cons "SESSION" "old"))
             "amount=3, =missing, unassigned")))
      (with-soft-assertions
        (expect (org-kit::babel-var-assignment-name "amount=3")
                :to-equal "amount")
        (expect (org-kit::babel-var-assignment-name "=empty")
                :to-equal nil)
        (expect (org-kit::babel-var-assignment-name "unassigned")
                :to-equal nil)
        (expect (babel-headers-test-values variables "VAR")
                :to-equal
                (list "other=2" "amount=3" "=missing" "unassigned"))
        (expect (org-kit::babel-merge-exclusive-words
                 "table value replace" "vector output"
                 org-kit::+babel-results-exclusive-groups+)
                :to-equal "vector output replace")
        (expect (org-kit::babel-merge-exclusive-words
                 nil "value" org-kit::+babel-results-exclusive-groups+)
                :to-equal "value")
        (expect (org-kit::babel-merge-header-parameter nil "VAR" nil)
                :to-equal nil)
        (expect (org-kit::babel-merge-header-parameter nil "RESULTS" "table")
                :to-equal (list (cons "RESULTS" "table")))
        (expect (org-kit::babel-merge-header-parameter
                 (list (cons "RESULTS" "table")) "RESULTS" "vector")
                :to-equal (list (cons "RESULTS" "vector")))
        (expect (org-kit::babel-merge-header-parameter nil "EXPORTS" "none")
                :to-equal (list (cons "EXPORTS" "none")))
        (expect (org-kit::babel-merge-header-parameter
                 (list (cons "EXPORTS" "code")) "EXPORTS" "none")
                :to-equal (list (cons "EXPORTS" "none")))
        (expect (org-kit::babel-merge-header-parameter nil "SESSION" "new")
                :to-equal (list (cons "SESSION" "new")))
        (expect (org-kit::babel-merge-header-parameter
                 (list (cons "SESSION" "old")) "SESSION" "new")
                :to-equal (list (cons "SESSION" "new")))
        (expect (org-kit::babel-merge-header-parameter nil "FLAG" nil)
                :to-equal (list (cons "FLAG" nil)))
        (expect (org-kit::babel-merge-header-text nil nil)
                :to-equal nil))))
  (it "applies a generic global property after an earlier append"
    (let* ((document
             (parse-org
              (babel-headers-test-lines
               "#+PROPERTY: unrelated value"
               "#+PROPERTY: header-args+ :session appended"
               "#+PROPERTY: header-args :session base"
               "#+begin_src emacs-lisp"
               "body"
               "#+end_src")))
           (block (first (babel-headers-test-src-blocks document)))
           (parameters (resolve-babel-parameters document block)))
      (with-soft-assertions
        (expect (babel-headers-test-values parameters "SESSION")
                :to-equal (list "base")))))
  (it "rejects invalid resolver inputs and foreign blocks"
    (let* ((document
             (parse-org
              (babel-headers-test-lines
               "#+begin_src emacs-lisp"
               "body"
               "#+end_src")))
           (block (first (babel-headers-test-src-blocks document)))
           (other-document
             (parse-org
              (babel-headers-test-lines
               "#+begin_src emacs-lisp"
               "body"
               "#+end_src")))
           (foreign-block
             (first (babel-headers-test-src-blocks other-document)))
           (invalid-document
             (handler-case
                 (progn (resolve-babel-parameters nil block) nil)
               (error (condition) condition)))
           (invalid-node
             (handler-case
                 (progn (resolve-babel-parameters document nil) nil)
               (error (condition) condition)))
           (foreign-error
             (handler-case
                 (progn
                   (resolve-babel-parameters document foreign-block)
                   nil)
               (error (condition) condition))))
      (with-soft-assertions
        (expect (typep invalid-document
                       (find-symbol "TYPE-ERROR" "COMMON-LISP"))
                :to-be t)
        (expect (typep invalid-node
                       (find-symbol "TYPE-ERROR" "COMMON-LISP"))
                :to-be t)
        (expect (org-babel-error-reason foreign-error)
                :to-be :document-mismatch)))))

(describe "Babel header helper edge cases"
  (it "handles unmatched closers and incomplete header markers"
    (with-soft-assertions
      (expect (org-kit::babel-parse-header-arguments
               ") } ] :paren :brace :bracket")
              :to-equal
              '(("PAREN" . nil) ("BRACE" . nil) ("BRACKET" . nil)))
      (expect (org-kit::babel-parse-header-arguments ":") :to-be nil)
      (expect (org-kit::babel-parse-header-arguments ": ") :to-be nil)))

  (it "drops empty header-list segments and preserves unknown exclusive words"
    (with-soft-assertions
      (expect (org-kit::babel-split-header-list ",a,," #\,)
              :to-equal '("a"))
      (expect (org-kit::babel-merge-exclusive-words
               "keep" "custom" org-kit::+babel-results-exclusive-groups+)
              :to-equal "custom keep")))

  (it "keeps valueless exclusive parameters and ignores malformed settings"
    (let ((document (parse-org "")))
      (setf (document-settings document)
            '(:property-settings (nil 17)))
      (with-soft-assertions
        (expect (org-kit::babel-merge-header-parameter nil "RESULTS" nil)
                :to-equal '(("RESULTS" . nil)))
        (expect (org-kit::babel-merge-header-parameter nil "EXPORTS" nil)
                :to-equal '(("EXPORTS" . nil)))
        (expect (multiple-value-list
                 (org-kit::babel-global-property-value document "header-args"))
                :to-equal '(nil nil))))))

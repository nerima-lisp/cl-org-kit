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
               "#+HEADER: :session affiliated :results raw :var header=5 :header-flag"
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
                :to-equal '("language"))
        (expect (babel-headers-test-values parent-parameters "RESULTS")
                :to-equal '("table value replace"))
        (expect (babel-headers-test-values parent-parameters "EXPORTS")
                :to-equal '("both"))
        (expect (babel-headers-test-values parent-parameters "CACHE")
                :to-equal '("yes"))
        (expect (babel-headers-test-values parent-parameters "VAR")
                :to-equal '("parent=3" "language=2"))

        (expect (babel-headers-test-values child-parameters "SESSION")
                :to-equal '("language"))
        (expect (babel-headers-test-values child-parameters "RESULTS")
                :to-equal '("verbatim value replace"))
        (expect (babel-headers-test-values child-parameters "EXPORTS")
                :to-equal '("both"))
        (expect (babel-headers-test-values child-parameters "CACHE")
                :to-equal '("no"))
        (expect (babel-headers-test-values child-parameters "VAR")
                :to-equal '("child=4" "language=2"))

        (expect (babel-headers-test-values affiliated-parameters "SESSION")
                :to-equal '("affiliated"))
        (expect (babel-headers-test-values affiliated-parameters "RESULTS")
                :to-equal '("raw verbatim value replace"))
        (expect (babel-headers-test-values affiliated-parameters "VAR")
                :to-equal
                '("child=4" "language=2" "header=5"))
        (expect (babel-headers-test-values affiliated-parameters "HEADER-FLAG")
                :to-equal '(nil))

        (expect (babel-headers-test-values local-parameters "SESSION")
                :to-equal '("local"))
        (expect (babel-headers-test-values local-parameters "RESULTS")
                :to-equal '("html verbatim value replace"))
        (expect (babel-headers-test-values local-parameters "VAR")
                :to-equal
                '("child=4" "language=2" "header=5" "local=6"))
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
          (expect (org-babel-error-reason condition) :to-be :invalid-target))))))

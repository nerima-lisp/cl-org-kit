;;;; t/public-api-test.lisp
;;;;
;;;; Pin the exported API so changes are deliberate and semver-classified.
(in-package #:cl-org-kit/test)

(defparameter +public-api+
  '()
  "Every symbol the ORG-KIT package exports.")

(defun external-symbol-names ()
  (let ((names '()))
    (do-external-symbols (symbol (find-package "ORG-KIT"))
      (push (symbol-name symbol) names))
    names))

(defun documented-p (symbol)
  "True when SYMBOL carries a docstring in every role it actually fills."
  (let ((roles '()))
    (when (fboundp symbol)
      (push (documentation symbol 'function) roles))
    (when (boundp symbol)
      (push (documentation symbol 'variable) roles))
    (when (find-class symbol nil)
      (push (documentation symbol 'type) roles))
    (and roles (every #'identity roles))))

(describe "public API surface"
  (it "exports exactly the pinned symbols"
    (let ((actual (sort (external-symbol-names) #'string<))
          (expected (sort (copy-list +public-api+) #'string<)))
      (with-soft-assertions
        (expect (set-difference actual expected :test #'string=) :to-equal '())
        (expect (set-difference expected actual :test #'string=) :to-equal '()))))

  (it "documents every exported symbol"
    (with-soft-assertions
      (dolist (name (sort (external-symbol-names) #'string<))
        (expect (list name (and (documented-p (find-symbol name "ORG-KIT")) t))
                :to-equal (list name t)))))

  (it "keeps the package name stable and nickname-free"
    (expect (package-name (find-package "ORG-KIT")) :to-equal "ORG-KIT")
    (expect (package-nicknames (find-package "ORG-KIT")) :to-equal '())))

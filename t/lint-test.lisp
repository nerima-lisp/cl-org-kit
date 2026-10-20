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

(describe "Org lint registration and dispatch"
  (it "registers every test source in the canonical ASDF suite"
    (let ((registered (lint-test-component-paths))
          (on-disk (lint-test-files-on-disk)))
      (expect (set-difference registered on-disk :test #'string=) :to-equal '())
      (expect (set-difference on-disk registered :test #'string=) :to-equal '())))

  (it "registers all 61 Org checker names: 58 dispatchable and three deferred"
    (let* ((table org-kit::*org-lint-checkers*)
           (active (remove-if (lambda (checker)
                                (eq (third checker) :dependency-wait))
                              table))
           (deferred (remove-if-not (lambda (checker)
                                      (eq (third checker) :dependency-wait))
                                    table)))
      (expect (length table) :to-be 61)
      (expect (count :document-only table :key #'third) :to-be 46)
      (expect (count :caller-material table :key #'third) :to-be 12)
      (expect (count :dependency-wait table :key #'third) :to-be 3)
      (expect (length active) :to-be 58)
      (expect (length deferred) :to-be 3)
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

  (it "does not run a context-dependent checker without its required material"
    (expect (org-lint (parse-org "#+BEGIN_SRC not-a-language
body
#+END_SRC
")
                      :checkers '("suspicious-language-in-src-block"))
            :to-equal '()))

  (it "rejects an unknown checker name"
    (signals org-kit-error
      (org-lint (parse-org "Text.") :checkers '("not-a-checker"))))

  (it "requires an already parsed document"
    (signals org-kit-error (org-lint (make-document "Text."))))

  (it "validates context as a proper keyword plist"
    (signals org-kit-error (org-lint (parse-org "Text.") :context '(:not-a-pair)))))
(describe "Org lint validation edge cases"
  (it "rejects invalid documents and diagnostic positions"
  (signals type-error (org-lint nil))
  (let ((position (read-from-string "0")))
    (signals type-error
      (org-kit::%make-org-lint-diagnostic :position position))))
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
  (it "rejects malformed checker selections"
    (let ((document (parse-org "Text.")))
      (signals type-error
        (org-lint document :checkers (quote ("known" . malformed))))
      (signals type-error
        (org-lint document :checkers (quote (42))))
      (let ((cycle (list "known")))
        (setf (cdr (last cycle)) cycle)
        (expect (org-kit::%lint-string-list-p cycle) :to-be nil))))
  (it "rejects diagnostic offsets outside the document"
    (let ((document (parse-org "Text.")))
      (expect (org-kit::%lint-report document "test" -1 "message") :to-be nil)
      (expect (org-kit::%lint-report document "test" 6 "message") :to-be nil)
      (expect (org-kit::%lint-report document "test" "1" "message") :to-be nil)))
)

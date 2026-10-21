;;;; t/babel-detangle-test.lisp
(in-package #:cl-org-kit/test)

(defun babel-detangle-test-lines (&rest lines)
  (format nil "~{~A~^~%~}" lines))

(defun babel-detangle-test-condition (function)
  (handler-case
      (progn (funcall function) nil)
    (error (condition) condition)))

(defun babel-detangle-test-ranges (result)
  (mapcar (lambda (span)
            (list (edit-span-start span) (edit-span-end span)))
          (edit-result-spans result)))

(describe "Babel detangling"
  (it "updates a named source block and preserves its surrounding source"
    (let* ((source
             (babel-detangle-test-lines
              "before"
              "#+NAME: sample"
              "#+BEGIN_SRC text"
              "old body"
              "#+END_SRC"
              "after"))
           (old-body (format nil "old body~%"))
           (new-body (format nil "new body~%"))
           (body-start (search old-body source))
           (body-end (+ body-start (length old-body)))
           (document (parse-org source))
           (tangled
             (babel-detangle-test-lines
              ";; [[file:sample.org::*sample][sample]]"
              "new body"
              "// sample ends here"))
           (result (babel-detangle document tangled))
           (updated (document-text (edit-result-updated-document result)))
           (expected
             (concatenate 'string (subseq source 0 body-start) new-body
                          (subseq source body-end))))
      (with-soft-assertions
        (expect (edit-result-p result) :to-be t)
        (expect updated :to-equal expected)
        (expect (babel-detangle-test-ranges result)
                :to-equal (list (list body-start body-end)))
        (expect (subseq updated 0 body-start)
                :to-equal (subseq source 0 body-start))
        (expect (subseq updated (+ body-start (length new-body)))
                :to-equal (subseq source body-end)))))

  (it "resolves an unnamed heading block by its per-heading source-block number"
    (let* ((source
             (babel-detangle-test-lines
              "* Heading"
              "#+NAME: named-first"
              "#+BEGIN_SRC text"
              "named body"
              "#+END_SRC"
              "#+BEGIN_SRC text"
              "old heading body"
              "#+END_SRC"
              "tail"))
           (old-body (format nil "old heading body~%"))
           (new-body (format nil "new heading body~%"))
           (body-start (search old-body source))
           (body-end (+ body-start (length old-body)))
           (document (parse-org source))
           (tangled
             (babel-detangle-test-lines
              "{- [[file:sample.org::*Heading][Heading:2]] -}"
              "new heading body"
              "{- Heading:2 ends here -}"))
           (result (babel-detangle document tangled)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result))
                :to-equal
                (concatenate 'string (subseq source 0 body-start) new-body
                             (subseq source body-end)))
        (expect (babel-detangle-test-ranges result)
                :to-equal (list (list body-start body-end))))))

  (it "uses the no-heading bucket number after named blocks"
    (let* ((source
             (babel-detangle-test-lines
              "#+NAME: named-first"
              "#+BEGIN_SRC text"
              "named body"
              "#+END_SRC"
              "#+BEGIN_SRC text"
              "old body"
              "#+END_SRC"))
           (document (parse-org source))
           (result
             (babel-detangle
              document
              (babel-detangle-test-lines
               "# [[file:sample.org][No heading:2]]"
               "new body"
               ";; No heading:2 ends here"))))
      (expect (document-text (edit-result-updated-document result))
              :to-equal
              (babel-detangle-test-lines
               "#+NAME: named-first"
               "#+BEGIN_SRC text"
               "named body"
               "#+END_SRC"
               "#+BEGIN_SRC text"
               "new body"
               "#+END_SRC"))))

  (it "applies multiple linked body updates with source-ordered spans"
    (let* ((source
             (babel-detangle-test-lines
              "#+NAME: sample"
              "#+BEGIN_SRC text"
              "first old"
              "#+END_SRC"
              "* Heading"
              "#+BEGIN_SRC text"
              "second old"
              "#+END_SRC"
              "outside"))
           (first-body (format nil "first old~%"))
           (second-body (format nil "second old~%"))
           (first-start (search first-body source))
           (first-end (+ first-start (length first-body)))
           (second-start (search second-body source))
           (second-end (+ second-start (length second-body)))
           (first-new (format nil "first new~%"))
           (second-new (format nil "second new~%"))
           (document (parse-org source))
           (result
             (babel-detangle
              document
              (babel-detangle-test-lines
               "# [[file:sample.org::*sample][sample]]"
               "first new"
               "# sample ends here"
               "/* [[file:sample.org::*Heading][Heading:1]] */"
               "second new"
               "// Heading:1 ends here")))
           (expected
             (concatenate 'string
                          (subseq source 0 first-start) first-new
                          (subseq source first-end second-start) second-new
                          (subseq source second-end))))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result))
                :to-equal expected)
        (expect (babel-detangle-test-ranges result)
                :to-equal (list (list first-start first-end)
                                (list second-start second-end)))
        (expect (subseq expected
                        (+ first-start (length first-new))
                        (+ first-start (length first-new)
                           (- second-start first-end)))
                :to-equal (subseq source first-end second-start))
        (expect (subseq expected (+ second-start (- (length first-new)
                                                   (length first-body))
                                  (length second-new)))
                :to-equal (subseq source second-end)))))

  (it "leaves unmatched links and standalone end prose untouched"
    (let* ((source
             (babel-detangle-test-lines
              "#+NAME: sample"
              "#+BEGIN_SRC text"
              "original"
              "#+END_SRC"))
           (document (parse-org source))
           (result
             (babel-detangle
              document
              (babel-detangle-test-lines
               "text says sample ends here"
               "# [[file:other.org][missing]]"
               "// missing ends here"))))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result))
                :to-equal source)
        (expect (edit-result-spans result) :to-equal nil))))

  (it "reports ambiguous names instead of choosing a source block"
    (flet ((assert-ambiguous (source source-name)
         (let* ((document (parse-org source))
                (condition
                  (babel-detangle-test-condition
                   (lambda ()
                     (babel-detangle
                      document
                      (babel-detangle-test-lines
                       (format nil "# [[file:sample.org][~A]]" source-name)
                       "updated"
                       (format nil "# ~A ends here" source-name)))))))
           (expect (typep condition (quote org-babel-error)) :to-be t)
           (when (typep condition (quote org-babel-error))
             (expect (org-babel-error-reason condition)
                     :to-be :ambiguous-detangle-source-name)))))
  (assert-ambiguous
   (babel-detangle-test-lines
    "#+NAME: same" "#+BEGIN_SRC text" "one" "#+END_SRC"
    "#+NAME: same" "#+BEGIN_SRC text" "two" "#+END_SRC")
   "same")
  (assert-ambiguous
   (babel-detangle-test-lines
    "#+NAME: Heading:1" "#+BEGIN_SRC text" "named" "#+END_SRC"
    "* Heading" "#+BEGIN_SRC text" "generated" "#+END_SRC")
   "Heading:1")))
  (it "rejects an incomplete or mismatched linked section"
    (let* ((source
             (babel-detangle-test-lines
              "#+NAME: first" "#+BEGIN_SRC text" "one" "#+END_SRC"
              "#+NAME: second" "#+BEGIN_SRC text" "two" "#+END_SRC"))
           (document (parse-org source))
           (incomplete
             (babel-detangle-test-condition
              (lambda ()
                (babel-detangle
                 document
                 (babel-detangle-test-lines
                  "# [[file:sample.org][first]]" "updated")))))
           (mismatched
             (babel-detangle-test-condition
              (lambda ()
                (babel-detangle
                 document
                 (babel-detangle-test-lines
                  "# [[file:sample.org][first]]"
                  "updated"
                  "# second ends here"))))))
      (with-soft-assertions
        (expect (typep incomplete 'org-babel-error) :to-be t)
        (when (typep incomplete 'org-babel-error)
          (expect (org-babel-error-reason incomplete)
                  :to-be :malformed-detangle-section))
        (expect (typep mismatched 'org-babel-error) :to-be t)
        (when (typep mismatched 'org-babel-error)
          (expect (org-babel-error-reason mismatched)
                  :to-be :malformed-detangle-section)))))

  (it "enforces input and search-work limits"
    (let* ((source
             (babel-detangle-test-lines
              "#+NAME: sample" "#+BEGIN_SRC text" "old" "#+END_SRC"))
           (document
             (parse-org source
                        :limits (make-limits
                                 :max-input-length (length source))))
           (oversized
             (babel-detangle-test-condition
              (lambda ()
                (babel-detangle document (make-string (1+ (length source))
                                                       :initial-element #\x)))))
           (work-limited
             (babel-detangle-test-condition
              (lambda ()
                (babel-detangle
                 (parse-org source :limits (make-limits :max-search-work 0))
                 "")))))
      (with-soft-assertions
        (expect (typep oversized 'org-resource-limit-error) :to-be t)
        (when (typep oversized 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name oversized)
                  :to-be :max-input-length))
        (expect (typep work-limited 'org-resource-limit-error) :to-be t)
        (when (typep work-limited 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name work-limited)
                  :to-be :max-search-work)))))

  (it "enforces output character and octet limits"
    (let* ((source
             (babel-detangle-test-lines
              "#+NAME: sample" "#+BEGIN_SRC text" "x" "#+END_SRC"))
           (tangled
             (babel-detangle-test-lines
              "# [[file:sample.org][sample]]" "longer" "# sample ends here"))
           (character-limited
             (babel-detangle-test-condition
              (lambda ()
                (babel-detangle
                 (parse-org source
                            :limits (make-limits
                                     :max-output-length (length source)))
                 tangled))))
           (octet-limited
             (babel-detangle-test-condition
              (lambda ()
                (babel-detangle
                 (parse-org source
                            :limits (make-limits
                                     :max-output-octets (length source)))
                 tangled)))))
      (with-soft-assertions
        (expect (typep character-limited 'org-resource-limit-error) :to-be t)
        (when (typep character-limited 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name character-limited)
                  :to-be :max-output-length))
        (expect (typep octet-limited 'org-resource-limit-error) :to-be t)
        (when (typep octet-limited 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name octet-limited)
                  :to-be :max-output-octets)))))
)

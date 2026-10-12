;;;; t/object-other-test.lisp
(in-package #:cl-org-kit/test)

(defun other-test-match (source &optional
                                (start 0)
                                (end (length source))
                                (container-type :paragraph)
                                (range-start 0))
  (let ((document (make-document source))
        (org-kit::*object-parse-range-start* range-start))
    (org-kit::try-parse-other-object document start end container-type)))

(defun other-test-property (match key)
  (getf (org-kit::object-match-properties match) key))

(describe "other object recognition"
          (it
           "recognizes fixture entities, unknown entity spellings, and post-blank"
           (let* ((source "\\alpha{}  tail")
                  (match (other-test-match source))
                  (unknown-source "\\notAnEntity")
                  (unknown-match (other-test-match unknown-source)))
             (with-soft-assertions
              (expect (org-kit::object-match-type match) :to-be :entity)
              (expect (org-kit::object-match-end match) :to-be 10)
              (expect (org-kit::object-match-post-blank match) :to-be 2)
              (expect (org-kit::object-match-properties match)
                      :to-equal
                      '(:name "alpha"
                              :latex
                              "\\alpha"
                              :latex-math-p
                              t
                              :html
                              "&alpha;"
                              :ascii
                              "alpha"
                              :utf-8
                              "α"
                              :use-brackets-p
                              t))
              (expect (org-kit::object-match-type unknown-match)
                      :to-be
                      :latex-fragment)
              (expect (other-test-property unknown-match :value)
                      :to-equal
                      "\\notAnEntity"))))
          (it "recognizes inline Babel calls and inline source blocks"
              (let* ((call-source
                      "call_sum[ :var x=1 ](1, 2)[  :results raw  ]   tail")
                     (call-match (other-test-match call-source))
                     (source-block
                      (other-test-match
                       "src_emacs-lisp[:results raw]{(+ 1 2)} tail"))
                     (empty-source (other-test-match "src_x{}"))
                     (bad-boundary (other-test-match "xcall_sum(1)" 1)))
                (with-soft-assertions
                 (expect (org-kit::object-match-type call-match)
                         :to-be
                         :inline-babel-call)
                 (expect (org-kit::object-match-end call-match)
                         :to-be
                         (search "tail" call-source))
                 (expect (org-kit::object-match-post-blank call-match) :to-be 3)
                 (expect (org-kit::object-match-properties call-match)
                         :to-equal
                         '(:call "sum"
                                 :inside-header
                                 ":var x=1"
                                 :arguments
                                 "1, 2"
                                 :end-header
                                 ":results raw"))
                 (expect (org-kit::object-match-type source-block)
                         :to-be
                         :inline-src-block)
                 (expect (org-kit::object-match-properties source-block)
                         :to-equal
                         '(:language "emacs-lisp"
                                     :value
                                     "(+ 1 2)"
                                     :parameters
                                     ":results raw"))
                 (expect (other-test-property empty-source :value) :to-equal "")
                 (expect bad-boundary :to-be nil))))
          (it "recognizes LaTeX fragments, line breaks, and statistics cookies"
              (let* ((math-source "$x+y$  ")
                     (math-match (other-test-match math-source))
                     (paren-match (other-test-match "\\(a + b\\)"))
                     (break-source (format nil "a\\\\ ~%next"))
                     (break-start 1)
                     (break-match
                      (other-test-match break-source
                                        break-start
                                        (length break-source)))
                     (cookie-match (other-test-match "[2/5]")))
                (with-soft-assertions
                 (expect (org-kit::object-match-type math-match)
                         :to-be
                         :latex-fragment)
                 (expect (org-kit::object-match-end math-match) :to-be 7)
                 (expect (org-kit::object-match-post-blank math-match) :to-be 2)
                 (expect (other-test-property math-match :value)
                         :to-equal
                         "$x+y$")
                 (expect (other-test-property paren-match :value)
                         :to-equal
                         "\\(a + b\\)")
                 (expect (org-kit::object-match-type break-match)
                         :to-be
                         :line-break)
                 (expect (org-kit::object-match-end break-match) :to-be 5)
                 (expect (org-kit::object-match-properties break-match)
                         :to-be
                         nil)
                 (expect (org-kit::object-match-type cookie-match)
                         :to-be
                         :statistics-cookie)
                 (expect (org-kit::object-match-properties cookie-match)
                         :to-equal
                         '(:value "[2/5]" :type nil)))))
          (it "recognizes subscript, superscript, and table-cell content spans"
              (let* ((subscript (other-test-match "H_2O" 1))
                     (bracketed (other-test-match "x_{i+1}" 1))
                     (superscript (other-test-match "x^3" 1))
                     (spaced-boundary (other-test-match "x_ y" 1))
                     (cell-source "| Cell | Other |")
                     (cell
                      (other-test-match cell-source
                                        1
                                        (length cell-source)
                                        :table-row)))
                (with-soft-assertions
                 (expect (org-kit::object-match-type subscript)
                         :to-be
                         :subscript)
                 (expect
                  (list (org-kit::object-match-end subscript)
                        (org-kit::object-match-contents-begin subscript)
                        (org-kit::object-match-contents-end subscript)
                        (other-test-property subscript :use-brackets-p))
                  :to-equal
                  '(4 2 4 nil))
                 (expect
                  (list (org-kit::object-match-end bracketed)
                        (org-kit::object-match-contents-begin bracketed)
                        (org-kit::object-match-contents-end bracketed)
                        (other-test-property bracketed :use-brackets-p))
                  :to-equal
                  '(7 3 6 t))
                 (expect (org-kit::object-match-type superscript)
                         :to-be
                         :superscript)
                 (expect spaced-boundary :to-be nil)
                 (expect (org-kit::object-match-type cell) :to-be :table-cell)
                 (expect
                  (list (org-kit::object-match-end cell)
                        (org-kit::object-match-contents-begin cell)
                        (org-kit::object-match-contents-end cell)
                        (org-kit::object-match-properties cell))
                  :to-equal
                  '(8 2 6 (:value nil)))
                 (expect
                  (other-test-match cell-source
                                    1
                                    (length cell-source)
                                    :paragraph)
                  :to-be
                  nil)))))

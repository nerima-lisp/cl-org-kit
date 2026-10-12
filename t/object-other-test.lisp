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

(describe "other object boundary recognition"
          (it "recognizes special entities and LaTeX fragments"
              (let* ((therefore (other-test-match "\\there4"))
                     (superscript (other-test-match "\\sup1"))
                     (fraction (other-test-match "\\frac12"))
                     (space-entity (other-test-match "\\_ "))
                     (latex-command (other-test-match "\\foo*{a}[b]!"))
                     (display-math (other-test-match "$$x+y$$,"))
                     (bracket-math (other-test-match "\\[x^2\\]")))
                (with-soft-assertions
                 (expect (other-test-property therefore :name)
                         :to-equal
                         "there4")
                 (expect (other-test-property superscript :name)
                         :to-equal
                         "sup1")
                 (expect (other-test-property fraction :name)
                         :to-equal
                         "frac12")
                 (expect (other-test-property space-entity :name)
                         :to-equal
                         "_ ")
                 (expect (other-test-property latex-command :value)
                         :to-equal
                         "\\foo*{a}[b]")
                 (expect (other-test-property display-math :value)
                         :to-equal
                         "$$x+y$$")
                 (expect (other-test-property bracket-math :value)
                         :to-equal
                         "\\[x^2\\]")))))

(describe "other object helper edges"
          (it
           "recognizes active starts, escapes, whitespace, and header normalization"
           (let ((header (format nil ":one~%  :two~C" #\Tab)))
             (with-soft-assertions
              (let ((org-kit::*object-parse-range-start* nil))
                (expect (org-kit::other-object-active-start-p 0) :to-be t)
                (expect (org-kit::other-object-active-start-p 1) :to-be nil)
                (expect (org-kit::other-object-word-boundary-p "a!" 1)
                        :to-be
                        nil)
                (expect (org-kit::other-object-word-boundary-p " !" 1) :to-be t))
              (expect (org-kit::other-object-escaped-p "\\x" 1) :to-be t)
              (expect (org-kit::other-object-escaped-p "\\\\x" 2) :to-be nil)
              (expect (org-kit::other-object-nonblank-string "   " 0 3)
                      :to-be
                      nil)
              (expect
               (org-kit::other-object-normalize-header header 0 (length header))
               :to-equal
               ":one :two")))))

(describe "other object line breaks and inline failures"
          (it
           "recognizes CR, CRLF, terminal breaks, and rejects malformed blocks"
           (let* ((crlf
                   (other-test-match
                    (format nil "\\\\ ~C~Cnext" #\Return #\Newline)))
                  (cr (other-test-match (format nil "\\\\ ~Cnext" #\Return)))
                  (ending (other-test-match "\\\\"))
                  (bad-break (other-test-match "\\\\x"))
                  (call-header (other-test-match "call_x[open(1)"))
                  (call-no-paren (other-test-match "call_x[ok]tail"))
                  (call-no-args-end (other-test-match "call_x(1"))
                  (source-header (other-test-match "src_x[open{body}"))
                  (source-no-body (other-test-match "src_x[ok]"))
                  (source-unclosed (other-test-match "src_x{body")))
             (with-soft-assertions
              (expect (org-kit::object-match-end crlf) :to-be 5)
              (expect (org-kit::object-match-end cr) :to-be 4)
              (expect (org-kit::object-match-end ending) :to-be 2)
              (expect bad-break :to-be nil)
              (expect call-header :to-be nil)
              (expect call-no-paren :to-be nil)
              (expect call-no-args-end :to-be nil)
              (expect source-header :to-be nil)
              (expect source-no-body :to-be nil)
              (expect source-unclosed :to-be nil)
              (expect
               (org-kit::other-object-parse-line-break
                (make-string 3 :initial-element #\\)
                1
                3)
               :to-be
               nil)))))

(describe "other object scripts and table rows"
          (it
           "recognizes signed scripts, Unicode entity boundaries, and row endings"
           (let* ((signed-sub (other-test-match "x_+3" 1))
                  (signed-super (other-test-match "x^-2" 1))
                  (line-text (format nil "|last~%row"))
                  (line-cell
                   (other-test-match line-text 1 (length line-text) :table-row))
                  (carriage-text (format nil "|cell~Cnext" #\Return))
                  (carriage-cell
                   (other-test-match carriage-text
                                     1
                                     (length carriage-text)
                                     :table-row))
                  (no-separator (other-test-match "|single" 1 7 :table-row))
                  (blank-cell (other-test-match "||" 1 2 :table-row))
                  (trimmed-cell
                   (other-test-match "| value   |next" 1 15 :table-row)))
             (with-soft-assertions
              (expect (org-kit::object-match-end signed-sub) :to-be 4)
              (expect (org-kit::object-match-end signed-super) :to-be 4)
              (expect (org-kit::object-match-end line-cell) :to-be 5)
              (expect (org-kit::object-match-end carriage-cell) :to-be 5)
              (expect (org-kit::object-match-end no-separator) :to-be 7)
              (expect
               (list (org-kit::object-match-contents-begin blank-cell)
                     (org-kit::object-match-contents-end blank-cell))
               :to-equal
               (list 1 1))
              (expect
               (list (org-kit::object-match-contents-begin trimmed-cell)
                     (org-kit::object-match-contents-end trimmed-cell))
               :to-equal
               (list 2 7))
              (expect (org-kit::other-object-entity-name-at "\\alphaé" 0 7)
                      :to-be
                      nil)
              (expect
               (org-kit::other-object-parse-sub-super "x_." 1 3 :subscript #\_)
               :to-be
               nil)
              (let ((org-kit::*object-parse-range-start* 1))
                (expect
                 (org-kit::other-object-parse-sub-super "x_2"
                                                        1
                                                        3
                                                        :subscript
                                                        #\_)
                 :to-be
                 nil))))))

(describe "other object fallback and dollar boundaries"
          (it
           "keeps unknown entities and malformed dollar delimiters outside objects"
           (let* ((unknown-underscore (other-test-match "\\_"))
                  (unmatched-command (other-test-match "\\foo{a"))
                  (embedded-source "a$x$")
                  (embedded-dollar (other-test-match embedded-source 1))
                  (opening-whitespace (other-test-match "$ x$"))
                  (closing-whitespace (other-test-match "$x $"))
                  (unmatched-double-dollar (other-test-match "$$x$")))
             (with-soft-assertions (expect unknown-underscore :to-be nil)
                                   (expect
                                    (org-kit::object-match-type
                                     unmatched-command)
                                    :to-be
                                    :latex-fragment)
                                   (expect
                                    (org-kit::object-match-end
                                     unmatched-command)
                                    :to-be
                                    4)
                                   (expect
                                    (other-test-property unmatched-command
                                                         :value)
                                    :to-equal
                                    "\\foo")
                                   (expect
                                    (org-kit::object-match-type embedded-dollar)
                                    :to-be
                                    :latex-fragment)
                                   (expect
                                    (org-kit::object-match-end embedded-dollar)
                                    :to-be
                                    4)
                                   (expect
                                    (other-test-property embedded-dollar :value)
                                    :to-equal
                                    "$x$")
                                   (expect opening-whitespace :to-be nil)
                                   (expect closing-whitespace :to-be nil)
                                   (expect unmatched-double-dollar :to-be nil)))))

(describe "other object helper edge branches"
  (it "covers paired spans, entities, and LaTeX delimiters"
    (let* ((document (make-document "x"))
           (slashes (make-string 2 :initial-element #\\)))
      (with-soft-assertions
        (expect (org-kit::other-object-character-p "abc" 1 #\b 3)
                :to-be-truthy)
        (expect (org-kit::other-object-character-p "abc" 3 #\b 3)
                :to-be
                nil)
        (expect (org-kit::other-object-paired-span "a(b(c)d)e" 1 9 #\( #\))
                :to-be-truthy)
        (expect (org-kit::other-object-paired-span "(a\\)b)" 0 6 #\( #\))
                :to-be-truthy)
        (expect (org-kit::other-object-paired-span "(unfinished" 0 11 #\( #\))
                :to-be
                nil)
        (expect (org-kit::other-object-normalize-header "   " 0 3)
                :to-be
                nil)
        (expect (org-kit::other-object-normalize-header
                 (format nil " a~c~% ~cb " #\Tab #\Tab) 0 8)
                :to-be-truthy)
        (expect (org-kit::other-object-entity-name-at "&there4;" 0 8)
                :to-be-truthy)
        (expect (org-kit::other-object-entity-name-at "&sup1;" 0 6)
                :to-be-truthy)
        (expect (org-kit::other-object-entity-name-at "&frac12;" 0 8)
                :to-be-truthy)
        (expect (org-kit::other-object-entity-name-at "&alpha{}" 0 8)
                :to-be-truthy)
        (expect (org-kit::other-object-entity-name-at "&alphaX" 0 7)
                :to-be
                nil)
        (expect (org-kit::other-object-parse-latex-command "\\alpha" 0 6)
                :to-be
                6)
        (expect (org-kit::other-object-parse-latex-command "\\alpha*" 0 7)
                :to-be
                7)
        (expect (org-kit::other-object-parse-latex-command "\\foo[bar]" 0 9)
                :to-be
                9)
        (expect (org-kit::other-object-parse-latex-command "\\foo{a{b}}" 0 10)
                :to-be
                4)
        (expect (org-kit::other-object-parse-latex-command
                 (format nil "\\foo{a~%b}") 0 9)
                :to-be
                4)
        (expect (org-kit::other-object-dollar-fragment-end "$$x$$" 0 5)
                :to-be
                5)
        (expect (org-kit::other-object-dollar-fragment-end "$$x$$" 1 5)
                :to-be
                nil)
        (expect (org-kit::other-object-dollar-fragment-end "$ x$" 0 4)
                :to-be
                nil)
        (expect (org-kit::other-object-parse-latex-fragment "\\(x\\)" 0 5)
                :to-be-truthy)
        (expect (org-kit::other-object-parse-latex-fragment "\\[x\\]" 0 5)
                :to-be-truthy)
        (expect (org-kit::other-object-parse-latex-fragment "\\(x" 0 3)
                :to-be
                nil)
        (expect (org-kit::other-object-parse-latex-fragment "\\alpha" 0 6)
                :to-be-truthy)
        (expect (org-kit::other-object-parse-line-break
                 (format nil "~a~c" slashes #\Newline) 0 3)
                :to-be-truthy)
        (expect (org-kit::other-object-parse-line-break slashes 0 2)
                :to-be-truthy)
        (expect (org-kit::other-object-parse-line-break
                 (format nil "~a~c~c" slashes #\Return #\Newline) 0 4)
                :to-be-truthy)
        (expect (org-kit::other-object-parse-line-break
                 (format nil "~a~c" slashes #\Return) 0 3)
                :to-be-truthy)
        (expect (org-kit::other-object-parse-line-break "\\\\\\" 1 3)
                :to-be
                nil)
        (expect (org-kit::other-object-parse-statistics-cookie "[90%]" 0 5)
                :to-be-truthy)
        (expect (org-kit::other-object-parse-statistics-cookie "[2/5]" 0 5)
                :to-be-truthy)
        (expect (org-kit::other-object-parse-statistics-cookie "[x%]" 0 4)
                :to-be
                nil)
        (expect (org-kit::other-object-parse-statistics-cookie "[2/x]" 0 5)
                :to-be
                nil))))
  (it "covers Babel calls, source blocks, scripts, and table boundaries"
    (with-soft-assertions
      (expect (org-kit::other-object-parse-inline-babel-call "call_fn(x)" 0 10)
              :to-be-truthy)
      (expect
       (org-kit::other-object-parse-inline-babel-call "call_fn[h](x)[end]" 0 18)
       :to-be-truthy)
      (expect (org-kit::other-object-parse-inline-babel-call "call_fn[h(x)" 0 12)
              :to-be
              nil)
      (expect (org-kit::other-object-parse-inline-babel-call "call_(x)" 0 8)
              :to-be
              nil)
      (expect (org-kit::other-object-parse-inline-src-block "src_lisp{x}" 0 11)
              :to-be-truthy)
      (expect
       (org-kit::other-object-parse-inline-src-block "src_lisp[p]{x}" 0 14)
       :to-be-truthy)
      (expect (org-kit::other-object-parse-inline-src-block "src_lisp[p{x}" 0 12)
              :to-be
              nil)
      (expect (org-kit::other-object-parse-inline-src-block "src_{x}" 0 7)
              :to-be
              nil)
      (expect
       (org-kit::other-object-parse-sub-super "x^word" 1 6 :superscript #\^)
       :to-be-truthy)
      (expect
       (org-kit::other-object-parse-sub-super "x^{word}" 1 8 :superscript #\^)
       :to-be-truthy)
      (expect
       (org-kit::other-object-parse-sub-super "x^" 1 2 :superscript #\^)
       :to-be
       nil)
      (expect
       (org-kit::other-object-parse-sub-super "x ^word" 2 7 :superscript #\^)
       :to-be
       nil)
      (expect
       (org-kit::other-object-parse-table-cell "a|b" 2 3 :table-row)
       :to-be-truthy)
      (expect
       (org-kit::other-object-parse-table-cell "a|b" 0 3 :paragraph)
       :to-be
       nil)
      (expect
       (org-kit::other-object-parse-table-cell "a|b" 0 3 :table-row)
       :to-be-truthy)
      (expect
       (org-kit::other-object-parse-table-cell "abc" 1 3 :table-row)
       :to-be
       nil)
      (expect (org-kit::try-parse-other-object (make-document "x") -1 0 :paragraph)
              :to-be
              nil)))
  (it "covers rejected positions, empty names, and optional delimiters"
    (let ((document (make-document "x")))
      (with-soft-assertions
        (expect (org-kit::other-object-valid-range-p document 0 1)
                :to-be-truthy)
        (expect (org-kit::other-object-valid-range-p document 2 3) :to-be nil)
        (expect (org-kit::other-object-valid-range-p document 1 0) :to-be nil)
        (expect (org-kit::other-object-valid-range-p document 0 0) :to-be nil)
        (expect (org-kit::other-object-paired-span "x()" 0 3 #\( #\))
                :to-be
                nil)
        (expect (org-kit::other-object-normalize-header "   " 0 3) :to-be nil)
        (expect (org-kit::other-object-entity-name-at "&_" 0 2) :to-be nil)
        (expect (org-kit::other-object-entity-name-at "&alpha" 0 6)
                :to-be-truthy)
        (expect (org-kit::other-object-entity-name-at "&_  " 0 4)
                :to-be-truthy)
        (expect (org-kit::other-object-entity-name-at "&_ " 0 3)
                :to-be-truthy)
        (expect (org-kit::other-object-parse-latex-command "\\1" 0 2)
                :to-be
                nil)
        (expect (org-kit::other-object-parse-latex-fragment "\\" 0 1)
                :to-be
                nil)
        (expect (org-kit::other-object-parse-latex-fragment "\\(" 0 2)
                :to-be
                nil)
        (expect (org-kit::other-object-parse-latex-fragment "\\[" 0 2)
                :to-be
                nil)
        (expect (org-kit::other-object-dollar-fragment-end "$x$" 0 3)
                :to-be
                3)
        (expect (org-kit::other-object-parse-statistics-cookie "[2/]" 0 4)
                :to-be-truthy)
        (expect (org-kit::other-object-parse-statistics-cookie "x" 0 1)
                :to-be
                nil)
        (expect (org-kit::other-object-parse-statistics-cookie "[12%x]" 0 6)
                :to-be
                nil)
        (expect
         (org-kit::other-object-parse-inline-babel-call "call_fn[h]tail" 0 14)
         :to-be
         nil)
        (expect (org-kit::other-object-parse-inline-src-block "src_lisp{x}" 0 11)
                :to-be-truthy)
        (expect
         (org-kit::other-object-parse-sub-super "x^(word)" 1 8 :superscript #\^)
         :to-be-truthy)
        (expect (org-kit::other-object-parse-table-cell "a|b" 0 3 :table-row)
                :to-be-truthy)))))

;;;; t/object-other-test.lisp
(in-package #:cl-org-kit/test)

(defun other-test-match (source &optional
                                (start 0)
                                (end (length source))
                                (container-type :paragraph)
                                (range-start 0))
  (let* ((document (make-document source))
         (objects (org-kit::parse-objects document
                                          range-start
                                          end
                                          container-type)))
    (find-if (lambda (object)
               (and (= (node-begin object) start)
                    (not (eq (node-type object) :plain-text))))
             objects)))

(defun other-test-property (object key)
  (node-property object key))

(defun other-test-parse-org-matches (source type)
  (let ((matches nil))
    (map-nodes (lambda (node)
                 (when (eq (node-type node) type)
                   (push node matches)))
               (document-root (org-kit:parse-org source)))
    (nreverse matches)))

(defun other-test-parse-org-match (source type)
  (first (other-test-parse-org-matches source type)))

(describe "other object recognition"
          (it
           "recognizes fixture entities, unknown entity spellings, and post-blank"
           (let* ((source "\\alpha{}  tail")
                  (match (other-test-match source))
                  (unknown-source "\\notAnEntity")
                  (unknown-match (other-test-match unknown-source)))
             (with-soft-assertions
              (expect (node-type match) :to-be :entity)
              (expect (node-end match) :to-be 10)
              (expect (node-post-blank match) :to-be 2)
              (expect (node-properties match)
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
              (expect (node-type unknown-match)
                      :to-be
                      :latex-fragment)
              (expect (other-test-property unknown-match :value)
                      :to-equal
                      "\\notAnEntity"))))
          (it "recognizes inline Babel calls and inline source blocks"
              (let* ((call-source
                      (format nil
                              "call_sum[:var x=1~C~C :var y=2](1, 2)[ :results~% raw ]   tail"
                              #\Return
                              #\Newline))
                     (call-match (other-test-match call-source))
                     (source-block
                      (other-test-match
                       (format nil
                               "src_emacs-lisp[:results~% raw]{(+ 1 2)} tail")))
                     (empty-source (other-test-match "src_x{}"))
                     (bad-boundary (other-test-match "xcall_sum(1)" 1))
                     (api-call
                      (other-test-parse-org-match
                       call-source
                       :inline-babel-call))
                     (api-source-block
                      (other-test-parse-org-match
                       (format nil "src_r[:results~% raw]{body}")
                       :inline-src-block)))
                (with-soft-assertions
                 (expect (node-type call-match)
                         :to-be
                         :inline-babel-call)
                 (expect (node-end call-match)
                         :to-be
                         (search "tail" call-source))
                 (expect (node-post-blank call-match) :to-be 3)
                 (expect (node-properties call-match)
                         :to-equal
                         '(:call "sum"
                                 :inside-header
                                 ":var x=1 :var y=2"
                                 :arguments
                                 "1, 2"
                                 :end-header
                                 ":results raw"))
                 (expect (node-type source-block)
                         :to-be
                         :inline-src-block)
                 (expect (node-properties source-block)
                         :to-equal
                         '(:language "emacs-lisp"
                                     :value
                                     "(+ 1 2)"
                                     :parameters
                                     ":results raw"))
                 (expect (other-test-property empty-source :value) :to-equal "")
                 (expect bad-boundary :to-be nil)
                 (expect (node-type api-call) :to-be :inline-babel-call)
                 (expect (node-property api-call :inside-header)
                         :to-equal
                         ":var x=1 :var y=2")
                 (expect (node-property api-call :end-header)
                         :to-equal
                         ":results raw")
                 (expect (node-type api-source-block)
                         :to-be
                         :inline-src-block)
                 (expect (node-property api-source-block :parameters)
                         :to-equal
                         ":results raw"))))
          (it "handles empty and multiline Babel fields in Org paragraphs"
              (let* ((source
                      (format nil
                              "call_empty[](1)~%call_blank(   )~%call_plain(1)~%call_pair[:var x=1~%:var y=2](1)~%call_missing[header]tail~%call_x[]"))
                     (calls
                      (other-test-parse-org-matches
                       source
                       :inline-babel-call))
                     (end-of-input-call
                       (other-test-parse-org-match
                        "call_x"
                        :inline-babel-call)))
                (with-soft-assertions
                  (expect (length calls) :to-be 4)
                  (expect end-of-input-call :to-be nil)
                  (expect (node-property (first calls) :inside-header)
                          :to-be
                          nil)
                  (expect (node-property (second calls) :arguments)
                          :to-be
                          nil)
                  (expect (node-property (third calls) :inside-header)
                          :to-be
                          nil)
                  (expect (node-property (fourth calls) :inside-header)
                          :to-equal
                          ":var x=1 :var y=2"))))
          (it "recognizes inline source blocks without parameters"
              (let ((match
                     (other-test-parse-org-match "src_lisp{body}"
                                                 :inline-src-block))
                    (missing-body
                     (other-test-parse-org-match "src_x"
                                                 :inline-src-block)))
                (with-soft-assertions
                  (expect (node-type match) :to-be :inline-src-block)
                  (expect (node-property match :parameters) :to-be nil)
                  (expect missing-body :to-be nil))))
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
                 (expect (node-type math-match)
                         :to-be
                         :latex-fragment)
                 (expect (node-end math-match) :to-be 7)
                 (expect (node-post-blank math-match) :to-be 2)
                 (expect (other-test-property math-match :value)
                         :to-equal
                         "$x+y$")
                 (expect (other-test-property paren-match :value)
                         :to-equal
                         "\\(a + b\\)")
                 (expect (node-type break-match)
                         :to-be
                         :line-break)
                 (expect (node-end break-match) :to-be 5)
                 (expect (node-properties break-match)
                         :to-be
                         nil)
                 (expect (node-type cookie-match)
                         :to-be
                         :statistics-cookie)
                 (expect (node-properties cookie-match)
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
                 (expect (node-type subscript)
                         :to-be
                         :subscript)
                 (expect
                  (list (node-end subscript)
                        (node-contents-begin subscript)
                        (node-contents-end subscript)
                        (other-test-property subscript :use-brackets-p))
                  :to-equal
                  '(4 2 4 nil))
                 (expect
                  (list (node-end bracketed)
                        (node-contents-begin bracketed)
                        (node-contents-end bracketed)
                        (other-test-property bracketed :use-brackets-p))
                  :to-equal
                  '(7 3 6 t))
                 (expect (node-type superscript)
                         :to-be
                         :superscript)
                 (expect spaced-boundary :to-be nil)
                 (expect (node-type cell) :to-be :table-cell)
                 (expect
                  (list (node-end cell)
                        (node-contents-begin cell)
                        (node-contents-end cell)
                        (node-properties cell))
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

(describe "other object public boundary cases"
  (it "keeps partial fractions and letter-adjacent entity names out of entities"
    (let* ((source "\\frac1, \\alphaβ, \\alpha, \\frac12")
           (entities (other-test-parse-org-matches source :entity))
           (short-fraction
             (other-test-parse-org-matches "\\frac" :entity)))
      (with-soft-assertions
        (expect (mapcar (lambda (node) (node-property node :name)) entities)
                :to-equal
                '("alpha" "frac12"))
        (expect short-fraction :to-equal nil))))
  (it "recognizes percentage and fraction cookies but rejects malformed ones"
    (let* ((source "Statistics [25%], [1/2], [x], and [1/2x].")
           (cookies
             (other-test-parse-org-matches source :statistics-cookie))
           (trailing-opener
             (other-test-parse-org-matches "[" :statistics-cookie))
           (trailing-fraction
             (other-test-parse-org-matches "[1/" :statistics-cookie))
           (trailing-percent
             (other-test-parse-org-matches "[1%" :statistics-cookie)))
      (with-soft-assertions
        (expect (mapcar (lambda (node) (node-property node :value)) cookies)
                :to-equal
                '("[25%]" "[1/2]"))
        (expect trailing-opener :to-equal nil)
        (expect trailing-fraction :to-equal nil)
        (expect trailing-percent :to-equal nil))))
  (it "does not treat three trailing backslashes as a line break"
    (let* ((source
             (concatenate 'string
                          "Text "
                          (make-string 3 :initial-element #\\)
                          (string #\Newline)
                          "continued."))
           (line-breaks
             (other-test-parse-org-matches source :line-break)))
      (expect line-breaks :to-equal nil))))

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
              (expect (node-end crlf) :to-be 5)
              (expect (node-end cr) :to-be 4)
              (expect (node-end ending) :to-be 2)
              (expect bad-break :to-be nil)
              (expect call-header :to-be nil)
              (expect call-no-paren :to-be nil)
              (expect call-no-args-end :to-be nil)
              (expect source-header :to-be nil)
              (expect source-no-body :to-be nil)
              (expect source-unclosed :to-be nil)
              ))))

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
              (expect (node-end signed-sub) :to-be 4)
              (expect (node-end signed-super) :to-be 4)
              (expect (node-end line-cell) :to-be 5)
              (expect (node-end carriage-cell) :to-be 5)
              (expect (node-end no-separator) :to-be 7)
              (expect
               (list (node-contents-begin blank-cell)
                     (node-contents-end blank-cell))
               :to-equal
               (list 1 1))
              (expect
               (list (node-contents-begin trimmed-cell)
                     (node-contents-end trimmed-cell))
               :to-equal
               (list 2 7))
              ))))

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
                                    (node-type
                                     unmatched-command)
                                    :to-be
                                    :latex-fragment)
                                   (expect
                                    (node-end
                                     unmatched-command)
                                    :to-be
                                    4)
                                   (expect
                                    (other-test-property unmatched-command
                                                         :value)
                                    :to-equal
                                    "\\foo")
                                   (expect
                                    (node-type embedded-dollar)
                                    :to-be
                                    :latex-fragment)
                                   (expect
                                    (node-end embedded-dollar)
                                    :to-be
                                    4)
                                   (expect
                                    (other-test-property embedded-dollar :value)
                                    :to-equal
                                    "$x$")
                                   (expect opening-whitespace :to-be nil)
                                   (expect closing-whitespace :to-be nil)
                                   (expect unmatched-double-dollar :to-be nil)))))

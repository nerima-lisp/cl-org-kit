(in-package #:cl-org-kit/test)

(defun export-latex-test-lines (&rest lines)
  (concatenate 'string (format nil "~{~A~^~%~}" lines)
               (string #\Newline)))

(defun export-latex-test-full-source ()
  (export-latex-test-lines
   "#+TITLE: Export #$%&_{}~^"
   "#+AUTHOR: Fixture Author"
   "#+DATE: 2026-10-17"
   "#+LATEX_CLASS: article"
   "#+LATEX_CLASS_OPTIONS: [12pt]"
   "#+LATEX_HEADER: \\usepackage{amsmath}"
   "#+LATEX_HEADER: \\input{/etc/passwd}"
   "#+LATEX: \\typeout{raw #$%&_{}~^}"
   "#+LATEX: \\immediate\\write18{echo review}"
   "#+MACRO: hello Hello $1"
   "#+OPTIONS: toc:nil num:nil"
   "* TODO [#A] All node types :fixture:"
   "SCHEDULED: <2026-10-17 Sat> DEADLINE: <2026-10-18 Sun>"
   ":PROPERTIES:"
   ":CUSTOM_ID: all-node-types"
   ":OWNER: fixture"
   ":END:"
   "Text #$%&_{}~^ with *bold*, /italic/, +strike+, _under_, =code=, ~verbatim~, H_2O and x^2."
   ""
   "Entity \\alpha{}; fragment $x+y$; snippet @@latex:\\emph{raw #$%&_{}~^}@@."
   "Citation [cite/t: see;@smith2024;@doe-2025 p. 4;as noted]."
   "Links [[https://example.test/a_{b}#frag?x=1%20&y=two][safe label]] and [[javascript:alert(1)][unsafe label]]."
   "Target <<local-target>> and radio target <<<radio phrase>>>."
   "Macro {{{hello(world)}}}; footnotes [fn:note] and [fn::inline note]."
   "Inline Babel call call_sum(1) and source src_emacs-lisp{(+ 1 2)}."
   "Statistics [50%] and timestamp <2026-10-17 Sat>."
   "A hard line break follows this text\\\\"
   "[continued] and continues here."
   "#+ATTR_LATEX: :width 0.5\\textwidth :options [scale=1.1]"
   "[[file:figure.png]]"
   "- [X] checked item"
   "- unchecked item"
   "  - nested item"
   ""
   "+ Term :: descriptive definition"
   ""
   "| Head | Value |"
   "|------+-------|"
   "| Alpha | *bold* |"
   ""
   "+---+---+"
   "| 1 | 2 |"
   "+---+---+"
   "#+TBLFM: $2=$1+1"
   ""
   "#+BEGIN_CENTER"
   "centered text"
   "#+END_CENTER"
   "#+BEGIN_QUOTE"
   "quoted text"
   "#+END_QUOTE"
   "#+BEGIN_NOTE details"
   "special block text"
   "#+END_NOTE"
   "#+BEGIN_VERSE"
   "verse line one"
   "verse line two"
   "#+END_VERSE"
   "#+BEGIN_EXAMPLE"
   "example text #$%&_{}~^"
   "#+END_EXAMPLE"
   "#+BEGIN_SRC text :exports code"
   "source text #$%&_{}~^"
   "#+END_SRC"
   "#+BEGIN_EXPORT latex"
   "\\textbf{raw export #$%&_{}~^}"
   "#+END_EXPORT"
   "#+BEGIN_COMMENT"
   "hidden comment"
   "#+END_COMMENT"
   "#+BEGIN: report :scope file"
   "dynamic block text"
   "#+END:"
   "\\begin{equation}"
   "x + y"
   "\\end{equation}"
   "%%(diary-float 10 1 1)"
   "#+CALL: sum(1, 2)"
   "# ordinary comment"
   ": fixed-width line"
   "-----"
   "CLOCK: [2026-10-17 Sat 10:00]--[2026-10-17 Sat 11:00] => 1:00"
   ":NOTES:"
   "drawer text"
   ":END:"
   "* Second heading"
   "Fuzzy target."
   "[fn:note] Footnote definition text."
   "*************** TODO Inline task"
   "Inline task body."))

(defun export-latex-test-full-expected ()
  (format nil "~{~A~%~}"
          '("\\documentclass[12pt]{article}"
            "\\usepackage[utf8]{inputenc}"
            "\\usepackage[T1]{fontenc}"
            "\\usepackage[normalem]{ulem}"
            "\\usepackage{graphicx}"
            "\\usepackage{hyperref}"
            "\\usepackage{amsmath}"
            "\\input{/etc/passwd}"
            ""
            "\\title{Export \\#\\$\\%\\&\\_\\{\\}\\textasciitilde{}\\textasciicircum{}}"
            "\\author{Fixture Author}"
            "\\date{2026-10-17}"
            ""
            "\\begin{document}"
            "\\maketitle"
            "\\typeout{raw #$%&_{}~^}"
            "\\immediate\\write18{echo review}"
            ""
            "\\section*{TODO All node types \\texttt{[fixture]}}\\label{org:all-node-types}"
            ""
            "Text \\#\\$\\%\\&\\textsubscript{}\\textasciitilde{}\\textasciicircum{} with \\textbf{bold}, \\emph{italic}, \\sout{strike}, \\uline{under}, \\texttt{code}, \\texttt{verbatim}, H\\textsubscript{2O} and x\\textsuperscript{2}."
            ""
            ""
            "Entity \\alpha; fragment $x+y$; snippet \\emph{raw #$%&_{}~^}."
            "Citation [cite/t: see;@smith2024;@doe-2025 p. 4;as noted]."
            "Links \\href{https://example.test/a\\_\\{b\\}\\#frag?x=1\\%20\\&y=two}{safe label} and unsafe label."
            "Target \\label{org:local-target} and radio target \\label{org:radio-phrase}radio phrase."
            "Macro Hello world; footnotes \\footnote{Footnote definition text."
            "} and \\footnote{inline note}."
            "Inline Babel call call\\_sum(1) and source \\texttt{(+ 1 2)}."
            "Statistics [50\\%] and timestamp <2026-10-17 Sat>."
            "A hard line break follows this text\\\\{}"
            "[continued] and continues here."
            ""
            ""
            "% ATTR_LATEX: :width 0.5\\textwidth :options [scale=1.1]"
            "\\includegraphics{figure.png}"
            ""
            ""
            "\\begin{itemize}"
            "\\item{} [X] checked item"
            ""
            "\\item unchecked item"
            ""
            ""
            "\\begin{itemize}"
            "\\item nested item"
            ""
            "\\end{itemize}"
            "\\item descriptive definition"
            ""
            "\\end{itemize}"
            ""
            "\\begin{tabular}{ll}"
            ""
            "Head & Value \\\\"
            "\\hline"
            "Alpha & \\textbf{bold} \\\\"
            "\\end{tabular}"
            ""
            "+---+---+"
            "| 1 | 2 |"
            "+---+---+"
            ""
            ""
            "\\begin{center}"
            "centered text"
            ""
            "\\end{center}"
            ""
            "\\begin{quote}"
            "quoted text"
            ""
            "\\end{quote}"
            ""
            "\\begin{note}"
            "special block text"
            ""
            "\\end{note}"
            ""
            "\\begin{verse}"
            "verse line one"
            "verse line two"
            ""
            "\\end{verse}"
            ""
            "\\begin{verbatim}"
            "example text #$%&_{}~^"
            "\\end{verbatim}"
            ""
            "\\begin{verbatim}"
            "source text #$%&_{}~^"
            "\\end{verbatim}"
            ""
            "\\textbf{raw export #$%&_{}~^}"
            ""
            ""
            "\\begin{verbatim}"
            "#+BEGIN: report :scope file"
            "dynamic block text"
            "#+END:"
            "\\end{verbatim}"
            ""
            "\\begin{equation}"
            "x + y"
            "\\end{equation}"
            ""
            ""
            "\\%\\%(diary-float 10 1 1)"
            ""
            ""
            "\\#+CALL: sum(1, 2)"
            ""
            ""
            "\\begin{verbatim}"
            "fixed-width line"
            "\\end{verbatim}"
            ""
            "\\noindent\\rule{\\linewidth}{0.4pt}"
            ""
            "drawer text"
            ""
            ""
            "\\section*{Second heading}\\label{org:second-heading}"
            ""
            "Fuzzy target."
            ""
            ""
            "\\subparagraph*{TODO Inline task}\\label{org:inline-task}"
            ""
            "Inline task body."
            ""
            ""
            "\\end{document}")))

(defun export-latex-test-node-types (document)
  (let ((types nil))
    (map-nodes (lambda (node) (pushnew (node-type node) types :test #'eq))
               (document-root document))
    (sort types #'string< :key #'symbol-name)))

(defun export-latex-test-catch-error (function)
  (handler-case (progn (funcall function) nil)
    (error (condition) condition)))

(defun export-latex-test-state (document &key allow-raw-latex)
  (let* ((preparation
           (org-kit::export-prepare
            document :latex :allow-raw-latex allow-raw-latex))
         (state (org-kit::%make-export-latex-state
                 :preparation preparation :limits (document-limits document))))
    (org-kit::export-latex-index-references state)
    state))

(defun export-latex-test-view (document type &key properties children payload
                                         (post-blank 0))
  (org-kit::%make-export-view
   :node (make-node type :properties properties :post-blank post-blank)
   :children children :payload payload :source-document document))

(defvar *export-latex-test-link-observation* nil)

(defun export-latex-test-derived-renderer (context)
  (concatenate
   'string
   (if (export-option-explicit-p context :num)
       (format nil "num=~A;"
               (if (export-option-value context :num) "on" "off"))
       "num=default;")
   (export-render-parent-renderer context)))

(defun export-latex-test-derived-preamble (context)
  (concatenate 'string "P[" (export-render-parent-preamble context) "]"))

(defun export-latex-test-derived-trailer (context)
  (concatenate 'string "T[" (export-render-parent-trailer context) "]"))

(defun export-latex-test-derived-template (context preamble body trailer)
  (concatenate 'string "<"
               (export-render-parent-template context preamble body trailer)
               ">"))

(defun export-latex-test-derived-plain-text (node view frame context)
  (declare (ignore frame))
  (concatenate 'string "["
               (export-render-parent-node context node :view view)
               "]"))

(defun export-latex-test-derived-body-filter (body context)
  (declare (ignore context))
  (concatenate 'string body "|body-filter"))

(defun export-latex-test-derived-link-handler (path description frame context)
  (setf *export-latex-test-link-observation*
        (list path description (export-render-frame-mode frame)
              (export-render-frame-table-cell-p frame)
              (export-render-context-backend context)))
  (make-export-link-result
   :kind :link
   :destination (concatenate 'string "https://doi.org/" path)
   :description description))

(describe "LaTeX export"
  (it "matches the complete fixed expected fixture through the public API"
    (let* ((source (export-latex-test-full-source))
           (document (parse-org source))
           (actual (export-org document :latex :allow-raw-latex t))
           (expected (export-latex-test-full-expected)))
      (with-soft-assertions
        (expect (plusp (length source)) :to-be t)
        (expect (export-latex-test-node-types document)
                :to-equal
                (sort (copy-list (node-types)) #'string< :key #'symbol-name))
        (expect actual :to-equal expected)
        (expect (search "hidden comment" actual) :to-be nil)
        (expect (search "javascript:" actual) :to-be nil))))

  (it "escapes every TeX special in text and URL arguments"
    (let* ((source
             (export-latex-test-lines
              "#+TITLE: Specials # $ % & _ { } ~ ^ \\"
              "#+AUTHOR: Author # $ % & _ { } ~ ^ \\"
              "#+DATE: Date # $ % & _ { } ~ ^ \\"
              "#+OPTIONS: toc:nil num:nil"
              "Text body."
              ""
              "[[https://example.test/a$_{b}#frag?x=1%20&y=two~three^four][URL label]]"
              "[[https://example.test/back\\slash][backslash fallback]]"))
           (output (export-org (parse-org source) :latex)))
      (with-soft-assertions
        (expect (search
                 "\\title{Specials \\# \\$ \\% \\& \\_ \\{ \\} \\textasciitilde{} \\textasciicircum{} \\textbackslash{}}"
                 output)
                :to-be-truthy)
        (expect (search
                 "\\author{Author \\# \\$ \\% \\& \\_ \\{ \\} \\textasciitilde{} \\textasciicircum{} \\textbackslash{}}"
                 output)
                :to-be-truthy)
        (expect (search
                 "\\date{Date \\# \\$ \\% \\& \\_ \\{ \\} \\textasciitilde{} \\textasciicircum{} \\textbackslash{}}"
                 output)
                :to-be-truthy)
        (expect (search
                 "\\href{https://example.test/a\\$\\_\\{b\\}\\#frag?x=1\\%20\\&y=two\\textasciitilde{}three\\textasciicircum{}four}{URL label}"
                 output)
                :to-be-truthy)
        (expect (search "backslash fallback" output) :to-be-truthy)
        (expect (search "https://example.test/back" output) :to-be nil))))

  (it "compares every raw LaTeX channel exactly with and without opt-in"
    (flet ((render (source allow-raw)
             (export-org (parse-org source) :latex
                         :allow-raw-latex allow-raw))
           (expected-document (body)
             (format nil "~{~A~%~}"
                     (append
                      '("\\documentclass[11pt]{article}"
                        "\\usepackage[utf8]{inputenc}"
                        "\\usepackage[T1]{fontenc}"
                        "\\usepackage[normalem]{ulem}"
                        "\\usepackage{graphicx}"
                        "\\usepackage{hyperref}"
                        ""
                        "\\date{}"
                        ""
                        "\\begin{document}")
                      (when body (list body))
                      (if body (quote ("" "\\end{document}")) (quote ("" "" "\\end{document}")))))))
      (let* ((default-expected (expected-document nil))
             (header-source
               (export-latex-test-lines
                "#+LATEX_CLASS: report"
                "#+LATEX_CLASS_OPTIONS: 10pt"
                "#+LATEX_HEADER: \\usepackage{amsmath}"
                "#+LATEX_HEADER_EXTRA: \\input{/etc/passwd}"))
             (header-expected
               (format nil "~{~A~%~}"
                       '("\\documentclass[10pt]{report}"
                         "\\usepackage[utf8]{inputenc}"
                         "\\usepackage[T1]{fontenc}"
                         "\\usepackage[normalem]{ulem}"
                         "\\usepackage{graphicx}"
                         "\\usepackage{hyperref}"
                         "\\usepackage{amsmath}"
                         "\\input{/etc/passwd}"
                         ""
                         "\\date{}"
                         ""
                         "\\begin{document}"
                         ""
                         ""
                         "\\end{document}")))
             (raw-cases
               (list
                (list "#+LATEX: \\catcode65=12" "\\catcode65=12")
                (list (format nil
                              "#+BEGIN_EXPORT latex~%\\input{/etc/passwd}~%#+END_EXPORT")
                      (format nil "\\input{/etc/passwd}~%"))
                (list (format nil
                              "#+BEGIN_EXPORT tex~%\\write18{echo review}~%#+END_EXPORT")
                      (format nil "\\write18{echo review}~%"))
                (list "@@latex:\\end{verbatim}@@" "\\end{verbatim}")
                (list "$\\input{/etc/passwd}$"
                      "$\\input{/etc/passwd}$")
                (let ((environment
                        (format nil
                                "\\begin{equation}~%\\input{/etc/passwd}~%\\end{equation}")))
                  (list environment environment)))))
        (with-soft-assertions
          (expect (render header-source nil) :to-equal default-expected)
          (expect (render header-source t) :to-equal header-expected)
          (dolist (case raw-cases)
            (expect (render (first case) nil) :to-equal default-expected)
            (expect (render (first case) t)
                    :to-equal (expected-document (second case))))))))
  (it "defaults to safe output and preserves raw LaTeX only when opted in"
    (let* ((source (export-latex-test-full-source))
           (tex-source
             (format nil "#+BEGIN_EXPORT tex~%\\textit{tex export}~%#+END_EXPORT"))
           (tex-document (parse-org tex-source))
           (tex-default-output (export-org tex-document :latex))
           (tex-output
             (export-org tex-document :latex :allow-raw-latex t))
           (tex-expected
             (format nil "~{~A~%~}"
                     '("\\documentclass[11pt]{article}"
                       "\\usepackage[utf8]{inputenc}"
                       "\\usepackage[T1]{fontenc}"
                       "\\usepackage[normalem]{ulem}"
                       "\\usepackage{graphicx}"
                       "\\usepackage{hyperref}"
                       ""
                       "\\date{}"
                       ""
                       "\\begin{document}"
                       "\\textit{tex export}"
                       ""
                       ""
                       "\\end{document}")))
           (document (parse-org source))
           (default-output (export-org document :latex))
           (output (export-org document :latex :allow-raw-latex t))
           (empty-output (export-org (parse-org "") :latex))
           (empty-expected
             (format nil "~{~A~%~}"
                     '("\\documentclass[11pt]{article}"
                       "\\usepackage[utf8]{inputenc}"
                       "\\usepackage[T1]{fontenc}"
                       "\\usepackage[normalem]{ulem}"
                       "\\usepackage{graphicx}"
                       "\\usepackage{hyperref}"
                       ""
                       "\\date{}"
                       ""
                       "\\begin{document}"
                       ""
                       ""
                       "\\end{document}"))))
      (with-soft-assertions
        (expect empty-output :to-equal empty-expected)
        (expect tex-output :to-equal tex-expected)
        (expect (search "\\textit{tex export}" tex-default-output)
                :to-be nil)
        (expect (search "\\documentclass[11pt]{article}" default-output)
                :to-be-truthy)
        (expect (search "\\documentclass[12pt]{article}" default-output)
                :to-be nil)
        (expect (search "\\usepackage{amsmath}" default-output) :to-be nil)
        (expect (and (null (search "\\typeout{raw #$%&_{}~^}" default-output))
             (null (search "\\input{/etc/passwd}" default-output))
             (null (search "\\write18" default-output)))
        :to-be t)
        (expect (search "\\begin{equation}" default-output) :to-be nil)
        (expect (search "\\emph{raw #$%&_{}~^}" default-output) :to-be nil)
        (expect (search "\\textbf{raw export #$%&_{}~^}" default-output)
                :to-be nil)
        (expect (search "\\documentclass[12pt]{article}" output)
                :to-be-truthy)
        (expect (search "\\usepackage{amsmath}" output) :to-be-truthy)
        (expect (and (search "\\typeout{raw #$%&_{}~^}" output)
             (search "\\input{/etc/passwd}" output)
             (search "\\write18" output))
        :to-be-truthy)
        (expect (search "\\begin{equation}" output) :to-be-truthy)
        (expect (search "\\textbf{raw export #$%&_{}~^}" output)
                :to-be-truthy)
        (expect (search "% ATTR_LATEX: :width 0.5\\textwidth :options [scale=1.1]"
                        output)
                :to-be-truthy)
        (expect (search "unsafe label" output) :to-be-truthy)
        (expect (search "javascript:" output) :to-be nil))))

  (it "enforces the configured output-length budget"
    (let* ((document (parse-org "Body"
                                :limits (make-limits :max-output-length 80)))
           (condition (export-latex-test-catch-error
                       (lambda () (export-org document :latex)))))
      (with-soft-assertions
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when (typep condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name condition)
                  :to-be :max-output-length)
          (expect (> (org-resource-limit-error-actual condition) 80)
                  :to-be t)))))

  (it "accepts output when the budget equals its rendered length"
    (let* ((source "- list item")
           (expected (export-org (parse-org source) :latex))
           (actual (export-org
                    (parse-org source
                               :limits (make-limits
                                        :max-output-length
                                        (length expected)))
                    :latex)))
      (expect actual :to-equal expected)))

  (it "preflights raw LaTeX headers and attributes against the output budget"
    (let* ((large-value (make-string 256 :initial-element #\x))
           (empty-output (export-org (parse-org "") :latex))
           (limits (make-limits :max-output-length
                                (+ (length empty-output) 64)))
           (header-condition
             (export-latex-test-catch-error
              (lambda ()
                (export-org
                 (parse-org (format nil "#+LATEX_HEADER: ~A" large-value)
                            :limits limits)
                 :latex :allow-raw-latex t))))
           (attribute-condition
             (export-latex-test-catch-error
              (lambda ()
                (export-org
                 (parse-org
                  (format nil "#+ATTR_LATEX: ~A~%[[file:image.png]]"
                          large-value)
                  :limits limits)
                 :latex)))))
      (with-soft-assertions
        (expect (typep header-condition 'org-resource-limit-error)
                :to-be t)
        (expect (typep attribute-condition 'org-resource-limit-error)
                :to-be t))))

  (it "preflights source blocks and rejects verbatim closing delimiters"
    (let* ((source-body (make-string 256 :initial-element #\x))
           (empty-output (export-org (parse-org "") :latex))
           (limit (+ (length empty-output) 64))
           (source (format nil "#+BEGIN_SRC text~%~A~%#+END_SRC"
                           source-body))
           (condition
             (export-latex-test-catch-error
              (lambda ()
                (export-org
                 (parse-org source
                            :limits (make-limits :max-output-length limit))
                 :latex))))
           (unsafe-sources
             (list
              (format nil "#+BEGIN_EXAMPLE~%\\\\end{verbatim}~%#+END_EXAMPLE")
              (format nil "#+BEGIN_SRC text~%\\\\end{verbatim}~%#+END_SRC")
              ": \\end{verbatim}"
              (format nil "#+BEGIN: report :scope file~%\\\\end{verbatim}~%#+END:")))
           (unsafe-conditions
             (mapcar
              (lambda (unsafe-source)
                (export-latex-test-catch-error
                 (lambda ()
                   (export-org (parse-org unsafe-source) :latex))))
              unsafe-sources))
           (special-block-name
             (format nil "x}~Cinput{/etc/passwd}~Cbegin{x"
                     #\Backslash #\Backslash))
           (special-block-output
             (export-org
              (parse-org
               (format nil "#+BEGIN_~A~%safe body~%#+END_~A"
                       special-block-name special-block-name))
              :latex)))
      (with-soft-assertions
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when (typep condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name condition)
                  :to-be :max-output-length)
          (expect (> (org-resource-limit-error-actual condition) limit)
                  :to-be t))
        (expect (= (length unsafe-conditions) 4) :to-be t)
        (expect
         (every
          (lambda (unsafe-condition)
            (and (typep unsafe-condition 'org-export-error)
                 (eq (org-export-error-reason unsafe-condition)
                     :unsafe-verbatim-body)))
          unsafe-conditions)
         :to-be t)
        (expect (search "\\begin{quote}" special-block-output)
                :to-be-truthy)
        (expect (search "\\input{" special-block-output) :to-be nil)
        (expect (search "\\begin{x}" special-block-output) :to-be nil))))

  (it "renders default preamble, heading options, and safe link forms"
    (let* ((source
             (export-latex-test-lines
              "#+TITLE: Configured title"
              "#+OPTIONS: toc:t num:1 title:nil todo:t pri:t tags:t"
              "* TODO [#A] Main :one:two:"
              ":PROPERTIES:"
              ":CUSTOM_ID: main"
              ":END:"
              "[[#main][Jump]] [[file:notes.txt][Readme]] [[mailto:user@example.test][Mail]]"
              "** Child"
              "- [ ] unchecked"
              "- [-] partial"))
           (document (parse-org source))
           (output (export-org document :latex)))
      (with-soft-assertions
        (expect (search "\\documentclass[11pt]{article}" output)
                :to-be-truthy)
        (expect (search "\\title{Configured title}" output) :to-be-truthy)
        (expect (search "\\tableofcontents" output) :to-be-truthy)
        (expect (search "\\section{TODO [A] Main \\texttt{[one:two]}}"
                        output)
                :to-be-truthy)
        (expect (search "\\subsection*{Child}" output) :to-be-truthy)
        (expect (search "\\hyperref[org:main]{Jump}" output) :to-be-truthy)
        (expect (search "\\href{notes.txt}{Readme}" output) :to-be-truthy)
        (expect (search "\\href{mailto:user@example.test}{Mail}" output)
                :to-be-truthy)
        (expect (search "\\item{} [ ] unchecked" output) :to-be-truthy)
        (expect (search "\\item{} [-] partial" output) :to-be-truthy)
        (expect (search "\\maketitle" output) :to-be nil))))

  (it "rejects global LaTeX keywords in included fragments"
    (let* ((fragments
             (list
              "#+LATEX_CLASS: article"
              "#+LATEX_CLASS_OPTIONS: [10pt]"
              "#+LATEX_HEADER: \\usepackage{amsmath}"
              "#+LATEX_HEADER_EXTRA: extra"))
           (conditions
             (mapcar
              (lambda (fragment)
                (export-latex-test-catch-error
                 (lambda ()
                   (export-org
                    (parse-org "#+INCLUDE: \"fragment.org\"") :latex
                    :include-resolver
                    (lambda (path source-name)
                      (declare (ignore path source-name))
                      fragment)))))
              fragments)))
      (with-soft-assertions
        (expect (every (lambda (condition)
                         (and (typep condition 'org-export-unsupported-syntax)
                              (eq (org-export-unsupported-syntax-feature condition)
                                  :fragment-keyword-scope)))
                       conditions)
                :to-be t))))

  (it "covers LaTeX renderer fallbacks and inline view separators"
    (let* ((document (parse-org "Body"))
           (state (export-latex-test-state document))
           (first (export-latex-test-view
                   document :inline-src-block :properties '(:value "left")
                   :post-blank 2))
           (second (export-latex-test-view
                    document :inline-src-block :properties '(:value "right")))
           (table-cell-a
             (export-latex-test-view
              document :table-cell
              :children (list (export-latex-test-view
                               document :inline-src-block
                               :properties '(:value "A")))))
           (table-cell-b
             (export-latex-test-view
              document :table-cell
              :children (list (export-latex-test-view
                               document :inline-src-block
                               :properties '(:value "B")))))
           (table-row
             (export-latex-test-view document :table-row
                                     :children (list table-cell-a table-cell-b)))
           (item
             (export-latex-test-view
              document :item :properties '(:checkbox :trans :tag ("a]b"))
              :children (list (export-latex-test-view
                               document :inline-src-block
                               :properties '(:value "definition")))))
           (ordered-list
             (export-latex-test-view document :plain-list
                                     :properties '(:type :ordered)
                                     :children (list item)))
           (descriptive-list
             (export-latex-test-view document :plain-list
                                     :properties '(:type :descriptive)
                                     :children (list item)))
           (radio-target (make-node :radio-target :properties '(:value "target")))
           (footnote (make-node :footnote-reference :properties '(:label "missing")))
           (radio-output nil)
           (unresolved-footnote
             (export-latex-test-catch-error
              (lambda ()
                (org-kit::export-latex-render-object state footnote 0)))))
      (setf (gethash radio-target
                     (org-kit::export-latex-state-reference-ids state))
            "org:target")
      (setf radio-output
            (org-kit::export-latex-render-object state radio-target 0))
      (with-soft-assertions
        (progn
          (expect (org-kit::export-latex-rendered-views
                   state (list first second) 0 t)
                  :to-equal "\\texttt{left}  \\texttt{right}")
          (expect (org-kit::export-latex-render-inline-views
                   state
                   (list (export-latex-test-view
                          document :latex-fragment
                          :properties '(:value "\\ignored{}")
                          :post-blank 1)
                         (export-latex-test-view
                          document :inline-src-block
                          :properties '(:value "visible")))
                   0)
                  :to-equal " \\texttt{visible}"))
        (expect (org-kit::export-latex-render-view state table-row)
                :to-equal "\\texttt{A} & \\texttt{B}")
        (expect (org-kit::export-latex-render-view state table-cell-a)
                :to-equal "\\texttt{A}")
        (expect (search "\\begin{enumerate}"
                        (org-kit::export-latex-render-view state ordered-list))
                :to-be-truthy)
        (expect (search "\\begin{description}"
                        (org-kit::export-latex-render-view state descriptive-list))
                :to-be-truthy)
        (expect (search "\\item[{a]b}] [-] \\texttt{definition}"
                (org-kit::export-latex-render-view state descriptive-list))
        :to-be-truthy)
        (progn
          (expect (org-kit::export-latex-render-view state item)
                  :to-equal "\\item{} [-] \\texttt{definition}")
          (progn
            (expect (org-kit::export-latex-render-view
                     state (export-latex-test-view
                            document :special-block
                            :properties '(:type "proof*")))
                    :to-equal
                    (format nil "\\begin{proof*}~%~%\\end{proof*}"))
            (expect (org-kit::export-latex-render-view
                     state (export-latex-test-view
                            document :special-block
                            :properties (list :type 42)))
                    :to-equal
                    (format nil "\\begin{quote}~%~%\\end{quote}"))
            (expect (org-kit::export-latex-render-view
                     state (export-latex-test-view
                            document :special-block
                            :properties (list :type "")))
                    :to-equal
                    (format nil "\\begin{quote}~%~%\\end{quote}"))
            (expect (org-kit::export-latex-render-view
                     state (export-latex-test-view
                            document :special-block
                            :properties (list :type "pπ")))
                    :to-equal
                    (format nil "\\begin{quote}~%~%\\end{quote}"))))
        (expect radio-output :to-equal "\\label{org:target}target")
        (progn
  (expect (typep unresolved-footnote 'org-export-error) :to-be t)
  (let* ((recursive-reference
           (export-latex-test-view
            document :footnote-reference :properties '(:label "a")))
         (definition
           (export-latex-test-view
            document :footnote-definition
            :children
            (list (export-latex-test-view
                   document :paragraph :children (list recursive-reference)))))
         (preparation (org-kit::export-latex-state-preparation state)))
    (setf (org-kit::export-preparation-footnotes preparation)
          (list (list :label "a" :definition definition)))
    (progn
      (expect (org-kit::export-latex-render-view state recursive-reference)
              :to-equal "\\footnote{\\footnotemark}")
      (let* ((nested-reference
               (export-latex-test-view
                document :footnote-reference :properties '(:label "b")))
             (nested-definition
               (export-latex-test-view document :footnote-definition))
             (outer-definition
               (export-latex-test-view
                document :footnote-definition
                :children
                (list (export-latex-test-view
                       document :paragraph :children (list nested-reference)))))
             (preparation (org-kit::export-latex-state-preparation state)))
        (setf (org-kit::export-preparation-footnotes preparation)
              (list (list :label "a" :definition outer-definition)
                    (list :label "b" :definition nested-definition)))
        (expect (org-kit::export-latex-render-view state recursive-reference)
                :to-equal "\\footnote{\\footnotemark}")))
    (expect (org-kit::export-latex-state-active-footnotes state)
            :to-be nil))
  (let* ((limited-document
           (parse-org "Body"
                      :limits (make-limits :max-export-expansion-depth 0)))
         (limited-state (export-latex-test-state limited-document))
         (recursive-reference
           (export-latex-test-view
            limited-document :footnote-reference :properties '(:label "a")))
         (definition
           (export-latex-test-view
            limited-document :footnote-definition
            :children
            (list (export-latex-test-view
                   limited-document :paragraph
                   :children (list recursive-reference)))))
         (preparation
           (org-kit::export-latex-state-preparation limited-state)))
    (setf (org-kit::export-preparation-footnotes preparation)
          (list (list :label "a" :definition definition)))
    (let ((condition
            (export-latex-test-catch-error
             (lambda ()
               (org-kit::export-latex-render-view
                limited-state recursive-reference)))))
      (expect (and (typep condition 'org-resource-limit-error)
                   (eq (org-resource-limit-error-limit-name condition)
                       :max-export-expansion-depth))
              :to-be t))
    (expect (org-kit::export-latex-state-active-footnotes limited-state)
            :to-be nil)))
        (expect (org-kit::export-latex-source-text
                 (make-node :latex-fragment :properties '(:value "raw")))
                :to-equal "raw")
        (expect (org-kit::export-latex-source-text (make-node :latex-fragment))
                :to-equal "")
        (expect (org-kit::export-latex-render-code state
                                                   (format nil "code~%~%") 0)
                :to-equal (format nil "\\begin{verbatim}~%code~%\\end{verbatim}")))))

  (it "renders title macros, keyword forms, and source result views"
    (let* ((document (parse-org "Body"))
           (state (export-latex-test-state document :allow-raw-latex t))
           (macro-node (make-node :macro :post-blank 1))
           (macro-view
             (export-latex-test-view
              document :headline :properties (list :title
                                                    (list "Title " macro-node
                                                          (make-node
                                                           :inline-src-block
                                                           :properties
                                                           '(:value "direct"))))
              :payload (list :title-macros
                             (list (cons macro-node
                                         (list (export-latex-test-view
                                                document :inline-src-block
                                                :properties '(:value "world"))))))))
           (source-result
             (export-latex-test-view
              document :src-block :properties '(:value "hidden source")
              :payload '(:requested-exports :results)
              :children (list (export-latex-test-view
                               document :inline-src-block
                               :properties '(:value "result")))))
           (raw-keyword
             (export-latex-test-view document :keyword
                                     :properties '(:key "LATEX" :value "\\raw{}")))
           (attribute-keyword
             (export-latex-test-view document :keyword
                                     :properties '(:key "ATTR_LATEX" :value ":width 2")))
           (foreign-export
             (export-latex-test-view document :export-block
                                     :properties '(:type "html" :value "<b>x</b>")))
           (fallback-object (make-node :org-data)))
      (with-soft-assertions
        (expect (org-kit::export-latex-render-title
                 state (make-node :headline :properties
                                  '(:title nil :raw-value "Raw # title")) nil 0)
                :to-equal "Raw \\# title")
        (expect (search "\\texttt{world} \\texttt{direct}"
                        (org-kit::export-latex-render-title state
                                                            (org-kit::export-view-node
                                                             macro-view)
                                                            macro-view 0))
                :to-be-truthy)
        (progn
          (expect (org-kit::export-latex-render-view state source-result)
                  :to-equal "\\texttt{result}")
          (expect
           (org-kit::export-latex-render-view
            state
            (export-latex-test-view
             document :src-block
             :properties (list :value "visible code")
             :payload (list :requested-exports :code)
             :children
             (list (export-latex-test-view
                    document :inline-src-block
                    :properties (list :value "hidden result")))))
           :to-equal
           (format nil "\\begin{verbatim}~%visible code~%\\end{verbatim}")))
        (expect (org-kit::export-latex-render-view state raw-keyword)
                :to-equal "\\raw{}")
        (expect (org-kit::export-latex-render-view state attribute-keyword)
                :to-equal "% ATTR_LATEX: :width 2")
        (expect (org-kit::export-latex-render-view state foreign-export)
                :to-equal "")
        (expect (org-kit::export-latex-render-object
                 state fallback-object 0)
                :to-equal ""))))

  (it "expands link abbreviations and preserves default class-option syntax"
    (let* ((document (parse-org "#+LATEX_CLASS_OPTIONS: 10pt"))
           (preparation (org-kit::export-prepare document :latex :allow-raw-latex t))
           (state (org-kit::%make-export-latex-state
                   :preparation preparation :limits (document-limits document))))
      (setf (getf (document-settings document) :link-abbrevs)
            '(("docs" . "https://example.test/docs/%s")
              ("bare" . "https://example.test/base/")))
      (with-soft-assertions
        (expect (org-kit::export-latex-expand-link-abbreviation "docs:guide" document)
                :to-equal "https://example.test/docs/guide")
        (expect (org-kit::export-latex-expand-link-abbreviation "bare:guide" document)
                :to-equal "https://example.test/base/guide")
        (expect (org-kit::export-latex-expand-link-abbreviation "docs::guide" document)
                :to-equal "https://example.test/docs/guide")
        (expect (org-kit::export-latex-expand-link-abbreviation "docs::" document)
                :to-equal "https://example.test/docs/")
        (expect (org-kit::export-latex-expand-link-abbreviation "docs" document)
                :to-equal "https://example.test/docs/")
        (expect (org-kit::export-latex-expand-link-abbreviation "bare" document)
                :to-equal "https://example.test/base/")
        (expect (org-kit::export-latex-expand-link-abbreviation "bare:" document)
                :to-equal "https://example.test/base/")
        (expect (org-kit::export-latex-expand-link-abbreviation "other:guide" document)
                :to-equal "other:guide")
        (expect (search "\\documentclass[10pt]{article}"
                        (org-kit::export-latex-preamble state))
                :to-be-truthy))))

  (it "covers LaTeX renderer edge branches"
    (let* ((document
             (parse-org
              (export-latex-test-lines
               "#+OPTIONS: pri:t" "* Character" "* Numeric")))
           (state (export-latex-test-state document :allow-raw-latex t))
           (headline-views
             (org-kit::export-collect-views
              (org-kit::export-preparation-root-views
               (org-kit::export-latex-state-preparation state))))
           (character-heading
             (find-if
              (lambda (view)
                (let ((node (org-kit::export-view-node view)))
                  (and (eq (node-type node) :headline)
                       (string= (node-property node :raw-value) "Character"))))
              headline-views))
           (numeric-heading
             (find-if
              (lambda (view)
                (let ((node (org-kit::export-view-node view)))
                  (and (eq (node-type node) :headline)
                       (string= (node-property node :raw-value) "Numeric"))))
              headline-views))
           (tag-node (make-node :code :properties '(:value "~Term~")))
           (tagged-item
             (export-latex-test-view
              document :item :properties (list :tag (list tag-node))))
           (tagged-list
             (export-latex-test-view
              document :plain-list :properties '(:type :descriptive)
              :children (list tagged-item)))
           (label
             (export-latex-test-view
              document :inline-src-block :properties '(:value "label")))
           (hash-link
             (export-latex-test-view
              document :link :properties '(:type "custom-id" :path "#anchor")
              :children (list label)))
           (unsafe-file-link
             (export-latex-test-view
              document :link :properties '(:type "file" :path "//unsafe.example")
              :children (list label)))
           (image-link
             (export-latex-test-view
              document :link :properties '(:type "file" :path "image.png")
              :children (list label)))
           (untyped-link
             (export-latex-test-view document :link :children (list label)))
           (paragraph
             (export-latex-test-view document :paragraph :children (list label)))
           (item-with-empty-child
             (export-latex-test-view
              document :item
              :children (list paragraph
                              (export-latex-test-view document :comment))))
           (utf8-entity (make-node :entity :properties '(:utf-8 "U")))
           (unicode-entity (make-node :entity :properties '(:unicode "N")))
           (empty-entity (make-node :entity))
           (plain-code (make-node :code :properties '(:value "plain")))
           (unclosed-code (make-node :code :properties '(:value "~open")))
           (empty-code (make-node :code :properties '(:value "")))
           (inline-babel (make-node :inline-babel-call :properties '(:value "call")))
           (macro
             (make-node :macro :children
                        (list (make-node :inline-src-block
                                         :properties '(:value "macro")))))
           (empty-footnote (make-node :footnote-reference))
           (empty-footnote-view
             (export-latex-test-view document :footnote-reference))
           (unknown-keyword
             (export-latex-test-view
              document :keyword :properties '(:key "OTHER" :value "ignored")))
           (empty-source-block
             (export-latex-test-view
              document :src-block :properties '(:value "")
              :payload '(:requested-exports :both)))
           (latex-snippet
             (make-node :export-snippet
                        :properties '(:back-end "latex" :value "\\raw{}")))
           (foreign-snippet
             (make-node :export-snippet
                        :properties '(:back-end "html" :value "<b>x</b>")))
           (no-path-link (make-node :link :properties '(:type "custom-id")))
           (external-link
             (make-node :link :properties '(:type "https" :path "example.test")))
           (fuzzy-link
             (make-node :link :properties '(:type "fuzzy" :path "anchor")))
           (raw-link
             (make-node :link
                        :properties '(:type "https" :path "example.test"
                                      :raw-link "https://example.test")))
           (source-document (make-document "xe"))
           (source-example (make-node :example-block :begin 0 :end 1))
           (source-entity (make-node :entity :begin 1 :end 2))
           (source-root
             (make-node :org-data :children (list source-example source-entity))))
      (setf (document-root source-document) source-root
            (gethash "anchor" (org-kit::export-latex-state-references state))
            "org:anchor"
            (node-property (org-kit::export-view-node character-heading)
                           :priority)
            #\B
            (node-property (org-kit::export-view-node numeric-heading)
                           :priority)
            10)
      (let* ((source-state (export-latex-test-state source-document))
             (source-example-view
               (org-kit::%make-export-view
                :node source-example :source-document source-document)))
        (with-soft-assertions
          (expect (search "\\section{[B] Character}\\label{org:character}"
                          (org-kit::export-latex-render-view state character-heading))
                  :to-be-truthy)
          (expect (search "\\section{[10] Numeric}\\label{org:numeric}"
                          (org-kit::export-latex-render-view state numeric-heading))
                  :to-be-truthy)
          (setf (node-property (org-kit::export-view-node numeric-heading) :priority)
                "P")
          (expect (search "\\section{[P] Numeric}"
                          (org-kit::export-latex-render-view state numeric-heading))
                  :to-be-truthy)
          (expect (search "\\item[{\\texttt{Term}}]"
                          (org-kit::export-latex-render-view state tagged-list))
                  :to-be-truthy)
          (expect (org-kit::export-latex-render-list-item
                   state item-with-empty-child 0)
                  :to-equal "\\item \\texttt{label}")
          (expect (org-kit::export-latex-render-view state hash-link)
                  :to-equal "\\hyperref[org:anchor]{\\texttt{label}}")
          (expect (org-kit::export-latex-render-view state unsafe-file-link)
                  :to-equal "\\texttt{label}")
          (progn
            (expect (org-kit::export-latex-render-view state image-link)
                    :to-equal "\\includegraphics{image.png}")
            (expect
             (org-kit::export-latex-render-view
              state (export-latex-test-view
                     document :link :properties '(:type "file" :path "/etc/passwd.png")
                     :children (list label)))
             :to-equal "\\texttt{label}")
            (expect
             (org-kit::export-latex-render-view
              state (export-latex-test-view
                     document :link :properties '(:type "file" :path "../../secret.png")
                     :children (list label)))
             :to-equal "\\texttt{label}")
            (expect
             (org-kit::export-latex-render-view
              state (export-latex-test-view
                     document :link :properties '(:type "file" :path "assets/../secret.png")
                     :children (list label)))
             :to-equal "\\texttt{label}")
            (expect
             (org-kit::export-latex-render-view
              state (export-latex-test-view
                     document :link :properties '(:type "file" :path "assets/figure_1.png")
                     :children (list label)))
             :to-equal "\\includegraphics{assets/figure_1.png}"))
          (expect (org-kit::export-latex-render-view state untyped-link)
                  :to-equal "\\texttt{label}")
          (expect (org-kit::export-latex-render-object state no-path-link 0)
                  :to-equal "")
          (expect (org-kit::export-latex-internal-link-id state nil)
                  :to-be nil)
          (expect (org-kit::export-latex-internal-link-id state "")
                  :to-be nil)
          (expect (org-kit::export-latex-image-path-p nil)
                  :to-be nil)
          (expect (org-kit::export-latex-link-source no-path-link) :to-equal "")
          (expect (org-kit::export-latex-link-source external-link)
                  :to-equal "https:example.test")
          (expect (org-kit::export-latex-link-source fuzzy-link)
                  :to-equal "anchor")
          (expect (org-kit::export-latex-link-source raw-link)
                  :to-equal "https://example.test")
          (expect (org-kit::export-latex-render-object state utf8-entity 0)
                  :to-equal "U")
          (expect (org-kit::export-latex-render-object state unicode-entity 0)
                  :to-equal "N")
          (expect (org-kit::export-latex-render-object state empty-entity 0)
                  :to-equal "")
          (expect (org-kit::export-latex-render-object state plain-code 0)
                  :to-equal "\\texttt{plain}")
          (expect (org-kit::export-latex-render-object state unclosed-code 0)
                  :to-equal "\\texttt{\\textasciitilde{}open}")
          (expect (org-kit::export-latex-render-object state empty-code 0)
                  :to-equal "\\texttt{}")
          (expect (org-kit::export-latex-render-object state inline-babel 0)
                  :to-equal "call")
          (expect (org-kit::export-latex-render-object state macro 0)
                  :to-equal "\\texttt{macro}")
          (expect (org-kit::export-latex-render-object state empty-footnote 0)
                  :to-equal "\\footnote{}")
          (expect (org-kit::export-latex-render-object
                   state (org-kit::export-view-node empty-footnote-view) 0
                   empty-footnote-view)
                  :to-equal "\\footnote{}")
          (expect (org-kit::export-latex-render-view state unknown-keyword)
                  :to-equal "")
          (expect (org-kit::export-latex-render-object state latex-snippet 0)
                  :to-equal "\\raw{}")
          (expect (org-kit::export-latex-render-object state foreign-snippet 0)
                  :to-equal "")
          (expect (org-kit::export-latex-render-view state empty-source-block)
                  :to-equal "")
          (expect (org-kit::export-latex-render-code state "plain" 0)
                  :to-equal (format nil "\\begin{verbatim}~%plain~%\\end{verbatim}"))
          (expect (org-kit::export-latex-render-code state "" 0)
                  :to-equal (format nil "\\begin{verbatim}~%~%\\end{verbatim}"))
          (expect (org-kit::export-latex-join state 0 (list nil "" "A" "B") ";")
                  :to-equal "A;B")
          (expect (org-kit::export-latex-reference-slug "A:B")
                  :to-equal "a:b")
          (expect (org-kit::export-latex-reference-slug "text value")
                  :to-equal "text-value")
          (expect (org-kit::export-latex-reference-slug "text- value")
                  :to-equal "text-value")
          (expect (org-kit::export-latex-reference-slug
                   (concatenate 'string "-" (string (code-char #xE9))))
                  :to-equal "")
          (expect (org-kit::export-latex-reference-slug nil) :to-equal "")
          (expect (search "\\section*{Undecorated}"
                          (export-org
                           (parse-org
                            (export-latex-test-lines
                             "#+OPTIONS: todo:nil tags:nil num:nil"
                             "* TODO [#A] Undecorated :hidden:"))
                           :latex))
                  :to-be-truthy)
          (expect (org-kit::export-latex-render-object source-state source-entity 0)
                  :to-equal "e")
          (expect (org-kit::export-latex-render-view
                   source-state source-example-view)
                  :to-equal
                  (format nil "\\begin{verbatim}~%x~%\\end{verbatim}"))))))

  (it "passes post-blank to element filters without changing block layout"
    (let* ((backend :fr106-latex-post-blank-test)
           (registered-p nil)
           (filtered-value nil)
           (document
             (parse-org
              (export-latex-test-lines
               "#+OPTIONS: toc:nil num:nil"
               ""
               "word"
               ""
               "")))
           (expected-output (export-org document :latex))
           (paragraph nil))
      (map-nodes
       (lambda (node)
         (when (eq (node-type node) :paragraph)
           (setf paragraph node)))
       (document-root document))
      (unwind-protect
           (progn
             (register-derived-export-backend
              backend :latex
              :filters
              (list
               (cons '(:element :paragraph)
                     (list (lambda (value context)
                             (declare (ignore context))
                             (setf filtered-value value)
                             value)))))
             (setf registered-p t)
             (let* ((post-blank (node-post-blank paragraph))
                    (expected-filter-value
                      (concatenate 'string "word"
                                   (make-string (1+ post-blank)
                                                :initial-element #\Newline)))
                    (output (export-org document backend)))
               (with-soft-assertions
                 (expect (plusp post-blank) :to-be t)
                 (expect filtered-value :to-equal expected-filter-value)
                 (expect output :to-equal expected-output))))
        (when registered-p
          (unregister-export-backend backend)))))

  (it "dispatches derived LaTeX components, nodes, filters, options, and links"
    (let ((backend :fr106-latex-derived-test)
          (registered-p nil)
          (link-registered-p nil)
          (output nil)
          (*export-latex-test-link-observation* nil))
      (unwind-protect
           (progn
             (register-derived-export-backend
              backend :latex
              :renderer #'export-latex-test-derived-renderer
              :preamble-renderer #'export-latex-test-derived-preamble
              :trailer-renderer #'export-latex-test-derived-trailer
              :template #'export-latex-test-derived-template
              :transcoders
              (list (cons :plain-text #'export-latex-test-derived-plain-text))
              :filters
              (list (cons :body (list #'export-latex-test-derived-body-filter)))
              :document-class "scrartcl")
             (setf registered-p t)
             (register-export-link-type
              backend "doi" #'export-latex-test-derived-link-handler)
             (setf link-registered-p t)
             (setf output
                   (export-org
                    (parse-org
                     (export-latex-test-lines
                      "#+OPTIONS: toc:nil num:nil"
                      "Plain # text."
                      ""
                      "[[doi:10.1234/example][Paper & data]]"
                      ""
                      "| [[doi:10.1234/table][Cell]] |"))
                    backend))
             (with-soft-assertions
               (expect (char output 0) :to-be #\<)
               (expect (char output (1- (length output))) :to-be #\>)
               (expect (search "P[\\documentclass[11pt]{scrartcl}" output)
                       :to-be-truthy)
               (expect (search "num=off;" output) :to-be-truthy)
               (expect (search (format nil "[Plain \\# text.~%]") output)
                       :to-be-truthy)
               (expect
                (search
                 "\\href{https://doi.org/10.1234/example}{Paper \\& data}"
                 output)
                :to-be-truthy)
               (expect
                (search "\\href{https://doi.org/10.1234/table}{Cell}" output)
                :to-be-truthy)
               (expect (search "|body-filter" output) :to-be-truthy)
               (expect (search "T[" output) :to-be-truthy)
               (expect *export-latex-test-link-observation*
                       :to-equal
                       '("10.1234/table" "Cell" :inline t
                         :fr106-latex-derived-test))))
        (when link-registered-p
          (unregister-export-link-type backend "doi"))
        (when registered-p
          (unregister-export-backend backend)))))
  (it "dispatches an unprepared title macro through its registered node policy"
    (let ((backend :fr106-latex-title-policy-test)
          (registered-p nil)
          (condition nil))
      (unwind-protect
           (progn
             (register-derived-export-backend
              backend :latex
              :node-policies
              (list
               (cons :macro
                     '(:unsupported :feature :title-macro-policy-test
                       :message "Title macros are disabled for this backend"))))
             (setf registered-p t)
             (setf condition
                   (export-latex-test-catch-error
                    (lambda ()
                      (export-org
                       (parse-org "* {{{unknown-title-macro}}}") backend))))
             (with-soft-assertions
               (expect (typep condition 'org-export-unsupported-syntax)
                       :to-be t)
               (when (typep condition 'org-export-unsupported-syntax)
                 (expect (org-export-error-backend condition) :to-be backend)
                 (expect (org-export-unsupported-syntax-feature condition)
                         :to-be :title-macro-policy-test))))
        (when registered-p
          (unregister-export-backend backend)))))

  (it "handles duplicate reference aliases and headers after default packages"
    (let* ((output
             (export-org
              (parse-org
               (export-latex-test-lines
                "* Same"
                ":PROPERTIES:"
                ":CUSTOM_ID: Same"
                ":END:"
                "* Same"))
              :latex))
           (header (format nil "\\usepackage{example}~%"))
           (document (make-document ""))
           (empty-header-node
             (make-node :keyword :properties
                        '(:key "LATEX_HEADER" :value "")))
           (header-node
             (make-node :keyword :properties
                        (list :key "LATEX_HEADER" :value header)))
           (root (make-node :org-data :children
                            (list empty-header-node header-node))))
      (setf (document-root document) root)
      (let* ((state (export-latex-test-state document :allow-raw-latex t))
             (preamble (org-kit::export-latex-preamble state)))
        (with-soft-assertions
          (expect (search "\\label{org:same}" output) :to-be-truthy)
          (expect (search "\\label{org:same-2}" output) :to-be-truthy)
          (expect
           (< (search "\\usepackage[utf8]{inputenc}" preamble)
              (search "\\usepackage{hyperref}" preamble)
              (search header preamble))
           :to-be t)
          (expect (search (concatenate 'string header (string #\Newline))
                          preamble)
                  :to-be-truthy)))))
  (it "verifies LaTeX link results and legacy render entry points"
    (let* ((document
             (parse-org
              (export-latex-test-lines
               "#+OPTIONS: toc:nil num:nil"
               "#+ATTR_LATEX: :width 0.5\\textwidth"
               "[[asset:https://cdn.example/image.png][Remote image]]"
               "[[asset:images/local.png][Local image]]"
               "[[asset:javascript:alert(1)][Fallback label]]"
               ""
               "Post blank"
               ""
               "")))
           (legacy-document (parse-org "Legacy body."))
           (legacy-state (export-latex-test-state legacy-document))
           (legacy-view
             (export-latex-test-view
              legacy-document :inline-src-block
              :properties (list :value "invalid")))
           (invalid-arguments
             (export-latex-test-catch-error
              (lambda ()
                (org-kit::export-latex-render-view
                 legacy-state legacy-view :unknown-option t))))
           (rendered-document
             (org-kit::export-latex-render-document legacy-state))
           (backend :latex-renderer-compatibility-test)
           (registered-p nil)
           (link-registered-p nil)
           (inline-result nil)
           (fallback-result nil)
           (output nil)
           (context-error nil))
      (unwind-protect
           (progn
             (register-derived-export-backend
              backend :latex
              :renderer
              (lambda (context)
                (let* ((latex-context (org-kit::export-context-for-owner context :latex))
                       (state (org-kit::export-latex-set-render-context latex-context))
                       (inline-view
                         (export-latex-test-view
                          document :inline-src-block
                          :properties (list :value "inline")
                          :post-blank 2))
                       (node
                         (make-node :inline-src-block
                                    :properties (list :value "fallback"))))
                  (progn
                    (setf inline-result
                          (org-kit::export-latex-render-view
                           state inline-view 0 :inline)
                          fallback-result
                          (export-render-node latex-context node))
                    (setf context-error
                          (export-latex-test-catch-error
                           (lambda ()
                             (org-kit::export-latex-render-object
                              state
                              (make-node :footnote-reference
                                         :properties (list :label "missing"))
                              0)))))
                  (org-kit::export-latex-render-body latex-context)))
              :template
              (lambda (context preamble body trailer)
                (declare (ignore context preamble trailer))
                body))
             (setf registered-p t)
             (register-export-link-type
              backend "asset"
              (lambda (path description frame context)
                (declare (ignore frame context))
                (make-export-link-result
                 :kind :image :destination path :description description)))
             (setf link-registered-p t
                   output (export-org document backend))
             (with-soft-assertions
               (expect (search "\\includegraphics{https://cdn.example/image.png}"
                               output)
                       :to-be-truthy)
               (expect (search "\\includegraphics{images/local.png}" output)
                       :to-be-truthy)
               (expect (search "Fallback label" output) :to-be-truthy)
               (expect (search "javascript:alert(1)" output) :to-be nil)
               (expect (search "% ATTR_LATEX: :width 0.5" output)
                       :to-be-truthy)
               (expect inline-result
                       :to-equal "\\texttt{inline}")
               (progn
                 (expect fallback-result :to-equal "\\texttt{fallback}")
                 (expect (typep context-error 'org-export-error) :to-be t)
                 (when (typep context-error 'org-export-error)
                   (expect (org-export-error-backend context-error) :to-be backend)
                   (expect (org-export-error-reason context-error)
                           :to-be :unresolved-footnote)))
               (expect (typep invalid-arguments 'simple-error) :to-be t)
               (expect (search "Legacy body." rendered-document) :to-be-truthy)
               (expect (search "\\end{document}" rendered-document)
                       :to-be-truthy)))
        (when link-registered-p
          (unregister-export-link-type backend "asset"))
        (when registered-p
          (unregister-export-backend backend)))))
  (it "falls back for unsafe registered LaTeX link results"
    (let ((backend :latex-test-unsafe-link-result)
          (backend-registered-p nil)
          (handler-registered-p nil)
          (output nil))
      (unwind-protect
          (progn
            (register-derived-export-backend backend :latex)
            (setf backend-registered-p t)
            (register-export-link-type
             backend "unsafe-link"
             (lambda (path description frame context)
               (declare (ignore path frame context))
               (make-export-link-result
                :kind :link
                :destination "javascript:alert(1)"
                :description description)))
            (setf handler-registered-p t
                  output
                  (export-org
                   (parse-org "[[unsafe-link:target][Blocked label]]")
                   backend))
            (with-soft-assertions
              (expect
               (and (search "Blocked label" output)
                    (not (search "\\href{" output))
                    (not (search "javascript:alert(1)" output)))
                  :to-be t)))
        (when handler-registered-p
          (unregister-export-link-type backend "unsafe-link"))
        (when backend-registered-p
          (unregister-export-backend backend)))))
  (it "rejects LaTeX render options without values"
    (let* ((document (parse-org "Body"))
           (state (export-latex-test-state document))
           (view
             (export-latex-test-view
              document :inline-src-block :properties (list :value "sample")))
           (condition
             (handler-case
                 (progn
                   (org-kit::export-latex-render-view state view :table-cell-p)
                   nil)
               (simple-error (caught) caught))))
      (with-soft-assertions
        (expect (not (null condition)) :to-be t)
        (when condition
          (expect (simple-condition-format-control condition)
                  :to-equal "Invalid LaTeX render arguments")))))
  (it "preserves spacing owned by a registered LaTeX transcoder"
    (let* ((document (parse-org "Before /styled/ after."))
           (inline-node nil)
           (backend :latex-test-inline-spacing)
           (registered-p nil)
           (output nil))
      (map-nodes
       (lambda (node)
         (when (eq (node-type node) :italic)
           (setf inline-node node)))
       (document-root document))
      (when inline-node
        (setf (org-kit::%node-post-blank inline-node) 2))
      (unwind-protect
          (progn
            (register-derived-export-backend
             backend :latex
             :transcoders
             (list
              (cons :italic
                    (lambda (node view frame context)
                      (declare (ignore node view frame context))
                      "OVERRIDE  "))))
            (setf registered-p t
                  output (export-org document backend))
            (with-soft-assertions
              (expect inline-node :not :to-be nil)
              (when inline-node
                (expect (node-post-blank inline-node) :to-be 2)
                (expect (search "OVERRIDE    after." output)
                        :not :to-be nil)
                (expect (search "OVERRIDE after." output) :to-be nil))))
        (when registered-p
          (unregister-export-backend backend)))))
  (it "uses the context-aware renderer when built-in LaTeX has a final-output filter"
    (let* ((registry org-kit::*export-backend-registry*)
           (builtin-handlers org-kit::*export-builtin-link-type-handlers*)
           (old-descriptor nil)
           (old-builtin-types nil)
           (had-descriptor-p nil)
           (had-builtin-types-p nil)
           (filter-context nil)
           (output nil))
      (sb-thread:with-mutex (org-kit::*export-backend-registry-lock*)
        (multiple-value-setq (old-descriptor had-descriptor-p)
          (gethash :latex registry))
        (multiple-value-setq (old-builtin-types had-builtin-types-p)
          (gethash :latex builtin-handlers)))
      (unwind-protect
           (progn
             (org-kit::%register-builtin-export-backend
              :latex
              :renderer #'org-kit::export-latex-render-body
              :preamble-renderer #'org-kit::export-latex-render-preamble
              :trailer-renderer #'org-kit::export-latex-render-trailer
              :make-state #'org-kit::export-latex-make-state
              :document-class "article"
              :transcoders (org-kit::export-latex-node-transcoders)
              :node-wrapper #'org-kit::export-latex-node-wrapper
              :accepted-keywords
              (list "LATEX_CLASS" "LATEX_CLASS_OPTIONS" "LATEX_HEADER"
                    "LATEX_HEADER_EXTRA" "LATEX" "ATTR_LATEX")
              :retained-keywords (list "LATEX" "ATTR_LATEX")
              :raw-output-options
              (list (cons :latex :allow-raw-latex))
              :export-block-names (list "latex" "tex")
              :export-snippet-names (list "latex")
              :filters
              (append
               (org-kit::%export-backend-descriptor-filters old-descriptor)
               (list
                (cons :final-output
                      (list
                       (lambda (text context)
                         (setf filter-context context)
                         (format nil "~A|final-output-filter" text)))))))
             (setf output (export-org (parse-org "Filtered body.") :latex))
             (with-soft-assertions
               (expect (search "Filtered body." output) :to-be-truthy)
               (expect
                (string=
                 (subseq output
                         (- (length output)
                            (length "|final-output-filter")))
                 "|final-output-filter")
                :to-be t)
               (expect
                (and filter-context
                     (eq (export-render-context-backend filter-context) :latex)
                     (export-render-context-state filter-context))
                :to-be-truthy)))
        (sb-thread:with-mutex (org-kit::*export-backend-registry-lock*)
          (if had-descriptor-p
              (setf (gethash :latex registry) old-descriptor)
              (remhash :latex registry))
          (if had-builtin-types-p
              (setf (gethash :latex builtin-handlers) old-builtin-types)
              (remhash :latex builtin-handlers))))
      (with-soft-assertions
        (expect (eq (org-kit::export-registry-descriptor :latex)
                    old-descriptor)
                :to-be t)
        (multiple-value-bind (restored-builtin-types present-p)
            (gethash :latex builtin-handlers)
          (expect (eq present-p had-builtin-types-p) :to-be t)
          (when had-builtin-types-p
            (expect (eq restored-builtin-types old-builtin-types)
                    :to-be t)))))))

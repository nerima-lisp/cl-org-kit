;;;; t/bibtex-test.lisp
(in-package #:cl-org-kit/test)

(defun bibtex-test-headlines (document)
  (let ((headlines nil))
    (map-nodes (lambda (node)
                 (when (eq (node-type node) :headline)
                   (push node headlines)))
               (document-root document))
    (nreverse headlines)))

(defun bibtex-test-property (headline key)
  (let* ((section (find :section (node-children headline) :key #'node-type))
         (drawer (and section
                      (find :property-drawer (node-children section)
                            :key #'node-type))))
    (when drawer
      (loop for property in (node-children drawer)
            when (and (eq (node-type property) :node-property)
                      (string-equal key (node-property property :key)))
              return (node-property property :value)))))

(defun bibtex-test-condition (thunk)
  (handler-case
      (progn (funcall thunk) nil)
    (error (condition) condition)))

(defun bibtex-test-expect-error (condition reason)
  (expect (typep condition 'org-bibtex-error) :to-be t)
  (when (typep condition 'org-bibtex-error)
    (expect (org-bibtex-error-reason condition) :to-be reason)))

(describe "BibTeX import and export"
  (it "imports and roundtrips an article against the Org 9.8.10 oracle"
    (let* ((input "@Article{key42, author={Ada   Lovelace}, title={Analytical Engine}, journal={Notes}, year={1843}}")
           (document (parse-org ""))
           (result (bibtex-import document input))
           (updated (edit-result-updated-document result))
           (heading (first (bibtex-test-headlines updated)))
           (output (bibtex-export updated heading)))
      (with-soft-assertions
        (expect (plusp (length input)) :to-be t)
        (expect (edit-result-p result) :to-be t)
        (expect (document-text document) :to-equal "")
        (expect (document-text updated)
                :to-equal
                (format nil "* Analytical Engine~%:PROPERTIES:~%:TITLE:    Analytical Engine~%:BTYPE:    article~%:CUSTOM_ID: key42~%:AUTHOR:   Ada Lovelace~%:JOURNAL:  Notes~%:YEAR:     1843~%:END:~%"))
        (expect output
                :to-equal
                (format nil "@article{key42,~%  author={Ada Lovelace},~%  title={Analytical Engine},~%  journal={Notes},~%  year={1843}~%}~%")))))

  (it "exports an article with required and optional Org field ordering"
    (let* ((source
             (format nil "* Analytical Engine~%:PROPERTIES:~%:TITLE: Analytical Engine~%:BTYPE: article~%:CUSTOM_ID: key42~%:AUTHOR: Ada Lovelace~%:JOURNAL: Notes~%:YEAR: 1843~%:END:~%"))
           (document (parse-org source))
           (heading (first (bibtex-test-headlines document)))
           (output (bibtex-export document heading)))
      (with-soft-assertions
        (expect (plusp (length source)) :to-be t)
        (expect output
                :to-equal
                (format nil "@article{key42,~%  author={Ada Lovelace},~%  title={Analytical Engine},~%  journal={Notes},~%  year={1843}~%}~%")))))
  (it "matches Org BibTeX whitespace normalization"
    (let* ((edges-input
             (format nil "@misc{edges, title={~CTitle~C}}" #\Tab #\Return))
           (edges-document (parse-org ""))
           (edges-updated
             (edit-result-updated-document
              (bibtex-import edges-document edges-input)))
           (edges-heading (first (bibtex-test-headlines edges-updated)))
           (page-input
             (format nil "@misc{page, title={~CTitle~C}, note={~Cnote~C}}"
                     #\Page #\Page #\VT #\VT))
           (page-document (parse-org ""))
           (page-updated
             (edit-result-updated-document
              (bibtex-import page-document page-input)))
           (page-heading (first (bibtex-test-headlines page-updated))))
      (with-soft-assertions
        (expect (plusp (length edges-input)) :to-be t)
        (expect (document-text edges-updated)
                :to-equal
                (format nil "*  Title ~%:PROPERTIES:~%:TITLE:    Title~%:BTYPE:    misc~%:CUSTOM_ID: edges~%:END:~%"))
        (expect (bibtex-test-property edges-heading "TITLE") :to-equal "Title")
        (expect (bibtex-export edges-updated edges-heading)
                :to-equal
                (format nil "@misc{edges,~%  title={Title}~%}~%"))
        (expect (plusp (length page-input)) :to-be t)
        (expect (document-text page-updated)
                :to-equal
                (format nil "* ~CTitle~C~%:PROPERTIES:~%:TITLE:    ~CTitle~C~%:BTYPE:    misc~%:CUSTOM_ID: page~%:NOTE:     ~Cnote~C~%:END:~%"
                        #\Page #\Page #\Page #\Page #\VT #\VT))
        (expect (bibtex-test-property page-heading "TITLE")
                :to-equal
                (format nil "~CTitle~C" #\Page #\Page))
        (expect (bibtex-test-property page-heading "NOTE")
                :to-equal
                (format nil "~Cnote~C" #\VT #\VT))
        (expect (bibtex-export page-updated page-heading)
                :to-equal
                (format nil "@misc{page,~%  title={~CTitle~C},~%  note={~Cnote~C}~%}~%"
                        #\Page #\Page #\VT #\VT)))))

  (it "parses parenthesized entries and quoted or bare values"
    (let* ((input
             (format nil "@Article(key, author=\"Ada   Lovelace\", title=\"Quoted~%Title\", journal=Notes, year=1843)"))
           (document (parse-org ""))
           (updated
             (edit-result-updated-document (bibtex-import document input))))
      (with-soft-assertions
        (expect (plusp (length input)) :to-be t)
        (expect (document-text updated)
                :to-equal
                (format nil "* Quoted Title~%:PROPERTIES:~%:TITLE:    Quoted Title~%:BTYPE:    article~%:CUSTOM_ID: key~%:AUTHOR:   Ada Lovelace~%:JOURNAL:  Notes~%:YEAR:     1843~%:END:~%")))))

  (it "imports multiple entries in their source order"
    (let* ((input
             "@article{first,author={A},title={First},journal={J},year={1}} @article{second,author={B},title={Second},journal={K},year={2}}")
           (document (parse-org ""))
           (result (bibtex-import document input))
           (updated (edit-result-updated-document result)))
      (with-soft-assertions
        (expect (plusp (length input)) :to-be t)
        (expect (mapcar (lambda (headline) (node-property headline :raw-value))
                        (bibtex-test-headlines updated))
                :to-equal '("First" "Second"))
        (expect (search "CUSTOM_ID: first" (document-text updated))
                :to-be-truthy)
        (expect (< (search "CUSTOM_ID: first" (document-text updated))
                   (search "CUSTOM_ID: second" (document-text updated)))
                :to-be t))))

  (it "applies the property prefix while leaving CUSTOM_ID unchanged"
    (let* ((input
             "@article{prefixed,author={Ada},title={A Title},journal={J},year={1843}}")
           (document (parse-org ""))
           (updated
             (edit-result-updated-document
              (bibtex-import document input :prefix "bib-")))
           (heading (first (bibtex-test-headlines updated))))
      (with-soft-assertions
        (expect (plusp (length input)) :to-be t)
        (expect (bibtex-test-property heading "BIB-TITLE") :to-equal "A Title")
        (expect (bibtex-test-property heading "BIB-BTYPE") :to-equal "article")
        (expect (bibtex-test-property heading "CUSTOM_ID") :to-equal "prefixed")
        (expect (bibtex-test-property heading "BIB-AUTHOR") :to-equal "Ada")
        (expect (bibtex-export updated heading :prefix "bib-")
                :to-equal
                (format nil "@article{prefixed,~%  author={Ada},~%  title={A Title},~%  journal={J},~%  year={1843}~%}~%")))))

  (it "keeps imported field order and exports only known fields in type order"
    (let* ((input
             "@article{ordered, doi={10/x}, year={1843}, arbitrary={kept}, title={Order}, number={2}, month={jan}, author={Ada}, pages={1-2}, journal={J}, volume={3}}")
           (document (parse-org ""))
           (updated
             (edit-result-updated-document (bibtex-import document input)))
           (heading (first (bibtex-test-headlines updated))))
      (with-soft-assertions
        (expect (plusp (length input)) :to-be t)
        (expect (mapcar (lambda (property)
                          (cons (node-property property :key)
                                (node-property property :value)))
                        (node-children
                         (find :property-drawer
                               (node-children
                                (find :section (node-children heading)
                                      :key #'node-type))
                               :key #'node-type)))
                :to-equal
                '(("TITLE" . "Order") ("BTYPE" . "article")
                  ("CUSTOM_ID" . "ordered") ("DOI" . "10/x")
                  ("YEAR" . "1843") ("ARBITRARY" . "kept")
                  ("NUMBER" . "2") ("MONTH" . "jan")
                  ("AUTHOR" . "Ada") ("PAGES" . "1-2")
                  ("JOURNAL" . "J") ("VOLUME" . "3")))
        (expect (bibtex-export updated heading)
                :to-equal
                (format nil "@article{ordered,~%  author={Ada},~%  title={Order},~%  journal={J},~%  year={1843},~%  volume={3},~%  number={2},~%  pages={1-2},~%  month={jan},~%  doi={10/x}~%}~%")))))

  (it "exports every present field in BibTeX alternative groups in declared order"
    (let* ((book-source
             (format nil "* Book~%:PROPERTIES:~%:BTYPE: book~%:CUSTOM_ID: book-key~%:EDITOR: Ed~%:AUTHOR: Author~%:TITLE: Book title~%:PUBLISHER: Press~%:YEAR: 2000~%:VOLUME: 2~%:NUMBER: 3~%:SERIES: Set~%:END:~%"))
           (book-document (parse-org book-source))
           (book (first (bibtex-test-headlines book-document)))
           (inbook-source
             (format nil "* Chapter~%:PROPERTIES:~%:BTYPE: inbook~%:CUSTOM_ID: chapter-key~%:AUTHOR: Author~%:EDITOR: Ed~%:TITLE: Chapter title~%:CHAPTER: 4~%:PAGES: 20-30~%:PUBLISHER: Press~%:YEAR: 2000~%:END:~%"))
           (inbook-document (parse-org inbook-source))
           (inbook (first (bibtex-test-headlines inbook-document))))
      (with-soft-assertions
        (expect (plusp (length book-source)) :to-be t)
        (expect (bibtex-export book-document book)
                :to-equal
                (format nil "@book{book-key,~%  editor={Ed},~%  author={Author},~%  title={Book title},~%  publisher={Press},~%  year={2000},~%  volume={2},~%  number={3},~%  series={Set}~%}~%"))
        (expect (plusp (length inbook-source)) :to-be t)
        (expect (bibtex-export inbook-document inbook)
                :to-equal
                (format nil "@inbook{chapter-key,~%  author={Author},~%  editor={Ed},~%  title={Chapter title},~%  chapter={4},~%  pages={20-30},~%  publisher={Press},~%  year={2000}~%}~%")))))

  (it "matches Org title fallback and empty-value export behavior"
    (let* ((source
             (format nil "* Headline title~%:PROPERTIES:~%:BTYPE: article~%:CUSTOM_ID: fallback~%:AUTHOR: Ada~%:JOURNAL: Notes~%:YEAR: 1843~%:END:~%"))
           (document (parse-org source))
           (heading (first (bibtex-test-headlines document)))
           (empty-title-source
             (format nil "* Headline title~%:PROPERTIES:~%:TITLE:~%:BTYPE: article~%:CUSTOM_ID: blank~%:AUTHOR:~%:END:~%"))
           (empty-title-document (parse-org empty-title-source))
           (empty-title-heading
             (first (bibtex-test-headlines empty-title-document)))
           (misc-source
             (format nil "* ~%:PROPERTIES:~%:BTYPE: misc~%:CUSTOM_ID: empty~%:END:~%"))
           (misc-document (parse-org misc-source))
           (misc-heading (first (bibtex-test-headlines misc-document))))
      (with-soft-assertions
        (expect (plusp (length source)) :to-be t)
        (expect (bibtex-test-property heading "TITLE") :to-be nil)
        (expect (bibtex-export document heading)
                :to-equal
                (format nil "@article{fallback,~%  author={Ada},~%  title={Headline title},~%  journal={Notes},~%  year={1843}~%}~%"))
        (expect (plusp (length empty-title-source)) :to-be t)
        (expect (bibtex-export empty-title-document empty-title-heading)
                :to-equal
                (format nil "@article{blank,~%  author={},~%  title={}~%}~%"))
        (expect (plusp (length misc-source)) :to-be t)
        (expect (bibtex-export misc-document misc-heading)
                :to-equal
                (format nil "@misc{empty,~%  title={}~%}~%")))))

  (it "converts incomplete entries with the fields present"
    (let* ((input "@article{missing, author={Ada}, title={No journal or year}}")
           (document (parse-org ""))
           (updated
             (edit-result-updated-document (bibtex-import document input)))
           (heading (first (bibtex-test-headlines updated)))
           (output (bibtex-export updated heading)))
      (with-soft-assertions
        (expect (plusp (length input)) :to-be t)
        (expect (document-text updated)
                :to-equal
                (format nil "* No journal or year~%:PROPERTIES:~%:TITLE:    No journal or year~%:BTYPE:    article~%:CUSTOM_ID: missing~%:AUTHOR:   Ada~%:END:~%"))
        (expect output
                :to-equal
                (format nil "@article{missing,~%  author={Ada},~%  title={No journal or year}~%}~%")))))
  (it "imports and exports an empty title like Org"
    (let* ((input "@article{empty, title={}}")
           (document (parse-org ""))
           (updated
             (edit-result-updated-document (bibtex-import document input)))
           (heading (first (bibtex-test-headlines updated)))
           (output (bibtex-export updated heading)))
      (with-soft-assertions
        (expect (plusp (length input)) :to-be t)
        (expect (document-text updated)
                :to-equal
                (format nil "* ~%:PROPERTIES:~%:TITLE:~%:BTYPE:    article~%:CUSTOM_ID: empty~%:END:~%"))
        (expect output
                :to-equal
                (format nil "@article{empty,~%  title={}~%}~%")))))

  (it "reports parser and Org validation errors with bounded context"
    (let ((cases
            (list
             (list "@{key, author={A}}" :missing-type)
             (list "@" :missing-type)
             (list "@article{, author={A}}" :missing-key)
             (list "@unknown{key, title={T}}" :unknown-type)
             (list "@article[key, author={A}]" :malformed-delimiter)
             (list "@article" :malformed-delimiter)
             (list "@misc{open-key" :unclosed-entry)
             (list "@misc{bad key, title={T}}" :malformed-key)
             (list "@misc{comma-eof," :unclosed-entry)
             (list "@misc{after-value, title={T}," :unclosed-entry)
             (list "@misc{bare-eof, title=bare" :unclosed-entry)
             (list "@misc{missing-field, =x}" :missing-field)
             (list "@misc{k, title}" :malformed-field)
             (list "@misc{k, title" :malformed-field)
             (list "@misc{duplicate, title={T}, title={U}}" :duplicate-field)
             (list "@misc{unclosed-value, title={T" :unclosed-value)
             (list "@misc(k, title=foo})" :malformed-delimiter)
             (list "" :empty-input)
             (list "% comment only" :empty-input)
             (list "not BibTeX" :unexpected-token))))
      (with-soft-assertions
        (dolist (case cases)
          (bibtex-test-expect-error
           (bibtex-test-condition
            (lambda () (bibtex-import (parse-org "") (first case))))
           (second case)))
        (let* ((input "@misc{empty-fields,}")
               (condition
                 (bibtex-test-condition
                  (lambda () (bibtex-import (parse-org "") input)))))
          (bibtex-test-expect-error condition :missing-title))
        (let* ((input "@misc{empty}")
               (condition
                 (bibtex-test-condition
                  (lambda () (bibtex-import (parse-org "") input)))))
          (bibtex-test-expect-error condition :missing-title))
        (let* ((input "@misc{untitled, author={Ada}}")
               (condition
                 (bibtex-test-condition
                  (lambda () (bibtex-import (parse-org "") input)))))
          (bibtex-test-expect-error condition :missing-title)
          (when (typep condition 'org-bibtex-error)
            (let ((report (princ-to-string condition)))
              (expect (org-bibtex-error-position condition) :to-be 0)
              (expect (org-bibtex-error-context condition) :to-equal input)
              (expect report
                      :to-equal
                      "BibTeX entry is missing its title (missing-title) at position 0 near \"@misc{untitled, author={Ada}}\""))))
        (let* ((input
                 (concatenate 'string "% leading" (string #\Newline)
                              "@misc{comments, title={T}}% trailing"))
               (updated
                 (edit-result-updated-document
                  (bibtex-import (parse-org "") input)))
               (heading (first (bibtex-test-headlines updated))))
          (expect (bibtex-test-property heading "TITLE") :to-equal "T"))
        (let* ((document (parse-org (format nil "* No properties~%")))
               (heading (first (bibtex-test-headlines document)))
               (condition
                 (bibtex-test-condition
                  (lambda () (bibtex-export document heading)))))
          (bibtex-test-expect-error condition :missing-type))
        (let* ((document
                 (parse-org
                  (format nil "* No key~%:PROPERTIES:~%:BTYPE: article~%:END:~%")))
               (heading (first (bibtex-test-headlines document)))
               (condition
                 (bibtex-test-condition
                  (lambda () (bibtex-export document heading)))))
          (bibtex-test-expect-error condition :missing-key)))))

  (it "checks the document input limit before parsing BibTeX"
    (let* ((document
             (parse-org "" :limits (make-limits :max-input-length 8)))
           (input "this input is too long and malformed")
           (condition
             (handler-case
                 (progn (bibtex-import document input) nil)
               (org-resource-limit-error (condition) condition))))
      (with-soft-assertions
        (expect (plusp (length input)) :to-be t)
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when (typep condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name condition)
                  :to-be :max-input-length)
          (expect (org-resource-limit-error-actual condition)
                  :to-be (length input))))))

  (it "appends one edit span and preserves all original document text"
    (let* ((source (format nil "#+TITLE: Existing~%~%* Existing~%Body"))
           (input "@article{added, author={Ada}, title={Added}, journal={Notes}, year={1843}}")
           (document (parse-org source))
           (result (bibtex-import document input))
           (updated (edit-result-updated-document result))
           (span (first (edit-result-spans result))))
      (with-soft-assertions
        (expect (plusp (length input)) :to-be t)
        (expect (document-text document) :to-equal source)
        (expect (subseq (document-text updated) 0 (length source))
                :to-equal source)
        (expect (length (edit-result-spans result)) :to-be 1)
        (expect (list (edit-span-start span) (edit-span-end span))
                :to-equal (list (length source) (length source)))
        (expect (document-text updated)
                :to-equal
                (concatenate
                 'string source
                 (format nil "~%* Added~%:PROPERTIES:~%:TITLE:    Added~%:BTYPE:    article~%:CUSTOM_ID: added~%:AUTHOR:   Ada~%:JOURNAL:  Notes~%:YEAR:     1843~%:END:~%"))))))
  (it "appends imports without a separator to a newline-terminated document"
    (let* ((source (format nil "* Existing~%"))
           (input "@misc{added, title={Added}}")
           (document (parse-org source))
           (updated
             (edit-result-updated-document (bibtex-import document input))))
      (with-soft-assertions
        (expect (document-text document) :to-equal source)
        (expect (document-text updated)
                :to-equal
                (concatenate
                 'string source
                 (format nil
                         "* Added~%:PROPERTIES:~%:TITLE:    Added~%:BTYPE:    misc~%:CUSTOM_ID: added~%:END:~%"))))))
  (it "skips generated-property alignment for a NIL property value"
    (let* ((document
             (parse-org
              (format nil
                      "* Has value~%:PROPERTIES:~%:NOTE: value~%:END:~%")))
           (heading (first (bibtex-test-headlines document)))
           (section (find :section (node-children heading) :key #'node-type))
           (drawer
             (find :property-drawer (node-children section) :key #'node-type))
           (property
             (find :node-property (node-children drawer) :key #'node-type)))
      (setf (node-property property :value) nil)
      (expect (eq (org-kit::bibtex-align-generated-properties
                   document heading)
                  document)
              :to-be t)))
  (it "leaves property alignment unchanged when there is no drawer"
    (let* ((document (parse-org (format nil "* Plain~%")))
           (heading (first (bibtex-test-headlines document))))
      (expect (eq (org-kit::bibtex-align-generated-properties
                   document heading)
                  document)
              :to-be t)))
  (it "reports unknown export types with type context"
    (let* ((document
             (parse-org
              (format nil
                      "* Unsupported~%:PROPERTIES:~%:BTYPE: future~%:CUSTOM_ID: key~%:END:~%")))
           (heading (first (bibtex-test-headlines document)))
           (condition
             (bibtex-test-condition
              (lambda () (bibtex-export document heading)))))
      (with-soft-assertions
        (bibtex-test-expect-error condition :unknown-type)
        (when (typep condition 'org-bibtex-error)
          (let ((report (princ-to-string condition)))
            (expect (org-bibtex-error-context condition) :to-equal "future")
            (expect (search "Unknown BibTeX entry type" report)
                    :to-be-truthy)
            (expect (search "unknown-type" report) :to-be-truthy))))))
  (it "validates BibTeX document, input, and prefix arguments"
    (let ((input "@misc{prefix, title={T}}"))
      (with-soft-assertions
        (let ((condition
                (bibtex-test-condition
                 (lambda () (bibtex-import nil input)))))
          (expect (typep condition 'type-error) :to-be t))
        (let ((condition
                (bibtex-test-condition
                 (lambda () (bibtex-export nil nil)))))
          (expect (typep condition 'type-error) :to-be t))
        (let ((condition
                (bibtex-test-condition
                 (lambda () (bibtex-import (make-document "") input)))))
          (bibtex-test-expect-error condition :invalid-document))
        (let ((condition
                (bibtex-test-condition
                 (lambda () (bibtex-export (make-document "") nil)))))
          (bibtex-test-expect-error condition :invalid-document))
        (let ((condition
                (bibtex-test-condition
                 (lambda () (bibtex-import (parse-org "") nil)))))
          (bibtex-test-expect-error condition :invalid-input))
        (dolist (prefix
                 (list 7 ":" (format nil "bad~%prefix")
                       (format nil "bad~Cprefix" #\Return)))
          (let ((condition
                  (bibtex-test-condition
                   (lambda ()
                     (bibtex-import (parse-org "") input :prefix prefix)))))
            (bibtex-test-expect-error condition :invalid-prefix))))))
  (it "normalizes escaped, nested, and concatenated BibTeX values like Org"
    (let* ((input "@misc{values, title=\"A \\\"quoted\\\" title\", note={{Nested} text}, year=1, howpublished={Wrapped} # {tail}, url=\"Quoted\" # \" tail\"}")
           (expected-title "A \\\"quoted\\\" title")
           (updated
             (edit-result-updated-document
              (bibtex-import (parse-org "") input)))
           (heading (first (bibtex-test-headlines updated)))
           (trailing-input "@misc{trailing, title={T},}")
           (trailing-updated
             (edit-result-updated-document
              (bibtex-import (parse-org "") trailing-input)))
           (trailing-heading
             (first (bibtex-test-headlines trailing-updated))))
      (with-soft-assertions
        (expect (bibtex-test-property heading "TITLE")
                :to-equal expected-title)
        (expect (bibtex-test-property heading "NOTE")
                :to-equal "{Nested} text")
        (expect (bibtex-test-property heading "YEAR") :to-equal "1")
        (expect (bibtex-test-property heading "HOWPUBLISHED")
                :to-equal "Wrapped} # {tail")
        (expect (bibtex-test-property heading "URL")
                :to-equal "Quoted\" # \" tail")
        (expect (bibtex-test-property trailing-heading "TITLE")
                :to-equal "T")))))

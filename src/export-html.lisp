;;;; src/export-html.lisp
(in-package #:org-kit)

(defstruct (export-html-state (:constructor %make-export-html-state))
  preparation
  limits
  resolution-index
  render-context
  render-frame
  (output-offset 0)
  trim-list-indent-p
  standalone-image-node
  standalone-image-attributes
  (output (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
  verse-output-p
  (explicit-references (make-hash-table :test #'eq))
  (references (make-hash-table :test #'eq))
  (wrapper-references (make-hash-table :test #'eq))
  (used-references (make-hash-table :test #'equalp))
  (reference-suffixes (make-hash-table :test #'equalp))
  (reference-targets (make-hash-table :test #'equalp))
  (footnote-definition-ids (make-hash-table :test #'equal))
  (footnote-reference-ids (make-hash-table :test #'eq))
  (footnote-first-reference-ids (make-hash-table :test #'equal))
  (footnote-reference-counts (make-hash-table :test #'equal))
  (footnote-numbers (make-hash-table :test #'equal))
  (heading-numbers (make-hash-table :test #'eq))
  (next-reference 0))

(defparameter +export-html-default-css+
  "  #content { max-width: 60em; margin: auto; }
  .title  { text-align: center;
             margin-bottom: .2em; }
  .subtitle { text-align: center;
              font-size: medium;
              font-weight: bold;
              margin-top:0; }
  .todo   { font-family: monospace; color: red; }
  .done   { font-family: monospace; color: green; }
  .priority { font-family: monospace; color: orange; }
  .tag    { background-color: #eee; font-family: monospace;
            padding: 2px; font-size: 80%; font-weight: normal; }
  .timestamp { color: #bebebe; }
  .timestamp-kwd { color: #5f9ea0; }
  .org-right  { margin-left: auto; margin-right: 0px;  text-align: right; }
  .org-left   { margin-left: 0px;  margin-right: auto; text-align: left; }
  .org-center { margin-left: auto; margin-right: auto; text-align: center; }
  .underline { text-decoration: underline; }
  #postamble p, #preamble p { font-size: 90%; margin: .2em; }
  p.verse { margin-left: 3%; }
  pre {
    border: 1px solid #e6e6e6;
    border-radius: 3px;
    background-color: #f2f2f2;
    padding: 8pt;
    font-family: monospace;
    overflow: auto;
    margin: 1.2em;
  }
  pre.src {
    position: relative;
    overflow: auto;
  }
  pre.src:before {
    display: none;
    position: absolute;
    top: -8px;
    right: 12px;
    padding: 3px;
    color: #555;
    background-color: #f2f2f299;
  }
  pre.src:hover:before { display: inline; margin-top: 14px;}
  /* Languages per Org manual */
  pre.src-asymptote:before { content: 'Asymptote'; }
  pre.src-awk:before { content: 'Awk'; }
  pre.src-authinfo::before { content: 'Authinfo'; }
  pre.src-c:before { content: 'C'; }
  pre.src-C:before { content: 'C'; }
  /* pre.src-C++ doesn't work in CSS */
  pre.src-clojure:before { content: 'Clojure'; }
  pre.src-css:before { content: 'CSS'; }
  pre.src-D:before { content: 'D'; }
  pre.src-ditaa:before { content: 'ditaa'; }
  pre.src-dot:before { content: 'Graphviz'; }
  pre.src-calc:before { content: 'Emacs Calc'; }
  pre.src-emacs-lisp:before { content: 'Emacs Lisp'; }
  pre.src-fortran:before { content: 'Fortran'; }
  pre.src-gnuplot:before { content: 'gnuplot'; }
  pre.src-haskell:before { content: 'Haskell'; }
  pre.src-hledger:before { content: 'hledger'; }
  pre.src-java:before { content: 'Java'; }
  pre.src-js:before { content: 'JavaScript'; }
  pre.src-latex:before { content: 'LaTeX'; }
  pre.src-ledger:before { content: 'Ledger'; }
  pre.src-lisp:before { content: 'Lisp'; }
  pre.src-lilypond:before { content: 'Lilypond'; }
  pre.src-lua:before { content: 'Lua'; }
  pre.src-matlab:before { content: 'MATLAB'; }
  pre.src-mscgen:before { content: 'Mscgen'; }
  pre.src-ocaml:before { content: 'Objective Caml'; }
  pre.src-octave:before { content: 'Octave'; }
  pre.src-org:before { content: 'Org mode'; }
  pre.src-oz:before { content: 'OZ'; }
  pre.src-plantuml:before { content: 'Plantuml'; }
  pre.src-processing:before { content: 'Processing.js'; }
  pre.src-python:before { content: 'Python'; }
  pre.src-R:before { content: 'R'; }
  pre.src-ruby:before { content: 'Ruby'; }
  pre.src-sass:before { content: 'Sass'; }
  pre.src-scheme:before { content: 'Scheme'; }
  pre.src-screen:before { content: 'Gnu Screen'; }
  pre.src-sed:before { content: 'Sed'; }
  pre.src-sh:before { content: 'shell'; }
  pre.src-sql:before { content: 'SQL'; }
  pre.src-sqlite:before { content: 'SQLite'; }
  /* additional languages in org.el's org-babel-load-languages alist */
  pre.src-forth:before { content: 'Forth'; }
  pre.src-io:before { content: 'IO'; }
  pre.src-J:before { content: 'J'; }
  pre.src-makefile:before { content: 'Makefile'; }
  pre.src-maxima:before { content: 'Maxima'; }
  pre.src-perl:before { content: 'Perl'; }
  pre.src-picolisp:before { content: 'Pico Lisp'; }
  pre.src-scala:before { content: 'Scala'; }
  pre.src-shell:before { content: 'Shell Script'; }
  pre.src-ebnf2ps:before { content: 'ebfn2ps'; }
  /* additional language identifiers per \"defun org-babel-execute\"
       in ob-*.el */
  pre.src-cpp:before  { content: 'C++'; }
  pre.src-abc:before  { content: 'ABC'; }
  pre.src-coq:before  { content: 'Coq'; }
  pre.src-groovy:before  { content: 'Groovy'; }
  /* additional language identifiers from org-babel-shell-names in
     ob-shell.el: ob-shell is the only babel language using a lambda to put
     the execution function name together. */
  pre.src-bash:before  { content: 'bash'; }
  pre.src-csh:before  { content: 'csh'; }
  pre.src-ash:before  { content: 'ash'; }
  pre.src-dash:before  { content: 'dash'; }
  pre.src-ksh:before  { content: 'ksh'; }
  pre.src-mksh:before  { content: 'mksh'; }
  pre.src-posh:before  { content: 'posh'; }
  /* Additional Emacs modes also supported by the LaTeX listings package */
  pre.src-ada:before { content: 'Ada'; }
  pre.src-asm:before { content: 'Assembler'; }
  pre.src-caml:before { content: 'Caml'; }
  pre.src-delphi:before { content: 'Delphi'; }
  pre.src-html:before { content: 'HTML'; }
  pre.src-idl:before { content: 'IDL'; }
  pre.src-mercury:before { content: 'Mercury'; }
  pre.src-metapost:before { content: 'MetaPost'; }
  pre.src-modula-2:before { content: 'Modula-2'; }
  pre.src-pascal:before { content: 'Pascal'; }
  pre.src-ps:before { content: 'PostScript'; }
  pre.src-prolog:before { content: 'Prolog'; }
  pre.src-simula:before { content: 'Simula'; }
  pre.src-tcl:before { content: 'tcl'; }
  pre.src-tex:before { content: 'TeX'; }
  pre.src-plain-tex:before { content: 'Plain TeX'; }
  pre.src-verilog:before { content: 'Verilog'; }
  pre.src-vhdl:before { content: 'VHDL'; }
  pre.src-xml:before { content: 'XML'; }
  pre.src-nxml:before { content: 'XML'; }
  /* add a generic configuration mode; LaTeX export needs an additional
     (add-to-list 'org-latex-listings-langs '(conf \" \")) in .emacs */
  pre.src-conf:before { content: 'Configuration File'; }

  table { border-collapse:collapse; }
  caption.t-above { caption-side: top; }
  caption.t-bottom { caption-side: bottom; }
  td, th { vertical-align:top;  }
  th.org-right  { text-align: center;  }
  th.org-left   { text-align: center;   }
  th.org-center { text-align: center; }
  td.org-right  { text-align: right;  }
  td.org-left   { text-align: left;   }
  td.org-center { text-align: center; }
  dt { font-weight: bold; }
  .footpara { display: inline; }
  .footdef  { margin-bottom: 1em; }
  .figure { padding: 1em; }
  .figure p { text-align: center; }
  .equation-container {
    display: table;
    text-align: center;
    width: 100%;
  }
  .equation {
    vertical-align: middle;
  }
  .equation-label {
    display: table-cell;
    text-align: right;
    vertical-align: middle;
  }
  .inlinetask {
    padding: 10px;
    border: 2px solid gray;
    margin: 10px;
    background: #ffffcc;
  }
  #org-div-home-and-up
   { text-align: right; font-size: 70%; white-space: nowrap; }
  textarea { overflow-x: auto; }
  .linenr { font-size: smaller }
  .code-highlighted { background-color: #ffff00; }
  .org-info-js_info-navigation { border-style: none; }
  #org-info-js_console-label
    { font-size: 10px; font-weight: bold; white-space: nowrap; }
  .org-info-js_search-highlight
    { background-color: #ffff00; color: #000000; font-weight: bold; }
  .org-svg { }
")

(defun export-html-check-output-length (state additional-length)
  (let* ((output (export-html-state-output state))
         (next-length (+ (export-html-state-output-offset state)
                         (fill-pointer output) additional-length)))
    (export-check-output-actual
     next-length (export-html-state-limits state) :html
     *export-current-export-phase* :kind :native-write)))

(defun export-html-escaped-output-length (context text start end)
  (loop for position from start below end
        for character = (char text position)
        sum (case character
              (#\& 5)
              ((#\< #\>) 4)
              (#\" (if (eq context :attribute) 6 1))
              (t 1))))

(defun export-html-check-escaped-parts (state context parts)
  (export-html-check-output-length
   state
   (loop for (text start end) in parts
         sum (export-html-escaped-output-length
              context text start (or end (length text))))))

(defun export-html-write-range (state text start end)
  (let* ((output (export-html-state-output state))
         (length (- end start)))
    (export-html-check-output-length state length)
    (loop for position from start below end
          do (vector-push-extend (char text position) output)))
  state)

(defun export-html-write (state text)
  (export-html-write-range state text 0 (length text)))

(defun export-html-append-checked-result (state text)
  (let ((output (export-html-state-output state)))
    (loop for character across text
          do (vector-push-extend character output)))
  state)

(defun export-html-write-spaces (state count)
  (let ((output (export-html-state-output state)))
    (export-html-check-output-length state count)
    (loop repeat count
          do (vector-push-extend #\Space output)))
  state)

(defun export-html-escape-to-output-range (state context text start end)
  (unless (or (eq context :text) (eq context :attribute))
    (export-error :html :invalid-escape-context nil
                  "Unknown HTML escaping context"))
  (when (= start end)
    (return-from export-html-escape-to-output-range state))
  (unless
      (loop for position from start below end
            for character = (char text position)
            thereis (or (char= character #\&)
                        (char= character #\<)
                        (char= character #\>)
                        (and (eq context :attribute)
                             (char= character #\"))))
    (return-from export-html-escape-to-output-range
      (export-html-write-range state text start end)))
  (let ((run-start start))
    (loop for position from start below end
          for character = (char text position)
          for replacement =
            (case context
              (:text (case character
                       (#\& "&amp;")
                       (#\< "&lt;")
                       (#\> "&gt;")))
              (:attribute (case character
                            (#\& "&amp;")
                            (#\< "&lt;")
                            (#\> "&gt;")
                            (#\" "&quot;"))))
          when replacement
            do (when (< run-start position)
                 (export-html-write-range state text run-start position))
               (export-html-write state replacement)
               (setf run-start (1+ position))
          finally (when (< run-start end)
                    (export-html-write-range state text run-start end)))
    state))

(defun export-html-escape-to-output (state context text)
  (export-html-escape-to-output-range state context text 0 (length text)))

(defun export-html-write-attribute (state name value)
  (export-html-write state " ")
  (export-html-write state name)
  (export-html-write state "=\"")
  (export-html-escape-to-output state :attribute value)
  (export-html-write state "\"")
  state)

(defun export-html-attribute-error (node message)
  (export-unsupported-syntax :html node (export-source-position node)
                             :html-attribute message))

(defun export-html-next-attribute-token (value node index)
  (let ((start nil)
        (quoted-p nil)
        (length (length value)))
    (loop while (< index length)
          for character = (char value index)
          do (cond
               ((char= character #\")
                (unless start (setf start index))
                (setf quoted-p (not quoted-p)))
               ((and (not quoted-p)
                     (find character '(#\Space #\Tab #\Newline #\Return)))
                (when start
                  (return-from export-html-next-attribute-token
                    (values start index (1+ index)))))
               ((null start) (setf start index)))
             (incf index))
    (when quoted-p
      (export-html-attribute-error node "Unclosed quoted HTML attribute value"))
    (if start
        (values start length length)
        (values nil nil length))))

(defun export-html-generated-reference (state)
  (let ((used (export-html-state-used-references state)))
    (loop for candidate =
            (string-downcase
             (format nil "org~7,'0X"
                     (incf (export-html-state-next-reference state))))
          unless (gethash candidate used)
            do (setf (gethash candidate used) t)
               (return candidate))))

(defun export-html-reserve-reference (state candidate)
  (let ((used (export-html-state-used-references state))
        (suffixes (export-html-state-reference-suffixes state)))
    (loop for suffix = (gethash candidate suffixes 0)
          for reference = (if (zerop suffix)
                              candidate
                              (format nil "~A.~D" candidate suffix))
          do (if (gethash reference used)
                 (setf (gethash candidate suffixes) (1+ suffix))
                 (progn
                   (setf (gethash candidate suffixes) (1+ suffix)
                         (gethash reference used) t)
                   (return reference))))))

(defun export-html-explicit-reference (state node)
  (multiple-value-bind (reference found-p)
      (gethash node (export-html-state-explicit-references state))
    (if found-p
        reference
        (let* ((document (node-document node))
               (reference
                 (or (and (member (node-type node) '(:headline :inlinetask)
                                  :test #'eq)
                          (multiple-value-bind (value sources)
                              (effective-property document node "CUSTOM_ID")
                            (declare (ignore sources))
                            value))
                     (node-property node :NAME))))
          (setf (gethash node (export-html-state-explicit-references state))
                reference)
          reference))))

(defun export-html-reference (state node)
  (or (gethash node (export-html-state-references state))
      (let* ((explicit (export-html-explicit-reference state node))
             (reference (or explicit (export-html-generated-reference state))))
        (setf (gethash node (export-html-state-references state)) reference)
        (when explicit
          (unless (gethash explicit (export-html-state-reference-targets state))
            (setf (gethash explicit
                           (export-html-state-reference-targets state))
                  reference)))
        reference)))

(defun export-html-index-reference-alias (state alias reference)
  (when (and (stringp alias) (plusp (length alias)))
    (let ((targets (export-html-state-reference-targets state)))
      (unless (gethash alias targets)
        (setf (gethash alias targets) reference)))))

(defun export-html-wrapper-reference (state node)
  (or (gethash node (export-html-state-wrapper-references state))
      (setf (gethash node (export-html-state-wrapper-references state))
            (format nil "outline-container-~A"
                    (export-html-reference state node)))))

(defun export-html-heading-tags (state node)
  (let* ((tags (node-property node :tags))
         (tag-option (assoc :tags
                            (export-preparation-options
                             (export-html-state-preparation state)))))
    (when (and tags (or (null tag-option) (cdr tag-option)))
      (export-html-write state "&nbsp;&nbsp;&nbsp<span class=\"tag\">")
      (dolist (tag tags)
        (export-html-write state "<span class=\"")
        (export-html-escape-to-output state :attribute tag)
        (export-html-write state "\">")
        (export-html-escape-to-output state :text tag)
        (export-html-write state "</span>"))
      (export-html-write state "</span>"))))

(defun export-html-heading-title (state node &optional view)
  (let ((segments (node-property node :title))
        (title-macros (and view
                           (getf (export-view-payload view) :title-macros))))
    (if segments
        (dolist (segment segments)
          (if (node-p segment)
              (progn
                (let ((macro-entry (assoc segment title-macros :test #'eq)))
                  (if macro-entry
                      (export-html-write-inline-views state (cdr macro-entry))
                      (export-html-dispatch-node state segment nil nil :mode :inline)))
                (when (plusp (node-post-blank segment))
                  (export-html-write-spaces state
                                            (node-post-blank segment))))
              (export-html-escape-to-output state :text segment)))
        (export-html-escape-to-output state :text
                                      (or (node-property node :raw-value) ""))))
  (export-html-heading-tags state node))

(defun export-html-write-default-css (state)
  (export-html-write state "<style type=\"text/css\">
")
  (export-html-write state +export-html-default-css+)
  (export-html-write state "</style>
"))

(defun export-html-numbering-depth (state)
  (let* ((options (export-preparation-options
                   (export-html-state-preparation state)))
         (numbering (assoc :num options)))
    (cond ((null numbering) 6)
          ((eq (cdr numbering) t) 6)
          ((integerp (cdr numbering)) (cdr numbering))
          (t 0))))

(defun export-html-index-footnote-reference (state node)
  (let* ((label (node-property node :label))
         (number (and label
                      (gethash label
                               (export-html-state-footnote-numbers state)))))
    (when number
      (let* ((count (1+ (gethash label
                                 (export-html-state-footnote-reference-counts state)
                                 0)))
             (candidate (if (= count 1)
                            (format nil "fnr.~A" label)
                            (format nil "fnr.~A.~D" label (1- count))))
             (reference (export-html-reserve-reference state candidate)))
        (setf (gethash label
                       (export-html-state-footnote-reference-counts state))
              count
              (gethash node (export-html-state-footnote-reference-ids state))
              reference)
        (unless (gethash label
                         (export-html-state-footnote-first-reference-ids state))
          (setf (gethash label
                         (export-html-state-footnote-first-reference-ids state))
                reference))))))

(defun export-html-index-preparation (state)
  (let* ((preparation (export-html-state-preparation state))
         (counts (make-array
                  (1+ (limit-value
                       (export-html-state-limits state) :max-heading-depth))
                  :initial-element 0))
         (max-numbered-depth (export-html-numbering-depth state))
         (root-views (export-preparation-root-views preparation)))
    (dolist (entry (export-preparation-footnotes preparation))
      (setf (gethash (getf entry :label)
                     (export-html-state-footnote-numbers state))
            (getf entry :number)))
    (let ((stack (reverse root-views)))
      (loop while stack
            for view = (pop stack)
            for node = (export-view-node view)
            for type = (node-type node)
            do (when (member type '(:headline :inlinetask :target :radio-target
                                    :example-block :special-block)
                             :test #'eq)
                 (let ((explicit (export-html-explicit-reference state node)))
                   (when explicit
                     (setf (gethash explicit
                                    (export-html-state-used-references state))
                           t))))
               (dolist (child (reverse (export-view-children view)))
                 (push child stack))))
    (let ((stack root-views))
      (loop while stack
          for view = (pop stack)
          for node = (export-view-node view)
          for type = (node-type node)
          do (when (member type '(:headline :inlinetask :target :radio-target
                                  :example-block :special-block)
                           :test #'eq)
               (let ((reference (export-html-reference state node)))
                 (when (member type '(:headline :inlinetask) :test #'eq)
                   (export-html-index-reference-alias
                    state (node-property node :raw-value) reference))
                 (when (member type '(:target :radio-target) :test #'eq)
                   (export-html-index-reference-alias
                    state (node-property node :value) reference)))
               (when (member type '(:headline :inlinetask) :test #'eq)
                 (export-html-wrapper-reference state node)
                 (let ((level (node-property node :level)))
                   (when (and level (<= level max-numbered-depth)
                              (< level (length counts)))
                     (incf (aref counts level))
                     (loop for depth from (1+ level) below (length counts)
                           do (setf (aref counts depth) 0))
                     (setf (gethash node
                                    (export-html-state-heading-numbers state))
                           (format nil "~{~D~^.~}."
                                   (loop for depth from 1 to level
                                         collect (aref counts depth))))))))
               (dolist (child (reverse (export-view-children view)))
                 (push child stack))))
    state))

(defun export-html-write-heading-number (state node &optional plain-p)
  (let ((number (gethash node (export-html-state-heading-numbers state))))
    (when number
      (if plain-p
          (progn
            (export-html-escape-to-output state :text number)
            (export-html-write state " "))
          (progn
            (export-html-write state "<span class=\"section-number-")
            (export-html-write state
                               (write-to-string (1+ (node-property node :level))))
            (export-html-write state "\">")
            (export-html-escape-to-output state :text number)
            (export-html-write state "</span> "))))))

(defun export-html-write-heading-state (state node)
  (let* ((options (export-preparation-options
                   (export-html-state-preparation state)))
         (todo-option (assoc :todo options))
         (priority-option (assoc :priority options))
         (todo (node-property node :todo-keyword))
         (priority (node-property node :priority)))
    (when (and todo (or (null todo-option) (cdr todo-option)))
      (export-html-write state "<span class=\"")
      (export-html-write state
                         (if (eq (node-property node :todo-type) :done)
                             "done " "todo "))
      (export-html-escape-to-output state :attribute todo)
      (export-html-write state "\">")
      (export-html-escape-to-output state :text todo)
      (export-html-write state "</span> "))
    (when (and priority (or (null priority-option) (cdr priority-option)))
      (export-html-write state "<span class=\"priority\">[")
      (export-html-escape-to-output
       state :text
       (if (<= (char-code #\A) priority (char-code #\Z))
           (string (code-char priority))
           (write-to-string priority)))
      (export-html-write state "]</span> "))))

(defun export-html-write-views (state views)
  (dolist (view views)
    (export-html-render-view state view)))

(defun export-html-escape-verse-text-range (state text start end)
  (loop with line-start = start
        for newline = (position #\Newline text :start line-start :end end)
        do (export-html-escape-to-output-range
            state :text text line-start (or newline end))
           (when newline
             (export-html-write state "<br />")
             (export-html-write state (string #\Newline))
             (setf line-start (1+ newline)))
        while newline))

(defun export-html-write-text-range (state text start end)
  (if (export-html-state-verse-output-p state)
      (export-html-escape-verse-text-range state text start end)
      (export-html-escape-to-output-range state :text text start end)))

(defun export-html-write-text (state text)
  (export-html-write-text-range state text 0 (length text)))

(defun export-html-write-inline-views (state views &optional trim-list-indent)
  (loop with trim-list-indent-p = trim-list-indent
        for remaining on views
        for view = (car remaining)
        for node = (export-view-node view)
        for type = (node-type node)
        do (cond
             ((member type '(:section :paragraph) :test #'eq)
              (export-html-write-inline-views
               state (export-view-children view) trim-list-indent-p))
             ((eq type :macro)
              (export-html-write-inline-views
               state (export-view-children view) trim-list-indent-p))
             ((and trim-list-indent-p
                   (member type '(:plain-text :text) :test #'eq))
              (let* ((text (or (node-text node) ""))
                     (start
                       (loop for position from 0 below (length text)
                             while (member (char text position)
                                           '(#\Space #\Tab) :test #'char=)
                             finally (return position))))
                (export-html-write-text-range state text start (length text))
                (when (< start (length text))
                  (setf trim-list-indent-p nil))))
             (t (export-html-render-view state view :inline)))
           (when (and (not (member type '(:section :paragraph) :test #'eq))
                      (plusp (node-post-blank node)))
             (export-html-write-spaces state (node-post-blank node)))))

(defun export-html-trim-output-newline (state)
  (let* ((output (export-html-state-output state))
         (length (fill-pointer output)))
    (when (char= (char output (1- length)) #\Newline)
      (decf (fill-pointer output)))))

(defun export-html-copy-view-with-children (view children)
  (%make-export-view :node (export-view-node view)
                     :children children
                     :payload (export-view-payload view)
                     :source-document (export-view-source-document view)))
(defun export-html-append-to-deepest-section (heading pending)
  (let* ((heading-children (export-view-children heading))
         (section
           (find-if
            (lambda (child)
              (eq (node-type (export-view-node child)) :section))
            heading-children))
         (last-heading
           (find-if
            (lambda (child)
              (member (node-type (export-view-node child))
                      (list :headline :inlinetask)
                      :test
                      (function eq)))
            heading-children
            :from-end
            t))
         (last-section
           (and last-heading
                (find-if
                 (lambda (child)
                   (eq (node-type (export-view-node child)) :section))
                 (export-view-children last-heading)))))
    (cond
      ((and last-heading last-section)
       (export-html-copy-view-with-children
        heading
        (mapcar
         (lambda (child)
           (if (eq child last-heading)
               (export-html-append-to-deepest-section last-heading pending)
               child))
         heading-children)))
      ((null section) heading)
      (t
       (let* ((section-children (export-view-children section))
              (previous (car (last section-children)))
              (pending-view (first pending))
              (pending-node
                (and pending-view (export-view-node pending-view)))
              (pending-child-view
                (and pending-node
                     (eq (node-type pending-node) :section)
                     (first (export-view-children pending-view))))
              (next-view (or pending-child-view pending-view))
              (previous-node
                (and previous (export-view-node previous)))
              (next-node
                (and next-view (export-view-node next-view)))
              (previous-source
                (and previous (export-view-source-document previous)))
              (next-source
                (and next-view (export-view-source-document next-view)))
              (previous-source-text
                (and previous-source (document-text previous-source)))
              (merge-paragraphs-p
                (and previous-node
                     next-node
                     (eq (node-type previous-node) :paragraph)
                     (eq (node-type next-node) :paragraph)
                     previous-source
                     next-source
                     (not (eq previous-source next-source))
                     (null (node-properties previous-node))
                     (null (node-properties next-node))
                     (zerop (node-post-blank previous-node))
                     (eql (node-begin next-node) 0)
                     (stringp previous-source-text)
                     (eql (node-end previous-node)
                          (length previous-source-text))))
              (remaining-pending
                (if merge-paragraphs-p
                    (if pending-child-view
                        (cons
                         (export-html-copy-view-with-children
                          pending-view
                          (rest (export-view-children pending-view)))
                         (rest pending))
                        (rest pending))
                    pending))
              (updated-section-children
                (if merge-paragraphs-p
                    (append
                     (butlast section-children)
                     (list
                      (export-html-copy-view-with-children
                       previous
                       (append (export-view-children previous)
                               (export-view-children next-view))))
                     remaining-pending)
                    (append section-children pending))))
         (export-html-copy-view-with-children
          heading
          (mapcar
           (lambda (child)
             (if (eq child section)
                 (export-html-copy-view-with-children
                  section updated-section-children)
                 child))
           heading-children)))))))

(defun export-html-group-section-children (views)
  (let ((result nil)
        (active-heading nil)
        (active-section nil)
        (pending-children nil)
        (section-dirty-p nil))
    (labels ((flush-section ()
               (when (and active-heading active-section section-dirty-p)
                 (setf (car result)
                       (export-html-append-to-deepest-section
                        active-heading
                        (nreverse pending-children))))))
      (dolist (view views)
        (let ((type (node-type (export-view-node view))))
          (if (member type (list :headline :inlinetask) :test (function eq))
              (progn
                (flush-section)
                (push view result)
                (setf active-heading view
                      active-section
                      (find-if
                       (lambda (child)
                         (eq (node-type (export-view-node child)) :section))
                       (export-view-children view))
                      pending-children nil
                      section-dirty-p nil))
              (if active-section
                  (progn
                    ;; Expanded INCLUDE siblings belong to the imported section.
                    (push view pending-children)
                    (setf section-dirty-p t))
                  (progn
                    (push view result)
                    (setf active-heading nil))))))
      (flush-section)
      (nreverse result))))

(defun export-html-write-node-children (state node child-views view)
  (let ((previous (export-html-state-verse-output-p state))
        (verse-p (eq (node-type node) :verse-block)))
    (when verse-p
      (setf (export-html-state-verse-output-p state) t))
    (unwind-protect
         (if view
             (export-html-write-views
              state
              (if (eq (node-type node) :section)
                  (export-html-group-section-children child-views)
                  child-views))
             (dolist (child (node-children node))
               (export-html-dispatch-node state child nil nil)))
      (when verse-p
        (setf (export-html-state-verse-output-p state) previous)))))

(defun export-html-view-children-of-type (views type)
  (remove-if-not (lambda (view)
                   (eq (node-type (export-view-node view)) type))
                 views))

(defun export-html-write-raw-text (state node &optional (property :value))
  (export-html-escape-to-output state :text
                                (or (node-property node property)
                                    (node-text node) "")))

(defun export-html-ends-with-newline-p (text)
  (and (plusp (length text))
       (char= #\Newline (char text (1- (length text))))))

(defun export-html-entity-reference-p (value)
  (and (stringp value)
       (> (length value) 2)
       (char= (char value 0) #\&)
       (char= (char value (1- (length value))) #\;)
       (loop for position from 1 below (1- (length value))
             for character = (char value position)
             for code = (char-code character)
             always (or (export-ascii-alpha-p character)
                        (<= #x30 code #x39)
                        (char= character #\#)))))

(defun export-html-safe-attribute-value (node key value start end)
  (when (member key '("height" "width") :test #'string=)
    (let ((digits-end
            (if (and (< start end)
                     (char= (char value (1- end)) #\%))
                (1- end)
                end)))
      (unless (and (< start digits-end)
                   (loop for position from start below digits-end
                         always (digit-char-p (char value position))))
        (export-html-attribute-error
         node "HTML image dimensions must be decimal integers or percentages"))))
  (subseq value start end))

(defun export-html-attribute-key (value start end)
  (when (and (> (- end start) 1)
             (char= (char value start) #\:))
    (loop for key in '("alt" "class" "height" "id" "title" "width")
          when (string-equal key value :start2 (1+ start) :end2 end)
            return key)))

(defun export-html-parse-attributes (node state)
  (let ((value (node-property node :ATTR_HTML))
        (attributes nil)
        (output-length 0))
    (when value
      (unless (stringp value)
        (export-html-attribute-error node "ATTR_HTML must be a string"))
      (let ((index 0))
        (loop
          (multiple-value-bind (key-start key-end next-index)
              (export-html-next-attribute-token value node index)
            (setf index next-index)
            (unless key-start
              (return))
            (multiple-value-bind (value-start value-end value-next-index)
                (export-html-next-attribute-token value node index)
              (setf index value-next-index)
              (let ((key (export-html-attribute-key
                          value key-start key-end)))
                (unless (and key value-start
                             (not (assoc key attributes :test #'string=)))
                  (export-html-attribute-error
                   node "Unsupported or malformed ATTR_HTML attribute"))
                (when (and (> (- value-end value-start) 1)
                           (char= (char value value-start) #\")
                           (char= (char value (1- value-end)) #\"))
                  (incf value-start)
                  (decf value-end))
                (let* ((value-output-length
                         (export-html-escaped-output-length
                          :attribute value value-start value-end))
                       (attribute-output-length
                         (+ 4 (length key) value-output-length)))
                  (export-html-check-output-length
                   state (+ output-length attribute-output-length))
                  (incf output-length attribute-output-length)
                  (push (cons key
                              (export-html-safe-attribute-value
                               node key value value-start value-end))
                        attributes)))))))
      (unless attributes
        (export-html-attribute-error node "ATTR_HTML has no attributes")))
    (nreverse attributes)))

(defun export-html-index-attribute-ids (state)
  (let ((stack (reverse
                (export-preparation-root-views
                 (export-html-state-preparation state)))))
    (loop while stack
          for view = (pop stack)
          for node = (export-view-node view)
          do (when (node-property node :ATTR_HTML)
               (unless (member (node-type node)
                               '(:link :paragraph :table) :test #'eq)
                 (export-html-attribute-error
                  node "ATTR_HTML is supported only on HTML links and images"))
               (dolist (attribute (export-html-parse-attributes node state))
                 (when (string= (car attribute) "id")
                   (setf (gethash (cdr attribute)
                                  (export-html-state-used-references state))
                         t))))
             (dolist (child (reverse (export-view-children view)))
               (push child stack))))
  state)

(defun export-html-index-footnote-ids (state)
  (let* ((preparation (export-html-state-preparation state))
         (footnotes (export-preparation-footnotes preparation)))
    (dolist (entry footnotes)
      (let* ((label (getf entry :label))
             (reference (export-html-reserve-reference
                         state (format nil "fn.~A" label))))
        (setf (gethash label
                       (export-html-state-footnote-definition-ids state))
              reference)))
    (let ((stack (reverse (export-preparation-root-views preparation))))
      (loop while stack
            for view = (pop stack)
            for node = (export-view-node view)
            do (when (eq (node-type node) :footnote-reference)
                 (export-html-index-footnote-reference state node))
               (dolist (child (reverse (export-view-children view)))
                 (push child stack)))))
  state)

(defun export-html-image-path-p (path) (export-image-path-p path))

(defun export-html-preparation-documents (preparation)
  (let ((documents nil)
        (stack (reverse (export-preparation-root-views preparation))))
    (loop while stack
          for view = (pop stack)
          for document = (export-view-source-document view)
          do (when (and document (not (member document documents :test #'eq)))
               (push document documents))
             (dolist (child (reverse (export-view-children view)))
               (push child stack)))
    (nreverse documents)))

(defun export-html-link-resolution-index (state node)
  (or (export-html-state-resolution-index state)
      (let* ((preparation (export-html-state-preparation state))
             (documents (if preparation
                            (export-html-preparation-documents preparation)
                            nil))
             (document (node-document node)))
        (setf (export-html-state-resolution-index state)
              (make-link-resolution-index
               (if (and document (not (member document documents :test #'eq)))
                   (append documents (list document))
                   documents))))))

(defun export-html-link-resolution-reference (state node)
  (let ((resolution (handler-case
                        (resolve-link-with-index
                         node (export-html-link-resolution-index state node))
                        (org-link-resolution-error () nil)
                        (org-id-error () nil)
                        (org-resource-limit-error () nil))))
    (loop for candidate in (and resolution
                                (link-resolution-candidates resolution))
          for reference = (gethash candidate
                                   (export-html-state-references state))
          when reference
            return reference)))

(defun export-html-link-destination (state node)
  (let* ((type (node-property node :type))
         (path (node-property node :path))
         (destination
           (cond
             ((and (stringp type)
                   (stringp path)
                   (member type '("http" "https" "mailto")
                           :test #'string-equal)
                   (notany #'export-control-character-p path)
                   (not (find #\\ path)))
              (export-html-check-escaped-parts
               state :attribute
               (list (list type 0 (length type))
                     (list ":" 0 1)
                     (list path 0 (length path))))
              (concatenate 'string type ":" path))
             ((and (stringp type) (string-equal type "id"))
              (let ((reference (export-html-link-resolution-reference state node)))
                (when reference
                  (export-html-check-escaped-parts
                   state :attribute (list (list "#" 0 1)
                                          (list reference 0 (length reference))))
                  (concatenate 'string "#" reference))))
             ((and (stringp type) (string-equal type "custom-id"))
              (when (stringp path)
                (let ((target
                        (or (export-html-link-resolution-reference state node)
                            path)))
                  (when (and (notany #'export-control-character-p target)
                             (not (find #\\ target)))
                    (export-html-check-escaped-parts
                     state :attribute (list (list "#" 0 1)
                                            (list target 0 (length target))))
                    (concatenate 'string "#" target)))))
             ((and (stringp type)
                   (string-equal type "file")
                   (stringp path)
                   (search "::" path)
                   (zerop (search "::" path)))
              (let ((reference (export-html-link-resolution-reference state node)))
                (when reference
                  (export-html-check-escaped-parts
                   state :attribute (list (list "#" 0 1)
                                          (list reference 0 (length reference))))
                  (concatenate 'string "#" reference))))
             ((and (stringp type) (string-equal type "file"))
              (when (and (export-html-image-path-p path)
                         (stringp path)
                         (export-safe-url-p path path))
                (export-html-check-escaped-parts
                 state :attribute (list (list path 0 (length path))))
                path))
             ((and (stringp type)
                   (member type '("fuzzy" "radio") :test #'string-equal))
              (let ((target
                      (or (export-html-link-resolution-reference state node)
                          (gethash path
                                   (export-html-state-reference-targets state)))))
                (if (and target
                         (notany #'export-control-character-p path)
                         (notany #'export-control-character-p target)
                         (not (find #\\ target)))
                    (progn
                      (export-html-check-escaped-parts
                       state :attribute
                       (list (list "#" 0 1) (list target 0 (length target))))
                      (concatenate 'string "#" target))
                    path)))
             ((and (stringp type) (stringp path)) nil)
             (t path))))
    (when (and (stringp path)
               (stringp destination)
               (export-safe-url-p path destination))
      destination)))

(defun export-html-image-link-p (node)
  (let ((type (node-property node :type))
        (path (node-property node :path)))
    (and (stringp type)
         (stringp path)
         (member type '("file" "fuzzy") :test #'string-equal)
         (export-html-image-path-p path))))

(defun export-html-link-label (state node child-views view)
  (if (or child-views
          (and view (export-view-children view))
          (and (null view) (node-children node)))
      (export-html-write-node-children state node child-views view)
      (let* ((type (node-property node :type))
             (path (node-property node :path)))
        (export-html-escape-to-output
         state :text
         (or (node-property node :raw-link)
             (and (stringp type)
                  (stringp path)
                  (member type '("http" "https" "mailto")
                          :test #'string-equal)
                  (export-html-check-escaped-parts
                   state :text
                   (list (list type 0 (length type))
                         (list ":" 0 1)
                         (list path 0 (length path))))
                  (concatenate 'string type ":" path))
             path
             "")))))

(defun export-html-render-image
    (state node attributes &optional handler-destination handler-description)
  (let* ((path (node-property node :path))
         (destination (or handler-destination
                          (export-html-link-destination state node)))
         (alt (or (cdr (assoc "alt" attributes :test #'string=))
                  handler-description path "")))
    (when destination
      (export-html-write state "<img src=\"")
      (export-html-escape-to-output state :attribute destination)
      (export-html-write state "\" alt=\"")
      (export-html-escape-to-output state :attribute alt)
      (export-html-write state "\"")
      (dolist (attribute attributes)
        (unless (string= (car attribute) "alt")
          (export-html-write-attribute state (car attribute) (cdr attribute))))
      (export-html-write state " />"))))

(defun export-html-render-link (state node child-views view &optional attributes)
  (let* ((context (export-html-state-render-context state))
         (frame (export-html-state-render-frame state))
         (result (and context frame
                      (export-call-link-type-handler-for-view
                       context node view frame))))
    (if result
        (let* ((path (node-property node :path))
               (destination (export-link-result-destination result))
               (description (export-link-result-description result))
               (kind (export-link-result-kind result))
               (image-alt
                (and (eq kind :image)
                     (or (cdr (assoc "alt" attributes :test #'string=))
                         description path "")))
               (output-description
                (if (eq kind :image) image-alt description))
               (scheme-end (export-explicit-url-scheme-end destination))
               (safe-p
                (and (stringp path)
                     (export-safe-url-p path destination)
                     (or (eq kind :link)
                         (or (null scheme-end)
                             (member (subseq destination 0 scheme-end)
                                      '("http" "https")
                                      :test #'string-equal))))))
          (if safe-p
              (progn
                (export-html-check-escaped-parts
                 state :attribute (list (list destination 0 (length destination))))
                (export-html-check-escaped-parts
                 state (if (eq kind :image) :attribute :text)
                 (list (list output-description 0 (length output-description))))
                (case kind
                  (:image
                   (export-html-render-image
                    state node attributes destination output-description))
                  (:link
                   (export-html-write state "<a href=\"")
                   (export-html-escape-to-output state :attribute destination)
                   (export-html-write state "\">")
                   (export-html-escape-to-output state :text description)
                   (export-html-write state "</a>"))))
            (export-html-link-label state node child-views view)))
      (if (export-html-image-link-p node)
          (export-html-render-image state node attributes)
        (let ((destination (export-html-link-destination state node)))
          (if destination
              (progn
                (export-html-write state "<a href=\"")
                (export-html-escape-to-output state :attribute destination)
                (export-html-write state "\">")
                (export-html-link-label state node child-views view)
                (export-html-write state "</a>"))
            (export-html-link-label state node child-views view))))))
  )

(defun export-html-footnote-reference (state node child-views view)
  (let* ((label (node-property node :label))
         (number (and label
                      (gethash label
                               (export-html-state-footnote-numbers state)))))
    (cond
      ((and (null label) (eq (node-property node :type) :inline))
       (if view
           (export-html-write-inline-views state child-views)
           (dolist (child (node-children node))
             (export-html-dispatch-node state child nil nil :mode :inline))))
      (number
       (let ((reference-id
               (gethash node
                        (export-html-state-footnote-reference-ids state)))
             (definition-id
               (gethash label
                        (export-html-state-footnote-definition-ids state))))
         (export-html-write state "<sup><a id=\"")
         (export-html-escape-to-output state :attribute reference-id)
         (export-html-write state "\" class=\"footref\" href=\"#")
         (export-html-escape-to-output state :attribute definition-id)
         (export-html-write state "\" role=\"doc-backlink\">")
         (export-html-write state (write-to-string number))
         (export-html-write state "</a></sup>")))
      (label
       (export-error :html :unresolved-footnote node
                     "Footnote reference has no selected definition")))))

(defun export-html-render-footnotes (state)
  (let ((footnotes (export-preparation-footnotes
                    (export-html-state-preparation state))))
    (when footnotes
      (export-html-write state "<div id=\"footnotes\">
")
      (export-html-write state "<h2 class=\"footnotes\">Footnotes: </h2>
")
      (export-html-write state "<div id=\"text-footnotes\">

")
      (dolist (entry footnotes)
        (let* ((label (getf entry :label))
               (number (getf entry :number))
               (view (getf entry :definition))
               (definition-id
                 (gethash label
                          (export-html-state-footnote-definition-ids state)))
               (reference-id
                 (gethash label
                          (export-html-state-footnote-first-reference-ids state))))
          (export-html-write state "<div class=\"footdef\"><sup><a id=\"")
          (export-html-escape-to-output state :attribute definition-id)
          (export-html-write state "\" class=\"footnum\" href=\"#")
          (export-html-escape-to-output state :attribute reference-id)
          (export-html-write state "\" role=\"doc-backlink\">")
          (export-html-write state (write-to-string number))
          (export-html-write state "</a></sup> <div class=\"footpara\" role=\"doc-footnote\">")
          (dolist (child (export-view-children view))
            (let ((node (export-view-node child)))
              (if (eq (node-type node) :paragraph)
                  (progn
                    (export-html-write state "<p class=\"footpara\">
")
                    (export-html-write-views state (export-view-children child))
                    (unless (export-html-output-ends-with-newline-p state)
                      (export-html-write state (string #\Newline)))
                    (export-html-write state "</p>"))
                  (export-html-render-view state child))))
          (export-html-write state "</div></div>
")))
      (export-html-write state "

</div>
</div>"))))

(defun export-html-render-toc (state)
  (let ((entries (copy-list (export-preparation-toc
                             (export-html-state-preparation state)))))
    (when entries
      (export-html-write state "<div id=\"table-of-contents\" role=\"doc-toc\">
")
      (export-html-write state "<h2>Table of Contents</h2>
")
      (export-html-write state "<div id=\"text-table-of-contents\" role=\"doc-toc\">
")
      (labels ((render-list (minimum-level)
                 (export-html-write state "<ul>
")
                 (loop while entries
                       for entry = (first entries)
                       for level = (getf entry :level)
                       do (cond
                            ((< level minimum-level) (return))
                            ((> level minimum-level)
                             (render-list level))
                            (t
                             (pop entries)
                             (let* ((view (getf entry :view))
                                    (node (export-view-node view))
                                    (reference (export-html-reference state node)))
                               (export-html-write state "<li><a href=\"#")
                               (export-html-escape-to-output state :attribute reference)
                               (export-html-write state "\">")
                               (export-html-write-heading-number state node t)
                               (export-html-write-heading-state state node)
                               (export-html-heading-title state node view)
                               (export-html-write state "</a>")
                               (when (and entries
                                          (> (getf (first entries) :level) level))
                                 (render-list (getf (first entries) :level)))
                               (export-html-write state "</li>
")))))
                 (export-html-write state "</ul>
")))
        (render-list (getf (first entries) :level)))
      (export-html-write state "</div>
</div>
"))))

(defun export-html-render-heading (state node child-views &optional view)
  (let* ((level (node-property node :level))
         (html-level (1+ level))
         (tag-level (min 6 (max 1 html-level)))
         (tag (format nil "h~D" tag-level))
         (reference (export-html-reference state node))
         (wrapper-reference (export-html-wrapper-reference state node))
         (section-views (export-html-view-children-of-type child-views :section))
         (other-views (remove-if (lambda (view)
                                   (eq (node-type (export-view-node view)) :section))
                                 child-views))
         (custom-reference (export-html-explicit-reference state node))
         (section-reference
           (cond (custom-reference (concatenate 'string "text-" custom-reference))
                 ((gethash node (export-html-state-heading-numbers state))
                  (concatenate 'string "text-"
                               (string-right-trim "."
                                                  (gethash node
                                                           (export-html-state-heading-numbers
                                                            state)))))
                 (t (concatenate 'string "text-" reference)))))
    (export-html-write state "<div id=\"")
    (export-html-escape-to-output state :attribute wrapper-reference)
    (export-html-write state "\" class=\"outline-")
    (export-html-write state (write-to-string html-level))
    (export-html-write state "\">
<")
    (export-html-write state tag)
    (export-html-write state " id=\"")
    (export-html-escape-to-output state :attribute reference)
    (export-html-write state "\">")
    (export-html-write-heading-number state node)
    (export-html-write-heading-state state node)
    (export-html-heading-title state node view)
    (export-html-write state "</")
    (export-html-write state tag)
    (export-html-write state ">
")
    (when section-views
      (export-html-write state "<div class=\"outline-text-")
      (export-html-write state (write-to-string html-level))
      (export-html-write state "\" id=\"")
      (export-html-escape-to-output state :attribute section-reference)
      (export-html-write state "\">
")
      (export-html-write-views state section-views)
      (export-html-write state "</div>
"))
    (export-html-write-views state other-views)
    (export-html-write state "</div>
")))

(defun export-html-standalone-image-view (child-views)
  (let ((image-view nil))
    (dolist (child child-views)
      (let* ((node (export-view-node child))
             (type (node-type node)))
        (cond
          ((eq type :link)
           (unless (and (null image-view) (export-html-image-link-p node))
             (return-from export-html-standalone-image-view nil))
           (setf image-view child))
          ((and (member type '(:plain-text :text) :test #'eq)
                (stringp (node-text node))
                (every #'export-whitespace-character-p (node-text node))))
          (t (return-from export-html-standalone-image-view nil)))))
    image-view))
(defun export-html-validate-attribute-nodes (state)
  (let ((stack (reverse
                (export-preparation-root-views
                 (export-html-state-preparation state)))))
    (loop while stack
          for view = (pop stack)
          for node = (export-view-node view)
          do (when (and (node-property node :ATTR_HTML)
                        (eq (node-type node) :paragraph)
                        (null (export-html-standalone-image-view
                               (export-view-children view))))
               (export-html-attribute-error
                node "ATTR_HTML requires one unambiguous image link"))
             (dolist (child (reverse (export-view-children view)))
               (push child stack))))
  state)

(defun export-html-render-standalone-image (state node child-views image-view
                                           attributes)
  (export-html-write state "<div id=\"")
  (export-html-escape-to-output state :attribute
                                (export-html-reference state node))
  (export-html-write state (format nil "\" class=\"figure\">~%<p>"))
  (dolist (child child-views)
    (when (eq child image-view)
      (let ((link (export-view-node child)))
        (export-html-dispatch-node
         state link (export-view-children child) child
         :mode :inline :image-attributes attributes))))
  (export-html-write state (format nil "~%</p>~%</div>~%")))

(defun export-html-render-paragraph (state node child-views
                                    &optional trim-list-indent)
  (let* ((attribute-value (node-property node :ATTR_HTML))
         (image-view (export-html-standalone-image-view child-views))
         (attributes (when attribute-value
                       (export-html-parse-attributes node state))))
    (when (and attribute-value (null image-view))
      (export-html-attribute-error
       node "ATTR_HTML requires one unambiguous image link"))
    (if image-view
        (export-html-render-standalone-image
         state node child-views image-view attributes)
        (progn
          (export-html-write state "<p>
")
          (export-html-write-inline-views
           state child-views trim-list-indent)
          (unless (export-html-output-ends-with-newline-p state)
            (export-html-write state (string #\Newline)))
          (export-html-write state "</p>")
          (export-html-write state (string #\Newline))))))

(defun export-html-output-ends-with-newline-p (state)
  (let* ((output (export-html-state-output state))
         (length (fill-pointer output)))
    (char= (char output (1- length)) #\Newline)))

(defun export-html-render-example-block (state node)
  (let ((reference (export-html-reference state node))
        (value (or (node-property node :value) (node-text node) "")))
    (export-html-write state "<pre class=\"example\" id=\"")
    (export-html-escape-to-output state :attribute reference)
    (export-html-write state "\">
")
    (export-html-escape-to-output state :text value)
    (unless (export-html-ends-with-newline-p value)
      (export-html-write state "
"))
    (export-html-write state "</pre>
")))

(defun export-html-render-fixed-width (state node)
  (let ((value (or (node-property node :value) (node-text node) "")))
    (export-html-write state "<pre class=\"example\">
")
    (export-html-escape-to-output state :text value)
    (unless (export-html-ends-with-newline-p value)
      (export-html-write state "
"))
    (export-html-write state "</pre>
")))

(defun export-html-literal-object-value-range (node)
  (let* ((document (node-document node))
         (source (and document (document-text document)))
         (start (node-begin node))
         (end (node-end node)))
    (if (and source start end)
        (let ((trimmed-end (- end (node-post-blank node))))
          (if (> (- trimmed-end start) 2)
              (values source (1+ start) (1- trimmed-end))
              (values source start start)))
        (values "" 0 0))))

(defun export-html-render-source-block (state view)
  (let* ((node (export-view-node view))
         (mode (getf (export-view-payload view) :exports))
         (language (or (node-property node :language) "")))
    (when (member mode '(:code :both) :test #'eq)
      (export-html-write state "<div class=\"org-src-container\">
<pre class=\"src src-")
      (export-html-escape-to-output state :attribute language)
      (export-html-write state "\"><code>")
      (let ((value (or (node-property node :value) (node-text node) "")))
        (export-html-escape-to-output state :text value)
        (unless (export-html-ends-with-newline-p value)
          (export-html-write state "
")))
      (export-html-write state "</code></pre>
</div>
"))
    (when (member mode '(:results :both) :test #'eq)
      (export-html-write-views state (export-view-children view)))))

(defun export-html-render-timestamp (state node)
  (let ((start (timestamp-start node)))
    (if start
        (let ((iso (org-time-value-iso8601
                    (%time-value-from-local-value start))))
          (export-html-write state "<time class=\"timestamp\" datetime=\"")
          (export-html-escape-to-output state :attribute iso)
          (export-html-write state "\">")
          (export-html-write-raw-text state node)
          (export-html-write state "</time>"))
        (export-html-write-raw-text state node))))

(defun export-html-render-code-block (state node)
  (let* ((dynamic-block-p (eq (node-type node) :dynamic-block))
         (document (and dynamic-block-p (node-document node)))
         (source (and document (document-text document)))
         (start (and dynamic-block-p (node-contents-begin node)))
         (end (and dynamic-block-p (node-contents-end node)))
         (source-range-p (and source start end))
         (value (unless source-range-p
                  (or (node-property node :value) (node-text node) ""))))
    (export-html-write state "<pre class=\"example\">
")
    (if source-range-p
        (export-html-escape-to-output-range state :text source start end)
        (export-html-escape-to-output state :text value))
    (unless (if source-range-p
                (and (> end start)
                     (char= (char source (1- end)) #\Newline))
                (export-html-ends-with-newline-p value))
      (export-html-write state "
"))
    (export-html-write state "</pre>
")))

(defun export-html-render-export-block (state node)
  (let* ((backend (node-property node :type))
         (value (or (node-property node :value) "")))
    (when (string-equal (or backend "") "HTML")
      (if (export-preparation-allow-raw-html
           (export-html-state-preparation state))
          (export-html-write state value)
          (progn
            (export-html-write state "<pre class=\"example\">
")
            (export-html-escape-to-output state :text value)
            (unless (export-html-ends-with-newline-p value)
              (export-html-write state "
"))
            (export-html-write state "</pre>
"))))))

(defun export-html-render-snippet (state node)
  (let ((backend (node-property node :back-end))
        (value (or (node-property node :value) "")))
    (when (string-equal (or backend "") "html")
      (if (export-preparation-allow-raw-html
           (export-html-state-preparation state))
          (export-html-write state value)
          (export-html-escape-to-output state :text value)))))

(defun export-html-render-list-item (state view list-kind)
  (let* ((node (export-view-node view))
         (children (export-view-children view))
         (tag (if (eq list-kind :descriptive) "dd" "li"))
         (checkbox (node-property node :checkbox))
         (checkbox-class (case checkbox
                           (:off "off")
                           (:on "on")
                           (:trans "trans")))
         (checkbox-markup (case checkbox
                            (:off "[&nbsp;]")
                            (:on "[X]")
                            (:trans "[-]")))
         (paragraph-seen-p nil)
         (paragraph-views (export-html-view-children-of-type children :paragraph))
         (wrap-paragraphs-p
           (or (> (length paragraph-views) 1)
               (some (lambda (child)
                       (not (member
                             (node-type (export-view-node child))
                             '(:paragraph :plain-list) :test #'eq)))
                     children))))
    (if (eq list-kind :descriptive)
        (progn
          (export-html-write state "<dt>")
          (let ((term (node-property node :tag)))
            (dolist (segment term)
              (if (node-p segment)
                  (export-html-dispatch-node state segment nil nil :mode :inline)
                  (export-html-escape-to-output state :text segment))))
          (export-html-write state "</dt>
<dd"))
        (export-html-write state "<li"))
    (when checkbox-class
      (export-html-write state " class=\"")
      (export-html-write state checkbox-class)
      (export-html-write state "\""))
    (export-html-write state ">")
    (when checkbox
      (export-html-write state "<code>")
      (export-html-write state checkbox-markup)
      (export-html-write state "</code> "))
    (dolist (child children)
      (let ((child-node (export-view-node child)))
        (if (eq (node-type child-node) :paragraph)
            (progn
              (when paragraph-seen-p
                (export-html-write state (string #\Newline)))
              (if wrap-paragraphs-p
    (export-html-dispatch-node
     state child-node (export-view-children child) child
     :mode :block :trim-list-indent-p t)
    (export-html-write-inline-views
     state (export-view-children child) t))
              (setf paragraph-seen-p t))
            (progn
              (when (and (or paragraph-seen-p
                             (not (eq child (first children))))
                         (not (export-html-output-ends-with-newline-p state)))
                (export-html-write state "
"))
              (export-html-render-view state child)))))
    (export-html-trim-output-newline state)
    (export-html-write state (format nil "</~A>" tag))))

(defun export-html-render-list (state node child-views)
  (let* ((kind (node-property node :type))
         (tag (case kind
                (:ordered "ol")
                (:descriptive "dl")
                (otherwise "ul")))
         (class (case kind
                  (:ordered "org-ol")
                  (:descriptive "org-dl")
                  (otherwise "org-ul"))))
    (export-html-write state "<")
    (export-html-write state tag)
    (export-html-write state " class=\"")
    (export-html-write state class)
    (export-html-write state "\">
")
    (dolist (view child-views)
      (export-html-render-list-item state view kind)
      (export-html-write state "
"))
    (export-html-write state "</")
    (export-html-write state tag)
    (export-html-write state ">
")))

(defun export-html-render-table-row (state view header-p)
  (let ((cells (export-view-children view))
        (tag (if header-p "th" "td")))
    (export-html-write state "<tr>
")
    (dolist (cell-view cells)
      (export-html-write state "<")
      (export-html-write state tag)
      (when header-p
        (export-html-write state " scope=\"col\""))
      (export-html-write state " class=\"org-left\">")
      (let ((cell (export-view-node cell-view)))
        (if (export-view-children cell-view)
            (export-html-write-views state (export-view-children cell-view))
            (let ((value (or (node-property cell :value) "")))
              (if (zerop (length value))
                  (export-html-write state "&nbsp;")
                  (export-html-escape-to-output state :text value)))))
      (export-html-write state "</")
      (export-html-write state tag)
      (export-html-write state ">
"))
    (export-html-write state "</tr>
")))

(defun export-html-render-table (state node child-views)
  (let* ((attributes (export-html-parse-attributes node state))
         (rows (remove-if (lambda (view)
                            (eq (node-property (export-view-node view) :type)
                                :rule))
                          child-views))
         (header (first rows))
         (body (rest rows))
         (column-count (length (and header (export-view-children header)))))
    (export-html-write state "<table border=\"2\" cellspacing=\"0\" cellpadding=\"6\" rules=\"groups\" frame=\"hsides\"")
    (dolist (attribute attributes)
      (export-html-write-attribute state (car attribute) (cdr attribute)))
    (export-html-write state ">")
    (loop repeat 3
          do
      (export-html-write state (string #\Newline)))
    (when (plusp column-count)
      (export-html-write state "<colgroup>
")
      (loop for index below column-count
            do (export-html-write state "<col class=\"org-left\" />")
               (export-html-write
                state (if (= index (1- column-count))
                          (string #\Newline)
                          (format nil "~%~%"))))
      (export-html-write state "</colgroup>
"))
    (when header
      (export-html-write state "<thead>
")
      (export-html-render-table-row state header t)
      (export-html-write state "</thead>
"))
    (when body
      (export-html-write state "<tbody>
")
      (loop for remaining on body
            do (export-html-render-table-row state (first remaining) nil)
               (when (rest remaining)
                 (export-html-write state "
")))
      (export-html-write state "</tbody>
"))
    (export-html-write state "</table>
")))

(defun export-html-render-node-legacy (state node child-views view)
  (let ((type (node-type node)))
    (when (and (node-property node :ATTR_HTML)
               (not (member type '(:link :paragraph :table) :test #'eq)))
      (export-html-attribute-error
       node "ATTR_HTML is supported only on HTML links and images"))
    (case type
      ((:org-data :section :drawer :special-block)
       (case type
         (:special-block
          (let ((name (or (node-property node :type) "special-block")))
            (export-html-write state "<div class=\"")
            (export-html-escape-to-output state :attribute
                                          (string-upcase name))
            (export-html-write state "\" id=\"")
            (export-html-escape-to-output
             state :attribute (export-html-reference state node))
            (export-html-write state "\">")
            (export-html-write state (string #\Newline))
            (export-html-write-node-children state node child-views view)
            (export-html-write state (format nil "~%</div>~%"))))
         (otherwise
          (export-html-write-node-children state node child-views view))))
      ((:headline :inlinetask)
       (export-html-render-heading state node child-views view))
      (:paragraph
       (export-html-render-paragraph
 state node child-views
 (export-html-state-trim-list-indent-p state)))
      (:center-block
       (export-html-write state "<div class=\"org-center\">
")
       (export-html-write-node-children state node child-views view)
       (export-html-write state "</div>
"))
      (:quote-block
       (export-html-write state "<blockquote>
")
       (export-html-write-node-children state node child-views view)
       (export-html-write state "</blockquote>
"))
      (:plain-list
       (export-html-render-list state node child-views))
      (:table
       (export-html-render-table state node child-views))
      (:example-block
       (export-html-render-example-block state node))
      (:fixed-width
       (export-html-render-fixed-width state node))
      (:src-block
       (export-html-render-source-block state view))
      ((:dynamic-block :latex-environment)
       (export-html-render-code-block state node))
      (:export-block
       (export-html-render-export-block state node))
      (:export-snippet
       (export-html-render-snippet state node))
      (:horizontal-rule
       (export-html-write state "<hr />
"))
      ((:bold :italic :strike-through :underline :subscript :superscript
        :code :verbatim :inline-src-block)
       (let ((tag (case type
                    (:bold "b")
                    (:italic "i")
                    (:strike-through "del")
                    (:underline nil)
                    (:subscript "sub")
                    (:superscript "sup")
                    (otherwise "code"))))
         (if (eq type :underline)
             (export-html-write state "<span class=\"underline\">")
             (progn
               (export-html-write state "<")
               (export-html-write state tag)
               (export-html-write state ">")))
         (if (member type '(:inline-src-block :code :verbatim) :test #'eq)
             (if (member type '(:code :verbatim) :test #'eq)
                 (multiple-value-bind (value start end)
                     (export-html-literal-object-value-range node)
                   (export-html-escape-to-output-range
                    state :text value start end))
                 (export-html-escape-to-output
                  state :text (or (node-property node :value) "")))
             (export-html-write-node-children state node child-views view))
         (if (eq type :underline)
             (export-html-write state "</span>")
             (progn
               (export-html-write state "</")
               (export-html-write state tag)
               (export-html-write state ">")))))
      (:link
       (export-html-render-link
 state node child-views view
 (when (eq node (export-html-state-standalone-image-node state))
   (export-html-state-standalone-image-attributes state))))
      (:footnote-reference
       (export-html-footnote-reference state node child-views view))
      (:footnote-definition nil)
      (:line-break
       (export-html-write state "<br />
"))
      (:verse-block
       (export-html-write state "<p class=\"verse\">
")
       (export-html-write-node-children state node child-views view)
       (export-html-trim-output-newline state)
       (export-html-write state "
</p>
"))
      (:entity
       (let ((entity (node-property node :html)))
         (if (export-html-entity-reference-p entity)
             (export-html-write state entity)
             (export-html-escape-to-output state :text (or entity "")))))
      ((:text :plain-text)
       (export-html-write-text state (or (node-text node) "")))
      ((:babel-call :inline-babel-call :citation :citation-reference
        :latex-fragment :statistics-cookie)
       (export-html-write-raw-text state node))
      (:timestamp
       (export-html-render-timestamp state node))
      ((:target :radio-target)
       (let ((reference (export-html-reference state node)))
         (if (eq type :target)
             (progn
               (export-html-write state "<a id=\"")
               (export-html-escape-to-output state :attribute reference)
               (export-html-write state "\"></a>"))
             (progn
               (export-html-write state "<a id=\"")
               (export-html-escape-to-output state :attribute reference)
               (export-html-write state "\">")
               (export-html-write-node-children state node child-views view)
               (export-html-write state "</a>")))))
      ((:keyword :comment :comment-block :property-drawer :node-property
        :planning :clock)
       nil))))

(defun export-html-dispatch-node (state node child-views view
                                      &key (mode nil mode-p)
                                           (trim-list-indent-p nil)
                                           (image-attributes nil image-attributes-p))
  (let* ((context (export-html-state-render-context state))
         (frame (export-html-state-render-frame state))
         (mode (if mode-p mode
                   (if frame (export-render-frame-mode frame) :block)))
         (table-cell-p
           (and frame (export-render-frame-table-cell-p frame)))
         (preserve-source-breaks-p
           (and frame
                (export-render-frame-preserve-source-breaks-p frame)))
         (previous-trim (export-html-state-trim-list-indent-p state))
         (previous-image-node (export-html-state-standalone-image-node state))
         (previous-image-attributes
           (export-html-state-standalone-image-attributes state)))
    (unwind-protect
         (progn
           (setf (export-html-state-trim-list-indent-p state)
                 trim-list-indent-p
                 (export-html-state-standalone-image-node state)
                 (and image-attributes-p node)
                 (export-html-state-standalone-image-attributes state)
                 (and image-attributes-p image-attributes))
           (if (null context)
               (export-html-render-node-legacy state node child-views view)
               (let* ((output (export-html-state-output state))
                      (base-offset (export-html-state-output-offset state))
                      (dispatch-offset (+ base-offset (fill-pointer output)))
                      (result nil))
                 (unwind-protect
                      (let ((*export-current-output-offset* dispatch-offset))
                        (setf result
                              (if view
                                  (export-render-view
                                   context view :mode mode
                                   :table-cell-p table-cell-p
                                   :preserve-source-breaks-p
                                   preserve-source-breaks-p)
                                  (export-render-node
                                   context node :mode mode
                                   :table-cell-p table-cell-p
                                   :preserve-source-breaks-p
                                   preserve-source-breaks-p)))
                        (if (eq (export-render-context-backend context) :html)
                            (export-html-append-checked-result state result)
                            (export-html-write state result)))
                   (setf (export-html-state-output-offset state) base-offset)))))
      (setf (export-html-state-trim-list-indent-p state) previous-trim
            (export-html-state-standalone-image-node state) previous-image-node
            (export-html-state-standalone-image-attributes state)
            previous-image-attributes))))
(defun export-html-render-view (state view &optional (mode nil mode-p))
  (if mode-p
      (export-html-dispatch-node
       state (export-view-node view) (export-view-children view) view
       :mode mode)
      (export-html-dispatch-node
       state (export-view-node view) (export-view-children view) view)))
(defun export-html-transcode-node (node view frame context)
  (let* ((state (export-render-context-state context))
         (parent-output (export-html-state-output state))
         (parent-offset (export-html-state-output-offset state))
         (parent-frame (export-html-state-render-frame state))
         (parent-context (export-html-state-render-context state))
         (offset (max parent-offset (export-render-frame-output-offset frame)))
         (output (make-array 0 :element-type 'character
                             :adjustable t :fill-pointer 0)))
    (setf (export-html-state-preparation state)
          (org-kit::%export-render-context-preparation context)
          (export-html-state-limits state) (export-render-context-limits context)
          (export-html-state-render-context state) context
          (export-html-state-render-frame state) frame
          (export-html-state-output-offset state) offset
          (export-html-state-output state) output)
    (unwind-protect
         (progn
           (export-html-render-node-legacy
 state node (and view (export-view-children view)) view)
           (coerce (export-html-state-output state) 'string))
      (setf (export-html-state-output state) parent-output
            (export-html-state-output-offset state) parent-offset
            (export-html-state-render-frame state) parent-frame
            (export-html-state-render-context state) parent-context))))

(defun export-html-option-enabled-p (state key &optional (default t))
  (let ((entry (assoc key
                      (export-preparation-options
                       (export-html-state-preparation state)))))
    (cond ((null entry) default)
          ((and (eq key :creator) (eq (cdr entry) :comment)) t)
          (t (cdr entry)))))

(defun export-html-write-head-metadata (state)
  (let* ((metadata (export-preparation-metadata
                    (export-html-state-preparation state)))
         (title (getf metadata :title))
         (author (getf metadata :author))
         (language (or (getf metadata :language) "en")))
    (export-html-write state "<?xml version=\"1.0\" encoding=\"utf-8\"?>
")
    (export-html-write state "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
")
    (export-html-write state "<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"")
    (export-html-escape-to-output state :attribute language)
    (export-html-write state "\" xml:lang=\"")
    (export-html-escape-to-output state :attribute language)
    (export-html-write state "\">
<head>
")
    (export-html-write state "<meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\" />
")
    (export-html-write state "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />
")
    (export-html-write state "<title>")
    (export-html-escape-to-output state :text (or title ""))
    (export-html-write state "</title>
")
    (when (and author (export-html-option-enabled-p state :author))
      (export-html-write state "<meta name=\"author\" content=\"")
      (export-html-escape-to-output state :attribute author)
      (export-html-write state "\" />
"))
    (when (export-html-option-enabled-p state :creator)
      (export-html-write state "<meta name=\"generator\" content=\"Org Mode\" />
"))
    (export-html-write-default-css state)
    (export-html-write state "</head>
<body>
")))

(defun export-html-write-postamble (state)
  (let* ((metadata (export-preparation-metadata
                    (export-html-state-preparation state)))
         (date (getf metadata :date))
         (author (getf metadata :author)))
    (export-html-write state "<div id=\"postamble\" class=\"status\">
")
    (when (and date (export-html-option-enabled-p state :date))
      (export-html-write state "<p class=\"date\">Date: ")
      (export-html-escape-to-output state :text date)
      (export-html-write state "</p>
"))
    (when (and author (export-html-option-enabled-p state :author))
      (export-html-write state "<p class=\"author\">Author: ")
      (export-html-escape-to-output state :text author)
      (export-html-write state "</p>
"))
    (export-html-write
     state
     (format nil
             "<p class=\"validation\"><a href=\"https://validator.w3.org/check?uri=referer\">Validate</a></p>~%</div>~%</body>~%</html>"))))

(defun export-html-render-document (state)
  (let* ((preparation (export-html-state-preparation state))
         (title (getf (export-preparation-metadata preparation) :title)))
    (export-html-write-head-metadata state)
    (export-html-write state "<div id=\"content\" class=\"content\">
")
    (when (and title (export-html-option-enabled-p state :title))
      (export-html-write state "<h1 class=\"title\">")
      (export-html-escape-to-output state :text title)
      (export-html-write state "</h1>
"))
    (export-html-render-toc state)
    (export-html-write-views state (export-preparation-root-views preparation))
    (export-html-render-footnotes state)
    (export-html-write state "</div>
")
    (export-html-write-postamble state)
    (coerce (export-html-state-output state) 'string)))

(defmethod export-render-preparation
    ((preparation export-preparation) (backend (eql :html)))
  (let* ((state (%make-export-html-state
                 :preparation preparation
                 :limits (document-limits
                          (export-preparation-document preparation)))))
    (export-html-index-attribute-ids state)
    (export-html-index-preparation state)
    (export-html-index-footnote-ids state)
    (export-html-render-document state)))
(defun export-html-make-state (context)
  (let* ((preparation (%export-render-context-preparation context))
         (state (%make-export-html-state
                 :preparation preparation
                 :limits (export-render-context-limits context))))
    (export-html-index-attribute-ids state)
    (export-html-validate-attribute-nodes state)
    (export-html-index-preparation state)
    (export-html-index-footnote-ids state)
    state))

(defun export-html-node-transcoders ()
  (mapcar (lambda (type)
            (cons type #'export-html-transcode-node))
          (node-types)))
(defun export-html-start-render-component (state)
  (setf (export-html-state-output-offset state)
        *export-current-output-offset*
        (export-html-state-output state)
        (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
  state)
(defun export-html-render-preamble (context)
  (let* ((state (export-render-context-state context))
         (preparation (org-kit::%export-render-context-preparation context)))
    (setf (export-html-state-preparation state) preparation
          (export-html-state-limits state) (export-render-context-limits context)
          (export-html-state-render-context state) context
          (export-html-state-render-frame state) nil
          (export-html-state-output-offset state) 0
          (export-html-state-output state)
          (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)
          (export-html-state-trim-list-indent-p state) nil
          (export-html-state-standalone-image-node state) nil
          (export-html-state-standalone-image-attributes state) nil)
    (export-html-write-head-metadata state)
    (coerce (export-html-state-output state) 'string)))
(defun export-html-render-body (context)
  (let* ((state (export-render-context-state context))
         (preparation (org-kit::%export-render-context-preparation context))
         (title (getf (export-preparation-metadata preparation) :title)))
    (setf (export-html-state-preparation state) preparation
          (export-html-state-limits state) (export-render-context-limits context)
          (export-html-state-render-context state) context)
    (export-html-start-render-component state)
    (export-html-write state "<div id=\"content\" class=\"content\">
")
    (when (and title (export-html-option-enabled-p state :title))
      (export-html-write state "<h1 class=\"title\">")
      (export-html-escape-to-output state :text title)
      (export-html-write state "</h1>
"))
    (export-html-render-toc state)
    (export-html-write-views state (export-preparation-root-views preparation))
    (export-html-render-footnotes state)
    (coerce (export-html-state-output state) 'string)))
(defun export-html-render-trailer (context)
  (let* ((state (export-render-context-state context))
         (preparation (org-kit::%export-render-context-preparation context)))
    (setf (export-html-state-preparation state) preparation
          (export-html-state-limits state) (export-render-context-limits context)
          (export-html-state-render-context state) context)
    (export-html-start-render-component state)
    (export-html-write state "</div>
")
    (export-html-write-postamble state)
    (coerce (export-html-state-output state) 'string)))

(defun export-html-node-policies ()
  '((:diary-sexp :unsupported
                 :feature :diary-sexp
                 :message "Diary sexp export is unsupported")))

(eval-when (:load-toplevel :execute)
  (%register-builtin-export-backend
   :html
   :renderer #'export-html-render-body
   :preamble-renderer #'export-html-render-preamble
   :trailer-renderer #'export-html-render-trailer
   :make-state #'export-html-make-state
   :transcoders (export-html-node-transcoders)
   :node-policies (export-html-node-policies)
   :raw-output-options '((:html . :allow-raw-html))
   :export-block-names '("html")
   :export-snippet-names '("html")))

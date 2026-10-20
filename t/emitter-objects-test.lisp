;;;; t/emitter-objects-test.lisp
(in-package #:cl-org-kit/test)

(defun emitter-object-test-node (type properties &optional children)
  (make-node type :properties properties :children children))

(defun emitter-object-test-output (type properties &optional children &key (post-blank 0))
  (let ((org-kit::*emitter-current-limits* (make-limits))
        (org-kit::*emitter-node-count* 0)
        (org-kit::*emitter-object-depth* 0))
    (org-kit::emitter-object-child-string
     (make-node type :properties properties :children children :post-blank post-blank))))

(defun emitter-object-test-rejects-output (type properties &optional children &key (post-blank 0))
  (signals org-kit-error
    (emitter-object-test-output type properties children :post-blank post-blank)))

(defun emitter-object-test-attached-root (source type properties post-blank)
  (let* ((document (parse-org source))
         (root (document-root document))
         (end (length source))
         (object (make-node type :begin 0 :end end :post-blank post-blank
                            :properties properties))
         (paragraph (make-node :paragraph :begin 0 :end end
                               :children (list object)))
         (section (make-node :section :begin 0 :end end
                             :contents-begin 0 :contents-end end
                             :children (list paragraph))))
    (setf (node-children root) (list section))
    root))

(defun emitter-object-test-timestamp-properties (&optional (type :active))
  (list :type type
        :year-start 2026 :month-start 10 :day-start 13
        :year-end 2026 :month-end 10 :day-end 13))

(defun emitter-object-test-timestamp-output (properties)
  (emitter-object-test-output
   :timestamp (append properties (emitter-object-test-timestamp-properties))))

(defun emitter-object-test-root (type object)
  (let ((elements
          (case type
            (:plain-text
             (return-from emitter-object-test-root object))
            (:citation-reference
             (list (emitter-object-test-node
                    :citation
                    '(:style "t" :key nil)
                    (list object))))
            (:table-cell
             (return-from emitter-object-test-root
               (make-node
                :org-data
                :children
                (list
                 (make-node
                  :section
                  :children
                  (list
                   (make-node
                    :table
                    :properties '(:type :org)
                    :children
                    (list
                     (make-node :table-row
                                :properties '(:type :standard)
                                :children (list object))))))))))
            ((:subscript :superscript)
             (list (emitter-object-test-node
                    :plain-text '(:value "H"))
                   object))
            (:line-break
             (list (emitter-object-test-node :plain-text '(:value "before "))
                   object
                   (emitter-object-test-node :plain-text '(:value "after"))))
            (:footnote-reference
             (list (emitter-object-test-node :plain-text '(:value "reference "))
                   object))
            (otherwise (list object)))))
    (make-node
     :org-data
     :children
     (list (make-node :section
                      :children
                      (list (make-node :paragraph :children elements)))))))

(defun emitter-object-test-property-keys (type)
  (case type
    (:citation '(:style :key))
    (:citation-reference '(:key :prefix :suffix :locator))
    (:entity '(:name :use-brackets-p))
    (:export-snippet '(:back-end :value))
    (:footnote-reference '(:label :type))
    (:inline-babel-call '(:call :inside-header :arguments :end-header))
    (:inline-src-block '(:language :value :parameters))
    (:link '(:type :path :format))
    (:macro '(:key :args))
    (:plain-text nil)
    (:radio-target '(:value))
    (:statistics-cookie '(:value :type))
    (:subscript '(:use-brackets-p))
    (:superscript '(:use-brackets-p))
    (:table-cell '(:value))
    (:target '(:value))
    (:timestamp '(:type :year-start :month-start :day-start
                  :hour-start :minute-start :year-end :month-end :day-end
                  :hour-end :minute-end :repeater-type :repeater-value
                  :repeater-unit :warning-type :warning-value :warning-unit))
    (otherwise nil)))

(defun emitter-object-test-string (node)
  (or (node-text node) (node-property node :value)))

(defun emitter-object-test-signature (node)
  (let ((type (node-type node)))
    (list type
          (loop for key in (emitter-object-test-property-keys type)
                collect (node-property node key))
          (when (eq type :plain-text)
            (emitter-object-test-string node))
          (mapcar #'emitter-object-test-signature (node-children node)))))

(defun emitter-object-test-find (root type)
  (let ((found nil))
    (map-nodes (lambda (node)
                 (when (and (null found) (eq (node-type node) type))
                   (setf found node)))
               root)
    found))

(defun emitter-object-test-cases ()
  (list
   (cons :bold
         (emitter-object-test-node
          :bold nil (list (emitter-object-test-node :plain-text '(:value "bold")))))
   (cons :citation
         (emitter-object-test-node
          :citation '(:style "t" :key nil)
          (list
           (emitter-object-test-node
            :citation-reference '(:key "smith2024" :prefix nil :suffix nil :locator nil))
           (emitter-object-test-node
            :citation-reference '(:key "doe2025" :prefix nil
                                  :suffix (" p. 4") :locator nil)))))
   (cons :citation-reference
         (emitter-object-test-node
          :citation-reference '(:key "smith2024" :prefix nil :suffix nil :locator nil)))
   (cons :code
         (emitter-object-test-node :code '(:value "literal code")))
   (cons :entity
         (emitter-object-test-node
          :entity '(:name "alpha" :use-brackets-p t)))
   (cons :export-snippet
         (emitter-object-test-node
          :export-snippet '(:back-end "html" :value "<mark>inline</mark>")))
   (cons :footnote-reference
         (emitter-object-test-node
          :footnote-reference '(:label "short" :type :standard)))
   (cons :inline-babel-call
         (emitter-object-test-node
          :inline-babel-call '(:call "sum" :inside-header "lang=lisp"
                                :arguments "1, 2" :end-header nil)))
   (cons :inline-src-block
         (emitter-object-test-node
          :inline-src-block '(:language "lisp" :value "(+ 1 2)"
                              :parameters ":results raw")))
   (cons :italic
         (emitter-object-test-node
          :italic nil (list (emitter-object-test-node :plain-text '(:value "italic")))))
   (cons :latex-fragment
         (emitter-object-test-node :latex-fragment '(:value "\\(x + y\\)")))
   (cons :line-break
         (emitter-object-test-node :line-break nil))
   (cons :link
         (emitter-object-test-node
          :link '(:type "https" :path "example.test" :format :bracket)
          (list (emitter-object-test-node :plain-text '(:value "site")))))
   (cons :macro
         (emitter-object-test-node :macro '(:key "greet" :args ("Ada"))))
   (cons :plain-text
         (emitter-object-test-node :plain-text '(:value "generated plain text")))
   (cons :radio-target
         (let ((value "radio target"))
           (emitter-object-test-node
            :radio-target `(:value ,value)
            (list (emitter-object-test-node :plain-text `(:value ,value))))))
   (cons :statistics-cookie
         (emitter-object-test-node
          :statistics-cookie '(:value "[40%]" :type nil)))
   (cons :strike-through
         (emitter-object-test-node
          :strike-through nil
          (list (emitter-object-test-node :plain-text '(:value "removed")))))
   (cons :subscript
         (emitter-object-test-node
          :subscript '(:use-brackets-p t)
          (list (emitter-object-test-node :plain-text '(:value "i+1")))))
   (cons :superscript
         (emitter-object-test-node
          :superscript '(:use-brackets-p nil)
          (list (emitter-object-test-node :plain-text '(:value "3")))))
   (cons :table-cell
         (emitter-object-test-node
          :table-cell '(:value "cell *bold*")
          (list (emitter-object-test-node :plain-text '(:value "cell "))
                (emitter-object-test-node
                 :bold nil
                 (list (emitter-object-test-node :plain-text '(:value "bold")))))))
   (cons :target
         (emitter-object-test-node :target '(:value "local-anchor")))
   (cons :timestamp
         (emitter-object-test-node
          :timestamp '(:type :active
                       :year-start 2026 :month-start 10 :day-start 12
                       :hour-start 9 :minute-start 30
                       :year-end 2026 :month-end 10 :day-end 12
                       :hour-end 9 :minute-end 30
                       :repeater-type :cumulate :repeater-value 1
                       :repeater-unit :week
                       :warning-type :all :warning-value 2 :warning-unit :day)))
   (cons :underline
         (emitter-object-test-node
          :underline nil
          (list (emitter-object-test-node :plain-text '(:value "underlined")))))
   (cons :verbatim
         (emitter-object-test-node :verbatim '(:value "literal text")))))

(defun emitter-object-test-round-trip (type object)
  (let* ((root (emitter-object-test-root type object))
         (output (emit-org root))
         (document (parse-org output))
         (parsed (emitter-object-test-find (document-root document) type)))
    (unless parsed
      (error "Object type ~S disappeared after emission:~%~A" type output))
    (values (emitter-object-test-signature object)
            (emitter-object-test-signature parsed)
            output)))

(describe "P6 inline object emission"
  (it "round trips every object type with its structural properties"
    (let* ((cases (emitter-object-test-cases))
           (types (mapcar #'car cases))
           (inventory (node-types :object)))
      (expect (equal (sort (copy-list types) #'string< :key #'symbol-name)
                     (sort (copy-list inventory) #'string< :key #'symbol-name))
              :to-be t)
      (dolist (case cases)
        (destructuring-bind (type . object) case
          (multiple-value-bind (expected actual output)
              (emitter-object-test-round-trip type object)
            (unless (equal expected actual)
              (error "Object type ~S changed after emission.~%Expected: ~S~%Actual: ~S~%Output:~%~A"
                     type expected actual output))
            (expect actual :to-equal expected))))
      (expect (emitter-object-test-output :code '(:value "literal code"))
              :to-equal "~literal code~")))

  (it "round trips parsed code and verbatim payloads from their delimited source"
    (let* ((document (parse-org "Parsed =verbatim=, then ~code~."))
           (output (emit-org (document-root document)))
           (round-trip (parse-org output))
           (verbatim (emitter-object-test-find (document-root round-trip) :verbatim))
           (code (emitter-object-test-find (document-root round-trip) :code)))
      (expect (not (null (search "=verbatim=" output))) :to-be t)
      (expect (not (null (search "~code~" output))) :to-be t)
      (expect (and verbatim (equal (node-text verbatim) "=verbatim=")) :to-be t)
      (expect (and code (equal (node-text code) "~code~")) :to-be t)
      (expect
       (not
        (null
         (search "=generated verbatim="
                 (emit-org
                  (emitter-object-test-root
                   :verbatim
                   (emitter-object-test-node
                    :verbatim '(:value "generated verbatim")))))))
       :to-be t)))

  (it "round trips timestamps with lexically valid but calendar-invalid dates"
    (dolist (source '("<2026-13-99>" "[2026-00-00]"))
      (let* ((document (parse-org source))
             (output (emit-org (document-root document)))
             (round-trip (parse-org output))
             (timestamp
               (emitter-object-test-find (document-root document) :timestamp))
             (reparsed
               (emitter-object-test-find (document-root round-trip) :timestamp)))
        (expect (and timestamp reparsed
                     (equal (emitter-object-test-signature timestamp)
                            (emitter-object-test-signature reparsed)))
                :to-be t)
        (expect (not (null (search source output))) :to-be t))))

  (it "emits radio links only when a matching target is present"
    (let* ((value "radio target")
           (target (emitter-object-test-node
                    :radio-target `(:value ,value)
                    (list (emitter-object-test-node :plain-text `(:value ,value)))))
           (link (emitter-object-test-node
                  :link `(:type "radio" :path ,value :format :plain)
                  (list (emitter-object-test-node :plain-text `(:value ,value)))))
           (root (make-node
                  :org-data
                  :children
                  (list (make-node
                         :section
                         :children
                         (list (make-node
                                :paragraph
                                :children
                                (list target
                                      (emitter-object-test-node
                                       :plain-text '(:value " occurrence "))
                                      link)))))))
           (output (emit-org root))
           (parsed (parse-org output))
           (found (emitter-object-test-find (document-root parsed) :link)))
      (expect (and found
                   (equal (emitter-object-test-signature link)
                          (emitter-object-test-signature found)))
              :to-be t)))

  (it "rejects plain text that would become an object or an element"
    (let ((unsafe-object
            (emitter-object-test-root
             :plain-text
             (emitter-object-test-node :plain-text '(:value "a *bold* phrase"))))
          (unsafe-element
            (emitter-object-test-root
             :plain-text
             (emitter-object-test-node
              :plain-text `(:value ,(format nil "#+TITLE: parsed as a keyword~%"))))))
      (signals org-kit-error (emit-org unsafe-object))
      (signals org-kit-error (emit-org unsafe-element))))

  (it "rejects plain text beginning with other recognized line elements"
    (dolist (value '("** parsed as a heading"
                     "CLOCK: [2026-10-13 Tue 09:00]"
                     "%%(diary-sexp)"
                     "| parsed as a table row |"
                     ": drawer:"
                     ":name:"
                     "-----"
                     "- parsed as a list item"
                     "3. parsed as an ordered list item"
                     "12. parsed as a multi-digit ordered list item"
                     "4) parsed as an ordered list item"))
      (signals org-kit-error
        (emit-org
         (emitter-object-test-root
          :plain-text
          (emitter-object-test-node :plain-text (list :value value)))))))

  (it "rejects text that would silently create a radio link"
    (let* ((value "radio target")
           (target (emitter-object-test-node
                    :radio-target `(:value ,value)
                    (list (emitter-object-test-node :plain-text `(:value ,value)))))
           (root (make-node
                  :org-data
                  :children
                  (list (make-node
                         :section
                         :children
                         (list (make-node
                                :paragraph
                                :children
                                (list target
                                      (emitter-object-test-node
                                       :plain-text
                                       '(:value "an occurrence: radio target")))))))))
           (parsed (parse-org (format nil
                                      "<<<~A>>> an occurrence: ~A~%"
                                      value value)))
           (found (emitter-object-test-find (document-root parsed) :link)))
      (expect (and found
                   (equal (node-property found :type) "radio")
                   (equal (node-property found :path) value))
              :to-be t)
      (signals org-kit-error (emit-org root))))

  (it "rejects object payloads whose delimiters cannot preserve the tree"
    (let ((bad-markup
            (emitter-object-test-root
             :bold
             (emitter-object-test-node
              :bold nil
              (list (emitter-object-test-node :plain-text '(:value " spaced"))))))
          (bad-target
            (emitter-object-test-root
             :target
             (emitter-object-test-node :target '(:value "anchor>>")))))
      (signals org-kit-error (emit-org bad-markup))
      (signals org-kit-error (emit-org bad-target))))

  (it "rejects detached plain text that cannot be represented as an object"
    (emitter-object-test-rejects-output :plain-text nil)
    (emitter-object-test-rejects-output :plain-text '(:value nil))
    (emitter-object-test-rejects-output :plain-text '(:value ""))
    (emitter-object-test-rejects-output
     :plain-text '(:value "plain")
     (list (emitter-object-test-node :plain-text '(:value "child"))))
    (emitter-object-test-rejects-output
     :plain-text (list :value (format nil "bare-return~C" #\Return))))

  (it "normalizes CRLF in plain text and rejects element syntax on later lines"
    (expect (emitter-object-test-output
             :plain-text (list :value (format nil "first~C~%second" #\Return)))
            :to-equal (format nil "first~%second"))
    (dolist (value (list (format nil "first~%#+TITLE: keyword")
                         (format nil "first~%** heading")
                         (format nil "first~%| table |")))
      (emitter-object-test-rejects-output :plain-text (list :value value)))
    (expect (emitter-object-test-output :plain-text '(:value "*literal"))
            :to-equal "*literal"))

  (it "rejects markup objects with opaque delimiters or non-object children"
    (emitter-object-test-rejects-output
     :code '(:value "contains~delimiter"))
    (emitter-object-test-rejects-output :code '(:value nil))
    (emitter-object-test-rejects-output :code nil)
    (expect (emit-org (emitter-object-test-attached-root "~x~ " :code nil 1))
            :to-equal "~x~ ")
    (signals org-kit-error
      (emit-org (emitter-object-test-attached-root "x" :code nil 0)))
    (signals org-kit-error
      (emit-org (emitter-object-test-attached-root "~x~?" :code nil 1)))
    (signals org-kit-error
      (emit-org (emitter-object-test-attached-root "x" :code nil 2)))
    (emitter-object-test-rejects-output
     :code '(:value "payload")
     (list (emitter-object-test-node :plain-text '(:value "child"))))
    (emitter-object-test-rejects-output
     :bold nil
     (list (emitter-object-test-node :paragraph nil))))

  (it "rejects malformed subscript and superscript payloads"
    (emitter-object-test-rejects-output :subscript '(:use-brackets-p "yes")
                                        (list (emitter-object-test-node :plain-text '(:value "i"))))
    (emitter-object-test-rejects-output :subscript '(:use-brackets-p nil))
    (emitter-object-test-rejects-output
     :subscript '(:use-brackets-p nil)
     (list (emitter-object-test-node :plain-text '(:value "+-"))))
    (emitter-object-test-rejects-output :subscript '(:use-brackets-p nil)
                                        (list (emitter-object-test-node :plain-text '(:value "i+"))))
    (emitter-object-test-rejects-output :superscript '(:use-brackets-p nil)
                                        (list (emitter-object-test-node :plain-text '(:value "x-y"))))
    (emitter-object-test-rejects-output :subscript '(:use-brackets-p nil)
                                        (list (emitter-object-test-node :plain-text '(:value "a_b"))))
    (emitter-object-test-rejects-output :subscript '(:use-brackets-p t)
                                        (list (emitter-object-test-node :plain-text '(:value "x}"))))
    (expect (emitter-object-test-output
             :subscript '(:use-brackets-p nil)
             (list (emitter-object-test-node :plain-text '(:value "+n"))))
            :to-equal "_+n")
    (expect (emitter-object-test-output
             :superscript '(:use-brackets-p nil)
             (list (emitter-object-test-node :plain-text '(:value "-n"))))
            :to-equal "^-n")
    (expect (emitter-object-test-output
             :subscript '(:use-brackets-p nil)
             (list (emitter-object-test-node :plain-text '(:value "x.y,z"))))
            :to-equal "_x.y,z")
    (let* ((content (concatenate 'string "x" (string #\\) "}y"))
           (object (list (emitter-object-test-node :plain-text (list :value content)))))
      (expect (emitter-object-test-output :subscript '(:use-brackets-p t) object)
              :to-equal (format nil "_{~A}" content)))
    (emitter-object-test-rejects-output
     :subscript '(:use-brackets-p t)
     (list (emitter-object-test-node
            :plain-text (list :value (concatenate 'string "x" (string #\\)))))))

  (it "validates entity names, bracket flags, and child restrictions"
    (expect (emitter-object-test-output :entity '(:name "alpha" :use-brackets-p nil))
            :to-equal "\\alpha")
    (emitter-object-test-rejects-output :entity '(:name "not-an-entity"))
    (emitter-object-test-rejects-output :entity '(:name "alpha" :use-brackets-p :yes))
    (emitter-object-test-rejects-output
     :entity '(:name "alpha")
     (list (emitter-object-test-node :plain-text '(:value "child")))))

  (it "rejects export snippets whose fields would close or corrupt the form"
    (dolist (backend '("" "html_5" "html backend"))
      (emitter-object-test-rejects-output
       :export-snippet (list :back-end backend :value "content")))
    (dolist (value (list "close @@ marker"
                         (format nil "line~%break")
                         (format nil "carriage~Creturn" #\Return)))
      (emitter-object-test-rejects-output
       :export-snippet (list :back-end "html" :value value)))
    (emitter-object-test-rejects-output
     :export-snippet '(:back-end "html" :value "content")
     (list (emitter-object-test-node :plain-text '(:value "child")))))

  (it "preserves inline footnotes and rejects malformed references"
    (expect (emitter-object-test-output
             :footnote-reference '(:type :inline :label "note")
             (list (emitter-object-test-node :plain-text '(:value "inline note"))))
            :to-equal "[fn:note:inline note]")
    (emitter-object-test-rejects-output :footnote-reference '(:type :other :label "note"))
    (emitter-object-test-rejects-output :footnote-reference '(:type :standard))
    (emitter-object-test-rejects-output :footnote-reference '(:type :standard :label "bad label"))
    (emitter-object-test-rejects-output :footnote-reference '(:type :standard :label 3))
    (emitter-object-test-rejects-output :footnote-reference '(:type :inline))
    (emitter-object-test-rejects-output :footnote-reference '(:type :inline)
                                        (list (emitter-object-test-node :plain-text '(:value ""))))
    (emitter-object-test-rejects-output :footnote-reference '(:type :inline)
                                        (list (emitter-object-test-node :plain-text '(:value "bad ]")))))

  (it "validates Babel call headers and arguments"
    (expect (emitter-object-test-output
             :inline-babel-call '(:call "sum" :inside-header nil
                                  :arguments nil :end-header nil))
            :to-equal "call_sum()")
    (expect (emitter-object-test-output
             :inline-babel-call '(:call "sum" :inside-header "lang=lisp"
                                  :arguments "1, 2" :end-header "var=total"))
            :to-equal "call_sum[lang=lisp](1, 2)[var=total]")
    (dolist (call '("" "sum call" "sum[x]"))
      (emitter-object-test-rejects-output
       :inline-babel-call (list :call call)))
    (dolist (header (list "bad]header"
                          (format nil "line~%break")
                          (format nil "carriage~Creturn" #\Return)))
      (emitter-object-test-rejects-output
       :inline-babel-call (list :call "sum" :inside-header header))
      (emitter-object-test-rejects-output
       :inline-babel-call (list :call "sum" :end-header header)))
    (emitter-object-test-rejects-output
     :inline-babel-call '(:call "sum" :arguments "(unclosed"))
    (emitter-object-test-rejects-output
     :inline-babel-call '(:call "sum" :arguments 4))
    (emitter-object-test-rejects-output
     :inline-babel-call '(:call "sum" :inside-header ""))
    (emitter-object-test-rejects-output
     :inline-babel-call '(:call "sum")
     (list (emitter-object-test-node :plain-text '(:value "child")))))

  (it "validates inline source block fields"
    (expect (emitter-object-test-output
             :inline-src-block '(:language "lisp" :value "(+ 1 2)" :parameters nil))
            :to-equal "src_lisp{(+ 1 2)}")
    (dolist (language '("" "bad language" "bad{language"))
      (emitter-object-test-rejects-output
       :inline-src-block (list :language language :value "body")))
    (dolist (parameters (list "bad]params"
                              (format nil "line~%break")
                              (format nil "carriage~Creturn" #\Return)
                              ""))
      (emitter-object-test-rejects-output
       :inline-src-block (list :language "lisp" :value "body" :parameters parameters)))
    (emitter-object-test-rejects-output
     :inline-src-block '(:language "lisp" :value "{unclosed")))

  (it "emits each supported LaTeX delimiter form and rejects entity collisions"
    (dolist (value '("$$x + y$$" "$x + y$" "\\(x + y\\)" "\\[x + y\\]" "\\unknown"))
      (expect (emitter-object-test-output :latex-fragment (list :value value))
              :to-equal value))
    (dolist (value (list "" "$" "$$x$" "\\(x" "\\alpha"
                         (format nil "line~%break")
                         (format nil "carriage~Cbreak" #\Return)))
      (emitter-object-test-rejects-output :latex-fragment (list :value value))))

  (it "emits angle, plain, and escaped bracket links without changing link fields"
    (dolist (case '((:angle "https" "example.test" "<https:example.test>")
                    (:plain "https" "example.test" "https:example.test")
                    (:bracket "custom-id" "local-id" "[[#local-id]]")
                    (:bracket "coderef" "ref" "[[(ref)]]")
                    (:bracket "fuzzy" "Somewhere" "[[Somewhere]]")))
      (destructuring-bind (format type path expected) case
        (expect (emitter-object-test-output
                 :link (list :type type :path path :format format))
                :to-equal expected)))
    (let* ((link (emitter-object-test-node
                  :link '(:type "https" :path "a[b]" :format :bracket)))
           (emitted (emitter-object-test-root :link link))
           (output (emit-org emitted))
           (round-trip (parse-org output))
           (parsed-link (emitter-object-test-find (document-root round-trip) :link)))
      (expect (and parsed-link
                   (equal (node-property parsed-link :path) "a[b]"))
              :to-be t)))

  (it "rejects links whose format or path would change their meaning"
    (emitter-object-test-rejects-output :link '(:type "HTTPS" :path "example.test" :format :plain))
    (emitter-object-test-rejects-output :link '(:type "https" :path "" :format :plain))
    (emitter-object-test-rejects-output :link '(:type "https" :path "example.test" :format :unknown))
    (emitter-object-test-rejects-output :link '(:type "mystery" :path "path" :format :bracket))
    (emitter-object-test-rejects-output :link '(:type "https" :path "example.test." :format :plain))
    (emitter-object-test-rejects-output :link '(:type "https" :path "path with space" :format :plain))
    (emitter-object-test-rejects-output :link '(:type "mystery" :path "path" :format :plain))
    (emitter-object-test-rejects-output
     :link '(:type "https" :path "path" :format :bracket)
     (list (emitter-object-test-node :table-cell '(:value nil))))
    (emitter-object-test-rejects-output
     :link '(:type "https" :path "example.test" :format :plain)
     (list (emitter-object-test-node :plain-text '(:value "description"))))
    (emitter-object-test-rejects-output
     :link '(:type "radio" :path "target" :format :plain)
     (list (emitter-object-test-node :plain-text '(:value "different description"))))
    (emitter-object-test-rejects-output
     :link '(:type "https" :path "example.test" :format :angle)
     (list (emitter-object-test-node :plain-text '(:value "description"))))
    (emitter-object-test-rejects-output
     :link '(:type "https" :path "bad>path" :format :angle))
    (emitter-object-test-rejects-output
     :link (list :type "https" :path (format nil "bad~%path") :format :angle))
    (emitter-object-test-rejects-output
     :link (list :type "https" :path (format nil "bad~Cpath" #\Return)
                 :format :angle))
    (emitter-object-test-rejects-output
     :link (list :type "https" :path (format nil "bad~%path") :format :bracket))
    (emitter-object-test-rejects-output
     :link '(:type "https" :path "example.test" :format :bracket)
     (list (emitter-object-test-node :plain-text '(:value "broken ] description"))))
    (dolist (path '("#local" "/absolute" "./relative" "../relative"
                    "https:example.test" "(reference)"))
      (emitter-object-test-rejects-output
       :link (list :type "fuzzy" :path path :format :bracket)))
    (emitter-object-test-rejects-output
     :link '(:type "radio" :path "missing" :format :plain)))

  (it "validates macro names and argument lists"
    (expect (emitter-object-test-output :macro '(:key "greet" :args nil))
            :to-equal "{{{greet}}}")
    (dolist (key '("" "Greeting" "2greet" "bad key"))
      (emitter-object-test-rejects-output :macro (list :key key)))
    (dolist (arguments (list #("not a list") '("two,parts") '("close }}} marker")
                             (list (format nil "new~%line"))
                             (list (format nil "carriage~Cline" #\Return))))
      (emitter-object-test-rejects-output
       :macro (list :key "greet" :args arguments)))
    (emitter-object-test-rejects-output
     :macro '(:key "greet")
     (list (emitter-object-test-node :plain-text '(:value "child")))))

  (it "rejects malformed radio targets and statistics cookies"
    (dolist (value '("" " leading" "trailing " "has<angle>"
                     "<angle>" "has<angle>x"))
      (emitter-object-test-rejects-output
       :radio-target (list :value value)
       (list (emitter-object-test-node :plain-text (list :value value)))))
    (emitter-object-test-rejects-output
     :radio-target '(:value "target")
     (list (emitter-object-test-node :plain-text '(:value "different"))))
    (dolist (value '("[]" "x40]" "40%" "[4/4/4]" "[abc/3]" "[40]" "[4%%]" "[40%]extra"))
      (emitter-object-test-rejects-output
       :statistics-cookie (list :value value :type nil)))
    (emitter-object-test-rejects-output
     :statistics-cookie '(:value "[40%]" :type :percent)))

  (it "rejects table cells and targets with unrepresentable contents"
    (expect (emitter-object-test-output :table-cell '(:value "raw source value"))
            :to-equal "")
    (emitter-object-test-rejects-output :table-cell '(:value 42))
    (expect (emitter-object-test-output
             :table-cell '(:value "raw timestamp <2026-10-12 Mon>")
             (list (emitter-object-test-node
                    :plain-text '(:value "raw timestamp "))
                   (emitter-object-test-node
                    :timestamp
                    '(:type :active
                      :year-start 2026 :month-start 10 :day-start 12
                      :year-end 2026 :month-end 10 :day-end 12))))
            :to-equal "raw timestamp <2026-10-12>")
    (dolist (value (list "a|b" (format nil "a~%b")
                         (format nil "a~Cb" #\Return) " leading" "trailing "))
      (emitter-object-test-rejects-output
       :table-cell '(:value nil)
       (list (emitter-object-test-node :plain-text (list :value value)))))
    (dolist (value (list "" " leading" "trailing " "<angle>"
                         (format nil "line~%break")))
      (emitter-object-test-rejects-output :target (list :value value)))
    (emitter-object-test-rejects-output
     :target '(:value "target")
     (list (emitter-object-test-node :plain-text '(:value "child")))))

  (it "validates timestamps, ranges, diaries, and calendar fields"
    (expect (emitter-object-test-timestamp-output nil)
            :to-equal "<2026-10-13>")
    (expect (emitter-object-test-output
             :timestamp
             '(:type :active-range
               :year-start 2026 :month-start 10 :day-start 13
               :hour-start 9 :minute-start 30
               :year-end 2026 :month-end 10 :day-end 13
               :hour-end 10 :minute-end 0))
            :to-equal "<2026-10-13 09:30-10:00>")
    (expect (emitter-object-test-output
             :timestamp
             '(:type :inactive-range
               :year-start 2026 :month-start 10 :day-start 13
               :year-end 2026 :month-end 10 :day-end 14))
            :to-equal "[2026-10-13]--[2026-10-14]")
    (expect (emitter-object-test-output
             :timestamp
             '(:type :active-range
               :year-start 2026 :month-start 10 :day-start 13
               :hour-start 9 :minute-start 0
               :year-end 2026 :month-end 10 :day-end 14
               :hour-end 10 :minute-end 0))
            :to-equal "<2026-10-13 09:00>--<2026-10-14 10:00>")
    (dolist (properties '((:type :active :year-start nil)
                          (:type :active :year-start 10000)
                          (:type :active :month-start 100)
                          (:type :active :day-start -1)
                          (:type :active :hour-start 9)
                          (:type :active :minute-start 60)
                          (:type :active :hour-start 24 :minute-start 0)
                          (:type :active :year-end 2025)
                          (:type :other)))
      (emitter-object-test-rejects-output
       :timestamp
       (append properties (emitter-object-test-timestamp-properties))))
    (expect (emitter-object-test-output
             :timestamp '(:type :diary :value "<2026-10-13>"))
            :to-equal "<2026-10-13>")
    (expect (emitter-object-test-output
             :timestamp '(:type :diary :value "%%(diary-anniversary 1 1)"))
            :to-equal "<%%(diary-anniversary 1 1)>")
    (signals org-kit-error
      (emit-org
       (emitter-object-test-attached-root
        "source" :timestamp '(:type :diary :value 7) 0)))
    (dolist (properties '(nil
                          (:type :diary)
                          (:type :diary :value "not a diary expression")
                          (:type :diary :value "%%(diary)" :hour-start 9)
                          (:type :diary :value "%%(diary)" :minute-start 30)
                          (:type :diary :value "%%(diary)" :hour-end 10)
                          (:type :diary :value "%%(diary)" :minute-end 15)
                          (:type :diary :value "%%(diary)" :repeater-type :cumulate)
                          (:type :diary :value "%%(diary)" :warning-type :all)))
      (emitter-object-test-rejects-output :timestamp properties))
    (emitter-object-test-rejects-output
     :timestamp (list :type :diary :value (format nil "%%(diary~%expression)"))))

  (it "emits every supported timestamp adjustment and rejects incomplete fields"
    (dolist (case '((:cumulate :hour "+" #\h)
                    (:cumulate :day "+" #\d)
                    (:cumulate :week "+" #\w)
                    (:cumulate :month "+" #\m)
                    (:cumulate :year "+" #\y)
                    (:catch-up :day "++" #\d)
                    (:restart :month ".+" #\m)))
      (destructuring-bind (mode unit prefix suffix) case
        (expect (emitter-object-test-timestamp-output
                 (list :repeater-type mode :repeater-value 2 :repeater-unit unit))
                :to-equal (format nil "<2026-10-13 ~A2~C>" prefix suffix))))
    (dolist (case '((:all :hour "-" #\h)
                    (:all :day "-" #\d)
                    (:all :week "-" #\w)
                    (:all :month "-" #\m)
                    (:all :year "-" #\y)
                    (:first :day "--" #\d)))
      (destructuring-bind (mode unit prefix suffix) case
        (expect (emitter-object-test-timestamp-output
                 (list :warning-type mode :warning-value 2 :warning-unit unit))
                :to-equal (format nil "<2026-10-13 ~A2~C>" prefix suffix))))
    (dolist (properties '((:repeater-type :unknown :repeater-value 1 :repeater-unit :day)
                          (:repeater-type :cumulate :repeater-value 0 :repeater-unit :day)
                          (:repeater-type :cumulate :repeater-value 1 :repeater-unit :unknown)
                          (:repeater-value 1)
                          (:repeater-unit :day)
                          (:warning-type :unknown :warning-value 1 :warning-unit :day)
                          (:warning-type :all :warning-value 0 :warning-unit :day)
                          (:warning-value 1)
                          (:warning-unit :day)))
      (emitter-object-test-rejects-output
       :timestamp (append properties (emitter-object-test-timestamp-properties)))))

  (it "validates citation children, keys, styles, and suffixes"
    (expect (emitter-object-test-output
             :citation '(:style nil :key nil)
             (list (emitter-object-test-node
                    :citation-reference '(:key "smith2024" :prefix nil
                                          :suffix nil :locator nil))))
            :to-equal "[cite: @smith2024]")
    (emitter-object-test-rejects-output
     :citation '(:style 3 :key nil)
     (list (emitter-object-test-node :citation-reference '(:key "key"))))
    (emitter-object-test-rejects-output
     :citation '(:style "bad style" :key nil)
     (list (emitter-object-test-node :citation-reference '(:key "key"))))
    (emitter-object-test-rejects-output
     :citation '(:style nil :key "not-retained")
     (list (emitter-object-test-node :citation-reference '(:key "key"))))
    (emitter-object-test-rejects-output :citation '(:style nil :key nil))
    (emitter-object-test-rejects-output
     :citation '(:style nil :key nil)
     (list (emitter-object-test-node :plain-text '(:value "not a reference"))))
    (emitter-object-test-rejects-output :citation-reference '(:key "key"))
    (dolist (properties '((:key "" :prefix nil :suffix nil :locator nil)
                          (:key "key" :prefix "prefix" :suffix nil :locator nil)
                          (:key "key" :prefix nil :suffix nil :locator "p. 4")
                          (:key "bad key" :prefix nil :suffix nil :locator nil)
                          (:key "key" :prefix nil :suffix ("bad; suffix") :locator nil)
                          (:key "key" :prefix nil :suffix ("bad] suffix") :locator nil)
                          (:key "key" :prefix nil :suffix :not-a-list :locator nil)))
      (emitter-object-test-rejects-output
       :citation '(:style nil :key nil)
       (list (emitter-object-test-node :citation-reference properties))))
    (dolist (suffix (list (format nil "bad~%suffix")
                          (format nil "bad~C suffix" #\Return)))
      (emitter-object-test-rejects-output
       :citation '(:style nil :key nil)
       (list (emitter-object-test-node
              :citation-reference
              (list :key "key" :prefix nil :suffix (list suffix) :locator nil))))))

  (it "rejects invalid object nodes and post-blank spans"
    (signals org-kit-error
      (emit-org (make-node :entity :properties (list :name nil))))
    (emitter-object-test-rejects-output :table-cell '(:value nil) nil :post-blank 1)
    (expect (emitter-object-test-output :bold nil
                                        (list (emitter-object-test-node
                                               :plain-text '(:value "bold")))
                                        :post-blank 1)
            :to-equal "*bold* ")))

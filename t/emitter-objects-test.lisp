;;;; t/emitter-objects-test.lisp
(in-package #:cl-org-kit/test)

(defun emitter-object-test-node (type properties &optional children)
  (make-node type :properties properties :children children))

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
          :table-cell '(:value nil)
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
            (expect actual :to-equal expected))))))

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
                     "%%(diary-sexp)"))
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
      (signals org-kit-error (emit-org bad-target)))))

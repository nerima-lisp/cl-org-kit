;;;; t/edit-plain-list-test.lisp
(in-package #:cl-org-kit/test)

(defun edit-plain-list-test-nodes (document type)
  "Return DOCUMENT's nodes of TYPE in source order."
  (let ((nodes '()))
    (map-nodes (lambda (node)
                 (when (eq (node-type node) type)
                   (push node nodes)))
               (document-root document))
    (nreverse nodes)))

(defun edit-plain-list-test-error (thunk)
  "Return the condition type and reason raised by THUNK."
  (handler-case
      (progn (funcall thunk) '(:no-error))
    (org-edit-error (condition)
      (list (type-of condition) (org-edit-error-reason condition)))
    (error (condition)
      (list (type-of condition) :unexpected-condition))))

(defun edit-plain-list-test-span-ranges (result)
  "Return the source coordinate pairs reported by RESULT."
  (mapcar (lambda (span)
            (list (edit-span-start span) (edit-span-end span)))
          (edit-result-spans result)))

(defun edit-plain-list-test-utf8-bom-octets (text)
  (let* ((encoded (cl-codec-kit:string-to-octets text :encoding :utf-8))
         (result (make-array (+ 3 (length encoded))
                             :element-type '(unsigned-byte 8))))
    (replace result #(239 187 191))
    (replace result encoded :start1 3)
    result))

(describe "format-preserving plain-list edits"
  (it "inserts one item after an existing checkbox item and preserves its neighbors"
    (let* ((source (format nil "- [ ] first~%- second~%~%After~%"))
           (document (parse-org source))
           (items (edit-plain-list-test-nodes document :item))
           (result (org-kit::insert-list-item document (first items)
                                              "- [X] middle" :position :after))
           (edited (edit-result-updated-document result)))
      (with-soft-assertions
        (expect (edit-result-p result) :to-be t)
        (expect (document-text edited)
                :to-equal (format nil "- [ ] first~%- [X] middle~%- second~%~%After~%"))
        (expect (edit-plain-list-test-span-ranges result)
                :to-equal (list (list (node-end (first items))
                                      (node-end (first items)))))
        (expect (mapcar (lambda (item) (node-property item :checkbox))
                        (edit-plain-list-test-nodes edited :item))
                :to-equal '(:off :on nil))
        (expect (document-text document) :to-equal source))))

  (it "deletes one CRLF item while retaining the surrounding Japanese source"
    (let* ((crlf (format nil "~C~C" #\Return #\Newline))
           (source (concatenate 'string "前" crlf
                                "- keep" crlf
                                "- [X] 削除" crlf
                                "- last" crlf
                                "後" crlf))
           (document (parse-org source))
           (items (edit-plain-list-test-nodes document :item))
           (result (org-kit::delete-list-item document (second items)))
           (edited (edit-result-updated-document result)))
      (with-soft-assertions
        (expect (edit-result-p result) :to-be t)
        (expect (document-text edited)
                :to-equal (concatenate 'string "前" crlf "- keep" crlf
                                       "- last" crlf "後" crlf))
        (expect (edit-plain-list-test-span-ranges result)
                :to-equal (list (list (node-begin (second items))
                                      (node-end (second items)))))
        (expect (document-text document) :to-equal source))))

  (it "moves the selected item with its checkbox and exact item source"
    (let* ((source (format nil "- [ ] first~%- [X] second~%- third~%"))
           (document (parse-org source))
           (items (edit-plain-list-test-nodes document :item))
           (list (first (edit-plain-list-test-nodes document :plain-list)))
           (result (org-kit::move-list-item document (third items) (first items)
                                            :position :before))
           (edited (edit-result-updated-document result)))
      (with-soft-assertions
        (expect (edit-result-p result) :to-be t)
        (expect (document-text edited)
                :to-equal (format nil "- third~%- [ ] first~%- [X] second~%"))
        (expect (edit-plain-list-test-span-ranges result)
                :to-equal (list (list (node-begin list) (node-end list))))
        (expect (mapcar (lambda (item) (node-property item :checkbox))
                        (edit-plain-list-test-nodes edited :item))
                :to-equal '(nil :off :on))
        (expect (document-text document) :to-equal source))))

  (it "returns an empty edit result when moving an item relative to itself"
    (let* ((source (format nil "- first~%- second~%"))
           (document (parse-org source))
           (item (first (edit-plain-list-test-nodes document :item)))
           (result (org-kit::move-list-item document item item))
           (updated (edit-result-updated-document result)))
      (with-soft-assertions
        (expect (edit-result-p result) :to-be t)
        (expect (document-text updated) :to-equal source)
        (expect (eq updated document) :to-be nil)
        (expect (edit-result-spans result) :to-equal nil)
        (expect (document-text document) :to-equal source))))

  (it "indents an item under its preceding sibling"
    (let* ((source (format nil "- parent~%- child~%- peer~%"))
           (document (parse-org source))
           (items (edit-plain-list-test-nodes document :item))
           (result (org-kit::indent-list-item document (second items)))
           (edited (edit-result-updated-document result))
           (lists (edit-plain-list-test-nodes edited :plain-list))
           (edited-items (edit-plain-list-test-nodes edited :item)))
      (with-soft-assertions
        (expect (edit-result-p result) :to-be t)
        (expect (document-text edited)
                :to-equal (format nil "- parent~%  - child~%- peer~%"))
        (expect (edit-plain-list-test-span-ranges result)
                :to-equal (list (list (node-begin (second items))
                                      (node-end (second items)))))
        (expect (length (node-children (first lists))) :to-equal 2)
        (expect (eq (node-parent (second edited-items)) (second lists)) :to-be t)
        (expect (eq (node-parent (second lists)) (first edited-items)) :to-be t)
        (expect (document-text document) :to-equal source))))

  (it "unindents the final nested item to its containing list"
    (let* ((source (format nil "- parent~%  - child~%- peer~%"))
           (document (parse-org source))
           (items (edit-plain-list-test-nodes document :item))
           (nested-list (node-parent (second items)))
           (result (org-kit::unindent-list-item document (second items)))
           (edited (edit-result-updated-document result))
           (list (first (edit-plain-list-test-nodes edited :plain-list)))
           (edited-items (edit-plain-list-test-nodes edited :item)))
      (with-soft-assertions
        (expect (edit-result-p result) :to-be t)
        (expect (document-text edited)
                :to-equal (format nil "- parent~%- child~%- peer~%"))
        (expect (edit-plain-list-test-span-ranges result)
                :to-equal (list (list (node-begin nested-list)
                                      (node-end nested-list))))
        (expect (length (node-children list)) :to-equal 3)
        (expect (eq (node-parent (second edited-items)) list) :to-be t)
        (expect (document-text document) :to-equal source))))

  (it "outdents a non-final nested item after its remaining siblings"
    (let* ((source (format nil "- parent~%  - first~%  - second~%- peer~%"))
           (document (parse-org source))
           (items (edit-plain-list-test-nodes document :item))
           (nested-list (node-parent (second items)))
           (result (org-kit::unindent-list-item document (second items)))
           (edited (edit-result-updated-document result))
           (lists (edit-plain-list-test-nodes edited :plain-list))
           (edited-items (edit-plain-list-test-nodes edited :item)))
      (with-soft-assertions
        (expect (edit-result-p result) :to-be t)
        (expect (document-text edited)
                :to-equal (format nil "- parent~%  - second~%- first~%- peer~%"))
        (expect (edit-plain-list-test-span-ranges result)
                :to-equal (list (list (node-begin nested-list)
                                      (node-end nested-list))))
        (expect (eq (node-parent (second edited-items)) (second lists)) :to-be t)
        (expect (eq (node-parent (third edited-items)) (first lists)) :to-be t)
        (expect (eq (node-parent (second lists)) (first edited-items)) :to-be t)
        (expect (document-text document) :to-equal source))))

  (it "changes bullets without losing checkboxes or description-list syntax"
    (let* ((source (format nil "- [ ] alpha :: first~%- [X] beta :: second"))
           (document (parse-org source))
           (items (edit-plain-list-test-nodes document :item))
           (list (first (edit-plain-list-test-nodes document :plain-list)))
           (result (org-kit::set-list-bullet document list "*"))
           (edited (edit-result-updated-document result))
           (edited-list (first (edit-plain-list-test-nodes edited :plain-list))))
      (with-soft-assertions
        (expect (edit-result-p result) :to-be t)
        (expect (document-text edited)
                :to-equal (format nil " * [ ] alpha :: first~% * [X] beta :: second"))
        (expect (edit-plain-list-test-span-ranges result)
                :to-equal (mapcar (lambda (item)
                                    (list (node-post-affiliated item)
                                          (+ (node-post-affiliated item)
                                             (length (node-property item :bullet)))))
                                  items))
        (expect (node-property edited-list :type) :to-be :descriptive)
        (expect (node-type (node-parent edited-list)) :to-be :section)
        (expect (mapcar (lambda (item) (node-property item :bullet))
                        (edit-plain-list-test-nodes edited :item))
                :to-equal '("* " "* "))
        (expect (mapcar (lambda (item) (node-property item :checkbox))
                        (edit-plain-list-test-nodes edited :item))
                :to-equal '(:off :on))
        (expect (document-text document) :to-equal source))))

  (it "renumbers ordered items while preserving punctuation and checkbox state"
    (let* ((source (format nil "7) first~%14) [ ] second"))
           (document (parse-org source))
           (items (edit-plain-list-test-nodes document :item))
           (list (first (edit-plain-list-test-nodes document :plain-list)))
           (result (org-kit::renumber-ordered-list document list :start 1))
           (edited (edit-result-updated-document result)))
      (with-soft-assertions
        (expect (edit-result-p result) :to-be t)
        (expect (document-text edited)
                :to-equal (format nil "1) first~%2) [ ] second"))
        (expect (edit-plain-list-test-span-ranges result)
                :to-equal (mapcar (lambda (item)
                                    (list (node-post-affiliated item)
                                          (+ (node-post-affiliated item)
                                             (length (node-property item :bullet)))))
                                  items))
        (expect (mapcar (lambda (item) (node-property item :checkbox))
                        (edit-plain-list-test-nodes edited :item))
                :to-equal '(nil :off))
        (expect (document-text document) :to-equal source))))

  (it "preserves the original UTF-8 BOM and CRLF bytes outside a marker edit"
    (let* ((crlf (format nil "~C~C" #\Return #\Newline))
           (source (concatenate 'string "日本語" crlf "- [ ] 項目" crlf
                                "末尾" crlf))
           (document (parse-org (edit-plain-list-test-utf8-bom-octets source)
                                :encoding :utf-8))
           (item (first (edit-plain-list-test-nodes document :item)))
           (result (org-kit::set-list-bullet document item "*"))
           (edited (edit-result-updated-document result))
           (expected (concatenate 'string "日本語" crlf " * [ ] 項目" crlf
                                  "末尾" crlf)))
      (with-soft-assertions
        (expect (edit-result-p result) :to-be t)
        (expect (write-org edited :output-type :octets)
                :to-equalp (edit-plain-list-test-utf8-bom-octets expected))
        (expect (edit-plain-list-test-span-ranges result)
                :to-equal (list (list (node-post-affiliated item)
                                      (+ (node-post-affiliated item)
                                         (length (node-property item :bullet)))))))))

  (it "checks output limits before constructing the preview source"
    (let* ((source (format nil "- parent~%- child~%"))
           (document (parse-org source
                                :limits (make-limits
                                         :max-output-length (length source))))
           (items (edit-plain-list-test-nodes document :item))
           (original-splicer (symbol-function 'org-kit::edit-splice-source))
           (splice-called nil))
      (unwind-protect
           (progn
             (setf (symbol-function 'org-kit::edit-splice-source)
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (setf splice-called t)
                     (error "Source splice ran before the output limit check")))
             (handler-case
                 (progn
                   (org-kit::indent-list-item document (second items))
                   (expect nil :to-be t))
               (org-resource-limit-error (condition)
                 (with-soft-assertions
                   (expect (org-resource-limit-error-limit-name condition)
                           :to-be :max-output-length)
                   (expect (org-resource-limit-error-limit condition)
                           :to-be (length source))
                   (expect (org-resource-limit-error-actual condition)
                           :to-be (+ (length source) 2)))))
        (setf (symbol-function 'org-kit::edit-splice-source) original-splicer))
      (with-soft-assertions
        (expect splice-called :to-be nil)
        (expect (document-text document) :to-equal source))))

  (it "covers list helper boundaries and insertion positions"
    (let* ((source (format nil "- first~%- second"))
           (document (parse-org source))
           (items (edit-plain-list-test-nodes document :item))
           (list (first (edit-plain-list-test-nodes document :plain-list)))
           (before (insert-list-item document (second items) "- added"))
           (first-result (insert-list-item document list "- added" :position :first))
           (last-result (insert-list-item document list "- added" :position :last))
           (newline-document (parse-org (format nil "- first~%- second~%")))
           (newline-list (first (edit-plain-list-test-nodes newline-document
                                                           :plain-list)))
           (newline-result (insert-list-item newline-document newline-list
                                             "- added" :position :last))
           (crlf (format nil "~C~C" #\Return #\Newline)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document before))
                :to-equal (format nil "- first~%- added~%- second"))
        (expect (document-text (edit-result-updated-document first-result))
                :to-equal (format nil "- added~%- first~%- second"))
        (expect (document-text (edit-result-updated-document last-result))
                :to-equal (format nil "- first~%- second~%- added"))
        (expect (document-text (edit-result-updated-document newline-result))
                :to-equal (format nil "- first~%- second~%- added~%"))
        (expect (org-kit::edit-list-line-end (concatenate 'string "a" crlf "b") 0)
                :to-equal 1)
        (expect (org-kit::edit-list-line-end (format nil "a~%b") 0) :to-equal 1)
        (expect (org-kit::edit-list-line-end "a" 0) :to-equal 1)
        (expect (org-kit::edit-list-line-ending (concatenate 'string "a" crlf "b") 3)
                :to-equal crlf)
        (expect (org-kit::edit-list-line-ending (format nil "a~%b") 2)
                :to-equal (string #\Newline))
        (expect (org-kit::edit-list-line-ending (concatenate 'string "a" crlf "b") 0)
                :to-equal crlf)
        (expect (org-kit::edit-list-line-ending (format nil "a~%b") 0)
                :to-equal (string #\Newline))
        (expect (org-kit::edit-list-line-ending "a" 0)
                :to-equal (string #\Newline))
        (expect (org-kit::edit-list-adjust-indentation
                 (concatenate 'string "a" (string #\Newline) "   " crlf "b" crlf)
                 2 "target")
                :to-equal (concatenate 'string "  a" (string #\Newline)
                                       "   " crlf "  b" crlf))
        (expect (org-kit::edit-list-adjust-indentation "  a" -1 "target")
                :to-equal " a")
        (expect (edit-plain-list-test-error
                 (lambda () (org-kit::edit-list-adjust-indentation "a" -1 "target")))
                :to-equal '(org-edit-error :uneditable-structure))
        (expect (org-kit::edit-list-find-item-at document (node-post-affiliated (first items)))
                :to-be (first items))
        (expect (org-kit::edit-list-find-item-at document -1) :to-be nil)
        (expect (org-kit::edit-list-find-node-at document :plain-list
                                                 (node-post-affiliated list))
                :to-be list)
        (expect (org-kit::edit-list-find-node-at document :plain-list -1) :to-be nil)
        (expect (handler-case
                    (progn (org-kit::edit-list-resolve-target nil "item") nil)
                  (type-error () t))
                :to-be t)
        (expect (handler-case
                    (progn (org-kit::edit-list-check-document nil) nil)
                  (type-error () t))
                :to-be t))))

  (it "covers ordered marker rewrites and nested insertion"
    (let* ((ordered-document (parse-org (format nil "7) first~%14) second")))
           (ordered-list (first (edit-plain-list-test-nodes
                                 ordered-document :plain-list)))
           (ordered-result (set-list-bullet ordered-document ordered-list "2."))
           (nested-document (parse-org (format nil "- parent~%  - child~%")))
           (nested-list (second (edit-plain-list-test-nodes
                                 nested-document :plain-list)))
           (nested-result (insert-list-item nested-document nested-list
                                            "- inserted" :position :first))
           (unchanged-document (parse-org (format nil "- first~%- second")))
           (unchanged-list (first (edit-plain-list-test-nodes
                                   unchanged-document :plain-list)))
           (unchanged-result (set-list-bullet unchanged-document unchanged-list "-"))
           (move-document (parse-org (format nil "- first~%- second~%- third")))
           (move-items (edit-plain-list-test-nodes move-document :item))
           (move-result (move-list-item move-document (first move-items)
                                        (third move-items) :position :after)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document ordered-result))
                :to-equal (format nil "2. first~%3. second"))
        (expect (document-text (edit-result-updated-document nested-result))
                :to-equal (format nil "- parent~%  - inserted~%  - child~%"))
        (expect (edit-result-spans unchanged-result) :to-equal nil)
        (expect (document-text (edit-result-updated-document unchanged-result))
                :to-equal (document-text unchanged-document))
        (expect (document-text (edit-result-updated-document move-result))
                :to-equal (format nil "- second~%- third~%- first")))))

  (it "covers list validation and structural guard failures"
    (let* ((source (format nil "- first~%- second~%~%1. ordered~%"))
           (document (parse-org source))
           (items (edit-plain-list-test-nodes document :item))
           (lists (edit-plain-list-test-nodes document :plain-list))
           (first-item (first items))
           (first-list (first lists))
           (failure (lambda (thunk) (edit-plain-list-test-error thunk))))
      (with-soft-assertions
        (expect (failure (lambda ()
                           (org-kit::edit-list-assert-item-parent
                            document -1 :plain-list -1 "target")))
                :to-equal '(org-edit-error :unexpected-diff))
        (expect (failure (lambda ()
                           (org-kit::edit-list-assert-item-nested-under
                            document (node-post-affiliated first-item) -1 "target")))
                :to-equal '(org-edit-error :unexpected-diff))
        (let* ((orphan-document (parse-org (format nil "- item~%")))
               (orphan-item (first (edit-plain-list-test-nodes
                                    orphan-document :item)))
               (section (node-parent (node-parent orphan-item))))
          (setf (org-kit::%node-parent orphan-item) section)
          (expect (failure (lambda ()
                             (org-kit::edit-list-resolve-target
                              orphan-document orphan-item)))
                  :to-equal '(org-edit-error :uneditable-structure)))
        (expect (failure (lambda ()
                           (org-kit::edit-list-marker-separator "marker" "target")))
                :to-equal '(org-edit-error :uneditable-structure))
        (expect (failure (lambda ()
                           (org-kit::edit-list-ordered-marker-details "x. " "target")))
                :to-equal '(org-edit-error :uneditable-structure))
        (expect (multiple-value-list (org-kit::edit-list-bullet-spec "12)" "target"))
                :to-equal (list :ordered #\) 12))
        (expect (failure (lambda () (org-kit::edit-list-bullet-spec "" "target")))
                :to-equal '(org-edit-error :invalid-value))
        (expect (failure (lambda () (org-kit::edit-list-bullet-spec "2x" "target")))
                :to-equal '(org-edit-error :invalid-value))
        (expect (failure (lambda () (org-kit::edit-list-bullet-spec "2.." "target")))
                :to-equal '(org-edit-error :invalid-value))
        (expect (failure (lambda () (insert-list-item document first-item "")))
                :to-equal '(org-edit-error :invalid-value))
        (expect (failure (lambda ()
                           (insert-list-item document first-item (format nil "- added~%"))))
                :to-equal '(org-edit-error :invalid-value))
        (expect (failure (lambda () (insert-list-item document first-item "plain text")))
                :to-equal '(org-edit-error :invalid-value))
        (expect (failure (lambda ()
                           (insert-list-item document first-item
                                             (format nil "- one~%- two"))))
                :to-equal '(org-edit-error :invalid-value))
        (expect (failure (lambda ()
                           (insert-list-item document first-item "1. ordered")))
                :to-equal '(org-edit-error :invalid-value))
        (expect (failure (lambda () (insert-list-item document first-item nil)))
                :to-equal '(org-edit-error :invalid-value))
        (expect (failure (lambda ()
                           (insert-list-item document first-item
                                             (format nil "- added~%"))))
                :to-equal '(org-edit-error :invalid-value))
        (expect (failure (lambda ()
                           (move-list-item document first-item (third items)
                                           :position :invalid)))
                :to-equal '(org-edit-error :invalid-value))
        (expect (failure (lambda () (indent-list-item document (second items) :amount 1.5)))
                :to-equal '(org-edit-error :invalid-value))
        (expect (failure (lambda ()
                           (set-list-bullet document first-list "-")))
                :to-equal '(org-edit-error :uneditable-structure))
        (expect (failure (lambda () (renumber-ordered-list document first-list :start -1)))
                :to-equal '(org-edit-error :invalid-value))
        (expect (failure (lambda () (renumber-ordered-list document first-list :start 1.5)))
                :to-equal '(org-edit-error :invalid-value))
        (let* ((damaged-document (parse-org (format nil "- damaged~%")))
               (damaged-item (first (edit-plain-list-test-nodes
                                     damaged-document :item))))
          (setf (node-property damaged-item :bullet) "wrong ")
          (expect (failure (lambda ()
                             (org-kit::edit-list-item-marker-span
                              damaged-document damaged-item "item")))
                  :to-equal '(org-edit-error :uneditable-structure)))
        (let* ((source-with-later-content
                 (format nil "- parent~%  - child~%  trailing text~%"))
               (nested-document (parse-org source-with-later-content))
               (nested-item (second (edit-plain-list-test-nodes
                                     nested-document :item))))
          (expect (failure (lambda ()
                             (unindent-list-item nested-document nested-item)))
                  :to-equal '(org-edit-error :uneditable-structure)))
        (let* ((nested-document (parse-org (format nil "- parent~%  - child~%")))
               (nested-item (second (edit-plain-list-test-nodes
                                     nested-document :item)))
               (original (symbol-function 'org-kit::edit-list-line-indentation)))
          (unwind-protect
               (progn
                 (setf (symbol-function 'org-kit::edit-list-line-indentation)
                       (lambda (&rest arguments)
                         (declare (ignore arguments))
                         0))
                 (expect (failure (lambda ()
                                    (unindent-list-item nested-document nested-item)))
                         :to-equal '(org-edit-error :uneditable-structure)))
            (setf (symbol-function 'org-kit::edit-list-line-indentation) original)))
        (let ((original (symbol-function 'org-kit::parse-org)))
          (unwind-protect
               (progn
                 (setf (symbol-function 'org-kit::parse-org)
                       (lambda (&rest arguments)
                         (declare (ignore arguments))
                         (error 'org-parse-error)))
                 (expect (failure (lambda ()
                                    (insert-list-item document first-item "- added")))
                         :to-equal '(org-edit-error :invalid-value)))
            (setf (symbol-function 'org-kit::parse-org) original)))
        (expect (handler-case (progn (delete-list-item nil "item") nil)
                  (type-error () t))
                :to-be t)
        (expect (handler-case (progn (move-list-item nil "item" "item") nil)
                  (type-error () t))
                :to-be t)
        (expect (handler-case (progn (indent-list-item nil "item") nil)
                  (type-error () t))
                :to-be t)
        (expect (handler-case (progn (unindent-list-item nil "item") nil)
                  (type-error () t))
                :to-be t)
        (expect (handler-case (progn (renumber-ordered-list nil "list") nil)
                  (type-error () t))
                :to-be t))))

  (it "reports condition types and reasons for unsupported list edits"
    (let* ((source (format nil "- first~%- second~%~%1. ordered~%"))
           (document (parse-org source))
           (lists (edit-plain-list-test-nodes document :plain-list))
           (items (edit-plain-list-test-nodes document :item))
           (failure (lambda (thunk) (edit-plain-list-test-error thunk))))
      (with-soft-assertions
        (expect (funcall failure
                         (lambda () (org-kit::insert-list-item
                                     document (first items) "- added" :position :up)))
                :to-equal '(org-edit-error :invalid-value))
        (expect (funcall failure
                         (lambda () (org-kit::insert-list-item
                                     document (second lists) "- wrong" :position :last)))
                :to-equal '(org-edit-error :invalid-value))
        (expect (funcall failure
                         (lambda () (org-kit::move-list-item
                                     document (first items)
                                     (third items) :position :before)))
                :to-equal '(org-edit-error :invalid-value))
        (expect (funcall failure
                         (lambda () (org-kit::indent-list-item
                                     document (first items) :amount 0)))
                :to-equal '(org-edit-error :invalid-value))
        (expect (funcall failure
                         (lambda () (org-kit::unindent-list-item
                                     document (first items))))
                :to-equal '(org-edit-error :uneditable-structure))
        (expect (funcall failure
                         (lambda () (org-kit::set-list-bullet
                                     document (first lists) "?")))
                :to-equal '(org-edit-error :invalid-value))
        (expect (funcall failure
                         (lambda () (org-kit::renumber-ordered-list
                                     document (first lists))))
                :to-equal '(org-edit-error :invalid-value)))))))

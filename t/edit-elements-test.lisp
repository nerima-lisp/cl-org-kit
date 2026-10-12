;;;; t/edit-elements-test.lisp
(in-package #:cl-org-kit/test)

(defun edit-elements-test-nodes (document type)
  "Return DOCUMENT's nodes of TYPE in source order."
  (let ((nodes '()))
    (map-nodes (lambda (node)
                 (when (eq (node-type node) type)
                   (push node nodes)))
               (document-root document))
    (nreverse nodes)))

(defun edit-elements-test-error (thunk)
  "Return the reason and target of an edit error raised by THUNK."
  (handler-case
      (progn (funcall thunk) '(:no-error))
    (org-edit-error (condition)
      (list (org-edit-error-reason condition)
            (org-edit-error-target condition)))))

(defun edit-elements-test-utf8-bom-octets (text)
  (let* ((encoded (cl-codec-kit:string-to-octets text :encoding :utf-8))
         (result (make-array (+ 3 (length encoded))
                             :element-type '(unsigned-byte 8))))
    (replace result #(239 187 191))
    (replace result encoded :start1 3)
    result))

(describe "format-preserving element edits"
  (it "replaces a source block body while preserving adjacent mixed-newline source"
    (let* ((crlf (concatenate 'string (string #\Return) (string #\Newline)))
           (old-body (concatenate 'string "unique old line" crlf "second old line"
                                  (string #\Newline)))
           (new-body (concatenate 'string "fresh line" crlf "another fresh line"
                                  (string #\Newline)))
           (source (concatenate 'string
                                "#+NAME: keep-name" crlf
                                "#+BEGIN_SRC emacs-lisp" crlf
                                old-body
                                "#+END_SRC" crlf
                                "#+BEGIN_EXAMPLE" crlf
                                "example stays" crlf
                                "#+END_EXAMPLE" crlf
                                "* Outside" (string #\Newline)
                                "unchanged" (string #\Newline)))
           (document (parse-org source))
           (source-block (first (edit-elements-test-nodes document :src-block)))
           (old-body-start (search old-body source))
           (expected-source
             (concatenate 'string
                          (subseq source 0 old-body-start)
                          new-body
                          (subseq source (+ old-body-start (length old-body)))))
           (edited (replace-block-body document (list source-block) new-body))
           (edited-source-block
             (first (edit-elements-test-nodes edited :src-block)))
           (edited-example-block
             (first (edit-elements-test-nodes edited :example-block)))
           (edited-headline (first (edit-elements-test-nodes edited :headline))))
      (with-soft-assertions
        (expect (document-text edited) :to-equal expected-source)
        (expect (node-property edited-source-block :value) :to-equal new-body)
        (expect (node-property edited-source-block :name) :to-equal "keep-name")
        (expect (node-property edited-example-block :value)
                :to-equal (concatenate 'string "example stays" crlf))
        (expect (node-property edited-headline :raw-value) :to-equal "Outside")
        (expect (document-text document) :to-equal source)
        (expect (node-property source-block :value) :to-equal old-body))))

  (it "allows an empty example body and changes only that block's body"
    (let* ((source (format nil "before~%#+BEGIN_EXAMPLE~%old body~%#+END_EXAMPLE~%after~%"))
           (document (parse-org source))
           (block (first (edit-elements-test-nodes document :example-block)))
           (edited (replace-block-body document block "")))
      (with-soft-assertions
        (expect (document-text edited)
                :to-equal (format nil "before~%#+BEGIN_EXAMPLE~%#+END_EXAMPLE~%after~%"))
        (expect (node-property (first (edit-elements-test-nodes edited :example-block))
                               :value)
                :to-equal "")
        (expect (document-text document) :to-equal source)
        (expect (node-property block :value) :to-equal (format nil "old body~%")))))

  (it "preserves BOM and exact UTF-8 bytes around a Japanese CRLF block body"
    (let* ((crlf (concatenate 'string (string #\Return) (string #\Newline)))
           (source (concatenate 'string
                                "日本語" crlf
                                "#+BEGIN_SRC text" crlf
                                "古い本文" crlf
                                "#+END_SRC" crlf
                                "末尾" crlf))
           (document (parse-org (edit-elements-test-utf8-bom-octets source)
                                :encoding :utf-8))
           (block (first (edit-elements-test-nodes document :src-block)))
           (replacement (concatenate 'string "新しい本文" crlf))
           (expected (concatenate 'string
                                  "日本語" crlf
                                  "#+BEGIN_SRC text" crlf
                                  replacement
                                  "#+END_SRC" crlf
                                  "末尾" crlf))
           (edited (replace-block-body document block replacement)))
      (with-soft-assertions
        (expect (write-org edited :output-type :octets)
                :to-equalp (edit-elements-test-utf8-bom-octets expected)))))

  (it "rejects a block body that changes the parsed block structure"
    (let* ((source (format nil "#+BEGIN_SRC~%old body~%#+END_SRC~%"))
           (document (parse-org source))
           (block (first (edit-elements-test-nodes document :src-block)))
           (failure (edit-elements-test-error
                     (lambda ()
                       (replace-block-body
                        document block (format nil "#+END_SRC~%unexpected~%"))))))
      (with-soft-assertions
        (expect failure :to-equal '(:unexpected-diff "src-block"))
        (expect (document-text document) :to-equal source)
        (expect (node-property block :value) :to-equal (format nil "old body~%")))))

  (it "refuses a block opener with no source boundary for its body"
    (let* ((source "#+BEGIN_SRC")
           (document (parse-org source))
           (block (first (edit-elements-test-nodes document :src-block)))
           (failure (edit-elements-test-error
                     (lambda () (replace-block-body document block "body")))))
      (with-soft-assertions
        (expect failure :to-equal '(:uneditable-structure "src-block"))
        (expect (document-text document) :to-equal source))))

  (it "updates all checkbox states by changing only the marker character"
    (let* ((crlf (concatenate 'string (string #\Return) (string #\Newline)))
           (source (concatenate 'string "  1. [ ] first" crlf "  2. [X] other"))
           (document (parse-org source))
           (items (edit-elements-test-nodes document :item))
           (first-item (first items))
           (second-item (second items))
           (partial (set-checkbox-state document first-item :partial))
           (second-partial (set-checkbox-state document second-item :partial))
           (partial-item (first (edit-elements-test-nodes partial :item)))
           (checked (set-checkbox-state partial partial-item :checked))
           (checked-item (first (edit-elements-test-nodes checked :item)))
           (unchecked (set-checkbox-state checked checked-item :unchecked))
           (partial-items (edit-elements-test-nodes partial :item))
           (checked-items (edit-elements-test-nodes checked :item))
           (unchecked-items (edit-elements-test-nodes unchecked :item)))
      (with-soft-assertions
        (expect (document-text partial)
                :to-equal (concatenate 'string "  1. [-] first" crlf "  2. [X] other"))
        (expect (document-text second-partial)
                :to-equal (concatenate 'string "  1. [ ] first" crlf "  2. [-] other"))
        (expect (mapcar (lambda (item) (node-property item :checkbox))
                        (edit-elements-test-nodes second-partial :item))
                :to-equal '(:off :trans))
        (expect (mapcar (lambda (item) (node-property item :checkbox)) partial-items)
                :to-equal '(:trans :on))
        (expect (document-text checked)
                :to-equal (concatenate 'string "  1. [X] first" crlf "  2. [X] other"))
        (expect (mapcar (lambda (item) (node-property item :checkbox)) checked-items)
                :to-equal '(:on :on))
        (expect (document-text unchecked) :to-equal source)
        (expect (mapcar (lambda (item) (node-property item :checkbox)) unchecked-items)
                :to-equal '(:off :on))
        (expect (document-text document) :to-equal source)
        (expect (node-property first-item :checkbox) :to-be :off))))

  (it "preserves BOM and exact UTF-8 bytes around a Japanese CRLF checkbox"
    (let* ((crlf (concatenate 'string (string #\Return) (string #\Newline)))
           (source (concatenate 'string "- [ ] 日本語" crlf "後続" crlf))
           (document (parse-org (edit-elements-test-utf8-bom-octets source)
                                :encoding :utf-8))
           (item (first (edit-elements-test-nodes document :item)))
           (edited (set-checkbox-state document item :checked))
           (expected (concatenate 'string "- [X] 日本語" crlf "後続" crlf)))
      (with-soft-assertions
        (expect (write-org edited :output-type :octets)
                :to-equalp (edit-elements-test-utf8-bom-octets expected)))))

  (it "reports the wrong block target type in the edit error slots"
    (let* ((document (parse-org (format nil "* Heading~%")))
           (headline (first (edit-elements-test-nodes document :headline)))
           (failure (edit-elements-test-error
                     (lambda () (replace-block-body document headline "body")))))
      (with-soft-assertions
        (expect failure :to-equal '(:wrong-node-type "headline"))
        (expect (document-text document) :to-equal (format nil "* Heading~%")))))

  (it "reports a non-string block body as invalid-value with target slots"
    (let* ((source (format nil "#+BEGIN_SRC~%body~%#+END_SRC~%"))
           (document (parse-org source))
           (block (first (edit-elements-test-nodes document :src-block)))
           (failure (edit-elements-test-error
                     (lambda () (replace-block-body document block :body)))))
      (with-soft-assertions
        (expect failure :to-equal '(:invalid-value "src-block"))
        (expect (document-text document) :to-equal source))))

  (it "reports a missing checkbox as invalid-value with target slots"
    (let* ((source (format nil "- plain item~%"))
           (document (parse-org source))
           (item (first (edit-elements-test-nodes document :item)))
           (failure (edit-elements-test-error
                     (lambda () (set-checkbox-state document item :checked)))))
      (with-soft-assertions
        (expect failure :to-equal '(:invalid-value "item"))
        (expect (document-text document) :to-equal source))))

  (it "reports an invalid checkbox state and wrong node type with edit slots"
    (let* ((source (format nil "* Heading~%- [ ] item~%"))
           (document (parse-org source))
           (headline (first (edit-elements-test-nodes document :headline)))
           (item (first (edit-elements-test-nodes document :item)))
           (invalid-state (edit-elements-test-error
                           (lambda () (set-checkbox-state document item :indeterminate))))
           (wrong-type (edit-elements-test-error
                        (lambda () (set-checkbox-state document headline :checked)))))
      (with-soft-assertions
        (expect invalid-state :to-equal '(:invalid-value "item"))
        (expect wrong-type :to-equal '(:wrong-node-type "headline"))
        (expect (document-text document) :to-equal source)))))

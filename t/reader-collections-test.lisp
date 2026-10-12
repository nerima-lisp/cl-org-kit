;;;; t/reader-collections-test.lisp
(in-package #:cl-org-kit/test)

(defun collection-node-content (document cell)
  (let ((start (node-contents-begin cell))
        (end (node-contents-end cell)))
    (when (and start end)
      (subseq (document-text document) start end))))

(describe "plain lists and tables"
  (it "parses nested lists and ordered item metadata"
    (let* ((document (parse-org (format nil "- parent~%  - nested~%3. [@3] counted~%+ Term :: descriptive~%")))
           (root (document-root document))
           (list (first (node-children (first (node-children root)))))
           (items (node-children list))
           (parent-item (first items))
           (nested-list (find :plain-list (node-children parent-item)
                              :key #'node-type))
           (nested-item (first (node-children nested-list)))
           (ordered-item (second items)))
      (with-soft-assertions
        (expect (node-type list) :to-be :plain-list)
        (expect (node-property list :type) :to-be :unordered)
        (expect (length items) :to-be 3)
        (expect (node-property parent-item :bullet) :to-equal "- ")
        (expect (node-type nested-list) :to-be :plain-list)
        (expect (node-property nested-item :bullet) :to-equal "- ")
        (expect (node-property ordered-item :bullet) :to-equal "3. ")
        (expect (node-property ordered-item :counter) :to-be 3)
        (expect (node-parent list) :to-be (first (node-children root)))
        (expect (node-parent nested-list) :to-be parent-item)
        (expect (node-parent nested-item) :to-be nested-list)
        (expect (list (node-document nested-item) (node-property (third items) :tag)) :to-equal (list document '("Term"))))))

  (it "keeps empty list item contents absent"
    (let* ((document (parse-org (format nil "- ~%- text~%")))
           (list (first (node-children (first (node-children (document-root document))))))
           (items (node-children list))
           (empty-item (first items))
           (text-item (second items)))
      (with-soft-assertions
        (expect (length items) :to-be 2)
        (expect (node-contents-begin empty-item) :to-be nil)
        (expect (node-contents-end empty-item) :to-be nil)
        (expect (node-children empty-item) :to-equal '())
        (expect (collection-node-content document text-item) :to-equal (format nil "text~%")))))

  (it "parses rule rows, empty cells, and rows without a trailing bar"
    (let* ((document (parse-org
                      (format nil "| Name | Value |~%|------+-------|~%|  | |~%| final cell has no trailing bar~%")))
           (table (first (node-children (first (node-children (document-root document))))))
           (rows (node-children table))
           (first-row (first rows))
           (rule-row (second rows))
           (empty-row (third rows))
           (final-row (fourth rows))
           (first-cells (node-children first-row))
           (empty-cells (node-children empty-row))
           (final-cell (first (node-children final-row))))
      (with-soft-assertions
        (expect (node-type table) :to-be :table)
        (expect (node-property table :type) :to-be :org)
        (expect (length rows) :to-be 4)
        (expect (node-property first-row :type) :to-be :standard)
        (expect (collection-node-content document (first first-cells))
                :to-equal "Name")
        (expect (collection-node-content document (second first-cells))
                :to-equal "Value")
        (expect (node-property rule-row :type) :to-be :rule)
        (expect (node-contents-begin rule-row) :to-be nil)
        (expect (length empty-cells) :to-be 2)
        (expect (collection-node-content document (first empty-cells)) :to-equal "")
        (expect (collection-node-content document (second empty-cells)) :to-equal "")
        (expect (collection-node-content document final-cell)
                :to-equal "final cell has no trailing bar")
        (expect (node-parent first-row) :to-be table)
        (expect (node-parent (first first-cells)) :to-be first-row))))

  (it "keeps table.el and formula-only tables opaque"
    (dolist (source (list (format nil "+---+---+~%| one | two |~%+---+---+~%")
                          (format nil "#+TBLFM: $1=$2~%")))
      (let* ((document (parse-org source))
             (table (first (node-children (first (node-children (document-root document)))))))
        (with-soft-assertions
          (expect (node-type table) :to-be :table)
          (expect (node-property table :type) :to-be :table.el)
          (expect (node-contents-begin table) :to-be nil)
          (expect (node-contents-end table) :to-be nil)
          (expect (node-children table) :to-equal '())))))
  (it "classifies independent list kinds and checkbox states"
    (let* ((ordered-document (parse-org (format nil "1) [@4]~%")))
           (ordered-list
            (first (node-children
                    (first (node-children (document-root ordered-document))))))
           (ordered-item (first (node-children ordered-list)))
           (descriptive-document
            (parse-org (format nil "+   Term  :: description~%")))
           (descriptive-list
            (first (node-children
                    (first (node-children (document-root descriptive-document))))))
           (descriptive-item (first (node-children descriptive-list)))
           (checkbox-document
            (parse-org (format nil "- [ ] off~%- [X] on~%- [x] lower~%- [-]~%")))
           (checkbox-list
            (first (node-children
                    (first (node-children (document-root checkbox-document))))))
           (checkbox-items (node-children checkbox-list)))
      (with-soft-assertions
        (expect (node-property ordered-list :type) :to-be :ordered)
        (expect (node-property ordered-item :bullet) :to-equal "1) ")
        (expect (node-property ordered-item :counter) :to-be 4)
        (expect (node-contents-begin ordered-item) :to-be nil)
        (expect (node-property descriptive-list :type) :to-be :descriptive)
        (expect (node-property descriptive-item :tag) :to-equal '("Term"))
        (expect (mapcar (lambda (item) (node-property item :checkbox))
                        checkbox-items)
                :to-equal
                '(:off :on :on :trans))
        (expect (node-contents-begin (fourth checkbox-items)) :to-be nil))))

  (it "skips blank lines at list boundaries and includes indented continuations"
    (let* ((continued-document
            (parse-org (format nil "- first~%~%  continuation~%~%- second~%")))
           (continued-list
            (first (node-children
                    (first (node-children (document-root continued-document))))))
           (continued-items (node-children continued-list))
           (prose-source (format nil "- first~%~%prose~%"))
           (prose-document (parse-org prose-source))
           (prose-list
            (first (node-children
                    (first (node-children (document-root prose-document))))))
           (prose-section (first (node-children (document-root prose-document))))
           (unseparated-source (format nil "- first~%prose~%"))
           (unseparated-document (parse-org unseparated-source))
           (unseparated-list
            (first (node-children
                    (first (node-children (document-root unseparated-document)))))))
      (with-soft-assertions
        (expect (length continued-items) :to-be 2)
        (expect (some (lambda (paragraph)
                        (search "continuation" (node-text paragraph)))
                      (node-children (first continued-items)))
                :to-be-truthy)
        (expect (node-end prose-list) :to-be (search "prose" prose-source))
        (expect (node-end unseparated-list)
                :to-be
                (search "prose" unseparated-source))
        (expect (length (remove-if-not (lambda (node)
                                         (eq (node-type node) :paragraph))
                                       (node-children prose-section)))
                :to-be 1))))

  (it "keeps escaped table pipes in cells and parses objects inside cell and tag ranges"
    (let* ((escaped-source
            (format nil "~A~%" "| escaped\\|bar | pair\\\\|next |"))
           (escaped-document (parse-org escaped-source))
           (escaped-table
            (first (node-children
                    (first (node-children (document-root escaped-document))))))
           (escaped-row (first (node-children escaped-table)))
           (escaped-cells (node-children escaped-row))
           (object-table-document
            (parse-org (format nil "| *bold* [[https://example.com][link]] | plain |~%")))
           (object-table
            (first (node-children
                    (first (node-children (document-root object-table-document))))))
           (object-cell (first (node-children (first (node-children object-table)))))
           (bold (find :bold (node-children object-cell) :key #'node-type))
           (link (find :link (node-children object-cell) :key #'node-type))
           (tag-document (parse-org (format nil "+ /Term/ :: description~%")))
           (tag-list
            (first (node-children
                    (first (node-children (document-root tag-document))))))
           (tag-item (first (node-children tag-list)))
           (tag-object (first (node-property tag-item :tag))))
      (with-soft-assertions
        (expect (mapcar (lambda (cell) (collection-node-content escaped-document cell))
                        escaped-cells)
                :to-equal
                '("escaped\\|bar" "pair\\\\" "next"))
        (expect (node-type bold) :to-be :bold)
        (expect (node-parent bold) :to-be object-cell)
        (expect (node-type link) :to-be :link)
        (expect (node-parent link) :to-be object-cell)
        (expect (node-type tag-object) :to-be :italic)
        (expect (node-parent tag-object) :to-be tag-item))))
)
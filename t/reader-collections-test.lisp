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
    (let* ((source (format nil "| Name | Value |~%|------+-------|~%|  | |~%| final cell has no trailing bar~%"))
           (document (parse-org source))
           (table (first (node-children (first (node-children (document-root document))))))
           (rows (node-children table))
           (first-row (first rows))
           (rule-row (second rows))
           (empty-row (third rows))
           (final-row (fourth rows))
           (name-start (search "Name" source))
           (first-row-end (1+ (position #\Newline source)))
           (first-cells (node-children first-row))
           (empty-cells (node-children empty-row))
           (final-cell (first (node-children final-row))))
      (with-soft-assertions
        (expect (node-type table) :to-be :table)
        (expect (node-property table :type) :to-be :org)
        (expect (node-property table :tblfm) :to-be nil)
        (expect (length rows) :to-be 4)
        (expect (list (node-begin first-row) (node-end first-row))
                :to-equal (list 0 first-row-end))
        (expect (node-property (first first-cells) :value) :to-equal "Name")
        (expect (list (node-contents-begin (first first-cells))
                      (node-contents-end (first first-cells)))
                :to-equal (list name-start (+ name-start (length "Name"))))
        (expect (node-property first-row :type) :to-be :standard)
        (expect (collection-node-content document (first first-cells))
                :to-equal "Name")
        (expect (collection-node-content document (second first-cells))
                :to-equal "Value")
        (expect (node-property rule-row :type) :to-be :rule)
        (expect (node-contents-begin rule-row) :to-be nil)
        (expect (length empty-cells) :to-be 2)
        (expect (mapcar (lambda (cell) (node-property cell :value)) empty-cells)
                :to-equal '("" ""))
        (expect (collection-node-content document (first empty-cells)) :to-equal "")
        (expect (collection-node-content document (second empty-cells)) :to-equal "")
        (expect (collection-node-content document final-cell)
                :to-equal "final cell has no trailing bar")
        (expect (node-parent first-row) :to-be table)
        (expect (node-parent (first first-cells)) :to-be first-row))))

  (it "attaches one contiguous TBLFM directive to a pipe table"
    (let* ((source (format nil "| a | b |~%#+TBLFM: $2=$1~%After the table.~%"))
           (document (parse-org source))
           (section (first (node-children (document-root document))))
           (elements (node-children section))
           (table (first elements))
           (directive-start (search "#+TBLFM: $2=$1" source))
           (directive-end
             (1+ (position #\Newline source :start directive-start)))
           (cell (first (node-children (first (node-children table))))))
      (with-soft-assertions
        (expect (node-type table) :to-be :table)
        (expect (node-property table :type) :to-be :org)
        (expect (node-property table :tblfm) :to-equal '("$2=$1"))
        (expect (node-property table :reader-tblfm-ranges)
                :to-equal (list (cons directive-start directive-end)))
        (expect (node-end table) :to-be directive-end)
        (expect (node-contents-end table) :to-be directive-start)
        (expect (collection-node-content document cell) :to-equal "a")
        (expect (length elements) :to-be 2)
        (expect (node-type (second elements)) :to-be :paragraph))))

  (it "retains source positions for multiple contiguous TBLFM directives"
    (let* ((source (format nil "| x |~%#+TBLFM: $1=$1+1~%#+TBLFM: $1=$1-1~%"))
           (document (parse-org source))
           (section (first (node-children (document-root document))))
           (elements (node-children section))
           (table (first elements))
           (first-start (search "#+TBLFM: $1=$1+1" source))
           (first-end (1+ (position #\Newline source :start first-start)))
           (second-start (search "#+TBLFM: $1=$1-1" source))
           (second-end (1+ (position #\Newline source :start second-start))))
      (with-soft-assertions
        (expect (node-property table :tblfm)
                :to-equal '("$1=$1+1" "$1=$1-1"))
        (expect (node-property table :reader-tblfm-ranges)
                :to-equal (list (cons first-start first-end)
                                (cons second-start second-end)))
        (expect (node-end table) :to-be second-end)
        (expect (length elements) :to-be 1))))

  (it "leaves blank-separated and standalone TBLFM directives as keywords"
    (dolist (source (list (format nil "| a |~%~%#+TBLFM: $1=$1~%")
                          (format nil "#+TBLFM: $1=$1~%")))
      (let* ((document (parse-org source))
             (section (first (node-children (document-root document))))
             (elements (node-children section))
             (formula (car (last elements))))
        (with-soft-assertions
          (expect (node-type formula) :to-be :keyword)
          (expect (node-property formula :key) :to-equal "TBLFM")
          (expect (node-property formula :value) :to-equal "$1=$1")
          (when (> (length elements) 1)
            (expect (node-property (first elements) :tblfm) :to-be nil)
            (expect (node-end (first elements)) :to-be (node-begin formula)))))))

  (it "does not attach a TBLFM directive that precedes a pipe table"
    (let* ((document (parse-org (format nil "#+TBLFM: $1=$1~%| a |~%")))
           (section (first (node-children (document-root document))))
           (elements (node-children section))
           (formula (first elements))
           (table (second elements)))
      (with-soft-assertions
        (expect (node-type formula) :to-be :keyword)
        (expect (node-type table) :to-be :table)
        (expect (node-property table :type) :to-be :org)
        (expect (node-property table :tblfm) :to-be nil))))

  (it "matches Org's classification when a table.el border follows an attached formula"
    (let* ((source (format nil "+--+~%|a |~%#+TBLFM: x~%+--+~%"))
           (document (parse-org source))
           (section (first (node-children (document-root document))))
           (elements (node-children section))
           (table (second elements))
           (pipe-start (search "|a |" source)))
      (with-soft-assertions
        (expect (mapcar #'node-type elements)
                :to-equal '(:paragraph :table :paragraph))
        (expect (node-property table :type) :to-be :org)
        (expect (node-property table :tblfm) :to-equal '("x"))
        (expect (node-begin table) :to-be pipe-start)
        (expect (node-text table) :to-equal (format nil "|a |~%#+TBLFM: x~%"))
        (expect (node-text (first elements)) :to-equal (format nil "+--+~%"))
        (expect (node-text (third elements)) :to-equal (format nil "+--+~%"))))
    (let* ((source (format nil "+--+~%|a |~%+--+~%#+TBLFM: x~%"))
           (document (parse-org source))
           (section (first (node-children (document-root document))))
           (elements (node-children section))
           (table (first elements)))
      (with-soft-assertions
        (expect (length elements) :to-be 1)
        (expect (node-property table :type) :to-be :table.el)
        (expect (node-property table :value)
                :to-equal (format nil "+--+~%|a |~%+--+~%"))
        (expect (node-property table :tblfm) :to-equal '("x")))))

  (it "keeps table.el tables opaque"
    (let* ((source (format nil "+---+---+~%| one | two |~%+---+---+~%"))
           (document (parse-org source))
           (table (first (node-children (first (node-children (document-root document)))))))
      (with-soft-assertions
        (expect (node-type table) :to-be :table)
        (expect (node-property table :type) :to-be :table.el)
        (expect (node-property table :value) :to-equal source)
        (expect (node-property table :tblfm) :to-be nil)
        (expect (node-contents-begin table) :to-be nil)
        (expect (node-contents-end table) :to-be nil)
        (expect (node-children table) :to-equal '()))))

  (it "keeps table.el separators and formulas opaque and source-positioned"
    (let* ((source (format nil "+---+---+~%| one | two |~%+===+===+~%| three | four |~%+---+---+~%#+TBLFM: $1=$2~%#+TBLFM: $2=$1~%"))
           (document (parse-org source))
           (section (first (node-children (document-root document))))
           (table (first (node-children section)))
           (directive-start (search "#+TBLFM: $1=$2" source))
           (second-directive-start (search "#+TBLFM: $2=$1" source))
           (first-directive-end
             (1+ (position #\Newline source :start directive-start)))
           (directive-end (length source)))
      (with-soft-assertions
        (expect (node-type table) :to-be :table)
        (expect (node-property table :type) :to-be :table.el)
        (expect (node-property table :value)
                :to-equal (subseq source 0 directive-start))
        (expect (node-property table :tblfm) :to-equal '("$1=$2" "$2=$1"))
        (expect (node-property table :reader-tblfm-ranges)
                :to-equal (list (cons directive-start first-directive-end)
                                (cons second-directive-start directive-end)))
        (expect (node-end table) :to-be directive-end)
        (expect (node-children table) :to-equal '()))))

  (it "emits table.el from retained source because value and formula properties are informational"
    (let* ((source (format nil "+---+~%| x |~%+---+~%#+TBLFM: $1=$1+1~%"))
           (document (parse-org source))
           (section (first (node-children (document-root document))))
           (table (first (node-children section))))
      (setf (node-property table :value) "edited value"
            (node-property table :tblfm) '("$1=$1+99"))
      (expect (emit-org table) :to-equal source)))
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

  (it "keeps numeric-looking text without a list separator in a paragraph"
    (let* ((source (format nil "123~%1.foo~%"))
           (document (parse-org source))
           (section (first (node-children (document-root document))))
           (paragraph (first (node-children section))))
      (with-soft-assertions
        (expect (node-type paragraph) :to-be :paragraph)
        (expect (node-text paragraph) :to-equal source))))

  (it "keeps incomplete ordered counters as list item text"
    (let* ((source (format nil "- [@3~%- [@3x~%- [@34~%- [@] empty~%"))
           (document (parse-org source))
           (list (first (node-children (first (node-children (document-root document))))))
           (items (node-children list)))
      (with-soft-assertions
        (expect (node-property list :type) :to-be :unordered)
        (expect (length items) :to-be 4)
        (expect (mapcar (lambda (item) (node-property item :counter)) items)
                :to-equal '(nil nil nil nil))
        (expect (mapcar (lambda (item) (collection-node-content document item)) items)
                :to-equal (list (format nil "[@3~%")
                                (format nil "[@3x~%")
                                (format nil "[@34~%")
                                (format nil "[@] empty~%"))))))

  (it "keeps empty descriptive tags and descriptions distinct"
    (let* ((empty-tag-document (parse-org (format nil "+ :: body~%")))
           (empty-tag-list
            (first (node-children
                    (first (node-children (document-root empty-tag-document))))))
           (empty-tag-item (first (node-children empty-tag-list)))
           (empty-description-document (parse-org "+ Term ::"))
           (empty-description-list
            (first (node-children
                    (first (node-children (document-root empty-description-document))))))
           (empty-description-item (first (node-children empty-description-list))))
      (with-soft-assertions
        (expect (node-property empty-tag-list :type) :to-be :unordered)
        (expect (node-property empty-tag-item :tag) :to-be nil)
        (expect (collection-node-content empty-tag-document empty-tag-item)
                :to-equal (format nil ":: body~%"))
        (expect (node-property empty-description-list :type) :to-be :descriptive)
        (expect (node-property empty-description-item :tag) :to-equal '("Term"))
        (expect (node-contents-begin empty-description-item) :to-be nil)
        (expect (node-contents-end empty-description-item) :to-be nil))))

  (it "preserves trailing list blanks and empty items at end of input"
    (let* ((trailing-source (format nil "- item~%~%"))
           (trailing-document (parse-org trailing-source))
           (trailing-list
            (first (node-children
                    (first (node-children (document-root trailing-document))))))
           (empty-document (parse-org "- "))
           (empty-list
            (first (node-children
                    (first (node-children (document-root empty-document))))))
           (empty-item (first (node-children empty-list))))
      (with-soft-assertions
        (expect (node-type trailing-list) :to-be :plain-list)
        (expect (length (node-children trailing-list)) :to-be 1)
        (expect (node-post-blank trailing-list) :to-be 1)
        (expect (node-contents-begin empty-item) :to-be nil)
        (expect (node-contents-end empty-item) :to-be nil)
        (expect (node-children empty-item) :to-equal '()))))

  (it "parses an empty first table cell between adjacent pipes"
    (let* ((document (parse-org "|| value |"))
           (table (first (node-children (first (node-children (document-root document))))))
           (row (first (node-children table)))
           (cells (node-children row)))
      (with-soft-assertions
        (expect (node-type table) :to-be :table)
        (expect (length cells) :to-be 2)
        (expect (collection-node-content document (first cells)) :to-equal "")
        (expect (collection-node-content document (second cells)) :to-equal "value"))))
)

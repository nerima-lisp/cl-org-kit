;;;; t/edit-properties-test.lisp
(in-package #:cl-org-kit/test)

(defun edit-properties-test-headlines (document)
  (remove-if-not (lambda (node) (eq (node-type node) :headline))
                 (let ((nodes '()))
                   (map-nodes (lambda (node)
                                (when (eq (node-type node) :headline)
                                  (push node nodes)))
                              (document-root document))
                   (nreverse nodes))))

(defun edit-properties-test-headline (document)
  (first (edit-properties-test-headlines document)))

(defun edit-properties-test-section (headline)
  (find :section (node-children headline) :key #'node-type))

(defun edit-properties-test-property (headline key)
  (let* ((section (edit-properties-test-section headline))
         (drawer (and section
                      (find :property-drawer (node-children section)
                            :key #'node-type))))
    (and drawer
         (find key (node-children drawer)
               :key (lambda (node) (node-property node :key))
               :test #'string=))))

(defun edit-properties-test-keywords (document key)
  (let* ((root-children (node-children (document-root document)))
         (first (first root-children))
         (children (if (and first (eq (node-type first) :section))
                       (node-children first)
                       root-children)))
    (remove-if-not
     (lambda (node)
       (and (eq (node-type node) :keyword)
            (string= key (node-property node :key))))
     children)))

(defun edit-properties-test-planning (headline)
  (let ((section (edit-properties-test-section headline)))
    (and section
         (find :planning (node-children section) :key #'node-type))))

(defun edit-properties-test-replace-span (source start end replacement)
  (concatenate 'string (subseq source 0 start) replacement (subseq source end)))

(defun edit-properties-test-check-patch
    (input edited original start end replacement)
  (with-soft-assertions
    (expect (document-text input) :to-equal original)
    (expect (document-text edited)
            :to-equal (edit-properties-test-replace-span
                       original start end replacement))))

(defun edit-properties-test-check-error (thunk reason document original)
  (handler-case
      (progn
        (funcall thunk)
        (expect nil :to-be t))
    (org-edit-error (condition)
      (with-soft-assertions
        (expect (org-edit-error-reason condition) :to-be reason)
        (expect (stringp (org-edit-error-target condition)) :to-be t)
        (expect (<= (length (org-edit-error-target condition)) 80) :to-be t)
        (expect (document-text document) :to-equal original)))))

(defun edit-properties-test-utf8-bom-octets (source)
  (cl-codec-kit:string-to-octets
   (concatenate 'string (string (code-char #xfeff)) source)
   :encoding :utf-8))

(defun edit-properties-test-utf8-document (source)
  (parse-org (edit-properties-test-utf8-bom-octets source) :encoding :utf-8))

(defun edit-properties-test-expect-utf8-bytes (document source)
  (with-soft-assertions
    (expect (document-bom-p document) :to-be t)
    (expect (write-org document :output-type :octets)
            :to-equalp (edit-properties-test-utf8-bom-octets source))))

(defun edit-properties-test-line-after (source start)
  (let ((newline (position #\Newline source :start start)))
    (if newline (1+ newline) (length source))))

(defun edit-properties-test-canonical-headline (document title)
  (let ((headline (find title (edit-properties-test-headlines document)
                        :key (lambda (node) (node-property node :raw-value))
                        :test #'string=)))
    (and headline (org-kit::edit-canonical-node headline))))

(describe "headline property edits"
  (it "updates one property value while preserving every other source character"
    (let* ((source (format nil "* One~%:PROPERTIES:~%:ID: old~%:OWNER: Ada~%:END:~%Body~%* Two~%Body two~%"))
           (document (parse-org source))
           (other-before (edit-properties-test-canonical-headline document "Two"))
           (headline (edit-properties-test-headline document))
           (start (search "old" source))
           (edited (set-property document headline "ID" "new")))
      (edit-properties-test-check-patch document edited source start (+ start 3) "new")
      (with-soft-assertions
        (expect (node-property (edit-properties-test-property
                                (edit-properties-test-headline edited) "ID") :value)
                :to-equal "new")
        (expect (node-property (edit-properties-test-property
                                (edit-properties-test-headline edited) "OWNER") :value)
                :to-equal "Ada")
        (expect (edit-properties-test-canonical-headline edited "Two")
                :to-equalp other-before))))

  (it "creates a property drawer after planning and before section text"
    (let* ((source (format nil "* Heading~%SCHEDULED: <2026-10-13 Tue>~%body~%* Other~%"))
           (document (parse-org source))
           (headline (edit-properties-test-headline document))
           (planning (edit-properties-test-planning headline))
           (offset (edit-properties-test-line-after source (node-begin planning)))
           (insertion (format nil ":PROPERTIES:~%:OWNER: Lee~%:END:~%"))
           (edited (set-property document headline "OWNER" "Lee")))
      (edit-properties-test-check-patch document edited source offset offset insertion)
      (with-soft-assertions
        (expect (node-property (edit-properties-test-property
                                (edit-properties-test-headline edited) "OWNER") :value)
                :to-equal "Lee")
        (expect (node-property (edit-properties-test-planning
                                (edit-properties-test-headline edited)) :scheduled)
                :to-be-truthy))))

  (it "adds a property to an existing drawer and removes a non-final property locally"
    (let* ((source (format nil "* Heading~%:PROPERTIES:~%:ID: 7~%:END:~%body~%"))
           (document (parse-org source))
           (headline (edit-properties-test-headline document))
           (drawer (find :property-drawer
                         (node-children (edit-properties-test-section headline))
                         :key #'node-type))
           (close-start (search ":END:" source :start2 (node-begin drawer)))
           (insertion (format nil ":OWNER: Ada~%"))
           (added (set-property document headline "OWNER" "Ada"))
           (added-source (edit-properties-test-replace-span
                          source close-start close-start insertion))
           (added-headline (edit-properties-test-headline added))
           (owner (edit-properties-test-property added-headline "OWNER"))
           (start (node-begin owner))
           (end (node-end owner))
           (removed (remove-property added added-headline "OWNER")))
      (edit-properties-test-check-patch document added source
                                        close-start close-start insertion)
      (edit-properties-test-check-patch added removed added-source start end "")
      (with-soft-assertions
        (expect (node-property (edit-properties-test-property
                                (edit-properties-test-headline added) "ID") :value)
                :to-equal "7")
        (expect (edit-properties-test-property
                 (edit-properties-test-headline removed) "OWNER")
                :to-be nil))))

  (it "removes the entire drawer when its last property is removed"
    (let* ((source (format nil "* 見出し~C~C:PROPERTIES:~C~C:ID: 古い値~C~C:END:~C~C本文 A~C~C次 B~C~C"
                           #\Return #\Newline #\Return #\Newline
                           #\Return #\Newline #\Return #\Newline
                           #\Return #\Newline #\Return #\Newline))
           (document (edit-properties-test-utf8-document source))
           (headline (edit-properties-test-headline document))
           (section (edit-properties-test-section headline))
           (drawer (find :property-drawer (node-children section) :key #'node-type))
           (start (node-begin drawer))
           (end (node-end drawer))
           (expected (edit-properties-test-replace-span source start end ""))
           (edited (remove-property document headline "ID")))
      (edit-properties-test-check-patch document edited source start end "")
      (edit-properties-test-expect-utf8-bytes edited expected)
      (with-soft-assertions
        (expect (edit-properties-test-property
                 (edit-properties-test-headline edited) "ID")
                :to-be nil)
        (expect (edit-properties-test-section
                 (edit-properties-test-headline edited))
                :to-be nil))))

  (it "preserves UTF-8 property insertion bytes around a BOM and CRLF source"
    (let* ((source (format nil "* 見出し~C~C本文前 café~C~C本文後 東京~C~C"
                           #\Return #\Newline #\Return #\Newline #\Return #\Newline))
           (document (edit-properties-test-utf8-document source))
           (headline (edit-properties-test-headline document))
           (offset (edit-properties-test-line-after source
                                                    (node-post-affiliated headline)))
           (insertion (format nil ":PROPERTIES:~C~C:OWNER: 有人~C~C:END:~C~C"
                              #\Return #\Newline #\Return #\Newline
                              #\Return #\Newline))
           (expected (edit-properties-test-replace-span
                      source offset offset insertion))
           (edited (set-property document headline "OWNER" "有人")))
      (edit-properties-test-check-patch document edited source offset offset insertion)
      (edit-properties-test-expect-utf8-bytes edited expected)
      (with-soft-assertions
        (expect (node-property
                 (edit-properties-test-property
                  (edit-properties-test-headline edited) "OWNER") :value)
                :to-equal "有人"))))

  (it "rejects duplicate keys, missing keys and values that cross a line"
    (let* ((source (format nil "* Heading~%:PROPERTIES:~%:ID: one~%:ID: two~%:END:~%"))
           (document (parse-org source))
           (headline (edit-properties-test-headline document)))
      (edit-properties-test-check-error
       (lambda () (set-property document headline "ID" "new"))
       :ambiguous-target document source)
      (edit-properties-test-check-error
       (lambda () (remove-property document headline "ID"))
       :ambiguous-target document source)
      (edit-properties-test-check-error
       (lambda () (remove-property document headline "MISSING"))
       :target-not-found document source)
      (edit-properties-test-check-error
       (lambda () (set-property document headline "ID" (format nil "bad~%value")))
       :invalid-value document source)))

  (it "reports wrong node types and malformed property keys in edit slots"
    (let* ((source (format nil "#+TITLE: Doc~%* Heading~%"))
           (document (parse-org source))
           (keyword (first (edit-properties-test-keywords document "TITLE")))
           (headline (edit-properties-test-headline document)))
      (edit-properties-test-check-error
       (lambda () (set-property document keyword "ID" "x"))
       :wrong-node-type document source)
      (edit-properties-test-check-error
       (lambda () (set-property document headline "BAD:KEY" "x"))
       :invalid-value document source))))

(describe "planning metadata edits"
  (it "sets and removes SCHEDULED, DEADLINE and CLOSED with local patches"
    (dolist (entry '((:scheduled "SCHEDULED")
                     (:deadline "DEADLINE")
                     (:closed "CLOSED")))
      (let* ((kind (first entry))
             (label (second entry))
             (source (format nil "* 計画~%本文~%* Other~%"))
             (document (parse-org source))
             (headline (edit-properties-test-headline document))
             (offset (edit-properties-test-line-after source
                                                      (node-post-affiliated headline)))
             (line (format nil "~A: <2026-10-13 Tue>~%" label))
             (edited (set-planning document headline kind "<2026-10-13 Tue>"))
             (planning (edit-properties-test-planning
                        (edit-properties-test-headline edited)))
             (planning-line-start (search (format nil "~A:" label)
                                          (document-text edited)))
             (line-end (position #\Newline (document-text edited)
                                 :start planning-line-start))
             (removed (remove-planning edited
                                       (edit-properties-test-headline edited)
                                       kind)))
        (edit-properties-test-check-patch document edited source offset offset line)
        (edit-properties-test-check-patch edited removed (document-text edited)
                                          planning-line-start (1+ line-end) "")
        (with-soft-assertions
          (expect (node-property planning kind) :to-be-truthy)
          (expect (edit-properties-test-planning
                   (edit-properties-test-headline removed))
                  :to-be nil)
          (expect (edit-properties-test-canonical-headline removed "Other")
                  :to-equalp (edit-properties-test-canonical-headline
                              document "Other"))))))

  (it "updates one planning timestamp and adds or removes a field in a shared line"
    (let* ((source (format nil "* Task~%SCHEDULED: <2026-10-13 Tue>~%"))
           (document (parse-org source))
           (headline (edit-properties-test-headline document))
           (old-start (search "<2026-10-13 Tue>" source))
           (updated (set-planning document headline :scheduled "<2026-10-14 Wed>"))
           (updated-source (edit-properties-test-replace-span
                           source old-start (+ old-start (length "<2026-10-13 Tue>"))
                           "<2026-10-14 Wed>"))
           (updated-headline (edit-properties-test-headline updated))
           (new-source (document-text updated))
           (planning-line-start (search "SCHEDULED:" new-source))
           (line-end (position #\Newline new-source :start planning-line-start))
           (deadline-text " DEADLINE: <2026-10-15 Thu>")
           (added (set-planning updated updated-headline :deadline
                                "<2026-10-15 Thu>"))
           (added-source (edit-properties-test-replace-span
                          new-source line-end line-end deadline-text))
           (added-headline (edit-properties-test-headline added))
           (scheduled-start (search "SCHEDULED:" added-source))
           (timestamp-start (search "<2026-10-14 Wed>" added-source))
           (removed (remove-planning added added-headline :scheduled)))
      (edit-properties-test-check-patch document updated source old-start
                                        (+ old-start (length "<2026-10-13 Tue>"))
                                        "<2026-10-14 Wed>")
      (edit-properties-test-check-patch updated added new-source line-end line-end
                                        deadline-text)
      (edit-properties-test-check-patch
       added removed added-source scheduled-start
       (+ timestamp-start (length "<2026-10-14 Wed>") 1) "")
      (with-soft-assertions
        (expect (node-property (edit-properties-test-planning
                                (edit-properties-test-headline added)) :deadline)
                :to-be-truthy)
        (expect (node-property (edit-properties-test-planning
                                (edit-properties-test-headline removed)) :scheduled)
                :to-be nil)
        (expect (node-property (edit-properties-test-planning
                                (edit-properties-test-headline removed)) :deadline)
                :to-be-truthy))))

  (it "preserves UTF-8 BOM, CRLF and non-ASCII source bytes while adding planning"
    (let* ((source (format nil "* 見出し~C~C本文前 café~C~C本文後 東京~C~C"
                           #\Return #\Newline #\Return #\Newline #\Return #\Newline))
           (document (edit-properties-test-utf8-document source))
           (headline (edit-properties-test-headline document))
           (offset (edit-properties-test-line-after source (node-post-affiliated headline)))
           (line (format nil "SCHEDULED: <2026-10-13 Tue>~C~C" #\Return #\Newline))
           (expected (edit-properties-test-replace-span source offset offset line))
           (edited (set-planning document headline :scheduled "<2026-10-13 Tue>")))
      (edit-properties-test-check-patch document edited source offset offset line)
      (edit-properties-test-expect-utf8-bytes edited expected)))

  (it "keeps blank lines after a newly inserted planning line"
    (let* ((source (format nil "* Task~%~%body~%"))
           (document (parse-org source))
           (headline (edit-properties-test-headline document))
           (offset (edit-properties-test-line-after source
                                                    (node-post-affiliated headline)))
           (line (format nil "SCHEDULED: <2026-10-13 Tue>~%"))
           (expected-source (edit-properties-test-replace-span
                             source offset offset line))
           (edited (set-planning document headline :scheduled
                                 "<2026-10-13 Tue>")))
      (edit-properties-test-check-patch document edited source offset offset line)
      (with-soft-assertions
        (expect (document-text edited) :to-equal expected-source)
        (expect (org-kit::edit-canonical-document edited)
                :to-equalp (org-kit::edit-canonical-document
                            (parse-org expected-source))))))

  (it "rejects invalid timestamps and absent planning fields with reason slots"
    (let* ((source (format nil "* Heading~%"))
           (document (parse-org source))
           (headline (edit-properties-test-headline document)))
      (edit-properties-test-check-error
       (lambda () (set-planning document headline :scheduled "not a timestamp"))
       :invalid-value document source)
      (edit-properties-test-check-error
       (lambda () (remove-planning document headline :closed))
       :target-not-found document source)
      (edit-properties-test-check-error
       (lambda () (set-planning document headline :planning "<2026-10-13 Tue>"))
       :invalid-value document source))))

(describe "document keyword edits"
  (it "updates a keyword value while preserving UTF-8 BOM, CRLF and surrounding bytes"
    (let* ((source (format nil "#+TITLE: 古い題名~C~C#+LANGUAGE: ja~C~C* 見出し~C~C本文 café 東京~C~C"
                           #\Return #\Newline #\Return #\Newline
                           #\Return #\Newline #\Return #\Newline))
           (document (edit-properties-test-utf8-document source))
           (keyword (first (edit-properties-test-keywords document "TITLE")))
           (start (search "古い題名" source))
           (edited (set-keyword-value document keyword "新しい題名"))
           (expected (edit-properties-test-replace-span
                      source start (+ start (length "古い題名")) "新しい題名")))
      (edit-properties-test-check-patch document edited source
                                        start (+ start (length "古い題名")) "新しい題名")
      (edit-properties-test-expect-utf8-bytes edited expected)
      (with-soft-assertions
        (expect (node-property
                 (first (edit-properties-test-keywords edited "TITLE")) :value)
                :to-equal "新しい題名")
        (expect (mapcar (lambda (node) (node-property node :key))
                        (edit-properties-test-keywords edited "LANGUAGE"))
                :to-equal '("LANGUAGE")))))

  (it "adds an absent selector after the existing preamble keyword group"
    (let* ((source (format nil "#+TITLE: Doc~%~%* Heading~%"))
           (document (parse-org source))
           (last-keyword (first (edit-properties-test-keywords document "TITLE")))
           (offset (node-end last-keyword))
           (insertion (format nil "#+CUSTOM: Value~%"))
           (edited (set-keyword-value document '(:keyword-key "CUSTOM") "Value")))
      (edit-properties-test-check-patch document edited source offset offset insertion)
      (with-soft-assertions
        (expect (node-property
                 (first (edit-properties-test-keywords edited "CUSTOM")) :value)
                :to-equal "Value")
        (expect (node-property
                 (first (edit-properties-test-keywords edited "TITLE")) :value)
                :to-equal "Doc"))))

  (it "removes a selected keyword occurrence and preserves other document keywords"
    (let* ((source (format nil "#+TITLE: One~%#+TITLE: Two~%#+LANGUAGE: ja~%* Heading~%"))
           (document (parse-org source))
           (keywords (edit-properties-test-keywords document "TITLE"))
           (target-node (second keywords))
           (start (node-begin target-node))
           (end (node-end target-node))
           (expected (edit-properties-test-replace-span source start end ""))
           (edited (remove-keyword document '(:keyword-key "TITLE" 2))))
      (edit-properties-test-check-patch document edited source start end "")
      (with-soft-assertions
        (expect (mapcar (lambda (node) (node-property node :value))
                        (edit-properties-test-keywords edited "TITLE"))
                :to-equal '("One"))
        (expect (node-property
                 (first (edit-properties-test-keywords edited "LANGUAGE")) :value)
                :to-equal "ja")
        (expect (document-text edited) :to-equal expected))))

  (it "rejects ambiguous and missing selectors and invalid values with slots"
    (let* ((source (format nil "#+TITLE: One~%#+TITLE: Two~%* Heading~%"))
           (document (parse-org source)))
      (edit-properties-test-check-error
       (lambda () (set-keyword-value document '(:keyword-key "TITLE") "new"))
       :ambiguous-target document source)
      (edit-properties-test-check-error
       (lambda () (remove-keyword document '(:keyword-key "TITLE")))
       :ambiguous-target document source)
      (edit-properties-test-check-error
       (lambda () (remove-keyword document '(:keyword-key "TITLE" 3)))
       :target-not-found document source)
      (edit-properties-test-check-error
       (lambda () (set-keyword-value document '(:keyword-key "EXTRA")
                                     (format nil "bad~%value")))
       :invalid-value document source))))

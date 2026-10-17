(in-package #:cl-org-kit/test)

(defun edit-footnotes-test-nodes (document type)
  (let ((nodes '()))
    (map-nodes (lambda (node)
                 (when (eq (node-type node) type)
                   (push node nodes)))
               (document-root document))
    (nreverse nodes)))

(defun edit-footnotes-test-error (thunk)
  (handler-case
      (progn (funcall thunk) '(:no-error))
    (org-edit-error (condition)
      (list (org-edit-error-reason condition)
            (org-edit-error-target condition)))))

(defun edit-footnotes-test-utf8-bom-octets (text)
  (cl-codec-kit:string-to-octets
   (concatenate 'string (string (code-char #xfeff)) text)
   :encoding :utf-8))

(describe "format-preserving footnote edits"
  (it "inserts the next numeric label and reports unmatched labels"
    (let* ((source (format nil "Text [fn:4] ~%"))
           (document (parse-org source))
           (offset (position #\Newline source))
           (result nil)
           (label nil)
           (undefined nil)
           (unreferenced nil))
      (multiple-value-setq (result label undefined unreferenced)
        (insert-footnote document offset "new body"))
      (with-soft-assertions
        (expect (edit-result-p result) :to-be t)
        (expect label :to-equal "5")
        (expect undefined :to-equal '("4"))
        (expect unreferenced :to-equal nil)
        (expect (document-text (edit-result-updated-document result))
                :to-equal (format nil "Text [fn:4] [fn:5]~%[fn:5] new body~%"))
        (expect (document-text document) :to-equal source)
        (expect (length (edit-result-spans result)) :to-be 2))))

  (it "inserts the reference and definition on separate lines at EOF"
    (let* ((document (parse-org ""))
           (result nil)
           (label nil)
           (undefined nil)
           (unreferenced nil))
      (multiple-value-setq (result label undefined unreferenced)
        (insert-footnote document 0 "body"))
      (with-soft-assertions
        (expect label :to-equal "1")
        (expect (document-text (edit-result-updated-document result))
                :to-equal (format nil " [fn:1]~%[fn:1] body"))
        (expect undefined :to-equal nil)
        (expect unreferenced :to-equal nil))))

  (it "renumbers references and definitions by first reference while reporting unused definitions"
    (let* ((source (format nil
                           "Use [fn:5] then [fn:2].~%[fn:2] two~%[fn:5] five~%[fn:8] orphan~%"))
           (document (parse-org source))
           (result nil)
           (undefined nil)
           (unreferenced nil))
      (multiple-value-setq (result undefined unreferenced)
        (renumber-footnotes document))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result))
                :to-equal (format nil
                                  "Use [fn:1] then [fn:2].~%[fn:2] two~%[fn:1] five~%[fn:3] orphan~%"))
        (expect undefined :to-equal nil)
        (expect unreferenced :to-equal '("3"))
        (expect (document-text document) :to-equal source))))

  (it "collects definitions at the end in reference order and reports unresolved labels"
    (let* ((source (format nil
                           "First [fn:2] and [fn:missing].~%[fn:1] one~%* Next~%Ref [fn:1] and [fn:9].~%[fn:2] two~%[fn:unused] unused~%"))
           (document (parse-org source))
           (result nil)
           (undefined nil)
           (unreferenced nil))
      (multiple-value-setq (result undefined unreferenced)
        (normalize-footnotes document))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result))
                :to-equal (format nil
                                  "First [fn:2] and [fn:missing].~%* Next~%Ref [fn:1] and [fn:9].~%[fn:2] two~%[fn:1] one~%[fn:unused] unused~%"))
        (expect undefined :to-equal '("missing" "9"))
        (expect unreferenced :to-equal '("unused"))
        (expect (document-text document) :to-equal source)
        (expect (mapcar (lambda (node) (node-property node :label))
                        (edit-footnotes-test-nodes
                         (edit-result-updated-document result)
                         :footnote-definition))
                :to-equal '("2" "1" "unused")))))

  (it "keeps first-reference order when labels repeat"
    (let* ((source (format nil
                           "Use [fn:2], [fn:1], then [fn:2].~%[fn:1] one~%[fn:2] two~%"))
           (document (parse-org source))
           (result (normalize-footnotes document))
           (updated (edit-result-updated-document result)))
      (with-soft-assertions
        (expect (document-text updated)
                :to-equal (format nil
                                  "Use [fn:2], [fn:1], then [fn:2].~%[fn:2] two~%[fn:1] one~%"))
        (expect (mapcar (lambda (node) (node-property node :label))
                        (edit-footnotes-test-nodes updated :footnote-definition))
                :to-equal '("2" "1")))))

  (it "preserves unrelated UTF-8 bytes and CRLF while moving a definition"
    (let* ((crlf (format nil "~C~C" #\Return #\Newline))
           (source (concatenate 'string "前" crlf
                                "[fn:2] 二" crlf
                                "* 見出し" crlf
                                "後 [fn:2]" crlf))
           (document (parse-org (edit-footnotes-test-utf8-bom-octets source)
                                :encoding :utf-8))
           (definition (first (edit-footnotes-test-nodes
                               document :footnote-definition)))
           (expected (concatenate 'string "前" crlf
                                  "* 見出し" crlf
                                  "後 [fn:2]" crlf
                                  "[fn:2] 二" crlf))
           (result (normalize-footnotes document))
           (updated (edit-result-updated-document result)))
      (with-soft-assertions
        (expect (write-org updated :output-type :octets)
                :to-equalp (edit-footnotes-test-utf8-bom-octets expected))
        (expect (mapcar (lambda (span)
                          (list (edit-span-start span) (edit-span-end span)))
                        (edit-result-spans result))
                :to-equal
                (list (list (node-begin definition) (node-end definition))
                      (list (length source) (length source)))))))

  (it "checks output limits before generating the expected preview"
    (let* ((source (format nil "Text.~%"))
           (document (parse-org source
                                :limits (make-limits
                                         :max-output-length (length source))))
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
                   (insert-footnote document 0 "body")
                   (expect nil :to-be t))
               (org-resource-limit-error (condition)
                 (with-soft-assertions
                   (expect (org-resource-limit-error-limit-name condition)
                           :to-be :max-output-length)
                   (expect (org-resource-limit-error-limit condition)
                           :to-be (length source))
                   (expect (> (org-resource-limit-error-actual condition)
                             (length source))
                           :to-be t)))))
        (setf (symbol-function 'org-kit::edit-splice-source) original-splicer))
      (with-soft-assertions
        (expect splice-called :to-be nil)
        (expect (document-text document) :to-equal source))))

  (it "checks the retained definition source limit before preview splicing"
    (let* ((source (format nil "Text~%[fn:1] body~%"))
           (document (parse-org source
                                :limits (make-limits :max-output-length 4)))
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
                   (normalize-footnotes document)
                   (expect nil :to-be t))
               (org-resource-limit-error (condition)
                 (with-soft-assertions
                   (expect (org-resource-limit-error-limit-name condition)
                           :to-be :max-output-length)
                   (expect (org-resource-limit-error-limit condition) :to-be 4)
                   (expect (> (org-resource-limit-error-actual condition) 4)
                           :to-be t)))))
        (setf (symbol-function 'org-kit::edit-splice-source) original-splicer))
      (with-soft-assertions
        (expect splice-called :to-be nil)
        (expect (document-text document) :to-equal source))))

  (it "checks the final normalization output limit before preview splicing"
    (let* ((source (format nil "Text~%[fn:1] body~%"))
           (limit (1- (length source)))
           (document (parse-org source
                                :limits (make-limits :max-output-length limit)))
           (original-splicer (symbol-function 'org-kit::edit-splice-source))
           (splice-count 0))
      (unwind-protect
           (progn
             (setf (symbol-function 'org-kit::edit-splice-source)
                   (lambda (&rest arguments)
                     (incf splice-count)
                     (if (= splice-count 1)
                         (apply original-splicer arguments)
                         (error "Final source splice ran before the output limit check"))))
             (handler-case
                 (progn
                   (normalize-footnotes document)
                   (expect nil :to-be t))
               (org-resource-limit-error (condition)
                 (with-soft-assertions
                   (expect (org-resource-limit-error-limit-name condition)
                           :to-be :max-output-length)
                   (expect (org-resource-limit-error-limit condition) :to-be limit)
                   (expect (org-resource-limit-error-actual condition)
                           :to-be (length source))))))
        (setf (symbol-function 'org-kit::edit-splice-source) original-splicer))
      (with-soft-assertions
        (expect splice-count :to-be 1)
        (expect (document-text document) :to-equal source))))

  (it "covers footnote line endings, named labels and unchanged normalization"
    (let* ((crlf (format nil "~C~C" #\Return #\Newline))
           (source (concatenate 'string "Text" crlf))
           (document (parse-org source))
           (inserted (edit-result-updated-document
                      (insert-footnote document 0 "body")))
           (line-start-source (format nil "Text~%Next~%"))
           (line-start-document (parse-org line-start-source))
           (line-start-result (edit-result-updated-document
                               (insert-footnote line-start-document
                                                (search "Next" line-start-source)
                                                "body")))
           (referenced (parse-org (format nil "Text [fn:named].~%")))
           (renumbered (renumber-footnotes referenced))
           (definition-only (parse-org
                             (format nil "Text~%[fn:z] z~%[fn:a] a~%* Next")))
           (unreferenced-before-referenced
             (normalize-footnotes
              (parse-org (format nil
                                 "Use [fn:1]~%* Heading~%[fn:unused] u~%[fn:1] one"))))
           (normalized (normalize-footnotes definition-only))
           (empty-normalize (normalize-footnotes (parse-org "[fn:missing]")))
           (already-normalized (normalize-footnotes
                                (parse-org (format nil "Text~%[fn:1] body~%")))))
      (with-soft-assertions
        (expect (document-text inserted)
                :to-equal (concatenate 'string " [fn:1]" crlf "Text" crlf
                                       "[fn:1] body" crlf))
        (expect (document-text line-start-result)
                :to-equal (format nil "Text~% [fn:1]~%Next~%[fn:1] body~%"))
        (expect (document-text (edit-result-updated-document renumbered))
                :to-equal (format nil "Text [fn:named].~%"))
        (expect (edit-result-spans renumbered) :to-equal nil)
        (expect (document-text (edit-result-updated-document normalized))
                :to-equal (format nil "Text~%* Next~%[fn:z] z~%[fn:a] a~%"))
        (expect (length (edit-result-spans normalized)) :to-be 3)
        (expect (document-text (edit-result-updated-document
                                unreferenced-before-referenced))
                :to-equal (format nil
                                  "Use [fn:1]~%* Heading~%[fn:1] one~%[fn:unused] u~%"))
        (expect (edit-result-spans empty-normalize) :to-equal nil)
        (expect (document-text (edit-result-updated-document empty-normalize))
                :to-equal "[fn:missing]")
        (expect (edit-result-spans already-normalized) :to-equal nil)
        (expect (document-text (edit-result-updated-document already-normalized))
                :to-equal (format nil "Text~%[fn:1] body~%"))
        (expect (handler-case (progn (renumber-footnotes nil) nil)
                  (type-error () t)) :to-be t)
        (expect (handler-case (progn (normalize-footnotes nil) nil)
                  (type-error () t)) :to-be t))))

  (it "adds a separator when a retained definition span omits its line ending"
    (let* ((source (format nil "[fn:1] one~%[fn:2] two"))
           (document (parse-org source))
           (definitions (edit-footnotes-test-nodes document :footnote-definition)))
      (setf (org-kit::%node-end (first definitions))
            (1- (node-end (first definitions))))
      (let* ((result (normalize-footnotes document))
             (updated (edit-result-updated-document result)))
        (with-soft-assertions
          (expect (document-text updated)
                  :to-equal (format nil "~%[fn:1] one~%[fn:2] two"))
          (expect (document-text document) :to-equal source)))))

  (it "rejects invalid insertion inputs with an edit condition and reason"
    (let* ((source (format nil "Text.~%"))
           (document (parse-org source))
           (crlf-source (concatenate 'string "Text." (format nil "~C~C" #\Return #\Newline)))
           (crlf-document (parse-org crlf-source))
           (crlf-split (position #\Newline crlf-source))
           (failure (edit-footnotes-test-error
                     (lambda () (insert-footnote document 99 "body")))))
      (with-soft-assertions
        (expect failure :to-equal '(:invalid-value "99"))
        (expect (edit-footnotes-test-error
                 (lambda () (insert-footnote crlf-document crlf-split "body")))
                :to-equal (list :invalid-value (write-to-string crlf-split)))
        (expect (edit-footnotes-test-error
                 (lambda () (insert-footnote document 0 "")))
                :to-equal '(:invalid-value "footnote body"))
        (expect (edit-footnotes-test-error
                 (lambda () (insert-footnote document "bad" "body")))
                :to-equal '(:invalid-value "bad"))
        (expect (edit-footnotes-test-error
                 (lambda () (insert-footnote document -1 "body")))
                :to-equal '(:invalid-value "-1"))
        (expect (edit-footnotes-test-error
                 (lambda () (insert-footnote document 0 nil)))
                :to-equal '(:invalid-value "footnote body"))
        (expect (edit-footnotes-test-error
                 (lambda () (insert-footnote document 0 (format nil "body~%"))))
                :to-equal '(:invalid-value "footnote body"))
        (expect (edit-footnotes-test-error
                 (lambda () (insert-footnote document 0 (concatenate 'string "body" (string #\Return)))))
                :to-equal '(:invalid-value "footnote body"))
        (expect (handler-case (progn (insert-footnote nil 0 "body") nil)
                  (type-error () t))
                :to-be t)
        (expect (document-text document) :to-equal source)))))

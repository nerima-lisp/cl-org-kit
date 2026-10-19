(in-package #:cl-org-kit/test)

(defun edit-drawers-test-headline (document)
  (find :headline
        (let ((nodes '()))
          (map-nodes (lambda (node)
                       (when (eq (node-type node) :headline)
                         (push node nodes)))
                     (document-root document))
          (nreverse nodes))
        :key #'node-type))

(defun edit-drawers-test-error (thunk)
  (handler-case
      (progn (funcall thunk) '(:no-error))
    (org-edit-error (condition)
      (list (org-edit-error-reason condition)
            (org-edit-error-target condition)))))

(defun edit-drawers-test-utf8-bom-octets (text)
  (cl-codec-kit:string-to-octets
   (concatenate 'string (string (code-char #xfeff)) text)
   :encoding :utf-8))

(describe "format-preserving drawer edits"
  (it "creates a drawer before existing headline content"
    (let* ((source (format nil "* Heading~%body stays~%* Next~%"))
           (document (parse-org source))
           (headline (edit-drawers-test-headline document))
           (result (set-drawer-body document headline "notes"
                                    (format nil "line one~%line two")))
           (updated (edit-result-updated-document result)))
      (with-soft-assertions
        (expect (edit-result-p result) :to-be t)
        (expect (document-text updated)
                :to-equal (format nil
                                  "* Heading~%:NOTES:~%line one~%line two~%:END:~%body stays~%* Next~%"))
        (expect (node-property
                 (find :drawer
                       (let ((nodes '()))
                         (map-nodes (lambda (node)
                                      (when (eq (node-type node) :drawer)
                                        (push node nodes)))
                                    (document-root updated))
                         (nreverse nodes))
                       :key #'node-type)
                 :drawer-name)
                :to-equal "NOTES")
        (expect (document-text document) :to-equal source)
        (expect (mapcar (lambda (span)
                          (list (edit-span-start span) (edit-span-end span)))
                        (edit-result-spans result))
                :to-equal (list (list (length (format nil "* Heading~%"))
                                      (length (format nil "* Heading~%"))))))))

  (it "replaces only a drawer body and removes the drawer"
    (let* ((source (format nil "* A~%:NOTES:~%old body~%:END:~%body~%"))
           (document (parse-org source))
           (headline (edit-drawers-test-headline document))
           (replacement (set-drawer-body document headline "NOTES" "fresh body"))
           (replacement-document (edit-result-updated-document replacement))
           (replacement-source
             (format nil "* A~%:NOTES:~%fresh body~%:END:~%body~%"))
           (replacement-headline (edit-drawers-test-headline replacement-document))
           (removed (remove-drawer replacement-document replacement-headline "notes")))
      (with-soft-assertions
        (expect (document-text replacement-document) :to-equal replacement-source)
        (expect (document-text (edit-result-updated-document removed))
                :to-equal (format nil "* A~%body~%"))
        (expect (document-text document) :to-equal source)
        (expect (length (edit-result-spans replacement)) :to-be 1)
        (expect (length (edit-result-spans removed)) :to-be 1))))

  (it "preserves unrelated UTF-8 bytes and CRLF when replacing a drawer body"
    (let* ((crlf (format nil "~C~C" #\Return #\Newline))
           (source (concatenate 'string "* 見出し" crlf
                                ":NOTES:" crlf
                                "古い本文" crlf
                                ":END:" crlf
                                "後続" crlf))
           (document (parse-org (edit-drawers-test-utf8-bom-octets source)
                                :encoding :utf-8))
           (headline (edit-drawers-test-headline document))
           (expected (concatenate 'string "* 見出し" crlf
                                  ":NOTES:" crlf
                                  "新しい本文" crlf
                                  ":END:" crlf
                                  "後続" crlf))
           (result (set-drawer-body document headline "NOTES" "新しい本文")))
      (with-soft-assertions
        (expect (write-org (edit-result-updated-document result)
                           :output-type :octets)
                :to-equalp (edit-drawers-test-utf8-bom-octets expected)))))

  (it "rejects reserved drawers, malformed bodies and unclosed drawers with reasons"
    (let* ((source (format nil "* A~%:NOTES:~%old~%:END:~%"))
           (document (parse-org source))
           (headline (edit-drawers-test-headline document))
           (unclosed (parse-org (format nil "* A~%:NOTES:~%old~%")))
           (unclosed-headline (edit-drawers-test-headline unclosed)))
      (with-soft-assertions
        (expect (edit-drawers-test-error
                 (lambda () (set-drawer-body document headline "PROPERTIES" "x")))
                :to-equal '(:unsupported-option "headline"))
        (expect (edit-drawers-test-error
                 (lambda () (set-drawer-body document headline "LOGBOOK" "x")))
                :to-equal '(:unsupported-option "headline"))
        (expect (edit-drawers-test-error
                 (lambda () (set-drawer-body document headline "NOTES"
                                             (format nil "x~%:END:"))))
                :to-equal '(:invalid-value "headline"))
        (expect (edit-drawers-test-error
                 (lambda () (set-drawer-body unclosed unclosed-headline "NOTES" "new")))
                :to-equal '(:uneditable-structure "headline"))
        (expect (edit-drawers-test-error
                 (lambda () (remove-drawer unclosed unclosed-headline "NOTES")))
                :to-equal '(:uneditable-structure "headline"))
        (expect (document-text document) :to-equal source)
        (expect (document-text unclosed) :to-equal (format nil "* A~%:NOTES:~%old~%")))))

  (it "checks output limits before generating the expected preview"
    (let* ((source (format nil "* A~%"))
           (document (parse-org source
                                :limits (make-limits
                                         :max-output-length (length source))))
           (headline (edit-drawers-test-headline document))
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
                   (set-drawer-body document headline "NOTES" "x")
                   (expect nil :to-be t))
               (org-resource-limit-error (condition)
                 (with-soft-assertions
                   (expect (org-resource-limit-error-limit-name condition)
                           :to-be :max-output-length)
                   (expect (org-resource-limit-error-limit condition)
                           :to-equal (length source))
                   (expect (> (org-resource-limit-error-actual condition)
                             (length source))
                           :to-be t)))))
        (setf (symbol-function 'org-kit::edit-splice-source) original-splicer))
      (with-soft-assertions
        (expect splice-called :to-be nil)
        (expect (document-text document) :to-equal source))))

  (it "reports a missing drawer when asked to remove it"
    (let* ((source (format nil "* A~%body~%"))
           (document (parse-org source))
           (headline (edit-drawers-test-headline document)))
      (with-soft-assertions
        (expect (edit-drawers-test-error
                 (lambda () (remove-drawer document headline "NOTES")))
                :to-equal '(:target-not-found "headline"))
        (expect (document-text document) :to-equal source))))

  (it "covers drawer validation and insertion boundaries"
    (let* ((document (parse-org "* A"))
           (headline (edit-drawers-test-headline document))
           (property-source (format nil "* A~%:PROPERTIES:~%:ID: x~%:END:~%"))
           (property-document (parse-org property-source))
           (property-headline (edit-drawers-test-headline property-document))
           (property-result (set-drawer-body property-document property-headline
                                             "notes" (format nil "line~%")))
           (empty-result (set-drawer-body document headline "notes" "")))
      (with-soft-assertions
        (dolist (name (list nil "" ":NOTES"
                            (concatenate 'string "NO" (string #\Newline) "TES")
                            (concatenate 'string "NO" (string #\Return) "TES")
                            "END"))
          (expect (edit-drawers-test-error
                   (lambda () (set-drawer-body document headline name "body")))
                  :to-equal '(:invalid-value "headline")))
        (expect (edit-drawers-test-error
                 (lambda () (set-drawer-body document headline "notes" nil)))
                :to-equal '(:invalid-value "headline"))
        (expect (edit-drawers-test-error
                 (lambda () (set-drawer-body document headline "notes"
                                             (format nil "old~C~C :END:~C" #\Return #\Newline #\Tab))))
                :to-equal '(:invalid-value "headline"))
        (expect (document-text (edit-result-updated-document empty-result))
                :to-equal (format nil "* A~%:NOTES:~%:END:"))
        (expect (document-text (edit-result-updated-document property-result))
                :to-equal (concatenate 'string property-source
                                       (format nil ":NOTES:~%line~%:END:~%")))
        (expect (handler-case
                    (progn (set-drawer-body nil "headline" "notes" "") nil)
                  (type-error () t))
                :to-be t)
        (expect (handler-case
                    (progn (remove-drawer nil "headline" "notes") nil)
                  (type-error () t))
                :to-be t))))

  (it "rejects an unclosed drawer whose final line has no newline"
    (let* ((document (parse-org (format nil "* A~%:NOTES:~%old")))
           (headline (edit-drawers-test-headline document)))
      (with-soft-assertions
        (expect (edit-drawers-test-error
                 (lambda () (remove-drawer document headline "NOTES")))
                :to-equal '(:uneditable-structure "headline")))))

  (it "rejects duplicate matching drawers as ambiguous"
    (let* ((source (format nil
                           "* A~%:NOTES:~%one~%:END:~%:NOTES:~%two~%:END:~%"))
           (document (parse-org source))
           (headline (edit-drawers-test-headline document)))
      (with-soft-assertions
        (expect (edit-drawers-test-error
                 (lambda () (set-drawer-body document headline "NOTES" "updated")))
                :to-equal '(:ambiguous-target "headline"))
        (expect (edit-drawers-test-error
                 (lambda () (remove-drawer document headline "NOTES")))
                :to-equal '(:ambiguous-target "headline"))
        (expect (document-text document) :to-equal source))))
  (it "rejects drawer bodies that escape their section and names with spaces"
      (let* ((create-source (format nil "* A~%body~%"))
             (create-document (parse-org create-source))
             (create-headline (edit-drawers-test-headline create-document))
             (replace-source (format nil "* A~%:NOTES:~%old~%:END:~%"))
             (replace-document (parse-org replace-source))
             (replace-headline (edit-drawers-test-headline replace-document)))
        (with-soft-assertions
         (expect (edit-drawers-test-error
                  (lambda ()
                    (set-drawer-body create-document create-headline "NOTES"
                                     (format nil "x~%* B"))))
                 :to-equal '(:invalid-value "headline"))
         (expect (edit-drawers-test-error
                  (lambda ()
                    (set-drawer-body replace-document replace-headline "NOTES"
                                     (format nil "x~%* B"))))
                 :to-equal '(:invalid-value "headline"))
         (expect (edit-drawers-test-error
                  (lambda ()
                    (set-drawer-body create-document create-headline
                                     "MY NOTES" "body")))
                 :to-equal '(:invalid-value "headline"))
         (expect (document-text create-document) :to-equal create-source)
         (expect (document-text replace-document) :to-equal replace-source))))
  (it "places drawers before child headlines, removes the sole drawer, and translates parser errors"
    (let* ((source (format nil "* Parent~%** Child~%"))
           (document (parse-org source))
           (parent (edit-drawers-test-headline document))
           (created (set-drawer-body document parent "notes" "body"))
           (updated (edit-result-updated-document created))
           (updated-parent (edit-drawers-test-headline updated))
           (removed (remove-drawer updated updated-parent "NOTES")))
      (with-soft-assertions
        (expect (document-text updated)
                :to-equal (format nil "* Parent~%:NOTES:~%body~%:END:~%** Child~%"))
        (expect (document-text (edit-result-updated-document removed))
                :to-equal source)))
  (let* ((document (parse-org (format nil "* A~%")))
         (headline (edit-drawers-test-headline document))
         (original-parser (symbol-function 'org-kit::parse-org)))
    (unwind-protect
         (progn
           (setf (symbol-function 'org-kit::parse-org)
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (error 'org-parse-error)))
           (with-soft-assertions
             (expect (edit-drawers-test-error
                      (lambda () (set-drawer-body document headline "NOTES" "body")))
                     :to-equal '(:invalid-value "headline"))
             (expect (document-text document) :to-equal (format nil "* A~%"))))
      (setf (symbol-function 'org-kit::parse-org) original-parser)))))

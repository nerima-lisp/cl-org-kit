(in-package #:cl-org-kit/test)
(defun capture-target-test-time-context ()
  (make-time-context :reference-time (make-org-time-value "2024-02-19T12:34:00")
                     :zone (resolve-time-zone "UTC")))
(defun capture-target-test-condition (thunk)
  (handler-case (progn (funcall thunk) nil)
    (error (condition) condition)))
(defun capture-target-test-expect-error (thunk reason)
  (let ((condition (capture-target-test-condition thunk)))
    (with-soft-assertions
      (expect (typep condition 'org-capture-error) :to-be t)
      (when (typep condition 'org-capture-error)
        (expect (org-capture-error-reason condition) :to-be reason)))))
(defun capture-target-test-template (text &optional (cursor-offset 0))
  (make-capture-template-result :expanded-text text
                                :cursor-offset cursor-offset))
(defun capture-target-test-id-document (source-name title identifier)
  (parse-org
   (format nil "* ~A~%:PROPERTIES:~%:ID: ~A~%:END:~%" title identifier)
   :source-name source-name))
(describe "capture target insertion"
  (it "inserts below an existing headline and preserves all untouched text"
    (let* ((source
             (format nil
                     "#+TITLE: Preserve~%* Existing~%before~%* Later~%after~%"))
           (document (parse-org source :source-name "notes.org"))
           (template-result
             (expand-capture-template "** Captured %?"
                                      :time-context
                                      (capture-target-test-time-context)))
           (result
             (capture-into-target
              document '(:file+headline "notes.org" "Existing")
              template-result
              :time-context (capture-target-test-time-context)))
           (updated (capture-target-result-target-document result)))
      (with-soft-assertions
        (expect (document-text document) :to-equal source)
        (expect (document-text updated)
                :to-equal
                (format nil
                        "#+TITLE: Preserve~%* Existing~%before~%** Captured ~%* Later~%after~%"))
        (expect (capture-target-result-cursor-offset result) :to-be 48))))
  (it "creates a missing file headline from supplied contents without writing a file"
    (let* ((file
             (sb-ext:native-namestring
              (merge-pathnames
               (make-pathname :name (format nil "capture-no-write-~A"
                                            (gensym "CASE"))
                              :type "org")
               (uiop:temporary-directory))))
           (file-existed-p (probe-file file))
           (contents (format nil "#+TITLE: Preserve~%* Existing~%old~%"))
           (template-result (capture-target-test-template "Entry"))
           (time-context (capture-target-test-time-context))
           (result
             (capture-into-target
              nil (list :file+headline file "Inbox") template-result
              :file-contents (list (cons file contents))
              :time-context time-context))
           (updated (capture-target-result-target-document result)))
      (with-soft-assertions
        (expect file-existed-p :to-be nil)
        (expect (document-text updated)
                :to-equal
                (format nil
                        "#+TITLE: Preserve~%* Existing~%old~%* Inbox~%** Entry~%"))
        (expect (capture-target-result-cursor-offset result) :to-be 44)
        (expect (probe-file file) :to-be nil))))
  (it "creates each missing outline path heading"
    (let* ((source (format nil "#+TITLE: Preserve~%"))
           (document (parse-org source :source-name "outline.org"))
           (result
             (capture-into-target
              document
              '(:file+olp "outline.org" ("Area" "Subarea"))
              (capture-target-test-template "Entry")
              :time-context (capture-target-test-time-context)))
           (updated (capture-target-result-target-document result)))
      (with-soft-assertions
        (expect (document-text document) :to-equal source)
        (expect (document-text updated)
                :to-equal
                (format nil
                        "#+TITLE: Preserve~%* Area~%** Subarea~%*** Entry~%"))
        (expect (capture-target-result-cursor-offset result) :to-be 40))))
  (it "creates the year, month and day datetree path from the time context"
    (let* ((source (format nil "#+TITLE: Keep~%"))
           (document (parse-org source :source-name "daily.org"))
           (result
             (capture-into-target
              document '(:file+olp+datetree "daily.org")
              (capture-target-test-template "Entry")
              :time-context (capture-target-test-time-context)))
           (updated (capture-target-result-target-document result)))
      (with-soft-assertions
        (expect (document-text document) :to-equal source)
        (expect (document-text updated)
                :to-equal
                (format nil
                        "#+TITLE: Keep~%* 2024~%** 2024-02 February~%*** 2024-02-19 Mon~%**** Entry~%"))
        (expect (capture-target-result-cursor-offset result) :to-be 65))))
  (it "signals ambiguity and missing-target reasons for ID targets"
    (let* ((first (capture-target-test-id-document "first.org" "First" "shared"))
           (second (capture-target-test-id-document "second.org" "Second" "shared"))
           (template-result (capture-target-test-template "Entry"))
           (time-context (capture-target-test-time-context)))
      (capture-target-test-expect-error
       (lambda ()
         (capture-into-target (list first second) '(:id "shared") template-result
                              :time-context time-context))
       :ambiguous-target)
      (capture-target-test-expect-error
       (lambda ()
         (capture-into-target (list first) '(:id "missing") template-result
                              :time-context time-context))
       :target-not-found)))
  (it "resolves a regexp that names a headline and preserves following text"
    (let* ((source
             (format nil
                     "* Before~%before body~%* Anchor~%anchor body~%* After~%after body~%"))
           (document (parse-org source :source-name "notes.org"))
           (result
             (capture-into-target
              document '(:file+regexp "notes.org" "^\\* Anchor$")
              (capture-target-test-template "** Captured")
              :time-context (capture-target-test-time-context)))
           (updated (capture-target-result-target-document result)))
      (with-soft-assertions
        (expect (document-text document) :to-equal source)
        (expect (document-text updated)
                :to-equal
                (format nil
                        "* Before~%before body~%* Anchor~%anchor body~%** Captured~%* After~%after body~%"))
        (expect (capture-target-result-cursor-offset result) :to-be 42))))
  (it "rejects cursor offsets below zero and beyond expanded text"
    (let* ((document (parse-org (format nil "* Target~%body~%")
                                :source-name "cursor.org"))
           (time-context (capture-target-test-time-context)))
      (capture-target-test-expect-error
       (lambda ()
         (capture-into-target
          document '(:file+headline "cursor.org" "Target")
          (capture-target-test-template "x" -1)
          :time-context time-context))
       :invalid-template-result)
      (capture-target-test-expect-error
       (lambda ()
         (capture-into-target
          document '(:file+headline "cursor.org" "Target")
          (capture-target-test-template "x" 2)
          :time-context time-context))
       :invalid-template-result)))
  (it "uses vector documents and an existing outline path"
    (let* ((source (format nil "#+TITLE: Keep~%* Area~%** Subarea~%body~%"))
           (document (parse-org source :source-name "outline.org"))
           (template (capture-target-test-template "Entry" 5))
           (result
             (capture-into-target
              (vector document)
              '(:file+olp "outline.org" ("Area" "Subarea"))
              template
              :time-context (capture-target-test-time-context)))
           (updated (capture-target-result-target-document result)))
      (with-soft-assertions
        (expect (document-text document) :to-equal source)
        (expect (document-text updated)
                :to-equal
                (format nil
                        "#+TITLE: Keep~%* Area~%** Subarea~%body~%*** Entry~%"))
        (expect (edit-result-p (capture-target-result-edit-result result))
                :to-be t)
        (expect (capture-target-result-cursor-offset result)
                :to-be (1- (length (document-text updated)))))))
  (it "rejects malformed target, document, ID, and template inputs"
    (let* ((document (parse-org "* Target" :source-name "notes.org"))
           (duplicate (parse-org "* Other" :source-name "notes.org"))
           (inline-document
             (parse-org
              (format nil
                      "*************** TODO Inline~%:PROPERTIES:~%:ID: inline-id~%:END:~%*************** END~%")))
           (template (capture-target-test-template "Entry"))
           (time-context (capture-target-test-time-context)))
      (labels ((rejects (reason documents target template-result &rest options)
                 (capture-target-test-expect-error
                  (lambda ()
                    (apply #'capture-into-target
                           documents target template-result options))
                  reason)))
        (with-soft-assertions
          (rejects :invalid-documents (cons document :tail)
                   '(:file+headline "notes.org" "Target")
                   template :time-context time-context)
          (rejects :invalid-documents (list "not a document")
                   '(:file+headline "notes.org" "Target")
                   template :time-context time-context)
          (rejects :invalid-target (list document) (cons :id :tail)
                   template :time-context time-context)
          (rejects :invalid-target (list document) '(:unsupported)
                   template :time-context time-context)
          (rejects :invalid-target (list document)
                   '(:file+headline "notes.org")
                   template :time-context time-context)
          (rejects :invalid-target (list document)
                   '(:file+headline 7 "Target")
                   template :time-context time-context)
          (rejects :target-not-found nil
                   '(:file+headline "missing.org" "Inbox")
                   template :time-context time-context)
          (rejects :invalid-target nil
                   '(:file+headline "missing.org" "Inbox")
                   template :file-contents '(("missing.org" . 7))
                   :time-context time-context)
          (rejects :ambiguous-target (list document duplicate)
                   '(:file+headline "notes.org" "Target")
                   template :time-context time-context)
          (rejects :invalid-target (list document)
                   '(:file+headline "notes.org" 7)
                   template :time-context time-context)
          (rejects :invalid-target (list document)
                   '(:file+olp "notes.org" nil)
                   template :time-context time-context)
          (rejects :invalid-target (list document)
                   '(:file+olp "notes.org" ("Area" 7))
                   template :time-context time-context)
          (rejects :invalid-target (list document)
                   '(:file+regexp "notes.org" 7)
                   template :time-context time-context)
          (rejects :invalid-target (list document)
                   '(:file+regexp "notes.org" "[")
                   template :time-context time-context)
          (rejects :target-not-found (list document)
                   '(:file+regexp "notes.org" "absent")
                   template :time-context time-context)
          (rejects :invalid-target (list document) '(:id "")
                   template :time-context time-context)
          (rejects :invalid-target (list document) '(:id 7)
                   template :time-context time-context)
          (rejects :invalid-target (list inline-document) '(:id "inline-id")
                   template :time-context time-context)
          (rejects :invalid-template-result (list document)
                   '(:file+headline "notes.org" "Target")
                   (make-capture-template-result :expanded-text 7
                                                 :cursor-offset 0)
                   :time-context time-context)
          (rejects :invalid-template-result (list document)
                   '(:file+headline "notes.org" "Target")
                   (make-capture-template-result :expanded-text "Entry"
                                                 :cursor-offset "end")
                   :time-context time-context)
          (capture-target-test-expect-error
           (lambda ()
             (org-kit::capture-target-resolve
              (list document) '(:file+olp+datetree "notes.org")
              nil (make-limits) nil))
           :invalid-time-context)
          (rejects :invalid-time-context (list document)
                   '(:file+headline "notes.org" "Target") template)
          (rejects :invalid-limits (list document)
                   '(:file+headline "notes.org" "Target")
                   template :time-context time-context :limits 7)))))
  (it "rejects a capture outline that would shift below level one"
    (let* ((document (parse-org "* Target" :source-name "notes.org"))
           (template
             (capture-target-test-template
              (format nil "*** Deep~%* Shallow~%")))
           (condition
             (capture-target-test-condition
              (lambda ()
                (capture-into-target
                 document '(:file+headline "notes.org" "Target")
                 template
                 :time-context (capture-target-test-time-context))))))
      (with-soft-assertions
        (expect (typep condition 'org-capture-error) :to-be t)
        (when (typep condition 'org-capture-error)
          (expect (org-capture-error-reason condition) :to-be :invalid-template)))))
  (it "inserts a space before LF after a regexp match ending in bare CR"
    (let* ((source (format nil "* Target~%body~C" #\Return))
           (document (parse-org source :source-name "notes.org"))
           (template (capture-target-test-template (format nil "Entry~%")))
           (result
             (capture-into-target
              document '(:file+regexp "notes.org" "body")
              template
              :time-context (capture-target-test-time-context)))
           (updated (capture-target-result-target-document result))
           (regex-limit
             (capture-target-test-condition
              (lambda ()
                (capture-into-target
                 document '(:file+regexp "notes.org" "body")
                 template
                 :time-context (capture-target-test-time-context)
                 :limits (make-limits :max-search-regex-length 1)))))
           (work-limit
             (capture-target-test-condition
              (lambda ()
                (capture-into-target
                 document '(:file+regexp "notes.org" "body")
                 template
                 :time-context (capture-target-test-time-context)
                 :limits (make-limits :max-search-work 1))))))
      (with-soft-assertions
        (expect (document-text document) :to-equal source)
        (expect (document-text updated)
                :to-equal
                (format nil "* Target~%body~C ~%* Entry~%" #\Return))
        (expect (typep regex-limit 'org-resource-limit-error) :to-be t)
        (when (typep regex-limit 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name regex-limit)
                  :to-be :max-search-regex-length))
        (expect (typep work-limit 'org-resource-limit-error) :to-be t)
        (when (typep work-limit 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name work-limit)
                  :to-be :max-search-work)))))
  (it "inserts below a unique ID target"
    (let* ((source
             (format nil "* Target~%:PROPERTIES:~%:ID: target-id~%:END:~%"))
           (document (parse-org source :source-name "id.org"))
           (result
             (capture-into-target
              (list document)
              '(:id "target-id")
              (capture-target-test-template "Entry")
              :time-context (capture-target-test-time-context)))
           (updated (capture-target-result-target-document result)))
      (with-soft-assertions
        (expect (document-text document) :to-equal source)
        (expect (document-text updated)
                :to-equal
                (format nil
                        "* Target~%:PROPERTIES:~%:ID: target-id~%:END:~%** Entry~%"))
        (expect (edit-result-p (capture-target-result-edit-result result))
                :to-be t)
        (expect (capture-target-result-cursor-offset result)
                :to-be (+ (length source) 3)))))
  (it "continues an existing outline path when its child is missing"
    (let* ((source (format nil "* Area~%body~%"))
           (document (parse-org source :source-name "outline.org"))
           (result
             (capture-into-target
              document
              '(:file+olp "outline.org" ("Area" "Subarea"))
              (capture-target-test-template "Entry")
              :time-context (capture-target-test-time-context)))
           (updated (capture-target-result-target-document result)))
      (with-soft-assertions
        (expect (document-text document) :to-equal source)
        (expect (document-text updated)
                :to-equal
                (format nil "* Area~%body~%** Subarea~%*** Entry~%"))
        (expect (edit-result-p (capture-target-result-edit-result result))
                :to-be t))))
  (it "rejects a non-template result"
    (let* ((document (parse-org "* Target" :source-name "notes.org"))
           (condition
             (capture-target-test-condition
              (lambda ()
                (capture-into-target
                 document
                 '(:file+headline "notes.org" "Target")
                 "Entry"
                 :time-context (capture-target-test-time-context))))))
      (with-soft-assertions
        (expect (typep condition 'org-capture-error) :to-be t)
        (when (typep condition 'org-capture-error)
          (expect (org-capture-error-reason condition)
                  :to-be :invalid-template-result))))))

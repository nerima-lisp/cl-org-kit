;;;; t/edit-patches-test.lisp
(in-package #:cl-org-kit/test)

(defun edit-test-patches-from-steps (steps source-length)
  "Build ordered, disjoint patches from generated step values."
  (let ((cursor 0)
        (patches '()))
    (loop for step in steps
          for start = (min source-length (+ cursor (mod step 3)))
          for end = (min source-length (+ start (mod step 4)))
          for replacement = (format nil "R~D" step)
          when (< start source-length)
            do (push (org-kit::edit-make-patch start end replacement) patches)
               (setf cursor (min source-length (1+ end)))
          finally (return (nreverse patches)))))

(defun edit-test-unpatched-ranges-match-p (source output patches)
  "Return true when every source gap is byte-for-byte represented in OUTPUT."
  (let ((source-cursor 0)
        (output-cursor 0))
    (dolist (patch patches)
      (let ((unchanged-length (- (org-kit::edit-patch-start patch)
                                 source-cursor)))
        (unless (string= source output
                         :start1 source-cursor
                         :end1 (org-kit::edit-patch-start patch)
                         :start2 output-cursor
                         :end2 (+ output-cursor unchanged-length))
          (return-from edit-test-unpatched-ranges-match-p nil))
        (incf output-cursor
              (+ unchanged-length
                 (length (org-kit::edit-patch-replacement patch))))
        (setf source-cursor (org-kit::edit-patch-end patch))))
    (string= source output
             :start1 source-cursor
             :end1 (length source)
             :start2 output-cursor
             :end2 (length output))))

(defun edit-test-headline (document)
  "Return the first headline in DOCUMENT."
  (find :headline (node-children (document-root document)) :key #'node-type))

(describe "edit patches"
  (it "splices sorted patches and leaves every uncovered source range unchanged"
    (let* ((source "abcdefghi")
           (patches (list (org-kit::edit-make-patch 1 3 "XY")
                          (org-kit::edit-make-patch 6 8 "Z")))
           (output (org-kit::edit-splice-source source patches)))
      (with-soft-assertions
        (expect output :to-equal "aXYdefZi")
        (expect (edit-test-unpatched-ranges-match-p source output patches)
                :to-be t))))

  (it "sorts non-overlapping patches before splicing"
    (let* ((source "0123456789")
           (patches (list (org-kit::edit-make-patch 7 9 "x")
                          (org-kit::edit-make-patch 1 2 "y"))))
      (expect (org-kit::edit-splice-source
               source (org-kit::edit-normalize-patches patches))
              :to-equal "0y23456x9")))

  (it "builds canonical trees without source-only ranges and supports expected property edits"
    (let* ((document (parse-org (format nil "* old~%")))
           (headline (edit-test-headline document))
           (path (org-kit::edit-node-path document headline))
           (canonical (org-kit::edit-canonical-document document))
           (expected (org-kit::edit-canonical-copy canonical)))
      (with-soft-assertions
        (expect path :to-equal '(0))
        (expect (org-kit::edit-source-property-p :reader-title-range) :to-be t)
        (expect (org-kit::edit-source-property-p :x) :to-be nil)
        (expect (org-kit::edit-source-property-p :raw-value) :to-be nil)
        (expect (org-kit::edit-source-property-p "reader-title-range")
                :to-be nil)
        (expect (getf (getf (org-kit::edit-canonical-node-at-path canonical path)
                            :properties)
                      :reader-title-range)
                :to-be nil)
        (org-kit::edit-canonical-set-property expected path :raw-value "new")
        (expect (getf (getf (org-kit::edit-canonical-node-at-path expected path)
                            :properties)
                      :raw-value)
                :to-equal "new")
        (org-kit::edit-canonical-remove-property expected path :raw-value)
        (expect (getf (getf (org-kit::edit-canonical-node-at-path expected path)
                            :properties)
                      :raw-value :missing)
                :to-be :missing)
        (org-kit::edit-canonical-set-children expected path '(:replacement))
        (expect (getf (org-kit::edit-canonical-node-at-path expected path)
                      :children)
                :to-equal '(:replacement))
        (expect (org-kit::edit-canonical-value (vector 1 "x")) :to-equalp #(1 "x")))))

  (it "compares canonical strings exactly inside nested values"
    (with-soft-assertions
      (expect (org-kit::edit-canonical-equal-p "Title" "Title") :to-be t)
      (expect (org-kit::edit-canonical-equal-p "Title" "title") :to-be nil)
      (expect (org-kit::edit-canonical-equal-p "Title" :title) :to-be nil)
      (expect (org-kit::edit-canonical-equal-p :title "Title") :to-be nil)
      (expect (org-kit::edit-canonical-equal-p '(:title "Title")
                                               '(:title "title"))
              :to-be nil)
      (expect (org-kit::edit-canonical-equal-p '(:title "Title")
                                               '(:title "Title"))
              :to-be t)
      (expect (org-kit::edit-canonical-equal-p '(:title "Title") "Title")
              :to-be nil)
      (expect (org-kit::edit-canonical-equal-p "Title" '(:title "Title"))
              :to-be nil)
      (expect (org-kit::edit-canonical-equal-p #("Title") #("title"))
              :to-be nil)
      (expect (org-kit::edit-canonical-equal-p #(1 2) #(1 2)) :to-be t)
      (expect (org-kit::edit-canonical-equal-p #(1) #(1 2)) :to-be nil)
      (expect (org-kit::edit-canonical-equal-p #(#\a) "a") :to-be nil)
      (expect (org-kit::edit-canonical-equal-p "a" #(#\a)) :to-be nil)
      (expect (org-kit::edit-canonical-equal-p :same :same) :to-be t)
      (expect (org-kit::edit-canonical-equal-p :same :different) :to-be nil)
      (expect (org-kit::edit-canonical-equal-p 1 '(:value)) :to-be nil)))

  (it "reports atom and dotted-list targets through the public edit API"
    (let* ((source (format nil "#+TODO: TODO | DONE~%* Heading~%"))
           (document (parse-org source))
           (headline (edit-test-headline document))
           (circular (list headline)))
      (setf (cdr circular) circular)
      (with-soft-assertions
        (handler-case
            (progn (set-heading-todo document nil "TODO")
                   (expect nil :to-be t))
          (org-edit-error (condition)
            (expect (org-edit-error-reason condition) :to-be :target-not-found)))
        (handler-case
            (progn (set-heading-todo document (list headline headline) "TODO")
                   (expect nil :to-be t))
          (org-edit-error (condition)
            (expect (org-edit-error-reason condition) :to-be :ambiguous-target)))
        (handler-case
            (progn (set-heading-todo document '(:not-a-node) "TODO")
                   (expect nil :to-be t))
          (org-edit-error (condition)
            (expect (org-edit-error-reason condition) :to-be :wrong-node-type)))
        (handler-case
            (progn (set-heading-todo document 17 "TODO")
                   (expect nil :to-be t))
          (org-edit-error (condition)
            (expect (org-edit-error-reason condition) :to-be :target-not-found)))
        (handler-case
            (progn (set-heading-todo document (cons headline :tail) "TODO")
                   (expect nil :to-be t))
          (org-edit-error (condition)
            (expect (org-edit-error-reason condition) :to-be :invalid-value)))
        (handler-case
            (progn (set-heading-todo document circular "TODO")
                   (expect nil :to-be t))
          (org-edit-error (condition)
            (expect (org-edit-error-reason condition) :to-be :invalid-value)))
        (expect (document-text document) :to-equal source))))

  (it "accepts a one-element list target through the public edit API"
    (let* ((source (format nil "#+TODO: TODO | DONE~%* Heading~%"))
           (document (parse-org source))
           (headline (edit-test-headline document))
           (edited (set-heading-todo document (list headline) "TODO")))
      (with-soft-assertions
        (expect (document-text edited) :to-equal
                (format nil "#+TODO: TODO | DONE~%* TODO Heading~%"))
        (expect (document-text document) :to-equal source))))

  (it "creates a new reparsed document and preserves the input on success"
    (let* ((document (parse-org (format nil "* old~%") :source-name "notes.org"))
           (headline (edit-test-headline document))
           (edited (replace-heading-title document headline "new")))
      (with-soft-assertions
        (expect (eq edited document) :to-be nil)
        (expect (document-text document) :to-equal (format nil "* old~%"))
        (expect (document-text edited) :to-equal (format nil "* new~%"))
        (expect (document-source-name edited) :to-equal "notes.org"))))

  (it "retains UTF-8 BOM and source name through reparsing"
    (let* ((document (parse-org (octets #xEF #xBB #xBF #x2A #x20 #x78 #x0A)
                                :encoding :utf-8 :source-name "notes.org"))
           (headline (edit-test-headline document))
           (edited (replace-heading-title document headline "y")))
      (with-soft-assertions
        (expect (document-encoding edited) :to-be :utf-8)
        (expect (document-bom-p edited) :to-be t)
        (expect (document-source-name edited) :to-equal "notes.org")
        (expect (write-org edited :output-type :octets)
                :to-equalp (octets #xEF #xBB #xBF #x2A #x20 #x79 #x0A)))))

  (it "signals a resource limit with the configured output limit and actual size"
    (let* ((limits (make-limits :max-output-length 4))
           (document (parse-org (format nil "* x~%") :limits limits))
           (headline (edit-test-headline document)))
      (handler-case
          (progn
            (replace-heading-title document headline "long")
            (expect nil :to-be t))
        (org-resource-limit-error (condition)
          (with-soft-assertions
            (expect (org-resource-limit-error-limit-name condition)
                    :to-be :max-output-length)
            (expect (org-resource-limit-error-limit condition) :to-be 4)
            (expect (org-resource-limit-error-actual condition) :to-be 7))))))

  (it "refuses edited text that cannot be encoded using the original encoding"
    (let* ((document (parse-org (octets #x2A #x20 #x78 #x0A) :encoding :latin-1))
           (headline (edit-test-headline document)))
      (signals org-kit-error
        (replace-heading-title document headline "日"))
      (expect (document-text document) :to-equal (format nil "* x~%"))))

  (it "bounds and escapes invalid target descriptions through the public edit API"
    (let* ((source (format nil "#+TODO: TODO | DONE~%* Heading~%"))
           (document (parse-org source))
           (target (format nil "~A~C~A"
                           (make-string 10 :initial-element #\x)
                           #\Newline
                           (make-string 100 :initial-element #\y))))
      (handler-case
          (progn
            (set-heading-todo document target "TODO")
            (expect nil :to-be t))
        (org-edit-error (condition)
          (let ((description (org-edit-error-target condition)))
            (with-soft-assertions
              (expect (org-edit-error-reason condition) :to-be :target-not-found)
              (expect (<= (length description) 80) :to-be t)
              (expect (search "\\n" description) :to-be-truthy)))))))

  (it-property "random disjoint edits preserve every uncovered source segment"
      ((steps (gen-list (gen-integer :min 0 :max 10) :max-length 12)))
    (let* ((source "0123456789abcdefghijklmn")
           (patches (edit-test-patches-from-steps steps (length source)))
           (output (org-kit::edit-splice-source source patches)))
      (expect (edit-test-unpatched-ranges-match-p source output patches)
              :to-be t))))

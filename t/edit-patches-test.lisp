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
               source (org-kit::edit-normalize-patches patches (length source) nil))
              :to-equal "0y23456x9")))

  (it "rejects overlapping, same-position, malformed, and out-of-range patches"
    (with-soft-assertions
      (signals org-edit-error
        (org-kit::edit-normalize-patches
         (list (org-kit::edit-make-patch 1 4 "a")
               (org-kit::edit-make-patch 3 5 "b")) 8 "target"))
      (signals org-edit-error
        (org-kit::edit-normalize-patches
         (list (org-kit::edit-make-patch 2 2 "a")
               (org-kit::edit-make-patch 2 2 "b")) 8 "target"))
      (signals org-edit-error
        (org-kit::edit-normalize-patches (list :not-a-patch) 8 "target"))
      (signals org-edit-error
        (org-kit::edit-normalize-patches
         (list (org-kit::edit-make-patch 7 9 "x")) 8 "target"))
      (signals org-edit-error
        (org-kit::edit-normalize-patches
         (cons (org-kit::edit-make-patch 0 0 "x") :improper) 8 "target"))))

  (it "rejects circular patch lists without traversing them indefinitely"
    (let ((patches (list (org-kit::edit-make-patch 0 0 "x"))))
      (setf (cdr patches) patches)
      (signals org-edit-error
        (org-kit::edit-normalize-patches patches 8 "target"))))

  (it "builds canonical trees without source-only ranges and supports expected property edits"
    (let* ((document (parse-org (format nil "* old~%")))
           (headline (edit-test-headline document))
           (path (org-kit::edit-node-path document headline))
           (canonical (org-kit::edit-canonical-document document))
           (expected (org-kit::edit-canonical-copy canonical)))
      (with-soft-assertions
        (expect path :to-equal '(0))
        (expect (org-kit::edit-source-property-p :reader-title-range) :to-be t)
        (expect (org-kit::edit-source-property-p :raw-value) :to-be nil)
        (expect (org-kit::edit-source-property-p "reader-title-range")
                :to-be nil)
        (expect (getf (getf (org-kit::edit-canonical-node-at-path canonical path)
                            :properties)
                      :reader-title-range)
                :to-be nil)
        (signals org-edit-error
          (org-kit::edit-canonical-node-at-path canonical '(99)))
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

  (it "resolves a single owned node and reports empty, ambiguous, foreign, and wrong-type targets"
    (let* ((document (parse-org (format nil "* one~%* two~%")))
           (headline (edit-test-headline document))
           (other-document (parse-org (format nil "* other~%")))
           (foreign (edit-test-headline other-document)))
      (with-soft-assertions
        (expect (org-kit::edit-resolve-node document headline '(:headline))
                :to-be headline)
        (expect (org-kit::edit-resolve-node document (list headline) '(:headline))
                :to-be headline)
        (signals org-edit-error
          (org-kit::edit-resolve-node document nil))
        (signals org-edit-error
          (org-kit::edit-resolve-node document (list headline headline)))
        (signals org-edit-error
          (org-kit::edit-resolve-node document foreign))
        (signals org-edit-error
          (org-kit::edit-resolve-node document headline '(:item)))
        (signals org-edit-error
          (org-kit::edit-resolve-node document (list :not-a-node))))))

  (it "creates a new reparsed document and preserves the input on success"
    (let* ((document (parse-org (format nil "* old~%") :source-name "notes.org"))
           (headline (edit-test-headline document))
           (path (org-kit::edit-node-path document headline))
           (expected (org-kit::edit-canonical-copy
                      (org-kit::edit-canonical-document document))))
      (org-kit::edit-canonical-set-property expected path :raw-value "new")
      (org-kit::edit-canonical-set-property expected path :title '("new"))
      (let ((edited (org-kit::edit-apply-patches
                     document (list (org-kit::edit-make-patch 2 5 "new" headline))
                     :target headline :expected expected)))
        (with-soft-assertions
          (expect (eq edited document) :to-be nil)
          (expect (document-text document) :to-equal (format nil "* old~%"))
          (expect (document-text edited) :to-equal (format nil "* new~%"))
          (expect (document-source-name edited) :to-equal "notes.org")
          (expect (org-kit::edit-canonical-equal-p
                   expected (org-kit::edit-canonical-document edited))
                  :to-be t)))))

  (it "rejects a result whose canonical tree differs from the expected edit"
    (let* ((document (parse-org (format nil "* old~%")))
           (headline (edit-test-headline document))
           (expected (org-kit::edit-canonical-document document)))
      (with-soft-assertions
        (signals org-edit-error
          (org-kit::edit-apply-patches
           document (list (org-kit::edit-make-patch 2 5 "new" headline))
           :target headline :expected expected))
        (expect (document-text document) :to-equal (format nil "* old~%")))))

  (it "rejects case-only changes to the parsed tree"
    (let* ((document (parse-org (format nil "* title~%")))
           (headline (edit-test-headline document))
           (expected (org-kit::edit-canonical-document document)))
      (with-soft-assertions
        (signals org-edit-error
          (org-kit::edit-apply-patches
           document (list (org-kit::edit-make-patch 2 7 "TITLE" headline))
           :target headline :expected expected))
        (expect (document-text document) :to-equal (format nil "* title~%")))))

  (it "retains UTF-8 BOM and source name through reparsing"
    (let* ((document (parse-org (octets #xEF #xBB #xBF #x2A #x20 #x78 #x0A)
                                :encoding :utf-8 :source-name "notes.org"))
           (headline (edit-test-headline document))
           (path (org-kit::edit-node-path document headline))
           (expected (org-kit::edit-canonical-copy
                      (org-kit::edit-canonical-document document))))
      (org-kit::edit-canonical-set-property expected path :raw-value "y")
      (org-kit::edit-canonical-set-property expected path :title '("y"))
      (let ((edited (org-kit::edit-apply-patches
                     document (list (org-kit::edit-make-patch 2 3 "y" headline))
                     :target headline :expected expected)))
        (with-soft-assertions
          (expect (document-encoding edited) :to-be :utf-8)
          (expect (document-bom-p edited) :to-be t)
          (expect (document-source-name edited) :to-equal "notes.org")
          (expect (write-org edited :output-type :octets)
                  :to-equalp (octets #xEF #xBB #xBF #x2A #x20 #x79 #x0A)))))

  (it "signals a resource limit with the configured output limit and actual size"
    (let* ((limits (make-limits :max-output-length 4))
           (document (parse-org (format nil "* x~%") :limits limits))
           (headline (edit-test-headline document))
           (expected (org-kit::edit-canonical-copy
                      (org-kit::edit-canonical-document document))))
      (org-kit::edit-canonical-set-property
       expected (org-kit::edit-node-path document headline) :raw-value "long")
      (handler-case
          (progn
            (org-kit::edit-apply-patches
             document (list (org-kit::edit-make-patch 2 3 "long" headline))
             :target headline :expected expected)
            (expect nil :to-be t))
        (org-resource-limit-error (condition)
          (with-soft-assertions
            (expect (org-resource-limit-error-limit-name condition)
                    :to-be :max-output-length)
            (expect (org-resource-limit-error-limit condition) :to-be 4)
            (expect (org-resource-limit-error-actual condition) :to-be 7))))))

  (it "refuses edited text that cannot be encoded using the original encoding"
    (let* ((document (parse-org (octets #x2A #x20 #x78 #x0A) :encoding :latin-1))
           (headline (edit-test-headline document))
           (expected (org-kit::edit-canonical-copy
                      (org-kit::edit-canonical-document document))))
      (org-kit::edit-canonical-set-property
       expected (org-kit::edit-node-path document headline) :raw-value "日")
      (signals org-kit-error
        (org-kit::edit-apply-patches
         document (list (org-kit::edit-make-patch 2 3 "日" headline))
         :target headline :expected expected))
      (expect (document-text document) :to-equal (format nil "* x~%"))))

  (it "bounds and escapes target descriptions"
    (let ((description (org-kit::edit-target-description
                        (format nil "~A~Ctail" (make-string 100 :initial-element #\x)
                                #\Newline))))
      (with-soft-assertions
        (expect (<= (length description) 83) :to-be t)
        (expect (search "\\n" description) :to-be-truthy)))))

  (it-property "random disjoint edits preserve every uncovered source segment"
      ((steps (gen-list (gen-integer :min 0 :max 10) :max-length 12)))
    (let* ((source "0123456789abcdefghijklmn")
           (patches (edit-test-patches-from-steps steps (length source)))
           (output (org-kit::edit-splice-source source patches)))
      (expect (edit-test-unpatched-ranges-match-p source output patches)
              :to-be t))))

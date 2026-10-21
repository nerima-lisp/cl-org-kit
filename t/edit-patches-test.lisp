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
  "Return true when every source gap is character-for-character represented in OUTPUT."
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

(defun edit-test-span-ranges (spans)
  "Return the source coordinate pairs in SPANS."
  (mapcar (lambda (span)
            (list (edit-span-start span) (edit-span-end span)))
          spans))

(defun edit-test-expect-error-slots (condition target reason message)
  "Assert the public class, target, reason, and message of an edit failure."
  (with-soft-assertions
    (expect (typep condition 'org-edit-error) :to-be t)
    (expect (org-edit-error-target condition) :to-equal target)
    (expect (org-edit-error-reason condition) :to-be reason)
    (expect (org-kit-error-message condition) :to-equal message)))

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

  (it "constructs edit spans and results with their public readers"
    (let* ((document (parse-org "text"))
           (span (make-edit-span 1 3))
           (result (make-edit-result :updated-document document
                                     :spans (list span))))
      (with-soft-assertions
        (expect (edit-span-p span) :to-be t)
        (expect (edit-span-start span) :to-be 1)
        (expect (edit-span-end span) :to-be 3)
        (expect (edit-result-p result) :to-be t)
        (expect (edit-result-updated-document result) :to-be document)
        (expect (edit-result-spans result) :to-equal (list span)))))

  (it "keeps the low-level patch API document-returning"
    (let* ((source (format nil "* old~%"))
           (output (format nil "* new~%"))
           (document (parse-org source))
           (expected (org-kit::edit-canonical-document (parse-org output)))
           (result
             (org-kit::edit-apply-patches
              document (list (org-kit::edit-make-patch 2 5 "new"))
              :expected expected)))
      (with-soft-assertions
        (expect (document-p result) :to-be t)
        (expect (edit-result-p result) :to-be nil)
        (expect (document-text result) :to-equal output)
        (expect (document-text document) :to-equal source))))

  (it "returns an empty edit result for a single-document empty patch set"
    (let* ((source (format nil "* unchanged~%"))
           (document (parse-org source))
           (expected (org-kit::edit-canonical-document document))
           (result (org-kit::edit-apply-patches-result
                    document nil :expected expected)))
      (with-soft-assertions
        (expect (document-p (edit-result-updated-document result)) :to-be t)
        (expect (eq (edit-result-updated-document result) document) :to-be nil)
        (expect (edit-result-spans result) :to-equal nil)
        (expect (document-text (edit-result-updated-document result))
                :to-equal source)
        (expect (document-text document) :to-equal source))))

  (it "applies empty-range insertions and empty-text deletions"
    (let* ((source (format nil "* old~%"))
           (document (parse-org source))
           (inserted-text (format nil "* TODO old~%"))
           (inserted-expected (org-kit::edit-canonical-document
                               (parse-org inserted-text)))
           (inserted
             (org-kit::edit-apply-patches-result
              document (list (org-kit::edit-make-patch 2 2 "TODO "))
              :expected inserted-expected))
           (deleted-expected (org-kit::edit-canonical-document
                              (parse-org source)))
           (deleted
             (org-kit::edit-apply-patches-result
              (edit-result-updated-document inserted)
              (list (org-kit::edit-make-patch 2 7 ""))
              :expected deleted-expected)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document inserted))
                :to-equal inserted-text)
        (expect (edit-test-span-ranges (edit-result-spans inserted))
                :to-equal '((2 2)))
        (expect (document-text (edit-result-updated-document deleted))
                :to-equal source)
        (expect (edit-test-span-ranges (edit-result-spans deleted))
                :to-equal '((2 7)))
        (expect (document-text document) :to-equal source))))

  (it "returns sorted original-coordinate spans for multiple patches"
    (let* ((source (format nil "* old~%plain tail~%"))
           (document (parse-org source))
           (plain-start (search "plain" source))
           (patches (list (org-kit::edit-make-patch
                           plain-start (+ plain-start 5) "rough")
                          (org-kit::edit-make-patch 2 5 "new")))
           (expected-text (format nil "* new~%rough tail~%"))
           (expected (org-kit::edit-canonical-document
                      (parse-org expected-text)))
           (result (org-kit::edit-apply-patches-result
                    document patches :expected expected))
           (normalized (org-kit::edit-normalize-patches patches)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result))
                :to-equal expected-text)
        (expect (edit-test-span-ranges (edit-result-spans result))
                :to-equal (list (list 2 5)
                                (list plain-start (+ plain-start 5))))
        (expect (edit-test-unpatched-ranges-match-p
                 source expected-text normalized)
                :to-be t)
        (expect (document-text document) :to-equal source))))

  (it "orders a same-offset insertion before a non-empty replacement"
    (let* ((source (format nil "* old~%"))
           (document (parse-org source))
           (expected-text (format nil "* TODO new~%"))
           (expected (org-kit::edit-canonical-document
                      (parse-org expected-text)))
           (patches (list (org-kit::edit-make-patch 2 5 "new")
                          (org-kit::edit-make-patch 2 2 "TODO ")))
           (result (org-kit::edit-apply-patches-result
                    document patches :expected expected))
           (normalized (org-kit::edit-normalize-patches patches)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result))
                :to-equal expected-text)
        (expect (edit-test-span-ranges (edit-result-spans result))
                :to-equal '((2 2) (2 5)))
        (expect (edit-test-unpatched-ranges-match-p
                 source expected-text normalized)
                :to-be t)
        (expect (document-text document) :to-equal source))))

  (it "rejects two insertions at the same source offset"
    (let* ((source (format nil "* old~%"))
           (document (parse-org source))
           (patches (list (org-kit::edit-make-patch 2 2 "TODO ")
                          (org-kit::edit-make-patch 2 2 "WAITING "))))
      (handler-case
          (progn
            (org-kit::edit-apply-patches-result
             document patches :target "duplicate insert")
            (expect nil :to-be t))
        (org-edit-error (condition)
          (edit-test-expect-error-slots
           condition "duplicate insert" :overlapping-patches
           "Edit patches overlap")))
      (expect (document-text document) :to-equal source)))

  (it "checks the prospective output limit before source splicing"
    (let* ((limits (make-limits :max-output-length 6))
           (source (format nil "* x~%"))
           (document (parse-org source :limits limits))
           (patches (list (org-kit::edit-make-patch 2 3 "long")))
           (original-splicer (symbol-function 'org-kit::edit-splice-source))
           (splice-called nil))
      (unwind-protect
           (progn
             (setf (symbol-function 'org-kit::edit-splice-source)
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (setf splice-called t)
                     (error "Source splice ran before the limit check")))
             (handler-case
                 (progn
                   (org-kit::edit-apply-patches-result
                    document patches
                    :expected (org-kit::edit-canonical-document document))
                   (expect nil :to-be t))
               (org-resource-limit-error (condition)
                 (with-soft-assertions
                   (expect (org-kit-error-message condition)
                           :to-equal "Resource limit exceeded")
                   (expect (org-resource-limit-error-limit-name condition)
                           :to-be :max-output-length)
                   (expect (org-resource-limit-error-limit condition) :to-be 6)
                   (expect (org-resource-limit-error-actual condition)
                           :to-be 7)))))
        (setf (symbol-function 'org-kit::edit-splice-source) original-splicer))
      (with-soft-assertions
        (expect splice-called :to-be nil)
        (expect (document-text document) :to-equal source))))

  (it "accepts output just below and exactly at the configured character limit"
    (let* ((source (format nil "* x~%"))
           (output (format nil "* x!~%"))
           (output-length (length output))
           (patches (list (org-kit::edit-make-patch 2 3 "x!"))))
      (dolist (case (list (list (1+ output-length) :below-limit)
                          (list output-length :at-limit)))
        (destructuring-bind (limit boundary) case
          (let* ((limits (make-limits :max-output-length limit))
                 (document (parse-org source :limits limits))
                 (expected
                   (org-kit::edit-canonical-document (parse-org output)))
                 (result
                   (org-kit::edit-apply-patches-result
                    document patches :expected expected)))
            (with-soft-assertions
              (expect (if (eq boundary :below-limit)
                          (< output-length limit)
                          (= output-length limit))
                      :to-be t)
              (expect (document-text
                       (org-kit:edit-result-updated-document result))
                      :to-equal output)))))))

  (it "includes a retained BOM in the output character limit"
    (let* ((source (format nil "* x~%"))
           (input (concatenate 'string (string (code-char #xFEFF)) source))
           (limits (make-limits :max-output-length (length source)))
           (document (parse-org input :limits limits))
           (output (format nil "* x!~%"))
           (expected (org-kit::edit-canonical-document (parse-org output))))
      (handler-case
          (progn
            (org-kit::edit-apply-patches-result
             document (list (org-kit::edit-make-patch 2 3 "x!"))
             :expected expected)
            (expect nil :to-be t))
        (org-resource-limit-error (condition)
          (with-soft-assertions
            (expect (org-kit-error-message condition)
                    :to-equal "Resource limit exceeded")
            (expect (org-resource-limit-error-limit-name condition)
                    :to-be :max-output-length)
            (expect (org-resource-limit-error-limit condition)
                    :to-be (length source))
            (expect (org-resource-limit-error-actual condition)
                    :to-be (1+ (length output))))))
      (expect (document-bom-p document) :to-be t)
      (expect (document-text document) :to-equal source)))

  (it "rejects overlapping patches without changing the input document"
    (let* ((source (format nil "* old~%"))
           (document (parse-org source))
           (patches (list (org-kit::edit-make-patch 2 5 "new")
                          (org-kit::edit-make-patch 4 6 "x"))))
      (handler-case
          (progn
            (org-kit::edit-apply-patches-result
             document patches :target "overlap")
            (expect nil :to-be t))
        (org-edit-error (condition)
          (edit-test-expect-error-slots
           condition "overlap" :overlapping-patches "Edit patches overlap")))
      (expect (document-text document) :to-equal source)))

  (it "rejects malformed patch lists and ranges as invalid values"
    (let* ((document (parse-org "text"))
           (source (document-text document))
           (valid (org-kit::edit-make-patch 0 1 "T"))
           (circular (list valid)))
      (setf (cdr circular) circular)
      (dolist (case (list (list (cons valid :tail)
                                "Edit patches must be a proper list")
                          (list circular "Edit patches must be a proper list")
                          (list (list :not-a-patch)
                                "Edit patch has an invalid range or replacement")
                          (list (list (org-kit::%make-edit-patch :bad 1 "x"))
                                "Edit patch has an invalid range or replacement")
                          (list (list (org-kit::%make-edit-patch 0 :bad "x"))
                                "Edit patch has an invalid range or replacement")
                          (list (list (org-kit::edit-make-patch -1 0 "x"))
                                "Edit patch has an invalid range or replacement")
                          (list (list (org-kit::edit-make-patch 2 1 "x"))
                                "Edit patch has an invalid range or replacement")
                          (list (list (org-kit::edit-make-patch 0 5 "x"))
                                "Edit patch has an invalid range or replacement")
                          (list (list (org-kit::%make-edit-patch 0 1 nil))
                                "Edit patch has an invalid range or replacement")))
        (destructuring-bind (patches message) case
          (handler-case
              (progn
                (org-kit::edit-apply-patches-result
                 document patches :target "invalid patch"
                 :expected (org-kit::edit-canonical-document document))
                (expect nil :to-be t))
            (org-edit-error (condition)
              (edit-test-expect-error-slots
               condition "invalid patch" :invalid-value message)))))
      (expect (document-text document) :to-equal source)))

  (it "maps reparse failures and preserves resource errors without mutation"
    (let* ((source (format nil "* old~%"))
           (document (parse-org source))
           (expected (org-kit::edit-canonical-document document))
           (original-parser (symbol-function 'org-kit:parse-org)))
      (unwind-protect
           (progn
             (setf (symbol-function 'org-kit:parse-org)
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (error 'org-parse-error :message "forced parse failure")))
             (handler-case
                 (progn
                   (org-kit::edit-apply-patches-result
                    document (list (org-kit::edit-make-patch 2 5 "new"))
                    :target "parse target" :expected expected)
                   (expect nil :to-be t))
               (org-edit-error (condition)
                 (edit-test-expect-error-slots
                  condition "parse target" :validation-parse-failed
                  "Edited document failed validation parsing"))))
        (setf (symbol-function 'org-kit:parse-org) original-parser))
      (expect (document-text document) :to-equal source))
    (let* ((limits (make-limits :max-heading-depth 1))
           (source (format nil "* old~%"))
           (document (parse-org source :limits limits))
           (insert-offset (length source)))
      (handler-case
          (progn
            (org-kit::edit-apply-patches-result
             document
             (list (org-kit::edit-make-patch insert-offset insert-offset
                                             (format nil "** child~%")))
             :expected (org-kit::edit-canonical-document document))
            (expect nil :to-be t))
        (org-resource-limit-error (condition)
          (with-soft-assertions
            (expect (org-kit-error-message condition)
                    :to-equal "Resource limit exceeded")
            (expect (org-resource-limit-error-limit-name condition)
                    :to-be :max-heading-depth)
            (expect (org-resource-limit-error-limit condition) :to-be 1)
            (expect (org-resource-limit-error-actual condition) :to-be 2))))
      (expect (document-text document) :to-equal source)))

  (it "keeps canonical mismatches and multi-document transactions all-or-nothing"
    (let* ((source-one (format nil "* one~%"))
           (source-two (format nil "* two~%"))
           (document-one (parse-org source-one))
           (document-two (parse-org source-two))
           (patch-one (list (org-kit::edit-make-patch 2 5 "ONE")))
           (patch-two (list (org-kit::edit-make-patch 2 5 "TWO")))
           (expected-one (org-kit::edit-canonical-document
                          (parse-org (format nil "* ONE~%"))))
           (expected-two (org-kit::edit-canonical-document
                          (parse-org (format nil "* TWO~%"))))
           (successful
             (org-kit::edit-apply-patches-transaction
              (list (list document-one patch-one :expected expected-one)
                    (list document-two patch-two :expected expected-two)))))
      (with-soft-assertions
        (expect (length successful) :to-be 2)
        (expect (document-text
                 (edit-result-updated-document (first successful)))
                :to-equal (format nil "* ONE~%"))
        (expect (document-text
                 (edit-result-updated-document (second successful)))
                :to-equal (format nil "* TWO~%"))
        (expect (document-text document-one) :to-equal source-one)
        (expect (document-text document-two) :to-equal source-two)))
    (let* ((source-one (format nil "* one~%"))
           (source-two (format nil "* two~%"))
           (document-one (parse-org source-one))
           (document-two (parse-org source-two))
           (patch-one (list (org-kit::edit-make-patch 2 5 "ONE")))
           (patch-two (list (org-kit::edit-make-patch 2 5 "TWO")))
           (expected-one (org-kit::edit-canonical-document
                          (parse-org (format nil "* ONE~%"))))
           (not-two (org-kit::edit-canonical-document document-two))
           (transaction-result :no-result))
      (handler-case
          (progn
            (setf transaction-result
                  (org-kit::edit-apply-patches-transaction
                   (list (list document-one patch-one :expected expected-one)
                         (list document-two patch-two :target "second"
                               :expected not-two))))
            (expect nil :to-be t))
        (org-edit-error (condition)
          (edit-test-expect-error-slots
           condition "second" :unexpected-diff
           "Edited document contains an unexpected semantic change")))
      (with-soft-assertions
        (expect transaction-result :to-be :no-result)
        (expect (document-text document-one) :to-equal source-one)
        (expect (document-text document-two) :to-equal source-two))))

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

  (it "retains a character-input BOM and untouched bytes through a public edit"
    (let* ((source (format nil "* old~%"))
           (input (concatenate 'string (string (code-char #xFEFF)) source))
           (document (parse-org input))
           (original-octets (write-org document :output-type :octets))
           (headline (edit-test-headline document))
           (edited (replace-heading-title document headline "new"))
           (output-octets (write-org edited :output-type :octets))
           (patches (list (org-kit::edit-make-patch 2 5 "new"))))
      (with-soft-assertions
        (expect (document-encoding document) :to-be nil)
        (expect (document-bom-p document) :to-be t)
        (expect (document-text document) :to-equal source)
        (expect (document-text edited) :to-equal (format nil "* new~%"))
        (expect output-octets
                :to-equalp (octets #xEF #xBB #xBF #x2A #x20 #x6E #x65 #x77 #x0A))
        (expect (subseq output-octets 0 5)
                :to-equalp (subseq original-octets 0 5))
        (expect (subseq output-octets 8)
                :to-equalp (subseq original-octets 8))
        (expect (edit-test-unpatched-ranges-match-p
                 source (document-text edited) patches)
                :to-be t)
        (expect (write-org document :output-type :octets)
                :to-equalp original-octets))))

  (it "keeps generic UTF-16 BOM output singular through a public edit"
    (let* ((input-octets (octets #xFE #xFF #x00 #x2A #x00 #x20
                                 #x00 #x6F #x00 #x6C #x00 #x64 #x00 #x0A))
           (source (format nil "* old~%"))
           (document (parse-org input-octets :encoding :utf-16))
           (original-octets (write-org document :output-type :octets))
           (headline (edit-test-headline document))
           (edited (replace-heading-title document headline "new"))
           (output-octets (write-org edited :output-type :octets))
           (patches (list (org-kit::edit-make-patch 2 5 "new"))))
      (with-soft-assertions
        (expect (document-encoding document) :to-be :utf-16)
        (expect (document-bom-p document) :to-be t)
        (expect (document-text document) :to-equal source)
        (expect original-octets :to-equalp input-octets)
        (expect (document-text edited) :to-equal (format nil "* new~%"))
        (expect original-octets
                :to-equalp (octets #xFE #xFF #x00 #x2A #x00 #x20
                                   #x00 #x6F #x00 #x6C #x00 #x64 #x00 #x0A))
        (expect output-octets
                :to-equalp (octets #xFE #xFF #x00 #x2A #x00 #x20
                                   #x00 #x6E #x00 #x65 #x00 #x77 #x00 #x0A))
        (expect (subseq output-octets 0 6)
                :to-equalp (subseq original-octets 0 6))
        (expect (subseq output-octets 12)
                :to-equalp (subseq original-octets 12))
        (expect (edit-test-unpatched-ranges-match-p
                 source (document-text edited) patches)
                :to-be t)
        (expect (write-org document :output-type :octets)
                :to-equalp original-octets))))

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
            (expect (org-kit-error-message condition)
                    :to-equal "Resource limit exceeded")
            (expect (org-resource-limit-error-limit-name condition)
                    :to-be :max-output-length)
            (expect (org-resource-limit-error-limit condition) :to-be 4)
            (expect (org-resource-limit-error-actual condition) :to-be 7))))))

  (it "checks prospective output octets at the exact encoded and BOM boundaries"
    (let* ((source (format nil "* x~%"))
           (output (format nil "* あ~%"))
           (patches (list (org-kit::edit-make-patch 2 3 "あ")))
           (expected (org-kit::edit-canonical-document (parse-org output))))
      (dolist (limit '(5 6 7))
        (let ((document
                (parse-org source
                           :limits (make-limits :max-output-octets limit))))
          (if (< limit 6)
              (handler-case
                  (progn
                    (org-kit::edit-apply-patches-result
                     document patches :expected expected)
                    (expect nil :to-be t))
                (org-resource-limit-error (condition)
                  (with-soft-assertions
                    (expect (org-kit-error-message condition)
                            :to-equal "Resource limit exceeded")
                    (expect (org-resource-limit-error-limit-name condition)
                            :to-be :max-output-octets)
                    (expect (org-resource-limit-error-limit condition) :to-be 5)
                    (expect (org-resource-limit-error-actual condition)
                            :to-be 6))))
              (let ((result (org-kit::edit-apply-patches-result
                             document patches :expected expected)))
                (with-soft-assertions
                  (expect (document-text
                           (edit-result-updated-document result))
                          :to-equal output)
                  (expect (write-org (edit-result-updated-document result)
                                     :output-type :octets)
                          :to-equalp (octets #x2A #x20 #xE3 #x81 #x82 #x0A))
                  (expect (document-text document) :to-equal source))))
            (expect (document-text document) :to-equal source)))
    (let* ((source (format nil "* x~%"))
           (input (concatenate 'string (string (code-char #xFEFF)) source))
           (document
             (parse-org input :encoding :utf-8
                        :limits (make-limits :max-output-octets 6)))
           (patches (list (org-kit::edit-make-patch 2 3 "y")))
           (expected (org-kit::edit-canonical-document
                      (parse-org (format nil "* y~%")))))
      (handler-case
          (progn
            (org-kit::edit-apply-patches-result document patches :expected expected)
            (expect nil :to-be t))
        (org-resource-limit-error (condition)
          (with-soft-assertions
            (expect (org-resource-limit-error-limit-name condition)
                    :to-be :max-output-octets)
            (expect (org-resource-limit-error-limit condition) :to-be 6)
            (expect (org-resource-limit-error-actual condition) :to-be 7))))
      (expect (document-text document) :to-equal source)
      (expect (document-bom-p document) :to-be t))))

  (it "counts a generated generic UTF-16 preamble in output limits"
    (let* ((source (format nil "* x~%"))
           (input-octets (octets #x00 #x2A #x00 #x20 #x00 #x78 #x00 #x0A))
           (patches (list (org-kit::edit-make-patch 2 3 "y")))
           (expected (org-kit::edit-canonical-document
                      (parse-org (format nil "* y~%")))))
      (let ((document
              (parse-org input-octets :encoding :utf-16
                         :limits (make-limits :max-output-octets 9
                                              :max-output-length 5))))
        (handler-case
            (progn
              (org-kit::edit-apply-patches-result document patches
                                                  :expected expected)
              (expect nil :to-be t))
          (org-resource-limit-error (condition)
            (with-soft-assertions
              (expect (org-resource-limit-error-limit-name condition)
                      :to-be :max-output-octets)
              (expect (org-resource-limit-error-limit condition) :to-be 9)
              (expect (org-resource-limit-error-actual condition) :to-be 10))))
        (expect (document-text document) :to-equal source)
        (expect (document-bom-p document) :to-be nil))
      (let ((document
              (parse-org input-octets :encoding :utf-16
                         :limits (make-limits :max-output-octets 10
                                              :max-output-length 5))))
        (let* ((result (org-kit::edit-apply-patches-result
                        document patches :expected expected))
               (updated (edit-result-updated-document result)))
          (with-soft-assertions
            (expect (document-text updated) :to-equal (format nil "* y~%"))
            (expect (document-bom-p updated) :to-be t)
            (expect (write-org updated :output-type :octets)
                    :to-equalp (octets #xFE #xFF #x00 #x2A #x00 #x20
                                       #x00 #x79 #x00 #x0A))
            (expect (document-text document) :to-equal source)
            (expect (document-bom-p document) :to-be nil))))
      (let ((document
              (parse-org input-octets :encoding :utf-16
                         :limits (make-limits :max-output-octets 10
                                              :max-output-length 4))))
        (handler-case
            (progn
              (org-kit::edit-apply-patches-result document patches
                                                  :expected expected)
              (expect nil :to-be t))
          (org-resource-limit-error (condition)
            (with-soft-assertions
              (expect (org-resource-limit-error-limit-name condition)
                      :to-be :max-output-length)
              (expect (org-resource-limit-error-limit condition) :to-be 4)
              (expect (org-resource-limit-error-actual condition) :to-be 5))))
        (expect (document-text document) :to-equal source)
        (expect (document-bom-p document) :to-be nil))))

  (it "matches generic UTF-16 and UTF-32 edit boundaries to encoded output"
    (loop for encoding in '(:utf-16 :utf-32)
          for source = (format nil "* x~%")
          for input-octets = (cl-codec-kit:string-to-octets
                              source :encoding encoding)
          for output = (format nil "* y~%")
          for patches = (list (org-kit::edit-make-patch 2 3 "y"))
          for expected = (org-kit::edit-canonical-document (parse-org output))
          for encoded-output = (cl-codec-kit:string-to-octets
                                output :encoding encoding)
          for output-octets = (length encoded-output)
          do (progn
               (let* ((document
                        (parse-org input-octets :encoding encoding
                                   :limits (make-limits
                                            :max-output-octets output-octets)))
                       (prospective-octets
                         (org-kit::edit-prospective-output-octets
                          document source patches))
                       (result (org-kit::edit-apply-patches-result
                                document patches :expected expected))
                       (updated (edit-result-updated-document result))
                       (actual-output (write-org updated :output-type :octets)))
                 (with-soft-assertions
                   (expect (document-encoding document) :to-be encoding)
                   (expect (document-bom-p document) :to-be t)
                   (expect (document-encoding updated) :to-be encoding)
                   (expect (document-bom-p updated) :to-be t)
                   (expect prospective-octets :to-be output-octets)
                   (expect (length actual-output) :to-be output-octets)
                   (expect actual-output :to-equalp encoded-output)
                   (expect (document-text updated) :to-equal output)))
               (let ((limited-document
                       (parse-org input-octets :encoding encoding
                                  :limits (make-limits
                                           :max-output-octets
                                           (1- output-octets)))))
                 (signals org-resource-limit-error
                   (handler-bind
                       ((org-resource-limit-error
                          (lambda (condition)
                            (with-soft-assertions
                              (expect (org-resource-limit-error-limit-name condition)
                                      :to-be :max-output-octets)
                              (expect (org-resource-limit-error-limit condition)
                                      :to-be (1- output-octets))
                              (expect (org-resource-limit-error-actual condition)
                                      :to-be output-octets)))))
                     (org-kit::edit-apply-patches-result
                      limited-document patches :expected expected)))))))

  (it "checks edited input length before reparsing for strings and encoded bytes"
    (let* ((source (format nil "* x~%"))
           (document
             (parse-org source :limits (make-limits :max-input-length 4)))
           (patches (list (org-kit::edit-make-patch 2 3 "long")))
           (expected
             (org-kit::edit-canonical-document
              (parse-org (format nil "* long~%")))))
      (handler-case
          (progn
            (org-kit::edit-apply-patches-result document patches :expected expected)
            (expect nil :to-be t))
        (org-resource-limit-error (condition)
          (with-soft-assertions
            (expect (org-resource-limit-error-limit-name condition)
                    :to-be :max-input-length)
            (expect (org-resource-limit-error-limit condition) :to-be 4)
            (expect (org-resource-limit-error-actual condition) :to-be 7))))
      (expect (document-text document) :to-equal source))
    (let* ((source (format nil "* x~%"))
           (document
             (parse-org (octets #x2A #x20 #x78 #x0A)
                        :encoding :utf-8
                        :limits (make-limits :max-input-length 5)))
           (patches (list (org-kit::edit-make-patch 2 3 "あ")))
           (expected
             (org-kit::edit-canonical-document
              (parse-org (format nil "* あ~%")))))
      (handler-case
          (progn
            (org-kit::edit-apply-patches-result document patches :expected expected)
            (expect nil :to-be t))
        (org-resource-limit-error (condition)
          (with-soft-assertions
            (expect (org-resource-limit-error-limit-name condition)
                    :to-be :max-input-length)
            (expect (org-resource-limit-error-limit condition) :to-be 5)
            (expect (org-resource-limit-error-actual condition) :to-be 6))))
      (expect (document-text document) :to-equal source)))

  (it "applies output-octet budgets independently per document in a transaction"
    (let* ((source-one (format nil "* a~%"))
           (source-two (format nil "* b~%"))
           (document-one
             (parse-org source-one
                        :limits (make-limits :max-output-octets 4)))
           (document-two
             (parse-org source-two
                        :limits (make-limits :max-output-octets 4)))
           (patch-one (list (org-kit::edit-make-patch 2 3 "A")))
           (patch-two (list (org-kit::edit-make-patch 2 3 "B")))
           (expected-one
             (org-kit::edit-canonical-document (parse-org (format nil "* A~%"))))
           (expected-two
             (org-kit::edit-canonical-document (parse-org (format nil "* B~%"))))
           (result
             (org-kit::edit-apply-patches-transaction
              (list (list document-one patch-one :expected expected-one)
                    (list document-two patch-two :expected expected-two)))))
      (with-soft-assertions
        (expect (length result) :to-be 2)
        (expect (document-text
                 (edit-result-updated-document (first result)))
                :to-equal (format nil "* A~%"))
        (expect (document-text
                 (edit-result-updated-document (second result)))
                :to-equal (format nil "* B~%"))
        (expect (document-text document-one) :to-equal source-one)
        (expect (document-text document-two) :to-equal source-two)))
    (let* ((source-one (format nil "* a~%"))
           (source-two (format nil "* b~%"))
           (document-one
             (parse-org source-one
                        :limits (make-limits :max-output-octets 4)))
           (document-two
             (parse-org source-two
                        :limits (make-limits :max-output-octets 3)))
           (patch-one (list (org-kit::edit-make-patch 2 3 "A")))
           (patch-two (list (org-kit::edit-make-patch 2 3 "B")))
           (expected-one
             (org-kit::edit-canonical-document (parse-org (format nil "* A~%"))))
           (expected-two
             (org-kit::edit-canonical-document (parse-org (format nil "* B~%"))))
           (transaction-result :no-result))
      (handler-case
          (progn
            (setf transaction-result
                  (org-kit::edit-apply-patches-transaction
                   (list (list document-one patch-one :expected expected-one)
                         (list document-two patch-two :expected expected-two))))
            (expect nil :to-be t))
        (org-resource-limit-error (condition)
          (with-soft-assertions
            (expect (org-resource-limit-error-limit-name condition)
                    :to-be :max-output-octets)
            (expect (org-resource-limit-error-limit condition) :to-be 3)
            (expect (org-resource-limit-error-actual condition) :to-be 4))))
      (with-soft-assertions
        (expect transaction-result :to-be :no-result)
        (expect (document-text document-one) :to-equal source-one)
        (expect (document-text document-two) :to-equal source-two))))

  (it "uses the default output budget for an empty transaction"
    (expect (org-kit::edit-apply-patches-transaction nil) :to-equal nil))

  (it "refuses edited text that cannot be encoded using the original encoding"
    (let* ((document (parse-org (octets #x2A #x20 #x78 #x0A) :encoding :latin-1))
           (headline (edit-test-headline document)))
      (signals org-kit-error
        (handler-bind
            ((org-kit-error
               (lambda (condition)
                 (expect (org-kit-error-message condition)
                         :to-equal
                         "Edited text cannot be represented in LATIN-1"))))
          (replace-heading-title document headline "日")))
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
              (expect description
                      :to-equal
                      (concatenate 'string
                                   (make-string 10 :initial-element #\x)
                                   "\\n"
                                   (make-string 65 :initial-element #\y)
                                   "..."))))))))

  (it-property "random disjoint edits preserve every uncovered source segment"
      ((steps (gen-list (gen-integer :min 0 :max 10) :max-length 12)))
    (let* ((source "0123456789abcdefghijklmn")
           (patches (edit-test-patches-from-steps steps (length source)))
           (output (org-kit::edit-splice-source source patches)))
      (expect (edit-test-unpatched-ranges-match-p source output patches)
              :to-be t))))

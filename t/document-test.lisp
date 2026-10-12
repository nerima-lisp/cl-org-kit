;;;; t/document-test.lisp
(in-package #:cl-org-kit/test)

(defun lines (&rest strings)
  "Join STRINGS with newlines, without a trailing newline."
  (format nil "~{~A~^~%~}" strings))

(describe "documents"
  (it "records the decoded text, encoding, BOM and source name"
    (let ((document (make-document (octets #xEF #xBB #xBF #x2A #x20 #x61)
                                   :source-name "notes.org")))
      (with-soft-assertions
        (expect (document-p document) :to-be t)
        (expect (document-p "text") :to-be nil)
        (expect (document-text document) :to-equal "* a")
        (expect (document-encoding document) :to-be :utf-8)
        (expect (document-bom-p document) :to-be t)
        (expect (document-source-name document) :to-equal "notes.org")
        (expect (document-root document) :to-be nil))))

  (it "passes LIMITS and ENCODING through"
    (with-soft-assertions
      (expect (document-text (make-document (octets #xE9) :encoding :latin-1)) :to-equal "é")
      (signals org-resource-limit-error
        (make-document "abc" :limits (make-limits :max-input-length 1)))))

  (it "keeps the limits it was created with"
    (let* ((limits (make-limits :max-node-count 7))
           (document (make-document "a" :limits limits)))
      (with-soft-assertions
        (expect (document-limits document) :to-be limits)
        (expect (limits-p (document-limits (make-document "a"))) :to-be t)
        (expect (limit-value (document-limits (make-document "a")) :max-node-count)
                :to-be (limit-value (make-limits) :max-node-count)))))

  (it "stores in-buffer settings"
    (let ((document (make-document "")))
      (setf (document-settings document) '(:todo-keywords ("TODO")))
      (expect (document-settings document) :to-equal '(:todo-keywords ("TODO")))))

  (it "counts lines, including an empty last line after a final newline"
    (with-soft-assertions
      (expect (document-line-count (make-document "")) :to-be 1)
      (expect (document-line-count (make-document "a")) :to-be 1)
      (expect (document-line-count (make-document (lines "a" ""))) :to-be 2)
      (expect (document-line-count (make-document (lines "a" "b" "c"))) :to-be 3)))

  (it "maps offsets to 1-based lines and columns"
    (let ((document (make-document (lines "ab" "" "cde" "f"))))
      (with-soft-assertions
        (expect (multiple-value-list (offset-line-column document 0)) :to-equal '(1 1))
        (expect (multiple-value-list (offset-line-column document 2)) :to-equal '(1 3))
        (expect (multiple-value-list (offset-line-column document 3)) :to-equal '(2 1))
        (expect (multiple-value-list (offset-line-column document 4)) :to-equal '(3 1))
        (expect (multiple-value-list (offset-line-column document 6)) :to-equal '(3 3))
        (expect (multiple-value-list (offset-line-column document 8)) :to-equal '(4 1))
        (expect (multiple-value-list (offset-line-column document 9)) :to-equal '(4 2)))))

  (it "keeps the CR of a CRLF pair on its line"
    (let ((document (make-document (coerce '(#\a #\Return #\Newline #\b) 'string))))
      (with-soft-assertions
        (expect (multiple-value-list (offset-line-column document 1)) :to-equal '(1 2))
        (expect (multiple-value-list (offset-line-column document 3)) :to-equal '(2 1)))))

  (it "rejects offsets outside the text"
    (let ((document (make-document "ab")))
      (with-soft-assertions
        (signals type-error (offset-line-column document -1))
        (signals type-error (offset-line-column document 3))
        (signals type-error (offset-line-column document 1.5)))))

  (it "maps lines back to offsets"
    (let ((document (make-document (lines "ab" "c"))))
      (with-soft-assertions
        (expect (line-offset document 1) :to-be 0)
        (expect (line-offset document 2) :to-be 3)
        (signals type-error (line-offset document 0))
        (signals type-error (line-offset document 3))
        (signals type-error (line-offset document "1")))))

  (it "classifies line endings"
    (flet ((newline (text) (document-newline (make-document text))))
      (with-soft-assertions
        (expect (newline "a") :to-be :none)
        (expect (newline (lines "a" "b")) :to-be :lf)
        (expect (newline (string #\Newline)) :to-be :lf)
        (expect (newline (coerce '(#\a #\Return #\Newline) 'string)) :to-be :crlf)
        (expect (newline (coerce '(#\a #\Return #\Newline #\b #\Newline) 'string)) :to-be :mixed))))

  (it "reports whether the text ends with a newline"
    (with-soft-assertions
      (expect (document-final-newline-p (make-document "")) :to-be nil)
      (expect (document-final-newline-p (make-document "a")) :to-be nil)
      (expect (document-final-newline-p (make-document (lines "a" ""))) :to-be t))))

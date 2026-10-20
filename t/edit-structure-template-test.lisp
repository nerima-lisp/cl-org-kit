(in-package #:cl-org-kit/test)
(defun edit-structure-template-test-error (thunk)
  (handler-case
      (progn (funcall thunk) nil)
    (org-edit-error (condition) condition)))

(defun edit-structure-template-test-preserves-outside-span-p (source result)
  (let* ((spans (edit-result-spans result))
         (span (first spans))
         (start (and span (edit-span-start span)))
         (end (and span (edit-span-end span)))
         (updated (document-text (edit-result-updated-document result)))
         (updated-suffix-start (and span (- (length updated) (- (length source) end)))))
    (and (= (length spans) 1)
         (<= 0 start end (length source))
         (string= source updated :start1 0 :end1 start :start2 0 :end2 start)
         (string= source updated
                  :start1 end :end1 (length source)
                  :start2 updated-suffix-start :end2 (length updated)))))

(describe "Org structure template insertion"
  (it "inserts every Org 9.8.10 alias with exact empty-block bytes"
    (dolist (fixture
             (list
              (list "a" (format nil "#+begin_export ascii~%#+end_export~%"))
              (list "c" (format nil "#+begin_center~%#+end_center~%"))
              (list "C" (format nil "#+begin_comment~%#+end_comment~%"))
              (list "e" (format nil "#+begin_example~%#+end_example~%"))
              (list "E" (format nil "#+begin_export ~%#+end_export~%"))
              (list "h" (format nil "#+begin_export html~%#+end_export~%"))
              (list "l" (format nil "#+begin_export latex~%#+end_export~%"))
              (list "q" (format nil "#+begin_quote~%#+end_quote~%"))
              (list "s" (format nil "#+begin_src ~%#+end_src~%"))
              (list "v" (format nil "#+begin_verse~%#+end_verse~%"))
              (list "SRC" (format nil "#+BEGIN_SRC ~%#+END_SRC~%"))))
      (let* ((source "")
             (document (parse-org source))
             (result
               (org-kit::insert-structure-template
                document (make-edit-span 0 0) (first fixture)))
             (updated (edit-result-updated-document result)))
        (with-soft-assertions
          (expect (edit-result-p result) :to-be t)
          (expect (document-text updated) :to-equal (second fixture))
          (expect (edit-structure-template-test-preserves-outside-span-p
                   source result)
                  :to-be t)
          (expect (document-text document) :to-equal source)))))

  (it "wraps source, example, export and comment regions with Org escaping"
    (let* ((source (format nil "before~%* heading~%#+end_src~%after~%"))
           (start (search "* heading" source))
           (end (search "after" source)))
      (dolist (fixture
               (list (list "src" "begin" "src" "end" "src" t)
                     (list "example" "begin" "example" "end" "example" nil)
                     (list "export" "begin" "export" "end" "export" t)
                     (list "comment" "begin" "comment" "end" "comment" nil)
                     (list "SRC" "BEGIN" "SRC" "END" "SRC" t)))
        (let* ((type (first fixture))
               (begin-word (second fixture))
               (open-name (third fixture))
               (end-word (fourth fixture))
               (close-name (fifth fixture))
               (extended-p (sixth fixture))
               (document (parse-org source))
               (result
                 (org-kit::insert-structure-template
                  document (make-edit-span start end) type))
               (expected
                 (format nil
                         "before~%#+~A_~A~A~%,* heading~%,#+end_src~%#+~A_~A~%after~%"
                         begin-word open-name
                         (if extended-p " " "")
                         end-word close-name)))
          (with-soft-assertions
            (expect (document-text (edit-result-updated-document result))
                    :to-equal expected)
            (expect (edit-structure-template-test-preserves-outside-span-p
                     source result)
                    :to-be t)
            (expect (document-text document) :to-equal source))))))

  (it "inserts an empty block at a point in existing source"
    (let* ((source (format nil "before~%"))
           (document (parse-org source))
           (point (length source))
           (result
             (org-kit::insert-structure-template
              document (make-edit-span point point) "q")))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result))
                :to-equal
                (format nil "before~%#+begin_quote~%#+end_quote~%"))
        (expect (edit-span-start (first (edit-result-spans result)))
                :to-be point)
        (expect (edit-span-end (first (edit-result-spans result)))
                :to-be point)
        (expect (edit-structure-template-test-preserves-outside-span-p
                 source result)
                :to-be t)
        (expect (document-text document) :to-equal source))))

  (it "reports invalid template values and spans as edit errors"
    (let* ((source (format nil "plain~%"))
           (document (parse-org source))
           (invalid-type
             (edit-structure-template-test-error
              (lambda ()
                (org-kit::insert-structure-template
                 document (make-edit-span 0 0) ""))))
           (invalid-span
             (edit-structure-template-test-error
              (lambda ()
                (org-kit::insert-structure-template
                 document (make-edit-span -1 0) "quote")))))
      (with-soft-assertions
        (expect (typep invalid-type 'org-edit-error) :to-be t)
        (when (typep invalid-type 'org-edit-error)
          (expect (org-edit-error-reason invalid-type) :to-be :invalid-value)
          (expect (org-edit-error-target invalid-type)
                  :to-equal "structure template"))
        (expect (typep invalid-span 'org-edit-error) :to-be t)
        (when (typep invalid-span 'org-edit-error)
          (expect (org-edit-error-reason invalid-span) :to-be :invalid-value)
          (expect (org-edit-error-target invalid-span)
                  :to-equal "structure template"))
        (expect (document-text document) :to-equal source)))))
(describe "Org structure template validation and source boundaries"
  (it "requires a document and a valid source span"
    (let* ((document (parse-org ""))
           (document-error
             (handler-case
                 (progn
                   (org-kit::insert-structure-template
                    nil (make-edit-span 0 0) "quote")
                   nil)
               (type-error (condition) condition)))
           (invalid-spans
             (mapcar (lambda (span)
                       (edit-structure-template-test-error
                        (lambda ()
                          (org-kit::insert-structure-template
                           document span "quote"))))
                     (list nil
                           (make-edit-span "0" 0)
                           (make-edit-span 0 "0")
                           (make-edit-span 2 1)
                           (make-edit-span 0 1)))))
      (with-soft-assertions
        (expect (typep document-error 'type-error) :to-be t)
        (expect
         (every (lambda (condition)
                  (and (typep condition 'org-edit-error)
                       (eq (org-edit-error-reason condition) :invalid-value)
                       (string= (org-edit-error-target condition)
                                "structure template")))
                invalid-spans)
         :to-be t))))

  (it "rejects invalid template strings while accepting block-name punctuation"
    (let* ((document (parse-org ""))
           (invalid-types
             (mapcar (lambda (type)
                       (edit-structure-template-test-error
                        (lambda ()
                          (org-kit::insert-structure-template
                           document (make-edit-span 0 0) type))))
                     (list nil
                           ""
                           " "
                           " quote"
                           "quote "
                           (format nil "left~%right")
                           (format nil "left~Cright" #\Return)
                           (format nil "left~Cright" #\Tab)
                           "bad/name")))
           (valid-result
             (org-kit::insert-structure-template
              document (make-edit-span 0 0) "custom-name_2")))
      (with-soft-assertions
        (expect
         (every (lambda (condition)
                  (and (typep condition 'org-edit-error)
                       (eq (org-edit-error-reason condition) :invalid-value)
                       (string= (org-edit-error-target condition)
                                "structure template")))
                invalid-types)
         :to-be t)
        (expect (document-text (edit-result-updated-document valid-result))
                :to-equal
                (format nil "#+begin_custom-name_2~%#+end_custom-name_2~%")))))

  (it "preserves CRLF when inserting before or after a line"
    (let* ((crlf (format nil "~C~C" #\Return #\Newline))
           (source (concatenate 'string "before" crlf))
           (document (parse-org source))
           (at-start
             (org-kit::insert-structure-template
              document (make-edit-span 0 0) "quote"))
           (at-end
             (org-kit::insert-structure-template
              document (make-edit-span (length source) (length source)) "quote")))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document at-start))
                :to-equal
                (concatenate 'string "#+begin_quote" crlf "#+end_quote" crlf
                             source))
        (expect (document-text (edit-result-updated-document at-end))
                :to-equal
                (concatenate 'string source "#+begin_quote" crlf
                             "#+end_quote" crlf))
        (expect (document-text document) :to-equal source))))

  (it "inserts at an interior point and preserves horizontal indentation"
    (let* ((source (format nil "before~%"))
           (document (parse-org source))
           (middle
             (org-kit::insert-structure-template
              document (make-edit-span 3 3) "quote"))
           (indented-source (format nil " ~Cbody~%" #\Tab))
           (indented-document (parse-org indented-source))
           (indented
             (org-kit::insert-structure-template
              indented-document (make-edit-span 2 2) "quote")))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document middle))
                :to-equal
                (format nil "bef~%#+begin_quote~%#+end_quote~%ore~%"))
        (expect (document-text (edit-result-updated-document indented))
                :to-equal
                (concatenate 'string "  #+begin_quote" (string #\Newline)
                             "  #+end_quote" (string #\Newline)
                             indented-source))
        (expect (document-text document) :to-equal source)
        (expect (document-text indented-document) :to-equal indented-source))))

  (it "wraps plain verbatim lines and trims only trailing horizontal space"
    (let* ((plain-source "ordinary line")
           (plain-document (parse-org plain-source))
           (plain-result
             (org-kit::insert-structure-template
              plain-document (make-edit-span 0 (length plain-source)) "src"))
           (spaced-source (format nil "body ~C~%" #\Tab))
           (body-end (position #\Newline spaced-source))
           (spaced-document (parse-org spaced-source))
           (spaced-result
             (org-kit::insert-structure-template
              spaced-document (make-edit-span 0 body-end) "quote"))
           (blank-source (format nil "  ~%"))
           (blank-document (parse-org blank-source))
           (blank-result
             (org-kit::insert-structure-template
              blank-document
              (make-edit-span 0 (position #\Newline blank-source)) "quote")))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document plain-result))
                :to-equal
                (format nil "#+begin_src ~%ordinary line~%#+end_src~%"))
        (expect (document-text (edit-result-updated-document spaced-result))
                :to-equal
                (format nil "#+begin_quote~%body~%#+end_quote~%"))
        (expect (document-text (edit-result-updated-document blank-result))
                :to-equal
                (format nil "  #+begin_quote~%  #+end_quote~%")))))

  (it "escapes Org markers and preserves ordinary example content"
    (let* ((escaped-source (format nil " ,* item~% ,#+name: block"))
           (escaped-document (parse-org escaped-source))
           (escaped-result
             (org-kit:insert-structure-template
              escaped-document
              (make-edit-span 0 (length escaped-source)) "example"))
           (cases
             (list
              (list (format nil "first ordinary line~%second ordinary line")
                    (format nil
                            "#+begin_example~%first ordinary line~%second ordinary line~%#+end_example~%"))
              (list ""
                    (format nil "#+begin_example~%#+end_example~%"))
              (list "   "
                    (format nil "   #+begin_example~%   #+end_example~%"))
              (list ","
                    (format nil "#+begin_example~%,~%#+end_example~%"))
              (list "#"
                    (format nil "#+begin_example~%#~%#+end_example~%")))))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document escaped-result))
                :to-equal
                (format nil " #+begin_example~% ,,* item~% ,,#+name: block~% #+end_example~%"))
        (dolist (fixture cases)
          (let* ((source (first fixture))
                 (document (parse-org source))
                 (result
                   (org-kit:insert-structure-template
                    document (make-edit-span 0 (length source)) "example")))
            (expect (document-text (edit-result-updated-document result))
                    :to-equal (second fixture))
            (expect (document-text document) :to-equal source))))))
  (it "preserves an indented blank internal line in a verbatim block"
    (let* ((source (format nil "first line~%   ~%last line"))
           (document (parse-org source))
           (result
            (org-kit:insert-structure-template document
                                               (make-edit-span 0
                                                               (length source))
                                               "example")))
      (with-soft-assertions
       (expect (document-text (edit-result-updated-document result))
               :to-equal
               (format nil
                       "#+begin_example~%first line~%   ~%last line~%#+end_example~%"))
       (expect (document-text document) :to-equal source)))))


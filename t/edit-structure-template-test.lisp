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


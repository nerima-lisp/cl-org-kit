(in-package #:cl-org-kit/test)

(defun babel-results-test-source-block (document)
  (let ((block nil))
    (map-nodes (lambda (node)
                 (when (eq (node-type node) :src-block)
                   (setf block node)))
               (document-root document))
    block))

(defun babel-results-test-update (source value parameters &key hash language limits)
  (let* ((document (parse-org source :limits (or limits (make-limits))))
         (block (babel-results-test-source-block document)))
    (org-kit::update-babel-results document block value
                                   :parameters parameters
                                   :hash hash :language language)))

(defun babel-results-test-condition (function)
  (handler-case
      (progn (funcall function) nil)
    (error (condition) condition)))

(defun babel-results-test-call-with-range-counter (function)
  (let* ((symbol (find-symbol "STRING-SIZE-IN-OCTETS" "CL-CODEC-KIT"))
         (original (symbol-function symbol))
         (range-sizes nil))
    (unwind-protect
         (progn
           (setf (symbol-function symbol)
                 (lambda (&rest arguments)
                   (let* ((string (first arguments))
                          (start (or (getf (rest arguments) :start) 0))
                          (end (or (getf (rest arguments) :end)
                                   (length string))))
                     (push (- end start) range-sizes))
                   (apply original arguments)))
           (funcall function (lambda () (nreverse range-sizes))))
      (setf (symbol-function symbol) original))))

(describe "Babel result formatting"
  (it "writes every supported result format"
    (let* ((source (format nil "#+NAME: sample~%#+BEGIN_SRC text~%body~%#+END_SRC~%"))
           (cases
             (list
              (list '(("RESULTS" . "table")) '((1 2) (3 4))
                    (format nil "| 1 | 2 |~%| 3 | 4 |"))
              (list '(("RESULTS" . "list")) '("one" "two")
                    (format nil "- one~%- two"))
              (list '(("RESULTS" . "verbatim")) "text"
                    (format nil "#+BEGIN_EXAMPLE~%text~%#+END_EXAMPLE"))
              (list '(("RESULTS" . "raw")) "text" "text")
              (list '(("RESULTS" . "org")) "* result" "* result")
              (list '(("RESULTS" . "drawer")) "text"
                    (format nil ":RESULTS:~%text~%:END:"))
              (list '(("RESULTS" . "html")) "<b>text</b>"
                    (format nil "#+BEGIN_EXPORT html~%<b>text</b>~%#+END_EXPORT"))
              (list '(("RESULTS" . "code")) "(print 1)"
                    (format nil "#+BEGIN_SRC text~%(print 1)~%#+END_SRC")))))
      (with-soft-assertions
        (dolist (case cases)
          (destructuring-bind (parameters value expected) case
            (let* ((edit (babel-results-test-update source value parameters
                                                    :language "text"))
                   (updated (edit-result-updated-document edit)))
              (expect (document-text updated) :to-equal (format nil "~A#+RESULTS: sample~%~A~%" source expected)))))))))

(describe "Babel result edit ranges"
  (it "preserves mixed-newline text outside the inserted result region"
    (let* ((crlf (format nil "~C~C" #\Return #\Newline))
           (source (concatenate 'string
                                "前" crlf
                                "#+NAME: sample" crlf
                                "#+BEGIN_SRC text" crlf
                                "body" crlf
                                "#+END_SRC" crlf
                                crlf "outside" crlf "tail"))
           (document (parse-org source))
           (block (babel-results-test-source-block document))
           (edit (org-kit::update-babel-results document block "結果"))
           (updated (edit-result-updated-document edit))
           (new-source (document-text updated))
           (span (first (edit-result-spans edit)))
           (start (edit-span-start span))
           (end (edit-span-end span))
           (inserted-length (- (length new-source) (length source))))
      (with-soft-assertions
        (expect (= start end) :to-be t)
        (expect (cl-codec-kit:string-to-octets (subseq new-source 0 start) :encoding :utf-8) :to-equalp (cl-codec-kit:string-to-octets (subseq source 0 start) :encoding :utf-8))
        (expect (cl-codec-kit:string-to-octets (subseq new-source (+ start inserted-length)) :encoding :utf-8) :to-equalp (cl-codec-kit:string-to-octets (subseq source end) :encoding :utf-8))
        (expect (document-text document) :to-equal source)
        (expect (search (format nil "#+RESULTS: sample~C~C結果" #\Return #\Newline) new-source) :not :to-be nil))))

  (it "appends and prepends inside an existing result region"
    (let* ((source (format nil "#+NAME: sample~%#+BEGIN_SRC text~%body~%#+END_SRC~%~%#+RESULTS: sample~%old~%~%* Later~%"))
           (document (parse-org source))
           (block (babel-results-test-source-block document))
           (appended (org-kit::update-babel-results
                      document block "new"
                      :parameters '(("RESULTS" . "append"))))
           (prepended (org-kit::update-babel-results
                       document block "new"
                       :parameters '(("RESULTS" . "prepend"))))
           (silent (org-kit::update-babel-results
                    document block "ignored"
                    :parameters '(("RESULTS" . "silent")))))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document appended)) :to-equal (format nil "#+NAME: sample~%#+BEGIN_SRC text~%body~%#+END_SRC~%~%#+RESULTS: sample~%old~%new~%~%* Later~%"))
        (expect (document-text (edit-result-updated-document prepended)) :to-equal (format nil "#+NAME: sample~%#+BEGIN_SRC text~%body~%#+END_SRC~%~%#+RESULTS: sample~%new~%old~%~%* Later~%"))
        (expect (document-text (edit-result-updated-document silent))
                :to-equal source)
        (expect (edit-result-spans silent) :to-equal nil)))))

(describe "Babel result validation"
  (it "signals a named error for unsupported result options"
    (let* ((source (format nil "#+NAME: sample~%#+BEGIN_SRC text~%body~%#+END_SRC~%"))
           (condition
             (babel-results-test-condition
              (lambda ()
                (babel-results-test-update
                 source "value" '(("RESULTS" . "latex")))))))
      (with-soft-assertions
        (expect (typep condition 'org-babel-error) :to-be t)
        (when (typep condition 'org-babel-error)
          (expect (org-babel-error-reason condition)
                  :to-equal :unsupported-result-option)))))

  (it "checks result element, character, and octet output limits before emitting results"
  (let* ((source (format nil "#+NAME: sample~%#+BEGIN_SRC text~%body~%#+END_SRC~%"))
         (element-limits (make-limits :max-babel-result-elements 1))
         (output-limits (make-limits :max-output-length 3))
         (output-octet-limits (make-limits :max-output-octets 3))
         (output-document (parse-org source :limits output-limits))
         (output-stream
           (make-instance 'org-kit::babel-results-bounded-stream
                          :document output-document))
         (element-condition
           (babel-results-test-condition
            (lambda ()
              (babel-results-test-update
               source '("one" "two") nil :limits element-limits))))
         (output-condition nil)
         (output-range-sizes nil)
         (output-octet-condition
           (babel-results-test-condition
            (lambda ()
              (babel-results-test-update
               source "éé" nil :limits output-octet-limits)))))
    (babel-results-test-call-with-range-counter
     (lambda (range-sizes)
       (setf output-condition
             (babel-results-test-condition
              (lambda ()
                (org-kit::babel-results-check-write output-stream "large"))))
       (setf output-range-sizes (funcall range-sizes))))
    (with-soft-assertions
      (expect (typep element-condition 'org-resource-limit-error) :to-be t)
      (when (typep element-condition 'org-resource-limit-error)
        (expect (org-resource-limit-error-limit-name element-condition)
                :to-equal :max-babel-result-elements))
      (expect (typep output-condition 'org-resource-limit-error) :to-be t)
      (when (typep output-condition 'org-resource-limit-error)
        (expect (org-resource-limit-error-limit-name output-condition)
                :to-equal :max-output-length))
      (expect output-range-sizes :to-equal nil)
      (expect (typep output-octet-condition 'org-resource-limit-error) :to-be t)
      (when (typep output-octet-condition 'org-resource-limit-error)
        (expect (org-resource-limit-error-limit-name output-octet-condition)
                :to-equal :max-output-octets)
        (expect (org-resource-limit-error-actual output-octet-condition)
                :to-equal 4)))))

  (it "counts encoded octets in bounded chunks with one generic BOM preamble"
  (let* ((text (make-string 2050 :initial-element #\é))
         (expected-utf16
           (org-kit::edit-encoded-range-octets text 0 (length text) :utf-16))
         (utf8-count nil)
         (utf16-count nil)
         (range-sizes nil))
    (babel-results-test-call-with-range-counter
     (lambda (get-range-sizes)
       (setf utf8-count
             (org-kit::babel-results-encoded-range-octets
              text 0 (length text) :utf-8))
       (setf utf16-count
             (org-kit::babel-results-encoded-range-octets
              text 0 (length text) :utf-16))
       (setf range-sizes (funcall get-range-sizes))))
    (with-soft-assertions
      (expect utf8-count :to-be (* 2 (length text)))
      (expect utf16-count :to-be expected-utf16)
      (expect (notany (lambda (size) (> size 1024)) range-sizes) :to-be t)
      (expect (count 1024 range-sizes) :to-be 4)))))

(describe "Babel result channels and replacement" (it "supports output results and replaces the existing result region"
    (let* ((source
             (format nil
                     "#+NAME: sample~%#+BEGIN_SRC text~%body~%#+END_SRC~%~%#+RESULTS: sample~%old result~%"))
           (document (parse-org source))
           (block (babel-results-test-source-block document))
           (output
             (org-kit::update-babel-results
              document block "stdout"
              :parameters '(("RESULTS" . "output raw"))))
           (replacement (org-kit::update-babel-results document block "new value"))
           (invalid
             (babel-results-test-condition
              (lambda ()
                (org-kit::update-babel-results
                 document block 42
                 :parameters '(("RESULTS" . "output raw")))))))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document output)) :to-equal (format nil "#+NAME: sample~%#+BEGIN_SRC text~%body~%#+END_SRC~%~%#+RESULTS: sample~%stdout~%"))
        (expect (document-text (edit-result-updated-document replacement)) :to-equal (format nil "#+NAME: sample~%#+BEGIN_SRC text~%body~%#+END_SRC~%~%#+RESULTS: sample~%new value~%"))
        (expect (search "old result"
                        (document-text (edit-result-updated-document replacement)))
                :to-be nil)
        (expect (typep invalid 'org-babel-error) :to-be t)
        (when (typep invalid 'org-babel-error)
          (expect (org-babel-error-reason invalid)
                  :to-equal :unsupported-result-channel))))))
(describe "Babel result AST regions"
  (it "uses parsed table and list result nodes for replacement"
    (let* ((table-source
             (format nil
                     "#+NAME: sample~%#+BEGIN_SRC text~%body~%#+END_SRC~%~%#+RESULTS: sample~%| old |~%| kept |~%~%* Later~%outside~%"))
           (list-source
             (format nil
                     "#+NAME: sample~%#+BEGIN_SRC text~%body~%#+END_SRC~%~%#+RESULTS: sample~%- old~%- kept~%~%* Later~%outside~%"))
           (table-edit
             (babel-results-test-update
              table-source (list (list 3) (list 4))
              (list (cons "RESULTS" "table"))))
           (list-edit
             (babel-results-test-update
              list-source (list "new" "value")
              (list (cons "RESULTS" "list")))))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document table-edit))
                :to-equal
                (format nil
                        "#+NAME: sample~%#+BEGIN_SRC text~%body~%#+END_SRC~%~%#+RESULTS: sample~%| 3 |~%| 4 |~%~%* Later~%outside~%"))
        (expect (document-text (edit-result-updated-document list-edit))
                :to-equal
                (format nil
                        "#+NAME: sample~%#+BEGIN_SRC text~%body~%#+END_SRC~%~%#+RESULTS: sample~%- new~%- value~%~%* Later~%outside~%")))))
  (it "preserves and replaces the hash in a parsed RESULTS keyword"
    (let* ((source
             (format nil
                     "#+NAME: sample~%#+BEGIN_SRC text~%body~%#+END_SRC~%~%#+RESULTS[old-hash]: sample~%old result~%~%* Later~%outside~%"))
           (document (parse-org source))
           (block (babel-results-test-source-block document))
           (hash-values
             (multiple-value-list
              (org-kit::babel-results-adjacent-hash document block)))
           (edit
             (org-kit::update-babel-results document block "new value"
                                            :hash "new-hash")))
      (with-soft-assertions
        (expect (first hash-values) :to-equal "old-hash")
        (expect (second hash-values) :to-be t)
        (expect (document-text (edit-result-updated-document edit))
                :to-equal
                (format nil
                        "#+NAME: sample~%#+BEGIN_SRC text~%body~%#+END_SRC~%~%#+RESULTS[new-hash]: sample~%new value~%~%* Later~%outside~%")))))
)

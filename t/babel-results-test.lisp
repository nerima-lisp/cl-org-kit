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
(describe "Babel result boundaries"
  (it "normalizes text line endings and writes vector table rows"
    (let* ((source (format nil "#+NAME: sample~%#+BEGIN_SRC text~%body~%#+END_SRC~%"))
           (crlf (format nil "~C~C" #\Return #\Newline))
           (mixed-value
             (concatenate (quote string)
                          "first" crlf "second" (string #\Return)
                          "third" (string #\Newline) "last" crlf))
           (normalized (babel-results-test-update source mixed-value nil))
           (table
             (babel-results-test-update
              source #(#(1 2) #(3 4))
              (quote (("RESULTS" . "table"))))))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document normalized))
                :to-equal
                (format nil "#+NAME: sample~%#+BEGIN_SRC text~%body~%#+END_SRC~%#+RESULTS: sample~%first~%second~%third~%last~%"))
        (expect (document-text (edit-result-updated-document table))
                :to-equal
                (format nil "#+NAME: sample~%#+BEGIN_SRC text~%body~%#+END_SRC~%#+RESULTS: sample~%| 1 | 2 |~%| 3 | 4 |~%")))))
  (it "chooses the nearest newline style in mixed documents"
    (let* ((crlf (format nil "~C~C" #\Return #\Newline))
           (document
             (parse-org
              (concatenate (quote string) "first" crlf "second"
                           (string #\Newline))))
           (source (document-text document))
           (newline (string #\Newline)))
      (with-soft-assertions
        (expect (org-kit::document-newline document) :to-equal :mixed)
        (expect (org-kit::babel-results-newline document (length source))
                :to-equal newline)
        (expect (org-kit::babel-results-newline document (1- (length source)))
                :to-equal crlf)
        (expect (org-kit::babel-results-newline document 0)
                :to-equal newline))))
  (it "updates UTF-16 CRLF results without consuming whitespace after them"
    (let* ((crlf (format nil "~C~C" #\Return #\Newline))
           (source
             (concatenate
              (quote string)
              "#+NAME: sample" crlf
              "#+BEGIN_SRC text" crlf
              "body" crlf
              "#+END_SRC" crlf
              crlf
              "#+RESULTS: sample" crlf
              "old" crlf
              " " crlf
              crlf
              "* Later" crlf
              "outside" crlf))
           (octets
             (cl-codec-kit:string-to-octets source :encoding :utf-16))
           (document (parse-org octets :encoding :utf-16))
           (block (babel-results-test-source-block document))
           (edit (org-kit::update-babel-results document block "new"))
           (updated (edit-result-updated-document edit)))
      (with-soft-assertions
        (expect (document-encoding document) :to-equal :utf-16)
        (expect (document-text updated)
                :to-equal
                (concatenate
                 (quote string)
                 "#+NAME: sample" crlf
                 "#+BEGIN_SRC text" crlf
                 "body" crlf
                 "#+END_SRC" crlf
                 crlf
                 "#+RESULTS: sample" crlf
                 "new" crlf
                 " " crlf
                 crlf
                 "* Later" crlf
                 "outside" crlf)))))
  (it "rejects invalid inputs, targets, result metadata, and code languages"
    (let* ((source
             (format nil "#+NAME: sample~%#+BEGIN_SRC text~%body~%#+END_SRC~%"))
           (document (parse-org source))
           (block (babel-results-test-source-block document))
           (other-block
             (babel-results-test-source-block (parse-org source)))
           (invalid-document
             (babel-results-test-condition
              (lambda () (org-kit::update-babel-results nil nil "value"))))
           (invalid-block
             (babel-results-test-condition
              (lambda () (org-kit::update-babel-results document nil "value"))))
           (invalid-target
             (babel-results-test-condition
              (lambda ()
                (org-kit::babel-results-anchor-offset
                 document (document-root document)))))
           (mismatched-block
             (babel-results-test-condition
              (lambda ()
                (org-kit::babel-results-anchor-offset document other-block))))
           (invalid-parameters
             (babel-results-test-condition
              (lambda ()
                (org-kit::update-babel-results
                 document block "value"
                 :parameters (quote (("results" . "raw")))))))
           (invalid-label
             (babel-results-test-condition
              (lambda ()
                (org-kit::babel-results-marker-text
                 nil (format nil "first~%second")))))
           (invalid-hash
             (babel-results-test-condition
              (lambda () (org-kit::babel-results-validate-hash "bad[hash"))))
           (invalid-hash-type
             (babel-results-test-condition
              (lambda () (org-kit::babel-results-validate-hash 42))))
           (invalid-code-language
             (babel-results-test-condition
              (lambda ()
                (babel-results-test-update
                 source "value" (quote (("RESULTS" . "code")))
                 :language (format nil "bad~%language")))))
           (invalid-code-language-type
             (babel-results-test-condition
              (lambda ()
                (babel-results-test-update
                 source "value" (quote (("RESULTS" . "code")))
                 :language 42))))
           (invalid-table
             (babel-results-test-condition
              (lambda ()
                (babel-results-test-update
                 source "not a table" (quote (("RESULTS" . "table"))))))))
      (with-soft-assertions
        (expect (typep invalid-document 'type-error) :to-be t)
        (expect (typep invalid-block 'type-error) :to-be t)
        (expect (typep invalid-target 'org-babel-error) :to-be t)
        (when (typep invalid-target 'org-babel-error)
          (expect (org-babel-error-reason invalid-target)
                  :to-equal :invalid-target))
        (expect (typep mismatched-block 'org-babel-error) :to-be t)
        (when (typep mismatched-block 'org-babel-error)
          (expect (org-babel-error-reason mismatched-block)
                  :to-equal :document-mismatch))
        (expect (typep invalid-parameters 'type-error) :to-be t)
        (expect (typep invalid-label 'org-babel-error) :to-be t)
        (when (typep invalid-label 'org-babel-error)
          (expect (org-babel-error-reason invalid-label)
                  :to-equal :invalid-result-label))
        (expect (typep invalid-hash 'org-babel-error) :to-be t)
        (expect (typep invalid-hash-type 'org-babel-error) :to-be t)
        (when (typep invalid-hash 'org-babel-error)
          (expect (org-babel-error-reason invalid-hash)
                  :to-equal :invalid-result-hash))
        (expect (typep invalid-code-language 'org-babel-error) :to-be t)
        (expect (typep invalid-code-language-type 'org-babel-error) :to-be t)
        (when (typep invalid-code-language 'org-babel-error)
          (expect (org-babel-error-reason invalid-code-language)
                  :to-equal :invalid-result-language))
        (expect (typep invalid-table 'org-babel-error) :to-be t)
        (when (typep invalid-table 'org-babel-error)
          (expect (org-babel-error-reason invalid-table)
                  :to-equal :unsupported-result-format))))))
(describe "Babel result helper boundaries"
  (it "handles empty writes, trimmed line endings, and plain RESULTS keywords"
    (let* ((document (parse-org ""))
           (stream
             (make-instance 'org-kit::babel-results-bounded-stream
                            :document document))
           (keyword
             (org-kit:make-node
              :keyword
              :properties (list :key "RESULTS" :value "sample")))
           (newline (string #\Newline))
           (crlf (format nil "~C~C" #\Return #\Newline)))
      (sb-gray:stream-write-string stream "")
      (org-kit::babel-results-ensure-newline stream newline)
      (org-kit::babel-results-ensure-newline stream newline)
      (with-soft-assertions
        (expect (org-kit::babel-results-stream-characters stream) :to-be 1)
        (expect (get-output-stream-string
                 (org-kit::babel-results-stream-output stream))
                :to-equal
                newline)
        (expect (org-kit::babel-results-trim-final-newlines "") :to-equal "")
        (expect (org-kit::babel-results-trim-final-newlines "value")
                :to-equal
                "value")
        (expect (org-kit::babel-results-trim-final-newlines
                 (concatenate 'string "value" crlf))
                :to-equal
                "value")
        (expect (org-kit::babel-results-strip-final-newline "")
                :to-equal
                "")
        (expect (org-kit::babel-results-strip-final-newline "value")
                :to-equal
                "value")
        (expect
         (multiple-value-list
          (org-kit::babel-results-keyword-marker-fields keyword))
         :to-equal
         (list nil "sample" t)))))
  (it "rejects invalid result parameters and string table rows"
    (let* ((document
             (parse-org
              (format nil "#+NAME: sample~%#+BEGIN_SRC text~%body~%#+END_SRC")))
           (block (babel-results-test-source-block document))
           (key-error
             (babel-results-test-condition
              (lambda ()
                (org-kit::babel-results-parameters
                 document block (list (cons 42 "raw"))))))
           (value-error
             (babel-results-test-condition
              (lambda ()
                (org-kit::babel-results-parameters
                 document block (list (cons "RESULTS" 42))))))
           (row-error
             (babel-results-test-condition
              (lambda ()
                (org-kit::babel-results-sequence "not a table row")))))
      (with-soft-assertions
        (expect (typep key-error 'type-error) :to-be t)
        (expect (typep value-error 'type-error) :to-be t)
        (expect (typep row-error 'org-kit:org-babel-error) :to-be t)
        (when (typep row-error 'org-kit:org-babel-error)
          (expect (org-kit:org-babel-error-reason row-error)
                  :to-be
                  :unsupported-result-format)))))
  (it "uses the empty-content branches for append and prepend"
    (let* ((source
             (format nil
                     "#+NAME: sample~%#+BEGIN_SRC text~%body~%#+END_SRC~%#+RESULTS: sample~%"))
           (document (parse-org source))
           (block (babel-results-test-source-block document))
           (append-edit
             (org-kit::update-babel-results
              document block "added"
              :parameters '(("RESULTS" . "append"))))
           (prepend-edit
             (org-kit::update-babel-results
              document block "added"
              :parameters '(("RESULTS" . "prepend"))))
           (expected (format nil "~Aadded~%" source)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document append-edit))
                :to-equal
                expected)
        (expect (document-text (edit-result-updated-document prepend-edit))
                :to-equal
                expected))))
  (it "trims a CR-only post-blank and uses the node-begin fallback"
    (let* ((cr-document
             (parse-org (format nil "body~C" #\Return)))
           (text (org-kit:document-text cr-document))
           (cr-node
             (org-kit:make-node :paragraph
                                :end (length text)
                                :post-blank 1))
           (document
             (parse-org
              (format nil
                      "#+NAME: sample~%#+BEGIN_SRC text~%body~%#+END_SRC")))
           (block (babel-results-test-source-block document))
           (anchor (org-kit::babel-results-anchor-offset document block))
           (candidate
             (org-kit:make-node
              :table :begin anchor :end anchor
              :properties (list :results "sample"))))
      (org-kit:append-child (org-kit:node-parent block) candidate)
      (with-soft-assertions
        (expect (org-kit::babel-results-node-end-before-post-blank
                 cr-document cr-node)
                :to-be
                (1- (length text)))
        (expect
         (multiple-value-list
          (org-kit::babel-results-adjacent-region document block))
         :to-equal
         (list anchor anchor anchor nil "sample" t)))))
  (it "covers result metadata and insertion boundaries"
    (let* ((source
             (format nil "#+NAME: sample~%#+BEGIN_SRC text~%body~%#+END_SRC"))
           (document (parse-org source))
           (block (babel-results-test-source-block document))
           (flag-parameters
             (org-kit::babel-results-parameters
              document block '(("FLAG"))))
           (non-keyword (org-kit:make-node :table
                                           :properties (list :key "RESULTS")))
           (non-string-keyword
             (org-kit:make-node :keyword
                                :properties (list :key 42 :value "sample")))
           (other-keyword
             (org-kit:make-node :keyword
                                :properties (list :key "NOTRESULTS"
                                                  :value "sample")))
           (marker-document
             (parse-org (format nil "#+RESULTS[broken: sample~%")))
           (marker-node
             (let ((node
         (org-kit:make-node
          :keyword :begin 0
          :end (length (org-kit:document-text marker-document))
          :properties (list :key "RESULTS[broken" :value "sample"))))
      (org-kit:append-child (org-kit:document-root marker-document) node)
      node))
           (no-payload-marker (org-kit:make-node :keyword :post-blank 0))
           (blank-marker (org-kit:make-node :keyword :post-blank 1))
           (empty-document (parse-org ""))
           (empty-node (org-kit:make-node :paragraph :end 0 :post-blank 1))
           (lf-document (parse-org (format nil "a~%")))
           (lf-node (org-kit:make-node :paragraph
                                       :end (length (org-kit:document-text lf-document))
                                       :post-blank 1))
           (crlf-document
             (parse-org (format nil "a~C~C" #\Return #\Newline)))
           (crlf-node (org-kit:make-node
                       :paragraph
                       :end (length (org-kit:document-text crlf-document))
                       :post-blank 1))
           (space-document (parse-org (format nil "a ~C" #\Tab)))
           (space-node (org-kit:make-node
                        :paragraph
                        :end (length (org-kit:document-text space-document))
                        :post-blank 1))
           (invalid-label
             (babel-results-test-condition
              (lambda () (org-kit::babel-results-marker-text nil 42))))
           (insertion
             (org-kit::babel-results-insertion-text
              empty-document 0 "#+RESULTS:" "value")))
      (with-soft-assertions
        (expect (assoc "FLAG" flag-parameters :test #'string=) :not :to-be nil)
        (expect (cdr (assoc "FLAG" flag-parameters :test #'string=)) :to-be nil)
        (expect (multiple-value-list
                 (org-kit::babel-results-keyword-marker-fields non-keyword))
                :to-equal (list nil))
        (expect (multiple-value-list
                 (org-kit::babel-results-keyword-marker-fields non-string-keyword))
                :to-equal (list nil))
        (expect (multiple-value-list
                 (org-kit::babel-results-keyword-marker-fields other-keyword))
                :to-equal (list nil))
        (expect (multiple-value-list
                 (org-kit::babel-results-keyword-marker-fields marker-node))
                :to-equal (list nil))
        (expect (org-kit::babel-results-result-payload-node no-payload-marker)
                :to-be nil)
        (expect (org-kit::babel-results-result-payload-node blank-marker)
                :to-be nil)
        (expect (org-kit::babel-results-node-end-before-post-blank
                 empty-document empty-node)
                :to-be 0)
        (expect (org-kit::babel-results-node-end-before-post-blank
                 lf-document lf-node)
                :to-be 1)
        (expect (org-kit::babel-results-node-end-before-post-blank
                 crlf-document crlf-node)
                :to-be 1)
        (expect (org-kit::babel-results-node-end-before-post-blank
                 space-document space-node)
                :to-be 1)
        (expect (org-kit::babel-results-trim-final-newlines
                 (concatenate 'string "value" (string #\Return)))
                :to-equal "value")
        (expect (org-kit::babel-results-trim-final-newlines
                 (format nil "value~%~%"))
                :to-equal "value")
        (expect (typep invalid-label 'org-kit:org-babel-error) :to-be t)
        (when (typep invalid-label 'org-kit:org-babel-error)
          (expect (org-kit:org-babel-error-reason invalid-label)
                  :to-be :invalid-result-label))
        (expect insertion :to-equal (format nil "#+RESULTS:~%value~%")))))
  (it "covers vector, code, and newline-only result paths"
    (let* ((source
             (format nil "#+NAME: sample~%#+BEGIN_SRC text~%body~%#+END_SRC"))
           (document (parse-org source))
           (block (babel-results-test-source-block document))
           (newline-document (parse-org (string #\Newline)))
           (newline-node (org-kit:make-node :paragraph :end 1 :post-blank 1))
           (code
             (org-kit::babel-results-render
              document block "body" "value" "code" "text" 0))
           (raw
             (org-kit::babel-results-render
              document block "body" "value" "raw" "text" 0)))
      (with-soft-assertions
        (expect (org-kit::babel-results-sequence #(1 2)) :to-equal '(1 2))
        (expect code :to-equal
                (format nil "#+BEGIN_SRC text~%body~%#+END_SRC"))
        (expect raw :to-equal "body")
        (expect (org-kit::babel-results-node-end-before-post-blank
                 newline-document newline-node)
                :to-be 0))))
  (it "rejects scalar table values and unsupported render formats"
    (let* ((source
             (format nil "#+NAME: sample~%#+BEGIN_SRC text~%body~%#+END_SRC"))
           (document (parse-org source))
           (block (babel-results-test-source-block document))
           (sequence-error
             (babel-results-test-condition
              (lambda () (org-kit::babel-results-sequence 42))))
           (format-error
             (babel-results-test-condition
              (lambda ()
                (org-kit::babel-results-render
                 document block "body" "value" "unknown" "text" 0)))))
      (with-soft-assertions
        (expect (typep sequence-error 'org-kit:org-babel-error) :to-be t)
        (when (typep sequence-error 'org-kit:org-babel-error)
          (expect (org-kit:org-babel-error-reason sequence-error)
                  :to-be :unsupported-result-format))
        (expect (typep format-error 'org-kit:org-babel-error) :to-be t)
        (when (typep format-error 'org-kit:org-babel-error)
          (expect (org-kit:org-babel-error-reason format-error)
                  :to-be :unsupported-result-format))))))

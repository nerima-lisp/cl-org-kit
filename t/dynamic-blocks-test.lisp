(in-package #:cl-org-kit/test)

(defun dynamic-block-test-node (document)
  (let ((found nil))
    (map-nodes (lambda (node)
                 (when (and (null found) (eq (node-type node) :dynamic-block))
                   (setf found node)))
               (document-root document))
    found))

(defun dynamic-block-test-capture-condition (thunk)
  (handler-case
      (progn (funcall thunk) nil)
    (condition (condition) condition)))

(describe "dynamic block updates"
  (it "reports empty parser atoms and values at end of input"
    (multiple-value-bind (value next valid-p)
        (org-kit::dynamic-block-parse-atom-value ")" 0)
      (expect value :to-be nil)
      (expect next :to-be 0)
      (expect valid-p :to-be nil))
    (multiple-value-bind (value next valid-p)
        (org-kit::dynamic-block-parse-parameter-value "  " 0)
      (expect value :to-be nil)
      (expect next :to-be 2)
      (expect valid-p :to-be nil)))

  (it "dispatches parsed parameters and passes the supplied resolver and time context"
    (let* ((source (format nil
                           "#+BEGIN: custom-body :label \"two words\" :count 2~%old~%#+END:~%"))
           (document (parse-org source))
           (block (dynamic-block-test-node document))
           (source-octets (write-org document :output-type :octets))
           (resolver (lambda (selector) selector))
           (time-context
             (make-time-context :reference-time (make-org-time-value "2026-10-12")
                                :zone (make-fixed-time-zone 0)))
           (received nil))
      (org-kit::register-dynamic-block-handler
       "custom-body"
       (lambda (handler-document handler-block parameters
                &key document-resolver time-context)
         (declare (ignore handler-document handler-block))
         (setf received (list parameters document-resolver time-context))
         "fresh body"
         )
       :parameters '(:label :count))
      (let* ((edit (org-kit::update-dynamic-block
                    document block :document-resolver resolver
                    :time-context time-context))
             (updated (edit-result-updated-document edit)))
        (with-soft-assertions
          (expect received :to-equal
                  (list '(:label "two words" :count 2) resolver time-context))
          (expect (document-text updated)
                  :to-equal
                  (format nil
                          "#+BEGIN: custom-body :label \"two words\" :count 2~%fresh body~%#+END:~%"))
          (expect (document-text document) :to-equal source)
          (expect (write-org document :output-type :octets)
                  :to-equalp source-octets)))))

  (it "renders handler row data as an aligned table and patches only the body"
    (let* ((source (format nil "#+BEGIN: custom-rows~%stale~%#+END:~%"))
           (document (parse-org source))
           (block (dynamic-block-test-node document))
           (source-octets (write-org document :output-type :octets)))
      (org-kit::register-dynamic-block-handler
       "custom-rows"
       (lambda (handler-document handler-block parameters
                &key document-resolver time-context)
         (declare (ignore handler-document handler-block parameters
                          document-resolver time-context))
         '(("Key" "Value") ("A" "B"))))
      (let* ((edit (org-kit::update-dynamic-block document block))
             (updated (edit-result-updated-document edit)))
        (with-soft-assertions
          (expect (document-text updated)
                  :to-equal
                  (format nil
                          "#+BEGIN: custom-rows~%| Key | Value |~%|-----+-------|~%| A   | B     |~%#+END:~%"))
          (expect (length (edit-result-spans edit)) :to-be 1)
          (expect (document-text document) :to-equal source)
          (expect (write-org document :output-type :octets)
                  :to-equalp source-octets)))))

  (it "clears the body when a handler returns an empty row list"
    (let* ((source (format nil "#+BEGIN: custom-empty-rows~%stale~%#+END:~%"))
           (document (parse-org source))
           (block (dynamic-block-test-node document)))
      (org-kit:register-dynamic-block-handler
       "custom-empty-rows"
       (lambda (handler-document handler-block parameters
                &key document-resolver time-context)
         (declare (ignore handler-document handler-block parameters
                          document-resolver time-context))
         nil))
      (let ((updated
              (edit-result-updated-document
               (org-kit:update-dynamic-block document block))))
        (expect (document-text updated)
                :to-equal
                (format nil "#+BEGIN: custom-empty-rows~%#+END:~%")))))

  (it "preserves CRLF and adds a final line break to handler text"
    (let* ((newline (format nil "~C~C" #\Return #\Newline))
           (source (format nil "#+BEGIN: custom-crlf~Aold~A#+END:~A"
                           newline newline newline))
           (document (parse-org source))
           (block (dynamic-block-test-node document))
           (source-octets (write-org document :output-type :octets)))
      (org-kit::register-dynamic-block-handler
       "custom-crlf"
       (lambda (handler-document handler-block parameters
                &key document-resolver time-context)
         (declare (ignore handler-document handler-block parameters
                          document-resolver time-context))
         "fresh body"))
      (let* ((edit (org-kit::update-dynamic-block document block))
             (updated (edit-result-updated-document edit)))
        (with-soft-assertions
          (expect (document-text updated)
                  :to-equal
                  (format nil "#+BEGIN: custom-crlf~Afresh body~A#+END:~A"
                          newline newline newline))
          (expect (length (edit-result-spans edit)) :to-be 1)
          (expect (document-text document) :to-equal source)
          (expect (write-org document :output-type :octets)
                  :to-equalp source-octets)))))

  (it "normalizes embedded CRLF while preserving a bare return"
    (let* ((source (format nil "#+BEGIN: custom-mixed-endings~%old~%#+END:~%"))
           (document (parse-org source))
           (block (dynamic-block-test-node document))
           (body (format nil "first~C~Csecond~Cthird" #\Return #\Newline #\Return)))
      (org-kit:register-dynamic-block-handler
       "custom-mixed-endings"
       (lambda (handler-document handler-block parameters
                &key document-resolver time-context)
         (declare (ignore handler-document handler-block parameters
                          document-resolver time-context))
         body))
      (let ((updated
              (edit-result-updated-document
               (org-kit:update-dynamic-block document block))))
        (expect (document-text updated)
                :to-equal
                (format nil
                        "#+BEGIN: custom-mixed-endings~%first~%second~Cthird~%#+END:~%"
                        #\Return)))))

  (it "returns an edit result produced by the registered handler"
    (let* ((source (format nil "#+BEGIN: custom-edit~%old~%#+END:~%"))
           (document (parse-org source))
           (block (dynamic-block-test-node document)))
      (org-kit::register-dynamic-block-handler
       "custom-edit"
       (lambda (handler-document handler-block parameters
                &key document-resolver time-context)
         (declare (ignore parameters document-resolver time-context))
         (org-kit::dynamic-block-update-body
          handler-document handler-block "fresh")))
      (let* ((edit (org-kit::update-dynamic-block document block))
             (updated (edit-result-updated-document edit)))
        (expect (document-text updated)
                :to-equal
                (format nil "#+BEGIN: custom-edit~%fresh~%#+END:~%"))
        (expect (length (edit-result-spans edit)) :to-be 1)
        (expect (document-text document) :to-equal source))))

  (it "reports the exact unregistered dynamic block reason and name"
    (let* ((source (format nil "#+BEGIN: no-such-handler~%old~%#+END:~%"))
           (document (parse-org source))
           (block (dynamic-block-test-node document))
           (source-octets (write-org document :output-type :octets))
           (condition
             (dynamic-block-test-capture-condition
              (lambda () (org-kit::update-dynamic-block document block)))))
      (with-soft-assertions
        (expect (typep condition 'org-kit::org-dynamic-block-error) :to-be t)
        (when (typep condition 'org-kit::org-dynamic-block-error)
          (expect (org-kit::org-kit-error-message condition)
                  :to-equal "No handler is registered for this dynamic block")
          (expect (org-kit::org-dynamic-block-error-reason condition)
                  :to-be :unregistered-name)
          (expect (org-kit::org-dynamic-block-error-name condition)
                  :to-equal "no-such-handler"))
        (expect (document-text document) :to-equal source)
        (expect (write-org document :output-type :octets)
                :to-equalp source-octets))))

  (it "reports malformed generic block parameters without changing the document"
    (let* ((source (format nil "#+BEGIN: custom-body :label \"unterminated~%old~%#+END:~%"))
           (document (parse-org source))
           (block (dynamic-block-test-node document))
           (source-octets (write-org document :output-type :octets)))
      (org-kit::register-dynamic-block-handler
       "custom-body"
       (lambda (handler-document handler-block parameters
                &key document-resolver time-context)
         (declare (ignore handler-document handler-block parameters
                          document-resolver time-context))
         "unused"))
      (let ((condition
              (dynamic-block-test-capture-condition
               (lambda () (org-kit::update-dynamic-block document block)))))
        (with-soft-assertions
          (expect (typep condition 'org-kit::org-dynamic-block-error) :to-be t)
          (when (typep condition 'org-kit::org-dynamic-block-error)
            (expect (org-kit::org-dynamic-block-error-reason condition)
                    :to-be :invalid-parameters))
          (expect (document-text document) :to-equal source)
          (expect (write-org document :output-type :octets)
                  :to-equalp source-octets)))))

  (it "rejects undeclared parameter names without interning input symbols"
    (let* ((name "dynamic-block-ephemeral-parameter-947")
           (source (format nil "#+BEGIN: custom-empty :~A value~%old~%#+END:~%"
                           name))
           (document (parse-org source))
           (block (dynamic-block-test-node document)))
      (org-kit::register-dynamic-block-handler
       "custom-empty"
       (lambda (handler-document handler-block parameters
                &key document-resolver time-context)
         (declare (ignore handler-document handler-block parameters
                          document-resolver time-context))
         "unused"))
      (with-soft-assertions
        (expect (find-symbol (string-upcase name) :keyword) :to-be nil)
        (let ((condition
                (dynamic-block-test-capture-condition
                 (lambda () (org-kit::update-dynamic-block document block)))))
          (expect (typep condition 'org-kit::org-dynamic-block-error) :to-be t)
          (when (typep condition 'org-kit::org-dynamic-block-error)
            (expect (org-kit::org-dynamic-block-error-reason condition)
                    :to-be :unsupported-parameter)
            (expect (org-kit::org-dynamic-block-error-name condition)
                    :to-equal name))
          (expect (find-symbol (string-upcase name) :keyword) :to-be nil)
          (expect (document-text document) :to-equal source))))))

(describe "dynamic block handler contracts"
  (it "parses escaped strings, atoms, nested lists, tabs, and keyword names"
    (let* ((source
             (format nil
                     "#+BEGIN: parsed-values :text~C\"say \\\\ end\" :items~C(nil t -2 7 atom) :value_1 yes~%old~%#+END:~%"
                     #\Tab #\Tab))
           (document (parse-org source))
           (block (dynamic-block-test-node document))
           (received nil))
      (org-kit::register-dynamic-block-handler
       'parsed-values
       (lambda (handler-document handler-block parameters
                &key document-resolver time-context)
         (declare (ignore handler-document handler-block
                          document-resolver time-context))
         (setf received parameters)
         "parsed")
       :parameters '(:text :items :value_1))
      (let ((edit (org-kit:update-dynamic-block document block)))
        (with-soft-assertions
          (expect received
                  :to-equal '(:text "say \\ end"
                              :items (nil t -2 7 "atom")
                              :value_1 "yes"))
          (expect (document-text (edit-result-updated-document edit))
                  :to-equal
                  (format nil
                          "#+BEGIN: parsed-values :text~C\"say \\\\ end\" :items~C(nil t -2 7 atom) :value_1 yes~%parsed~%#+END:~%"
                          #\Tab #\Tab))))))

  (it "rejects invalid handler registrations"
    (let* ((function
             (lambda (document block parameters
                      &key document-resolver time-context)
               (declare (ignore document block parameters
                                document-resolver time-context))
               "unused"))
           (empty-name
             (dynamic-block-test-capture-condition
              (lambda ()
                (org-kit:register-dynamic-block-handler "" function))))
           (non-function
             (dynamic-block-test-capture-condition
              (lambda ()
                (org-kit:register-dynamic-block-handler "bad-function" 7))))
           (improper-parameters
             (dynamic-block-test-capture-condition
              (lambda ()
                (org-kit:register-dynamic-block-handler
                 "bad-parameters" function :parameters '(:known . :tail)))))
           (non-keyword-parameters
             (dynamic-block-test-capture-condition
              (lambda ()
                (org-kit:register-dynamic-block-handler
                 "bad-keyword" function :parameters '(known))))))
      (with-soft-assertions
        (expect (typep empty-name 'type-error) :to-be t)
        (expect (typep non-function 'type-error) :to-be t)
        (expect (typep improper-parameters 'type-error) :to-be t)
        (expect (typep non-keyword-parameters 'type-error) :to-be t))))

  (it "formats named and bare dynamic-block conditions"
    (let ((named
            (make-condition 'org-kit::org-dynamic-block-error
                            :message "No handler is registered for this dynamic block"
                            :reason :unregistered-name
                            :name "missing"))
          (bare
            (make-condition 'org-kit::org-dynamic-block-error
                            :message "Malformed dynamic block parameters")))
      (with-soft-assertions
        (expect (princ-to-string named)
                :to-equal
                "No handler is registered for this dynamic block (unregistered-name) for missing")
        (expect (princ-to-string bare)
                :to-equal "Malformed dynamic block parameters"))))

  (it "rejects invalid resolver, time-context, and handler result values"
    (let* ((source (format nil "#+BEGIN: custom-invalid-result~%old~%#+END:~%"))
           (document (parse-org source))
           (block (dynamic-block-test-node document))
           (source-octets (write-org document :output-type :octets)))
      (org-kit:register-dynamic-block-handler
       "custom-invalid-result"
       (lambda (handler-document handler-block parameters
                &key document-resolver time-context)
         (declare (ignore handler-document handler-block parameters
                          document-resolver time-context))
         17))
      (let ((bad-resolver
              (dynamic-block-test-capture-condition
               (lambda ()
                 (org-kit:update-dynamic-block
                  document block :document-resolver 17))))
            (bad-time-context
              (dynamic-block-test-capture-condition
               (lambda ()
                 (org-kit:update-dynamic-block
                  document block :time-context 17))))
            (bad-result
              (dynamic-block-test-capture-condition
               (lambda () (org-kit:update-dynamic-block document block)))))
        (with-soft-assertions
          (expect (typep bad-resolver 'type-error) :to-be t)
          (expect (type-error-datum bad-resolver) :to-be 17)
          (expect (typep bad-time-context 'type-error) :to-be t)
          (expect (type-error-datum bad-time-context) :to-be 17)
          (expect (typep bad-result 'type-error) :to-be t)
          (expect (type-error-datum bad-result) :to-be 17)
          (expect (document-text document) :to-equal source)
          (expect (write-org document :output-type :octets)
                  :to-equalp source-octets)))))

  (it "rejects malformed parameter names and values through the update API"
    (dolist (arguments (list "plain-text"
                             ":"
                             ": value 1"
                             ":value )"
                             ":value (unterminated"
                             ":value (\"unterminated"
                             ":value \"trailing\\"))
      (let* ((source (format nil "#+BEGIN: custom-malformed ~A~%old~%#+END:~%"
                             arguments))
             (document (parse-org source))
             (block (dynamic-block-test-node document))
             (source-octets (write-org document :output-type :octets)))
        (org-kit:register-dynamic-block-handler
         "custom-malformed"
         (lambda (handler-document handler-block parameters
                  &key document-resolver time-context)
           (declare (ignore handler-document handler-block parameters
                            document-resolver time-context))
           "unused")
         :parameters '(:value))
        (let ((condition
                (dynamic-block-test-capture-condition
                 (lambda () (org-kit:update-dynamic-block document block)))))
          (with-soft-assertions
            (expect (typep condition 'org-kit::org-dynamic-block-error) :to-be t)
            (when (typep condition 'org-kit::org-dynamic-block-error)
              (expect (org-kit::org-dynamic-block-error-reason condition)
                      :to-be :invalid-parameters))
            (expect (document-text document) :to-equal source)
            (expect (write-org document :output-type :octets)
                    :to-equalp source-octets))))))

  (it "reports missing and duplicate parameter names and values exactly"
    (dolist (case '((":label" :missing-parameter-value "label" nil)
                    (":label :label second"
                     :missing-parameter-value "label" nil)
                    (":label first :label second"
                     :duplicate-parameter "label" "second")))
      (destructuring-bind (arguments reason name value) case
        (org-kit:register-dynamic-block-handler
         "custom-parameter-error"
         (lambda (handler-document handler-block parameters
                  &key document-resolver time-context)
           (declare (ignore handler-document handler-block parameters
                            document-resolver time-context))
           "unused")
         :parameters '(:label))
        (let* ((source
                 (format nil "#+BEGIN: custom-parameter-error ~A~%old~%#+END:~%"
                         arguments))
               (document (parse-org source))
               (block (dynamic-block-test-node document))
               (condition
                 (dynamic-block-test-capture-condition
                  (lambda () (org-kit:update-dynamic-block document block)))))
          (with-soft-assertions
            (expect (typep condition 'org-kit::org-dynamic-block-error)
                    :to-be t)
            (when (typep condition 'org-kit::org-dynamic-block-error)
              (expect (org-kit::org-dynamic-block-error-reason condition)
                      :to-be reason)
              (expect (org-kit::org-dynamic-block-error-name condition)
                      :to-equal name)
              (expect (org-kit::dynamic-block-error-value condition)
                      :to-equal value))
            (expect (document-text document) :to-equal source))))))

  (it "rejects lists with non-string row cells"
    (let* ((source (format nil "#+BEGIN: custom-invalid-rows~%old~%#+END:~%"))
           (document (parse-org source))
           (block (dynamic-block-test-node document)))
      (org-kit:register-dynamic-block-handler
       "custom-invalid-rows"
       (lambda (handler-document handler-block parameters
                &key document-resolver time-context)
         (declare (ignore handler-document handler-block parameters
                          document-resolver time-context))
         '(("Header" 7))))
      (let ((condition
              (dynamic-block-test-capture-condition
               (lambda () (org-kit:update-dynamic-block document block)))))
        (expect (typep condition 'type-error) :to-be t)
        (expect (type-error-datum condition) :to-equal '(("Header" 7))))))

  (it "keeps a top-level headline in a generated dynamic block body"
    (let* ((source (format nil "#+BEGIN: custom-headline~%old~%#+END:~%"))
           (document (parse-org source))
           (block (dynamic-block-test-node document)))
      (org-kit:register-dynamic-block-handler
       "custom-headline"
       (lambda (handler-document handler-block parameters
                &key document-resolver time-context)
         (declare (ignore handler-document handler-block parameters
                          document-resolver time-context))
         "* Generated"))
      (let ((updated
              (edit-result-updated-document
               (org-kit:update-dynamic-block document block))))
        (expect (document-text updated)
                :to-equal
                (format nil "#+BEGIN: custom-headline~%* Generated~%#+END:~%")))))

  (it "escapes table delimiters in handler row data"
    (let* ((source (format nil "#+BEGIN: custom-escaped-rows~%old~%#+END:~%"))
           (document (parse-org source))
           (block (dynamic-block-test-node document)))
      (org-kit:register-dynamic-block-handler
       "custom-escaped-rows"
       (lambda (handler-document handler-block parameters
                &key document-resolver time-context)
         (declare (ignore handler-document handler-block parameters
                          document-resolver time-context))
         '(("Key|Part" "Value") ("A\\|B" "C"))))
      (let ((updated
              (edit-result-updated-document
               (org-kit:update-dynamic-block document block))))
        (with-soft-assertions
          (expect (search "Key\\|Part" (document-text updated))
                  :to-be-truthy)
          (expect (search "A\\|B" (document-text updated))
                  :to-be-truthy)))))
)

(describe "dynamic block parser boundary cases"
  (it "normalizes symbol names and rejects non-symbol parameter declarations"
    (expect (org-kit::dynamic-block-normalize-name 'Sample-Block)
            :to-equal "sample-block")
    (expect (typep
             (dynamic-block-test-capture-condition
              (lambda () (org-kit::dynamic-block-normalize-name 42)))
             'type-error)
            :to-be t)
    (let ((condition
            (dynamic-block-test-capture-condition
             (lambda ()
               (org-kit:register-dynamic-block-handler
                "invalid-parameter-type"
                (lambda (document block parameters
                         &key document-resolver time-context)
                  (declare (ignore document block parameters
                                   document-resolver time-context))
                  "unused")
                :parameters '("label"))))))
      (expect (typep condition 'type-error) :to-be t)))

  (it "reports clocktable parameter errors with optional fields"
    (let ((bare (make-condition 'org-kit::org-clocktable-parameter-error
                                :message "Invalid"))
          (detailed
            (make-condition 'org-kit::org-clocktable-parameter-error
                            :message "Invalid" :reason :invalid-value
                            :parameter :tstart :value "nonsense")))
      (with-soft-assertions
        (expect (princ-to-string bare) :to-equal "Invalid")
        (expect (princ-to-string detailed)
                :to-equal "Invalid (invalid-value) for :TSTART with nonsense"))))

  (it "keeps negative-looking atoms and rejects non-list row data"
    (multiple-value-bind (value next valid-p)
        (org-kit::dynamic-block-parse-atom-value "-x" 0)
      (expect (list value next valid-p) :to-equal '("-x" 2 t)))
    (expect (org-kit::dynamic-block-row-data-p '("Header")) :to-be nil))

  (it "preserves bare returns while normalizing CRLF bodies"
    (let ((bare-return (format nil "a~Cb" #\Return))
          (crlf-body (format nil "a~C~Cb" #\Return #\Newline))
          (ending-return (string #\Return)))
      (expect (org-kit::dynamic-block-normalized-body-length
               bare-return (string #\Newline))
              :to-be 4)
      (expect (org-kit::dynamic-block-normalized-body-length
               crlf-body (string #\Newline))
              :to-be 4)
      (expect (org-kit::dynamic-block-normalized-body-length
               ending-return (string #\Newline))
              :to-be 2)
      (expect (org-kit::dynamic-block-normalize-body
               bare-return (string #\Newline))
              :to-equal (format nil "a~Cb~%" #\Return))
      (expect (org-kit::dynamic-block-normalize-body
               ending-return (string #\Newline))
              :to-equal (format nil "~C~%" #\Return)))))

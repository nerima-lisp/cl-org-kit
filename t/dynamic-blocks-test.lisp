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

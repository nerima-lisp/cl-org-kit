;;;; t/crypt-test.lisp
(in-package #:cl-org-kit/test)

(defun crypt-test-headline (document title)
  (let ((headlines '()))
    (map-nodes (lambda (node)
                 (when (eq (node-type node) :headline)
                   (push node headlines)))
               (document-root document))
    (find title (nreverse headlines)
          :key (lambda (node)
                 (string-right-trim '(#\Return)
                                    (node-property node :raw-value)))
          :test #'string=)))

(defun crypt-test-register-armor-encryptor (registry)
  (register-callback
   registry :crypt-encrypt "org-crypt"
   (lambda (text)
     (format nil "-----BEGIN PGP MESSAGE-----~%~A~%-----END PGP MESSAGE-----"
             text))))

(defun crypt-test-armor (plaintext)
  (format nil "-----BEGIN PGP MESSAGE-----~%~A~%-----END PGP MESSAGE-----"
          plaintext))

(describe "Org crypt transformations"
  (it "round-trips deterministic armor and treats encrypted entries as no-ops"
    (let* ((source (format nil "* Secret~%plain secret~%~%* Peer~%keep~%"))
           (document (parse-org source))
           (heading (crypt-test-headline document "Secret"))
           (registry (make-callback-registry))
           (encrypt-calls 0)
           (decrypt-calls 0)
           (received-plaintext nil)
           (received-armor nil))
      (register-callback
       registry :crypt-encrypt "org-crypt"
       (lambda (text)
         (incf encrypt-calls)
         (setf received-plaintext text)
         (crypt-test-armor text)))
      (register-callback
       registry :crypt-decrypt "org-crypt"
       (lambda (armor)
         (incf decrypt-calls)
         (setf received-armor armor)
         "plain secret"))
      (let* ((encrypted-result (crypt-encrypt-entry document heading registry))
             (encrypted (edit-result-updated-document encrypted-result))
             (reencrypt-result
               (crypt-encrypt-entry encrypted
                                    (crypt-test-headline encrypted "Secret")
                                    registry))
             (decrypted-result
               (crypt-decrypt-entry encrypted
                                    (crypt-test-headline encrypted "Secret")
                                    registry)))
        (with-soft-assertions
          (expect (edit-result-p encrypted-result) :to-be t)
          (expect received-plaintext :to-equal "plain secret")
          (expect (edit-result-spans encrypted-result) :to-have-length 1)
          (expect (edit-result-spans reencrypt-result) :to-equal nil)
          (expect (document-text (edit-result-updated-document reencrypt-result))
                  :to-equal (document-text encrypted))
          (expect (document-text (edit-result-updated-document decrypted-result))
                  :to-equal source)
          (expect received-armor :to-equal (crypt-test-armor "plain secret"))
          (expect encrypt-calls :to-be 1)
          (expect decrypt-calls :to-be 1)
          (expect (document-text document) :to-equal source)))))

  (it "skips Org leading metadata and includes body drawers through the subtree end"
    (let* ((source
             (format nil
                     "* Entry~%SCHEDULED: <2026-10-20 Tue>~%:PROPERTIES:~%:ID: x~%:END:~%~%CLOCK: [2026-10-20 Tue 10:00]~%:LOGBOOK:~%CLOCK: [2026-10-20 Tue 10:00]--[2026-10-20 Tue 10:10] => 0:10~%:END:~%~%body~%:NOTE:~%drawer payload~%:END:~%last body line~%~%* Peer~%peer body~%"))
           (document (parse-org source))
           (heading (crypt-test-headline document "Entry"))
           (registry (make-callback-registry))
           (received nil))
      (register-callback registry :crypt-encrypt "org-crypt"
                         (lambda (text)
                           (setf received text)
                           (crypt-test-armor "ciphertext")))
      (let* ((result (crypt-encrypt-entry document heading registry))
             (output (document-text (edit-result-updated-document result)))
             (expected-payload
               (format nil "body~%:NOTE:~%drawer payload~%:END:~%last body line")))
        (with-soft-assertions
          (expect received :to-equal expected-payload)
          (expect (search "SCHEDULED:" output) :to-equal (search "SCHEDULED:" source))
          (expect (search ":PROPERTIES:" output) :to-equal
                  (search ":PROPERTIES:" source))
          (expect (search ":LOGBOOK:" output) :to-equal (search ":LOGBOOK:" source))
          (expect (search (format nil "~%~%* Peer~%peer body~%") output)
                  :to-be-truthy)
          (expect (search "ciphertext" output) :to-be-truthy)
          (expect (document-text document) :to-equal source)))))

  (it "treats a property drawer after a blank line as body text"
    (let* ((source
             (format nil
                     "* Entry~%SCHEDULED: <2026-10-20 Tue>~%~%:PROPERTIES:~%:ID: x~%:END:~%secret body~%"))
           (document (parse-org source))
           (heading (crypt-test-headline document "Entry"))
           (registry (make-callback-registry))
           (received nil))
      (register-callback registry :crypt-encrypt "org-crypt"
                         (lambda (text)
                           (setf received text)
                           (crypt-test-armor text)))
      (let* ((result (crypt-encrypt-entry document heading registry))
             (output (document-text (edit-result-updated-document result)))
             (expected-payload
               (format nil ":PROPERTIES:~%:ID: x~%:END:~%secret body")))
        (with-soft-assertions
          (expect received :to-equal expected-payload)
          (expect output
                  :to-equal
                  (concatenate 'string
                               (format nil "* Entry~%SCHEDULED: <2026-10-20 Tue>~%~%")
                               (crypt-test-armor expected-payload)))
          (expect (document-text document) :to-equal source)))))

  (it "skips only the planning line immediately after the heading"
    (let* ((source
             (format nil
                     "* Entry~%SCHEDULED: <2026-10-20 Tue>~%DEADLINE: <2026-10-21 Wed>~%secret body~%"))
           (document (parse-org source))
           (heading (crypt-test-headline document "Entry"))
           (registry (make-callback-registry))
           (received nil))
      (register-callback registry :crypt-encrypt "org-crypt"
                         (lambda (text)
                           (setf received text)
                           (crypt-test-armor text)))
      (let* ((result (crypt-encrypt-entry document heading registry))
             (output (document-text (edit-result-updated-document result)))
             (expected-payload
               (format nil "DEADLINE: <2026-10-21 Wed>~%secret body")))
        (with-soft-assertions
          (expect received :to-equal expected-payload)
          (expect output
                  :to-equal
                  (concatenate 'string
                               (format nil "* Entry~%SCHEDULED: <2026-10-20 Tue>~%")
                               (crypt-test-armor expected-payload)))
          (expect (document-text document) :to-equal source)))))

  (it "does not re-encrypt a payload with an exact complete armor block"
    (let* ((source (format nil "* Entry~%  ~A~%  ciphertext~%  ~A~%after~%"
                           "-----BEGIN PGP MESSAGE-----"
                           "-----END PGP MESSAGE-----"))
           (document (parse-org source))
           (registry (make-callback-registry))
           (calls 0))
      (register-callback registry :crypt-encrypt "org-crypt"
                         (lambda (text) (declare (ignore text)) (incf calls) "bad"))
      (let ((result (crypt-encrypt-entry
                     document (crypt-test-headline document "Entry") registry)))
        (with-soft-assertions
          (expect (edit-result-spans result) :to-equal nil)
          (expect (document-text (edit-result-updated-document result))
                  :to-equal source)
          (expect calls :to-be 0)))))

  (it "removes armor indentation and preserves text after the footer"
    (let* ((source (format nil "* Entry~%  ~A~%  encrypted bytes~%  ~A~%after footer~%~%* Peer~%safe~%"
                           "-----BEGIN PGP MESSAGE-----"
                           "-----END PGP MESSAGE-----"))
           (document (parse-org source))
           (registry (make-callback-registry))
           (received nil))
      (register-callback registry :crypt-decrypt "org-crypt"
                         (lambda (armor) (setf received armor) "plain text"))
      (let* ((result (crypt-decrypt-entry
                      document (crypt-test-headline document "Entry") registry))
             (output (document-text (edit-result-updated-document result))))
        (with-soft-assertions
          (expect received
                  :to-equal (crypt-test-armor "encrypted bytes"))
          (expect output
                  :to-equal (format nil "* Entry~%plain text~%after footer~%~%* Peer~%safe~%"))
          (expect (document-text document) :to-equal source)))))

  (it "shifts plaintext headings deeper than the encrypted entry"
    (let* ((source (format nil "* Entry~%~A~%" (crypt-test-armor "ciphertext")))
           (document (parse-org source))
           (registry (make-callback-registry)))
      (register-callback registry :crypt-decrypt "org-crypt"
                         (lambda (armor)
                           (declare (ignore armor))
                           (format nil "* Top~%text~%** Child~%inside")))
      (let* ((result (crypt-decrypt-entry
                      document (crypt-test-headline document "Entry") registry))
             (updated (edit-result-updated-document result))
             (output (document-text updated)))
        (with-soft-assertions
          (expect (search (format nil "** Top~%text~%*** Child~%inside") output)
                  :to-be-truthy)
          (expect (node-property (crypt-test-headline updated "Top") :level)
                  :to-be 2)
          (expect (node-property (crypt-test-headline updated "Child") :level)
                  :to-be 3)))))

  (it "wraps missing callbacks and callback failures without changing the input"
    (let* ((source (format nil "* Entry~%secret~%"))
           (document (parse-org source))
           (heading (crypt-test-headline document "Entry"))
           (missing
             (handler-case
                 (progn (crypt-encrypt-entry document heading
                                             (make-callback-registry))
                        nil)
               (org-crypt-error (condition) condition)))
           (registry (make-callback-registry))
           (failure (make-condition 'simple-error
                                    :format-control "stub failed"))
           (caught nil))
      (register-callback registry :crypt-encrypt "org-crypt"
                         (lambda (text) (declare (ignore text)) (error failure)))
      (setf caught
            (handler-case
                (progn (crypt-encrypt-entry document heading registry) nil)
              (org-crypt-error (condition) condition)))
      (with-soft-assertions
        (expect (typep missing 'org-crypt-error) :to-be t)
        (when missing
          (expect (org-crypt-error-reason missing) :to-be :missing-callback)
          (expect (typep (org-crypt-error-cause missing) 'org-callback-error)
                  :to-be t))
        (expect (typep caught 'org-crypt-error) :to-be t)
        (when caught
          (expect (org-crypt-error-reason caught) :to-be :callback-failed)
          (expect (eq (org-crypt-error-cause caught) failure) :to-be t))
        (expect (document-text document) :to-equal source))))

  (it "uses the default crypt tag for bulk encryption"
    (let* ((source (format nil "* Entry :crypt:~%secret~%"))
           (document (parse-org source))
           (registry (make-callback-registry)))
      (crypt-test-register-armor-encryptor registry)
      (let* ((result (crypt-encrypt-entries document registry))
             (output (document-text (edit-result-updated-document result))))
        (with-soft-assertions
          (expect (edit-result-spans result) :to-have-length 1)
          (expect (search (crypt-test-armor "secret") output) :to-be-truthy)
          (expect (document-text document) :to-equal source)))))

  (it "applies a caller-selected tag in bulk and leaves unselected entries intact"
    (let* ((source (format nil "* One :private:~%secret~%~%* Two :crypt:~%public~%"))
           (document (parse-org source))
           (registry (make-callback-registry))
           (received '()))
      (register-callback registry :crypt-encrypt "org-crypt"
                         (lambda (text)
                           (push text received)
                           (crypt-test-armor text)))
      (let* ((result (crypt-encrypt-entries document registry :tag "private"))
             (output (document-text (edit-result-updated-document result))))
        (with-soft-assertions
          (expect received :to-equal '("secret"))
          (expect (edit-result-spans result) :to-have-length 1)
          (expect (search (crypt-test-armor "secret") output) :to-be-truthy)
          (expect (search (format nil "* Two :crypt:~%public~%") output)
                  :to-be-truthy)
          (expect (document-text document) :to-equal source)))))

  (it "uses inherited tags when bulk-decrypting a tagged subtree"
    (let* ((source (format nil "* Parent :vault:~%** Child~%~A~%* Peer~%safe~%"
                           (crypt-test-armor "ciphertext")))
           (document (parse-org source))
           (registry (make-callback-registry)))
      (register-callback registry :crypt-decrypt "org-crypt"
                         (lambda (armor)
                           (declare (ignore armor))
                           "decrypted"))
      (let* ((result (crypt-decrypt-entries document registry :tag "vault"))
             (output (document-text (edit-result-updated-document result))))
        (with-soft-assertions
          (expect (search (format nil "** Child~%decrypted") output) :to-be-truthy)
          (expect (search (format nil "* Peer~%safe~%") output) :to-be-truthy)
          (expect (document-text document) :to-equal source)))))

  (it "returns no partial bulk result when a later callback fails"
    (let* ((source (format nil "* First :vault:~%one~%* Second :vault:~%two~%"))
           (document (parse-org source))
           (registry (make-callback-registry))
           (calls 0)
           (caught nil))
      (register-callback registry :crypt-encrypt "org-crypt"
                         (lambda (text)
                           (incf calls)
                           (if (= calls 2)
                               (error "bulk stub failed")
                               (crypt-test-armor text))))
      (setf caught
            (handler-case (progn (crypt-encrypt-entries document registry
                                                        :tag "vault") nil)
              (org-crypt-error (condition) condition)))
      (with-soft-assertions
        (expect (typep caught 'org-crypt-error) :to-be t)
        (when caught
          (expect (org-crypt-error-reason caught) :to-be :callback-failed))
        (expect calls :to-be 2)
        (expect (document-text document) :to-equal source))))

  (it "honors output limits before returning an edit result"
    (let* ((source (format nil "* Entry~%secret~%"))
           (limits (make-limits :max-output-length (length source)))
           (document (parse-org source :limits limits))
           (registry (make-callback-registry))
           (caught nil))
      (register-callback registry :crypt-encrypt "org-crypt"
                         (lambda (text) (crypt-test-armor text)))
      (setf caught
            (handler-case
                (progn (crypt-encrypt-entry
                        document (crypt-test-headline document "Entry") registry)
                       nil)
              (org-resource-limit-error (condition) condition)))
      (with-soft-assertions
        (expect (typep caught 'org-resource-limit-error) :to-be t)
        (when caught
          (expect (org-resource-limit-error-limit-name caught)
                  :to-be :max-output-length))
        (expect (document-text document) :to-equal source)))))

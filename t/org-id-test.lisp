;;;; t/org-id-test.lisp
(in-package #:cl-org-kit/test)

(defun org-id-test-heading (document title)
  (let ((found nil))
    (when (document-root document)
      (map-nodes (lambda (node)
                   (when (and (null found)
                              (eq (node-type node) :headline)
                              (string= (node-property node :raw-value) title))
                     (setf found node)))
                 (document-root document)))
    found))

(defun org-id-test-random-source (&optional (start 0))
  (let ((value start))
    (lambda ()
      (prog1 (mod value 256)
        (incf value)))))

(defun org-id-test-octets (identifier)
  (let ((octets (make-array 16 :element-type '(unsigned-byte 8)))
        (high-nibble nil)
        (index 0))
    (loop for character across identifier
          unless (char= character #\-)
            do (let ((nibble (digit-char-p character 16)))
                 (if high-nibble
                     (progn
                       (setf (aref octets index)
                             (+ (* high-nibble 16) nibble)
                             high-nibble nil)
                       (incf index))
                     (setf high-nibble nibble))))
    octets))

(defun org-id-test-canonical-uuid-p (identifier)
  (and (= (length identifier) 36)
       (loop for index from 0 below 36
             for character = (char identifier index)
             always (if (member index '(8 13 18 23))
                        (char= character #\-)
                        (not (null (find character "0123456789abcdef")))))
       (char= (char identifier 14) #\4)
       (not (null (find (char identifier 19) "89ab")))))

(defun org-id-test-condition (thunk)
  (handler-case
      (progn (funcall thunk) nil)
    (error (condition) condition)))

(defun org-id-test-expect-error (thunk reason)
  (let ((condition (org-id-test-condition thunk)))
    (with-soft-assertions
      (expect (typep condition 'org-id-error) :to-be t)
      (when (typep condition 'org-id-error)
        (expect (org-id-error-reason condition) :to-be reason)
        (expect (plusp (length (org-kit-error-message condition))) :to-be t)))))

(describe "Org IDs"
  (it "generates deterministic UUID v4 octets and sets the version and variant bits"
    (let* ((consumed 0)
           (identifier
             (generate-org-id
              :random-source (lambda ()
                               (prog1 consumed
                                 (incf consumed))))))
      (with-soft-assertions
        (expect identifier :to-equal "00010203-0405-4607-8809-0a0b0c0d0e0f")
        (expect consumed :to-be 16)
        (expect (org-id-test-octets identifier)
                :to-equalp #(0 1 2 3 4 5 70 7 136 9 10 11 12 13 14 15))
        (expect (logand (aref (org-id-test-octets identifier) 6) #xf0)
                :to-be #x40)
        (expect (logand (aref (org-id-test-octets identifier) 8) #xc0)
                :to-be #x80))))

  (it "produces the same UUID from the same fixed random source"
    (with-soft-assertions
      (expect (generate-org-id :random-source (org-id-test-random-source))
              :to-equal
              (generate-org-id :random-source (org-id-test-random-source)))
      (expect (org-id-test-canonical-uuid-p (generate-org-id)) :to-be t)))

  (it "sets a generated ID while preserving the exact surrounding source"
    (let* ((source (format nil "* Alpha~%Body~%* Beta~%tail~%"))
           (document (parse-org source))
           (heading (org-id-test-heading document "Alpha"))
           (identifier "00010203-0405-4607-8809-0a0b0c0d0e0f")
           (expected (format nil "* Alpha~%:PROPERTIES:~%:ID: ~A~%:END:~%Body~%* Beta~%tail~%"
                             identifier)))
      (multiple-value-bind (updated actual-id)
          (set-heading-id document heading
                          :random-source (org-id-test-random-source))
        (with-soft-assertions
          (expect (document-p updated) :to-be t)
          (expect (document-text updated) :to-equal expected)
          (expect actual-id :to-equal identifier)
          (expect (document-text document) :to-equal source)))))

  (it "replaces an existing ID property without changing surrounding source"
    (let* ((source (format nil "* Alpha~%:PROPERTIES:~%:ID: old~%:END:~%Body~%* Beta~%"))
           (document (parse-org source))
           (heading (org-id-test-heading document "Alpha"))
           (identifier "00010203-0405-4607-8809-0a0b0c0d0e0f")
           (expected (format nil "* Alpha~%:PROPERTIES:~%:ID: ~A~%:END:~%Body~%* Beta~%"
                             identifier)))
      (multiple-value-bind (updated actual-id)
          (set-heading-id document heading
                          :random-source (org-id-test-random-source))
        (with-soft-assertions
          (expect (document-text updated) :to-equal expected)
          (expect actual-id :to-equal identifier)
          (expect (document-text document) :to-equal source)))))

  (it "indexes matching headings across documents in document order and keeps duplicates"
    (let* ((first-document
             (parse-org
              (format nil "* First~%:PROPERTIES:~%:ID: shared~%:END:~%* Second~%:PROPERTIES:~%:ID: shared~%:END:~%")))
           (second-document
             (parse-org
              (format nil "* Third~%:PROPERTIES:~%:ID: shared~%:END:~%")))
           (index (make-org-id-index (list first-document second-document)))
           (candidates (org-id-index-candidates index "shared")))
      (with-soft-assertions
        (expect (org-id-index-p index) :to-be t)
        (expect candidates
                :to-equal (list (org-id-test-heading first-document "First")
                                (org-id-test-heading first-document "Second")
                                (org-id-test-heading second-document "Third")))
        (expect (org-id-index-candidates index "missing") :to-equal nil))))

  (it "accepts a single document and returns a fresh candidate list"
    (let* ((document
             (parse-org
              (format nil "* One~%:PROPERTIES:~%:ID: abc~%:END:~%")))
           (index (make-org-id-index document))
           (first-result (org-id-index-candidates index "abc")))
      (setf (car first-result) nil)
      (with-soft-assertions
        (expect (first (org-id-index-candidates index "abc"))
                :to-be (org-id-test-heading document "One")))))

  (it "signals named errors for invalid random sources, octets, and IDs"
    (let* ((document (parse-org (format nil "* Alpha~%")))
           (index (make-org-id-index document)))
      (org-id-test-expect-error
       (lambda () (generate-org-id :random-source :not-a-function))
       :invalid-random-source)
      (org-id-test-expect-error
       (lambda () (generate-org-id :random-source (lambda () 256)))
       :invalid-random-octet)
      (org-id-test-expect-error
       (lambda () (org-id-index-candidates index ""))
       :invalid-id)
      (org-id-test-expect-error
       (lambda () (org-id-index-candidates index 42))
       :invalid-id)))

  (it "signals named errors for invalid index and heading inputs"
    (let* ((document (parse-org (format nil "* Alpha~%")))
           (heading (org-id-test-heading document "Alpha"))
           (other-document (parse-org (format nil "* Other~%"))))
      (org-id-test-expect-error (lambda () (make-org-id-index "not a document"))
                                :invalid-documents)
      (org-id-test-expect-error (lambda () (make-org-id-index (list document "bad")))
                                :invalid-document)
      (org-id-test-expect-error (lambda () (org-id-index-candidates nil "x"))
                                :invalid-index)
      (org-id-test-expect-error (lambda () (set-heading-id "bad" heading))
                                :invalid-document)
      (org-id-test-expect-error (lambda () (set-heading-id document "not a node"))
                                :invalid-heading)
      (org-id-test-expect-error
       (lambda () (set-heading-id other-document heading))
       :foreign-heading)))

  (it "rejects invalid and duplicate local IDs before editing"
    (let* ((invalid-document
             (parse-org
              (format nil "* Invalid~%:PROPERTIES:~%:ID:   ~%:END:~%")))
           (invalid-heading (org-id-test-heading invalid-document "Invalid"))
           (invalid-source (document-text invalid-document))
           (duplicate-document
             (parse-org
              (format nil "* First~%:PROPERTIES:~%:ID: same~%:END:~%* Second~%:PROPERTIES:~%:ID: same~%:END:~%")))
           (duplicate-heading (org-id-test-heading duplicate-document "First"))
           (duplicate-source (document-text duplicate-document)))
      (org-id-test-expect-error
       (lambda () (set-heading-id invalid-document invalid-heading
                                  :random-source (org-id-test-random-source)))
       :invalid-id)
      (org-id-test-expect-error
       (lambda () (set-heading-id duplicate-document duplicate-heading
                                  :random-source (org-id-test-random-source)))
       :duplicate-id)
      (with-soft-assertions
        (expect (document-text invalid-document) :to-equal invalid-source)
        (expect (document-text duplicate-document) :to-equal duplicate-source))))

  (it "rejects a generated ID that collides with another heading in the document"
    (let* ((identifier "00010203-0405-4607-8809-0a0b0c0d0e0f")
           (source
             (format nil "* New~%* Existing~%:PROPERTIES:~%:ID: ~A~%:END:~%" identifier))
           (document (parse-org source))
           (heading (org-id-test-heading document "New")))
      (org-id-test-expect-error
       (lambda () (set-heading-id document heading
                                  :random-source (org-id-test-random-source)))
       :duplicate-id)
      (expect (document-text document) :to-equal source))))

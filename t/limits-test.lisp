;;;; t/limits-test.lisp
(in-package #:cl-org-kit/test)

(defparameter +limit-specifications+
  '((:max-input-length 67108864)
    (:max-heading-depth 256)
    (:max-element-depth 512)
    (:max-search-regex-length 4096)
    (:max-search-work 67108864)
    (:max-object-depth 128)
    (:max-node-count 4194304)
    (:max-output-length 67108864)
    (:max-output-octets 268435456)
    (:max-agenda-range-days 3660)
    (:max-agenda-entries 1000000)
    (:max-future-repeats 10000)
    (:max-clock-consistency-issues 100000)
    (:max-export-expansion-depth 32)
    (:max-export-expanded-characters 67108864)
    (:max-export-expanded-nodes 4194304)
    (:max-formula-tokens 10000)
    (:max-formula-ast-depth 256)
    (:max-formula-precision 34)
    (:max-formula-exponent 6144)
    (:max-formula-work 1000000)
    (:max-write-plan-entries 100000)
    (:max-write-plan-bytes 268435456)
    (:max-write-plan-path-bytes 4096)
    (:max-write-plan-path-depth 128))
  "Expected finite defaults for every resource limit.")

(defun limits-with-sequential-overrides (start)
  (apply #'make-limits
         (loop for specification in +limit-specifications+
               for value from start
               append (list (first specification) value))))

(defvar *limits-test-string-to-octets-calls* nil)

(defun limits-test-call-with-string-to-octets-counter (function)
  (let* ((symbol (find-symbol "STRING-TO-OCTETS" "CL-CODEC-KIT"))
         (original (symbol-function symbol)))
    (let ((*limits-test-string-to-octets-calls* 0))
      (unwind-protect
           (progn
             (setf (symbol-function symbol)
                   (lambda (&rest arguments)
                     (incf *limits-test-string-to-octets-calls*)
                     (apply original arguments)))
             (funcall function (lambda () *limits-test-string-to-octets-calls*)))
        (setf (symbol-function symbol) original)))))

(defun limits-test-call-with-encoding-alias (alias canonical function)
  (let ((cl-codec-kit::*encodings*
          (let ((encodings (make-hash-table :test 'eq)))
            (maphash (lambda (designator encoding)
                       (setf (gethash designator encodings) encoding))
                     cl-codec-kit::*encodings*)
            (setf (gethash alias encodings)
                  (cl-codec-kit:find-character-encoding canonical))
            encodings)))
    (funcall function)))

(describe "resource limits"
  (it "uses the configured defaults and accepts integer overrides"
    (let ((defaults (make-limits))
          (overrides (limits-with-sequential-overrides 101))
          (names (mapcar #'first +limit-specifications+)))
      (with-soft-assertions
        (expect (limits-p defaults) :to-be t)
        (expect (limits-p 3) :to-be nil)
        (expect (length names) :to-be (length +limit-specifications+))
        (expect (length (remove-duplicates (copy-list names)))
                :to-be (length +limit-specifications+))
        (loop for (name default) in +limit-specifications+
              for override from 101
              do (expect (limit-value defaults name) :to-be default)
                 (expect (and (typep default '(integer 0))
                              (<= default most-positive-fixnum))
                         :to-be t)
                 (expect (limit-value overrides name) :to-be override)))))

  (it "checks each resource boundary and reports all condition readers"
    (with-soft-assertions
      (dolist (specification +limit-specifications+)
        (let* ((name (first specification))
               (limit 2)
               (limits (apply #'make-limits (list name limit)))
               (condition
                 (handler-case
                     (progn (org-kit::check-limit limits name (1+ limit)) nil)
                   (org-resource-limit-error (condition) condition))))
          (expect (org-kit::check-limit limits name (1- limit)) :to-be (1- limit))
          (expect (org-kit::check-limit limits name limit) :to-be limit)
          (expect (typep condition 'org-resource-limit-error) :to-be t)
          (when condition
            (expect (org-resource-limit-error-limit-name condition) :to-be name)
            (expect (org-resource-limit-error-limit condition) :to-be limit)
            (expect (org-resource-limit-error-actual condition) :to-be (1+ limit)))))))

  (it "checks encoded octet limits for octet and binary stream output"
    (let* ((document (parse-org (octets #xFE #xFF #x00 #xE9) :encoding :utf-16be))
           (expected (octets #xFE #xFF #x00 #xE9))
           (below (make-limits :max-output-octets 3))
           (exact (make-limits :max-output-octets 4))
           (above (make-limits :max-output-octets 5)))
      (with-soft-assertions
        (expect (document-text document) :to-equal "é")
        (expect (document-encoding document) :to-be :utf-16be)
        (expect (document-bom-p document) :to-be t)
        (limits-test-call-with-string-to-octets-counter
         (lambda (calls)
           (let ((condition
                   (handler-case
                       (progn (write-org document :output-type :octets :limits below) nil)
                     (org-resource-limit-error (condition) condition))))
             (expect (typep condition 'org-resource-limit-error) :to-be t)
             (when condition
               (expect (org-resource-limit-error-limit-name condition)
                       :to-be :max-output-octets)
               (expect (org-resource-limit-error-limit condition) :to-be 3)
               (expect (org-resource-limit-error-actual condition) :to-be 4))
             (expect (funcall calls) :to-be 1))
           (expect (write-org document :output-type :octets :limits exact)
                   :to-equalp expected)
           (expect (funcall calls) :to-be 3)
           (expect (write-org document :output-type :octets :limits above)
                   :to-equalp expected)
           (expect (funcall calls) :to-be 5)
           (labels ((check-binary-stream (limits should-fail-p expected-calls)
                      (uiop:with-temporary-file
                          (:pathname pathname :stream stream
                           :element-type '(unsigned-byte 8) :direction :io)
                        (declare (ignore pathname))
                        (let ((result
                                (handler-case
                                    (write-org document :stream stream :output-type :stream
                                               :limits limits)
                                  (org-resource-limit-error (condition) condition))))
                          (if should-fail-p
                              (progn
                                (expect (typep result 'org-resource-limit-error) :to-be t)
                                (when (typep result 'org-resource-limit-error)
                                  (expect (org-resource-limit-error-limit-name result)
                                          :to-be :max-output-octets)
                                  (expect (org-resource-limit-error-limit result) :to-be 3)
                                  (expect (org-resource-limit-error-actual result) :to-be 4))
                                (expect (file-position stream) :to-be 0))
                              (progn
                                (expect (eq result stream) :to-be t)
                                (finish-output stream)
                                (expect (file-position stream) :to-be 4)
                                (file-position stream 0)
                                (let ((output (make-array 4
                                                          :element-type '(unsigned-byte 8))))
                                  (expect (read-sequence output stream) :to-be 4)
                                  (expect output :to-equalp expected))))
                          (expect (funcall calls) :to-be expected-calls)))))
             (check-binary-stream below t 6)
             (check-binary-stream exact nil 8)
             (check-binary-stream above nil 10)))))))

  (it "counts registered codec aliases outside the writer's built-in cases"
    (limits-test-call-with-encoding-alias
     :utf8 :utf-8
     (lambda ()
       (let* ((source (octets #xEF #xBB #xBF #x41))
              (expected (octets #xEF #xBB #xBF #x41))
              (document (parse-org source :encoding :utf8))
              (limits (make-limits :max-output-octets (length expected))))
         (with-soft-assertions
           (expect (document-text document) :to-equal "A")
           (expect (document-bom-p document) :to-be t)
           (expect (org-kit::reader-document-octet-count document)
                   :to-be (length expected))
           (expect (write-org document :output-type :octets :limits limits)
                   :to-equalp expected))))))

  (it "reports unsupported output encodings as ORG-KIT-ERROR"
    (let* ((document
             (org-kit::%make-document :text "A" :encoding :no-such-encoding
                                      :line-starts #(0) :limits (make-limits)))
           (condition
             (handler-case
                 (progn (write-org document :output-type :octets) nil)
               (org-kit-error (condition) condition))))
      (with-soft-assertions
        (expect (typep condition 'org-kit-error) :to-be t)
        (when condition
          (expect (search "NO-SUCH-ENCODING" (org-kit-error-message condition))
                  :to-be-truthy)))))

  (it "counts generic encoder output with no input BOM"
    (flet ((check-output (source encoding expected)
             (let* ((document (parse-org source :encoding encoding))
                    (codec-output
                      (cl-codec-kit:string-to-octets
                       (document-text document) :encoding encoding))
                    (limits (make-limits :max-output-octets (length expected))))
               (with-soft-assertions
                 (expect (document-bom-p document) :to-be nil)
                 (expect codec-output :to-equalp expected)
                 (expect (org-kit::reader-document-octet-count document)
                         :to-be (length codec-output))
                 (expect (write-org document :output-type :octets :limits limits)
                         :to-equalp expected)))))
      (check-output (octets #x00 #x41) :utf-16
                    (octets #xFE #xFF #x00 #x41))
      (check-output (octets #x00 #x00 #x00 #x41) :utf-32
                    (octets #x00 #x00 #xFE #xFF #x00 #x00 #x00 #x41))
      (check-output (octets #x00 #x41) :ucs-2
                    (octets #xFE #xFF #x00 #x41))
      (check-output (octets #x00 #x00 #x00 #x41) :ucs-4
                    (octets #x00 #x00 #xFE #xFF #x00 #x00 #x00 #x41))
      (check-output (octets #x41 #x00 #x00 #x00) :ucs-4/le
                    (octets #x41 #x00 #x00 #x00))))

  (it "checks actual octet counts across supported encodings and BOMs"
    (flet ((check-output (input encoding expected)
             (let* ((document (parse-org input :encoding encoding))
                    (limits (make-limits :max-output-octets (length expected))))
               (expect (write-org document :output-type :octets :limits limits)
                       :to-equalp expected))))
      (with-soft-assertions
        (check-output (octets #xEF #xBB #xBF #x41 #xC2 #x80 #xDF #xBF
                              #xE0 #xA0 #x80 #xF4 #x8F #xBF #xBF)
                      :utf-8
                      (octets #xEF #xBB #xBF #x41 #xC2 #x80 #xDF #xBF
                              #xE0 #xA0 #x80 #xF4 #x8F #xBF #xBF))
        (check-output (octets #xFE #xFF #xD7 #xFF #xE0 #x00 #xFF #xFF
                              #xD8 #x00 #xDC #x00 #xDB #xFF #xDF #xFF)
                      :utf-16be
                      (octets #xFE #xFF #xD7 #xFF #xE0 #x00 #xFF #xFF
                              #xD8 #x00 #xDC #x00 #xDB #xFF #xDF #xFF))
        (check-output (octets #xFF #xFE #x41 #x00) :utf-16/le
                      (octets #xFF #xFE #x41 #x00))
        (check-output (octets #xFE #xFF #x00 #x41) :utf-16/be
                      (octets #xFE #xFF #x00 #x41))
        (check-output (octets #x00 #x00 #xFE #xFF #x00 #x00 #x00 #x41)
                      :ucs-4/be
                      (octets #x00 #x00 #xFE #xFF #x00 #x00 #x00 #x41))
        (check-output (octets #xFF #xFE #x00 #x00 #x41 #x00 #x00 #x00)
                      :ucs-4/le
                      (octets #xFF #xFE #x00 #x00 #x41 #x00 #x00 #x00))
        (check-output (octets #xFE #xFF #x00 #x41) :utf-16
                      (octets #xFE #xFF #x00 #x41))
        (check-output (octets #x00 #x00 #xFE #xFF #x00 #x00 #x00 #x41)
                      :utf-32
                      (octets #x00 #x00 #xFE #xFF #x00 #x00 #x00 #x41))
        (check-output (octets #x00 #x00 #xFE #xFF #x00 #x00 #x00 #x41)
                      :ucs-4
                      (octets #x00 #x00 #xFE #xFF #x00 #x00 #x00 #x41))
        (check-output (octets #xFE #xFF #x00 #x41) :ucs-2
                      (octets #xFE #xFF #x00 #x41))
        (check-output (octets #xFE #xFF #x00 #x41) :ucs-2be
                      (octets #xFE #xFF #x00 #x41))
        (check-output (octets #xFE #xFF #x00 #x41) :ucs-2/be
                      (octets #xFE #xFF #x00 #x41))
        (check-output (octets #xFF #xFE #x41 #x00) :ucs-2le
                      (octets #xFF #xFE #x41 #x00))
        (check-output (octets #xFF #xFE #x41 #x00) :ucs-2/le
                      (octets #xFF #xFE #x41 #x00))
        (check-output (octets #x41 #x7F) :ascii (octets #x41 #x7F))
        (check-output (octets #x41 #x7F) :us-ascii (octets #x41 #x7F))
        (check-output (octets #xE9 #xFF) :iso-8859-1 (octets #xE9 #xFF))
        (check-output (octets #xE9 #xFF) :latin-1 (octets #xE9 #xFF)))))

  (it "rejects a non-Unicode source character before octet encoding"
    (flet ((check-unencodable (character encoding)
             (let ((document
                     (org-kit::%make-document :text (string character)
                                              :encoding encoding
                                              :line-starts #(0)
                                              :limits (make-limits))))
               (limits-test-call-with-string-to-octets-counter
                (lambda (calls)
                  (let ((condition
                          (handler-case
                              (progn (write-org document :output-type :octets) nil)
                            (cl-codec-kit:unencodable-character (condition) condition))))
                    (with-soft-assertions
                      (expect (typep condition 'cl-codec-kit:unencodable-character)
                              :to-be t)
                      (when condition
                        (expect (cl-codec-kit:unencodable-character-char condition)
                                :to-be character)
                        (expect (cl-codec-kit:unencodable-character-encoding condition)
                                :to-be encoding)
                        (expect (cl-codec-kit:unencodable-character-position condition)
                                :to-be 0))
                      (expect (funcall calls) :to-be 1))))))))
      (let ((surrogate (code-char #xD800)))
        (check-unencodable surrogate :utf-8)
        (check-unencodable surrogate :utf-16be)
        (check-unencodable surrogate :utf-32be))
      (check-unencodable (code-char #x10000) :ucs-2be)
      (check-unencodable (code-char #xE9) :ascii)
      (check-unencodable (code-char #x100) :iso-8859-1)))

  (it "rejects negative values and unknown names"
    (with-soft-assertions
      (dolist (specification +limit-specifications+)
        (signals type-error
          (apply #'make-limits (list (first specification) -1))))
      (signals error (limit-value (make-limits) :max-nothing))))

  (it "preserves document limits and uses the minimum applicable budget"
    (let* ((custom-limits (limits-with-sequential-overrides 1001))
           (document (parse-org "" :limits custom-limits))
           (lower-limits (limits-with-sequential-overrides 201))
           (higher-limits (limits-with-sequential-overrides 301))
           (lower-document (make-document "" :limits lower-limits))
           (higher-document (make-document "" :limits higher-limits))
           (document-limits-list (list (document-limits lower-document)
                                       (document-limits higher-document)))
           (default-document (parse-org ""))
           (defaults (make-limits)))
      (with-soft-assertions
        (expect (eq (document-limits document) custom-limits) :to-be t)
        (dolist (specification +limit-specifications+)
          (let ((name (first specification)))
            (expect (limit-value (document-limits document) name)
                    :to-be (limit-value custom-limits name))
            (expect (limit-value (document-limits default-document) name)
                    :to-be (limit-value defaults name))
            (expect (org-kit::minimum-limit-value document-limits-list name)
                    :to-be (min (limit-value lower-limits name)
                                (limit-value higher-limits name)))
            (expect (org-kit::minimum-limit-value (reverse document-limits-list) name)
                    :to-be (min (limit-value lower-limits name)
                                (limit-value higher-limits name)))
            (expect (org-kit::minimum-limit-value nil name)
                    :to-be (limit-value defaults name)))))))

  (it "enforces existing parser limits"
    (let* ((document (parse-org "" :limits (make-limits :max-node-count 1)))
           (node-limit-error
             (handler-case
                 (progn (parse-org "" :limits (make-limits :max-node-count 0)) nil)
               (org-resource-limit-error (condition) condition))))
      (with-soft-assertions
        (expect (document-p document) :to-be t)
        (expect (org-resource-limit-error-limit-name node-limit-error)
                :to-be :max-node-count)
        (expect (org-resource-limit-error-limit node-limit-error) :to-be 0)
        (expect (org-resource-limit-error-actual node-limit-error) :to-be 1)
        (signals org-resource-limit-error
          (parse-org "*a /b/*" :limits (make-limits :max-object-depth 1)))))))

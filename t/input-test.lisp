;;;; t/input-test.lisp
(in-package #:cl-org-kit/test)

(defun octets (&rest values)
  (make-array (length values) :element-type '(unsigned-byte 8) :initial-contents values))

(defun call-with-octet-file (octet-vector function)
  "Call FUNCTION with the pathname of a temporary file holding OCTET-VECTOR."
  (uiop:with-temporary-file (:pathname pathname :stream stream
                             :element-type '(unsigned-byte 8) :direction :output)
    (write-sequence octet-vector stream)
    (finish-output stream)
    (funcall function pathname)))

(defun decoded (input &key encoding (limits (make-limits)))
  (multiple-value-list (org-kit::decode-input input encoding limits)))

(describe "input decoding"
  (it "returns a string unchanged as a simple character string"
    (let ((result (decoded (make-array 3 :element-type 'character :adjustable t
                                         :initial-contents "* a"))))
      (with-soft-assertions
        (expect result :to-equal '("* a" nil nil))
        (expect (typep (first result) '(simple-array character (*))) :to-be t))))

  (it "removes and records a leading U+FEFF in a string"
    (expect (decoded (format nil "~C* a" (code-char #xFEFF))) :to-equal '("* a" nil t)))

  (it "decodes BOM-less octets as UTF-8"
    (expect (decoded (octets #x2A #x20 #xC3 #xA9)) :to-equal (list "* é" :utf-8 nil)))

  (it "detects a UTF-8 BOM"
    (expect (decoded (octets #xEF #xBB #xBF #x61)) :to-equal '("a" :utf-8 t)))

  (it "detects a UTF-16LE BOM"
    (let ((result (decoded (octets #xFF #xFE #x61 #x00))))
      (with-soft-assertions
        (expect (first result) :to-equal "a")
        (expect (third result) :to-be t))))

  (it "uses an explicit encoding and still strips a decoded BOM"
    (with-soft-assertions
      (expect (decoded (octets #x61 #xE9) :encoding :latin-1) :to-equal (list "aé" :latin-1 nil))
      (expect (decoded (octets #xEF #xBB #xBF #x62) :encoding :utf-8) :to-equal '("b" :utf-8 t))))

  (it "records a BOM consumed by an explicit generic UTF-16 encoding"
    (expect (decoded (octets #xFE #xFF #x00 #x61) :encoding :utf-16)
            :to-equal '("a" :utf-16 t)))

  (it "signals ORG-PARSE-ERROR with the octet offset for invalid UTF-8"
    (let ((condition (handler-case (decoded (octets #x61 #xC3 #x28))
                       (org-parse-error (c) c))))
      (expect (org-parse-error-offset condition) :to-be 1)))

  (it "signals ORG-KIT-ERROR for an unsupported encoding"
    (let ((condition (handler-case (decoded (octets #x61) :encoding :no-such-encoding)
                       (org-kit-error (c) c))))
      (with-soft-assertions
        (expect (typep condition 'org-parse-error) :to-be nil)
        (expect (search "NO-SUCH-ENCODING" (org-kit-error-message condition)) :to-be-truthy))))

  (it "reads a character stream"
    (with-input-from-string (stream (format nil "~C* a" (code-char #xFEFF)))
      (expect (decoded stream) :to-equal '("* a" nil t))))

  (it "reads a character stream longer than one chunk"
    (let ((text (make-string 10000 :initial-element #\z)))
      (with-input-from-string (stream text)
        (expect (first (decoded stream)) :to-equal text))))

  (it "reads an octet stream and a pathname"
    (call-with-octet-file
     (octets #xEF #xBB #xBF #x2A)
     (lambda (pathname)
       (with-soft-assertions
         (with-open-file (stream pathname :element-type '(unsigned-byte 8))
           (expect (decoded stream) :to-equal '("*" :utf-8 t)))
         (expect (decoded pathname) :to-equal '("*" :utf-8 t))))))

  (it "rejects an unsupported input type"
    (signals type-error (decoded 42)))

  (it "enforces :MAX-INPUT-LENGTH for every input kind"
    (let ((limits (make-limits :max-input-length 2)))
      (with-soft-assertions
        (expect (first (decoded "ab" :limits limits)) :to-equal "ab")
        (signals org-resource-limit-error (decoded "abc" :limits limits))
        (signals org-resource-limit-error (decoded (octets 97 98 99) :limits limits))
        (with-input-from-string (stream "abc")
          (signals org-resource-limit-error (decoded stream :limits limits)))))))

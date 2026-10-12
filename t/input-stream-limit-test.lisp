;;;; t/input-stream-limit-test.lisp
(in-package #:cl-org-kit/test)

(defclass input-limit-character-stream (sb-gray:fundamental-character-input-stream)
  ((text :initarg :text :reader input-limit-character-text)
   (position :initform 0 :accessor input-limit-character-position)))

(defmethod stream-element-type ((stream input-limit-character-stream))
  (declare (ignore stream))
  'character)

(defmethod sb-gray:stream-read-char ((stream input-limit-character-stream))
  (with-slots (text position) stream
    (if (< position (length text))
        (prog1 (char text position) (incf position))
        :eof)))

(defclass input-limit-octet-stream (sb-gray:fundamental-binary-input-stream)
  ((octets :initarg :octets :reader input-limit-octets)
   (position :initform 0 :accessor input-limit-octet-position)))

(defmethod stream-element-type ((stream input-limit-octet-stream))
  (declare (ignore stream))
  '(unsigned-byte 8))

(defmethod sb-gray:stream-read-byte ((stream input-limit-octet-stream))
  (with-slots (octets position) stream
    (if (< position (length octets))
        (prog1 (aref octets position) (incf position))
        :eof)))

(defun input-limit-error (stream)
  (handler-case
      (progn
        (org-kit::decode-input stream nil (make-limits :max-input-length 2))
        nil)
    (org-resource-limit-error (condition) condition)))

(describe "stream input-length limit"
  (it "reads only one excess character before signalling"
    (let* ((stream (make-instance 'input-limit-character-stream
                                  :text (make-string 5000 :initial-element #\x)))
           (condition (input-limit-error stream)))
      (with-soft-assertions
        (expect (org-resource-limit-error-limit-name condition) :to-be :max-input-length)
        (expect (org-resource-limit-error-limit condition) :to-be 2)
        (expect (org-resource-limit-error-actual condition) :to-be 3)
        (expect (input-limit-character-position stream) :to-be 3))))

  (it "reads only one excess octet before signalling"
    (let* ((stream (make-instance 'input-limit-octet-stream
                                  :octets (make-array 5000
                                                      :element-type '(unsigned-byte 8)
                                                      :initial-element 97)))
           (condition (input-limit-error stream)))
      (with-soft-assertions
        (expect (org-resource-limit-error-limit-name condition) :to-be :max-input-length)
        (expect (org-resource-limit-error-limit condition) :to-be 2)
        (expect (org-resource-limit-error-actual condition) :to-be 3)
        (expect (input-limit-octet-position stream) :to-be 3))))

  (it "accepts character and octet streams exactly at the limit"
    (let ((character-stream (make-instance 'input-limit-character-stream :text "ab"))
          (octet-stream (make-instance 'input-limit-octet-stream
                                       :octets (make-array 2
                                                           :element-type '(unsigned-byte 8)
                                                           :initial-contents '(97 98)))))
      (with-soft-assertions
        (expect (multiple-value-list
                 (org-kit::decode-input character-stream nil
                                        (make-limits :max-input-length 2)))
                :to-equal '("ab" nil nil))
        (expect (input-limit-character-position character-stream) :to-be 2)
        (expect (multiple-value-list
                 (org-kit::decode-input octet-stream nil
                                        (make-limits :max-input-length 2)))
                :to-equal '("ab" :utf-8 nil))
        (expect (input-limit-octet-position octet-stream) :to-be 2)))))

;;;; src/input.lisp
;;;;
;;;; Turn every accepted input into one simple character string. Only the
;;;; PATHNAME branch performs I/O of its own; a stream is read as given.
(in-package #:org-kit)

(defconstant +read-chunk-size+ 4096)

(defun strip-bom (text)
  "Return TEXT without a leading U+FEFF and, as a second value, whether one was removed."
  (if (and (plusp (length text)) (char= (char text 0) (code-char #xFEFF)))
      (values (subseq text 1) t)
      (values text nil)))

(defun decode-octets (octets encoding limits)
  "Decode OCTETS with ENCODING, or a detected Unicode encoding when ENCODING is NIL.
Return the text, the encoding used, and whether a BOM was present."
  (check-limit limits :max-input-length (length octets))
  (multiple-value-bind (detected bom-length)
      (cl-codec-kit:detect-unicode-encoding octets)
    (let ((decoded
            (handler-case
                (if encoding
                    (cl-codec-kit:octets-to-string octets :encoding encoding)
                    (cl-codec-kit:octets-to-string octets :encoding detected
                                                          :start bom-length))
              (cl-codec-kit:decode-error (condition)
                (error 'org-parse-error
                       :message "Input is not validly encoded"
                       :offset (cl-codec-kit:decode-error-position condition)))
              (cl-codec-kit:cl-codec-kit-error ()
                (error 'org-kit-error
                       :message (format nil "Unsupported encoding ~A"
                                        (bounded-text (prin1-to-string encoding))))))))
      (multiple-value-bind (text stripped) (strip-bom decoded)
        (values text
                (or encoding detected)
                (or stripped (and (null encoding) (plusp bom-length))))))))

(defun read-stream-contents (stream limits)
  "Read STREAM to its end, as a string for a character stream and as an octet
vector otherwise, signalling once the amount read exceeds :MAX-INPUT-LENGTH."
  (let* ((characterp (subtypep (stream-element-type stream) 'character))
         (element-type (if characterp 'character '(unsigned-byte 8)))
         (buffer (make-array +read-chunk-size+ :element-type element-type))
         (contents (make-array 0 :element-type element-type :adjustable t :fill-pointer 0)))
    (loop for count = (read-sequence buffer stream)
          while (plusp count)
          do (check-limit limits :max-input-length (+ (length contents) count))
             (loop for index below count
                   do (vector-push-extend (aref buffer index) contents)))
    (coerce contents (if characterp 'simple-string '(simple-array (unsigned-byte 8) (*))))))

(defun decode-string (string limits)
  (check-limit limits :max-input-length (length string))
  (multiple-value-bind (text stripped) (strip-bom string)
    (values text nil stripped)))

(defun decode-input (input encoding limits)
  "Return INPUT as a simple character string, the encoding it was decoded from
(NIL for character input), and whether it began with a byte order mark.
INPUT is a string, an octet vector, a stream, or a pathname."
  (multiple-value-bind (text used-encoding bom-p)
      (etypecase input
        (string (decode-string input limits))
        ((vector (unsigned-byte 8)) (decode-octets input encoding limits))
        (stream
         (let ((contents (read-stream-contents input limits)))
           (if (stringp contents)
               (decode-string contents limits)
               (decode-octets contents encoding limits))))
        (pathname
         (with-open-file (stream input :element-type '(unsigned-byte 8))
           (decode-octets (read-stream-contents stream limits) encoding limits))))
    (values (coerce text '(simple-array character (*))) used-encoding bom-p)))

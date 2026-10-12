(in-package #:org-kit)

(defun reader-encoding-name (encoding) (string-downcase (string encoding)))

(defun reader-output-bom (encoding)
  (let ((name (reader-encoding-name encoding)))
    (cond ((member name '("utf-8" "utf8") :test #'string=) #(239 187 191))
          ((member name '("utf-16le" "utf16le") :test #'string=) #(255 254))
          ((member name '("utf-16be" "utf16be") :test #'string=) #(254 255))
          ((member name '("utf-32le" "utf32le") :test #'string=) #(255 254 0 0))
          ((member name '("utf-32be" "utf32be") :test #'string=) #(0 0 254 255))
          (t #()))))

(defun reader-generic-bom-encoding-p (encoding)
  (member (reader-encoding-name encoding) '("utf-16" "utf16" "utf-32" "utf32" "ucs-2")
          :test #'string=))

(defun reader-document-octets (document)
  (let* ((encoding (or (document-encoding document) :utf-8))
         (octets (cl-codec-kit:string-to-octets (document-text document)
                                                :encoding encoding)))
    (if (or (not (document-bom-p document))
            (reader-generic-bom-encoding-p encoding))
        octets
        (let* ((bom (reader-output-bom encoding))
               (result (make-array (+ (length bom) (length octets))
                                   :element-type '(unsigned-byte 8))))
          (replace result bom)
          (replace result octets :start1 (length bom))
          result))))

(defun reader-document-output-text (document)
  (let ((text (document-text document)))
    (if (document-bom-p document)
        (concatenate 'string (string (code-char #xFEFF)) text)
        text)))

(defun write-org (document &key stream (output-type :string) (limits (make-limits)))
  "Write DOCUMENT as source text or octets, or to a character/octet STREAM.
OUTPUT-TYPE is :STRING, :OCTETS or :STREAM. The original newline spelling and
final-newline state are preserved. LIMITS bounds the character output length."
  (let ((text (reader-document-output-text document)))
    (check-limit limits :max-output-length (length text))
    (ecase output-type
      (:string text)
      (:octets (reader-document-octets document))
      (:stream
       (unless stream
         (error 'type-error :datum stream :expected-type 'stream))
       (if (subtypep (stream-element-type stream) 'character)
           (write-string text stream)
           (write-sequence (reader-document-octets document) stream))
       stream))))

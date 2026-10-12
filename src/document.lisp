;;;; src/document.lisp
;;;;
;;;; A DOCUMENT owns the decoded source text. Node offsets index into that text,
;;;; and line/column numbers are derived from its line index, so the writer can
;;;; reproduce the input from spans and nodes never store positions twice.
(in-package #:org-kit)

(defstruct (document (:constructor %make-document) (:conc-name %document-)
                     (:copier nil) (:predicate %document-p))
  "Decoded Org source with its line index, settings, resource limits and parse state."
  (text "" :type (simple-array character (*)) :read-only t)
  (encoding nil :read-only t)
  (bom-p nil :read-only t)
  (source-name nil :read-only t)
  (line-starts #() :type simple-vector :read-only t)
  (root nil)
  (settings '() :type list)
  (limits nil :type limits :read-only t)
  (object-context nil))

(defun line-start-offsets (text)
  "Return a simple vector of the offsets at which each line of TEXT starts.
Only #\\Newline ends a line; the #\\Return of a CRLF pair stays on its line."
  (coerce (cons 0 (loop for index from 0 below (length text)
                        when (char= (char text index) #\Newline)
                          collect (1+ index)))
          'simple-vector))

(defun make-document (input &key source-name encoding (limits (make-limits)))
  "Decode INPUT and return a DOCUMENT with no parsed tree yet.
INPUT is a string, a character or octet stream, an octet vector, or a pathname.
Octet input is decoded with ENCODING, or with the Unicode encoding detected from
its BOM or null-byte pattern when ENCODING is NIL. A leading BOM is removed from
the text and recorded by DOCUMENT-BOM-P. LIMITS bounds the input length and is
kept as DOCUMENT-LIMITS for later parses of the same document.
Signals ORG-PARSE-ERROR for undecodable octets and ORG-RESOURCE-LIMIT-ERROR for
oversized input."
  (multiple-value-bind (text used-encoding bom-p) (decode-input input encoding limits)
    (%make-document :text text :encoding used-encoding :bom-p bom-p
                    :source-name source-name
                    :line-starts (line-start-offsets text)
                    :limits limits)))

(defun document-p (object)
  "Return true when OBJECT is a DOCUMENT."
  (%document-p object))

(defun document-text (document)
  "Return the decoded source text of DOCUMENT, without any BOM. Node offsets
index into this string."
  (%document-text document))

(defun document-encoding (document)
  "Return the encoding octet input was decoded from, or NIL for character input."
  (%document-encoding document))

(defun document-bom-p (document)
  "Return true when the input began with a byte order mark."
  (%document-bom-p document))

(defun document-source-name (document)
  "Return the :SOURCE-NAME given to MAKE-DOCUMENT, or NIL."
  (%document-source-name document))

(defun document-limits (document)
  "Return the LIMITS object given to MAKE-DOCUMENT, or the defaults it used."
  (%document-limits document))

(defun document-settings (document)
  "Return the plist of in-buffer settings recorded for DOCUMENT."
  (%document-settings document))

(defun (setf document-settings) (settings document)
  "Replace the in-buffer settings plist of DOCUMENT."
  (setf (%document-settings document) settings))

(defun document-line-count (document)
  "Return the number of lines in DOCUMENT. Text ending in a newline has an
empty last line: \"a\" has one line, and \"a\" followed by a newline has two."
  (length (%document-line-starts document)))

(defun check-offset (document offset)
  (let ((length (length (%document-text document))))
    (unless (and (integerp offset) (<= 0 offset length))
      (error 'type-error :datum offset :expected-type `(integer 0 ,length)))))

(defun offset-line-column (document offset)
  "Return the 1-based line and 1-based column of character OFFSET in DOCUMENT.
OFFSET may equal the text length, naming the position after the last character."
  (check-offset document offset)
  (let* ((starts (%document-line-starts document))
         (low 0)
         (high (1- (length starts))))
    ;; Binary search for the last line starting at or before OFFSET.
    (loop while (< low high)
          do (let ((middle (ceiling (+ low high) 2)))
               (if (<= (svref starts middle) offset)
                   (setf low middle)
                   (setf high (1- middle)))))
    (values (1+ low) (1+ (- offset (svref starts low))))))

(defun line-offset (document line)
  "Return the character offset at which 1-based LINE of DOCUMENT starts."
  (let ((starts (%document-line-starts document)))
    (unless (and (integerp line) (<= 1 line (length starts)))
      (error 'type-error :datum line :expected-type `(integer 1 ,(length starts))))
    (svref starts (1- line))))

(defun document-newline (document)
  "Classify the line endings of DOCUMENT as :LF, :CRLF, :MIXED, or :NONE when
the text has no newline."
  (let ((text (%document-text document))
        (lf 0)
        (crlf 0))
    (loop for start across (%document-line-starts document)
          for newline = (1- start)
          when (>= newline 0)
            do (if (and (plusp newline) (char= (char text (1- newline)) #\Return))
                   (incf crlf)
                   (incf lf)))
    (cond ((and (zerop lf) (zerop crlf)) :none)
          ((zerop crlf) :lf)
          ((zerop lf) :crlf)
          (t :mixed))))

(defun document-final-newline-p (document)
  "Return true when the text of DOCUMENT ends with a newline."
  (let ((text (%document-text document)))
    (and (plusp (length text))
         (char= (char text (1- (length text))) #\Newline))))

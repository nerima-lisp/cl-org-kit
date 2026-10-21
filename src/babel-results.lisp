(in-package #:org-kit)

(defun babel-results-error (reason message context)
  (error 'org-babel-error :message message :reason reason
         :context (and (stringp context) (bounded-text context))))

(defun babel-results-parameters (document block parameters)
  (let ((base (if (eq (node-type block) :src-block)
                  (resolve-babel-parameters document block)
                  (copy-tree +babel-default-header-arguments+))))
    (dolist (entry parameters base)
      (unless (and (consp entry)
                   (stringp (car entry))
                   (string= (car entry) (string-upcase (car entry)))
                   (or (null (cdr entry)) (stringp (cdr entry))))
        (error 'type-error :datum entry
               :expected-type '(cons string (or string null))))
      (setf base
            (babel-merge-header-parameter base (car entry) (cdr entry))))))

(defun babel-results-options (parameters)
  (let* ((position (position "RESULTS" parameters :key #'car :test #'string=))
         (tokens (mapcar #'string-downcase
                         (babel-split-header-list
                          (and position (cdr (nth position parameters)))
                          :whitespace)))
         (operation (find-if (lambda (token)
                               (member token '("replace" "append" "prepend" "silent")
                                       :test #'string=))
                             tokens))
         (channel (or (find-if (lambda (token)
                                 (member token '("value" "output")
                                         :test #'string=))
                               tokens)
                      "value"))
         (format (or (find-if (lambda (token)
                                (member token '("table" "list" "verbatim" "raw"
                                                "drawer" "org" "html" "code")
                                        :test #'string=))
                              tokens)
                     "raw"))
         (known '("value" "output" "table" "list" "verbatim" "raw"
                  "drawer" "org" "html" "code" "replace" "append"
                  "prepend" "silent")))
    (dolist (token tokens)
      (unless (member token known :test #'string=)
        (babel-results-error :unsupported-result-option
                             "Unsupported Babel result option" token)))
    (values channel format (or operation "replace"))))

(defun babel-results-newline (document offset)
  (let ((text (document-text document)))
    (case (document-newline document)
      (:crlf (format nil "~C~C" #\Return #\Newline))
      (:mixed
       (let ((position (position #\Newline text :end (min offset (length text))
                                 :from-end t)))
         (if (and position (plusp position)
                  (char= (char text (1- position)) #\Return))
             (format nil "~C~C" #\Return #\Newline)
             (string #\Newline))))
      (otherwise (string #\Newline)))))

(defclass babel-results-bounded-stream
    (sb-gray:fundamental-character-output-stream)
  ((document :initarg :document :reader babel-results-stream-document)
   (output :initform (make-string-output-stream)
           :reader babel-results-stream-output)
   (characters :initform 0 :accessor babel-results-stream-characters)
   (octets :initform 0 :accessor babel-results-stream-octets)
   (last-character :initform nil :accessor babel-results-stream-last-character)))

(defun babel-results-encoded-range-octets (text start end encoding)
  (let* ((generic-preamble-p (reader-generic-bom-encoding-p encoding))
         (preamble (if generic-preamble-p
                       (edit-encoded-range-octets "" 0 0 encoding)
                       0))
         (body-octets
           (loop for chunk-start from start below end by 1024
                 for chunk-end = (min end (+ chunk-start 1024))
                 for chunk-octets =
                   (edit-encoded-range-octets text chunk-start chunk-end encoding)
                 sum (if generic-preamble-p
                         (max 0 (- chunk-octets preamble))
                         chunk-octets))))
    (+ preamble body-octets)))

(defun babel-results-check-write (stream text &optional (start 0) end)
  (let* ((end (or end (length text)))
         (document (babel-results-stream-document stream))
         (character-total
           (+ (babel-results-stream-characters stream) (- end start)))
         (encoding (or (document-encoding document) :utf-8)))
    (check-limit (document-limits document) :max-output-length character-total)
    (let* ((preamble (if (reader-generic-bom-encoding-p encoding)
                         (edit-encoded-range-octets "" 0 0 encoding)
                         0))
           (octet-total
             (+ (babel-results-stream-octets stream)
                (max 0 (- (babel-results-encoded-range-octets
                           text start end encoding)
                          preamble)))))
      (check-limit (document-limits document) :max-output-octets octet-total)
      (values character-total octet-total))))

(defmethod sb-gray:stream-write-char ((stream babel-results-bounded-stream) character)
  (multiple-value-bind (length octets)
      (babel-results-check-write stream (string character))
    (write-char character (babel-results-stream-output stream))
    (setf (babel-results-stream-characters stream) length
          (babel-results-stream-octets stream) octets
          (babel-results-stream-last-character stream) character))
  character)

(defmethod sb-gray:stream-write-string
    ((stream babel-results-bounded-stream) string &optional (start 0) end)
  (let ((end (or end (length string))))
    (multiple-value-bind (character-total octet-total)
        (babel-results-check-write stream string start end)
      (write-string string (babel-results-stream-output stream)
                    :start start :end end)
      (setf (babel-results-stream-characters stream) character-total
            (babel-results-stream-octets stream) octet-total
            (babel-results-stream-last-character stream)
            (and (> end start) (char string (1- end)))))
    string))

(defun babel-results-write-text (stream text newline)
  (loop with start = 0
        for position from 0 below (length text)
        for character = (char text position)
        when (or (char= character #\Newline) (char= character #\Return))
          do (write-string text stream :start start :end position)
             (write-string newline stream)
             (when (and (char= character #\Return)
                        (< (1+ position) (length text))
                        (char= (char text (1+ position)) #\Newline))
               (incf position))
             (setf start (1+ position))
        finally (write-string text stream :start start)))

(defun babel-results-write-value (stream value newline)
  (if (stringp value)
      (babel-results-write-text stream value newline)
      (princ value stream)))

(defun babel-results-sequence (value)
  (cond
    ((and (listp value) (numberp (edit-proper-list-length value))) value)
    ((and (vectorp value) (not (stringp value))) (coerce value 'list))
    (t (babel-results-error :unsupported-result-format
                            "Babel result format requires a sequence" value))))

(defun babel-results-write-table (stream value newline)
  (let* ((items
           (cond
             ((and (listp value)
                   (integerp (edit-proper-list-length value)))
              value)
             ((and (vectorp value) (not (stringp value)))
              (coerce value 'list))
             (t (list value))))
         (first-item (first items))
         (row-p (or (listp first-item)
                    (and (vectorp first-item)
                         (not (stringp first-item)))))
         (rows (if (and items row-p) items (mapcar #'list items))))
    (dolist (row rows)
      (write-char #\| stream)
      (dolist (cell (babel-results-sequence row))
        (write-char #\Space stream)
        (babel-results-write-value stream cell newline)
        (write-char #\Space stream)
        (write-char #\| stream))
      (write-string newline stream))))

(defun babel-results-write-list (stream value newline)
  (dolist (item (babel-results-sequence value))
    (write-string "- " stream)
    (babel-results-write-value stream item newline)
    (write-string newline stream)))

(defun babel-results-ensure-newline (stream newline)
  (unless (eql (babel-results-stream-last-character stream)
               (char newline (1- (length newline))))
    (write-string newline stream)))

(defun babel-results-write-wrapped (stream opener value closer newline)
  (write-string opener stream)
  (write-string newline stream)
  (babel-results-write-value stream value newline)
  (babel-results-ensure-newline stream newline)
  (write-string closer stream))

(defun babel-results-trim-final-newlines (text)
  (loop with end = (length text)
        while (and (plusp end)
                   (find (char text (1- end)) '(#\Newline #\Return)))
        do (if (and (> end 1)
                    (char= (char text (1- end)) #\Newline)
                    (char= (char text (- end 2)) #\Return))
               (decf end 2)
               (decf end))
        finally (return (subseq text 0 end))))

(defun babel-results-render (document block value channel format language offset)
  (let* ((stream (make-instance 'babel-results-bounded-stream :document document))
         (output stream)
         (newline (babel-results-newline document offset))
         (render-value (if (and (string= channel "output")
         (not (string= format "table")))
    (if (stringp value)
        value
        (babel-results-error
         :unsupported-result-channel
         "Babel output results must be strings" value))
    value))
         (language (or language (node-property block :language) "text")))
    (when (and (string= format "code")
               (or (not (stringp language))
                   (find-if (lambda (character)
                              (or (char= character #\Newline)
                                  (char= character #\Return)))
                            language)))
      (babel-results-error :invalid-result-language
                           "Babel code result language is invalid" language))
    (cond
      ((string= format "table")
       (babel-results-write-table output render-value newline))
      ((string= format "list")
       (babel-results-write-list output render-value newline))
      ((string= format "verbatim")
       (babel-results-write-wrapped output "#+BEGIN_EXAMPLE" render-value
                                    "#+END_EXAMPLE" newline))
      ((member format '("raw" "org") :test #'string=)
       (babel-results-write-value output render-value newline))
      ((string= format "drawer")
       (babel-results-write-wrapped output ":RESULTS:" render-value
                                    ":END:" newline))
      ((string= format "html")
       (babel-results-write-wrapped output "#+BEGIN_EXPORT html" render-value
                                    "#+END_EXPORT" newline))
      ((string= format "code")
       (babel-results-write-wrapped
        output (format nil "#+BEGIN_SRC ~A" language) render-value
        "#+END_SRC" newline))
      (t
       (babel-results-error :unsupported-result-format
                            "Unsupported Babel result format" format)))
    (babel-results-trim-final-newlines
     (get-output-stream-string (babel-results-stream-output stream)))))

(defun babel-results-anchor-offset (document block)
  (unless (member (node-type block) '(:src-block :babel-call) :test #'eq)
    (babel-results-error :invalid-target
                         "Babel results require a source block or call node" block))
  (unless (and (eq (node-document block) document) (integerp (node-end block)))
    (babel-results-error :document-mismatch
                         "Babel result anchor must belong to the document" block))
  (node-end block))

(defun babel-results-next-sibling (node)
  (let* ((parent (node-parent node))
         (children (and parent (node-children parent)))
         (position (and children (position node children :test #'eq))))
    (when position
      (nth (1+ position) children))))

(defun babel-results-node-end-before-post-blank (document node)
  (loop with source = (document-text document)
        with end = (node-end node)
        repeat (node-post-blank node)
        do (progn
             (cond
               ((and (plusp end)
                     (char= (char source (1- end)) #\Newline))
                (decf end)
                (when (and (plusp end)
                           (char= (char source (1- end)) #\Return))
                  (decf end)))
               ((and (plusp end)
                     (char= (char source (1- end)) #\Return))
                (decf end)))
             (loop while (and (plusp end)
                              (find (char source (1- end)) '(#\Space #\Tab)))
                   do (decf end)))
        finally (return end)))

(defun babel-results-keyword-marker-fields (node)
  (let ((key (node-property node :key))
        (label (node-property node :value))
        (text (node-text node)))
    (when (and (eq (node-type node) :keyword) (stringp key))
      (cond
        ((string= key "RESULTS")
         (values nil label t))
        ((and (>= (length key) 8)
              (string= (subseq key 0 8) "RESULTS[")
              (stringp text))
         (let* ((open (position #\[ text))
                (close (and open
                            (position #\] text :start (1+ open)))))
           (when close
             (values (subseq text (1+ open) close) label t))))))))

(defun babel-results-node-marker-fields (node)
  (if (eq (node-type node) :keyword)
      (babel-results-keyword-marker-fields node)
      (multiple-value-bind (label found-p) (node-property node :results)
        (when found-p
          (values nil label t)))))

(defun babel-results-result-payload-node (marker)
  (let ((next (and (zerop (node-post-blank marker))
                   (babel-results-next-sibling marker))))
    (and next (= (node-end marker) (node-begin next)) next)))

(defun babel-results-adjacent-region (document block)
  (let* ((anchor (babel-results-anchor-offset document block))
         (candidate (babel-results-next-sibling block)))
    (when (and candidate (= anchor (node-begin candidate)))
      (multiple-value-bind (hash label marker-p)
          (babel-results-node-marker-fields candidate)
        (when marker-p
          (let* ((keyword-p (eq (node-type candidate) :keyword))
                 (payload (and keyword-p
                               (babel-results-result-payload-node candidate)))
                 (payload-start
                   (if keyword-p
                       (babel-results-node-end-before-post-blank document candidate)
                       (or (node-post-affiliated candidate)
                           (node-begin candidate))))
                 (end (babel-results-node-end-before-post-blank
                       document (or payload candidate))))
            (values (node-begin candidate) end payload-start hash label t)))))))

(defun babel-results-adjacent-hash (document block)
  (multiple-value-bind (start end payload-start hash label found-p)
      (babel-results-adjacent-region document block)
    (declare (ignore start end payload-start label))
    (values hash found-p)))

(defun babel-results-strip-final-newline (text)
  (let ((length (length text)))
    (cond
      ((and (> length 1)
            (char= (char text (1- length)) #\Newline)
            (char= (char text (- length 2)) #\Return))
       (subseq text 0 (- length 2)))
      ((and (plusp length)
            (find (char text (1- length)) '(#\Newline #\Return)))
       (subseq text 0 (1- length)))
      (t text))))

(defun babel-results-marker-text (hash label)
  (when (and label
             (or (not (stringp label))
                 (find-if (lambda (character)
                            (or (char= character #\Newline)
                                (char= character #\Return)))
                          label)))
    (babel-results-error :invalid-result-label
                         "Babel result label cannot contain line breaks" label))
  (concatenate 'string "#+RESULTS"
               (if hash (format nil "[~A]" hash) "")
               ":"
               (if (and (stringp label) (plusp (length label)))
                   (concatenate 'string " " label)
                   "")))

(defun babel-results-validate-hash (hash)
  (when (and hash
             (or (not (stringp hash))
                 (find-if (lambda (character)
                            (or (char= character #\])
                                (char= character #\[)
                                (char= character #\Newline)
                                (char= character #\Return)))
                          hash)))
    (babel-results-error :invalid-result-hash
                         "Babel result hash cannot be written in a marker" hash))
  hash)

(defun babel-results-insertion-text (document offset marker content)
  (let* ((newline (babel-results-newline document offset))
         (source (document-text document))
         (prefix (if (and (plusp offset)
                          (not (find (char source (1- offset))
                                     '(#\Newline #\Return))))
                     newline
                     "")))
    (concatenate 'string prefix marker newline content newline)))

(defun babel-results-apply-patch (document patch target)
  (let* ((prepared (edit-prepare-patches document (list patch) :target target))
         (source (edit-prepared-patches-source prepared))
         (patches (edit-prepared-patches-patches prepared))
         (expected-document
           (edit-reparse-source document (edit-splice-source source patches) target))
         (verified-prepared
           (%make-edit-prepared-patches
            document source patches target
            (edit-canonical-document expected-document))))
    (edit-finish-prepared-patches verified-prepared)))

(defun update-babel-results (document block raw-result
                              &key parameters hash language)
  "Format RAW-RESULT at BLOCK's adjacent results region and return an edit result.

DOCUMENT remains unchanged; the result contains a reparsed document and source
spans for the result-region edit."
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document))
  (unless (node-p block)
    (error 'type-error :datum block :expected-type 'node))
  (edit-resolve-node document block '(:src-block :babel-call))
  (babel-results-validate-hash hash)
  (let ((parameters (babel-results-parameters document block parameters)))
    (multiple-value-bind (channel format operation)
        (babel-results-options parameters)
      (when (string= operation "silent")
        (return-from update-babel-results
          (make-edit-result :updated-document document :spans nil)))
      (let* ((context (make-babel-resolution-context :document document))
             (checked (babel-check-result-value context raw-result))
             (anchor (babel-results-anchor-offset document block))
             (new-content
               (babel-results-render document block checked channel format
                                     language anchor))
             (newline (babel-results-newline document anchor)))
        (multiple-value-bind (start end payload-start old-hash old-label found-p)
            (babel-results-adjacent-region document block)
          (declare (ignore old-hash))
          (let* ((label (or (and found-p old-label)
                            (node-property block :name)))
                 (marker (babel-results-marker-text hash label)))
            (if found-p
                (let* ((source (document-text document))
                       (old-content
                         (babel-results-strip-final-newline
                          (subseq source payload-start end)))
                       (combined
                         (cond
                           ((string= operation "append")
                            (if (zerop (length old-content)) new-content
                                (format nil "~A~A~A" old-content newline new-content)))
                           ((string= operation "prepend")
                            (if (zerop (length old-content)) new-content
                                (format nil "~A~A~A" new-content newline old-content)))
                           (t new-content)))
                       (replacement
                         (babel-results-insertion-text document start marker combined)))
                  (babel-results-apply-patch
                   document (edit-make-patch start end replacement block) block))
                (let ((replacement
                        (babel-results-insertion-text document anchor marker new-content)))
                  (babel-results-apply-patch
                   document (edit-make-patch anchor anchor replacement block) block)))))))))

;;;; Format-preserving insertion of Org structure templates.
(in-package #:org-kit)

(defparameter *edit-structure-template-aliases*
  '(("a" . "export ascii")
    ("c" . "center")
    ("C" . "comment")
    ("e" . "example")
    ("E" . "export")
    ("h" . "export html")
    ("l" . "export latex")
    ("q" . "quote")
    ("s" . "src")
    ("v" . "verse")))

(defun edit-structure-template-check-document (document)
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document))
  document)

(defun edit-structure-template-validate-span (document span target)
  (edit-structure-template-check-document document)
  (let ((length (length (document-text document))))
    (unless (and (edit-span-p span)
                 (integerp (edit-span-start span))
                 (integerp (edit-span-end span))
                 (<= 0 (edit-span-start span)
                     (edit-span-end span) length))
      (edit-signal-error target :invalid-value
                         "Edit span is invalid for this document")))
  span)

(defun edit-structure-template-horizontal-space-p (character)
  (find character '(#\Space #\Tab)))

(defun edit-structure-template-normalize-type (type target)
  (unless (and (stringp type) (plusp (length type))
               (not (find-if (lambda (character)
                               (find character '(#\Return #\Newline)))
                             type))
               (not (edit-structure-template-horizontal-space-p (char type 0)))
               (not (edit-structure-template-horizontal-space-p
                     (char type (1- (length type)))))
               (not (find #\Tab type)))
    (edit-signal-error target :invalid-value
                       "Structure template type must be a nonempty single-line string"))
  (let* ((normalized (or (cdr (assoc type *edit-structure-template-aliases*
                                     :test #'string=))
                         type))
         (token-end
          (or (position #\Space normalized)
              (length normalized))))
    (unless (every (lambda (character)
                     (or (alphanumericp character)
                         (find character '(#\- #\_))))
                   (subseq normalized 0 token-end))
      (edit-signal-error target :invalid-value
                         "Structure template type has an invalid block name"))
    normalized))

(defun edit-structure-template-first-token (type)
  (subseq type 0 (or (position #\Space type) (length type))))

(defun edit-structure-template-uppercase-p (type)
  (let ((token (edit-structure-template-first-token type)))
    (string= token (string-upcase token))))

(defun edit-structure-template-extended-p (type)
  (member (string-downcase type) '("src" "export") :test #'string=))

(defun edit-structure-template-verbatim-p (type)
  (let* ((name (string-downcase type))
         (length (length name)))
    (some (lambda (prefix)
            (and (>= length (length prefix))
                 (string= prefix name :end2 (length prefix))))
          '("example" "export" "src" "comment"))))

(defun edit-structure-template-line-start (source offset)
  (let ((newline (position #\Newline source :end offset :from-end t)))
    (if newline (1+ newline) 0)))

(defun edit-structure-template-line-end (source start)
  (let ((newline (position #\Newline source :start start)))
    (if (and newline (> newline start)
             (char= (char source (1- newline)) #\Return))
        (1- newline)
        (or newline (length source)))))

(defun edit-structure-template-newline (source offset)
  (let* ((previous (and (plusp offset)
                        (position #\Newline source :end offset :from-end t)))
         (next (position #\Newline source :start offset)))
    (cond
      ((and previous (plusp previous)
            (char= (char source (1- previous)) #\Return))
       (format nil "~C~C" #\Return #\Newline))
      (previous (string #\Newline))
      ((and next (plusp next)
            (char= (char source (1- next)) #\Return))
       (format nil "~C~C" #\Return #\Newline))
      (next (string #\Newline))
      (t (string #\Newline)))))

(defun edit-structure-template-indent (source line-start line-end)
  (let ((indent-end
          (loop for position from line-start below line-end
                while (edit-structure-template-horizontal-space-p
                       (char source position))
                finally (return position))))
    (make-string (- indent-end line-start) :initial-element #\Space)))

(defun edit-structure-template-escape-body (body)
  (with-output-to-string (stream)
    (loop with cursor = 0
          with length = (length body)
          while (< cursor length)
          do (let* ((indent-end
                      (loop for position from cursor below length
                            while (and (not (char= (char body position) #\Newline))
                                       (edit-structure-template-horizontal-space-p
                                        (char body position)))
                            finally (return position)))
                    (marker-end
                      (loop for position from indent-end below length
                            while (and (not (char= (char body position) #\Newline))
                                       (char= (char body position) #\,))
                            finally (return position)))
                    (escape-p
                      (and (< marker-end length)
                           (or (char= (char body marker-end) #\*)
                               (and (char= (char body marker-end) #\#)
                                    (< (1+ marker-end) length)
                                    (char= (char body (1+ marker-end)) #\+)))))
                    (newline (position #\Newline body :start cursor)))
               (if escape-p
                   (progn
                     (write-string body stream :start cursor :end marker-end)
                     (write-char #\, stream)
                     (write-string body stream :start marker-end
                                        :end (if newline (1+ newline) length))
                     (setf cursor (if newline (1+ newline) length)))
                   (progn
                     (write-string body stream :start cursor
                                        :end (if newline (1+ newline) length))
                     (setf cursor (if newline (1+ newline) length))))))))

(defun edit-structure-template-trimmed-body (body)
  (let ((end (length body)))
    (loop while (and (plusp end)
                     (find (char body (1- end))
                           '(#\Space #\Tab #\Return #\Newline)))
          do (decf end))
    (values (subseq body 0 end) (subseq body end))))

(defun edit-structure-template-open-line (type uppercase-p)
  (format nil "#+~A_~A~A"
          (if uppercase-p "BEGIN" "begin")
          type
          (if (edit-structure-template-extended-p type) " " "")))

(defun edit-structure-template-close-line (type uppercase-p)
  (format nil "#+~A_~A" (if uppercase-p "END" "end")
          (edit-structure-template-first-token type)))

(defun edit-structure-template-empty-patch (source offset type newline target)
  (let* ((line-start (edit-structure-template-line-start source offset))
         (line-end (edit-structure-template-line-end source line-start))
         (prefix-blank-p
           (loop for position from line-start below offset
                 always (edit-structure-template-horizontal-space-p
                         (char source position))))
         (indent (edit-structure-template-indent source line-start line-end))
         (uppercase-p (edit-structure-template-uppercase-p type))
         (opening (edit-structure-template-open-line type uppercase-p))
         (closing (edit-structure-template-close-line type uppercase-p))
         (insert-at (if prefix-blank-p line-start offset))
         (prefix (if prefix-blank-p "" newline))
         (replacement
           (concatenate 'string prefix indent opening newline
                        indent closing newline)))
    (edit-make-patch insert-at insert-at replacement target)))

(defun edit-structure-template-wrap-patch (source start end type newline target)
  (let* ((raw-body (subseq source start end))
         (uppercase-p (edit-structure-template-uppercase-p type))
         (line-start (edit-structure-template-line-start source start))
         (line-end (edit-structure-template-line-end source line-start))
         (indent (edit-structure-template-indent source line-start line-end))
         (opening (edit-structure-template-open-line type uppercase-p))
         (closing (edit-structure-template-close-line type uppercase-p)))
    (multiple-value-bind (body trailing)
        (edit-structure-template-trimmed-body raw-body)
      (let* ((escaped-body
               (if (edit-structure-template-verbatim-p type)
                   (edit-structure-template-escape-body body)
                   body))
             (trimmed-trailing
               (let ((start 0))
                 (loop while (and (< start (length trailing))
                                  (edit-structure-template-horizontal-space-p
                                   (char trailing start)))
                       do (incf start))
                 (subseq trailing start)))
             (body-ending
               (if (plusp (length escaped-body)) newline ""))
             (close-ending
               (if (or (plusp (length trimmed-trailing))
                       (and (< end (length source))
                            (char= (char source end) #\Newline)))
                   ""
                   newline))
             (replacement
               (concatenate 'string indent opening newline escaped-body
                            body-ending indent closing close-ending
                            trimmed-trailing)))
        (edit-make-patch start end replacement target)))))

(defun insert-structure-template (document span type)
  "Wrap SPAN in a structure block of TYPE, or insert an empty block for a zero-width span."
  (let* ((target "structure template")
         (source (document-text (edit-structure-template-check-document document))))
    (edit-structure-template-validate-span document span target)
    (let* ((normalized-type (edit-structure-template-normalize-type type target))
           (start (edit-span-start span))
           (end (edit-span-end span))
           (newline (edit-structure-template-newline source start))
           (patch
             (if (= start end)
                 (edit-structure-template-empty-patch
                  source start normalized-type newline target)
                 (edit-structure-template-wrap-patch
                  source start end normalized-type newline target)))
           (prepared (edit-prepare-patches document (list patch) :target target))
           (new-source
             (edit-splice-source
              source (edit-prepared-patches-patches prepared)))
           (preview (edit-reparse-source document new-source target)))
      (edit-apply-patches-result
       document (list patch) :target target
       :expected (edit-canonical-document preview)))))

;;;; src/capture-template.lisp
;;;; Caller-supplied Org capture template expansion.
(in-package #:org-kit)

(defun capture-signal (reason context message)
  (error 'org-capture-error
         :reason reason
         :context (cond ((stringp context) (bounded-text context))
                        ((symbolp context) context)
                        (t nil))
         :message message))

(defstruct (capture-template-result
            (:constructor make-capture-template-result
                (&key expanded-text cursor-offset))
            (:conc-name %capture-template-result-)
            (:copier nil)
            (:predicate capture-template-result-p))
  "Expanded capture text and its first cursor offset.
CURSOR-OFFSET is zero when the template has no %? marker."
  expanded-text
  cursor-offset)

(defun capture-template-result-expanded-text (result)
  "Return RESULT's fully expanded template text."
  (%capture-template-result-expanded-text result))

(defun capture-template-result-cursor-offset (result)
  "Return the first cursor offset, or zero when the template has no cursor."
  (%capture-template-result-cursor-offset result))

(defun capture-template-utf8-length (text)
  (length (cl-codec-kit:string-to-octets text :encoding :utf-8)))

(defun capture-template-assoc-value (key alist)
  (let ((entry (and (listp alist) (assoc key alist :test #'equal))))
    (values (and entry (cdr entry)) (not (null entry)))))

(defun capture-template-required-string (value context)
  (unless (stringp value)
    (capture-signal :invalid-template-value context
                    "Capture template values must be strings"))
  value)

(defun capture-template-format-org-time (time-value time-context with-time inactive-p)
  (let* ((value (%time-value-adapt time-value))
         (zone (org-time-context-zone time-context))
         (instant (%time-value-to-instant
                   value zone (org-time-context-dst-policy time-context)))
         (local (cl-date-kit:local-date-time-of-instant
                 instant (org-time-zone-backend zone)))
         (date (cl-date-kit:local-date-time-date local))
         (timestamp (edit-todo-format-point-timestamp
                     (if with-time local date))))
    (format nil "~A~A~A"
            (if inactive-p "[" "<")
            timestamp
            (if inactive-p "]" ">"))))

(defun capture-template-format-reference-time (time-context token)
  (let* ((value (org-time-context-reference-time time-context))
         (with-time (member token '(#\T #\U)))
         (inactive-p (member token '(#\u #\U))))
    (capture-template-format-org-time value time-context with-time inactive-p)))

(defun capture-template-format-prompted-time (value time-context)
  (cond
    ((stringp value)
     (if (and (>= (length value) 2)
              (member (char value 0) '(#\[ #\<))
              (member (char value (1- (length value))) '(#\] #\>)))
         value
         (handler-case
             (let* ((parsed (make-org-time-value value))
                    (with-time (eq (org-time-value-kind parsed) :date-time)))
               (capture-template-format-org-time parsed time-context with-time nil))
           (org-time-error ()
             (capture-signal :invalid-time-value value
                             "Prompted time is not a valid Org time")))))
    ((org-time-value-p value)
     (capture-template-format-org-time
      value time-context (eq (org-time-value-kind value) :date-time) nil))
    ((cl-date-kit:local-date-p value)
     (capture-template-format-org-time value time-context nil nil))
    ((cl-date-kit:local-date-time-p value)
     (capture-template-format-org-time value time-context t nil))
    (t (capture-signal :invalid-time-value value
                       "Prompted time is not a valid Org time"))))

(defun capture-template-tags-text (value)
  (let ((items (cond ((stringp value)
                      (list (string-trim '(#\: #\Space #\Tab) value)))
                     ((and (listp value) (every #'stringp value)) value)
                     (t (capture-signal :invalid-template-value :tags
                                        "Tag answer must be a string or string list")))))
    (setf items (remove "" items :test #'string=))
    (if (null items)
        ""
        (with-output-to-string (stream)
          (write-char #\: stream)
          (loop for item in items
                for first-p = t then nil
                do (unless first-p (write-char #\: stream))
                   (write-string (string-trim '(#\: #\Space #\Tab) item) stream))
          (write-char #\: stream)))))

(defun capture-template-expression-end (text start)
  (let ((depth 0)
        (index start)
        (in-string-p nil)
        (in-comment-p nil)
        (escaped-p nil))
    (loop while (< index (length text))
          for character = (char text index)
          do (cond
               (escaped-p
                (setf escaped-p nil)
                (incf index))
               (in-string-p
                (cond ((char= character #\\) (setf escaped-p t))
                      ((char= character #\") (setf in-string-p nil)))
                (incf index))
               (in-comment-p
                (when (char= character #\Newline)
                  (setf in-comment-p nil))
                (incf index))
               ((char= character #\;)
                (setf in-comment-p t)
                (incf index))
               ((char= character #\")
                (setf in-string-p t)
                (incf index))
               ((char= character #\()
                (incf depth)
                (incf index))
               ((char= character #\))
                (decf depth)
                (incf index)
                (when (zerop depth)
                  (return index)))
               (t (incf index))))))

(defun capture-template-indent-initial (initial prefix)
  (with-output-to-string (stream)
    (loop for index from 0 below (length initial)
          for character = (char initial index)
          do (write-char character stream)
             (when (and (char= character #\Newline)
                       (< index (1- (length initial))))
               (write-string prefix stream)))))

(defun expand-capture-template
    (template &key (initial nil initial-p) (annotation nil annotation-p)
                  time-context prompt-answers
                  (tag-answer nil tag-answer-p)
                  (prompted-time nil prompted-time-p)
                  file-contents callbacks limits)
  "Expand the supported capture placeholders with caller-provided values.
Return a CAPTURE-TEMPLATE-RESULT; no prompts or file reads are performed."
  (unless (stringp template)
    (capture-signal :invalid-template nil "Capture template must be a string"))
  (let ((effective-limits (or limits (make-limits)))
        (cursor-offset nil)
        (output-length 0)
        (line-prefix ""))
    (unless (limits-p effective-limits)
      (capture-signal :invalid-limits nil "Expected a limits object"))
    (unless (org-time-context-p time-context)
      (capture-signal :invalid-time-context :time-context
                      "Capture expansion requires an explicit time context"))
    (check-limit effective-limits :max-input-length (length template))
    (labels ((emit (stream text)
               (unless (stringp text)
                 (capture-signal :invalid-template-value nil
                                 "Expanded capture values must be strings"))
               (let ((next-length (+ output-length (length text))))
                 (check-limit effective-limits :max-output-length next-length)
                 (setf output-length next-length)
                 (write-string text stream)
                 (let ((newline (position #\Newline text :from-end t)))
                   (setf line-prefix
                         (if newline
                             (subseq text (1+ newline))
                             (concatenate 'string line-prefix text))))))
             (missing (kind context)
               (capture-signal :missing-value context
                               (format nil "Missing capture value for ~A" kind)))
             (answer (prompt)
               (multiple-value-bind (value found-p)
                   (capture-template-assoc-value prompt prompt-answers)
                 (unless found-p (missing :prompt prompt))
                 (capture-template-required-string value prompt)))
             (token-at (index)
               (and (< (1+ index) (length template))
                    (char template (1+ index)))))
      (let ((expanded
              (with-output-to-string (stream)
                (loop with index = 0
                      while (< index (length template))
                      for character = (char template index)
                      do (if (or (not (char= character #\%))
                                 (null (token-at index)))
                             (progn (emit stream (string character))
                                    (incf index))
                             (let ((token (token-at index)))
                               (cond
                                 ((char= token #\?)
                                  (unless cursor-offset
                                    (setf cursor-offset output-length))
                                  (incf index 2))
                                 ((char= token #\i)
                                  (unless initial-p (missing :initial :initial))
                                  (emit stream
                                        (capture-template-indent-initial
                                         (capture-template-required-string
                                          initial :initial)
                                         line-prefix))
                                  (incf index 2))
                                 ((char= token #\a)
                                  (unless annotation-p
                                    (missing :annotation :annotation))
                                  (emit stream
                                        (capture-template-required-string
                                         annotation :annotation))
                                  (incf index 2))
                                 ((member token '(#\t #\T #\u #\U))
                                  (emit stream
                                        (capture-template-format-reference-time
                                         time-context token))
                                  (incf index 2))
                                 ((and (char= token #\^)
                                       (< (+ index 2) (length template))
                                       (char= (char template (+ index 2)) #\{))
                                  (let ((end (position #\} template
                                                       :start (+ index 3))))
                                    (unless end
                                      (capture-signal :invalid-template
                                                      (subseq template index)
                                                      "Unclosed capture prompt"))
                                    (emit stream
                                          (answer (subseq template (+ index 3) end)))
                                    (setf index (1+ end))))
                                 ((and (char= token #\^)
                                       (< (+ index 2) (length template))
                                       (char= (char template (+ index 2)) #\g))
                                  (unless tag-answer-p (missing :tags :tags))
                                  (emit stream (capture-template-tags-text tag-answer))
                                  (incf index 3))
                                 ((and (char= token #\^)
                                       (< (+ index 2) (length template))
                                       (char= (char template (+ index 2)) #\t))
                                  (unless prompted-time-p
                                    (missing :prompted-time :prompted-time))
                                  (emit stream
                                        (capture-template-format-prompted-time
                                         prompted-time time-context))
                                  (incf index 3))
                                 ((char= token #\[)
                                  (let ((end (position #\] template
                                                       :start (+ index 2))))
                                    (unless end
                                      (capture-signal :invalid-template
                                                      (subseq template index)
                                                      "Unclosed capture file placeholder"))
                                    (let ((path (subseq template (+ index 2) end)))
                                      (multiple-value-bind (value found-p)
                                          (capture-template-assoc-value
                                           path file-contents)
                                        (unless found-p (missing :file path))
                                        (emit stream
                                              (capture-template-required-string
                                               value path))))
                                    (setf index (1+ end))))
                                 ((char= token #\()
                                  (let ((end (capture-template-expression-end
                                              template (1+ index))))
                                    (unless end
                                      (capture-signal :invalid-template
                                                      (subseq template index)
                                                      "Unbalanced Emacs Lisp placeholder"))
                                    (let ((value
                                            (evaluate-elisp
                                             callbacks
                                             (subseq template (1+ index) end))))
                                      (cond ((null value) nil)
                                            ((stringp value) (emit stream value))
                                            (t (capture-signal
                                                :invalid-callback-result :evaluator
                                                "Emacs Lisp evaluator must return a string or NIL")))
                                    (setf index end))))
                                 ((char= token #\^)
                                  (emit stream (string character))
                                  (incf index))
                                 (t
                                  (emit stream (string character))
                                  (incf index)))))))))
        (check-limit effective-limits :max-output-length (length expanded))
        (check-limit effective-limits :max-output-octets
                     (capture-template-utf8-length expanded))
        (make-capture-template-result
         :expanded-text expanded
         :cursor-offset (or cursor-offset 0))))))

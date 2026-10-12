;;;; src/object-markup.lisp
;;;;
;;;; Recognition for Org's delimited markup objects.
(in-package #:org-kit)

(defun markup-whitespace-character-p (character)
  (let ((code (char-code character)))
    (or (<= 9 code 13)
        (= code 32)
        (= code 133)
        (= code 160)
        (= code 5760)
        (<= 8192 code 8202)
        (member code '(8232 8233 8239 8287 12288)))))

(defun markup-open-boundary-p (text start)
  (or
   (and (integerp *object-parse-range-start*)
        (= start *object-parse-range-start*))
   (zerop start)
   (let ((previous (char text (1- start))))
     (or (markup-whitespace-character-p previous) (find previous "-(' \"{")))))

(defun markup-close-boundary-p (text after-mark end)
  (or (= after-mark end)
      (let ((following (char text after-mark)))
        (or (markup-whitespace-character-p following)
            (member following
                    '(#\- #\. #\, #\; #\: #\! #\? #\' #\" #\) #\} #\\ #\[))))))

(defun markup-type-for-delimiter (delimiter)
  (case delimiter
    (#\* :bold)
    (#\/ :italic)
    (#\_ :underline)
    (#\+ :strike-through)
    (#\~ :code)
    (#\= :verbatim)))

(defun markup-post-blank-end (text position end)
  (loop while (and (< position end)
                   (member (char text position) '(#\Space #\Tab)))
        do (incf position))
  position)

(defun try-parse-markup-object (document start end container-type)
  (declare (ignore container-type))
  (let* ((text (document-text document))
         (delimiter (char text start))
         (type (and delimiter (markup-type-for-delimiter delimiter))))
    (when
        (and type
             (markup-open-boundary-p text start)
             (< (1+ start) end)
             (not (markup-whitespace-character-p (char text (1+ start)))))
      (let ((closing
             (loop for position from (1+ start) below
                   end
                   when (and (char= (char text position) delimiter)
                             (not
                              (markup-whitespace-character-p
                               (char text (1- position))))
                             (markup-close-boundary-p text (1+ position) end))
                     return position)))
        (when closing
          (let* ((opaque-p (not (null (member type '(:code :verbatim)))))
                 (post-blank-end (markup-post-blank-end text (1+ closing) end)))
            (%make-object-match :type
                                type
                                :end
                                post-blank-end
                                :contents-begin
                                (unless opaque-p
                                  (1+ start))
                                :contents-end
                                (unless opaque-p
                                  closing)
                                :post-blank
                                (- post-blank-end (1+ closing))
                                :properties
                                nil
                                :opaque-p
                                opaque-p)))))))

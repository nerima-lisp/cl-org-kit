;;;; src/object-links.lisp
;;;;
;;;; Recognition for links and their secondary objects.
(in-package #:org-kit)

(defparameter *org-link-object-types*
  '("elisp" "file"
            "file+emacs"
            "file+sys"
            "ftp"
            "help"
            "http"
            "https"
            "mailto"
            "news"
            "shell"
            "shortdoc")
  "Org link protocols understood without user-defined link registrations.")

(defun link-object-prefix-at-p (text start end prefix)
  (and (<= (+ start (length prefix)) end)
       (string= prefix text :start2 start :end2 (+ start (length prefix)))))

(defun link-object-whitespace-p (character)
  (member character '(#\Space #\Tab #\Newline #\Return)))

(defun link-object-post-blank-end (text position end)
  (loop while (and (< position end)
                   (member (char text position) '(#\Space #\Tab)))
        do (incf position))
  position)

(defun link-object-match (type end
                               &key
                               contents-begin
                               contents-end
                               (post-blank 0)
                               properties)
  (%make-object-match :type
                      type
                      :end
                      end
                      :contents-begin
                      contents-begin
                      :contents-end
                      contents-end
                      :post-blank
                      post-blank
                      :properties
                      properties
                      :opaque-p
                      nil))

(defun link-object-boundary-before-p (text start)
  (or (zerop start)
      (and (integerp *object-parse-range-start*)
           (= start *object-parse-range-start*))
      (not
       (or (alphanumericp (char text (1- start)))
           (char= (char text (1- start)) #\_)))))

(defun link-object-boundary-after-p (text position end)
  (or (= position end)
      (let ((character (char text position)))
        (not (or (alphanumericp character) (char= character #\_))))))

(defun link-object-radio-link (document start end)
  (let* ((context (%document-object-context document))
         (targets
          (and context (org-kit::%object-context-radio-targets context)))
         (text (document-text document)))
    (when targets
      (let ((target
             (loop with longest = nil
                   for candidate in targets
                   for candidate-end = (+ start (length candidate))
                   when (and (plusp (length candidate))
                             (<= candidate-end end)
                             (string-equal candidate
                                           text
                                           :start2
                                           start
                                           :end2
                                           candidate-end)
                             (link-object-boundary-before-p text start)
                             (link-object-boundary-after-p text
                                                           candidate-end
                                                           end))
                     do (when
                            (or (null longest)
                                (> (length candidate) (length longest)))
                          (setf longest candidate))
                   finally (return longest))))
        (when target
          (let* ((target-end (+ start (length target)))
                 (post-blank-end
                  (link-object-post-blank-end text target-end end))
                 (path (copy-seq target)))
            (link-object-match :link
                               post-blank-end
                               :contents-begin
                               start
                               :contents-end
                               target-end
                               :post-blank
                               (- post-blank-end target-end)
                               :properties
                               (list :type "radio" :path path :format :plain))))))))

(defun link-object-target-boundary-p (text start end opening closing)
  (let* ((inner-start (+ start (length opening)))
         (inner-end (- end (length closing))))
    (and (< inner-start inner-end)
         (let ((first (char text inner-start))
               (last (char text (1- inner-end))))
           (and (not (member first '(#\< #\> #\Space #\Tab #\Newline #\Return)))
                (not (member last '(#\< #\> #\Space #\Tab #\Newline #\Return)))
                (loop for position from inner-start below inner-end
                      never (member (char text position)
                                    '(#\< #\> #\Newline #\Return))))))))

(defun link-object-radio-target-shadowed-p (text start end)
  (loop with position = (if (integerp *object-parse-range-start*)
                            *object-parse-range-start*
                            0)
        for opener = (search "<<<" text :start2 position :end2 start)
        while opener
        for close = (search ">>>" text :start2 (+ opener 3) :end2 end)
        when (or (null close) (>= close start))
          return t
        do (setf position (+ close 3))
        finally (return nil)))

(defun link-object-double-angle (document start end radio-p)
  (let* ((text (document-text document))
         (opening
          (if radio-p
              "<<<"
              "<<"))
         (closing
          (if radio-p
              ">>>"
              ">>")))
    (when
        (and (link-object-prefix-at-p text start end opening)
             (or (not radio-p)
                 (not (link-object-radio-target-shadowed-p text start end))))
      (let ((close
             (search closing text :start2 (+ start (length opening)) :end2 end)))
        (when
            (and close
                 (link-object-target-boundary-p text
                                                start
                                                (+ close (length closing))
                                                opening
                                                closing))
          (let* ((object-end (+ close (length closing)))
                 (post-blank-end
                  (link-object-post-blank-end text object-end end))
                 (value (subseq text (+ start (length opening)) close)))
            (link-object-match
             (if radio-p
                 :radio-target
                 :target)
             post-blank-end
             :contents-begin
             (and radio-p (+ start (length opening)))
             :contents-end
             (and radio-p close)
             :post-blank
             (- post-blank-end object-end)
             :properties
             (list :value value))))))))

(defun link-object-expand-abbreviation (document link)
  (let* ((separator (position #\: link))
         (key
          (if separator
              (subseq link 0 separator)
              link))
         (tail
          (when separator
            (subseq link
                    (+ separator
                       (if (and (< (1+ separator) (length link))
                                (char= (char link (1+ separator)) #\:))
                           2
                           1)))))
         (abbreviations (getf (document-settings document) :link-abbrevs))
         (entry
          (and (listp abbreviations) (assoc key abbreviations :test #'string=)))
         (replacement (and entry (cdr entry))))
    (if (not (stringp replacement))
        link
        (let ((placeholder (search "%s" replacement)))
          (if placeholder
              (concatenate 'string
                           (subseq replacement 0 placeholder)
                           (or tail "")
                           (subseq replacement (+ placeholder 2)))
              (concatenate 'string replacement (or tail "")))))))

(defun link-object-known-scheme-end (link)
  (let ((colon (position #\: link)))
    (when
        (and colon
             (> colon 0)
             (alpha-char-p (char link 0))
             (loop for position from 1 below colon
                   always (let ((character (char link position)))
                            (or (alphanumericp character)
                                (find character "+.-")))))
      (let ((type (string-downcase (subseq link 0 colon))))
        (when (member type *org-link-object-types* :test #'string=)
          (values type (subseq link (1+ colon))))))))

(defun link-object-classify-bracket-path (document path)
  (let* ((expanded (link-object-expand-abbreviation document path))
         (scheme-values
          (multiple-value-list (link-object-known-scheme-end expanded))))
    (cond
      ((or (and (plusp (length expanded)) (char= (char expanded 0) #\/))
           (and (>= (length expanded) 2) (string= "./" expanded :end2 2))
           (and (>= (length expanded) 3) (string= "../" expanded :end2 3)))
       (values "file" expanded))
      ((first scheme-values)
       (values (first scheme-values) (second scheme-values)))
      ((and (> (length expanded) 2)
            (char= (char expanded 0) #\()
            (char= (char expanded (1- (length expanded))) #\)))
       (values "coderef" (subseq expanded 1 (1- (length expanded)))))
      ((and (plusp (length expanded)) (char= (char expanded 0) #\#))
       (values "custom-id" (subseq expanded 1)))
      (t (values "fuzzy" expanded)))))

(defun link-object-unescape-path (path)
  (with-output-to-string (output)
    (loop with position = 0
          while (< position (length path))
          for character = (char path position)
          do (if (and (char= character #\\)
                      (< (1+ position) (length path))
                      (find (char path (1+ position)) "[]\\"))
                 (progn
                   (write-char (char path (1+ position)) output)
                   (incf position 2))
                 (progn
                   (write-char character output)
                   (incf position))))))

(defun link-object-normalize-bracket-path (path)
  (with-output-to-string (output)
    (loop with position = 0
          with horizontal = nil
          while (< position (length path))
          for character = (char path position)
          do (cond
               ((member character '(#\Space #\Tab)) (push character horizontal))
               ((char= character #\Newline)
                 (write-char #\Space output)
                 (setf horizontal nil)
                 (loop while (and (< (1+ position) (length path))
                                  (member (char path (1+ position))
                                          '(#\Space #\Tab)))
                       do (incf position)))
               (t
                 (dolist (pending (nreverse horizontal))
                   (write-char pending output))
                 (setf horizontal nil)
                 (write-char character output))) (incf position)
          finally (dolist (pending (nreverse horizontal))
                    (write-char pending output)))))

(defun link-object-bracket-close (text start end)
  (loop with position = start
        with escaped = nil
        while (< position end)
        for character = (char text position)
        do (cond
             (escaped
              (setf escaped nil))
             ((char= character #\\)
              (setf escaped t))
             ((char= character #\]) (return position))) (incf position)))

(defun link-object-bracket-description-close (text start end)
  (loop with position = start
        with depth = 0
        with escaped = nil
        while (< position (1- end))
        for character = (char text position)
        do (cond
             (escaped
              (setf escaped nil))
             ((char= character #\\)
              (setf escaped t))
             ((char= character #\[) (incf depth))
             ((char= character #\])
              (if (plusp depth)
                  (decf depth)
                  (when (char= (char text (1+ position)) #\])
                    (return position))))
             (t nil)) (incf position)))

(defun link-object-bracket (document start end)
  (let* ((text (document-text document))
         (path-start (+ start 2)))
    (when (link-object-prefix-at-p text start end "[[")
      (let ((path-end (link-object-bracket-close text path-start end)))
        (when (and path-end (> path-end path-start))
          (let ((after-path (1+ path-end)))
            (multiple-value-bind (link-end contents-begin contents-end)
                (cond
                  ((and (< after-path end) (char= (char text after-path) #\]))
                   (values (1+ after-path) nil nil))
                  ((and (< after-path end) (char= (char text after-path) #\[))
                   (let* ((description-start (1+ after-path))
                          (description-end
                           (link-object-bracket-description-close text
                                                                  description-start
                                                                  end)))
                     (when
                         (and description-end
                              (> description-end description-start))
                       (values (+ description-end 2)
                               description-start
                               description-end))))
                  (t (values nil nil nil)))
              (when link-end
                (let* ((raw
                        (link-object-normalize-bracket-path
                         (link-object-unescape-path
                          (subseq text path-start path-end))))
                       (post-blank-end
                        (link-object-post-blank-end text link-end end)))
                  (multiple-value-bind (type path)
                      (link-object-classify-bracket-path document raw)
                    (link-object-match :link
                                       post-blank-end
                                       :contents-begin
                                       contents-begin
                                       :contents-end
                                       contents-end
                                       :post-blank
                                       (- post-blank-end link-end)
                                       :properties
                                       (list :type
                                             type
                                             :path
                                             path
                                             :format
                                             :bracket))))))))))))

(defun link-object-plain-path-end (text path-start end)
  (let ((position path-start)
        (depth 0))
    (loop while (< position end)
          for character = (char text position)
          while (and (not (link-object-whitespace-p character))
                     (not (find character "[]<>"))
                     (not (and (char= character #\)) (zerop depth))))
          do (cond
               ((char= character #\() (incf depth))
               ((char= character #\)) (decf depth))) (incf position))
    (loop while (> position path-start)
          for character = (char text (1- position))
          while (find character ".,;:!?\"'!@#$%^&*_+=|{}")
          do (decf position))
    (when (> position path-start)
      position)))

(defun link-object-plain-link (document start end)
  (let ((text (document-text document)))
    (when
        (and (link-object-boundary-before-p text start)
             (alpha-char-p (char text start)))
      (let ((colon
             (loop for position from (1+ start) below
                   end
                   for character = (char text position)
                   when (char= character #\:)
                     return position
                   while (or (alphanumericp character) (find character "+.-")))))
        (when colon
          (let* ((type (string-downcase (subseq text start colon)))
                 (path-start (1+ colon)))
            (when
                (and (member type *org-link-object-types* :test #'string=)
                     (< path-start end)
                     (not (link-object-whitespace-p (char text path-start))))
              (let ((path-end (link-object-plain-path-end text path-start end)))
                (when path-end
                  (let* ((link-end path-end)
                         (post-blank-end
                          (link-object-post-blank-end text link-end end))
                         (path (subseq text path-start path-end)))
                    (link-object-match :link
                                       post-blank-end
                                       :post-blank
                                       (- post-blank-end link-end)
                                       :properties
                                       (list :type
                                             type
                                             :path
                                             path
                                             :format
                                             :plain))))))))))))

(defun link-object-angle-link (document start end)
  (let* ((text (document-text document))
         (type-start (1+ start)))
    (when
        (and (char= (char text start) #\<)
             (< type-start end)
             (alpha-char-p (char text type-start)))
      (let* ((colon (position #\: text :start type-start :end end))
             (close (and colon (position #\> text :start (1+ colon) :end end))))
        (when
            (and colon
                 close
                 (> colon type-start)
                 (not (position #\Newline text :start type-start :end close))
                 (not (position #\Return text :start type-start :end close)))
          (let* ((type (string-downcase (subseq text type-start colon)))
                 (path (subseq text (1+ colon) close))
                 (link-end (1+ close)))
            (when
                (and (member type *org-link-object-types* :test #'string=)
                     (plusp (length path)))
              (let ((post-blank-end
                     (link-object-post-blank-end text link-end end)))
                (link-object-match :link
                                   post-blank-end
                                   :post-blank
                                   (- post-blank-end link-end)
                                   :properties
                                   (list :type type :path path :format :angle))))))))))

(defun link-object-macro-arguments (string)
  (let ((normalized
         (with-output-to-string (output)
           (loop with position = 0
                 with wrote-character = nil
                 with pending-space = nil
                 while (< position (length string))
                 for character = (char string position)
                 do (if (link-object-whitespace-p character)
                        (setf pending-space t)
                        (progn
                          (when (and pending-space wrote-character)
                            (write-char #\Space output))
                          (setf pending-space nil)
                          (write-char character output)
                          (setf wrote-character t))) (incf position)))))
    (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) normalized)))
      (unless (zerop (length trimmed))
        (loop with start = 0
              for comma = (position #\, trimmed :start start)
              collect (string-trim '(#\Space #\Tab)
                                   (subseq trimmed start comma))
              while comma
              do (setf start (1+ comma)))))))

(defun link-object-macro (document start end)
  (let* ((text (document-text document))
         (body-start (+ start 3)))
    (when (link-object-prefix-at-p text start end "{{{")
      (let ((close (search "}}}" text :start2 body-start :end2 end)))
        (when
            (and close
                 (< body-start close)
                 (alpha-char-p (char text body-start)))
          (let* ((key-end
                  (loop for position from (1+ body-start) below close
                        for character = (char text position)
                        while (or (alphanumericp character)
                                  (find character "-_"))
                        finally (return position)))
                 (key (string-downcase (subseq text body-start key-end)))
                 (suffix-start key-end)
                 (args nil))
            (when
                (and (< suffix-start close)
                     (char= (char text suffix-start) #\())
              (when
                (char= (char text (1- close)) #\))
                (setf args (link-object-macro-arguments
                            (subseq text (1+ suffix-start) (1- close))))
                (setf suffix-start close)))
            (when (= suffix-start close)
              (let* ((object-end (+ close 3))
                     (post-blank-end
                      (link-object-post-blank-end text object-end end)))
                (link-object-match :macro
                                   post-blank-end
                                   :post-blank
                                   (- post-blank-end object-end)
                                   :properties
                                   (list :key
                                         key
                                         :args
                                         args
                                         :value
                                         (subseq text start object-end)))))))))))

(defun link-object-export-snippet (document start end)
  (let* ((text (document-text document))
         (backend-start (+ start 2))
         (colon (position #\: text :start backend-start :end end)))
    (when
        (and (link-object-prefix-at-p text start end "@@")
             colon
             (> colon backend-start)
             (loop for position from backend-start below colon
                   always (let ((character (char text position)))
                            (or (alpha-char-p character)
                                (digit-char-p character)
                                (char= character #\-))))
             (< (1+ colon) end))
      (let ((close (search "@@" text :start2 (1+ colon) :end2 end)))
        (when close
          (let* ((object-end (+ close 2))
                 (post-blank-end
                  (link-object-post-blank-end text object-end end)))
            (link-object-match :export-snippet
                               post-blank-end
                               :post-blank
                               (- post-blank-end object-end)
                               :properties
                               (list :back-end
                                     (subseq text backend-start colon)
                                     :value
                                     (subseq text (1+ colon) close)))))))))

(defun link-object-find-close-bracket (text start end)
  (loop with position = start
        with depth = 1
        with escaped = nil
        while (< position end)
        for character = (char text position)
        do (cond
             (escaped
              (setf escaped nil))
             ((char= character #\\)
              (setf escaped t))
             ((char= character #\[) (incf depth))
             ((char= character #\])
               (decf depth)
               (when (zerop depth)
                 (return position)))) (incf position)))

(defun link-object-footnote (document start end)
  (let* ((text (document-text document))
         (prefix-start (+ start 4)))
    (when (link-object-prefix-at-p text start end "[fn:")
      (let* ((position prefix-start)
             (label-start position)
             (label-end
              (loop while (and (< position end)
                               (or (alphanumericp (char text position))
                                   (find (char text position) "-_")))
                    do (incf position)
                    finally (return position)))
             (label
              (and (> label-end label-start)
                   (subseq text label-start label-end)))
             (inline-p (and (< position end) (char= (char text position) #\:)))
             (standard-p
              (and label (< position end) (char= (char text position) #\]))))
        (when (or inline-p standard-p)
          (let* ((contents-begin (and inline-p (1+ position)))
                 (close
                  (link-object-find-close-bracket text
                                                  (if inline-p
                                                      contents-begin
                                                      position)
                                                  end)))
            (when (and close (or standard-p (> close contents-begin)))
              (let* ((object-end (1+ close))
                     (post-blank-end
                      (link-object-post-blank-end text object-end end)))
                (link-object-match :footnote-reference
                                   post-blank-end
                                   :contents-begin
                                   (and inline-p contents-begin)
                                   :contents-end
                                   (and inline-p close)
                                   :post-blank
                                   (- post-blank-end object-end)
                                   :properties
                                   (list :label
                                         label
                                         :type
                                         (if inline-p
                                             :inline
                                             :standard)))))))))))

(defun link-object-citation-key-character-p (character)
  (or (alphanumericp character) (find character "-.:?!`'/*@+|(){}<>&_^$#%~")))

(defun link-object-citation-key-end (text start end)
  (when
      (and (< start end)
           (char= (char text start) #\@)
           (< (1+ start) end)
           (link-object-citation-key-character-p (char text (1+ start))))
    (loop for position from (+ start 2) below
          end
          while (link-object-citation-key-character-p (char text position))
          finally (return position))))

(defun link-object-find-closing-citation-bracket (text start end)
  (loop with position = (1+ start)
        with depth = 1
        with escaped = nil
        while (< position end)
        for character = (char text position)
        do (cond
             (escaped
              (setf escaped nil))
             ((char= character #\\)
              (setf escaped t))
             ((char= character #\[) (incf depth))
             ((char= character #\])
               (decf depth)
               (when (zerop depth)
                 (return position)))) (incf position)))

(defun link-object-citation (document start end)
  (let* ((text (document-text document))
         (position (+ start 5))
         (style nil))
    (when
        (and (char= (char text start) #\[)
             (link-object-prefix-at-p text start end "[cite"))
      (when (and (< position end) (char= (char text position) #\/))
        (let ((style-start (1+ position)))
          (setf position style-start)
          (loop while (and (< position end)
                           (or (alphanumericp (char text position))
                               (find (char text position) "/_-")))
                do (incf position))
          (when (> position style-start)
            (setf style (subseq text style-start position))))
        (unless style
          (return-from link-object-citation
            nil)))
      (when (and (< position end) (char= (char text position) #\:))
        (incf position)
        (loop while (and (< position end)
                         (member (char text position)
                                 '(#\Space #\Tab #\Newline)))
              do (incf position))
        (let* ((close
                (link-object-find-closing-citation-bracket text start end))
               (first-key-start
                (and close
                     (loop for cursor from position below close
                           when (link-object-citation-key-end text cursor close)
                             return cursor))))
          (when first-key-start
            (let* ((prefix-separator
                    (position #\;
                              text
                              :start
                              position
                              :end
                              first-key-start
                              :from-end
                              t))
                   (contents-begin
                    (if prefix-separator
                        (1+ prefix-separator)
                        position))
                   (body-end
                    (loop for cursor from (1- close) downto position
                          while (member (char text cursor)
                                        '(#\Space #\Tab #\Newline #\Return))
                          finally (return (1+ cursor))))
                   (last-separator
                    (and (> body-end position)
                         (position #\;
                                   text
                                   :start
                                   position
                                   :end
                                   body-end
                                   :from-end
                                   t)))
                   (key-after-separator
                    (and last-separator
                         (loop for cursor from (1+ last-separator) below body-end
                               when (link-object-citation-key-end text
                                                                  cursor
                                                                  body-end)
                                 return cursor)))
                   (contents-end
                    (if (and last-separator (not key-after-separator))
                        (1+ last-separator)
                        body-end))
                   (object-end (1+ close))
                   (post-blank-end
                    (link-object-post-blank-end text object-end end)))
              (link-object-match :citation
                                 post-blank-end
                                 :contents-begin
                                 contents-begin
                                 :contents-end
                                 contents-end
                                 :post-blank
                                 (- post-blank-end object-end)
                                 :properties
                                 (list :style style :key nil)))))))))

(defun link-object-citation-reference (document start end)
  (let* ((text (document-text document))
         (key-end (link-object-citation-key-end text start end)))
    (when key-end
      (let* ((separator (position #\; text :start key-end :end end))
             (reference-end
              (if separator
                  (1+ separator)
                  end))
             (suffix-end (or separator end))
             (suffix
              (and (< key-end suffix-end) (subseq text key-end suffix-end))))
        (link-object-match :citation-reference
                           reference-end
                           :properties
                           (list :key
                                 (subseq text (1+ start) key-end)
                                 :prefix
                                 nil
                                 :suffix
                                 (and suffix (list suffix))
                                 :locator
                                 nil))))))

(defun try-parse-link-object (document start end container-type)
  "Recognize a link or a link-related secondary object at START.
Return an OBJECT-MATCH or NIL; recursive children and restrictions are handled
by the shared object parser."
  (let* ((text (document-text document))
         (character (char text start)))
    (or
     (and (%object-type-allowed-p :link container-type)
          (link-object-radio-link document start end))
     (case character
       (#\[
        (or (link-object-citation document start end)
            (link-object-footnote document start end)
            (link-object-bracket document start end)))
       (#\<
        (or (link-object-double-angle document start end t)
            (link-object-angle-link document start end)
            (link-object-double-angle document start end nil)))
       (#\@
        (if (link-object-prefix-at-p text start end "@@")
            (link-object-export-snippet document start end)
            (link-object-citation-reference document start end)))
       (#\{ (link-object-macro document start end))
       (otherwise
        (cond
          ((alpha-char-p character)
           (link-object-plain-link document start end))))))))

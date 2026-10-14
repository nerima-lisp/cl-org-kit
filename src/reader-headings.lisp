;;;; src/reader-headings.lisp
(in-package #:org-kit)

(defun reader-heading-horizontal-space-p (character)
  (find character '(#\Space #\Tab)))

(defun reader-heading-skip-horizontal-space (text position limit)
  (loop while (and (< position limit)
                   (reader-heading-horizontal-space-p (char text position)))
        do (incf position)
        finally (return position)))

(defun reader-heading-trim-right (text start end)
  (loop while (and (> end start)
                   (reader-heading-horizontal-space-p (char text (1- end))))
        do (decf end)
        finally (return end)))

(defun reader-heading-words (text)
  (let ((words '())
        (word-start nil)
        (length (length text)))
    (loop for position from 0 to length
          do (if (or (= position length)
                     (reader-heading-horizontal-space-p (char text position)))
                 (when word-start
                   (push (subseq text word-start position) words)
                   (setf word-start nil))
                 (unless word-start
                   (setf word-start position))))
    (nreverse words)))

(defun reader-heading-settings-error (context offset control &rest arguments)
  (let ((message (apply #'format nil control arguments))
        (offset (or offset 0)))
    (if context
        (let ((document (reader-context-document context)))
          (multiple-value-bind (line column) (offset-line-column document offset)
            (error 'org-parse-error
                   :message message
                   :offset offset
                   :line line
                   :column column
                   :context (bounded-text (reader-line-text context offset))
                   :source-name (document-source-name document))))
        (error 'org-parse-error :message message :offset offset))))

(defun reader-heading-todo-declaration-info (source-key context offset)
  (when source-key
    (let ((normalized (string-upcase source-key)))
      (multiple-value-bind (kind base-length sequence-id-p)
          (org-setting-todo-declaration-prefix normalized)
        (when kind
          (if sequence-id-p
              (let* ((source-length (length source-key))
                     (id-start (1+ base-length))
                     (id-end (1- source-length)))
                (unless (and (> id-end id-start)
                             (char= (char source-key id-end) #\])
                             (loop for index from id-start below id-end
                                   for character = (char source-key index)
                                   always (not (find character
                                                    '(#\[ #\] #\Space #\Tab)))))
                  (reader-heading-settings-error
                   context offset "Malformed TODO sequence ID in ~A" source-key))
                (values kind (subseq source-key id-start id-end) t))
              (values kind nil t)))))))

(defun reader-heading-todo-name-valid-p (name)
  (and (plusp (length name))
       (not (find-if (lambda (character)
                       (or (find character '(#\Space #\Tab #\( #\) #\|))))
                     name))))

(defun reader-heading-todo-keyword (token context offset)
  (let* ((open (position #\( token))
         (close (position #\) token :from-end t))
         (name (if open (subseq token 0 open) token))
         (fast-key nil)
         (timestamp-p nil)
         (note-p nil)
         (exit-logging-p nil))
    (when (and (null open) close)
      (reader-heading-settings-error context offset
                                     "Unmatched TODO marker in ~A" token))
    (unless (reader-heading-todo-name-valid-p name)
      (reader-heading-settings-error context offset
                                     "Invalid or empty TODO keyword in ~A" token))
    (when open
      (unless (and close
                   (= close (1- (length token)))
                   (null (position #\( token :start (1+ open)))
                   (null (position #\) token :start (1+ open) :end close)))
        (reader-heading-settings-error context offset
                                       "Malformed TODO marker in ~A" token))
      (loop for marker across (subseq token (1+ open) close)
            do (case marker
                 (#\@
                  (when note-p
                    (reader-heading-settings-error context offset
                                                   "Duplicate @ marker in ~A" token))
                  (setf note-p t))
                 (#\!
                  (when timestamp-p
                    (reader-heading-settings-error context offset
                                                   "Duplicate ! marker in ~A" token))
                  (setf timestamp-p t))
                 (#\/
                  (when exit-logging-p
                    (reader-heading-settings-error context offset
                                                   "Duplicate / marker in ~A" token))
                  (setf exit-logging-p t))
                 (otherwise
                  (if fast-key
                      (reader-heading-settings-error
                       context offset "Malformed fast key in TODO keyword ~A" token)
                      (setf fast-key marker)))))
      (when (and (null fast-key)
                 (not note-p)
                 (not timestamp-p)
                 (not exit-logging-p))
        (reader-heading-settings-error context offset
                                       "Empty TODO marker in ~A" token)))
    (list :keyword name
          :logging-mode (cond (note-p :note)
                              (timestamp-p :timestamp)
                              (t :none))
          :exit-logging-p exit-logging-p
          :fast-key fast-key)))

(defun reader-heading-generated-sequence-id (declaration-kind declaration-index)
  (format nil "~(~A~)-~D" declaration-kind declaration-index))

(defun reader-heading-todo-sequence-entry (value declaration-kind sequence-id
                                           declaration-index context offset)
  (let* ((tokens (reader-heading-words value))
         (separators (loop for token in tokens
                           for index from 0
                           when (string= token "|")
                             collect index)))
    (when (null tokens)
      (reader-heading-settings-error context offset
                                     "TODO sequence has no keywords"))
    (when (> (length separators) 1)
      (reader-heading-settings-error context offset
                                     "TODO sequence has more than one DONE boundary"))
    (let* ((separator (first separators))
           (todo-tokens (if separator (subseq tokens 0 separator)
                            (butlast tokens)))
           (done-tokens (if separator (nthcdr (1+ separator) tokens)
                            (last tokens)))
           (todo-keywords (mapcar (lambda (token)
                                    (reader-heading-todo-keyword token context offset))
                                  todo-tokens))
           (done-keywords (mapcar (lambda (token)
                                    (reader-heading-todo-keyword token context offset))
                                  done-tokens)))
      (list :kind (if (eq declaration-kind :typ-todo) :type :sequence)
            :declaration-kind declaration-kind
            :sequence-id (or sequence-id
                             (reader-heading-generated-sequence-id
                              declaration-kind declaration-index))
            :declaration-index declaration-index
            :todo-keywords todo-keywords
            :done-keywords done-keywords
            :reader-offset offset))))

(defun reader-heading-active-todo-sequences (sequences)
  (let ((typed (remove-if-not
                (lambda (sequence)
                  (eq (getf sequence :declaration-kind) :typ-todo))
                sequences)))
    (if typed
        typed
        (remove-if-not
         (lambda (sequence)
           (member (getf sequence :declaration-kind) '(:todo :seq-todo)))
         sequences))))

(defun reader-heading-sequence-keyword-names (sequence)
  (append (mapcar (lambda (keyword) (getf keyword :keyword))
                  (getf sequence :todo-keywords))
          (mapcar (lambda (keyword) (getf keyword :keyword))
                  (getf sequence :done-keywords))))

(defun reader-heading-public-todo-sequences (sequences)
  (mapcar (lambda (sequence)
            (loop for (key value) on sequence by #'cddr
                  unless (eq key :reader-offset)
                    append (list key value)))
          sequences))

(defun reader-heading-keywords-for-sequence (sequence)
  (append (mapcar (lambda (keyword)
                    (cons (getf keyword :keyword) :todo))
                  (getf sequence :todo-keywords))
          (mapcar (lambda (keyword)
                    (cons (getf keyword :keyword) :done))
                  (getf sequence :done-keywords))))

(defun reader-heading-flatten-todo-sequences (sequences)
  (mapcan #'reader-heading-keywords-for-sequence
          (reader-heading-active-todo-sequences sequences)))

(defun reader-heading-normalized-todo-sequences-p (sequences)
  (and (listp sequences)
       sequences
       (every (lambda (sequence)
                (and (listp sequence)
                     (member (getf sequence :kind) '(:sequence :type))
                     (member (getf sequence :declaration-kind)
                             '(:todo :seq-todo :typ-todo))
                     (reader-heading-normalized-todo-keyword-list-p
                      (getf sequence :todo-keywords))
                     (reader-heading-normalized-todo-keyword-list-p
                      (getf sequence :done-keywords))))
              sequences)))

(defun reader-heading-normalized-todo-keyword-list-p (keywords)
  (and (listp keywords)
       (every (lambda (keyword)
                (and (listp keyword)
                     (stringp (getf keyword :keyword))))
              keywords)))

(defun reader-heading-todo-sequence (value)
  (reader-heading-keywords-for-sequence
   (reader-heading-todo-sequence-entry value :todo nil 1 nil nil)))

(defun reader-heading-legacy-todo-keyword-p (entry)
  (and (consp entry)
       (stringp (car entry))
       (member (cdr entry) '(nil :todo :done))))

(defun reader-heading-recognized-todo-keywords (settings)
  (let ((sequences (getf settings :todo-sequences))
        (keywords (getf settings :todo-keywords)))
    (if (and (reader-heading-normalized-todo-sequences-p sequences)
             (equal keywords (reader-heading-flatten-todo-sequences sequences)))
        (let ((active (reader-heading-active-todo-sequences sequences)))
          (append (mapcan #'reader-heading-keywords-for-sequence active)
                  (mapcan #'reader-heading-keywords-for-sequence
                          (remove-if (lambda (sequence)
                                       (member sequence active :test #'eq))
                                     sequences))))
        (if (listp keywords)
            (remove-if-not #'reader-heading-legacy-todo-keyword-p keywords)
            nil))))

(defun reader-heading-validate-todo-sequences (sequences context)
  (let ((ids (make-hash-table :test #'equal))
        (active-keywords (make-hash-table :test #'equal))
        (inactive-keywords (make-hash-table :test #'equal))
        (active-set (make-hash-table :test #'eq))
        (active (reader-heading-active-todo-sequences sequences)))
    (dolist (sequence active)
      (setf (gethash sequence active-set) t))
    (dolist (sequence sequences)
      (let ((id (getf sequence :sequence-id)))
        (when (gethash id ids)
          (reader-heading-settings-error
           context (getf sequence :reader-offset)
           "Ambiguous TODO sequence ID ~A" id))
        (setf (gethash id ids) t)))
    (dolist (sequence active)
      (dolist (keyword (reader-heading-sequence-keyword-names sequence))
        (when (gethash keyword active-keywords)
          (reader-heading-settings-error
           context (getf sequence :reader-offset)
           "Ambiguous TODO keyword ~A in active sequences" keyword))
        (setf (gethash keyword active-keywords) t)))
    (dolist (sequence sequences)
      (unless (gethash sequence active-set)
        (dolist (keyword (reader-heading-sequence-keyword-names sequence))
          (unless (gethash keyword active-keywords)
            (when (gethash keyword inactive-keywords)
              (reader-heading-settings-error
               context (getf sequence :reader-offset)
               "Ambiguous TODO keyword ~A in inactive sequences" keyword))
            (setf (gethash keyword inactive-keywords) t)))))
    sequences))

(defun reader-heading-default-todo-sequence ()
  (list :kind :sequence
        :declaration-kind :todo
        :sequence-id "todo-1"
        :declaration-index 1
        :todo-keywords
        (list (list :keyword "TODO" :logging-mode :none
                    :exit-logging-p nil :fast-key nil))
        :done-keywords
        (list (list :keyword "DONE" :logging-mode :none
                    :exit-logging-p nil :fast-key nil))))

(defun reader-heading-setting-present-p (settings key)
  (loop for tail on settings by #'cddr
        thereis (eq (first tail) key)))

(defun reader-heading-explicit-todo-sequences (keywords context)
  (let ((todo-keywords nil)
        (done-keywords nil))
    (dolist (entry keywords)
      (let ((keyword (list :keyword (car entry)
                           :logging-mode :none
                           :exit-logging-p nil
                           :fast-key nil)))
        (ecase (cdr entry)
          (:todo (push keyword todo-keywords))
          (:done (push keyword done-keywords)))))
    (if (null keywords)
        nil
        (let ((sequence
                (list :kind :sequence
                      :declaration-kind :todo
                      :sequence-id "todo-1"
                      :declaration-index 1
                      :todo-keywords (nreverse todo-keywords)
                      :done-keywords (nreverse done-keywords))))
          (reader-heading-validate-todo-sequences (list sequence) context)
          (list sequence)))))

(defun reader-heading-explicit-todo-keywords-p (keywords)
  (and (listp keywords)
       (every (lambda (entry)
                (and (consp entry)
                     (stringp (car entry))
                     (member (cdr entry) '(:todo :done))))
              keywords)))

(defun reader-heading-setting-kind (key)
  (when (stringp key)
    (multiple-value-bind (todo-kind base-length sequence-id-p)
        (org-setting-todo-declaration-prefix key)
      (declare (ignore base-length))
      (cond
        ((and todo-kind
              (or (not sequence-id-p)
                  (char= (char key (1- (length key))) #\])))
         :todo)
        ((string= key "STARTUP") :startup)
        ((string= key "LINK") :link)
        ((string= key "CATEGORY") :category)
        ((string= key "FILETAGS") :file-tags)
        ((string= key "TAGS") :tag-alist)
        ((string= key "PROPERTY") :property-settings)))))

(defun reader-heading-block-end-key (context offset)
  (let* ((line (reader-line-text context offset))
         (prefix "#+BEGIN_")
         (prefix-length (length prefix)))
    (if (and (>= (length line) prefix-length)
             (string-equal prefix line :end1 prefix-length :end2 prefix-length))
        (let* ((name-start
                (reader-heading-skip-horizontal-space line
                                                      prefix-length
                                                      (length line)))
               (name-end
                (or (position-if #'reader-heading-horizontal-space-p
                                 line
                                 :start
                                 name-start)
                    (length line))))
          (when (< name-start name-end)
            (concatenate 'string
                         "END_"
                         (string-upcase (subseq line name-start name-end)))))
        (multiple-value-bind (key value)
            (reader-keyword-fields context offset)
          (declare (ignore value))
          (when (and key (string= key "BEGIN"))
            :dynamic)))))

(defun reader-heading-block-close-p (context offset end-key)
  (if (eq end-key :dynamic)
      (multiple-value-bind (key value)
          (reader-keyword-fields context offset)
        (declare (ignore value))
        (and key (string= key "END")))
      (let* ((line (reader-line-text context offset))
             (prefix "#+END_")
             (prefix-length (length prefix)))
        (and (>= (length line) prefix-length)
             (string-equal prefix line :end1 prefix-length :end2 prefix-length)
             (let* ((name-start
                     (reader-heading-skip-horizontal-space line
                                                           prefix-length
                                                           (length line)))
                    (name-end
                     (or (position-if #'reader-heading-horizontal-space-p
                                      line
                                      :start
                                      name-start)
                         (length line))))
               (and (< name-start name-end)
                    (string-equal
                     end-key
                     (concatenate 'string
                                  "END_"
                                  (string-upcase
                                   (subseq line name-start name-end))))
                    (every #'reader-heading-horizontal-space-p
                           (subseq line name-end))))))))

(defun reader-heading-link-abbreviation (value)
  (let* ((start (reader-heading-skip-horizontal-space value 0 (length value)))
         (end
          (position-if #'reader-heading-horizontal-space-p value :start start)))
    (when end
      (let* ((name (subseq value start end))
             (replacement-start
              (reader-heading-skip-horizontal-space value end (length value)))
             (replacement-end
              (reader-heading-trim-right value replacement-start (length value))))
        (cons name (subseq value replacement-start replacement-end))))))

(defun reader-heading-tag-group-end (text start)
  (let* ((length (length text))
         (opening (char text start))
         (closing (if (char= opening #\[) #\] #\}))
         (position (1+ start))
         (brace-depth (if (char= opening #\{) 1 0)))
    (loop while (< position length)
          for character = (char text position)
          do (cond
               ((char= character #\\)
                (incf position (if (< (1+ position) length) 2 1)))
               ((char= character #\{)
                (incf brace-depth)
                (incf position))
               ((char= character #\})
                (when (plusp brace-depth)
                  (decf brace-depth))
                (if (and (char= opening #\{) (zerop brace-depth))
                    (return-from reader-heading-tag-group-end position)
                    (incf position)))
               ((and (zerop brace-depth) (char= character closing))
                (return-from reader-heading-tag-group-end position))
               (t
                (incf position))))))

(defun reader-heading-tag-token-end (text start limit)
  (let ((position start)
        (brace-depth 0))
    (loop while (< position limit)
          for character = (char text position)
          do (cond
               ((char= character #\\)
                (incf position (if (< (1+ position) limit) 2 1)))
               ((char= character #\{)
                (incf brace-depth)
                (incf position))
               ((char= character #\})
                (when (plusp brace-depth)
                  (decf brace-depth))
                (incf position))
               ((and (zerop brace-depth)
                     (reader-heading-horizontal-space-p character))
                (return position))
               (t
                (incf position)))
          finally (return limit))))

(defun reader-heading-tag-words (text)
  (let ((words '())
        (position 0)
        (limit (length text)))
    (loop while (< position limit)
          do (setf position
                   (reader-heading-skip-horizontal-space text position limit))
             (when (< position limit)
               (let ((end (reader-heading-tag-token-end text position limit)))
                 (push (subseq text position end) words)
                 (setf position end))))
    (nreverse words)))

(defun reader-heading-tag-definition (token)
  (unless (string= token "\\n")
    (let ((length (length token))
          (open (position #\( token :from-end t)))
      (cond
        ((and open
              (= (- length open) 3)
              (char= (char token (1- length)) #\))
              (reader-heading-valid-tag-name-p (subseq token 0 open)))
         (list :tag (subseq token 0 open)
               :fast-key (char token (1+ open))))
        ((reader-heading-valid-tag-name-p token)
         (list :tag token :fast-key nil))))))

(defun reader-heading-tag-regex-member (token)
  (let ((length (length token)))
    (when (and (>= length 2)
               (char= (char token 0) #\{)
               (char= (char token (1- length)) #\}))
      (list :regex (subseq token 1 (1- length))))))

(defun reader-heading-tag-group-definition (text start end)
  (let* ((opening (char text start))
         (body (subseq text (1+ start) end))
         (words (reader-heading-tag-words body))
         (colon (position ":" words :test #'string=))
         (named-p (and colon (plusp colon)))
         (group-name
           (when named-p
             (let ((definition (reader-heading-tag-definition (first words))))
               (getf definition :tag))))
         (member-words
           (if named-p (nthcdr (1+ colon) words) words))
         (members
           (loop for word in member-words
                 for regex = (reader-heading-tag-regex-member word)
                 for definition = (and (not regex)
                                       (reader-heading-tag-definition word))
                 for member = (or regex (getf definition :tag))
                 when member collect member)))
    (when (and members
               (if (char= opening #\[)
                   (and named-p group-name)
                   (or (and named-p group-name)
                       (not colon))))
      (list :group (or group-name "")
            :members members
            :exclusive-p (char= opening #\{)))))

(defun reader-heading-tag-alist (value)
  (let ((length (length value)))
    (if (and (>= length 2)
             (char= (char value 0) #\:)
             (char= (char value (1- length)) #\:))
        (mapcar (lambda (tag) (list :tag tag :fast-key nil))
                (reader-heading-split-tags value 1 (1- length)))
        (let ((entries '())
              (position 0)
              (limit (reader-heading-trim-right value 0 length)))
          (loop while (< position limit)
                do (setf position
                         (reader-heading-skip-horizontal-space value position limit))
                   (let ((character (char value position)))
                     (if (find character '(#\[ #\{))
                         (let ((end (reader-heading-tag-group-end value position)))
                           (unless end
                             (return))
                           (let ((entry
                                   (reader-heading-tag-group-definition
                                    value position end)))
                             (when entry
                               (push entry entries)))
                           (setf position (1+ end)))
                         (let* ((end (reader-heading-tag-token-end value
                                                                   position
                                                                   limit))
                                (entry
                                  (reader-heading-tag-definition
                                   (subseq value position end))))
                           (when entry
                             (push entry entries))
                           (setf position end)))))
          (nreverse entries)))))

(defun reader-heading-file-tag-list (text)
  (let ((tags '())
        (start 0)
        (limit (length text)))
    (loop for position from 0 to limit
          do (when (or (= position limit)
                       (char= (char text position) #\:))
               (let ((tag (subseq text start position)))
                 (unless (or (zerop (length tag))
                             (reader-heading-valid-tag-name-p tag))
                   (return-from reader-heading-file-tag-list (values nil nil)))
                 (unless (zerop (length tag))
                   (push tag tags))
                 (setf start (1+ position)))))
    (values (nreverse tags) t)))

(defun reader-heading-file-tags (value)
  (let ((length (length value)))
    (cond
      ((zerop length)
       (values nil t))
      (t
       (reader-heading-file-tag-list value)))))

(defun reader-heading-property-setting (value context offset)
  (let* ((start (reader-heading-skip-horizontal-space value 0 (length value)))
         (end (or (position-if #'reader-heading-horizontal-space-p value
                               :start start)
                  (length value))))
    (when (= start end)
      (reader-heading-settings-error context offset
                                     "PROPERTY directive has no name"))
    (cons (string-upcase (subseq value start end))
          (subseq value
                  (reader-heading-skip-horizontal-space value end (length value))))))

(defmethod reader-buffer-settings ((context reader-context) explicit-settings)
  (let ((settings (reader-default-settings))
        (todo-sequences nil)
        (todo-declaration-index 0)
        (startup nil)
        (link-abbrevs nil)
        (category nil)
        (category-seen nil)
        (file-tags nil)
        (file-tags-seen nil)
        (tag-alist nil)
        (tag-alist-seen nil)
        (property-settings nil)
        (property-settings-seen-p nil)
        (position 0)
        (limit (length (reader-context-text context)))
        (block-end-key nil))
    (loop while (< position limit)
          do (let ((next (min limit (reader-next-line context position))))
               (if block-end-key
                   (progn
                     (when
                         (reader-heading-block-close-p context
                                                       position
                                                       block-end-key)
                       (setf block-end-key nil))
                     (setf position next))
                   (let ((open-key
                          (reader-heading-block-end-key context position)))
                     (if open-key
                         (setf block-end-key open-key
                               position next)
                         (multiple-value-bind (key value source-key)
                             (reader-keyword-fields context position)
                           (multiple-value-bind (declaration-kind sequence-id
                                                todo-declaration-p)
                               (reader-heading-todo-declaration-info
                                source-key context position)
                             (if todo-declaration-p
                                 (progn
                                   (incf todo-declaration-index)
                                   (push (reader-heading-todo-sequence-entry
                                          value declaration-kind sequence-id
                                          todo-declaration-index context position)
                                         todo-sequences))
                                 (case (reader-heading-setting-kind key)
                                   (:startup
                                    (setf startup (append startup
                                                          (mapcar
                                                           (function string-downcase)
                                                           (reader-heading-words
                                                            value)))))
                                   (:link
                                    (let ((abbreviation
                                           (reader-heading-link-abbreviation value)))
                                      (when abbreviation
                                        (setf link-abbrevs
                                              (append link-abbrevs
                                                      (list abbreviation))))))
                                   (:category
                                    (when (plusp (length value))
                                      (setf category value
                                            category-seen t)))
                                   (:file-tags
                                    (multiple-value-bind (tags valid-p)
                                        (reader-heading-file-tags value)
                                      (when valid-p
                                        (setf file-tags-seen t
                                              file-tags (append file-tags tags)))))
                                   (:tag-alist
                                    (setf tag-alist-seen t
                                          tag-alist
                                          (append tag-alist
                                                  (reader-heading-tag-alist value))))
                                   (:property-settings
                                    (setf property-settings-seen-p t
                                          property-settings
                                          (append property-settings
                                                  (list
                                                   (reader-heading-property-setting
                                                    value context position)))))
                                   (otherwise nil))))
                           (setf position next)))))))
    (setf todo-sequences (nreverse todo-sequences))
    (unless todo-sequences
      (setf todo-sequences (list (reader-heading-default-todo-sequence))))
    (reader-heading-validate-todo-sequences todo-sequences context)
    (setf (getf settings :todo-sequences)
          (reader-heading-public-todo-sequences todo-sequences)
          (getf settings :todo-keywords)
          (reader-heading-flatten-todo-sequences todo-sequences))
    (when category-seen
      (setf (getf settings :category) category))
    (when file-tags-seen
      (setf (getf settings :file-tags) file-tags))
    (when tag-alist-seen
      (setf (getf settings :tag-alist) tag-alist))
    (when property-settings-seen-p
      (setf (getf settings :property-settings) property-settings))
    (when startup
      (setf (getf settings :startup) startup))
    (when link-abbrevs
      (setf (getf settings :link-abbrevs) link-abbrevs))
    (loop for tail on explicit-settings by (function cddr)
          do (setf (getf settings (first tail)) (second tail)))
    (cond
      ((reader-heading-setting-present-p explicit-settings :todo-keywords)
       (when (reader-heading-explicit-todo-keywords-p
              (getf settings :todo-keywords))
         (setf (getf settings :todo-sequences)
               (reader-heading-public-todo-sequences
                (reader-heading-explicit-todo-sequences
                 (getf settings :todo-keywords) context)))))
      ((reader-heading-setting-present-p explicit-settings :todo-sequences)
       (when (reader-heading-normalized-todo-sequences-p
              (getf settings :todo-sequences))
         (setf (getf settings :todo-keywords)
               (reader-heading-flatten-todo-sequences
                (getf settings :todo-sequences))))))
    (setf (reader-context-recognized-todo-keywords context)
          (reader-heading-recognized-todo-keywords settings))
    settings))

(defun reader-heading-inline-minimum-stars (context)
  (let ((minimum (getf (reader-context-settings context) :inlinetask-min-level)))
    (when minimum
      (check-type minimum (integer 1))
      (source-heading-level (reader-context-document context) minimum))))

(defun reader-heading-valid-tag-name-p (name)
  (and (plusp (length name))
       (every
        (lambda (character)
          (or (alphanumericp character) (find character '(#\_ #\@ #\# #\%))))
        name)))

(defun reader-heading-split-tags (text start end)
  (let ((tags '())
        (tag-start start))
    (loop for position from start to
          end
          do (when (or (= position end) (char= (char text position) #\:))
               (let ((tag (subseq text tag-start position)))
                 (unless (reader-heading-valid-tag-name-p tag)
                   (return-from reader-heading-split-tags
                     nil))
                 (push tag tags)
                 (setf tag-start (1+ position)))))
    (nreverse tags)))

(defun reader-heading-tags-at-end (text title-start line-end)
  (let* ((end (reader-heading-trim-right text title-start line-end))
         (closing-colon (1- end)))
    (when (and (> end title-start) (char= (char text closing-colon) #\:))
      (loop for group-start from title-start below closing-colon
            when (and (char= (char text group-start) #\:)
                      (or (= group-start title-start)
                          (reader-heading-horizontal-space-p
                           (char text (1- group-start)))))
              do (let ((tags
                        (reader-heading-split-tags text
                                                   (1+ group-start)
                                                   closing-colon)))
                   (when tags
                     (return-from reader-heading-tags-at-end
                       (values tags group-start))))))))

(defun reader-heading-priority-at (text position end)
  (when
      (and (<= (+ position 4) end)
           (char= (char text position) #\[)
           (char= (char text (1+ position)) #\#))
    (let ((close (position #\] text :start (+ position 2) :end end)))
      (when close
        (let* ((value (subseq text (+ position 2) close))
               (priority
                (cond
                  ((and (= (length value) 1)
                        (<= (char-code #\A)
                            (char-code (char value 0))
                            (char-code #\Z))) (char-code (char value 0)))
                  ((and (<= 1 (length value) 2)
                        (every (function digit-char-p) value)
                        (<= (parse-integer value) 64)) (parse-integer value)))))
          (when priority
            (values priority (1+ close))))))))

(defun reader-heading-properties (context start star-count line-end)
  (labels ((token-at (position token)
             (let ((token-end (+ position (length token)))
                   (text (reader-context-text context)))
               (and (<= token-end line-end)
                    (string= token text :start2 position :end2 token-end)
                    (or (= token-end line-end)
                        (reader-heading-horizontal-space-p
                         (char text token-end)))
                    token-end))))
    (let* ((text (reader-context-text context))
           (raw-line-end
            (if (and (< line-end (length text))
                     (char= (char text line-end) #\Return))
                (if (char= (char text (1- line-end)) #\Return)
                    ;; In CRCRLF, keep the data CR and exclude the delimiter CR.
                    line-end
                    (1+ line-end))
                line-end))
           (position
            (reader-heading-skip-horizontal-space text
                                                  (+ start star-count)
                                                  line-end))
           (todo-keywords
            (reader-context-recognized-todo-keywords context))
           (todo-keyword nil)
           (todo-type nil)
           (priority nil)
           (commentedp nil))
      (dolist (entry todo-keywords)
        (let ((token-end (token-at position (car entry))))
          (when token-end
            (setf todo-keyword (car entry)
                  todo-type (cdr entry)
                  position (reader-heading-skip-horizontal-space text
                                                                 token-end
                                                                 line-end))
            (return))))
      (multiple-value-bind (value next)
          (reader-heading-priority-at text position line-end)
        (when value
          (setf priority value
                position (reader-heading-skip-horizontal-space text
                                                               next
                                                               line-end))))
      (let ((comment-end (token-at position "COMMENT")))
        (when comment-end
          (setf commentedp t
                position (reader-heading-skip-horizontal-space text
                                                               comment-end
                                                               line-end))))
      (let* ((title-start position)
             (tags nil)
             (title-end raw-line-end))
        (multiple-value-bind (found-tags tag-start)
            (reader-heading-tags-at-end text title-start line-end)
          (when found-tags
            (setf tags found-tags
                  title-end tag-start)))
        (setf title-end (reader-heading-trim-right text title-start title-end))
        (let* ((object-start
                (reader-heading-skip-horizontal-space text
                                                      title-start
                                                      title-end))
               (object-end
                (reader-heading-trim-right text object-start title-end))
               (raw-value
                (string-trim (list #\Space #\Tab)
                             (subseq text title-start (min title-end line-end)))))
          (list :level
                (logical-heading-level (reader-context-document context)
                                       star-count)
                :todo-keyword
                todo-keyword
                :todo-type
                todo-type
                :priority
                priority
                :raw-value
                raw-value
                :title-range
                (cons object-start object-end)
                :tags
                tags
                :archivedp
                (if (member "ARCHIVE" tags :test (function string=))
                    t
                    nil)
                :commentedp
                (if commentedp
                    t
                    nil)
                :scheduled
                nil
                :deadline
                nil
                :closed
                nil))))))

(defun reader-heading-skip-blank-lines (context start limit)
  (let ((position start)
        (count 0))
    (loop while (and (< position limit) (reader-blank-line-p context position))
          do (incf count) (setf position (min limit
                                              (reader-next-line context
                                                                position))))
    (values position count)))

(defun reader-heading-end-marker-p (context start limit minimum-stars)
  (let ((star-count (reader-heading-level context start limit)))
    (when (and star-count (>= star-count minimum-stars))
      (let* ((line-end
              (min limit (nth-value 0 (reader-line-bounds context start))))
             (text (reader-context-text context))
             (title-start
              (reader-heading-skip-horizontal-space text
                                                    (+ start star-count)
                                                    line-end))
             (title-end (reader-heading-trim-right text title-start line-end)))
        (and (= (- title-end title-start) 3)
             (string-equal "END" text :start2 title-start :end2 title-end))))))

(defun reader-heading-inline-close (context start limit minimum-stars)
  (let ((position start)
        (block-end-key nil))
    (loop while (< position limit)
          do (let ((next (min limit (reader-next-line context position))))
               (if block-end-key
                   (progn
                     (when
                         (reader-heading-block-close-p context
                                                       position
                                                       block-end-key)
                       (setf block-end-key nil))
                     (setf position next))
                   (let ((open-key
                          (reader-heading-block-end-key context position))
                         (star-count
                          (reader-heading-level context position limit)))
                     (cond
                       (open-key
                        (setf block-end-key open-key
                              position next))
                       (star-count
                        (if (reader-heading-end-marker-p context
                                                         position
                                                         limit
                                                         minimum-stars)
                            (multiple-value-bind (after-close blank-count)
                                (reader-heading-skip-blank-lines context
                                                                 next
                                                                 limit)
                              (return-from reader-heading-inline-close
                                (values position after-close blank-count)))
                            (return-from reader-heading-inline-close
                              (values nil nil nil))))
                       (t
                        (setf position next)))))))
    (values nil nil nil)))

(defun reader-heading-subtree-end (context after-heading limit star-count)
  (let ((position after-heading)
        (block-end-key nil)
        (minimum-stars (reader-heading-inline-minimum-stars context)))
    (loop while (< position limit)
          do (let ((next (min limit (reader-next-line context position))))
               (if block-end-key
                   (progn
                     (when
                         (reader-heading-block-close-p context
                                                       position
                                                       block-end-key)
                       (setf block-end-key nil))
                     (setf position next))
                   (let* ((open-key
                           (reader-heading-block-end-key context position))
                          (candidate-level
                           (reader-heading-level context position limit)))
                     (cond
                       (open-key
                        (setf block-end-key open-key
                              position next))
                       ((and candidate-level (<= candidate-level star-count))
                        (return-from reader-heading-subtree-end
                          position))
                       ((and candidate-level
                             minimum-stars
                             (>= candidate-level minimum-stars))
                        (multiple-value-bind (close-start after-close)
                            (reader-heading-inline-close context
                                                         next
                                                         limit
                                                         minimum-stars)
                          (declare (ignore close-start))
                          (setf position (or after-close next))))
                       (t
                        (setf position next)))))))
    limit))

(defun reader-heading-first-child-start (context start end star-count)
  (let ((position start)
        (block-end-key nil)
        (minimum-stars (reader-heading-inline-minimum-stars context)))
    (loop while (< position end)
          do (let ((next (min end (reader-next-line context position))))
               (if block-end-key
                   (progn
                     (when
                         (reader-heading-block-close-p context
                                                       position
                                                       block-end-key)
                       (setf block-end-key nil))
                     (setf position next))
                   (let* ((open-key
                           (reader-heading-block-end-key context position))
                          (candidate-level
                           (reader-heading-level context position end)))
                     (cond
                       (open-key
                        (setf block-end-key open-key
                              position next))
                       ((and candidate-level (<= candidate-level star-count))
                        (return-from reader-heading-first-child-start
                          nil))
                       ((and candidate-level
                             minimum-stars
                             (>= candidate-level minimum-stars))
                        (multiple-value-bind (close-start after-close)
                            (reader-heading-inline-close context
                                                         next
                                                         end
                                                         minimum-stars)
                          (declare (ignore close-start))
                          (setf position (or after-close next))))
                       (candidate-level
                        (return-from reader-heading-first-child-start
                          position))
                       (t
                        (setf position next)))))))
    nil))

(defun reader-heading-section (context nodes depth)
  (when nodes
    (let* ((first (first nodes))
           (last (car (last nodes)))
           (begin (node-begin first))
           (end (node-end last)))
      (make-reader-node context
                        :section
                        :begin
                        begin
                        :end
                        end
                        :contents-begin
                        begin
                        :contents-end
                        end
                        :post-affiliated
                        begin
                        :children
                        nodes
                        :depth
                        depth))))

(defun reader-heading-children (context contents-begin
                                        contents-end
                                        star-count
                                        depth
                                        &optional
                                        planning-node
                                        headline-properties)
  (let ((text (reader-context-text context)))
    (labels ((planning-line-ranges (start line-end)
               (let ((position start)
                     (ranges nil))
                 (loop
                   (setf position
                         (reader-heading-skip-horizontal-space text
                                                               position
                                                               line-end))
                   (when (= position line-end)
                     (return (nreverse ranges)))
                   (let* ((entry
                           (find-if
                            (lambda (candidate)
                              (let* ((keyword (car candidate))
                                     (keyword-end (+ position
                                                    (length keyword))))
                                (and (<= keyword-end line-end)
                                     (string-equal keyword
                                                   text
                                                   :start2
                                                   position
                                                   :end2
                                                   keyword-end))))
                            '(("CLOSED:" . :closed)
                              ("DEADLINE:" . :deadline)
                              ("SCHEDULED:" . :scheduled))))
                          (keyword-end
                           (and entry (+ position (length (car entry)))))
                          (value-start
                           (and keyword-end
                                (reader-heading-skip-horizontal-space
                                 text keyword-end line-end))))
                     (unless (and entry
                                  (< keyword-end value-start)
                                  (< value-start line-end))
                       (return nil))
                     (multiple-value-bind (fields timestamp-end)
                         (%parse-single-timestamp text value-start line-end)
                       (unless fields
                         (return nil))
                       (when (assoc (cdr entry) ranges)
                         (return nil))
                       (push (cons (cdr entry)
                                   (cons value-start timestamp-end))
                             ranges)
                       (setf position timestamp-end))))))
             (planning-at (start end node-depth)
               (multiple-value-bind (raw-line-end after-line)
                   (reader-line-bounds context start)
                 (let* ((line-end (min end raw-line-end))
                        (ranges (planning-line-ranges start line-end)))
                   (when ranges
                     (let ((properties
                            (append
                             (list :closed nil :deadline nil :scheduled nil)
                             (when (assoc :closed ranges)
                               (list :reader-closed-range
                                     (cdr (assoc :closed ranges))))
                             (when (assoc :deadline ranges)
                               (list :reader-deadline-range
                                     (cdr (assoc :deadline ranges))))
                             (when (assoc :scheduled ranges)
                               (list :reader-scheduled-range
                                     (cdr (assoc :scheduled ranges)))))))
                       (multiple-value-bind (planning-end blank-count)
                           (reader-heading-skip-blank-lines context
                                                            (min end after-line)
                                                            end)
                         (declare (ignore blank-count))
                         (values
                          (make-reader-node context
                                            :planning
                                            :begin
                                            start
                                            :end
                                            planning-end
                                            :properties
                                            properties
                                            :depth
                                            node-depth)
                          planning-end))))))))
      (multiple-value-bind (found-planning)
          (if planning-node
              (values planning-node (node-end planning-node))
              (if (and contents-begin (< contents-begin contents-end))
                  (planning-at contents-begin contents-end (+ depth 2))
                  (values nil nil)))
        (let* ((planning-node (or planning-node found-planning))
               (content-start
                (if planning-node
                    (node-end planning-node)
                    contents-begin))
               (child-start
                (reader-heading-first-child-start context
                                                  content-start
                                                  contents-end
                                                  star-count))
               (section-nodes
                (if child-start
                    (reader-parse-elements context
                                           content-start
                                           child-start
                                           (+ depth 2))
                    (reader-parse-elements context
                                           content-start
                                           contents-end
                                           (+ depth 2))))
               (headline-nodes
                (when child-start
                  (reader-parse-elements context
                                         child-start
                                         contents-end
                                         (1+ depth)))))
          (when (and planning-node headline-properties)
            (dolist (key
                     (list :reader-closed-range
                           :reader-deadline-range
                           :reader-scheduled-range))
              (setf (getf headline-properties key)
                    (node-property planning-node key))))
          (when planning-node
            (push planning-node section-nodes))
          (let ((section
                 (reader-heading-section context section-nodes (1+ depth))))
            (append
             (when section
               (list section))
             headline-nodes)))))))

(defmethod parse-headline-at ((context reader-context) start limit depth)
  (let ((star-count (reader-heading-level context start limit)))
    (when star-count
      (let* ((level (logical-heading-level (reader-context-document context)
                                           star-count))
             (minimum-stars (reader-heading-inline-minimum-stars context))
             (inlinetask-p (and minimum-stars (>= star-count minimum-stars))))
        (check-limit (reader-context-limits context) :max-heading-depth level)
        (multiple-value-bind (line-end after-heading)
            (reader-line-bounds context start)
          (let* ((line-end (min limit line-end))
                 (properties
                  (reader-heading-properties context start star-count line-end)))
            (if inlinetask-p
                (multiple-value-bind (close-start after-close blank-count)
                    (reader-heading-inline-close context
                                                 after-heading
                                                 limit
                                                 minimum-stars)
                  (if close-start
                      (let* ((contents-begin
                              (nth-value 0
                                         (reader-heading-skip-blank-lines
                                          context
                                          after-heading
                                          close-start)))
                             (has-contents (< contents-begin close-start))
                             (children
                              (and has-contents
                                   (reader-parse-elements context
                                                          contents-begin
                                                          close-start
                                                          (1+ depth))))
                             (properties
                              (append properties
                                      (list :reader-title-range
                                            (getf properties :title-range))))
                             (properties
                              (loop for (key value) on properties by (function
                                                                      cddr)
                                    unless (eq key :title-range)
                                      append (list key value)))
                             (node
                              (make-reader-node context
                                                :inlinetask
                                                :begin
                                                start
                                                :end
                                                after-close
                                                :contents-begin
                                                (and has-contents
                                                     contents-begin)
                                                :contents-end
                                                (and has-contents close-start)
                                                :post-affiliated
                                                start
                                                :post-blank
                                                blank-count
                                                :properties
                                                properties
                                                :children
                                                children
                                                :depth
                                                depth)))
                        (values node after-close))
                      (multiple-value-bind (next blank-count)
                          (reader-heading-skip-blank-lines context
                                                           after-heading
                                                           limit)
                        (let* ((properties
                                (append properties
                                        (list :reader-title-range
                                              (getf properties :title-range))))
                               (properties
                                (loop for (key value) on properties by (function
                                                                        cddr)
                                      unless (eq key :title-range)
                                        append (list key value)))
                               (node
                                (make-reader-node context
                                                  :inlinetask
                                                  :begin
                                                  start
                                                  :end
                                                  next
                                                  :post-affiliated
                                                  start
                                                  :post-blank
                                                  blank-count
                                                  :properties
                                                  properties
                                                  :depth
                                                  depth)))
                          (values node next)))))
                (let* ((subtree-end
                        (reader-heading-subtree-end context
                                                    after-heading
                                                    limit
                                                    star-count))
                       (contents-begin
                        (nth-value 0
                                   (reader-heading-skip-blank-lines context
                                                                    after-heading
                                                                    subtree-end)))
                       (has-contents (< contents-begin subtree-end))
                       (children
                        (and has-contents
                             (reader-heading-children context
                                                      contents-begin
                                                      subtree-end
                                                      star-count
                                                      depth
                                                      nil
                                                      properties)))
                       (properties
                        (let* ((section
                                (find :section
                                      children
                                      :key
                                      (function node-type)))
                               (planning
                                (and section
                                     (find :planning
                                           (node-children section)
                                           :key
                                           (function node-type))))
                               (result (copy-list properties)))
                          (when planning
                            (dolist
                                (key
                                 (list :reader-closed-range
                                       :reader-deadline-range
                                       :reader-scheduled-range))
                              (let ((range (node-property planning key)))
                                (when range
                                  (setf (getf result key) range)))))
                          (setf (getf result :reader-title-range) (getf result
                                                                        :title-range))
                          result))
                       (properties
                        (loop for (key value) on properties by (function cddr)
                              unless (eq key :title-range)
                                append (list key value)))
                       (post-blank
                        (if has-contents
                            0
                            (nth-value 1
                                       (reader-heading-skip-blank-lines context
                                                                        after-heading
                                                                        subtree-end))))
                       (node
                        (make-reader-node context
                                          :headline
                                          :begin
                                          start
                                          :end
                                          subtree-end
                                          :contents-begin
                                          (and has-contents contents-begin)
                                          :contents-end
                                          (and has-contents subtree-end)
                                          :post-affiliated
                                          start
                                          :post-blank
                                          post-blank
                                          :properties
                                          properties
                                          :children
                                          children
                                          :depth
                                          depth)))
                  (values node subtree-end)))))))))

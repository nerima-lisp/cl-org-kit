;;;; src/agenda-search-view.lisp
(in-package #:org-kit)

(defstruct (agenda-search-view-term
            (:constructor %make-agenda-search-view-term (polarity kind value)))
  polarity
  kind
  value)

(defstruct (agenda-search-work-state
            (:constructor %make-agenda-search-work-state (limits)))
  limits
  (used 0))

(defun agenda-search-view-whitespace-p (character)
  (and character (find character '(#\Space #\Tab #\Newline #\Return))))

(defun agenda-search-view-regex-close (parser)
  (let* ((source (query-match-parser-source parser))
         (length (query-match-parser-length parser))
         (start (query-match-parser-position parser))
         (cursor (1+ start))
         (depth 1))
    (loop while (< cursor length)
          for character = (char source cursor)
          do (cond
               ((query-match-control-p character)
                (query-match-parser-fail parser cursor
                                         "Control character in regular expression"))
               ((char= character #\\)
                (when (= (1+ cursor) length)
                  (query-match-parser-fail parser cursor
                                           "Incomplete escape in regular expression"))
                (incf cursor 2))
               ((char= character #\{)
                (incf depth)
                (check-limit (query-match-parser-limits parser)
                             :max-element-depth depth)
                (incf cursor))
               ((char= character #\})
                (decf depth)
                (if (zerop depth)
                    (return-from agenda-search-view-regex-close cursor)
                    (incf cursor)))
               (t (incf cursor)))
          finally (query-match-parser-fail parser start
                                          "Unclosed regular expression"))))

(defun agenda-search-view-parse-regex (parser)
  (let* ((source (query-match-parser-source parser))
         (start (query-match-parser-position parser))
         (body-start (1+ start))
         (close (agenda-search-view-regex-close parser))
         (pattern (subseq source body-start close))
         (limits (query-match-parser-limits parser)))
    (check-limit limits :max-search-regex-length (length pattern))
    (prog1 (query-compile-emacs-regex
            pattern (query-match-parser-context parser) limits source body-start)
      (setf (query-match-parser-position parser) (1+ close)))))

(defun agenda-search-view-parse-word (parser)
  (let ((source (query-match-parser-source parser))
        (length (query-match-parser-length parser))
        (position (query-match-parser-position parser)))
    (with-output-to-string (output)
      (loop while (< position length)
            for character = (char source position)
            do (cond
                 ((agenda-search-view-whitespace-p character)
                  (return))
                 ((query-match-control-p character)
                  (query-match-parser-fail parser position
                                           "Control character in search query"))
                 ((char= character #\\)
                  (when (= (1+ position) length)
                    (query-match-parser-fail parser position
                                             "Incomplete escape in search term"))
                  (write-char (char source (1+ position)) output)
                  (incf position 2))
                 (t
                  (write-char character output)
                  (incf position))))
      (setf (query-match-parser-position parser) position))))

(defun agenda-search-view-parse-term (parser)
  (let* ((position (query-match-parser-position parser))
         (sign (query-match-parser-peek parser))
         (polarity (if (eql sign #\-) :excluded :required)))
    (when (or (eql sign #\+) (eql sign #\-))
      (incf (query-match-parser-position parser)))
    (when (or (query-match-parser-at-end-p parser)
              (agenda-search-view-whitespace-p
               (query-match-parser-peek parser)))
      (query-match-parser-fail parser position
                               "Search sign must be followed by a term"))
    (let* ((character (query-match-parser-peek parser))
           (kind (cond ((char= character #\{) :regex)
                       ((char= character #\") :phrase)
                       (t :word)))
           (value (case kind
                    (:regex (agenda-search-view-parse-regex parser))
                    (:phrase (query-match-parse-quoted-string parser))
                    (:word (agenda-search-view-parse-word parser)))))
      (unless (or (query-match-parser-at-end-p parser)
                  (agenda-search-view-whitespace-p
                   (query-match-parser-peek parser)))
        (query-match-parser-fail
         parser (query-match-parser-position parser)
         "Search terms must be separated by whitespace"))
      (%make-agenda-search-view-term polarity kind value))))

(defun agenda-search-view-operation-limits (documents)
  (let ((limits (mapcar #'document-limits documents)))
    (make-limits
     :max-input-length (minimum-limit-value limits :max-input-length)
     :max-element-depth (minimum-limit-value limits :max-element-depth)
     :max-search-regex-length
     (minimum-limit-value limits :max-search-regex-length)
     :max-agenda-entries (minimum-limit-value limits :max-agenda-entries))))

(defun agenda-search-view-parse-query (query limits)
  (check-type query string)
  (check-limit limits :max-input-length (length query))
  (let* ((length (length query))
         (start (or (position-if-not
                     (function agenda-search-view-whitespace-p)
                     query)
                    length))
         (boolean-p
           (and (< start length)
                (let ((character (char query start)))
                  (or (char= character #\+)
                      (char= character #\-)
                      (char= character #\{))))))
    (when (= start length)
      (query-signal-error query 0 "Search query cannot be empty"))
    (if boolean-p
        (let* ((context (make-query-context :case-fold-p t))
               (parser (%make-query-match-parser
                        :source query :length length
                        :context context :limits limits))
               (terms nil))
          (setf (query-match-parser-position parser) start)
          (loop until (query-match-parser-at-end-p parser)
                do (push (agenda-search-view-parse-term parser) terms)
                   (loop while (agenda-search-view-whitespace-p
                                (query-match-parser-peek parser))
                         do (incf (query-match-parser-position parser))))
          (nreverse terms))
        (list (%make-agenda-search-view-term
               :required :flexible-phrase
               (string-trim (list #\Space #\Tab #\Newline #\Return)
                            query))))))

(defun agenda-search-view-term-matches-p (term text work-state)
  (case (agenda-search-view-term-kind term)
    ((:word :phrase)
     (not (null (search (agenda-search-view-term-value term) text
                        :test (function char-equal)))))
    (:flexible-phrase
     (let* ((phrase (agenda-search-view-term-value term))
            (phrase-length (length phrase))
            (text-length (length text))
            (limits (agenda-search-work-state-limits work-state))
            (actual (+ (agenda-search-work-state-used work-state)
                       (* phrase-length text-length))))
       (check-limit limits :max-search-work actual)
       (setf (agenda-search-work-state-used work-state) actual)
       (labels ((matches-from (text-position phrase-position)
                  (loop
                    (when (= phrase-position phrase-length)
                      (return t))
                    (when (= text-position text-length)
                      (return nil))
                    (if (agenda-search-view-whitespace-p
                         (char phrase phrase-position))
                        (if (agenda-search-view-whitespace-p
                             (char text text-position))
                            (progn
                              (loop while (and (< phrase-position phrase-length)
                                               (agenda-search-view-whitespace-p
                                                (char phrase phrase-position)))
                                    do (incf phrase-position))
                              (loop while (and (< text-position text-length)
                                               (agenda-search-view-whitespace-p
                                                (char text text-position)))
                                    do (incf text-position)))
                            (return nil))
                        (if (char-equal (char phrase phrase-position)
                                        (char text text-position))
                            (progn
                              (incf phrase-position)
                              (incf text-position))
                            (return nil))))))
         (loop for start from 0 to text-length
               thereis (matches-from start 0)))))
    (:regex
     (let* ((limits (agenda-search-work-state-limits work-state))
            (actual (+ (agenda-search-work-state-used work-state)
                       (length text))))
       (check-limit limits :max-search-work actual)
       (setf (agenda-search-work-state-used work-state) actual)
       (cl-regex-kit:is-match-p
        (query-regex-compiled (agenda-search-view-term-value term)) text)))))

(defun agenda-search-view-matches-p (terms text work-state)
  (let ((required (remove-if-not
                   (lambda (term)
                     (eq (agenda-search-view-term-polarity term) :required))
                   terms))
        (excluded (remove-if-not
                   (lambda (term)
                     (eq (agenda-search-view-term-polarity term) :excluded))
                   terms)))
    (and (every (lambda (term)
                  (agenda-search-view-term-matches-p term text work-state))
                required)
         (notany (lambda (term)
                   (agenda-search-view-term-matches-p term text work-state))
                 excluded))))

(defun agenda-search-view-match-entry (document heading indices)
  (multiple-value-bind (line end-line) (node-line-range heading)
    (make-agenda-match-entry
     :document document :node heading
     :heading-path (node-heading-path-with-indices heading indices)
     :line line :end-line end-line)))

(defun agenda-search-view (documents query
                           &key sorting-strategy user-defined-sorter
                             time-context)
  "Return headings whose subtrees satisfy the Org search terms in QUERY."
  (let* ((documents (agenda-normalize-documents documents))
         (limits (agenda-search-view-operation-limits documents))
         (terms (agenda-search-view-parse-query query limits))
         (seen (make-hash-table :test #'eq))
         (count 0)
         (entries nil))
    (dolist (document documents)
      (let ((work-state (%make-agenda-search-work-state
                         (document-limits document))))
        (multiple-value-bind (headings timestamps indices)
            (agenda-document-heading-data document :collect-headlines-p t)
          (declare (ignore timestamps))
          (dolist (heading headings)
            (let ((text (or (node-text heading) "")))
              (when (and (not (gethash heading seen))
                         (agenda-search-view-matches-p terms text work-state))
                (setf (gethash heading seen) t)
                (incf count)
                (check-limit limits :max-agenda-entries count)
                (push (agenda-search-view-match-entry document heading indices)
                      entries)))))))
    (agenda-sort-view-entries (nreverse entries) sorting-strategy
                              :time-context time-context
                              :user-defined-sorter user-defined-sorter)))

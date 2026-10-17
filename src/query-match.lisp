;;;; src/query-match.lisp
(in-package #:org-kit)

(defstruct (query-match-parser (:constructor %make-query-match-parser))
  source
  length
  (position 0)
  context
  limits
  reference-time)

(defun query-match-ascii-letter-p (character)
  (and character
       (or (<= (char-code #\A) (char-code character) (char-code #\Z))
           (<= (char-code #\a) (char-code character) (char-code #\z)))))

(defun query-match-ascii-digit-p (character)
  (and character (<= (char-code #\0) (char-code character) (char-code #\9))))

(defun query-match-control-p (character)
  (and character
       (or (< (char-code character) #x20)
           (= (char-code character) #x7f))))

(defun query-match-property-name-char-p (character)
  (or (query-match-ascii-letter-p character)
      (query-match-ascii-digit-p character)
      (and character (char= character #\_))))

(defun query-match-property-escape-char-p (character)
  (and character
       (not (query-match-control-p character))
       (not (find character '(#\Space #\Tab #\Newline #\Return)))
       (or (query-match-property-name-char-p character)
           (and (graphic-char-p character)
                (not (alphanumericp character))))))

(defun query-match-parser-at-end-p (parser)
  (>= (query-match-parser-position parser)
      (query-match-parser-length parser)))

(defun query-match-parser-peek (parser &optional (offset 0))
  (let ((position (+ (query-match-parser-position parser) offset)))
    (when (< position (query-match-parser-length parser))
      (char (query-match-parser-source parser) position))))

(defun query-match-parser-fail (parser position message)
  (query-signal-error (query-match-parser-source parser) position message))

(defun query-match-operator-at (source position)
  (loop for (text operator) in '(("<=" :<=) (">=" :>=) ("<>" :!=)
                                 ("==" :=) ("!=" :!=) ("/=" :!=)
                                 ("<" :<) ("=" :=) (">" :>))
        when (and (<= (+ position (length text)) (length source))
                  (string= text source :start2 position
                                       :end2 (+ position (length text))))
          return (values text operator)))

(defun query-match-property-prefix (parser)
  (let* ((source (query-match-parser-source parser))
         (length (query-match-parser-length parser))
         (position (query-match-parser-position parser))
         (cursor position)
         (name (make-string-output-stream))
         (name-length 0))
    (loop while (< cursor length)
          for character = (char source cursor)
          do (cond
               ((query-match-property-name-char-p character)
                (write-char character name)
                (incf name-length)
                (incf cursor))
               ((char= character #\\)
                (if (and (< (1+ cursor) length)
                         (query-match-property-escape-char-p
                          (char source (1+ cursor))))
                    (progn
                      (write-char (char source (1+ cursor)) name)
                      (incf name-length)
                      (incf cursor 2))
                    (return)))
               (t (return))))
    (when (plusp name-length)
      (multiple-value-bind (surface operator)
          (query-match-operator-at source cursor)
        (when operator
          (values (get-output-stream-string name) surface operator cursor))))))

(defun query-match-and (predicates)
  (let ((children '()))
    (dolist (predicate predicates)
      (if (eq (car predicate) :and)
          (setf children (nconc children (copy-list (cdr predicate))))
          (setf children (nconc children (list predicate)))))
    (cond ((null (cdr children)) (car children))
          (t (cons :and children)))))

(defun query-match-or (predicates)
  (let ((children '()))
    (dolist (predicate predicates)
      (setf children (nconc children (list predicate))))
    (cond ((null (cdr children)) (car children))
          (t (cons :or children)))))

(defun query-match-negate (predicate)
  (cond
    ((eq (car predicate) :exists)
     (cons :missing (cdr predicate)))
    (t (list :not predicate))))

(defun query-match-planning-kind (name)
  (cond ((string= name "SCHEDULED") :scheduled)
        ((string= name "DEADLINE") :deadline)
        ((string= name "CLOSED") :closed)
        (t nil)))

(defun query-match-parse-regex (parser)
  (let* ((source (query-match-parser-source parser))
         (start (query-match-parser-position parser))
         (body-start (1+ start))
         (depth 1)
         (cursor body-start))
    (loop while (< cursor (query-match-parser-length parser))
          for character = (char source cursor)
          do (cond
               ((query-match-control-p character)
                (query-match-parser-fail parser cursor
                                         "Control character in regular expression"))
               ((char= character #\\)
                (when (= (1+ cursor) (query-match-parser-length parser))
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
                    (let* ((pattern (subseq source body-start cursor))
                           (compiled
                             (query-compile-emacs-regex
                              pattern (query-match-parser-context parser)
                              (query-match-parser-limits parser) source body-start)))
                      (setf (query-match-parser-position parser) (1+ cursor))
                      (return-from query-match-parse-regex compiled))
                    (incf cursor)))
               (t (incf cursor)))
          finally (query-match-parser-fail parser start
                                          "Unclosed regular expression"))))

(defun query-match-parse-tag-text (parser)
  (let ((source (query-match-parser-source parser))
        (length (query-match-parser-length parser))
        (position (query-match-parser-position parser)))
    (with-output-to-string (output)
      (loop while (< position length)
            for character = (char source position)
            do (cond
                 ((char= character #\\)
                  (when (= (1+ position) length)
                    (query-match-parser-fail parser position
                                             "Incomplete escape in Org match term"))
                  (let ((escaped (char source (1+ position))))
                    (when (query-match-control-p escaped)
                      (query-match-parser-fail parser position
                                               "Control character in Org match term"))
                    (write-char escaped output))
                  (incf position 2))
                 ((find character '(#\Space #\Tab #\Newline #\Return
                                   #\+ #\- #\& #\| #\/ #\( #\)
                                   #\{ #\}))
                  (return))
                 ((query-match-control-p character)
                  (query-match-parser-fail parser position
                                           "Control character in Org match term"))
                 (t
                  (write-char character output)
                  (incf position))))
      (setf (query-match-parser-position parser) position))))

(defun query-match-parse-quoted-string (parser)
  (let ((source (query-match-parser-source parser))
        (start (query-match-parser-position parser)))
    (incf (query-match-parser-position parser))
    (with-output-to-string (output)
      (loop until (query-match-parser-at-end-p parser)
            for position = (query-match-parser-position parser)
            for character = (char source position)
            do (cond
                 ((char= character #\")
                  (incf (query-match-parser-position parser))
                  (return-from query-match-parse-quoted-string
                    (get-output-stream-string output)))
                 ((char= character #\\)
                  (when (= (1+ position) (query-match-parser-length parser))
                    (query-match-parser-fail parser position
                                             "Incomplete escape in quoted value"))
                  (let ((escaped (char source (1+ position))))
                    (case escaped
                      (#\\ (write-char #\\ output))
                      (#\" (write-char #\" output))
                      (#\n (write-char #\Newline output))
                      (#\r (write-char #\Return output))
                      (#\t (write-char #\Tab output))
                      (otherwise
                       (query-match-parser-fail parser position
                                                "Unsupported escape in quoted value")))
                    (incf (query-match-parser-position parser) 2)))
                 ((query-match-control-p character)
                  (query-match-parser-fail parser position
                                           "Control character in quoted value"))
                 (t
                  (write-char character output)
                  (incf (query-match-parser-position parser))))
            finally (query-match-parser-fail parser start
                                            "Unclosed quoted value")))))

(defun query-match-parse-number (parser)
  (let* ((source (query-match-parser-source parser))
         (start (query-match-parser-position parser))
         (cursor start)
         (length (query-match-parser-length parser)))
    (when (char= (char source cursor) #\-)
      (incf cursor))
    (let ((integer-start cursor))
      (loop while (and (< cursor length)
                       (query-match-ascii-digit-p (char source cursor)))
            do (incf cursor))
      (when (= integer-start cursor)
        (query-match-parser-fail parser start "Expected a number or quoted value"))
      (when (and (< cursor length) (char= (char source cursor) #\.))
        (incf cursor)
        (let ((fraction-start cursor))
          (loop while (and (< cursor length)
                           (query-match-ascii-digit-p (char source cursor)))
                do (incf cursor))
          (when (= fraction-start cursor)
            (query-match-parser-fail parser start "Invalid numeric value"))))
      (let* ((text (subseq source start cursor))
             (negative-p (char= (char text 0) #\-))
             (dot (position #\. text))
             (number
               (if dot
                   (let* ((whole-text (subseq text (if negative-p 1 0) dot))
                          (fraction-text (subseq text (1+ dot)))
                          (whole (parse-integer whole-text))
                          (fraction (/ (parse-integer fraction-text)
                                       (expt 10 (length fraction-text)))))
                     (if negative-p (- (+ whole fraction)) (+ whole fraction)))
                   (parse-integer text))))
        (setf (query-match-parser-position parser) cursor)
        number))))

(defun query-match-parse-property-value (parser)
  (let ((position (query-match-parser-position parser)))
    (cond
      ((query-match-parser-at-end-p parser)
       (query-match-parser-fail parser position "Missing property comparison value"))
      ((char= (query-match-parser-peek parser) #\{)
       (let ((pattern-start (1+ position))
             (regex (query-match-parse-regex parser)))
         (values regex :regex
                 (subseq (query-match-parser-source parser)
                         pattern-start
                         (1- (query-match-parser-position parser)))))
       )
      ((char= (query-match-parser-peek parser) #\")
       (let* ((literal-position position)
              (text (query-match-parse-quoted-string parser)))
         (if (and (>= (length text) 2)
                  (char= (char text 0) #\<)
                  (char= (char text (1- (length text))) #\>))
             (values (query-parse-org-date-literal
                      text (query-match-parser-reference-time parser)
                      (query-match-parser-source parser) literal-position)
                     :date text)
             (values text :string text))))
      (t (values (query-match-parse-number parser) :number nil)))))

(defun query-match-parse-property (parser name surface operator operator-position)
  (let* ((planning-kind (query-match-planning-kind name))
         (regex-operator-p (member surface '("=" "==" "<>" "!=" "/=")
                                   :test #'string=))
         (regex-p nil)
         (require-present-p nil)
         (operator operator))
    (when (and (not (query-match-parser-at-end-p parser))
               (char= (query-match-parser-peek parser) #\*))
      (setf require-present-p t)
      (incf (query-match-parser-position parser)))
    (multiple-value-bind (value value-type regex-source)
        (query-match-parse-property-value parser)
      (setf regex-p (eq value-type :regex))
      (when (and regex-p (not regex-operator-p))
        (query-match-parser-fail parser operator-position
                                 "Ordered comparisons do not accept regular expressions"))
      (when regex-p
        (setf operator
              (if (member surface '("<>" "!=" "/=") :test #'string=)
                  :not-matches :matches)))
      (cond
        ((string= name "LEVEL")
         (list :level operator value))
        ((string= name "TODO")
         (unless (member operator '(:= :!= :matches :not-matches))
           (query-match-parser-fail parser operator-position
                                    "TODO supports equality and regular-expression operators"))
         (let ((predicate (list :todo operator value)))
           (if require-present-p
               (query-match-and
                (list predicate (list :not (list :todo-missing))))
               predicate)))
        ((and planning-kind regex-p require-present-p
              (eq operator :matches)
              (string= regex-source ".*"))
         (list :exists :planning planning-kind))
        ((and regex-p require-present-p (eq operator :matches)
              (string= regex-source ".*"))
         (list :exists :property name))
        (planning-kind
         (if regex-p
             (list :property :name name :operator operator :value value
                   :missing-as :nonmatch
                   :require-present-p (and require-present-p t))
             (list :planning planning-kind operator value)))
        (t
         (list :property :name name :operator operator :value value
               :missing-as (if (eq value-type :number) :zero :nonmatch)
               :require-present-p (and require-present-p t)))))))

(defun query-match-parse-regex-term (parser section)
  (let ((regex (query-match-parse-regex parser)))
    (if (eq section :todo)
        (list :todo :matches regex)
        (list :tag :matches regex))))

(defun query-match-parse-term (parser section)
  (let ((character (query-match-parser-peek parser)))
    (cond
      ((null character)
       (query-match-parser-fail parser (query-match-parser-position parser)
                                "Expected an Org match term"))
      ((char= character #\{)
       (query-match-parse-regex-term parser section))
      ((find character '(#\Space #\Tab #\Newline #\Return #\& #\| #\/ #\+
                         #\- #\( #\)))
       (query-match-parser-fail parser (query-match-parser-position parser)
                                (if (find character '(#\( #\)))
                                    "Parenthesized groups are not supported"
                                    "Expected an Org match term")))
      (t
       (multiple-value-bind (name surface operator operator-position)
           (and (eq section :match) (query-match-property-prefix parser))
         (if name
             (progn
               (setf (query-match-parser-position parser)
                     (+ operator-position (length surface)))
               (query-match-parse-property parser name surface operator
                                           operator-position))
             (let ((text (query-match-parse-tag-text parser)))
               (if (eq section :todo)
                   (list :todo := text)
                   (list :tag := text)))))))))

(defun query-match-parse-factor (parser section)
  (let ((sign (when (find (query-match-parser-peek parser) '(#\+ #\-))
                (prog1 (query-match-parser-peek parser)
                  (incf (query-match-parser-position parser))))))
    (when (and sign (query-match-parser-at-end-p parser))
      (query-match-parser-fail parser (query-match-parser-position parser)
                               "Sign must be followed by a term"))
    (let ((predicate (query-match-parse-term parser section)))
      (if (and sign (char= sign #\-))
          (query-match-negate predicate)
          predicate))))

(defun query-match-parse-and-expression (parser section)
  (let ((predicates (list (query-match-parse-factor parser section))))
    (loop for character = (query-match-parser-peek parser)
          while (member character '(#\& #\+ #\-))
          do (when (eql character #\&)
               (incf (query-match-parser-position parser)))
             (push (query-match-parse-factor parser section) predicates))
    (query-match-and (nreverse predicates))))

(defun query-match-parse-or-expression (parser section)
  (let ((predicates (list (query-match-parse-and-expression parser section))))
    (loop while (eql (query-match-parser-peek parser) #\|)
          do (incf (query-match-parser-position parser))
             (push (query-match-parse-and-expression parser section) predicates))
    (query-match-or (nreverse predicates))))

(defun query-parse-org-match (source context limits reference-time)
  "Parse SOURCE using Org match syntax and return a canonical predicate tree."
  (check-type source string)
  (check-type limits limits)
  (when context
    (check-type context query-context))
  (check-limit limits :max-input-length (length source))
  (when (zerop (length source))
    (query-signal-error source 0 "Org match cannot be empty"))
  (let* ((parser (%make-query-match-parser
                  :source source :length (length source) :context context
                  :limits limits :reference-time reference-time))
         (tag-predicate nil)
         (todo-predicate nil))
    (unless (eql (query-match-parser-peek parser) #\/)
      (setf tag-predicate (query-match-parse-or-expression parser :match)))
    (when (eql (query-match-parser-peek parser) #\/)
      (incf (query-match-parser-position parser))
      (let ((todo-only-p (eql (query-match-parser-peek parser) #\!)))
        (when todo-only-p (incf (query-match-parser-position parser)))
        (if (query-match-parser-at-end-p parser)
            (if todo-only-p
                (setf todo-predicate '(:todo-present))
                (query-match-parser-fail parser (query-match-parser-position parser)
                                         "TODO section cannot be empty"))
            (let ((todo-expression
                    (query-match-parse-or-expression parser :todo)))
              (setf todo-predicate
                    (if todo-only-p
                        (query-match-and (list '(:todo-present) todo-expression))
                        todo-expression))))
      (when (eql (query-match-parser-peek parser) #\/)
        (query-match-parser-fail parser (query-match-parser-position parser)
                                 "Only one TODO section is allowed"))))
    (unless (query-match-parser-at-end-p parser)
      (let ((character (query-match-parser-peek parser)))
        (query-match-parser-fail
         parser (query-match-parser-position parser)
         (if (find character '(#\( #\)))
             "Parenthesized groups are not supported"
             "Unexpected character in Org match"))))
    (query-match-and (remove nil (list tag-predicate todo-predicate)))))

(defun query-match-at (document match reference-time &key context)
  "Return headlines matching Org MATCH using REFERENCE-TIME.
REFERENCE-TIME must be a cl-date-kit:LOCAL-DATE-TIME."
  (check-type document document)
  (check-type match string)
  (check-type reference-time cl-date-kit:local-date-time)
  (when context
    (check-type context query-context))
  (let* ((limits (document-limits document))
         (predicate (query-parse-org-match match context limits reference-time))
         (query-ir (%make-query-ir :headlines predicate context)))
    (query-evaluate-ir document query-ir :reference-time reference-time)))

(defun query-match-now-reference-time (time-context)
  (unless (org-time-context-p time-context)
    (%org-time-error :invalid-time time-context))
  (cl-date-kit:local-date-time-of-instant
   (org-time-context-reference-instant time-context)
   (org-time-zone-backend (org-time-context-zone time-context))))

(defun query-match-now (document match &key time-context context)
  "Return headlines matching Org MATCH using TIME-CONTEXT's local time."
  (query-match-at document match
                  (query-match-now-reference-time time-context)
                  :context context))

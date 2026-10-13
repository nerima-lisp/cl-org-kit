;;;; src/query-eval.lisp
(in-package #:org-kit)

(defstruct (query-evaluation-state (:constructor %make-query-evaluation-state))
  document
  context
  reference-time
  limits
  regex-cache
  tag-groups
  depth)

(defun query-signal-error (query position message)
  (error 'org-query-error :message message
         :query (bounded-text (or query ""))
         :position position))

(defun query-canonical-list-length (value limits)
  "Count a proper list already normalized by a public query parser."
  (let ((length (length value)))
    (check-limit limits :max-node-count length)
    length))

(defun query-translate-emacs-regex (pattern max-depth query source-offset)
  (let ((output (make-array 0 :element-type 'character :fill-pointer 0
                             :adjustable t))
        (source-map '())
        (position 0)
        (class-start nil)
        (group-starts '()))
    (labels ((emit-character (character source-position)
               (vector-push-extend character output)
               (push source-position source-map))
             (emit-pair (first second source-position)
               (emit-character first source-position)
               (emit-character second source-position))
             (fail-at (at message)
               (query-signal-error query (+ source-offset at) message))
             (digit-p (character)
               (and character (<= (char-code #\0) (char-code character)
                                  (char-code #\9))))
             (interval-end (start)
               (loop for cursor from start below (1- (length pattern))
                     when (and (char= (char pattern cursor) #\\)
                               (char= (char pattern (1+ cursor)) #\}))
                       return cursor))
             (group-open-before-p (at)
               (or (and (>= at 2)
                        (char= (char pattern (- at 2)) #\\)
                        (find (char pattern (1- at)) '(#\( #\|)))
                   (and (>= at 4)
                        (char= (char pattern (- at 4)) #\\)
                        (char= (char pattern (- at 3)) #\()
                        (char= (char pattern (- at 2)) #\?)
                        (char= (char pattern (1- at)) #\:))))
             (group-close-after-p (at)
               (and (< (+ at 2) (length pattern))
                    (char= (char pattern (1+ at)) #\\)
                    (find (char pattern (+ at 2)) '(#\) #\|))))
             (parse-interval (open end)
               (let* ((body-start (+ open 2))
                      (body-end end)
                      (comma (position #\, pattern :start body-start :end body-end))
                      (second-comma (and comma
                                         (position #\, pattern :start (1+ comma)
                                                   :end body-end)))
                      (lower-start body-start)
                      (lower-end (or comma body-end))
                      (upper-start (and comma (1+ comma)))
                      (upper-end body-end)
                      (empty-lower-p (= lower-start lower-end))
                      (empty-upper-p (and comma (= upper-start upper-end)))
                      (lower-text (subseq pattern lower-start lower-end))
                      (upper-text (and comma
                                       (subseq pattern upper-start upper-end))))
                 (when second-comma
                   (fail-at open "Invalid repetition interval"))
                 (unless (and (or (not empty-lower-p)
                                  (and comma (not empty-upper-p)))
                              (every #'digit-p lower-text)
                              (or (not comma)
                                  (or empty-upper-p (every #'digit-p upper-text))))
                   (fail-at open "Invalid repetition interval"))
                 (let ((lower (unless empty-lower-p (parse-integer lower-text)))
                       (upper (when (and comma (not empty-upper-p))
                                (parse-integer upper-text))))
                   (when (or (and lower (> lower 1000))
                             (and upper (> upper 1000)))
                     (fail-at open "Repetition interval exceeds its limit"))
                   (when (and lower upper (> lower upper))
                     (fail-at open "Repetition interval bounds are reversed"))
                   (emit-character #\{ open)
                   (if empty-lower-p
                       (emit-character #\0 (1+ open))
                       (loop for cursor from lower-start below lower-end
                             do (emit-character (char pattern cursor) cursor)))
                   (when comma
                     (emit-character #\, comma)
                     (unless empty-upper-p
                       (loop for cursor from upper-start below upper-end
                             do (emit-character (char pattern cursor) cursor))))
                   (emit-character #\} (1+ end))))))
      (loop while (< position (length pattern))
            for character = (char pattern position)
            do (cond
                 (class-start
                  (cond
                    ((char= character #\\)
                     (unless (< (1+ position) (length pattern))
                       (fail-at position "Incomplete escape in regular expression"))
                     (let ((escaped (char pattern (1+ position))))
                       (unless (find escaped '(#\\ #\] #\- #\^))
                         (fail-at position "Unsupported escape in character class"))
                       (emit-pair #\\ escaped position)
                       (incf position 2)))
                    ((and (char= character #\])
                          (= position (1+ class-start)))
                     (emit-pair #\\ #\] position)
                     (incf position))
                    ((and (char= character #\])
                          (> position (1+ class-start)))
                     (emit-character character position)
                     (setf class-start nil)
                     (incf position))
                    (t
                     (emit-character character position)
                     (incf position))))
                 ((char= character #\\)
                  (unless (< (1+ position) (length pattern))
                    (fail-at position "Incomplete escape in regular expression"))
                  (let ((escaped (char pattern (1+ position))))
                    (case escaped
                      (#\|
                       (emit-character #\| position)
                       (incf position 2))
                      (#\(
                       (push position group-starts)
                       (when (> (length group-starts) max-depth)
                         (fail-at position "Regular expression nesting limit exceeded"))
                       (emit-character #\( position)
                       (if (and (< (+ position 3) (length pattern))
                                (char= (char pattern (+ position 2)) #\?)
                                (char= (char pattern (+ position 3)) #\:))
                           (progn
                             (emit-character #\? (+ position 2))
                             (emit-character #\: (+ position 3))
                             (incf position 4))
                           (incf position 2)))
                      (#\)
                       (unless group-starts
                         (fail-at position "Unmatched group close in regular expression"))
                       (pop group-starts)
                       (emit-character #\) position)
                       (incf position 2))
                      (#\{
                       (let ((end (interval-end (+ position 2))))
                         (unless end
                           (fail-at position "Unclosed repetition interval"))
                         (parse-interval position end)
                         (setf position (+ end 2))))
                      (#\}
                       (fail-at position "Unmatched repetition interval close"))
                      ((#\+ #\?)
                       (emit-pair #\\ escaped position)
                       (incf position 2))
                      ((#\* #\. #\^ #\$ #\[ #\])
                       (emit-pair #\\ escaped position)
                       (incf position 2))
                      (#\\
                       (emit-pair #\\ #\\ position)
                       (incf position 2))
                      (otherwise
                       (fail-at position "Unsupported Emacs regular expression escape")))))
                 ((and (char= character #\()
                       (< (1+ position) (length pattern))
                       (char= (char pattern (1+ position)) #\?))
                  (fail-at position "cl-regex-specific constructs are not supported"))
                 ((char= character #\[)
                  (setf class-start position)
                  (emit-character character position)
                  (incf position))
                 ((find character '(#\( #\) #\|))
                  (emit-pair #\\ character position)
                  (incf position))
                 ((find character '(#\{ #\}))
                  (emit-pair #\\ character position)
                  (incf position))
                 ((and (char= character #\^)
                       (not (or (zerop position) (group-open-before-p position))))
                  (emit-pair #\\ character position)
                  (incf position))
                 ((and (char= character #\$)
                       (not (or (= position (1- (length pattern)))
                                (group-close-after-p position))))
                  (emit-pair #\\ character position)
                  (incf position))
                 (t
                  (emit-character character position)
                  (incf position))))
      (when class-start
        (fail-at class-start "Unclosed character class in regular expression"))
      (when group-starts
        (fail-at (car (last group-starts)) "Unclosed group in regular expression"))
      (values (coerce output 'string)
              (coerce (nreverse source-map) 'simple-vector)))))

(defun query-compile-emacs-regex (pattern context limits query &optional (source-offset 0))
  "Compile an Emacs-regex subset pattern for shared query evaluation.
PATTERN length and group nesting are bounded by LIMITS. QUERY and SOURCE-OFFSET
locate translation or compilation errors in the original selector."
  (check-type pattern string)
  (let* ((max-pattern-length (limit-value limits :max-input-length))
         (max-depth (limit-value limits :max-element-depth)))
    (check-limit limits :max-input-length (length pattern))
    (multiple-value-bind (translated source-map)
        (query-translate-emacs-regex pattern max-depth query source-offset)
      (handler-case
          (let ((compiled
                  (cl-regex-kit:compile-regex
                   translated
                   :case-insensitive
                   (and context (query-context-case-fold-p context))
                   :multi-line t
                   :size-limit (max 1 max-pattern-length)
                   :nest-limit max-depth)))
            (when (cl-regex-kit:regex-advanced-p compiled)
              (query-signal-error query source-offset
                                  "Advanced regular expressions are not supported"))
            (%make-query-regex pattern compiled source-map))
        (cl-regex-kit:regex-syntax-error (condition)
          (let* ((compiled-position
                   (cl-regex-kit:regex-syntax-error-position condition))
                 (pattern-position
                   (if (and compiled-position
                            (< -1 compiled-position (length source-map)))
                       (aref source-map compiled-position)
                       (length pattern))))
            (query-signal-error query (+ source-offset pattern-position)
                                "Invalid regular expression")))))))

(defun query-compiled-regex (value state)
  (cond
    ((query-regex-p value) (query-regex-compiled value))
    (t
     (or (gethash value (query-evaluation-state-regex-cache state))
         (setf (gethash value (query-evaluation-state-regex-cache state))
               (query-regex-compiled
                (query-compile-emacs-regex
                 value (query-evaluation-state-context state)
                 (query-evaluation-state-limits state) nil)))))))

(defun query-date-value-p (value)
  (or (cl-date-kit:local-date-p value)
      (cl-date-kit:local-date-time-p value)))

(defun query-date-time-value (value)
  (if (cl-date-kit:local-date-p value)
      (cl-date-kit:local-date-at-start-of-day value)
      value))

(defun query-date-compare (left right)
  (cond
    ((and (cl-date-kit:local-date-p left)
          (cl-date-kit:local-date-p right))
     (cl-date-kit:local-date-compare left right))
    (t
     (cl-date-kit:local-date-time-compare (query-date-time-value left)
                                          (query-date-time-value right)))))

(defun query-reference-date (reference-time)
  (cond
    ((cl-date-kit:local-date-p reference-time) reference-time)
    ((cl-date-kit:local-date-time-p reference-time)
     (cl-date-kit:local-date-time-date reference-time))
    (t nil)))

(defun query-date-literal-body (text)
  (let ((length (length text)))
    (cond
      ((and (>= length 2)
            (char= (char text 0) #\<)
            (char= (char text (1- length)) #\>))
       (subseq text 1 (1- length)))
      ((and (>= length 2)
            (char= (char text 0) #\[)
            (char= (char text (1- length)) #\]))
       (subseq text 1 (1- length)))
      (t text))))

(defun query-org-relative-date (body reference-time)
  (let ((reference-date (query-reference-date reference-time)))
    (cond
      ((string-equal body "now")
       (and (or (cl-date-kit:local-date-p reference-time)
                (cl-date-kit:local-date-time-p reference-time))
            reference-time))
      ((string-equal body "today") reference-date)
      ((string-equal body "tomorrow")
       (and reference-date (cl-date-kit:local-date-plus-days reference-date 1)))
      ((and reference-date
            (>= (length body) 3)
            (find (char body 0) '(#\+ #\-))
            (every #'digit-char-p (subseq body 1 (1- (length body))))
            (find (char body (1- (length body))) '(#\d #\w #\m #\y)))
       (let* ((amount (parse-integer body :start 1 :end (1- (length body))))
              (amount (if (char= (char body 0) #\-) (- amount) amount))
              (unit (char body (1- (length body)))))
         (case unit
           (#\d (cl-date-kit:local-date-plus-days reference-date amount))
           (#\w (cl-date-kit:local-date-plus-weeks reference-date amount))
           (#\m (cl-date-kit:local-date-plus-months reference-date amount))
           (#\y (cl-date-kit:local-date-plus-years reference-date amount)))))
      (t nil))))

(defun query-org-date-time-token-p (token)
  (and (= (length token) 5)
       (char= (char token 2) #\:)
       (every #'digit-char-p (concatenate 'string
                                          (subseq token 0 2)
                                          (subseq token 3 5)))))

(defun query-org-date-time-start (token)
  (cond
    ((query-org-date-time-token-p token) token)
    ((and (= (length token) 11)
          (char= (char token 5) #\-)
          (query-org-date-time-token-p (subseq token 0 5))
          (query-org-date-time-token-p (subseq token 6 11)))
     (subseq token 0 5))
    (t nil)))

(defun query-org-weekday-token-p (token)
  (member token '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
          :test #'string-equal))

(defun query-org-date-suffix-tokens (body)
  (let ((tokens '())
        (position 10)
        (length (length body)))
    (loop while (< position length)
          do (loop while (and (< position length)
                              (find (char body position) '(#\Space #\Tab)))
                    do (incf position))
             (when (< position length)
               (let ((start position))
                 (loop while (and (< position length)
                                  (not (find (char body position)
                                             '(#\Space #\Tab))))
                       do (incf position))
                 (push (subseq body start position) tokens))))
    (nreverse tokens)))

(defun query-parse-absolute-org-date (body)
  (when (and (>= (length body) 10)
             (char= (char body 4) #\-)
             (char= (char body 7) #\-)
             (every #'digit-char-p (subseq body 0 4))
             (every #'digit-char-p (subseq body 5 7))
             (every #'digit-char-p (subseq body 8 10)))
    (let* ((date (cl-date-kit:make-local-date
                  (parse-integer body :start 0 :end 4)
                  (parse-integer body :start 5 :end 7)
                  (parse-integer body :start 8 :end 10)))
           (tokens (query-org-date-suffix-tokens body))
           (tokens (if (and tokens (query-org-weekday-token-p (first tokens)))
                       (rest tokens)
                       tokens))
           (time-token (and tokens (query-org-date-time-start (first tokens)))))
      (when (and (<= (length tokens) 1)
                 (or (null tokens) time-token))
        (if time-token
            (cl-date-kit:make-local-date-time
             date
             (cl-date-kit:make-local-time
              (parse-integer time-token :start 0 :end 2)
              (parse-integer time-token :start 3 :end 5)
              0))
            date)))))

(defun query-parse-org-date-literal (text reference-time
                                     &optional query position silent-p)
  "Parse an Org date literal into a cl-date-kit date or date-time.
TEXT accepts Org brackets, ISO dates, and the supported relative words.
Relative values require REFERENCE-TIME; QUERY and POSITION locate query errors.
When SILENT-P is true, invalid values return NIL for document-property coercion."
  (let* ((body (query-date-literal-body text))
         (value
           (handler-case
               (or (query-org-relative-date body reference-time)
                   (query-parse-absolute-org-date body))
             (error () nil))))
    (cond
      (value value)
      (silent-p nil)
      (t
       (query-signal-error query position
                           (if (member body '("today" "tomorrow" "now")
                                       :test #'string-equal)
                               "Relative date requires a reference time"
                               "Invalid Org date literal"))))))

(defun query-number-from-string (string)
  (let* ((length (length string))
         (position 0)
         (negative-p nil)
         (integer-start nil)
         (integer-end nil)
         (fraction-start nil)
         (fraction-end nil))
    (when (and (< position length)
               (find (char string position) '(#\+ #\-)))
      (setf negative-p (char= (char string position) #\-))
      (incf position))
    (setf integer-start position)
    (loop while (and (< position length) (digit-char-p (char string position)))
          do (incf position))
    (setf integer-end position)
    (when (and (< position length) (char= (char string position) #\.))
      (incf position)
      (setf fraction-start position)
      (loop while (and (< position length) (digit-char-p (char string position)))
            do (incf position))
      (setf fraction-end position))
    (when (and (= position length)
               (or (> integer-end integer-start)
                   (and fraction-start (> fraction-end fraction-start)))
               (or (null fraction-start)
                   (> fraction-end fraction-start)))
      (let* ((whole (if (> integer-end integer-start)
                        (parse-integer string :start integer-start :end integer-end)
                        0))
             (fraction (if fraction-start
                           (parse-integer string :start fraction-start :end fraction-end)
                           0))
             (scale (if fraction-start
                        (expt 10 (- fraction-end fraction-start))
                        1))
             (number (+ whole (/ fraction scale))))
        (if negative-p (- number) number)))))

(defun query-coerce-actual-value (actual expected reference-time)
  (cond
    ((and (numberp expected) (stringp actual))
     (or (query-number-from-string actual) actual))
    ((and (query-date-value-p expected) (node-p actual)
          (eq (node-type actual) :timestamp))
     (timestamp-start actual))
    ((and (query-date-value-p expected) (stringp actual))
     (or (query-parse-org-date-literal actual reference-time nil nil t)
         actual))
    (t actual)))

(defun query-value-string (value)
  (cond
    ((stringp value) value)
    ((characterp value) (string value))
    ((numberp value) (princ-to-string value))
    ((symbolp value) (symbol-name value))
    ((and (node-p value) (eq (node-type value) :timestamp))
     (or (node-text value) ""))
    ((cl-date-kit:local-date-p value)
     (cl-date-kit:format-local-date value))
    ((cl-date-kit:local-date-time-p value)
     (cl-date-kit:format-local-date-time value))
    (t "")))

(defun query-regex-match-p (pattern value state)
  (let ((text (query-value-string value)))
    (cl-regex-kit:is-match-p (query-compiled-regex pattern state) text)))

(defun query-compare-values (operator expected actual reference-time)
  (let* ((actual (query-coerce-actual-value actual expected reference-time))
         (comparison
           (cond
             ((and (numberp expected) (numberp actual))
              (cond ((< actual expected) -1)
                    ((> actual expected) 1)
                    (t 0)))
             ((and (query-date-value-p expected) (query-date-value-p actual))
              (query-date-compare actual expected))
             (t
              (let ((left (query-value-string actual))
                    (right (query-value-string expected)))
                (cond ((string= left right) 0)
                      ((string< left right) -1)
                      (t 1)))))))
    (when comparison
      (case operator
        ((:= :==) (zerop comparison))
        ((:!= :<> :/=) (not (zerop comparison)))
        ((:<) (minusp comparison))
        ((:<=) (not (plusp comparison)))
        ((:>) (plusp comparison))
        ((:>=) (not (minusp comparison)))))))

(defun query-predicate-children (predicate)
  (case (car predicate)
    ((:and :or)
     (cdr predicate))
    (:not
     (list (second predicate)))
    (otherwise nil)))

(defun query-validate-predicate-tree (predicate limits)
  "Validate nested boolean predicates iteratively before candidate traversal."
  (let ((pending (list (list predicate 1)))
        (total 0))
    (loop while pending
          for entry = (pop pending)
          for node = (first entry)
          for depth = (second entry)
          do (check-limit limits :max-element-depth depth)
             (incf total (query-canonical-list-length node limits))
             (check-limit limits :max-node-count total)
             (dolist (child (reverse (query-predicate-children node)))
               (push (list child (1+ depth)) pending)))
  t))

(defun query-check-predicate (state)
  (let ((depth (1+ (query-evaluation-state-depth state))))
    (check-limit (query-evaluation-state-limits state) :max-element-depth depth)
    depth))

(defun query-path-segment-match-p (selector segment)
  (cond
    ((stringp selector)
     (string= selector (getf segment :title)))
    ((eq selector :*) t)
    (t
     (and (string= (second selector) (getf segment :title))
          (= (third selector) (getf segment :index))))))

(defun query-path-matches-p (selector path limits)
  (let* ((selector-count (query-canonical-list-length selector limits))
         (path-count (query-canonical-list-length path limits))
         (previous (make-array (1+ path-count) :initial-element nil)))
    (check-limit limits :max-element-depth selector-count)
    (setf (aref previous 0) t)
    (dolist (part selector)
      (let ((next (make-array (1+ path-count) :initial-element nil)))
        (if (eq part :**)
            (progn
              (setf (aref next 0) (aref previous 0))
              (loop for index from 1 to path-count
                    do (setf (aref next index)
                             (or (aref previous index)
                                 (aref next (1- index))))))
            (loop for index from 1 to path-count
                  do (setf (aref next index)
                           (and (aref previous (1- index))
                                (query-path-segment-match-p
                                 part (nth (1- index) path))))))
        (setf previous next)))
    (aref previous path-count)))

(defun query-node-property-by-name (node name)
  (loop for (key value) on (node-properties node) by #'cddr
        when (and (symbolp key) (string= name (symbol-name key)))
          do (return-from query-node-property-by-name (values value t)))
  (values nil nil))

(defun query-drawer-property (headline name)
  (labels ((search-node (node)
             (unless (eq (node-type node) :headline)
               (when (eq (node-type node) :property-drawer)
                 (dolist (property (node-children node))
                   (when (and (eq (node-type property) :node-property)
                              (string= name (node-property property :key "")))
                     (return-from query-drawer-property
                       (values (node-property property :value) t)))))
               (unless (eq (node-type node) :property-drawer)
                 (dolist (child (node-children node))
                   (search-node child))))))
    (dolist (child (node-children headline))
      (search-node child)))
  (values nil nil))

(defun query-local-property (headline name)
  (multiple-value-bind (value present) (query-node-property-by-name headline name)
    (if present
        (values value t)
        (query-drawer-property headline name))))

(defun query-heading-chain (headline)
  (loop for current = headline then (query-nearest-parent-headline current)
        while current collect current))

(defun query-planning-value (headline kind)
  (let ((key (ecase kind
               (:scheduled :scheduled)
               (:deadline :deadline)
               (:closed :closed))))
    (multiple-value-bind (timestamp present) (node-property headline key)
      (values (when (and present (node-p timestamp)
                         (eq (node-type timestamp) :timestamp))
                (timestamp-start timestamp))
              (and present (node-p timestamp)
                   (eq (node-type timestamp) :timestamp))))))

(defun query-done-keyword-p (keyword document)
  (let* ((settings (document-settings document))
         (sequences (getf settings :todo-sequences))
         (keywords (getf settings :todo-keywords)))
    (or (loop for sequence in sequences
              thereis (member keyword (getf sequence :done-keywords)
                              :test #'string=))
        (loop for entry in keywords
              thereis (and (consp entry) (stringp (car entry))
                           (string= keyword (car entry))
                           (eq (cdr entry) :done)))
        (and (null sequences) (null keywords) (string= keyword "DONE")))))

(defun query-todo-keyword (headline)
  (let ((value (node-property headline :todo-keyword)))
    (and (stringp value) value)))

(defun query-todo-state (headline document)
  (let ((keyword (query-todo-keyword headline))
        (type (node-property headline :todo-type)))
    (cond ((member type '(:todo :done)) type)
          ((query-done-keyword-p keyword document) :done)
          (t :todo))))

(defun query-todo-present-p (headline document)
  (and (query-todo-keyword headline)
       (not (eq (query-todo-state headline document) :done))))

(defun query-tag-group-name (entry)
  (getf entry :group))

(defun query-tag-group-by-name (name groups)
  (find name groups :key #'query-tag-group-name :test #'string=))

(defun query-merged-tag-groups (state)
  (or (query-evaluation-state-tag-groups state)
      (let* ((context-groups
               (query-context-tag-groups (query-evaluation-state-context state)))
             (document-groups
               (getf (document-settings (query-evaluation-state-document state))
                     :tag-alist))
             (merged (copy-tree context-groups)))
        (dolist (entry document-groups)
          (let ((name (query-tag-group-name entry)))
            (when name
              (let ((existing (position name merged
                                        :key #'query-tag-group-name
                                        :test #'string=)))
                (if existing
                    (setf (nth existing merged) (copy-tree entry))
                    (setf merged (append merged (list (copy-tree entry)))))))))
        (setf (query-evaluation-state-tag-groups state) merged))))

(defun query-validate-tag-group-chain (name groups stack depth state)
  (check-limit (query-evaluation-state-limits state) :max-element-depth depth)
  (when (member name stack :test #'string=)
    (query-signal-error nil nil "Cyclic tag group definition"))
  (let ((group (query-tag-group-by-name name groups)))
    (when group
      (dolist (member (getf group :members))
        (cond
          ((stringp member)
           (when (query-tag-group-by-name member groups)
             (query-validate-tag-group-chain member groups (cons name stack)
                                             (1+ depth) state)))
          ((and (%query-proper-list-p member)
                (eq (first member) :regex)
                (= (length member) 2)
                (stringp (second member)))
           (query-compiled-regex (second member) state))
          (t (query-signal-error nil nil "Invalid tag group member")))))))

(defun query-validate-tag-groups (state)
  (let ((groups (query-merged-tag-groups state)))
    (dolist (entry groups)
      (let ((name (query-tag-group-name entry)))
        (when name
          (query-validate-tag-group-chain name groups '() 1 state))))))

;; QUERY-EVALUATE-TAG validates the complete group graph before matching.
(defun query-tag-in-group-p (tag group groups state)
  (some
   (lambda (member)
     (cond
       ((stringp member)
        (if (string= member tag)
            t
            (let ((nested (query-tag-group-by-name member groups)))
              (and nested
                     (query-tag-in-group-p tag nested groups state)))))
       (t
        (query-regex-match-p (second member) tag state))))
   (getf group :members)))

(defun query-tag-name-matches-p (tag requested groups state)
  (or (string= tag requested)
      (let ((group (query-tag-group-by-name requested groups)))
        (and group (query-tag-in-group-p tag group groups state)))))

(defun query-append-unique-tags (tags result)
  (dolist (tag tags result)
    (when (and (stringp tag) (not (member tag result :test #'string=)))
      (setf result (append result (list tag))))))

(defun query-effective-tags (headline state)
  (let* ((context (query-evaluation-state-context state))
         (document (query-evaluation-state-document state))
         (inherit-p (query-context-tag-inheritance-p context))
         (headings (if inherit-p (query-heading-chain headline) (list headline)))
         (tags '()))
    (dolist (current headings)
      (setf tags (query-append-unique-tags
                  (node-property current :tags) tags)))
    (when inherit-p
      (setf tags (query-append-unique-tags
                  (getf (document-settings document) :file-tags) tags)))
    tags))

(defun query-tags-string (tags)
  (if tags
      (with-output-to-string (stream)
        (dolist (tag tags)
          (write-char #\: stream)
          (write-string tag stream))
        (write-char #\: stream))
      ""))

(defun query-category-from-source-name (document)
  (let ((source-name (document-source-name document)))
    (when (and source-name (plusp (length source-name)))
      (let* ((name (pathname-name (pathname source-name)))
             (type (pathname-type (pathname source-name))))
        (if type name (or name (file-namestring (pathname source-name))))))))

(defun query-category-value (document headline context)
  (or (query-context-category context)
      (loop for current in (query-heading-chain headline)
            thereis (multiple-value-bind (value present)
                        (query-local-property current "CATEGORY")
                      (and present value)))
      (getf (document-settings document) :category)
      (query-category-from-source-name document)))

(defun query-entry-timestamp (headline timestamp-type)
  (labels ((search-node (node)
             (when (and (not (eq node headline))
                        (eq (node-type node) :headline))
               (return-from search-node nil))
             (unless (eq (node-type node) :property-drawer)
               (when (eq (node-type node) :timestamp)
                 (let ((type (node-property node :type)))
                   (when (if (eq timestamp-type :active)
                             (member type '(:active :active-range))
                             (member type '(:inactive :inactive-range)))
                     (return-from search-node node))))
               (dolist (child (node-children node))
                 (let ((found (search-node child)))
                   (when found (return-from search-node found)))))))
    (search-node headline)))

(defun query-logical-heading-level (document headline)
  (when (and (document-p document)
             (node-p headline)
             (eq (node-type headline) :headline))
    (let* ((text (document-text document))
           (start (node-begin headline)))
      (when (and (stringp text)
                 (integerp start)
                 (<= 0 start)
                 (< start (length text))
                 (char= (char text start) #\*))
        (let ((position start)
              (star-count 0)
              (limits (document-limits document)))
          (loop while (and (< position (length text))
                           (char= (char text position) #\*))
                do (incf star-count)
                   (check-limit limits :max-heading-depth star-count)
                   (incf position))
          (when (plusp star-count)
            (logical-heading-level document star-count)))))))

(defun query-special-property-value (document headline name context state)
  (cond
    ((string= name "LEVEL")
     (let ((level (query-logical-heading-level document headline)))
       (values level (not (null level)) t)))
    ((string= name "TODO")
     (let ((keyword (query-todo-keyword headline)))
       (values (or keyword "") (not (null keyword)) t)))
    ((string= name "PRIORITY")
     (let ((priority (node-property headline :priority)))
       (values (cond ((characterp priority) (string priority))
                     ((and (integerp priority) (<= 0 priority #x10ffff))
                      (let ((character (code-char priority)))
                        (and character (string character))))
                     ((stringp priority) priority)
                     (t nil))
               (not (null priority)) t)))
    ((member name '("SCHEDULED" "DEADLINE" "CLOSED") :test #'string=)
     (multiple-value-bind (value present)
         (query-planning-value headline
                               (cond ((string= name "SCHEDULED") :scheduled)
                                     ((string= name "DEADLINE") :deadline)
                                     (t :closed)))
       (values value present t)))
    ((member name '("TAGS" "ALLTAGS") :test #'string=)
     (let ((tags (if (string= name "TAGS")
                     (node-property headline :tags)
                     (query-effective-tags headline state))))
       (values (query-tags-string tags) t t)))
    ((string= name "FILE")
     (let ((file (or (query-context-file context)
                     (document-source-name document))))
       (values file (not (null file)) t)))
    ((string= name "CATEGORY")
     (let ((category (query-category-value document headline context)))
       (values category (not (null category)) t)))
    ((string= name "ITEM")
     (let ((item (node-property headline :raw-value)))
       (values item (not (null item)) t)))
    ((string= name "BLOCKED")
     (let ((provider (query-context-blocked-provider context)))
       (if provider
           (let ((value (funcall provider headline)))
             (values value (not (null value)) t))
           (values nil nil t))))
    ((string= name "CLOCKSUM")
     (let ((provider (query-context-clock-sum-provider context)))
       (if provider
           (let ((value (funcall provider headline)))
             (values value (not (null value)) t))
           (values nil nil t))))
    ((string= name "CLOCKSUM_T")
     (let ((provider (query-context-clock-sum-today-provider context)))
       (if provider
           (let ((value (funcall provider headline)))
             (values value (not (null value)) t))
           (values nil nil t))))
    ((member name '("TIMESTAMP" "TIMESTAMP_IA") :test #'string=)
     (let* ((timestamp
              (query-entry-timestamp headline
                                     (if (string= name "TIMESTAMP")
                                         :active :inactive)))
            (value (and timestamp (timestamp-start timestamp))))
       (values value (not (null value)) t)))
    (t (values nil nil nil))))

(defun query-resolve-property (document headline name state)
  (let ((context (query-evaluation-state-context state)))
    (multiple-value-bind (value present special-p)
        (query-special-property-value document headline name context state)
      (if special-p
          (values value present)
          (let ((headings (if (query-context-property-inheritance-p context)
                              (query-heading-chain headline)
                              (list headline))))
            (dolist (current headings)
              (multiple-value-bind (local local-p)
                  (query-local-property current name)
                (when local-p
                  (return-from query-resolve-property (values local t)))))
            (values nil nil))))))

(defun query-planning-present-p (headline kind)
  (nth-value 1 (query-planning-value headline kind)))

(defun query-evaluate-property (predicate headline state)
  (let* ((arguments (cdr predicate))
         (name (getf arguments :name))
         (operator (getf arguments :operator))
         (expected (getf arguments :value))
         (missing-as (getf arguments :missing-as))
         (require-present-p (getf arguments :require-present-p)))
    (multiple-value-bind (actual present)
        (query-resolve-property (query-evaluation-state-document state)
                                headline name state)
      (cond
        ((not present)
         (cond
           (require-present-p nil)
           ((eq missing-as :zero)
            (query-compare-values operator expected 0
                                  (query-evaluation-state-reference-time state)))
           (t nil)))
        ((member operator '(:matches :not-matches))
         (let ((matched (query-regex-match-p expected actual state)))
           (if (eq operator :matches) matched (not matched))))
        (t
         (query-compare-values operator expected actual
                               (query-evaluation-state-reference-time state)))))))

(defun query-evaluate-planning (predicate headline state)
  (let* ((arguments (cdr predicate))
         (kind (first arguments))
         (operator (second arguments))
         (expected (third arguments)))
    (multiple-value-bind (actual present) (query-planning-value headline kind)
      (and present actual
           (query-compare-values operator expected actual
                                 (query-evaluation-state-reference-time state))))))

(defun query-evaluate-existence (predicate headline state)
  (let ((arguments (cdr predicate)))
    (let* ((kind (first arguments))
           (name (second arguments))
           (present
             (cond
               ((eq kind :property)
                (nth-value 1 (query-resolve-property
                             (query-evaluation-state-document state)
                             headline name state)))
               (t
                (query-planning-present-p headline name)))))
      (if (eq (car predicate) :exists) present (not present)))))

(defun query-evaluate-tag (predicate headline state)
  (let* ((arguments (cdr predicate))
         (operator (first arguments))
         (expected (second arguments))
         (tags (query-effective-tags headline state)))
    (query-validate-tag-groups state)
    (case operator
      ((:=)
       (some (lambda (tag)
               (query-tag-name-matches-p
                tag expected (query-merged-tag-groups state) state))
             tags))
      ((:!=)
       (not (some (lambda (tag)
                    (query-tag-name-matches-p
                     tag expected (query-merged-tag-groups state) state))
                  tags)))
      ((:matches)
       (some (lambda (tag) (query-regex-match-p expected tag state)) tags))
      ((:not-matches)
       (not (some (lambda (tag) (query-regex-match-p expected tag state)) tags))))))

(defun query-evaluate-todo (predicate headline state)
  (let* ((arguments (cdr predicate))
         (operator (first arguments))
         (expected (second arguments))
         (keyword (or (query-todo-keyword headline) "")))
    (case operator
      ((:=) (string= keyword expected))
      ((:!=) (not (string= keyword expected)))
      ((:matches) (query-regex-match-p expected keyword state))
      ((:not-matches) (not (query-regex-match-p expected keyword state))))))

(defun query-evaluate-level (predicate headline state)
  (let* ((arguments (cdr predicate))
         (operator (first arguments))
         (expected (second arguments))
         (actual (query-logical-heading-level
                  (query-evaluation-state-document state)
                  headline)))
    (unless (and (member operator '(:= :!= :< :<= :> :>=))
                 (integerp expected))
      (query-signal-error nil nil "Malformed LEVEL predicate in canonical query"))
    (and (integerp actual)
         (query-compare-values operator expected actual
                               (query-evaluation-state-reference-time state)))))

(defun query-evaluate-predicate (predicate candidate state)
  (let* ((depth (query-check-predicate state))
         (operator (car predicate))
         (headline (query-nearest-headline candidate)))
    (let ((state (copy-query-evaluation-state state)))
      (setf (query-evaluation-state-depth state) depth)
      (cond
        ((eq operator :and)
         (every (lambda (child) (query-evaluate-predicate child candidate state))
                (cdr predicate)))
        ((eq operator :or)
         (some (lambda (child) (query-evaluate-predicate child candidate state))
               (cdr predicate)))
        ((eq operator :not)
         (not (query-evaluate-predicate (second predicate) candidate state)))
        ((eq operator :type)
         (eq (node-type candidate) (second predicate)))
        ((eq operator :path)
         (query-path-matches-p (second predicate) (node-heading-path candidate)
                               (query-evaluation-state-limits state)))
        ((eq operator :todo-present)
         (and headline
              (query-todo-present-p headline
                                    (query-evaluation-state-document state))))
        ((eq operator :todo-missing)
         (and headline
              (null (query-todo-keyword headline))))
        ((eq operator :level)
         (and headline (query-evaluate-level predicate headline state)))
        ((eq operator :todo)
         (and headline (query-evaluate-todo predicate headline state)))
        ((eq operator :tag)
         (and headline (query-evaluate-tag predicate headline state)))
        ((eq operator :property)
         (and headline (query-evaluate-property predicate headline state)))
        ((eq operator :planning)
         (and headline (query-evaluate-planning predicate headline state)))
        (t
         (and headline (query-evaluate-existence predicate headline state)))))))

(defun query-evaluate-ir (document query-ir &key reference-time)
  "Evaluate a parser-produced QUERY-IR against DOCUMENT and return matching
nodes in document order. REFERENCE-TIME is used for typed relative-date values;
this function never reads the current time."
  (check-type document document)
  (check-type query-ir query-ir)
  (let* ((root (document-root document))
         (context (or (query-ir-context query-ir) (make-query-context)))
         (limits (document-limits document))
         (state (%make-query-evaluation-state
                 :document document
                 :context context
                 :reference-time reference-time
                 :limits limits
                 :regex-cache (make-hash-table :test #'equal)
                 :depth 0))
         (scope (query-ir-scope query-ir))
         (predicate (query-ir-predicate query-ir))
         (results '()))
    (query-validate-predicate-tree predicate limits)
    (when root
      (map-nodes
       (lambda (candidate)
         (when (and (not (eq (node-type candidate) :org-data))
                    (or (eq scope :nodes)
                        (eq (node-type candidate) :headline))
                    (query-evaluate-predicate predicate candidate state))
           (push candidate results)))
       root))
    (nreverse results)))

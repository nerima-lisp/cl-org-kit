;;;; src/query-path.lisp
(in-package #:org-kit)

(defstruct (query-path-parser (:constructor %make-query-path-parser))
  source
  (position 0)
  (end 0)
  context
  limits
  reference-time
  (node-count 0))

(defun query-path-control-character-p (character)
  (let ((code (char-code character)))
    (or (< code #x20) (<= #x7f code #x9f))))

(defun query-path-ascii-letter-p (character)
  (and character
       (or (<= (char-code #\A) (char-code character) (char-code #\Z))
           (<= (char-code #\a) (char-code character) (char-code #\z)))))

(defun query-path-ascii-digit-p (character)
  (and character
       (<= (char-code #\0) (char-code character) (char-code #\9))))

(defun query-path-source-has-p (source position text &optional (end (length source)))
  (and (<= (+ position (length text)) end)
       (string= source text :start1 position :end1 (+ position (length text))
                            :start2 0 :end2 (length text))))

(defun query-path-parser-at-end-p (parser)
  (>= (query-path-parser-position parser) (query-path-parser-end parser)))

(defun query-path-parser-peek (parser &optional (offset 0))
  (let ((position (+ (query-path-parser-position parser) offset)))
    (when (< position (query-path-parser-end parser))
      (char (query-path-parser-source parser) position))))

(defun query-path-parser-fail (parser position message)
  (query-signal-error (query-path-parser-source parser) position message))

(defun query-path-parser-expect (parser character message)
  (unless (eql (query-path-parser-peek parser) character)
    (query-path-parser-fail parser (query-path-parser-position parser) message))
  (incf (query-path-parser-position parser))
  character)

(defun query-path-parser-consume (parser text)
  (when (query-path-source-has-p (query-path-parser-source parser)
                                 (query-path-parser-position parser)
                                 text (query-path-parser-end parser))
    (incf (query-path-parser-position parser) (length text))
    t))

(defun query-path-parser-note-node (parser)
  (incf (query-path-parser-node-count parser))
  (check-limit (query-path-parser-limits parser) :max-node-count
               (query-path-parser-node-count parser)))

(defun query-path-parser-quoted-string (parser)
  (let ((start (query-path-parser-position parser))
        (characters '())
        (source-positions '()))
    (query-path-parser-expect parser #\" "Expected a quoted string")
    (let ((content-start (query-path-parser-position parser)))
      (loop for position = (query-path-parser-position parser)
            for character = (query-path-parser-peek parser)
            until (char= character #\")
            do (cond
                 ((char= character #\\)
                  (incf (query-path-parser-position parser))
                  (let ((escaped (query-path-parser-peek parser)))
                    (case escaped
                      (#\\ (push #\\ characters))
                      (#\" (push #\" characters))
                      (#\n (push #\Newline characters))
                      (#\r (push #\Return characters))
                      (#\t (push #\Tab characters))
                      (otherwise
                       (query-path-parser-fail
                        parser position "Unsupported quoted-string escape")))
                    (push position source-positions)
                    (incf (query-path-parser-position parser))))
                 ((query-path-control-character-p character)
                  (query-path-parser-fail parser position
                                          "Control character in quoted string"))
                 (t
                  (push character characters)
                  (push position source-positions)
                  (incf (query-path-parser-position parser)))))
      (let ((closing-quote (query-path-parser-position parser)))
        (incf (query-path-parser-position parser))
        (values (coerce (nreverse characters) 'string)
                (coerce (nreverse source-positions) 'vector)
                start content-start closing-quote)))))

(defun query-path-parser-date-literal (parser)
  (query-path-parser-consume parser "date(")
  (multiple-value-bind (text map quote-start content-start closing-quote)
      (query-path-parser-quoted-string parser)
    (declare (ignore map content-start closing-quote))
    (query-path-parser-expect parser #\) "Expected ')' after date literal")
    (query-parse-org-date-literal
     text (query-path-parser-reference-time parser)
     (query-path-parser-source parser) quote-start)))

(defun query-path-parser-regex-literal (parser)
  (query-path-parser-consume parser "regex(")
  (multiple-value-bind (pattern source-map quote-start content-start closing-quote)
      (query-path-parser-quoted-string parser)
    (declare (ignore quote-start))
    (query-path-parser-expect parser #\) "Expected ')' after regex literal")
    (handler-case
        (query-compile-emacs-regex
         pattern (query-path-parser-context parser)
         (query-path-parser-limits parser)
         (query-path-parser-source parser) content-start)
      (org-query-error (condition)
        (let* ((condition-position (org-query-error-position condition))
               (index (and condition-position
                           (- condition-position content-start)))
               (mapped-position
                 (if (and index (< index (length source-map)))
                     (aref source-map index)
                     closing-quote)))
          (query-path-parser-fail
           parser mapped-position (org-kit-error-message condition)))))))

(defun query-path-parser-integer (parser)
  (let ((start (query-path-parser-position parser)))
    (when (eql (query-path-parser-peek parser) #\-)
      (incf (query-path-parser-position parser)))
    (let ((digit-start (query-path-parser-position parser)))
      (loop while (query-path-ascii-digit-p (query-path-parser-peek parser))
            do (incf (query-path-parser-position parser)))
      (when (= digit-start (query-path-parser-position parser))
        (query-path-parser-fail parser start "Expected an integer"))
      (parse-integer (query-path-parser-source parser)
                     :start start
                     :end (query-path-parser-position parser)))))

(defun query-path-parser-identifier (parser)
  (let ((start (query-path-parser-position parser)))
    (unless (query-path-ascii-letter-p (query-path-parser-peek parser))
      (query-path-parser-fail parser start "Expected a property name"))
    (incf (query-path-parser-position parser))
    (loop while (let ((character (query-path-parser-peek parser)))
                  (or (query-path-ascii-letter-p character)
                      (query-path-ascii-digit-p character)
                      (eql character #\_)))
          do (incf (query-path-parser-position parser)))
    (subseq (query-path-parser-source parser)
            start (query-path-parser-position parser))))

(defun query-path-parser-property-name (parser)
  (let ((name
          (if (eql (query-path-parser-peek parser) #\")
              (query-path-parser-quoted-string parser)
              (query-path-parser-identifier parser))))
    (when (zerop (length name))
      (query-path-parser-fail parser (query-path-parser-position parser)
                              "Property name cannot be empty"))
    name))

(defun query-path-parser-operator (parser operators)
  (loop for (text operator) in operators
        when (query-path-parser-consume parser text)
          return operator
        finally
           (query-path-parser-fail parser (query-path-parser-position parser)
                                   "Expected a comparison operator")))

(defun query-path-parser-equality (parser)
  (query-path-parser-operator parser '(("!=" :!=) ("=" :=))))

(defun query-path-parser-comparator (parser)
  (query-path-parser-operator
   parser '(("<=" :<=) (">=" :>=) ("!=" :!=) ("=" :=) ("<" :<) (">" :>))))

(defun query-path-parser-value (parser)
  (cond
    ((eql (query-path-parser-peek parser) #\")
     (values (query-path-parser-quoted-string parser) :string))
    ((query-path-source-has-p (query-path-parser-source parser)
                              (query-path-parser-position parser) "date("
                              (query-path-parser-end parser))
     (values (query-path-parser-date-literal parser) :date))
    ((query-path-source-has-p (query-path-parser-source parser)
                              (query-path-parser-position parser) "regex("
                              (query-path-parser-end parser))
     (values (query-path-parser-regex-literal parser) :regex))
    (t
     (values (query-path-parser-integer parser) :integer))))

(defun query-path-parser-type (parser)
  (let ((start (query-path-parser-position parser)))
    (unless (eql (query-path-parser-peek parser) #\:)
      (query-path-parser-fail parser start "Expected a colon-prefixed node type"))
    (incf (query-path-parser-position parser))
    (let ((name-start (query-path-parser-position parser)))
      (loop for character = (query-path-parser-peek parser)
            while (and character
                       (not (find character '(#\& #\| #\) #\}))))
            do (incf (query-path-parser-position parser)))
      (let* ((name (subseq (query-path-parser-source parser)
                           name-start (query-path-parser-position parser)))
             (node-type
               (find-if
                (lambda (candidate)
                  (string= name
                           (string-downcase (symbol-name candidate))))
                (node-types))))
        (or node-type
            (query-path-parser-fail parser start "Unknown node type"))))))

(defun query-path-parser-planning-kind (parser)
  (let ((start (query-path-parser-position parser)))
    (loop for character = (query-path-parser-peek parser)
          while (query-path-ascii-letter-p character)
          do (incf (query-path-parser-position parser)))
    (let ((name (subseq (query-path-parser-source parser)
                        start (query-path-parser-position parser))))
      (cond ((string= name "SCHEDULED") :scheduled)
            ((string= name "DEADLINE") :deadline)
            ((string= name "CLOSED") :closed)
            (t (query-path-parser-fail parser start
                                       "Unknown planning field"))))))

(defun query-path-parser-quoted-or-regex (parser operator kind)
  (let ((value-kind
          (if (query-path-source-has-p (query-path-parser-source parser)
                                       (query-path-parser-position parser)
                                       "regex("
                                       (query-path-parser-end parser))
              :regex
              :string)))
    (unless (or (eq value-kind :regex)
                (eql (query-path-parser-peek parser) #\"))
      (query-path-parser-fail parser (query-path-parser-position parser)
                              "Expected a quoted string or regex literal"))
    (let ((value (if (eq value-kind :regex)
                     (query-path-parser-regex-literal parser)
                     (query-path-parser-quoted-string parser))))
      (values (list kind
                    (if (eq value-kind :regex)
                        (if (eq operator :!=) :not-matches :matches)
                        operator)
                    value)
              value-kind))))

(defun query-path-parser-property-predicate (parser)
  (query-path-parser-consume parser "property.")
  (let* ((name (query-path-parser-property-name parser))
         (operator (query-path-parser-comparator parser)))
    (multiple-value-bind (value value-kind)
        (query-path-parser-value parser)
      (if (eq value-kind :regex)
          (progn
            (unless (member operator '(:= :!=) :test #'eq)
              (query-path-parser-fail
               parser (query-path-parser-position parser)
               "Regex property comparisons require '=' or '!='"))
            (list :property
                  :name name
                  :operator (if (eq operator :!=) :not-matches :matches)
                  :value value
                  :missing-as :nonmatch))
          (let ((missing-as (if (eq value-kind :integer) :zero :nonmatch)))
            (when (query-path-parser-consume parser "@missing=")
              (setf missing-as
                    (cond
                      ((query-path-parser-consume parser "zero") :zero)
                      ((query-path-parser-consume parser "nonmatch") :nonmatch)
                      (t
                       (query-path-parser-fail
                        parser (query-path-parser-position parser)
                        "Unknown property missing mode"))))
              (when (and (eq missing-as :zero) (not (eq value-kind :integer)))
                (query-path-parser-fail
                 parser (query-path-parser-position parser)
                 "'@missing=zero' requires an integer value")))
            (list :property
                  :name name
                  :operator operator
                  :value value
                  :missing-as missing-as))))))

(defun query-path-parser-predicate (parser)
  (let ((source (query-path-parser-source parser)))
    (cond
      ((query-path-parser-consume parser "todo-present")
       (query-path-parser-note-node parser)
       '(:todo-present))
      ((query-path-parser-consume parser "todo-missing")
       (query-path-parser-note-node parser)
       '(:todo-missing))
      ((query-path-parser-consume parser "todo")
       (let ((operator (query-path-parser-equality parser)))
         (multiple-value-bind (predicate kind)
             (query-path-parser-quoted-or-regex parser operator :todo)
           (declare (ignore kind))
           (query-path-parser-note-node parser)
           predicate)))
      ((query-path-parser-consume parser "tag")
       (let ((operator (query-path-parser-equality parser)))
         (multiple-value-bind (predicate kind)
             (query-path-parser-quoted-or-regex parser operator :tag)
           (declare (ignore kind))
           (query-path-parser-note-node parser)
           predicate)))
      ((query-path-parser-consume parser "type")
       (let* ((operator (query-path-parser-equality parser))
              (node-type (query-path-parser-type parser))
              (predicate (list :type node-type)))
         (query-path-parser-note-node parser)
         (if (eq operator :=)
             predicate
             (progn
               (query-path-parser-note-node parser)
               (list :not predicate)))))
      ((query-path-parser-consume parser "level")
       (let ((operator (query-path-parser-comparator parser))
             (value (query-path-parser-integer parser)))
         (query-path-parser-note-node parser)
         (list :level operator value)))
      ((query-path-source-has-p source (query-path-parser-position parser)
                                "property."
                                (query-path-parser-end parser))
       (let ((predicate (query-path-parser-property-predicate parser)))
         (query-path-parser-note-node parser)
         predicate))
      ((query-path-parser-consume parser "exists.property.")
       (let ((name (query-path-parser-property-name parser)))
         (query-path-parser-note-node parser)
         (list :exists :property name)))
      ((query-path-parser-consume parser "missing.property.")
       (let ((name (query-path-parser-property-name parser)))
         (query-path-parser-note-node parser)
         (list :missing :property name)))
      ((query-path-parser-consume parser "planning.")
       (let* ((kind (query-path-parser-planning-kind parser))
              (operator (query-path-parser-comparator parser))
              (value-start (query-path-parser-position parser)))
         (unless (query-path-source-has-p source value-start "date("
                                          (query-path-parser-end parser))
           (query-path-parser-fail parser value-start
                                   "Planning comparisons require a date literal"))
         (let ((value (query-path-parser-date-literal parser)))
           (query-path-parser-note-node parser)
           (list :planning kind operator value))))
      ((query-path-parser-consume parser "exists.planning.")
       (let ((kind (query-path-parser-planning-kind parser)))
         (query-path-parser-note-node parser)
         (list :exists :planning kind)))
      ((query-path-parser-consume parser "missing.planning.")
       (let ((kind (query-path-parser-planning-kind parser)))
         (query-path-parser-note-node parser)
         (list :missing :planning kind)))
      (t
       (query-path-parser-fail parser (query-path-parser-position parser)
                               "Unknown filter predicate")))))

(declaim (ftype function query-path-parser-filter-or))

(defun query-path-parser-filter-atom (parser depth)
  (check-limit (query-path-parser-limits parser) :max-element-depth depth)
  (if (eql (query-path-parser-peek parser) #\()
      (progn
        (incf (query-path-parser-position parser))
        (let ((predicate (query-path-parser-filter-or parser (1+ depth))))
          (query-path-parser-expect parser #\) "Expected ')' after filter group")
          predicate))
      (query-path-parser-predicate parser)))

(defun query-path-parser-filter-unary (parser depth)
  (check-limit (query-path-parser-limits parser) :max-element-depth depth)
  (if (eql (query-path-parser-peek parser) #\!)
      (let ((start (query-path-parser-position parser)))
        (incf (query-path-parser-position parser))
        (when (eql (query-path-parser-peek parser) #\!)
          (query-path-parser-fail parser start
                                  "Negation must precede one predicate or group"))
        (let ((predicate (query-path-parser-filter-atom parser (1+ depth))))
          (query-path-parser-note-node parser)
          (list :not predicate)))
      (query-path-parser-filter-atom parser depth)))

(defun query-path-parser-filter-and (parser depth)
  (let ((predicates (list (query-path-parser-filter-unary parser depth))))
    (loop while (eql (query-path-parser-peek parser) #\&)
          do (incf (query-path-parser-position parser))
             (push (query-path-parser-filter-unary parser depth) predicates))
    (setf predicates (nreverse predicates))
    (if (cdr predicates)
        (progn
          (query-path-parser-note-node parser)
          (cons :and predicates))
        (first predicates))))

(defun query-path-parser-filter-or (parser depth)
  (let ((predicates (list (query-path-parser-filter-and parser depth))))
    (loop while (eql (query-path-parser-peek parser) #\|)
          do (incf (query-path-parser-position parser))
             (push (query-path-parser-filter-and parser depth) predicates))
    (setf predicates (nreverse predicates))
    (if (cdr predicates)
        (progn
          (query-path-parser-note-node parser)
          (cons :or predicates))
        (first predicates))))

(defun query-path-filter-start (source)
  (loop with position = 0
        while (< position (length source))
        for character = (char source position)
        do (cond
             ((char= character #\\)
              (incf position 2))
             ((char= character #\?)
              (if (and (< (1+ position) (length source))
                       (char= (char source (1+ position)) #\{))
                  (return position)
                  (query-signal-error source position
                                      "Unescaped '?' in path selector")))
             ((find character '(#\{ #\}))
              (query-signal-error source position
                                  "Unescaped brace in path selector"))
             (t
              (incf position)))
        finally (return nil)))

(defun query-path-filter-end (source start)
  (let ((position (+ start 2))
        (in-string-p nil)
        (escaped-p nil))
    (loop while (< position (length source))
          for character = (char source position)
          do (cond
               (in-string-p
                (cond
                  (escaped-p (setf escaped-p nil))
                  ((char= character #\\) (setf escaped-p t))
                  ((char= character #\") (setf in-string-p nil))))
               ((char= character #\") (setf in-string-p t))
               ((char= character #\})
                (unless (= position (1- (length source)))
                  (query-signal-error source (1+ position)
                                      "Unexpected text after filter"))
                (return position))
               ((char= character #\{)
                (query-signal-error source position
                                    "Unexpected brace in filter")))
             (incf position)
          finally
             (query-signal-error source (length source)
                                 "Unclosed filter clause"))))

(defun query-path-parse-segment (source units limits)
  (let* ((units (nreverse units))
         (length (length units))
         (open-indices '())
         (close-indices '())
         (star-indices '())
         (index nil)
         (title-end length))
    (loop for unit in units
          for position from 0
          for character = (first unit)
          for escaped-p = (second unit)
          do (unless escaped-p
               (cond
                 ((char= character #\[) (push position open-indices))
                 ((char= character #\]) (push position close-indices))
                 ((char= character #\*) (push position star-indices)))))
    (when (or open-indices close-indices)
      (unless (and (= (length open-indices) 1)
                   (= (length close-indices) 1)
                   (= (first close-indices) (1- length)))
        (query-signal-error
         source (third (nth (or (first open-indices) (first close-indices)) units))
         "Malformed path index"))
      (let* ((open (first open-indices))
             (close (first close-indices))
             (digits (subseq units (1+ open) close)))
        (unless (and digits
                     (not (char= (first (first digits)) #\0))
                     (every (lambda (unit)
                              (query-path-ascii-digit-p (first unit)))
                            digits))
          (query-signal-error source (third (nth open units))
                              "Path index must be a positive integer"))
        (setf index
              (parse-integer
               (coerce (mapcar #'first digits) 'string)
               :junk-allowed nil)
              title-end open)))
    (when star-indices
      (if (and (null index)
               (member length '(1 2))
               (every (lambda (unit)
                        (and (not (second unit))
                             (char= (first unit) #\*)))
                      units))
          (return-from query-path-parse-segment
            (if (= length 1) :* :**))
          (query-signal-error source
                              (third (nth (first star-indices) units))
                              "Wildcard must occupy a complete path segment")))
    (when (zerop title-end)
      (query-signal-error source (third (first units))
                          "Path title cannot be empty"))
    (let ((title (coerce (mapcar #'first (subseq units 0 title-end)) 'string)))
      (check-limit limits :max-input-length (length title))
      (if index
          (list :name title index)
          title))))

(defun query-path-parse-selector (source end limits)
  (let ((position 0)
        (segments '())
        (segment-units '())
        (segment-start 0)
        (count 0))
    (when (and (< position end) (char= (char source position) #\/))
      (incf position)
      (setf segment-start position))
    (when (= position end)
      (query-signal-error source position "Path selector cannot be empty"))
    (labels ((finish-segment ()
               (unless segment-units
                 (query-signal-error source segment-start
                                     "Path selector contains an empty segment"))
               (push (query-path-parse-segment source segment-units limits)
                     segments)
               (setf segment-units nil)
               (incf count)
               (check-limit limits :max-element-depth count)
               (check-limit limits :max-node-count count)))
      (loop while (< position end)
            for character = (char source position)
            do (cond
                 ((char= character #\/)
                  (finish-segment)
                  (incf position)
                  (setf segment-start position)
                  (when (= position end)
                    (query-signal-error source position
                                        "Path selector cannot end with '/'")))
                 ((char= character #\\)
                  (let ((escape-position position))
                    (incf position)
                    (when (= position end)
                      (query-signal-error source escape-position
                                          "Incomplete path escape"))
                    (let ((escaped (char source position)))
                      (unless (find escaped '(#\/ #\* #\[ #\] #\\ #\? #\{ #\}))
                        (query-signal-error source escape-position
                                            "Unsupported path escape"))
                      (push (list escaped t escape-position) segment-units)
                      (incf position))))
                 ((query-path-control-character-p character)
                  (query-signal-error source position
                                      "Control character in path selector"))
                 (t
                  (push (list character nil position) segment-units)
                  (incf position))))
      (finish-segment))
    (nreverse segments)))

(defun query-path-parse-source (source context limits reference-time)
  (let* ((filter-start (query-path-filter-start source))
         (path-end (or filter-start (length source)))
         (filter-end (and filter-start
                          (query-path-filter-end source filter-start)))
         (selector (query-path-parse-selector source path-end limits))
         (node-count (length selector))
         (filter
           (when filter-start
             (let ((parser
                     (%make-query-path-parser
                      :source source
                      :position (+ filter-start 2)
                      :end filter-end
                      :context context
                      :limits limits
                      :reference-time reference-time
                      :node-count node-count)))
               (when (= (query-path-parser-position parser) filter-end)
                 (query-path-parser-fail parser filter-end
                                         "Filter expression cannot be empty"))
               (let ((predicate (query-path-parser-filter-or parser 1)))
                 (unless (query-path-parser-at-end-p parser)
                   (query-path-parser-fail
                    parser (query-path-parser-position parser)
                    "Unexpected character in filter"))
                 predicate)))))
    (values selector filter)))

(defun query-path-check-predicate-depth (predicate limits)
  (let ((pending (list (list predicate 1))))
    (loop for (node depth) = (pop pending)
          while node
          do (check-limit limits :max-element-depth depth)
             (case (first node)
               ((:and :or)
                (dolist (child (rest node))
                  (push (list child (1+ depth)) pending)))
               (:not
                (push (list (second node) (1+ depth)) pending))))
    t))

(defun query-path (document path-filter
                   &key (scope :headlines) context reference-time)
  "Evaluate PATH-FILTER against DOCUMENT and return matching nodes.
PATH-FILTER is a root-based headline selector with an optional boolean filter.
SCOPE is :HEADLINES or :NODES; CONTEXT configures shared query semantics.
REFERENCE-TIME is required when the filter contains a relative date literal."
  (check-type document document)
  (check-type path-filter string)
  (check-type reference-time (or null cl-date-kit:local-date-time))
  (unless (member scope '(:headlines :nodes) :test #'eq)
    (error 'type-error :datum scope :expected-type '(member :headlines :nodes)))
  (when (and context (not (query-context-p context)))
    (error 'type-error :datum context :expected-type 'query-context))
  (let* ((context (or context (make-query-context)))
         (limits (document-limits document)))
    (check-limit limits :max-input-length (length path-filter))
    (multiple-value-bind (selector filter)
        (query-path-parse-source path-filter context limits reference-time)
      (let* ((path-predicate (list :path selector))
             (predicate (if filter (list :and path-predicate filter)
                            path-predicate))
             (query-ir (%make-query-ir scope predicate context)))
        (query-path-check-predicate-depth predicate limits)
        (query-evaluate-ir document query-ir :reference-time reference-time)))))

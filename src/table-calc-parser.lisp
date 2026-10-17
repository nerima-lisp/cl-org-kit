;;;; src/table-calc-parser.lisp
(in-package #:org-kit)

(defstruct (table-calc-token (:constructor %make-table-calc-token))
  kind start end value)

(defstruct (table-calc-reference (:constructor %make-table-calc-reference))
  row-kind row-value row-offset column-kind column-value
  row-start row-end column-start column-end start end)

(defstruct (table-calc-expression (:constructor %make-table-calc-expression))
  kind start end value children depth)

(defstruct (table-calc-assignment (:constructor %make-table-calc-assignment))
  kind target target-end expression mode flags format-precision start end)

(defstruct (table-calc-program (:constructor %make-table-calc-program))
  source assignments)

(defstruct (table-calc-parser (:constructor %make-table-calc-parser))
  formula tokens cursor limits nesting functions symbol-context)

(defstruct (safe-decimal (:constructor %make-safe-decimal))
  coefficient exponent negative-zero-p float-p)

(defun table-calc-fail (formula position feature &optional (message "Unsupported table formula syntax"))
  (error 'org-table-unsupported-syntax
         :message message
         :formula (bounded-text formula)
         :position (max 0 (min position (length formula)))
         :feature feature))

(defun table-calc-digit-p (character)
  (let ((code (char-code character)))
    (<= 48 code 57)))

(defun table-calc-alpha-p (character)
  (let ((code (char-code character)))
    (or (<= 65 code 90) (<= 97 code 122))))

(defun table-calc-name-start-p (character)
  (or (table-calc-alpha-p character) (char= character #\_)))

(defun table-calc-name-character-p (character)
  (or (table-calc-name-start-p character)
      (table-calc-digit-p character)))

(defun table-calc-space-p (character)
  (find character '(#\Space #\Tab #\Return #\Newline)))

(defun table-calc-tokenize-number (formula start)
  (let* ((length (length formula))
         (position start))
    (loop while (and (< position length)
                     (table-calc-digit-p (char formula position)))
          do (incf position))
    (when (and (< position length)
               (char= (char formula position) #\.)
               (not (and (< (1+ position) length)
                         (char= (char formula (1+ position)) #\.))))
      (incf position)
      (unless (and (< position length)
                   (table-calc-digit-p (char formula position)))
        (table-calc-fail formula position :invalid-number))
      (loop while (and (< position length)
                       (table-calc-digit-p (char formula position)))
            do (incf position)))
    (when (and (< position length)
               (find (char formula position) "eE"))
      (incf position)
      (when (and (< position length)
                 (find (char formula position) "+-"))
        (incf position))
      (unless (and (< position length)
                   (table-calc-digit-p (char formula position)))
        (table-calc-fail formula position :invalid-number))
      (loop while (and (< position length)
                       (table-calc-digit-p (char formula position)))
            do (incf position)))
    (values position (%make-table-calc-token :kind :number
                                             :start start :end position))))

(defun table-calc-tokenize-identifier (formula start)
  (let ((position start)
        (length (length formula)))
    (loop while (and (< position length)
                     (table-calc-name-character-p (char formula position)))
          do (incf position))
    (values position
            (%make-table-calc-token :kind :identifier :start start :end position
                                    :value (subseq formula start position)))))

(defun table-calc-tokenize-string (formula start)
  (let ((position (1+ start))
        (value (make-string-output-stream))
        (length (length formula)))
    (loop while (< position length)
          for character = (char formula position)
          do (cond
               ((char= character #\")
                (return-from table-calc-tokenize-string
                  (values (1+ position)
                          (%make-table-calc-token
                           :kind :string :start start :end (1+ position)
                           :value (get-output-stream-string value)))))
               ((char= character #\\)
                (incf position)
                (when (>= position length)
                  (table-calc-fail formula start :unterminated-string))
                (write-char (char formula position) value))
               (t (write-char character value)))
             (incf position))
    (table-calc-fail formula start :unterminated-string)))

(defun table-calc-tokenize-duration (formula start)
  (let ((position start)
        (length (length formula)))
    (loop while (and (< position length)
                     (table-calc-digit-p (char formula position)))
          do (incf position))
    (when (and (< position length) (char= (char formula position) #\:))
      (let ((first-colon position))
        (incf position)
        (let ((minute-start position))
          (loop while (and (< position length)
                           (table-calc-digit-p (char formula position)))
                do (incf position))
          (when (> position minute-start)
            (when (and (< position length) (char= (char formula position) #\:))
              (incf position)
              (let ((second-start position))
                (loop while (and (< position length)
                                 (table-calc-digit-p (char formula position)))
                      do (incf position))
                (when (= position second-start)
                  (setf position (1+ first-colon)))))
            (when (or (= position (1+ first-colon))
                      (not (and (< position length)
                                (table-calc-name-character-p
                                 (char formula position)))))
              (return-from table-calc-tokenize-duration
                (values position
                        (%make-table-calc-token
                         :kind :duration :start start :end position
                         :value (subseq formula start position))))))))
    (values nil nil))))

(defun table-calc-tokenize-suffix (formula start)
  (let ((position (1+ start))
        (length (length formula)))
    (loop while (and (< position length)
                     (not (and (< (1+ position) length)
                               (char= (char formula position) #\:)
                               (char= (char formula (1+ position)) #\:))))
          do (incf position))
    (values position
            (%make-table-calc-token :kind :suffix :start start :end position
                                    :value (subseq formula (1+ start) position)))))

(defun table-calc-tokenize (formula limits)
  (let ((length (length formula))
        (position 0)
        (count 0)
        (tokens '()))
    (labels ((emit (token next-position)
               (incf count)
               (push token tokens)
               (setf position next-position))
             (check-next-token ()
               (check-limit limits :max-formula-tokens (1+ count)))
             (single-token (kind)
               (check-next-token)
               (emit (%make-table-calc-token :kind kind :start position
                                             :end (1+ position))
                     (1+ position))))
      (loop while (< position length)
            do (let ((character (char formula position)))
                 (cond
                   ((table-calc-space-p character)
                    (incf position))
                   ((char= character #\#)
                    (table-calc-fail formula position
                                     (if (and (< (1+ position) length)
                                              (char= (char formula (1+ position)) #\.))
                                         :reader-eval
                                         :reader-dispatch)))
                   ((or (char= character #\') (= (char-code character) 96))
                    (table-calc-fail formula position :quoted-lisp))
                   ((table-calc-digit-p character)
                    (check-next-token)
                    (multiple-value-bind (duration-end duration-token)
                        (table-calc-tokenize-duration formula position)
                      (if duration-token
                          (emit duration-token duration-end)
                          (multiple-value-bind (end token)
                              (table-calc-tokenize-number formula position)
                            (emit token end)))))
                   ((char= character #\")
                    (check-next-token)
                    (multiple-value-bind (end token)
                        (table-calc-tokenize-string formula position)
                      (emit token end)))
                   ((table-calc-name-start-p character)
                    (check-next-token)
                    (multiple-value-bind (end token)
                        (table-calc-tokenize-identifier formula position)
                      (emit token end)))
                   ((and (char= character #\:)
                         (< (1+ position) length)
                         (char= (char formula (1+ position)) #\:))
                    (check-next-token)
                    (emit (%make-table-calc-token :kind :separator :start position
                                                  :end (+ position 2))
                          (+ position 2)))
                   ((char= character #\.)
                    (if (and (< (1+ position) length)
                             (char= (char formula (1+ position)) #\.))
                        (progn
                          (check-next-token)
                          (emit (%make-table-calc-token :kind :range :start position
                                                      :end (+ position 2))
                                (+ position 2)))
                        (table-calc-fail formula position :unsupported-character)))
                   ((char= character #\+)
                    (single-token :plus))
                   ((char= character #\-)
                    (single-token :minus))
                   ((char= character #\*)
                    (single-token :multiply))
                   ((char= character #\/)
                    (single-token :divide))
                   ((char= character #\@)
                    (single-token :at))
                   ((and (char= character #\<)
                         (> position 0)
                         (char= (char formula (1- position)) #\@))
                    (single-token :less))
                   ((char= character #\>)
                    (single-token :greater))
                   ((and (char= character #\|)
                         (< (1+ position) length)
                         (char= (char formula (1+ position)) #\|))
                    (check-next-token)
                    (emit (%make-table-calc-token :kind :logical-or :start position
                                                  :end (+ position 2))
                          (+ position 2)))
                   ((char= character #\<)
                    (single-token :less))
                   ((char= character #\$)
                    (single-token :dollar))
                   ((char= character #\=)
                    (if (and (< (1+ position) length)
                             (char= (char formula (1+ position)) #\=))
                        (progn
                          (check-next-token)
                          (emit (%make-table-calc-token :kind :equals :start position
                                                        :end (+ position 2))
                                (+ position 2)))
                        (single-token :equals)))
                   ((char= character #\,)
                    (single-token :comma))
                   ((char= character #\()
                    (single-token :left-paren))
                   ((char= character #\))
                    (single-token :right-paren))
                   ((char= character #\;)
                    (check-next-token)
                    (multiple-value-bind (end token)
                        (table-calc-tokenize-suffix formula position)
                      (emit token end)))
                   (t
                    (table-calc-fail formula position :unsupported-character)))))
      (coerce (nreverse tokens) 'simple-vector))))

(defun table-calc-token-text (parser token)
  (subseq (table-calc-parser-formula parser)
          (table-calc-token-start token)
          (table-calc-token-end token)))

(defun table-calc-current-token (parser)
  (let ((cursor (table-calc-parser-cursor parser))
        (tokens (table-calc-parser-tokens parser)))
    (when (< cursor (length tokens))
      (svref tokens cursor))))

(defun table-calc-current-kind (parser)
  (let ((token (table-calc-current-token parser)))
    (when token (table-calc-token-kind token))))

(defun table-calc-next-kind (parser)
  (let* ((cursor (1+ (table-calc-parser-cursor parser)))
         (tokens (table-calc-parser-tokens parser)))
    (when (< cursor (length tokens))
      (table-calc-token-kind (svref tokens cursor)))))

(defun table-calc-advance (parser)
  (prog1 (table-calc-current-token parser)
    (incf (table-calc-parser-cursor parser))))

(defun table-calc-position (parser)
  (let ((token (table-calc-current-token parser)))
    (if token (table-calc-token-start token)
        (length (table-calc-parser-formula parser)))))

(defun table-calc-expect (parser kind feature)
  (if (eql (table-calc-current-kind parser) kind)
      (table-calc-advance parser)
      (table-calc-fail (table-calc-parser-formula parser)
                       (table-calc-position parser) feature)))

(defun table-calc-token-digits (parser token)
  (let ((text (table-calc-token-text parser token)))
    (and (plusp (length text))
         (every #'table-calc-digit-p text)
         text)))

(defun table-calc-reference-integer (parser token)
  (let* ((digits (table-calc-token-digits parser token))
         (first-nonzero (and digits
                             (position-if-not (lambda (character)
                                                (char= character #\0))
                                              digits)))
         (normalized (if first-nonzero (subseq digits first-nonzero) "0"))
         (maximum (write-to-string most-positive-fixnum :base 10 :radix nil)))
    (unless digits
      (table-calc-fail (table-calc-parser-formula parser)
                       (table-calc-token-start token) :invalid-reference))
    (if (or (> (length normalized) (length maximum))
            (and (= (length normalized) (length maximum))
                 (string> normalized maximum)))
        :too-large
        (parse-integer normalized))))

(defun table-calc-roman-ordinal (text)
  (flet ((roman-value (character)
           (case character
             (#\I 1) (#\V 5) (#\X 10) (#\L 50)
             (#\C 100) (#\D 500) (#\M 1000)
             (otherwise 0))))
    (let ((total 0))
      (loop for index from 0 below (length text)
            for value = (roman-value (char text index))
            for next-value = (if (< (1+ index) (length text))
                                 (roman-value (char text (1+ index)))
                                 0)
            do (when (zerop value)
                 (return-from table-calc-roman-ordinal nil))
               (incf total (if (< value next-value) (- value) value)))
      (and (plusp total) total))))

(defun table-calc-parse-reference (parser)
  (let* ((formula (table-calc-parser-formula parser))
         (start-token (table-calc-current-token parser))
         (start (table-calc-token-start start-token))
         (row-kind :current)
         (row-value nil)
         (row-offset nil)
         (row-start nil)
         (row-end nil)
         (column-kind nil)
         (column-value nil)
         (column-start nil)
         (column-end nil))
    (labels ((parse-column ()
               (cond
                 ((eql (table-calc-current-kind parser) :number)
                  (let ((token (table-calc-advance parser)))
                    (setf column-kind :absolute
                          column-value (table-calc-reference-integer parser token)
                          column-start (table-calc-token-start token)
                          column-end (table-calc-token-end token))))
                 ((eql (table-calc-current-kind parser) :identifier)
                  (let ((token (table-calc-advance parser)))
                    (setf column-kind :name
                          column-value (table-calc-token-value token)
                          column-start (table-calc-token-start token)
                          column-end (table-calc-token-end token))))
                 (t
                  (table-calc-fail formula (table-calc-position parser)
                                   :invalid-reference))))
             (parse-hline-ordinal (relative-kind)
               (let* ((token (table-calc-expect parser :identifier
                                                :invalid-reference))
                      (ordinal (table-calc-roman-ordinal
                                (table-calc-token-value token))))
                 (unless ordinal
                   (table-calc-fail formula (table-calc-token-start token)
                                    :invalid-reference))
                 (setf row-kind relative-kind
                       row-value ordinal
                       row-start (table-calc-token-start token)
                       row-end (table-calc-token-end token)))))
      (if (eql (table-calc-current-kind parser) :at)
          (progn
            (table-calc-advance parser)
            (cond
              ((eql (table-calc-current-kind parser) :less)
               (let ((token (table-calc-advance parser)))
                 (setf row-kind :first
                       row-start (table-calc-token-start token)
                       row-end (table-calc-token-end token))))
              ((eql (table-calc-current-kind parser) :greater)
               (let ((token (table-calc-advance parser)))
                 (setf row-kind :last
                       row-start (table-calc-token-start token)
                       row-end (table-calc-token-end token))))
              ((eql (table-calc-current-kind parser) :plus)
               (table-calc-advance parser)
               (if (eql (table-calc-current-kind parser) :identifier)
                   (parse-hline-ordinal :hline-relative-below)
                   (let ((token (table-calc-expect parser :number
                                                   :invalid-reference)))
                     (setf row-kind :relative
                           row-value (table-calc-reference-integer parser token)
                           row-start (table-calc-token-start token)
                           row-end (table-calc-token-end token)))))
              ((eql (table-calc-current-kind parser) :minus)
               (table-calc-advance parser)
               (if (eql (table-calc-current-kind parser) :identifier)
                   (parse-hline-ordinal :hline-relative-above)
                   (let* ((token (table-calc-expect parser :number
                                                    :invalid-reference))
                          (value (table-calc-reference-integer parser token)))
                     (setf row-kind :relative
                           row-value (if (integerp value) (- value) value)
                           row-start (table-calc-token-start token)
                           row-end (table-calc-token-end token)))))
              ((eql (table-calc-current-kind parser) :number)
               (let* ((token (table-calc-advance parser))
                      (value (table-calc-reference-integer parser token)))
                 (setf row-kind (if (eql value 0) :current :absolute)
                       row-value (unless (eql value 0) value)
                       row-start (table-calc-token-start token)
                       row-end (table-calc-token-end token))))
              ((eql (table-calc-current-kind parser) :identifier)
               (let ((token (table-calc-current-token parser)))
                 (if (string= (table-calc-token-value token) "INVALID")
                     (progn
                       (table-calc-advance parser)
                       (setf row-kind :invalid
                             row-start (table-calc-token-start token)
                             row-end (table-calc-token-end token)))
                     (progn
                       (parse-hline-ordinal :hline-anchor)
                       (when (eql (table-calc-current-kind parser) :plus)
                         (table-calc-advance parser)
                         (let ((offset-token
                                 (table-calc-expect parser :number
                                                   :invalid-reference)))
                           (setf row-kind :hline-following
                                 row-offset
                                 (table-calc-reference-integer parser offset-token)
                                 row-end (table-calc-token-end offset-token))))))))
              (t
               (table-calc-fail formula (table-calc-position parser)
                                :invalid-reference)))
            (if (eql (table-calc-current-kind parser) :dollar)
                (progn
                  (table-calc-advance parser)
                  (if (eql row-kind :relative)
                      (let ((minus (table-calc-expect parser :minus
                                                      :invalid-reference)))
                        (setf column-start (table-calc-token-start minus))
                        (let* ((token (table-calc-expect parser :number
                                                         :invalid-reference))
                               (value (table-calc-reference-integer parser token)))
                          (setf column-kind :relative
                                column-value
                                (if (integerp value) (- value) value)
                                column-end (table-calc-token-end token))))
                      (parse-column)))
                (setf column-kind :current)))
          (progn
            (table-calc-expect parser :dollar :invalid-reference)
            (parse-column)
            (setf row-kind :current)))
    (let* ((last-token (svref (table-calc-parser-tokens parser)
                              (1- (table-calc-parser-cursor parser))))
           (end (table-calc-token-end last-token)))
      (%make-table-calc-reference :row-kind row-kind :row-value row-value
                                  :row-offset row-offset
                                  :column-kind column-kind :column-value column-value
                                  :row-start row-start :row-end row-end
                                  :column-start column-start :column-end column-end
                                  :start start :end end)))))

(defun table-calc-node (parser kind start end value children)
  (let ((depth (1+ (reduce #'max children
                           :key #'table-calc-expression-depth
                           :initial-value 0))))
    (check-limit (table-calc-parser-limits parser) :max-formula-ast-depth depth)
    (%make-table-calc-expression :kind kind :start start :end end
                                 :value value :children children :depth depth)))

(defun table-calc-enter-nesting (parser)
  (let ((depth (1+ (table-calc-parser-nesting parser))))
    (check-limit (table-calc-parser-limits parser) :max-formula-ast-depth depth)
    (setf (table-calc-parser-nesting parser) depth)))

(defun table-calc-leave-nesting (parser)
  (decf (table-calc-parser-nesting parser)))

(defun table-calc-expression-operator-precedence (kind)
  (case kind
    (:logical-or 1)
    ((:less :greater :equals) 5)
    ((:plus :minus) 10)
    (:divide 20)
    (:multiply 30)
    (otherwise nil)))

(defun table-calc-expression-operator (kind)
  (case kind
    (:plus #\+)
    (:minus #\-)
    (:divide #\/)
    (:multiply #\*)))

(defun table-calc-number-expression (parser token)
  (parse-safe-decimal (table-calc-parser-formula parser)
                      :start (table-calc-token-start token)
                      :end (table-calc-token-end token)
                      :limits (table-calc-parser-limits parser)))

(defun table-calc-parse-duration-seconds (text)
  (let* ((first-colon (position #\: text))
         (second-colon (position #\: text :start (1+ first-colon)))
         (hours (parse-integer text :end first-colon))
         (minutes (parse-integer text :start (1+ first-colon)
                                 :end (or second-colon (length text))))
         (seconds (if second-colon
                      (parse-integer text :start (1+ second-colon))
                      0)))
    (when (or (> minutes 59) (> seconds 59))
      (table-calc-fail text 0 :invalid-duration))
    (+ (* hours 3600) (* minutes 60) seconds)))

(defun table-calc-comparison-operator (kind)
  (case kind
    (:less :less)
    (:greater :greater)
    (:equals :equal)))

(defun table-calc-parse-prefix (parser)
  (let* ((formula (table-calc-parser-formula parser))
         (token (table-calc-current-token parser))
         (kind (table-calc-current-kind parser)))
    (cond
      ((eql kind :number)
       (table-calc-advance parser)
       (let ((value (table-calc-number-expression parser token)))
         (table-calc-node parser :number (table-calc-token-start token)
                          (table-calc-token-end token) value '())))
      ((eql kind :duration)
       (table-calc-advance parser)
       (table-calc-node parser :duration (table-calc-token-start token)
                        (table-calc-token-end token)
                        (table-calc-parse-duration-seconds
                         (table-calc-token-value token)) '()))
      ((eql kind :string)
       (table-calc-advance parser)
       (table-calc-node parser :string (table-calc-token-start token)
                        (table-calc-token-end token)
                        (table-calc-token-value token) '()))
      ((member kind '(:plus :minus))
       (let* ((operator (table-calc-advance parser))
              (start (table-calc-token-start operator)))
         (table-calc-enter-nesting parser)
         (unwind-protect
              (let ((child (table-calc-parse-prefix parser)))
                (table-calc-node parser :unary start (table-calc-expression-end child)
                                 (table-calc-expression-operator kind) (list child)))
           (table-calc-leave-nesting parser))))
      ((member kind '(:at :dollar))
       (let* ((reference (table-calc-parse-reference parser))
              (node (table-calc-node parser :reference
                                     (table-calc-reference-start reference)
                                     (table-calc-reference-end reference)
                                     reference '())))
         (when (eql (table-calc-current-kind parser) :range)
           (table-calc-advance parser)
           (unless (member (table-calc-current-kind parser)
                           '(:at :dollar :identifier))
             (table-calc-fail formula (table-calc-position parser) :invalid-range))
           (let* ((end-reference
                    (if (and (eql (table-calc-current-kind parser) :identifier)
                             (table-calc-roman-ordinal
                              (table-calc-token-value
                               (table-calc-current-token parser))))
                        (let* ((token (table-calc-advance parser))
                               (ordinal (table-calc-roman-ordinal
                                         (table-calc-token-value token))))
                          (%make-table-calc-reference
                           :row-kind :hline-anchor :row-value ordinal
                           :column-kind :current :start
                           (table-calc-token-start token) :end
                           (table-calc-token-end token)))
                        (table-calc-parse-reference parser)))
                  (end-node (table-calc-node parser :reference
                                             (table-calc-reference-start end-reference)
                                             (table-calc-reference-end end-reference)
                                             end-reference '())))
             (setf node (table-calc-node parser :range
                                         (table-calc-expression-start node)
                                         (table-calc-expression-end end-node)
                                         nil (list node end-node)))))
         node))
      ((eql kind :identifier)
       (table-calc-parse-symbol-or-function parser token))
      ((eql kind :left-paren)
       (let ((start (table-calc-token-start (table-calc-advance parser))))
         (table-calc-enter-nesting parser)
         (unwind-protect
              (let* ((expression (table-calc-parse-expression parser 0))
                     (close (table-calc-expect parser :right-paren :unmatched-parenthesis)))
                (setf (table-calc-expression-start expression)
                      start
                      (table-calc-expression-end expression)
                      (table-calc-token-end close))
                expression)
           (table-calc-leave-nesting parser))))
      (t
       (table-calc-fail formula (table-calc-position parser) :expected-expression)))))

(defun table-calc-parser-known-functions (parser)
  (let ((registered (table-calc-parser-functions parser)))
    (append '("vsum" "vmean" "vmin" "vmax" "vcount"
              "exp" "sin" "tan" "taylor" "if" "string" "typeof" "remote")
            (mapcar (lambda (entry)
                      (if (consp entry) (car entry) entry))
                    registered))))

(defun table-calc-parse-symbol-or-function (parser token)
  (let ((formula (table-calc-parser-formula parser)))
    (if (eql (table-calc-next-kind parser) :left-paren)
        (table-calc-parse-function parser token)
        (if (table-calc-parser-symbol-context parser)
            (progn
              (table-calc-advance parser)
              (table-calc-node parser :symbol (table-calc-token-start token)
                              (table-calc-token-end token)
                              (table-calc-token-value token) '()))
            (table-calc-fail formula (table-calc-token-start token)
                             :unknown-symbol)))))

(defun table-calc-parse-function-arguments (parser)
  (let ((arguments nil)
        (previous-context (table-calc-parser-symbol-context parser)))
    (setf (table-calc-parser-symbol-context parser) t)
    (unwind-protect
         (progn
           (unless (eql (table-calc-current-kind parser) :right-paren)
             (loop
               (push (table-calc-parse-expression parser 0) arguments)
               (if (eql (table-calc-current-kind parser) :comma)
                   (table-calc-advance parser)
                   (return))))
           (values (nreverse arguments)
                   (table-calc-expect parser :right-paren :malformed-function)))
      (setf (table-calc-parser-symbol-context parser) previous-context))))

(defun table-calc-validate-function-arguments (parser name arguments start)
  (let ((formula (table-calc-parser-formula parser)))
    (cond
      ((member name '("vsum" "vmean" "vmin" "vmax" "vcount")
               :test #'string=)
       (unless (and (= (length arguments) 1)
                    (let* ((argument (first arguments))
                           (children (table-calc-expression-children argument)))
                      (or (eq (table-calc-expression-kind argument) :range)
                          (and (eq (table-calc-expression-kind argument) :function)
                               (string= (table-calc-expression-value argument) "remote")
                               (= (length children) 2)
                               (eq (table-calc-expression-kind (second children))
                                   :range)))))
         (table-calc-fail formula
                          (if arguments
                              (table-calc-expression-start (first arguments))
                              start)
                          :unsupported-function-argument)))
      ((string= name "remote")
       (unless (and (= (length arguments) 2)
                    (member (table-calc-expression-kind (first arguments))
                            '(:symbol :string))
                    (member (table-calc-expression-kind (second arguments))
                            '(:reference :range)))
         (table-calc-fail formula start :unsupported-function-argument)))
      ((string= name "if")
       (unless (= (length arguments) 3)
         (table-calc-fail formula start :unsupported-function-argument)))
      ((string= name "taylor")
       (unless (and (= (length arguments) 3)
                    (eq (table-calc-expression-kind (second arguments)) :comparison)
                    (eq (table-calc-expression-value (second arguments)) :equal)
                    (eq (table-calc-expression-kind
                         (first (table-calc-expression-children
                                 (second arguments)))) :symbol))
         (table-calc-fail formula start :unsupported-function-argument)))
      ((member name '("exp" "sin" "tan" "string" "typeof")
               :test #'string=)
       (unless (= (length arguments) 1)
         (table-calc-fail formula start :unsupported-function-argument)))
      (t nil))))

(defun table-calc-parse-function (parser name-token)
  (let* ((formula (table-calc-parser-formula parser))
         (name (table-calc-token-value name-token)))
    (table-calc-advance parser)
    (unless (member name (table-calc-parser-known-functions parser)
                    :test #'string=)
      (table-calc-fail formula (table-calc-token-start name-token)
                       :unknown-function))
    (table-calc-advance parser)
    (table-calc-enter-nesting parser)
    (unwind-protect
         (multiple-value-bind (arguments close)
             (table-calc-parse-function-arguments parser)
           (table-calc-validate-function-arguments
            parser name arguments (table-calc-token-start name-token))
           (table-calc-node parser :function (table-calc-token-start name-token)
                            (table-calc-token-end close)
                            name
                            arguments))
      (table-calc-leave-nesting parser))))

(defun table-calc-parse-expression (parser minimum-precedence)
  (let ((left (table-calc-parse-prefix parser)))
    (loop for kind = (table-calc-current-kind parser)
          for precedence = (table-calc-expression-operator-precedence kind)
          while (and precedence (>= precedence minimum-precedence))
          do (let* ((operator-token (table-calc-advance parser))
                    (right (table-calc-parse-expression parser (1+ precedence))))
               (let* ((operator-kind (table-calc-token-kind operator-token))
                      (comparison-p
                        (member operator-kind '(:less :greater :equals)))
                      (logical-p (eq operator-kind :logical-or)))
                 (setf left
                       (table-calc-node parser
                                        (cond (comparison-p :comparison)
                                              (logical-p :logical-or)
                                              (t :binary))
                                        (table-calc-expression-start left)
                                        (table-calc-expression-end right)
                                        (if comparison-p
                                            (table-calc-comparison-operator
                                             operator-kind)
                                            (if logical-p :or
                                            (table-calc-expression-operator
                                             operator-kind)))
                                        (list left right))))))
    left))

(defun table-calc-parse-target (parser)
  (unless (member (table-calc-current-kind parser) '(:at :dollar))
    (table-calc-fail (table-calc-parser-formula parser)
                     (table-calc-position parser) :unsupported-assignment))
  (let* ((reference (table-calc-parse-reference parser))
         (row-kind (table-calc-reference-row-kind reference))
         (column-kind (table-calc-reference-column-kind reference)))
    (if (eql (table-calc-current-kind parser) :range)
        (progn
          (table-calc-advance parser)
          (unless (member (table-calc-current-kind parser)
                          '(:at :dollar :identifier))
              (table-calc-fail (table-calc-parser-formula parser)
                               (table-calc-position parser) :invalid-reference))
          (let* ((end-reference
                   (if (and (eql (table-calc-current-kind parser) :identifier)
                            (table-calc-roman-ordinal
                             (table-calc-token-value
                              (table-calc-current-token parser))))
                       (let* ((token (table-calc-advance parser))
                              (ordinal (table-calc-roman-ordinal
                                        (table-calc-token-value token))))
                         (%make-table-calc-reference
                          :row-kind :hline-anchor :row-value ordinal
                          :column-kind :current :start
                          (table-calc-token-start token) :end
                          (table-calc-token-end token)))
                       (table-calc-parse-reference parser)))
                 (end-row-kind (table-calc-reference-row-kind end-reference))
                 (end-column-kind (table-calc-reference-column-kind end-reference)))
            (unless (and (member row-kind '(:absolute :current :first :last
                                            :hline-anchor :hline-following))
                         (member end-row-kind '(:absolute :current :first :last
                                                :hline-anchor :hline-following))
                         (member column-kind '(:current :absolute :name))
                         (member end-column-kind '(:current :absolute :name)))
              (table-calc-fail (table-calc-parser-formula parser)
                               (table-calc-reference-start reference)
                               :invalid-reference))
            (values :range reference end-reference)))
        (cond
          ((and (member row-kind '(:absolute :first :last))
                (member column-kind '(:absolute :name)))
           (values :field reference nil))
          ((and (eql row-kind :current)
                (member column-kind '(:absolute :name)))
           (values :column reference nil))
          ((and (member row-kind '(:absolute :first :last))
                (eql column-kind :current))
           (values :row reference nil))
          ((eql row-kind :invalid)
           (values :invalid reference nil))
          (t
           (table-calc-fail (table-calc-parser-formula parser)
                            (table-calc-reference-start reference)
                            :unsupported-formula-target))))))

(defun table-calc-format-precision (text)
  (let* ((length (length text))
         (digits-end (1- length)))
    (unless (and (>= length 4)
                 (char= (char text 0) #\%)
                 (char= (char text 1) #\.)
                 (member (char text digits-end) '(#\f #\e))
                 (loop for index from 2 below digits-end
                       always (table-calc-digit-p (char text index)))
                 (< (- digits-end 2) 10))
      (return-from table-calc-format-precision nil))
    (let ((digits (subseq text 2 digits-end)))
      (when (or (and (> (length digits) 1)
                     (char= (char digits 0) #\0)))
        (return-from table-calc-format-precision nil))
      (let ((precision (parse-integer digits)))
        (if (char= (char text digits-end) #\e)
            (cons :scientific precision)
            precision)))))

(defun table-calc-parse-suffix (parser token)
  (let* ((formula (table-calc-parser-formula parser))
         (text (table-calc-token-value token))
         (format-start (position #\% text))
         (flag-text (subseq text 0 (or format-start (length text))))
         (format-text (when format-start (subseq text format-start)))
         (flags nil)
         (e-p nil)
         (n-p nil)
         (format-precision (when format-text
                             (table-calc-format-precision format-text))))
    (loop with position = 0
          while (progn
                  (loop while (and (< position (length flag-text))
                                   (table-calc-space-p (char flag-text position)))
                        do (incf position))
                  (< position (length flag-text)))
          do (let* ((character (char flag-text position))
                    (start position))
               (cond
                 ((char= character #\L)
                  (table-calc-fail formula
                                   (+ (table-calc-token-start token) 1 position)
                                   :lisp-mode))
                 ((char= character #\E) (setf e-p t) (incf position))
                 ((char= character #\N) (setf n-p t) (incf position))
                 ((and (char= character #\f)
                       (< (+ position 2) (length flag-text))
                       (char= (char flag-text (1+ position)) #\-)
                       (char= (char flag-text (+ position 2)) #\1))
                  (setf format-precision -1)
                  (incf position 3))
                 ((member character '(#\D #\R #\F #\S #\u #\T #\t #\U))
                  (push (case character
                          (#\D :degrees) (#\R :radians)
                          (#\F :fraction) (#\S :symbolic)
                          (#\u :units) (#\T :duration-clock)
                          (#\t :duration-decimal) (#\U :duration-minute))
                        flags)
                  (incf position))
                 ((member character '(#\p #\n #\s #\e #\f))
                  (incf position)
                  (let ((digit-start position))
                    (loop while (and (< position (length flag-text))
                                     (table-calc-digit-p
                                      (char flag-text position)))
                          do (incf position))
                    (unless (> position digit-start)
                      (table-calc-fail formula
                                       (+ (table-calc-token-start token) 1 start)
                                       :unsupported-mode))
                    (let ((precision (parse-integer flag-text
                                                    :start digit-start
                                                    :end position)))
                      (when (zerop precision)
                        (table-calc-fail formula
                                         (+ (table-calc-token-start token) 1 start)
                                         :unsupported-mode))
                      (check-limit (table-calc-parser-limits parser)
                                   :max-formula-precision precision)
                      (push (cons (case character
                                    (#\p :precision)
                                    (#\n :display-normal)
                                    (#\s :display-sci)
                                    (#\e :display-eng)
                                    (#\f :display-fixed))
                                  precision)
                            flags))))
                 (t
                  (table-calc-fail formula
                                   (+ (table-calc-token-start token) 1 position)
                                   :unsupported-mode)))))
    (when format-text
      (unless (table-calc-format-precision format-text)
        (table-calc-fail formula
                         (+ (table-calc-token-start token) 1 format-start)
                         :unsupported-format))
      (setf format-precision (table-calc-format-precision format-text)))
    (when (and format-precision (not (eql format-precision -1)))
      (check-limit (table-calc-parser-limits parser)
                   :max-formula-precision
                   (if (consp format-precision) (cdr format-precision)
                       format-precision)))
    (let ((mode (cond ((and e-p n-p) :en)
                      (e-p :e)
                      (n-p :n)
                      (t :default))))
      (values mode format-precision (nreverse flags)))))

(defun table-calc-validate-range-context (formula expression)
  (loop with pending = (list (cons expression nil))
        while pending
        do (let* ((entry (pop pending))
                  (node (car entry))
                  (parent-kind (cdr entry))
                  (kind (table-calc-expression-kind node)))
             (when (and (eql kind :range)
                        (not (eql parent-kind :function)))
               (table-calc-fail formula
                                (table-calc-expression-start node)
                                :range-outside-vector-function))
             (loop for child in (table-calc-expression-children node)
                   do (push (cons child kind) pending)))
        finally (return expression)))

(defun table-calc-next-assignment-has-mode-p (parser character)
  (loop for index from (table-calc-parser-cursor parser)
        below (length (table-calc-parser-tokens parser))
        for token = (svref (table-calc-parser-tokens parser) index)
        for kind = (table-calc-token-kind token)
        while (not (eq kind :separator))
        thereis (and (eq kind :suffix)
                     (find character (table-calc-token-value token)))))

(defun table-calc-parse-assignment (parser)
  (let* ((formula (table-calc-parser-formula parser))
         (start (table-calc-position parser)))
    (when (eql (table-calc-current-kind parser) :separator)
      (table-calc-fail formula start :empty-assignment))
    (multiple-value-bind (kind target target-end) (table-calc-parse-target parser)
      (table-calc-expect parser :equals :unsupported-assignment)
      (when (member (table-calc-current-kind parser) '(:separator :suffix nil))
        (table-calc-fail formula (table-calc-position parser) :empty-expression))
      (let* ((previous-symbol-context
               (table-calc-parser-symbol-context parser))
             (expression
               (unwind-protect
                    (progn
                      (when (table-calc-next-assignment-has-mode-p parser #\S)
                        (setf (table-calc-parser-symbol-context parser) t))
                      (table-calc-parse-expression parser 0))
                 (setf (table-calc-parser-symbol-context parser)
                       previous-symbol-context)))
             (mode :default)
             (flags nil)
             (format-precision nil))
        (table-calc-validate-range-context formula expression)
        (when (eql (table-calc-current-kind parser) :suffix)
          (multiple-value-setq (mode format-precision flags)
            (table-calc-parse-suffix parser (table-calc-advance parser))))
        (%make-table-calc-assignment
         :kind kind :target target :target-end target-end :expression expression
         :mode mode :flags flags :format-precision format-precision
         :start start
         :end (table-calc-token-end
               (svref (table-calc-parser-tokens parser)
                      (1- (table-calc-parser-cursor parser)))))))))

(defun table-calc-parse-formula (formula &optional (limits (make-limits)) functions)
  (let* ((tokens (table-calc-tokenize formula limits))
         (parser (%make-table-calc-parser :formula formula :tokens tokens
                                          :cursor 0 :limits limits :nesting 0
                                          :functions functions
                                          :symbol-context nil))
         (assignments '()))
    (when (zerop (length tokens))
      (table-calc-fail formula 0 :empty-formula))
    (loop
      do (push (table-calc-parse-assignment parser) assignments)
         (if (eql (table-calc-current-kind parser) :separator)
             (progn
               (table-calc-advance parser)
               (when (null (table-calc-current-token parser))
                 (return)))
             (return)))
    (when (table-calc-current-token parser)
      (table-calc-fail
       formula (table-calc-position parser)
       (if (eq (table-calc-current-kind parser) :comma)
           :unsupported-character
           :trailing-syntax)))
    (%make-table-calc-program :source formula :assignments (nreverse assignments))))

(defun parse-safe-decimal-exponent (text start end limits)
  (let ((negative nil)
        (position start)
        (magnitude 0)
        (ceiling (1+ (limit-value limits :max-formula-exponent)))
        (digits 0))
    (when (and (< position end) (find (char text position) "+-"))
      (setf negative (char= (char text position) #\-))
      (incf position))
    (loop while (and (< position end) (table-calc-digit-p (char text position)))
          for digit = (- (char-code (char text position)) 48)
          do (incf digits)
             (when (< magnitude ceiling)
               (if (> magnitude (floor (- ceiling digit) 10))
                   (setf magnitude ceiling)
                   (setf magnitude (+ (* magnitude 10) digit))))
             (incf position))
    (unless (plusp digits)
      (return-from parse-safe-decimal-exponent (values nil position nil)))
    (let ((exponent (if negative (- magnitude) magnitude)))
      (check-limit limits :max-formula-exponent (abs exponent))
      (values exponent position t))))

(defun safe-decimal-strip-trailing-zeroes (coefficient exponent)
  (loop while (and (not (zerop coefficient))
                   (zerop (mod coefficient 10)))
        do (setf coefficient (truncate coefficient 10))
           (incf exponent))
  (values coefficient exponent))

(defun parse-safe-decimal (text &key (start 0) (end (length text))
                                     (limits (make-limits)))
  (let ((position start)
        (negative nil)
        (whole-digits 0)
        (fraction-digits 0)
        (decimal-point-p nil)
        (significant-digits 0)
        (coefficient 0)
        (explicit-exponent 0))
    (when (and (< position end) (find (char text position) "+-"))
      (setf negative (char= (char text position) #\-))
      (incf position))
    (loop while (< position end)
          for character = (char text position)
          while (or (table-calc-digit-p character) (char= character #\.))
          do (cond
               ((table-calc-digit-p character)
                (let ((digit (- (char-code character) 48)))
                  (if decimal-point-p
                      (incf fraction-digits)
                      (incf whole-digits))
                  (when (or (plusp significant-digits) (plusp digit))
                    (incf significant-digits)
                    (check-limit limits :max-formula-precision significant-digits)
                    (setf coefficient (+ (* coefficient 10) digit)))
                  (incf position)))
               ((not decimal-point-p)
                (setf decimal-point-p t)
                (incf position))
               (t
                (return))))
    (unless (and (plusp whole-digits)
                 (or (not decimal-point-p) (plusp fraction-digits)))
      (return-from parse-safe-decimal nil))
    (when (and (< position end) (find (char text position) "eE"))
      (multiple-value-bind (exponent next-position valid-p)
          (parse-safe-decimal-exponent text (1+ position) end limits)
        (unless valid-p
          (return-from parse-safe-decimal nil))
        (setf explicit-exponent exponent
              position next-position)))
    (unless (= position end)
      (return-from parse-safe-decimal nil))
    (when (zerop significant-digits)
      (setf significant-digits 1)
      (check-limit limits :max-formula-precision significant-digits))
    (let ((scale (- explicit-exponent fraction-digits)))
      (multiple-value-setq (coefficient scale)
        (safe-decimal-strip-trailing-zeroes coefficient scale))
      (check-limit limits :max-formula-exponent (abs scale))
      (when negative
        (setf coefficient (- coefficient)))
      (%make-safe-decimal :coefficient coefficient :exponent scale
                          :negative-zero-p (and negative (zerop coefficient))
                          :float-p (or decimal-point-p (not (zerop explicit-exponent)))))))

(defun safe-decimal-rational (decimal)
  (let ((coefficient (safe-decimal-coefficient decimal))
        (exponent (safe-decimal-exponent decimal)))
    (cond ((zerop coefficient) 0)
          ((>= exponent 0) (* coefficient (expt 10 exponent)))
          (t (/ coefficient (expt 10 (- exponent)))))))

(defun safe-decimal-magnitude-compare (left right)
  (let* ((left-digits (write-to-string (abs (safe-decimal-coefficient left))
                                       :base 10 :radix nil))
         (right-digits (write-to-string (abs (safe-decimal-coefficient right))
                                        :base 10 :radix nil))
         (left-order (+ (length left-digits) (safe-decimal-exponent left)))
         (right-order (+ (length right-digits) (safe-decimal-exponent right))))
    (cond
      ((> left-order right-order) 1)
      ((< left-order right-order) -1)
      (t
       (loop for index below (max (length left-digits) (length right-digits))
             for left-digit = (if (< index (length left-digits))
                                  (char left-digits index) #\0)
             for right-digit = (if (< index (length right-digits))
                                   (char right-digits index) #\0)
             when (char> left-digit right-digit) do (return 1)
             when (char< left-digit right-digit) do (return -1)
             finally (return 0))))))

(defun safe-decimal-compare (left right)
  (let ((left-coefficient (safe-decimal-coefficient left))
        (right-coefficient (safe-decimal-coefficient right)))
    (cond
      ((and (zerop left-coefficient) (zerop right-coefficient)) 0)
      ((zerop left-coefficient) (if (minusp right-coefficient) 1 -1))
      ((zerop right-coefficient) (if (minusp left-coefficient) -1 1))
      ((and (minusp left-coefficient) (not (minusp right-coefficient))) -1)
      ((and (not (minusp left-coefficient)) (minusp right-coefficient)) 1)
      (t
       (let ((comparison (safe-decimal-magnitude-compare left right)))
         (if (minusp left-coefficient) (- comparison) comparison))))))

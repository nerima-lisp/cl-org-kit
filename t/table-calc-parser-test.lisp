;;;; t/table-calc-parser-test.lisp
(in-package #:cl-org-kit/test)

(defun table-calc-test-condition (formula &optional (limits (make-limits)))
  (handler-case
      (progn (org-kit::table-calc-parse-formula formula limits) nil)
    (error (condition) condition)))

(defun table-calc-test-assignment-expression (formula)
  (org-kit::table-calc-assignment-expression
   (first (org-kit::table-calc-program-assignments
           (org-kit::table-calc-parse-formula formula)))))

(defun table-calc-test-decimal (text &optional (limits (make-limits)))
  (org-kit::parse-safe-decimal text :limits limits))

(defun table-calc-test-oracle-path (tree name &optional (root "oracle"))
  (asdf:system-relative-pathname
   "cl-org-kit"
   (format nil "t/fixtures/table-calc/~A/~A/~A" root tree name)))

(defun table-calc-test-corpus-formulas (name &optional (root "oracle"))
  (let ((source (uiop:read-file-string
                 (table-calc-test-oracle-path "corpus" name root)))
        (formulas '()))
    (with-input-from-string (stream source)
      (loop for line = (read-line stream nil nil)
            while line
            for directive = (search "#+TBLFM:" line :test #'char-equal)
            when directive
              do (push (string-trim '(#\Space #\Tab #\Return)
                                    (subseq line (+ directive (length "#+TBLFM:"))))
                       formulas)))
    (nreverse formulas)))

(defun table-calc-test-oracle-rows (name &optional (root "oracle"))
  (let ((source (uiop:read-file-string
                 (table-calc-test-oracle-path "expected" name root)))
        (rows '()))
    (with-input-from-string (stream source)
      (loop for line = (read-line stream nil nil)
            while line
            unless (or (string= line "") (char= (char line 0) #\;))
              do (let ((separator (position #\Tab line)))
                   (unless (and separator
                                (null (position #\Tab line :start (1+ separator))))
                     (error "Malformed literal table-calc oracle row"))
                   (push (cons (subseq line 0 separator)
                               (subseq line (1+ separator)))
                         rows))))
    (nreverse rows)))

(describe "table formula grammar and reference IR"
  (it "parses Calc operator precedence, associativity, grouping, unary signs, and exponents"
    (let* ((division-root (table-calc-test-assignment-expression "$1=2/3*4"))
           (division-right (second (org-kit::table-calc-expression-children division-root)))
           (addition-root (table-calc-test-assignment-expression "$1=2+3/4"))
           (mixed-root (table-calc-test-assignment-expression "$1=2+3*4/5"))
           (chain-root (table-calc-test-assignment-expression "$1=12/3/2"))
           (unary-root (table-calc-test-assignment-expression "$1=2*-3"))
           (negative-subtraction (table-calc-test-assignment-expression "$1=2--3"))
           (grouped-root (table-calc-test-assignment-expression "$1=+(1.25E+2)"))
           (spaced-root (table-calc-test-assignment-expression
                         (format nil "$1 ~C= ~C~C1" #\Tab #\Return #\Newline)))
           (decimal (org-kit::table-calc-expression-value
                     (first (org-kit::table-calc-expression-children grouped-root)))))
      (with-soft-assertions
        (expect (org-kit::table-calc-expression-kind division-root) :to-be :binary)
        (expect (org-kit::table-calc-expression-value division-root) :to-be #\/)
        (expect (org-kit::table-calc-expression-value division-right) :to-be #\*)
        (expect (org-kit::table-calc-expression-value addition-root) :to-be #\+)
        (expect (org-kit::table-calc-expression-value mixed-root) :to-be #\+)
        (expect (org-kit::table-calc-expression-value
                 (second (org-kit::table-calc-expression-children mixed-root)))
                :to-be #\/)
        (expect (org-kit::table-calc-expression-value chain-root) :to-be #\/)
        (expect (org-kit::table-calc-expression-value
                 (first (org-kit::table-calc-expression-children chain-root)))
                :to-be #\/)
        (expect (org-kit::table-calc-expression-value unary-root) :to-be #\*)
        (expect (org-kit::table-calc-expression-kind
                 (second (org-kit::table-calc-expression-children unary-root)))
                :to-be :unary)
        (expect (org-kit::table-calc-expression-value negative-subtraction) :to-be #\-)
        (expect (org-kit::table-calc-expression-kind
                 (second (org-kit::table-calc-expression-children negative-subtraction)))
                :to-be :unary)
        (expect (org-kit::table-calc-expression-start grouped-root) :to-be 3)
        (expect (org-kit::table-calc-expression-end grouped-root) :to-be 13)
        (expect (org-kit::safe-decimal-rational decimal) :to-be 125)
        (expect (org-kit::table-calc-expression-kind spaced-root) :to-be :number)
        (expect (org-kit::safe-decimal-exponent
                 (org-kit::table-calc-expression-value
                  (table-calc-test-assignment-expression "$1=1e-3")))
                :to-be -3))))

  (it "records formula offsets and parses field, column, named, relative, last-row, and range references"
    (let* ((formula "$3=$1+$name::@2$3=@+1$-2+@>$1::@3$4=vsum(@1$2..@2$1)")
           (program (org-kit::table-calc-parse-formula formula))
           (assignments (org-kit::table-calc-program-assignments program))
           (column (first assignments))
           (field (second assignments))
           (range-function
             (org-kit::table-calc-assignment-expression (third assignments)))
           (range (first (org-kit::table-calc-expression-children range-function)))
           (sum (org-kit::table-calc-assignment-expression column))
           (relative (first (org-kit::table-calc-expression-children sum)))
           (named (second (org-kit::table-calc-expression-children sum)))
           (relative-and-last
             (org-kit::table-calc-assignment-expression field))
           (relative-reference
             (org-kit::table-calc-expression-value
              (first (org-kit::table-calc-expression-children relative-and-last))))
           (last-reference
             (org-kit::table-calc-expression-value
              (second (org-kit::table-calc-expression-children relative-and-last))))
           (underscore-name (table-calc-test-assignment-expression "$1=$_name"))
           (punctuated-name (table-calc-test-assignment-expression "$1=$col-2"))
           (punctuated-children
             (org-kit::table-calc-expression-children punctuated-name))
           (range-children (org-kit::table-calc-expression-children range))
           (range-start (org-kit::table-calc-expression-value (first range-children)))
           (range-end (org-kit::table-calc-expression-value (second range-children))))
      (with-soft-assertions
        (expect (length assignments) :to-be 3)
        (expect (org-kit::table-calc-assignment-kind column) :to-be :column)
        (expect (org-kit::table-calc-assignment-kind field) :to-be :field)
        (expect (org-kit::table-calc-assignment-start column) :to-be 0)
        (expect (org-kit::table-calc-expression-start sum)
                :to-be (search "$1" formula))
        (expect (org-kit::table-calc-expression-end sum)
                :to-be (+ (search "$name" formula) (length "$name")))
        (expect (org-kit::table-calc-reference-row-kind
                 (org-kit::table-calc-expression-value relative))
                :to-be :current)
        (expect (org-kit::table-calc-reference-column-value
                 (org-kit::table-calc-expression-value relative))
                :to-be 1)
        (expect (org-kit::table-calc-reference-column-kind
                 (org-kit::table-calc-expression-value named))
                :to-be :name)
        (expect (org-kit::table-calc-reference-column-value
                 (org-kit::table-calc-expression-value named))
                :to-equal "name")
        (expect (org-kit::table-calc-reference-column-value
                 (org-kit::table-calc-expression-value underscore-name))
                :to-equal "_name")
        (expect (org-kit::table-calc-expression-kind punctuated-name) :to-be :binary)
        (expect (org-kit::table-calc-expression-value punctuated-name) :to-be #\-)
        (expect (org-kit::table-calc-reference-column-value
                 (org-kit::table-calc-expression-value (first punctuated-children)))
                :to-equal "col")
        (expect (org-kit::safe-decimal-rational
                 (org-kit::table-calc-expression-value (second punctuated-children)))
                :to-be 2)
        (expect (org-kit::table-calc-reference-row-kind relative-reference)
                :to-be :relative)
        (expect (org-kit::table-calc-reference-row-value relative-reference)
                :to-be 1)
        (expect (org-kit::table-calc-reference-column-kind relative-reference)
                :to-be :relative)
        (expect (org-kit::table-calc-reference-column-value relative-reference)
                :to-be -2)
        (expect (org-kit::table-calc-reference-row-kind last-reference) :to-be :last)
        (expect (org-kit::table-calc-reference-column-value last-reference) :to-be 1)
        (expect (org-kit::table-calc-expression-kind range) :to-be :range)
        (expect (org-kit::table-calc-reference-row-value range-start) :to-be 1)
        (expect (org-kit::table-calc-reference-column-value range-start) :to-be 2)
        (expect (org-kit::table-calc-reference-row-value range-end) :to-be 2)
        (expect (org-kit::table-calc-reference-column-value range-end) :to-be 1)
        (expect (org-kit::table-calc-expression-start range)
                :to-be (search "@1$2" formula))
        (expect (org-kit::table-calc-expression-end range)
                :to-be (1- (length formula))))))

  (it "parses bare @> as the last row in the current column"
    (let* ((formula "@2$3=@>")
           (assignment (first (org-kit::table-calc-program-assignments
                               (org-kit::table-calc-parse-formula formula))))
           (expression (org-kit::table-calc-assignment-expression assignment))
           (reference (org-kit::table-calc-expression-value expression)))
      (with-soft-assertions
        (expect (org-kit::table-calc-expression-kind expression) :to-be :reference)
        (expect (org-kit::table-calc-reference-row-kind reference) :to-be :last)
        (expect (org-kit::table-calc-reference-row-value reference) :to-be nil)
        (expect (org-kit::table-calc-reference-column-kind reference) :to-be :current)
        (expect (org-kit::table-calc-reference-column-value reference) :to-be nil)
        (expect (org-kit::table-calc-reference-start reference) :to-be 5)
        (expect (org-kit::table-calc-reference-end reference) :to-be 7))))

  (it "records absolute axis spans and parses deleted-row references"
    (let* ((formula "$1=@INVALID$02")
           (program (org-kit::table-calc-parse-formula formula))
           (assignment (first (org-kit::table-calc-program-assignments program)))
           (target (org-kit::table-calc-assignment-target assignment))
           (reference
             (org-kit::table-calc-expression-value
              (org-kit::table-calc-assignment-expression assignment))))
      (with-soft-assertions
        (expect (org-kit::table-calc-reference-column-start target) :to-be 1)
        (expect (org-kit::table-calc-reference-column-end target) :to-be 2)
        (expect (org-kit::table-calc-reference-row-kind reference) :to-be :invalid)
        (expect (org-kit::table-calc-reference-row-value reference) :to-be nil)
        (expect (org-kit::table-calc-reference-row-start reference) :to-be 4)
        (expect (org-kit::table-calc-reference-row-end reference) :to-be 11)
        (expect (org-kit::table-calc-reference-column-kind reference)
                :to-be :absolute)
        (expect (org-kit::table-calc-reference-column-value reference) :to-be 2)
        (expect (org-kit::table-calc-reference-column-start reference) :to-be 12)
        (expect (org-kit::table-calc-reference-column-end reference) :to-be 14))))

  (it "preserves named columns and parses deleted-row references"
    (let* ((formula "$1=@3$INVALID::$name=@INVALID$1")
           (assignments (org-kit::table-calc-program-assignments
                         (org-kit::table-calc-parse-formula formula)))
           (qualified-reference
             (org-kit::table-calc-expression-value
              (org-kit::table-calc-assignment-expression (first assignments))))
           (target
             (org-kit::table-calc-assignment-target (second assignments)))
           (invalid-row-reference
             (org-kit::table-calc-expression-value
              (org-kit::table-calc-assignment-expression (second assignments)))))
      (with-soft-assertions
        (expect (org-kit::table-calc-assignment-kind (second assignments))
                :to-be :column)
        (expect (org-kit::table-calc-reference-row-kind qualified-reference)
                :to-be :absolute)
        (expect (org-kit::table-calc-reference-row-value qualified-reference)
                :to-be 3)
        (expect (org-kit::table-calc-reference-column-kind qualified-reference)
                :to-be :name)
        (expect (org-kit::table-calc-reference-column-value qualified-reference)
                :to-equal "INVALID")
        (expect (org-kit::table-calc-reference-column-start qualified-reference)
                :to-be 6)
        (expect (org-kit::table-calc-reference-column-end qualified-reference)
                :to-be 13)
        (expect (org-kit::table-calc-reference-column-kind target)
                :to-be :name)
        (expect (org-kit::table-calc-reference-column-value target)
                :to-equal "name")
        (expect (org-kit::table-calc-reference-row-kind invalid-row-reference)
                :to-be :invalid)
        (expect (org-kit::table-calc-reference-column-kind invalid-row-reference)
                :to-be :absolute))))

  (it "parses first and last row targets with hline-anchor value references"
    (let* ((formula "@<$2=@I$2::@>$2=vsum(@I$2..@II$2)")
           (assignments
             (org-kit::table-calc-program-assignments
              (org-kit::table-calc-parse-formula formula)))
           (first-assignment (first assignments))
           (last-assignment (second assignments))
           (first-target (org-kit::table-calc-assignment-target first-assignment))
           (last-target (org-kit::table-calc-assignment-target last-assignment))
           (first-value
             (org-kit::table-calc-expression-value
              (org-kit::table-calc-assignment-expression first-assignment)))
           (range
             (first (org-kit::table-calc-expression-children
                     (org-kit::table-calc-assignment-expression last-assignment))))
           (range-references
             (mapcar #'org-kit::table-calc-expression-value
                     (org-kit::table-calc-expression-children range))))
      (with-soft-assertions
        (expect (mapcar #'org-kit::table-calc-assignment-kind assignments)
                :to-equal '(:field :field))
        (expect (org-kit::table-calc-reference-row-kind first-target)
                :to-be :first)
        (expect (org-kit::table-calc-reference-row-kind last-target)
                :to-be :last)
        (expect (org-kit::table-calc-reference-row-kind first-value)
                :to-be :hline-anchor)
        (expect (org-kit::table-calc-reference-row-value first-value) :to-be 1)
        (expect (mapcar #'org-kit::table-calc-reference-row-kind range-references)
                :to-equal '(:hline-anchor :hline-anchor))
        (expect (mapcar #'org-kit::table-calc-reference-row-value range-references)
                :to-equal '(1 2)))))

  (it "keeps the exact case of named column references"
    (let* ((formula "$1=$INVALID+$invalid")
           (expression
             (org-kit::table-calc-assignment-expression
              (first (org-kit::table-calc-program-assignments
                      (org-kit::table-calc-parse-formula formula)))))
           (references (org-kit::table-calc-expression-children expression)))
      (with-soft-assertions
        (expect (mapcar (lambda (reference)
                          (org-kit::table-calc-reference-column-kind
                           (org-kit::table-calc-expression-value reference)))
                        references)
                :to-equal '(:name :name))
        (expect (mapcar (lambda (reference)
                          (org-kit::table-calc-reference-column-value
                           (org-kit::table-calc-expression-value reference)))
                        references)
                :to-equal '("INVALID" "invalid")))))

  (it "normalizes zero-padded references and bounds oversized reference integers"
    (let* ((overlong-digits
             (make-string (1+ (length (write-to-string most-positive-fixnum)))
                          :initial-element #\9))
           (zero-reference
             (org-kit::table-calc-assignment-target
              (first (org-kit::table-calc-program-assignments
                      (org-kit::table-calc-parse-formula "@000$1=1")))))
           (overlong-row
             (org-kit::table-calc-assignment-target
              (first (org-kit::table-calc-program-assignments
                      (org-kit::table-calc-parse-formula
                       (format nil "@~A$1=1" overlong-digits))))))
           (overlong-column
             (org-kit::table-calc-expression-value
              (table-calc-test-assignment-expression
               (format nil "$1=@+1$-~A" overlong-digits))))
           (zero-value (org-kit::table-calc-reference-row-value zero-reference))
           (row-value (org-kit::table-calc-reference-row-value overlong-row)))
      (with-soft-assertions
        (expect zero-value :to-be 0)
        (expect row-value :to-be :too-large)
        (expect (org-kit::table-calc-reference-column-value overlong-column)
                :to-be :too-large))))

  (it "records each assignment as a half-open source span"
    (let* ((assignments (org-kit::table-calc-program-assignments
                         (org-kit::table-calc-parse-formula "$1=1::$1=2")))
           (first-assignment (first assignments))
           (second-assignment (second assignments)))
      (with-soft-assertions
        (expect (org-kit::table-calc-assignment-start first-assignment) :to-be 0)
        (expect (org-kit::table-calc-assignment-end first-assignment) :to-be 4)
        (expect (org-kit::table-calc-assignment-start second-assignment) :to-be 6)
        (expect (org-kit::table-calc-assignment-end second-assignment) :to-be 10))))

  (it "parses the five supported vector functions and only contract modes and formats"
    (let* ((formula
             "$3=vsum($1..$2)::$3=vmean($1..$2)::$3=vmin($1..$2)::$3=vmax($1..$2)::$3=vcount($1..$2)")
           (functions
             (mapcar (lambda (assignment)
                       (org-kit::table-calc-expression-value
                        (org-kit::table-calc-assignment-expression assignment)))
                     (org-kit::table-calc-program-assignments
                      (org-kit::table-calc-parse-formula formula))))
           (modes (org-kit::table-calc-program-assignments
                   (org-kit::table-calc-parse-formula
                    "$1=2;E::$1=2;N::$1=2;EN::$1=2;%.2f::$1=2;EN%.12f"))))
      (with-soft-assertions
        (expect functions :to-equal '(:vsum :vmean :vmin :vmax :vcount))
        (expect (mapcar #'org-kit::table-calc-assignment-mode modes)
                :to-equal '(:e :n :en :default :en))
        (expect (mapcar #'org-kit::table-calc-assignment-format-precision modes)
                :to-equal '(nil nil nil 2 12)))))

  (it "rejects Lisp and reader syntax, unlisted functions, malformed references, and extra features"
    (let ((cases
            (list (list "'(+ 1 2)" 0 :quoted-lisp)
                  (list "`(+ 1 2)" 0 :quoted-lisp)
                  (list "#" 0 :reader-dispatch)
                  (list "#.(error \"run\")" 0 :reader-eval)
                  (list "#x" 0 :reader-dispatch)
                  (list "#1=(x)" 0 :reader-dispatch)
                  (list "@2$1=1;L" 7 :lisp-mode)
                  (list "@2$1=1;D" 7 :unsupported-mode)
                  (list "@2$1=1;%.2e" 7 :unsupported-format)
                  (list "@2$1=1;ENp3%.1f" 9 :unsupported-mode)
                  (list "$1=sin($1..$2)" 3 :unknown-function)
                  (list "$1=VSUM($1..$2)" 3 :unknown-function)
                  (list "$1=vsum(1)" 8 :unsupported-function-argument)
                  (list "x=1" 0 :unsupported-assignment)
                  (list "$1=foo" 3 :unknown-symbol)
                  (list "@2$=1" 3 :invalid-reference)
                  (list "$=1" 1 :invalid-reference)
                  (list "$1=@+" 5 :invalid-reference)
                  (list "$1=@x$1" 4 :invalid-reference)
                  (list "@1.5$1=1" 1 :invalid-reference)
                  (list "$1=@>$" 6 :invalid-reference)
                  (list "@+1$-1=1" 0 :unsupported-formula-target)
                  (list "@I$1=1" 0 :unsupported-formula-target)
                  (list "@2$1=$1..1" 9 :invalid-range)
                  (list "$1=.5" 3 :unsupported-character)
                  (list "$1=1+." 5 :unsupported-character)
                  (list "$1=5." 5 :invalid-number)
                  (list "$1=1e" 5 :invalid-number)
                  (list "$1=)" 3 :expected-expression)
                  (list "$1=1 2" 5 :trailing-syntax)
                  (list "$1=1,5" 4 :unsupported-character)
                  (list "$1=1^2" 4 :unsupported-character)
                  (list "$1=1;Q" 5 :unsupported-mode)
                  (list "$1=1;EX" 6 :unsupported-mode)
                  (list "$1=1;%x2f" 5 :unsupported-format)
                  (list "$1=1;%.f" 5 :unsupported-format)
                  (list "$1=1;%.af" 5 :unsupported-format)
                  (list "$1=1;%.02f" 5 :unsupported-format)
                  (list "$1=1;%.123f" 5 :unsupported-format)
                  (list "$1=1;%.13f" 5 :unsupported-format)
                  (list "@2$1=" 5 :empty-expression)
                  (list "::" 0 :empty-assignment)
                  (list "" 0 :empty-formula))))
      (with-soft-assertions
        (dolist (case cases)
          (let ((condition (table-calc-test-condition (first case))))
            (expect (typep condition 'org-table-unsupported-syntax) :to-be t)
            (when condition
              (expect (org-table-unsupported-syntax-position condition) :to-be (second case))
              (expect (org-table-unsupported-syntax-feature condition) :to-be (third case))))))))

  (it "accepts and preserves a trailing separator without adding an empty assignment"
    (dolist (formula '("$1=1::" "$1=1::$2=$1::"))
      (let ((program (org-kit::table-calc-parse-formula formula)))
        (with-soft-assertions
          (expect (org-kit::table-calc-program-source program) :to-equal formula)
          (expect (length (org-kit::table-calc-program-assignments program))
                  :to-be (if (string= formula "$1=1::") 1 2))))))

  (it "does not evaluate quoted forms or reader dispatch payloads"
    (let ((quoted (table-calc-test-condition "'(progn (error \"executed\"))"))
          (dispatch (table-calc-test-condition "#.(error \"executed\")")))
      (with-soft-assertions
        (expect (typep quoted 'org-table-unsupported-syntax) :to-be t)
        (expect (typep dispatch 'org-table-unsupported-syntax) :to-be t)
        (expect (org-table-unsupported-syntax-feature quoted) :to-be :quoted-lisp)
        (expect (org-table-unsupported-syntax-feature dispatch) :to-be :reader-eval))))

  (it "reports all unsupported-syntax condition readers and bounds the excerpt"
    (let* ((long-formula (concatenate 'string "$1=" (make-string 100 :initial-element #\x)
                                      "^") )
           (condition (table-calc-test-condition long-formula))
           (bare (make-condition 'org-table-unsupported-syntax :message "Refused")))
      (with-soft-assertions
        (expect (subtypep 'org-table-unsupported-syntax 'org-kit-error) :to-be t)
        (expect (org-table-unsupported-syntax-formula condition)
                :to-equal (org-kit::bounded-text long-formula))
        (expect (org-table-unsupported-syntax-position condition) :to-be 103)
        (expect (org-table-unsupported-syntax-feature condition)
                :to-be :unsupported-character)
        (expect (org-table-unsupported-syntax-formula bare) :to-be nil)
        (expect (org-table-unsupported-syntax-position bare) :to-be nil)
        (expect (org-table-unsupported-syntax-feature bare) :to-be nil)
        (expect (report bare) :to-equal "Refused"))))

  (it "enforces token, AST-depth, precision, and exponent limits at their boundaries"
    (let* ((token-exact (make-limits :max-formula-tokens 4))
           (token-below (make-limits :max-formula-tokens 3))
           (depth-one (make-limits :max-formula-ast-depth 1))
           (depth-two (make-limits :max-formula-ast-depth 2))
           (depth-three (make-limits :max-formula-ast-depth 3))
           (precision-two (make-limits :max-formula-precision 2))
           (exponent-two (make-limits :max-formula-exponent 2))
           (suffix-token-limit (make-limits :max-formula-tokens 4))
           (token-condition (table-calc-test-condition "$1=1" token-below))
           (reader-eval-condition
             (table-calc-test-condition "$1=1#." token-exact))
           (long-identifier-condition
             (table-calc-test-condition
              (concatenate 'string "$1=" (make-string 100000 :initial-element #\x))
              token-below))
           (long-suffix-condition
             (table-calc-test-condition
              (concatenate 'string "$1=1;" (make-string 100000 :initial-element #\x))
              suffix-token-limit))
           (depth-condition (table-calc-test-condition "$1=-1" depth-one))
           (precision-condition (table-calc-test-condition "$1=1.23" precision-two))
           (exponent-condition (table-calc-test-condition "$1=1e3" exponent-two))
           (saturated-exponent-condition
             (table-calc-test-condition "$1=1e999999" exponent-two)))
      (with-soft-assertions
        (expect (length (org-kit::table-calc-program-assignments
                         (org-kit::table-calc-parse-formula "$1=1" token-exact)))
                :to-be 1)
        (expect (typep reader-eval-condition 'org-table-unsupported-syntax)
                :to-be t)
        (when (typep reader-eval-condition 'org-table-unsupported-syntax)
          (expect (org-table-unsupported-syntax-position reader-eval-condition)
                  :to-be 4)
          (expect (org-table-unsupported-syntax-feature reader-eval-condition)
                  :to-be :reader-eval))
        (expect (org-resource-limit-error-limit-name token-condition)
                :to-be :max-formula-tokens)
        (expect (org-resource-limit-error-limit token-condition) :to-be 3)
        (expect (org-resource-limit-error-actual token-condition) :to-be 4)
        (expect (org-resource-limit-error-limit-name long-identifier-condition)
                :to-be :max-formula-tokens)
        (expect (org-resource-limit-error-actual long-identifier-condition) :to-be 4)
        (expect (org-resource-limit-error-limit-name long-suffix-condition)
                :to-be :max-formula-tokens)
        (expect (org-resource-limit-error-actual long-suffix-condition) :to-be 5)
        (expect (table-calc-test-condition "$1=1" (make-limits :max-formula-tokens 5))
                :to-be nil)
        (expect (org-resource-limit-error-limit-name depth-condition)
                :to-be :max-formula-ast-depth)
        (expect (table-calc-test-condition "$1=1" depth-two) :to-be nil)
        (expect (table-calc-test-condition "$1=-1" depth-two) :to-be nil)
        (expect (table-calc-test-condition "$1=-1" depth-three) :to-be nil)
        (expect (table-calc-test-condition "$1=(((1)))" depth-three) :to-be nil)
        (expect (org-resource-limit-error-actual
                 (table-calc-test-condition "$1=((((1))))" depth-three))
                :to-be 4)
        (expect (table-calc-test-condition "$1=1.2" precision-two) :to-be nil)
        (expect (table-calc-test-condition "$1=1" precision-two) :to-be nil)
        (expect (org-resource-limit-error-limit-name precision-condition)
                :to-be :max-formula-precision)
        (expect (org-resource-limit-error-actual precision-condition) :to-be 3)
        (expect (table-calc-test-condition "$1=1e2" exponent-two) :to-be nil)
        (expect (table-calc-test-condition "$1=1e1" exponent-two) :to-be nil)
        (expect (org-resource-limit-error-limit-name exponent-condition)
                :to-be :max-formula-exponent)
        (expect (org-resource-limit-error-actual exponent-condition) :to-be 3)
        (expect (org-resource-limit-error-actual saturated-exponent-condition)
                :to-be 3))))

  (it "preserves fixnum bounds for parsed references and decimals"
    (let* ((reference-limit (write-to-string most-positive-fixnum))
           (reference-over-limit (write-to-string (1+ most-positive-fixnum)))
           (reference-at-limit
             (org-kit::table-calc-parse-formula
              (format nil "@~A$1=1" reference-limit)))
           (reference-over
             (org-kit::table-calc-parse-formula
              (format nil "@~A$1=1" reference-over-limit)))
           (zero (table-calc-test-decimal "-0"))
           (positive-exponent (table-calc-test-decimal "1e2"))
           (negative-exponent (table-calc-test-decimal "1e-2")))
      (with-soft-assertions
        (expect (org-kit::table-calc-reference-row-value
                 (org-kit::table-calc-assignment-target
                  (first (org-kit::table-calc-program-assignments reference-at-limit))))
                :to-be most-positive-fixnum)
        (expect (org-kit::table-calc-reference-row-value
                 (org-kit::table-calc-assignment-target
                  (first (org-kit::table-calc-program-assignments reference-over))))
                :to-be :too-large)
        (expect (org-kit::safe-decimal-rational zero) :to-be 0)
        (expect (org-kit::safe-decimal-rational positive-exponent) :to-be 100)
        (expect (org-kit::safe-decimal-rational negative-exponent) :to-be 1/100)
        (expect (org-kit::safe-decimal-compare (table-calc-test-decimal "2")
                                               (table-calc-test-decimal "10"))
                :to-be -1)
        (expect (org-kit::safe-decimal-compare (table-calc-test-decimal "1.02")
                                               (table-calc-test-decimal "1.01"))
                :to-be 1))))

  (it "parses bounded signed decimals with default and explicit limits"
    (let* ((default-limits (org-kit::parse-safe-decimal "1"))
           (bounded-decimal
             (org-kit::parse-safe-decimal "x1y" :start 1 :end 2))
           (negative-zero (table-calc-test-decimal "-0.000e0"))
           (normalized (table-calc-test-decimal "0012.300e1"))
           (precision-limit (make-limits :max-formula-precision 2))
           (exponent-limit (make-limits :max-formula-exponent 2))
           (precision-condition
             (handler-case
                 (progn (table-calc-test-decimal "123" precision-limit) nil)
               (org-resource-limit-error (condition) condition)))
           (exponent-condition
             (handler-case
                 (progn (table-calc-test-decimal "1e3" exponent-limit) nil)
               (org-resource-limit-error (condition) condition))))
      (with-soft-assertions
        (expect (org-kit::safe-decimal-coefficient default-limits) :to-be 1)
        (expect (org-kit::safe-decimal-coefficient bounded-decimal) :to-be 1)
        (expect (org-kit::safe-decimal-negative-zero-p negative-zero) :to-be t)
        (expect (org-kit::safe-decimal-coefficient normalized) :to-be 123)
        (expect (org-kit::safe-decimal-exponent normalized) :to-be 0)
        (expect (org-kit::safe-decimal-rational normalized) :to-be 123)
        (expect (org-kit::safe-decimal-compare negative-zero (table-calc-test-decimal "0"))
                :to-be 0)
        (expect (table-calc-test-decimal "1e2" exponent-limit) :to-be-truthy)
        (expect (org-resource-limit-error-limit-name precision-condition)
                :to-be :max-formula-precision)
        (expect (org-resource-limit-error-actual precision-condition) :to-be 3)
        (expect (org-resource-limit-error-limit-name exponent-condition)
                :to-be :max-formula-exponent)
        (expect (org-resource-limit-error-actual exponent-condition) :to-be 3)
        (dolist (invalid '("" ".5" "5." "1e" "1e+" "1.2.3" "1,5" "1x" "++1"))
          (expect (table-calc-test-decimal invalid) :to-be nil)))))

  (it "compares safe decimals across signs, magnitudes, and equal values"
    (let* ((zero (table-calc-test-decimal "0"))
           (positive (table-calc-test-decimal "+1"))
           (negative (table-calc-test-decimal "-1"))
           (small-positive (table-calc-test-decimal "1.01"))
           (large-positive (table-calc-test-decimal "1.02"))
           (same-positive (table-calc-test-decimal "1.010")))
      (with-soft-assertions
        (expect (org-kit::safe-decimal-rational positive) :to-be 1)
        (expect (org-kit::safe-decimal-compare zero zero) :to-be 0)
        (expect (org-kit::safe-decimal-compare zero negative) :to-be 1)
        (expect (org-kit::safe-decimal-compare positive zero) :to-be 1)
        (expect (org-kit::safe-decimal-compare negative positive) :to-be -1)
        (expect (org-kit::safe-decimal-compare positive negative) :to-be 1)
        (expect (org-kit::safe-decimal-compare (table-calc-test-decimal "10")
                                               (table-calc-test-decimal "2"))
                :to-be 1)
        (expect (org-kit::safe-decimal-compare small-positive large-positive)
                :to-be -1)
        (expect (org-kit::safe-decimal-compare small-positive same-positive)
                :to-be 0)
        (expect (org-kit::safe-decimal-compare (table-calc-test-decimal "1.0")
                                               (table-calc-test-decimal "1.01"))
                :to-be -1)
        (expect (org-kit::safe-decimal-compare (table-calc-test-decimal "1.01")
                                               (table-calc-test-decimal "1.0"))
                :to-be 1)
        (expect (org-kit::safe-decimal-compare (table-calc-test-decimal "-10")
                                               (table-calc-test-decimal "-2"))
                :to-be -1))))

  (it "parses every formula in the Emacs oracle corpus"
    (let* ((corpus-formulas
             (append (table-calc-test-corpus-formulas "arithmetic.org")
                     (table-calc-test-corpus-formulas "references.org")))
           (rows (append (table-calc-test-oracle-rows "arithmetic.tsv")
                         (table-calc-test-oracle-rows "references.tsv")))
           (formulas (mapcar #'car rows))
           (values (mapcar #'cdr rows)))
      (with-soft-assertions
        (expect formulas :to-equal corpus-formulas)
        (expect values
                :to-equal '("0.16666667" "2.75" "4.4" "2" "-6" "-6" "5"
                            "9" "1e-3" "22" "33" "3" "6" "2" "1" "3"
                            "3" "11" "6" "9" "9" "11,20,33"))
        (dolist (row rows)
          (let* ((formula (car row))
                 (program (org-kit::table-calc-parse-formula formula))
                 (assignments (org-kit::table-calc-program-assignments program)))
            (expect (org-kit::table-calc-program-source program) :to-equal formula)
            (expect (org-kit::table-calc-assignment-start (first assignments)) :to-be 0)
            (expect (org-kit::table-calc-assignment-end (car (last assignments)))
                    :to-be (length formula)))))))

  (it "compares exact decimal values, including zero and negative values"
    (let ((decimal-zero (table-calc-test-decimal "0"))
          (negative-zero (table-calc-test-decimal "-0"))
          (positive-one (table-calc-test-decimal "1"))
          (negative-one (table-calc-test-decimal "-1")))
      (with-soft-assertions
        (expect (org-kit::safe-decimal-compare decimal-zero negative-zero) :to-be 0)
        (expect (org-kit::safe-decimal-compare decimal-zero negative-one) :to-be 1)
        (expect (org-kit::safe-decimal-compare decimal-zero positive-one) :to-be -1)
        (expect (org-kit::safe-decimal-compare positive-one decimal-zero) :to-be 1)
        (expect (org-kit::safe-decimal-compare negative-one decimal-zero) :to-be -1)
        (expect (org-kit::safe-decimal-compare negative-one positive-one) :to-be -1)
        (expect (org-kit::safe-decimal-compare positive-one negative-one) :to-be 1)
        (expect (org-kit::safe-decimal-compare (table-calc-test-decimal "1.2")
                                      (table-calc-test-decimal "1.20"))
                :to-be 0)
        (expect (org-kit::safe-decimal-compare (table-calc-test-decimal "1.01")
                                      (table-calc-test-decimal "1.02"))
                :to-be -1)
        (expect (org-kit::safe-decimal-compare (table-calc-test-decimal "1e3")
                                      (table-calc-test-decimal "999"))
                :to-be 1)
        (expect (org-kit::safe-decimal-compare (table-calc-test-decimal "999")
                                      (table-calc-test-decimal "1e3"))
                :to-be -1)
        (expect (org-kit::safe-decimal-compare (table-calc-test-decimal "-2")
                                      (table-calc-test-decimal "-3"))
                :to-be 1))))
)

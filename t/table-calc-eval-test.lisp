;;;; t/table-calc-eval-test.lisp
(in-package #:cl-org-kit/test)

(defun table-calc-eval-test-assignment (formula &optional (index 0) (limits (make-limits)))
  (let ((assignment
          (nth index
               (org-kit::table-calc-program-assignments
                (org-kit::table-calc-parse-formula formula limits)))))
    (or assignment (error "No table formula assignment at index ~D." index))))

(defun table-calc-eval-test-position-function
    (base-row base-column last-row named-columns &optional hline-anchors)
  (lambda (reference)
    (values
     (case (org-kit::table-calc-reference-row-kind reference)
       (:current base-row)
       (:absolute (org-kit::table-calc-reference-row-value reference))
       (:relative (+ base-row (org-kit::table-calc-reference-row-value reference)))
       (:first 1)
       (:last last-row)
       (:hline-anchor
        (cdr (assoc (org-kit::table-calc-reference-row-value reference)
                    hline-anchors)))
       (:invalid nil))
     (case (org-kit::table-calc-reference-column-kind reference)
       (:current base-column)
       (:absolute (org-kit::table-calc-reference-column-value reference))
       (:relative (+ base-column
                     (org-kit::table-calc-reference-column-value reference)))
       (:name
        (or (cdr (assoc (org-kit::table-calc-reference-column-value reference)
                        named-columns :test #'string=))
            (error "No test column for named reference.")))
       (otherwise (error "Unsupported reference column kind in evaluator test."))))))

(defun table-calc-eval-test-cell-map (&rest entries)
  (let ((cells (make-hash-table :test #'equal)))
    (dolist (entry entries cells)
      (setf (gethash (list (first entry) (second entry)) cells)
            (third entry)))))

(defun table-calc-eval-test-cell-value-function (cells)
  (lambda (row column)
    (multiple-value-bind (value present-p)
        (gethash (list row column) cells)
      (if present-p
          value
          (error "No test cell value for row ~D column ~D." row column)))))

(defun table-calc-eval-test-evaluate
    (formula &key (assignment-index 0) (base-row 1) (base-column 1)
                  (last-row 1) (named-columns nil) (cells nil)
                  (limits (make-limits)) work-budget hline-anchors)
  (let ((assignment (table-calc-eval-test-assignment
                     formula assignment-index limits)))
    (org-kit::table-calc-evaluate-assignment
     assignment
     (table-calc-eval-test-position-function
      base-row base-column last-row named-columns hline-anchors)
     (table-calc-eval-test-cell-value-function cells)
     :limits limits
     :work-budget (or work-budget
                      (org-kit::make-table-calc-work-budget)))))

(defun table-calc-eval-test-condition (formula &rest arguments)
  (handler-case
      (apply #'table-calc-eval-test-evaluate formula arguments)
    (error (condition) condition)))

(defun table-calc-eval-test-number-comparison (actual expected)
  (org-kit::safe-decimal-compare (table-calc-test-decimal actual)
                                 (table-calc-test-decimal expected)))

(defun table-calc-eval-test-expect-table-error (condition reason)
  (with-soft-assertions
    (expect (typep condition 'org-table-error) :to-be t)
    (when (typep condition 'org-table-error)
      (expect (org-table-error-reason condition) :to-be reason)
      (expect (stringp (org-table-error-target condition)) :to-be t)
      (expect (org-kit-error-message condition) :to-be-truthy))))

(defun table-calc-eval-test-expect-unsupported-syntax (condition feature)
  (expect (typep condition 'org-table-unsupported-syntax) :to-be t)
  (when (typep condition 'org-table-unsupported-syntax)
    (expect (org-table-unsupported-syntax-feature condition)
            :to-be feature)))

(defun table-calc-eval-test-expect-range-policy (formula)
  (let* ((corpus (table-calc-test-corpus-formulas
                  "range-vector-algebra.org" "policy"))
         (oracle-row
           (assoc formula
                  (table-calc-test-oracle-rows
                   "range-vector-algebra.tsv" "policy")
                  :test #'string=)))
    (expect (member formula corpus :test #'string=) :to-be-truthy)
    (expect oracle-row :to-be-truthy)
    (when oracle-row
      (expect (stringp (cdr oracle-row)) :to-be t)
      (expect (plusp (length (cdr oracle-row))) :to-be t))
    (table-calc-eval-test-expect-unsupported-syntax
     (table-calc-eval-test-condition
      formula :base-row 1 :base-column 2 :last-row 2
      :cells (table-calc-eval-test-cell-map '(1 1 "1") '(2 1 "2")))
     :range-outside-vector-function)))

(defun table-calc-eval-test-expect-limit (condition name limit actual)
  (with-soft-assertions
    (expect (typep condition 'org-resource-limit-error) :to-be t)
    (when (typep condition 'org-resource-limit-error)
      (expect (org-resource-limit-error-limit-name condition) :to-be name)
      (expect (org-resource-limit-error-limit condition) :to-be limit)
      (expect (org-resource-limit-error-actual condition) :to-be actual))))

(defun table-calc-eval-test-find-node (root type)
  (or (when (eq (node-type root) type) root)
      (some (lambda (child)
              (table-calc-eval-test-find-node child type))
            (node-children root))))

(defun table-calc-eval-test-oracle-rows-at
    (name base-row base-column last-row cells)
  (let ((rows (table-calc-test-oracle-rows name)))
    (expect (> (length rows) 0) :to-be t)
    (dolist (row rows)
      (expect (table-calc-eval-test-evaluate
               (car row) :base-row base-row :base-column base-column
               :last-row last-row :cells cells)
              :to-equal (cdr row)))))

(defun table-calc-eval-test-oracle-vector-rows (name cells)
  (table-calc-eval-test-oracle-rows-at name 1 2 3 cells))

(describe "table formula numeric evaluation"
  (it "evaluates arithmetic ASTs to the fixed Org oracle values"
    (let ((rows (table-calc-test-oracle-rows "arithmetic.tsv")))
      (expect (> (length rows) 0) :to-be t)
      (dolist (row rows)
        (let ((actual (table-calc-eval-test-evaluate (car row))))
          (if (string= (car row) "@1$1=1e-3")
              (expect (table-calc-eval-test-number-comparison actual (cdr row))
                      :to-be 0)
              (expect actual :to-equal (cdr row)))))))

  (it "evaluates absolute, relative, last-row, named, and range references"
    (let* ((rows (table-calc-test-oracle-rows "references.tsv"))
           (cases
             '(("@2$3=$1+$2" 2 3 3 nil ((2 1 "2") (2 2 "20")))
               ("@2$3=@+1$-2+@+1$-1" 2 3 3 nil ((3 1 "3") (3 2 "30")))
               ("@2$3=@>$1" 2 3 3 nil ((3 1 "3")))
               ("@2$3=vsum(@1$1..@3$1)" 2 3 3 nil
                ((1 1 "1") (2 1 "2") (3 1 "3")))
               ("@2$3=vmean(@1$1..@3$1)" 2 3 3 nil
                ((1 1 "1") (2 1 "2") (3 1 "3")))
               ("@2$3=vmin(@1$1..@3$1)" 2 3 3 nil
                ((1 1 "1") (2 1 "2") (3 1 "3")))
               ("@2$3=vmax(@1$1..@3$1)" 2 3 3 nil
                ((1 1 "1") (2 1 "2") (3 1 "3")))
               ("@2$3=vcount(@1$1..@3$1)" 2 3 3 nil
                ((1 1 "1") (2 1 "2") (3 1 "3")))
               ("@1$3=vsum($1..$2)" 1 3 1 nil
                ((1 1 "1") (1 2 "10")))
               ("@2$3=vsum(@3$1..@1$1)" 2 3 3 nil
                ((1 1 "1") (2 1 "2") (3 1 "3")))
               ("@2$3=@>" 2 3 3 nil ((3 3 "9")))
               ("@2$3=$x-1" 2 3 3 (("x" . 2)) ((2 2 "10"))))))
      (dolist (case cases)
        (destructuring-bind (formula base-row base-column last-row names cells) case
          (let ((expected (cdr (assoc formula rows :test #'string=))))
            (expect (stringp expected) :to-be t)
            (expect (table-calc-eval-test-number-comparison
                     (table-calc-eval-test-evaluate
                      formula :base-row base-row :base-column base-column
                      :last-row last-row :named-columns names
                      :cells (apply #'table-calc-eval-test-cell-map cells))
                     expected)
                    :to-be 0))))))

  (it "matches vector, E, EN, and nonnumeric N values in the Org fixtures"
    (table-calc-eval-test-oracle-vector-rows
     "t4b/vector-default.tsv"
     (table-calc-eval-test-cell-map '(1 1 "1") '(2 1 "") '(3 1 "5")))
    (table-calc-eval-test-oracle-vector-rows
     "t4b/vector-e.tsv"
     (table-calc-eval-test-cell-map '(1 1 "1") '(2 1 "") '(3 1 "5")))
    (table-calc-eval-test-oracle-vector-rows
     "t4b/vector-en.tsv"
     (table-calc-eval-test-cell-map '(1 1 "1") '(2 1 "") '(3 1 "5")))
    (table-calc-eval-test-oracle-vector-rows
     "t4b/vector-n-nonnumeric.tsv"
     (table-calc-eval-test-cell-map '(1 1 "1") '(2 1 "missing") '(3 1 "5"))))

  (it "matches Org for rectangular vector ranges"
    (table-calc-eval-test-oracle-rows-at
     "t4b/vector-rectangle.tsv" 5 4 5
     (table-calc-eval-test-cell-map
      '(2 1 "1") '(2 2 "2") '(2 3 "3")
      '(3 1 "4") '(3 2 "5") '(3 3 "6")
      '(4 1 "7") '(4 2 "8") '(4 3 "9"))))

  (it "matches Org EN handling of nonnumeric vector cells"
    (table-calc-eval-test-oracle-vector-rows
     "t4b/vector-en-nonnumeric.tsv"
     (table-calc-eval-test-cell-map
      '(1 1 "1") '(2 1 "missing") '(3 1 "5"))))

  (it "pins the decimal text returned by a scalar reference"
    (table-calc-eval-test-oracle-rows-at
     "t4b/reference-format.tsv" 1 2 1
     (table-calc-eval-test-cell-map '(1 1 "1.2300"))))

  (it "applies N to blank vector slots as zero while retaining their count"
    (let ((cells (table-calc-eval-test-cell-map '(1 1 ""))))
      (dolist (case '(("@1$2=vsum(@1$1..@1$1);N" . "0")
                      ("@1$2=vmean(@1$1..@1$1);N" . "0")
                      ("@1$2=vmin(@1$1..@1$1);N" . "0")
                      ("@1$2=vmax(@1$1..@1$1);N" . "0")
                      ("@1$2=vcount(@1$1..@1$1);N" . "1")))
        (expect (table-calc-eval-test-evaluate
                 (car case) :base-row 1 :base-column 2 :cells cells)
                :to-equal (cdr case)))))

  (it "converts an empty scalar reference according to its assignment mode"
    (let ((cells (table-calc-eval-test-cell-map '(1 2 ""))))
      (dolist (case '(("@1$1=@1$2" . "0")
                      ("@1$1=@1$2;N" . "0")
                      ("@1$1=@1$2;EN" . "0")))
        (expect (table-calc-eval-test-evaluate
                 (car case) :base-row 1 :base-column 1 :cells cells)
                :to-equal (cdr case)))))

  (it "rejects an E-mode NaN at the assignment boundary"
    (let* ((cells (table-calc-eval-test-cell-map '(1 2 "")))
           (condition
             (table-calc-eval-test-condition
              "@1$1=@1$2;E" :base-row 1 :base-column 1 :cells cells)))
      (table-calc-eval-test-expect-table-error condition :non-finite-result))
    (let* ((cells (table-calc-eval-test-cell-map '(1 2 "")))
           (condition
             (table-calc-eval-test-condition
              "@1$1=@1$2+1;E" :base-row 1 :base-column 1 :cells cells)))
      (table-calc-eval-test-expect-table-error condition :non-finite-result))
    (let ((cells (table-calc-eval-test-cell-map '(1 2 ""))))
      (dolist (formula '("@1$1=-@1$2;E" "@1$1=+@1$2;E"))
        (table-calc-eval-test-expect-table-error
         (table-calc-eval-test-condition
          formula :base-row 1 :base-column 1 :cells cells)
         :non-finite-result))))

  (it "handles a filtered empty vector and propagates E-mode NaN"
    (let* ((blank (table-calc-eval-test-cell-map '(1 1 "")))
           (sum (table-calc-eval-test-evaluate
                 "@1$2=vsum(@1$1..@1$1)" :base-row 1 :base-column 2 :cells blank))
           (count (table-calc-eval-test-evaluate
                   "@1$2=vcount(@1$1..@1$1)" :base-row 1 :base-column 2
                   :cells blank)))
      (expect sum :to-equal "0")
      (expect count :to-equal "0")
      (dolist (function '("vmean" "vmin" "vmax"))
        (table-calc-eval-test-expect-table-error
         (table-calc-eval-test-condition
          (format nil "@1$2=~A(@1$1..@1$1)" function)
          :base-row 1 :base-column 2 :cells blank)
         :empty-range))
      (expect (table-calc-eval-test-evaluate
               "@1$2=vcount(@1$1..@1$1);E"
               :base-row 1 :base-column 2 :cells blank)
              :to-equal "1")
      (dolist (function '("vsum" "vmean" "vmin" "vmax"))
        (table-calc-eval-test-expect-table-error
         (table-calc-eval-test-condition
          (format nil "@1$2=~A(@1$1..@1$1);E" function)
          :base-row 1 :base-column 2 :cells blank)
         :non-finite-result))))

  (it "rounds fixed format output half-even and preserves contract negative zero"
    (let ((rows (table-calc-test-oracle-rows "t4b/format-rounding.tsv")))
      (expect (> (length rows) 0) :to-be t)
      (dolist (row rows)
        (expect (table-calc-eval-test-evaluate (car row))
                :to-equal (cdr row))))
    (expect (table-calc-eval-test-evaluate "@1$1=1.15;%.1f")
            :to-equal "1.2")
    (expect (table-calc-eval-test-evaluate "@1$1=-0;%.2f")
            :to-equal "-0.00")
    (expect (table-calc-eval-test-evaluate "@1$1=1e-3")
            :to-equal "0.001")))

  (it "evaluates current-column targets and assignment defaults through parsed input"
    (let* ((assignment (table-calc-eval-test-assignment "$2=7"))
           (result
             (org-kit::table-calc-evaluate-assignment
              assignment
              (table-calc-eval-test-position-function 1 2 1 nil)
              (table-calc-eval-test-cell-value-function
               (table-calc-eval-test-cell-map)))))
      (expect result :to-equal "7")))

  (it "rejects invalid and named targets before evaluation formats them"
    (dolist (case '(("@INVALID$2=1" :invalid-target "@INVALID$2")
                    ("$name=1" :unsupported-target "$name")))
      (let ((condition (table-calc-eval-test-condition (first case))))
        (table-calc-eval-test-expect-table-error condition (second case))
        (expect (org-table-error-target condition) :to-equal (third case))))
    (let ((condition
            (table-calc-eval-test-condition
             "@1$1=@INVALID$2" :cells (table-calc-eval-test-cell-map))))
      (table-calc-eval-test-expect-table-error condition :invalid-reference)
      (expect (org-table-error-target condition) :to-equal "@1$1")))

  (it "evaluates hline-anchor ranges across inserted rows"
    (let ((before-insertion
            (table-calc-eval-test-evaluate
             "@1$3=vsum(@I$2..@II$2)"
             :base-row 1 :base-column 3 :last-row 5
             :hline-anchors '((1 . 3) (2 . 4))
             :cells (table-calc-eval-test-cell-map
                     '(3 2 "10") '(4 2 "20"))))
          (after-insertion
            (table-calc-eval-test-evaluate
             "@1$3=vsum(@I$2..@II$2)"
             :base-row 1 :base-column 3 :last-row 6
             :hline-anchors '((1 . 3) (2 . 5))
             :cells (table-calc-eval-test-cell-map
                     '(3 2 "10") '(4 2 "") '(5 2 "20")))))
      (expect before-insertion :to-equal "30")
      (expect after-insertion :to-equal "30"))
    (let ((condition
            (table-calc-eval-test-condition
             "@1$3=vsum(@I$2..@II$2)"
             :cells (table-calc-eval-test-cell-map '(1 2 "10")))))
      (table-calc-eval-test-expect-table-error condition :unsupported-reference)))

  (it "preserves invalid and named targets through the public table edit API"
    (let* ((formula "@INVALID$2=1::$name=2")
           (source (format nil
                           "| A | B |~%|---+---|~%| x | y |~%#+TBLFM: ~A~%"
                           formula))
           (document (parse-org source))
           (table (table-calc-eval-test-find-node (document-root document) :table))
           (result (table-insert-row document table 2))
           (updated (document-text (edit-result-updated-document result))))
      (with-soft-assertions
        (expect (edit-result-p result) :to-be t)
        (expect (search (format nil "#+TBLFM: ~A" formula) updated)
                :not :to-be nil)
        (expect (document-text document) :to-equal source))))

  (it "preserves signed zero through unary, addition, and multiplication"
    (dolist (case '(("@1$1=--0" . "0")
                    ("@1$1=-0+-0" . "-0")
                    ("@1$1=0*-9" . "-0")))
      (expect (table-calc-eval-test-evaluate (car case))
              :to-equal (cdr case))))

  (it "rounds large-order addition without expanding distant decimal places"
    (let ((limits (make-limits :max-formula-precision 2)))
      (dolist (formula '("@1$1=1e3+1" "@1$1=1+1e3"))
        (expect (table-calc-eval-test-evaluate formula :limits limits)
                :to-equal "1000"))))

  (it "checks division ratio ordering and exponent bounds"
    (dolist (case '(("@1$1=1/22" . "0.045454545")
                    ("@1$1=12/2" . "6")
                    ("@1$1=9/11" . "0.81818182")
                    ("@1$1=10/99" . "0.1010101")
                    ("@1$1=9/99" . "0.090909091")
                    ("@1$1=99/9" . "11")
                    ("@1$1=1/10" . "0.1")
                    ("@1$1=0/1" . "0")
                    ("@1$1=-0/1" . "-0")))
      (expect (table-calc-eval-test-evaluate (car case))
              :to-equal (cdr case)))
    (let* ((limits (make-limits :max-formula-precision 2
                                :max-formula-exponent 2))
           (overflow
             (table-calc-eval-test-condition "@1$1=1e2/1e-2" :limits limits))
           (underflow
             (table-calc-eval-test-condition "@1$1=1e-2/1e2" :limits limits)))
      (table-calc-eval-test-expect-limit
       overflow :max-formula-exponent 2 3)
      (table-calc-eval-test-expect-limit
       underflow :max-formula-exponent 2 4)))

  (it "checks rounding carry against the exponent limit and rounds negative products"
    (let* ((limits (make-limits :max-formula-precision 1
                                :max-formula-exponent 0))
           (condition (table-calc-eval-test-condition "@1$1=9*9" :limits limits))
           (negative-limits (make-limits :max-formula-precision 1
                                         :max-formula-exponent 1)))
      (table-calc-eval-test-expect-limit
       condition :max-formula-exponent 0 1)
      (expect (table-calc-eval-test-evaluate "@1$1=-9*9"
                                             :limits negative-limits)
              :to-equal "-80")))

  (it "signals a table error for non-string callback cell values"
    (table-calc-eval-test-expect-table-error
     (table-calc-eval-test-condition
      "@1$1=@1$2" :base-row 1 :base-column 1
      :cells (table-calc-eval-test-cell-map '(1 2 7)))
     :invalid-value))

  (it "checks zero precision through formula inputs"
    (let* ((zero-precision-limits (make-limits :max-formula-precision 0))
           (zero-precision
             (table-calc-eval-test-condition
              "@1$2=vcount(@1$1..@1$1)" :base-row 1 :base-column 2
              :cells (table-calc-eval-test-cell-map '(1 1 ""))
              :limits zero-precision-limits)))
      (table-calc-eval-test-expect-limit
       zero-precision :max-formula-precision 0 1)))

  (it "rejects a range as an assignment value per Org policy"
    (table-calc-eval-test-expect-range-policy
     "@1$2=@1$1..@2$1"))

  (it "rejects arithmetic on a range per Org policy"
    (table-calc-eval-test-expect-range-policy
     "@1$2=(@1$1..@2$1)+1"))

  (it "rejects unary arithmetic on a range per Org policy"
    (table-calc-eval-test-expect-range-policy
     "@1$2=-@1$1..@2$1"))

  (it "signals table conditions for zero division and external non-finite numbers"
    (table-calc-eval-test-expect-table-error
     (table-calc-eval-test-condition "@1$1=1/0")
     :division-by-zero)
    (table-calc-eval-test-expect-table-error
     (table-calc-eval-test-condition
      "@1$2=@1$1" :base-row 1 :base-column 2
      :cells (table-calc-eval-test-cell-map '(1 1 "Infinity")))
     :invalid-number))

  (it "rounds bounded intermediates and checks precision and exponent limits"
    (let* ((precision-limits (make-limits :max-formula-precision 2))
           (precision-input-over
             (table-calc-eval-test-condition
              "@1$1=123" :limits precision-limits))
           (exponent-limits (make-limits :max-formula-exponent 2))
           (exponent-over
             (table-calc-eval-test-condition
              "@1$1=1e2*10" :limits exponent-limits)))
      (expect (table-calc-eval-test-evaluate "@1$1=99*99"
                                             :limits precision-limits)
              :to-equal "9800")
      (table-calc-eval-test-expect-limit
       precision-input-over :max-formula-precision 2 3)
      (expect (table-calc-eval-test-evaluate
               "@1$1=1e2" :limits exponent-limits)
              :to-equal "100")
      (table-calc-eval-test-expect-limit
       exponent-over :max-formula-exponent 2 3)))

  (it "evaluates an AST at the configured maximum depth"
    (let* ((limits (make-limits :max-formula-ast-depth 256))
           (formula (format nil "@1$1=~A1"
                            (make-string 255 :initial-element #\+))))
      (expect (table-calc-eval-test-evaluate formula :limits limits)
              :to-equal "1")))

  (it "enforces the shared work budget before exceeding its limit"
    (let* ((limits (make-limits :max-formula-work 3))
           (budget (org-kit::make-table-calc-work-budget))
           (over-limits (make-limits :max-formula-work 2))
           (over-budget (org-kit::make-table-calc-work-budget)))
      (expect (table-calc-eval-test-evaluate "@1$1=1+1"
                                             :limits limits :work-budget budget)
              :to-equal "2")
      (table-calc-eval-test-expect-limit
       (table-calc-eval-test-condition "@1$1=1+1"
                                       :limits limits :work-budget budget)
       :max-formula-work 3 4)
      (table-calc-eval-test-expect-limit
       (table-calc-eval-test-condition "@1$1=1+1"
                                       :limits over-limits
                                       :work-budget over-budget)
       :max-formula-work 2 3)))

(describe "table formula evaluator work limits"
  (it "charges cell and vector work before reading cells"
    (let* ((formula "@1$3=vsum(@1$1..@1$2)")
           (assignment (table-calc-eval-test-assignment formula))
           (reads 0)
           (read-cell (lambda (row column)
                        (declare (ignore row))
                        (incf reads)
                        (if (= column 1) "1" "2")))
           (position-function
             (table-calc-eval-test-position-function 1 3 1 nil)))
      (expect (org-kit::table-calc-evaluate-assignment
               assignment position-function read-cell
               :limits (make-limits :max-formula-work 8))
              :to-equal "3")
      (expect reads :to-equal 2)
      (setf reads 0)
      (table-calc-eval-test-expect-limit
       (handler-case
           (org-kit::table-calc-evaluate-assignment
            assignment position-function read-cell
            :limits (make-limits :max-formula-work 7))
         (error (condition) condition))
       :max-formula-work 7 8)
      (expect reads :to-equal 0))
    (let* ((assignment (table-calc-eval-test-assignment "@1$3=@1$1"))
           (reads 0)
           (position-function
             (table-calc-eval-test-position-function 1 3 1 nil))
           (condition
             (handler-case
                 (org-kit::table-calc-evaluate-assignment
                  assignment position-function
                  (lambda (row column)
                    (declare (ignore row column))
                    (incf reads)
                    "1")
                  :limits (make-limits :max-formula-work 1))
               (error (condition) condition))))
      (table-calc-eval-test-expect-limit
       condition :max-formula-work 1 2)
      (expect reads :to-equal 0))))

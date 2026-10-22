;;;; t/table-calc-eval-test.lisp
(in-package #:cl-org-kit/test)

(defun table-calc-eval-test-assignment
    (formula &optional (index 0) (limits (make-limits)) functions)
  (let ((assignment
          (nth index
               (org-kit::table-calc-program-assignments
                (org-kit::table-calc-parse-formula formula limits functions)))))
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
                  (limits (make-limits)) work-budget hline-anchors functions
                  reference-value-function remote-function current-row current-column)
  (let ((assignment (table-calc-eval-test-assignment
                     formula assignment-index limits functions)))
    (org-kit::table-calc-evaluate-assignment
     assignment
     (table-calc-eval-test-position-function
      base-row base-column last-row named-columns hline-anchors)
     (table-calc-eval-test-cell-value-function cells)
     :limits limits
     :work-budget (or work-budget
                      (org-kit::make-table-calc-work-budget))
     :functions functions
     :reference-value-function reference-value-function
     :remote-function remote-function
     :current-row current-row
     :current-column current-column)))

(defun table-calc-eval-test-expression-value (formula)
  (let* ((assignment (table-calc-eval-test-assignment formula))
         (limits (make-limits)))
    (org-kit::table-calc-evaluate-expression
     (org-kit::table-calc-assignment-expression assignment)
     (table-calc-eval-test-position-function 1 1 1 nil)
     (table-calc-eval-test-cell-value-function
      (table-calc-eval-test-cell-map))
     (org-kit::table-calc-assignment-mode assignment)
     limits (org-kit::make-table-calc-work-budget)
     (org-kit::table-calc-assignment-target-description assignment)
     :flags (org-kit::table-calc-assignment-flags assignment))))

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

  (it "evaluates each Calc function listed in the Org table manual"
    (dolist (case '(("@1$1=exp(1)" . "2.7182818")
                    ("@1$1=exp(-50)" . "1.9287498e-22")
                    ("@1$1=exp(80);p20" . "5.5406224e34")
                    ("@1$1=sin(30)" . "0.5")
                    ("@1$1=sin(0.0000000001)" . "1.7453293e-12")
                    ("@1$1=sin(30);D" . "0.5")
                    ("@1$1=tan(45);D" . "1.")
                    ("@1$1=taylor(exp(x),x=7,2)"
                     . "1096.63315843 x + 548.316579215 (x - 7)^2 - 6579.79895058")
                    ("@1$1=taylor(exp(x),x=0,2)" . "x + x^2 / 2 + 1")
                    ("@1$1=if(1<20,teen,string(\"\"))" . "teen")
                    ("@1$1=string(42)" . "42")
                    ("@1$1=vmean(@1$2..@1$2);Ef-1" . "")
                    ("@1$1=typeof(string(\"\"))" . "101")))
      (expect
        (table-calc-eval-test-evaluate
         (car case)
         :cells (table-calc-eval-test-cell-map '(1 2 "")))
        :to-equal (cdr case)))
    (expect (table-calc-eval-test-evaluate "@1$1=vmean(@1$2..@1$3)"
                                           :cells (table-calc-eval-test-cell-map
                                                   '(1 2 "2") '(1 3 "4")))
            :to-equal "3")
    (expect (table-calc-eval-test-evaluate "@1$1=vsum(@1$2..@1$3)"
                                           :cells (table-calc-eval-test-cell-map
                                                   '(1 2 "2") '(1 3 "4")))
            :to-equal "6"))

  (it "keeps default result display separate from calculation precision"
    (expect (table-calc-eval-test-evaluate "@1$1=exp(1);p20")
            :to-equal "2.7182818"))

  (it "formats numeric results with Calc output modes and format strings"
    (dolist (case '(("@1$1=1/3;n3" . "0.333")
                    ("@1$1=12345.0;s3" . "1.23e4")
                    ("@1$1=12345.0;e2" . "12e3")
                    ("@1$1=1/3;f4" . "0.3333")
                    ("@1$1=12345.0;%.2e" . "1.23e+04")
                    ("@1$1=sin(30);Dp3%.1e" . "5.0e-01")
                    ("@1$1=sin(30);ENp3%.1f" . "0.5")))
      (expect (table-calc-eval-test-evaluate (car case))
              :to-equal (cdr case))))

  (it "supports duration suffix modes and explicit degree and radian modes"
    (dolist (case '(("@1$1=2:12+1:47;T" . "03:59:00")
                    ("@1$1=2:12+1:47;U" . "03:59")
                    ("@1$1=3:02:20+(-2:07:00);t" . "0.92")
                    ("@1$1=sin(30);D" . "0.5")
                    ("@1$1=sin(90);R" . "0.89399666")
                    ("@1$1=1;F" . "1")
                    ("@1$1=1/3;F" . "1:3")
                    ("@1$1=1;S" . "1")
                    ("@1$1=1;u" . "1")))
      (expect (table-calc-eval-test-evaluate (car case)) :to-equal (cdr case)))
    (let ((cells (table-calc-eval-test-cell-map
                  '(1 1 "1:02") '(1 2 "2:03"))))
      (dolist (case '(("@1$3=@1$1+@1$2;T" . "03:05:00")
                      ("@1$3=@1$1+@1$2;U" . "03:05")
                      ("@1$3=@1$1+@1$2;t" . "3.08")))
        (expect (table-calc-eval-test-evaluate (car case) :cells cells)
                :to-equal (cdr case))))
    (expect (table-calc-eval-test-evaluate
             "@1$3=@1$1+@1$2;t"
             :cells (table-calc-eval-test-cell-map
                     '(1 1 "1:30:00") '(1 2 "2:00:00")))
            :to-equal "3.50"))

  (it "reports symbolic and unit mode limits with a named table error"
    (let ((symbolic
            (table-calc-eval-test-condition "@1$1=x+1;S"))
          (unit-bearing
            (table-calc-eval-test-condition
             "@1$1=@1$2;u"
             :cells (table-calc-eval-test-cell-map '(1 2 "12 km")))))
      (table-calc-eval-test-expect-table-error symbolic :unsupported-mode)
      (table-calc-eval-test-expect-table-error unit-bearing :unsupported-mode)))

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
            :to-equal "1e-3")))

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

  (it "evaluates @0 as the current row"
    (expect (table-calc-eval-test-evaluate
             "$2=@0$1"
             :base-row 3 :base-column 2 :last-row 3
             :cells (table-calc-eval-test-cell-map '(3 1 "42")))
            :to-equal "42"))

  (it "rejects relative references that resolve outside the table"
    (let ((condition
            (table-calc-eval-test-condition
             "@1$1=@+1$-1" :base-column 1
             :cells (table-calc-eval-test-cell-map))))
      (table-calc-eval-test-expect-table-error condition :unsupported-reference)
      (expect (org-table-error-target condition) :to-equal "@1$1")))

  (it "rejects current-row references when the position resolver returns row zero"
    (let ((condition
            (table-calc-eval-test-condition
             "$2=@0$1" :base-row 0 :base-column 2
             :cells (table-calc-eval-test-cell-map))))
      (table-calc-eval-test-expect-table-error condition :unsupported-reference)
      (expect (org-table-error-target condition) :to-equal "$2")))

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
      (expect before-insertion :to-equal "10")
      (expect after-insertion :to-equal "10"))
    (let ((condition
            (table-calc-eval-test-condition
             "@1$3=vsum(@I$2..@II$2)"
             :cells (table-calc-eval-test-cell-map '(1 2 "10")))))
      (progn
        (table-calc-eval-test-expect-table-error condition :unsupported-reference)
        (table-calc-eval-test-expect-table-error
         (table-calc-eval-test-condition
          "@1$3=vsum(@I$2..@II$2)"
          :hline-anchors (list (cons 1 2) (cons 2 1)))
         :invalid-reference))))

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
                :to-equal "1000."))))

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
    (dolist (case '(("@<$1=1/0" . "@<$1")
                    ("@>$1=1/0" . "@>$1")))
      (let ((condition (table-calc-eval-test-condition (car case))))
        (table-calc-eval-test-expect-table-error condition :division-by-zero)
        (expect (org-table-error-target condition) :to-equal (cdr case))))
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
              :to-equal "100.")
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

(describe "table formula evaluator helper branches"
  (it "uses default precision in decimal arithmetic helpers"
    (let* ((limits (make-limits :max-formula-precision 4))
           (zero (table-calc-test-decimal "0" limits))
           (one (table-calc-test-decimal "1" limits))
           (two (table-calc-test-decimal "2" limits))
           (large (table-calc-test-decimal "1e8" limits)))
      (expect (org-kit::safe-decimal-rational
               (org-kit::table-calc-decimal-add zero zero limits))
              :to-equal 0)
      (expect (org-kit::safe-decimal-rational
               (org-kit::table-calc-decimal-add zero one limits))
              :to-equal 1)
      (expect (org-kit::safe-decimal-rational
               (org-kit::table-calc-decimal-add one zero limits))
              :to-equal 1)
      (expect (org-kit::safe-decimal-rational
               (org-kit::table-calc-decimal-add large one limits))
              :to-equal 100000000)
      (expect (org-kit::safe-decimal-rational
               (org-kit::table-calc-decimal-add one two limits))
              :to-equal 3)
      (expect (org-kit::safe-decimal-rational
               (org-kit::table-calc-decimal-multiply
                (table-calc-test-decimal "1.2" limits)
                (table-calc-test-decimal "5" limits) limits))
              :to-equal 6)
      (expect (org-kit::safe-decimal-rational
               (org-kit::table-calc-decimal-multiply zero one limits))
              :to-equal 0)
      (expect (org-kit::safe-decimal-rational
               (org-kit::table-calc-decimal-divide
                one (table-calc-test-decimal "4" limits) limits "target"))
              :to-equal 1/4)
      (expect (org-kit::safe-decimal-rational
               (org-kit::table-calc-decimal-divide
                zero two limits "target"))
              :to-equal 0)))

  (it "evaluates mixed rational and decimal arithmetic and decimal conversions"
    (let* ((limits (make-limits))
           (decimal (table-calc-test-decimal "1.25" limits))
           (one (table-calc-test-decimal "1" limits))
           (zero (table-calc-test-decimal "0" limits)))
      (dolist (case `((#\+ 1/3 1/2 5/6)
                      (#\- 1/3 1/2 -1/6)
                      (#\* 1/3 1/2 1/6)
                      (#\/ 1/3 1/2 2/3)
                      (#\+ ,one 1/3 4/3)
                      (#\+ 1/3 ,one 4/3)
                      (#\+ 1/3 ,zero 1/3)))
        (expect (org-kit::table-calc-decimal-binary
                 (first case) (second case) (third case) limits "target")
                :to-equal (fourth case)))
      (table-calc-eval-test-expect-table-error
       (handler-case
           (org-kit::table-calc-decimal-binary #\/ 1/3 0 limits "target")
         (error (condition) condition))
       :division-by-zero)
      (let ((negative-rational
              (org-kit::table-calc-number-to-decimal -1/3 limits))
            (terminating-rational
              (org-kit::table-calc-number-to-decimal 1/8 limits))
            (float (org-kit::table-calc-number-to-decimal 1d0 limits)))
        (expect (minusp (org-kit::safe-decimal-rational negative-rational))
                :to-be t)
        (expect (org-kit::safe-decimal-rational terminating-rational)
                :to-equal 1/8)
        (expect (org-kit::safe-decimal-float-p float) :to-be t)
        (expect (org-kit::safe-decimal-rational float) :to-equal 1))
      (dolist (condition
                (list (handler-case
                          (org-kit::table-calc-float-to-decimal
                           most-positive-double-float limits)
                        (error (condition) condition))
                      (handler-case
                          (org-kit::table-calc-float-to-decimal #c(1d0 2d0) limits)
                        (error (condition) condition))))
        (expect (typep condition 'org-table-error) :to-be t)
        (when (typep condition 'org-table-error)
          (expect (org-table-error-reason condition) :to-be :non-finite-result)
          (expect (org-table-error-target condition) :to-be :formula)))
      (let ((condition
              (handler-case
                  (org-kit::table-calc-number-to-decimal t limits)
                (error (condition) condition))))
        (expect (typep condition 'org-table-error) :to-be t)
        (when (typep condition 'org-table-error)
          (expect (org-table-error-reason condition) :to-be :invalid-value)
          (expect (org-table-error-target condition) :to-be :formula)))
      (expect (org-kit::safe-decimal-rational
               (org-kit::table-calc-float-to-decimal 1d0 limits))
              :to-equal 1)
      (expect (org-kit::table-calc-decimal-negative-p decimal) :to-be nil)))

  (it "coerces scalar values, durations, and Calc type codes"
    (let* ((limits (make-limits))
           (decimal (table-calc-test-decimal "1.25" limits))
           (integer-decimal (table-calc-test-decimal "3" limits))
           (fraction-decimal (org-kit::%make-safe-decimal
                              :coefficient 15 :exponent -1
                              :negative-zero-p nil :float-p nil))
           (float-decimal (org-kit::%make-safe-decimal
                           :coefficient 15 :exponent -1
                           :negative-zero-p nil :float-p t)))
      (expect (org-kit::safe-decimal-rational
               (org-kit::table-calc-coerce-reference-value
                decimal limits nil))
              :to-equal 5/4)
      (expect (org-kit::table-calc-coerce-reference-value
               decimal limits t)
              :to-equal 5/4)
      (expect (org-kit::safe-decimal-rational
               (org-kit::table-calc-coerce-reference-value
                3/2 limits nil))
              :to-equal 3/2)
      (expect (org-kit::table-calc-coerce-reference-value 3/2 limits t)
              :to-equal 3/2)
      (expect (org-kit::table-calc-coerce-reference-value
               " 2.50 " limits t)
              :to-equal 5/2)
      (expect (org-kit::safe-decimal-rational
               (org-kit::table-calc-coerce-reference-value
                "2.50" limits nil))
              :to-equal 5/2)
      (expect (org-kit::safe-decimal-rational
               (org-kit::table-calc-coerce-reference-value
                " -1:02 " limits nil t))
              :to-equal -3720)
      (expect (org-kit::table-calc-coerce-reference-value
               "-1:02" limits t t)
              :to-equal -3720)
      (expect (org-kit::table-calc-coerce-reference-value "units" limits nil)
              :to-equal "units")
      (expect (org-kit::table-calc-coerce-reference-value t limits nil)
              :to-be t)
      (dolist (text '("1:2" "1:99"))
        (table-calc-eval-test-expect-table-error
         (handler-case
             (org-kit::table-calc-coerce-reference-value
              text limits nil nil t "target")
           (error (condition) condition))
         :unsupported-mode))
      (expect (org-kit::table-calc-function-input-mode :e) :to-equal :e)
      (expect (org-kit::table-calc-function-input-mode :n) :to-equal :n)
      (expect (org-kit::table-calc-function-input-mode :en) :to-equal :e)
      (expect (org-kit::table-calc-function-input-mode :default)
              :to-equal :default)
      (expect (org-kit::table-calc-type-code org-kit::+table-calc-nan+)
              :to-equal 12)
      (expect (org-kit::table-calc-type-code "text") :to-equal 101)
      (expect (org-kit::table-calc-type-code float-decimal) :to-equal 3)
      (expect (org-kit::table-calc-type-code fraction-decimal) :to-equal 2)
      (expect (org-kit::table-calc-type-code integer-decimal) :to-equal 1)
      (expect (org-kit::table-calc-type-code 4) :to-equal 1)
      (expect (org-kit::table-calc-type-code 1/2) :to-equal 2)
      (expect (org-kit::table-calc-type-code 1d0) :to-equal 3)
      (expect (org-kit::table-calc-type-code t) :to-equal 0)
      (expect (org-kit::table-calc-value-string "text" limits)
              :to-equal "text")
      (expect (org-kit::table-calc-value-string decimal limits)
              :to-equal "1.25")
      (expect (org-kit::table-calc-value-string 5/2 limits)
              :to-equal "2.5")
      (expect (org-kit::table-calc-value-string org-kit::+table-calc-nan+ limits)
              :to-equal "nan")
      (let ((condition
              (handler-case (org-kit::table-calc-value-string t limits)
                (error (condition) condition))))
        (expect (typep condition 'org-table-error) :to-be t)
        (when (typep condition 'org-table-error)
          (expect (org-table-error-reason condition)
                  :to-be :invalid-function-result)
          (expect (org-table-error-target condition) :to-be :formula)))))

  (it "compares string and numeric operands and rejects mixed values"
    (dolist (case '((:less "a" "b" t)
                    (:less "b" "a" nil)
                    (:greater "b" "a" t)
                    (:greater "a" "b" nil)
                    (:equal "same" "same" t)
                    (:equal "a" "b" nil)
                    (:less 1/3 1/2 t)
                    (:greater 1/2 1/3 t)
                    (:equal 1/2 2/4 t)))
      (expect (org-kit::table-calc-compare-values
               (first case) (second case) (third case) "target")
              :to-be (fourth case)))
    (table-calc-eval-test-expect-table-error
     (handler-case
         (org-kit::table-calc-compare-values :equal "1" 1 "target")
       (error (condition) condition))
     :invalid-value)
    (expect (org-kit::table-calc-callback-argument
             (table-calc-test-decimal "2.5") "target")
            :to-equal 5/2)
    (expect (org-kit::table-calc-callback-argument 3/2 "target")
            :to-equal 3/2)
    (expect (org-kit::table-calc-callback-argument "text" "target")
            :to-equal "text")
    (table-calc-eval-test-expect-table-error
     (handler-case (org-kit::table-calc-callback-argument t "target")
       (error (condition) condition))
     :invalid-function-result)
    (expect (org-kit::table-calc-callback-result "text" "target")
            :to-equal "text")
    (expect (org-kit::table-calc-callback-result 3/2 "target")
            :to-equal 3/2)
    (table-calc-eval-test-expect-table-error
     (handler-case (org-kit::table-calc-callback-result t "target")
       (error (condition) condition))
     :invalid-function-result))

  (it "covers rational vectors and remote range callback values"
    (let ((functions (list (cons "third" (lambda (value)
                                          (declare (ignore value)) 1/3)))))
      (dolist (formula '("@1$1=1/3+1/2;F"
                         "@1$1=third(1)+1;F"
                         "@1$1=1+third(1);F"))
        (expect (table-calc-eval-test-evaluate
                 formula :functions functions)
                :to-equal (if (string= formula "@1$1=1/3+1/2;F")
                              "5:6" "4:3"))))
    (let ((cells (table-calc-eval-test-cell-map
                  '(1 2 "1.5") '(1 3 "0.75"))))
      (dolist (case '(("@1$1=vsum(@1$2..@1$3);F" . "9:4")
                      ("@1$1=vmean(@1$2..@1$3);F" . "9:8")
                      ("@1$1=vmin(@1$2..@1$3);F" . "3:4")
                      ("@1$1=vmax(@1$2..@1$3);F" . "3:2")))
        (expect (table-calc-eval-test-evaluate (car case) :cells cells)
                :to-equal (cdr case)))
      (table-calc-eval-test-expect-table-error
       (handler-case
           (org-kit::table-calc-evaluate-vector
            "unknown" (org-kit::%make-table-calc-range-value 1 1 1 1)
            :default (table-calc-eval-test-cell-value-function cells)
            (make-limits) (org-kit::make-table-calc-work-budget) "target")
         (error (condition) condition))
       :invalid-function-result)
      (expect (table-calc-eval-test-evaluate
               "@1$1=@1$2;F"
               :cells (table-calc-eval-test-cell-map '(1 2 "1.5")))
              :to-equal "3:2"))
    (expect (table-calc-eval-test-evaluate
             "@1$1=vsum(remote(inputs,@1$1..@1$2))"
             :remote-function (lambda (name reference row column)
                                (declare (ignore reference row column))
                                (expect name :to-equal "inputs")
                                #("2" "3")))
            :to-equal "5")
    (expect (table-calc-eval-test-evaluate
             "@1$1=vsum(remote(inputs,@1$1..@1$2))"
             :remote-function (lambda (name reference row column)
                                (declare (ignore name reference row column))
                                '("2" "3")))
            :to-equal "5")
    (expect (table-calc-eval-test-evaluate
             "@1$1=remote(inputs,@1$2)"
             :remote-function (lambda (name reference row column)
                                (declare (ignore name reference row column))
                                "2.5"))
            :to-equal "2.5")
    (table-calc-eval-test-expect-table-error
     (table-calc-eval-test-condition
      "@1$1=remote(inputs,@1$1..@1$2)"
      :remote-function (lambda (name reference row column)
                         (declare (ignore name reference row column))
                         "not-a-range"))
     :invalid-function-result)
    (table-calc-eval-test-expect-table-error
     (table-calc-eval-test-condition "@1$1=remote(inputs,@1$1)")
     :unknown-remote-table)
    (table-calc-eval-test-expect-table-error
     (handler-case
         (org-kit::table-calc-function-value
          "remote" nil nil (make-limits)
          (org-kit::make-table-calc-work-budget) "target" nil nil :default
          nil nil 12)
       (error (condition) condition))
     :unknown-remote-table))

  (it "covers conditional, logical, unary, and callback error paths"
    (expect (table-calc-eval-test-expression-value
             "@1$1=1<2||2<1")
            :to-be t)
    (expect (table-calc-eval-test-expression-value
             "@1$1=1<0||2<1")
            :to-be nil)
    (table-calc-eval-test-expect-table-error
     (table-calc-eval-test-condition "@1$1=\"x\"||1<2")
     :invalid-value)
    (expect (table-calc-eval-test-expression-value "@1$1=+1/3;F")
            :to-equal 1/3)
    (expect (table-calc-eval-test-expression-value "@1$1=-1/3;F")
            :to-equal -1/3)
    (expect (org-kit::safe-decimal-rational
             (table-calc-eval-test-expression-value "@1$1=+1"))
            :to-equal 1)
    (dolist (case '(("@1$1=+\"x\"" :invalid-number)
                    ("@1$1=-\"x\"" :invalid-number)
                    ("@1$1=+\"x\";S" :unsupported-mode)
                    ("@1$1=-\"x\";S" :unsupported-mode)
                    ("@1$1=(1<2)+1" :invalid-number)
                    ("@1$1=1+(1<2)" :invalid-number)
                    ("@1$1=(1<2)+1;S" :unsupported-mode)))
      (table-calc-eval-test-expect-table-error
       (table-calc-eval-test-condition (first case)) (second case)))
    (expect (table-calc-eval-test-evaluate "@1$1=if(1<0,2,3)")
            :to-equal "3")
    (expect (table-calc-eval-test-evaluate "@1$1=if(3,2,1)")
            :to-equal "2")
    (progn
      (expect (table-calc-eval-test-evaluate "@1$1=if(0,2,1)") :to-equal "1")
      (table-calc-eval-test-expect-table-error
       (table-calc-eval-test-condition
        "@1$1=if(@1$1,2,1)"
        :reference-value-function
        (lambda (reference)
          (declare (ignore reference))
          (values :invalid t)))
       :invalid-value))
    (dolist (name '("exp" "sin" "tan"))
      (table-calc-eval-test-expect-table-error
       (table-calc-eval-test-condition
        (format nil "@1$1=~A(\"x\")" name))
       :invalid-number)
      (table-calc-eval-test-expect-table-error
       (table-calc-eval-test-condition
        (format nil "@1$1=~A(\"x\");S" name))
       :unsupported-mode))
    (table-calc-eval-test-expect-table-error
     (table-calc-eval-test-condition
      "@1$1=identity(1<2)"
      :functions (list (cons "identity" #'identity)))
     :invalid-function-result)
    (table-calc-eval-test-expect-table-error
     (table-calc-eval-test-condition
      "@1$1=constant(1)"
      :functions (list (cons "constant" (lambda (value)
                                            (declare (ignore value)) t))))
     :invalid-function-result)
    (table-calc-eval-test-expect-table-error
     (table-calc-eval-test-condition
      "@1$1=missing(1)"
      :functions (list (cons "missing" nil)))
     :unknown-function))

  (it "covers Taylor validation and precision-independent formatting branches"
    (expect (stringp (table-calc-eval-test-evaluate
                      "@1$1=taylor(exp(x),x=1/3,2);F"))
            :to-be t)
    (expect (org-kit::table-calc-taylor-float-string 1d0)
            :to-equal "1")
    (dolist (case '(("@1$1=taylor(sin(x),x=0,2)" :unsupported-mode)
                    ("@1$1=taylor(exp(x),x=0,3)" :unsupported-mode)
                    ("@1$1=taylor(1,x=0,2)" :unsupported-mode)
                    ("@1$1=taylor(exp(x),x=y,2)" :unsupported-mode)
                    ("@1$1=taylor(exp(x),x=710,2)" :non-finite-result)
                    ("@1$1=exp(710)" :non-finite-result)))
      (table-calc-eval-test-expect-table-error
       (table-calc-eval-test-condition (first case)) (second case)))
    (table-calc-eval-test-expect-table-error
     (table-calc-eval-test-condition
      "@1$1=taylor(@1$2,x=7,2)"
      :cells (table-calc-eval-test-cell-map '(1 2 "3")))
     :unsupported-mode)
    (dolist (text '("abc" "1:2"))
      (expect (org-kit::table-calc-duration-cell-seconds text)
              :to-be nil))
    (dolist (text '("1:99" "1:00:99"))
      (expect (org-kit::table-calc-duration-cell-seconds text)
              :to-be nil))
    (expect (org-kit::table-calc-duration-cell-seconds "1:02")
            :to-equal 3720)
    (expect (org-kit::table-calc-duration-cell-seconds "1:02:03")
            :to-equal 3723)
    (expect (org-kit::table-calc-duration-cell-seconds "-1:02")
            :to-equal -3720)
    (expect (org-kit::table-calc-rounded-seconds 3/2) :to-equal 2)
    (expect (org-kit::table-calc-rounded-seconds -3/2) :to-equal -2)
    (expect (table-calc-eval-test-evaluate "@1$1=3/2;T")
            :to-equal "00:00:02")
    (expect (table-calc-eval-test-evaluate "@1$1=-3/2;T")
            :to-equal "-00:00:02")
    (expect (org-kit::table-calc-format-result
             (table-calc-test-decimal "1.25") -1 nil (make-limits) "target")
            :to-equal "1.2")
    (expect (org-kit::table-calc-format-result
             org-kit::+table-calc-nan+ -1 nil (make-limits) "target")
            :to-equal "")
    (table-calc-eval-test-expect-table-error
     (handler-case
         (org-kit::table-calc-format-result t nil nil (make-limits) "target")
       (error (condition) condition))
     :unsupported-mode))

  (it "rejects evaluator-only malformed expression nodes"
    (let* ((limits (make-limits))
           (expression (org-kit::%make-table-calc-expression
                        :kind :unknown :start 0 :end 0 :value nil
                        :children nil :depth 1)))
      (table-calc-eval-test-expect-table-error
       (handler-case
           (org-kit::table-calc-evaluate-expression
            expression
            (table-calc-eval-test-position-function 1 1 1 nil)
            (table-calc-eval-test-cell-value-function
             (table-calc-eval-test-cell-map))
            :default limits (org-kit::make-table-calc-work-budget) "target")
         (error (condition) condition))
       :unsupported-mode)))
  (it "covers empty coercions, duration bounds, zero formatting, and malformed Taylor ASTs"
    (let ((limits (make-limits)))
      (with-soft-assertions
        (dolist (text '("" ":12" "1:02:3"))
          (expect (org-kit::table-calc-duration-cell-seconds text)
                  :to-be nil))
        (expect (org-kit::table-calc-coerce-reference-value "" limits nil) :to-equal "")
        (expect (org-kit::table-calc-taylor-float-string 0d0)
                :to-equal "0")
        (expect (org-kit::table-calc-number-base10-order 0 limits)
                :to-be 0)
        (expect (org-kit::table-calc-scientific-string 0 2 nil limits t)
                :to-equal "0.00e+00")
        (expect (org-kit::table-calc-scientific-string 1d12 2 nil limits t) :to-equal "1.00e+12")
        (expect (table-calc-eval-test-evaluate "@1$1=taylor(exp(x),x=1,2)")
                :to-equal "2.71828182846 x + 1.35914091423 (x - 1)^2 + 0")
        (table-calc-eval-test-expect-table-error
         (table-calc-eval-test-condition
          "@1$1=taylor(exp(x),x=1,string(\"2\"))")
         :unsupported-mode)
        (expect
         (org-kit::table-calc-number-rational
          (org-kit::table-calc-evaluate-vector
           :vsum
           (org-kit::%make-table-calc-remote-range-value '("2" "3"))
           :default nil limits (org-kit::make-table-calc-work-budget) "target"))
         :to-equal 5)
        (table-calc-eval-test-expect-table-error
         (table-calc-eval-test-condition
          "@1$1=vsum(remote(inputs,@1$1..@1$2))"
          :remote-function (lambda (name reference row column)
                             (declare (ignore name reference row column))
                             42))
         :invalid-function-result)
        (flet ((expression (kind value children)
                 (org-kit::%make-table-calc-expression
                  :kind kind :start 0 :end 0 :value value
                  :children children :depth 1)))
          (let* ((symbol (expression :symbol "x" nil))
                 (other-symbol (expression :symbol "y" nil))
                 (number (expression :number 2 nil))
                 (variable-assignment
                   (expression :assignment nil
                               (list symbol (expression :number 1 nil))))
                 (degree (expression :number 2 nil))
                 (bad-functions
                   (list (expression :function :exp (list symbol))
                         (expression :function "sin" (list symbol))
                         (expression :function "exp" nil)
                         (expression :function "exp" (list number))
                         (expression :function "exp" (list other-symbol)))))
            (dolist (function-expression bad-functions)
              (table-calc-eval-test-expect-table-error
               (handler-case
                   (org-kit::table-calc-taylor-number
                    (expression :taylor nil
                                (list function-expression
                                      variable-assignment degree))
                    nil nil :default limits
                    (org-kit::make-table-calc-work-budget) "target"
                    nil nil nil nil nil nil)
                 (error (condition) condition))
               :unsupported-mode)))))))
  (it "parses org-sbe literal values and rejects malformed reader data"
      (let* ((quote-character (string (code-char 39)))
             (limits (make-limits))
             (assignment
              (table-calc-eval-test-assignment
               (format nil "@1$1=~A(org-sbe sample)" quote-character)))
             (expression (org-kit::table-calc-assignment-expression assignment))
             (callbacks (make-callback-registry)))
        (labels ((evaluate-source (source &optional org-sbe-function (evaluation-limits limits))
                                  (setf (org-kit::table-calc-expression-value expression) source)
                                  (org-kit::table-calc-evaluate-assignment
                                   assignment
                                   (table-calc-eval-test-position-function 1 1 1 nil)
                                   (table-calc-eval-test-cell-value-function
                                    (table-calc-eval-test-cell-map))
                                   :limits evaluation-limits
                                   :work-budget (org-kit::make-table-calc-work-budget)
                                   :callbacks callbacks
                                   :org-sbe-function org-sbe-function))
                 (unsupported (source)
                              (handler-case
                                  (evaluate-source source)
                                (org-table-unsupported-syntax (condition) condition))))
          (let* ((escaped-value
                  (format nil
                          "line~Cnext~Cpage~Cform~Cback~Creturn quote~C slash~Cend"
                          #\Newline #\Tab #\Page #\Backspace #\Return #\" #\\))
                 (source
                  (format nil
                          "  ~A(org-sbe sample (bare $ \"literal text\") (number $ 7) (bare-number 7) (list $ (1 2)) (empty) (42) (many \"1\" \"2\") (literal-many $ \"3\" $ \"4\") (literal-numbers $ 1 $ 2) (escaped $ \"line\\nnext\\tpage\\fform\\bback\\rreturn quote\\\" slash\\\\end\") (nil))"
                          quote-character))
                 (received nil)
                 (result
                  (evaluate-source
                   source
                   (lambda (block-name header variables variable-parameters)
                     (setf received
                           (list block-name header variables variable-parameters))
                     "block result"))))
            (with-soft-assertions
             (expect result :to-equal "block result")
             (expect received
                     :to-equal
                     (list "sample" nil
                          (list (cons "bare" "literal text")
                                (cons "number" 7)
                                (cons "bare-number" 7)
                                (cons "list" '(1 2))
                                (cons "empty" "nil")
                                (cons "42" "nil")
                                (cons "many" '(1 2))
                                (cons "literal-many" (list "3" "4"))
                                (cons "literal-numbers" '(1 2))
                                (cons "escaped" escaped-value)
                                (cons "nil" "nil"))
                          (list (list "bare" (list "literal text") (list t))
                                (list "number" (list 7) (list t))
                                (list "bare-number" (list 7) (list nil))
                                (list "list" (list '(1 2)) (list t))
                                (list "empty" nil nil)
                                (list "42" nil nil)
                                (list "many" (list 1 2) (list nil nil))
                                (list "literal-many" (list "3" "4") (list t t))
                                (list "literal-numbers" (list 1 2) (list t t))
                                (list "escaped" (list escaped-value) (list t))
                                (list "nil" nil nil))))))
          (let ((evaluator-inputs nil)
                (non-list (format nil "~Anon-list" quote-character)))
            (register-callback
             callbacks :evaluator :emacs-lisp
             (lambda (source)
               (setf evaluator-inputs (append evaluator-inputs (list source)))
               "evaluator result"))
            (with-soft-assertions
             (dolist (source (list "" quote-character non-list))
               (expect (evaluate-source source) :to-equal "evaluator result"))
             (expect evaluator-inputs
                     :to-equal
                     (list "" quote-character non-list))))
          (dolist (case
                      (list
                       (cons (format nil "~A(org-sbe sample #.(error \"blocked\"))"
                                     quote-character)
                             :reader-dispatch)
                       (cons (format nil "~A(org-sbe sample))" quote-character)
                             :org-sbe)
                       (cons (format nil "~A(org-sbe sample" quote-character)
                             :org-sbe)
                       (cons (format nil "~A(org-sbe sample) trailing" quote-character)
                             :org-sbe)
                       (cons (format nil "~A(org-sbe sample (one .))" quote-character)
                             :org-sbe)
                       (cons (format nil "~A(org-sbe sample (many 1 2))" quote-character)
                             :org-sbe)
                       (cons (format nil "~A(org-sbe sample (many \"1\" 2))" quote-character)
                             :org-sbe)
                       (cons (format nil "~A(org-sbe)" quote-character)
                             :org-sbe)
                       (cons (format nil "~A(org-sbe sample 42)" quote-character)
                             :org-sbe)
                       (cons (format nil "~A(org-sbe sample ())" quote-character)
                             :org-sbe)
                       (cons (format nil "~A(org-sbe sample (one $))" quote-character)
                             :org-sbe)
                       (cons (format nil "~A(org-sbe sample (one $ (a . b)))"
                                     quote-character)
                             :org-sbe)
                       (cons (format nil "~A(org-sbe sample (one (a . b)))"
                                     quote-character)
                             :org-sbe)
                       (cons (format nil "~A(org-sbe sample)" quote-character)
                             :org-sbe)
                       (cons (format nil "~A(org-sbe 42)" quote-character)
                             :org-sbe)
                       (cons (format nil
                              "~A(org-sbe sample (many \"1\" \"(\"))"
                              quote-character)
                             :org-sbe)
                       (cons (format nil
                              "~A(org-sbe sample (many \"1\" \"\"))"
                              quote-character)
                             :org-sbe)
                       (cons (format nil "~A(org-sbe . sample)" quote-character)
                             :org-sbe)
                       (cons (format nil
                              "~A(org-sbe sample (many \"1\" \"(1 . 2)\"))"
                              quote-character)
                             :org-sbe)))
            (table-calc-eval-test-expect-unsupported-syntax
             (unsupported (car case))
             (cdr case)))
          (let* ((source
                   (format nil
                           "~A(org-sbe sample (many \"1\" \"(((1)))\"))"
                           quote-character))
                 (condition
                   (handler-case
                       (progn
                         (evaluate-source source nil
                                          (make-limits :max-formula-ast-depth 2))
                         nil)
                     (org-resource-limit-error (condition) condition))))
            (table-calc-eval-test-expect-limit
             condition :max-formula-ast-depth 2 3))))))
